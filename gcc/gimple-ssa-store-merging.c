/* GIMPLE store merging pass.
   Copyright (C) 2016-2017 Free Software Foundation, Inc.
   Contributed by ARM Ltd.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

/* The purpose of this pass is to combine multiple memory stores of
   constant values, values loaded from memory or bitwise operations
   on those to consecutive memory locations into fewer wider stores.
   For example, if we have a sequence peforming four byte stores to
   consecutive memory locations:
   [p     ] := imm1;
   [p + 1B] := imm2;
   [p + 2B] := imm3;
   [p + 3B] := imm4;
   we can transform this into a single 4-byte store if the target supports it:
  [p] := imm1:imm2:imm3:imm4 //concatenated immediates according to endianness.

   Or:
   [p     ] := [q     ];
   [p + 1B] := [q + 1B];
   [p + 2B] := [q + 2B];
   [p + 3B] := [q + 3B];
   if there is no overlap can be transformed into a single 4-byte
   load followed by single 4-byte store.

   Or:
   [p     ] := [q     ] ^ imm1;
   [p + 1B] := [q + 1B] ^ imm2;
   [p + 2B] := [q + 2B] ^ imm3;
   [p + 3B] := [q + 3B] ^ imm4;
   if there is no overlap can be transformed into a single 4-byte
   load, xored with imm1:imm2:imm3:imm4 and stored using a single 4-byte store.

   The algorithm is applied to each basic block in three phases:

   1) Scan through the basic block recording assignments to
   destinations that can be expressed as a store to memory of a certain size
   at a certain bit offset from expressions we can handle.  For bit-fields
   we also note the surrounding bit region, bits that could be stored in
   a read-modify-write operation when storing the bit-field.  Record store
   chains to different bases in a hash_map (m_stores) and make sure to
   terminate such chains when appropriate (for example when when the stored
   values get used subsequently).
   These stores can be a result of structure element initializers, array stores
   etc.  A store_immediate_info object is recorded for every such store.
   Record as many such assignments to a single base as possible until a
   statement that interferes with the store sequence is encountered.
   Each store has up to 2 operands, which can be an immediate constant
   or a memory load, from which the value to be stored can be computed.
   At most one of the operands can be a constant.  The operands are recorded
   in store_operand_info struct.

   2) Analyze the chain of stores recorded in phase 1) (i.e. the vector of
   store_immediate_info objects) and coalesce contiguous stores into
   merged_store_group objects.  For bit-fields stores, we don't need to
   require the stores to be contiguous, just their surrounding bit regions
   have to be contiguous.  If the expression being stored is different
   between adjacent stores, such as one store storing a constant and
   following storing a value loaded from memory, or if the loaded memory
   objects are not adjacent, a new merged_store_group is created as well.

   For example, given the stores:
   [p     ] := 0;
   [p + 1B] := 1;
   [p + 3B] := 0;
   [p + 4B] := 1;
   [p + 5B] := 0;
   [p + 6B] := 0;
   This phase would produce two merged_store_group objects, one recording the
   two bytes stored in the memory region [p : p + 1] and another
   recording the four bytes stored in the memory region [p + 3 : p + 6].

   3) The merged_store_group objects produced in phase 2) are processed
   to generate the sequence of wider stores that set the contiguous memory
   regions to the sequence of bytes that correspond to it.  This may emit
   multiple stores per store group to handle contiguous stores that are not
   of a size that is a power of 2.  For example it can try to emit a 40-bit
   store as a 32-bit store followed by an 8-bit store.
   We try to emit as wide stores as we can while respecting STRICT_ALIGNMENT or
   TARGET_SLOW_UNALIGNED_ACCESS rules.

   Note on endianness and example:
   Consider 2 contiguous 16-bit stores followed by 2 contiguous 8-bit stores:
   [p     ] := 0x1234;
   [p + 2B] := 0x5678;
   [p + 4B] := 0xab;
   [p + 5B] := 0xcd;

   The memory layout for little-endian (LE) and big-endian (BE) must be:
  p |LE|BE|
  ---------
  0 |34|12|
  1 |12|34|
  2 |78|56|
  3 |56|78|
  4 |ab|ab|
  5 |cd|cd|

  To merge these into a single 48-bit merged value 'val' in phase 2)
  on little-endian we insert stores to higher (consecutive) bitpositions
  into the most significant bits of the merged value.
  The final merged value would be: 0xcdab56781234

  For big-endian we insert stores to higher bitpositions into the least
  significant bits of the merged value.
  The final merged value would be: 0x12345678abcd

  Then, in phase 3), we want to emit this 48-bit value as a 32-bit store
  followed by a 16-bit store.  Again, we must consider endianness when
  breaking down the 48-bit value 'val' computed above.
  For little endian we emit:
  [p]      (32-bit) := 0x56781234; // val & 0x0000ffffffff;
  [p + 4B] (16-bit) := 0xcdab;    // (val & 0xffff00000000) >> 32;

  Whereas for big-endian we emit:
  [p]      (32-bit) := 0x12345678; // (val & 0xffffffff0000) >> 16;
  [p + 4B] (16-bit) := 0xabcd;     //  val & 0x00000000ffff;  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "tree.h"
#include "gimple.h"
#include "builtins.h"
#include "fold-const.h"
#include "tree-pass.h"
#include "ssa.h"
#include "gimple-pretty-print.h"
#include "alias.h"
#include "fold-const.h"
#include "params.h"
#include "print-tree.h"
#include "tree-hash-traits.h"
#include "gimple-iterator.h"
#include "gimplify.h"
#include "stor-layout.h"
#include "timevar.h"
#include "tree-cfg.h"
#include "tree-eh.h"
#include "target.h"
#include "gimplify-me.h"
#include "rtl.h"
#include "expr.h"	/* For get_bit_range.  */
#include "selftest.h"

/* The maximum size (in bits) of the stores this pass should generate.  */
#define MAX_STORE_BITSIZE (BITS_PER_WORD)
#define MAX_STORE_BYTES (MAX_STORE_BITSIZE / BITS_PER_UNIT)

/* Limit to bound the number of aliasing checks for loads with the same
   vuse as the corresponding store.  */
#define MAX_STORE_ALIAS_CHECKS 64

namespace {

/* Struct recording one operand for the store, which is either a constant,
   then VAL represents the constant and all the other fields are zero,
   or a memory load, then VAL represents the reference, BASE_ADDR is non-NULL
   and the other fields also reflect the memory load.  */

struct store_operand_info
{
  tree val;
  tree base_addr;
  unsigned HOST_WIDE_INT bitsize;
  unsigned HOST_WIDE_INT bitpos;
  unsigned HOST_WIDE_INT bitregion_start;
  unsigned HOST_WIDE_INT bitregion_end;
  gimple *stmt;
  store_operand_info ();
};

store_operand_info::store_operand_info ()
  : val (NULL_TREE), base_addr (NULL_TREE), bitsize (0), bitpos (0),
    bitregion_start (0), bitregion_end (0), stmt (NULL)
{
}

/* Struct recording the information about a single store of an immediate
   to memory.  These are created in the first phase and coalesced into
   merged_store_group objects in the second phase.  */

struct store_immediate_info
{
  unsigned HOST_WIDE_INT bitsize;
  unsigned HOST_WIDE_INT bitpos;
  unsigned HOST_WIDE_INT bitregion_start;
  /* This is one past the last bit of the bit region.  */
  unsigned HOST_WIDE_INT bitregion_end;
  gimple *stmt;
  unsigned int order;
  /* INTEGER_CST for constant stores, MEM_REF for memory copy or
     BIT_*_EXPR for logical bitwise operation.  */
  enum tree_code rhs_code;
  /* Operands.  For BIT_*_EXPR rhs_code both operands are used, otherwise
     just the first one.  */
  store_operand_info ops[2];
  store_immediate_info (unsigned HOST_WIDE_INT, unsigned HOST_WIDE_INT,
			unsigned HOST_WIDE_INT, unsigned HOST_WIDE_INT,
			gimple *, unsigned int, enum tree_code,
			const store_operand_info &,
			const store_operand_info &);
};

store_immediate_info::store_immediate_info (unsigned HOST_WIDE_INT bs,
					    unsigned HOST_WIDE_INT bp,
					    unsigned HOST_WIDE_INT brs,
					    unsigned HOST_WIDE_INT bre,
					    gimple *st,
					    unsigned int ord,
					    enum tree_code rhscode,
					    const store_operand_info &op0r,
					    const store_operand_info &op1r)
  : bitsize (bs), bitpos (bp), bitregion_start (brs), bitregion_end (bre),
    stmt (st), order (ord), rhs_code (rhscode)
#if __cplusplus >= 201103L
    , ops { op0r, op1r }
{
}
#else
{
  ops[0] = op0r;
  ops[1] = op1r;
}
#endif

/* Struct representing a group of stores to contiguous memory locations.
   These are produced by the second phase (coalescing) and consumed in the
   third phase that outputs the widened stores.  */

struct merged_store_group
{
  unsigned HOST_WIDE_INT start;
  unsigned HOST_WIDE_INT width;
  unsigned HOST_WIDE_INT bitregion_start;
  unsigned HOST_WIDE_INT bitregion_end;
  /* The size of the allocated memory for val and mask.  */
  unsigned HOST_WIDE_INT buf_size;
  unsigned HOST_WIDE_INT align_base;
  unsigned HOST_WIDE_INT load_align_base[2];

  unsigned int align;
  unsigned int load_align[2];
  unsigned int first_order;
  unsigned int last_order;

  auto_vec<store_immediate_info *> stores;
  /* We record the first and last original statements in the sequence because
     we'll need their vuse/vdef and replacement position.  It's easier to keep
     track of them separately as 'stores' is reordered by apply_stores.  */
  gimple *last_stmt;
  gimple *first_stmt;
  unsigned char *val;
  unsigned char *mask;

  merged_store_group (store_immediate_info *);
  ~merged_store_group ();
  void merge_into (store_immediate_info *);
  void merge_overlapping (store_immediate_info *);
  bool apply_stores ();
private:
  void do_merge (store_immediate_info *);
};

/* Debug helper.  Dump LEN elements of byte array PTR to FD in hex.  */

static void
dump_char_array (FILE *fd, unsigned char *ptr, unsigned int len)
{
  if (!fd)
    return;

  for (unsigned int i = 0; i < len; i++)
    fprintf (fd, "%x ", ptr[i]);
  fprintf (fd, "\n");
}

/* Shift left the bytes in PTR of SZ elements by AMNT bits, carrying over the
   bits between adjacent elements.  AMNT should be within
   [0, BITS_PER_UNIT).
   Example, AMNT = 2:
   00011111|11100000 << 2 = 01111111|10000000
   PTR[1]  | PTR[0]         PTR[1]  | PTR[0].  */

static void
shift_bytes_in_array (unsigned char *ptr, unsigned int sz, unsigned int amnt)
{
  if (amnt == 0)
    return;

  unsigned char carry_over = 0U;
  unsigned char carry_mask = (~0U) << (unsigned char) (BITS_PER_UNIT - amnt);
  unsigned char clear_mask = (~0U) << amnt;

  for (unsigned int i = 0; i < sz; i++)
    {
      unsigned prev_carry_over = carry_over;
      carry_over = (ptr[i] & carry_mask) >> (BITS_PER_UNIT - amnt);

      ptr[i] <<= amnt;
      if (i != 0)
	{
	  ptr[i] &= clear_mask;
	  ptr[i] |= prev_carry_over;
	}
    }
}

/* Like shift_bytes_in_array but for big-endian.
   Shift right the bytes in PTR of SZ elements by AMNT bits, carrying over the
   bits between adjacent elements.  AMNT should be within
   [0, BITS_PER_UNIT).
   Example, AMNT = 2:
   00011111|11100000 >> 2 = 00000111|11111000
   PTR[0]  | PTR[1]         PTR[0]  | PTR[1].  */

static void
shift_bytes_in_array_right (unsigned char *ptr, unsigned int sz,
			    unsigned int amnt)
{
  if (amnt == 0)
    return;

  unsigned char carry_over = 0U;
  unsigned char carry_mask = ~(~0U << amnt);

  for (unsigned int i = 0; i < sz; i++)
    {
      unsigned prev_carry_over = carry_over;
      carry_over = ptr[i] & carry_mask;

      carry_over <<= (unsigned char) BITS_PER_UNIT - amnt;
      ptr[i] >>= amnt;
      ptr[i] |= prev_carry_over;
    }
}

/* Clear out LEN bits starting from bit START in the byte array
   PTR.  This clears the bits to the *right* from START.
   START must be within [0, BITS_PER_UNIT) and counts starting from
   the least significant bit.  */

static void
clear_bit_region_be (unsigned char *ptr, unsigned int start,
		     unsigned int len)
{
  if (len == 0)
    return;
  /* Clear len bits to the right of start.  */
  else if (len <= start + 1)
    {
      unsigned char mask = (~(~0U << len));
      mask = mask << (start + 1U - len);
      ptr[0] &= ~mask;
    }
  else if (start != BITS_PER_UNIT - 1)
    {
      clear_bit_region_be (ptr, start, (start % BITS_PER_UNIT) + 1);
      clear_bit_region_be (ptr + 1, BITS_PER_UNIT - 1,
			   len - (start % BITS_PER_UNIT) - 1);
    }
  else if (start == BITS_PER_UNIT - 1
	   && len > BITS_PER_UNIT)
    {
      unsigned int nbytes = len / BITS_PER_UNIT;
      memset (ptr, 0, nbytes);
      if (len % BITS_PER_UNIT != 0)
	clear_bit_region_be (ptr + nbytes, BITS_PER_UNIT - 1,
			     len % BITS_PER_UNIT);
    }
  else
    gcc_unreachable ();
}

/* In the byte array PTR clear the bit region starting at bit
   START and is LEN bits wide.
   For regions spanning multiple bytes do this recursively until we reach
   zero LEN or a region contained within a single byte.  */

static void
clear_bit_region (unsigned char *ptr, unsigned int start,
		  unsigned int len)
{
  /* Degenerate base case.  */
  if (len == 0)
    return;
  else if (start >= BITS_PER_UNIT)
    clear_bit_region (ptr + 1, start - BITS_PER_UNIT, len);
  /* Second base case.  */
  else if ((start + len) <= BITS_PER_UNIT)
    {
      unsigned char mask = (~0U) << (unsigned char) (BITS_PER_UNIT - len);
      mask >>= BITS_PER_UNIT - (start + len);

      ptr[0] &= ~mask;

      return;
    }
  /* Clear most significant bits in a byte and proceed with the next byte.  */
  else if (start != 0)
    {
      clear_bit_region (ptr, start, BITS_PER_UNIT - start);
      clear_bit_region (ptr + 1, 0, len - (BITS_PER_UNIT - start));
    }
  /* Whole bytes need to be cleared.  */
  else if (start == 0 && len > BITS_PER_UNIT)
    {
      unsigned int nbytes = len / BITS_PER_UNIT;
      /* We could recurse on each byte but we clear whole bytes, so a simple
	 memset will do.  */
      memset (ptr, '\0', nbytes);
      /* Clear the remaining sub-byte region if there is one.  */
      if (len % BITS_PER_UNIT != 0)
	clear_bit_region (ptr + nbytes, 0, len % BITS_PER_UNIT);
    }
  else
    gcc_unreachable ();
}

/* Write BITLEN bits of EXPR to the byte array PTR at
   bit position BITPOS.  PTR should contain TOTAL_BYTES elements.
   Return true if the operation succeeded.  */

static bool
encode_tree_to_bitpos (tree expr, unsigned char *ptr, int bitlen, int bitpos,
		       unsigned int total_bytes)
{
  unsigned int first_byte = bitpos / BITS_PER_UNIT;
  tree tmp_int = expr;
  bool sub_byte_op_p = ((bitlen % BITS_PER_UNIT)
			|| (bitpos % BITS_PER_UNIT)
			|| !int_mode_for_size (bitlen, 0).exists ());

  if (!sub_byte_op_p)
    return native_encode_expr (tmp_int, ptr + first_byte, total_bytes) != 0;

  /* LITTLE-ENDIAN
     We are writing a non byte-sized quantity or at a position that is not
     at a byte boundary.
     |--------|--------|--------| ptr + first_byte
           ^              ^
           xxx xxxxxxxx xxx< bp>
           |______EXPR____|

     First native_encode_expr EXPR into a temporary buffer and shift each
     byte in the buffer by 'bp' (carrying the bits over as necessary).
     |00000000|00xxxxxx|xxxxxxxx| << bp = |000xxxxx|xxxxxxxx|xxx00000|
                                              <------bitlen---->< bp>
    Then we clear the destination bits:
    |---00000|00000000|000-----| ptr + first_byte
        <-------bitlen--->< bp>

    Finally we ORR the bytes of the shifted EXPR into the cleared region:
    |---xxxxx||xxxxxxxx||xxx-----| ptr + first_byte.

   BIG-ENDIAN
   We are writing a non byte-sized quantity or at a position that is not
   at a byte boundary.
     ptr + first_byte |--------|--------|--------|
                            ^              ^
                       <bp >xxx xxxxxxxx xxx
                            |_____EXPR_____|

     First native_encode_expr EXPR into a temporary buffer and shift each
     byte in the buffer to the right by (carrying the bits over as necessary).
     We shift by as much as needed to align the most significant bit of EXPR
     with bitpos:
     |00xxxxxx|xxxxxxxx| >> 3 = |00000xxx|xxxxxxxx|xxxxx000|
        <---bitlen---->          <bp ><-----bitlen----->
    Then we clear the destination bits:
    ptr + first_byte |-----000||00000000||00000---|
                      <bp ><-------bitlen----->

    Finally we ORR the bytes of the shifted EXPR into the cleared region:
    ptr + first_byte |---xxxxx||xxxxxxxx||xxx-----|.
    The awkwardness comes from the fact that bitpos is counted from the
    most significant bit of a byte.  */

  /* We must be dealing with fixed-size data at this point, since the
     total size is also fixed.  */
  fixed_size_mode mode = as_a <fixed_size_mode> (TYPE_MODE (TREE_TYPE (expr)));
  /* Allocate an extra byte so that we have space to shift into.  */
  unsigned int byte_size = GET_MODE_SIZE (mode) + 1;
  unsigned char *tmpbuf = XALLOCAVEC (unsigned char, byte_size);
  memset (tmpbuf, '\0', byte_size);
  /* The store detection code should only have allowed constants that are
     accepted by native_encode_expr.  */
  if (native_encode_expr (expr, tmpbuf, byte_size - 1) == 0)
    gcc_unreachable ();

  /* The native_encode_expr machinery uses TYPE_MODE to determine how many
     bytes to write.  This means it can write more than
     ROUND_UP (bitlen, BITS_PER_UNIT) / BITS_PER_UNIT bytes (for example
     write 8 bytes for a bitlen of 40).  Skip the bytes that are not within
     bitlen and zero out the bits that are not relevant as well (that may
     contain a sign bit due to sign-extension).  */
  unsigned int padding
    = byte_size - ROUND_UP (bitlen, BITS_PER_UNIT) / BITS_PER_UNIT - 1;
  /* On big-endian the padding is at the 'front' so just skip the initial
     bytes.  */
  if (BYTES_BIG_ENDIAN)
    tmpbuf += padding;

  byte_size -= padding;

  if (bitlen % BITS_PER_UNIT != 0)
    {
      if (BYTES_BIG_ENDIAN)
	clear_bit_region_be (tmpbuf, BITS_PER_UNIT - 1,
			     BITS_PER_UNIT - (bitlen % BITS_PER_UNIT));
      else
	clear_bit_region (tmpbuf, bitlen,
			  byte_size * BITS_PER_UNIT - bitlen);
    }
  /* Left shifting relies on the last byte being clear if bitlen is
     a multiple of BITS_PER_UNIT, which might not be clear if
     there are padding bytes.  */
  else if (!BYTES_BIG_ENDIAN)
    tmpbuf[byte_size - 1] = '\0';

  /* Clear the bit region in PTR where the bits from TMPBUF will be
     inserted into.  */
  if (BYTES_BIG_ENDIAN)
    clear_bit_region_be (ptr + first_byte,
			 BITS_PER_UNIT - 1 - (bitpos % BITS_PER_UNIT), bitlen);
  else
    clear_bit_region (ptr + first_byte, bitpos % BITS_PER_UNIT, bitlen);

  int shift_amnt;
  int bitlen_mod = bitlen % BITS_PER_UNIT;
  int bitpos_mod = bitpos % BITS_PER_UNIT;

  bool skip_byte = false;
  if (BYTES_BIG_ENDIAN)
    {
      /* BITPOS and BITLEN are exactly aligned and no shifting
	 is necessary.  */
      if (bitpos_mod + bitlen_mod == BITS_PER_UNIT
	  || (bitpos_mod == 0 && bitlen_mod == 0))
	shift_amnt = 0;
      /* |. . . . . . . .|
	  <bp >   <blen >.
	 We always shift right for BYTES_BIG_ENDIAN so shift the beginning
	 of the value until it aligns with 'bp' in the next byte over.  */
      else if (bitpos_mod + bitlen_mod < BITS_PER_UNIT)
	{
	  shift_amnt = bitlen_mod + bitpos_mod;
	  skip_byte = bitlen_mod != 0;
	}
      /* |. . . . . . . .|
	  <----bp--->
	    <---blen---->.
	 Shift the value right within the same byte so it aligns with 'bp'.  */
      else
	shift_amnt = bitlen_mod + bitpos_mod - BITS_PER_UNIT;
    }
  else
    shift_amnt = bitpos % BITS_PER_UNIT;

  /* Create the shifted version of EXPR.  */
  if (!BYTES_BIG_ENDIAN)
    {
      shift_bytes_in_array (tmpbuf, byte_size, shift_amnt);
      if (shift_amnt == 0)
	byte_size--;
    }
  else
    {
      gcc_assert (BYTES_BIG_ENDIAN);
      shift_bytes_in_array_right (tmpbuf, byte_size, shift_amnt);
      /* If shifting right forced us to move into the next byte skip the now
	 empty byte.  */
      if (skip_byte)
	{
	  tmpbuf++;
	  byte_size--;
	}
    }

  /* Insert the bits from TMPBUF.  */
  for (unsigned int i = 0; i < byte_size; i++)
    ptr[first_byte + i] |= tmpbuf[i];

  return true;
}

/* Sorting function for store_immediate_info objects.
   Sorts them by bitposition.  */

static int
sort_by_bitpos (const void *x, const void *y)
{
  store_immediate_info *const *tmp = (store_immediate_info * const *) x;
  store_immediate_info *const *tmp2 = (store_immediate_info * const *) y;

  if ((*tmp)->bitpos < (*tmp2)->bitpos)
    return -1;
  else if ((*tmp)->bitpos > (*tmp2)->bitpos)
    return 1;
  else
    /* If they are the same let's use the order which is guaranteed to
       be different.  */
    return (*tmp)->order - (*tmp2)->order;
}

/* Sorting function for store_immediate_info objects.
   Sorts them by the order field.  */

static int
sort_by_order (const void *x, const void *y)
{
  store_immediate_info *const *tmp = (store_immediate_info * const *) x;
  store_immediate_info *const *tmp2 = (store_immediate_info * const *) y;

  if ((*tmp)->order < (*tmp2)->order)
    return -1;
  else if ((*tmp)->order > (*tmp2)->order)
    return 1;

  gcc_unreachable ();
}

/* Initialize a merged_store_group object from a store_immediate_info
   object.  */

merged_store_group::merged_store_group (store_immediate_info *info)
{
  start = info->bitpos;
  width = info->bitsize;
  bitregion_start = info->bitregion_start;
  bitregion_end = info->bitregion_end;
  /* VAL has memory allocated for it in apply_stores once the group
     width has been finalized.  */
  val = NULL;
  mask = NULL;
  unsigned HOST_WIDE_INT align_bitpos = 0;
  get_object_alignment_1 (gimple_assign_lhs (info->stmt),
			  &align, &align_bitpos);
  align_base = start - align_bitpos;
  for (int i = 0; i < 2; ++i)
    {
      store_operand_info &op = info->ops[i];
      if (op.base_addr == NULL_TREE)
	{
	  load_align[i] = 0;
	  load_align_base[i] = 0;
	}
      else
	{
	  get_object_alignment_1 (op.val, &load_align[i], &align_bitpos);
	  load_align_base[i] = op.bitpos - align_bitpos;
	}
    }
  stores.create (1);
  stores.safe_push (info);
  last_stmt = info->stmt;
  last_order = info->order;
  first_stmt = last_stmt;
  first_order = last_order;
  buf_size = 0;
}

merged_store_group::~merged_store_group ()
{
  if (val)
    XDELETEVEC (val);
}

/* Helper method for merge_into and merge_overlapping to do
   the common part.  */
void
merged_store_group::do_merge (store_immediate_info *info)
{
  bitregion_start = MIN (bitregion_start, info->bitregion_start);
  bitregion_end = MAX (bitregion_end, info->bitregion_end);

  unsigned int this_align;
  unsigned HOST_WIDE_INT align_bitpos = 0;
  get_object_alignment_1 (gimple_assign_lhs (info->stmt),
			  &this_align, &align_bitpos);
  if (this_align > align)
    {
      align = this_align;
      align_base = info->bitpos - align_bitpos;
    }
  for (int i = 0; i < 2; ++i)
    {
      store_operand_info &op = info->ops[i];
      if (!op.base_addr)
	continue;

      get_object_alignment_1 (op.val, &this_align, &align_bitpos);
      if (this_align > load_align[i])
	{
	  load_align[i] = this_align;
	  load_align_base[i] = op.bitpos - align_bitpos;
	}
    }

  gimple *stmt = info->stmt;
  stores.safe_push (info);
  if (info->order > last_order)
    {
      last_order = info->order;
      last_stmt = stmt;
    }
  else if (info->order < first_order)
    {
      first_order = info->order;
      first_stmt = stmt;
    }
}

/* Merge a store recorded by INFO into this merged store.
   The store is not overlapping with the existing recorded
   stores.  */

void
merged_store_group::merge_into (store_immediate_info *info)
{
  unsigned HOST_WIDE_INT wid = info->bitsize;
  /* Make sure we're inserting in the position we think we're inserting.  */
  gcc_assert (info->bitpos >= start + width
	      && info->bitregion_start <= bitregion_end);

  width += wid;
  do_merge (info);
}

/* Merge a store described by INFO into this merged store.
   INFO overlaps in some way with the current store (i.e. it's not contiguous
   which is handled by merged_store_group::merge_into).  */

void
merged_store_group::merge_overlapping (store_immediate_info *info)
{
  /* If the store extends the size of the group, extend the width.  */
  if (info->bitpos + info->bitsize > start + width)
    width += info->bitpos + info->bitsize - (start + width);

  do_merge (info);
}

/* Go through all the recorded stores in this group in program order and
   apply their values to the VAL byte array to create the final merged
   value.  Return true if the operation succeeded.  */

bool
merged_store_group::apply_stores ()
{
  /* Make sure we have more than one store in the group, otherwise we cannot
     merge anything.  */
  if (bitregion_start % BITS_PER_UNIT != 0
      || bitregion_end % BITS_PER_UNIT != 0
      || stores.length () == 1)
    return false;

  stores.qsort (sort_by_order);
  store_immediate_info *info;
  unsigned int i;
  /* Create a buffer of a size that is 2 times the number of bytes we're
     storing.  That way native_encode_expr can write power-of-2-sized
     chunks without overrunning.  */
  buf_size = 2 * ((bitregion_end - bitregion_start) / BITS_PER_UNIT);
  val = XNEWVEC (unsigned char, 2 * buf_size);
  mask = val + buf_size;
  memset (val, 0, buf_size);
  memset (mask, ~0U, buf_size);

  FOR_EACH_VEC_ELT (stores, i, info)
    {
      unsigned int pos_in_buffer = info->bitpos - bitregion_start;
      tree cst = NULL_TREE;
      if (info->ops[0].val && info->ops[0].base_addr == NULL_TREE)
	cst = info->ops[0].val;
      else if (info->ops[1].val && info->ops[1].base_addr == NULL_TREE)
	cst = info->ops[1].val;
      bool ret = true;
      if (cst)
	ret = encode_tree_to_bitpos (cst, val, info->bitsize,
				     pos_in_buffer, buf_size);
      if (cst && dump_file && (dump_flags & TDF_DETAILS))
	{
	  if (ret)
	    {
	      fprintf (dump_file, "After writing ");
	      print_generic_expr (dump_file, cst, 0);
	      fprintf (dump_file, " of size " HOST_WIDE_INT_PRINT_DEC
			" at position %d the merged region contains:\n",
			info->bitsize, pos_in_buffer);
	      dump_char_array (dump_file, val, buf_size);
	    }
	  else
	    fprintf (dump_file, "Failed to merge stores\n");
        }
      if (!ret)
	return false;
      unsigned char *m = mask + (pos_in_buffer / BITS_PER_UNIT);
      if (BYTES_BIG_ENDIAN)
	clear_bit_region_be (m, (BITS_PER_UNIT - 1
				 - (pos_in_buffer % BITS_PER_UNIT)),
			     info->bitsize);
      else
	clear_bit_region (m, pos_in_buffer % BITS_PER_UNIT, info->bitsize);
    }
  return true;
}

/* Structure describing the store chain.  */

struct imm_store_chain_info
{
  /* Doubly-linked list that imposes an order on chain processing.
     PNXP (prev's next pointer) points to the head of a list, or to
     the next field in the previous chain in the list.
     See pass_store_merging::m_stores_head for more rationale.  */
  imm_store_chain_info *next, **pnxp;
  tree base_addr;
  auto_vec<store_immediate_info *> m_store_info;
  auto_vec<merged_store_group *> m_merged_store_groups;

  imm_store_chain_info (imm_store_chain_info *&inspt, tree b_a)
  : next (inspt), pnxp (&inspt), base_addr (b_a)
  {
    inspt = this;
    if (next)
      {
	gcc_checking_assert (pnxp == next->pnxp);
	next->pnxp = &next;
      }
  }
  ~imm_store_chain_info ()
  {
    *pnxp = next;
    if (next)
      {
	gcc_checking_assert (&next == next->pnxp);
	next->pnxp = pnxp;
      }
  }
  bool terminate_and_process_chain ();
  bool coalesce_immediate_stores ();
  bool output_merged_store (merged_store_group *);
  bool output_merged_stores ();
};

const pass_data pass_data_tree_store_merging = {
  GIMPLE_PASS,     /* type */
  "store-merging", /* name */
  OPTGROUP_NONE,   /* optinfo_flags */
  TV_GIMPLE_STORE_MERGING,	 /* tv_id */
  PROP_ssa,	/* properties_required */
  0,		   /* properties_provided */
  0,		   /* properties_destroyed */
  0,		   /* todo_flags_start */
  TODO_update_ssa, /* todo_flags_finish */
};

class pass_store_merging : public gimple_opt_pass
{
public:
  pass_store_merging (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_tree_store_merging, ctxt), m_stores_head ()
  {
  }

  /* Pass not supported for PDP-endianness, nor for insane hosts
     or target character sizes where native_{encode,interpret}_expr
     doesn't work properly.  */
  virtual bool
  gate (function *)
  {
    return flag_store_merging
	   && WORDS_BIG_ENDIAN == BYTES_BIG_ENDIAN
	   && CHAR_BIT == 8
	   && BITS_PER_UNIT == 8;
  }

  virtual unsigned int execute (function *);

private:
  hash_map<tree_operand_hash, struct imm_store_chain_info *> m_stores;

  /* Form a doubly-linked stack of the elements of m_stores, so that
     we can iterate over them in a predictable way.  Using this order
     avoids extraneous differences in the compiler output just because
     of tree pointer variations (e.g. different chains end up in
     different positions of m_stores, so they are handled in different
     orders, so they allocate or release SSA names in different
     orders, and when they get reused, subsequent passes end up
     getting different SSA names, which may ultimately change
     decisions when going out of SSA).  */
  imm_store_chain_info *m_stores_head;

  void process_store (gimple *);
  bool terminate_and_process_all_chains ();
  bool terminate_all_aliasing_chains (imm_store_chain_info **,
				      gimple *);
  bool terminate_and_release_chain (imm_store_chain_info *);
}; // class pass_store_merging

/* Terminate and process all recorded chains.  Return true if any changes
   were made.  */

bool
pass_store_merging::terminate_and_process_all_chains ()
{
  bool ret = false;
  while (m_stores_head)
    ret |= terminate_and_release_chain (m_stores_head);
  gcc_assert (m_stores.elements () == 0);
  gcc_assert (m_stores_head == NULL);

  return ret;
}

/* Terminate all chains that are affected by the assignment to DEST, appearing
   in statement STMT and ultimately points to the object BASE.  Return true if
   at least one aliasing chain was terminated.  BASE and DEST are allowed to
   be NULL_TREE.  In that case the aliasing checks are performed on the whole
   statement rather than a particular operand in it.  VAR_OFFSET_P signifies
   whether STMT represents a store to BASE offset by a variable amount.
   If that is the case we have to terminate any chain anchored at BASE.  */

bool
pass_store_merging::terminate_all_aliasing_chains (imm_store_chain_info
						     **chain_info,
						   gimple *stmt)
{
  bool ret = false;

  /* If the statement doesn't touch memory it can't alias.  */
  if (!gimple_vuse (stmt))
    return false;

  /* Check if the assignment destination (BASE) is part of a store chain.
     This is to catch non-constant stores to destinations that may be part
     of a chain.  */
  if (chain_info)
    {
      store_immediate_info *info;
      unsigned int i;
      FOR_EACH_VEC_ELT ((*chain_info)->m_store_info, i, info)
	{
	  if (ref_maybe_used_by_stmt_p (stmt, gimple_assign_lhs (info->stmt))
	      || stmt_may_clobber_ref_p (stmt, gimple_assign_lhs (info->stmt)))
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  fprintf (dump_file, "stmt causes chain termination:\n");
		  print_gimple_stmt (dump_file, stmt, 0);
		}
	      terminate_and_release_chain (*chain_info);
	      ret = true;
	      break;
	    }
	}
    }

  /* Check for aliasing with all other store chains.  */
  for (imm_store_chain_info *next = m_stores_head, *cur = next; cur; cur = next)
    {
      next = cur->next;

      /* We already checked all the stores in chain_info and terminated the
	 chain if necessary.  Skip it here.  */
      if (chain_info && (*chain_info) == cur)
	continue;

      /* We can't use the base object here as that does not reliably exist.
	 Build a ao_ref from the base object address (if we know the
	 minimum and maximum offset and the maximum size we could improve
	 things here).  */
      ao_ref chain_ref;
      ao_ref_init_from_ptr_and_size (&chain_ref, cur->base_addr, NULL_TREE);
      if (ref_maybe_used_by_stmt_p (stmt, &chain_ref)
	  || stmt_may_clobber_ref_p_1 (stmt, &chain_ref))
	{
	  terminate_and_release_chain (cur);
	  ret = true;
	}
    }

  return ret;
}

/* Helper function.  Terminate the recorded chain storing to base object
   BASE.  Return true if the merging and output was successful.  The m_stores
   entry is removed after the processing in any case.  */

bool
pass_store_merging::terminate_and_release_chain (imm_store_chain_info *chain_info)
{
  bool ret = chain_info->terminate_and_process_chain ();
  m_stores.remove (chain_info->base_addr);
  delete chain_info;
  return ret;
}

/* Return true if stmts in between FIRST (inclusive) and LAST (exclusive)
   may clobber REF.  FIRST and LAST must be in the same basic block and
   have non-NULL vdef.  */

bool
stmts_may_clobber_ref_p (gimple *first, gimple *last, tree ref)
{
  ao_ref r;
  ao_ref_init (&r, ref);
  unsigned int count = 0;
  tree vop = gimple_vdef (last);
  gimple *stmt;

  gcc_checking_assert (gimple_bb (first) == gimple_bb (last));
  do
    {
      stmt = SSA_NAME_DEF_STMT (vop);
      if (stmt_may_clobber_ref_p_1 (stmt, &r))
	return true;
      /* Avoid quadratic compile time by bounding the number of checks
	 we perform.  */
      if (++count > MAX_STORE_ALIAS_CHECKS)
	return true;
      vop = gimple_vuse (stmt);
    }
  while (stmt != first);
  return false;
}

/* Return true if INFO->ops[IDX] is mergeable with the
   corresponding loads already in MERGED_STORE group.
   BASE_ADDR is the base address of the whole store group.  */

bool
compatible_load_p (merged_store_group *merged_store,
		   store_immediate_info *info,
		   tree base_addr, int idx)
{
  store_immediate_info *infof = merged_store->stores[0];
  if (!info->ops[idx].base_addr
      || (info->ops[idx].bitpos - infof->ops[idx].bitpos
	  != info->bitpos - infof->bitpos)
      || !operand_equal_p (info->ops[idx].base_addr,
			   infof->ops[idx].base_addr, 0))
    return false;

  store_immediate_info *infol = merged_store->stores.last ();
  tree load_vuse = gimple_vuse (info->ops[idx].stmt);
  /* In this case all vuses should be the same, e.g.
     _1 = s.a; _2 = s.b; _3 = _1 | 1; t.a = _3; _4 = _2 | 2; t.b = _4;
     or
     _1 = s.a; _2 = s.b; t.a = _1; t.b = _2;
     and we can emit the coalesced load next to any of those loads.  */
  if (gimple_vuse (infof->ops[idx].stmt) == load_vuse
      && gimple_vuse (infol->ops[idx].stmt) == load_vuse)
    return true;

  /* Otherwise, at least for now require that the load has the same
     vuse as the store.  See following examples.  */
  if (gimple_vuse (info->stmt) != load_vuse)
    return false;

  if (gimple_vuse (infof->stmt) != gimple_vuse (infof->ops[idx].stmt)
      || (infof != infol
	  && gimple_vuse (infol->stmt) != gimple_vuse (infol->ops[idx].stmt)))
    return false;

  /* If the load is from the same location as the store, already
     the construction of the immediate chain info guarantees no intervening
     stores, so no further checks are needed.  Example:
     _1 = s.a; _2 = _1 & -7; s.a = _2; _3 = s.b; _4 = _3 & -7; s.b = _4;  */
  if (info->ops[idx].bitpos == info->bitpos
      && operand_equal_p (info->ops[idx].base_addr, base_addr, 0))
    return true;

  /* Otherwise, we need to punt if any of the loads can be clobbered by any
     of the stores in the group, or any other stores in between those.
     Previous calls to compatible_load_p ensured that for all the
     merged_store->stores IDX loads, no stmts starting with
     merged_store->first_stmt and ending right before merged_store->last_stmt
     clobbers those loads.  */
  gimple *first = merged_store->first_stmt;
  gimple *last = merged_store->last_stmt;
  unsigned int i;
  store_immediate_info *infoc;
  /* The stores are sorted by increasing store bitpos, so if info->stmt store
     comes before the so far first load, we'll be changing
     merged_store->first_stmt.  In that case we need to give up if
     any of the earlier processed loads clobber with the stmts in the new
     range.  */
  if (info->order < merged_store->first_order)
    {
      FOR_EACH_VEC_ELT (merged_store->stores, i, infoc)
	if (stmts_may_clobber_ref_p (info->stmt, first, infoc->ops[idx].val))
	  return false;
      first = info->stmt;
    }
  /* Similarly, we could change merged_store->last_stmt, so ensure
     in that case no stmts in the new range clobber any of the earlier
     processed loads.  */
  else if (info->order > merged_store->last_order)
    {
      FOR_EACH_VEC_ELT (merged_store->stores, i, infoc)
	if (stmts_may_clobber_ref_p (last, info->stmt, infoc->ops[idx].val))
	  return false;
      last = info->stmt;
    }
  /* And finally, we'd be adding a new load to the set, ensure it isn't
     clobbered in the new range.  */
  if (stmts_may_clobber_ref_p (first, last, info->ops[idx].val))
    return false;

  /* Otherwise, we are looking for:
     _1 = s.a; _2 = _1 ^ 15; t.a = _2; _3 = s.b; _4 = _3 ^ 15; t.b = _4;
     or
     _1 = s.a; t.a = _1; _2 = s.b; t.b = _2;  */
  return true;
}

/* Go through the candidate stores recorded in m_store_info and merge them
   into merged_store_group objects recorded into m_merged_store_groups
   representing the widened stores.  Return true if coalescing was successful
   and the number of widened stores is fewer than the original number
   of stores.  */

bool
imm_store_chain_info::coalesce_immediate_stores ()
{
  /* Anything less can't be processed.  */
  if (m_store_info.length () < 2)
    return false;

  if (dump_file && (dump_flags & TDF_DETAILS))
    fprintf (dump_file, "Attempting to coalesce %u stores in chain.\n",
	     m_store_info.length ());

  store_immediate_info *info;
  unsigned int i;

  /* Order the stores by the bitposition they write to.  */
  m_store_info.qsort (sort_by_bitpos);

  info = m_store_info[0];
  merged_store_group *merged_store = new merged_store_group (info);

  FOR_EACH_VEC_ELT (m_store_info, i, info)
    {
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Store %u:\nbitsize:" HOST_WIDE_INT_PRINT_DEC
			      " bitpos:" HOST_WIDE_INT_PRINT_DEC " val:\n",
		   i, info->bitsize, info->bitpos);
	  print_generic_expr (dump_file, gimple_assign_rhs1 (info->stmt));
	  fprintf (dump_file, "\n------------\n");
	}

      if (i == 0)
	continue;

      /* |---store 1---|
	       |---store 2---|
       Overlapping stores.  */
      unsigned HOST_WIDE_INT start = info->bitpos;
      if (IN_RANGE (start, merged_store->start,
		    merged_store->start + merged_store->width - 1))
	{
	  /* Only allow overlapping stores of constants.  */
	  if (info->rhs_code == INTEGER_CST
	      && merged_store->stores[0]->rhs_code == INTEGER_CST)
	    {
	      merged_store->merge_overlapping (info);
	      continue;
	    }
	}
      /* |---store 1---||---store 2---|
	 This store is consecutive to the previous one.
	 Merge it into the current store group.  There can be gaps in between
	 the stores, but there can't be gaps in between bitregions.  */
      else if (info->bitregion_start <= merged_store->bitregion_end
	       && info->rhs_code == merged_store->stores[0]->rhs_code)
	{
	  store_immediate_info *infof = merged_store->stores[0];

	  /* All the rhs_code ops that take 2 operands are commutative,
	     swap the operands if it could make the operands compatible.  */
	  if (infof->ops[0].base_addr
	      && infof->ops[1].base_addr
	      && info->ops[0].base_addr
	      && info->ops[1].base_addr
	      && (info->ops[1].bitpos - infof->ops[0].bitpos
		  == info->bitpos - infof->bitpos)
	      && operand_equal_p (info->ops[1].base_addr,
				  infof->ops[0].base_addr, 0))
	    std::swap (info->ops[0], info->ops[1]);
	  if ((!infof->ops[0].base_addr
	       || compatible_load_p (merged_store, info, base_addr, 0))
	      && (!infof->ops[1].base_addr
		  || compatible_load_p (merged_store, info, base_addr, 1)))
	    {
	      merged_store->merge_into (info);
	      continue;
	    }
	}

      /* |---store 1---| <gap> |---store 2---|.
	 Gap between stores or the rhs not compatible.  Start a new group.  */

      /* Try to apply all the stores recorded for the group to determine
	 the bitpattern they write and discard it if that fails.
	 This will also reject single-store groups.  */
      if (!merged_store->apply_stores ())
	delete merged_store;
      else
	m_merged_store_groups.safe_push (merged_store);

      merged_store = new merged_store_group (info);
    }

  /* Record or discard the last store group.  */
  if (!merged_store->apply_stores ())
    delete merged_store;
  else
    m_merged_store_groups.safe_push (merged_store);

  gcc_assert (m_merged_store_groups.length () <= m_store_info.length ());
  bool success
    = !m_merged_store_groups.is_empty ()
      && m_merged_store_groups.length () < m_store_info.length ();

  if (success && dump_file)
    fprintf (dump_file, "Coalescing successful!\n"
			"Merged into %u stores\n",
	     m_merged_store_groups.length ());

  return success;
}

/* Return the type to use for the merged stores or loads described by STMTS.
   This is needed to get the alias sets right.  If IS_LOAD, look for rhs,
   otherwise lhs.  Additionally set *CLIQUEP and *BASEP to MR_DEPENDENCE_*
   of the MEM_REFs if any.  */

static tree
get_alias_type_for_stmts (vec<gimple *> &stmts, bool is_load,
			  unsigned short *cliquep, unsigned short *basep)
{
  gimple *stmt;
  unsigned int i;
  tree type = NULL_TREE;
  tree ret = NULL_TREE;
  *cliquep = 0;
  *basep = 0;

  FOR_EACH_VEC_ELT (stmts, i, stmt)
    {
      tree ref = is_load ? gimple_assign_rhs1 (stmt)
			 : gimple_assign_lhs (stmt);
      tree type1 = reference_alias_ptr_type (ref);
      tree base = get_base_address (ref);

      if (i == 0)
	{
	  if (TREE_CODE (base) == MEM_REF)
	    {
	      *cliquep = MR_DEPENDENCE_CLIQUE (base);
	      *basep = MR_DEPENDENCE_BASE (base);
	    }
	  ret = type = type1;
	  continue;
	}
      if (!alias_ptr_types_compatible_p (type, type1))
	ret = ptr_type_node;
      if (TREE_CODE (base) != MEM_REF
	  || *cliquep != MR_DEPENDENCE_CLIQUE (base)
	  || *basep != MR_DEPENDENCE_BASE (base))
	{
	  *cliquep = 0;
	  *basep = 0;
	}
    }
  return ret;
}

/* Return the location_t information we can find among the statements
   in STMTS.  */

static location_t
get_location_for_stmts (vec<gimple *> &stmts)
{
  gimple *stmt;
  unsigned int i;

  FOR_EACH_VEC_ELT (stmts, i, stmt)
    if (gimple_has_location (stmt))
      return gimple_location (stmt);

  return UNKNOWN_LOCATION;
}

/* Used to decribe a store resulting from splitting a wide store in smaller
   regularly-sized stores in split_group.  */

struct split_store
{
  unsigned HOST_WIDE_INT bytepos;
  unsigned HOST_WIDE_INT size;
  unsigned HOST_WIDE_INT align;
  auto_vec<store_immediate_info *> orig_stores;
  /* True if there is a single orig stmt covering the whole split store.  */
  bool orig;
  split_store (unsigned HOST_WIDE_INT, unsigned HOST_WIDE_INT,
	       unsigned HOST_WIDE_INT);
};

/* Simple constructor.  */

split_store::split_store (unsigned HOST_WIDE_INT bp,
			  unsigned HOST_WIDE_INT sz,
			  unsigned HOST_WIDE_INT al)
			  : bytepos (bp), size (sz), align (al), orig (false)
{
  orig_stores.create (0);
}

/* Record all stores in GROUP that write to the region starting at BITPOS and
   is of size BITSIZE.  Record infos for such statements in STORES if
   non-NULL.  The stores in GROUP must be sorted by bitposition.  Return INFO
   if there is exactly one original store in the range.  */

static store_immediate_info *
find_constituent_stores (struct merged_store_group *group,
			 vec<store_immediate_info *> *stores,
			 unsigned int *first,
			 unsigned HOST_WIDE_INT bitpos,
			 unsigned HOST_WIDE_INT bitsize)
{
  store_immediate_info *info, *ret = NULL;
  unsigned int i;
  bool second = false;
  bool update_first = true;
  unsigned HOST_WIDE_INT end = bitpos + bitsize;
  for (i = *first; group->stores.iterate (i, &info); ++i)
    {
      unsigned HOST_WIDE_INT stmt_start = info->bitpos;
      unsigned HOST_WIDE_INT stmt_end = stmt_start + info->bitsize;
      if (stmt_end <= bitpos)
	{
	  /* BITPOS passed to this function never decreases from within the
	     same split_group call, so optimize and don't scan info records
	     which are known to end before or at BITPOS next time.
	     Only do it if all stores before this one also pass this.  */
	  if (update_first)
	    *first = i + 1;
	  continue;
	}
      else
	update_first = false;

      /* The stores in GROUP are ordered by bitposition so if we're past
	 the region for this group return early.  */
      if (stmt_start >= end)
	return ret;

      if (stores)
	{
	  stores->safe_push (info);
	  if (ret)
	    {
	      ret = NULL;
	      second = true;
	    }
	}
      else if (ret)
	return NULL;
      if (!second)
	ret = info;
    }
  return ret;
}

/* Split a merged store described by GROUP by populating the SPLIT_STORES
   vector (if non-NULL) with split_store structs describing the byte offset
   (from the base), the bit size and alignment of each store as well as the
   original statements involved in each such split group.
   This is to separate the splitting strategy from the statement
   building/emission/linking done in output_merged_store.
   Return number of new stores.
   If ALLOW_UNALIGNED_STORE is false, then all stores must be aligned.
   If ALLOW_UNALIGNED_LOAD is false, then all loads must be aligned.
   If SPLIT_STORES is NULL, it is just a dry run to count number of
   new stores.  */

static unsigned int
split_group (merged_store_group *group, bool allow_unaligned_store,
	     bool allow_unaligned_load,
	     vec<struct split_store *> *split_stores)
{
  unsigned HOST_WIDE_INT pos = group->bitregion_start;
  unsigned HOST_WIDE_INT size = group->bitregion_end - pos;
  unsigned HOST_WIDE_INT bytepos = pos / BITS_PER_UNIT;
  unsigned HOST_WIDE_INT group_align = group->align;
  unsigned HOST_WIDE_INT align_base = group->align_base;
  unsigned HOST_WIDE_INT group_load_align = group_align;

  gcc_assert ((size % BITS_PER_UNIT == 0) && (pos % BITS_PER_UNIT == 0));

  unsigned int ret = 0, first = 0;
  unsigned HOST_WIDE_INT try_pos = bytepos;
  group->stores.qsort (sort_by_bitpos);

  if (!allow_unaligned_load)
    for (int i = 0; i < 2; ++i)
      if (group->load_align[i])
	group_load_align = MIN (group_load_align, group->load_align[i]);

  while (size > 0)
    {
      if ((allow_unaligned_store || group_align <= BITS_PER_UNIT)
	  && group->mask[try_pos - bytepos] == (unsigned char) ~0U)
	{
	  /* Skip padding bytes.  */
	  ++try_pos;
	  size -= BITS_PER_UNIT;
	  continue;
	}

      unsigned HOST_WIDE_INT try_bitpos = try_pos * BITS_PER_UNIT;
      unsigned int try_size = MAX_STORE_BITSIZE, nonmasked;
      unsigned HOST_WIDE_INT align_bitpos
	= (try_bitpos - align_base) & (group_align - 1);
      unsigned HOST_WIDE_INT align = group_align;
      if (align_bitpos)
	align = least_bit_hwi (align_bitpos);
      if (!allow_unaligned_store)
	try_size = MIN (try_size, align);
      if (!allow_unaligned_load)
	{
	  /* If we can't do or don't want to do unaligned stores
	     as well as loads, we need to take the loads into account
	     as well.  */
	  unsigned HOST_WIDE_INT load_align = group_load_align;
	  align_bitpos = (try_bitpos - align_base) & (load_align - 1);
	  if (align_bitpos)
	    load_align = least_bit_hwi (align_bitpos);
	  for (int i = 0; i < 2; ++i)
	    if (group->load_align[i])
	      {
		align_bitpos = try_bitpos - group->stores[0]->bitpos;
		align_bitpos += group->stores[0]->ops[i].bitpos;
		align_bitpos -= group->load_align_base[i];
		align_bitpos &= (group_load_align - 1);
		if (align_bitpos)
		  {
		    unsigned HOST_WIDE_INT a = least_bit_hwi (align_bitpos);
		    load_align = MIN (load_align, a);
		  }
	      }
	  try_size = MIN (try_size, load_align);
	}
      store_immediate_info *info
	= find_constituent_stores (group, NULL, &first, try_bitpos, try_size);
      if (info)
	{
	  /* If there is just one original statement for the range, see if
	     we can just reuse the original store which could be even larger
	     than try_size.  */
	  unsigned HOST_WIDE_INT stmt_end
	    = ROUND_UP (info->bitpos + info->bitsize, BITS_PER_UNIT);
	  info = find_constituent_stores (group, NULL, &first, try_bitpos,
					  stmt_end - try_bitpos);
	  if (info && info->bitpos >= try_bitpos)
	    {
	      try_size = stmt_end - try_bitpos;
	      goto found;
	    }
	}

      /* Approximate store bitsize for the case when there are no padding
	 bits.  */
      while (try_size > size)
	try_size /= 2;
      /* Now look for whole padding bytes at the end of that bitsize.  */
      for (nonmasked = try_size / BITS_PER_UNIT; nonmasked > 0; --nonmasked)
	if (group->mask[try_pos - bytepos + nonmasked - 1]
	    != (unsigned char) ~0U)
	  break;
      if (nonmasked == 0)
	{
	  /* If entire try_size range is padding, skip it.  */
	  try_pos += try_size / BITS_PER_UNIT;
	  size -= try_size;
	  continue;
	}
      /* Otherwise try to decrease try_size if second half, last 3 quarters
	 etc. are padding.  */
      nonmasked *= BITS_PER_UNIT;
      while (nonmasked <= try_size / 2)
	try_size /= 2;
      if (!allow_unaligned_store && group_align > BITS_PER_UNIT)
	{
	  /* Now look for whole padding bytes at the start of that bitsize.  */
	  unsigned int try_bytesize = try_size / BITS_PER_UNIT, masked;
	  for (masked = 0; masked < try_bytesize; ++masked)
	    if (group->mask[try_pos - bytepos + masked] != (unsigned char) ~0U)
	      break;
	  masked *= BITS_PER_UNIT;
	  gcc_assert (masked < try_size);
	  if (masked >= try_size / 2)
	    {
	      while (masked >= try_size / 2)
		{
		  try_size /= 2;
		  try_pos += try_size / BITS_PER_UNIT;
		  size -= try_size;
		  masked -= try_size;
		}
	      /* Need to recompute the alignment, so just retry at the new
		 position.  */
	      continue;
	    }
	}

    found:
      ++ret;

      if (split_stores)
	{
	  struct split_store *store
	    = new split_store (try_pos, try_size, align);
	  info = find_constituent_stores (group, &store->orig_stores,
					  &first, try_bitpos, try_size);
	  if (info
	      && info->bitpos >= try_bitpos
	      && info->bitpos + info->bitsize <= try_bitpos + try_size)
	    store->orig = true;
	  split_stores->safe_push (store);
	}

      try_pos += try_size / BITS_PER_UNIT;
      size -= try_size;
    }

  return ret;
}

/* Given a merged store group GROUP output the widened version of it.
   The store chain is against the base object BASE.
   Try store sizes of at most MAX_STORE_BITSIZE bits wide and don't output
   unaligned stores for STRICT_ALIGNMENT targets or if it's too expensive.
   Make sure that the number of statements output is less than the number of
   original statements.  If a better sequence is possible emit it and
   return true.  */

bool
imm_store_chain_info::output_merged_store (merged_store_group *group)
{
  unsigned HOST_WIDE_INT start_byte_pos
    = group->bitregion_start / BITS_PER_UNIT;

  unsigned int orig_num_stmts = group->stores.length ();
  if (orig_num_stmts < 2)
    return false;

  auto_vec<struct split_store *, 32> split_stores;
  split_stores.create (0);
  bool allow_unaligned_store
    = !STRICT_ALIGNMENT && PARAM_VALUE (PARAM_STORE_MERGING_ALLOW_UNALIGNED);
  bool allow_unaligned_load = allow_unaligned_store;
  if (allow_unaligned_store)
    {
      /* If unaligned stores are allowed, see how many stores we'd emit
	 for unaligned and how many stores we'd emit for aligned stores.
	 Only use unaligned stores if it allows fewer stores than aligned.  */
      unsigned aligned_cnt
	= split_group (group, false, allow_unaligned_load, NULL);
      unsigned unaligned_cnt
	= split_group (group, true, allow_unaligned_load, NULL);
      if (aligned_cnt <= unaligned_cnt)
	allow_unaligned_store = false;
    }
  split_group (group, allow_unaligned_store, allow_unaligned_load,
	       &split_stores);

  if (split_stores.length () >= orig_num_stmts)
    {
      /* We didn't manage to reduce the number of statements.  Bail out.  */
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Exceeded original number of stmts (%u)."
			      "  Not profitable to emit new sequence.\n",
		   orig_num_stmts);
	}
      return false;
    }

  gimple_stmt_iterator last_gsi = gsi_for_stmt (group->last_stmt);
  gimple_seq seq = NULL;
  tree last_vdef, new_vuse;
  last_vdef = gimple_vdef (group->last_stmt);
  new_vuse = gimple_vuse (group->last_stmt);

  gimple *stmt = NULL;
  split_store *split_store;
  unsigned int i;
  auto_vec<gimple *, 32> orig_stmts;
  tree addr = force_gimple_operand_1 (unshare_expr (base_addr), &seq,
				      is_gimple_mem_ref_addr, NULL_TREE);

  tree load_addr[2] = { NULL_TREE, NULL_TREE };
  gimple_seq load_seq[2] = { NULL, NULL };
  gimple_stmt_iterator load_gsi[2] = { gsi_none (), gsi_none () };
  for (int j = 0; j < 2; ++j)
    {
      store_operand_info &op = group->stores[0]->ops[j];
      if (op.base_addr == NULL_TREE)
	continue;

      store_immediate_info *infol = group->stores.last ();
      if (gimple_vuse (op.stmt) == gimple_vuse (infol->ops[j].stmt))
	{
	  load_gsi[j] = gsi_for_stmt (op.stmt);
	  load_addr[j]
	    = force_gimple_operand_1 (unshare_expr (op.base_addr),
				      &load_seq[j], is_gimple_mem_ref_addr,
				      NULL_TREE);
	}
      else if (operand_equal_p (base_addr, op.base_addr, 0))
	load_addr[j] = addr;
      else
	{
	  gimple_seq this_seq;
	  load_addr[j]
	    = force_gimple_operand_1 (unshare_expr (op.base_addr),
				      &this_seq, is_gimple_mem_ref_addr,
				      NULL_TREE);
	  gimple_seq_add_seq_without_update (&seq, this_seq);
	}
    }

  FOR_EACH_VEC_ELT (split_stores, i, split_store)
    {
      unsigned HOST_WIDE_INT try_size = split_store->size;
      unsigned HOST_WIDE_INT try_pos = split_store->bytepos;
      unsigned HOST_WIDE_INT align = split_store->align;
      tree dest, src;
      location_t loc;
      if (split_store->orig)
	{
	  /* If there is just a single constituent store which covers
	     the whole area, just reuse the lhs and rhs.  */
	  gimple *orig_stmt = split_store->orig_stores[0]->stmt;
	  dest = gimple_assign_lhs (orig_stmt);
	  src = gimple_assign_rhs1 (orig_stmt);
	  loc = gimple_location (orig_stmt);
	}
      else
	{
	  store_immediate_info *info;
	  unsigned short clique, base;
	  unsigned int k;
	  FOR_EACH_VEC_ELT (split_store->orig_stores, k, info)
	    orig_stmts.safe_push (info->stmt);
	  tree offset_type
	    = get_alias_type_for_stmts (orig_stmts, false, &clique, &base);
	  loc = get_location_for_stmts (orig_stmts);
	  orig_stmts.truncate (0);

	  tree int_type = build_nonstandard_integer_type (try_size, UNSIGNED);
	  int_type = build_aligned_type (int_type, align);
	  dest = fold_build2 (MEM_REF, int_type, addr,
			      build_int_cst (offset_type, try_pos));
	  if (TREE_CODE (dest) == MEM_REF)
	    {
	      MR_DEPENDENCE_CLIQUE (dest) = clique;
	      MR_DEPENDENCE_BASE (dest) = base;
	    }

	  tree mask
	    = native_interpret_expr (int_type,
				     group->mask + try_pos - start_byte_pos,
				     group->buf_size);

	  tree ops[2];
	  for (int j = 0;
	       j < 1 + (split_store->orig_stores[0]->ops[1].val != NULL_TREE);
	       ++j)
	    {
	      store_operand_info &op = split_store->orig_stores[0]->ops[j];
	      if (op.base_addr)
		{
		  FOR_EACH_VEC_ELT (split_store->orig_stores, k, info)
		    orig_stmts.safe_push (info->ops[j].stmt);

		  offset_type = get_alias_type_for_stmts (orig_stmts, true,
							  &clique, &base);
		  location_t load_loc = get_location_for_stmts (orig_stmts);
		  orig_stmts.truncate (0);

		  unsigned HOST_WIDE_INT load_align = group->load_align[j];
		  unsigned HOST_WIDE_INT align_bitpos
		    = (try_pos * BITS_PER_UNIT
		       - split_store->orig_stores[0]->bitpos
		       + op.bitpos) & (load_align - 1);
		  if (align_bitpos)
		    load_align = least_bit_hwi (align_bitpos);

		  tree load_int_type
		    = build_nonstandard_integer_type (try_size, UNSIGNED);
		  load_int_type
		    = build_aligned_type (load_int_type, load_align);

		  unsigned HOST_WIDE_INT load_pos
		    = (try_pos * BITS_PER_UNIT
		       - split_store->orig_stores[0]->bitpos
		       + op.bitpos) / BITS_PER_UNIT;
		  ops[j] = fold_build2 (MEM_REF, load_int_type, load_addr[j],
					build_int_cst (offset_type, load_pos));
		  if (TREE_CODE (ops[j]) == MEM_REF)
		    {
		      MR_DEPENDENCE_CLIQUE (ops[j]) = clique;
		      MR_DEPENDENCE_BASE (ops[j]) = base;
		    }
		  if (!integer_zerop (mask))
		    /* The load might load some bits (that will be masked off
		       later on) uninitialized, avoid -W*uninitialized
		       warnings in that case.  */
		    TREE_NO_WARNING (ops[j]) = 1;

		  stmt = gimple_build_assign (make_ssa_name (int_type),
					      ops[j]);
		  gimple_set_location (stmt, load_loc);
		  if (gsi_bb (load_gsi[j]))
		    {
		      gimple_set_vuse (stmt, gimple_vuse (op.stmt));
		      gimple_seq_add_stmt_without_update (&load_seq[j], stmt);
		    }
		  else
		    {
		      gimple_set_vuse (stmt, new_vuse);
		      gimple_seq_add_stmt_without_update (&seq, stmt);
		    }
		  ops[j] = gimple_assign_lhs (stmt);
		}
	      else
		ops[j] = native_interpret_expr (int_type,
						group->val + try_pos
						- start_byte_pos,
						group->buf_size);
	    }

	  switch (split_store->orig_stores[0]->rhs_code)
	    {
	    case BIT_AND_EXPR:
	    case BIT_IOR_EXPR:
	    case BIT_XOR_EXPR:
	      FOR_EACH_VEC_ELT (split_store->orig_stores, k, info)
		{
		  tree rhs1 = gimple_assign_rhs1 (info->stmt);
		  orig_stmts.safe_push (SSA_NAME_DEF_STMT (rhs1));
		}
	      location_t bit_loc;
	      bit_loc = get_location_for_stmts (orig_stmts);
	      orig_stmts.truncate (0);

	      stmt
		= gimple_build_assign (make_ssa_name (int_type),
				       split_store->orig_stores[0]->rhs_code,
				       ops[0], ops[1]);
	      gimple_set_location (stmt, bit_loc);
	      /* If there is just one load and there is a separate
		 load_seq[0], emit the bitwise op right after it.  */
	      if (load_addr[1] == NULL_TREE && gsi_bb (load_gsi[0]))
		gimple_seq_add_stmt_without_update (&load_seq[0], stmt);
	      /* Otherwise, if at least one load is in seq, we need to
		 emit the bitwise op right before the store.  If there
		 are two loads and are emitted somewhere else, it would
		 be better to emit the bitwise op as early as possible;
		 we don't track where that would be possible right now
		 though.  */
	      else
		gimple_seq_add_stmt_without_update (&seq, stmt);
	      src = gimple_assign_lhs (stmt);
	      break;
	    default:
	      src = ops[0];
	      break;
	    }

	  if (!integer_zerop (mask))
	    {
	      tree tem = make_ssa_name (int_type);
	      tree load_src = unshare_expr (dest);
	      /* The load might load some or all bits uninitialized,
		 avoid -W*uninitialized warnings in that case.
		 As optimization, it would be nice if all the bits are
		 provably uninitialized (no stores at all yet or previous
		 store a CLOBBER) we'd optimize away the load and replace
		 it e.g. with 0.  */
	      TREE_NO_WARNING (load_src) = 1;
	      stmt = gimple_build_assign (tem, load_src);
	      gimple_set_location (stmt, loc);
	      gimple_set_vuse (stmt, new_vuse);
	      gimple_seq_add_stmt_without_update (&seq, stmt);

	      /* FIXME: If there is a single chunk of zero bits in mask,
		 perhaps use BIT_INSERT_EXPR instead?  */
	      stmt = gimple_build_assign (make_ssa_name (int_type),
					  BIT_AND_EXPR, tem, mask);
	      gimple_set_location (stmt, loc);
	      gimple_seq_add_stmt_without_update (&seq, stmt);
	      tem = gimple_assign_lhs (stmt);

	      if (TREE_CODE (src) == INTEGER_CST)
		src = wide_int_to_tree (int_type,
					wi::bit_and_not (wi::to_wide (src),
							 wi::to_wide (mask)));
	      else
		{
		  tree nmask
		    = wide_int_to_tree (int_type,
					wi::bit_not (wi::to_wide (mask)));
		  stmt = gimple_build_assign (make_ssa_name (int_type),
					      BIT_AND_EXPR, src, nmask);
		  gimple_set_location (stmt, loc);
		  gimple_seq_add_stmt_without_update (&seq, stmt);
		  src = gimple_assign_lhs (stmt);
		}
	      stmt = gimple_build_assign (make_ssa_name (int_type),
					  BIT_IOR_EXPR, tem, src);
	      gimple_set_location (stmt, loc);
	      gimple_seq_add_stmt_without_update (&seq, stmt);
	      src = gimple_assign_lhs (stmt);
	    }
	}

      stmt = gimple_build_assign (dest, src);
      gimple_set_location (stmt, loc);
      gimple_set_vuse (stmt, new_vuse);
      gimple_seq_add_stmt_without_update (&seq, stmt);

      tree new_vdef;
      if (i < split_stores.length () - 1)
	new_vdef = make_ssa_name (gimple_vop (cfun), stmt);
      else
	new_vdef = last_vdef;

      gimple_set_vdef (stmt, new_vdef);
      SSA_NAME_DEF_STMT (new_vdef) = stmt;
      new_vuse = new_vdef;
    }

  FOR_EACH_VEC_ELT (split_stores, i, split_store)
    delete split_store;

  gcc_assert (seq);
  if (dump_file)
    {
      fprintf (dump_file,
	       "New sequence of %u stmts to replace old one of %u stmts\n",
	       split_stores.length (), orig_num_stmts);
      if (dump_flags & TDF_DETAILS)
	print_gimple_seq (dump_file, seq, 0, TDF_VOPS | TDF_MEMSYMS);
    }
  gsi_insert_seq_after (&last_gsi, seq, GSI_SAME_STMT);
  for (int j = 0; j < 2; ++j)
    if (load_seq[j])
      gsi_insert_seq_after (&load_gsi[j], load_seq[j], GSI_SAME_STMT);

  return true;
}

/* Process the merged_store_group objects created in the coalescing phase.
   The stores are all against the base object BASE.
   Try to output the widened stores and delete the original statements if
   successful.  Return true iff any changes were made.  */

bool
imm_store_chain_info::output_merged_stores ()
{
  unsigned int i;
  merged_store_group *merged_store;
  bool ret = false;
  FOR_EACH_VEC_ELT (m_merged_store_groups, i, merged_store)
    {
      if (output_merged_store (merged_store))
	{
	  unsigned int j;
	  store_immediate_info *store;
	  FOR_EACH_VEC_ELT (merged_store->stores, j, store)
	    {
	      gimple *stmt = store->stmt;
	      gimple_stmt_iterator gsi = gsi_for_stmt (stmt);
	      gsi_remove (&gsi, true);
	      if (stmt != merged_store->last_stmt)
		{
		  unlink_stmt_vdef (stmt);
		  release_defs (stmt);
		}
	    }
	  ret = true;
	}
    }
  if (ret && dump_file)
    fprintf (dump_file, "Merging successful!\n");

  return ret;
}

/* Coalesce the store_immediate_info objects recorded against the base object
   BASE in the first phase and output them.
   Delete the allocated structures.
   Return true if any changes were made.  */

bool
imm_store_chain_info::terminate_and_process_chain ()
{
  /* Process store chain.  */
  bool ret = false;
  if (m_store_info.length () > 1)
    {
      ret = coalesce_immediate_stores ();
      if (ret)
	ret = output_merged_stores ();
    }

  /* Delete all the entries we allocated ourselves.  */
  store_immediate_info *info;
  unsigned int i;
  FOR_EACH_VEC_ELT (m_store_info, i, info)
    delete info;

  merged_store_group *merged_info;
  FOR_EACH_VEC_ELT (m_merged_store_groups, i, merged_info)
    delete merged_info;

  return ret;
}

/* Return true iff LHS is a destination potentially interesting for
   store merging.  In practice these are the codes that get_inner_reference
   can process.  */

static bool
lhs_valid_for_store_merging_p (tree lhs)
{
  tree_code code = TREE_CODE (lhs);

  if (code == ARRAY_REF || code == ARRAY_RANGE_REF || code == MEM_REF
      || code == COMPONENT_REF || code == BIT_FIELD_REF)
    return true;

  return false;
}

/* Return true if the tree RHS is a constant we want to consider
   during store merging.  In practice accept all codes that
   native_encode_expr accepts.  */

static bool
rhs_valid_for_store_merging_p (tree rhs)
{
  return native_encode_expr (rhs, NULL,
			     GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (rhs)))) != 0;
}

/* If MEM is a memory reference usable for store merging (either as
   store destination or for loads), return the non-NULL base_addr
   and set *PBITSIZE, *PBITPOS, *PBITREGION_START and *PBITREGION_END.
   Otherwise return NULL, *PBITPOS should be still valid even for that
   case.  */

static tree
mem_valid_for_store_merging (tree mem, unsigned HOST_WIDE_INT *pbitsize,
			     unsigned HOST_WIDE_INT *pbitpos,
			     unsigned HOST_WIDE_INT *pbitregion_start,
			     unsigned HOST_WIDE_INT *pbitregion_end)
{
  HOST_WIDE_INT bitsize;
  HOST_WIDE_INT bitpos;
  unsigned HOST_WIDE_INT bitregion_start = 0;
  unsigned HOST_WIDE_INT bitregion_end = 0;
  machine_mode mode;
  int unsignedp = 0, reversep = 0, volatilep = 0;
  tree offset;
  tree base_addr = get_inner_reference (mem, &bitsize, &bitpos, &offset, &mode,
					&unsignedp, &reversep, &volatilep);
  *pbitsize = bitsize;
  if (bitsize == 0)
    return NULL_TREE;

  if (TREE_CODE (mem) == COMPONENT_REF
      && DECL_BIT_FIELD_TYPE (TREE_OPERAND (mem, 1)))
    {
      get_bit_range (&bitregion_start, &bitregion_end, mem, &bitpos, &offset);
      if (bitregion_end)
	++bitregion_end;
    }

  if (reversep)
    return NULL_TREE;

  /* We do not want to rewrite TARGET_MEM_REFs.  */
  if (TREE_CODE (base_addr) == TARGET_MEM_REF)
    return NULL_TREE;
  /* In some cases get_inner_reference may return a
     MEM_REF [ptr + byteoffset].  For the purposes of this pass
     canonicalize the base_addr to MEM_REF [ptr] and take
     byteoffset into account in the bitpos.  This occurs in
     PR 23684 and this way we can catch more chains.  */
  else if (TREE_CODE (base_addr) == MEM_REF)
    {
      offset_int bit_off, byte_off = mem_ref_offset (base_addr);
      bit_off = byte_off << LOG2_BITS_PER_UNIT;
      bit_off += bitpos;
      if (!wi::neg_p (bit_off) && wi::fits_shwi_p (bit_off))
	{
	  bitpos = bit_off.to_shwi ();
	  if (bitregion_end)
	    {
	      bit_off = byte_off << LOG2_BITS_PER_UNIT;
	      bit_off += bitregion_start;
	      if (wi::fits_uhwi_p (bit_off))
		{
		  bitregion_start = bit_off.to_uhwi ();
		  bit_off = byte_off << LOG2_BITS_PER_UNIT;
		  bit_off += bitregion_end;
		  if (wi::fits_uhwi_p (bit_off))
		    bitregion_end = bit_off.to_uhwi ();
		  else
		    bitregion_end = 0;
		}
	      else
		bitregion_end = 0;
	    }
	}
      else
	return NULL_TREE;
      base_addr = TREE_OPERAND (base_addr, 0);
    }
  /* get_inner_reference returns the base object, get at its
     address now.  */
  else
    {
      if (bitpos < 0)
	return NULL_TREE;
      base_addr = build_fold_addr_expr (base_addr);
    }

  if (!bitregion_end)
    {
      bitregion_start = ROUND_DOWN (bitpos, BITS_PER_UNIT);
      bitregion_end = ROUND_UP (bitpos + bitsize, BITS_PER_UNIT);
    }

  if (offset != NULL_TREE)
    {
      /* If the access is variable offset then a base decl has to be
	 address-taken to be able to emit pointer-based stores to it.
	 ???  We might be able to get away with re-using the original
	 base up to the first variable part and then wrapping that inside
	 a BIT_FIELD_REF.  */
      tree base = get_base_address (base_addr);
      if (! base
	  || (DECL_P (base) && ! TREE_ADDRESSABLE (base)))
	return NULL_TREE;

      base_addr = build2 (POINTER_PLUS_EXPR, TREE_TYPE (base_addr),
			  base_addr, offset);
    }

  *pbitsize = bitsize;
  *pbitpos = bitpos;
  *pbitregion_start = bitregion_start;
  *pbitregion_end = bitregion_end;
  return base_addr;
}

/* Return true if STMT is a load that can be used for store merging.
   In that case fill in *OP.  BITSIZE, BITPOS, BITREGION_START and
   BITREGION_END are properties of the corresponding store.  */

static bool
handled_load (gimple *stmt, store_operand_info *op,
	      unsigned HOST_WIDE_INT bitsize, unsigned HOST_WIDE_INT bitpos,
	      unsigned HOST_WIDE_INT bitregion_start,
	      unsigned HOST_WIDE_INT bitregion_end)
{
  if (!is_gimple_assign (stmt) || !gimple_vuse (stmt))
    return false;
  if (gimple_assign_load_p (stmt)
      && !stmt_can_throw_internal (stmt)
      && !gimple_has_volatile_ops (stmt))
    {
      tree mem = gimple_assign_rhs1 (stmt);
      op->base_addr
	= mem_valid_for_store_merging (mem, &op->bitsize, &op->bitpos,
				       &op->bitregion_start,
				       &op->bitregion_end);
      if (op->base_addr != NULL_TREE
	  && op->bitsize == bitsize
	  && ((op->bitpos - bitpos) % BITS_PER_UNIT) == 0
	  && op->bitpos - op->bitregion_start >= bitpos - bitregion_start
	  && op->bitregion_end - op->bitpos >= bitregion_end - bitpos)
	{
	  op->stmt = stmt;
	  op->val = mem;
	  return true;
	}
    }
  return false;
}

/* Record the store STMT for store merging optimization if it can be
   optimized.  */

void
pass_store_merging::process_store (gimple *stmt)
{
  tree lhs = gimple_assign_lhs (stmt);
  tree rhs = gimple_assign_rhs1 (stmt);
  unsigned HOST_WIDE_INT bitsize, bitpos;
  unsigned HOST_WIDE_INT bitregion_start;
  unsigned HOST_WIDE_INT bitregion_end;
  tree base_addr
    = mem_valid_for_store_merging (lhs, &bitsize, &bitpos,
				   &bitregion_start, &bitregion_end);
  if (bitsize == 0)
    return;

  bool invalid = (base_addr == NULL_TREE
		  || ((bitsize > MAX_BITSIZE_MODE_ANY_INT)
		       && (TREE_CODE (rhs) != INTEGER_CST)));
  enum tree_code rhs_code = ERROR_MARK;
  store_operand_info ops[2];
  if (invalid)
    ;
  else if (rhs_valid_for_store_merging_p (rhs))
    {
      rhs_code = INTEGER_CST;
      ops[0].val = rhs;
    }
  else if (TREE_CODE (rhs) != SSA_NAME || !has_single_use (rhs))
    invalid = true;
  else
    {
      gimple *def_stmt = SSA_NAME_DEF_STMT (rhs), *def_stmt1, *def_stmt2;
      if (!is_gimple_assign (def_stmt))
	invalid = true;
      else if (handled_load (def_stmt, &ops[0], bitsize, bitpos,
			     bitregion_start, bitregion_end))
	rhs_code = MEM_REF;
      else
	switch ((rhs_code = gimple_assign_rhs_code (def_stmt)))
	  {
	  case BIT_AND_EXPR:
	  case BIT_IOR_EXPR:
	  case BIT_XOR_EXPR:
	    tree rhs1, rhs2;
	    rhs1 = gimple_assign_rhs1 (def_stmt);
	    rhs2 = gimple_assign_rhs2 (def_stmt);
	    invalid = true;
	    if (TREE_CODE (rhs1) != SSA_NAME || !has_single_use (rhs1))
	      break;
	    def_stmt1 = SSA_NAME_DEF_STMT (rhs1);
	    if (!is_gimple_assign (def_stmt1)
		|| !handled_load (def_stmt1, &ops[0], bitsize, bitpos,
				  bitregion_start, bitregion_end))
	      break;
	    if (rhs_valid_for_store_merging_p (rhs2))
	      ops[1].val = rhs2;
	    else if (TREE_CODE (rhs2) != SSA_NAME || !has_single_use (rhs2))
	      break;
	    else
	      {
		def_stmt2 = SSA_NAME_DEF_STMT (rhs2);
		if (!is_gimple_assign (def_stmt2))
		  break;
		else if (!handled_load (def_stmt2, &ops[1], bitsize, bitpos,
					bitregion_start, bitregion_end))
		  break;
	      }
	    invalid = false;
	    break;
	  default:
	    invalid = true;
	    break;
	  }
    }

  struct imm_store_chain_info **chain_info = NULL;
  if (base_addr)
    chain_info = m_stores.get (base_addr);

  if (invalid)
    {
      terminate_all_aliasing_chains (chain_info, stmt);
      return;
    }

  store_immediate_info *info;
  if (chain_info)
    {
      unsigned int ord = (*chain_info)->m_store_info.length ();
      info = new store_immediate_info (bitsize, bitpos, bitregion_start,
				       bitregion_end, stmt, ord, rhs_code,
				       ops[0], ops[1]);
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Recording immediate store from stmt:\n");
	  print_gimple_stmt (dump_file, stmt, 0);
	}
      (*chain_info)->m_store_info.safe_push (info);
      /* If we reach the limit of stores to merge in a chain terminate and
	 process the chain now.  */
      if ((*chain_info)->m_store_info.length ()
	  == (unsigned int) PARAM_VALUE (PARAM_MAX_STORES_TO_MERGE))
	{
	  if (dump_file && (dump_flags & TDF_DETAILS))
	    fprintf (dump_file,
		     "Reached maximum number of statements to merge:\n");
	  terminate_and_release_chain (*chain_info);
	}
      return;
    }

  /* Store aliases any existing chain?  */
  terminate_all_aliasing_chains (chain_info, stmt);
  /* Start a new chain.  */
  struct imm_store_chain_info *new_chain
    = new imm_store_chain_info (m_stores_head, base_addr);
  info = new store_immediate_info (bitsize, bitpos, bitregion_start,
				   bitregion_end, stmt, 0, rhs_code,
				   ops[0], ops[1]);
  new_chain->m_store_info.safe_push (info);
  m_stores.put (base_addr, new_chain);
  if (dump_file && (dump_flags & TDF_DETAILS))
    {
      fprintf (dump_file, "Starting new chain with statement:\n");
      print_gimple_stmt (dump_file, stmt, 0);
      fprintf (dump_file, "The base object is:\n");
      print_generic_expr (dump_file, base_addr);
      fprintf (dump_file, "\n");
    }
}

/* Entry point for the pass.  Go over each basic block recording chains of
   immediate stores.  Upon encountering a terminating statement (as defined
   by stmt_terminates_chain_p) process the recorded stores and emit the widened
   variants.  */

unsigned int
pass_store_merging::execute (function *fun)
{
  basic_block bb;
  hash_set<gimple *> orig_stmts;

  FOR_EACH_BB_FN (bb, fun)
    {
      gimple_stmt_iterator gsi;
      unsigned HOST_WIDE_INT num_statements = 0;
      /* Record the original statements so that we can keep track of
	 statements emitted in this pass and not re-process new
	 statements.  */
      for (gsi = gsi_after_labels (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  if (is_gimple_debug (gsi_stmt (gsi)))
	    continue;

	  if (++num_statements >= 2)
	    break;
	}

      if (num_statements < 2)
	continue;

      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Processing basic block <%d>:\n", bb->index);

      for (gsi = gsi_after_labels (bb); !gsi_end_p (gsi); gsi_next (&gsi))
	{
	  gimple *stmt = gsi_stmt (gsi);

	  if (is_gimple_debug (stmt))
	    continue;

	  if (gimple_has_volatile_ops (stmt))
	    {
	      /* Terminate all chains.  */
	      if (dump_file && (dump_flags & TDF_DETAILS))
		fprintf (dump_file, "Volatile access terminates "
				    "all chains\n");
	      terminate_and_process_all_chains ();
	      continue;
	    }

	  if (gimple_assign_single_p (stmt) && gimple_vdef (stmt)
	      && !stmt_can_throw_internal (stmt)
	      && lhs_valid_for_store_merging_p (gimple_assign_lhs (stmt)))
	    process_store (stmt);
	  else
	    terminate_all_aliasing_chains (NULL, stmt);
	}
      terminate_and_process_all_chains ();
    }
  return 0;
}

} // anon namespace

/* Construct and return a store merging pass object.  */

gimple_opt_pass *
make_pass_store_merging (gcc::context *ctxt)
{
  return new pass_store_merging (ctxt);
}

#if CHECKING_P

namespace selftest {

/* Selftests for store merging helpers.  */

/* Assert that all elements of the byte arrays X and Y, both of length N
   are equal.  */

static void
verify_array_eq (unsigned char *x, unsigned char *y, unsigned int n)
{
  for (unsigned int i = 0; i < n; i++)
    {
      if (x[i] != y[i])
	{
	  fprintf (stderr, "Arrays do not match.  X:\n");
	  dump_char_array (stderr, x, n);
	  fprintf (stderr, "Y:\n");
	  dump_char_array (stderr, y, n);
	}
      ASSERT_EQ (x[i], y[i]);
    }
}

/* Test shift_bytes_in_array and that it carries bits across between
   bytes correctly.  */

static void
verify_shift_bytes_in_array (void)
{
   /* byte 1   | byte 0
      00011111 | 11100000.  */
  unsigned char orig[2] = { 0xe0, 0x1f };
  unsigned char in[2];
  memcpy (in, orig, sizeof orig);

  unsigned char expected[2] = { 0x80, 0x7f };
  shift_bytes_in_array (in, sizeof (in), 2);
  verify_array_eq (in, expected, sizeof (in));

  memcpy (in, orig, sizeof orig);
  memcpy (expected, orig, sizeof orig);
  /* Check that shifting by zero doesn't change anything.  */
  shift_bytes_in_array (in, sizeof (in), 0);
  verify_array_eq (in, expected, sizeof (in));

}

/* Test shift_bytes_in_array_right and that it carries bits across between
   bytes correctly.  */

static void
verify_shift_bytes_in_array_right (void)
{
   /* byte 1   | byte 0
      00011111 | 11100000.  */
  unsigned char orig[2] = { 0x1f, 0xe0};
  unsigned char in[2];
  memcpy (in, orig, sizeof orig);
  unsigned char expected[2] = { 0x07, 0xf8};
  shift_bytes_in_array_right (in, sizeof (in), 2);
  verify_array_eq (in, expected, sizeof (in));

  memcpy (in, orig, sizeof orig);
  memcpy (expected, orig, sizeof orig);
  /* Check that shifting by zero doesn't change anything.  */
  shift_bytes_in_array_right (in, sizeof (in), 0);
  verify_array_eq (in, expected, sizeof (in));
}

/* Test clear_bit_region that it clears exactly the bits asked and
   nothing more.  */

static void
verify_clear_bit_region (void)
{
  /* Start with all bits set and test clearing various patterns in them.  */
  unsigned char orig[3] = { 0xff, 0xff, 0xff};
  unsigned char in[3];
  unsigned char expected[3];
  memcpy (in, orig, sizeof in);

  /* Check zeroing out all the bits.  */
  clear_bit_region (in, 0, 3 * BITS_PER_UNIT);
  expected[0] = expected[1] = expected[2] = 0;
  verify_array_eq (in, expected, sizeof in);

  memcpy (in, orig, sizeof in);
  /* Leave the first and last bits intact.  */
  clear_bit_region (in, 1, 3 * BITS_PER_UNIT - 2);
  expected[0] = 0x1;
  expected[1] = 0;
  expected[2] = 0x80;
  verify_array_eq (in, expected, sizeof in);
}

/* Test verify_clear_bit_region_be that it clears exactly the bits asked and
   nothing more.  */

static void
verify_clear_bit_region_be (void)
{
  /* Start with all bits set and test clearing various patterns in them.  */
  unsigned char orig[3] = { 0xff, 0xff, 0xff};
  unsigned char in[3];
  unsigned char expected[3];
  memcpy (in, orig, sizeof in);

  /* Check zeroing out all the bits.  */
  clear_bit_region_be (in, BITS_PER_UNIT - 1, 3 * BITS_PER_UNIT);
  expected[0] = expected[1] = expected[2] = 0;
  verify_array_eq (in, expected, sizeof in);

  memcpy (in, orig, sizeof in);
  /* Leave the first and last bits intact.  */
  clear_bit_region_be (in, BITS_PER_UNIT - 2, 3 * BITS_PER_UNIT - 2);
  expected[0] = 0x80;
  expected[1] = 0;
  expected[2] = 0x1;
  verify_array_eq (in, expected, sizeof in);
}


/* Run all of the selftests within this file.  */

void
store_merging_c_tests (void)
{
  verify_shift_bytes_in_array ();
  verify_shift_bytes_in_array_right ();
  verify_clear_bit_region ();
  verify_clear_bit_region_be ();
}

} // namespace selftest
#endif /* CHECKING_P.  */
