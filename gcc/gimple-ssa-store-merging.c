/* GIMPLE store merging and byte swapping passes.
   Copyright (C) 2009-2018 Free Software Foundation, Inc.
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

/* The purpose of the store merging pass is to combine multiple memory stores
   of constant values, values loaded from memory, bitwise operations on those,
   or bit-field values, to consecutive locations, into fewer wider stores.

   For example, if we have a sequence peforming four byte stores to
   consecutive memory locations:
   [p     ] := imm1;
   [p + 1B] := imm2;
   [p + 2B] := imm3;
   [p + 3B] := imm4;
   we can transform this into a single 4-byte store if the target supports it:
   [p] := imm1:imm2:imm3:imm4 concatenated according to endianness.

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

   Or:
   [p:1 ] := imm;
   [p:31] := val & 0x7FFFFFFF;
   we can transform this into a single 4-byte store if the target supports it:
   [p] := imm:(val & 0x7FFFFFFF) concatenated according to endianness.

   The algorithm is applied to each basic block in three phases:

   1) Scan through the basic block and record assignments to destinations
   that can be expressed as a store to memory of a certain size at a certain
   bit offset from base expressions we can handle.  For bit-fields we also
   record the surrounding bit region, i.e. bits that could be stored in
   a read-modify-write operation when storing the bit-field.  Record store
   chains to different bases in a hash_map (m_stores) and make sure to
   terminate such chains when appropriate (for example when when the stored
   values get used subsequently).
   These stores can be a result of structure element initializers, array stores
   etc.  A store_immediate_info object is recorded for every such store.
   Record as many such assignments to a single base as possible until a
   statement that interferes with the store sequence is encountered.
   Each store has up to 2 operands, which can be a either constant, a memory
   load or an SSA name, from which the value to be stored can be computed.
   At most one of the operands can be a constant.  The operands are recorded
   in store_operand_info struct.

   2) Analyze the chains of stores recorded in phase 1) (i.e. the vector of
   store_immediate_info objects) and coalesce contiguous stores into
   merged_store_group objects.  For bit-field stores, we don't need to
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
   We try to emit as wide stores as we can while respecting STRICT_ALIGNMENT
   or TARGET_SLOW_UNALIGNED_ACCESS settings.

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
#include "gimple-fold.h"
#include "stor-layout.h"
#include "timevar.h"
#include "tree-cfg.h"
#include "tree-eh.h"
#include "target.h"
#include "gimplify-me.h"
#include "rtl.h"
#include "expr.h"	/* For get_bit_range.  */
#include "optabs-tree.h"
#include "selftest.h"

/* The maximum size (in bits) of the stores this pass should generate.  */
#define MAX_STORE_BITSIZE (BITS_PER_WORD)
#define MAX_STORE_BYTES (MAX_STORE_BITSIZE / BITS_PER_UNIT)

/* Limit to bound the number of aliasing checks for loads with the same
   vuse as the corresponding store.  */
#define MAX_STORE_ALIAS_CHECKS 64

namespace {

struct bswap_stat
{
  /* Number of hand-written 16-bit nop / bswaps found.  */
  int found_16bit;

  /* Number of hand-written 32-bit nop / bswaps found.  */
  int found_32bit;

  /* Number of hand-written 64-bit nop / bswaps found.  */
  int found_64bit;
} nop_stats, bswap_stats;

/* A symbolic number structure is used to detect byte permutation and selection
   patterns of a source.  To achieve that, its field N contains an artificial
   number consisting of BITS_PER_MARKER sized markers tracking where does each
   byte come from in the source:

   0	   - target byte has the value 0
   FF	   - target byte has an unknown value (eg. due to sign extension)
   1..size - marker value is the byte index in the source (0 for lsb).

   To detect permutations on memory sources (arrays and structures), a symbolic
   number is also associated:
   - a base address BASE_ADDR and an OFFSET giving the address of the source;
   - a range which gives the difference between the highest and lowest accessed
     memory location to make such a symbolic number;
   - the address SRC of the source element of lowest address as a convenience
     to easily get BASE_ADDR + offset + lowest bytepos;
   - number of expressions N_OPS bitwise ored together to represent
     approximate cost of the computation.

   Note 1: the range is different from size as size reflects the size of the
   type of the current expression.  For instance, for an array char a[],
   (short) a[0] | (short) a[3] would have a size of 2 but a range of 4 while
   (short) a[0] | ((short) a[0] << 1) would still have a size of 2 but this
   time a range of 1.

   Note 2: for non-memory sources, range holds the same value as size.

   Note 3: SRC points to the SSA_NAME in case of non-memory source.  */

struct symbolic_number {
  uint64_t n;
  tree type;
  tree base_addr;
  tree offset;
  poly_int64_pod bytepos;
  tree src;
  tree alias_set;
  tree vuse;
  unsigned HOST_WIDE_INT range;
  int n_ops;
};

#define BITS_PER_MARKER 8
#define MARKER_MASK ((1 << BITS_PER_MARKER) - 1)
#define MARKER_BYTE_UNKNOWN MARKER_MASK
#define HEAD_MARKER(n, size) \
  ((n) & ((uint64_t) MARKER_MASK << (((size) - 1) * BITS_PER_MARKER)))

/* The number which the find_bswap_or_nop_1 result should match in
   order to have a nop.  The number is masked according to the size of
   the symbolic number before using it.  */
#define CMPNOP (sizeof (int64_t) < 8 ? 0 : \
  (uint64_t)0x08070605 << 32 | 0x04030201)

/* The number which the find_bswap_or_nop_1 result should match in
   order to have a byte swap.  The number is masked according to the
   size of the symbolic number before using it.  */
#define CMPXCHG (sizeof (int64_t) < 8 ? 0 : \
  (uint64_t)0x01020304 << 32 | 0x05060708)

/* Perform a SHIFT or ROTATE operation by COUNT bits on symbolic
   number N.  Return false if the requested operation is not permitted
   on a symbolic number.  */

inline bool
do_shift_rotate (enum tree_code code,
		 struct symbolic_number *n,
		 int count)
{
  int i, size = TYPE_PRECISION (n->type) / BITS_PER_UNIT;
  unsigned head_marker;

  if (count % BITS_PER_UNIT != 0)
    return false;
  count = (count / BITS_PER_UNIT) * BITS_PER_MARKER;

  /* Zero out the extra bits of N in order to avoid them being shifted
     into the significant bits.  */
  if (size < 64 / BITS_PER_MARKER)
    n->n &= ((uint64_t) 1 << (size * BITS_PER_MARKER)) - 1;

  switch (code)
    {
    case LSHIFT_EXPR:
      n->n <<= count;
      break;
    case RSHIFT_EXPR:
      head_marker = HEAD_MARKER (n->n, size);
      n->n >>= count;
      /* Arithmetic shift of signed type: result is dependent on the value.  */
      if (!TYPE_UNSIGNED (n->type) && head_marker)
	for (i = 0; i < count / BITS_PER_MARKER; i++)
	  n->n |= (uint64_t) MARKER_BYTE_UNKNOWN
		  << ((size - 1 - i) * BITS_PER_MARKER);
      break;
    case LROTATE_EXPR:
      n->n = (n->n << count) | (n->n >> ((size * BITS_PER_MARKER) - count));
      break;
    case RROTATE_EXPR:
      n->n = (n->n >> count) | (n->n << ((size * BITS_PER_MARKER) - count));
      break;
    default:
      return false;
    }
  /* Zero unused bits for size.  */
  if (size < 64 / BITS_PER_MARKER)
    n->n &= ((uint64_t) 1 << (size * BITS_PER_MARKER)) - 1;
  return true;
}

/* Perform sanity checking for the symbolic number N and the gimple
   statement STMT.  */

inline bool
verify_symbolic_number_p (struct symbolic_number *n, gimple *stmt)
{
  tree lhs_type;

  lhs_type = gimple_expr_type (stmt);

  if (TREE_CODE (lhs_type) != INTEGER_TYPE)
    return false;

  if (TYPE_PRECISION (lhs_type) != TYPE_PRECISION (n->type))
    return false;

  return true;
}

/* Initialize the symbolic number N for the bswap pass from the base element
   SRC manipulated by the bitwise OR expression.  */

bool
init_symbolic_number (struct symbolic_number *n, tree src)
{
  int size;

  if (! INTEGRAL_TYPE_P (TREE_TYPE (src)))
    return false;

  n->base_addr = n->offset = n->alias_set = n->vuse = NULL_TREE;
  n->src = src;

  /* Set up the symbolic number N by setting each byte to a value between 1 and
     the byte size of rhs1.  The highest order byte is set to n->size and the
     lowest order byte to 1.  */
  n->type = TREE_TYPE (src);
  size = TYPE_PRECISION (n->type);
  if (size % BITS_PER_UNIT != 0)
    return false;
  size /= BITS_PER_UNIT;
  if (size > 64 / BITS_PER_MARKER)
    return false;
  n->range = size;
  n->n = CMPNOP;
  n->n_ops = 1;

  if (size < 64 / BITS_PER_MARKER)
    n->n &= ((uint64_t) 1 << (size * BITS_PER_MARKER)) - 1;

  return true;
}

/* Check if STMT might be a byte swap or a nop from a memory source and returns
   the answer. If so, REF is that memory source and the base of the memory area
   accessed and the offset of the access from that base are recorded in N.  */

bool
find_bswap_or_nop_load (gimple *stmt, tree ref, struct symbolic_number *n)
{
  /* Leaf node is an array or component ref. Memorize its base and
     offset from base to compare to other such leaf node.  */
  poly_int64 bitsize, bitpos, bytepos;
  machine_mode mode;
  int unsignedp, reversep, volatilep;
  tree offset, base_addr;

  /* Not prepared to handle PDP endian.  */
  if (BYTES_BIG_ENDIAN != WORDS_BIG_ENDIAN)
    return false;

  if (!gimple_assign_load_p (stmt) || gimple_has_volatile_ops (stmt))
    return false;

  base_addr = get_inner_reference (ref, &bitsize, &bitpos, &offset, &mode,
				   &unsignedp, &reversep, &volatilep);

  if (TREE_CODE (base_addr) == TARGET_MEM_REF)
    /* Do not rewrite TARGET_MEM_REF.  */
    return false;
  else if (TREE_CODE (base_addr) == MEM_REF)
    {
      poly_offset_int bit_offset = 0;
      tree off = TREE_OPERAND (base_addr, 1);

      if (!integer_zerop (off))
	{
	  poly_offset_int boff = mem_ref_offset (base_addr);
	  boff <<= LOG2_BITS_PER_UNIT;
	  bit_offset += boff;
	}

      base_addr = TREE_OPERAND (base_addr, 0);

      /* Avoid returning a negative bitpos as this may wreak havoc later.  */
      if (maybe_lt (bit_offset, 0))
	{
	  tree byte_offset = wide_int_to_tree
	    (sizetype, bits_to_bytes_round_down (bit_offset));
	  bit_offset = num_trailing_bits (bit_offset);
	  if (offset)
	    offset = size_binop (PLUS_EXPR, offset, byte_offset);
	  else
	    offset = byte_offset;
	}

      bitpos += bit_offset.force_shwi ();
    }
  else
    base_addr = build_fold_addr_expr (base_addr);

  if (!multiple_p (bitpos, BITS_PER_UNIT, &bytepos))
    return false;
  if (!multiple_p (bitsize, BITS_PER_UNIT))
    return false;
  if (reversep)
    return false;

  if (!init_symbolic_number (n, ref))
    return false;
  n->base_addr = base_addr;
  n->offset = offset;
  n->bytepos = bytepos;
  n->alias_set = reference_alias_ptr_type (ref);
  n->vuse = gimple_vuse (stmt);
  return true;
}

/* Compute the symbolic number N representing the result of a bitwise OR on 2
   symbolic number N1 and N2 whose source statements are respectively
   SOURCE_STMT1 and SOURCE_STMT2.  */

gimple *
perform_symbolic_merge (gimple *source_stmt1, struct symbolic_number *n1,
			gimple *source_stmt2, struct symbolic_number *n2,
			struct symbolic_number *n)
{
  int i, size;
  uint64_t mask;
  gimple *source_stmt;
  struct symbolic_number *n_start;

  tree rhs1 = gimple_assign_rhs1 (source_stmt1);
  if (TREE_CODE (rhs1) == BIT_FIELD_REF
      && TREE_CODE (TREE_OPERAND (rhs1, 0)) == SSA_NAME)
    rhs1 = TREE_OPERAND (rhs1, 0);
  tree rhs2 = gimple_assign_rhs1 (source_stmt2);
  if (TREE_CODE (rhs2) == BIT_FIELD_REF
      && TREE_CODE (TREE_OPERAND (rhs2, 0)) == SSA_NAME)
    rhs2 = TREE_OPERAND (rhs2, 0);

  /* Sources are different, cancel bswap if they are not memory location with
     the same base (array, structure, ...).  */
  if (rhs1 != rhs2)
    {
      uint64_t inc;
      HOST_WIDE_INT start1, start2, start_sub, end_sub, end1, end2, end;
      struct symbolic_number *toinc_n_ptr, *n_end;
      basic_block bb1, bb2;

      if (!n1->base_addr || !n2->base_addr
	  || !operand_equal_p (n1->base_addr, n2->base_addr, 0))
	return NULL;

      if (!n1->offset != !n2->offset
	  || (n1->offset && !operand_equal_p (n1->offset, n2->offset, 0)))
	return NULL;

      start1 = 0;
      if (!(n2->bytepos - n1->bytepos).is_constant (&start2))
	return NULL;

      if (start1 < start2)
	{
	  n_start = n1;
	  start_sub = start2 - start1;
	}
      else
	{
	  n_start = n2;
	  start_sub = start1 - start2;
	}

      bb1 = gimple_bb (source_stmt1);
      bb2 = gimple_bb (source_stmt2);
      if (dominated_by_p (CDI_DOMINATORS, bb1, bb2))
	source_stmt = source_stmt1;
      else
	source_stmt = source_stmt2;

      /* Find the highest address at which a load is performed and
	 compute related info.  */
      end1 = start1 + (n1->range - 1);
      end2 = start2 + (n2->range - 1);
      if (end1 < end2)
	{
	  end = end2;
	  end_sub = end2 - end1;
	}
      else
	{
	  end = end1;
	  end_sub = end1 - end2;
	}
      n_end = (end2 > end1) ? n2 : n1;

      /* Find symbolic number whose lsb is the most significant.  */
      if (BYTES_BIG_ENDIAN)
	toinc_n_ptr = (n_end == n1) ? n2 : n1;
      else
	toinc_n_ptr = (n_start == n1) ? n2 : n1;

      n->range = end - MIN (start1, start2) + 1;

      /* Check that the range of memory covered can be represented by
	 a symbolic number.  */
      if (n->range > 64 / BITS_PER_MARKER)
	return NULL;

      /* Reinterpret byte marks in symbolic number holding the value of
	 bigger weight according to target endianness.  */
      inc = BYTES_BIG_ENDIAN ? end_sub : start_sub;
      size = TYPE_PRECISION (n1->type) / BITS_PER_UNIT;
      for (i = 0; i < size; i++, inc <<= BITS_PER_MARKER)
	{
	  unsigned marker
	    = (toinc_n_ptr->n >> (i * BITS_PER_MARKER)) & MARKER_MASK;
	  if (marker && marker != MARKER_BYTE_UNKNOWN)
	    toinc_n_ptr->n += inc;
	}
    }
  else
    {
      n->range = n1->range;
      n_start = n1;
      source_stmt = source_stmt1;
    }

  if (!n1->alias_set
      || alias_ptr_types_compatible_p (n1->alias_set, n2->alias_set))
    n->alias_set = n1->alias_set;
  else
    n->alias_set = ptr_type_node;
  n->vuse = n_start->vuse;
  n->base_addr = n_start->base_addr;
  n->offset = n_start->offset;
  n->src = n_start->src;
  n->bytepos = n_start->bytepos;
  n->type = n_start->type;
  size = TYPE_PRECISION (n->type) / BITS_PER_UNIT;

  for (i = 0, mask = MARKER_MASK; i < size; i++, mask <<= BITS_PER_MARKER)
    {
      uint64_t masked1, masked2;

      masked1 = n1->n & mask;
      masked2 = n2->n & mask;
      if (masked1 && masked2 && masked1 != masked2)
	return NULL;
    }
  n->n = n1->n | n2->n;
  n->n_ops = n1->n_ops + n2->n_ops;

  return source_stmt;
}

/* find_bswap_or_nop_1 invokes itself recursively with N and tries to perform
   the operation given by the rhs of STMT on the result.  If the operation
   could successfully be executed the function returns a gimple stmt whose
   rhs's first tree is the expression of the source operand and NULL
   otherwise.  */

gimple *
find_bswap_or_nop_1 (gimple *stmt, struct symbolic_number *n, int limit)
{
  enum tree_code code;
  tree rhs1, rhs2 = NULL;
  gimple *rhs1_stmt, *rhs2_stmt, *source_stmt1;
  enum gimple_rhs_class rhs_class;

  if (!limit || !is_gimple_assign (stmt))
    return NULL;

  rhs1 = gimple_assign_rhs1 (stmt);

  if (find_bswap_or_nop_load (stmt, rhs1, n))
    return stmt;

  /* Handle BIT_FIELD_REF.  */
  if (TREE_CODE (rhs1) == BIT_FIELD_REF
      && TREE_CODE (TREE_OPERAND (rhs1, 0)) == SSA_NAME)
    {
      unsigned HOST_WIDE_INT bitsize = tree_to_uhwi (TREE_OPERAND (rhs1, 1));
      unsigned HOST_WIDE_INT bitpos = tree_to_uhwi (TREE_OPERAND (rhs1, 2));
      if (bitpos % BITS_PER_UNIT == 0
	  && bitsize % BITS_PER_UNIT == 0
	  && init_symbolic_number (n, TREE_OPERAND (rhs1, 0)))
	{
	  /* Handle big-endian bit numbering in BIT_FIELD_REF.  */
	  if (BYTES_BIG_ENDIAN)
	    bitpos = TYPE_PRECISION (n->type) - bitpos - bitsize;

	  /* Shift.  */
	  if (!do_shift_rotate (RSHIFT_EXPR, n, bitpos))
	    return NULL;

	  /* Mask.  */
	  uint64_t mask = 0;
	  uint64_t tmp = (1 << BITS_PER_UNIT) - 1;
	  for (unsigned i = 0; i < bitsize / BITS_PER_UNIT;
	       i++, tmp <<= BITS_PER_UNIT)
	    mask |= (uint64_t) MARKER_MASK << (i * BITS_PER_MARKER);
	  n->n &= mask;

	  /* Convert.  */
	  n->type = TREE_TYPE (rhs1);
	  if (!n->base_addr)
	    n->range = TYPE_PRECISION (n->type) / BITS_PER_UNIT;

	  return verify_symbolic_number_p (n, stmt) ? stmt : NULL;
	}

      return NULL;
    }

  if (TREE_CODE (rhs1) != SSA_NAME)
    return NULL;

  code = gimple_assign_rhs_code (stmt);
  rhs_class = gimple_assign_rhs_class (stmt);
  rhs1_stmt = SSA_NAME_DEF_STMT (rhs1);

  if (rhs_class == GIMPLE_BINARY_RHS)
    rhs2 = gimple_assign_rhs2 (stmt);

  /* Handle unary rhs and binary rhs with integer constants as second
     operand.  */

  if (rhs_class == GIMPLE_UNARY_RHS
      || (rhs_class == GIMPLE_BINARY_RHS
	  && TREE_CODE (rhs2) == INTEGER_CST))
    {
      if (code != BIT_AND_EXPR
	  && code != LSHIFT_EXPR
	  && code != RSHIFT_EXPR
	  && code != LROTATE_EXPR
	  && code != RROTATE_EXPR
	  && !CONVERT_EXPR_CODE_P (code))
	return NULL;

      source_stmt1 = find_bswap_or_nop_1 (rhs1_stmt, n, limit - 1);

      /* If find_bswap_or_nop_1 returned NULL, STMT is a leaf node and
	 we have to initialize the symbolic number.  */
      if (!source_stmt1)
	{
	  if (gimple_assign_load_p (stmt)
	      || !init_symbolic_number (n, rhs1))
	    return NULL;
	  source_stmt1 = stmt;
	}

      switch (code)
	{
	case BIT_AND_EXPR:
	  {
	    int i, size = TYPE_PRECISION (n->type) / BITS_PER_UNIT;
	    uint64_t val = int_cst_value (rhs2), mask = 0;
	    uint64_t tmp = (1 << BITS_PER_UNIT) - 1;

	    /* Only constants masking full bytes are allowed.  */
	    for (i = 0; i < size; i++, tmp <<= BITS_PER_UNIT)
	      if ((val & tmp) != 0 && (val & tmp) != tmp)
		return NULL;
	      else if (val & tmp)
		mask |= (uint64_t) MARKER_MASK << (i * BITS_PER_MARKER);

	    n->n &= mask;
	  }
	  break;
	case LSHIFT_EXPR:
	case RSHIFT_EXPR:
	case LROTATE_EXPR:
	case RROTATE_EXPR:
	  if (!do_shift_rotate (code, n, (int) TREE_INT_CST_LOW (rhs2)))
	    return NULL;
	  break;
	CASE_CONVERT:
	  {
	    int i, type_size, old_type_size;
	    tree type;

	    type = gimple_expr_type (stmt);
	    type_size = TYPE_PRECISION (type);
	    if (type_size % BITS_PER_UNIT != 0)
	      return NULL;
	    type_size /= BITS_PER_UNIT;
	    if (type_size > 64 / BITS_PER_MARKER)
	      return NULL;

	    /* Sign extension: result is dependent on the value.  */
	    old_type_size = TYPE_PRECISION (n->type) / BITS_PER_UNIT;
	    if (!TYPE_UNSIGNED (n->type) && type_size > old_type_size
		&& HEAD_MARKER (n->n, old_type_size))
	      for (i = 0; i < type_size - old_type_size; i++)
		n->n |= (uint64_t) MARKER_BYTE_UNKNOWN
			<< ((type_size - 1 - i) * BITS_PER_MARKER);

	    if (type_size < 64 / BITS_PER_MARKER)
	      {
		/* If STMT casts to a smaller type mask out the bits not
		   belonging to the target type.  */
		n->n &= ((uint64_t) 1 << (type_size * BITS_PER_MARKER)) - 1;
	      }
	    n->type = type;
	    if (!n->base_addr)
	      n->range = type_size;
	  }
	  break;
	default:
	  return NULL;
	};
      return verify_symbolic_number_p (n, stmt) ? source_stmt1 : NULL;
    }

  /* Handle binary rhs.  */

  if (rhs_class == GIMPLE_BINARY_RHS)
    {
      struct symbolic_number n1, n2;
      gimple *source_stmt, *source_stmt2;

      if (code != BIT_IOR_EXPR)
	return NULL;

      if (TREE_CODE (rhs2) != SSA_NAME)
	return NULL;

      rhs2_stmt = SSA_NAME_DEF_STMT (rhs2);

      switch (code)
	{
	case BIT_IOR_EXPR:
	  source_stmt1 = find_bswap_or_nop_1 (rhs1_stmt, &n1, limit - 1);

	  if (!source_stmt1)
	    return NULL;

	  source_stmt2 = find_bswap_or_nop_1 (rhs2_stmt, &n2, limit - 1);

	  if (!source_stmt2)
	    return NULL;

	  if (TYPE_PRECISION (n1.type) != TYPE_PRECISION (n2.type))
	    return NULL;

	  if (n1.vuse != n2.vuse)
	    return NULL;

	  source_stmt
	    = perform_symbolic_merge (source_stmt1, &n1, source_stmt2, &n2, n);

	  if (!source_stmt)
	    return NULL;

	  if (!verify_symbolic_number_p (n, stmt))
	    return NULL;

	  break;
	default:
	  return NULL;
	}
      return source_stmt;
    }
  return NULL;
}

/* Helper for find_bswap_or_nop and try_coalesce_bswap to compute
   *CMPXCHG, *CMPNOP and adjust *N.  */

void
find_bswap_or_nop_finalize (struct symbolic_number *n, uint64_t *cmpxchg,
			    uint64_t *cmpnop)
{
  unsigned rsize;
  uint64_t tmpn, mask;

  /* The number which the find_bswap_or_nop_1 result should match in order
     to have a full byte swap.  The number is shifted to the right
     according to the size of the symbolic number before using it.  */
  *cmpxchg = CMPXCHG;
  *cmpnop = CMPNOP;

  /* Find real size of result (highest non-zero byte).  */
  if (n->base_addr)
    for (tmpn = n->n, rsize = 0; tmpn; tmpn >>= BITS_PER_MARKER, rsize++);
  else
    rsize = n->range;

  /* Zero out the bits corresponding to untouched bytes in original gimple
     expression.  */
  if (n->range < (int) sizeof (int64_t))
    {
      mask = ((uint64_t) 1 << (n->range * BITS_PER_MARKER)) - 1;
      *cmpxchg >>= (64 / BITS_PER_MARKER - n->range) * BITS_PER_MARKER;
      *cmpnop &= mask;
    }

  /* Zero out the bits corresponding to unused bytes in the result of the
     gimple expression.  */
  if (rsize < n->range)
    {
      if (BYTES_BIG_ENDIAN)
	{
	  mask = ((uint64_t) 1 << (rsize * BITS_PER_MARKER)) - 1;
	  *cmpxchg &= mask;
	  *cmpnop >>= (n->range - rsize) * BITS_PER_MARKER;
	}
      else
	{
	  mask = ((uint64_t) 1 << (rsize * BITS_PER_MARKER)) - 1;
	  *cmpxchg >>= (n->range - rsize) * BITS_PER_MARKER;
	  *cmpnop &= mask;
	}
      n->range = rsize;
    }

  n->range *= BITS_PER_UNIT;
}

/* Check if STMT completes a bswap implementation or a read in a given
   endianness consisting of ORs, SHIFTs and ANDs and sets *BSWAP
   accordingly.  It also sets N to represent the kind of operations
   performed: size of the resulting expression and whether it works on
   a memory source, and if so alias-set and vuse.  At last, the
   function returns a stmt whose rhs's first tree is the source
   expression.  */

gimple *
find_bswap_or_nop (gimple *stmt, struct symbolic_number *n, bool *bswap)
{
  /* The last parameter determines the depth search limit.  It usually
     correlates directly to the number n of bytes to be touched.  We
     increase that number by log2(n) + 1 here in order to also
     cover signed -> unsigned conversions of the src operand as can be seen
     in libgcc, and for initial shift/and operation of the src operand.  */
  int limit = TREE_INT_CST_LOW (TYPE_SIZE_UNIT (gimple_expr_type (stmt)));
  limit += 1 + (int) ceil_log2 ((unsigned HOST_WIDE_INT) limit);
  gimple *ins_stmt = find_bswap_or_nop_1 (stmt, n, limit);

  if (!ins_stmt)
    return NULL;

  uint64_t cmpxchg, cmpnop;
  find_bswap_or_nop_finalize (n, &cmpxchg, &cmpnop);

  /* A complete byte swap should make the symbolic number to start with
     the largest digit in the highest order byte. Unchanged symbolic
     number indicates a read with same endianness as target architecture.  */
  if (n->n == cmpnop)
    *bswap = false;
  else if (n->n == cmpxchg)
    *bswap = true;
  else
    return NULL;

  /* Useless bit manipulation performed by code.  */
  if (!n->base_addr && n->n == cmpnop && n->n_ops == 1)
    return NULL;

  return ins_stmt;
}

const pass_data pass_data_optimize_bswap =
{
  GIMPLE_PASS, /* type */
  "bswap", /* name */
  OPTGROUP_NONE, /* optinfo_flags */
  TV_NONE, /* tv_id */
  PROP_ssa, /* properties_required */
  0, /* properties_provided */
  0, /* properties_destroyed */
  0, /* todo_flags_start */
  0, /* todo_flags_finish */
};

class pass_optimize_bswap : public gimple_opt_pass
{
public:
  pass_optimize_bswap (gcc::context *ctxt)
    : gimple_opt_pass (pass_data_optimize_bswap, ctxt)
  {}

  /* opt_pass methods: */
  virtual bool gate (function *)
    {
      return flag_expensive_optimizations && optimize && BITS_PER_UNIT == 8;
    }

  virtual unsigned int execute (function *);

}; // class pass_optimize_bswap

/* Perform the bswap optimization: replace the expression computed in the rhs
   of gsi_stmt (GSI) (or if NULL add instead of replace) by an equivalent
   bswap, load or load + bswap expression.
   Which of these alternatives replace the rhs is given by N->base_addr (non
   null if a load is needed) and BSWAP.  The type, VUSE and set-alias of the
   load to perform are also given in N while the builtin bswap invoke is given
   in FNDEL.  Finally, if a load is involved, INS_STMT refers to one of the
   load statements involved to construct the rhs in gsi_stmt (GSI) and
   N->range gives the size of the rhs expression for maintaining some
   statistics.

   Note that if the replacement involve a load and if gsi_stmt (GSI) is
   non-NULL, that stmt is moved just after INS_STMT to do the load with the
   same VUSE which can lead to gsi_stmt (GSI) changing of basic block.  */

tree
bswap_replace (gimple_stmt_iterator gsi, gimple *ins_stmt, tree fndecl,
	       tree bswap_type, tree load_type, struct symbolic_number *n,
	       bool bswap)
{
  tree src, tmp, tgt = NULL_TREE;
  gimple *bswap_stmt;

  gimple *cur_stmt = gsi_stmt (gsi);
  src = n->src;
  if (cur_stmt)
    tgt = gimple_assign_lhs (cur_stmt);

  /* Need to load the value from memory first.  */
  if (n->base_addr)
    {
      gimple_stmt_iterator gsi_ins = gsi;
      if (ins_stmt)
	gsi_ins = gsi_for_stmt (ins_stmt);
      tree addr_expr, addr_tmp, val_expr, val_tmp;
      tree load_offset_ptr, aligned_load_type;
      gimple *load_stmt;
      unsigned align = get_object_alignment (src);
      poly_int64 load_offset = 0;

      if (cur_stmt)
	{
	  basic_block ins_bb = gimple_bb (ins_stmt);
	  basic_block cur_bb = gimple_bb (cur_stmt);
	  if (!dominated_by_p (CDI_DOMINATORS, cur_bb, ins_bb))
	    return NULL_TREE;

	  /* Move cur_stmt just before one of the load of the original
	     to ensure it has the same VUSE.  See PR61517 for what could
	     go wrong.  */
	  if (gimple_bb (cur_stmt) != gimple_bb (ins_stmt))
	    reset_flow_sensitive_info (gimple_assign_lhs (cur_stmt));
	  gsi_move_before (&gsi, &gsi_ins);
	  gsi = gsi_for_stmt (cur_stmt);
	}
      else
	gsi = gsi_ins;

      /* Compute address to load from and cast according to the size
	 of the load.  */
      addr_expr = build_fold_addr_expr (src);
      if (is_gimple_mem_ref_addr (addr_expr))
	addr_tmp = unshare_expr (addr_expr);
      else
	{
	  addr_tmp = unshare_expr (n->base_addr);
	  if (!is_gimple_mem_ref_addr (addr_tmp))
	    addr_tmp = force_gimple_operand_gsi_1 (&gsi, addr_tmp,
						   is_gimple_mem_ref_addr,
						   NULL_TREE, true,
						   GSI_SAME_STMT);
	  load_offset = n->bytepos;
	  if (n->offset)
	    {
	      tree off
		= force_gimple_operand_gsi (&gsi, unshare_expr (n->offset),
					    true, NULL_TREE, true,
					    GSI_SAME_STMT);
	      gimple *stmt
		= gimple_build_assign (make_ssa_name (TREE_TYPE (addr_tmp)),
				       POINTER_PLUS_EXPR, addr_tmp, off);
	      gsi_insert_before (&gsi, stmt, GSI_SAME_STMT);
	      addr_tmp = gimple_assign_lhs (stmt);
	    }
	}

      /* Perform the load.  */
      aligned_load_type = load_type;
      if (align < TYPE_ALIGN (load_type))
	aligned_load_type = build_aligned_type (load_type, align);
      load_offset_ptr = build_int_cst (n->alias_set, load_offset);
      val_expr = fold_build2 (MEM_REF, aligned_load_type, addr_tmp,
			      load_offset_ptr);

      if (!bswap)
	{
	  if (n->range == 16)
	    nop_stats.found_16bit++;
	  else if (n->range == 32)
	    nop_stats.found_32bit++;
	  else
	    {
	      gcc_assert (n->range == 64);
	      nop_stats.found_64bit++;
	    }

	  /* Convert the result of load if necessary.  */
	  if (tgt && !useless_type_conversion_p (TREE_TYPE (tgt), load_type))
	    {
	      val_tmp = make_temp_ssa_name (aligned_load_type, NULL,
					    "load_dst");
	      load_stmt = gimple_build_assign (val_tmp, val_expr);
	      gimple_set_vuse (load_stmt, n->vuse);
	      gsi_insert_before (&gsi, load_stmt, GSI_SAME_STMT);
	      gimple_assign_set_rhs_with_ops (&gsi, NOP_EXPR, val_tmp);
	      update_stmt (cur_stmt);
	    }
	  else if (cur_stmt)
	    {
	      gimple_assign_set_rhs_with_ops (&gsi, MEM_REF, val_expr);
	      gimple_set_vuse (cur_stmt, n->vuse);
	      update_stmt (cur_stmt);
	    }
	  else
	    {
	      tgt = make_ssa_name (load_type);
	      cur_stmt = gimple_build_assign (tgt, MEM_REF, val_expr);
	      gimple_set_vuse (cur_stmt, n->vuse);
	      gsi_insert_before (&gsi, cur_stmt, GSI_SAME_STMT);
	    }

	  if (dump_file)
	    {
	      fprintf (dump_file,
		       "%d bit load in target endianness found at: ",
		       (int) n->range);
	      print_gimple_stmt (dump_file, cur_stmt, 0);
	    }
	  return tgt;
	}
      else
	{
	  val_tmp = make_temp_ssa_name (aligned_load_type, NULL, "load_dst");
	  load_stmt = gimple_build_assign (val_tmp, val_expr);
	  gimple_set_vuse (load_stmt, n->vuse);
	  gsi_insert_before (&gsi, load_stmt, GSI_SAME_STMT);
	}
      src = val_tmp;
    }
  else if (!bswap)
    {
      gimple *g = NULL;
      if (tgt && !useless_type_conversion_p (TREE_TYPE (tgt), TREE_TYPE (src)))
	{
	  if (!is_gimple_val (src))
	    return NULL_TREE;
	  g = gimple_build_assign (tgt, NOP_EXPR, src);
	}
      else if (cur_stmt)
	g = gimple_build_assign (tgt, src);
      else
	tgt = src;
      if (n->range == 16)
	nop_stats.found_16bit++;
      else if (n->range == 32)
	nop_stats.found_32bit++;
      else
	{
	  gcc_assert (n->range == 64);
	  nop_stats.found_64bit++;
	}
      if (dump_file)
	{
	  fprintf (dump_file,
		   "%d bit reshuffle in target endianness found at: ",
		   (int) n->range);
	  if (cur_stmt)
	    print_gimple_stmt (dump_file, cur_stmt, 0);
	  else
	    {
	      print_generic_expr (dump_file, tgt, TDF_NONE);
	      fprintf (dump_file, "\n");
	    }
	}
      if (cur_stmt)
	gsi_replace (&gsi, g, true);
      return tgt;
    }
  else if (TREE_CODE (src) == BIT_FIELD_REF)
    src = TREE_OPERAND (src, 0);

  if (n->range == 16)
    bswap_stats.found_16bit++;
  else if (n->range == 32)
    bswap_stats.found_32bit++;
  else
    {
      gcc_assert (n->range == 64);
      bswap_stats.found_64bit++;
    }

  tmp = src;

  /* Convert the src expression if necessary.  */
  if (!useless_type_conversion_p (TREE_TYPE (tmp), bswap_type))
    {
      gimple *convert_stmt;

      tmp = make_temp_ssa_name (bswap_type, NULL, "bswapsrc");
      convert_stmt = gimple_build_assign (tmp, NOP_EXPR, src);
      gsi_insert_before (&gsi, convert_stmt, GSI_SAME_STMT);
    }

  /* Canonical form for 16 bit bswap is a rotate expression.  Only 16bit values
     are considered as rotation of 2N bit values by N bits is generally not
     equivalent to a bswap.  Consider for instance 0x01020304 r>> 16 which
     gives 0x03040102 while a bswap for that value is 0x04030201.  */
  if (bswap && n->range == 16)
    {
      tree count = build_int_cst (NULL, BITS_PER_UNIT);
      src = fold_build2 (LROTATE_EXPR, bswap_type, tmp, count);
      bswap_stmt = gimple_build_assign (NULL, src);
    }
  else
    bswap_stmt = gimple_build_call (fndecl, 1, tmp);

  if (tgt == NULL_TREE)
    tgt = make_ssa_name (bswap_type);
  tmp = tgt;

  /* Convert the result if necessary.  */
  if (!useless_type_conversion_p (TREE_TYPE (tgt), bswap_type))
    {
      gimple *convert_stmt;

      tmp = make_temp_ssa_name (bswap_type, NULL, "bswapdst");
      convert_stmt = gimple_build_assign (tgt, NOP_EXPR, tmp);
      gsi_insert_after (&gsi, convert_stmt, GSI_SAME_STMT);
    }

  gimple_set_lhs (bswap_stmt, tmp);

  if (dump_file)
    {
      fprintf (dump_file, "%d bit bswap implementation found at: ",
	       (int) n->range);
      if (cur_stmt)
	print_gimple_stmt (dump_file, cur_stmt, 0);
      else
	{
	  print_generic_expr (dump_file, tgt, TDF_NONE);
	  fprintf (dump_file, "\n");
	}
    }

  if (cur_stmt)
    {
      gsi_insert_after (&gsi, bswap_stmt, GSI_SAME_STMT);
      gsi_remove (&gsi, true);
    }
  else
    gsi_insert_before (&gsi, bswap_stmt, GSI_SAME_STMT);
  return tgt;
}

/* Find manual byte swap implementations as well as load in a given
   endianness. Byte swaps are turned into a bswap builtin invokation
   while endian loads are converted to bswap builtin invokation or
   simple load according to the target endianness.  */

unsigned int
pass_optimize_bswap::execute (function *fun)
{
  basic_block bb;
  bool bswap32_p, bswap64_p;
  bool changed = false;
  tree bswap32_type = NULL_TREE, bswap64_type = NULL_TREE;

  bswap32_p = (builtin_decl_explicit_p (BUILT_IN_BSWAP32)
	       && optab_handler (bswap_optab, SImode) != CODE_FOR_nothing);
  bswap64_p = (builtin_decl_explicit_p (BUILT_IN_BSWAP64)
	       && (optab_handler (bswap_optab, DImode) != CODE_FOR_nothing
		   || (bswap32_p && word_mode == SImode)));

  /* Determine the argument type of the builtins.  The code later on
     assumes that the return and argument type are the same.  */
  if (bswap32_p)
    {
      tree fndecl = builtin_decl_explicit (BUILT_IN_BSWAP32);
      bswap32_type = TREE_VALUE (TYPE_ARG_TYPES (TREE_TYPE (fndecl)));
    }

  if (bswap64_p)
    {
      tree fndecl = builtin_decl_explicit (BUILT_IN_BSWAP64);
      bswap64_type = TREE_VALUE (TYPE_ARG_TYPES (TREE_TYPE (fndecl)));
    }

  memset (&nop_stats, 0, sizeof (nop_stats));
  memset (&bswap_stats, 0, sizeof (bswap_stats));
  calculate_dominance_info (CDI_DOMINATORS);

  FOR_EACH_BB_FN (bb, fun)
    {
      gimple_stmt_iterator gsi;

      /* We do a reverse scan for bswap patterns to make sure we get the
	 widest match. As bswap pattern matching doesn't handle previously
	 inserted smaller bswap replacements as sub-patterns, the wider
	 variant wouldn't be detected.  */
      for (gsi = gsi_last_bb (bb); !gsi_end_p (gsi);)
	{
	  gimple *ins_stmt, *cur_stmt = gsi_stmt (gsi);
	  tree fndecl = NULL_TREE, bswap_type = NULL_TREE, load_type;
	  enum tree_code code;
	  struct symbolic_number n;
	  bool bswap;

	  /* This gsi_prev (&gsi) is not part of the for loop because cur_stmt
	     might be moved to a different basic block by bswap_replace and gsi
	     must not points to it if that's the case.  Moving the gsi_prev
	     there make sure that gsi points to the statement previous to
	     cur_stmt while still making sure that all statements are
	     considered in this basic block.  */
	  gsi_prev (&gsi);

	  if (!is_gimple_assign (cur_stmt))
	    continue;

	  code = gimple_assign_rhs_code (cur_stmt);
	  switch (code)
	    {
	    case LROTATE_EXPR:
	    case RROTATE_EXPR:
	      if (!tree_fits_uhwi_p (gimple_assign_rhs2 (cur_stmt))
		  || tree_to_uhwi (gimple_assign_rhs2 (cur_stmt))
		     % BITS_PER_UNIT)
		continue;
	      /* Fall through.  */
	    case BIT_IOR_EXPR:
	      break;
	    default:
	      continue;
	    }

	  ins_stmt = find_bswap_or_nop (cur_stmt, &n, &bswap);

	  if (!ins_stmt)
	    continue;

	  switch (n.range)
	    {
	    case 16:
	      /* Already in canonical form, nothing to do.  */
	      if (code == LROTATE_EXPR || code == RROTATE_EXPR)
		continue;
	      load_type = bswap_type = uint16_type_node;
	      break;
	    case 32:
	      load_type = uint32_type_node;
	      if (bswap32_p)
		{
		  fndecl = builtin_decl_explicit (BUILT_IN_BSWAP32);
		  bswap_type = bswap32_type;
		}
	      break;
	    case 64:
	      load_type = uint64_type_node;
	      if (bswap64_p)
		{
		  fndecl = builtin_decl_explicit (BUILT_IN_BSWAP64);
		  bswap_type = bswap64_type;
		}
	      break;
	    default:
	      continue;
	    }

	  if (bswap && !fndecl && n.range != 16)
	    continue;

	  if (bswap_replace (gsi_for_stmt (cur_stmt), ins_stmt, fndecl,
			     bswap_type, load_type, &n, bswap))
	    changed = true;
	}
    }

  statistics_counter_event (fun, "16-bit nop implementations found",
			    nop_stats.found_16bit);
  statistics_counter_event (fun, "32-bit nop implementations found",
			    nop_stats.found_32bit);
  statistics_counter_event (fun, "64-bit nop implementations found",
			    nop_stats.found_64bit);
  statistics_counter_event (fun, "16-bit bswap implementations found",
			    bswap_stats.found_16bit);
  statistics_counter_event (fun, "32-bit bswap implementations found",
			    bswap_stats.found_32bit);
  statistics_counter_event (fun, "64-bit bswap implementations found",
			    bswap_stats.found_64bit);

  return (changed ? TODO_update_ssa : 0);
}

} // anon namespace

gimple_opt_pass *
make_pass_optimize_bswap (gcc::context *ctxt)
{
  return new pass_optimize_bswap (ctxt);
}

namespace {

/* Struct recording one operand for the store, which is either a constant,
   then VAL represents the constant and all the other fields are zero, or
   a memory load, then VAL represents the reference, BASE_ADDR is non-NULL
   and the other fields also reflect the memory load, or an SSA name, then
   VAL represents the SSA name and all the other fields are zero,  */

struct store_operand_info
{
  tree val;
  tree base_addr;
  poly_uint64 bitsize;
  poly_uint64 bitpos;
  poly_uint64 bitregion_start;
  poly_uint64 bitregion_end;
  gimple *stmt;
  bool bit_not_p;
  store_operand_info ();
};

store_operand_info::store_operand_info ()
  : val (NULL_TREE), base_addr (NULL_TREE), bitsize (0), bitpos (0),
    bitregion_start (0), bitregion_end (0), stmt (NULL), bit_not_p (false)
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
  /* INTEGER_CST for constant stores, MEM_REF for memory copy,
     BIT_*_EXPR for logical bitwise operation, BIT_INSERT_EXPR
     for bit insertion.
     LROTATE_EXPR if it can be only bswap optimized and
     ops are not really meaningful.
     NOP_EXPR if bswap optimization detected identity, ops
     are not meaningful.  */
  enum tree_code rhs_code;
  /* Two fields for bswap optimization purposes.  */
  struct symbolic_number n;
  gimple *ins_stmt;
  /* True if BIT_{AND,IOR,XOR}_EXPR result is inverted before storing.  */
  bool bit_not_p;
  /* True if ops have been swapped and thus ops[1] represents
     rhs1 of BIT_{AND,IOR,XOR}_EXPR and ops[0] represents rhs2.  */
  bool ops_swapped_p;
  /* Operands.  For BIT_*_EXPR rhs_code both operands are used, otherwise
     just the first one.  */
  store_operand_info ops[2];
  store_immediate_info (unsigned HOST_WIDE_INT, unsigned HOST_WIDE_INT,
			unsigned HOST_WIDE_INT, unsigned HOST_WIDE_INT,
			gimple *, unsigned int, enum tree_code,
			struct symbolic_number &, gimple *, bool,
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
					    struct symbolic_number &nr,
					    gimple *ins_stmtp,
					    bool bitnotp,
					    const store_operand_info &op0r,
					    const store_operand_info &op1r)
  : bitsize (bs), bitpos (bp), bitregion_start (brs), bitregion_end (bre),
    stmt (st), order (ord), rhs_code (rhscode), n (nr),
    ins_stmt (ins_stmtp), bit_not_p (bitnotp), ops_swapped_p (false)
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
  poly_uint64 load_align_base[2];

  unsigned int align;
  unsigned int load_align[2];
  unsigned int first_order;
  unsigned int last_order;
  bool bit_insertion;

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
  bool can_be_merged_into (store_immediate_info *);
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
    fprintf (fd, "%02x ", ptr[i]);
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
  bit_insertion = false;
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

/* Return true if the store described by INFO can be merged into the group.  */

bool
merged_store_group::can_be_merged_into (store_immediate_info *info)
{
  /* Do not merge bswap patterns.  */
  if (info->rhs_code == LROTATE_EXPR)
    return false;

  /* The canonical case.  */
  if (info->rhs_code == stores[0]->rhs_code)
    return true;

  /* BIT_INSERT_EXPR is compatible with INTEGER_CST.  */
  if (info->rhs_code == BIT_INSERT_EXPR && stores[0]->rhs_code == INTEGER_CST)
    return true;

  if (stores[0]->rhs_code == BIT_INSERT_EXPR && info->rhs_code == INTEGER_CST)
    return true;

  /* We can turn MEM_REF into BIT_INSERT_EXPR for bit-field stores.  */
  if (info->rhs_code == MEM_REF
      && (stores[0]->rhs_code == INTEGER_CST
	  || stores[0]->rhs_code == BIT_INSERT_EXPR)
      && info->bitregion_start == stores[0]->bitregion_start
      && info->bitregion_end == stores[0]->bitregion_end)
    return true;

  if (stores[0]->rhs_code == MEM_REF
      && (info->rhs_code == INTEGER_CST
	  || info->rhs_code == BIT_INSERT_EXPR)
      && info->bitregion_start == stores[0]->bitregion_start
      && info->bitregion_end == stores[0]->bitregion_end)
    return true;

  return false;
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
  /* Make sure we're inserting in the position we think we're inserting.  */
  gcc_assert (info->bitpos >= start + width
	      && info->bitregion_start <= bitregion_end);

  width = info->bitpos + info->bitsize - start;
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
    width = info->bitpos + info->bitsize - start;

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
  /* Create a power-of-2-sized buffer for native_encode_expr.  */
  buf_size = 1 << ceil_log2 ((bitregion_end - bitregion_start) / BITS_PER_UNIT);
  val = XNEWVEC (unsigned char, 2 * buf_size);
  mask = val + buf_size;
  memset (val, 0, buf_size);
  memset (mask, ~0U, buf_size);

  FOR_EACH_VEC_ELT (stores, i, info)
    {
      unsigned int pos_in_buffer = info->bitpos - bitregion_start;
      tree cst;
      if (info->ops[0].val && info->ops[0].base_addr == NULL_TREE)
	cst = info->ops[0].val;
      else if (info->ops[1].val && info->ops[1].base_addr == NULL_TREE)
	cst = info->ops[1].val;
      else
	cst = NULL_TREE;
      bool ret = true;
      if (cst)
	{
	  if (info->rhs_code == BIT_INSERT_EXPR)
	    bit_insertion = true;
	  else
	    ret = encode_tree_to_bitpos (cst, val, info->bitsize,
					 pos_in_buffer, buf_size);
	}
      unsigned char *m = mask + (pos_in_buffer / BITS_PER_UNIT);
      if (BYTES_BIG_ENDIAN)
	clear_bit_region_be (m, (BITS_PER_UNIT - 1
				 - (pos_in_buffer % BITS_PER_UNIT)),
			     info->bitsize);
      else
	clear_bit_region (m, pos_in_buffer % BITS_PER_UNIT, info->bitsize);
      if (cst && dump_file && (dump_flags & TDF_DETAILS))
	{
	  if (ret)
	    {
	      fputs ("After writing ", dump_file);
	      print_generic_expr (dump_file, cst, TDF_NONE);
	      fprintf (dump_file, " of size " HOST_WIDE_INT_PRINT_DEC
		       " at position %d\n", info->bitsize, pos_in_buffer);
	      fputs ("  the merged value contains ", dump_file);
	      dump_char_array (dump_file, val, buf_size);
	      fputs ("  the merged mask contains  ", dump_file);
	      dump_char_array (dump_file, mask, buf_size);
	      if (bit_insertion)
		fputs ("  bit insertion is required\n", dump_file);
	    }
	  else
	    fprintf (dump_file, "Failed to merge stores\n");
	}
      if (!ret)
	return false;
    }
  stores.qsort (sort_by_bitpos);
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
  bool try_coalesce_bswap (merged_store_group *, unsigned int, unsigned int);
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

  /* Pass not supported for PDP-endian, nor for insane hosts or
     target character sizes where native_{encode,interpret}_expr
     doesn't work properly.  */
  virtual bool
  gate (function *)
  {
    return flag_store_merging
	   && BYTES_BIG_ENDIAN == WORDS_BIG_ENDIAN
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
  bool terminate_all_aliasing_chains (imm_store_chain_info **, gimple *);
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

/* Terminate all chains that are affected by the statement STMT.
   CHAIN_INFO is the chain we should ignore from the checks if
   non-NULL.  */

bool
pass_store_merging::terminate_all_aliasing_chains (imm_store_chain_info
						     **chain_info,
						   gimple *stmt)
{
  bool ret = false;

  /* If the statement doesn't touch memory it can't alias.  */
  if (!gimple_vuse (stmt))
    return false;

  tree store_lhs = gimple_store_p (stmt) ? gimple_get_lhs (stmt) : NULL_TREE;
  for (imm_store_chain_info *next = m_stores_head, *cur = next; cur; cur = next)
    {
      next = cur->next;

      /* We already checked all the stores in chain_info and terminated the
	 chain if necessary.  Skip it here.  */
      if (chain_info && *chain_info == cur)
	continue;

      store_immediate_info *info;
      unsigned int i;
      FOR_EACH_VEC_ELT (cur->m_store_info, i, info)
	{
	  tree lhs = gimple_assign_lhs (info->stmt);
	  if (ref_maybe_used_by_stmt_p (stmt, lhs)
	      || stmt_may_clobber_ref_p (stmt, lhs)
	      || (store_lhs && refs_output_dependent_p (store_lhs, lhs)))
	    {
	      if (dump_file && (dump_flags & TDF_DETAILS))
		{
		  fprintf (dump_file, "stmt causes chain termination:\n");
		  print_gimple_stmt (dump_file, stmt, 0);
		}
	      terminate_and_release_chain (cur);
	      ret = true;
	      break;
	    }
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
   have non-NULL vdef.  We want to be able to sink load of REF across
   stores between FIRST and LAST, up to right before LAST.  */

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
      if (gimple_store_p (stmt)
	  && refs_anti_dependent_p (ref, gimple_get_lhs (stmt)))
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
      || maybe_ne (info->ops[idx].bitpos - infof->ops[idx].bitpos,
		   info->bitpos - infof->bitpos)
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
  if (known_eq (info->ops[idx].bitpos, info->bitpos)
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

/* Add all refs loaded to compute VAL to REFS vector.  */

void
gather_bswap_load_refs (vec<tree> *refs, tree val)
{
  if (TREE_CODE (val) != SSA_NAME)
    return;

  gimple *stmt = SSA_NAME_DEF_STMT (val);
  if (!is_gimple_assign (stmt))
    return;

  if (gimple_assign_load_p (stmt))
    {
      refs->safe_push (gimple_assign_rhs1 (stmt));
      return;
    }

  switch (gimple_assign_rhs_class (stmt))
    {
    case GIMPLE_BINARY_RHS:
      gather_bswap_load_refs (refs, gimple_assign_rhs2 (stmt));
      /* FALLTHRU */
    case GIMPLE_UNARY_RHS:
      gather_bswap_load_refs (refs, gimple_assign_rhs1 (stmt));
      break;
    default:
      gcc_unreachable ();
    }
}

/* Check if there are any stores in M_STORE_INFO after index I
   (where M_STORE_INFO must be sorted by sort_by_bitpos) that overlap
   a potential group ending with END that have their order
   smaller than LAST_ORDER.  RHS_CODE is the kind of store in the
   group.  Return true if there are no such stores.
   Consider:
     MEM[(long long int *)p_28] = 0;
     MEM[(long long int *)p_28 + 8B] = 0;
     MEM[(long long int *)p_28 + 16B] = 0;
     MEM[(long long int *)p_28 + 24B] = 0;
     _129 = (int) _130;
     MEM[(int *)p_28 + 8B] = _129;
     MEM[(int *)p_28].a = -1;
   We already have
     MEM[(long long int *)p_28] = 0;
     MEM[(int *)p_28].a = -1;
   stmts in the current group and need to consider if it is safe to
   add MEM[(long long int *)p_28 + 8B] = 0; store into the same group.
   There is an overlap between that store and the MEM[(int *)p_28 + 8B] = _129;
   store though, so if we add the MEM[(long long int *)p_28 + 8B] = 0;
   into the group and merging of those 3 stores is successful, merged
   stmts will be emitted at the latest store from that group, i.e.
   LAST_ORDER, which is the MEM[(int *)p_28].a = -1; store.
   The MEM[(int *)p_28 + 8B] = _129; store that originally follows
   the MEM[(long long int *)p_28 + 8B] = 0; would now be before it,
   so we need to refuse merging MEM[(long long int *)p_28 + 8B] = 0;
   into the group.  That way it will be its own store group and will
   not be touched.  If RHS_CODE is INTEGER_CST and there are overlapping
   INTEGER_CST stores, those are mergeable using merge_overlapping,
   so don't return false for those.  */

static bool
check_no_overlap (vec<store_immediate_info *> m_store_info, unsigned int i,
		  enum tree_code rhs_code, unsigned int last_order,
		  unsigned HOST_WIDE_INT end)
{
  unsigned int len = m_store_info.length ();
  for (++i; i < len; ++i)
    {
      store_immediate_info *info = m_store_info[i];
      if (info->bitpos >= end)
	break;
      if (info->order < last_order
	  && (rhs_code != INTEGER_CST || info->rhs_code != INTEGER_CST))
	return false;
    }
  return true;
}

/* Return true if m_store_info[first] and at least one following store
   form a group which store try_size bitsize value which is byte swapped
   from a memory load or some value, or identity from some value.
   This uses the bswap pass APIs.  */

bool
imm_store_chain_info::try_coalesce_bswap (merged_store_group *merged_store,
					  unsigned int first,
					  unsigned int try_size)
{
  unsigned int len = m_store_info.length (), last = first;
  unsigned HOST_WIDE_INT width = m_store_info[first]->bitsize;
  if (width >= try_size)
    return false;
  for (unsigned int i = first + 1; i < len; ++i)
    {
      if (m_store_info[i]->bitpos != m_store_info[first]->bitpos + width
	  || m_store_info[i]->ins_stmt == NULL)
	return false;
      width += m_store_info[i]->bitsize;
      if (width >= try_size)
	{
	  last = i;
	  break;
	}
    }
  if (width != try_size)
    return false;

  bool allow_unaligned
    = !STRICT_ALIGNMENT && PARAM_VALUE (PARAM_STORE_MERGING_ALLOW_UNALIGNED);
  /* Punt if the combined store would not be aligned and we need alignment.  */
  if (!allow_unaligned)
    {
      unsigned int align = merged_store->align;
      unsigned HOST_WIDE_INT align_base = merged_store->align_base;
      for (unsigned int i = first + 1; i <= last; ++i)
	{
	  unsigned int this_align;
	  unsigned HOST_WIDE_INT align_bitpos = 0;
	  get_object_alignment_1 (gimple_assign_lhs (m_store_info[i]->stmt),
				  &this_align, &align_bitpos);
	  if (this_align > align)
	    {
	      align = this_align;
	      align_base = m_store_info[i]->bitpos - align_bitpos;
	    }
	}
      unsigned HOST_WIDE_INT align_bitpos
	= (m_store_info[first]->bitpos - align_base) & (align - 1);
      if (align_bitpos)
	align = least_bit_hwi (align_bitpos);
      if (align < try_size)
	return false;
    }

  tree type;
  switch (try_size)
    {
    case 16: type = uint16_type_node; break;
    case 32: type = uint32_type_node; break;
    case 64: type = uint64_type_node; break;
    default: gcc_unreachable ();
    }
  struct symbolic_number n;
  gimple *ins_stmt = NULL;
  int vuse_store = -1;
  unsigned int first_order = merged_store->first_order;
  unsigned int last_order = merged_store->last_order;
  gimple *first_stmt = merged_store->first_stmt;
  gimple *last_stmt = merged_store->last_stmt;
  unsigned HOST_WIDE_INT end = merged_store->start + merged_store->width;
  store_immediate_info *infof = m_store_info[first];

  for (unsigned int i = first; i <= last; ++i)
    {
      store_immediate_info *info = m_store_info[i];
      struct symbolic_number this_n = info->n;
      this_n.type = type;
      if (!this_n.base_addr)
	this_n.range = try_size / BITS_PER_UNIT;
      else
	/* Update vuse in case it has changed by output_merged_stores.  */
	this_n.vuse = gimple_vuse (info->ins_stmt);
      unsigned int bitpos = info->bitpos - infof->bitpos;
      if (!do_shift_rotate (LSHIFT_EXPR, &this_n,
			    BYTES_BIG_ENDIAN
			    ? try_size - info->bitsize - bitpos
			    : bitpos))
	return false;
      if (this_n.base_addr && vuse_store)
	{
	  unsigned int j;
	  for (j = first; j <= last; ++j)
	    if (this_n.vuse == gimple_vuse (m_store_info[j]->stmt))
	      break;
	  if (j > last)
	    {
	      if (vuse_store == 1)
		return false;
	      vuse_store = 0;
	    }
	}
      if (i == first)
	{
	  n = this_n;
	  ins_stmt = info->ins_stmt;
	}
      else
	{
	  if (n.base_addr && n.vuse != this_n.vuse)
	    {
	      if (vuse_store == 0)
		return false;
	      vuse_store = 1;
	    }
	  if (info->order > last_order)
	    {
	      last_order = info->order;
	      last_stmt = info->stmt;
	    }
	  else if (info->order < first_order)
	    {
	      first_order = info->order;
	      first_stmt = info->stmt;
	    }
	  end = MAX (end, info->bitpos + info->bitsize);

	  ins_stmt = perform_symbolic_merge (ins_stmt, &n, info->ins_stmt,
					     &this_n, &n);
	  if (ins_stmt == NULL)
	    return false;
	}
    }

  uint64_t cmpxchg, cmpnop;
  find_bswap_or_nop_finalize (&n, &cmpxchg, &cmpnop);

  /* A complete byte swap should make the symbolic number to start with
     the largest digit in the highest order byte.  Unchanged symbolic
     number indicates a read with same endianness as target architecture.  */
  if (n.n != cmpnop && n.n != cmpxchg)
    return false;

  if (n.base_addr == NULL_TREE && !is_gimple_val (n.src))
    return false;

  if (!check_no_overlap (m_store_info, last, LROTATE_EXPR, last_order, end))
    return false;

  /* Don't handle memory copy this way if normal non-bswap processing
     would handle it too.  */
  if (n.n == cmpnop && (unsigned) n.n_ops == last - first + 1)
    {
      unsigned int i;
      for (i = first; i <= last; ++i)
	if (m_store_info[i]->rhs_code != MEM_REF)
	  break;
      if (i == last + 1)
	return false;
    }

  if (n.n == cmpxchg)
    switch (try_size)
      {
      case 16:
	/* Will emit LROTATE_EXPR.  */
	break;
      case 32:
	if (builtin_decl_explicit_p (BUILT_IN_BSWAP32)
	    && optab_handler (bswap_optab, SImode) != CODE_FOR_nothing)
	  break;
	return false;
      case 64:
	if (builtin_decl_explicit_p (BUILT_IN_BSWAP64)
	    && optab_handler (bswap_optab, DImode) != CODE_FOR_nothing)
	  break;
	return false;
      default:
	gcc_unreachable ();
      }

  if (!allow_unaligned && n.base_addr)
    {
      unsigned int align = get_object_alignment (n.src);
      if (align < try_size)
	return false;
    }

  /* If each load has vuse of the corresponding store, need to verify
     the loads can be sunk right before the last store.  */
  if (vuse_store == 1)
    {
      auto_vec<tree, 64> refs;
      for (unsigned int i = first; i <= last; ++i)
	gather_bswap_load_refs (&refs,
				gimple_assign_rhs1 (m_store_info[i]->stmt));

      unsigned int i;
      tree ref;
      FOR_EACH_VEC_ELT (refs, i, ref)
	if (stmts_may_clobber_ref_p (first_stmt, last_stmt, ref))
	  return false;
      n.vuse = NULL_TREE;
    }

  infof->n = n;
  infof->ins_stmt = ins_stmt;
  for (unsigned int i = first; i <= last; ++i)
    {
      m_store_info[i]->rhs_code = n.n == cmpxchg ? LROTATE_EXPR : NOP_EXPR;
      m_store_info[i]->ops[0].base_addr = NULL_TREE;
      m_store_info[i]->ops[1].base_addr = NULL_TREE;
      if (i != first)
	merged_store->merge_into (m_store_info[i]);
    }

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
    fprintf (dump_file, "Attempting to coalesce %u stores in chain\n",
	     m_store_info.length ());

  store_immediate_info *info;
  unsigned int i, ignore = 0;

  /* Order the stores by the bitposition they write to.  */
  m_store_info.qsort (sort_by_bitpos);

  info = m_store_info[0];
  merged_store_group *merged_store = new merged_store_group (info);
  if (dump_file && (dump_flags & TDF_DETAILS))
    fputs ("New store group\n", dump_file);

  FOR_EACH_VEC_ELT (m_store_info, i, info)
    {
      if (i <= ignore)
	goto done;

      /* First try to handle group of stores like:
	 p[0] = data >> 24;
	 p[1] = data >> 16;
	 p[2] = data >> 8;
	 p[3] = data;
	 using the bswap framework.  */
      if (info->bitpos == merged_store->start + merged_store->width
	  && merged_store->stores.length () == 1
	  && merged_store->stores[0]->ins_stmt != NULL
	  && info->ins_stmt != NULL)
	{
	  unsigned int try_size;
	  for (try_size = 64; try_size >= 16; try_size >>= 1)
	    if (try_coalesce_bswap (merged_store, i - 1, try_size))
	      break;

	  if (try_size >= 16)
	    {
	      ignore = i + merged_store->stores.length () - 1;
	      m_merged_store_groups.safe_push (merged_store);
	      if (ignore < m_store_info.length ())
		merged_store = new merged_store_group (m_store_info[ignore]);
	      else
		merged_store = NULL;
	      goto done;
	    }
	}

      /* |---store 1---|
	       |---store 2---|
	 Overlapping stores.  */
      if (IN_RANGE (info->bitpos, merged_store->start,
		    merged_store->start + merged_store->width - 1))
	{
	  /* Only allow overlapping stores of constants.  */
	  if (info->rhs_code == INTEGER_CST
	      && merged_store->stores[0]->rhs_code == INTEGER_CST)
	    {
	      merged_store->merge_overlapping (info);
	      goto done;
	    }
	}
      /* |---store 1---||---store 2---|
	 This store is consecutive to the previous one.
	 Merge it into the current store group.  There can be gaps in between
	 the stores, but there can't be gaps in between bitregions.  */
      else if (info->bitregion_start <= merged_store->bitregion_end
	       && merged_store->can_be_merged_into (info))
	{
	  store_immediate_info *infof = merged_store->stores[0];

	  /* All the rhs_code ops that take 2 operands are commutative,
	     swap the operands if it could make the operands compatible.  */
	  if (infof->ops[0].base_addr
	      && infof->ops[1].base_addr
	      && info->ops[0].base_addr
	      && info->ops[1].base_addr
	      && known_eq (info->ops[1].bitpos - infof->ops[0].bitpos,
			   info->bitpos - infof->bitpos)
	      && operand_equal_p (info->ops[1].base_addr,
				  infof->ops[0].base_addr, 0))
	    {
	      std::swap (info->ops[0], info->ops[1]);
	      info->ops_swapped_p = true;
	    }
	  if (check_no_overlap (m_store_info, i, info->rhs_code,
				MAX (merged_store->last_order,
				     info->order),
				MAX (merged_store->start
				     + merged_store->width,
				     info->bitpos + info->bitsize)))
	    {
	      /* Turn MEM_REF into BIT_INSERT_EXPR for bit-field stores.  */
	      if (info->rhs_code == MEM_REF && infof->rhs_code != MEM_REF)
		{
		  info->rhs_code = BIT_INSERT_EXPR;
		  info->ops[0].val = gimple_assign_rhs1 (info->stmt);
		  info->ops[0].base_addr = NULL_TREE;
		}
	      else if (infof->rhs_code == MEM_REF && info->rhs_code != MEM_REF)
		{
		  store_immediate_info *infoj;
		  unsigned int j;
		  FOR_EACH_VEC_ELT (merged_store->stores, j, infoj)
		    {
		      infoj->rhs_code = BIT_INSERT_EXPR;
		      infoj->ops[0].val = gimple_assign_rhs1 (infoj->stmt);
		      infoj->ops[0].base_addr = NULL_TREE;
		    }
		}
	      if ((infof->ops[0].base_addr
		   ? compatible_load_p (merged_store, info, base_addr, 0)
		   : !info->ops[0].base_addr)
		  && (infof->ops[1].base_addr
		      ? compatible_load_p (merged_store, info, base_addr, 1)
		      : !info->ops[1].base_addr))
		{
		  merged_store->merge_into (info);
		  goto done;
		}
	    }
	}

      /* |---store 1---| <gap> |---store 2---|.
	 Gap between stores or the rhs not compatible.  Start a new group.  */

      /* Try to apply all the stores recorded for the group to determine
	 the bitpattern they write and discard it if that fails.
	 This will also reject single-store groups.  */
      if (merged_store->apply_stores ())
	m_merged_store_groups.safe_push (merged_store);
      else
	delete merged_store;

      merged_store = new merged_store_group (info);
      if (dump_file && (dump_flags & TDF_DETAILS))
	fputs ("New store group\n", dump_file);

    done:
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Store %u:\nbitsize:" HOST_WIDE_INT_PRINT_DEC
			      " bitpos:" HOST_WIDE_INT_PRINT_DEC " val:",
		   i, info->bitsize, info->bitpos);
	  print_generic_expr (dump_file, gimple_assign_rhs1 (info->stmt));
	  fputc ('\n', dump_file);
	}
    }

  /* Record or discard the last store group.  */
  if (merged_store)
    {
      if (merged_store->apply_stores ())
	m_merged_store_groups.safe_push (merged_store);
      else
	delete merged_store;
    }

  gcc_assert (m_merged_store_groups.length () <= m_store_info.length ());

  bool success
    = !m_merged_store_groups.is_empty ()
      && m_merged_store_groups.length () < m_store_info.length ();

  if (success && dump_file)
    fprintf (dump_file, "Coalescing successful!\nMerged into %u stores\n",
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

/* Return how many SSA_NAMEs used to compute value to store in the INFO
   store have multiple uses.  If any SSA_NAME has multiple uses, also
   count statements needed to compute it.  */

static unsigned
count_multiple_uses (store_immediate_info *info)
{
  gimple *stmt = info->stmt;
  unsigned ret = 0;
  switch (info->rhs_code)
    {
    case INTEGER_CST:
      return 0;
    case BIT_AND_EXPR:
    case BIT_IOR_EXPR:
    case BIT_XOR_EXPR:
      if (info->bit_not_p)
	{
	  if (!has_single_use (gimple_assign_rhs1 (stmt)))
	    ret = 1; /* Fall through below to return
			the BIT_NOT_EXPR stmt and then
			BIT_{AND,IOR,XOR}_EXPR and anything it
			uses.  */
	  else
	    /* stmt is after this the BIT_NOT_EXPR.  */
	    stmt = SSA_NAME_DEF_STMT (gimple_assign_rhs1 (stmt));
	}
      if (!has_single_use (gimple_assign_rhs1 (stmt)))
	{
	  ret += 1 + info->ops[0].bit_not_p;
	  if (info->ops[1].base_addr)
	    ret += 1 + info->ops[1].bit_not_p;
	  return ret + 1;
	}
      stmt = SSA_NAME_DEF_STMT (gimple_assign_rhs1 (stmt));
      /* stmt is now the BIT_*_EXPR.  */
      if (!has_single_use (gimple_assign_rhs1 (stmt)))
	ret += 1 + info->ops[info->ops_swapped_p].bit_not_p;
      else if (info->ops[info->ops_swapped_p].bit_not_p)
	{
	  gimple *stmt2 = SSA_NAME_DEF_STMT (gimple_assign_rhs1 (stmt));
	  if (!has_single_use (gimple_assign_rhs1 (stmt2)))
	    ++ret;
	}
      if (info->ops[1].base_addr == NULL_TREE)
	{
	  gcc_checking_assert (!info->ops_swapped_p);
	  return ret;
	}
      if (!has_single_use (gimple_assign_rhs2 (stmt)))
	ret += 1 + info->ops[1 - info->ops_swapped_p].bit_not_p;
      else if (info->ops[1 - info->ops_swapped_p].bit_not_p)
	{
	  gimple *stmt2 = SSA_NAME_DEF_STMT (gimple_assign_rhs2 (stmt));
	  if (!has_single_use (gimple_assign_rhs1 (stmt2)))
	    ++ret;
	}
      return ret;
    case MEM_REF:
      if (!has_single_use (gimple_assign_rhs1 (stmt)))
	return 1 + info->ops[0].bit_not_p;
      else if (info->ops[0].bit_not_p)
	{
	  stmt = SSA_NAME_DEF_STMT (gimple_assign_rhs1 (stmt));
	  if (!has_single_use (gimple_assign_rhs1 (stmt)))
	    return 1;
	}
      return 0;
    case BIT_INSERT_EXPR:
      return has_single_use (gimple_assign_rhs1 (stmt)) ? 0 : 1;
    default:
      gcc_unreachable ();
    }
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
	     vec<struct split_store *> *split_stores,
	     unsigned *total_orig,
	     unsigned *total_new)
{
  unsigned HOST_WIDE_INT pos = group->bitregion_start;
  unsigned HOST_WIDE_INT size = group->bitregion_end - pos;
  unsigned HOST_WIDE_INT bytepos = pos / BITS_PER_UNIT;
  unsigned HOST_WIDE_INT group_align = group->align;
  unsigned HOST_WIDE_INT align_base = group->align_base;
  unsigned HOST_WIDE_INT group_load_align = group_align;
  bool any_orig = false;

  gcc_assert ((size % BITS_PER_UNIT == 0) && (pos % BITS_PER_UNIT == 0));

  if (group->stores[0]->rhs_code == LROTATE_EXPR
      || group->stores[0]->rhs_code == NOP_EXPR)
    {
      /* For bswap framework using sets of stores, all the checking
	 has been done earlier in try_coalesce_bswap and needs to be
	 emitted as a single store.  */
      if (total_orig)
	{
	  /* Avoid the old/new stmt count heuristics.  It should be
	     always beneficial.  */
	  total_new[0] = 1;
	  total_orig[0] = 2;
	}

      if (split_stores)
	{
	  unsigned HOST_WIDE_INT align_bitpos
	    = (group->start - align_base) & (group_align - 1);
	  unsigned HOST_WIDE_INT align = group_align;
	  if (align_bitpos)
	    align = least_bit_hwi (align_bitpos);
	  bytepos = group->start / BITS_PER_UNIT;
	  struct split_store *store
	    = new split_store (bytepos, group->width, align);
	  unsigned int first = 0;
	  find_constituent_stores (group, &store->orig_stores,
				   &first, group->start, group->width);
	  split_stores->safe_push (store);
	}

      return 1;
    }

  unsigned int ret = 0, first = 0;
  unsigned HOST_WIDE_INT try_pos = bytepos;

  if (total_orig)
    {
      unsigned int i;
      store_immediate_info *info = group->stores[0];

      total_new[0] = 0;
      total_orig[0] = 1; /* The orig store.  */
      info = group->stores[0];
      if (info->ops[0].base_addr)
	total_orig[0]++;
      if (info->ops[1].base_addr)
	total_orig[0]++;
      switch (info->rhs_code)
	{
	case BIT_AND_EXPR:
	case BIT_IOR_EXPR:
	case BIT_XOR_EXPR:
	  total_orig[0]++; /* The orig BIT_*_EXPR stmt.  */
	  break;
	default:
	  break;
	}
      total_orig[0] *= group->stores.length ();

      FOR_EACH_VEC_ELT (group->stores, i, info)
	{
	  total_new[0] += count_multiple_uses (info);
	  total_orig[0] += (info->bit_not_p
			    + info->ops[0].bit_not_p
			    + info->ops[1].bit_not_p);
	}
    }

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
		align_bitpos
		  = known_alignment (try_bitpos
				     - group->stores[0]->bitpos
				     + group->stores[0]->ops[i].bitpos
				     - group->load_align_base[i]);
		if (align_bitpos & (group_load_align - 1))
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
	    {
	      store->orig = true;
	      any_orig = true;
	    }
	  split_stores->safe_push (store);
	}

      try_pos += try_size / BITS_PER_UNIT;
      size -= try_size;
    }

  if (total_orig)
    {
      unsigned int i;
      struct split_store *store;
      /* If we are reusing some original stores and any of the
	 original SSA_NAMEs had multiple uses, we need to subtract
	 those now before we add the new ones.  */
      if (total_new[0] && any_orig)
	{
	  FOR_EACH_VEC_ELT (*split_stores, i, store)
	    if (store->orig)
	      total_new[0] -= count_multiple_uses (store->orig_stores[0]);
	}
      total_new[0] += ret; /* The new store.  */
      store_immediate_info *info = group->stores[0];
      if (info->ops[0].base_addr)
	total_new[0] += ret;
      if (info->ops[1].base_addr)
	total_new[0] += ret;
      switch (info->rhs_code)
	{
	case BIT_AND_EXPR:
	case BIT_IOR_EXPR:
	case BIT_XOR_EXPR:
	  total_new[0] += ret; /* The new BIT_*_EXPR stmt.  */
	  break;
	default:
	  break;
	}
      FOR_EACH_VEC_ELT (*split_stores, i, store)
	{
	  unsigned int j;
	  bool bit_not_p[3] = { false, false, false };
	  /* If all orig_stores have certain bit_not_p set, then
	     we'd use a BIT_NOT_EXPR stmt and need to account for it.
	     If some orig_stores have certain bit_not_p set, then
	     we'd use a BIT_XOR_EXPR with a mask and need to account for
	     it.  */
	  FOR_EACH_VEC_ELT (store->orig_stores, j, info)
	    {
	      if (info->ops[0].bit_not_p)
		bit_not_p[0] = true;
	      if (info->ops[1].bit_not_p)
		bit_not_p[1] = true;
	      if (info->bit_not_p)
		bit_not_p[2] = true;
	    }
	  total_new[0] += bit_not_p[0] + bit_not_p[1] + bit_not_p[2];
	}

    }

  return ret;
}

/* Return the operation through which the operand IDX (if < 2) or
   result (IDX == 2) should be inverted.  If NOP_EXPR, no inversion
   is done, if BIT_NOT_EXPR, all bits are inverted, if BIT_XOR_EXPR,
   the bits should be xored with mask.  */

static enum tree_code
invert_op (split_store *split_store, int idx, tree int_type, tree &mask)
{
  unsigned int i;
  store_immediate_info *info;
  unsigned int cnt = 0;
  bool any_paddings = false;
  FOR_EACH_VEC_ELT (split_store->orig_stores, i, info)
    {
      bool bit_not_p = idx < 2 ? info->ops[idx].bit_not_p : info->bit_not_p;
      if (bit_not_p)
	{
	  ++cnt;
	  tree lhs = gimple_assign_lhs (info->stmt);
	  if (INTEGRAL_TYPE_P (TREE_TYPE (lhs))
	      && TYPE_PRECISION (TREE_TYPE (lhs)) < info->bitsize)
	    any_paddings = true;
	}
    }
  mask = NULL_TREE;
  if (cnt == 0)
    return NOP_EXPR;
  if (cnt == split_store->orig_stores.length () && !any_paddings)
    return BIT_NOT_EXPR;

  unsigned HOST_WIDE_INT try_bitpos = split_store->bytepos * BITS_PER_UNIT;
  unsigned buf_size = split_store->size / BITS_PER_UNIT;
  unsigned char *buf
    = XALLOCAVEC (unsigned char, buf_size);
  memset (buf, ~0U, buf_size);
  FOR_EACH_VEC_ELT (split_store->orig_stores, i, info)
    {
      bool bit_not_p = idx < 2 ? info->ops[idx].bit_not_p : info->bit_not_p;
      if (!bit_not_p)
	continue;
      /* Clear regions with bit_not_p and invert afterwards, rather than
	 clear regions with !bit_not_p, so that gaps in between stores aren't
	 set in the mask.  */
      unsigned HOST_WIDE_INT bitsize = info->bitsize;
      unsigned HOST_WIDE_INT prec = bitsize;
      unsigned int pos_in_buffer = 0;
      if (any_paddings)
	{
	  tree lhs = gimple_assign_lhs (info->stmt);
	  if (INTEGRAL_TYPE_P (TREE_TYPE (lhs))
	      && TYPE_PRECISION (TREE_TYPE (lhs)) < bitsize)
	    prec = TYPE_PRECISION (TREE_TYPE (lhs));
	}
      if (info->bitpos < try_bitpos)
	{
	  gcc_assert (info->bitpos + bitsize > try_bitpos);
	  if (!BYTES_BIG_ENDIAN)
	    {
	      if (prec <= try_bitpos - info->bitpos)
		continue;
	      prec -= try_bitpos - info->bitpos;
	    }
	  bitsize -= try_bitpos - info->bitpos;
	  if (BYTES_BIG_ENDIAN && prec > bitsize)
	    prec = bitsize;
	}
      else
	pos_in_buffer = info->bitpos - try_bitpos;
      if (prec < bitsize)
	{
	  /* If this is a bool inversion, invert just the least significant
	     prec bits rather than all bits of it.  */
	  if (BYTES_BIG_ENDIAN)
	    {
	      pos_in_buffer += bitsize - prec;
	      if (pos_in_buffer >= split_store->size)
		continue;
	    }
	  bitsize = prec;
	}
      if (pos_in_buffer + bitsize > split_store->size)
	bitsize = split_store->size - pos_in_buffer;
      unsigned char *p = buf + (pos_in_buffer / BITS_PER_UNIT);
      if (BYTES_BIG_ENDIAN)
	clear_bit_region_be (p, (BITS_PER_UNIT - 1
				 - (pos_in_buffer % BITS_PER_UNIT)), bitsize);
      else
	clear_bit_region (p, pos_in_buffer % BITS_PER_UNIT, bitsize);
    }
  for (unsigned int i = 0; i < buf_size; ++i)
    buf[i] = ~buf[i];
  mask = native_interpret_expr (int_type, buf, buf_size);
  return BIT_XOR_EXPR;
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
  split_store *split_store;
  unsigned int i;
  unsigned HOST_WIDE_INT start_byte_pos
    = group->bitregion_start / BITS_PER_UNIT;

  unsigned int orig_num_stmts = group->stores.length ();
  if (orig_num_stmts < 2)
    return false;

  auto_vec<struct split_store *, 32> split_stores;
  bool allow_unaligned_store
    = !STRICT_ALIGNMENT && PARAM_VALUE (PARAM_STORE_MERGING_ALLOW_UNALIGNED);
  bool allow_unaligned_load = allow_unaligned_store;
  if (allow_unaligned_store)
    {
      /* If unaligned stores are allowed, see how many stores we'd emit
	 for unaligned and how many stores we'd emit for aligned stores.
	 Only use unaligned stores if it allows fewer stores than aligned.  */
      unsigned aligned_cnt
	= split_group (group, false, allow_unaligned_load, NULL, NULL, NULL);
      unsigned unaligned_cnt
	= split_group (group, true, allow_unaligned_load, NULL, NULL, NULL);
      if (aligned_cnt <= unaligned_cnt)
	allow_unaligned_store = false;
    }
  unsigned total_orig, total_new;
  split_group (group, allow_unaligned_store, allow_unaligned_load,
	       &split_stores, &total_orig, &total_new);

  if (split_stores.length () >= orig_num_stmts)
    {
      /* We didn't manage to reduce the number of statements.  Bail out.  */
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Exceeded original number of stmts (%u)."
			    "  Not profitable to emit new sequence.\n",
		 orig_num_stmts);
      FOR_EACH_VEC_ELT (split_stores, i, split_store)
	delete split_store;
      return false;
    }
  if (total_orig <= total_new)
    {
      /* If number of estimated new statements is above estimated original
	 statements, bail out too.  */
      if (dump_file && (dump_flags & TDF_DETAILS))
	fprintf (dump_file, "Estimated number of original stmts (%u)"
			    " not larger than estimated number of new"
			    " stmts (%u).\n",
		 total_orig, total_new);
      FOR_EACH_VEC_ELT (split_stores, i, split_store)
	delete split_store;
      return false;
    }

  gimple_stmt_iterator last_gsi = gsi_for_stmt (group->last_stmt);
  gimple_seq seq = NULL;
  tree last_vdef, new_vuse;
  last_vdef = gimple_vdef (group->last_stmt);
  new_vuse = gimple_vuse (group->last_stmt);
  tree bswap_res = NULL_TREE;

  if (group->stores[0]->rhs_code == LROTATE_EXPR
      || group->stores[0]->rhs_code == NOP_EXPR)
    {
      tree fndecl = NULL_TREE, bswap_type = NULL_TREE, load_type;
      gimple *ins_stmt = group->stores[0]->ins_stmt;
      struct symbolic_number *n = &group->stores[0]->n;
      bool bswap = group->stores[0]->rhs_code == LROTATE_EXPR;

      switch (n->range)
	{
	case 16:
	  load_type = bswap_type = uint16_type_node;
	  break;
	case 32:
	  load_type = uint32_type_node;
	  if (bswap)
	    {
	      fndecl = builtin_decl_explicit (BUILT_IN_BSWAP32);
	      bswap_type = TREE_VALUE (TYPE_ARG_TYPES (TREE_TYPE (fndecl)));
	    }
	  break;
	case 64:
	  load_type = uint64_type_node;
	  if (bswap)
	    {
	      fndecl = builtin_decl_explicit (BUILT_IN_BSWAP64);
	      bswap_type = TREE_VALUE (TYPE_ARG_TYPES (TREE_TYPE (fndecl)));
	    }
	  break;
	default:
	  gcc_unreachable ();
	}

      /* If the loads have each vuse of the corresponding store,
	 we've checked the aliasing already in try_coalesce_bswap and
	 we want to sink the need load into seq.  So need to use new_vuse
	 on the load.  */
      if (n->base_addr)
	{
	  if (n->vuse == NULL)
	    {
	      n->vuse = new_vuse;
	      ins_stmt = NULL;
	    }
	  else
	    /* Update vuse in case it has changed by output_merged_stores.  */
	    n->vuse = gimple_vuse (ins_stmt);
	}
      bswap_res = bswap_replace (gsi_start (seq), ins_stmt, fndecl,
				 bswap_type, load_type, n, bswap);
      gcc_assert (bswap_res);
    }

  gimple *stmt = NULL;
  auto_vec<gimple *, 32> orig_stmts;
  gimple_seq this_seq;
  tree addr = force_gimple_operand_1 (unshare_expr (base_addr), &this_seq,
				      is_gimple_mem_ref_addr, NULL_TREE);
  gimple_seq_add_seq_without_update (&seq, this_seq);

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
	  /* We can't pick the location randomly; while we've verified
	     all the loads have the same vuse, they can be still in different
	     basic blocks and we need to pick the one from the last bb:
	     int x = q[0];
	     if (x == N) return;
	     int y = q[1];
	     p[0] = x;
	     p[1] = y;
	     otherwise if we put the wider load at the q[0] load, we might
	     segfault if q[1] is not mapped.  */
	  basic_block bb = gimple_bb (op.stmt);
	  gimple *ostmt = op.stmt;
	  store_immediate_info *info;
	  FOR_EACH_VEC_ELT (group->stores, i, info)
	    {
	      gimple *tstmt = info->ops[j].stmt;
	      basic_block tbb = gimple_bb (tstmt);
	      if (dominated_by_p (CDI_DOMINATORS, tbb, bb))
		{
		  ostmt = tstmt;
		  bb = tbb;
		}
	    }
	  load_gsi[j] = gsi_for_stmt (ostmt);
	  load_addr[j]
	    = force_gimple_operand_1 (unshare_expr (op.base_addr),
				      &load_seq[j], is_gimple_mem_ref_addr,
				      NULL_TREE);
	}
      else if (operand_equal_p (base_addr, op.base_addr, 0))
	load_addr[j] = addr;
      else
	{
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
      unsigned HOST_WIDE_INT try_bitpos = try_pos * BITS_PER_UNIT;
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

	  tree mask;
	  if (bswap_res)
	    mask = integer_zero_node;
	  else
	    mask = native_interpret_expr (int_type,
					  group->mask + try_pos
					  - start_byte_pos,
					  group->buf_size);

	  tree ops[2];
	  for (int j = 0;
	       j < 1 + (split_store->orig_stores[0]->ops[1].val != NULL_TREE);
	       ++j)
	    {
	      store_operand_info &op = split_store->orig_stores[0]->ops[j];
	      if (bswap_res)
		ops[j] = bswap_res;
	      else if (op.base_addr)
		{
		  FOR_EACH_VEC_ELT (split_store->orig_stores, k, info)
		    orig_stmts.safe_push (info->ops[j].stmt);

		  offset_type = get_alias_type_for_stmts (orig_stmts, true,
							  &clique, &base);
		  location_t load_loc = get_location_for_stmts (orig_stmts);
		  orig_stmts.truncate (0);

		  unsigned HOST_WIDE_INT load_align = group->load_align[j];
		  unsigned HOST_WIDE_INT align_bitpos
		    = known_alignment (try_bitpos
				       - split_store->orig_stores[0]->bitpos
				       + op.bitpos);
		  if (align_bitpos & (load_align - 1))
		    load_align = least_bit_hwi (align_bitpos);

		  tree load_int_type
		    = build_nonstandard_integer_type (try_size, UNSIGNED);
		  load_int_type
		    = build_aligned_type (load_int_type, load_align);

		  poly_uint64 load_pos
		    = exact_div (try_bitpos
				 - split_store->orig_stores[0]->bitpos
				 + op.bitpos,
				 BITS_PER_UNIT);
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
		  tree xor_mask;
		  enum tree_code inv_op
		    = invert_op (split_store, j, int_type, xor_mask);
		  if (inv_op != NOP_EXPR)
		    {
		      stmt = gimple_build_assign (make_ssa_name (int_type),
						  inv_op, ops[j], xor_mask);
		      gimple_set_location (stmt, load_loc);
		      ops[j] = gimple_assign_lhs (stmt);

		      if (gsi_bb (load_gsi[j]))
			gimple_seq_add_stmt_without_update (&load_seq[j],
							    stmt);
		      else
			gimple_seq_add_stmt_without_update (&seq, stmt);
		    }
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
	      tree xor_mask;
	      enum tree_code inv_op;
	      inv_op = invert_op (split_store, 2, int_type, xor_mask);
	      if (inv_op != NOP_EXPR)
		{
		  stmt = gimple_build_assign (make_ssa_name (int_type),
					      inv_op, src, xor_mask);
		  gimple_set_location (stmt, bit_loc);
		  if (load_addr[1] == NULL_TREE && gsi_bb (load_gsi[0]))
		    gimple_seq_add_stmt_without_update (&load_seq[0], stmt);
		  else
		    gimple_seq_add_stmt_without_update (&seq, stmt);
		  src = gimple_assign_lhs (stmt);
		}
	      break;
	    case LROTATE_EXPR:
	    case NOP_EXPR:
	      src = ops[0];
	      if (!is_gimple_val (src))
		{
		  stmt = gimple_build_assign (make_ssa_name (TREE_TYPE (src)),
					      src);
		  gimple_seq_add_stmt_without_update (&seq, stmt);
		  src = gimple_assign_lhs (stmt);
		}
	      if (!useless_type_conversion_p (int_type, TREE_TYPE (src)))
		{
		  stmt = gimple_build_assign (make_ssa_name (int_type),
					      NOP_EXPR, src);
		  gimple_seq_add_stmt_without_update (&seq, stmt);
		  src = gimple_assign_lhs (stmt);
		}
	      inv_op = invert_op (split_store, 2, int_type, xor_mask);
	      if (inv_op != NOP_EXPR)
		{
		  stmt = gimple_build_assign (make_ssa_name (int_type),
					      inv_op, src, xor_mask);
		  gimple_set_location (stmt, loc);
		  gimple_seq_add_stmt_without_update (&seq, stmt);
		  src = gimple_assign_lhs (stmt);
		}
	      break;
	    default:
	      src = ops[0];
	      break;
	    }

	  /* If bit insertion is required, we use the source as an accumulator
	     into which the successive bit-field values are manually inserted.
	     FIXME: perhaps use BIT_INSERT_EXPR instead in some cases?  */
	  if (group->bit_insertion)
	    FOR_EACH_VEC_ELT (split_store->orig_stores, k, info)
	      if (info->rhs_code == BIT_INSERT_EXPR
		  && info->bitpos < try_bitpos + try_size
		  && info->bitpos + info->bitsize > try_bitpos)
		{
		  /* Mask, truncate, convert to final type, shift and ior into
		     the accumulator.  Note that every step can be a no-op.  */
		  const HOST_WIDE_INT start_gap = info->bitpos - try_bitpos;
		  const HOST_WIDE_INT end_gap
		    = (try_bitpos + try_size) - (info->bitpos + info->bitsize);
		  tree tem = info->ops[0].val;
		  if (TYPE_PRECISION (TREE_TYPE (tem)) <= info->bitsize)
		    {
		      tree bitfield_type
			= build_nonstandard_integer_type (info->bitsize,
							  UNSIGNED);
		      tem = gimple_convert (&seq, loc, bitfield_type, tem);
		    }
		  else if ((BYTES_BIG_ENDIAN ? start_gap : end_gap) > 0)
		    {
		      const unsigned HOST_WIDE_INT imask
			= (HOST_WIDE_INT_1U << info->bitsize) - 1;
		      tem = gimple_build (&seq, loc,
					  BIT_AND_EXPR, TREE_TYPE (tem), tem,
					  build_int_cst (TREE_TYPE (tem),
							 imask));
		    }
		  const HOST_WIDE_INT shift
		    = (BYTES_BIG_ENDIAN ? end_gap : start_gap);
		  if (shift < 0)
		    tem = gimple_build (&seq, loc,
					RSHIFT_EXPR, TREE_TYPE (tem), tem,
					build_int_cst (NULL_TREE, -shift));
		  tem = gimple_convert (&seq, loc, int_type, tem);
		  if (shift > 0)
		    tem = gimple_build (&seq, loc,
					LSHIFT_EXPR, int_type, tem,
					build_int_cst (NULL_TREE, shift));
		  src = gimple_build (&seq, loc,
				      BIT_IOR_EXPR, int_type, tem, src);
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
	       "New sequence of %u stores to replace old one of %u stores\n",
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
  unsigned HOST_WIDE_INT size;
  return (GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (rhs))).is_constant (&size)
	  && native_encode_expr (rhs, NULL, size) != 0);
}

/* If MEM is a memory reference usable for store merging (either as
   store destination or for loads), return the non-NULL base_addr
   and set *PBITSIZE, *PBITPOS, *PBITREGION_START and *PBITREGION_END.
   Otherwise return NULL, *PBITPOS should be still valid even for that
   case.  */

static tree
mem_valid_for_store_merging (tree mem, poly_uint64 *pbitsize,
			     poly_uint64 *pbitpos,
			     poly_uint64 *pbitregion_start,
			     poly_uint64 *pbitregion_end)
{
  poly_int64 bitsize, bitpos;
  poly_uint64 bitregion_start = 0, bitregion_end = 0;
  machine_mode mode;
  int unsignedp = 0, reversep = 0, volatilep = 0;
  tree offset;
  tree base_addr = get_inner_reference (mem, &bitsize, &bitpos, &offset, &mode,
					&unsignedp, &reversep, &volatilep);
  *pbitsize = bitsize;
  if (known_eq (bitsize, 0))
    return NULL_TREE;

  if (TREE_CODE (mem) == COMPONENT_REF
      && DECL_BIT_FIELD_TYPE (TREE_OPERAND (mem, 1)))
    {
      get_bit_range (&bitregion_start, &bitregion_end, mem, &bitpos, &offset);
      if (maybe_ne (bitregion_end, 0U))
	bitregion_end += 1;
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
      poly_offset_int byte_off = mem_ref_offset (base_addr);
      poly_offset_int bit_off = byte_off << LOG2_BITS_PER_UNIT;
      bit_off += bitpos;
      if (known_ge (bit_off, 0) && bit_off.to_shwi (&bitpos))
	{
	  if (maybe_ne (bitregion_end, 0U))
	    {
	      bit_off = byte_off << LOG2_BITS_PER_UNIT;
	      bit_off += bitregion_start;
	      if (bit_off.to_uhwi (&bitregion_start))
		{
		  bit_off = byte_off << LOG2_BITS_PER_UNIT;
		  bit_off += bitregion_end;
		  if (!bit_off.to_uhwi (&bitregion_end))
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
      if (maybe_lt (bitpos, 0))
	return NULL_TREE;
      base_addr = build_fold_addr_expr (base_addr);
    }

  if (known_eq (bitregion_end, 0U))
    {
      bitregion_start = round_down_to_byte_boundary (bitpos);
      bitregion_end = bitpos;
      bitregion_end = round_up_to_byte_boundary (bitregion_end + bitsize);
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
	      poly_uint64 bitsize, poly_uint64 bitpos,
	      poly_uint64 bitregion_start, poly_uint64 bitregion_end)
{
  if (!is_gimple_assign (stmt))
    return false;
  if (gimple_assign_rhs_code (stmt) == BIT_NOT_EXPR)
    {
      tree rhs1 = gimple_assign_rhs1 (stmt);
      if (TREE_CODE (rhs1) == SSA_NAME
	  && handled_load (SSA_NAME_DEF_STMT (rhs1), op, bitsize, bitpos,
			   bitregion_start, bitregion_end))
	{
	  /* Don't allow _1 = load; _2 = ~1; _3 = ~_2; which should have
	     been optimized earlier, but if allowed here, would confuse the
	     multiple uses counting.  */
	  if (op->bit_not_p)
	    return false;
	  op->bit_not_p = !op->bit_not_p;
	  return true;
	}
      return false;
    }
  if (gimple_vuse (stmt)
      && gimple_assign_load_p (stmt)
      && !stmt_can_throw_internal (stmt)
      && !gimple_has_volatile_ops (stmt))
    {
      tree mem = gimple_assign_rhs1 (stmt);
      op->base_addr
	= mem_valid_for_store_merging (mem, &op->bitsize, &op->bitpos,
				       &op->bitregion_start,
				       &op->bitregion_end);
      if (op->base_addr != NULL_TREE
	  && known_eq (op->bitsize, bitsize)
	  && multiple_p (op->bitpos - bitpos, BITS_PER_UNIT)
	  && known_ge (op->bitpos - op->bitregion_start,
		       bitpos - bitregion_start)
	  && known_ge (op->bitregion_end - op->bitpos,
		       bitregion_end - bitpos))
	{
	  op->stmt = stmt;
	  op->val = mem;
	  op->bit_not_p = false;
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
  poly_uint64 bitsize, bitpos;
  poly_uint64 bitregion_start, bitregion_end;
  tree base_addr
    = mem_valid_for_store_merging (lhs, &bitsize, &bitpos,
				   &bitregion_start, &bitregion_end);
  if (known_eq (bitsize, 0U))
    return;

  bool invalid = (base_addr == NULL_TREE
		  || (maybe_gt (bitsize,
				(unsigned int) MAX_BITSIZE_MODE_ANY_INT)
		      && (TREE_CODE (rhs) != INTEGER_CST)));
  enum tree_code rhs_code = ERROR_MARK;
  bool bit_not_p = false;
  struct symbolic_number n;
  gimple *ins_stmt = NULL;
  store_operand_info ops[2];
  if (invalid)
    ;
  else if (rhs_valid_for_store_merging_p (rhs))
    {
      rhs_code = INTEGER_CST;
      ops[0].val = rhs;
    }
  else if (TREE_CODE (rhs) != SSA_NAME)
    invalid = true;
  else
    {
      gimple *def_stmt = SSA_NAME_DEF_STMT (rhs), *def_stmt1, *def_stmt2;
      if (!is_gimple_assign (def_stmt))
	invalid = true;
      else if (handled_load (def_stmt, &ops[0], bitsize, bitpos,
			     bitregion_start, bitregion_end))
	rhs_code = MEM_REF;
      else if (gimple_assign_rhs_code (def_stmt) == BIT_NOT_EXPR) 
	{
	  tree rhs1 = gimple_assign_rhs1 (def_stmt);
	  if (TREE_CODE (rhs1) == SSA_NAME
	      && is_gimple_assign (SSA_NAME_DEF_STMT (rhs1)))
	    {
	      bit_not_p = true;
	      def_stmt = SSA_NAME_DEF_STMT (rhs1);
	    }
	}

      if (rhs_code == ERROR_MARK && !invalid)
	switch ((rhs_code = gimple_assign_rhs_code (def_stmt)))
	  {
	  case BIT_AND_EXPR:
	  case BIT_IOR_EXPR:
	  case BIT_XOR_EXPR:
	    tree rhs1, rhs2;
	    rhs1 = gimple_assign_rhs1 (def_stmt);
	    rhs2 = gimple_assign_rhs2 (def_stmt);
	    invalid = true;
	    if (TREE_CODE (rhs1) != SSA_NAME)
	      break;
	    def_stmt1 = SSA_NAME_DEF_STMT (rhs1);
	    if (!is_gimple_assign (def_stmt1)
		|| !handled_load (def_stmt1, &ops[0], bitsize, bitpos,
				  bitregion_start, bitregion_end))
	      break;
	    if (rhs_valid_for_store_merging_p (rhs2))
	      ops[1].val = rhs2;
	    else if (TREE_CODE (rhs2) != SSA_NAME)
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

      unsigned HOST_WIDE_INT const_bitsize;
      if (bitsize.is_constant (&const_bitsize)
	  && (const_bitsize % BITS_PER_UNIT) == 0
	  && const_bitsize <= 64
	  && multiple_p (bitpos, BITS_PER_UNIT))
	{
	  ins_stmt = find_bswap_or_nop_1 (def_stmt, &n, 12);
	  if (ins_stmt)
	    {
	      uint64_t nn = n.n;
	      for (unsigned HOST_WIDE_INT i = 0;
		   i < const_bitsize;
		   i += BITS_PER_UNIT, nn >>= BITS_PER_MARKER)
		if ((nn & MARKER_MASK) == 0
		    || (nn & MARKER_MASK) == MARKER_BYTE_UNKNOWN)
		  {
		    ins_stmt = NULL;
		    break;
		  }
	      if (ins_stmt)
		{
		  if (invalid)
		    {
		      rhs_code = LROTATE_EXPR;
		      ops[0].base_addr = NULL_TREE;
		      ops[1].base_addr = NULL_TREE;
		    }
		  invalid = false;
		}
	    }
	}

      if (invalid
	  && bitsize.is_constant (&const_bitsize)
	  && ((const_bitsize % BITS_PER_UNIT) != 0
	      || !multiple_p (bitpos, BITS_PER_UNIT))
	  && const_bitsize <= 64)
	{
	  /* Bypass a conversion to the bit-field type.  */
	  if (!bit_not_p
	      && is_gimple_assign (def_stmt)
	      && CONVERT_EXPR_CODE_P (rhs_code))
	    {
	      tree rhs1 = gimple_assign_rhs1 (def_stmt);
	      if (TREE_CODE (rhs1) == SSA_NAME
		  && INTEGRAL_TYPE_P (TREE_TYPE (rhs1)))
		rhs = rhs1;
	    }
	  rhs_code = BIT_INSERT_EXPR;
	  bit_not_p = false;
	  ops[0].val = rhs;
	  ops[0].base_addr = NULL_TREE;
	  ops[1].base_addr = NULL_TREE;
	  invalid = false;
	}
    }

  unsigned HOST_WIDE_INT const_bitsize, const_bitpos;
  unsigned HOST_WIDE_INT const_bitregion_start, const_bitregion_end;
  if (invalid
      || !bitsize.is_constant (&const_bitsize)
      || !bitpos.is_constant (&const_bitpos)
      || !bitregion_start.is_constant (&const_bitregion_start)
      || !bitregion_end.is_constant (&const_bitregion_end))
    {
      terminate_all_aliasing_chains (NULL, stmt);
      return;
    }

  if (!ins_stmt)
    memset (&n, 0, sizeof (n));

  struct imm_store_chain_info **chain_info = NULL;
  if (base_addr)
    chain_info = m_stores.get (base_addr);

  store_immediate_info *info;
  if (chain_info)
    {
      unsigned int ord = (*chain_info)->m_store_info.length ();
      info = new store_immediate_info (const_bitsize, const_bitpos,
				       const_bitregion_start,
				       const_bitregion_end,
				       stmt, ord, rhs_code, n, ins_stmt,
				       bit_not_p, ops[0], ops[1]);
      if (dump_file && (dump_flags & TDF_DETAILS))
	{
	  fprintf (dump_file, "Recording immediate store from stmt:\n");
	  print_gimple_stmt (dump_file, stmt, 0);
	}
      (*chain_info)->m_store_info.safe_push (info);
      terminate_all_aliasing_chains (chain_info, stmt);
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
  terminate_all_aliasing_chains (NULL, stmt);
  /* Start a new chain.  */
  struct imm_store_chain_info *new_chain
    = new imm_store_chain_info (m_stores_head, base_addr);
  info = new store_immediate_info (const_bitsize, const_bitpos,
				   const_bitregion_start,
				   const_bitregion_end,
				   stmt, 0, rhs_code, n, ins_stmt,
				   bit_not_p, ops[0], ops[1]);
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

  calculate_dominance_info (CDI_DOMINATORS);

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
