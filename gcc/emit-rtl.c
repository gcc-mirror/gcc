/* Emit RTL for the GCC expander.
   Copyright (C) 1987-2016 Free Software Foundation, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */


/* Middle-to-low level generation of rtx code and insns.

   This file contains support functions for creating rtl expressions
   and manipulating them in the doubly-linked chain of insns.

   The patterns of the insns are created by machine-dependent
   routines in insn-emit.c, which is generated automatically from
   the machine description.  These routines make the individual rtx's
   of the pattern with `gen_rtx_fmt_ee' and others in genrtl.[ch],
   which are automatically generated from rtl.def; what is machine
   dependent is the kind of rtx's they make and what arguments they
   use.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "df.h"
#include "tm_p.h"
#include "stringpool.h"
#include "insn-config.h"
#include "regs.h"
#include "emit-rtl.h"
#include "recog.h"
#include "diagnostic-core.h"
#include "alias.h"
#include "fold-const.h"
#include "varasm.h"
#include "cfgrtl.h"
#include "tree-eh.h"
#include "explow.h"
#include "expr.h"
#include "params.h"
#include "builtins.h"
#include "rtl-iter.h"
#include "stor-layout.h"
#include "opts.h"

struct target_rtl default_target_rtl;
#if SWITCHABLE_TARGET
struct target_rtl *this_target_rtl = &default_target_rtl;
#endif

#define initial_regno_reg_rtx (this_target_rtl->x_initial_regno_reg_rtx)

/* Commonly used modes.  */

machine_mode byte_mode;	/* Mode whose width is BITS_PER_UNIT.  */
machine_mode word_mode;	/* Mode whose width is BITS_PER_WORD.  */
machine_mode double_mode;	/* Mode whose width is DOUBLE_TYPE_SIZE.  */
machine_mode ptr_mode;	/* Mode whose width is POINTER_SIZE.  */

/* Datastructures maintained for currently processed function in RTL form.  */

struct rtl_data x_rtl;

/* Indexed by pseudo register number, gives the rtx for that pseudo.
   Allocated in parallel with regno_pointer_align.
   FIXME: We could put it into emit_status struct, but gengtype is not able to deal
   with length attribute nested in top level structures.  */

rtx * regno_reg_rtx;

/* This is *not* reset after each function.  It gives each CODE_LABEL
   in the entire compilation a unique label number.  */

static GTY(()) int label_num = 1;

/* We record floating-point CONST_DOUBLEs in each floating-point mode for
   the values of 0, 1, and 2.  For the integer entries and VOIDmode, we
   record a copy of const[012]_rtx and constm1_rtx.  CONSTM1_RTX
   is set only for MODE_INT and MODE_VECTOR_INT modes.  */

rtx const_tiny_rtx[4][(int) MAX_MACHINE_MODE];

rtx const_true_rtx;

REAL_VALUE_TYPE dconst0;
REAL_VALUE_TYPE dconst1;
REAL_VALUE_TYPE dconst2;
REAL_VALUE_TYPE dconstm1;
REAL_VALUE_TYPE dconsthalf;

/* Record fixed-point constant 0 and 1.  */
FIXED_VALUE_TYPE fconst0[MAX_FCONST0];
FIXED_VALUE_TYPE fconst1[MAX_FCONST1];

/* We make one copy of (const_int C) where C is in
   [- MAX_SAVED_CONST_INT, MAX_SAVED_CONST_INT]
   to save space during the compilation and simplify comparisons of
   integers.  */

rtx const_int_rtx[MAX_SAVED_CONST_INT * 2 + 1];

/* Standard pieces of rtx, to be substituted directly into things.  */
rtx pc_rtx;
rtx ret_rtx;
rtx simple_return_rtx;
rtx cc0_rtx;

/* Marker used for denoting an INSN, which should never be accessed (i.e.,
   this pointer should normally never be dereferenced), but is required to be
   distinct from NULL_RTX.  Currently used by peephole2 pass.  */
rtx_insn *invalid_insn_rtx;

/* A hash table storing CONST_INTs whose absolute value is greater
   than MAX_SAVED_CONST_INT.  */

struct const_int_hasher : ggc_cache_ptr_hash<rtx_def>
{
  typedef HOST_WIDE_INT compare_type;

  static hashval_t hash (rtx i);
  static bool equal (rtx i, HOST_WIDE_INT h);
};

static GTY ((cache)) hash_table<const_int_hasher> *const_int_htab;

struct const_wide_int_hasher : ggc_cache_ptr_hash<rtx_def>
{
  static hashval_t hash (rtx x);
  static bool equal (rtx x, rtx y);
};

static GTY ((cache)) hash_table<const_wide_int_hasher> *const_wide_int_htab;

/* A hash table storing register attribute structures.  */
struct reg_attr_hasher : ggc_cache_ptr_hash<reg_attrs>
{
  static hashval_t hash (reg_attrs *x);
  static bool equal (reg_attrs *a, reg_attrs *b);
};

static GTY ((cache)) hash_table<reg_attr_hasher> *reg_attrs_htab;

/* A hash table storing all CONST_DOUBLEs.  */
struct const_double_hasher : ggc_cache_ptr_hash<rtx_def>
{
  static hashval_t hash (rtx x);
  static bool equal (rtx x, rtx y);
};

static GTY ((cache)) hash_table<const_double_hasher> *const_double_htab;

/* A hash table storing all CONST_FIXEDs.  */
struct const_fixed_hasher : ggc_cache_ptr_hash<rtx_def>
{
  static hashval_t hash (rtx x);
  static bool equal (rtx x, rtx y);
};

static GTY ((cache)) hash_table<const_fixed_hasher> *const_fixed_htab;

#define cur_insn_uid (crtl->emit.x_cur_insn_uid)
#define cur_debug_insn_uid (crtl->emit.x_cur_debug_insn_uid)
#define first_label_num (crtl->emit.x_first_label_num)

static void set_used_decls (tree);
static void mark_label_nuses (rtx);
#if TARGET_SUPPORTS_WIDE_INT
static rtx lookup_const_wide_int (rtx);
#endif
static rtx lookup_const_double (rtx);
static rtx lookup_const_fixed (rtx);
static reg_attrs *get_reg_attrs (tree, int);
static rtx gen_const_vector (machine_mode, int);
static void copy_rtx_if_shared_1 (rtx *orig);

/* Probability of the conditional branch currently proceeded by try_split.
   Set to -1 otherwise.  */
int split_branch_probability = -1;

/* Returns a hash code for X (which is a really a CONST_INT).  */

hashval_t
const_int_hasher::hash (rtx x)
{
  return (hashval_t) INTVAL (x);
}

/* Returns nonzero if the value represented by X (which is really a
   CONST_INT) is the same as that given by Y (which is really a
   HOST_WIDE_INT *).  */

bool
const_int_hasher::equal (rtx x, HOST_WIDE_INT y)
{
  return (INTVAL (x) == y);
}

#if TARGET_SUPPORTS_WIDE_INT
/* Returns a hash code for X (which is a really a CONST_WIDE_INT).  */

hashval_t
const_wide_int_hasher::hash (rtx x)
{
  int i;
  unsigned HOST_WIDE_INT hash = 0;
  const_rtx xr = x;

  for (i = 0; i < CONST_WIDE_INT_NUNITS (xr); i++)
    hash += CONST_WIDE_INT_ELT (xr, i);

  return (hashval_t) hash;
}

/* Returns nonzero if the value represented by X (which is really a
   CONST_WIDE_INT) is the same as that given by Y (which is really a
   CONST_WIDE_INT).  */

bool
const_wide_int_hasher::equal (rtx x, rtx y)
{
  int i;
  const_rtx xr = x;
  const_rtx yr = y;
  if (CONST_WIDE_INT_NUNITS (xr) != CONST_WIDE_INT_NUNITS (yr))
    return false;

  for (i = 0; i < CONST_WIDE_INT_NUNITS (xr); i++)
    if (CONST_WIDE_INT_ELT (xr, i) != CONST_WIDE_INT_ELT (yr, i))
      return false;

  return true;
}
#endif

/* Returns a hash code for X (which is really a CONST_DOUBLE).  */
hashval_t
const_double_hasher::hash (rtx x)
{
  const_rtx const value = x;
  hashval_t h;

  if (TARGET_SUPPORTS_WIDE_INT == 0 && GET_MODE (value) == VOIDmode)
    h = CONST_DOUBLE_LOW (value) ^ CONST_DOUBLE_HIGH (value);
  else
    {
      h = real_hash (CONST_DOUBLE_REAL_VALUE (value));
      /* MODE is used in the comparison, so it should be in the hash.  */
      h ^= GET_MODE (value);
    }
  return h;
}

/* Returns nonzero if the value represented by X (really a ...)
   is the same as that represented by Y (really a ...) */
bool
const_double_hasher::equal (rtx x, rtx y)
{
  const_rtx const a = x, b = y;

  if (GET_MODE (a) != GET_MODE (b))
    return 0;
  if (TARGET_SUPPORTS_WIDE_INT == 0 && GET_MODE (a) == VOIDmode)
    return (CONST_DOUBLE_LOW (a) == CONST_DOUBLE_LOW (b)
	    && CONST_DOUBLE_HIGH (a) == CONST_DOUBLE_HIGH (b));
  else
    return real_identical (CONST_DOUBLE_REAL_VALUE (a),
			   CONST_DOUBLE_REAL_VALUE (b));
}

/* Returns a hash code for X (which is really a CONST_FIXED).  */

hashval_t
const_fixed_hasher::hash (rtx x)
{
  const_rtx const value = x;
  hashval_t h;

  h = fixed_hash (CONST_FIXED_VALUE (value));
  /* MODE is used in the comparison, so it should be in the hash.  */
  h ^= GET_MODE (value);
  return h;
}

/* Returns nonzero if the value represented by X is the same as that
   represented by Y.  */

bool
const_fixed_hasher::equal (rtx x, rtx y)
{
  const_rtx const a = x, b = y;

  if (GET_MODE (a) != GET_MODE (b))
    return 0;
  return fixed_identical (CONST_FIXED_VALUE (a), CONST_FIXED_VALUE (b));
}

/* Return true if the given memory attributes are equal.  */

bool
mem_attrs_eq_p (const struct mem_attrs *p, const struct mem_attrs *q)
{
  if (p == q)
    return true;
  if (!p || !q)
    return false;
  return (p->alias == q->alias
	  && p->offset_known_p == q->offset_known_p
	  && (!p->offset_known_p || p->offset == q->offset)
	  && p->size_known_p == q->size_known_p
	  && (!p->size_known_p || p->size == q->size)
	  && p->align == q->align
	  && p->addrspace == q->addrspace
	  && (p->expr == q->expr
	      || (p->expr != NULL_TREE && q->expr != NULL_TREE
		  && operand_equal_p (p->expr, q->expr, 0))));
}

/* Set MEM's memory attributes so that they are the same as ATTRS.  */

static void
set_mem_attrs (rtx mem, mem_attrs *attrs)
{
  /* If everything is the default, we can just clear the attributes.  */
  if (mem_attrs_eq_p (attrs, mode_mem_attrs[(int) GET_MODE (mem)]))
    {
      MEM_ATTRS (mem) = 0;
      return;
    }

  if (!MEM_ATTRS (mem)
      || !mem_attrs_eq_p (attrs, MEM_ATTRS (mem)))
    {
      MEM_ATTRS (mem) = ggc_alloc<mem_attrs> ();
      memcpy (MEM_ATTRS (mem), attrs, sizeof (mem_attrs));
    }
}

/* Returns a hash code for X (which is a really a reg_attrs *).  */

hashval_t
reg_attr_hasher::hash (reg_attrs *x)
{
  const reg_attrs *const p = x;

  return ((p->offset * 1000) ^ (intptr_t) p->decl);
}

/* Returns nonzero if the value represented by X  is the same as that given by
   Y.  */

bool
reg_attr_hasher::equal (reg_attrs *x, reg_attrs *y)
{
  const reg_attrs *const p = x;
  const reg_attrs *const q = y;

  return (p->decl == q->decl && p->offset == q->offset);
}
/* Allocate a new reg_attrs structure and insert it into the hash table if
   one identical to it is not already in the table.  We are doing this for
   MEM of mode MODE.  */

static reg_attrs *
get_reg_attrs (tree decl, int offset)
{
  reg_attrs attrs;

  /* If everything is the default, we can just return zero.  */
  if (decl == 0 && offset == 0)
    return 0;

  attrs.decl = decl;
  attrs.offset = offset;

  reg_attrs **slot = reg_attrs_htab->find_slot (&attrs, INSERT);
  if (*slot == 0)
    {
      *slot = ggc_alloc<reg_attrs> ();
      memcpy (*slot, &attrs, sizeof (reg_attrs));
    }

  return *slot;
}


#if !HAVE_blockage
/* Generate an empty ASM_INPUT, which is used to block attempts to schedule,
   and to block register equivalences to be seen across this insn.  */

rtx
gen_blockage (void)
{
  rtx x = gen_rtx_ASM_INPUT (VOIDmode, "");
  MEM_VOLATILE_P (x) = true;
  return x;
}
#endif


/* Set the mode and register number of X to MODE and REGNO.  */

void
set_mode_and_regno (rtx x, machine_mode mode, unsigned int regno)
{
  unsigned int nregs = (HARD_REGISTER_NUM_P (regno)
			? hard_regno_nregs[regno][mode]
			: 1);
  PUT_MODE_RAW (x, mode);
  set_regno_raw (x, regno, nregs);
}

/* Generate a new REG rtx.  Make sure ORIGINAL_REGNO is set properly, and
   don't attempt to share with the various global pieces of rtl (such as
   frame_pointer_rtx).  */

rtx
gen_raw_REG (machine_mode mode, unsigned int regno)
{
  rtx x = rtx_alloc_stat (REG MEM_STAT_INFO);
  set_mode_and_regno (x, mode, regno);
  REG_ATTRS (x) = NULL;
  ORIGINAL_REGNO (x) = regno;
  return x;
}

/* There are some RTL codes that require special attention; the generation
   functions do the raw handling.  If you add to this list, modify
   special_rtx in gengenrtl.c as well.  */

rtx_expr_list *
gen_rtx_EXPR_LIST (machine_mode mode, rtx expr, rtx expr_list)
{
  return as_a <rtx_expr_list *> (gen_rtx_fmt_ee (EXPR_LIST, mode, expr,
						 expr_list));
}

rtx_insn_list *
gen_rtx_INSN_LIST (machine_mode mode, rtx insn, rtx insn_list)
{
  return as_a <rtx_insn_list *> (gen_rtx_fmt_ue (INSN_LIST, mode, insn,
						 insn_list));
}

rtx_insn *
gen_rtx_INSN (machine_mode mode, rtx_insn *prev_insn, rtx_insn *next_insn,
	      basic_block bb, rtx pattern, int location, int code,
	      rtx reg_notes)
{
  return as_a <rtx_insn *> (gen_rtx_fmt_uuBeiie (INSN, mode,
						 prev_insn, next_insn,
						 bb, pattern, location, code,
						 reg_notes));
}

rtx
gen_rtx_CONST_INT (machine_mode mode ATTRIBUTE_UNUSED, HOST_WIDE_INT arg)
{
  if (arg >= - MAX_SAVED_CONST_INT && arg <= MAX_SAVED_CONST_INT)
    return const_int_rtx[arg + MAX_SAVED_CONST_INT];

#if STORE_FLAG_VALUE != 1 && STORE_FLAG_VALUE != -1
  if (const_true_rtx && arg == STORE_FLAG_VALUE)
    return const_true_rtx;
#endif

  /* Look up the CONST_INT in the hash table.  */
  rtx *slot = const_int_htab->find_slot_with_hash (arg, (hashval_t) arg,
						   INSERT);
  if (*slot == 0)
    *slot = gen_rtx_raw_CONST_INT (VOIDmode, arg);

  return *slot;
}

rtx
gen_int_mode (HOST_WIDE_INT c, machine_mode mode)
{
  return GEN_INT (trunc_int_for_mode (c, mode));
}

/* CONST_DOUBLEs might be created from pairs of integers, or from
   REAL_VALUE_TYPEs.  Also, their length is known only at run time,
   so we cannot use gen_rtx_raw_CONST_DOUBLE.  */

/* Determine whether REAL, a CONST_DOUBLE, already exists in the
   hash table.  If so, return its counterpart; otherwise add it
   to the hash table and return it.  */
static rtx
lookup_const_double (rtx real)
{
  rtx *slot = const_double_htab->find_slot (real, INSERT);
  if (*slot == 0)
    *slot = real;

  return *slot;
}

/* Return a CONST_DOUBLE rtx for a floating-point value specified by
   VALUE in mode MODE.  */
rtx
const_double_from_real_value (REAL_VALUE_TYPE value, machine_mode mode)
{
  rtx real = rtx_alloc (CONST_DOUBLE);
  PUT_MODE (real, mode);

  real->u.rv = value;

  return lookup_const_double (real);
}

/* Determine whether FIXED, a CONST_FIXED, already exists in the
   hash table.  If so, return its counterpart; otherwise add it
   to the hash table and return it.  */

static rtx
lookup_const_fixed (rtx fixed)
{
  rtx *slot = const_fixed_htab->find_slot (fixed, INSERT);
  if (*slot == 0)
    *slot = fixed;

  return *slot;
}

/* Return a CONST_FIXED rtx for a fixed-point value specified by
   VALUE in mode MODE.  */

rtx
const_fixed_from_fixed_value (FIXED_VALUE_TYPE value, machine_mode mode)
{
  rtx fixed = rtx_alloc (CONST_FIXED);
  PUT_MODE (fixed, mode);

  fixed->u.fv = value;

  return lookup_const_fixed (fixed);
}

#if TARGET_SUPPORTS_WIDE_INT == 0
/* Constructs double_int from rtx CST.  */

double_int
rtx_to_double_int (const_rtx cst)
{
  double_int r;

  if (CONST_INT_P (cst))
      r = double_int::from_shwi (INTVAL (cst));
  else if (CONST_DOUBLE_AS_INT_P (cst))
    {
      r.low = CONST_DOUBLE_LOW (cst);
      r.high = CONST_DOUBLE_HIGH (cst);
    }
  else
    gcc_unreachable ();
  
  return r;
}
#endif

#if TARGET_SUPPORTS_WIDE_INT
/* Determine whether CONST_WIDE_INT WINT already exists in the hash table.
   If so, return its counterpart; otherwise add it to the hash table and
   return it.  */

static rtx
lookup_const_wide_int (rtx wint)
{
  rtx *slot = const_wide_int_htab->find_slot (wint, INSERT);
  if (*slot == 0)
    *slot = wint;

  return *slot;
}
#endif

/* Return an rtx constant for V, given that the constant has mode MODE.
   The returned rtx will be a CONST_INT if V fits, otherwise it will be
   a CONST_DOUBLE (if !TARGET_SUPPORTS_WIDE_INT) or a CONST_WIDE_INT
   (if TARGET_SUPPORTS_WIDE_INT).  */

rtx
immed_wide_int_const (const wide_int_ref &v, machine_mode mode)
{
  unsigned int len = v.get_len ();
  unsigned int prec = GET_MODE_PRECISION (mode);

  /* Allow truncation but not extension since we do not know if the
     number is signed or unsigned.  */
  gcc_assert (prec <= v.get_precision ());

  if (len < 2 || prec <= HOST_BITS_PER_WIDE_INT)
    return gen_int_mode (v.elt (0), mode);

#if TARGET_SUPPORTS_WIDE_INT
  {
    unsigned int i;
    rtx value;
    unsigned int blocks_needed
      = (prec + HOST_BITS_PER_WIDE_INT - 1) / HOST_BITS_PER_WIDE_INT;

    if (len > blocks_needed)
      len = blocks_needed;

    value = const_wide_int_alloc (len);

    /* It is so tempting to just put the mode in here.  Must control
       myself ... */
    PUT_MODE (value, VOIDmode);
    CWI_PUT_NUM_ELEM (value, len);

    for (i = 0; i < len; i++)
      CONST_WIDE_INT_ELT (value, i) = v.elt (i);

    return lookup_const_wide_int (value);
  }
#else
  return immed_double_const (v.elt (0), v.elt (1), mode);
#endif
}

#if TARGET_SUPPORTS_WIDE_INT == 0
/* Return a CONST_DOUBLE or CONST_INT for a value specified as a pair
   of ints: I0 is the low-order word and I1 is the high-order word.
   For values that are larger than HOST_BITS_PER_DOUBLE_INT, the
   implied upper bits are copies of the high bit of i1.  The value
   itself is neither signed nor unsigned.  Do not use this routine for
   non-integer modes; convert to REAL_VALUE_TYPE and use
   const_double_from_real_value.  */

rtx
immed_double_const (HOST_WIDE_INT i0, HOST_WIDE_INT i1, machine_mode mode)
{
  rtx value;
  unsigned int i;

  /* There are the following cases (note that there are no modes with
     HOST_BITS_PER_WIDE_INT < GET_MODE_BITSIZE (mode) < HOST_BITS_PER_DOUBLE_INT):

     1) If GET_MODE_BITSIZE (mode) <= HOST_BITS_PER_WIDE_INT, then we use
	gen_int_mode.
     2) If the value of the integer fits into HOST_WIDE_INT anyway
        (i.e., i1 consists only from copies of the sign bit, and sign
	of i0 and i1 are the same), then we return a CONST_INT for i0.
     3) Otherwise, we create a CONST_DOUBLE for i0 and i1.  */
  if (mode != VOIDmode)
    {
      gcc_assert (GET_MODE_CLASS (mode) == MODE_INT
		  || GET_MODE_CLASS (mode) == MODE_PARTIAL_INT
		  /* We can get a 0 for an error mark.  */
		  || GET_MODE_CLASS (mode) == MODE_VECTOR_INT
		  || GET_MODE_CLASS (mode) == MODE_VECTOR_FLOAT
		  || GET_MODE_CLASS (mode) == MODE_POINTER_BOUNDS);

      if (GET_MODE_BITSIZE (mode) <= HOST_BITS_PER_WIDE_INT)
	return gen_int_mode (i0, mode);
    }

  /* If this integer fits in one word, return a CONST_INT.  */
  if ((i1 == 0 && i0 >= 0) || (i1 == ~0 && i0 < 0))
    return GEN_INT (i0);

  /* We use VOIDmode for integers.  */
  value = rtx_alloc (CONST_DOUBLE);
  PUT_MODE (value, VOIDmode);

  CONST_DOUBLE_LOW (value) = i0;
  CONST_DOUBLE_HIGH (value) = i1;

  for (i = 2; i < (sizeof CONST_DOUBLE_FORMAT - 1); i++)
    XWINT (value, i) = 0;

  return lookup_const_double (value);
}
#endif

rtx
gen_rtx_REG (machine_mode mode, unsigned int regno)
{
  /* In case the MD file explicitly references the frame pointer, have
     all such references point to the same frame pointer.  This is
     used during frame pointer elimination to distinguish the explicit
     references to these registers from pseudos that happened to be
     assigned to them.

     If we have eliminated the frame pointer or arg pointer, we will
     be using it as a normal register, for example as a spill
     register.  In such cases, we might be accessing it in a mode that
     is not Pmode and therefore cannot use the pre-allocated rtx.

     Also don't do this when we are making new REGs in reload, since
     we don't want to get confused with the real pointers.  */

  if (mode == Pmode && !reload_in_progress && !lra_in_progress)
    {
      if (regno == FRAME_POINTER_REGNUM
	  && (!reload_completed || frame_pointer_needed))
	return frame_pointer_rtx;

      if (!HARD_FRAME_POINTER_IS_FRAME_POINTER
	  && regno == HARD_FRAME_POINTER_REGNUM
	  && (!reload_completed || frame_pointer_needed))
	return hard_frame_pointer_rtx;
#if !HARD_FRAME_POINTER_IS_ARG_POINTER
      if (FRAME_POINTER_REGNUM != ARG_POINTER_REGNUM
	  && regno == ARG_POINTER_REGNUM)
	return arg_pointer_rtx;
#endif
#ifdef RETURN_ADDRESS_POINTER_REGNUM
      if (regno == RETURN_ADDRESS_POINTER_REGNUM)
	return return_address_pointer_rtx;
#endif
      if (regno == (unsigned) PIC_OFFSET_TABLE_REGNUM
	  && PIC_OFFSET_TABLE_REGNUM != INVALID_REGNUM
	  && fixed_regs[PIC_OFFSET_TABLE_REGNUM])
	return pic_offset_table_rtx;
      if (regno == STACK_POINTER_REGNUM)
	return stack_pointer_rtx;
    }

#if 0
  /* If the per-function register table has been set up, try to re-use
     an existing entry in that table to avoid useless generation of RTL.

     This code is disabled for now until we can fix the various backends
     which depend on having non-shared hard registers in some cases.   Long
     term we want to re-enable this code as it can significantly cut down
     on the amount of useless RTL that gets generated.

     We'll also need to fix some code that runs after reload that wants to
     set ORIGINAL_REGNO.  */

  if (cfun
      && cfun->emit
      && regno_reg_rtx
      && regno < FIRST_PSEUDO_REGISTER
      && reg_raw_mode[regno] == mode)
    return regno_reg_rtx[regno];
#endif

  return gen_raw_REG (mode, regno);
}

rtx
gen_rtx_MEM (machine_mode mode, rtx addr)
{
  rtx rt = gen_rtx_raw_MEM (mode, addr);

  /* This field is not cleared by the mere allocation of the rtx, so
     we clear it here.  */
  MEM_ATTRS (rt) = 0;

  return rt;
}

/* Generate a memory referring to non-trapping constant memory.  */

rtx
gen_const_mem (machine_mode mode, rtx addr)
{
  rtx mem = gen_rtx_MEM (mode, addr);
  MEM_READONLY_P (mem) = 1;
  MEM_NOTRAP_P (mem) = 1;
  return mem;
}

/* Generate a MEM referring to fixed portions of the frame, e.g., register
   save areas.  */

rtx
gen_frame_mem (machine_mode mode, rtx addr)
{
  rtx mem = gen_rtx_MEM (mode, addr);
  MEM_NOTRAP_P (mem) = 1;
  set_mem_alias_set (mem, get_frame_alias_set ());
  return mem;
}

/* Generate a MEM referring to a temporary use of the stack, not part
    of the fixed stack frame.  For example, something which is pushed
    by a target splitter.  */
rtx
gen_tmp_stack_mem (machine_mode mode, rtx addr)
{
  rtx mem = gen_rtx_MEM (mode, addr);
  MEM_NOTRAP_P (mem) = 1;
  if (!cfun->calls_alloca)
    set_mem_alias_set (mem, get_frame_alias_set ());
  return mem;
}

/* We want to create (subreg:OMODE (obj:IMODE) OFFSET).  Return true if
   this construct would be valid, and false otherwise.  */

bool
validate_subreg (machine_mode omode, machine_mode imode,
		 const_rtx reg, unsigned int offset)
{
  unsigned int isize = GET_MODE_SIZE (imode);
  unsigned int osize = GET_MODE_SIZE (omode);

  /* All subregs must be aligned.  */
  if (offset % osize != 0)
    return false;

  /* The subreg offset cannot be outside the inner object.  */
  if (offset >= isize)
    return false;

  /* ??? This should not be here.  Temporarily continue to allow word_mode
     subregs of anything.  The most common offender is (subreg:SI (reg:DF)).
     Generally, backends are doing something sketchy but it'll take time to
     fix them all.  */
  if (omode == word_mode)
    ;
  /* ??? Similarly, e.g. with (subreg:DF (reg:TI)).  Though store_bit_field
     is the culprit here, and not the backends.  */
  else if (osize >= UNITS_PER_WORD && isize >= osize)
    ;
  /* Allow component subregs of complex and vector.  Though given the below
     extraction rules, it's not always clear what that means.  */
  else if ((COMPLEX_MODE_P (imode) || VECTOR_MODE_P (imode))
	   && GET_MODE_INNER (imode) == omode)
    ;
  /* ??? x86 sse code makes heavy use of *paradoxical* vector subregs,
     i.e. (subreg:V4SF (reg:SF) 0).  This surely isn't the cleanest way to
     represent this.  It's questionable if this ought to be represented at
     all -- why can't this all be hidden in post-reload splitters that make
     arbitrarily mode changes to the registers themselves.  */
  else if (VECTOR_MODE_P (omode) && GET_MODE_INNER (omode) == imode)
    ;
  /* Subregs involving floating point modes are not allowed to
     change size.  Therefore (subreg:DI (reg:DF) 0) is fine, but
     (subreg:SI (reg:DF) 0) isn't.  */
  else if (FLOAT_MODE_P (imode) || FLOAT_MODE_P (omode))
    {
      if (! (isize == osize
	     /* LRA can use subreg to store a floating point value in
		an integer mode.  Although the floating point and the
		integer modes need the same number of hard registers,
		the size of floating point mode can be less than the
		integer mode.  LRA also uses subregs for a register
		should be used in different mode in on insn.  */
	     || lra_in_progress))
	return false;
    }

  /* Paradoxical subregs must have offset zero.  */
  if (osize > isize)
    return offset == 0;

  /* This is a normal subreg.  Verify that the offset is representable.  */

  /* For hard registers, we already have most of these rules collected in
     subreg_offset_representable_p.  */
  if (reg && REG_P (reg) && HARD_REGISTER_P (reg))
    {
      unsigned int regno = REGNO (reg);

#ifdef CANNOT_CHANGE_MODE_CLASS
      if ((COMPLEX_MODE_P (imode) || VECTOR_MODE_P (imode))
	  && GET_MODE_INNER (imode) == omode)
	;
      else if (REG_CANNOT_CHANGE_MODE_P (regno, imode, omode))
	return false;
#endif

      return subreg_offset_representable_p (regno, imode, offset, omode);
    }

  /* For pseudo registers, we want most of the same checks.  Namely:
     If the register no larger than a word, the subreg must be lowpart.
     If the register is larger than a word, the subreg must be the lowpart
     of a subword.  A subreg does *not* perform arbitrary bit extraction.
     Given that we've already checked mode/offset alignment, we only have
     to check subword subregs here.  */
  if (osize < UNITS_PER_WORD
      && ! (lra_in_progress && (FLOAT_MODE_P (imode) || FLOAT_MODE_P (omode))))
    {
      machine_mode wmode = isize > UNITS_PER_WORD ? word_mode : imode;
      unsigned int low_off = subreg_lowpart_offset (omode, wmode);
      if (offset % UNITS_PER_WORD != low_off)
	return false;
    }
  return true;
}

rtx
gen_rtx_SUBREG (machine_mode mode, rtx reg, int offset)
{
  gcc_assert (validate_subreg (mode, GET_MODE (reg), reg, offset));
  return gen_rtx_raw_SUBREG (mode, reg, offset);
}

/* Generate a SUBREG representing the least-significant part of REG if MODE
   is smaller than mode of REG, otherwise paradoxical SUBREG.  */

rtx
gen_lowpart_SUBREG (machine_mode mode, rtx reg)
{
  machine_mode inmode;

  inmode = GET_MODE (reg);
  if (inmode == VOIDmode)
    inmode = mode;
  return gen_rtx_SUBREG (mode, reg,
			 subreg_lowpart_offset (mode, inmode));
}

rtx
gen_rtx_VAR_LOCATION (machine_mode mode, tree decl, rtx loc,
		      enum var_init_status status)
{
  rtx x = gen_rtx_fmt_te (VAR_LOCATION, mode, decl, loc);
  PAT_VAR_LOCATION_STATUS (x) = status;
  return x;
}


/* Create an rtvec and stores within it the RTXen passed in the arguments.  */

rtvec
gen_rtvec (int n, ...)
{
  int i;
  rtvec rt_val;
  va_list p;

  va_start (p, n);

  /* Don't allocate an empty rtvec...  */
  if (n == 0)
    {
      va_end (p);
      return NULL_RTVEC;
    }

  rt_val = rtvec_alloc (n);

  for (i = 0; i < n; i++)
    rt_val->elem[i] = va_arg (p, rtx);

  va_end (p);
  return rt_val;
}

rtvec
gen_rtvec_v (int n, rtx *argp)
{
  int i;
  rtvec rt_val;

  /* Don't allocate an empty rtvec...  */
  if (n == 0)
    return NULL_RTVEC;

  rt_val = rtvec_alloc (n);

  for (i = 0; i < n; i++)
    rt_val->elem[i] = *argp++;

  return rt_val;
}

rtvec
gen_rtvec_v (int n, rtx_insn **argp)
{
  int i;
  rtvec rt_val;

  /* Don't allocate an empty rtvec...  */
  if (n == 0)
    return NULL_RTVEC;

  rt_val = rtvec_alloc (n);

  for (i = 0; i < n; i++)
    rt_val->elem[i] = *argp++;

  return rt_val;
}


/* Return the number of bytes between the start of an OUTER_MODE
   in-memory value and the start of an INNER_MODE in-memory value,
   given that the former is a lowpart of the latter.  It may be a
   paradoxical lowpart, in which case the offset will be negative
   on big-endian targets.  */

int
byte_lowpart_offset (machine_mode outer_mode,
		     machine_mode inner_mode)
{
  if (GET_MODE_SIZE (outer_mode) < GET_MODE_SIZE (inner_mode))
    return subreg_lowpart_offset (outer_mode, inner_mode);
  else
    return -subreg_lowpart_offset (inner_mode, outer_mode);
}

/* Generate a REG rtx for a new pseudo register of mode MODE.
   This pseudo is assigned the next sequential register number.  */

rtx
gen_reg_rtx (machine_mode mode)
{
  rtx val;
  unsigned int align = GET_MODE_ALIGNMENT (mode);

  gcc_assert (can_create_pseudo_p ());

  /* If a virtual register with bigger mode alignment is generated,
     increase stack alignment estimation because it might be spilled
     to stack later.  */
  if (SUPPORTS_STACK_ALIGNMENT
      && crtl->stack_alignment_estimated < align
      && !crtl->stack_realign_processed)
    {
      unsigned int min_align = MINIMUM_ALIGNMENT (NULL, mode, align);
      if (crtl->stack_alignment_estimated < min_align)
	crtl->stack_alignment_estimated = min_align;
    }

  if (generating_concat_p
      && (GET_MODE_CLASS (mode) == MODE_COMPLEX_FLOAT
	  || GET_MODE_CLASS (mode) == MODE_COMPLEX_INT))
    {
      /* For complex modes, don't make a single pseudo.
	 Instead, make a CONCAT of two pseudos.
	 This allows noncontiguous allocation of the real and imaginary parts,
	 which makes much better code.  Besides, allocating DCmode
	 pseudos overstrains reload on some machines like the 386.  */
      rtx realpart, imagpart;
      machine_mode partmode = GET_MODE_INNER (mode);

      realpart = gen_reg_rtx (partmode);
      imagpart = gen_reg_rtx (partmode);
      return gen_rtx_CONCAT (mode, realpart, imagpart);
    }

  /* Do not call gen_reg_rtx with uninitialized crtl.  */
  gcc_assert (crtl->emit.regno_pointer_align_length);

  /* Make sure regno_pointer_align, and regno_reg_rtx are large
     enough to have an element for this pseudo reg number.  */

  if (reg_rtx_no == crtl->emit.regno_pointer_align_length)
    {
      int old_size = crtl->emit.regno_pointer_align_length;
      char *tmp;
      rtx *new1;

      tmp = XRESIZEVEC (char, crtl->emit.regno_pointer_align, old_size * 2);
      memset (tmp + old_size, 0, old_size);
      crtl->emit.regno_pointer_align = (unsigned char *) tmp;

      new1 = GGC_RESIZEVEC (rtx, regno_reg_rtx, old_size * 2);
      memset (new1 + old_size, 0, old_size * sizeof (rtx));
      regno_reg_rtx = new1;

      crtl->emit.regno_pointer_align_length = old_size * 2;
    }

  val = gen_raw_REG (mode, reg_rtx_no);
  regno_reg_rtx[reg_rtx_no++] = val;
  return val;
}

/* Return TRUE if REG is a PARM_DECL, FALSE otherwise.  */

bool
reg_is_parm_p (rtx reg)
{
  tree decl;

  gcc_assert (REG_P (reg));
  decl = REG_EXPR (reg);
  return (decl && TREE_CODE (decl) == PARM_DECL);
}

/* Update NEW with the same attributes as REG, but with OFFSET added
   to the REG_OFFSET.  */

static void
update_reg_offset (rtx new_rtx, rtx reg, int offset)
{
  REG_ATTRS (new_rtx) = get_reg_attrs (REG_EXPR (reg),
				   REG_OFFSET (reg) + offset);
}

/* Generate a register with same attributes as REG, but with OFFSET
   added to the REG_OFFSET.  */

rtx
gen_rtx_REG_offset (rtx reg, machine_mode mode, unsigned int regno,
		    int offset)
{
  rtx new_rtx = gen_rtx_REG (mode, regno);

  update_reg_offset (new_rtx, reg, offset);
  return new_rtx;
}

/* Generate a new pseudo-register with the same attributes as REG, but
   with OFFSET added to the REG_OFFSET.  */

rtx
gen_reg_rtx_offset (rtx reg, machine_mode mode, int offset)
{
  rtx new_rtx = gen_reg_rtx (mode);

  update_reg_offset (new_rtx, reg, offset);
  return new_rtx;
}

/* Adjust REG in-place so that it has mode MODE.  It is assumed that the
   new register is a (possibly paradoxical) lowpart of the old one.  */

void
adjust_reg_mode (rtx reg, machine_mode mode)
{
  update_reg_offset (reg, reg, byte_lowpart_offset (mode, GET_MODE (reg)));
  PUT_MODE (reg, mode);
}

/* Copy REG's attributes from X, if X has any attributes.  If REG and X
   have different modes, REG is a (possibly paradoxical) lowpart of X.  */

void
set_reg_attrs_from_value (rtx reg, rtx x)
{
  int offset;
  bool can_be_reg_pointer = true;

  /* Don't call mark_reg_pointer for incompatible pointer sign
     extension.  */
  while (GET_CODE (x) == SIGN_EXTEND
	 || GET_CODE (x) == ZERO_EXTEND
	 || GET_CODE (x) == TRUNCATE
	 || (GET_CODE (x) == SUBREG && subreg_lowpart_p (x)))
    {
#if defined(POINTERS_EXTEND_UNSIGNED)
      if (((GET_CODE (x) == SIGN_EXTEND && POINTERS_EXTEND_UNSIGNED)
	   || (GET_CODE (x) == ZERO_EXTEND && ! POINTERS_EXTEND_UNSIGNED)
	   || (paradoxical_subreg_p (x)
	       && ! (SUBREG_PROMOTED_VAR_P (x)
		     && SUBREG_CHECK_PROMOTED_SIGN (x,
						    POINTERS_EXTEND_UNSIGNED))))
	  && !targetm.have_ptr_extend ())
	can_be_reg_pointer = false;
#endif
      x = XEXP (x, 0);
    }

  /* Hard registers can be reused for multiple purposes within the same
     function, so setting REG_ATTRS, REG_POINTER and REG_POINTER_ALIGN
     on them is wrong.  */
  if (HARD_REGISTER_P (reg))
    return;

  offset = byte_lowpart_offset (GET_MODE (reg), GET_MODE (x));
  if (MEM_P (x))
    {
      if (MEM_OFFSET_KNOWN_P (x))
	REG_ATTRS (reg) = get_reg_attrs (MEM_EXPR (x),
					 MEM_OFFSET (x) + offset);
      if (can_be_reg_pointer && MEM_POINTER (x))
	mark_reg_pointer (reg, 0);
    }
  else if (REG_P (x))
    {
      if (REG_ATTRS (x))
	update_reg_offset (reg, x, offset);
      if (can_be_reg_pointer && REG_POINTER (x))
	mark_reg_pointer (reg, REGNO_POINTER_ALIGN (REGNO (x)));
    }
}

/* Generate a REG rtx for a new pseudo register, copying the mode
   and attributes from X.  */

rtx
gen_reg_rtx_and_attrs (rtx x)
{
  rtx reg = gen_reg_rtx (GET_MODE (x));
  set_reg_attrs_from_value (reg, x);
  return reg;
}

/* Set the register attributes for registers contained in PARM_RTX.
   Use needed values from memory attributes of MEM.  */

void
set_reg_attrs_for_parm (rtx parm_rtx, rtx mem)
{
  if (REG_P (parm_rtx))
    set_reg_attrs_from_value (parm_rtx, mem);
  else if (GET_CODE (parm_rtx) == PARALLEL)
    {
      /* Check for a NULL entry in the first slot, used to indicate that the
	 parameter goes both on the stack and in registers.  */
      int i = XEXP (XVECEXP (parm_rtx, 0, 0), 0) ? 0 : 1;
      for (; i < XVECLEN (parm_rtx, 0); i++)
	{
	  rtx x = XVECEXP (parm_rtx, 0, i);
	  if (REG_P (XEXP (x, 0)))
	    REG_ATTRS (XEXP (x, 0))
	      = get_reg_attrs (MEM_EXPR (mem),
			       INTVAL (XEXP (x, 1)));
	}
    }
}

/* Set the REG_ATTRS for registers in value X, given that X represents
   decl T.  */

void
set_reg_attrs_for_decl_rtl (tree t, rtx x)
{
  if (!t)
    return;
  tree tdecl = t;
  if (GET_CODE (x) == SUBREG)
    {
      gcc_assert (subreg_lowpart_p (x));
      x = SUBREG_REG (x);
    }
  if (REG_P (x))
    REG_ATTRS (x)
      = get_reg_attrs (t, byte_lowpart_offset (GET_MODE (x),
					       DECL_P (tdecl)
					       ? DECL_MODE (tdecl)
					       : TYPE_MODE (TREE_TYPE (tdecl))));
  if (GET_CODE (x) == CONCAT)
    {
      if (REG_P (XEXP (x, 0)))
        REG_ATTRS (XEXP (x, 0)) = get_reg_attrs (t, 0);
      if (REG_P (XEXP (x, 1)))
	REG_ATTRS (XEXP (x, 1))
	  = get_reg_attrs (t, GET_MODE_UNIT_SIZE (GET_MODE (XEXP (x, 0))));
    }
  if (GET_CODE (x) == PARALLEL)
    {
      int i, start;

      /* Check for a NULL entry, used to indicate that the parameter goes
	 both on the stack and in registers.  */
      if (XEXP (XVECEXP (x, 0, 0), 0))
	start = 0;
      else
	start = 1;

      for (i = start; i < XVECLEN (x, 0); i++)
	{
	  rtx y = XVECEXP (x, 0, i);
	  if (REG_P (XEXP (y, 0)))
	    REG_ATTRS (XEXP (y, 0)) = get_reg_attrs (t, INTVAL (XEXP (y, 1)));
	}
    }
}

/* Assign the RTX X to declaration T.  */

void
set_decl_rtl (tree t, rtx x)
{
  DECL_WRTL_CHECK (t)->decl_with_rtl.rtl = x;
  if (x)
    set_reg_attrs_for_decl_rtl (t, x);
}

/* Assign the RTX X to parameter declaration T.  BY_REFERENCE_P is true
   if the ABI requires the parameter to be passed by reference.  */

void
set_decl_incoming_rtl (tree t, rtx x, bool by_reference_p)
{
  DECL_INCOMING_RTL (t) = x;
  if (x && !by_reference_p)
    set_reg_attrs_for_decl_rtl (t, x);
}

/* Identify REG (which may be a CONCAT) as a user register.  */

void
mark_user_reg (rtx reg)
{
  if (GET_CODE (reg) == CONCAT)
    {
      REG_USERVAR_P (XEXP (reg, 0)) = 1;
      REG_USERVAR_P (XEXP (reg, 1)) = 1;
    }
  else
    {
      gcc_assert (REG_P (reg));
      REG_USERVAR_P (reg) = 1;
    }
}

/* Identify REG as a probable pointer register and show its alignment
   as ALIGN, if nonzero.  */

void
mark_reg_pointer (rtx reg, int align)
{
  if (! REG_POINTER (reg))
    {
      REG_POINTER (reg) = 1;

      if (align)
	REGNO_POINTER_ALIGN (REGNO (reg)) = align;
    }
  else if (align && align < REGNO_POINTER_ALIGN (REGNO (reg)))
    /* We can no-longer be sure just how aligned this pointer is.  */
    REGNO_POINTER_ALIGN (REGNO (reg)) = align;
}

/* Return 1 plus largest pseudo reg number used in the current function.  */

int
max_reg_num (void)
{
  return reg_rtx_no;
}

/* Return 1 + the largest label number used so far in the current function.  */

int
max_label_num (void)
{
  return label_num;
}

/* Return first label number used in this function (if any were used).  */

int
get_first_label_num (void)
{
  return first_label_num;
}

/* If the rtx for label was created during the expansion of a nested
   function, then first_label_num won't include this label number.
   Fix this now so that array indices work later.  */

void
maybe_set_first_label_num (rtx_code_label *x)
{
  if (CODE_LABEL_NUMBER (x) < first_label_num)
    first_label_num = CODE_LABEL_NUMBER (x);
}

/* Return a value representing some low-order bits of X, where the number
   of low-order bits is given by MODE.  Note that no conversion is done
   between floating-point and fixed-point values, rather, the bit
   representation is returned.

   This function handles the cases in common between gen_lowpart, below,
   and two variants in cse.c and combine.c.  These are the cases that can
   be safely handled at all points in the compilation.

   If this is not a case we can handle, return 0.  */

rtx
gen_lowpart_common (machine_mode mode, rtx x)
{
  int msize = GET_MODE_SIZE (mode);
  int xsize;
  machine_mode innermode;

  /* Unfortunately, this routine doesn't take a parameter for the mode of X,
     so we have to make one up.  Yuk.  */
  innermode = GET_MODE (x);
  if (CONST_INT_P (x)
      && msize * BITS_PER_UNIT <= HOST_BITS_PER_WIDE_INT)
    innermode = mode_for_size (HOST_BITS_PER_WIDE_INT, MODE_INT, 0);
  else if (innermode == VOIDmode)
    innermode = mode_for_size (HOST_BITS_PER_DOUBLE_INT, MODE_INT, 0);

  xsize = GET_MODE_SIZE (innermode);

  gcc_assert (innermode != VOIDmode && innermode != BLKmode);

  if (innermode == mode)
    return x;

  /* MODE must occupy no more words than the mode of X.  */
  if ((msize + (UNITS_PER_WORD - 1)) / UNITS_PER_WORD
      > ((xsize + (UNITS_PER_WORD - 1)) / UNITS_PER_WORD))
    return 0;

  /* Don't allow generating paradoxical FLOAT_MODE subregs.  */
  if (SCALAR_FLOAT_MODE_P (mode) && msize > xsize)
    return 0;

  if ((GET_CODE (x) == ZERO_EXTEND || GET_CODE (x) == SIGN_EXTEND)
      && (GET_MODE_CLASS (mode) == MODE_INT
	  || GET_MODE_CLASS (mode) == MODE_PARTIAL_INT))
    {
      /* If we are getting the low-order part of something that has been
	 sign- or zero-extended, we can either just use the object being
	 extended or make a narrower extension.  If we want an even smaller
	 piece than the size of the object being extended, call ourselves
	 recursively.

	 This case is used mostly by combine and cse.  */

      if (GET_MODE (XEXP (x, 0)) == mode)
	return XEXP (x, 0);
      else if (msize < GET_MODE_SIZE (GET_MODE (XEXP (x, 0))))
	return gen_lowpart_common (mode, XEXP (x, 0));
      else if (msize < xsize)
	return gen_rtx_fmt_e (GET_CODE (x), mode, XEXP (x, 0));
    }
  else if (GET_CODE (x) == SUBREG || REG_P (x)
	   || GET_CODE (x) == CONCAT || GET_CODE (x) == CONST_VECTOR
	   || CONST_DOUBLE_AS_FLOAT_P (x) || CONST_SCALAR_INT_P (x))
    return lowpart_subreg (mode, x, innermode);

  /* Otherwise, we can't do this.  */
  return 0;
}

rtx
gen_highpart (machine_mode mode, rtx x)
{
  unsigned int msize = GET_MODE_SIZE (mode);
  rtx result;

  /* This case loses if X is a subreg.  To catch bugs early,
     complain if an invalid MODE is used even in other cases.  */
  gcc_assert (msize <= UNITS_PER_WORD
	      || msize == (unsigned int) GET_MODE_UNIT_SIZE (GET_MODE (x)));

  result = simplify_gen_subreg (mode, x, GET_MODE (x),
				subreg_highpart_offset (mode, GET_MODE (x)));
  gcc_assert (result);

  /* simplify_gen_subreg is not guaranteed to return a valid operand for
     the target if we have a MEM.  gen_highpart must return a valid operand,
     emitting code if necessary to do so.  */
  if (MEM_P (result))
    {
      result = validize_mem (result);
      gcc_assert (result);
    }

  return result;
}

/* Like gen_highpart, but accept mode of EXP operand in case EXP can
   be VOIDmode constant.  */
rtx
gen_highpart_mode (machine_mode outermode, machine_mode innermode, rtx exp)
{
  if (GET_MODE (exp) != VOIDmode)
    {
      gcc_assert (GET_MODE (exp) == innermode);
      return gen_highpart (outermode, exp);
    }
  return simplify_gen_subreg (outermode, exp, innermode,
			      subreg_highpart_offset (outermode, innermode));
}

/* Return the SUBREG_BYTE for an OUTERMODE lowpart of an INNERMODE value.  */

unsigned int
subreg_lowpart_offset (machine_mode outermode, machine_mode innermode)
{
  unsigned int offset = 0;
  int difference = (GET_MODE_SIZE (innermode) - GET_MODE_SIZE (outermode));

  if (difference > 0)
    {
      if (WORDS_BIG_ENDIAN)
	offset += (difference / UNITS_PER_WORD) * UNITS_PER_WORD;
      if (BYTES_BIG_ENDIAN)
	offset += difference % UNITS_PER_WORD;
    }

  return offset;
}

/* Return offset in bytes to get OUTERMODE high part
   of the value in mode INNERMODE stored in memory in target format.  */
unsigned int
subreg_highpart_offset (machine_mode outermode, machine_mode innermode)
{
  unsigned int offset = 0;
  int difference = (GET_MODE_SIZE (innermode) - GET_MODE_SIZE (outermode));

  gcc_assert (GET_MODE_SIZE (innermode) >= GET_MODE_SIZE (outermode));

  if (difference > 0)
    {
      if (! WORDS_BIG_ENDIAN)
	offset += (difference / UNITS_PER_WORD) * UNITS_PER_WORD;
      if (! BYTES_BIG_ENDIAN)
	offset += difference % UNITS_PER_WORD;
    }

  return offset;
}

/* Return 1 iff X, assumed to be a SUBREG,
   refers to the least significant part of its containing reg.
   If X is not a SUBREG, always return 1 (it is its own low part!).  */

int
subreg_lowpart_p (const_rtx x)
{
  if (GET_CODE (x) != SUBREG)
    return 1;
  else if (GET_MODE (SUBREG_REG (x)) == VOIDmode)
    return 0;

  return (subreg_lowpart_offset (GET_MODE (x), GET_MODE (SUBREG_REG (x)))
	  == SUBREG_BYTE (x));
}

/* Return true if X is a paradoxical subreg, false otherwise.  */
bool
paradoxical_subreg_p (const_rtx x)
{
  if (GET_CODE (x) != SUBREG)
    return false;
  return (GET_MODE_PRECISION (GET_MODE (x))
	  > GET_MODE_PRECISION (GET_MODE (SUBREG_REG (x))));
}

/* Return subword OFFSET of operand OP.
   The word number, OFFSET, is interpreted as the word number starting
   at the low-order address.  OFFSET 0 is the low-order word if not
   WORDS_BIG_ENDIAN, otherwise it is the high-order word.

   If we cannot extract the required word, we return zero.  Otherwise,
   an rtx corresponding to the requested word will be returned.

   VALIDATE_ADDRESS is nonzero if the address should be validated.  Before
   reload has completed, a valid address will always be returned.  After
   reload, if a valid address cannot be returned, we return zero.

   If VALIDATE_ADDRESS is zero, we simply form the required address; validating
   it is the responsibility of the caller.

   MODE is the mode of OP in case it is a CONST_INT.

   ??? This is still rather broken for some cases.  The problem for the
   moment is that all callers of this thing provide no 'goal mode' to
   tell us to work with.  This exists because all callers were written
   in a word based SUBREG world.
   Now use of this function can be deprecated by simplify_subreg in most
   cases.
 */

rtx
operand_subword (rtx op, unsigned int offset, int validate_address, machine_mode mode)
{
  if (mode == VOIDmode)
    mode = GET_MODE (op);

  gcc_assert (mode != VOIDmode);

  /* If OP is narrower than a word, fail.  */
  if (mode != BLKmode
      && (GET_MODE_SIZE (mode) < UNITS_PER_WORD))
    return 0;

  /* If we want a word outside OP, return zero.  */
  if (mode != BLKmode
      && (offset + 1) * UNITS_PER_WORD > GET_MODE_SIZE (mode))
    return const0_rtx;

  /* Form a new MEM at the requested address.  */
  if (MEM_P (op))
    {
      rtx new_rtx = adjust_address_nv (op, word_mode, offset * UNITS_PER_WORD);

      if (! validate_address)
	return new_rtx;

      else if (reload_completed)
	{
	  if (! strict_memory_address_addr_space_p (word_mode,
						    XEXP (new_rtx, 0),
						    MEM_ADDR_SPACE (op)))
	    return 0;
	}
      else
	return replace_equiv_address (new_rtx, XEXP (new_rtx, 0));
    }

  /* Rest can be handled by simplify_subreg.  */
  return simplify_gen_subreg (word_mode, op, mode, (offset * UNITS_PER_WORD));
}

/* Similar to `operand_subword', but never return 0.  If we can't
   extract the required subword, put OP into a register and try again.
   The second attempt must succeed.  We always validate the address in
   this case.

   MODE is the mode of OP, in case it is CONST_INT.  */

rtx
operand_subword_force (rtx op, unsigned int offset, machine_mode mode)
{
  rtx result = operand_subword (op, offset, 1, mode);

  if (result)
    return result;

  if (mode != BLKmode && mode != VOIDmode)
    {
      /* If this is a register which can not be accessed by words, copy it
	 to a pseudo register.  */
      if (REG_P (op))
	op = copy_to_reg (op);
      else
	op = force_reg (mode, op);
    }

  result = operand_subword (op, offset, 1, mode);
  gcc_assert (result);

  return result;
}

/* Returns 1 if both MEM_EXPR can be considered equal
   and 0 otherwise.  */

int
mem_expr_equal_p (const_tree expr1, const_tree expr2)
{
  if (expr1 == expr2)
    return 1;

  if (! expr1 || ! expr2)
    return 0;

  if (TREE_CODE (expr1) != TREE_CODE (expr2))
    return 0;

  return operand_equal_p (expr1, expr2, 0);
}

/* Return OFFSET if XEXP (MEM, 0) - OFFSET is known to be ALIGN
   bits aligned for 0 <= OFFSET < ALIGN / BITS_PER_UNIT, or
   -1 if not known.  */

int
get_mem_align_offset (rtx mem, unsigned int align)
{
  tree expr;
  unsigned HOST_WIDE_INT offset;

  /* This function can't use
     if (!MEM_EXPR (mem) || !MEM_OFFSET_KNOWN_P (mem)
	 || (MAX (MEM_ALIGN (mem),
	          MAX (align, get_object_alignment (MEM_EXPR (mem))))
	     < align))
       return -1;
     else
       return (- MEM_OFFSET (mem)) & (align / BITS_PER_UNIT - 1);
     for two reasons:
     - COMPONENT_REFs in MEM_EXPR can have NULL first operand,
       for <variable>.  get_inner_reference doesn't handle it and
       even if it did, the alignment in that case needs to be determined
       from DECL_FIELD_CONTEXT's TYPE_ALIGN.
     - it would do suboptimal job for COMPONENT_REFs, even if MEM_EXPR
       isn't sufficiently aligned, the object it is in might be.  */
  gcc_assert (MEM_P (mem));
  expr = MEM_EXPR (mem);
  if (expr == NULL_TREE || !MEM_OFFSET_KNOWN_P (mem))
    return -1;

  offset = MEM_OFFSET (mem);
  if (DECL_P (expr))
    {
      if (DECL_ALIGN (expr) < align)
	return -1;
    }
  else if (INDIRECT_REF_P (expr))
    {
      if (TYPE_ALIGN (TREE_TYPE (expr)) < (unsigned int) align)
	return -1;
    }
  else if (TREE_CODE (expr) == COMPONENT_REF)
    {
      while (1)
	{
	  tree inner = TREE_OPERAND (expr, 0);
	  tree field = TREE_OPERAND (expr, 1);
	  tree byte_offset = component_ref_field_offset (expr);
	  tree bit_offset = DECL_FIELD_BIT_OFFSET (field);

	  if (!byte_offset
	      || !tree_fits_uhwi_p (byte_offset)
	      || !tree_fits_uhwi_p (bit_offset))
	    return -1;

	  offset += tree_to_uhwi (byte_offset);
	  offset += tree_to_uhwi (bit_offset) / BITS_PER_UNIT;

	  if (inner == NULL_TREE)
	    {
	      if (TYPE_ALIGN (DECL_FIELD_CONTEXT (field))
		  < (unsigned int) align)
		return -1;
	      break;
	    }
	  else if (DECL_P (inner))
	    {
	      if (DECL_ALIGN (inner) < align)
		return -1;
	      break;
	    }
	  else if (TREE_CODE (inner) != COMPONENT_REF)
	    return -1;
	  expr = inner;
	}
    }
  else
    return -1;

  return offset & ((align / BITS_PER_UNIT) - 1);
}

/* Given REF (a MEM) and T, either the type of X or the expression
   corresponding to REF, set the memory attributes.  OBJECTP is nonzero
   if we are making a new object of this type.  BITPOS is nonzero if
   there is an offset outstanding on T that will be applied later.  */

void
set_mem_attributes_minus_bitpos (rtx ref, tree t, int objectp,
				 HOST_WIDE_INT bitpos)
{
  HOST_WIDE_INT apply_bitpos = 0;
  tree type;
  struct mem_attrs attrs, *defattrs, *refattrs;
  addr_space_t as;

  /* It can happen that type_for_mode was given a mode for which there
     is no language-level type.  In which case it returns NULL, which
     we can see here.  */
  if (t == NULL_TREE)
    return;

  type = TYPE_P (t) ? t : TREE_TYPE (t);
  if (type == error_mark_node)
    return;

  /* If we have already set DECL_RTL = ref, get_alias_set will get the
     wrong answer, as it assumes that DECL_RTL already has the right alias
     info.  Callers should not set DECL_RTL until after the call to
     set_mem_attributes.  */
  gcc_assert (!DECL_P (t) || ref != DECL_RTL_IF_SET (t));

  memset (&attrs, 0, sizeof (attrs));

  /* Get the alias set from the expression or type (perhaps using a
     front-end routine) and use it.  */
  attrs.alias = get_alias_set (t);

  MEM_VOLATILE_P (ref) |= TYPE_VOLATILE (type);
  MEM_POINTER (ref) = POINTER_TYPE_P (type);

  /* Default values from pre-existing memory attributes if present.  */
  refattrs = MEM_ATTRS (ref);
  if (refattrs)
    {
      /* ??? Can this ever happen?  Calling this routine on a MEM that
	 already carries memory attributes should probably be invalid.  */
      attrs.expr = refattrs->expr;
      attrs.offset_known_p = refattrs->offset_known_p;
      attrs.offset = refattrs->offset;
      attrs.size_known_p = refattrs->size_known_p;
      attrs.size = refattrs->size;
      attrs.align = refattrs->align;
    }

  /* Otherwise, default values from the mode of the MEM reference.  */
  else
    {
      defattrs = mode_mem_attrs[(int) GET_MODE (ref)];
      gcc_assert (!defattrs->expr);
      gcc_assert (!defattrs->offset_known_p);

      /* Respect mode size.  */
      attrs.size_known_p = defattrs->size_known_p;
      attrs.size = defattrs->size;
      /* ??? Is this really necessary?  We probably should always get
	 the size from the type below.  */

      /* Respect mode alignment for STRICT_ALIGNMENT targets if T is a type;
         if T is an object, always compute the object alignment below.  */
      if (TYPE_P (t))
	attrs.align = defattrs->align;
      else
	attrs.align = BITS_PER_UNIT;
      /* ??? If T is a type, respecting mode alignment may *also* be wrong
	 e.g. if the type carries an alignment attribute.  Should we be
	 able to simply always use TYPE_ALIGN?  */
    }

  /* We can set the alignment from the type if we are making an object or if
     this is an INDIRECT_REF.  */
  if (objectp || TREE_CODE (t) == INDIRECT_REF)
    attrs.align = MAX (attrs.align, TYPE_ALIGN (type));

  /* If the size is known, we can set that.  */
  tree new_size = TYPE_SIZE_UNIT (type);

  /* The address-space is that of the type.  */
  as = TYPE_ADDR_SPACE (type);

  /* If T is not a type, we may be able to deduce some more information about
     the expression.  */
  if (! TYPE_P (t))
    {
      tree base;

      if (TREE_THIS_VOLATILE (t))
	MEM_VOLATILE_P (ref) = 1;

      /* Now remove any conversions: they don't change what the underlying
	 object is.  Likewise for SAVE_EXPR.  */
      while (CONVERT_EXPR_P (t)
	     || TREE_CODE (t) == VIEW_CONVERT_EXPR
	     || TREE_CODE (t) == SAVE_EXPR)
	t = TREE_OPERAND (t, 0);

      /* Note whether this expression can trap.  */
      MEM_NOTRAP_P (ref) = !tree_could_trap_p (t);

      base = get_base_address (t);
      if (base)
	{
	  if (DECL_P (base)
	      && TREE_READONLY (base)
	      && (TREE_STATIC (base) || DECL_EXTERNAL (base))
	      && !TREE_THIS_VOLATILE (base))
	    MEM_READONLY_P (ref) = 1;

	  /* Mark static const strings readonly as well.  */
	  if (TREE_CODE (base) == STRING_CST
	      && TREE_READONLY (base)
	      && TREE_STATIC (base))
	    MEM_READONLY_P (ref) = 1;

	  /* Address-space information is on the base object.  */
	  if (TREE_CODE (base) == MEM_REF
	      || TREE_CODE (base) == TARGET_MEM_REF)
	    as = TYPE_ADDR_SPACE (TREE_TYPE (TREE_TYPE (TREE_OPERAND (base,
								      0))));
	  else
	    as = TYPE_ADDR_SPACE (TREE_TYPE (base));
	}

      /* If this expression uses it's parent's alias set, mark it such
	 that we won't change it.  */
      if (component_uses_parent_alias_set_from (t) != NULL_TREE)
	MEM_KEEP_ALIAS_SET_P (ref) = 1;

      /* If this is a decl, set the attributes of the MEM from it.  */
      if (DECL_P (t))
	{
	  attrs.expr = t;
	  attrs.offset_known_p = true;
	  attrs.offset = 0;
	  apply_bitpos = bitpos;
	  new_size = DECL_SIZE_UNIT (t);
	}

      /* ???  If we end up with a constant here do record a MEM_EXPR.  */
      else if (CONSTANT_CLASS_P (t))
	;

      /* If this is a field reference, record it.  */
      else if (TREE_CODE (t) == COMPONENT_REF)
	{
	  attrs.expr = t;
	  attrs.offset_known_p = true;
	  attrs.offset = 0;
	  apply_bitpos = bitpos;
	  if (DECL_BIT_FIELD (TREE_OPERAND (t, 1)))
	    new_size = DECL_SIZE_UNIT (TREE_OPERAND (t, 1));
	}

      /* If this is an array reference, look for an outer field reference.  */
      else if (TREE_CODE (t) == ARRAY_REF)
	{
	  tree off_tree = size_zero_node;
	  /* We can't modify t, because we use it at the end of the
	     function.  */
	  tree t2 = t;

	  do
	    {
	      tree index = TREE_OPERAND (t2, 1);
	      tree low_bound = array_ref_low_bound (t2);
	      tree unit_size = array_ref_element_size (t2);

	      /* We assume all arrays have sizes that are a multiple of a byte.
		 First subtract the lower bound, if any, in the type of the
		 index, then convert to sizetype and multiply by the size of
		 the array element.  */
	      if (! integer_zerop (low_bound))
		index = fold_build2 (MINUS_EXPR, TREE_TYPE (index),
				     index, low_bound);

	      off_tree = size_binop (PLUS_EXPR,
				     size_binop (MULT_EXPR,
						 fold_convert (sizetype,
							       index),
						 unit_size),
				     off_tree);
	      t2 = TREE_OPERAND (t2, 0);
	    }
	  while (TREE_CODE (t2) == ARRAY_REF);

	  if (DECL_P (t2)
	      || TREE_CODE (t2) == COMPONENT_REF)
	    {
	      attrs.expr = t2;
	      attrs.offset_known_p = false;
	      if (tree_fits_uhwi_p (off_tree))
		{
		  attrs.offset_known_p = true;
		  attrs.offset = tree_to_uhwi (off_tree);
		  apply_bitpos = bitpos;
		}
	    }
	  /* Else do not record a MEM_EXPR.  */
	}

      /* If this is an indirect reference, record it.  */
      else if (TREE_CODE (t) == MEM_REF 
	       || TREE_CODE (t) == TARGET_MEM_REF)
	{
	  attrs.expr = t;
	  attrs.offset_known_p = true;
	  attrs.offset = 0;
	  apply_bitpos = bitpos;
	}

      /* Compute the alignment.  */
      unsigned int obj_align;
      unsigned HOST_WIDE_INT obj_bitpos;
      get_object_alignment_1 (t, &obj_align, &obj_bitpos);
      obj_bitpos = (obj_bitpos - bitpos) & (obj_align - 1);
      if (obj_bitpos != 0)
	obj_align = least_bit_hwi (obj_bitpos);
      attrs.align = MAX (attrs.align, obj_align);
    }

  if (tree_fits_uhwi_p (new_size))
    {
      attrs.size_known_p = true;
      attrs.size = tree_to_uhwi (new_size);
    }

  /* If we modified OFFSET based on T, then subtract the outstanding
     bit position offset.  Similarly, increase the size of the accessed
     object to contain the negative offset.  */
  if (apply_bitpos)
    {
      gcc_assert (attrs.offset_known_p);
      attrs.offset -= apply_bitpos / BITS_PER_UNIT;
      if (attrs.size_known_p)
	attrs.size += apply_bitpos / BITS_PER_UNIT;
    }

  /* Now set the attributes we computed above.  */
  attrs.addrspace = as;
  set_mem_attrs (ref, &attrs);
}

void
set_mem_attributes (rtx ref, tree t, int objectp)
{
  set_mem_attributes_minus_bitpos (ref, t, objectp, 0);
}

/* Set the alias set of MEM to SET.  */

void
set_mem_alias_set (rtx mem, alias_set_type set)
{
  struct mem_attrs attrs;

  /* If the new and old alias sets don't conflict, something is wrong.  */
  gcc_checking_assert (alias_sets_conflict_p (set, MEM_ALIAS_SET (mem)));
  attrs = *get_mem_attrs (mem);
  attrs.alias = set;
  set_mem_attrs (mem, &attrs);
}

/* Set the address space of MEM to ADDRSPACE (target-defined).  */

void
set_mem_addr_space (rtx mem, addr_space_t addrspace)
{
  struct mem_attrs attrs;

  attrs = *get_mem_attrs (mem);
  attrs.addrspace = addrspace;
  set_mem_attrs (mem, &attrs);
}

/* Set the alignment of MEM to ALIGN bits.  */

void
set_mem_align (rtx mem, unsigned int align)
{
  struct mem_attrs attrs;

  attrs = *get_mem_attrs (mem);
  attrs.align = align;
  set_mem_attrs (mem, &attrs);
}

/* Set the expr for MEM to EXPR.  */

void
set_mem_expr (rtx mem, tree expr)
{
  struct mem_attrs attrs;

  attrs = *get_mem_attrs (mem);
  attrs.expr = expr;
  set_mem_attrs (mem, &attrs);
}

/* Set the offset of MEM to OFFSET.  */

void
set_mem_offset (rtx mem, HOST_WIDE_INT offset)
{
  struct mem_attrs attrs;

  attrs = *get_mem_attrs (mem);
  attrs.offset_known_p = true;
  attrs.offset = offset;
  set_mem_attrs (mem, &attrs);
}

/* Clear the offset of MEM.  */

void
clear_mem_offset (rtx mem)
{
  struct mem_attrs attrs;

  attrs = *get_mem_attrs (mem);
  attrs.offset_known_p = false;
  set_mem_attrs (mem, &attrs);
}

/* Set the size of MEM to SIZE.  */

void
set_mem_size (rtx mem, HOST_WIDE_INT size)
{
  struct mem_attrs attrs;

  attrs = *get_mem_attrs (mem);
  attrs.size_known_p = true;
  attrs.size = size;
  set_mem_attrs (mem, &attrs);
}

/* Clear the size of MEM.  */

void
clear_mem_size (rtx mem)
{
  struct mem_attrs attrs;

  attrs = *get_mem_attrs (mem);
  attrs.size_known_p = false;
  set_mem_attrs (mem, &attrs);
}

/* Return a memory reference like MEMREF, but with its mode changed to MODE
   and its address changed to ADDR.  (VOIDmode means don't change the mode.
   NULL for ADDR means don't change the address.)  VALIDATE is nonzero if the
   returned memory location is required to be valid.  INPLACE is true if any
   changes can be made directly to MEMREF or false if MEMREF must be treated
   as immutable.

   The memory attributes are not changed.  */

static rtx
change_address_1 (rtx memref, machine_mode mode, rtx addr, int validate,
		  bool inplace)
{
  addr_space_t as;
  rtx new_rtx;

  gcc_assert (MEM_P (memref));
  as = MEM_ADDR_SPACE (memref);
  if (mode == VOIDmode)
    mode = GET_MODE (memref);
  if (addr == 0)
    addr = XEXP (memref, 0);
  if (mode == GET_MODE (memref) && addr == XEXP (memref, 0)
      && (!validate || memory_address_addr_space_p (mode, addr, as)))
    return memref;

  /* Don't validate address for LRA.  LRA can make the address valid
     by itself in most efficient way.  */
  if (validate && !lra_in_progress)
    {
      if (reload_in_progress || reload_completed)
	gcc_assert (memory_address_addr_space_p (mode, addr, as));
      else
	addr = memory_address_addr_space (mode, addr, as);
    }

  if (rtx_equal_p (addr, XEXP (memref, 0)) && mode == GET_MODE (memref))
    return memref;

  if (inplace)
    {
      XEXP (memref, 0) = addr;
      return memref;
    }

  new_rtx = gen_rtx_MEM (mode, addr);
  MEM_COPY_ATTRIBUTES (new_rtx, memref);
  return new_rtx;
}

/* Like change_address_1 with VALIDATE nonzero, but we are not saying in what
   way we are changing MEMREF, so we only preserve the alias set.  */

rtx
change_address (rtx memref, machine_mode mode, rtx addr)
{
  rtx new_rtx = change_address_1 (memref, mode, addr, 1, false);
  machine_mode mmode = GET_MODE (new_rtx);
  struct mem_attrs attrs, *defattrs;

  attrs = *get_mem_attrs (memref);
  defattrs = mode_mem_attrs[(int) mmode];
  attrs.expr = NULL_TREE;
  attrs.offset_known_p = false;
  attrs.size_known_p = defattrs->size_known_p;
  attrs.size = defattrs->size;
  attrs.align = defattrs->align;

  /* If there are no changes, just return the original memory reference.  */
  if (new_rtx == memref)
    {
      if (mem_attrs_eq_p (get_mem_attrs (memref), &attrs))
	return new_rtx;

      new_rtx = gen_rtx_MEM (mmode, XEXP (memref, 0));
      MEM_COPY_ATTRIBUTES (new_rtx, memref);
    }

  set_mem_attrs (new_rtx, &attrs);
  return new_rtx;
}

/* Return a memory reference like MEMREF, but with its mode changed
   to MODE and its address offset by OFFSET bytes.  If VALIDATE is
   nonzero, the memory address is forced to be valid.
   If ADJUST_ADDRESS is zero, OFFSET is only used to update MEM_ATTRS
   and the caller is responsible for adjusting MEMREF base register.
   If ADJUST_OBJECT is zero, the underlying object associated with the
   memory reference is left unchanged and the caller is responsible for
   dealing with it.  Otherwise, if the new memory reference is outside
   the underlying object, even partially, then the object is dropped.
   SIZE, if nonzero, is the size of an access in cases where MODE
   has no inherent size.  */

rtx
adjust_address_1 (rtx memref, machine_mode mode, HOST_WIDE_INT offset,
		  int validate, int adjust_address, int adjust_object,
		  HOST_WIDE_INT size)
{
  rtx addr = XEXP (memref, 0);
  rtx new_rtx;
  machine_mode address_mode;
  int pbits;
  struct mem_attrs attrs = *get_mem_attrs (memref), *defattrs;
  unsigned HOST_WIDE_INT max_align;
#ifdef POINTERS_EXTEND_UNSIGNED
  machine_mode pointer_mode
    = targetm.addr_space.pointer_mode (attrs.addrspace);
#endif

  /* VOIDmode means no mode change for change_address_1.  */
  if (mode == VOIDmode)
    mode = GET_MODE (memref);

  /* Take the size of non-BLKmode accesses from the mode.  */
  defattrs = mode_mem_attrs[(int) mode];
  if (defattrs->size_known_p)
    size = defattrs->size;

  /* If there are no changes, just return the original memory reference.  */
  if (mode == GET_MODE (memref) && !offset
      && (size == 0 || (attrs.size_known_p && attrs.size == size))
      && (!validate || memory_address_addr_space_p (mode, addr,
						    attrs.addrspace)))
    return memref;

  /* ??? Prefer to create garbage instead of creating shared rtl.
     This may happen even if offset is nonzero -- consider
     (plus (plus reg reg) const_int) -- so do this always.  */
  addr = copy_rtx (addr);

  /* Convert a possibly large offset to a signed value within the
     range of the target address space.  */
  address_mode = get_address_mode (memref);
  pbits = GET_MODE_BITSIZE (address_mode);
  if (HOST_BITS_PER_WIDE_INT > pbits)
    {
      int shift = HOST_BITS_PER_WIDE_INT - pbits;
      offset = (((HOST_WIDE_INT) ((unsigned HOST_WIDE_INT) offset << shift))
		>> shift);
    }

  if (adjust_address)
    {
      /* If MEMREF is a LO_SUM and the offset is within the alignment of the
	 object, we can merge it into the LO_SUM.  */
      if (GET_MODE (memref) != BLKmode && GET_CODE (addr) == LO_SUM
	  && offset >= 0
	  && (unsigned HOST_WIDE_INT) offset
	      < GET_MODE_ALIGNMENT (GET_MODE (memref)) / BITS_PER_UNIT)
	addr = gen_rtx_LO_SUM (address_mode, XEXP (addr, 0),
			       plus_constant (address_mode,
					      XEXP (addr, 1), offset));
#ifdef POINTERS_EXTEND_UNSIGNED
      /* If MEMREF is a ZERO_EXTEND from pointer_mode and the offset is valid
	 in that mode, we merge it into the ZERO_EXTEND.  We take advantage of
	 the fact that pointers are not allowed to overflow.  */
      else if (POINTERS_EXTEND_UNSIGNED > 0
	       && GET_CODE (addr) == ZERO_EXTEND
	       && GET_MODE (XEXP (addr, 0)) == pointer_mode
	       && trunc_int_for_mode (offset, pointer_mode) == offset)
	addr = gen_rtx_ZERO_EXTEND (address_mode,
				    plus_constant (pointer_mode,
						   XEXP (addr, 0), offset));
#endif
      else
	addr = plus_constant (address_mode, addr, offset);
    }

  new_rtx = change_address_1 (memref, mode, addr, validate, false);

  /* If the address is a REG, change_address_1 rightfully returns memref,
     but this would destroy memref's MEM_ATTRS.  */
  if (new_rtx == memref && offset != 0)
    new_rtx = copy_rtx (new_rtx);

  /* Conservatively drop the object if we don't know where we start from.  */
  if (adjust_object && (!attrs.offset_known_p || !attrs.size_known_p))
    {
      attrs.expr = NULL_TREE;
      attrs.alias = 0;
    }

  /* Compute the new values of the memory attributes due to this adjustment.
     We add the offsets and update the alignment.  */
  if (attrs.offset_known_p)
    {
      attrs.offset += offset;

      /* Drop the object if the new left end is not within its bounds.  */
      if (adjust_object && attrs.offset < 0)
	{
	  attrs.expr = NULL_TREE;
	  attrs.alias = 0;
	}
    }

  /* Compute the new alignment by taking the MIN of the alignment and the
     lowest-order set bit in OFFSET, but don't change the alignment if OFFSET
     if zero.  */
  if (offset != 0)
    {
      max_align = least_bit_hwi (offset) * BITS_PER_UNIT;
      attrs.align = MIN (attrs.align, max_align);
    }

  if (size)
    {
      /* Drop the object if the new right end is not within its bounds.  */
      if (adjust_object && (offset + size) > attrs.size)
	{
	  attrs.expr = NULL_TREE;
	  attrs.alias = 0;
	}
      attrs.size_known_p = true;
      attrs.size = size;
    }
  else if (attrs.size_known_p)
    {
      gcc_assert (!adjust_object);
      attrs.size -= offset;
      /* ??? The store_by_pieces machinery generates negative sizes,
	 so don't assert for that here.  */
    }

  set_mem_attrs (new_rtx, &attrs);

  return new_rtx;
}

/* Return a memory reference like MEMREF, but with its mode changed
   to MODE and its address changed to ADDR, which is assumed to be
   MEMREF offset by OFFSET bytes.  If VALIDATE is
   nonzero, the memory address is forced to be valid.  */

rtx
adjust_automodify_address_1 (rtx memref, machine_mode mode, rtx addr,
			     HOST_WIDE_INT offset, int validate)
{
  memref = change_address_1 (memref, VOIDmode, addr, validate, false);
  return adjust_address_1 (memref, mode, offset, validate, 0, 0, 0);
}

/* Return a memory reference like MEMREF, but whose address is changed by
   adding OFFSET, an RTX, to it.  POW2 is the highest power of two factor
   known to be in OFFSET (possibly 1).  */

rtx
offset_address (rtx memref, rtx offset, unsigned HOST_WIDE_INT pow2)
{
  rtx new_rtx, addr = XEXP (memref, 0);
  machine_mode address_mode;
  struct mem_attrs attrs, *defattrs;

  attrs = *get_mem_attrs (memref);
  address_mode = get_address_mode (memref);
  new_rtx = simplify_gen_binary (PLUS, address_mode, addr, offset);

  /* At this point we don't know _why_ the address is invalid.  It
     could have secondary memory references, multiplies or anything.

     However, if we did go and rearrange things, we can wind up not
     being able to recognize the magic around pic_offset_table_rtx.
     This stuff is fragile, and is yet another example of why it is
     bad to expose PIC machinery too early.  */
  if (! memory_address_addr_space_p (GET_MODE (memref), new_rtx,
				     attrs.addrspace)
      && GET_CODE (addr) == PLUS
      && XEXP (addr, 0) == pic_offset_table_rtx)
    {
      addr = force_reg (GET_MODE (addr), addr);
      new_rtx = simplify_gen_binary (PLUS, address_mode, addr, offset);
    }

  update_temp_slot_address (XEXP (memref, 0), new_rtx);
  new_rtx = change_address_1 (memref, VOIDmode, new_rtx, 1, false);

  /* If there are no changes, just return the original memory reference.  */
  if (new_rtx == memref)
    return new_rtx;

  /* Update the alignment to reflect the offset.  Reset the offset, which
     we don't know.  */
  defattrs = mode_mem_attrs[(int) GET_MODE (new_rtx)];
  attrs.offset_known_p = false;
  attrs.size_known_p = defattrs->size_known_p;
  attrs.size = defattrs->size;
  attrs.align = MIN (attrs.align, pow2 * BITS_PER_UNIT);
  set_mem_attrs (new_rtx, &attrs);
  return new_rtx;
}

/* Return a memory reference like MEMREF, but with its address changed to
   ADDR.  The caller is asserting that the actual piece of memory pointed
   to is the same, just the form of the address is being changed, such as
   by putting something into a register.  INPLACE is true if any changes
   can be made directly to MEMREF or false if MEMREF must be treated as
   immutable.  */

rtx
replace_equiv_address (rtx memref, rtx addr, bool inplace)
{
  /* change_address_1 copies the memory attribute structure without change
     and that's exactly what we want here.  */
  update_temp_slot_address (XEXP (memref, 0), addr);
  return change_address_1 (memref, VOIDmode, addr, 1, inplace);
}

/* Likewise, but the reference is not required to be valid.  */

rtx
replace_equiv_address_nv (rtx memref, rtx addr, bool inplace)
{
  return change_address_1 (memref, VOIDmode, addr, 0, inplace);
}

/* Return a memory reference like MEMREF, but with its mode widened to
   MODE and offset by OFFSET.  This would be used by targets that e.g.
   cannot issue QImode memory operations and have to use SImode memory
   operations plus masking logic.  */

rtx
widen_memory_access (rtx memref, machine_mode mode, HOST_WIDE_INT offset)
{
  rtx new_rtx = adjust_address_1 (memref, mode, offset, 1, 1, 0, 0);
  struct mem_attrs attrs;
  unsigned int size = GET_MODE_SIZE (mode);

  /* If there are no changes, just return the original memory reference.  */
  if (new_rtx == memref)
    return new_rtx;

  attrs = *get_mem_attrs (new_rtx);

  /* If we don't know what offset we were at within the expression, then
     we can't know if we've overstepped the bounds.  */
  if (! attrs.offset_known_p)
    attrs.expr = NULL_TREE;

  while (attrs.expr)
    {
      if (TREE_CODE (attrs.expr) == COMPONENT_REF)
	{
	  tree field = TREE_OPERAND (attrs.expr, 1);
	  tree offset = component_ref_field_offset (attrs.expr);

	  if (! DECL_SIZE_UNIT (field))
	    {
	      attrs.expr = NULL_TREE;
	      break;
	    }

	  /* Is the field at least as large as the access?  If so, ok,
	     otherwise strip back to the containing structure.  */
	  if (TREE_CODE (DECL_SIZE_UNIT (field)) == INTEGER_CST
	      && compare_tree_int (DECL_SIZE_UNIT (field), size) >= 0
	      && attrs.offset >= 0)
	    break;

	  if (! tree_fits_uhwi_p (offset))
	    {
	      attrs.expr = NULL_TREE;
	      break;
	    }

	  attrs.expr = TREE_OPERAND (attrs.expr, 0);
	  attrs.offset += tree_to_uhwi (offset);
	  attrs.offset += (tree_to_uhwi (DECL_FIELD_BIT_OFFSET (field))
			   / BITS_PER_UNIT);
	}
      /* Similarly for the decl.  */
      else if (DECL_P (attrs.expr)
	       && DECL_SIZE_UNIT (attrs.expr)
	       && TREE_CODE (DECL_SIZE_UNIT (attrs.expr)) == INTEGER_CST
	       && compare_tree_int (DECL_SIZE_UNIT (attrs.expr), size) >= 0
	       && (! attrs.offset_known_p || attrs.offset >= 0))
	break;
      else
	{
	  /* The widened memory access overflows the expression, which means
	     that it could alias another expression.  Zap it.  */
	  attrs.expr = NULL_TREE;
	  break;
	}
    }

  if (! attrs.expr)
    attrs.offset_known_p = false;

  /* The widened memory may alias other stuff, so zap the alias set.  */
  /* ??? Maybe use get_alias_set on any remaining expression.  */
  attrs.alias = 0;
  attrs.size_known_p = true;
  attrs.size = size;
  set_mem_attrs (new_rtx, &attrs);
  return new_rtx;
}

/* A fake decl that is used as the MEM_EXPR of spill slots.  */
static GTY(()) tree spill_slot_decl;

tree
get_spill_slot_decl (bool force_build_p)
{
  tree d = spill_slot_decl;
  rtx rd;
  struct mem_attrs attrs;

  if (d || !force_build_p)
    return d;

  d = build_decl (DECL_SOURCE_LOCATION (current_function_decl),
		  VAR_DECL, get_identifier ("%sfp"), void_type_node);
  DECL_ARTIFICIAL (d) = 1;
  DECL_IGNORED_P (d) = 1;
  TREE_USED (d) = 1;
  spill_slot_decl = d;

  rd = gen_rtx_MEM (BLKmode, frame_pointer_rtx);
  MEM_NOTRAP_P (rd) = 1;
  attrs = *mode_mem_attrs[(int) BLKmode];
  attrs.alias = new_alias_set ();
  attrs.expr = d;
  set_mem_attrs (rd, &attrs);
  SET_DECL_RTL (d, rd);

  return d;
}

/* Given MEM, a result from assign_stack_local, fill in the memory
   attributes as appropriate for a register allocator spill slot.
   These slots are not aliasable by other memory.  We arrange for
   them all to use a single MEM_EXPR, so that the aliasing code can
   work properly in the case of shared spill slots.  */

void
set_mem_attrs_for_spill (rtx mem)
{
  struct mem_attrs attrs;
  rtx addr;

  attrs = *get_mem_attrs (mem);
  attrs.expr = get_spill_slot_decl (true);
  attrs.alias = MEM_ALIAS_SET (DECL_RTL (attrs.expr));
  attrs.addrspace = ADDR_SPACE_GENERIC;

  /* We expect the incoming memory to be of the form:
	(mem:MODE (plus (reg sfp) (const_int offset)))
     with perhaps the plus missing for offset = 0.  */
  addr = XEXP (mem, 0);
  attrs.offset_known_p = true;
  attrs.offset = 0;
  if (GET_CODE (addr) == PLUS
      && CONST_INT_P (XEXP (addr, 1)))
    attrs.offset = INTVAL (XEXP (addr, 1));

  set_mem_attrs (mem, &attrs);
  MEM_NOTRAP_P (mem) = 1;
}

/* Return a newly created CODE_LABEL rtx with a unique label number.  */

rtx_code_label *
gen_label_rtx (void)
{
  return as_a <rtx_code_label *> (
	    gen_rtx_CODE_LABEL (VOIDmode, NULL_RTX, NULL_RTX,
				NULL, label_num++, NULL));
}

/* For procedure integration.  */

/* Install new pointers to the first and last insns in the chain.
   Also, set cur_insn_uid to one higher than the last in use.
   Used for an inline-procedure after copying the insn chain.  */

void
set_new_first_and_last_insn (rtx_insn *first, rtx_insn *last)
{
  rtx_insn *insn;

  set_first_insn (first);
  set_last_insn (last);
  cur_insn_uid = 0;

  if (MIN_NONDEBUG_INSN_UID || MAY_HAVE_DEBUG_INSNS)
    {
      int debug_count = 0;

      cur_insn_uid = MIN_NONDEBUG_INSN_UID - 1;
      cur_debug_insn_uid = 0;

      for (insn = first; insn; insn = NEXT_INSN (insn))
	if (INSN_UID (insn) < MIN_NONDEBUG_INSN_UID)
	  cur_debug_insn_uid = MAX (cur_debug_insn_uid, INSN_UID (insn));
	else
	  {
	    cur_insn_uid = MAX (cur_insn_uid, INSN_UID (insn));
	    if (DEBUG_INSN_P (insn))
	      debug_count++;
	  }

      if (debug_count)
	cur_debug_insn_uid = MIN_NONDEBUG_INSN_UID + debug_count;
      else
	cur_debug_insn_uid++;
    }
  else
    for (insn = first; insn; insn = NEXT_INSN (insn))
      cur_insn_uid = MAX (cur_insn_uid, INSN_UID (insn));

  cur_insn_uid++;
}

/* Go through all the RTL insn bodies and copy any invalid shared
   structure.  This routine should only be called once.  */

static void
unshare_all_rtl_1 (rtx_insn *insn)
{
  /* Unshare just about everything else.  */
  unshare_all_rtl_in_chain (insn);

  /* Make sure the addresses of stack slots found outside the insn chain
     (such as, in DECL_RTL of a variable) are not shared
     with the insn chain.

     This special care is necessary when the stack slot MEM does not
     actually appear in the insn chain.  If it does appear, its address
     is unshared from all else at that point.  */
  unsigned int i;
  rtx temp;
  FOR_EACH_VEC_SAFE_ELT (stack_slot_list, i, temp)
    (*stack_slot_list)[i] = copy_rtx_if_shared (temp);
}

/* Go through all the RTL insn bodies and copy any invalid shared
   structure, again.  This is a fairly expensive thing to do so it
   should be done sparingly.  */

void
unshare_all_rtl_again (rtx_insn *insn)
{
  rtx_insn *p;
  tree decl;

  for (p = insn; p; p = NEXT_INSN (p))
    if (INSN_P (p))
      {
	reset_used_flags (PATTERN (p));
	reset_used_flags (REG_NOTES (p));
	if (CALL_P (p))
	  reset_used_flags (CALL_INSN_FUNCTION_USAGE (p));
      }

  /* Make sure that virtual stack slots are not shared.  */
  set_used_decls (DECL_INITIAL (cfun->decl));

  /* Make sure that virtual parameters are not shared.  */
  for (decl = DECL_ARGUMENTS (cfun->decl); decl; decl = DECL_CHAIN (decl))
    set_used_flags (DECL_RTL (decl));

  rtx temp;
  unsigned int i;
  FOR_EACH_VEC_SAFE_ELT (stack_slot_list, i, temp)
    reset_used_flags (temp);

  unshare_all_rtl_1 (insn);
}

unsigned int
unshare_all_rtl (void)
{
  unshare_all_rtl_1 (get_insns ());
  return 0;
}


/* Check that ORIG is not marked when it should not be and mark ORIG as in use,
   Recursively does the same for subexpressions.  */

static void
verify_rtx_sharing (rtx orig, rtx insn)
{
  rtx x = orig;
  int i;
  enum rtx_code code;
  const char *format_ptr;

  if (x == 0)
    return;

  code = GET_CODE (x);

  /* These types may be freely shared.  */

  switch (code)
    {
    case REG:
    case DEBUG_EXPR:
    case VALUE:
    CASE_CONST_ANY:
    case SYMBOL_REF:
    case LABEL_REF:
    case CODE_LABEL:
    case PC:
    case CC0:
    case RETURN:
    case SIMPLE_RETURN:
    case SCRATCH:
      /* SCRATCH must be shared because they represent distinct values.  */
      return;
    case CLOBBER:
      /* Share clobbers of hard registers (like cc0), but do not share pseudo reg
         clobbers or clobbers of hard registers that originated as pseudos.
         This is needed to allow safe register renaming.  */
      if (REG_P (XEXP (x, 0)) && REGNO (XEXP (x, 0)) < FIRST_PSEUDO_REGISTER
	  && ORIGINAL_REGNO (XEXP (x, 0)) == REGNO (XEXP (x, 0)))
	return;
      break;

    case CONST:
      if (shared_const_p (orig))
	return;
      break;

    case MEM:
      /* A MEM is allowed to be shared if its address is constant.  */
      if (CONSTANT_ADDRESS_P (XEXP (x, 0))
	  || reload_completed || reload_in_progress)
	return;

      break;

    default:
      break;
    }

  /* This rtx may not be shared.  If it has already been seen,
     replace it with a copy of itself.  */
  if (flag_checking && RTX_FLAG (x, used))
    {
      error ("invalid rtl sharing found in the insn");
      debug_rtx (insn);
      error ("shared rtx");
      debug_rtx (x);
      internal_error ("internal consistency failure");
    }
  gcc_assert (!RTX_FLAG (x, used));

  RTX_FLAG (x, used) = 1;

  /* Now scan the subexpressions recursively.  */

  format_ptr = GET_RTX_FORMAT (code);

  for (i = 0; i < GET_RTX_LENGTH (code); i++)
    {
      switch (*format_ptr++)
	{
	case 'e':
	  verify_rtx_sharing (XEXP (x, i), insn);
	  break;

	case 'E':
	  if (XVEC (x, i) != NULL)
	    {
	      int j;
	      int len = XVECLEN (x, i);

	      for (j = 0; j < len; j++)
		{
		  /* We allow sharing of ASM_OPERANDS inside single
		     instruction.  */
		  if (j && GET_CODE (XVECEXP (x, i, j)) == SET
		      && (GET_CODE (SET_SRC (XVECEXP (x, i, j)))
			  == ASM_OPERANDS))
		    verify_rtx_sharing (SET_DEST (XVECEXP (x, i, j)), insn);
		  else
		    verify_rtx_sharing (XVECEXP (x, i, j), insn);
		}
	    }
	  break;
	}
    }
  return;
}

/* Reset used-flags for INSN.  */

static void
reset_insn_used_flags (rtx insn)
{
  gcc_assert (INSN_P (insn));
  reset_used_flags (PATTERN (insn));
  reset_used_flags (REG_NOTES (insn));
  if (CALL_P (insn))
    reset_used_flags (CALL_INSN_FUNCTION_USAGE (insn));
}

/* Go through all the RTL insn bodies and clear all the USED bits.  */

static void
reset_all_used_flags (void)
{
  rtx_insn *p;

  for (p = get_insns (); p; p = NEXT_INSN (p))
    if (INSN_P (p))
      {
	rtx pat = PATTERN (p);
	if (GET_CODE (pat) != SEQUENCE)
	  reset_insn_used_flags (p);
	else
	  {
	    gcc_assert (REG_NOTES (p) == NULL);
	    for (int i = 0; i < XVECLEN (pat, 0); i++)
	      {
		rtx insn = XVECEXP (pat, 0, i);
		if (INSN_P (insn))
		  reset_insn_used_flags (insn);
	      }
	  }
      }
}

/* Verify sharing in INSN.  */

static void
verify_insn_sharing (rtx insn)
{
  gcc_assert (INSN_P (insn));
  reset_used_flags (PATTERN (insn));
  reset_used_flags (REG_NOTES (insn));
  if (CALL_P (insn))
    reset_used_flags (CALL_INSN_FUNCTION_USAGE (insn));
}

/* Go through all the RTL insn bodies and check that there is no unexpected
   sharing in between the subexpressions.  */

DEBUG_FUNCTION void
verify_rtl_sharing (void)
{
  rtx_insn *p;

  timevar_push (TV_VERIFY_RTL_SHARING);

  reset_all_used_flags ();

  for (p = get_insns (); p; p = NEXT_INSN (p))
    if (INSN_P (p))
      {
	rtx pat = PATTERN (p);
	if (GET_CODE (pat) != SEQUENCE)
	  verify_insn_sharing (p);
	else
	  for (int i = 0; i < XVECLEN (pat, 0); i++)
	      {
		rtx insn = XVECEXP (pat, 0, i);
		if (INSN_P (insn))
		  verify_insn_sharing (insn);
	      }
      }

  reset_all_used_flags ();

  timevar_pop (TV_VERIFY_RTL_SHARING);
}

/* Go through all the RTL insn bodies and copy any invalid shared structure.
   Assumes the mark bits are cleared at entry.  */

void
unshare_all_rtl_in_chain (rtx_insn *insn)
{
  for (; insn; insn = NEXT_INSN (insn))
    if (INSN_P (insn))
      {
	PATTERN (insn) = copy_rtx_if_shared (PATTERN (insn));
	REG_NOTES (insn) = copy_rtx_if_shared (REG_NOTES (insn));
	if (CALL_P (insn))
	  CALL_INSN_FUNCTION_USAGE (insn)
	    = copy_rtx_if_shared (CALL_INSN_FUNCTION_USAGE (insn));
      }
}

/* Go through all virtual stack slots of a function and mark them as
   shared.  We never replace the DECL_RTLs themselves with a copy,
   but expressions mentioned into a DECL_RTL cannot be shared with
   expressions in the instruction stream.

   Note that reload may convert pseudo registers into memories in-place.
   Pseudo registers are always shared, but MEMs never are.  Thus if we
   reset the used flags on MEMs in the instruction stream, we must set
   them again on MEMs that appear in DECL_RTLs.  */

static void
set_used_decls (tree blk)
{
  tree t;

  /* Mark decls.  */
  for (t = BLOCK_VARS (blk); t; t = DECL_CHAIN (t))
    if (DECL_RTL_SET_P (t))
      set_used_flags (DECL_RTL (t));

  /* Now process sub-blocks.  */
  for (t = BLOCK_SUBBLOCKS (blk); t; t = BLOCK_CHAIN (t))
    set_used_decls (t);
}

/* Mark ORIG as in use, and return a copy of it if it was already in use.
   Recursively does the same for subexpressions.  Uses
   copy_rtx_if_shared_1 to reduce stack space.  */

rtx
copy_rtx_if_shared (rtx orig)
{
  copy_rtx_if_shared_1 (&orig);
  return orig;
}

/* Mark *ORIG1 as in use, and set it to a copy of it if it was already in
   use.  Recursively does the same for subexpressions.  */

static void
copy_rtx_if_shared_1 (rtx *orig1)
{
  rtx x;
  int i;
  enum rtx_code code;
  rtx *last_ptr;
  const char *format_ptr;
  int copied = 0;
  int length;

  /* Repeat is used to turn tail-recursion into iteration.  */
repeat:
  x = *orig1;

  if (x == 0)
    return;

  code = GET_CODE (x);

  /* These types may be freely shared.  */

  switch (code)
    {
    case REG:
    case DEBUG_EXPR:
    case VALUE:
    CASE_CONST_ANY:
    case SYMBOL_REF:
    case LABEL_REF:
    case CODE_LABEL:
    case PC:
    case CC0:
    case RETURN:
    case SIMPLE_RETURN:
    case SCRATCH:
      /* SCRATCH must be shared because they represent distinct values.  */
      return;
    case CLOBBER:
      /* Share clobbers of hard registers (like cc0), but do not share pseudo reg
         clobbers or clobbers of hard registers that originated as pseudos.
         This is needed to allow safe register renaming.  */
      if (REG_P (XEXP (x, 0)) && REGNO (XEXP (x, 0)) < FIRST_PSEUDO_REGISTER
	  && ORIGINAL_REGNO (XEXP (x, 0)) == REGNO (XEXP (x, 0)))
	return;
      break;

    case CONST:
      if (shared_const_p (x))
	return;
      break;

    case DEBUG_INSN:
    case INSN:
    case JUMP_INSN:
    case CALL_INSN:
    case NOTE:
    case BARRIER:
      /* The chain of insns is not being copied.  */
      return;

    default:
      break;
    }

  /* This rtx may not be shared.  If it has already been seen,
     replace it with a copy of itself.  */

  if (RTX_FLAG (x, used))
    {
      x = shallow_copy_rtx (x);
      copied = 1;
    }
  RTX_FLAG (x, used) = 1;

  /* Now scan the subexpressions recursively.
     We can store any replaced subexpressions directly into X
     since we know X is not shared!  Any vectors in X
     must be copied if X was copied.  */

  format_ptr = GET_RTX_FORMAT (code);
  length = GET_RTX_LENGTH (code);
  last_ptr = NULL;

  for (i = 0; i < length; i++)
    {
      switch (*format_ptr++)
	{
	case 'e':
          if (last_ptr)
            copy_rtx_if_shared_1 (last_ptr);
	  last_ptr = &XEXP (x, i);
	  break;

	case 'E':
	  if (XVEC (x, i) != NULL)
	    {
	      int j;
	      int len = XVECLEN (x, i);

              /* Copy the vector iff I copied the rtx and the length
		 is nonzero.  */
	      if (copied && len > 0)
		XVEC (x, i) = gen_rtvec_v (len, XVEC (x, i)->elem);

              /* Call recursively on all inside the vector.  */
	      for (j = 0; j < len; j++)
                {
		  if (last_ptr)
		    copy_rtx_if_shared_1 (last_ptr);
                  last_ptr = &XVECEXP (x, i, j);
                }
	    }
	  break;
	}
    }
  *orig1 = x;
  if (last_ptr)
    {
      orig1 = last_ptr;
      goto repeat;
    }
  return;
}

/* Set the USED bit in X and its non-shareable subparts to FLAG.  */

static void
mark_used_flags (rtx x, int flag)
{
  int i, j;
  enum rtx_code code;
  const char *format_ptr;
  int length;

  /* Repeat is used to turn tail-recursion into iteration.  */
repeat:
  if (x == 0)
    return;

  code = GET_CODE (x);

  /* These types may be freely shared so we needn't do any resetting
     for them.  */

  switch (code)
    {
    case REG:
    case DEBUG_EXPR:
    case VALUE:
    CASE_CONST_ANY:
    case SYMBOL_REF:
    case CODE_LABEL:
    case PC:
    case CC0:
    case RETURN:
    case SIMPLE_RETURN:
      return;

    case DEBUG_INSN:
    case INSN:
    case JUMP_INSN:
    case CALL_INSN:
    case NOTE:
    case LABEL_REF:
    case BARRIER:
      /* The chain of insns is not being copied.  */
      return;

    default:
      break;
    }

  RTX_FLAG (x, used) = flag;

  format_ptr = GET_RTX_FORMAT (code);
  length = GET_RTX_LENGTH (code);

  for (i = 0; i < length; i++)
    {
      switch (*format_ptr++)
	{
	case 'e':
          if (i == length-1)
            {
              x = XEXP (x, i);
	      goto repeat;
            }
	  mark_used_flags (XEXP (x, i), flag);
	  break;

	case 'E':
	  for (j = 0; j < XVECLEN (x, i); j++)
	    mark_used_flags (XVECEXP (x, i, j), flag);
	  break;
	}
    }
}

/* Clear all the USED bits in X to allow copy_rtx_if_shared to be used
   to look for shared sub-parts.  */

void
reset_used_flags (rtx x)
{
  mark_used_flags (x, 0);
}

/* Set all the USED bits in X to allow copy_rtx_if_shared to be used
   to look for shared sub-parts.  */

void
set_used_flags (rtx x)
{
  mark_used_flags (x, 1);
}

/* Copy X if necessary so that it won't be altered by changes in OTHER.
   Return X or the rtx for the pseudo reg the value of X was copied into.
   OTHER must be valid as a SET_DEST.  */

rtx
make_safe_from (rtx x, rtx other)
{
  while (1)
    switch (GET_CODE (other))
      {
      case SUBREG:
	other = SUBREG_REG (other);
	break;
      case STRICT_LOW_PART:
      case SIGN_EXTEND:
      case ZERO_EXTEND:
	other = XEXP (other, 0);
	break;
      default:
	goto done;
      }
 done:
  if ((MEM_P (other)
       && ! CONSTANT_P (x)
       && !REG_P (x)
       && GET_CODE (x) != SUBREG)
      || (REG_P (other)
	  && (REGNO (other) < FIRST_PSEUDO_REGISTER
	      || reg_mentioned_p (other, x))))
    {
      rtx temp = gen_reg_rtx (GET_MODE (x));
      emit_move_insn (temp, x);
      return temp;
    }
  return x;
}

/* Emission of insns (adding them to the doubly-linked list).  */

/* Return the last insn emitted, even if it is in a sequence now pushed.  */

rtx_insn *
get_last_insn_anywhere (void)
{
  struct sequence_stack *seq;
  for (seq = get_current_sequence (); seq; seq = seq->next)
    if (seq->last != 0)
      return seq->last;
  return 0;
}

/* Return the first nonnote insn emitted in current sequence or current
   function.  This routine looks inside SEQUENCEs.  */

rtx_insn *
get_first_nonnote_insn (void)
{
  rtx_insn *insn = get_insns ();

  if (insn)
    {
      if (NOTE_P (insn))
	for (insn = next_insn (insn);
	     insn && NOTE_P (insn);
	     insn = next_insn (insn))
	  continue;
      else
	{
	  if (NONJUMP_INSN_P (insn)
	      && GET_CODE (PATTERN (insn)) == SEQUENCE)
	    insn = as_a <rtx_sequence *> (PATTERN (insn))->insn (0);
	}
    }

  return insn;
}

/* Return the last nonnote insn emitted in current sequence or current
   function.  This routine looks inside SEQUENCEs.  */

rtx_insn *
get_last_nonnote_insn (void)
{
  rtx_insn *insn = get_last_insn ();

  if (insn)
    {
      if (NOTE_P (insn))
	for (insn = previous_insn (insn);
	     insn && NOTE_P (insn);
	     insn = previous_insn (insn))
	  continue;
      else
	{
	  if (NONJUMP_INSN_P (insn))
	    if (rtx_sequence *seq = dyn_cast <rtx_sequence *> (PATTERN (insn)))
	      insn = seq->insn (seq->len () - 1);
	}
    }

  return insn;
}

/* Return the number of actual (non-debug) insns emitted in this
   function.  */

int
get_max_insn_count (void)
{
  int n = cur_insn_uid;

  /* The table size must be stable across -g, to avoid codegen
     differences due to debug insns, and not be affected by
     -fmin-insn-uid, to avoid excessive table size and to simplify
     debugging of -fcompare-debug failures.  */
  if (cur_debug_insn_uid > MIN_NONDEBUG_INSN_UID)
    n -= cur_debug_insn_uid;
  else
    n -= MIN_NONDEBUG_INSN_UID;

  return n;
}


/* Return the next insn.  If it is a SEQUENCE, return the first insn
   of the sequence.  */

rtx_insn *
next_insn (rtx_insn *insn)
{
  if (insn)
    {
      insn = NEXT_INSN (insn);
      if (insn && NONJUMP_INSN_P (insn)
	  && GET_CODE (PATTERN (insn)) == SEQUENCE)
	insn = as_a <rtx_sequence *> (PATTERN (insn))->insn (0);
    }

  return insn;
}

/* Return the previous insn.  If it is a SEQUENCE, return the last insn
   of the sequence.  */

rtx_insn *
previous_insn (rtx_insn *insn)
{
  if (insn)
    {
      insn = PREV_INSN (insn);
      if (insn && NONJUMP_INSN_P (insn))
	if (rtx_sequence *seq = dyn_cast <rtx_sequence *> (PATTERN (insn)))
	  insn = seq->insn (seq->len () - 1);
    }

  return insn;
}

/* Return the next insn after INSN that is not a NOTE.  This routine does not
   look inside SEQUENCEs.  */

rtx_insn *
next_nonnote_insn (rtx uncast_insn)
{
  rtx_insn *insn = safe_as_a <rtx_insn *> (uncast_insn);
  while (insn)
    {
      insn = NEXT_INSN (insn);
      if (insn == 0 || !NOTE_P (insn))
	break;
    }

  return insn;
}

/* Return the next insn after INSN that is not a NOTE, but stop the
   search before we enter another basic block.  This routine does not
   look inside SEQUENCEs.  */

rtx_insn *
next_nonnote_insn_bb (rtx_insn *insn)
{
  while (insn)
    {
      insn = NEXT_INSN (insn);
      if (insn == 0 || !NOTE_P (insn))
	break;
      if (NOTE_INSN_BASIC_BLOCK_P (insn))
	return NULL;
    }

  return insn;
}

/* Return the previous insn before INSN that is not a NOTE.  This routine does
   not look inside SEQUENCEs.  */

rtx_insn *
prev_nonnote_insn (rtx uncast_insn)
{
  rtx_insn *insn = safe_as_a <rtx_insn *> (uncast_insn);

  while (insn)
    {
      insn = PREV_INSN (insn);
      if (insn == 0 || !NOTE_P (insn))
	break;
    }

  return insn;
}

/* Return the previous insn before INSN that is not a NOTE, but stop
   the search before we enter another basic block.  This routine does
   not look inside SEQUENCEs.  */

rtx_insn *
prev_nonnote_insn_bb (rtx uncast_insn)
{
  rtx_insn *insn = safe_as_a <rtx_insn *> (uncast_insn);

  while (insn)
    {
      insn = PREV_INSN (insn);
      if (insn == 0 || !NOTE_P (insn))
	break;
      if (NOTE_INSN_BASIC_BLOCK_P (insn))
	return NULL;
    }

  return insn;
}

/* Return the next insn after INSN that is not a DEBUG_INSN.  This
   routine does not look inside SEQUENCEs.  */

rtx_insn *
next_nondebug_insn (rtx uncast_insn)
{
  rtx_insn *insn = safe_as_a <rtx_insn *> (uncast_insn);

  while (insn)
    {
      insn = NEXT_INSN (insn);
      if (insn == 0 || !DEBUG_INSN_P (insn))
	break;
    }

  return insn;
}

/* Return the previous insn before INSN that is not a DEBUG_INSN.
   This routine does not look inside SEQUENCEs.  */

rtx_insn *
prev_nondebug_insn (rtx uncast_insn)
{
  rtx_insn *insn = safe_as_a <rtx_insn *> (uncast_insn);

  while (insn)
    {
      insn = PREV_INSN (insn);
      if (insn == 0 || !DEBUG_INSN_P (insn))
	break;
    }

  return insn;
}

/* Return the next insn after INSN that is not a NOTE nor DEBUG_INSN.
   This routine does not look inside SEQUENCEs.  */

rtx_insn *
next_nonnote_nondebug_insn (rtx uncast_insn)
{
  rtx_insn *insn = safe_as_a <rtx_insn *> (uncast_insn);

  while (insn)
    {
      insn = NEXT_INSN (insn);
      if (insn == 0 || (!NOTE_P (insn) && !DEBUG_INSN_P (insn)))
	break;
    }

  return insn;
}

/* Return the previous insn before INSN that is not a NOTE nor DEBUG_INSN.
   This routine does not look inside SEQUENCEs.  */

rtx_insn *
prev_nonnote_nondebug_insn (rtx uncast_insn)
{
  rtx_insn *insn = safe_as_a <rtx_insn *> (uncast_insn);

  while (insn)
    {
      insn = PREV_INSN (insn);
      if (insn == 0 || (!NOTE_P (insn) && !DEBUG_INSN_P (insn)))
	break;
    }

  return insn;
}

/* Return the next INSN, CALL_INSN or JUMP_INSN after INSN;
   or 0, if there is none.  This routine does not look inside
   SEQUENCEs.  */

rtx_insn *
next_real_insn (rtx uncast_insn)
{
  rtx_insn *insn = safe_as_a <rtx_insn *> (uncast_insn);

  while (insn)
    {
      insn = NEXT_INSN (insn);
      if (insn == 0 || INSN_P (insn))
	break;
    }

  return insn;
}

/* Return the last INSN, CALL_INSN or JUMP_INSN before INSN;
   or 0, if there is none.  This routine does not look inside
   SEQUENCEs.  */

rtx_insn *
prev_real_insn (rtx uncast_insn)
{
  rtx_insn *insn = safe_as_a <rtx_insn *> (uncast_insn);

  while (insn)
    {
      insn = PREV_INSN (insn);
      if (insn == 0 || INSN_P (insn))
	break;
    }

  return insn;
}

/* Return the last CALL_INSN in the current list, or 0 if there is none.
   This routine does not look inside SEQUENCEs.  */

rtx_call_insn *
last_call_insn (void)
{
  rtx_insn *insn;

  for (insn = get_last_insn ();
       insn && !CALL_P (insn);
       insn = PREV_INSN (insn))
    ;

  return safe_as_a <rtx_call_insn *> (insn);
}

/* Find the next insn after INSN that really does something.  This routine
   does not look inside SEQUENCEs.  After reload this also skips over
   standalone USE and CLOBBER insn.  */

int
active_insn_p (const_rtx insn)
{
  return (CALL_P (insn) || JUMP_P (insn)
	  || JUMP_TABLE_DATA_P (insn) /* FIXME */
	  || (NONJUMP_INSN_P (insn)
	      && (! reload_completed
		  || (GET_CODE (PATTERN (insn)) != USE
		      && GET_CODE (PATTERN (insn)) != CLOBBER))));
}

rtx_insn *
next_active_insn (rtx uncast_insn)
{
  rtx_insn *insn = safe_as_a <rtx_insn *> (uncast_insn);

  while (insn)
    {
      insn = NEXT_INSN (insn);
      if (insn == 0 || active_insn_p (insn))
	break;
    }

  return insn;
}

/* Find the last insn before INSN that really does something.  This routine
   does not look inside SEQUENCEs.  After reload this also skips over
   standalone USE and CLOBBER insn.  */

rtx_insn *
prev_active_insn (rtx uncast_insn)
{
  rtx_insn *insn = safe_as_a <rtx_insn *> (uncast_insn);

  while (insn)
    {
      insn = PREV_INSN (insn);
      if (insn == 0 || active_insn_p (insn))
	break;
    }

  return insn;
}

/* Return the next insn that uses CC0 after INSN, which is assumed to
   set it.  This is the inverse of prev_cc0_setter (i.e., prev_cc0_setter
   applied to the result of this function should yield INSN).

   Normally, this is simply the next insn.  However, if a REG_CC_USER note
   is present, it contains the insn that uses CC0.

   Return 0 if we can't find the insn.  */

rtx_insn *
next_cc0_user (rtx uncast_insn)
{
  rtx_insn *insn = safe_as_a <rtx_insn *> (uncast_insn);

  rtx note = find_reg_note (insn, REG_CC_USER, NULL_RTX);

  if (note)
    return safe_as_a <rtx_insn *> (XEXP (note, 0));

  insn = next_nonnote_insn (insn);
  if (insn && NONJUMP_INSN_P (insn) && GET_CODE (PATTERN (insn)) == SEQUENCE)
    insn = as_a <rtx_sequence *> (PATTERN (insn))->insn (0);

  if (insn && INSN_P (insn) && reg_mentioned_p (cc0_rtx, PATTERN (insn)))
    return insn;

  return 0;
}

/* Find the insn that set CC0 for INSN.  Unless INSN has a REG_CC_SETTER
   note, it is the previous insn.  */

rtx_insn *
prev_cc0_setter (rtx_insn *insn)
{
  rtx note = find_reg_note (insn, REG_CC_SETTER, NULL_RTX);

  if (note)
    return safe_as_a <rtx_insn *> (XEXP (note, 0));

  insn = prev_nonnote_insn (insn);
  gcc_assert (sets_cc0_p (PATTERN (insn)));

  return insn;
}

/* Find a RTX_AUTOINC class rtx which matches DATA.  */

static int
find_auto_inc (const_rtx x, const_rtx reg)
{
  subrtx_iterator::array_type array;
  FOR_EACH_SUBRTX (iter, array, x, NONCONST)
    {
      const_rtx x = *iter;
      if (GET_RTX_CLASS (GET_CODE (x)) == RTX_AUTOINC
	  && rtx_equal_p (reg, XEXP (x, 0)))
	return true;
    }
  return false;
}

/* Increment the label uses for all labels present in rtx.  */

static void
mark_label_nuses (rtx x)
{
  enum rtx_code code;
  int i, j;
  const char *fmt;

  code = GET_CODE (x);
  if (code == LABEL_REF && LABEL_P (LABEL_REF_LABEL (x)))
    LABEL_NUSES (LABEL_REF_LABEL (x))++;

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
	mark_label_nuses (XEXP (x, i));
      else if (fmt[i] == 'E')
	for (j = XVECLEN (x, i) - 1; j >= 0; j--)
	  mark_label_nuses (XVECEXP (x, i, j));
    }
}


/* Try splitting insns that can be split for better scheduling.
   PAT is the pattern which might split.
   TRIAL is the insn providing PAT.
   LAST is nonzero if we should return the last insn of the sequence produced.

   If this routine succeeds in splitting, it returns the first or last
   replacement insn depending on the value of LAST.  Otherwise, it
   returns TRIAL.  If the insn to be returned can be split, it will be.  */

rtx_insn *
try_split (rtx pat, rtx_insn *trial, int last)
{
  rtx_insn *before = PREV_INSN (trial);
  rtx_insn *after = NEXT_INSN (trial);
  rtx note;
  rtx_insn *seq, *tem;
  int probability;
  rtx_insn *insn_last, *insn;
  int njumps = 0;
  rtx_insn *call_insn = NULL;

  /* We're not good at redistributing frame information.  */
  if (RTX_FRAME_RELATED_P (trial))
    return trial;

  if (any_condjump_p (trial)
      && (note = find_reg_note (trial, REG_BR_PROB, 0)))
    split_branch_probability = XINT (note, 0);
  probability = split_branch_probability;

  seq = split_insns (pat, trial);

  split_branch_probability = -1;

  if (!seq)
    return trial;

  /* Avoid infinite loop if any insn of the result matches
     the original pattern.  */
  insn_last = seq;
  while (1)
    {
      if (INSN_P (insn_last)
	  && rtx_equal_p (PATTERN (insn_last), pat))
	return trial;
      if (!NEXT_INSN (insn_last))
	break;
      insn_last = NEXT_INSN (insn_last);
    }

  /* We will be adding the new sequence to the function.  The splitters
     may have introduced invalid RTL sharing, so unshare the sequence now.  */
  unshare_all_rtl_in_chain (seq);

  /* Mark labels and copy flags.  */
  for (insn = insn_last; insn ; insn = PREV_INSN (insn))
    {
      if (JUMP_P (insn))
	{
	  if (JUMP_P (trial))
	    CROSSING_JUMP_P (insn) = CROSSING_JUMP_P (trial);
	  mark_jump_label (PATTERN (insn), insn, 0);
	  njumps++;
	  if (probability != -1
	      && any_condjump_p (insn)
	      && !find_reg_note (insn, REG_BR_PROB, 0))
	    {
	      /* We can preserve the REG_BR_PROB notes only if exactly
		 one jump is created, otherwise the machine description
		 is responsible for this step using
		 split_branch_probability variable.  */
	      gcc_assert (njumps == 1);
	      add_int_reg_note (insn, REG_BR_PROB, probability);
	    }
	}
    }

  /* If we are splitting a CALL_INSN, look for the CALL_INSN
     in SEQ and copy any additional information across.  */
  if (CALL_P (trial))
    {
      for (insn = insn_last; insn ; insn = PREV_INSN (insn))
	if (CALL_P (insn))
	  {
	    rtx_insn *next;
	    rtx *p;

	    gcc_assert (call_insn == NULL_RTX);
	    call_insn = insn;

	    /* Add the old CALL_INSN_FUNCTION_USAGE to whatever the
	       target may have explicitly specified.  */
	    p = &CALL_INSN_FUNCTION_USAGE (insn);
	    while (*p)
	      p = &XEXP (*p, 1);
	    *p = CALL_INSN_FUNCTION_USAGE (trial);

	    /* If the old call was a sibling call, the new one must
	       be too.  */
	    SIBLING_CALL_P (insn) = SIBLING_CALL_P (trial);

	    /* If the new call is the last instruction in the sequence,
	       it will effectively replace the old call in-situ.  Otherwise
	       we must move any following NOTE_INSN_CALL_ARG_LOCATION note
	       so that it comes immediately after the new call.  */
	    if (NEXT_INSN (insn))
	      for (next = NEXT_INSN (trial);
		   next && NOTE_P (next);
		   next = NEXT_INSN (next))
		if (NOTE_KIND (next) == NOTE_INSN_CALL_ARG_LOCATION)
		  {
		    remove_insn (next);
		    add_insn_after (next, insn, NULL);
		    break;
		  }
	  }
    }

  /* Copy notes, particularly those related to the CFG.  */
  for (note = REG_NOTES (trial); note; note = XEXP (note, 1))
    {
      switch (REG_NOTE_KIND (note))
	{
	case REG_EH_REGION:
	  copy_reg_eh_region_note_backward (note, insn_last, NULL);
	  break;

	case REG_NORETURN:
	case REG_SETJMP:
	case REG_TM:
	  for (insn = insn_last; insn != NULL_RTX; insn = PREV_INSN (insn))
	    {
	      if (CALL_P (insn))
		add_reg_note (insn, REG_NOTE_KIND (note), XEXP (note, 0));
	    }
	  break;

	case REG_NON_LOCAL_GOTO:
	  for (insn = insn_last; insn != NULL_RTX; insn = PREV_INSN (insn))
	    {
	      if (JUMP_P (insn))
		add_reg_note (insn, REG_NOTE_KIND (note), XEXP (note, 0));
	    }
	  break;

	case REG_INC:
	  if (!AUTO_INC_DEC)
	    break;

	  for (insn = insn_last; insn != NULL_RTX; insn = PREV_INSN (insn))
	    {
	      rtx reg = XEXP (note, 0);
	      if (!FIND_REG_INC_NOTE (insn, reg)
		  && find_auto_inc (PATTERN (insn), reg))
		add_reg_note (insn, REG_INC, reg);
	    }
	  break;

	case REG_ARGS_SIZE:
	  fixup_args_size_notes (NULL, insn_last, INTVAL (XEXP (note, 0)));
	  break;

	case REG_CALL_DECL:
	  gcc_assert (call_insn != NULL_RTX);
	  add_reg_note (call_insn, REG_NOTE_KIND (note), XEXP (note, 0));
	  break;

	default:
	  break;
	}
    }

  /* If there are LABELS inside the split insns increment the
     usage count so we don't delete the label.  */
  if (INSN_P (trial))
    {
      insn = insn_last;
      while (insn != NULL_RTX)
	{
	  /* JUMP_P insns have already been "marked" above.  */
	  if (NONJUMP_INSN_P (insn))
	    mark_label_nuses (PATTERN (insn));

	  insn = PREV_INSN (insn);
	}
    }

  tem = emit_insn_after_setloc (seq, trial, INSN_LOCATION (trial));

  delete_insn (trial);

  /* Recursively call try_split for each new insn created; by the
     time control returns here that insn will be fully split, so
     set LAST and continue from the insn after the one returned.
     We can't use next_active_insn here since AFTER may be a note.
     Ignore deleted insns, which can be occur if not optimizing.  */
  for (tem = NEXT_INSN (before); tem != after; tem = NEXT_INSN (tem))
    if (! tem->deleted () && INSN_P (tem))
      tem = try_split (PATTERN (tem), tem, 1);

  /* Return either the first or the last insn, depending on which was
     requested.  */
  return last
    ? (after ? PREV_INSN (after) : get_last_insn ())
    : NEXT_INSN (before);
}

/* Make and return an INSN rtx, initializing all its slots.
   Store PATTERN in the pattern slots.  */

rtx_insn *
make_insn_raw (rtx pattern)
{
  rtx_insn *insn;

  insn = as_a <rtx_insn *> (rtx_alloc (INSN));

  INSN_UID (insn) = cur_insn_uid++;
  PATTERN (insn) = pattern;
  INSN_CODE (insn) = -1;
  REG_NOTES (insn) = NULL;
  INSN_LOCATION (insn) = curr_insn_location ();
  BLOCK_FOR_INSN (insn) = NULL;

#ifdef ENABLE_RTL_CHECKING
  if (insn
      && INSN_P (insn)
      && (returnjump_p (insn)
	  || (GET_CODE (insn) == SET
	      && SET_DEST (insn) == pc_rtx)))
    {
      warning (0, "ICE: emit_insn used where emit_jump_insn needed:\n");
      debug_rtx (insn);
    }
#endif

  return insn;
}

/* Like `make_insn_raw' but make a DEBUG_INSN instead of an insn.  */

static rtx_insn *
make_debug_insn_raw (rtx pattern)
{
  rtx_debug_insn *insn;

  insn = as_a <rtx_debug_insn *> (rtx_alloc (DEBUG_INSN));
  INSN_UID (insn) = cur_debug_insn_uid++;
  if (cur_debug_insn_uid > MIN_NONDEBUG_INSN_UID)
    INSN_UID (insn) = cur_insn_uid++;

  PATTERN (insn) = pattern;
  INSN_CODE (insn) = -1;
  REG_NOTES (insn) = NULL;
  INSN_LOCATION (insn) = curr_insn_location ();
  BLOCK_FOR_INSN (insn) = NULL;

  return insn;
}

/* Like `make_insn_raw' but make a JUMP_INSN instead of an insn.  */

static rtx_insn *
make_jump_insn_raw (rtx pattern)
{
  rtx_jump_insn *insn;

  insn = as_a <rtx_jump_insn *> (rtx_alloc (JUMP_INSN));
  INSN_UID (insn) = cur_insn_uid++;

  PATTERN (insn) = pattern;
  INSN_CODE (insn) = -1;
  REG_NOTES (insn) = NULL;
  JUMP_LABEL (insn) = NULL;
  INSN_LOCATION (insn) = curr_insn_location ();
  BLOCK_FOR_INSN (insn) = NULL;

  return insn;
}

/* Like `make_insn_raw' but make a CALL_INSN instead of an insn.  */

static rtx_insn *
make_call_insn_raw (rtx pattern)
{
  rtx_call_insn *insn;

  insn = as_a <rtx_call_insn *> (rtx_alloc (CALL_INSN));
  INSN_UID (insn) = cur_insn_uid++;

  PATTERN (insn) = pattern;
  INSN_CODE (insn) = -1;
  REG_NOTES (insn) = NULL;
  CALL_INSN_FUNCTION_USAGE (insn) = NULL;
  INSN_LOCATION (insn) = curr_insn_location ();
  BLOCK_FOR_INSN (insn) = NULL;

  return insn;
}

/* Like `make_insn_raw' but make a NOTE instead of an insn.  */

static rtx_note *
make_note_raw (enum insn_note subtype)
{
  /* Some notes are never created this way at all.  These notes are
     only created by patching out insns.  */
  gcc_assert (subtype != NOTE_INSN_DELETED_LABEL
	      && subtype != NOTE_INSN_DELETED_DEBUG_LABEL);

  rtx_note *note = as_a <rtx_note *> (rtx_alloc (NOTE));
  INSN_UID (note) = cur_insn_uid++;
  NOTE_KIND (note) = subtype;
  BLOCK_FOR_INSN (note) = NULL;
  memset (&NOTE_DATA (note), 0, sizeof (NOTE_DATA (note)));
  return note;
}

/* Add INSN to the end of the doubly-linked list, between PREV and NEXT.
   INSN may be any object that can appear in the chain: INSN_P and NOTE_P objects,
   but also BARRIERs and JUMP_TABLE_DATAs.  PREV and NEXT may be NULL.  */

static inline void
link_insn_into_chain (rtx_insn *insn, rtx_insn *prev, rtx_insn *next)
{
  SET_PREV_INSN (insn) = prev;
  SET_NEXT_INSN (insn) = next;
  if (prev != NULL)
    {
      SET_NEXT_INSN (prev) = insn;
      if (NONJUMP_INSN_P (prev) && GET_CODE (PATTERN (prev)) == SEQUENCE)
	{
	  rtx_sequence *sequence = as_a <rtx_sequence *> (PATTERN (prev));
	  SET_NEXT_INSN (sequence->insn (sequence->len () - 1)) = insn;
	}
    }
  if (next != NULL)
    {
      SET_PREV_INSN (next) = insn;
      if (NONJUMP_INSN_P (next) && GET_CODE (PATTERN (next)) == SEQUENCE)
	{
	  rtx_sequence *sequence = as_a <rtx_sequence *> (PATTERN (next));
	  SET_PREV_INSN (sequence->insn (0)) = insn;
	}
    }

  if (NONJUMP_INSN_P (insn) && GET_CODE (PATTERN (insn)) == SEQUENCE)
    {
      rtx_sequence *sequence = as_a <rtx_sequence *> (PATTERN (insn));
      SET_PREV_INSN (sequence->insn (0)) = prev;
      SET_NEXT_INSN (sequence->insn (sequence->len () - 1)) = next;
    }
}

/* Add INSN to the end of the doubly-linked list.
   INSN may be an INSN, JUMP_INSN, CALL_INSN, CODE_LABEL, BARRIER or NOTE.  */

void
add_insn (rtx_insn *insn)
{
  rtx_insn *prev = get_last_insn ();
  link_insn_into_chain (insn, prev, NULL);
  if (NULL == get_insns ())
    set_first_insn (insn);
  set_last_insn (insn);
}

/* Add INSN into the doubly-linked list after insn AFTER.  */

static void
add_insn_after_nobb (rtx_insn *insn, rtx_insn *after)
{
  rtx_insn *next = NEXT_INSN (after);

  gcc_assert (!optimize || !after->deleted ());

  link_insn_into_chain (insn, after, next);

  if (next == NULL)
    {
      struct sequence_stack *seq;

      for (seq = get_current_sequence (); seq; seq = seq->next)
	if (after == seq->last)
	  {
	    seq->last = insn;
	    break;
	  }
    }
}

/* Add INSN into the doubly-linked list before insn BEFORE.  */

static void
add_insn_before_nobb (rtx_insn *insn, rtx_insn *before)
{
  rtx_insn *prev = PREV_INSN (before);

  gcc_assert (!optimize || !before->deleted ());

  link_insn_into_chain (insn, prev, before);

  if (prev == NULL)
    {
      struct sequence_stack *seq;

      for (seq = get_current_sequence (); seq; seq = seq->next)
	if (before == seq->first)
	  {
	    seq->first = insn;
	    break;
	  }

      gcc_assert (seq);
    }
}

/* Like add_insn_after_nobb, but try to set BLOCK_FOR_INSN.
   If BB is NULL, an attempt is made to infer the bb from before.

   This and the next function should be the only functions called
   to insert an insn once delay slots have been filled since only
   they know how to update a SEQUENCE. */

void
add_insn_after (rtx uncast_insn, rtx uncast_after, basic_block bb)
{
  rtx_insn *insn = as_a <rtx_insn *> (uncast_insn);
  rtx_insn *after = as_a <rtx_insn *> (uncast_after);
  add_insn_after_nobb (insn, after);
  if (!BARRIER_P (after)
      && !BARRIER_P (insn)
      && (bb = BLOCK_FOR_INSN (after)))
    {
      set_block_for_insn (insn, bb);
      if (INSN_P (insn))
	df_insn_rescan (insn);
      /* Should not happen as first in the BB is always
	 either NOTE or LABEL.  */
      if (BB_END (bb) == after
	  /* Avoid clobbering of structure when creating new BB.  */
	  && !BARRIER_P (insn)
	  && !NOTE_INSN_BASIC_BLOCK_P (insn))
	BB_END (bb) = insn;
    }
}

/* Like add_insn_before_nobb, but try to set BLOCK_FOR_INSN.
   If BB is NULL, an attempt is made to infer the bb from before.

   This and the previous function should be the only functions called
   to insert an insn once delay slots have been filled since only
   they know how to update a SEQUENCE. */

void
add_insn_before (rtx uncast_insn, rtx uncast_before, basic_block bb)
{
  rtx_insn *insn = as_a <rtx_insn *> (uncast_insn);
  rtx_insn *before = as_a <rtx_insn *> (uncast_before);
  add_insn_before_nobb (insn, before);

  if (!bb
      && !BARRIER_P (before)
      && !BARRIER_P (insn))
    bb = BLOCK_FOR_INSN (before);

  if (bb)
    {
      set_block_for_insn (insn, bb);
      if (INSN_P (insn))
	df_insn_rescan (insn);
      /* Should not happen as first in the BB is always either NOTE or
	 LABEL.  */
      gcc_assert (BB_HEAD (bb) != insn
		  /* Avoid clobbering of structure when creating new BB.  */
		  || BARRIER_P (insn)
		  || NOTE_INSN_BASIC_BLOCK_P (insn));
    }
}

/* Replace insn with an deleted instruction note.  */

void
set_insn_deleted (rtx insn)
{
  if (INSN_P (insn))
    df_insn_delete (as_a <rtx_insn *> (insn));
  PUT_CODE (insn, NOTE);
  NOTE_KIND (insn) = NOTE_INSN_DELETED;
}


/* Unlink INSN from the insn chain.

   This function knows how to handle sequences.
   
   This function does not invalidate data flow information associated with
   INSN (i.e. does not call df_insn_delete).  That makes this function
   usable for only disconnecting an insn from the chain, and re-emit it
   elsewhere later.

   To later insert INSN elsewhere in the insn chain via add_insn and
   similar functions, PREV_INSN and NEXT_INSN must be nullified by
   the caller.  Nullifying them here breaks many insn chain walks.

   To really delete an insn and related DF information, use delete_insn.  */

void
remove_insn (rtx uncast_insn)
{
  rtx_insn *insn = as_a <rtx_insn *> (uncast_insn);
  rtx_insn *next = NEXT_INSN (insn);
  rtx_insn *prev = PREV_INSN (insn);
  basic_block bb;

  if (prev)
    {
      SET_NEXT_INSN (prev) = next;
      if (NONJUMP_INSN_P (prev) && GET_CODE (PATTERN (prev)) == SEQUENCE)
	{
	  rtx_sequence *sequence = as_a <rtx_sequence *> (PATTERN (prev));
	  SET_NEXT_INSN (sequence->insn (sequence->len () - 1)) = next;
	}
    }
  else
    {
      struct sequence_stack *seq;

      for (seq = get_current_sequence (); seq; seq = seq->next)
	if (insn == seq->first)
	  {
	    seq->first = next;
	    break;
	  }

      gcc_assert (seq);
    }

  if (next)
    {
      SET_PREV_INSN (next) = prev;
      if (NONJUMP_INSN_P (next) && GET_CODE (PATTERN (next)) == SEQUENCE)
	{
	  rtx_sequence *sequence = as_a <rtx_sequence *> (PATTERN (next));
	  SET_PREV_INSN (sequence->insn (0)) = prev;
	}
    }
  else
    {
      struct sequence_stack *seq;

      for (seq = get_current_sequence (); seq; seq = seq->next)
	if (insn == seq->last)
	  {
	    seq->last = prev;
	    break;
	  }

      gcc_assert (seq);
    }

  /* Fix up basic block boundaries, if necessary.  */
  if (!BARRIER_P (insn)
      && (bb = BLOCK_FOR_INSN (insn)))
    {
      if (BB_HEAD (bb) == insn)
	{
	  /* Never ever delete the basic block note without deleting whole
	     basic block.  */
	  gcc_assert (!NOTE_P (insn));
	  BB_HEAD (bb) = next;
	}
      if (BB_END (bb) == insn)
	BB_END (bb) = prev;
    }
}

/* Append CALL_FUSAGE to the CALL_INSN_FUNCTION_USAGE for CALL_INSN.  */

void
add_function_usage_to (rtx call_insn, rtx call_fusage)
{
  gcc_assert (call_insn && CALL_P (call_insn));

  /* Put the register usage information on the CALL.  If there is already
     some usage information, put ours at the end.  */
  if (CALL_INSN_FUNCTION_USAGE (call_insn))
    {
      rtx link;

      for (link = CALL_INSN_FUNCTION_USAGE (call_insn); XEXP (link, 1) != 0;
	   link = XEXP (link, 1))
	;

      XEXP (link, 1) = call_fusage;
    }
  else
    CALL_INSN_FUNCTION_USAGE (call_insn) = call_fusage;
}

/* Delete all insns made since FROM.
   FROM becomes the new last instruction.  */

void
delete_insns_since (rtx_insn *from)
{
  if (from == 0)
    set_first_insn (0);
  else
    SET_NEXT_INSN (from) = 0;
  set_last_insn (from);
}

/* This function is deprecated, please use sequences instead.

   Move a consecutive bunch of insns to a different place in the chain.
   The insns to be moved are those between FROM and TO.
   They are moved to a new position after the insn AFTER.
   AFTER must not be FROM or TO or any insn in between.

   This function does not know about SEQUENCEs and hence should not be
   called after delay-slot filling has been done.  */

void
reorder_insns_nobb (rtx_insn *from, rtx_insn *to, rtx_insn *after)
{
  if (flag_checking)
    {
      for (rtx_insn *x = from; x != to; x = NEXT_INSN (x))
	gcc_assert (after != x);
      gcc_assert (after != to);
    }

  /* Splice this bunch out of where it is now.  */
  if (PREV_INSN (from))
    SET_NEXT_INSN (PREV_INSN (from)) = NEXT_INSN (to);
  if (NEXT_INSN (to))
    SET_PREV_INSN (NEXT_INSN (to)) = PREV_INSN (from);
  if (get_last_insn () == to)
    set_last_insn (PREV_INSN (from));
  if (get_insns () == from)
    set_first_insn (NEXT_INSN (to));

  /* Make the new neighbors point to it and it to them.  */
  if (NEXT_INSN (after))
    SET_PREV_INSN (NEXT_INSN (after)) = to;

  SET_NEXT_INSN (to) = NEXT_INSN (after);
  SET_PREV_INSN (from) = after;
  SET_NEXT_INSN (after) = from;
  if (after == get_last_insn ())
    set_last_insn (to);
}

/* Same as function above, but take care to update BB boundaries.  */
void
reorder_insns (rtx_insn *from, rtx_insn *to, rtx_insn *after)
{
  rtx_insn *prev = PREV_INSN (from);
  basic_block bb, bb2;

  reorder_insns_nobb (from, to, after);

  if (!BARRIER_P (after)
      && (bb = BLOCK_FOR_INSN (after)))
    {
      rtx_insn *x;
      df_set_bb_dirty (bb);

      if (!BARRIER_P (from)
	  && (bb2 = BLOCK_FOR_INSN (from)))
	{
	  if (BB_END (bb2) == to)
	    BB_END (bb2) = prev;
	  df_set_bb_dirty (bb2);
	}

      if (BB_END (bb) == after)
	BB_END (bb) = to;

      for (x = from; x != NEXT_INSN (to); x = NEXT_INSN (x))
	if (!BARRIER_P (x))
	  df_insn_change_bb (x, bb);
    }
}


/* Emit insn(s) of given code and pattern
   at a specified place within the doubly-linked list.

   All of the emit_foo global entry points accept an object
   X which is either an insn list or a PATTERN of a single
   instruction.

   There are thus a few canonical ways to generate code and
   emit it at a specific place in the instruction stream.  For
   example, consider the instruction named SPOT and the fact that
   we would like to emit some instructions before SPOT.  We might
   do it like this:

	start_sequence ();
	... emit the new instructions ...
	insns_head = get_insns ();
	end_sequence ();

	emit_insn_before (insns_head, SPOT);

   It used to be common to generate SEQUENCE rtl instead, but that
   is a relic of the past which no longer occurs.  The reason is that
   SEQUENCE rtl results in much fragmented RTL memory since the SEQUENCE
   generated would almost certainly die right after it was created.  */

static rtx_insn *
emit_pattern_before_noloc (rtx x, rtx before, rtx last, basic_block bb,
                           rtx_insn *(*make_raw) (rtx))
{
  rtx_insn *insn;

  gcc_assert (before);

  if (x == NULL_RTX)
    return safe_as_a <rtx_insn *> (last);

  switch (GET_CODE (x))
    {
    case DEBUG_INSN:
    case INSN:
    case JUMP_INSN:
    case CALL_INSN:
    case CODE_LABEL:
    case BARRIER:
    case NOTE:
      insn = as_a <rtx_insn *> (x);
      while (insn)
	{
	  rtx_insn *next = NEXT_INSN (insn);
	  add_insn_before (insn, before, bb);
	  last = insn;
	  insn = next;
	}
      break;

#ifdef ENABLE_RTL_CHECKING
    case SEQUENCE:
      gcc_unreachable ();
      break;
#endif

    default:
      last = (*make_raw) (x);
      add_insn_before (last, before, bb);
      break;
    }

  return safe_as_a <rtx_insn *> (last);
}

/* Make X be output before the instruction BEFORE.  */

rtx_insn *
emit_insn_before_noloc (rtx x, rtx_insn *before, basic_block bb)
{
  return emit_pattern_before_noloc (x, before, before, bb, make_insn_raw);
}

/* Make an instruction with body X and code JUMP_INSN
   and output it before the instruction BEFORE.  */

rtx_jump_insn *
emit_jump_insn_before_noloc (rtx x, rtx_insn *before)
{
  return as_a <rtx_jump_insn *> (
		emit_pattern_before_noloc (x, before, NULL_RTX, NULL,
					   make_jump_insn_raw));
}

/* Make an instruction with body X and code CALL_INSN
   and output it before the instruction BEFORE.  */

rtx_insn *
emit_call_insn_before_noloc (rtx x, rtx_insn *before)
{
  return emit_pattern_before_noloc (x, before, NULL_RTX, NULL,
				    make_call_insn_raw);
}

/* Make an instruction with body X and code DEBUG_INSN
   and output it before the instruction BEFORE.  */

rtx_insn *
emit_debug_insn_before_noloc (rtx x, rtx before)
{
  return emit_pattern_before_noloc (x, before, NULL_RTX, NULL,
				    make_debug_insn_raw);
}

/* Make an insn of code BARRIER
   and output it before the insn BEFORE.  */

rtx_barrier *
emit_barrier_before (rtx before)
{
  rtx_barrier *insn = as_a <rtx_barrier *> (rtx_alloc (BARRIER));

  INSN_UID (insn) = cur_insn_uid++;

  add_insn_before (insn, before, NULL);
  return insn;
}

/* Emit the label LABEL before the insn BEFORE.  */

rtx_code_label *
emit_label_before (rtx label, rtx_insn *before)
{
  gcc_checking_assert (INSN_UID (label) == 0);
  INSN_UID (label) = cur_insn_uid++;
  add_insn_before (label, before, NULL);
  return as_a <rtx_code_label *> (label);
}

/* Helper for emit_insn_after, handles lists of instructions
   efficiently.  */

static rtx_insn *
emit_insn_after_1 (rtx_insn *first, rtx uncast_after, basic_block bb)
{
  rtx_insn *after = safe_as_a <rtx_insn *> (uncast_after);
  rtx_insn *last;
  rtx_insn *after_after;
  if (!bb && !BARRIER_P (after))
    bb = BLOCK_FOR_INSN (after);

  if (bb)
    {
      df_set_bb_dirty (bb);
      for (last = first; NEXT_INSN (last); last = NEXT_INSN (last))
	if (!BARRIER_P (last))
	  {
	    set_block_for_insn (last, bb);
	    df_insn_rescan (last);
	  }
      if (!BARRIER_P (last))
	{
	  set_block_for_insn (last, bb);
	  df_insn_rescan (last);
	}
      if (BB_END (bb) == after)
	BB_END (bb) = last;
    }
  else
    for (last = first; NEXT_INSN (last); last = NEXT_INSN (last))
      continue;

  after_after = NEXT_INSN (after);

  SET_NEXT_INSN (after) = first;
  SET_PREV_INSN (first) = after;
  SET_NEXT_INSN (last) = after_after;
  if (after_after)
    SET_PREV_INSN (after_after) = last;

  if (after == get_last_insn ())
    set_last_insn (last);

  return last;
}

static rtx_insn *
emit_pattern_after_noloc (rtx x, rtx uncast_after, basic_block bb,
			  rtx_insn *(*make_raw)(rtx))
{
  rtx_insn *after = safe_as_a <rtx_insn *> (uncast_after);
  rtx_insn *last = after;

  gcc_assert (after);

  if (x == NULL_RTX)
    return last;

  switch (GET_CODE (x))
    {
    case DEBUG_INSN:
    case INSN:
    case JUMP_INSN:
    case CALL_INSN:
    case CODE_LABEL:
    case BARRIER:
    case NOTE:
      last = emit_insn_after_1 (as_a <rtx_insn *> (x), after, bb);
      break;

#ifdef ENABLE_RTL_CHECKING
    case SEQUENCE:
      gcc_unreachable ();
      break;
#endif

    default:
      last = (*make_raw) (x);
      add_insn_after (last, after, bb);
      break;
    }

  return last;
}

/* Make X be output after the insn AFTER and set the BB of insn.  If
   BB is NULL, an attempt is made to infer the BB from AFTER.  */

rtx_insn *
emit_insn_after_noloc (rtx x, rtx after, basic_block bb)
{
  return emit_pattern_after_noloc (x, after, bb, make_insn_raw);
}


/* Make an insn of code JUMP_INSN with body X
   and output it after the insn AFTER.  */

rtx_jump_insn *
emit_jump_insn_after_noloc (rtx x, rtx after)
{
  return as_a <rtx_jump_insn *> (
		emit_pattern_after_noloc (x, after, NULL, make_jump_insn_raw));
}

/* Make an instruction with body X and code CALL_INSN
   and output it after the instruction AFTER.  */

rtx_insn *
emit_call_insn_after_noloc (rtx x, rtx after)
{
  return emit_pattern_after_noloc (x, after, NULL, make_call_insn_raw);
}

/* Make an instruction with body X and code CALL_INSN
   and output it after the instruction AFTER.  */

rtx_insn *
emit_debug_insn_after_noloc (rtx x, rtx after)
{
  return emit_pattern_after_noloc (x, after, NULL, make_debug_insn_raw);
}

/* Make an insn of code BARRIER
   and output it after the insn AFTER.  */

rtx_barrier *
emit_barrier_after (rtx after)
{
  rtx_barrier *insn = as_a <rtx_barrier *> (rtx_alloc (BARRIER));

  INSN_UID (insn) = cur_insn_uid++;

  add_insn_after (insn, after, NULL);
  return insn;
}

/* Emit the label LABEL after the insn AFTER.  */

rtx_insn *
emit_label_after (rtx label, rtx_insn *after)
{
  gcc_checking_assert (INSN_UID (label) == 0);
  INSN_UID (label) = cur_insn_uid++;
  add_insn_after (label, after, NULL);
  return as_a <rtx_insn *> (label);
}

/* Notes require a bit of special handling: Some notes need to have their
   BLOCK_FOR_INSN set, others should never have it set, and some should
   have it set or clear depending on the context.   */

/* Return true iff a note of kind SUBTYPE should be emitted with routines
   that never set BLOCK_FOR_INSN on NOTE.  BB_BOUNDARY is true if the
   caller is asked to emit a note before BB_HEAD, or after BB_END.  */

static bool
note_outside_basic_block_p (enum insn_note subtype, bool on_bb_boundary_p)
{
  switch (subtype)
    {
      /* NOTE_INSN_SWITCH_TEXT_SECTIONS only appears between basic blocks.  */
      case NOTE_INSN_SWITCH_TEXT_SECTIONS:
	return true;

      /* Notes for var tracking and EH region markers can appear between or
	 inside basic blocks.  If the caller is emitting on the basic block
	 boundary, do not set BLOCK_FOR_INSN on the new note.  */
      case NOTE_INSN_VAR_LOCATION:
      case NOTE_INSN_CALL_ARG_LOCATION:
      case NOTE_INSN_EH_REGION_BEG:
      case NOTE_INSN_EH_REGION_END:
	return on_bb_boundary_p;

      /* Otherwise, BLOCK_FOR_INSN must be set.  */
      default:
	return false;
    }
}

/* Emit a note of subtype SUBTYPE after the insn AFTER.  */

rtx_note *
emit_note_after (enum insn_note subtype, rtx_insn *after)
{
  rtx_note *note = make_note_raw (subtype);
  basic_block bb = BARRIER_P (after) ? NULL : BLOCK_FOR_INSN (after);
  bool on_bb_boundary_p = (bb != NULL && BB_END (bb) == after);

  if (note_outside_basic_block_p (subtype, on_bb_boundary_p))
    add_insn_after_nobb (note, after);
  else
    add_insn_after (note, after, bb);
  return note;
}

/* Emit a note of subtype SUBTYPE before the insn BEFORE.  */

rtx_note *
emit_note_before (enum insn_note subtype, rtx_insn *before)
{
  rtx_note *note = make_note_raw (subtype);
  basic_block bb = BARRIER_P (before) ? NULL : BLOCK_FOR_INSN (before);
  bool on_bb_boundary_p = (bb != NULL && BB_HEAD (bb) == before);

  if (note_outside_basic_block_p (subtype, on_bb_boundary_p))
    add_insn_before_nobb (note, before);
  else
    add_insn_before (note, before, bb);
  return note;
}

/* Insert PATTERN after AFTER, setting its INSN_LOCATION to LOC.
   MAKE_RAW indicates how to turn PATTERN into a real insn.  */

static rtx_insn *
emit_pattern_after_setloc (rtx pattern, rtx uncast_after, int loc,
			   rtx_insn *(*make_raw) (rtx))
{
  rtx_insn *after = safe_as_a <rtx_insn *> (uncast_after);
  rtx_insn *last = emit_pattern_after_noloc (pattern, after, NULL, make_raw);

  if (pattern == NULL_RTX || !loc)
    return last;

  after = NEXT_INSN (after);
  while (1)
    {
      if (active_insn_p (after)
	  && !JUMP_TABLE_DATA_P (after) /* FIXME */
	  && !INSN_LOCATION (after))
	INSN_LOCATION (after) = loc;
      if (after == last)
	break;
      after = NEXT_INSN (after);
    }
  return last;
}

/* Insert PATTERN after AFTER.  MAKE_RAW indicates how to turn PATTERN
   into a real insn.  SKIP_DEBUG_INSNS indicates whether to insert after
   any DEBUG_INSNs.  */

static rtx_insn *
emit_pattern_after (rtx pattern, rtx uncast_after, bool skip_debug_insns,
		    rtx_insn *(*make_raw) (rtx))
{
  rtx_insn *after = safe_as_a <rtx_insn *> (uncast_after);
  rtx_insn *prev = after;

  if (skip_debug_insns)
    while (DEBUG_INSN_P (prev))
      prev = PREV_INSN (prev);

  if (INSN_P (prev))
    return emit_pattern_after_setloc (pattern, after, INSN_LOCATION (prev),
				      make_raw);
  else
    return emit_pattern_after_noloc (pattern, after, NULL, make_raw);
}

/* Like emit_insn_after_noloc, but set INSN_LOCATION according to LOC.  */
rtx_insn *
emit_insn_after_setloc (rtx pattern, rtx after, int loc)
{
  return emit_pattern_after_setloc (pattern, after, loc, make_insn_raw);
}

/* Like emit_insn_after_noloc, but set INSN_LOCATION according to AFTER.  */
rtx_insn *
emit_insn_after (rtx pattern, rtx after)
{
  return emit_pattern_after (pattern, after, true, make_insn_raw);
}

/* Like emit_jump_insn_after_noloc, but set INSN_LOCATION according to LOC.  */
rtx_jump_insn *
emit_jump_insn_after_setloc (rtx pattern, rtx after, int loc)
{
  return as_a <rtx_jump_insn *> (
	emit_pattern_after_setloc (pattern, after, loc, make_jump_insn_raw));
}

/* Like emit_jump_insn_after_noloc, but set INSN_LOCATION according to AFTER.  */
rtx_jump_insn *
emit_jump_insn_after (rtx pattern, rtx after)
{
  return as_a <rtx_jump_insn *> (
	emit_pattern_after (pattern, after, true, make_jump_insn_raw));
}

/* Like emit_call_insn_after_noloc, but set INSN_LOCATION according to LOC.  */
rtx_insn *
emit_call_insn_after_setloc (rtx pattern, rtx after, int loc)
{
  return emit_pattern_after_setloc (pattern, after, loc, make_call_insn_raw);
}

/* Like emit_call_insn_after_noloc, but set INSN_LOCATION according to AFTER.  */
rtx_insn *
emit_call_insn_after (rtx pattern, rtx after)
{
  return emit_pattern_after (pattern, after, true, make_call_insn_raw);
}

/* Like emit_debug_insn_after_noloc, but set INSN_LOCATION according to LOC.  */
rtx_insn *
emit_debug_insn_after_setloc (rtx pattern, rtx after, int loc)
{
  return emit_pattern_after_setloc (pattern, after, loc, make_debug_insn_raw);
}

/* Like emit_debug_insn_after_noloc, but set INSN_LOCATION according to AFTER.  */
rtx_insn *
emit_debug_insn_after (rtx pattern, rtx after)
{
  return emit_pattern_after (pattern, after, false, make_debug_insn_raw);
}

/* Insert PATTERN before BEFORE, setting its INSN_LOCATION to LOC.
   MAKE_RAW indicates how to turn PATTERN into a real insn.  INSNP
   indicates if PATTERN is meant for an INSN as opposed to a JUMP_INSN,
   CALL_INSN, etc.  */

static rtx_insn *
emit_pattern_before_setloc (rtx pattern, rtx uncast_before, int loc, bool insnp,
			    rtx_insn *(*make_raw) (rtx))
{
  rtx_insn *before = as_a <rtx_insn *> (uncast_before);
  rtx_insn *first = PREV_INSN (before);
  rtx_insn *last = emit_pattern_before_noloc (pattern, before,
					      insnp ? before : NULL_RTX,
					      NULL, make_raw);

  if (pattern == NULL_RTX || !loc)
    return last;

  if (!first)
    first = get_insns ();
  else
    first = NEXT_INSN (first);
  while (1)
    {
      if (active_insn_p (first)
	  && !JUMP_TABLE_DATA_P (first) /* FIXME */
	  && !INSN_LOCATION (first))
	INSN_LOCATION (first) = loc;
      if (first == last)
	break;
      first = NEXT_INSN (first);
    }
  return last;
}

/* Insert PATTERN before BEFORE.  MAKE_RAW indicates how to turn PATTERN
   into a real insn.  SKIP_DEBUG_INSNS indicates whether to insert
   before any DEBUG_INSNs.  INSNP indicates if PATTERN is meant for an
   INSN as opposed to a JUMP_INSN, CALL_INSN, etc.  */

static rtx_insn *
emit_pattern_before (rtx pattern, rtx uncast_before, bool skip_debug_insns,
		     bool insnp, rtx_insn *(*make_raw) (rtx))
{
  rtx_insn *before = safe_as_a <rtx_insn *> (uncast_before);
  rtx_insn *next = before;

  if (skip_debug_insns)
    while (DEBUG_INSN_P (next))
      next = PREV_INSN (next);

  if (INSN_P (next))
    return emit_pattern_before_setloc (pattern, before, INSN_LOCATION (next),
				       insnp, make_raw);
  else
    return emit_pattern_before_noloc (pattern, before,
                                      insnp ? before : NULL_RTX,
                                      NULL, make_raw);
}

/* Like emit_insn_before_noloc, but set INSN_LOCATION according to LOC.  */
rtx_insn *
emit_insn_before_setloc (rtx pattern, rtx_insn *before, int loc)
{
  return emit_pattern_before_setloc (pattern, before, loc, true,
				     make_insn_raw);
}

/* Like emit_insn_before_noloc, but set INSN_LOCATION according to BEFORE.  */
rtx_insn *
emit_insn_before (rtx pattern, rtx before)
{
  return emit_pattern_before (pattern, before, true, true, make_insn_raw);
}

/* like emit_insn_before_noloc, but set INSN_LOCATION according to LOC.  */
rtx_jump_insn *
emit_jump_insn_before_setloc (rtx pattern, rtx_insn *before, int loc)
{
  return as_a <rtx_jump_insn *> (
	emit_pattern_before_setloc (pattern, before, loc, false,
				    make_jump_insn_raw));
}

/* Like emit_jump_insn_before_noloc, but set INSN_LOCATION according to BEFORE.  */
rtx_jump_insn *
emit_jump_insn_before (rtx pattern, rtx before)
{
  return as_a <rtx_jump_insn *> (
	emit_pattern_before (pattern, before, true, false,
			     make_jump_insn_raw));
}

/* Like emit_insn_before_noloc, but set INSN_LOCATION according to LOC.  */
rtx_insn *
emit_call_insn_before_setloc (rtx pattern, rtx_insn *before, int loc)
{
  return emit_pattern_before_setloc (pattern, before, loc, false,
				     make_call_insn_raw);
}

/* Like emit_call_insn_before_noloc,
   but set insn_location according to BEFORE.  */
rtx_insn *
emit_call_insn_before (rtx pattern, rtx_insn *before)
{
  return emit_pattern_before (pattern, before, true, false,
			      make_call_insn_raw);
}

/* Like emit_insn_before_noloc, but set INSN_LOCATION according to LOC.  */
rtx_insn *
emit_debug_insn_before_setloc (rtx pattern, rtx before, int loc)
{
  return emit_pattern_before_setloc (pattern, before, loc, false,
				     make_debug_insn_raw);
}

/* Like emit_debug_insn_before_noloc,
   but set insn_location according to BEFORE.  */
rtx_insn *
emit_debug_insn_before (rtx pattern, rtx_insn *before)
{
  return emit_pattern_before (pattern, before, false, false,
			      make_debug_insn_raw);
}

/* Take X and emit it at the end of the doubly-linked
   INSN list.

   Returns the last insn emitted.  */

rtx_insn *
emit_insn (rtx x)
{
  rtx_insn *last = get_last_insn ();
  rtx_insn *insn;

  if (x == NULL_RTX)
    return last;

  switch (GET_CODE (x))
    {
    case DEBUG_INSN:
    case INSN:
    case JUMP_INSN:
    case CALL_INSN:
    case CODE_LABEL:
    case BARRIER:
    case NOTE:
      insn = as_a <rtx_insn *> (x);
      while (insn)
	{
	  rtx_insn *next = NEXT_INSN (insn);
	  add_insn (insn);
	  last = insn;
	  insn = next;
	}
      break;

#ifdef ENABLE_RTL_CHECKING
    case JUMP_TABLE_DATA:
    case SEQUENCE:
      gcc_unreachable ();
      break;
#endif

    default:
      last = make_insn_raw (x);
      add_insn (last);
      break;
    }

  return last;
}

/* Make an insn of code DEBUG_INSN with pattern X
   and add it to the end of the doubly-linked list.  */

rtx_insn *
emit_debug_insn (rtx x)
{
  rtx_insn *last = get_last_insn ();
  rtx_insn *insn;

  if (x == NULL_RTX)
    return last;

  switch (GET_CODE (x))
    {
    case DEBUG_INSN:
    case INSN:
    case JUMP_INSN:
    case CALL_INSN:
    case CODE_LABEL:
    case BARRIER:
    case NOTE:
      insn = as_a <rtx_insn *> (x);
      while (insn)
	{
	  rtx_insn *next = NEXT_INSN (insn);
	  add_insn (insn);
	  last = insn;
	  insn = next;
	}
      break;

#ifdef ENABLE_RTL_CHECKING
    case JUMP_TABLE_DATA:
    case SEQUENCE:
      gcc_unreachable ();
      break;
#endif

    default:
      last = make_debug_insn_raw (x);
      add_insn (last);
      break;
    }

  return last;
}

/* Make an insn of code JUMP_INSN with pattern X
   and add it to the end of the doubly-linked list.  */

rtx_insn *
emit_jump_insn (rtx x)
{
  rtx_insn *last = NULL;
  rtx_insn *insn;

  switch (GET_CODE (x))
    {
    case DEBUG_INSN:
    case INSN:
    case JUMP_INSN:
    case CALL_INSN:
    case CODE_LABEL:
    case BARRIER:
    case NOTE:
      insn = as_a <rtx_insn *> (x);
      while (insn)
	{
	  rtx_insn *next = NEXT_INSN (insn);
	  add_insn (insn);
	  last = insn;
	  insn = next;
	}
      break;

#ifdef ENABLE_RTL_CHECKING
    case JUMP_TABLE_DATA:
    case SEQUENCE:
      gcc_unreachable ();
      break;
#endif

    default:
      last = make_jump_insn_raw (x);
      add_insn (last);
      break;
    }

  return last;
}

/* Make an insn of code CALL_INSN with pattern X
   and add it to the end of the doubly-linked list.  */

rtx_insn *
emit_call_insn (rtx x)
{
  rtx_insn *insn;

  switch (GET_CODE (x))
    {
    case DEBUG_INSN:
    case INSN:
    case JUMP_INSN:
    case CALL_INSN:
    case CODE_LABEL:
    case BARRIER:
    case NOTE:
      insn = emit_insn (x);
      break;

#ifdef ENABLE_RTL_CHECKING
    case SEQUENCE:
    case JUMP_TABLE_DATA:
      gcc_unreachable ();
      break;
#endif

    default:
      insn = make_call_insn_raw (x);
      add_insn (insn);
      break;
    }

  return insn;
}

/* Add the label LABEL to the end of the doubly-linked list.  */

rtx_code_label *
emit_label (rtx uncast_label)
{
  rtx_code_label *label = as_a <rtx_code_label *> (uncast_label);

  gcc_checking_assert (INSN_UID (label) == 0);
  INSN_UID (label) = cur_insn_uid++;
  add_insn (label);
  return label;
}

/* Make an insn of code JUMP_TABLE_DATA
   and add it to the end of the doubly-linked list.  */

rtx_jump_table_data *
emit_jump_table_data (rtx table)
{
  rtx_jump_table_data *jump_table_data =
    as_a <rtx_jump_table_data *> (rtx_alloc (JUMP_TABLE_DATA));
  INSN_UID (jump_table_data) = cur_insn_uid++;
  PATTERN (jump_table_data) = table;
  BLOCK_FOR_INSN (jump_table_data) = NULL;
  add_insn (jump_table_data);
  return jump_table_data;
}

/* Make an insn of code BARRIER
   and add it to the end of the doubly-linked list.  */

rtx_barrier *
emit_barrier (void)
{
  rtx_barrier *barrier = as_a <rtx_barrier *> (rtx_alloc (BARRIER));
  INSN_UID (barrier) = cur_insn_uid++;
  add_insn (barrier);
  return barrier;
}

/* Emit a copy of note ORIG.  */

rtx_note *
emit_note_copy (rtx_note *orig)
{
  enum insn_note kind = (enum insn_note) NOTE_KIND (orig);
  rtx_note *note = make_note_raw (kind);
  NOTE_DATA (note) = NOTE_DATA (orig);
  add_insn (note);
  return note;
}

/* Make an insn of code NOTE or type NOTE_NO
   and add it to the end of the doubly-linked list.  */

rtx_note *
emit_note (enum insn_note kind)
{
  rtx_note *note = make_note_raw (kind);
  add_insn (note);
  return note;
}

/* Emit a clobber of lvalue X.  */

rtx_insn *
emit_clobber (rtx x)
{
  /* CONCATs should not appear in the insn stream.  */
  if (GET_CODE (x) == CONCAT)
    {
      emit_clobber (XEXP (x, 0));
      return emit_clobber (XEXP (x, 1));
    }
  return emit_insn (gen_rtx_CLOBBER (VOIDmode, x));
}

/* Return a sequence of insns to clobber lvalue X.  */

rtx_insn *
gen_clobber (rtx x)
{
  rtx_insn *seq;

  start_sequence ();
  emit_clobber (x);
  seq = get_insns ();
  end_sequence ();
  return seq;
}

/* Emit a use of rvalue X.  */

rtx_insn *
emit_use (rtx x)
{
  /* CONCATs should not appear in the insn stream.  */
  if (GET_CODE (x) == CONCAT)
    {
      emit_use (XEXP (x, 0));
      return emit_use (XEXP (x, 1));
    }
  return emit_insn (gen_rtx_USE (VOIDmode, x));
}

/* Return a sequence of insns to use rvalue X.  */

rtx_insn *
gen_use (rtx x)
{
  rtx_insn *seq;

  start_sequence ();
  emit_use (x);
  seq = get_insns ();
  end_sequence ();
  return seq;
}

/* Notes like REG_EQUAL and REG_EQUIV refer to a set in an instruction.
   Return the set in INSN that such notes describe, or NULL if the notes
   have no meaning for INSN.  */

rtx
set_for_reg_notes (rtx insn)
{
  rtx pat, reg;

  if (!INSN_P (insn))
    return NULL_RTX;

  pat = PATTERN (insn);
  if (GET_CODE (pat) == PARALLEL)
    {
      /* We do not use single_set because that ignores SETs of unused
	 registers.  REG_EQUAL and REG_EQUIV notes really do require the
	 PARALLEL to have a single SET.  */
      if (multiple_sets (insn))
	return NULL_RTX;
      pat = XVECEXP (pat, 0, 0);
    }

  if (GET_CODE (pat) != SET)
    return NULL_RTX;

  reg = SET_DEST (pat);

  /* Notes apply to the contents of a STRICT_LOW_PART.  */
  if (GET_CODE (reg) == STRICT_LOW_PART
      || GET_CODE (reg) == ZERO_EXTRACT)
    reg = XEXP (reg, 0);

  /* Check that we have a register.  */
  if (!(REG_P (reg) || GET_CODE (reg) == SUBREG))
    return NULL_RTX;

  return pat;
}

/* Place a note of KIND on insn INSN with DATUM as the datum. If a
   note of this type already exists, remove it first.  */

rtx
set_unique_reg_note (rtx insn, enum reg_note kind, rtx datum)
{
  rtx note = find_reg_note (insn, kind, NULL_RTX);

  switch (kind)
    {
    case REG_EQUAL:
    case REG_EQUIV:
      /* We need to support the REG_EQUAL on USE trick of find_reloads.  */
      if (!set_for_reg_notes (insn) && GET_CODE (PATTERN (insn)) != USE)
	return NULL_RTX;

      /* Don't add ASM_OPERAND REG_EQUAL/REG_EQUIV notes.
	 It serves no useful purpose and breaks eliminate_regs.  */
      if (GET_CODE (datum) == ASM_OPERANDS)
	return NULL_RTX;

      /* Notes with side effects are dangerous.  Even if the side-effect
	 initially mirrors one in PATTERN (INSN), later optimizations
	 might alter the way that the final register value is calculated
	 and so move or alter the side-effect in some way.  The note would
	 then no longer be a valid substitution for SET_SRC.  */
      if (side_effects_p (datum))
	return NULL_RTX;
      break;

    default:
      break;
    }

  if (note)
    XEXP (note, 0) = datum;
  else
    {
      add_reg_note (insn, kind, datum);
      note = REG_NOTES (insn);
    }

  switch (kind)
    {
    case REG_EQUAL:
    case REG_EQUIV:
      df_notes_rescan (as_a <rtx_insn *> (insn));
      break;
    default:
      break;
    }

  return note;
}

/* Like set_unique_reg_note, but don't do anything unless INSN sets DST.  */
rtx
set_dst_reg_note (rtx insn, enum reg_note kind, rtx datum, rtx dst)
{
  rtx set = set_for_reg_notes (insn);

  if (set && SET_DEST (set) == dst)
    return set_unique_reg_note (insn, kind, datum);
  return NULL_RTX;
}

/* Emit the rtl pattern X as an appropriate kind of insn.  Also emit a
   following barrier if the instruction needs one and if ALLOW_BARRIER_P
   is true.

   If X is a label, it is simply added into the insn chain.  */

rtx_insn *
emit (rtx x, bool allow_barrier_p)
{
  enum rtx_code code = classify_insn (x);

  switch (code)
    {
    case CODE_LABEL:
      return emit_label (x);
    case INSN:
      return emit_insn (x);
    case  JUMP_INSN:
      {
	rtx_insn *insn = emit_jump_insn (x);
	if (allow_barrier_p
	    && (any_uncondjump_p (insn) || GET_CODE (x) == RETURN))
	  return emit_barrier ();
	return insn;
      }
    case CALL_INSN:
      return emit_call_insn (x);
    case DEBUG_INSN:
      return emit_debug_insn (x);
    default:
      gcc_unreachable ();
    }
}

/* Space for free sequence stack entries.  */
static GTY ((deletable)) struct sequence_stack *free_sequence_stack;

/* Begin emitting insns to a sequence.  If this sequence will contain
   something that might cause the compiler to pop arguments to function
   calls (because those pops have previously been deferred; see
   INHIBIT_DEFER_POP for more details), use do_pending_stack_adjust
   before calling this function.  That will ensure that the deferred
   pops are not accidentally emitted in the middle of this sequence.  */

void
start_sequence (void)
{
  struct sequence_stack *tem;

  if (free_sequence_stack != NULL)
    {
      tem = free_sequence_stack;
      free_sequence_stack = tem->next;
    }
  else
    tem = ggc_alloc<sequence_stack> ();

  tem->next = get_current_sequence ()->next;
  tem->first = get_insns ();
  tem->last = get_last_insn ();
  get_current_sequence ()->next = tem;

  set_first_insn (0);
  set_last_insn (0);
}

/* Set up the insn chain starting with FIRST as the current sequence,
   saving the previously current one.  See the documentation for
   start_sequence for more information about how to use this function.  */

void
push_to_sequence (rtx_insn *first)
{
  rtx_insn *last;

  start_sequence ();

  for (last = first; last && NEXT_INSN (last); last = NEXT_INSN (last))
    ;

  set_first_insn (first);
  set_last_insn (last);
}

/* Like push_to_sequence, but take the last insn as an argument to avoid
   looping through the list.  */

void
push_to_sequence2 (rtx_insn *first, rtx_insn *last)
{
  start_sequence ();

  set_first_insn (first);
  set_last_insn (last);
}

/* Set up the outer-level insn chain
   as the current sequence, saving the previously current one.  */

void
push_topmost_sequence (void)
{
  struct sequence_stack *top;

  start_sequence ();

  top = get_topmost_sequence ();
  set_first_insn (top->first);
  set_last_insn (top->last);
}

/* After emitting to the outer-level insn chain, update the outer-level
   insn chain, and restore the previous saved state.  */

void
pop_topmost_sequence (void)
{
  struct sequence_stack *top;

  top = get_topmost_sequence ();
  top->first = get_insns ();
  top->last = get_last_insn ();

  end_sequence ();
}

/* After emitting to a sequence, restore previous saved state.

   To get the contents of the sequence just made, you must call
   `get_insns' *before* calling here.

   If the compiler might have deferred popping arguments while
   generating this sequence, and this sequence will not be immediately
   inserted into the instruction stream, use do_pending_stack_adjust
   before calling get_insns.  That will ensure that the deferred
   pops are inserted into this sequence, and not into some random
   location in the instruction stream.  See INHIBIT_DEFER_POP for more
   information about deferred popping of arguments.  */

void
end_sequence (void)
{
  struct sequence_stack *tem = get_current_sequence ()->next;

  set_first_insn (tem->first);
  set_last_insn (tem->last);
  get_current_sequence ()->next = tem->next;

  memset (tem, 0, sizeof (*tem));
  tem->next = free_sequence_stack;
  free_sequence_stack = tem;
}

/* Return 1 if currently emitting into a sequence.  */

int
in_sequence_p (void)
{
  return get_current_sequence ()->next != 0;
}

/* Put the various virtual registers into REGNO_REG_RTX.  */

static void
init_virtual_regs (void)
{
  regno_reg_rtx[VIRTUAL_INCOMING_ARGS_REGNUM] = virtual_incoming_args_rtx;
  regno_reg_rtx[VIRTUAL_STACK_VARS_REGNUM] = virtual_stack_vars_rtx;
  regno_reg_rtx[VIRTUAL_STACK_DYNAMIC_REGNUM] = virtual_stack_dynamic_rtx;
  regno_reg_rtx[VIRTUAL_OUTGOING_ARGS_REGNUM] = virtual_outgoing_args_rtx;
  regno_reg_rtx[VIRTUAL_CFA_REGNUM] = virtual_cfa_rtx;
  regno_reg_rtx[VIRTUAL_PREFERRED_STACK_BOUNDARY_REGNUM]
    = virtual_preferred_stack_boundary_rtx;
}


/* Used by copy_insn_1 to avoid copying SCRATCHes more than once.  */
static rtx copy_insn_scratch_in[MAX_RECOG_OPERANDS];
static rtx copy_insn_scratch_out[MAX_RECOG_OPERANDS];
static int copy_insn_n_scratches;

/* When an insn is being copied by copy_insn_1, this is nonzero if we have
   copied an ASM_OPERANDS.
   In that case, it is the original input-operand vector.  */
static rtvec orig_asm_operands_vector;

/* When an insn is being copied by copy_insn_1, this is nonzero if we have
   copied an ASM_OPERANDS.
   In that case, it is the copied input-operand vector.  */
static rtvec copy_asm_operands_vector;

/* Likewise for the constraints vector.  */
static rtvec orig_asm_constraints_vector;
static rtvec copy_asm_constraints_vector;

/* Recursively create a new copy of an rtx for copy_insn.
   This function differs from copy_rtx in that it handles SCRATCHes and
   ASM_OPERANDs properly.
   Normally, this function is not used directly; use copy_insn as front end.
   However, you could first copy an insn pattern with copy_insn and then use
   this function afterwards to properly copy any REG_NOTEs containing
   SCRATCHes.  */

rtx
copy_insn_1 (rtx orig)
{
  rtx copy;
  int i, j;
  RTX_CODE code;
  const char *format_ptr;

  if (orig == NULL)
    return NULL;

  code = GET_CODE (orig);

  switch (code)
    {
    case REG:
    case DEBUG_EXPR:
    CASE_CONST_ANY:
    case SYMBOL_REF:
    case CODE_LABEL:
    case PC:
    case CC0:
    case RETURN:
    case SIMPLE_RETURN:
      return orig;
    case CLOBBER:
      /* Share clobbers of hard registers (like cc0), but do not share pseudo reg
         clobbers or clobbers of hard registers that originated as pseudos.
         This is needed to allow safe register renaming.  */
      if (REG_P (XEXP (orig, 0)) && REGNO (XEXP (orig, 0)) < FIRST_PSEUDO_REGISTER
	  && ORIGINAL_REGNO (XEXP (orig, 0)) == REGNO (XEXP (orig, 0)))
	return orig;
      break;

    case SCRATCH:
      for (i = 0; i < copy_insn_n_scratches; i++)
	if (copy_insn_scratch_in[i] == orig)
	  return copy_insn_scratch_out[i];
      break;

    case CONST:
      if (shared_const_p (orig))
	return orig;
      break;

      /* A MEM with a constant address is not sharable.  The problem is that
	 the constant address may need to be reloaded.  If the mem is shared,
	 then reloading one copy of this mem will cause all copies to appear
	 to have been reloaded.  */

    default:
      break;
    }

  /* Copy the various flags, fields, and other information.  We assume
     that all fields need copying, and then clear the fields that should
     not be copied.  That is the sensible default behavior, and forces
     us to explicitly document why we are *not* copying a flag.  */
  copy = shallow_copy_rtx (orig);

  /* We do not copy the USED flag, which is used as a mark bit during
     walks over the RTL.  */
  RTX_FLAG (copy, used) = 0;

  /* We do not copy JUMP, CALL, or FRAME_RELATED for INSNs.  */
  if (INSN_P (orig))
    {
      RTX_FLAG (copy, jump) = 0;
      RTX_FLAG (copy, call) = 0;
      RTX_FLAG (copy, frame_related) = 0;
    }

  format_ptr = GET_RTX_FORMAT (GET_CODE (copy));

  for (i = 0; i < GET_RTX_LENGTH (GET_CODE (copy)); i++)
    switch (*format_ptr++)
      {
      case 'e':
	if (XEXP (orig, i) != NULL)
	  XEXP (copy, i) = copy_insn_1 (XEXP (orig, i));
	break;

      case 'E':
      case 'V':
	if (XVEC (orig, i) == orig_asm_constraints_vector)
	  XVEC (copy, i) = copy_asm_constraints_vector;
	else if (XVEC (orig, i) == orig_asm_operands_vector)
	  XVEC (copy, i) = copy_asm_operands_vector;
	else if (XVEC (orig, i) != NULL)
	  {
	    XVEC (copy, i) = rtvec_alloc (XVECLEN (orig, i));
	    for (j = 0; j < XVECLEN (copy, i); j++)
	      XVECEXP (copy, i, j) = copy_insn_1 (XVECEXP (orig, i, j));
	  }
	break;

      case 't':
      case 'w':
      case 'i':
      case 's':
      case 'S':
      case 'u':
      case '0':
	/* These are left unchanged.  */
	break;

      default:
	gcc_unreachable ();
      }

  if (code == SCRATCH)
    {
      i = copy_insn_n_scratches++;
      gcc_assert (i < MAX_RECOG_OPERANDS);
      copy_insn_scratch_in[i] = orig;
      copy_insn_scratch_out[i] = copy;
    }
  else if (code == ASM_OPERANDS)
    {
      orig_asm_operands_vector = ASM_OPERANDS_INPUT_VEC (orig);
      copy_asm_operands_vector = ASM_OPERANDS_INPUT_VEC (copy);
      orig_asm_constraints_vector = ASM_OPERANDS_INPUT_CONSTRAINT_VEC (orig);
      copy_asm_constraints_vector = ASM_OPERANDS_INPUT_CONSTRAINT_VEC (copy);
    }

  return copy;
}

/* Create a new copy of an rtx.
   This function differs from copy_rtx in that it handles SCRATCHes and
   ASM_OPERANDs properly.
   INSN doesn't really have to be a full INSN; it could be just the
   pattern.  */
rtx
copy_insn (rtx insn)
{
  copy_insn_n_scratches = 0;
  orig_asm_operands_vector = 0;
  orig_asm_constraints_vector = 0;
  copy_asm_operands_vector = 0;
  copy_asm_constraints_vector = 0;
  return copy_insn_1 (insn);
}

/* Return a copy of INSN that can be used in a SEQUENCE delay slot,
   on that assumption that INSN itself remains in its original place.  */

rtx_insn *
copy_delay_slot_insn (rtx_insn *insn)
{
  /* Copy INSN with its rtx_code, all its notes, location etc.  */
  insn = as_a <rtx_insn *> (copy_rtx (insn));
  INSN_UID (insn) = cur_insn_uid++;
  return insn;
}

/* Initialize data structures and variables in this file
   before generating rtl for each function.  */

void
init_emit (void)
{
  set_first_insn (NULL);
  set_last_insn (NULL);
  if (MIN_NONDEBUG_INSN_UID)
    cur_insn_uid = MIN_NONDEBUG_INSN_UID;
  else
    cur_insn_uid = 1;
  cur_debug_insn_uid = 1;
  reg_rtx_no = LAST_VIRTUAL_REGISTER + 1;
  first_label_num = label_num;
  get_current_sequence ()->next = NULL;

  /* Init the tables that describe all the pseudo regs.  */

  crtl->emit.regno_pointer_align_length = LAST_VIRTUAL_REGISTER + 101;

  crtl->emit.regno_pointer_align
    = XCNEWVEC (unsigned char, crtl->emit.regno_pointer_align_length);

  regno_reg_rtx = ggc_vec_alloc<rtx> (crtl->emit.regno_pointer_align_length);

  /* Put copies of all the hard registers into regno_reg_rtx.  */
  memcpy (regno_reg_rtx,
	  initial_regno_reg_rtx,
	  FIRST_PSEUDO_REGISTER * sizeof (rtx));

  /* Put copies of all the virtual register rtx into regno_reg_rtx.  */
  init_virtual_regs ();

  /* Indicate that the virtual registers and stack locations are
     all pointers.  */
  REG_POINTER (stack_pointer_rtx) = 1;
  REG_POINTER (frame_pointer_rtx) = 1;
  REG_POINTER (hard_frame_pointer_rtx) = 1;
  REG_POINTER (arg_pointer_rtx) = 1;

  REG_POINTER (virtual_incoming_args_rtx) = 1;
  REG_POINTER (virtual_stack_vars_rtx) = 1;
  REG_POINTER (virtual_stack_dynamic_rtx) = 1;
  REG_POINTER (virtual_outgoing_args_rtx) = 1;
  REG_POINTER (virtual_cfa_rtx) = 1;

#ifdef STACK_BOUNDARY
  REGNO_POINTER_ALIGN (STACK_POINTER_REGNUM) = STACK_BOUNDARY;
  REGNO_POINTER_ALIGN (FRAME_POINTER_REGNUM) = STACK_BOUNDARY;
  REGNO_POINTER_ALIGN (HARD_FRAME_POINTER_REGNUM) = STACK_BOUNDARY;
  REGNO_POINTER_ALIGN (ARG_POINTER_REGNUM) = STACK_BOUNDARY;

  REGNO_POINTER_ALIGN (VIRTUAL_INCOMING_ARGS_REGNUM) = STACK_BOUNDARY;
  REGNO_POINTER_ALIGN (VIRTUAL_STACK_VARS_REGNUM) = STACK_BOUNDARY;
  REGNO_POINTER_ALIGN (VIRTUAL_STACK_DYNAMIC_REGNUM) = STACK_BOUNDARY;
  REGNO_POINTER_ALIGN (VIRTUAL_OUTGOING_ARGS_REGNUM) = STACK_BOUNDARY;
  REGNO_POINTER_ALIGN (VIRTUAL_CFA_REGNUM) = BITS_PER_WORD;
#endif

#ifdef INIT_EXPANDERS
  INIT_EXPANDERS;
#endif
}

/* Generate a vector constant for mode MODE and constant value CONSTANT.  */

static rtx
gen_const_vector (machine_mode mode, int constant)
{
  rtx tem;
  rtvec v;
  int units, i;
  machine_mode inner;

  units = GET_MODE_NUNITS (mode);
  inner = GET_MODE_INNER (mode);

  gcc_assert (!DECIMAL_FLOAT_MODE_P (inner));

  v = rtvec_alloc (units);

  /* We need to call this function after we set the scalar const_tiny_rtx
     entries.  */
  gcc_assert (const_tiny_rtx[constant][(int) inner]);

  for (i = 0; i < units; ++i)
    RTVEC_ELT (v, i) = const_tiny_rtx[constant][(int) inner];

  tem = gen_rtx_raw_CONST_VECTOR (mode, v);
  return tem;
}

/* Generate a vector like gen_rtx_raw_CONST_VEC, but use the zero vector when
   all elements are zero, and the one vector when all elements are one.  */
rtx
gen_rtx_CONST_VECTOR (machine_mode mode, rtvec v)
{
  machine_mode inner = GET_MODE_INNER (mode);
  int nunits = GET_MODE_NUNITS (mode);
  rtx x;
  int i;

  /* Check to see if all of the elements have the same value.  */
  x = RTVEC_ELT (v, nunits - 1);
  for (i = nunits - 2; i >= 0; i--)
    if (RTVEC_ELT (v, i) != x)
      break;

  /* If the values are all the same, check to see if we can use one of the
     standard constant vectors.  */
  if (i == -1)
    {
      if (x == CONST0_RTX (inner))
	return CONST0_RTX (mode);
      else if (x == CONST1_RTX (inner))
	return CONST1_RTX (mode);
      else if (x == CONSTM1_RTX (inner))
	return CONSTM1_RTX (mode);
    }

  return gen_rtx_raw_CONST_VECTOR (mode, v);
}

/* Initialise global register information required by all functions.  */

void
init_emit_regs (void)
{
  int i;
  machine_mode mode;
  mem_attrs *attrs;

  /* Reset register attributes */
  reg_attrs_htab->empty ();

  /* We need reg_raw_mode, so initialize the modes now.  */
  init_reg_modes_target ();

  /* Assign register numbers to the globally defined register rtx.  */
  stack_pointer_rtx = gen_raw_REG (Pmode, STACK_POINTER_REGNUM);
  frame_pointer_rtx = gen_raw_REG (Pmode, FRAME_POINTER_REGNUM);
  hard_frame_pointer_rtx = gen_raw_REG (Pmode, HARD_FRAME_POINTER_REGNUM);
  arg_pointer_rtx = gen_raw_REG (Pmode, ARG_POINTER_REGNUM);
  virtual_incoming_args_rtx =
    gen_raw_REG (Pmode, VIRTUAL_INCOMING_ARGS_REGNUM);
  virtual_stack_vars_rtx =
    gen_raw_REG (Pmode, VIRTUAL_STACK_VARS_REGNUM);
  virtual_stack_dynamic_rtx =
    gen_raw_REG (Pmode, VIRTUAL_STACK_DYNAMIC_REGNUM);
  virtual_outgoing_args_rtx =
    gen_raw_REG (Pmode, VIRTUAL_OUTGOING_ARGS_REGNUM);
  virtual_cfa_rtx = gen_raw_REG (Pmode, VIRTUAL_CFA_REGNUM);
  virtual_preferred_stack_boundary_rtx =
    gen_raw_REG (Pmode, VIRTUAL_PREFERRED_STACK_BOUNDARY_REGNUM);

  /* Initialize RTL for commonly used hard registers.  These are
     copied into regno_reg_rtx as we begin to compile each function.  */
  for (i = 0; i < FIRST_PSEUDO_REGISTER; i++)
    initial_regno_reg_rtx[i] = gen_raw_REG (reg_raw_mode[i], i);

#ifdef RETURN_ADDRESS_POINTER_REGNUM
  return_address_pointer_rtx
    = gen_raw_REG (Pmode, RETURN_ADDRESS_POINTER_REGNUM);
#endif

  pic_offset_table_rtx = NULL_RTX;
  if ((unsigned) PIC_OFFSET_TABLE_REGNUM != INVALID_REGNUM)
    pic_offset_table_rtx = gen_raw_REG (Pmode, PIC_OFFSET_TABLE_REGNUM);

  for (i = 0; i < (int) MAX_MACHINE_MODE; i++)
    {
      mode = (machine_mode) i;
      attrs = ggc_cleared_alloc<mem_attrs> ();
      attrs->align = BITS_PER_UNIT;
      attrs->addrspace = ADDR_SPACE_GENERIC;
      if (mode != BLKmode)
	{
	  attrs->size_known_p = true;
	  attrs->size = GET_MODE_SIZE (mode);
	  if (STRICT_ALIGNMENT)
	    attrs->align = GET_MODE_ALIGNMENT (mode);
	}
      mode_mem_attrs[i] = attrs;
    }
}

/* Initialize global machine_mode variables.  */

void
init_derived_machine_modes (void)
{
  byte_mode = VOIDmode;
  word_mode = VOIDmode;

  for (machine_mode mode = GET_CLASS_NARROWEST_MODE (MODE_INT);
       mode != VOIDmode;
       mode = GET_MODE_WIDER_MODE (mode))
    {
      if (GET_MODE_BITSIZE (mode) == BITS_PER_UNIT
	  && byte_mode == VOIDmode)
	byte_mode = mode;

      if (GET_MODE_BITSIZE (mode) == BITS_PER_WORD
	  && word_mode == VOIDmode)
	word_mode = mode;
    }

  ptr_mode = mode_for_size (POINTER_SIZE, GET_MODE_CLASS (Pmode), 0);
}

/* Create some permanent unique rtl objects shared between all functions.  */

void
init_emit_once (void)
{
  int i;
  machine_mode mode;
  machine_mode double_mode;

  /* Initialize the CONST_INT, CONST_WIDE_INT, CONST_DOUBLE,
     CONST_FIXED, and memory attribute hash tables.  */
  const_int_htab = hash_table<const_int_hasher>::create_ggc (37);

#if TARGET_SUPPORTS_WIDE_INT
  const_wide_int_htab = hash_table<const_wide_int_hasher>::create_ggc (37);
#endif
  const_double_htab = hash_table<const_double_hasher>::create_ggc (37);

  const_fixed_htab = hash_table<const_fixed_hasher>::create_ggc (37);

  reg_attrs_htab = hash_table<reg_attr_hasher>::create_ggc (37);

#ifdef INIT_EXPANDERS
  /* This is to initialize {init|mark|free}_machine_status before the first
     call to push_function_context_to.  This is needed by the Chill front
     end which calls push_function_context_to before the first call to
     init_function_start.  */
  INIT_EXPANDERS;
#endif

  /* Create the unique rtx's for certain rtx codes and operand values.  */

  /* Process stack-limiting command-line options.  */
  if (opt_fstack_limit_symbol_arg != NULL)
    stack_limit_rtx 
      = gen_rtx_SYMBOL_REF (Pmode, ggc_strdup (opt_fstack_limit_symbol_arg));
  if (opt_fstack_limit_register_no >= 0)
    stack_limit_rtx = gen_rtx_REG (Pmode, opt_fstack_limit_register_no);

  /* Don't use gen_rtx_CONST_INT here since gen_rtx_CONST_INT in this case
     tries to use these variables.  */
  for (i = - MAX_SAVED_CONST_INT; i <= MAX_SAVED_CONST_INT; i++)
    const_int_rtx[i + MAX_SAVED_CONST_INT] =
      gen_rtx_raw_CONST_INT (VOIDmode, (HOST_WIDE_INT) i);

  if (STORE_FLAG_VALUE >= - MAX_SAVED_CONST_INT
      && STORE_FLAG_VALUE <= MAX_SAVED_CONST_INT)
    const_true_rtx = const_int_rtx[STORE_FLAG_VALUE + MAX_SAVED_CONST_INT];
  else
    const_true_rtx = gen_rtx_CONST_INT (VOIDmode, STORE_FLAG_VALUE);

  double_mode = mode_for_size (DOUBLE_TYPE_SIZE, MODE_FLOAT, 0);

  real_from_integer (&dconst0, double_mode, 0, SIGNED);
  real_from_integer (&dconst1, double_mode, 1, SIGNED);
  real_from_integer (&dconst2, double_mode, 2, SIGNED);

  dconstm1 = dconst1;
  dconstm1.sign = 1;

  dconsthalf = dconst1;
  SET_REAL_EXP (&dconsthalf, REAL_EXP (&dconsthalf) - 1);

  for (i = 0; i < 3; i++)
    {
      const REAL_VALUE_TYPE *const r =
	(i == 0 ? &dconst0 : i == 1 ? &dconst1 : &dconst2);

      for (mode = GET_CLASS_NARROWEST_MODE (MODE_FLOAT);
	   mode != VOIDmode;
	   mode = GET_MODE_WIDER_MODE (mode))
	const_tiny_rtx[i][(int) mode] =
	  const_double_from_real_value (*r, mode);

      for (mode = GET_CLASS_NARROWEST_MODE (MODE_DECIMAL_FLOAT);
	   mode != VOIDmode;
	   mode = GET_MODE_WIDER_MODE (mode))
	const_tiny_rtx[i][(int) mode] =
	  const_double_from_real_value (*r, mode);

      const_tiny_rtx[i][(int) VOIDmode] = GEN_INT (i);

      for (mode = GET_CLASS_NARROWEST_MODE (MODE_INT);
	   mode != VOIDmode;
	   mode = GET_MODE_WIDER_MODE (mode))
	const_tiny_rtx[i][(int) mode] = GEN_INT (i);

      for (mode = MIN_MODE_PARTIAL_INT;
	   mode <= MAX_MODE_PARTIAL_INT;
	   mode = (machine_mode)((int)(mode) + 1))
	const_tiny_rtx[i][(int) mode] = GEN_INT (i);
    }

  const_tiny_rtx[3][(int) VOIDmode] = constm1_rtx;

  for (mode = GET_CLASS_NARROWEST_MODE (MODE_INT);
       mode != VOIDmode;
       mode = GET_MODE_WIDER_MODE (mode))
    const_tiny_rtx[3][(int) mode] = constm1_rtx;

  for (mode = MIN_MODE_PARTIAL_INT;
       mode <= MAX_MODE_PARTIAL_INT;
       mode = (machine_mode)((int)(mode) + 1))
    const_tiny_rtx[3][(int) mode] = constm1_rtx;
      
  for (mode = GET_CLASS_NARROWEST_MODE (MODE_COMPLEX_INT);
       mode != VOIDmode;
       mode = GET_MODE_WIDER_MODE (mode))
    {
      rtx inner = const_tiny_rtx[0][(int)GET_MODE_INNER (mode)];
      const_tiny_rtx[0][(int) mode] = gen_rtx_CONCAT (mode, inner, inner);
    }

  for (mode = GET_CLASS_NARROWEST_MODE (MODE_COMPLEX_FLOAT);
       mode != VOIDmode;
       mode = GET_MODE_WIDER_MODE (mode))
    {
      rtx inner = const_tiny_rtx[0][(int)GET_MODE_INNER (mode)];
      const_tiny_rtx[0][(int) mode] = gen_rtx_CONCAT (mode, inner, inner);
    }

  for (mode = GET_CLASS_NARROWEST_MODE (MODE_VECTOR_INT);
       mode != VOIDmode;
       mode = GET_MODE_WIDER_MODE (mode))
    {
      const_tiny_rtx[0][(int) mode] = gen_const_vector (mode, 0);
      const_tiny_rtx[1][(int) mode] = gen_const_vector (mode, 1);
      const_tiny_rtx[3][(int) mode] = gen_const_vector (mode, 3);
    }

  for (mode = GET_CLASS_NARROWEST_MODE (MODE_VECTOR_FLOAT);
       mode != VOIDmode;
       mode = GET_MODE_WIDER_MODE (mode))
    {
      const_tiny_rtx[0][(int) mode] = gen_const_vector (mode, 0);
      const_tiny_rtx[1][(int) mode] = gen_const_vector (mode, 1);
    }

  for (mode = GET_CLASS_NARROWEST_MODE (MODE_FRACT);
       mode != VOIDmode;
       mode = GET_MODE_WIDER_MODE (mode))
    {
      FCONST0 (mode).data.high = 0;
      FCONST0 (mode).data.low = 0;
      FCONST0 (mode).mode = mode;
      const_tiny_rtx[0][(int) mode] = CONST_FIXED_FROM_FIXED_VALUE (
				      FCONST0 (mode), mode);
    }

  for (mode = GET_CLASS_NARROWEST_MODE (MODE_UFRACT);
       mode != VOIDmode;
       mode = GET_MODE_WIDER_MODE (mode))
    {
      FCONST0 (mode).data.high = 0;
      FCONST0 (mode).data.low = 0;
      FCONST0 (mode).mode = mode;
      const_tiny_rtx[0][(int) mode] = CONST_FIXED_FROM_FIXED_VALUE (
				      FCONST0 (mode), mode);
    }

  for (mode = GET_CLASS_NARROWEST_MODE (MODE_ACCUM);
       mode != VOIDmode;
       mode = GET_MODE_WIDER_MODE (mode))
    {
      FCONST0 (mode).data.high = 0;
      FCONST0 (mode).data.low = 0;
      FCONST0 (mode).mode = mode;
      const_tiny_rtx[0][(int) mode] = CONST_FIXED_FROM_FIXED_VALUE (
				      FCONST0 (mode), mode);

      /* We store the value 1.  */
      FCONST1 (mode).data.high = 0;
      FCONST1 (mode).data.low = 0;
      FCONST1 (mode).mode = mode;
      FCONST1 (mode).data
	= double_int_one.lshift (GET_MODE_FBIT (mode),
				 HOST_BITS_PER_DOUBLE_INT,
				 SIGNED_FIXED_POINT_MODE_P (mode));
      const_tiny_rtx[1][(int) mode] = CONST_FIXED_FROM_FIXED_VALUE (
				      FCONST1 (mode), mode);
    }

  for (mode = GET_CLASS_NARROWEST_MODE (MODE_UACCUM);
       mode != VOIDmode;
       mode = GET_MODE_WIDER_MODE (mode))
    {
      FCONST0 (mode).data.high = 0;
      FCONST0 (mode).data.low = 0;
      FCONST0 (mode).mode = mode;
      const_tiny_rtx[0][(int) mode] = CONST_FIXED_FROM_FIXED_VALUE (
				      FCONST0 (mode), mode);

      /* We store the value 1.  */
      FCONST1 (mode).data.high = 0;
      FCONST1 (mode).data.low = 0;
      FCONST1 (mode).mode = mode;
      FCONST1 (mode).data
	= double_int_one.lshift (GET_MODE_FBIT (mode),
				 HOST_BITS_PER_DOUBLE_INT,
				 SIGNED_FIXED_POINT_MODE_P (mode));
      const_tiny_rtx[1][(int) mode] = CONST_FIXED_FROM_FIXED_VALUE (
				      FCONST1 (mode), mode);
    }

  for (mode = GET_CLASS_NARROWEST_MODE (MODE_VECTOR_FRACT);
       mode != VOIDmode;
       mode = GET_MODE_WIDER_MODE (mode))
    {
      const_tiny_rtx[0][(int) mode] = gen_const_vector (mode, 0);
    }

  for (mode = GET_CLASS_NARROWEST_MODE (MODE_VECTOR_UFRACT);
       mode != VOIDmode;
       mode = GET_MODE_WIDER_MODE (mode))
    {
      const_tiny_rtx[0][(int) mode] = gen_const_vector (mode, 0);
    }

  for (mode = GET_CLASS_NARROWEST_MODE (MODE_VECTOR_ACCUM);
       mode != VOIDmode;
       mode = GET_MODE_WIDER_MODE (mode))
    {
      const_tiny_rtx[0][(int) mode] = gen_const_vector (mode, 0);
      const_tiny_rtx[1][(int) mode] = gen_const_vector (mode, 1);
    }

  for (mode = GET_CLASS_NARROWEST_MODE (MODE_VECTOR_UACCUM);
       mode != VOIDmode;
       mode = GET_MODE_WIDER_MODE (mode))
    {
      const_tiny_rtx[0][(int) mode] = gen_const_vector (mode, 0);
      const_tiny_rtx[1][(int) mode] = gen_const_vector (mode, 1);
    }

  for (i = (int) CCmode; i < (int) MAX_MACHINE_MODE; ++i)
    if (GET_MODE_CLASS ((machine_mode) i) == MODE_CC)
      const_tiny_rtx[0][i] = const0_rtx;

  const_tiny_rtx[0][(int) BImode] = const0_rtx;
  if (STORE_FLAG_VALUE == 1)
    const_tiny_rtx[1][(int) BImode] = const1_rtx;

  for (mode = GET_CLASS_NARROWEST_MODE (MODE_POINTER_BOUNDS);
       mode != VOIDmode;
       mode = GET_MODE_WIDER_MODE (mode))
    {
      wide_int wi_zero = wi::zero (GET_MODE_PRECISION (mode));
      const_tiny_rtx[0][mode] = immed_wide_int_const (wi_zero, mode);
    }

  pc_rtx = gen_rtx_fmt_ (PC, VOIDmode);
  ret_rtx = gen_rtx_fmt_ (RETURN, VOIDmode);
  simple_return_rtx = gen_rtx_fmt_ (SIMPLE_RETURN, VOIDmode);
  cc0_rtx = gen_rtx_fmt_ (CC0, VOIDmode);
  invalid_insn_rtx = gen_rtx_INSN (VOIDmode,
				   /*prev_insn=*/NULL,
				   /*next_insn=*/NULL,
				   /*bb=*/NULL,
				   /*pattern=*/NULL_RTX,
				   /*location=*/-1,
				   CODE_FOR_nothing,
				   /*reg_notes=*/NULL_RTX);
}

/* Produce exact duplicate of insn INSN after AFTER.
   Care updating of libcall regions if present.  */

rtx_insn *
emit_copy_of_insn_after (rtx_insn *insn, rtx_insn *after)
{
  rtx_insn *new_rtx;
  rtx link;

  switch (GET_CODE (insn))
    {
    case INSN:
      new_rtx = emit_insn_after (copy_insn (PATTERN (insn)), after);
      break;

    case JUMP_INSN:
      new_rtx = emit_jump_insn_after (copy_insn (PATTERN (insn)), after);
      CROSSING_JUMP_P (new_rtx) = CROSSING_JUMP_P (insn);
      break;

    case DEBUG_INSN:
      new_rtx = emit_debug_insn_after (copy_insn (PATTERN (insn)), after);
      break;

    case CALL_INSN:
      new_rtx = emit_call_insn_after (copy_insn (PATTERN (insn)), after);
      if (CALL_INSN_FUNCTION_USAGE (insn))
	CALL_INSN_FUNCTION_USAGE (new_rtx)
	  = copy_insn (CALL_INSN_FUNCTION_USAGE (insn));
      SIBLING_CALL_P (new_rtx) = SIBLING_CALL_P (insn);
      RTL_CONST_CALL_P (new_rtx) = RTL_CONST_CALL_P (insn);
      RTL_PURE_CALL_P (new_rtx) = RTL_PURE_CALL_P (insn);
      RTL_LOOPING_CONST_OR_PURE_CALL_P (new_rtx)
	= RTL_LOOPING_CONST_OR_PURE_CALL_P (insn);
      break;

    default:
      gcc_unreachable ();
    }

  /* Update LABEL_NUSES.  */
  mark_jump_label (PATTERN (new_rtx), new_rtx, 0);

  INSN_LOCATION (new_rtx) = INSN_LOCATION (insn);

  /* If the old insn is frame related, then so is the new one.  This is
     primarily needed for IA-64 unwind info which marks epilogue insns,
     which may be duplicated by the basic block reordering code.  */
  RTX_FRAME_RELATED_P (new_rtx) = RTX_FRAME_RELATED_P (insn);

  /* Copy all REG_NOTES except REG_LABEL_OPERAND since mark_jump_label
     will make them.  REG_LABEL_TARGETs are created there too, but are
     supposed to be sticky, so we copy them.  */
  for (link = REG_NOTES (insn); link; link = XEXP (link, 1))
    if (REG_NOTE_KIND (link) != REG_LABEL_OPERAND)
      {
	if (GET_CODE (link) == EXPR_LIST)
	  add_reg_note (new_rtx, REG_NOTE_KIND (link),
			copy_insn_1 (XEXP (link, 0)));
	else
	  add_shallow_copy_of_reg_note (new_rtx, link);
      }

  INSN_CODE (new_rtx) = INSN_CODE (insn);
  return new_rtx;
}

static GTY((deletable)) rtx hard_reg_clobbers [NUM_MACHINE_MODES][FIRST_PSEUDO_REGISTER];
rtx
gen_hard_reg_clobber (machine_mode mode, unsigned int regno)
{
  if (hard_reg_clobbers[mode][regno])
    return hard_reg_clobbers[mode][regno];
  else
    return (hard_reg_clobbers[mode][regno] =
	    gen_rtx_CLOBBER (VOIDmode, gen_rtx_REG (mode, regno)));
}

location_t prologue_location;
location_t epilogue_location;

/* Hold current location information and last location information, so the
   datastructures are built lazily only when some instructions in given
   place are needed.  */
static location_t curr_location;

/* Allocate insn location datastructure.  */
void
insn_locations_init (void)
{
  prologue_location = epilogue_location = 0;
  curr_location = UNKNOWN_LOCATION;
}

/* At the end of emit stage, clear current location.  */
void
insn_locations_finalize (void)
{
  epilogue_location = curr_location;
  curr_location = UNKNOWN_LOCATION;
}

/* Set current location.  */
void
set_curr_insn_location (location_t location)
{
  curr_location = location;
}

/* Get current location.  */
location_t
curr_insn_location (void)
{
  return curr_location;
}

/* Return lexical scope block insn belongs to.  */
tree
insn_scope (const rtx_insn *insn)
{
  return LOCATION_BLOCK (INSN_LOCATION (insn));
}

/* Return line number of the statement that produced this insn.  */
int
insn_line (const rtx_insn *insn)
{
  return LOCATION_LINE (INSN_LOCATION (insn));
}

/* Return source file of the statement that produced this insn.  */
const char *
insn_file (const rtx_insn *insn)
{
  return LOCATION_FILE (INSN_LOCATION (insn));
}

/* Return expanded location of the statement that produced this insn.  */
expanded_location
insn_location (const rtx_insn *insn)
{
  return expand_location (INSN_LOCATION (insn));
}

/* Return true if memory model MODEL requires a pre-operation (release-style)
   barrier or a post-operation (acquire-style) barrier.  While not universal,
   this function matches behavior of several targets.  */

bool
need_atomic_barrier_p (enum memmodel model, bool pre)
{
  switch (model & MEMMODEL_BASE_MASK)
    {
    case MEMMODEL_RELAXED:
    case MEMMODEL_CONSUME:
      return false;
    case MEMMODEL_RELEASE:
      return pre;
    case MEMMODEL_ACQUIRE:
      return !pre;
    case MEMMODEL_ACQ_REL:
    case MEMMODEL_SEQ_CST:
      return true;
    default:
      gcc_unreachable ();
    }
}

#include "gt-emit-rtl.h"
