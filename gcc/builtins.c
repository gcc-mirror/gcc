/* Expand builtin functions.
   Copyright (C) 1988-2017 Free Software Foundation, Inc.

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

/* Legacy warning!  Please add no further builtin simplifications here
   (apart from pure constant folding) - builtin simplifications should go
   to match.pd or gimple-fold.c instead.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "backend.h"
#include "target.h"
#include "rtl.h"
#include "tree.h"
#include "memmodel.h"
#include "gimple.h"
#include "predict.h"
#include "tm_p.h"
#include "stringpool.h"
#include "tree-vrp.h"
#include "tree-ssanames.h"
#include "expmed.h"
#include "optabs.h"
#include "emit-rtl.h"
#include "recog.h"
#include "diagnostic-core.h"
#include "alias.h"
#include "fold-const.h"
#include "fold-const-call.h"
#include "stor-layout.h"
#include "calls.h"
#include "varasm.h"
#include "tree-object-size.h"
#include "realmpfr.h"
#include "cfgrtl.h"
#include "except.h"
#include "dojump.h"
#include "explow.h"
#include "stmt.h"
#include "expr.h"
#include "libfuncs.h"
#include "output.h"
#include "typeclass.h"
#include "langhooks.h"
#include "value-prof.h"
#include "builtins.h"
#include "stringpool.h"
#include "attribs.h"
#include "asan.h"
#include "cilk.h"
#include "tree-chkp.h"
#include "rtl-chkp.h"
#include "internal-fn.h"
#include "case-cfn-macros.h"
#include "gimple-fold.h"
#include "intl.h"

struct target_builtins default_target_builtins;
#if SWITCHABLE_TARGET
struct target_builtins *this_target_builtins = &default_target_builtins;
#endif

/* Define the names of the builtin function types and codes.  */
const char *const built_in_class_names[BUILT_IN_LAST]
  = {"NOT_BUILT_IN", "BUILT_IN_FRONTEND", "BUILT_IN_MD", "BUILT_IN_NORMAL"};

#define DEF_BUILTIN(X, N, C, T, LT, B, F, NA, AT, IM, COND) #X,
const char * built_in_names[(int) END_BUILTINS] =
{
#include "builtins.def"
};

/* Setup an array of builtin_info_type, make sure each element decl is
   initialized to NULL_TREE.  */
builtin_info_type builtin_info[(int)END_BUILTINS];

/* Non-zero if __builtin_constant_p should be folded right away.  */
bool force_folding_builtin_constant_p;

static rtx c_readstr (const char *, machine_mode);
static int target_char_cast (tree, char *);
static rtx get_memory_rtx (tree, tree);
static int apply_args_size (void);
static int apply_result_size (void);
static rtx result_vector (int, rtx);
static void expand_builtin_prefetch (tree);
static rtx expand_builtin_apply_args (void);
static rtx expand_builtin_apply_args_1 (void);
static rtx expand_builtin_apply (rtx, rtx, rtx);
static void expand_builtin_return (rtx);
static enum type_class type_to_class (tree);
static rtx expand_builtin_classify_type (tree);
static rtx expand_builtin_mathfn_3 (tree, rtx, rtx);
static rtx expand_builtin_mathfn_ternary (tree, rtx, rtx);
static rtx expand_builtin_interclass_mathfn (tree, rtx);
static rtx expand_builtin_sincos (tree);
static rtx expand_builtin_cexpi (tree, rtx);
static rtx expand_builtin_int_roundingfn (tree, rtx);
static rtx expand_builtin_int_roundingfn_2 (tree, rtx);
static rtx expand_builtin_next_arg (void);
static rtx expand_builtin_va_start (tree);
static rtx expand_builtin_va_end (tree);
static rtx expand_builtin_va_copy (tree);
static rtx expand_builtin_strcmp (tree, rtx);
static rtx expand_builtin_strncmp (tree, rtx, machine_mode);
static rtx builtin_memcpy_read_str (void *, HOST_WIDE_INT, machine_mode);
static rtx expand_builtin_memchr (tree, rtx);
static rtx expand_builtin_memcpy (tree, rtx);
static rtx expand_builtin_memcpy_with_bounds (tree, rtx);
static rtx expand_builtin_memory_copy_args (tree dest, tree src, tree len,
					    rtx target, tree exp, int endp);
static rtx expand_builtin_memmove (tree, rtx);
static rtx expand_builtin_mempcpy (tree, rtx);
static rtx expand_builtin_mempcpy_with_bounds (tree, rtx);
static rtx expand_builtin_mempcpy_args (tree, tree, tree, rtx, tree, int);
static rtx expand_builtin_strcat (tree, rtx);
static rtx expand_builtin_strcpy (tree, rtx);
static rtx expand_builtin_strcpy_args (tree, tree, rtx);
static rtx expand_builtin_stpcpy (tree, rtx, machine_mode);
static rtx expand_builtin_stpncpy (tree, rtx);
static rtx expand_builtin_strncat (tree, rtx);
static rtx expand_builtin_strncpy (tree, rtx);
static rtx builtin_memset_gen_str (void *, HOST_WIDE_INT, machine_mode);
static rtx expand_builtin_memset (tree, rtx, machine_mode);
static rtx expand_builtin_memset_with_bounds (tree, rtx, machine_mode);
static rtx expand_builtin_memset_args (tree, tree, tree, rtx, machine_mode, tree);
static rtx expand_builtin_bzero (tree);
static rtx expand_builtin_strlen (tree, rtx, machine_mode);
static rtx expand_builtin_alloca (tree);
static rtx expand_builtin_unop (machine_mode, tree, rtx, rtx, optab);
static rtx expand_builtin_frame_address (tree, tree);
static tree stabilize_va_list_loc (location_t, tree, int);
static rtx expand_builtin_expect (tree, rtx);
static tree fold_builtin_constant_p (tree);
static tree fold_builtin_classify_type (tree);
static tree fold_builtin_strlen (location_t, tree, tree);
static tree fold_builtin_inf (location_t, tree, int);
static tree rewrite_call_expr (location_t, tree, int, tree, int, ...);
static bool validate_arg (const_tree, enum tree_code code);
static rtx expand_builtin_fabs (tree, rtx, rtx);
static rtx expand_builtin_signbit (tree, rtx);
static tree fold_builtin_memcmp (location_t, tree, tree, tree);
static tree fold_builtin_isascii (location_t, tree);
static tree fold_builtin_toascii (location_t, tree);
static tree fold_builtin_isdigit (location_t, tree);
static tree fold_builtin_fabs (location_t, tree, tree);
static tree fold_builtin_abs (location_t, tree, tree);
static tree fold_builtin_unordered_cmp (location_t, tree, tree, tree, enum tree_code,
					enum tree_code);
static tree fold_builtin_0 (location_t, tree);
static tree fold_builtin_1 (location_t, tree, tree);
static tree fold_builtin_2 (location_t, tree, tree, tree);
static tree fold_builtin_3 (location_t, tree, tree, tree, tree);
static tree fold_builtin_varargs (location_t, tree, tree*, int);

static tree fold_builtin_strpbrk (location_t, tree, tree, tree);
static tree fold_builtin_strspn (location_t, tree, tree);
static tree fold_builtin_strcspn (location_t, tree, tree);

static rtx expand_builtin_object_size (tree);
static rtx expand_builtin_memory_chk (tree, rtx, machine_mode,
				      enum built_in_function);
static void maybe_emit_chk_warning (tree, enum built_in_function);
static void maybe_emit_sprintf_chk_warning (tree, enum built_in_function);
static void maybe_emit_free_warning (tree);
static tree fold_builtin_object_size (tree, tree);

unsigned HOST_WIDE_INT target_newline;
unsigned HOST_WIDE_INT target_percent;
static unsigned HOST_WIDE_INT target_c;
static unsigned HOST_WIDE_INT target_s;
char target_percent_c[3];
char target_percent_s[3];
char target_percent_s_newline[4];
static tree do_mpfr_remquo (tree, tree, tree);
static tree do_mpfr_lgamma_r (tree, tree, tree);
static void expand_builtin_sync_synchronize (void);

/* Return true if NAME starts with __builtin_ or __sync_.  */

static bool
is_builtin_name (const char *name)
{
  if (strncmp (name, "__builtin_", 10) == 0)
    return true;
  if (strncmp (name, "__sync_", 7) == 0)
    return true;
  if (strncmp (name, "__atomic_", 9) == 0)
    return true;
  if (flag_cilkplus 
      && (!strcmp (name, "__cilkrts_detach")   
	  || !strcmp (name, "__cilkrts_pop_frame")))
    return true;
  return false;
}


/* Return true if DECL is a function symbol representing a built-in.  */

bool
is_builtin_fn (tree decl)
{
  return TREE_CODE (decl) == FUNCTION_DECL && DECL_BUILT_IN (decl);
}

/* Return true if NODE should be considered for inline expansion regardless
   of the optimization level.  This means whenever a function is invoked with
   its "internal" name, which normally contains the prefix "__builtin".  */

bool
called_as_built_in (tree node)
{
  /* Note that we must use DECL_NAME, not DECL_ASSEMBLER_NAME_SET_P since
     we want the name used to call the function, not the name it
     will have. */
  const char *name = IDENTIFIER_POINTER (DECL_NAME (node));
  return is_builtin_name (name);
}

/* Compute values M and N such that M divides (address of EXP - N) and such
   that N < M.  If these numbers can be determined, store M in alignp and N in
   *BITPOSP and return true.  Otherwise return false and store BITS_PER_UNIT to
   *alignp and any bit-offset to *bitposp.

   Note that the address (and thus the alignment) computed here is based
   on the address to which a symbol resolves, whereas DECL_ALIGN is based
   on the address at which an object is actually located.  These two
   addresses are not always the same.  For example, on ARM targets,
   the address &foo of a Thumb function foo() has the lowest bit set,
   whereas foo() itself starts on an even address.

   If ADDR_P is true we are taking the address of the memory reference EXP
   and thus cannot rely on the access taking place.  */

static bool
get_object_alignment_2 (tree exp, unsigned int *alignp,
			unsigned HOST_WIDE_INT *bitposp, bool addr_p)
{
  HOST_WIDE_INT bitsize, bitpos;
  tree offset;
  machine_mode mode;
  int unsignedp, reversep, volatilep;
  unsigned int align = BITS_PER_UNIT;
  bool known_alignment = false;

  /* Get the innermost object and the constant (bitpos) and possibly
     variable (offset) offset of the access.  */
  exp = get_inner_reference (exp, &bitsize, &bitpos, &offset, &mode,
			     &unsignedp, &reversep, &volatilep);

  /* Extract alignment information from the innermost object and
     possibly adjust bitpos and offset.  */
  if (TREE_CODE (exp) == FUNCTION_DECL)
    {
      /* Function addresses can encode extra information besides their
	 alignment.  However, if TARGET_PTRMEMFUNC_VBIT_LOCATION
	 allows the low bit to be used as a virtual bit, we know
	 that the address itself must be at least 2-byte aligned.  */
      if (TARGET_PTRMEMFUNC_VBIT_LOCATION == ptrmemfunc_vbit_in_pfn)
	align = 2 * BITS_PER_UNIT;
    }
  else if (TREE_CODE (exp) == LABEL_DECL)
    ;
  else if (TREE_CODE (exp) == CONST_DECL)
    {
      /* The alignment of a CONST_DECL is determined by its initializer.  */
      exp = DECL_INITIAL (exp);
      align = TYPE_ALIGN (TREE_TYPE (exp));
      if (CONSTANT_CLASS_P (exp))
	align = (unsigned) CONSTANT_ALIGNMENT (exp, align);

      known_alignment = true;
    }
  else if (DECL_P (exp))
    {
      align = DECL_ALIGN (exp);
      known_alignment = true;
    }
  else if (TREE_CODE (exp) == INDIRECT_REF
	   || TREE_CODE (exp) == MEM_REF
	   || TREE_CODE (exp) == TARGET_MEM_REF)
    {
      tree addr = TREE_OPERAND (exp, 0);
      unsigned ptr_align;
      unsigned HOST_WIDE_INT ptr_bitpos;
      unsigned HOST_WIDE_INT ptr_bitmask = ~0;

      /* If the address is explicitely aligned, handle that.  */
      if (TREE_CODE (addr) == BIT_AND_EXPR
	  && TREE_CODE (TREE_OPERAND (addr, 1)) == INTEGER_CST)
	{
	  ptr_bitmask = TREE_INT_CST_LOW (TREE_OPERAND (addr, 1));
	  ptr_bitmask *= BITS_PER_UNIT;
	  align = least_bit_hwi (ptr_bitmask);
	  addr = TREE_OPERAND (addr, 0);
	}

      known_alignment
	= get_pointer_alignment_1 (addr, &ptr_align, &ptr_bitpos);
      align = MAX (ptr_align, align);

      /* Re-apply explicit alignment to the bitpos.  */
      ptr_bitpos &= ptr_bitmask;

      /* The alignment of the pointer operand in a TARGET_MEM_REF
	 has to take the variable offset parts into account.  */
      if (TREE_CODE (exp) == TARGET_MEM_REF)
	{
	  if (TMR_INDEX (exp))
	    {
	      unsigned HOST_WIDE_INT step = 1;
	      if (TMR_STEP (exp))
		step = TREE_INT_CST_LOW (TMR_STEP (exp));
	      align = MIN (align, least_bit_hwi (step) * BITS_PER_UNIT);
	    }
	  if (TMR_INDEX2 (exp))
	    align = BITS_PER_UNIT;
	  known_alignment = false;
	}

      /* When EXP is an actual memory reference then we can use
	 TYPE_ALIGN of a pointer indirection to derive alignment.
	 Do so only if get_pointer_alignment_1 did not reveal absolute
	 alignment knowledge and if using that alignment would
	 improve the situation.  */
      unsigned int talign;
      if (!addr_p && !known_alignment
	  && (talign = min_align_of_type (TREE_TYPE (exp)) * BITS_PER_UNIT)
	  && talign > align)
	align = talign;
      else
	{
	  /* Else adjust bitpos accordingly.  */
	  bitpos += ptr_bitpos;
	  if (TREE_CODE (exp) == MEM_REF
	      || TREE_CODE (exp) == TARGET_MEM_REF)
	    bitpos += mem_ref_offset (exp).to_short_addr () * BITS_PER_UNIT;
	}
    }
  else if (TREE_CODE (exp) == STRING_CST)
    {
      /* STRING_CST are the only constant objects we allow to be not
         wrapped inside a CONST_DECL.  */
      align = TYPE_ALIGN (TREE_TYPE (exp));
      if (CONSTANT_CLASS_P (exp))
	align = (unsigned) CONSTANT_ALIGNMENT (exp, align);

      known_alignment = true;
    }

  /* If there is a non-constant offset part extract the maximum
     alignment that can prevail.  */
  if (offset)
    {
      unsigned int trailing_zeros = tree_ctz (offset);
      if (trailing_zeros < HOST_BITS_PER_INT)
	{
	  unsigned int inner = (1U << trailing_zeros) * BITS_PER_UNIT;
	  if (inner)
	    align = MIN (align, inner);
	}
    }

  *alignp = align;
  *bitposp = bitpos & (*alignp - 1);
  return known_alignment;
}

/* For a memory reference expression EXP compute values M and N such that M
   divides (&EXP - N) and such that N < M.  If these numbers can be determined,
   store M in alignp and N in *BITPOSP and return true.  Otherwise return false
   and store BITS_PER_UNIT to *alignp and any bit-offset to *bitposp.  */

bool
get_object_alignment_1 (tree exp, unsigned int *alignp,
			unsigned HOST_WIDE_INT *bitposp)
{
  return get_object_alignment_2 (exp, alignp, bitposp, false);
}

/* Return the alignment in bits of EXP, an object.  */

unsigned int
get_object_alignment (tree exp)
{
  unsigned HOST_WIDE_INT bitpos = 0;
  unsigned int align;

  get_object_alignment_1 (exp, &align, &bitpos);

  /* align and bitpos now specify known low bits of the pointer.
     ptr & (align - 1) == bitpos.  */

  if (bitpos != 0)
    align = least_bit_hwi (bitpos);
  return align;
}

/* For a pointer valued expression EXP compute values M and N such that M
   divides (EXP - N) and such that N < M.  If these numbers can be determined,
   store M in alignp and N in *BITPOSP and return true.  Return false if
   the results are just a conservative approximation.

   If EXP is not a pointer, false is returned too.  */

bool
get_pointer_alignment_1 (tree exp, unsigned int *alignp,
			 unsigned HOST_WIDE_INT *bitposp)
{
  STRIP_NOPS (exp);

  if (TREE_CODE (exp) == ADDR_EXPR)
    return get_object_alignment_2 (TREE_OPERAND (exp, 0),
				   alignp, bitposp, true);
  else if (TREE_CODE (exp) == POINTER_PLUS_EXPR)
    {
      unsigned int align;
      unsigned HOST_WIDE_INT bitpos;
      bool res = get_pointer_alignment_1 (TREE_OPERAND (exp, 0),
					  &align, &bitpos);
      if (TREE_CODE (TREE_OPERAND (exp, 1)) == INTEGER_CST)
	bitpos += TREE_INT_CST_LOW (TREE_OPERAND (exp, 1)) * BITS_PER_UNIT;
      else
	{
	  unsigned int trailing_zeros = tree_ctz (TREE_OPERAND (exp, 1));
	  if (trailing_zeros < HOST_BITS_PER_INT)
	    {
	      unsigned int inner = (1U << trailing_zeros) * BITS_PER_UNIT;
	      if (inner)
		align = MIN (align, inner);
	    }
	}
      *alignp = align;
      *bitposp = bitpos & (align - 1);
      return res;
    }
  else if (TREE_CODE (exp) == SSA_NAME
	   && POINTER_TYPE_P (TREE_TYPE (exp)))
    {
      unsigned int ptr_align, ptr_misalign;
      struct ptr_info_def *pi = SSA_NAME_PTR_INFO (exp);

      if (pi && get_ptr_info_alignment (pi, &ptr_align, &ptr_misalign))
	{
	  *bitposp = ptr_misalign * BITS_PER_UNIT;
	  *alignp = ptr_align * BITS_PER_UNIT;
	  /* Make sure to return a sensible alignment when the multiplication
	     by BITS_PER_UNIT overflowed.  */
	  if (*alignp == 0)
	    *alignp = 1u << (HOST_BITS_PER_INT - 1);
	  /* We cannot really tell whether this result is an approximation.  */
	  return false;
	}
      else
	{
	  *bitposp = 0;
	  *alignp = BITS_PER_UNIT;
	  return false;
	}
    }
  else if (TREE_CODE (exp) == INTEGER_CST)
    {
      *alignp = BIGGEST_ALIGNMENT;
      *bitposp = ((TREE_INT_CST_LOW (exp) * BITS_PER_UNIT)
		  & (BIGGEST_ALIGNMENT - 1));
      return true;
    }

  *bitposp = 0;
  *alignp = BITS_PER_UNIT;
  return false;
}

/* Return the alignment in bits of EXP, a pointer valued expression.
   The alignment returned is, by default, the alignment of the thing that
   EXP points to.  If it is not a POINTER_TYPE, 0 is returned.

   Otherwise, look at the expression to see if we can do better, i.e., if the
   expression is actually pointing at an object whose alignment is tighter.  */

unsigned int
get_pointer_alignment (tree exp)
{
  unsigned HOST_WIDE_INT bitpos = 0;
  unsigned int align;

  get_pointer_alignment_1 (exp, &align, &bitpos);

  /* align and bitpos now specify known low bits of the pointer.
     ptr & (align - 1) == bitpos.  */

  if (bitpos != 0)
    align = least_bit_hwi (bitpos);

  return align;
}

/* Return the number of non-zero elements in the sequence
   [ PTR, PTR + MAXELTS ) where each element's size is ELTSIZE bytes.
   ELTSIZE must be a power of 2 less than 8.  Used by c_strlen.  */

static unsigned
string_length (const void *ptr, unsigned eltsize, unsigned maxelts)
{
  gcc_checking_assert (eltsize == 1 || eltsize == 2 || eltsize == 4);

  unsigned n;

  if (eltsize == 1)
    {
      /* Optimize the common case of plain char.  */
      for (n = 0; n < maxelts; n++)
	{
	  const char *elt = (const char*) ptr + n;
	  if (!*elt)
	    break;
	}
    }
  else
    {
      for (n = 0; n < maxelts; n++)
	{
	  const char *elt = (const char*) ptr + n * eltsize;
	  if (!memcmp (elt, "\0\0\0\0", eltsize))
	    break;
	}
    }
  return n;
}

/* Compute the length of a null-terminated character string or wide
   character string handling character sizes of 1, 2, and 4 bytes.
   TREE_STRING_LENGTH is not the right way because it evaluates to
   the size of the character array in bytes (as opposed to characters)
   and because it can contain a zero byte in the middle.

   ONLY_VALUE should be nonzero if the result is not going to be emitted
   into the instruction stream and zero if it is going to be expanded.
   E.g. with i++ ? "foo" : "bar", if ONLY_VALUE is nonzero, constant 3
   is returned, otherwise NULL, since
   len = c_strlen (src, 1); if (len) expand_expr (len, ...); would not
   evaluate the side-effects.

   If ONLY_VALUE is two then we do not emit warnings about out-of-bound
   accesses.  Note that this implies the result is not going to be emitted
   into the instruction stream.

   The value returned is of type `ssizetype'.

   Unfortunately, string_constant can't access the values of const char
   arrays with initializers, so neither can we do so here.  */

tree
c_strlen (tree src, int only_value)
{
  STRIP_NOPS (src);
  if (TREE_CODE (src) == COND_EXPR
      && (only_value || !TREE_SIDE_EFFECTS (TREE_OPERAND (src, 0))))
    {
      tree len1, len2;

      len1 = c_strlen (TREE_OPERAND (src, 1), only_value);
      len2 = c_strlen (TREE_OPERAND (src, 2), only_value);
      if (tree_int_cst_equal (len1, len2))
	return len1;
    }

  if (TREE_CODE (src) == COMPOUND_EXPR
      && (only_value || !TREE_SIDE_EFFECTS (TREE_OPERAND (src, 0))))
    return c_strlen (TREE_OPERAND (src, 1), only_value);

  location_t loc = EXPR_LOC_OR_LOC (src, input_location);

  /* Offset from the beginning of the string in bytes.  */
  tree byteoff;
  src = string_constant (src, &byteoff);
  if (src == 0)
    return NULL_TREE;

  /* Determine the size of the string element.  */
  unsigned eltsize
    = tree_to_uhwi (TYPE_SIZE_UNIT (TREE_TYPE (TREE_TYPE (src))));

  /* Set MAXELTS to sizeof (SRC) / sizeof (*SRC) - 1, the maximum possible
     length of SRC.  */
  unsigned maxelts = TREE_STRING_LENGTH (src) / eltsize - 1;

  /* PTR can point to the byte representation of any string type, including
     char* and wchar_t*.  */
  const char *ptr = TREE_STRING_POINTER (src);

  if (byteoff && TREE_CODE (byteoff) != INTEGER_CST)
    {
      /* If the string has an internal zero byte (e.g., "foo\0bar"), we can't
	 compute the offset to the following null if we don't know where to
	 start searching for it.  */
      if (string_length (ptr, eltsize, maxelts) < maxelts)
	{
	  /* Return when an embedded null character is found.  */
	  return NULL_TREE;
	}

      /* We don't know the starting offset, but we do know that the string
	 has no internal zero bytes.  We can assume that the offset falls
	 within the bounds of the string; otherwise, the programmer deserves
	 what he gets.  Subtract the offset from the length of the string,
	 and return that.  This would perhaps not be valid if we were dealing
	 with named arrays in addition to literal string constants.  */

      return size_diffop_loc (loc, size_int (maxelts * eltsize), byteoff);
    }

  /* Offset from the beginning of the string in elements.  */
  HOST_WIDE_INT eltoff;

  /* We have a known offset into the string.  Start searching there for
     a null character if we can represent it as a single HOST_WIDE_INT.  */
  if (byteoff == 0)
    eltoff = 0;
  else if (! tree_fits_shwi_p (byteoff))
    eltoff = -1;
  else
    eltoff = tree_to_shwi (byteoff) / eltsize;

  /* If the offset is known to be out of bounds, warn, and call strlen at
     runtime.  */
  if (eltoff < 0 || eltoff > maxelts)
    {
     /* Suppress multiple warnings for propagated constant strings.  */
      if (only_value != 2
	  && !TREE_NO_WARNING (src))
        {
	  warning_at (loc, 0, "offset %qwi outside bounds of constant string",
		      eltoff);
          TREE_NO_WARNING (src) = 1;
        }
      return NULL_TREE;
    }

  /* Use strlen to search for the first zero byte.  Since any strings
     constructed with build_string will have nulls appended, we win even
     if we get handed something like (char[4])"abcd".

     Since ELTOFF is our starting index into the string, no further
     calculation is needed.  */
  unsigned len = string_length (ptr + eltoff * eltsize, eltsize,
				maxelts - eltoff);

  return ssize_int (len);
}

/* Return a constant integer corresponding to target reading
   GET_MODE_BITSIZE (MODE) bits from string constant STR.  */

static rtx
c_readstr (const char *str, machine_mode mode)
{
  HOST_WIDE_INT ch;
  unsigned int i, j;
  HOST_WIDE_INT tmp[MAX_BITSIZE_MODE_ANY_INT / HOST_BITS_PER_WIDE_INT];

  gcc_assert (GET_MODE_CLASS (mode) == MODE_INT);
  unsigned int len = (GET_MODE_PRECISION (mode) + HOST_BITS_PER_WIDE_INT - 1)
    / HOST_BITS_PER_WIDE_INT;

  gcc_assert (len <= MAX_BITSIZE_MODE_ANY_INT / HOST_BITS_PER_WIDE_INT);
  for (i = 0; i < len; i++)
    tmp[i] = 0;

  ch = 1;
  for (i = 0; i < GET_MODE_SIZE (mode); i++)
    {
      j = i;
      if (WORDS_BIG_ENDIAN)
	j = GET_MODE_SIZE (mode) - i - 1;
      if (BYTES_BIG_ENDIAN != WORDS_BIG_ENDIAN
	  && GET_MODE_SIZE (mode) >= UNITS_PER_WORD)
	j = j + UNITS_PER_WORD - 2 * (j % UNITS_PER_WORD) - 1;
      j *= BITS_PER_UNIT;

      if (ch)
	ch = (unsigned char) str[i];
      tmp[j / HOST_BITS_PER_WIDE_INT] |= ch << (j % HOST_BITS_PER_WIDE_INT);
    }

  wide_int c = wide_int::from_array (tmp, len, GET_MODE_PRECISION (mode));
  return immed_wide_int_const (c, mode);
}

/* Cast a target constant CST to target CHAR and if that value fits into
   host char type, return zero and put that value into variable pointed to by
   P.  */

static int
target_char_cast (tree cst, char *p)
{
  unsigned HOST_WIDE_INT val, hostval;

  if (TREE_CODE (cst) != INTEGER_CST
      || CHAR_TYPE_SIZE > HOST_BITS_PER_WIDE_INT)
    return 1;

  /* Do not care if it fits or not right here.  */
  val = TREE_INT_CST_LOW (cst);

  if (CHAR_TYPE_SIZE < HOST_BITS_PER_WIDE_INT)
    val &= (HOST_WIDE_INT_1U << CHAR_TYPE_SIZE) - 1;

  hostval = val;
  if (HOST_BITS_PER_CHAR < HOST_BITS_PER_WIDE_INT)
    hostval &= (HOST_WIDE_INT_1U << HOST_BITS_PER_CHAR) - 1;

  if (val != hostval)
    return 1;

  *p = hostval;
  return 0;
}

/* Similar to save_expr, but assumes that arbitrary code is not executed
   in between the multiple evaluations.  In particular, we assume that a
   non-addressable local variable will not be modified.  */

static tree
builtin_save_expr (tree exp)
{
  if (TREE_CODE (exp) == SSA_NAME
      || (TREE_ADDRESSABLE (exp) == 0
	  && (TREE_CODE (exp) == PARM_DECL
	      || (VAR_P (exp) && !TREE_STATIC (exp)))))
    return exp;

  return save_expr (exp);
}

/* Given TEM, a pointer to a stack frame, follow the dynamic chain COUNT
   times to get the address of either a higher stack frame, or a return
   address located within it (depending on FNDECL_CODE).  */

static rtx
expand_builtin_return_addr (enum built_in_function fndecl_code, int count)
{
  int i;
  rtx tem = INITIAL_FRAME_ADDRESS_RTX;
  if (tem == NULL_RTX)
    {
      /* For a zero count with __builtin_return_address, we don't care what
	 frame address we return, because target-specific definitions will
	 override us.  Therefore frame pointer elimination is OK, and using
	 the soft frame pointer is OK.

	 For a nonzero count, or a zero count with __builtin_frame_address,
	 we require a stable offset from the current frame pointer to the
	 previous one, so we must use the hard frame pointer, and
	 we must disable frame pointer elimination.  */
      if (count == 0 && fndecl_code == BUILT_IN_RETURN_ADDRESS)
	tem = frame_pointer_rtx;
      else
	{
	  tem = hard_frame_pointer_rtx;

	  /* Tell reload not to eliminate the frame pointer.  */
	  crtl->accesses_prior_frames = 1;
	}
    }

  if (count > 0)
    SETUP_FRAME_ADDRESSES ();

  /* On the SPARC, the return address is not in the frame, it is in a
     register.  There is no way to access it off of the current frame
     pointer, but it can be accessed off the previous frame pointer by
     reading the value from the register window save area.  */
  if (RETURN_ADDR_IN_PREVIOUS_FRAME && fndecl_code == BUILT_IN_RETURN_ADDRESS)
    count--;

  /* Scan back COUNT frames to the specified frame.  */
  for (i = 0; i < count; i++)
    {
      /* Assume the dynamic chain pointer is in the word that the
	 frame address points to, unless otherwise specified.  */
      tem = DYNAMIC_CHAIN_ADDRESS (tem);
      tem = memory_address (Pmode, tem);
      tem = gen_frame_mem (Pmode, tem);
      tem = copy_to_reg (tem);
    }

  /* For __builtin_frame_address, return what we've got.  But, on
     the SPARC for example, we may have to add a bias.  */
  if (fndecl_code == BUILT_IN_FRAME_ADDRESS)
    return FRAME_ADDR_RTX (tem);

  /* For __builtin_return_address, get the return address from that frame.  */
#ifdef RETURN_ADDR_RTX
  tem = RETURN_ADDR_RTX (count, tem);
#else
  tem = memory_address (Pmode,
			plus_constant (Pmode, tem, GET_MODE_SIZE (Pmode)));
  tem = gen_frame_mem (Pmode, tem);
#endif
  return tem;
}

/* Alias set used for setjmp buffer.  */
static alias_set_type setjmp_alias_set = -1;

/* Construct the leading half of a __builtin_setjmp call.  Control will
   return to RECEIVER_LABEL.  This is also called directly by the SJLJ
   exception handling code.  */

void
expand_builtin_setjmp_setup (rtx buf_addr, rtx receiver_label)
{
  machine_mode sa_mode = STACK_SAVEAREA_MODE (SAVE_NONLOCAL);
  rtx stack_save;
  rtx mem;

  if (setjmp_alias_set == -1)
    setjmp_alias_set = new_alias_set ();

  buf_addr = convert_memory_address (Pmode, buf_addr);

  buf_addr = force_reg (Pmode, force_operand (buf_addr, NULL_RTX));

  /* We store the frame pointer and the address of receiver_label in
     the buffer and use the rest of it for the stack save area, which
     is machine-dependent.  */

  mem = gen_rtx_MEM (Pmode, buf_addr);
  set_mem_alias_set (mem, setjmp_alias_set);
  emit_move_insn (mem, targetm.builtin_setjmp_frame_value ());

  mem = gen_rtx_MEM (Pmode, plus_constant (Pmode, buf_addr,
					   GET_MODE_SIZE (Pmode))),
  set_mem_alias_set (mem, setjmp_alias_set);

  emit_move_insn (validize_mem (mem),
		  force_reg (Pmode, gen_rtx_LABEL_REF (Pmode, receiver_label)));

  stack_save = gen_rtx_MEM (sa_mode,
			    plus_constant (Pmode, buf_addr,
					   2 * GET_MODE_SIZE (Pmode)));
  set_mem_alias_set (stack_save, setjmp_alias_set);
  emit_stack_save (SAVE_NONLOCAL, &stack_save);

  /* If there is further processing to do, do it.  */
  if (targetm.have_builtin_setjmp_setup ())
    emit_insn (targetm.gen_builtin_setjmp_setup (buf_addr));

  /* We have a nonlocal label.   */
  cfun->has_nonlocal_label = 1;
}

/* Construct the trailing part of a __builtin_setjmp call.  This is
   also called directly by the SJLJ exception handling code.
   If RECEIVER_LABEL is NULL, instead contruct a nonlocal goto handler.  */

void
expand_builtin_setjmp_receiver (rtx receiver_label)
{
  rtx chain;

  /* Mark the FP as used when we get here, so we have to make sure it's
     marked as used by this function.  */
  emit_use (hard_frame_pointer_rtx);

  /* Mark the static chain as clobbered here so life information
     doesn't get messed up for it.  */
  chain = targetm.calls.static_chain (current_function_decl, true);
  if (chain && REG_P (chain))
    emit_clobber (chain);

  /* Now put in the code to restore the frame pointer, and argument
     pointer, if needed.  */
  if (! targetm.have_nonlocal_goto ())
    {
      /* First adjust our frame pointer to its actual value.  It was
	 previously set to the start of the virtual area corresponding to
	 the stacked variables when we branched here and now needs to be
	 adjusted to the actual hardware fp value.

	 Assignments to virtual registers are converted by
	 instantiate_virtual_regs into the corresponding assignment
	 to the underlying register (fp in this case) that makes
	 the original assignment true.
	 So the following insn will actually be decrementing fp by
	 STARTING_FRAME_OFFSET.  */
      emit_move_insn (virtual_stack_vars_rtx, hard_frame_pointer_rtx);

      /* Restoring the frame pointer also modifies the hard frame pointer.
	 Mark it used (so that the previous assignment remains live once
	 the frame pointer is eliminated) and clobbered (to represent the
	 implicit update from the assignment).  */
      emit_use (hard_frame_pointer_rtx);
      emit_clobber (hard_frame_pointer_rtx);
    }

  if (!HARD_FRAME_POINTER_IS_ARG_POINTER && fixed_regs[ARG_POINTER_REGNUM])
    {
      /* If the argument pointer can be eliminated in favor of the
	 frame pointer, we don't need to restore it.  We assume here
	 that if such an elimination is present, it can always be used.
	 This is the case on all known machines; if we don't make this
	 assumption, we do unnecessary saving on many machines.  */
      size_t i;
      static const struct elims {const int from, to;} elim_regs[] = ELIMINABLE_REGS;

      for (i = 0; i < ARRAY_SIZE (elim_regs); i++)
	if (elim_regs[i].from == ARG_POINTER_REGNUM
	    && elim_regs[i].to == HARD_FRAME_POINTER_REGNUM)
	  break;

      if (i == ARRAY_SIZE (elim_regs))
	{
	  /* Now restore our arg pointer from the address at which it
	     was saved in our stack frame.  */
	  emit_move_insn (crtl->args.internal_arg_pointer,
			  copy_to_reg (get_arg_pointer_save_area ()));
	}
    }

  if (receiver_label != NULL && targetm.have_builtin_setjmp_receiver ())
    emit_insn (targetm.gen_builtin_setjmp_receiver (receiver_label));
  else if (targetm.have_nonlocal_goto_receiver ())
    emit_insn (targetm.gen_nonlocal_goto_receiver ());
  else
    { /* Nothing */ }

  /* We must not allow the code we just generated to be reordered by
     scheduling.  Specifically, the update of the frame pointer must
     happen immediately, not later.  */
  emit_insn (gen_blockage ());
}

/* __builtin_longjmp is passed a pointer to an array of five words (not
   all will be used on all machines).  It operates similarly to the C
   library function of the same name, but is more efficient.  Much of
   the code below is copied from the handling of non-local gotos.  */

static void
expand_builtin_longjmp (rtx buf_addr, rtx value)
{
  rtx fp, lab, stack;
  rtx_insn *insn, *last;
  machine_mode sa_mode = STACK_SAVEAREA_MODE (SAVE_NONLOCAL);

  /* DRAP is needed for stack realign if longjmp is expanded to current
     function  */
  if (SUPPORTS_STACK_ALIGNMENT)
    crtl->need_drap = true;

  if (setjmp_alias_set == -1)
    setjmp_alias_set = new_alias_set ();

  buf_addr = convert_memory_address (Pmode, buf_addr);

  buf_addr = force_reg (Pmode, buf_addr);

  /* We require that the user must pass a second argument of 1, because
     that is what builtin_setjmp will return.  */
  gcc_assert (value == const1_rtx);

  last = get_last_insn ();
  if (targetm.have_builtin_longjmp ())
    emit_insn (targetm.gen_builtin_longjmp (buf_addr));
  else
    {
      fp = gen_rtx_MEM (Pmode, buf_addr);
      lab = gen_rtx_MEM (Pmode, plus_constant (Pmode, buf_addr,
					       GET_MODE_SIZE (Pmode)));

      stack = gen_rtx_MEM (sa_mode, plus_constant (Pmode, buf_addr,
						   2 * GET_MODE_SIZE (Pmode)));
      set_mem_alias_set (fp, setjmp_alias_set);
      set_mem_alias_set (lab, setjmp_alias_set);
      set_mem_alias_set (stack, setjmp_alias_set);

      /* Pick up FP, label, and SP from the block and jump.  This code is
	 from expand_goto in stmt.c; see there for detailed comments.  */
      if (targetm.have_nonlocal_goto ())
	/* We have to pass a value to the nonlocal_goto pattern that will
	   get copied into the static_chain pointer, but it does not matter
	   what that value is, because builtin_setjmp does not use it.  */
	emit_insn (targetm.gen_nonlocal_goto (value, lab, stack, fp));
      else
	{
	  lab = copy_to_reg (lab);

	  emit_clobber (gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (VOIDmode)));
	  emit_clobber (gen_rtx_MEM (BLKmode, hard_frame_pointer_rtx));

	  emit_move_insn (hard_frame_pointer_rtx, fp);
	  emit_stack_restore (SAVE_NONLOCAL, stack);

	  emit_use (hard_frame_pointer_rtx);
	  emit_use (stack_pointer_rtx);
	  emit_indirect_jump (lab);
	}
    }

  /* Search backwards and mark the jump insn as a non-local goto.
     Note that this precludes the use of __builtin_longjmp to a
     __builtin_setjmp target in the same function.  However, we've
     already cautioned the user that these functions are for
     internal exception handling use only.  */
  for (insn = get_last_insn (); insn; insn = PREV_INSN (insn))
    {
      gcc_assert (insn != last);

      if (JUMP_P (insn))
	{
	  add_reg_note (insn, REG_NON_LOCAL_GOTO, const0_rtx);
	  break;
	}
      else if (CALL_P (insn))
	break;
    }
}

static inline bool
more_const_call_expr_args_p (const const_call_expr_arg_iterator *iter)
{
  return (iter->i < iter->n);
}

/* This function validates the types of a function call argument list
   against a specified list of tree_codes.  If the last specifier is a 0,
   that represents an ellipsis, otherwise the last specifier must be a
   VOID_TYPE.  */

static bool
validate_arglist (const_tree callexpr, ...)
{
  enum tree_code code;
  bool res = 0;
  va_list ap;
  const_call_expr_arg_iterator iter;
  const_tree arg;

  va_start (ap, callexpr);
  init_const_call_expr_arg_iterator (callexpr, &iter);

  /* Get a bitmap of pointer argument numbers declared attribute nonnull.  */
  tree fn = CALL_EXPR_FN (callexpr);
  bitmap argmap = get_nonnull_args (TREE_TYPE (TREE_TYPE (fn)));

  for (unsigned argno = 1; ; ++argno)
    {
      code = (enum tree_code) va_arg (ap, int);

      switch (code)
	{
	case 0:
	  /* This signifies an ellipses, any further arguments are all ok.  */
	  res = true;
	  goto end;
	case VOID_TYPE:
	  /* This signifies an endlink, if no arguments remain, return
	     true, otherwise return false.  */
	  res = !more_const_call_expr_args_p (&iter);
	  goto end;
	case POINTER_TYPE:
	  /* The actual argument must be nonnull when either the whole
	     called function has been declared nonnull, or when the formal
	     argument corresponding to the actual argument has been.  */
	  if (argmap
	      && (bitmap_empty_p (argmap) || bitmap_bit_p (argmap, argno)))
	    {
	      arg = next_const_call_expr_arg (&iter);
	      if (!validate_arg (arg, code) || integer_zerop (arg))
		goto end;
	      break;
	    }
	  /* FALLTHRU */
	default:
	  /* If no parameters remain or the parameter's code does not
	     match the specified code, return false.  Otherwise continue
	     checking any remaining arguments.  */
	  arg = next_const_call_expr_arg (&iter);
	  if (!validate_arg (arg, code))
	    goto end;
	  break;
	}
    }

  /* We need gotos here since we can only have one VA_CLOSE in a
     function.  */
 end: ;
  va_end (ap);

  BITMAP_FREE (argmap);

  return res;
}

/* Expand a call to __builtin_nonlocal_goto.  We're passed the target label
   and the address of the save area.  */

static rtx
expand_builtin_nonlocal_goto (tree exp)
{
  tree t_label, t_save_area;
  rtx r_label, r_save_area, r_fp, r_sp;
  rtx_insn *insn;

  if (!validate_arglist (exp, POINTER_TYPE, POINTER_TYPE, VOID_TYPE))
    return NULL_RTX;

  t_label = CALL_EXPR_ARG (exp, 0);
  t_save_area = CALL_EXPR_ARG (exp, 1);

  r_label = expand_normal (t_label);
  r_label = convert_memory_address (Pmode, r_label);
  r_save_area = expand_normal (t_save_area);
  r_save_area = convert_memory_address (Pmode, r_save_area);
  /* Copy the address of the save location to a register just in case it was
     based on the frame pointer.   */
  r_save_area = copy_to_reg (r_save_area);
  r_fp = gen_rtx_MEM (Pmode, r_save_area);
  r_sp = gen_rtx_MEM (STACK_SAVEAREA_MODE (SAVE_NONLOCAL),
		      plus_constant (Pmode, r_save_area,
				     GET_MODE_SIZE (Pmode)));

  crtl->has_nonlocal_goto = 1;

  /* ??? We no longer need to pass the static chain value, afaik.  */
  if (targetm.have_nonlocal_goto ())
    emit_insn (targetm.gen_nonlocal_goto (const0_rtx, r_label, r_sp, r_fp));
  else
    {
      r_label = copy_to_reg (r_label);

      emit_clobber (gen_rtx_MEM (BLKmode, gen_rtx_SCRATCH (VOIDmode)));
      emit_clobber (gen_rtx_MEM (BLKmode, hard_frame_pointer_rtx));

      /* Restore frame pointer for containing function.  */
      emit_move_insn (hard_frame_pointer_rtx, r_fp);
      emit_stack_restore (SAVE_NONLOCAL, r_sp);

      /* USE of hard_frame_pointer_rtx added for consistency;
	 not clear if really needed.  */
      emit_use (hard_frame_pointer_rtx);
      emit_use (stack_pointer_rtx);

      /* If the architecture is using a GP register, we must
	 conservatively assume that the target function makes use of it.
	 The prologue of functions with nonlocal gotos must therefore
	 initialize the GP register to the appropriate value, and we
	 must then make sure that this value is live at the point
	 of the jump.  (Note that this doesn't necessarily apply
	 to targets with a nonlocal_goto pattern; they are free
	 to implement it in their own way.  Note also that this is
	 a no-op if the GP register is a global invariant.)  */
      unsigned regnum = PIC_OFFSET_TABLE_REGNUM;
      if (regnum != INVALID_REGNUM && fixed_regs[regnum])
	emit_use (pic_offset_table_rtx);

      emit_indirect_jump (r_label);
    }

  /* Search backwards to the jump insn and mark it as a
     non-local goto.  */
  for (insn = get_last_insn (); insn; insn = PREV_INSN (insn))
    {
      if (JUMP_P (insn))
	{
	  add_reg_note (insn, REG_NON_LOCAL_GOTO, const0_rtx);
	  break;
	}
      else if (CALL_P (insn))
	break;
    }

  return const0_rtx;
}

/* __builtin_update_setjmp_buf is passed a pointer to an array of five words
   (not all will be used on all machines) that was passed to __builtin_setjmp.
   It updates the stack pointer in that block to the current value.  This is
   also called directly by the SJLJ exception handling code.  */

void
expand_builtin_update_setjmp_buf (rtx buf_addr)
{
  machine_mode sa_mode = STACK_SAVEAREA_MODE (SAVE_NONLOCAL);
  rtx stack_save
    = gen_rtx_MEM (sa_mode,
		   memory_address
		   (sa_mode,
		    plus_constant (Pmode, buf_addr,
				   2 * GET_MODE_SIZE (Pmode))));

  emit_stack_save (SAVE_NONLOCAL, &stack_save);
}

/* Expand a call to __builtin_prefetch.  For a target that does not support
   data prefetch, evaluate the memory address argument in case it has side
   effects.  */

static void
expand_builtin_prefetch (tree exp)
{
  tree arg0, arg1, arg2;
  int nargs;
  rtx op0, op1, op2;

  if (!validate_arglist (exp, POINTER_TYPE, 0))
    return;

  arg0 = CALL_EXPR_ARG (exp, 0);

  /* Arguments 1 and 2 are optional; argument 1 (read/write) defaults to
     zero (read) and argument 2 (locality) defaults to 3 (high degree of
     locality).  */
  nargs = call_expr_nargs (exp);
  if (nargs > 1)
    arg1 = CALL_EXPR_ARG (exp, 1);
  else
    arg1 = integer_zero_node;
  if (nargs > 2)
    arg2 = CALL_EXPR_ARG (exp, 2);
  else
    arg2 = integer_three_node;

  /* Argument 0 is an address.  */
  op0 = expand_expr (arg0, NULL_RTX, Pmode, EXPAND_NORMAL);

  /* Argument 1 (read/write flag) must be a compile-time constant int.  */
  if (TREE_CODE (arg1) != INTEGER_CST)
    {
      error ("second argument to %<__builtin_prefetch%> must be a constant");
      arg1 = integer_zero_node;
    }
  op1 = expand_normal (arg1);
  /* Argument 1 must be either zero or one.  */
  if (INTVAL (op1) != 0 && INTVAL (op1) != 1)
    {
      warning (0, "invalid second argument to %<__builtin_prefetch%>;"
	       " using zero");
      op1 = const0_rtx;
    }

  /* Argument 2 (locality) must be a compile-time constant int.  */
  if (TREE_CODE (arg2) != INTEGER_CST)
    {
      error ("third argument to %<__builtin_prefetch%> must be a constant");
      arg2 = integer_zero_node;
    }
  op2 = expand_normal (arg2);
  /* Argument 2 must be 0, 1, 2, or 3.  */
  if (INTVAL (op2) < 0 || INTVAL (op2) > 3)
    {
      warning (0, "invalid third argument to %<__builtin_prefetch%>; using zero");
      op2 = const0_rtx;
    }

  if (targetm.have_prefetch ())
    {
      struct expand_operand ops[3];

      create_address_operand (&ops[0], op0);
      create_integer_operand (&ops[1], INTVAL (op1));
      create_integer_operand (&ops[2], INTVAL (op2));
      if (maybe_expand_insn (targetm.code_for_prefetch, 3, ops))
	return;
    }

  /* Don't do anything with direct references to volatile memory, but
     generate code to handle other side effects.  */
  if (!MEM_P (op0) && side_effects_p (op0))
    emit_insn (op0);
}

/* Get a MEM rtx for expression EXP which is the address of an operand
   to be used in a string instruction (cmpstrsi, movmemsi, ..).  LEN is
   the maximum length of the block of memory that might be accessed or
   NULL if unknown.  */

static rtx
get_memory_rtx (tree exp, tree len)
{
  tree orig_exp = exp;
  rtx addr, mem;

  /* When EXP is not resolved SAVE_EXPR, MEM_ATTRS can be still derived
     from its expression, for expr->a.b only <variable>.a.b is recorded.  */
  if (TREE_CODE (exp) == SAVE_EXPR && !SAVE_EXPR_RESOLVED_P (exp))
    exp = TREE_OPERAND (exp, 0);

  addr = expand_expr (orig_exp, NULL_RTX, ptr_mode, EXPAND_NORMAL);
  mem = gen_rtx_MEM (BLKmode, memory_address (BLKmode, addr));

  /* Get an expression we can use to find the attributes to assign to MEM.
     First remove any nops.  */
  while (CONVERT_EXPR_P (exp)
	 && POINTER_TYPE_P (TREE_TYPE (TREE_OPERAND (exp, 0))))
    exp = TREE_OPERAND (exp, 0);

  /* Build a MEM_REF representing the whole accessed area as a byte blob,
     (as builtin stringops may alias with anything).  */
  exp = fold_build2 (MEM_REF,
		     build_array_type (char_type_node,
				       build_range_type (sizetype,
							 size_one_node, len)),
		     exp, build_int_cst (ptr_type_node, 0));

  /* If the MEM_REF has no acceptable address, try to get the base object
     from the original address we got, and build an all-aliasing
     unknown-sized access to that one.  */
  if (is_gimple_mem_ref_addr (TREE_OPERAND (exp, 0)))
    set_mem_attributes (mem, exp, 0);
  else if (TREE_CODE (TREE_OPERAND (exp, 0)) == ADDR_EXPR
	   && (exp = get_base_address (TREE_OPERAND (TREE_OPERAND (exp, 0),
						     0))))
    {
      exp = build_fold_addr_expr (exp);
      exp = fold_build2 (MEM_REF,
			 build_array_type (char_type_node,
					   build_range_type (sizetype,
							     size_zero_node,
							     NULL)),
			 exp, build_int_cst (ptr_type_node, 0));
      set_mem_attributes (mem, exp, 0);
    }
  set_mem_alias_set (mem, 0);
  return mem;
}

/* Built-in functions to perform an untyped call and return.  */

#define apply_args_mode \
  (this_target_builtins->x_apply_args_mode)
#define apply_result_mode \
  (this_target_builtins->x_apply_result_mode)

/* Return the size required for the block returned by __builtin_apply_args,
   and initialize apply_args_mode.  */

static int
apply_args_size (void)
{
  static int size = -1;
  int align;
  unsigned int regno;
  machine_mode mode;

  /* The values computed by this function never change.  */
  if (size < 0)
    {
      /* The first value is the incoming arg-pointer.  */
      size = GET_MODE_SIZE (Pmode);

      /* The second value is the structure value address unless this is
	 passed as an "invisible" first argument.  */
      if (targetm.calls.struct_value_rtx (cfun ? TREE_TYPE (cfun->decl) : 0, 0))
	size += GET_MODE_SIZE (Pmode);

      for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
	if (FUNCTION_ARG_REGNO_P (regno))
	  {
	    mode = targetm.calls.get_raw_arg_mode (regno);

	    gcc_assert (mode != VOIDmode);

	    align = GET_MODE_ALIGNMENT (mode) / BITS_PER_UNIT;
	    if (size % align != 0)
	      size = CEIL (size, align) * align;
	    size += GET_MODE_SIZE (mode);
	    apply_args_mode[regno] = mode;
	  }
	else
	  {
	    apply_args_mode[regno] = VOIDmode;
	  }
    }
  return size;
}

/* Return the size required for the block returned by __builtin_apply,
   and initialize apply_result_mode.  */

static int
apply_result_size (void)
{
  static int size = -1;
  int align, regno;
  machine_mode mode;

  /* The values computed by this function never change.  */
  if (size < 0)
    {
      size = 0;

      for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
	if (targetm.calls.function_value_regno_p (regno))
	  {
	    mode = targetm.calls.get_raw_result_mode (regno);

	    gcc_assert (mode != VOIDmode);

	    align = GET_MODE_ALIGNMENT (mode) / BITS_PER_UNIT;
	    if (size % align != 0)
	      size = CEIL (size, align) * align;
	    size += GET_MODE_SIZE (mode);
	    apply_result_mode[regno] = mode;
	  }
	else
	  apply_result_mode[regno] = VOIDmode;

      /* Allow targets that use untyped_call and untyped_return to override
	 the size so that machine-specific information can be stored here.  */
#ifdef APPLY_RESULT_SIZE
      size = APPLY_RESULT_SIZE;
#endif
    }
  return size;
}

/* Create a vector describing the result block RESULT.  If SAVEP is true,
   the result block is used to save the values; otherwise it is used to
   restore the values.  */

static rtx
result_vector (int savep, rtx result)
{
  int regno, size, align, nelts;
  machine_mode mode;
  rtx reg, mem;
  rtx *savevec = XALLOCAVEC (rtx, FIRST_PSEUDO_REGISTER);

  size = nelts = 0;
  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    if ((mode = apply_result_mode[regno]) != VOIDmode)
      {
	align = GET_MODE_ALIGNMENT (mode) / BITS_PER_UNIT;
	if (size % align != 0)
	  size = CEIL (size, align) * align;
	reg = gen_rtx_REG (mode, savep ? regno : INCOMING_REGNO (regno));
	mem = adjust_address (result, mode, size);
	savevec[nelts++] = (savep
			    ? gen_rtx_SET (mem, reg)
			    : gen_rtx_SET (reg, mem));
	size += GET_MODE_SIZE (mode);
      }
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec_v (nelts, savevec));
}

/* Save the state required to perform an untyped call with the same
   arguments as were passed to the current function.  */

static rtx
expand_builtin_apply_args_1 (void)
{
  rtx registers, tem;
  int size, align, regno;
  machine_mode mode;
  rtx struct_incoming_value = targetm.calls.struct_value_rtx (cfun ? TREE_TYPE (cfun->decl) : 0, 1);

  /* Create a block where the arg-pointer, structure value address,
     and argument registers can be saved.  */
  registers = assign_stack_local (BLKmode, apply_args_size (), -1);

  /* Walk past the arg-pointer and structure value address.  */
  size = GET_MODE_SIZE (Pmode);
  if (targetm.calls.struct_value_rtx (cfun ? TREE_TYPE (cfun->decl) : 0, 0))
    size += GET_MODE_SIZE (Pmode);

  /* Save each register used in calling a function to the block.  */
  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    if ((mode = apply_args_mode[regno]) != VOIDmode)
      {
	align = GET_MODE_ALIGNMENT (mode) / BITS_PER_UNIT;
	if (size % align != 0)
	  size = CEIL (size, align) * align;

	tem = gen_rtx_REG (mode, INCOMING_REGNO (regno));

	emit_move_insn (adjust_address (registers, mode, size), tem);
	size += GET_MODE_SIZE (mode);
      }

  /* Save the arg pointer to the block.  */
  tem = copy_to_reg (crtl->args.internal_arg_pointer);
  /* We need the pointer as the caller actually passed them to us, not
     as we might have pretended they were passed.  Make sure it's a valid
     operand, as emit_move_insn isn't expected to handle a PLUS.  */
  if (STACK_GROWS_DOWNWARD)
    tem
      = force_operand (plus_constant (Pmode, tem,
				      crtl->args.pretend_args_size),
		       NULL_RTX);
  emit_move_insn (adjust_address (registers, Pmode, 0), tem);

  size = GET_MODE_SIZE (Pmode);

  /* Save the structure value address unless this is passed as an
     "invisible" first argument.  */
  if (struct_incoming_value)
    {
      emit_move_insn (adjust_address (registers, Pmode, size),
		      copy_to_reg (struct_incoming_value));
      size += GET_MODE_SIZE (Pmode);
    }

  /* Return the address of the block.  */
  return copy_addr_to_reg (XEXP (registers, 0));
}

/* __builtin_apply_args returns block of memory allocated on
   the stack into which is stored the arg pointer, structure
   value address, static chain, and all the registers that might
   possibly be used in performing a function call.  The code is
   moved to the start of the function so the incoming values are
   saved.  */

static rtx
expand_builtin_apply_args (void)
{
  /* Don't do __builtin_apply_args more than once in a function.
     Save the result of the first call and reuse it.  */
  if (apply_args_value != 0)
    return apply_args_value;
  {
    /* When this function is called, it means that registers must be
       saved on entry to this function.  So we migrate the
       call to the first insn of this function.  */
    rtx temp;

    start_sequence ();
    temp = expand_builtin_apply_args_1 ();
    rtx_insn *seq = get_insns ();
    end_sequence ();

    apply_args_value = temp;

    /* Put the insns after the NOTE that starts the function.
       If this is inside a start_sequence, make the outer-level insn
       chain current, so the code is placed at the start of the
       function.  If internal_arg_pointer is a non-virtual pseudo,
       it needs to be placed after the function that initializes
       that pseudo.  */
    push_topmost_sequence ();
    if (REG_P (crtl->args.internal_arg_pointer)
	&& REGNO (crtl->args.internal_arg_pointer) > LAST_VIRTUAL_REGISTER)
      emit_insn_before (seq, parm_birth_insn);
    else
      emit_insn_before (seq, NEXT_INSN (entry_of_function ()));
    pop_topmost_sequence ();
    return temp;
  }
}

/* Perform an untyped call and save the state required to perform an
   untyped return of whatever value was returned by the given function.  */

static rtx
expand_builtin_apply (rtx function, rtx arguments, rtx argsize)
{
  int size, align, regno;
  machine_mode mode;
  rtx incoming_args, result, reg, dest, src;
  rtx_call_insn *call_insn;
  rtx old_stack_level = 0;
  rtx call_fusage = 0;
  rtx struct_value = targetm.calls.struct_value_rtx (cfun ? TREE_TYPE (cfun->decl) : 0, 0);

  arguments = convert_memory_address (Pmode, arguments);

  /* Create a block where the return registers can be saved.  */
  result = assign_stack_local (BLKmode, apply_result_size (), -1);

  /* Fetch the arg pointer from the ARGUMENTS block.  */
  incoming_args = gen_reg_rtx (Pmode);
  emit_move_insn (incoming_args, gen_rtx_MEM (Pmode, arguments));
  if (!STACK_GROWS_DOWNWARD)
    incoming_args = expand_simple_binop (Pmode, MINUS, incoming_args, argsize,
					 incoming_args, 0, OPTAB_LIB_WIDEN);

  /* Push a new argument block and copy the arguments.  Do not allow
     the (potential) memcpy call below to interfere with our stack
     manipulations.  */
  do_pending_stack_adjust ();
  NO_DEFER_POP;

  /* Save the stack with nonlocal if available.  */
  if (targetm.have_save_stack_nonlocal ())
    emit_stack_save (SAVE_NONLOCAL, &old_stack_level);
  else
    emit_stack_save (SAVE_BLOCK, &old_stack_level);

  /* Allocate a block of memory onto the stack and copy the memory
     arguments to the outgoing arguments address.  We can pass TRUE
     as the 4th argument because we just saved the stack pointer
     and will restore it right after the call.  */
  allocate_dynamic_stack_space (argsize, 0, BIGGEST_ALIGNMENT, true);

  /* Set DRAP flag to true, even though allocate_dynamic_stack_space
     may have already set current_function_calls_alloca to true.
     current_function_calls_alloca won't be set if argsize is zero,
     so we have to guarantee need_drap is true here.  */
  if (SUPPORTS_STACK_ALIGNMENT)
    crtl->need_drap = true;

  dest = virtual_outgoing_args_rtx;
  if (!STACK_GROWS_DOWNWARD)
    {
      if (CONST_INT_P (argsize))
	dest = plus_constant (Pmode, dest, -INTVAL (argsize));
      else
	dest = gen_rtx_PLUS (Pmode, dest, negate_rtx (Pmode, argsize));
    }
  dest = gen_rtx_MEM (BLKmode, dest);
  set_mem_align (dest, PARM_BOUNDARY);
  src = gen_rtx_MEM (BLKmode, incoming_args);
  set_mem_align (src, PARM_BOUNDARY);
  emit_block_move (dest, src, argsize, BLOCK_OP_NORMAL);

  /* Refer to the argument block.  */
  apply_args_size ();
  arguments = gen_rtx_MEM (BLKmode, arguments);
  set_mem_align (arguments, PARM_BOUNDARY);

  /* Walk past the arg-pointer and structure value address.  */
  size = GET_MODE_SIZE (Pmode);
  if (struct_value)
    size += GET_MODE_SIZE (Pmode);

  /* Restore each of the registers previously saved.  Make USE insns
     for each of these registers for use in making the call.  */
  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    if ((mode = apply_args_mode[regno]) != VOIDmode)
      {
	align = GET_MODE_ALIGNMENT (mode) / BITS_PER_UNIT;
	if (size % align != 0)
	  size = CEIL (size, align) * align;
	reg = gen_rtx_REG (mode, regno);
	emit_move_insn (reg, adjust_address (arguments, mode, size));
	use_reg (&call_fusage, reg);
	size += GET_MODE_SIZE (mode);
      }

  /* Restore the structure value address unless this is passed as an
     "invisible" first argument.  */
  size = GET_MODE_SIZE (Pmode);
  if (struct_value)
    {
      rtx value = gen_reg_rtx (Pmode);
      emit_move_insn (value, adjust_address (arguments, Pmode, size));
      emit_move_insn (struct_value, value);
      if (REG_P (struct_value))
	use_reg (&call_fusage, struct_value);
      size += GET_MODE_SIZE (Pmode);
    }

  /* All arguments and registers used for the call are set up by now!  */
  function = prepare_call_address (NULL, function, NULL, &call_fusage, 0, 0);

  /* Ensure address is valid.  SYMBOL_REF is already valid, so no need,
     and we don't want to load it into a register as an optimization,
     because prepare_call_address already did it if it should be done.  */
  if (GET_CODE (function) != SYMBOL_REF)
    function = memory_address (FUNCTION_MODE, function);

  /* Generate the actual call instruction and save the return value.  */
  if (targetm.have_untyped_call ())
    {
      rtx mem = gen_rtx_MEM (FUNCTION_MODE, function);
      emit_call_insn (targetm.gen_untyped_call (mem, result,
						result_vector (1, result)));
    }
  else if (targetm.have_call_value ())
    {
      rtx valreg = 0;

      /* Locate the unique return register.  It is not possible to
	 express a call that sets more than one return register using
	 call_value; use untyped_call for that.  In fact, untyped_call
	 only needs to save the return registers in the given block.  */
      for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
	if ((mode = apply_result_mode[regno]) != VOIDmode)
	  {
	    gcc_assert (!valreg); /* have_untyped_call required.  */

	    valreg = gen_rtx_REG (mode, regno);
	  }

      emit_insn (targetm.gen_call_value (valreg,
					 gen_rtx_MEM (FUNCTION_MODE, function),
					 const0_rtx, NULL_RTX, const0_rtx));

      emit_move_insn (adjust_address (result, GET_MODE (valreg), 0), valreg);
    }
  else
    gcc_unreachable ();

  /* Find the CALL insn we just emitted, and attach the register usage
     information.  */
  call_insn = last_call_insn ();
  add_function_usage_to (call_insn, call_fusage);

  /* Restore the stack.  */
  if (targetm.have_save_stack_nonlocal ())
    emit_stack_restore (SAVE_NONLOCAL, old_stack_level);
  else
    emit_stack_restore (SAVE_BLOCK, old_stack_level);
  fixup_args_size_notes (call_insn, get_last_insn (), 0);

  OK_DEFER_POP;

  /* Return the address of the result block.  */
  result = copy_addr_to_reg (XEXP (result, 0));
  return convert_memory_address (ptr_mode, result);
}

/* Perform an untyped return.  */

static void
expand_builtin_return (rtx result)
{
  int size, align, regno;
  machine_mode mode;
  rtx reg;
  rtx_insn *call_fusage = 0;

  result = convert_memory_address (Pmode, result);

  apply_result_size ();
  result = gen_rtx_MEM (BLKmode, result);

  if (targetm.have_untyped_return ())
    {
      rtx vector = result_vector (0, result);
      emit_jump_insn (targetm.gen_untyped_return (result, vector));
      emit_barrier ();
      return;
    }

  /* Restore the return value and note that each value is used.  */
  size = 0;
  for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
    if ((mode = apply_result_mode[regno]) != VOIDmode)
      {
	align = GET_MODE_ALIGNMENT (mode) / BITS_PER_UNIT;
	if (size % align != 0)
	  size = CEIL (size, align) * align;
	reg = gen_rtx_REG (mode, INCOMING_REGNO (regno));
	emit_move_insn (reg, adjust_address (result, mode, size));

	push_to_sequence (call_fusage);
	emit_use (reg);
	call_fusage = get_insns ();
	end_sequence ();
	size += GET_MODE_SIZE (mode);
      }

  /* Put the USE insns before the return.  */
  emit_insn (call_fusage);

  /* Return whatever values was restored by jumping directly to the end
     of the function.  */
  expand_naked_return ();
}

/* Used by expand_builtin_classify_type and fold_builtin_classify_type.  */

static enum type_class
type_to_class (tree type)
{
  switch (TREE_CODE (type))
    {
    case VOID_TYPE:	   return void_type_class;
    case INTEGER_TYPE:	   return integer_type_class;
    case ENUMERAL_TYPE:	   return enumeral_type_class;
    case BOOLEAN_TYPE:	   return boolean_type_class;
    case POINTER_TYPE:	   return pointer_type_class;
    case REFERENCE_TYPE:   return reference_type_class;
    case OFFSET_TYPE:	   return offset_type_class;
    case REAL_TYPE:	   return real_type_class;
    case COMPLEX_TYPE:	   return complex_type_class;
    case FUNCTION_TYPE:	   return function_type_class;
    case METHOD_TYPE:	   return method_type_class;
    case RECORD_TYPE:	   return record_type_class;
    case UNION_TYPE:
    case QUAL_UNION_TYPE:  return union_type_class;
    case ARRAY_TYPE:	   return (TYPE_STRING_FLAG (type)
				   ? string_type_class : array_type_class);
    case LANG_TYPE:	   return lang_type_class;
    default:		   return no_type_class;
    }
}

/* Expand a call EXP to __builtin_classify_type.  */

static rtx
expand_builtin_classify_type (tree exp)
{
  if (call_expr_nargs (exp))
    return GEN_INT (type_to_class (TREE_TYPE (CALL_EXPR_ARG (exp, 0))));
  return GEN_INT (no_type_class);
}

/* This helper macro, meant to be used in mathfn_built_in below,
   determines which among a set of three builtin math functions is
   appropriate for a given type mode.  The `F' and `L' cases are
   automatically generated from the `double' case.  */
#define CASE_MATHFN(MATHFN) \
  CASE_CFN_##MATHFN: \
  fcode = BUILT_IN_##MATHFN; fcodef = BUILT_IN_##MATHFN##F ; \
  fcodel = BUILT_IN_##MATHFN##L ; break;
/* Similar to above, but appends _R after any F/L suffix.  */
#define CASE_MATHFN_REENT(MATHFN) \
  case CFN_BUILT_IN_##MATHFN##_R: \
  case CFN_BUILT_IN_##MATHFN##F_R: \
  case CFN_BUILT_IN_##MATHFN##L_R: \
  fcode = BUILT_IN_##MATHFN##_R; fcodef = BUILT_IN_##MATHFN##F_R ; \
  fcodel = BUILT_IN_##MATHFN##L_R ; break;

/* Return a function equivalent to FN but operating on floating-point
   values of type TYPE, or END_BUILTINS if no such function exists.
   This is purely an operation on function codes; it does not guarantee
   that the target actually has an implementation of the function.  */

static built_in_function
mathfn_built_in_2 (tree type, combined_fn fn)
{
  built_in_function fcode, fcodef, fcodel;

  switch (fn)
    {
    CASE_MATHFN (ACOS)
    CASE_MATHFN (ACOSH)
    CASE_MATHFN (ASIN)
    CASE_MATHFN (ASINH)
    CASE_MATHFN (ATAN)
    CASE_MATHFN (ATAN2)
    CASE_MATHFN (ATANH)
    CASE_MATHFN (CBRT)
    CASE_MATHFN (CEIL)
    CASE_MATHFN (CEXPI)
    CASE_MATHFN (COPYSIGN)
    CASE_MATHFN (COS)
    CASE_MATHFN (COSH)
    CASE_MATHFN (DREM)
    CASE_MATHFN (ERF)
    CASE_MATHFN (ERFC)
    CASE_MATHFN (EXP)
    CASE_MATHFN (EXP10)
    CASE_MATHFN (EXP2)
    CASE_MATHFN (EXPM1)
    CASE_MATHFN (FABS)
    CASE_MATHFN (FDIM)
    CASE_MATHFN (FLOOR)
    CASE_MATHFN (FMA)
    CASE_MATHFN (FMAX)
    CASE_MATHFN (FMIN)
    CASE_MATHFN (FMOD)
    CASE_MATHFN (FREXP)
    CASE_MATHFN (GAMMA)
    CASE_MATHFN_REENT (GAMMA) /* GAMMA_R */
    CASE_MATHFN (HUGE_VAL)
    CASE_MATHFN (HYPOT)
    CASE_MATHFN (ILOGB)
    CASE_MATHFN (ICEIL)
    CASE_MATHFN (IFLOOR)
    CASE_MATHFN (INF)
    CASE_MATHFN (IRINT)
    CASE_MATHFN (IROUND)
    CASE_MATHFN (ISINF)
    CASE_MATHFN (J0)
    CASE_MATHFN (J1)
    CASE_MATHFN (JN)
    CASE_MATHFN (LCEIL)
    CASE_MATHFN (LDEXP)
    CASE_MATHFN (LFLOOR)
    CASE_MATHFN (LGAMMA)
    CASE_MATHFN_REENT (LGAMMA) /* LGAMMA_R */
    CASE_MATHFN (LLCEIL)
    CASE_MATHFN (LLFLOOR)
    CASE_MATHFN (LLRINT)
    CASE_MATHFN (LLROUND)
    CASE_MATHFN (LOG)
    CASE_MATHFN (LOG10)
    CASE_MATHFN (LOG1P)
    CASE_MATHFN (LOG2)
    CASE_MATHFN (LOGB)
    CASE_MATHFN (LRINT)
    CASE_MATHFN (LROUND)
    CASE_MATHFN (MODF)
    CASE_MATHFN (NAN)
    CASE_MATHFN (NANS)
    CASE_MATHFN (NEARBYINT)
    CASE_MATHFN (NEXTAFTER)
    CASE_MATHFN (NEXTTOWARD)
    CASE_MATHFN (POW)
    CASE_MATHFN (POWI)
    CASE_MATHFN (POW10)
    CASE_MATHFN (REMAINDER)
    CASE_MATHFN (REMQUO)
    CASE_MATHFN (RINT)
    CASE_MATHFN (ROUND)
    CASE_MATHFN (SCALB)
    CASE_MATHFN (SCALBLN)
    CASE_MATHFN (SCALBN)
    CASE_MATHFN (SIGNBIT)
    CASE_MATHFN (SIGNIFICAND)
    CASE_MATHFN (SIN)
    CASE_MATHFN (SINCOS)
    CASE_MATHFN (SINH)
    CASE_MATHFN (SQRT)
    CASE_MATHFN (TAN)
    CASE_MATHFN (TANH)
    CASE_MATHFN (TGAMMA)
    CASE_MATHFN (TRUNC)
    CASE_MATHFN (Y0)
    CASE_MATHFN (Y1)
    CASE_MATHFN (YN)

    default:
      return END_BUILTINS;
    }

  if (TYPE_MAIN_VARIANT (type) == double_type_node)
    return fcode;
  else if (TYPE_MAIN_VARIANT (type) == float_type_node)
    return fcodef;
  else if (TYPE_MAIN_VARIANT (type) == long_double_type_node)
    return fcodel;
  else
    return END_BUILTINS;
}

/* Return mathematic function equivalent to FN but operating directly on TYPE,
   if available.  If IMPLICIT_P is true use the implicit builtin declaration,
   otherwise use the explicit declaration.  If we can't do the conversion,
   return null.  */

static tree
mathfn_built_in_1 (tree type, combined_fn fn, bool implicit_p)
{
  built_in_function fcode2 = mathfn_built_in_2 (type, fn);
  if (fcode2 == END_BUILTINS)
    return NULL_TREE;

  if (implicit_p && !builtin_decl_implicit_p (fcode2))
    return NULL_TREE;

  return builtin_decl_explicit (fcode2);
}

/* Like mathfn_built_in_1, but always use the implicit array.  */

tree
mathfn_built_in (tree type, combined_fn fn)
{
  return mathfn_built_in_1 (type, fn, /*implicit=*/ 1);
}

/* Like mathfn_built_in_1, but take a built_in_function and
   always use the implicit array.  */

tree
mathfn_built_in (tree type, enum built_in_function fn)
{
  return mathfn_built_in_1 (type, as_combined_fn (fn), /*implicit=*/ 1);
}

/* If BUILT_IN_NORMAL function FNDECL has an associated internal function,
   return its code, otherwise return IFN_LAST.  Note that this function
   only tests whether the function is defined in internals.def, not whether
   it is actually available on the target.  */

internal_fn
associated_internal_fn (tree fndecl)
{
  gcc_checking_assert (DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL);
  tree return_type = TREE_TYPE (TREE_TYPE (fndecl));
  switch (DECL_FUNCTION_CODE (fndecl))
    {
#define DEF_INTERNAL_FLT_FN(NAME, FLAGS, OPTAB, TYPE) \
    CASE_FLT_FN (BUILT_IN_##NAME): return IFN_##NAME;
#define DEF_INTERNAL_INT_FN(NAME, FLAGS, OPTAB, TYPE) \
    CASE_INT_FN (BUILT_IN_##NAME): return IFN_##NAME;
#include "internal-fn.def"

    CASE_FLT_FN (BUILT_IN_POW10):
      return IFN_EXP10;

    CASE_FLT_FN (BUILT_IN_DREM):
      return IFN_REMAINDER;

    CASE_FLT_FN (BUILT_IN_SCALBN):
    CASE_FLT_FN (BUILT_IN_SCALBLN):
      if (REAL_MODE_FORMAT (TYPE_MODE (return_type))->b == 2)
	return IFN_LDEXP;
      return IFN_LAST;

    default:
      return IFN_LAST;
    }
}

/* If CALL is a call to a BUILT_IN_NORMAL function that could be replaced
   on the current target by a call to an internal function, return the
   code of that internal function, otherwise return IFN_LAST.  The caller
   is responsible for ensuring that any side-effects of the built-in
   call are dealt with correctly.  E.g. if CALL sets errno, the caller
   must decide that the errno result isn't needed or make it available
   in some other way.  */

internal_fn
replacement_internal_fn (gcall *call)
{
  if (gimple_call_builtin_p (call, BUILT_IN_NORMAL))
    {
      internal_fn ifn = associated_internal_fn (gimple_call_fndecl (call));
      if (ifn != IFN_LAST)
	{
	  tree_pair types = direct_internal_fn_types (ifn, call);
	  optimization_type opt_type = bb_optimization_type (gimple_bb (call));
	  if (direct_internal_fn_supported_p (ifn, types, opt_type))
	    return ifn;
	}
    }
  return IFN_LAST;
}

/* Expand a call to the builtin trinary math functions (fma).
   Return NULL_RTX if a normal call should be emitted rather than expanding the
   function in-line.  EXP is the expression that is a call to the builtin
   function; if convenient, the result should be placed in TARGET.
   SUBTARGET may be used as the target for computing one of EXP's
   operands.  */

static rtx
expand_builtin_mathfn_ternary (tree exp, rtx target, rtx subtarget)
{
  optab builtin_optab;
  rtx op0, op1, op2, result;
  rtx_insn *insns;
  tree fndecl = get_callee_fndecl (exp);
  tree arg0, arg1, arg2;
  machine_mode mode;

  if (!validate_arglist (exp, REAL_TYPE, REAL_TYPE, REAL_TYPE, VOID_TYPE))
    return NULL_RTX;

  arg0 = CALL_EXPR_ARG (exp, 0);
  arg1 = CALL_EXPR_ARG (exp, 1);
  arg2 = CALL_EXPR_ARG (exp, 2);

  switch (DECL_FUNCTION_CODE (fndecl))
    {
    CASE_FLT_FN (BUILT_IN_FMA):
      builtin_optab = fma_optab; break;
    default:
      gcc_unreachable ();
    }

  /* Make a suitable register to place result in.  */
  mode = TYPE_MODE (TREE_TYPE (exp));

  /* Before working hard, check whether the instruction is available.  */
  if (optab_handler (builtin_optab, mode) == CODE_FOR_nothing)
    return NULL_RTX;

  result = gen_reg_rtx (mode);

  /* Always stabilize the argument list.  */
  CALL_EXPR_ARG (exp, 0) = arg0 = builtin_save_expr (arg0);
  CALL_EXPR_ARG (exp, 1) = arg1 = builtin_save_expr (arg1);
  CALL_EXPR_ARG (exp, 2) = arg2 = builtin_save_expr (arg2);

  op0 = expand_expr (arg0, subtarget, VOIDmode, EXPAND_NORMAL);
  op1 = expand_normal (arg1);
  op2 = expand_normal (arg2);

  start_sequence ();

  /* Compute into RESULT.
     Set RESULT to wherever the result comes back.  */
  result = expand_ternary_op (mode, builtin_optab, op0, op1, op2,
			      result, 0);

  /* If we were unable to expand via the builtin, stop the sequence
     (without outputting the insns) and call to the library function
     with the stabilized argument list.  */
  if (result == 0)
    {
      end_sequence ();
      return expand_call (exp, target, target == const0_rtx);
    }

  /* Output the entire sequence.  */
  insns = get_insns ();
  end_sequence ();
  emit_insn (insns);

  return result;
}

/* Expand a call to the builtin sin and cos math functions.
   Return NULL_RTX if a normal call should be emitted rather than expanding the
   function in-line.  EXP is the expression that is a call to the builtin
   function; if convenient, the result should be placed in TARGET.
   SUBTARGET may be used as the target for computing one of EXP's
   operands.  */

static rtx
expand_builtin_mathfn_3 (tree exp, rtx target, rtx subtarget)
{
  optab builtin_optab;
  rtx op0;
  rtx_insn *insns;
  tree fndecl = get_callee_fndecl (exp);
  machine_mode mode;
  tree arg;

  if (!validate_arglist (exp, REAL_TYPE, VOID_TYPE))
    return NULL_RTX;

  arg = CALL_EXPR_ARG (exp, 0);

  switch (DECL_FUNCTION_CODE (fndecl))
    {
    CASE_FLT_FN (BUILT_IN_SIN):
    CASE_FLT_FN (BUILT_IN_COS):
      builtin_optab = sincos_optab; break;
    default:
      gcc_unreachable ();
    }

  /* Make a suitable register to place result in.  */
  mode = TYPE_MODE (TREE_TYPE (exp));

  /* Check if sincos insn is available, otherwise fallback
     to sin or cos insn.  */
  if (optab_handler (builtin_optab, mode) == CODE_FOR_nothing)
    switch (DECL_FUNCTION_CODE (fndecl))
      {
      CASE_FLT_FN (BUILT_IN_SIN):
	builtin_optab = sin_optab; break;
      CASE_FLT_FN (BUILT_IN_COS):
	builtin_optab = cos_optab; break;
      default:
	gcc_unreachable ();
      }

  /* Before working hard, check whether the instruction is available.  */
  if (optab_handler (builtin_optab, mode) != CODE_FOR_nothing)
    {
      rtx result = gen_reg_rtx (mode);

      /* Wrap the computation of the argument in a SAVE_EXPR, as we may
	 need to expand the argument again.  This way, we will not perform
	 side-effects more the once.  */
      CALL_EXPR_ARG (exp, 0) = arg = builtin_save_expr (arg);

      op0 = expand_expr (arg, subtarget, VOIDmode, EXPAND_NORMAL);

      start_sequence ();

      /* Compute into RESULT.
	 Set RESULT to wherever the result comes back.  */
      if (builtin_optab == sincos_optab)
	{
	  int ok;

	  switch (DECL_FUNCTION_CODE (fndecl))
	    {
	    CASE_FLT_FN (BUILT_IN_SIN):
	      ok = expand_twoval_unop (builtin_optab, op0, 0, result, 0);
	      break;
	    CASE_FLT_FN (BUILT_IN_COS):
	      ok = expand_twoval_unop (builtin_optab, op0, result, 0, 0);
	      break;
	    default:
	      gcc_unreachable ();
	    }
	  gcc_assert (ok);
	}
      else
	result = expand_unop (mode, builtin_optab, op0, result, 0);

      if (result != 0)
	{
	  /* Output the entire sequence.  */
	  insns = get_insns ();
	  end_sequence ();
	  emit_insn (insns);
	  return result;
	}

      /* If we were unable to expand via the builtin, stop the sequence
	 (without outputting the insns) and call to the library function
	 with the stabilized argument list.  */
      end_sequence ();
    }

  return expand_call (exp, target, target == const0_rtx);
}

/* Given an interclass math builtin decl FNDECL and it's argument ARG
   return an RTL instruction code that implements the functionality.
   If that isn't possible or available return CODE_FOR_nothing.  */

static enum insn_code
interclass_mathfn_icode (tree arg, tree fndecl)
{
  bool errno_set = false;
  optab builtin_optab = unknown_optab;
  machine_mode mode;

  switch (DECL_FUNCTION_CODE (fndecl))
    {
    CASE_FLT_FN (BUILT_IN_ILOGB):
      errno_set = true; builtin_optab = ilogb_optab; break;
    CASE_FLT_FN (BUILT_IN_ISINF):
      builtin_optab = isinf_optab; break;
    case BUILT_IN_ISNORMAL:
    case BUILT_IN_ISFINITE:
    CASE_FLT_FN (BUILT_IN_FINITE):
    case BUILT_IN_FINITED32:
    case BUILT_IN_FINITED64:
    case BUILT_IN_FINITED128:
    case BUILT_IN_ISINFD32:
    case BUILT_IN_ISINFD64:
    case BUILT_IN_ISINFD128:
      /* These builtins have no optabs (yet).  */
      break;
    default:
      gcc_unreachable ();
    }

  /* There's no easy way to detect the case we need to set EDOM.  */
  if (flag_errno_math && errno_set)
    return CODE_FOR_nothing;

  /* Optab mode depends on the mode of the input argument.  */
  mode = TYPE_MODE (TREE_TYPE (arg));

  if (builtin_optab)
    return optab_handler (builtin_optab, mode);
  return CODE_FOR_nothing;
}

/* Expand a call to one of the builtin math functions that operate on
   floating point argument and output an integer result (ilogb, isinf,
   isnan, etc).
   Return 0 if a normal call should be emitted rather than expanding the
   function in-line.  EXP is the expression that is a call to the builtin
   function; if convenient, the result should be placed in TARGET.  */

static rtx
expand_builtin_interclass_mathfn (tree exp, rtx target)
{
  enum insn_code icode = CODE_FOR_nothing;
  rtx op0;
  tree fndecl = get_callee_fndecl (exp);
  machine_mode mode;
  tree arg;

  if (!validate_arglist (exp, REAL_TYPE, VOID_TYPE))
    return NULL_RTX;

  arg = CALL_EXPR_ARG (exp, 0);
  icode = interclass_mathfn_icode (arg, fndecl);
  mode = TYPE_MODE (TREE_TYPE (arg));

  if (icode != CODE_FOR_nothing)
    {
      struct expand_operand ops[1];
      rtx_insn *last = get_last_insn ();
      tree orig_arg = arg;

      /* Wrap the computation of the argument in a SAVE_EXPR, as we may
	 need to expand the argument again.  This way, we will not perform
	 side-effects more the once.  */
      CALL_EXPR_ARG (exp, 0) = arg = builtin_save_expr (arg);

      op0 = expand_expr (arg, NULL_RTX, VOIDmode, EXPAND_NORMAL);

      if (mode != GET_MODE (op0))
	op0 = convert_to_mode (mode, op0, 0);

      create_output_operand (&ops[0], target, TYPE_MODE (TREE_TYPE (exp)));
      if (maybe_legitimize_operands (icode, 0, 1, ops)
	  && maybe_emit_unop_insn (icode, ops[0].value, op0, UNKNOWN))
	return ops[0].value;

      delete_insns_since (last);
      CALL_EXPR_ARG (exp, 0) = orig_arg;
    }

  return NULL_RTX;
}

/* Expand a call to the builtin sincos math function.
   Return NULL_RTX if a normal call should be emitted rather than expanding the
   function in-line.  EXP is the expression that is a call to the builtin
   function.  */

static rtx
expand_builtin_sincos (tree exp)
{
  rtx op0, op1, op2, target1, target2;
  machine_mode mode;
  tree arg, sinp, cosp;
  int result;
  location_t loc = EXPR_LOCATION (exp);
  tree alias_type, alias_off;

  if (!validate_arglist (exp, REAL_TYPE,
 			 POINTER_TYPE, POINTER_TYPE, VOID_TYPE))
    return NULL_RTX;

  arg = CALL_EXPR_ARG (exp, 0);
  sinp = CALL_EXPR_ARG (exp, 1);
  cosp = CALL_EXPR_ARG (exp, 2);

  /* Make a suitable register to place result in.  */
  mode = TYPE_MODE (TREE_TYPE (arg));

  /* Check if sincos insn is available, otherwise emit the call.  */
  if (optab_handler (sincos_optab, mode) == CODE_FOR_nothing)
    return NULL_RTX;

  target1 = gen_reg_rtx (mode);
  target2 = gen_reg_rtx (mode);

  op0 = expand_normal (arg);
  alias_type = build_pointer_type_for_mode (TREE_TYPE (arg), ptr_mode, true);
  alias_off = build_int_cst (alias_type, 0);
  op1 = expand_normal (fold_build2_loc (loc, MEM_REF, TREE_TYPE (arg),
					sinp, alias_off));
  op2 = expand_normal (fold_build2_loc (loc, MEM_REF, TREE_TYPE (arg),
					cosp, alias_off));

  /* Compute into target1 and target2.
     Set TARGET to wherever the result comes back.  */
  result = expand_twoval_unop (sincos_optab, op0, target2, target1, 0);
  gcc_assert (result);

  /* Move target1 and target2 to the memory locations indicated
     by op1 and op2.  */
  emit_move_insn (op1, target1);
  emit_move_insn (op2, target2);

  return const0_rtx;
}

/* Expand a call to the internal cexpi builtin to the sincos math function.
   EXP is the expression that is a call to the builtin function; if convenient,
   the result should be placed in TARGET.  */

static rtx
expand_builtin_cexpi (tree exp, rtx target)
{
  tree fndecl = get_callee_fndecl (exp);
  tree arg, type;
  machine_mode mode;
  rtx op0, op1, op2;
  location_t loc = EXPR_LOCATION (exp);

  if (!validate_arglist (exp, REAL_TYPE, VOID_TYPE))
    return NULL_RTX;

  arg = CALL_EXPR_ARG (exp, 0);
  type = TREE_TYPE (arg);
  mode = TYPE_MODE (TREE_TYPE (arg));

  /* Try expanding via a sincos optab, fall back to emitting a libcall
     to sincos or cexp.  We are sure we have sincos or cexp because cexpi
     is only generated from sincos, cexp or if we have either of them.  */
  if (optab_handler (sincos_optab, mode) != CODE_FOR_nothing)
    {
      op1 = gen_reg_rtx (mode);
      op2 = gen_reg_rtx (mode);

      op0 = expand_expr (arg, NULL_RTX, VOIDmode, EXPAND_NORMAL);

      /* Compute into op1 and op2.  */
      expand_twoval_unop (sincos_optab, op0, op2, op1, 0);
    }
  else if (targetm.libc_has_function (function_sincos))
    {
      tree call, fn = NULL_TREE;
      tree top1, top2;
      rtx op1a, op2a;

      if (DECL_FUNCTION_CODE (fndecl) == BUILT_IN_CEXPIF)
	fn = builtin_decl_explicit (BUILT_IN_SINCOSF);
      else if (DECL_FUNCTION_CODE (fndecl) == BUILT_IN_CEXPI)
	fn = builtin_decl_explicit (BUILT_IN_SINCOS);
      else if (DECL_FUNCTION_CODE (fndecl) == BUILT_IN_CEXPIL)
	fn = builtin_decl_explicit (BUILT_IN_SINCOSL);
      else
	gcc_unreachable ();

      op1 = assign_temp (TREE_TYPE (arg), 1, 1);
      op2 = assign_temp (TREE_TYPE (arg), 1, 1);
      op1a = copy_addr_to_reg (XEXP (op1, 0));
      op2a = copy_addr_to_reg (XEXP (op2, 0));
      top1 = make_tree (build_pointer_type (TREE_TYPE (arg)), op1a);
      top2 = make_tree (build_pointer_type (TREE_TYPE (arg)), op2a);

      /* Make sure not to fold the sincos call again.  */
      call = build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (fn)), fn);
      expand_normal (build_call_nary (TREE_TYPE (TREE_TYPE (fn)),
				      call, 3, arg, top1, top2));
    }
  else
    {
      tree call, fn = NULL_TREE, narg;
      tree ctype = build_complex_type (type);

      if (DECL_FUNCTION_CODE (fndecl) == BUILT_IN_CEXPIF)
	fn = builtin_decl_explicit (BUILT_IN_CEXPF);
      else if (DECL_FUNCTION_CODE (fndecl) == BUILT_IN_CEXPI)
	fn = builtin_decl_explicit (BUILT_IN_CEXP);
      else if (DECL_FUNCTION_CODE (fndecl) == BUILT_IN_CEXPIL)
	fn = builtin_decl_explicit (BUILT_IN_CEXPL);
      else
	gcc_unreachable ();

      /* If we don't have a decl for cexp create one.  This is the
	 friendliest fallback if the user calls __builtin_cexpi
	 without full target C99 function support.  */
      if (fn == NULL_TREE)
	{
	  tree fntype;
	  const char *name = NULL;

	  if (DECL_FUNCTION_CODE (fndecl) == BUILT_IN_CEXPIF)
	    name = "cexpf";
	  else if (DECL_FUNCTION_CODE (fndecl) == BUILT_IN_CEXPI)
	    name = "cexp";
	  else if (DECL_FUNCTION_CODE (fndecl) == BUILT_IN_CEXPIL)
	    name = "cexpl";

	  fntype = build_function_type_list (ctype, ctype, NULL_TREE);
	  fn = build_fn_decl (name, fntype);
	}

      narg = fold_build2_loc (loc, COMPLEX_EXPR, ctype,
			  build_real (type, dconst0), arg);

      /* Make sure not to fold the cexp call again.  */
      call = build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (fn)), fn);
      return expand_expr (build_call_nary (ctype, call, 1, narg),
			  target, VOIDmode, EXPAND_NORMAL);
    }

  /* Now build the proper return type.  */
  return expand_expr (build2 (COMPLEX_EXPR, build_complex_type (type),
			      make_tree (TREE_TYPE (arg), op2),
			      make_tree (TREE_TYPE (arg), op1)),
		      target, VOIDmode, EXPAND_NORMAL);
}

/* Conveniently construct a function call expression.  FNDECL names the
   function to be called, N is the number of arguments, and the "..."
   parameters are the argument expressions.  Unlike build_call_exr
   this doesn't fold the call, hence it will always return a CALL_EXPR.  */

static tree
build_call_nofold_loc (location_t loc, tree fndecl, int n, ...)
{
  va_list ap;
  tree fntype = TREE_TYPE (fndecl);
  tree fn = build1 (ADDR_EXPR, build_pointer_type (fntype), fndecl);

  va_start (ap, n);
  fn = build_call_valist (TREE_TYPE (fntype), fn, n, ap);
  va_end (ap);
  SET_EXPR_LOCATION (fn, loc);
  return fn;
}

/* Expand a call to one of the builtin rounding functions gcc defines
   as an extension (lfloor and lceil).  As these are gcc extensions we
   do not need to worry about setting errno to EDOM.
   If expanding via optab fails, lower expression to (int)(floor(x)).
   EXP is the expression that is a call to the builtin function;
   if convenient, the result should be placed in TARGET.  */

static rtx
expand_builtin_int_roundingfn (tree exp, rtx target)
{
  convert_optab builtin_optab;
  rtx op0, tmp;
  rtx_insn *insns;
  tree fndecl = get_callee_fndecl (exp);
  enum built_in_function fallback_fn;
  tree fallback_fndecl;
  machine_mode mode;
  tree arg;

  if (!validate_arglist (exp, REAL_TYPE, VOID_TYPE))
    gcc_unreachable ();

  arg = CALL_EXPR_ARG (exp, 0);

  switch (DECL_FUNCTION_CODE (fndecl))
    {
    CASE_FLT_FN (BUILT_IN_ICEIL):
    CASE_FLT_FN (BUILT_IN_LCEIL):
    CASE_FLT_FN (BUILT_IN_LLCEIL):
      builtin_optab = lceil_optab;
      fallback_fn = BUILT_IN_CEIL;
      break;

    CASE_FLT_FN (BUILT_IN_IFLOOR):
    CASE_FLT_FN (BUILT_IN_LFLOOR):
    CASE_FLT_FN (BUILT_IN_LLFLOOR):
      builtin_optab = lfloor_optab;
      fallback_fn = BUILT_IN_FLOOR;
      break;

    default:
      gcc_unreachable ();
    }

  /* Make a suitable register to place result in.  */
  mode = TYPE_MODE (TREE_TYPE (exp));

  target = gen_reg_rtx (mode);

  /* Wrap the computation of the argument in a SAVE_EXPR, as we may
     need to expand the argument again.  This way, we will not perform
     side-effects more the once.  */
  CALL_EXPR_ARG (exp, 0) = arg = builtin_save_expr (arg);

  op0 = expand_expr (arg, NULL, VOIDmode, EXPAND_NORMAL);

  start_sequence ();

  /* Compute into TARGET.  */
  if (expand_sfix_optab (target, op0, builtin_optab))
    {
      /* Output the entire sequence.  */
      insns = get_insns ();
      end_sequence ();
      emit_insn (insns);
      return target;
    }

  /* If we were unable to expand via the builtin, stop the sequence
     (without outputting the insns).  */
  end_sequence ();

  /* Fall back to floating point rounding optab.  */
  fallback_fndecl = mathfn_built_in (TREE_TYPE (arg), fallback_fn);

  /* For non-C99 targets we may end up without a fallback fndecl here
     if the user called __builtin_lfloor directly.  In this case emit
     a call to the floor/ceil variants nevertheless.  This should result
     in the best user experience for not full C99 targets.  */
  if (fallback_fndecl == NULL_TREE)
    {
      tree fntype;
      const char *name = NULL;

      switch (DECL_FUNCTION_CODE (fndecl))
	{
	case BUILT_IN_ICEIL:
	case BUILT_IN_LCEIL:
	case BUILT_IN_LLCEIL:
	  name = "ceil";
	  break;
	case BUILT_IN_ICEILF:
	case BUILT_IN_LCEILF:
	case BUILT_IN_LLCEILF:
	  name = "ceilf";
	  break;
	case BUILT_IN_ICEILL:
	case BUILT_IN_LCEILL:
	case BUILT_IN_LLCEILL:
	  name = "ceill";
	  break;
	case BUILT_IN_IFLOOR:
	case BUILT_IN_LFLOOR:
	case BUILT_IN_LLFLOOR:
	  name = "floor";
	  break;
	case BUILT_IN_IFLOORF:
	case BUILT_IN_LFLOORF:
	case BUILT_IN_LLFLOORF:
	  name = "floorf";
	  break;
	case BUILT_IN_IFLOORL:
	case BUILT_IN_LFLOORL:
	case BUILT_IN_LLFLOORL:
	  name = "floorl";
	  break;
	default:
	  gcc_unreachable ();
	}

      fntype = build_function_type_list (TREE_TYPE (arg),
					 TREE_TYPE (arg), NULL_TREE);
      fallback_fndecl = build_fn_decl (name, fntype);
    }

  exp = build_call_nofold_loc (EXPR_LOCATION (exp), fallback_fndecl, 1, arg);

  tmp = expand_normal (exp);
  tmp = maybe_emit_group_store (tmp, TREE_TYPE (exp));

  /* Truncate the result of floating point optab to integer
     via expand_fix ().  */
  target = gen_reg_rtx (mode);
  expand_fix (target, tmp, 0);

  return target;
}

/* Expand a call to one of the builtin math functions doing integer
   conversion (lrint).
   Return 0 if a normal call should be emitted rather than expanding the
   function in-line.  EXP is the expression that is a call to the builtin
   function; if convenient, the result should be placed in TARGET.  */

static rtx
expand_builtin_int_roundingfn_2 (tree exp, rtx target)
{
  convert_optab builtin_optab;
  rtx op0;
  rtx_insn *insns;
  tree fndecl = get_callee_fndecl (exp);
  tree arg;
  machine_mode mode;
  enum built_in_function fallback_fn = BUILT_IN_NONE;

  if (!validate_arglist (exp, REAL_TYPE, VOID_TYPE))
     gcc_unreachable ();

  arg = CALL_EXPR_ARG (exp, 0);

  switch (DECL_FUNCTION_CODE (fndecl))
    {
    CASE_FLT_FN (BUILT_IN_IRINT):
      fallback_fn = BUILT_IN_LRINT;
      gcc_fallthrough ();
    CASE_FLT_FN (BUILT_IN_LRINT):
    CASE_FLT_FN (BUILT_IN_LLRINT):
      builtin_optab = lrint_optab;
      break;

    CASE_FLT_FN (BUILT_IN_IROUND):
      fallback_fn = BUILT_IN_LROUND;
      gcc_fallthrough ();
    CASE_FLT_FN (BUILT_IN_LROUND):
    CASE_FLT_FN (BUILT_IN_LLROUND):
      builtin_optab = lround_optab;
      break;

    default:
      gcc_unreachable ();
    }

  /* There's no easy way to detect the case we need to set EDOM.  */
  if (flag_errno_math && fallback_fn == BUILT_IN_NONE)
    return NULL_RTX;

  /* Make a suitable register to place result in.  */
  mode = TYPE_MODE (TREE_TYPE (exp));

  /* There's no easy way to detect the case we need to set EDOM.  */
  if (!flag_errno_math)
    {
      rtx result = gen_reg_rtx (mode);

      /* Wrap the computation of the argument in a SAVE_EXPR, as we may
	 need to expand the argument again.  This way, we will not perform
	 side-effects more the once.  */
      CALL_EXPR_ARG (exp, 0) = arg = builtin_save_expr (arg);

      op0 = expand_expr (arg, NULL, VOIDmode, EXPAND_NORMAL);

      start_sequence ();

      if (expand_sfix_optab (result, op0, builtin_optab))
	{
	  /* Output the entire sequence.  */
	  insns = get_insns ();
	  end_sequence ();
	  emit_insn (insns);
	  return result;
	}

      /* If we were unable to expand via the builtin, stop the sequence
	 (without outputting the insns) and call to the library function
	 with the stabilized argument list.  */
      end_sequence ();
    }

  if (fallback_fn != BUILT_IN_NONE)
    {
      /* Fall back to rounding to long int.  Use implicit_p 0 - for non-C99
	 targets, (int) round (x) should never be transformed into
	 BUILT_IN_IROUND and if __builtin_iround is called directly, emit
	 a call to lround in the hope that the target provides at least some
	 C99 functions.  This should result in the best user experience for
	 not full C99 targets.  */
      tree fallback_fndecl = mathfn_built_in_1
	(TREE_TYPE (arg), as_combined_fn (fallback_fn), 0);

      exp = build_call_nofold_loc (EXPR_LOCATION (exp),
				   fallback_fndecl, 1, arg);

      target = expand_call (exp, NULL_RTX, target == const0_rtx);
      target = maybe_emit_group_store (target, TREE_TYPE (exp));
      return convert_to_mode (mode, target, 0);
    }

  return expand_call (exp, target, target == const0_rtx);
}

/* Expand a call to the powi built-in mathematical function.  Return NULL_RTX if
   a normal call should be emitted rather than expanding the function
   in-line.  EXP is the expression that is a call to the builtin
   function; if convenient, the result should be placed in TARGET.  */

static rtx
expand_builtin_powi (tree exp, rtx target)
{
  tree arg0, arg1;
  rtx op0, op1;
  machine_mode mode;
  machine_mode mode2;

  if (! validate_arglist (exp, REAL_TYPE, INTEGER_TYPE, VOID_TYPE))
    return NULL_RTX;

  arg0 = CALL_EXPR_ARG (exp, 0);
  arg1 = CALL_EXPR_ARG (exp, 1);
  mode = TYPE_MODE (TREE_TYPE (exp));

  /* Emit a libcall to libgcc.  */

  /* Mode of the 2nd argument must match that of an int.  */
  mode2 = mode_for_size (INT_TYPE_SIZE, MODE_INT, 0);

  if (target == NULL_RTX)
    target = gen_reg_rtx (mode);

  op0 = expand_expr (arg0, NULL_RTX, mode, EXPAND_NORMAL);
  if (GET_MODE (op0) != mode)
    op0 = convert_to_mode (mode, op0, 0);
  op1 = expand_expr (arg1, NULL_RTX, mode2, EXPAND_NORMAL);
  if (GET_MODE (op1) != mode2)
    op1 = convert_to_mode (mode2, op1, 0);

  target = emit_library_call_value (optab_libfunc (powi_optab, mode),
				    target, LCT_CONST, mode, 2,
				    op0, mode, op1, mode2);

  return target;
}

/* Expand expression EXP which is a call to the strlen builtin.  Return
   NULL_RTX if we failed the caller should emit a normal call, otherwise
   try to get the result in TARGET, if convenient.  */

static rtx
expand_builtin_strlen (tree exp, rtx target,
		       machine_mode target_mode)
{
  if (!validate_arglist (exp, POINTER_TYPE, VOID_TYPE))
    return NULL_RTX;
  else
    {
      struct expand_operand ops[4];
      rtx pat;
      tree len;
      tree src = CALL_EXPR_ARG (exp, 0);
      rtx src_reg;
      rtx_insn *before_strlen;
      machine_mode insn_mode;
      enum insn_code icode = CODE_FOR_nothing;
      unsigned int align;

      /* If the length can be computed at compile-time, return it.  */
      len = c_strlen (src, 0);
      if (len)
	return expand_expr (len, target, target_mode, EXPAND_NORMAL);

      /* If the length can be computed at compile-time and is constant
	 integer, but there are side-effects in src, evaluate
	 src for side-effects, then return len.
	 E.g. x = strlen (i++ ? "xfoo" + 1 : "bar");
	 can be optimized into: i++; x = 3;  */
      len = c_strlen (src, 1);
      if (len && TREE_CODE (len) == INTEGER_CST)
	{
	  expand_expr (src, const0_rtx, VOIDmode, EXPAND_NORMAL);
	  return expand_expr (len, target, target_mode, EXPAND_NORMAL);
	}

      align = get_pointer_alignment (src) / BITS_PER_UNIT;

      /* If SRC is not a pointer type, don't do this operation inline.  */
      if (align == 0)
	return NULL_RTX;

      /* Bail out if we can't compute strlen in the right mode.  */
      FOR_EACH_MODE_FROM (insn_mode, target_mode)
	{
	  icode = optab_handler (strlen_optab, insn_mode);
	  if (icode != CODE_FOR_nothing)
	    break;
	}
      if (insn_mode == VOIDmode)
	return NULL_RTX;

      /* Make a place to hold the source address.  We will not expand
	 the actual source until we are sure that the expansion will
	 not fail -- there are trees that cannot be expanded twice.  */
      src_reg = gen_reg_rtx (Pmode);

      /* Mark the beginning of the strlen sequence so we can emit the
	 source operand later.  */
      before_strlen = get_last_insn ();

      create_output_operand (&ops[0], target, insn_mode);
      create_fixed_operand (&ops[1], gen_rtx_MEM (BLKmode, src_reg));
      create_integer_operand (&ops[2], 0);
      create_integer_operand (&ops[3], align);
      if (!maybe_expand_insn (icode, 4, ops))
	return NULL_RTX;

      /* Now that we are assured of success, expand the source.  */
      start_sequence ();
      pat = expand_expr (src, src_reg, Pmode, EXPAND_NORMAL);
      if (pat != src_reg)
	{
#ifdef POINTERS_EXTEND_UNSIGNED
	  if (GET_MODE (pat) != Pmode)
	    pat = convert_to_mode (Pmode, pat,
				   POINTERS_EXTEND_UNSIGNED);
#endif
	  emit_move_insn (src_reg, pat);
	}
      pat = get_insns ();
      end_sequence ();

      if (before_strlen)
	emit_insn_after (pat, before_strlen);
      else
	emit_insn_before (pat, get_insns ());

      /* Return the value in the proper mode for this function.  */
      if (GET_MODE (ops[0].value) == target_mode)
	target = ops[0].value;
      else if (target != 0)
	convert_move (target, ops[0].value, 0);
      else
	target = convert_to_mode (target_mode, ops[0].value, 0);

      return target;
    }
}

/* Callback routine for store_by_pieces.  Read GET_MODE_BITSIZE (MODE)
   bytes from constant string DATA + OFFSET and return it as target
   constant.  */

static rtx
builtin_memcpy_read_str (void *data, HOST_WIDE_INT offset,
			 machine_mode mode)
{
  const char *str = (const char *) data;

  gcc_assert (offset >= 0
	      && ((unsigned HOST_WIDE_INT) offset + GET_MODE_SIZE (mode)
		  <= strlen (str) + 1));

  return c_readstr (str + offset, mode);
}

/* LEN specify length of the block of memcpy/memset operation.
   Figure out its range and put it into MIN_SIZE/MAX_SIZE. 
   In some cases we can make very likely guess on max size, then we
   set it into PROBABLE_MAX_SIZE.  */

static void
determine_block_size (tree len, rtx len_rtx,
		      unsigned HOST_WIDE_INT *min_size,
		      unsigned HOST_WIDE_INT *max_size,
		      unsigned HOST_WIDE_INT *probable_max_size)
{
  if (CONST_INT_P (len_rtx))
    {
      *min_size = *max_size = *probable_max_size = UINTVAL (len_rtx);
      return;
    }
  else
    {
      wide_int min, max;
      enum value_range_type range_type = VR_UNDEFINED;

      /* Determine bounds from the type.  */
      if (tree_fits_uhwi_p (TYPE_MIN_VALUE (TREE_TYPE (len))))
	*min_size = tree_to_uhwi (TYPE_MIN_VALUE (TREE_TYPE (len)));
      else
	*min_size = 0;
      if (tree_fits_uhwi_p (TYPE_MAX_VALUE (TREE_TYPE (len))))
	*probable_max_size = *max_size
	  = tree_to_uhwi (TYPE_MAX_VALUE (TREE_TYPE (len)));
      else
	*probable_max_size = *max_size = GET_MODE_MASK (GET_MODE (len_rtx));

      if (TREE_CODE (len) == SSA_NAME)
	range_type = get_range_info (len, &min, &max);
      if (range_type == VR_RANGE)
	{
	  if (wi::fits_uhwi_p (min) && *min_size < min.to_uhwi ())
	    *min_size = min.to_uhwi ();
	  if (wi::fits_uhwi_p (max) && *max_size > max.to_uhwi ())
	    *probable_max_size = *max_size = max.to_uhwi ();
	}
      else if (range_type == VR_ANTI_RANGE)
	{
	  /* Anti range 0...N lets us to determine minimal size to N+1.  */
	  if (min == 0)
	    {
	      if (wi::fits_uhwi_p (max) && max.to_uhwi () + 1 != 0)
		*min_size = max.to_uhwi () + 1;
	    }
	  /* Code like

	     int n;
	     if (n < 100)
	       memcpy (a, b, n)

	     Produce anti range allowing negative values of N.  We still
	     can use the information and make a guess that N is not negative.
	     */
	  else if (!wi::leu_p (max, 1 << 30) && wi::fits_uhwi_p (min))
	    *probable_max_size = min.to_uhwi () - 1;
	}
    }
  gcc_checking_assert (*max_size <=
		       (unsigned HOST_WIDE_INT)
			  GET_MODE_MASK (GET_MODE (len_rtx)));
}

/* Try to verify that the sizes and lengths of the arguments to a string
   manipulation function given by EXP are within valid bounds and that
   the operation does not lead to buffer overflow.  Arguments other than
   EXP may be null.  When non-null, the arguments have the following
   meaning:
   SIZE is the user-supplied size argument to the function (such as in
   memcpy(d, s, SIZE) or strncpy(d, s, SIZE).  It specifies the exact
   number of bytes to write.
   MAXLEN is the user-supplied bound on the length of the source sequence
   (such as in strncat(d, s, N).  It specifies the upper limit on the number
   of bytes to write.
   SRC is the source string (such as in strcpy(d, s)) when the expression
   EXP is a string function call (as opposed to a memory call like memcpy).
   As an exception, SRC can also be an integer denoting the precomputed
   size of the source string or object (for functions like memcpy).
   OBJSIZE is the size of the destination object specified by the last
   argument to the _chk builtins, typically resulting from the expansion
   of __builtin_object_size (such as in __builtin___strcpy_chk(d, s,
   OBJSIZE).

   When SIZE is null LEN is checked to verify that it doesn't exceed
   SIZE_MAX.

   If the call is successfully verified as safe from buffer overflow
   the function returns true, otherwise false..  */

static bool
check_sizes (int opt, tree exp, tree size, tree maxlen, tree src, tree objsize)
{
  /* The size of the largest object is half the address space, or
     SSIZE_MAX.  (This is way too permissive.)  */
  tree maxobjsize = TYPE_MAX_VALUE (ssizetype);

  tree slen = NULL_TREE;

  tree range[2] = { NULL_TREE, NULL_TREE };

  /* Set to true when the exact number of bytes written by a string
     function like strcpy is not known and the only thing that is
     known is that it must be at least one (for the terminating nul).  */
  bool at_least_one = false;
  if (src)
    {
      /* SRC is normally a pointer to string but as a special case
	 it can be an integer denoting the length of a string.  */
      if (POINTER_TYPE_P (TREE_TYPE (src)))
	{
	  /* Try to determine the range of lengths the source string
	     refers to.  If it can be determined and is less than
	     the upper bound given by MAXLEN add one to it for
	     the terminating nul.  Otherwise, set it to one for
	     the same reason, or to MAXLEN as appropriate.  */
	  get_range_strlen (src, range);
	  if (range[0] && (!maxlen || TREE_CODE (maxlen) == INTEGER_CST))
	    {
	      if (maxlen && tree_int_cst_le (maxlen, range[0]))
		range[0] = range[1] = maxlen;
	      else
		range[0] = fold_build2 (PLUS_EXPR, size_type_node,
					range[0], size_one_node);

	      if (maxlen && tree_int_cst_le (maxlen, range[1]))
		range[1] = maxlen;
	      else if (!integer_all_onesp (range[1]))
		range[1] = fold_build2 (PLUS_EXPR, size_type_node,
					range[1], size_one_node);

	      slen = range[0];
	    }
	  else
	    {
	      at_least_one = true;
	      slen = size_one_node;
	    }
	}
      else
	slen = src;
    }

  if (!size && !maxlen)
    {
      /* When the only available piece of data is the object size
	 there is nothing to do.  */
      if (!slen)
	return true;

      /* Otherwise, when the length of the source sequence is known
	 (as with with strlen), set SIZE to it.  */
      if (!range[0])
	size = slen;
    }

  if (!objsize)
    objsize = maxobjsize;

  /* The SIZE is exact if it's non-null, constant, and in range of
     unsigned HOST_WIDE_INT.  */
  bool exactsize = size && tree_fits_uhwi_p (size);

  if (size)
    get_size_range (size, range);

  /* First check the number of bytes to be written against the maximum
     object size.  */
  if (range[0] && tree_int_cst_lt (maxobjsize, range[0]))
    {
      location_t loc = tree_nonartificial_location (exp);
      loc = expansion_point_location_if_in_system_header (loc);

      if (range[0] == range[1])
	warning_at (loc, opt,
		    "%K%qD specified size %E "
		    "exceeds maximum object size %E",
		    exp, get_callee_fndecl (exp), range[0], maxobjsize);
	  else
	    warning_at (loc, opt,
			"%K%qD specified size between %E and %E "
			"exceeds maximum object size %E",
			exp, get_callee_fndecl (exp),
			range[0], range[1], maxobjsize);
      return false;
    }

  /* Next check the number of bytes to be written against the destination
     object size.  */
  if (range[0] || !exactsize || integer_all_onesp (size))
    {
      if (range[0]
	  && ((tree_fits_uhwi_p (objsize)
	       && tree_int_cst_lt (objsize, range[0]))
	      || (tree_fits_uhwi_p (size)
		  && tree_int_cst_lt (size, range[0]))))
	{
	  location_t loc = tree_nonartificial_location (exp);
	  loc = expansion_point_location_if_in_system_header (loc);

	  if (size == slen && at_least_one)
	    {
	      /* This is a call to strcpy with a destination of 0 size
		 and a source of unknown length.  The call will write
		 at least one byte past the end of the destination.  */
	      warning_at (loc, opt,
			  "%K%qD writing %E or more bytes into a region "
			  "of size %E overflows the destination",
			  exp, get_callee_fndecl (exp), range[0], objsize);
	    }
	  else if (tree_int_cst_equal (range[0], range[1]))
	    warning_at (loc, opt,
			(integer_onep (range[0])
			 ? G_("%K%qD writing %E byte into a region "
			      "of size %E overflows the destination")
			 : G_("%K%qD writing %E bytes into a region "
			      "of size %E overflows the destination")),
			exp, get_callee_fndecl (exp), range[0], objsize);
	  else if (tree_int_cst_sign_bit (range[1]))
	    {
	      /* Avoid printing the upper bound if it's invalid.  */
	      warning_at (loc, opt,
			  "%K%qD writing %E or more bytes into a region "
			  "of size %E overflows the destination",
			  exp, get_callee_fndecl (exp), range[0], objsize);
	    }
	  else
	    warning_at (loc, opt,
			"%K%qD writing between %E and %E bytes into "
			"a region of size %E overflows the destination",
			exp, get_callee_fndecl (exp), range[0],	range[1],
			objsize);

	  /* Return error when an overflow has been detected.  */
	  return false;
	}
    }

  /* Check the maximum length of the source sequence against the size
     of the destination object if known, or against the maximum size
     of an object.  */
  if (maxlen)
    {
      get_size_range (maxlen, range);

      if (range[0] && objsize && tree_fits_uhwi_p (objsize))
	{
	  location_t loc = tree_nonartificial_location (exp);
	  loc = expansion_point_location_if_in_system_header (loc);

	  if (tree_int_cst_lt (maxobjsize, range[0]))
	    {
	      /* Warn about crazy big sizes first since that's more
		 likely to be meaningful than saying that the bound
		 is greater than the object size if both are big.  */
	      if (range[0] == range[1])
		warning_at (loc, opt,
			    "%K%qD specified bound %E "
			    "exceeds maximum object size %E",
			    exp, get_callee_fndecl (exp),
			    range[0], maxobjsize);
	      else
		warning_at (loc, opt,
			    "%K%qD specified bound between %E and %E "
			    "exceeds maximum object size %E",
			    exp, get_callee_fndecl (exp),
			    range[0], range[1], maxobjsize);

	      return false;
	    }

	  if (objsize != maxobjsize && tree_int_cst_lt (objsize, range[0]))
	    {
	      if (tree_int_cst_equal (range[0], range[1]))
		warning_at (loc, opt,
			    "%K%qD specified bound %E "
			    "exceeds destination size %E",
			    exp, get_callee_fndecl (exp),
			    range[0], objsize);
	      else
		warning_at (loc, opt,
			    "%K%qD specified bound between %E and %E "
			    "exceeds destination size %E",
			    exp, get_callee_fndecl (exp),
			    range[0], range[1], objsize);
	      return false;
	    }
	}
    }

  if (slen
      && slen == src
      && size && range[0]
      && tree_int_cst_lt (slen, range[0]))
    {
      location_t loc = tree_nonartificial_location (exp);

      if (tree_int_cst_equal (range[0], range[1]))
	warning_at (loc, opt,
		    (tree_int_cst_equal (range[0], integer_one_node)
		     ? G_("%K%qD reading %E byte from a region of size %E")
		     : G_("%K%qD reading %E bytes from a region of size %E")),
		    exp, get_callee_fndecl (exp), range[0], slen);
      else if (tree_int_cst_sign_bit (range[1]))
	{
	  /* Avoid printing the upper bound if it's invalid.  */
	  warning_at (loc, opt,
		      "%K%qD reading %E or more bytes from a region "
		      "of size %E",
		      exp, get_callee_fndecl (exp), range[0], slen);
	}
      else
	warning_at (loc, opt,
		    "%K%qD reading between %E and %E bytes from a region "
		    "of size %E",
		    exp, get_callee_fndecl (exp), range[0], range[1], slen);
      return false;
    }

  return true;
}

/* Helper to compute the size of the object referenced by the DEST
   expression which must of of pointer type, using Object Size type
   OSTYPE (only the least significant 2 bits are used).  Return
   the size of the object if successful or NULL when the size cannot
   be determined.  */

static inline tree
compute_objsize (tree dest, int ostype)
{
  unsigned HOST_WIDE_INT size;
  if (compute_builtin_object_size (dest, ostype & 3, &size))
    return build_int_cst (sizetype, size);

  return NULL_TREE;
}

/* Helper to determine and check the sizes of the source and the destination
   of calls to __builtin_{bzero,memcpy,mempcpy,memset} calls.  EXP is the
   call expression, DEST is the destination argument, SRC is the source
   argument or null, and LEN is the number of bytes.  Use Object Size type-0
   regardless of the OPT_Wstringop_overflow_ setting.  Return true on success
   (no overflow or invalid sizes), false otherwise.  */

static bool
check_memop_sizes (tree exp, tree dest, tree src, tree size)
{
  if (!warn_stringop_overflow)
    return true;

  /* For functions like memset and memcpy that operate on raw memory
     try to determine the size of the largest source and destination
     object using type-0 Object Size regardless of the object size
     type specified by the option.  */
  tree srcsize = src ? compute_objsize (src, 0) : NULL_TREE;
  tree dstsize = compute_objsize (dest, 0);

  return check_sizes (OPT_Wstringop_overflow_, exp,
		      size, /*maxlen=*/NULL_TREE, srcsize, dstsize);
}

/* Validate memchr arguments without performing any expansion.
   Return NULL_RTX.  */

static rtx
expand_builtin_memchr (tree exp, rtx)
{
  if (!validate_arglist (exp,
 			 POINTER_TYPE, INTEGER_TYPE, INTEGER_TYPE, VOID_TYPE))
    return NULL_RTX;

  tree arg1 = CALL_EXPR_ARG (exp, 0);
  tree len = CALL_EXPR_ARG (exp, 2);

  /* Diagnose calls where the specified length exceeds the size
     of the object.  */
  if (warn_stringop_overflow)
    {
      tree size = compute_objsize (arg1, 0);
      check_sizes (OPT_Wstringop_overflow_,
		   exp, len, /*maxlen=*/NULL_TREE,
		   size, /*objsize=*/NULL_TREE);
    }

  return NULL_RTX;
}

/* Expand a call EXP to the memcpy builtin.
   Return NULL_RTX if we failed, the caller should emit a normal call,
   otherwise try to get the result in TARGET, if convenient (and in
   mode MODE if that's convenient).  */

static rtx
expand_builtin_memcpy (tree exp, rtx target)
{
  if (!validate_arglist (exp,
 			 POINTER_TYPE, POINTER_TYPE, INTEGER_TYPE, VOID_TYPE))
    return NULL_RTX;

  tree dest = CALL_EXPR_ARG (exp, 0);
  tree src = CALL_EXPR_ARG (exp, 1);
  tree len = CALL_EXPR_ARG (exp, 2);

  check_memop_sizes (exp, dest, src, len);

  return expand_builtin_memory_copy_args (dest, src, len, target, exp,
					  /*endp=*/ 0);
}

/* Check a call EXP to the memmove built-in for validity.
   Return NULL_RTX on both success and failure.  */

static rtx
expand_builtin_memmove (tree exp, rtx)
{
  if (!validate_arglist (exp,
 			 POINTER_TYPE, POINTER_TYPE, INTEGER_TYPE, VOID_TYPE))
    return NULL_RTX;

  tree dest = CALL_EXPR_ARG (exp, 0);
  tree src = CALL_EXPR_ARG (exp, 1);
  tree len = CALL_EXPR_ARG (exp, 2);

  check_memop_sizes (exp, dest, src, len);

  return NULL_RTX;
}

/* Expand an instrumented call EXP to the memcpy builtin.
   Return NULL_RTX if we failed, the caller should emit a normal call,
   otherwise try to get the result in TARGET, if convenient (and in
   mode MODE if that's convenient).  */

static rtx
expand_builtin_memcpy_with_bounds (tree exp, rtx target)
{
  if (!validate_arglist (exp,
			 POINTER_TYPE, POINTER_BOUNDS_TYPE,
			 POINTER_TYPE, POINTER_BOUNDS_TYPE,
			 INTEGER_TYPE, VOID_TYPE))
    return NULL_RTX;
  else
    {
      tree dest = CALL_EXPR_ARG (exp, 0);
      tree src = CALL_EXPR_ARG (exp, 2);
      tree len = CALL_EXPR_ARG (exp, 4);
      rtx res = expand_builtin_memory_copy_args (dest, src, len, target, exp,
						 /*end_p=*/ 0);

      /* Return src bounds with the result.  */
      if (res)
	{
	  rtx bnd = force_reg (targetm.chkp_bound_mode (),
			       expand_normal (CALL_EXPR_ARG (exp, 1)));
	  res = chkp_join_splitted_slot (res, bnd);
	}
      return res;
    }
}

/* Expand a call EXP to the mempcpy builtin.
   Return NULL_RTX if we failed; the caller should emit a normal call,
   otherwise try to get the result in TARGET, if convenient (and in
   mode MODE if that's convenient).  If ENDP is 0 return the
   destination pointer, if ENDP is 1 return the end pointer ala
   mempcpy, and if ENDP is 2 return the end pointer minus one ala
   stpcpy.  */

static rtx
expand_builtin_mempcpy (tree exp, rtx target)
{
  if (!validate_arglist (exp,
 			 POINTER_TYPE, POINTER_TYPE, INTEGER_TYPE, VOID_TYPE))
    return NULL_RTX;

  tree dest = CALL_EXPR_ARG (exp, 0);
  tree src = CALL_EXPR_ARG (exp, 1);
  tree len = CALL_EXPR_ARG (exp, 2);

  /* Avoid expanding mempcpy into memcpy when the call is determined
     to overflow the buffer.  This also prevents the same overflow
     from being diagnosed again when expanding memcpy.  */
  if (!check_memop_sizes (exp, dest, src, len))
    return NULL_RTX;

  return expand_builtin_mempcpy_args (dest, src, len,
				      target, exp, /*endp=*/ 1);
}

/* Expand an instrumented call EXP to the mempcpy builtin.
   Return NULL_RTX if we failed, the caller should emit a normal call,
   otherwise try to get the result in TARGET, if convenient (and in
   mode MODE if that's convenient).  */

static rtx
expand_builtin_mempcpy_with_bounds (tree exp, rtx target)
{
  if (!validate_arglist (exp,
			 POINTER_TYPE, POINTER_BOUNDS_TYPE,
			 POINTER_TYPE, POINTER_BOUNDS_TYPE,
			 INTEGER_TYPE, VOID_TYPE))
    return NULL_RTX;
  else
    {
      tree dest = CALL_EXPR_ARG (exp, 0);
      tree src = CALL_EXPR_ARG (exp, 2);
      tree len = CALL_EXPR_ARG (exp, 4);
      rtx res = expand_builtin_mempcpy_args (dest, src, len, target,
					     exp, 1);

      /* Return src bounds with the result.  */
      if (res)
	{
	  rtx bnd = force_reg (targetm.chkp_bound_mode (),
			       expand_normal (CALL_EXPR_ARG (exp, 1)));
	  res = chkp_join_splitted_slot (res, bnd);
	}
      return res;
    }
}

/* Helper function to do the actual work for expand of memory copy family
   functions (memcpy, mempcpy, stpcpy).  Expansing should assign LEN bytes
   of memory from SRC to DEST and assign to TARGET if convenient.
   If ENDP is 0 return the
   destination pointer, if ENDP is 1 return the end pointer ala
   mempcpy, and if ENDP is 2 return the end pointer minus one ala
   stpcpy.  */

static rtx
expand_builtin_memory_copy_args (tree dest, tree src, tree len,
				 rtx target, tree exp, int endp)
{
  const char *src_str;
  unsigned int src_align = get_pointer_alignment (src);
  unsigned int dest_align = get_pointer_alignment (dest);
  rtx dest_mem, src_mem, dest_addr, len_rtx;
  HOST_WIDE_INT expected_size = -1;
  unsigned int expected_align = 0;
  unsigned HOST_WIDE_INT min_size;
  unsigned HOST_WIDE_INT max_size;
  unsigned HOST_WIDE_INT probable_max_size;

  /* If DEST is not a pointer type, call the normal function.  */
  if (dest_align == 0)
    return NULL_RTX;

  /* If either SRC is not a pointer type, don't do this
     operation in-line.  */
  if (src_align == 0)
    return NULL_RTX;

  if (currently_expanding_gimple_stmt)
    stringop_block_profile (currently_expanding_gimple_stmt,
			    &expected_align, &expected_size);

  if (expected_align < dest_align)
    expected_align = dest_align;
  dest_mem = get_memory_rtx (dest, len);
  set_mem_align (dest_mem, dest_align);
  len_rtx = expand_normal (len);
  determine_block_size (len, len_rtx, &min_size, &max_size,
			&probable_max_size);
  src_str = c_getstr (src);

  /* If SRC is a string constant and block move would be done
     by pieces, we can avoid loading the string from memory
     and only stored the computed constants.  */
  if (src_str
      && CONST_INT_P (len_rtx)
      && (unsigned HOST_WIDE_INT) INTVAL (len_rtx) <= strlen (src_str) + 1
      && can_store_by_pieces (INTVAL (len_rtx), builtin_memcpy_read_str,
			      CONST_CAST (char *, src_str),
			      dest_align, false))
    {
      dest_mem = store_by_pieces (dest_mem, INTVAL (len_rtx),
				  builtin_memcpy_read_str,
				  CONST_CAST (char *, src_str),
				  dest_align, false, endp);
      dest_mem = force_operand (XEXP (dest_mem, 0), target);
      dest_mem = convert_memory_address (ptr_mode, dest_mem);
      return dest_mem;
    }

  src_mem = get_memory_rtx (src, len);
  set_mem_align (src_mem, src_align);

  /* Copy word part most expediently.  */
  dest_addr = emit_block_move_hints (dest_mem, src_mem, len_rtx,
				     CALL_EXPR_TAILCALL (exp)
				     && (endp == 0 || target == const0_rtx)
				     ? BLOCK_OP_TAILCALL : BLOCK_OP_NORMAL,
				     expected_align, expected_size,
				     min_size, max_size, probable_max_size);

  if (dest_addr == 0)
    {
      dest_addr = force_operand (XEXP (dest_mem, 0), target);
      dest_addr = convert_memory_address (ptr_mode, dest_addr);
    }

  if (endp && target != const0_rtx)
    {
      dest_addr = gen_rtx_PLUS (ptr_mode, dest_addr, len_rtx);
      /* stpcpy pointer to last byte.  */
      if (endp == 2)
	dest_addr = gen_rtx_MINUS (ptr_mode, dest_addr, const1_rtx);
    }

  return dest_addr;
}

static rtx
expand_builtin_mempcpy_args (tree dest, tree src, tree len,
			     rtx target, tree orig_exp, int endp)
{
  return expand_builtin_memory_copy_args (dest, src, len, target, orig_exp,
					  endp);
}

/* Expand into a movstr instruction, if one is available.  Return NULL_RTX if
   we failed, the caller should emit a normal call, otherwise try to
   get the result in TARGET, if convenient.  If ENDP is 0 return the
   destination pointer, if ENDP is 1 return the end pointer ala
   mempcpy, and if ENDP is 2 return the end pointer minus one ala
   stpcpy.  */

static rtx
expand_movstr (tree dest, tree src, rtx target, int endp)
{
  struct expand_operand ops[3];
  rtx dest_mem;
  rtx src_mem;

  if (!targetm.have_movstr ())
    return NULL_RTX;

  dest_mem = get_memory_rtx (dest, NULL);
  src_mem = get_memory_rtx (src, NULL);
  if (!endp)
    {
      target = force_reg (Pmode, XEXP (dest_mem, 0));
      dest_mem = replace_equiv_address (dest_mem, target);
    }

  create_output_operand (&ops[0], endp ? target : NULL_RTX, Pmode);
  create_fixed_operand (&ops[1], dest_mem);
  create_fixed_operand (&ops[2], src_mem);
  if (!maybe_expand_insn (targetm.code_for_movstr, 3, ops))
    return NULL_RTX;

  if (endp && target != const0_rtx)
    {
      target = ops[0].value;
      /* movstr is supposed to set end to the address of the NUL
	 terminator.  If the caller requested a mempcpy-like return value,
	 adjust it.  */
      if (endp == 1)
	{
	  rtx tem = plus_constant (GET_MODE (target),
				   gen_lowpart (GET_MODE (target), target), 1);
	  emit_move_insn (target, force_operand (tem, NULL_RTX));
	}
    }
  return target;
}

/* Do some very basic size validation of a call to the strcpy builtin
   given by EXP.  Return NULL_RTX to have the built-in expand to a call
   to the library function.  */

static rtx
expand_builtin_strcat (tree exp, rtx)
{
  if (!validate_arglist (exp, POINTER_TYPE, POINTER_TYPE, VOID_TYPE)
      || !warn_stringop_overflow)
    return NULL_RTX;

  tree dest = CALL_EXPR_ARG (exp, 0);
  tree src = CALL_EXPR_ARG (exp, 1);

  /* There is no way here to determine the length of the string in
     the destination to which the SRC string is being appended so
     just diagnose cases when the souce string is longer than
     the destination object.  */

  tree destsize = compute_objsize (dest, warn_stringop_overflow - 1);

  check_sizes (OPT_Wstringop_overflow_,
	       exp, /*size=*/NULL_TREE, /*maxlen=*/NULL_TREE, src, destsize);

  return NULL_RTX;
}

/* Expand expression EXP, which is a call to the strcpy builtin.  Return
   NULL_RTX if we failed the caller should emit a normal call, otherwise
   try to get the result in TARGET, if convenient (and in mode MODE if that's
   convenient).  */

static rtx
expand_builtin_strcpy (tree exp, rtx target)
{
  if (!validate_arglist (exp, POINTER_TYPE, POINTER_TYPE, VOID_TYPE))
    return NULL_RTX;

  tree dest = CALL_EXPR_ARG (exp, 0);
  tree src = CALL_EXPR_ARG (exp, 1);

  if (warn_stringop_overflow)
    {
      tree destsize = compute_objsize (dest, warn_stringop_overflow - 1);
      check_sizes (OPT_Wstringop_overflow_,
		   exp, /*size=*/NULL_TREE, /*maxlen=*/NULL_TREE, src, destsize);
    }

  return expand_builtin_strcpy_args (dest, src, target);
}

/* Helper function to do the actual work for expand_builtin_strcpy.  The
   arguments to the builtin_strcpy call DEST and SRC are broken out
   so that this can also be called without constructing an actual CALL_EXPR.
   The other arguments and return value are the same as for
   expand_builtin_strcpy.  */

static rtx
expand_builtin_strcpy_args (tree dest, tree src, rtx target)
{
  return expand_movstr (dest, src, target, /*endp=*/0);
}

/* Expand a call EXP to the stpcpy builtin.
   Return NULL_RTX if we failed the caller should emit a normal call,
   otherwise try to get the result in TARGET, if convenient (and in
   mode MODE if that's convenient).  */

static rtx
expand_builtin_stpcpy (tree exp, rtx target, machine_mode mode)
{
  tree dst, src;
  location_t loc = EXPR_LOCATION (exp);

  if (!validate_arglist (exp, POINTER_TYPE, POINTER_TYPE, VOID_TYPE))
    return NULL_RTX;

  dst = CALL_EXPR_ARG (exp, 0);
  src = CALL_EXPR_ARG (exp, 1);

  if (warn_stringop_overflow)
    {
      tree destsize = compute_objsize (dst, warn_stringop_overflow - 1);
      check_sizes (OPT_Wstringop_overflow_,
		   exp, /*size=*/NULL_TREE, /*maxlen=*/NULL_TREE, src, destsize);
    }

  /* If return value is ignored, transform stpcpy into strcpy.  */
  if (target == const0_rtx && builtin_decl_implicit (BUILT_IN_STRCPY))
    {
      tree fn = builtin_decl_implicit (BUILT_IN_STRCPY);
      tree result = build_call_nofold_loc (loc, fn, 2, dst, src);
      return expand_expr (result, target, mode, EXPAND_NORMAL);
    }
  else
    {
      tree len, lenp1;
      rtx ret;

      /* Ensure we get an actual string whose length can be evaluated at
	 compile-time, not an expression containing a string.  This is
	 because the latter will potentially produce pessimized code
	 when used to produce the return value.  */
      if (! c_getstr (src) || ! (len = c_strlen (src, 0)))
	return expand_movstr (dst, src, target, /*endp=*/2);

      lenp1 = size_binop_loc (loc, PLUS_EXPR, len, ssize_int (1));
      ret = expand_builtin_mempcpy_args (dst, src, lenp1,
					 target, exp, /*endp=*/2);

      if (ret)
	return ret;

      if (TREE_CODE (len) == INTEGER_CST)
	{
	  rtx len_rtx = expand_normal (len);

	  if (CONST_INT_P (len_rtx))
	    {
	      ret = expand_builtin_strcpy_args (dst, src, target);

	      if (ret)
		{
		  if (! target)
		    {
		      if (mode != VOIDmode)
			target = gen_reg_rtx (mode);
		      else
			target = gen_reg_rtx (GET_MODE (ret));
		    }
		  if (GET_MODE (target) != GET_MODE (ret))
		    ret = gen_lowpart (GET_MODE (target), ret);

		  ret = plus_constant (GET_MODE (ret), ret, INTVAL (len_rtx));
		  ret = emit_move_insn (target, force_operand (ret, NULL_RTX));
		  gcc_assert (ret);

		  return target;
		}
	    }
	}

      return expand_movstr (dst, src, target, /*endp=*/2);
    }
}

/* Check a call EXP to the stpncpy built-in for validity.
   Return NULL_RTX on both success and failure.  */

static rtx
expand_builtin_stpncpy (tree exp, rtx)
{
  if (!validate_arglist (exp,
			 POINTER_TYPE, POINTER_TYPE, INTEGER_TYPE, VOID_TYPE)
      || !warn_stringop_overflow)
    return NULL_RTX;

  /* The source and destination of the call.  */
  tree dest = CALL_EXPR_ARG (exp, 0);
  tree src = CALL_EXPR_ARG (exp, 1);

  /* The exact number of bytes to write (not the maximum).  */
  tree len = CALL_EXPR_ARG (exp, 2);

  /* The size of the destination object.  */
  tree destsize = compute_objsize (dest, warn_stringop_overflow - 1);

  check_sizes (OPT_Wstringop_overflow_,
	       exp, len, /*maxlen=*/NULL_TREE, src, destsize);

  return NULL_RTX;
}

/* Callback routine for store_by_pieces.  Read GET_MODE_BITSIZE (MODE)
   bytes from constant string DATA + OFFSET and return it as target
   constant.  */

rtx
builtin_strncpy_read_str (void *data, HOST_WIDE_INT offset,
			  machine_mode mode)
{
  const char *str = (const char *) data;

  if ((unsigned HOST_WIDE_INT) offset > strlen (str))
    return const0_rtx;

  return c_readstr (str + offset, mode);
}

/* Helper to check the sizes of sequences and the destination of calls
   to __builtin_strncat and __builtin___strncat_chk.  Returns true on
   success (no overflow or invalid sizes), false otherwise.  */

static bool
check_strncat_sizes (tree exp, tree objsize)
{
  tree dest = CALL_EXPR_ARG (exp, 0);
  tree src = CALL_EXPR_ARG (exp, 1);
  tree maxlen = CALL_EXPR_ARG (exp, 2);

  /* Try to determine the range of lengths that the source expression
     refers to.  */
  tree lenrange[2];
  get_range_strlen (src, lenrange);

  /* Try to verify that the destination is big enough for the shortest
     string.  */

  if (!objsize && warn_stringop_overflow)
    {
      /* If it hasn't been provided by __strncat_chk, try to determine
	 the size of the destination object into which the source is
	 being copied.  */
      objsize = compute_objsize (dest, warn_stringop_overflow - 1);
    }

  /* Add one for the terminating nul.  */
  tree srclen = (lenrange[0]
		 ? fold_build2 (PLUS_EXPR, size_type_node, lenrange[0],
				size_one_node)
		 : NULL_TREE);

  /* Strncat copies at most MAXLEN bytes and always appends the terminating
     nul so the specified upper bound should never be equal to (or greater
     than) the size of the destination.  */
  if (tree_fits_uhwi_p (maxlen) && tree_fits_uhwi_p (objsize)
      && tree_int_cst_equal (objsize, maxlen))
    {
      location_t loc = tree_nonartificial_location (exp);
      loc = expansion_point_location_if_in_system_header (loc);

      warning_at (loc, OPT_Wstringop_overflow_,
		  "%K%qD specified bound %E equals destination size",
		  exp, get_callee_fndecl (exp), maxlen);

      return false;
    }

  if (!srclen
      || (maxlen && tree_fits_uhwi_p (maxlen)
	  && tree_fits_uhwi_p (srclen)
	  && tree_int_cst_lt (maxlen, srclen)))
    srclen = maxlen;

  /* The number of bytes to write is LEN but check_sizes will also
     check SRCLEN if LEN's value isn't known.  */
  return check_sizes (OPT_Wstringop_overflow_,
		      exp, /*size=*/NULL_TREE, maxlen, srclen, objsize);
}

/* Similar to expand_builtin_strcat, do some very basic size validation
   of a call to the strcpy builtin given by EXP.  Return NULL_RTX to have
   the built-in expand to a call to the library function.  */

static rtx
expand_builtin_strncat (tree exp, rtx)
{
  if (!validate_arglist (exp,
			 POINTER_TYPE, POINTER_TYPE, INTEGER_TYPE, VOID_TYPE)
      || !warn_stringop_overflow)
    return NULL_RTX;

  tree dest = CALL_EXPR_ARG (exp, 0);
  tree src = CALL_EXPR_ARG (exp, 1);
  /* The upper bound on the number of bytes to write.  */
  tree maxlen = CALL_EXPR_ARG (exp, 2);
  /* The length of the source sequence.  */
  tree slen = c_strlen (src, 1);

  /* Try to determine the range of lengths that the source expression
     refers to.  */
  tree lenrange[2];
  if (slen)
    lenrange[0] = lenrange[1] = slen;
  else
    get_range_strlen (src, lenrange);

  /* Try to verify that the destination is big enough for the shortest
     string.  First try to determine the size of the destination object
     into which the source is being copied.  */
  tree destsize = compute_objsize (dest, warn_stringop_overflow - 1);

  /* Add one for the terminating nul.  */
  tree srclen = (lenrange[0]
		 ? fold_build2 (PLUS_EXPR, size_type_node, lenrange[0],
				size_one_node)
		 : NULL_TREE);

  /* Strncat copies at most MAXLEN bytes and always appends the terminating
     nul so the specified upper bound should never be equal to (or greater
     than) the size of the destination.  */
  if (tree_fits_uhwi_p (maxlen) && tree_fits_uhwi_p (destsize)
      && tree_int_cst_equal (destsize, maxlen))
    {
      location_t loc = tree_nonartificial_location (exp);
      loc = expansion_point_location_if_in_system_header (loc);

      warning_at (loc, OPT_Wstringop_overflow_,
		  "%K%qD specified bound %E equals destination size",
		  exp, get_callee_fndecl (exp), maxlen);

      return NULL_RTX;
    }

  if (!srclen
      || (maxlen && tree_fits_uhwi_p (maxlen)
	  && tree_fits_uhwi_p (srclen)
	  && tree_int_cst_lt (maxlen, srclen)))
    srclen = maxlen;

  /* The number of bytes to write is LEN but check_sizes will also
     check SRCLEN if LEN's value isn't known.  */
  check_sizes (OPT_Wstringop_overflow_,
	       exp, /*size=*/NULL_TREE, maxlen, srclen, destsize);

  return NULL_RTX;
}

/* Expand expression EXP, which is a call to the strncpy builtin.  Return
   NULL_RTX if we failed the caller should emit a normal call.  */

static rtx
expand_builtin_strncpy (tree exp, rtx target)
{
  location_t loc = EXPR_LOCATION (exp);

  if (validate_arglist (exp,
 			POINTER_TYPE, POINTER_TYPE, INTEGER_TYPE, VOID_TYPE))
    {
      tree dest = CALL_EXPR_ARG (exp, 0);
      tree src = CALL_EXPR_ARG (exp, 1);
      /* The number of bytes to write (not the maximum).  */
      tree len = CALL_EXPR_ARG (exp, 2);
      /* The length of the source sequence.  */
      tree slen = c_strlen (src, 1);

      if (warn_stringop_overflow)
	{
	  tree destsize = compute_objsize (dest,
					   warn_stringop_overflow - 1);

	  /* The number of bytes to write is LEN but check_sizes will also
	     check SLEN if LEN's value isn't known.  */
	  check_sizes (OPT_Wstringop_overflow_,
		       exp, len, /*maxlen=*/NULL_TREE, src, destsize);
	}

      /* We must be passed a constant len and src parameter.  */
      if (!tree_fits_uhwi_p (len) || !slen || !tree_fits_uhwi_p (slen))
	return NULL_RTX;

      slen = size_binop_loc (loc, PLUS_EXPR, slen, ssize_int (1));

      /* We're required to pad with trailing zeros if the requested
	 len is greater than strlen(s2)+1.  In that case try to
	 use store_by_pieces, if it fails, punt.  */
      if (tree_int_cst_lt (slen, len))
	{
	  unsigned int dest_align = get_pointer_alignment (dest);
	  const char *p = c_getstr (src);
	  rtx dest_mem;

	  if (!p || dest_align == 0 || !tree_fits_uhwi_p (len)
	      || !can_store_by_pieces (tree_to_uhwi (len),
				       builtin_strncpy_read_str,
				       CONST_CAST (char *, p),
				       dest_align, false))
	    return NULL_RTX;

	  dest_mem = get_memory_rtx (dest, len);
	  store_by_pieces (dest_mem, tree_to_uhwi (len),
			   builtin_strncpy_read_str,
			   CONST_CAST (char *, p), dest_align, false, 0);
	  dest_mem = force_operand (XEXP (dest_mem, 0), target);
	  dest_mem = convert_memory_address (ptr_mode, dest_mem);
	  return dest_mem;
	}
    }
  return NULL_RTX;
}

/* Callback routine for store_by_pieces.  Read GET_MODE_BITSIZE (MODE)
   bytes from constant string DATA + OFFSET and return it as target
   constant.  */

rtx
builtin_memset_read_str (void *data, HOST_WIDE_INT offset ATTRIBUTE_UNUSED,
			 machine_mode mode)
{
  const char *c = (const char *) data;
  char *p = XALLOCAVEC (char, GET_MODE_SIZE (mode));

  memset (p, *c, GET_MODE_SIZE (mode));

  return c_readstr (p, mode);
}

/* Callback routine for store_by_pieces.  Return the RTL of a register
   containing GET_MODE_SIZE (MODE) consecutive copies of the unsigned
   char value given in the RTL register data.  For example, if mode is
   4 bytes wide, return the RTL for 0x01010101*data.  */

static rtx
builtin_memset_gen_str (void *data, HOST_WIDE_INT offset ATTRIBUTE_UNUSED,
			machine_mode mode)
{
  rtx target, coeff;
  size_t size;
  char *p;

  size = GET_MODE_SIZE (mode);
  if (size == 1)
    return (rtx) data;

  p = XALLOCAVEC (char, size);
  memset (p, 1, size);
  coeff = c_readstr (p, mode);

  target = convert_to_mode (mode, (rtx) data, 1);
  target = expand_mult (mode, target, coeff, NULL_RTX, 1);
  return force_reg (mode, target);
}

/* Expand expression EXP, which is a call to the memset builtin.  Return
   NULL_RTX if we failed the caller should emit a normal call, otherwise
   try to get the result in TARGET, if convenient (and in mode MODE if that's
   convenient).  */

static rtx
expand_builtin_memset (tree exp, rtx target, machine_mode mode)
{
  if (!validate_arglist (exp,
 			 POINTER_TYPE, INTEGER_TYPE, INTEGER_TYPE, VOID_TYPE))
    return NULL_RTX;

  tree dest = CALL_EXPR_ARG (exp, 0);
  tree val = CALL_EXPR_ARG (exp, 1);
  tree len = CALL_EXPR_ARG (exp, 2);

  check_memop_sizes (exp, dest, NULL_TREE, len);

  return expand_builtin_memset_args (dest, val, len, target, mode, exp);
}

/* Expand expression EXP, which is an instrumented call to the memset builtin.
   Return NULL_RTX if we failed the caller should emit a normal call, otherwise
   try to get the result in TARGET, if convenient (and in mode MODE if that's
   convenient).  */

static rtx
expand_builtin_memset_with_bounds (tree exp, rtx target, machine_mode mode)
{
  if (!validate_arglist (exp,
			 POINTER_TYPE, POINTER_BOUNDS_TYPE,
			 INTEGER_TYPE, INTEGER_TYPE, VOID_TYPE))
    return NULL_RTX;
  else
    {
      tree dest = CALL_EXPR_ARG (exp, 0);
      tree val = CALL_EXPR_ARG (exp, 2);
      tree len = CALL_EXPR_ARG (exp, 3);
      rtx res = expand_builtin_memset_args (dest, val, len, target, mode, exp);

      /* Return src bounds with the result.  */
      if (res)
	{
	  rtx bnd = force_reg (targetm.chkp_bound_mode (),
			       expand_normal (CALL_EXPR_ARG (exp, 1)));
	  res = chkp_join_splitted_slot (res, bnd);
	}
      return res;
    }
}

/* Helper function to do the actual work for expand_builtin_memset.  The
   arguments to the builtin_memset call DEST, VAL, and LEN are broken out
   so that this can also be called without constructing an actual CALL_EXPR.
   The other arguments and return value are the same as for
   expand_builtin_memset.  */

static rtx
expand_builtin_memset_args (tree dest, tree val, tree len,
			    rtx target, machine_mode mode, tree orig_exp)
{
  tree fndecl, fn;
  enum built_in_function fcode;
  machine_mode val_mode;
  char c;
  unsigned int dest_align;
  rtx dest_mem, dest_addr, len_rtx;
  HOST_WIDE_INT expected_size = -1;
  unsigned int expected_align = 0;
  unsigned HOST_WIDE_INT min_size;
  unsigned HOST_WIDE_INT max_size;
  unsigned HOST_WIDE_INT probable_max_size;

  dest_align = get_pointer_alignment (dest);

  /* If DEST is not a pointer type, don't do this operation in-line.  */
  if (dest_align == 0)
    return NULL_RTX;

  if (currently_expanding_gimple_stmt)
    stringop_block_profile (currently_expanding_gimple_stmt,
			    &expected_align, &expected_size);

  if (expected_align < dest_align)
    expected_align = dest_align;

  /* If the LEN parameter is zero, return DEST.  */
  if (integer_zerop (len))
    {
      /* Evaluate and ignore VAL in case it has side-effects.  */
      expand_expr (val, const0_rtx, VOIDmode, EXPAND_NORMAL);
      return expand_expr (dest, target, mode, EXPAND_NORMAL);
    }

  /* Stabilize the arguments in case we fail.  */
  dest = builtin_save_expr (dest);
  val = builtin_save_expr (val);
  len = builtin_save_expr (len);

  len_rtx = expand_normal (len);
  determine_block_size (len, len_rtx, &min_size, &max_size,
			&probable_max_size);
  dest_mem = get_memory_rtx (dest, len);
  val_mode = TYPE_MODE (unsigned_char_type_node);

  if (TREE_CODE (val) != INTEGER_CST)
    {
      rtx val_rtx;

      val_rtx = expand_normal (val);
      val_rtx = convert_to_mode (val_mode, val_rtx, 0);

      /* Assume that we can memset by pieces if we can store
       * the coefficients by pieces (in the required modes).
       * We can't pass builtin_memset_gen_str as that emits RTL.  */
      c = 1;
      if (tree_fits_uhwi_p (len)
	  && can_store_by_pieces (tree_to_uhwi (len),
				  builtin_memset_read_str, &c, dest_align,
				  true))
	{
	  val_rtx = force_reg (val_mode, val_rtx);
	  store_by_pieces (dest_mem, tree_to_uhwi (len),
			   builtin_memset_gen_str, val_rtx, dest_align,
			   true, 0);
	}
      else if (!set_storage_via_setmem (dest_mem, len_rtx, val_rtx,
					dest_align, expected_align,
					expected_size, min_size, max_size,
					probable_max_size))
	goto do_libcall;

      dest_mem = force_operand (XEXP (dest_mem, 0), NULL_RTX);
      dest_mem = convert_memory_address (ptr_mode, dest_mem);
      return dest_mem;
    }

  if (target_char_cast (val, &c))
    goto do_libcall;

  if (c)
    {
      if (tree_fits_uhwi_p (len)
	  && can_store_by_pieces (tree_to_uhwi (len),
				  builtin_memset_read_str, &c, dest_align,
				  true))
	store_by_pieces (dest_mem, tree_to_uhwi (len),
			 builtin_memset_read_str, &c, dest_align, true, 0);
      else if (!set_storage_via_setmem (dest_mem, len_rtx,
					gen_int_mode (c, val_mode),
					dest_align, expected_align,
					expected_size, min_size, max_size,
					probable_max_size))
	goto do_libcall;

      dest_mem = force_operand (XEXP (dest_mem, 0), NULL_RTX);
      dest_mem = convert_memory_address (ptr_mode, dest_mem);
      return dest_mem;
    }

  set_mem_align (dest_mem, dest_align);
  dest_addr = clear_storage_hints (dest_mem, len_rtx,
				   CALL_EXPR_TAILCALL (orig_exp)
				   ? BLOCK_OP_TAILCALL : BLOCK_OP_NORMAL,
				   expected_align, expected_size,
				   min_size, max_size,
				   probable_max_size);

  if (dest_addr == 0)
    {
      dest_addr = force_operand (XEXP (dest_mem, 0), NULL_RTX);
      dest_addr = convert_memory_address (ptr_mode, dest_addr);
    }

  return dest_addr;

 do_libcall:
  fndecl = get_callee_fndecl (orig_exp);
  fcode = DECL_FUNCTION_CODE (fndecl);
  if (fcode == BUILT_IN_MEMSET
      || fcode == BUILT_IN_CHKP_MEMSET_NOBND_NOCHK_CHKP)
    fn = build_call_nofold_loc (EXPR_LOCATION (orig_exp), fndecl, 3,
				dest, val, len);
  else if (fcode == BUILT_IN_BZERO)
    fn = build_call_nofold_loc (EXPR_LOCATION (orig_exp), fndecl, 2,
				dest, len);
  else
    gcc_unreachable ();
  gcc_assert (TREE_CODE (fn) == CALL_EXPR);
  CALL_EXPR_TAILCALL (fn) = CALL_EXPR_TAILCALL (orig_exp);
  return expand_call (fn, target, target == const0_rtx);
}

/* Expand expression EXP, which is a call to the bzero builtin.  Return
   NULL_RTX if we failed the caller should emit a normal call.  */

static rtx
expand_builtin_bzero (tree exp)
{
  if (!validate_arglist (exp, POINTER_TYPE, INTEGER_TYPE, VOID_TYPE))
    return NULL_RTX;

  tree dest = CALL_EXPR_ARG (exp, 0);
  tree size = CALL_EXPR_ARG (exp, 1);

  check_memop_sizes (exp, dest, NULL_TREE, size);

  /* New argument list transforming bzero(ptr x, int y) to
     memset(ptr x, int 0, size_t y).   This is done this way
     so that if it isn't expanded inline, we fallback to
     calling bzero instead of memset.  */

  location_t loc = EXPR_LOCATION (exp);

  return expand_builtin_memset_args (dest, integer_zero_node,
				     fold_convert_loc (loc,
						       size_type_node, size),
				     const0_rtx, VOIDmode, exp);
}

/* Try to expand cmpstr operation ICODE with the given operands.
   Return the result rtx on success, otherwise return null.  */

static rtx
expand_cmpstr (insn_code icode, rtx target, rtx arg1_rtx, rtx arg2_rtx,
	       HOST_WIDE_INT align)
{
  machine_mode insn_mode = insn_data[icode].operand[0].mode;

  if (target && (!REG_P (target) || HARD_REGISTER_P (target)))
    target = NULL_RTX;

  struct expand_operand ops[4];
  create_output_operand (&ops[0], target, insn_mode);
  create_fixed_operand (&ops[1], arg1_rtx);
  create_fixed_operand (&ops[2], arg2_rtx);
  create_integer_operand (&ops[3], align);
  if (maybe_expand_insn (icode, 4, ops))
    return ops[0].value;
  return NULL_RTX;
}

/* Expand expression EXP, which is a call to the memcmp built-in function.
   Return NULL_RTX if we failed and the caller should emit a normal call,
   otherwise try to get the result in TARGET, if convenient.
   RESULT_EQ is true if we can relax the returned value to be either zero
   or nonzero, without caring about the sign.  */

static rtx
expand_builtin_memcmp (tree exp, rtx target, bool result_eq)
{
  if (!validate_arglist (exp,
 			 POINTER_TYPE, POINTER_TYPE, INTEGER_TYPE, VOID_TYPE))
    return NULL_RTX;

  tree arg1 = CALL_EXPR_ARG (exp, 0);
  tree arg2 = CALL_EXPR_ARG (exp, 1);
  tree len = CALL_EXPR_ARG (exp, 2);

  /* Diagnose calls where the specified length exceeds the size of either
     object.  */
  if (warn_stringop_overflow)
    {
      tree size = compute_objsize (arg1, 0);
      if (check_sizes (OPT_Wstringop_overflow_,
		       exp, len, /*maxlen=*/NULL_TREE,
		       size, /*objsize=*/NULL_TREE))
	{
	  size = compute_objsize (arg2, 0);
	  check_sizes (OPT_Wstringop_overflow_,
		       exp, len, /*maxlen=*/NULL_TREE,
		       size, /*objsize=*/NULL_TREE);
	}
    }

  machine_mode mode = TYPE_MODE (TREE_TYPE (exp));
  location_t loc = EXPR_LOCATION (exp);

  unsigned int arg1_align = get_pointer_alignment (arg1) / BITS_PER_UNIT;
  unsigned int arg2_align = get_pointer_alignment (arg2) / BITS_PER_UNIT;

  /* If we don't have POINTER_TYPE, call the function.  */
  if (arg1_align == 0 || arg2_align == 0)
    return NULL_RTX;

  rtx arg1_rtx = get_memory_rtx (arg1, len);
  rtx arg2_rtx = get_memory_rtx (arg2, len);
  rtx len_rtx = expand_normal (fold_convert_loc (loc, sizetype, len));

  /* Set MEM_SIZE as appropriate.  */
  if (CONST_INT_P (len_rtx))
    {
      set_mem_size (arg1_rtx, INTVAL (len_rtx));
      set_mem_size (arg2_rtx, INTVAL (len_rtx));
    }

  by_pieces_constfn constfn = NULL;

  const char *src_str = c_getstr (arg2);
  if (result_eq && src_str == NULL)
    {
      src_str = c_getstr (arg1);
      if (src_str != NULL)
	std::swap (arg1_rtx, arg2_rtx);
    }

  /* If SRC is a string constant and block move would be done
     by pieces, we can avoid loading the string from memory
     and only stored the computed constants.  */
  if (src_str
      && CONST_INT_P (len_rtx)
      && (unsigned HOST_WIDE_INT) INTVAL (len_rtx) <= strlen (src_str) + 1)
    constfn = builtin_memcpy_read_str;

  rtx result = emit_block_cmp_hints (arg1_rtx, arg2_rtx, len_rtx,
				     TREE_TYPE (len), target,
				     result_eq, constfn,
				     CONST_CAST (char *, src_str));

  if (result)
    {
      /* Return the value in the proper mode for this function.  */
      if (GET_MODE (result) == mode)
	return result;

      if (target != 0)
	{
	  convert_move (target, result, 0);
	  return target;
	}

      return convert_to_mode (mode, result, 0);
    }

  return NULL_RTX;
}

/* Expand expression EXP, which is a call to the strcmp builtin.  Return NULL_RTX
   if we failed the caller should emit a normal call, otherwise try to get
   the result in TARGET, if convenient.  */

static rtx
expand_builtin_strcmp (tree exp, ATTRIBUTE_UNUSED rtx target)
{
  if (!validate_arglist (exp, POINTER_TYPE, POINTER_TYPE, VOID_TYPE))
    return NULL_RTX;

  insn_code cmpstr_icode = direct_optab_handler (cmpstr_optab, SImode);
  insn_code cmpstrn_icode = direct_optab_handler (cmpstrn_optab, SImode);
  if (cmpstr_icode != CODE_FOR_nothing || cmpstrn_icode != CODE_FOR_nothing)
    {
      rtx arg1_rtx, arg2_rtx;
      tree fndecl, fn;
      tree arg1 = CALL_EXPR_ARG (exp, 0);
      tree arg2 = CALL_EXPR_ARG (exp, 1);
      rtx result = NULL_RTX;

      unsigned int arg1_align = get_pointer_alignment (arg1) / BITS_PER_UNIT;
      unsigned int arg2_align = get_pointer_alignment (arg2) / BITS_PER_UNIT;

      /* If we don't have POINTER_TYPE, call the function.  */
      if (arg1_align == 0 || arg2_align == 0)
	return NULL_RTX;

      /* Stabilize the arguments in case gen_cmpstr(n)si fail.  */
      arg1 = builtin_save_expr (arg1);
      arg2 = builtin_save_expr (arg2);

      arg1_rtx = get_memory_rtx (arg1, NULL);
      arg2_rtx = get_memory_rtx (arg2, NULL);

      /* Try to call cmpstrsi.  */
      if (cmpstr_icode != CODE_FOR_nothing)
	result = expand_cmpstr (cmpstr_icode, target, arg1_rtx, arg2_rtx,
				MIN (arg1_align, arg2_align));

      /* Try to determine at least one length and call cmpstrnsi.  */
      if (!result && cmpstrn_icode != CODE_FOR_nothing)
	{
	  tree len;
	  rtx arg3_rtx;

	  tree len1 = c_strlen (arg1, 1);
	  tree len2 = c_strlen (arg2, 1);

	  if (len1)
	    len1 = size_binop (PLUS_EXPR, ssize_int (1), len1);
	  if (len2)
	    len2 = size_binop (PLUS_EXPR, ssize_int (1), len2);

	  /* If we don't have a constant length for the first, use the length
	     of the second, if we know it.  We don't require a constant for
	     this case; some cost analysis could be done if both are available
	     but neither is constant.  For now, assume they're equally cheap,
	     unless one has side effects.  If both strings have constant lengths,
	     use the smaller.  */

	  if (!len1)
	    len = len2;
	  else if (!len2)
	    len = len1;
	  else if (TREE_SIDE_EFFECTS (len1))
	    len = len2;
	  else if (TREE_SIDE_EFFECTS (len2))
	    len = len1;
	  else if (TREE_CODE (len1) != INTEGER_CST)
	    len = len2;
	  else if (TREE_CODE (len2) != INTEGER_CST)
	    len = len1;
	  else if (tree_int_cst_lt (len1, len2))
	    len = len1;
	  else
	    len = len2;

	  /* If both arguments have side effects, we cannot optimize.  */
	  if (len && !TREE_SIDE_EFFECTS (len))
	    {
	      arg3_rtx = expand_normal (len);
	      result = expand_cmpstrn_or_cmpmem
		(cmpstrn_icode, target, arg1_rtx, arg2_rtx, TREE_TYPE (len),
		 arg3_rtx, MIN (arg1_align, arg2_align));
	    }
	}

      if (result)
	{
	  /* Return the value in the proper mode for this function.  */
	  machine_mode mode = TYPE_MODE (TREE_TYPE (exp));
	  if (GET_MODE (result) == mode)
	    return result;
	  if (target == 0)
	    return convert_to_mode (mode, result, 0);
	  convert_move (target, result, 0);
	  return target;
	}

      /* Expand the library call ourselves using a stabilized argument
	 list to avoid re-evaluating the function's arguments twice.  */
      fndecl = get_callee_fndecl (exp);
      fn = build_call_nofold_loc (EXPR_LOCATION (exp), fndecl, 2, arg1, arg2);
      gcc_assert (TREE_CODE (fn) == CALL_EXPR);
      CALL_EXPR_TAILCALL (fn) = CALL_EXPR_TAILCALL (exp);
      return expand_call (fn, target, target == const0_rtx);
    }
  return NULL_RTX;
}

/* Expand expression EXP, which is a call to the strncmp builtin. Return
   NULL_RTX if we failed the caller should emit a normal call, otherwise try to get
   the result in TARGET, if convenient.  */

static rtx
expand_builtin_strncmp (tree exp, ATTRIBUTE_UNUSED rtx target,
			ATTRIBUTE_UNUSED machine_mode mode)
{
  location_t loc ATTRIBUTE_UNUSED = EXPR_LOCATION (exp);

  if (!validate_arglist (exp,
 			 POINTER_TYPE, POINTER_TYPE, INTEGER_TYPE, VOID_TYPE))
    return NULL_RTX;

  /* If c_strlen can determine an expression for one of the string
     lengths, and it doesn't have side effects, then emit cmpstrnsi
     using length MIN(strlen(string)+1, arg3).  */
  insn_code cmpstrn_icode = direct_optab_handler (cmpstrn_optab, SImode);
  if (cmpstrn_icode != CODE_FOR_nothing)
  {
    tree len, len1, len2, len3;
    rtx arg1_rtx, arg2_rtx, arg3_rtx;
    rtx result;
    tree fndecl, fn;
    tree arg1 = CALL_EXPR_ARG (exp, 0);
    tree arg2 = CALL_EXPR_ARG (exp, 1);
    tree arg3 = CALL_EXPR_ARG (exp, 2);

    unsigned int arg1_align = get_pointer_alignment (arg1) / BITS_PER_UNIT;
    unsigned int arg2_align = get_pointer_alignment (arg2) / BITS_PER_UNIT;

    len1 = c_strlen (arg1, 1);
    len2 = c_strlen (arg2, 1);

    if (len1)
      len1 = size_binop_loc (loc, PLUS_EXPR, ssize_int (1), len1);
    if (len2)
      len2 = size_binop_loc (loc, PLUS_EXPR, ssize_int (1), len2);

    len3 = fold_convert_loc (loc, sizetype, arg3);

    /* If we don't have a constant length for the first, use the length
       of the second, if we know it.  If neither string is constant length,
       use the given length argument.  We don't require a constant for
       this case; some cost analysis could be done if both are available
       but neither is constant.  For now, assume they're equally cheap,
       unless one has side effects.  If both strings have constant lengths,
       use the smaller.  */

    if (!len1 && !len2)
      len = len3;
    else if (!len1)
      len = len2;
    else if (!len2)
      len = len1;
    else if (TREE_SIDE_EFFECTS (len1))
      len = len2;
    else if (TREE_SIDE_EFFECTS (len2))
      len = len1;
    else if (TREE_CODE (len1) != INTEGER_CST)
      len = len2;
    else if (TREE_CODE (len2) != INTEGER_CST)
      len = len1;
    else if (tree_int_cst_lt (len1, len2))
      len = len1;
    else
      len = len2;

    /* If we are not using the given length, we must incorporate it here.
       The actual new length parameter will be MIN(len,arg3) in this case.  */
    if (len != len3)
      len = fold_build2_loc (loc, MIN_EXPR, TREE_TYPE (len), len, len3);
    arg1_rtx = get_memory_rtx (arg1, len);
    arg2_rtx = get_memory_rtx (arg2, len);
    arg3_rtx = expand_normal (len);
    result = expand_cmpstrn_or_cmpmem (cmpstrn_icode, target, arg1_rtx,
				       arg2_rtx, TREE_TYPE (len), arg3_rtx,
				       MIN (arg1_align, arg2_align));
    if (result)
      {
	/* Return the value in the proper mode for this function.  */
	mode = TYPE_MODE (TREE_TYPE (exp));
	if (GET_MODE (result) == mode)
	  return result;
	if (target == 0)
	  return convert_to_mode (mode, result, 0);
	convert_move (target, result, 0);
	return target;
      }

    /* Expand the library call ourselves using a stabilized argument
       list to avoid re-evaluating the function's arguments twice.  */
    fndecl = get_callee_fndecl (exp);
    fn = build_call_nofold_loc (EXPR_LOCATION (exp), fndecl, 3,
				arg1, arg2, len);
    gcc_assert (TREE_CODE (fn) == CALL_EXPR);
    CALL_EXPR_TAILCALL (fn) = CALL_EXPR_TAILCALL (exp);
    return expand_call (fn, target, target == const0_rtx);
  }
  return NULL_RTX;
}

/* Expand a call to __builtin_saveregs, generating the result in TARGET,
   if that's convenient.  */

rtx
expand_builtin_saveregs (void)
{
  rtx val;
  rtx_insn *seq;

  /* Don't do __builtin_saveregs more than once in a function.
     Save the result of the first call and reuse it.  */
  if (saveregs_value != 0)
    return saveregs_value;

  /* When this function is called, it means that registers must be
     saved on entry to this function.  So we migrate the call to the
     first insn of this function.  */

  start_sequence ();

  /* Do whatever the machine needs done in this case.  */
  val = targetm.calls.expand_builtin_saveregs ();

  seq = get_insns ();
  end_sequence ();

  saveregs_value = val;

  /* Put the insns after the NOTE that starts the function.  If this
     is inside a start_sequence, make the outer-level insn chain current, so
     the code is placed at the start of the function.  */
  push_topmost_sequence ();
  emit_insn_after (seq, entry_of_function ());
  pop_topmost_sequence ();

  return val;
}

/* Expand a call to __builtin_next_arg.  */

static rtx
expand_builtin_next_arg (void)
{
  /* Checking arguments is already done in fold_builtin_next_arg
     that must be called before this function.  */
  return expand_binop (ptr_mode, add_optab,
		       crtl->args.internal_arg_pointer,
		       crtl->args.arg_offset_rtx,
		       NULL_RTX, 0, OPTAB_LIB_WIDEN);
}

/* Make it easier for the backends by protecting the valist argument
   from multiple evaluations.  */

static tree
stabilize_va_list_loc (location_t loc, tree valist, int needs_lvalue)
{
  tree vatype = targetm.canonical_va_list_type (TREE_TYPE (valist));

  /* The current way of determining the type of valist is completely
     bogus.  We should have the information on the va builtin instead.  */
  if (!vatype)
    vatype = targetm.fn_abi_va_list (cfun->decl);

  if (TREE_CODE (vatype) == ARRAY_TYPE)
    {
      if (TREE_SIDE_EFFECTS (valist))
	valist = save_expr (valist);

      /* For this case, the backends will be expecting a pointer to
	 vatype, but it's possible we've actually been given an array
	 (an actual TARGET_CANONICAL_VA_LIST_TYPE (valist)).
	 So fix it.  */
      if (TREE_CODE (TREE_TYPE (valist)) == ARRAY_TYPE)
	{
	  tree p1 = build_pointer_type (TREE_TYPE (vatype));
	  valist = build_fold_addr_expr_with_type_loc (loc, valist, p1);
	}
    }
  else
    {
      tree pt = build_pointer_type (vatype);

      if (! needs_lvalue)
	{
	  if (! TREE_SIDE_EFFECTS (valist))
	    return valist;

	  valist = fold_build1_loc (loc, ADDR_EXPR, pt, valist);
	  TREE_SIDE_EFFECTS (valist) = 1;
	}

      if (TREE_SIDE_EFFECTS (valist))
	valist = save_expr (valist);
      valist = fold_build2_loc (loc, MEM_REF,
				vatype, valist, build_int_cst (pt, 0));
    }

  return valist;
}

/* The "standard" definition of va_list is void*.  */

tree
std_build_builtin_va_list (void)
{
  return ptr_type_node;
}

/* The "standard" abi va_list is va_list_type_node.  */

tree
std_fn_abi_va_list (tree fndecl ATTRIBUTE_UNUSED)
{
  return va_list_type_node;
}

/* The "standard" type of va_list is va_list_type_node.  */

tree
std_canonical_va_list_type (tree type)
{
  tree wtype, htype;

  wtype = va_list_type_node;
  htype = type;

  if (TREE_CODE (wtype) == ARRAY_TYPE)
    {
      /* If va_list is an array type, the argument may have decayed
	 to a pointer type, e.g. by being passed to another function.
	 In that case, unwrap both types so that we can compare the
	 underlying records.  */
      if (TREE_CODE (htype) == ARRAY_TYPE
	  || POINTER_TYPE_P (htype))
	{
	  wtype = TREE_TYPE (wtype);
	  htype = TREE_TYPE (htype);
	}
    }
  if (TYPE_MAIN_VARIANT (wtype) == TYPE_MAIN_VARIANT (htype))
    return va_list_type_node;

  return NULL_TREE;
}

/* The "standard" implementation of va_start: just assign `nextarg' to
   the variable.  */

void
std_expand_builtin_va_start (tree valist, rtx nextarg)
{
  rtx va_r = expand_expr (valist, NULL_RTX, VOIDmode, EXPAND_WRITE);
  convert_move (va_r, nextarg, 0);

  /* We do not have any valid bounds for the pointer, so
     just store zero bounds for it.  */
  if (chkp_function_instrumented_p (current_function_decl))
    chkp_expand_bounds_reset_for_mem (valist,
				      make_tree (TREE_TYPE (valist),
						 nextarg));
}

/* Expand EXP, a call to __builtin_va_start.  */

static rtx
expand_builtin_va_start (tree exp)
{
  rtx nextarg;
  tree valist;
  location_t loc = EXPR_LOCATION (exp);

  if (call_expr_nargs (exp) < 2)
    {
      error_at (loc, "too few arguments to function %<va_start%>");
      return const0_rtx;
    }

  if (fold_builtin_next_arg (exp, true))
    return const0_rtx;

  nextarg = expand_builtin_next_arg ();
  valist = stabilize_va_list_loc (loc, CALL_EXPR_ARG (exp, 0), 1);

  if (targetm.expand_builtin_va_start)
    targetm.expand_builtin_va_start (valist, nextarg);
  else
    std_expand_builtin_va_start (valist, nextarg);

  return const0_rtx;
}

/* Expand EXP, a call to __builtin_va_end.  */

static rtx
expand_builtin_va_end (tree exp)
{
  tree valist = CALL_EXPR_ARG (exp, 0);

  /* Evaluate for side effects, if needed.  I hate macros that don't
     do that.  */
  if (TREE_SIDE_EFFECTS (valist))
    expand_expr (valist, const0_rtx, VOIDmode, EXPAND_NORMAL);

  return const0_rtx;
}

/* Expand EXP, a call to __builtin_va_copy.  We do this as a
   builtin rather than just as an assignment in stdarg.h because of the
   nastiness of array-type va_list types.  */

static rtx
expand_builtin_va_copy (tree exp)
{
  tree dst, src, t;
  location_t loc = EXPR_LOCATION (exp);

  dst = CALL_EXPR_ARG (exp, 0);
  src = CALL_EXPR_ARG (exp, 1);

  dst = stabilize_va_list_loc (loc, dst, 1);
  src = stabilize_va_list_loc (loc, src, 0);

  gcc_assert (cfun != NULL && cfun->decl != NULL_TREE);

  if (TREE_CODE (targetm.fn_abi_va_list (cfun->decl)) != ARRAY_TYPE)
    {
      t = build2 (MODIFY_EXPR, targetm.fn_abi_va_list (cfun->decl), dst, src);
      TREE_SIDE_EFFECTS (t) = 1;
      expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);
    }
  else
    {
      rtx dstb, srcb, size;

      /* Evaluate to pointers.  */
      dstb = expand_expr (dst, NULL_RTX, Pmode, EXPAND_NORMAL);
      srcb = expand_expr (src, NULL_RTX, Pmode, EXPAND_NORMAL);
      size = expand_expr (TYPE_SIZE_UNIT (targetm.fn_abi_va_list (cfun->decl)),
      		  NULL_RTX, VOIDmode, EXPAND_NORMAL);

      dstb = convert_memory_address (Pmode, dstb);
      srcb = convert_memory_address (Pmode, srcb);

      /* "Dereference" to BLKmode memories.  */
      dstb = gen_rtx_MEM (BLKmode, dstb);
      set_mem_alias_set (dstb, get_alias_set (TREE_TYPE (TREE_TYPE (dst))));
      set_mem_align (dstb, TYPE_ALIGN (targetm.fn_abi_va_list (cfun->decl)));
      srcb = gen_rtx_MEM (BLKmode, srcb);
      set_mem_alias_set (srcb, get_alias_set (TREE_TYPE (TREE_TYPE (src))));
      set_mem_align (srcb, TYPE_ALIGN (targetm.fn_abi_va_list (cfun->decl)));

      /* Copy.  */
      emit_block_move (dstb, srcb, size, BLOCK_OP_NORMAL);
    }

  return const0_rtx;
}

/* Expand a call to one of the builtin functions __builtin_frame_address or
   __builtin_return_address.  */

static rtx
expand_builtin_frame_address (tree fndecl, tree exp)
{
  /* The argument must be a nonnegative integer constant.
     It counts the number of frames to scan up the stack.
     The value is either the frame pointer value or the return
     address saved in that frame.  */
  if (call_expr_nargs (exp) == 0)
    /* Warning about missing arg was already issued.  */
    return const0_rtx;
  else if (! tree_fits_uhwi_p (CALL_EXPR_ARG (exp, 0)))
    {
      error ("invalid argument to %qD", fndecl);
      return const0_rtx;
    }
  else
    {
      /* Number of frames to scan up the stack.  */
      unsigned HOST_WIDE_INT count = tree_to_uhwi (CALL_EXPR_ARG (exp, 0));

      rtx tem = expand_builtin_return_addr (DECL_FUNCTION_CODE (fndecl), count);

      /* Some ports cannot access arbitrary stack frames.  */
      if (tem == NULL)
	{
	  warning (0, "unsupported argument to %qD", fndecl);
	  return const0_rtx;
	}

      if (count)
	{
	  /* Warn since no effort is made to ensure that any frame
	     beyond the current one exists or can be safely reached.  */
	  warning (OPT_Wframe_address, "calling %qD with "
		   "a nonzero argument is unsafe", fndecl);
	}

      /* For __builtin_frame_address, return what we've got.  */
      if (DECL_FUNCTION_CODE (fndecl) == BUILT_IN_FRAME_ADDRESS)
	return tem;

      if (!REG_P (tem)
	  && ! CONSTANT_P (tem))
	tem = copy_addr_to_reg (tem);
      return tem;
    }
}

/* Expand EXP, a call to the alloca builtin.  Return NULL_RTX if we
   failed and the caller should emit a normal call.  */

static rtx
expand_builtin_alloca (tree exp)
{
  rtx op0;
  rtx result;
  unsigned int align;
  tree fndecl = get_callee_fndecl (exp);
  bool alloca_with_align = (DECL_FUNCTION_CODE (fndecl)
			    == BUILT_IN_ALLOCA_WITH_ALIGN);
  bool alloca_for_var = CALL_ALLOCA_FOR_VAR_P (exp);
  bool valid_arglist
    = (alloca_with_align
       ? validate_arglist (exp, INTEGER_TYPE, INTEGER_TYPE, VOID_TYPE)
       : validate_arglist (exp, INTEGER_TYPE, VOID_TYPE));

  if (!valid_arglist)
    return NULL_RTX;

  if ((alloca_with_align && !warn_vla_limit)
      || (!alloca_with_align && !warn_alloca_limit))
    {
      /* -Walloca-larger-than and -Wvla-larger-than settings override
	 the more general -Walloc-size-larger-than so unless either of
	 the former options is specified check the alloca arguments for
	 overflow.  */
      tree args[] = { CALL_EXPR_ARG (exp, 0), NULL_TREE };
      int idx[] = { 0, -1 };
      maybe_warn_alloc_args_overflow (fndecl, exp, args, idx);
    }

  /* Compute the argument.  */
  op0 = expand_normal (CALL_EXPR_ARG (exp, 0));

  /* Compute the alignment.  */
  align = (alloca_with_align
	   ? TREE_INT_CST_LOW (CALL_EXPR_ARG (exp, 1))
	   : BIGGEST_ALIGNMENT);

  /* Allocate the desired space.  If the allocation stems from the declaration
     of a variable-sized object, it cannot accumulate.  */
  result = allocate_dynamic_stack_space (op0, 0, align, alloca_for_var);
  result = convert_memory_address (ptr_mode, result);

  return result;
}

/* Emit a call to __asan_allocas_unpoison call in EXP.  Replace second argument
   of the call with virtual_stack_dynamic_rtx because in asan pass we emit a
   dummy value into second parameter relying on this function to perform the
   change.  See motivation for this in comment to handle_builtin_stack_restore
   function.  */

static rtx
expand_asan_emit_allocas_unpoison (tree exp)
{
  tree arg0 = CALL_EXPR_ARG (exp, 0);
  rtx top = expand_expr (arg0, NULL_RTX, ptr_mode, EXPAND_NORMAL);
  rtx bot = convert_memory_address (ptr_mode, virtual_stack_dynamic_rtx);
  rtx ret = init_one_libfunc ("__asan_allocas_unpoison");
  ret = emit_library_call_value (ret, NULL_RTX, LCT_NORMAL, ptr_mode, 2, top,
				 ptr_mode, bot, ptr_mode);
  return ret;
}

/* Expand a call to bswap builtin in EXP.
   Return NULL_RTX if a normal call should be emitted rather than expanding the
   function in-line.  If convenient, the result should be placed in TARGET.
   SUBTARGET may be used as the target for computing one of EXP's operands.  */

static rtx
expand_builtin_bswap (machine_mode target_mode, tree exp, rtx target,
		      rtx subtarget)
{
  tree arg;
  rtx op0;

  if (!validate_arglist (exp, INTEGER_TYPE, VOID_TYPE))
    return NULL_RTX;

  arg = CALL_EXPR_ARG (exp, 0);
  op0 = expand_expr (arg,
		     subtarget && GET_MODE (subtarget) == target_mode
		     ? subtarget : NULL_RTX,
		     target_mode, EXPAND_NORMAL);
  if (GET_MODE (op0) != target_mode)
    op0 = convert_to_mode (target_mode, op0, 1);

  target = expand_unop (target_mode, bswap_optab, op0, target, 1);

  gcc_assert (target);

  return convert_to_mode (target_mode, target, 1);
}

/* Expand a call to a unary builtin in EXP.
   Return NULL_RTX if a normal call should be emitted rather than expanding the
   function in-line.  If convenient, the result should be placed in TARGET.
   SUBTARGET may be used as the target for computing one of EXP's operands.  */

static rtx
expand_builtin_unop (machine_mode target_mode, tree exp, rtx target,
		     rtx subtarget, optab op_optab)
{
  rtx op0;

  if (!validate_arglist (exp, INTEGER_TYPE, VOID_TYPE))
    return NULL_RTX;

  /* Compute the argument.  */
  op0 = expand_expr (CALL_EXPR_ARG (exp, 0),
		     (subtarget
		      && (TYPE_MODE (TREE_TYPE (CALL_EXPR_ARG (exp, 0)))
			  == GET_MODE (subtarget))) ? subtarget : NULL_RTX,
		     VOIDmode, EXPAND_NORMAL);
  /* Compute op, into TARGET if possible.
     Set TARGET to wherever the result comes back.  */
  target = expand_unop (TYPE_MODE (TREE_TYPE (CALL_EXPR_ARG (exp, 0))),
			op_optab, op0, target, op_optab != clrsb_optab);
  gcc_assert (target);

  return convert_to_mode (target_mode, target, 0);
}

/* Expand a call to __builtin_expect.  We just return our argument
   as the builtin_expect semantic should've been already executed by
   tree branch prediction pass. */

static rtx
expand_builtin_expect (tree exp, rtx target)
{
  tree arg;

  if (call_expr_nargs (exp) < 2)
    return const0_rtx;
  arg = CALL_EXPR_ARG (exp, 0);

  target = expand_expr (arg, target, VOIDmode, EXPAND_NORMAL);
  /* When guessing was done, the hints should be already stripped away.  */
  gcc_assert (!flag_guess_branch_prob
	      || optimize == 0 || seen_error ());
  return target;
}

/* Expand a call to __builtin_assume_aligned.  We just return our first
   argument as the builtin_assume_aligned semantic should've been already
   executed by CCP.  */

static rtx
expand_builtin_assume_aligned (tree exp, rtx target)
{
  if (call_expr_nargs (exp) < 2)
    return const0_rtx;
  target = expand_expr (CALL_EXPR_ARG (exp, 0), target, VOIDmode,
			EXPAND_NORMAL);
  gcc_assert (!TREE_SIDE_EFFECTS (CALL_EXPR_ARG (exp, 1))
	      && (call_expr_nargs (exp) < 3
		  || !TREE_SIDE_EFFECTS (CALL_EXPR_ARG (exp, 2))));
  return target;
}

void
expand_builtin_trap (void)
{
  if (targetm.have_trap ())
    {
      rtx_insn *insn = emit_insn (targetm.gen_trap ());
      /* For trap insns when not accumulating outgoing args force
	 REG_ARGS_SIZE note to prevent crossjumping of calls with
	 different args sizes.  */
      if (!ACCUMULATE_OUTGOING_ARGS)
	add_reg_note (insn, REG_ARGS_SIZE, GEN_INT (stack_pointer_delta));
    }
  else
    {
      tree fn = builtin_decl_implicit (BUILT_IN_ABORT);
      tree call_expr = build_call_expr (fn, 0);
      expand_call (call_expr, NULL_RTX, false);
    }

  emit_barrier ();
}

/* Expand a call to __builtin_unreachable.  We do nothing except emit
   a barrier saying that control flow will not pass here.

   It is the responsibility of the program being compiled to ensure
   that control flow does never reach __builtin_unreachable.  */
static void
expand_builtin_unreachable (void)
{
  emit_barrier ();
}

/* Expand EXP, a call to fabs, fabsf or fabsl.
   Return NULL_RTX if a normal call should be emitted rather than expanding
   the function inline.  If convenient, the result should be placed
   in TARGET.  SUBTARGET may be used as the target for computing
   the operand.  */

static rtx
expand_builtin_fabs (tree exp, rtx target, rtx subtarget)
{
  machine_mode mode;
  tree arg;
  rtx op0;

  if (!validate_arglist (exp, REAL_TYPE, VOID_TYPE))
    return NULL_RTX;

  arg = CALL_EXPR_ARG (exp, 0);
  CALL_EXPR_ARG (exp, 0) = arg = builtin_save_expr (arg);
  mode = TYPE_MODE (TREE_TYPE (arg));
  op0 = expand_expr (arg, subtarget, VOIDmode, EXPAND_NORMAL);
  return expand_abs (mode, op0, target, 0, safe_from_p (target, arg, 1));
}

/* Expand EXP, a call to copysign, copysignf, or copysignl.
   Return NULL is a normal call should be emitted rather than expanding the
   function inline.  If convenient, the result should be placed in TARGET.
   SUBTARGET may be used as the target for computing the operand.  */

static rtx
expand_builtin_copysign (tree exp, rtx target, rtx subtarget)
{
  rtx op0, op1;
  tree arg;

  if (!validate_arglist (exp, REAL_TYPE, REAL_TYPE, VOID_TYPE))
    return NULL_RTX;

  arg = CALL_EXPR_ARG (exp, 0);
  op0 = expand_expr (arg, subtarget, VOIDmode, EXPAND_NORMAL);

  arg = CALL_EXPR_ARG (exp, 1);
  op1 = expand_normal (arg);

  return expand_copysign (op0, op1, target);
}

/* Expand a call to __builtin___clear_cache.  */

static rtx
expand_builtin___clear_cache (tree exp)
{
  if (!targetm.code_for_clear_cache)
    {
#ifdef CLEAR_INSN_CACHE
      /* There is no "clear_cache" insn, and __clear_cache() in libgcc
	 does something.  Just do the default expansion to a call to
	 __clear_cache().  */
      return NULL_RTX;
#else
      /* There is no "clear_cache" insn, and __clear_cache() in libgcc
	 does nothing.  There is no need to call it.  Do nothing.  */
      return const0_rtx;
#endif /* CLEAR_INSN_CACHE */
    }

  /* We have a "clear_cache" insn, and it will handle everything.  */
  tree begin, end;
  rtx begin_rtx, end_rtx;

  /* We must not expand to a library call.  If we did, any
     fallback library function in libgcc that might contain a call to
     __builtin___clear_cache() would recurse infinitely.  */
  if (!validate_arglist (exp, POINTER_TYPE, POINTER_TYPE, VOID_TYPE))
    {
      error ("both arguments to %<__builtin___clear_cache%> must be pointers");
      return const0_rtx;
    }

  if (targetm.have_clear_cache ())
    {
      struct expand_operand ops[2];

      begin = CALL_EXPR_ARG (exp, 0);
      begin_rtx = expand_expr (begin, NULL_RTX, Pmode, EXPAND_NORMAL);

      end = CALL_EXPR_ARG (exp, 1);
      end_rtx = expand_expr (end, NULL_RTX, Pmode, EXPAND_NORMAL);

      create_address_operand (&ops[0], begin_rtx);
      create_address_operand (&ops[1], end_rtx);
      if (maybe_expand_insn (targetm.code_for_clear_cache, 2, ops))
	return const0_rtx;
    }
  return const0_rtx;
}

/* Given a trampoline address, make sure it satisfies TRAMPOLINE_ALIGNMENT.  */

static rtx
round_trampoline_addr (rtx tramp)
{
  rtx temp, addend, mask;

  /* If we don't need too much alignment, we'll have been guaranteed
     proper alignment by get_trampoline_type.  */
  if (TRAMPOLINE_ALIGNMENT <= STACK_BOUNDARY)
    return tramp;

  /* Round address up to desired boundary.  */
  temp = gen_reg_rtx (Pmode);
  addend = gen_int_mode (TRAMPOLINE_ALIGNMENT / BITS_PER_UNIT - 1, Pmode);
  mask = gen_int_mode (-TRAMPOLINE_ALIGNMENT / BITS_PER_UNIT, Pmode);

  temp  = expand_simple_binop (Pmode, PLUS, tramp, addend,
			       temp, 0, OPTAB_LIB_WIDEN);
  tramp = expand_simple_binop (Pmode, AND, temp, mask,
			       temp, 0, OPTAB_LIB_WIDEN);

  return tramp;
}

static rtx
expand_builtin_init_trampoline (tree exp, bool onstack)
{
  tree t_tramp, t_func, t_chain;
  rtx m_tramp, r_tramp, r_chain, tmp;

  if (!validate_arglist (exp, POINTER_TYPE, POINTER_TYPE,
			 POINTER_TYPE, VOID_TYPE))
    return NULL_RTX;

  t_tramp = CALL_EXPR_ARG (exp, 0);
  t_func = CALL_EXPR_ARG (exp, 1);
  t_chain = CALL_EXPR_ARG (exp, 2);

  r_tramp = expand_normal (t_tramp);
  m_tramp = gen_rtx_MEM (BLKmode, r_tramp);
  MEM_NOTRAP_P (m_tramp) = 1;

  /* If ONSTACK, the TRAMP argument should be the address of a field
     within the local function's FRAME decl.  Either way, let's see if
     we can fill in the MEM_ATTRs for this memory.  */
  if (TREE_CODE (t_tramp) == ADDR_EXPR)
    set_mem_attributes (m_tramp, TREE_OPERAND (t_tramp, 0), true);

  /* Creator of a heap trampoline is responsible for making sure the
     address is aligned to at least STACK_BOUNDARY.  Normally malloc
     will ensure this anyhow.  */
  tmp = round_trampoline_addr (r_tramp);
  if (tmp != r_tramp)
    {
      m_tramp = change_address (m_tramp, BLKmode, tmp);
      set_mem_align (m_tramp, TRAMPOLINE_ALIGNMENT);
      set_mem_size (m_tramp, TRAMPOLINE_SIZE);
    }

  /* The FUNC argument should be the address of the nested function.
     Extract the actual function decl to pass to the hook.  */
  gcc_assert (TREE_CODE (t_func) == ADDR_EXPR);
  t_func = TREE_OPERAND (t_func, 0);
  gcc_assert (TREE_CODE (t_func) == FUNCTION_DECL);

  r_chain = expand_normal (t_chain);

  /* Generate insns to initialize the trampoline.  */
  targetm.calls.trampoline_init (m_tramp, t_func, r_chain);

  if (onstack)
    {
      trampolines_created = 1;

      if (targetm.calls.custom_function_descriptors != 0)
	warning_at (DECL_SOURCE_LOCATION (t_func), OPT_Wtrampolines,
		    "trampoline generated for nested function %qD", t_func);
    }

  return const0_rtx;
}

static rtx
expand_builtin_adjust_trampoline (tree exp)
{
  rtx tramp;

  if (!validate_arglist (exp, POINTER_TYPE, VOID_TYPE))
    return NULL_RTX;

  tramp = expand_normal (CALL_EXPR_ARG (exp, 0));
  tramp = round_trampoline_addr (tramp);
  if (targetm.calls.trampoline_adjust_address)
    tramp = targetm.calls.trampoline_adjust_address (tramp);

  return tramp;
}

/* Expand a call to the builtin descriptor initialization routine.
   A descriptor is made up of a couple of pointers to the static
   chain and the code entry in this order.  */

static rtx
expand_builtin_init_descriptor (tree exp)
{
  tree t_descr, t_func, t_chain;
  rtx m_descr, r_descr, r_func, r_chain;

  if (!validate_arglist (exp, POINTER_TYPE, POINTER_TYPE, POINTER_TYPE,
			 VOID_TYPE))
    return NULL_RTX;

  t_descr = CALL_EXPR_ARG (exp, 0);
  t_func = CALL_EXPR_ARG (exp, 1);
  t_chain = CALL_EXPR_ARG (exp, 2);

  r_descr = expand_normal (t_descr);
  m_descr = gen_rtx_MEM (BLKmode, r_descr);
  MEM_NOTRAP_P (m_descr) = 1;

  r_func = expand_normal (t_func);
  r_chain = expand_normal (t_chain);

  /* Generate insns to initialize the descriptor.  */
  emit_move_insn (adjust_address_nv (m_descr, ptr_mode, 0), r_chain);
  emit_move_insn (adjust_address_nv (m_descr, ptr_mode,
				     POINTER_SIZE / BITS_PER_UNIT), r_func);

  return const0_rtx;
}

/* Expand a call to the builtin descriptor adjustment routine.  */

static rtx
expand_builtin_adjust_descriptor (tree exp)
{
  rtx tramp;

  if (!validate_arglist (exp, POINTER_TYPE, VOID_TYPE))
    return NULL_RTX;

  tramp = expand_normal (CALL_EXPR_ARG (exp, 0));

  /* Unalign the descriptor to allow runtime identification.  */
  tramp = plus_constant (ptr_mode, tramp,
			 targetm.calls.custom_function_descriptors);

  return force_operand (tramp, NULL_RTX);
}

/* Expand the call EXP to the built-in signbit, signbitf or signbitl
   function.  The function first checks whether the back end provides
   an insn to implement signbit for the respective mode.  If not, it
   checks whether the floating point format of the value is such that
   the sign bit can be extracted.  If that is not the case, error out.
   EXP is the expression that is a call to the builtin function; if
   convenient, the result should be placed in TARGET.  */
static rtx
expand_builtin_signbit (tree exp, rtx target)
{
  const struct real_format *fmt;
  scalar_float_mode fmode;
  machine_mode imode, rmode;
  tree arg;
  int word, bitpos;
  enum insn_code icode;
  rtx temp;
  location_t loc = EXPR_LOCATION (exp);

  if (!validate_arglist (exp, REAL_TYPE, VOID_TYPE))
    return NULL_RTX;

  arg = CALL_EXPR_ARG (exp, 0);
  fmode = SCALAR_FLOAT_TYPE_MODE (TREE_TYPE (arg));
  rmode = TYPE_MODE (TREE_TYPE (exp));
  fmt = REAL_MODE_FORMAT (fmode);

  arg = builtin_save_expr (arg);

  /* Expand the argument yielding a RTX expression. */
  temp = expand_normal (arg);

  /* Check if the back end provides an insn that handles signbit for the
     argument's mode. */
  icode = optab_handler (signbit_optab, fmode);
  if (icode != CODE_FOR_nothing)
    {
      rtx_insn *last = get_last_insn ();
      target = gen_reg_rtx (TYPE_MODE (TREE_TYPE (exp)));
      if (maybe_emit_unop_insn (icode, target, temp, UNKNOWN))
	return target;
      delete_insns_since (last);
    }

  /* For floating point formats without a sign bit, implement signbit
     as "ARG < 0.0".  */
  bitpos = fmt->signbit_ro;
  if (bitpos < 0)
  {
    /* But we can't do this if the format supports signed zero.  */
    gcc_assert (!fmt->has_signed_zero || !HONOR_SIGNED_ZEROS (fmode));

    arg = fold_build2_loc (loc, LT_EXPR, TREE_TYPE (exp), arg,
		       build_real (TREE_TYPE (arg), dconst0));
    return expand_expr (arg, target, VOIDmode, EXPAND_NORMAL);
  }

  if (GET_MODE_SIZE (fmode) <= UNITS_PER_WORD)
    {
      imode = int_mode_for_mode (fmode);
      gcc_assert (imode != BLKmode);
      temp = gen_lowpart (imode, temp);
    }
  else
    {
      imode = word_mode;
      /* Handle targets with different FP word orders.  */
      if (FLOAT_WORDS_BIG_ENDIAN)
	word = (GET_MODE_BITSIZE (fmode) - bitpos) / BITS_PER_WORD;
      else
	word = bitpos / BITS_PER_WORD;
      temp = operand_subword_force (temp, word, fmode);
      bitpos = bitpos % BITS_PER_WORD;
    }

  /* Force the intermediate word_mode (or narrower) result into a
     register.  This avoids attempting to create paradoxical SUBREGs
     of floating point modes below.  */
  temp = force_reg (imode, temp);

  /* If the bitpos is within the "result mode" lowpart, the operation
     can be implement with a single bitwise AND.  Otherwise, we need
     a right shift and an AND.  */

  if (bitpos < GET_MODE_BITSIZE (rmode))
    {
      wide_int mask = wi::set_bit_in_zero (bitpos, GET_MODE_PRECISION (rmode));

      if (GET_MODE_SIZE (imode) > GET_MODE_SIZE (rmode))
	temp = gen_lowpart (rmode, temp);
      temp = expand_binop (rmode, and_optab, temp,
			   immed_wide_int_const (mask, rmode),
			   NULL_RTX, 1, OPTAB_LIB_WIDEN);
    }
  else
    {
      /* Perform a logical right shift to place the signbit in the least
	 significant bit, then truncate the result to the desired mode
	 and mask just this bit.  */
      temp = expand_shift (RSHIFT_EXPR, imode, temp, bitpos, NULL_RTX, 1);
      temp = gen_lowpart (rmode, temp);
      temp = expand_binop (rmode, and_optab, temp, const1_rtx,
			   NULL_RTX, 1, OPTAB_LIB_WIDEN);
    }

  return temp;
}

/* Expand fork or exec calls.  TARGET is the desired target of the
   call.  EXP is the call. FN is the
   identificator of the actual function.  IGNORE is nonzero if the
   value is to be ignored.  */

static rtx
expand_builtin_fork_or_exec (tree fn, tree exp, rtx target, int ignore)
{
  tree id, decl;
  tree call;

  /* If we are not profiling, just call the function.  */
  if (!profile_arc_flag)
    return NULL_RTX;

  /* Otherwise call the wrapper.  This should be equivalent for the rest of
     compiler, so the code does not diverge, and the wrapper may run the
     code necessary for keeping the profiling sane.  */

  switch (DECL_FUNCTION_CODE (fn))
    {
    case BUILT_IN_FORK:
      id = get_identifier ("__gcov_fork");
      break;

    case BUILT_IN_EXECL:
      id = get_identifier ("__gcov_execl");
      break;

    case BUILT_IN_EXECV:
      id = get_identifier ("__gcov_execv");
      break;

    case BUILT_IN_EXECLP:
      id = get_identifier ("__gcov_execlp");
      break;

    case BUILT_IN_EXECLE:
      id = get_identifier ("__gcov_execle");
      break;

    case BUILT_IN_EXECVP:
      id = get_identifier ("__gcov_execvp");
      break;

    case BUILT_IN_EXECVE:
      id = get_identifier ("__gcov_execve");
      break;

    default:
      gcc_unreachable ();
    }

  decl = build_decl (DECL_SOURCE_LOCATION (fn),
		     FUNCTION_DECL, id, TREE_TYPE (fn));
  DECL_EXTERNAL (decl) = 1;
  TREE_PUBLIC (decl) = 1;
  DECL_ARTIFICIAL (decl) = 1;
  TREE_NOTHROW (decl) = 1;
  DECL_VISIBILITY (decl) = VISIBILITY_DEFAULT;
  DECL_VISIBILITY_SPECIFIED (decl) = 1;
  call = rewrite_call_expr (EXPR_LOCATION (exp), exp, 0, decl, 0);
  return expand_call (call, target, ignore);
 }



/* Reconstitute a mode for a __sync intrinsic operation.  Since the type of
   the pointer in these functions is void*, the tree optimizers may remove
   casts.  The mode computed in expand_builtin isn't reliable either, due
   to __sync_bool_compare_and_swap.

   FCODE_DIFF should be fcode - base, where base is the FOO_1 code for the
   group of builtins.  This gives us log2 of the mode size.  */

static inline machine_mode
get_builtin_sync_mode (int fcode_diff)
{
  /* The size is not negotiable, so ask not to get BLKmode in return
     if the target indicates that a smaller size would be better.  */
  return mode_for_size (BITS_PER_UNIT << fcode_diff, MODE_INT, 0);
}

/* Expand the memory expression LOC and return the appropriate memory operand
   for the builtin_sync operations.  */

static rtx
get_builtin_sync_mem (tree loc, machine_mode mode)
{
  rtx addr, mem;

  addr = expand_expr (loc, NULL_RTX, ptr_mode, EXPAND_SUM);
  addr = convert_memory_address (Pmode, addr);

  /* Note that we explicitly do not want any alias information for this
     memory, so that we kill all other live memories.  Otherwise we don't
     satisfy the full barrier semantics of the intrinsic.  */
  mem = validize_mem (gen_rtx_MEM (mode, addr));

  /* The alignment needs to be at least according to that of the mode.  */
  set_mem_align (mem, MAX (GET_MODE_ALIGNMENT (mode),
			   get_pointer_alignment (loc)));
  set_mem_alias_set (mem, ALIAS_SET_MEMORY_BARRIER);
  MEM_VOLATILE_P (mem) = 1;

  return mem;
}

/* Make sure an argument is in the right mode.
   EXP is the tree argument. 
   MODE is the mode it should be in.  */

static rtx
expand_expr_force_mode (tree exp, machine_mode mode)
{
  rtx val;
  machine_mode old_mode;

  val = expand_expr (exp, NULL_RTX, mode, EXPAND_NORMAL);
  /* If VAL is promoted to a wider mode, convert it back to MODE.  Take care
     of CONST_INTs, where we know the old_mode only from the call argument.  */

  old_mode = GET_MODE (val);
  if (old_mode == VOIDmode)
    old_mode = TYPE_MODE (TREE_TYPE (exp));
  val = convert_modes (mode, old_mode, val, 1);
  return val;
}


/* Expand the __sync_xxx_and_fetch and __sync_fetch_and_xxx intrinsics.
   EXP is the CALL_EXPR.  CODE is the rtx code
   that corresponds to the arithmetic or logical operation from the name;
   an exception here is that NOT actually means NAND.  TARGET is an optional
   place for us to store the results; AFTER is true if this is the
   fetch_and_xxx form.  */

static rtx
expand_builtin_sync_operation (machine_mode mode, tree exp,
			       enum rtx_code code, bool after,
			       rtx target)
{
  rtx val, mem;
  location_t loc = EXPR_LOCATION (exp);

  if (code == NOT && warn_sync_nand)
    {
      tree fndecl = get_callee_fndecl (exp);
      enum built_in_function fcode = DECL_FUNCTION_CODE (fndecl);

      static bool warned_f_a_n, warned_n_a_f;

      switch (fcode)
	{
	case BUILT_IN_SYNC_FETCH_AND_NAND_1:
	case BUILT_IN_SYNC_FETCH_AND_NAND_2:
	case BUILT_IN_SYNC_FETCH_AND_NAND_4:
	case BUILT_IN_SYNC_FETCH_AND_NAND_8:
	case BUILT_IN_SYNC_FETCH_AND_NAND_16:
	  if (warned_f_a_n)
	    break;

	  fndecl = builtin_decl_implicit (BUILT_IN_SYNC_FETCH_AND_NAND_N);
	  inform (loc, "%qD changed semantics in GCC 4.4", fndecl);
	  warned_f_a_n = true;
	  break;

	case BUILT_IN_SYNC_NAND_AND_FETCH_1:
	case BUILT_IN_SYNC_NAND_AND_FETCH_2:
	case BUILT_IN_SYNC_NAND_AND_FETCH_4:
	case BUILT_IN_SYNC_NAND_AND_FETCH_8:
	case BUILT_IN_SYNC_NAND_AND_FETCH_16:
	  if (warned_n_a_f)
	    break;

	 fndecl = builtin_decl_implicit (BUILT_IN_SYNC_NAND_AND_FETCH_N);
	  inform (loc, "%qD changed semantics in GCC 4.4", fndecl);
	  warned_n_a_f = true;
	  break;

	default:
	  gcc_unreachable ();
	}
    }

  /* Expand the operands.  */
  mem = get_builtin_sync_mem (CALL_EXPR_ARG (exp, 0), mode);
  val = expand_expr_force_mode (CALL_EXPR_ARG (exp, 1), mode);

  return expand_atomic_fetch_op (target, mem, val, code, MEMMODEL_SYNC_SEQ_CST,
				 after);
}

/* Expand the __sync_val_compare_and_swap and __sync_bool_compare_and_swap
   intrinsics. EXP is the CALL_EXPR.  IS_BOOL is
   true if this is the boolean form.  TARGET is a place for us to store the
   results; this is NOT optional if IS_BOOL is true.  */

static rtx
expand_builtin_compare_and_swap (machine_mode mode, tree exp,
				 bool is_bool, rtx target)
{
  rtx old_val, new_val, mem;
  rtx *pbool, *poval;

  /* Expand the operands.  */
  mem = get_builtin_sync_mem (CALL_EXPR_ARG (exp, 0), mode);
  old_val = expand_expr_force_mode (CALL_EXPR_ARG (exp, 1), mode);
  new_val = expand_expr_force_mode (CALL_EXPR_ARG (exp, 2), mode);

  pbool = poval = NULL;
  if (target != const0_rtx)
    {
      if (is_bool)
	pbool = &target;
      else
	poval = &target;
    }
  if (!expand_atomic_compare_and_swap (pbool, poval, mem, old_val, new_val,
				       false, MEMMODEL_SYNC_SEQ_CST,
				       MEMMODEL_SYNC_SEQ_CST))
    return NULL_RTX;

  return target;
}

/* Expand the __sync_lock_test_and_set intrinsic.  Note that the most
   general form is actually an atomic exchange, and some targets only
   support a reduced form with the second argument being a constant 1.
   EXP is the CALL_EXPR; TARGET is an optional place for us to store
   the results.  */

static rtx
expand_builtin_sync_lock_test_and_set (machine_mode mode, tree exp,
				       rtx target)
{
  rtx val, mem;

  /* Expand the operands.  */
  mem = get_builtin_sync_mem (CALL_EXPR_ARG (exp, 0), mode);
  val = expand_expr_force_mode (CALL_EXPR_ARG (exp, 1), mode);

  return expand_sync_lock_test_and_set (target, mem, val);
}

/* Expand the __sync_lock_release intrinsic.  EXP is the CALL_EXPR.  */

static void
expand_builtin_sync_lock_release (machine_mode mode, tree exp)
{
  rtx mem;

  /* Expand the operands.  */
  mem = get_builtin_sync_mem (CALL_EXPR_ARG (exp, 0), mode);

  expand_atomic_store (mem, const0_rtx, MEMMODEL_SYNC_RELEASE, true);
}

/* Given an integer representing an ``enum memmodel'', verify its
   correctness and return the memory model enum.  */

static enum memmodel
get_memmodel (tree exp)
{
  rtx op;
  unsigned HOST_WIDE_INT val;
  source_location loc
    = expansion_point_location_if_in_system_header (input_location);

  /* If the parameter is not a constant, it's a run time value so we'll just
     convert it to MEMMODEL_SEQ_CST to avoid annoying runtime checking.  */
  if (TREE_CODE (exp) != INTEGER_CST)
    return MEMMODEL_SEQ_CST;

  op = expand_normal (exp);

  val = INTVAL (op);
  if (targetm.memmodel_check)
    val = targetm.memmodel_check (val);
  else if (val & ~MEMMODEL_MASK)
    {
      warning_at (loc, OPT_Winvalid_memory_model,
		  "unknown architecture specifier in memory model to builtin");
      return MEMMODEL_SEQ_CST;
    }

  /* Should never see a user explicit SYNC memodel model, so >= LAST works. */
  if (memmodel_base (val) >= MEMMODEL_LAST)
    {
      warning_at (loc, OPT_Winvalid_memory_model,
		  "invalid memory model argument to builtin");
      return MEMMODEL_SEQ_CST;
    }

  /* Workaround for Bugzilla 59448. GCC doesn't track consume properly, so
     be conservative and promote consume to acquire.  */
  if (val == MEMMODEL_CONSUME)
    val = MEMMODEL_ACQUIRE;

  return (enum memmodel) val;
}

/* Expand the __atomic_exchange intrinsic:
   	TYPE __atomic_exchange (TYPE *object, TYPE desired, enum memmodel)
   EXP is the CALL_EXPR.
   TARGET is an optional place for us to store the results.  */

static rtx
expand_builtin_atomic_exchange (machine_mode mode, tree exp, rtx target)
{
  rtx val, mem;
  enum memmodel model;

  model = get_memmodel (CALL_EXPR_ARG (exp, 2));

  if (!flag_inline_atomics)
    return NULL_RTX;

  /* Expand the operands.  */
  mem = get_builtin_sync_mem (CALL_EXPR_ARG (exp, 0), mode);
  val = expand_expr_force_mode (CALL_EXPR_ARG (exp, 1), mode);

  return expand_atomic_exchange (target, mem, val, model);
}

/* Expand the __atomic_compare_exchange intrinsic:
   	bool __atomic_compare_exchange (TYPE *object, TYPE *expect, 
					TYPE desired, BOOL weak, 
					enum memmodel success,
					enum memmodel failure)
   EXP is the CALL_EXPR.
   TARGET is an optional place for us to store the results.  */

static rtx
expand_builtin_atomic_compare_exchange (machine_mode mode, tree exp, 
					rtx target)
{
  rtx expect, desired, mem, oldval;
  rtx_code_label *label;
  enum memmodel success, failure;
  tree weak;
  bool is_weak;
  source_location loc
    = expansion_point_location_if_in_system_header (input_location);

  success = get_memmodel (CALL_EXPR_ARG (exp, 4));
  failure = get_memmodel (CALL_EXPR_ARG (exp, 5));

  if (failure > success)
    {
      warning_at (loc, OPT_Winvalid_memory_model,
		  "failure memory model cannot be stronger than success "
		  "memory model for %<__atomic_compare_exchange%>");
      success = MEMMODEL_SEQ_CST;
    }
 
  if (is_mm_release (failure) || is_mm_acq_rel (failure))
    {
      warning_at (loc, OPT_Winvalid_memory_model,
		  "invalid failure memory model for "
		  "%<__atomic_compare_exchange%>");
      failure = MEMMODEL_SEQ_CST;
      success = MEMMODEL_SEQ_CST;
    }

 
  if (!flag_inline_atomics)
    return NULL_RTX;

  /* Expand the operands.  */
  mem = get_builtin_sync_mem (CALL_EXPR_ARG (exp, 0), mode);

  expect = expand_normal (CALL_EXPR_ARG (exp, 1));
  expect = convert_memory_address (Pmode, expect);
  expect = gen_rtx_MEM (mode, expect);
  desired = expand_expr_force_mode (CALL_EXPR_ARG (exp, 2), mode);

  weak = CALL_EXPR_ARG (exp, 3);
  is_weak = false;
  if (tree_fits_shwi_p (weak) && tree_to_shwi (weak) != 0)
    is_weak = true;

  if (target == const0_rtx)
    target = NULL;

  /* Lest the rtl backend create a race condition with an imporoper store
     to memory, always create a new pseudo for OLDVAL.  */
  oldval = NULL;

  if (!expand_atomic_compare_and_swap (&target, &oldval, mem, expect, desired,
				       is_weak, success, failure))
    return NULL_RTX;

  /* Conditionally store back to EXPECT, lest we create a race condition
     with an improper store to memory.  */
  /* ??? With a rearrangement of atomics at the gimple level, we can handle
     the normal case where EXPECT is totally private, i.e. a register.  At
     which point the store can be unconditional.  */
  label = gen_label_rtx ();
  emit_cmp_and_jump_insns (target, const0_rtx, NE, NULL,
			   GET_MODE (target), 1, label);
  emit_move_insn (expect, oldval);
  emit_label (label);

  return target;
}

/* Helper function for expand_ifn_atomic_compare_exchange - expand
   internal ATOMIC_COMPARE_EXCHANGE call into __atomic_compare_exchange_N
   call.  The weak parameter must be dropped to match the expected parameter
   list and the expected argument changed from value to pointer to memory
   slot.  */

static void
expand_ifn_atomic_compare_exchange_into_call (gcall *call, machine_mode mode)
{
  unsigned int z;
  vec<tree, va_gc> *vec;

  vec_alloc (vec, 5);
  vec->quick_push (gimple_call_arg (call, 0));
  tree expected = gimple_call_arg (call, 1);
  rtx x = assign_stack_temp_for_type (mode, GET_MODE_SIZE (mode),
				      TREE_TYPE (expected));
  rtx expd = expand_expr (expected, x, mode, EXPAND_NORMAL);
  if (expd != x)
    emit_move_insn (x, expd);
  tree v = make_tree (TREE_TYPE (expected), x);
  vec->quick_push (build1 (ADDR_EXPR,
			   build_pointer_type (TREE_TYPE (expected)), v));
  vec->quick_push (gimple_call_arg (call, 2));
  /* Skip the boolean weak parameter.  */
  for (z = 4; z < 6; z++)
    vec->quick_push (gimple_call_arg (call, z));
  built_in_function fncode
    = (built_in_function) ((int) BUILT_IN_ATOMIC_COMPARE_EXCHANGE_1
			   + exact_log2 (GET_MODE_SIZE (mode)));
  tree fndecl = builtin_decl_explicit (fncode);
  tree fn = build1 (ADDR_EXPR, build_pointer_type (TREE_TYPE (fndecl)),
		    fndecl);
  tree exp = build_call_vec (boolean_type_node, fn, vec);
  tree lhs = gimple_call_lhs (call);
  rtx boolret = expand_call (exp, NULL_RTX, lhs == NULL_TREE);
  if (lhs)
    {
      rtx target = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);
      if (GET_MODE (boolret) != mode)
	boolret = convert_modes (mode, GET_MODE (boolret), boolret, 1);
      x = force_reg (mode, x);
      write_complex_part (target, boolret, true);
      write_complex_part (target, x, false);
    }
}

/* Expand IFN_ATOMIC_COMPARE_EXCHANGE internal function.  */

void
expand_ifn_atomic_compare_exchange (gcall *call)
{
  int size = tree_to_shwi (gimple_call_arg (call, 3)) & 255;
  gcc_assert (size == 1 || size == 2 || size == 4 || size == 8 || size == 16);
  machine_mode mode = mode_for_size (BITS_PER_UNIT * size, MODE_INT, 0);
  rtx expect, desired, mem, oldval, boolret;
  enum memmodel success, failure;
  tree lhs;
  bool is_weak;
  source_location loc
    = expansion_point_location_if_in_system_header (gimple_location (call));

  success = get_memmodel (gimple_call_arg (call, 4));
  failure = get_memmodel (gimple_call_arg (call, 5));

  if (failure > success)
    {
      warning_at (loc, OPT_Winvalid_memory_model,
		  "failure memory model cannot be stronger than success "
		  "memory model for %<__atomic_compare_exchange%>");
      success = MEMMODEL_SEQ_CST;
    }

  if (is_mm_release (failure) || is_mm_acq_rel (failure))
    {
      warning_at (loc, OPT_Winvalid_memory_model,
		  "invalid failure memory model for "
		  "%<__atomic_compare_exchange%>");
      failure = MEMMODEL_SEQ_CST;
      success = MEMMODEL_SEQ_CST;
    }

  if (!flag_inline_atomics)
    {
      expand_ifn_atomic_compare_exchange_into_call (call, mode);
      return;
    }

  /* Expand the operands.  */
  mem = get_builtin_sync_mem (gimple_call_arg (call, 0), mode);

  expect = expand_expr_force_mode (gimple_call_arg (call, 1), mode);
  desired = expand_expr_force_mode (gimple_call_arg (call, 2), mode);

  is_weak = (tree_to_shwi (gimple_call_arg (call, 3)) & 256) != 0;

  boolret = NULL;
  oldval = NULL;

  if (!expand_atomic_compare_and_swap (&boolret, &oldval, mem, expect, desired,
				       is_weak, success, failure))
    {
      expand_ifn_atomic_compare_exchange_into_call (call, mode);
      return;
    }

  lhs = gimple_call_lhs (call);
  if (lhs)
    {
      rtx target = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);
      if (GET_MODE (boolret) != mode)
	boolret = convert_modes (mode, GET_MODE (boolret), boolret, 1);
      write_complex_part (target, boolret, true);
      write_complex_part (target, oldval, false);
    }
}

/* Expand the __atomic_load intrinsic:
   	TYPE __atomic_load (TYPE *object, enum memmodel)
   EXP is the CALL_EXPR.
   TARGET is an optional place for us to store the results.  */

static rtx
expand_builtin_atomic_load (machine_mode mode, tree exp, rtx target)
{
  rtx mem;
  enum memmodel model;

  model = get_memmodel (CALL_EXPR_ARG (exp, 1));
  if (is_mm_release (model) || is_mm_acq_rel (model))
    {
      source_location loc
	= expansion_point_location_if_in_system_header (input_location);
      warning_at (loc, OPT_Winvalid_memory_model,
		  "invalid memory model for %<__atomic_load%>");
      model = MEMMODEL_SEQ_CST;
    }

  if (!flag_inline_atomics)
    return NULL_RTX;

  /* Expand the operand.  */
  mem = get_builtin_sync_mem (CALL_EXPR_ARG (exp, 0), mode);

  return expand_atomic_load (target, mem, model);
}


/* Expand the __atomic_store intrinsic:
   	void __atomic_store (TYPE *object, TYPE desired, enum memmodel)
   EXP is the CALL_EXPR.
   TARGET is an optional place for us to store the results.  */

static rtx
expand_builtin_atomic_store (machine_mode mode, tree exp)
{
  rtx mem, val;
  enum memmodel model;

  model = get_memmodel (CALL_EXPR_ARG (exp, 2));
  if (!(is_mm_relaxed (model) || is_mm_seq_cst (model)
	|| is_mm_release (model)))
    {
      source_location loc
	= expansion_point_location_if_in_system_header (input_location);
      warning_at (loc, OPT_Winvalid_memory_model,
		  "invalid memory model for %<__atomic_store%>");
      model = MEMMODEL_SEQ_CST;
    }

  if (!flag_inline_atomics)
    return NULL_RTX;

  /* Expand the operands.  */
  mem = get_builtin_sync_mem (CALL_EXPR_ARG (exp, 0), mode);
  val = expand_expr_force_mode (CALL_EXPR_ARG (exp, 1), mode);

  return expand_atomic_store (mem, val, model, false);
}

/* Expand the __atomic_fetch_XXX intrinsic:
   	TYPE __atomic_fetch_XXX (TYPE *object, TYPE val, enum memmodel)
   EXP is the CALL_EXPR.
   TARGET is an optional place for us to store the results.
   CODE is the operation, PLUS, MINUS, ADD, XOR, or IOR.
   FETCH_AFTER is true if returning the result of the operation.
   FETCH_AFTER is false if returning the value before the operation.
   IGNORE is true if the result is not used.
   EXT_CALL is the correct builtin for an external call if this cannot be
   resolved to an instruction sequence.  */

static rtx
expand_builtin_atomic_fetch_op (machine_mode mode, tree exp, rtx target,
				enum rtx_code code, bool fetch_after,
				bool ignore, enum built_in_function ext_call)
{
  rtx val, mem, ret;
  enum memmodel model;
  tree fndecl;
  tree addr;

  model = get_memmodel (CALL_EXPR_ARG (exp, 2));

  /* Expand the operands.  */
  mem = get_builtin_sync_mem (CALL_EXPR_ARG (exp, 0), mode);
  val = expand_expr_force_mode (CALL_EXPR_ARG (exp, 1), mode);

  /* Only try generating instructions if inlining is turned on.  */
  if (flag_inline_atomics)
    {
      ret = expand_atomic_fetch_op (target, mem, val, code, model, fetch_after);
      if (ret)
	return ret;
    }

  /* Return if a different routine isn't needed for the library call.  */
  if (ext_call == BUILT_IN_NONE)
    return NULL_RTX;

  /* Change the call to the specified function.  */
  fndecl = get_callee_fndecl (exp);
  addr = CALL_EXPR_FN (exp);
  STRIP_NOPS (addr);

  gcc_assert (TREE_OPERAND (addr, 0) == fndecl);
  TREE_OPERAND (addr, 0) = builtin_decl_explicit (ext_call);

  /* If we will emit code after the call, the call can not be a tail call.
     If it is emitted as a tail call, a barrier is emitted after it, and
     then all trailing code is removed.  */
  if (!ignore)
    CALL_EXPR_TAILCALL (exp) = 0;

  /* Expand the call here so we can emit trailing code.  */
  ret = expand_call (exp, target, ignore);

  /* Replace the original function just in case it matters.  */
  TREE_OPERAND (addr, 0) = fndecl;

  /* Then issue the arithmetic correction to return the right result.  */
  if (!ignore)
    {
      if (code == NOT)
	{
	  ret = expand_simple_binop (mode, AND, ret, val, NULL_RTX, true,
				     OPTAB_LIB_WIDEN);
	  ret = expand_simple_unop (mode, NOT, ret, target, true);
	}
      else
	ret = expand_simple_binop (mode, code, ret, val, target, true,
				   OPTAB_LIB_WIDEN);
    }
  return ret;
}

/* Expand IFN_ATOMIC_BIT_TEST_AND_* internal function.  */

void
expand_ifn_atomic_bit_test_and (gcall *call)
{
  tree ptr = gimple_call_arg (call, 0);
  tree bit = gimple_call_arg (call, 1);
  tree flag = gimple_call_arg (call, 2);
  tree lhs = gimple_call_lhs (call);
  enum memmodel model = MEMMODEL_SYNC_SEQ_CST;
  machine_mode mode = TYPE_MODE (TREE_TYPE (flag));
  enum rtx_code code;
  optab optab;
  struct expand_operand ops[5];

  gcc_assert (flag_inline_atomics);

  if (gimple_call_num_args (call) == 4)
    model = get_memmodel (gimple_call_arg (call, 3));

  rtx mem = get_builtin_sync_mem (ptr, mode);
  rtx val = expand_expr_force_mode (bit, mode);

  switch (gimple_call_internal_fn (call))
    {
    case IFN_ATOMIC_BIT_TEST_AND_SET:
      code = IOR;
      optab = atomic_bit_test_and_set_optab;
      break;
    case IFN_ATOMIC_BIT_TEST_AND_COMPLEMENT:
      code = XOR;
      optab = atomic_bit_test_and_complement_optab;
      break;
    case IFN_ATOMIC_BIT_TEST_AND_RESET:
      code = AND;
      optab = atomic_bit_test_and_reset_optab;
      break;
    default:
      gcc_unreachable ();
    }

  if (lhs == NULL_TREE)
    {
      val = expand_simple_binop (mode, ASHIFT, const1_rtx,
				 val, NULL_RTX, true, OPTAB_DIRECT);
      if (code == AND)
	val = expand_simple_unop (mode, NOT, val, NULL_RTX, true);
      expand_atomic_fetch_op (const0_rtx, mem, val, code, model, false);
      return;
    }

  rtx target = expand_expr (lhs, NULL_RTX, VOIDmode, EXPAND_WRITE);
  enum insn_code icode = direct_optab_handler (optab, mode);
  gcc_assert (icode != CODE_FOR_nothing);
  create_output_operand (&ops[0], target, mode);
  create_fixed_operand (&ops[1], mem);
  create_convert_operand_to (&ops[2], val, mode, true);
  create_integer_operand (&ops[3], model);
  create_integer_operand (&ops[4], integer_onep (flag));
  if (maybe_expand_insn (icode, 5, ops))
    return;

  rtx bitval = val;
  val = expand_simple_binop (mode, ASHIFT, const1_rtx,
			     val, NULL_RTX, true, OPTAB_DIRECT);
  rtx maskval = val;
  if (code == AND)
    val = expand_simple_unop (mode, NOT, val, NULL_RTX, true);
  rtx result = expand_atomic_fetch_op (gen_reg_rtx (mode), mem, val,
				       code, model, false);
  if (integer_onep (flag))
    {
      result = expand_simple_binop (mode, ASHIFTRT, result, bitval,
				    NULL_RTX, true, OPTAB_DIRECT);
      result = expand_simple_binop (mode, AND, result, const1_rtx, target,
				    true, OPTAB_DIRECT);
    }
  else
    result = expand_simple_binop (mode, AND, result, maskval, target, true,
				  OPTAB_DIRECT);
  if (result != target)
    emit_move_insn (target, result);
}

/* Expand an atomic clear operation.
	void _atomic_clear (BOOL *obj, enum memmodel)
   EXP is the call expression.  */

static rtx
expand_builtin_atomic_clear (tree exp) 
{
  machine_mode mode;
  rtx mem, ret;
  enum memmodel model;

  mode = mode_for_size (BOOL_TYPE_SIZE, MODE_INT, 0);
  mem = get_builtin_sync_mem (CALL_EXPR_ARG (exp, 0), mode);
  model = get_memmodel (CALL_EXPR_ARG (exp, 1));

  if (is_mm_consume (model) || is_mm_acquire (model) || is_mm_acq_rel (model))
    {
      source_location loc
	= expansion_point_location_if_in_system_header (input_location);
      warning_at (loc, OPT_Winvalid_memory_model,
		  "invalid memory model for %<__atomic_store%>");
      model = MEMMODEL_SEQ_CST;
    }

  /* Try issuing an __atomic_store, and allow fallback to __sync_lock_release.
     Failing that, a store is issued by __atomic_store.  The only way this can
     fail is if the bool type is larger than a word size.  Unlikely, but
     handle it anyway for completeness.  Assume a single threaded model since
     there is no atomic support in this case, and no barriers are required.  */
  ret = expand_atomic_store (mem, const0_rtx, model, true);
  if (!ret)
    emit_move_insn (mem, const0_rtx);
  return const0_rtx;
}

/* Expand an atomic test_and_set operation.
	bool _atomic_test_and_set (BOOL *obj, enum memmodel)
   EXP is the call expression.  */

static rtx
expand_builtin_atomic_test_and_set (tree exp, rtx target)
{
  rtx mem;
  enum memmodel model;
  machine_mode mode;

  mode = mode_for_size (BOOL_TYPE_SIZE, MODE_INT, 0);
  mem = get_builtin_sync_mem (CALL_EXPR_ARG (exp, 0), mode);
  model = get_memmodel (CALL_EXPR_ARG (exp, 1));

  return expand_atomic_test_and_set (target, mem, model);
}


/* Return true if (optional) argument ARG1 of size ARG0 is always lock free on
   this architecture.  If ARG1 is NULL, use typical alignment for size ARG0.  */

static tree
fold_builtin_atomic_always_lock_free (tree arg0, tree arg1)
{
  int size;
  machine_mode mode;
  unsigned int mode_align, type_align;

  if (TREE_CODE (arg0) != INTEGER_CST)
    return NULL_TREE;

  size = INTVAL (expand_normal (arg0)) * BITS_PER_UNIT;
  mode = mode_for_size (size, MODE_INT, 0);
  mode_align = GET_MODE_ALIGNMENT (mode);

  if (TREE_CODE (arg1) == INTEGER_CST)
    {
      unsigned HOST_WIDE_INT val = UINTVAL (expand_normal (arg1));

      /* Either this argument is null, or it's a fake pointer encoding
         the alignment of the object.  */
      val = least_bit_hwi (val);
      val *= BITS_PER_UNIT;

      if (val == 0 || mode_align < val)
        type_align = mode_align;
      else
        type_align = val;
    }
  else
    {
      tree ttype = TREE_TYPE (arg1);

      /* This function is usually invoked and folded immediately by the front
	 end before anything else has a chance to look at it.  The pointer
	 parameter at this point is usually cast to a void *, so check for that
	 and look past the cast.  */
      if (CONVERT_EXPR_P (arg1)
	  && POINTER_TYPE_P (ttype)
	  && VOID_TYPE_P (TREE_TYPE (ttype))
	  && POINTER_TYPE_P (TREE_TYPE (TREE_OPERAND (arg1, 0))))
	arg1 = TREE_OPERAND (arg1, 0);

      ttype = TREE_TYPE (arg1);
      gcc_assert (POINTER_TYPE_P (ttype));

      /* Get the underlying type of the object.  */
      ttype = TREE_TYPE (ttype);
      type_align = TYPE_ALIGN (ttype);
    }

  /* If the object has smaller alignment, the lock free routines cannot
     be used.  */
  if (type_align < mode_align)
    return boolean_false_node;

  /* Check if a compare_and_swap pattern exists for the mode which represents
     the required size.  The pattern is not allowed to fail, so the existence
     of the pattern indicates support is present.  Also require that an
     atomic load exists for the required size.  */
  if (can_compare_and_swap_p (mode, true) && can_atomic_load_p (mode))
    return boolean_true_node;
  else
    return boolean_false_node;
}

/* Return true if the parameters to call EXP represent an object which will
   always generate lock free instructions.  The first argument represents the
   size of the object, and the second parameter is a pointer to the object 
   itself.  If NULL is passed for the object, then the result is based on 
   typical alignment for an object of the specified size.  Otherwise return 
   false.  */

static rtx
expand_builtin_atomic_always_lock_free (tree exp)
{
  tree size;
  tree arg0 = CALL_EXPR_ARG (exp, 0);
  tree arg1 = CALL_EXPR_ARG (exp, 1);

  if (TREE_CODE (arg0) != INTEGER_CST)
    {
      error ("non-constant argument 1 to __atomic_always_lock_free");
      return const0_rtx;
    }

  size = fold_builtin_atomic_always_lock_free (arg0, arg1);
  if (size == boolean_true_node)
    return const1_rtx;
  return const0_rtx;
}

/* Return a one or zero if it can be determined that object ARG1 of size ARG 
   is lock free on this architecture.  */

static tree
fold_builtin_atomic_is_lock_free (tree arg0, tree arg1)
{
  if (!flag_inline_atomics)
    return NULL_TREE;
  
  /* If it isn't always lock free, don't generate a result.  */
  if (fold_builtin_atomic_always_lock_free (arg0, arg1) == boolean_true_node)
    return boolean_true_node;

  return NULL_TREE;
}

/* Return true if the parameters to call EXP represent an object which will
   always generate lock free instructions.  The first argument represents the
   size of the object, and the second parameter is a pointer to the object 
   itself.  If NULL is passed for the object, then the result is based on 
   typical alignment for an object of the specified size.  Otherwise return 
   NULL*/

static rtx
expand_builtin_atomic_is_lock_free (tree exp)
{
  tree size;
  tree arg0 = CALL_EXPR_ARG (exp, 0);
  tree arg1 = CALL_EXPR_ARG (exp, 1);

  if (!INTEGRAL_TYPE_P (TREE_TYPE (arg0)))
    {
      error ("non-integer argument 1 to __atomic_is_lock_free");
      return NULL_RTX;
    }

  if (!flag_inline_atomics)
    return NULL_RTX; 

  /* If the value is known at compile time, return the RTX for it.  */
  size = fold_builtin_atomic_is_lock_free (arg0, arg1);
  if (size == boolean_true_node)
    return const1_rtx;

  return NULL_RTX;
}

/* Expand the __atomic_thread_fence intrinsic:
   	void __atomic_thread_fence (enum memmodel)
   EXP is the CALL_EXPR.  */

static void
expand_builtin_atomic_thread_fence (tree exp)
{
  enum memmodel model = get_memmodel (CALL_EXPR_ARG (exp, 0));
  expand_mem_thread_fence (model);
}

/* Expand the __atomic_signal_fence intrinsic:
   	void __atomic_signal_fence (enum memmodel)
   EXP is the CALL_EXPR.  */

static void
expand_builtin_atomic_signal_fence (tree exp)
{
  enum memmodel model = get_memmodel (CALL_EXPR_ARG (exp, 0));
  expand_mem_signal_fence (model);
}

/* Expand the __sync_synchronize intrinsic.  */

static void
expand_builtin_sync_synchronize (void)
{
  expand_mem_thread_fence (MEMMODEL_SYNC_SEQ_CST);
}

static rtx
expand_builtin_thread_pointer (tree exp, rtx target)
{
  enum insn_code icode;
  if (!validate_arglist (exp, VOID_TYPE))
    return const0_rtx;
  icode = direct_optab_handler (get_thread_pointer_optab, Pmode);
  if (icode != CODE_FOR_nothing)
    {
      struct expand_operand op;
      /* If the target is not sutitable then create a new target. */
      if (target == NULL_RTX
	  || !REG_P (target)
	  || GET_MODE (target) != Pmode)
	target = gen_reg_rtx (Pmode);
      create_output_operand (&op, target, Pmode);
      expand_insn (icode, 1, &op);
      return target;
    }
  error ("__builtin_thread_pointer is not supported on this target");
  return const0_rtx;
}

static void
expand_builtin_set_thread_pointer (tree exp)
{
  enum insn_code icode;
  if (!validate_arglist (exp, POINTER_TYPE, VOID_TYPE))
    return;
  icode = direct_optab_handler (set_thread_pointer_optab, Pmode);
  if (icode != CODE_FOR_nothing)
    {
      struct expand_operand op;
      rtx val = expand_expr (CALL_EXPR_ARG (exp, 0), NULL_RTX,
			     Pmode, EXPAND_NORMAL);      
      create_input_operand (&op, val, Pmode);
      expand_insn (icode, 1, &op);
      return;
    }
  error ("__builtin_set_thread_pointer is not supported on this target");
}


/* Emit code to restore the current value of stack.  */

static void
expand_stack_restore (tree var)
{
  rtx_insn *prev;
  rtx sa = expand_normal (var);

  sa = convert_memory_address (Pmode, sa);

  prev = get_last_insn ();
  emit_stack_restore (SAVE_BLOCK, sa);

  record_new_stack_level ();

  fixup_args_size_notes (prev, get_last_insn (), 0);
}

/* Emit code to save the current value of stack.  */

static rtx
expand_stack_save (void)
{
  rtx ret = NULL_RTX;

  emit_stack_save (SAVE_BLOCK, &ret);
  return ret;
}


/* Expand an expression EXP that calls a built-in function,
   with result going to TARGET if that's convenient
   (and in mode MODE if that's convenient).
   SUBTARGET may be used as the target for computing one of EXP's operands.
   IGNORE is nonzero if the value is to be ignored.  */

rtx
expand_builtin (tree exp, rtx target, rtx subtarget, machine_mode mode,
		int ignore)
{
  tree fndecl = get_callee_fndecl (exp);
  enum built_in_function fcode = DECL_FUNCTION_CODE (fndecl);
  machine_mode target_mode = TYPE_MODE (TREE_TYPE (exp));
  int flags;

  if (DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_MD)
    return targetm.expand_builtin (exp, target, subtarget, mode, ignore);

  /* When ASan is enabled, we don't want to expand some memory/string
     builtins and rely on libsanitizer's hooks.  This allows us to avoid
     redundant checks and be sure, that possible overflow will be detected
     by ASan.  */

  if ((flag_sanitize & SANITIZE_ADDRESS) && asan_intercepted_p (fcode))
    return expand_call (exp, target, ignore);

  /* When not optimizing, generate calls to library functions for a certain
     set of builtins.  */
  if (!optimize
      && !called_as_built_in (fndecl)
      && fcode != BUILT_IN_FORK
      && fcode != BUILT_IN_EXECL
      && fcode != BUILT_IN_EXECV
      && fcode != BUILT_IN_EXECLP
      && fcode != BUILT_IN_EXECLE
      && fcode != BUILT_IN_EXECVP
      && fcode != BUILT_IN_EXECVE
      && fcode != BUILT_IN_ALLOCA
      && fcode != BUILT_IN_ALLOCA_WITH_ALIGN
      && fcode != BUILT_IN_FREE
      && fcode != BUILT_IN_CHKP_SET_PTR_BOUNDS
      && fcode != BUILT_IN_CHKP_INIT_PTR_BOUNDS
      && fcode != BUILT_IN_CHKP_NULL_PTR_BOUNDS
      && fcode != BUILT_IN_CHKP_COPY_PTR_BOUNDS
      && fcode != BUILT_IN_CHKP_NARROW_PTR_BOUNDS
      && fcode != BUILT_IN_CHKP_STORE_PTR_BOUNDS
      && fcode != BUILT_IN_CHKP_CHECK_PTR_LBOUNDS
      && fcode != BUILT_IN_CHKP_CHECK_PTR_UBOUNDS
      && fcode != BUILT_IN_CHKP_CHECK_PTR_BOUNDS
      && fcode != BUILT_IN_CHKP_GET_PTR_LBOUND
      && fcode != BUILT_IN_CHKP_GET_PTR_UBOUND
      && fcode != BUILT_IN_CHKP_BNDRET)
    return expand_call (exp, target, ignore);

  /* The built-in function expanders test for target == const0_rtx
     to determine whether the function's result will be ignored.  */
  if (ignore)
    target = const0_rtx;

  /* If the result of a pure or const built-in function is ignored, and
     none of its arguments are volatile, we can avoid expanding the
     built-in call and just evaluate the arguments for side-effects.  */
  if (target == const0_rtx
      && ((flags = flags_from_decl_or_type (fndecl)) & (ECF_CONST | ECF_PURE))
      && !(flags & ECF_LOOPING_CONST_OR_PURE))
    {
      bool volatilep = false;
      tree arg;
      call_expr_arg_iterator iter;

      FOR_EACH_CALL_EXPR_ARG (arg, iter, exp)
	if (TREE_THIS_VOLATILE (arg))
	  {
	    volatilep = true;
	    break;
	  }

      if (! volatilep)
	{
	  FOR_EACH_CALL_EXPR_ARG (arg, iter, exp)
	    expand_expr (arg, const0_rtx, VOIDmode, EXPAND_NORMAL);
	  return const0_rtx;
	}
    }

  /* expand_builtin_with_bounds is supposed to be used for
     instrumented builtin calls.  */
  gcc_assert (!CALL_WITH_BOUNDS_P (exp));

  switch (fcode)
    {
    CASE_FLT_FN (BUILT_IN_FABS):
    CASE_FLT_FN_FLOATN_NX (BUILT_IN_FABS):
    case BUILT_IN_FABSD32:
    case BUILT_IN_FABSD64:
    case BUILT_IN_FABSD128:
      target = expand_builtin_fabs (exp, target, subtarget);
      if (target)
	return target;
      break;

    CASE_FLT_FN (BUILT_IN_COPYSIGN):
    CASE_FLT_FN_FLOATN_NX (BUILT_IN_COPYSIGN):
      target = expand_builtin_copysign (exp, target, subtarget);
      if (target)
	return target;
      break;

      /* Just do a normal library call if we were unable to fold
	 the values.  */
    CASE_FLT_FN (BUILT_IN_CABS):
      break;

    CASE_FLT_FN (BUILT_IN_FMA):
      target = expand_builtin_mathfn_ternary (exp, target, subtarget);
      if (target)
	return target;
      break;

    CASE_FLT_FN (BUILT_IN_ILOGB):
      if (! flag_unsafe_math_optimizations)
	break;
      gcc_fallthrough ();
    CASE_FLT_FN (BUILT_IN_ISINF):
    CASE_FLT_FN (BUILT_IN_FINITE):
    case BUILT_IN_ISFINITE:
    case BUILT_IN_ISNORMAL:
      target = expand_builtin_interclass_mathfn (exp, target);
      if (target)
	return target;
      break;

    CASE_FLT_FN (BUILT_IN_ICEIL):
    CASE_FLT_FN (BUILT_IN_LCEIL):
    CASE_FLT_FN (BUILT_IN_LLCEIL):
    CASE_FLT_FN (BUILT_IN_LFLOOR):
    CASE_FLT_FN (BUILT_IN_IFLOOR):
    CASE_FLT_FN (BUILT_IN_LLFLOOR):
      target = expand_builtin_int_roundingfn (exp, target);
      if (target)
	return target;
      break;

    CASE_FLT_FN (BUILT_IN_IRINT):
    CASE_FLT_FN (BUILT_IN_LRINT):
    CASE_FLT_FN (BUILT_IN_LLRINT):
    CASE_FLT_FN (BUILT_IN_IROUND):
    CASE_FLT_FN (BUILT_IN_LROUND):
    CASE_FLT_FN (BUILT_IN_LLROUND):
      target = expand_builtin_int_roundingfn_2 (exp, target);
      if (target)
	return target;
      break;

    CASE_FLT_FN (BUILT_IN_POWI):
      target = expand_builtin_powi (exp, target);
      if (target)
	return target;
      break;

    CASE_FLT_FN (BUILT_IN_CEXPI):
      target = expand_builtin_cexpi (exp, target);
      gcc_assert (target);
      return target;

    CASE_FLT_FN (BUILT_IN_SIN):
    CASE_FLT_FN (BUILT_IN_COS):
      if (! flag_unsafe_math_optimizations)
	break;
      target = expand_builtin_mathfn_3 (exp, target, subtarget);
      if (target)
	return target;
      break;

    CASE_FLT_FN (BUILT_IN_SINCOS):
      if (! flag_unsafe_math_optimizations)
	break;
      target = expand_builtin_sincos (exp);
      if (target)
	return target;
      break;

    case BUILT_IN_APPLY_ARGS:
      return expand_builtin_apply_args ();

      /* __builtin_apply (FUNCTION, ARGUMENTS, ARGSIZE) invokes
	 FUNCTION with a copy of the parameters described by
	 ARGUMENTS, and ARGSIZE.  It returns a block of memory
	 allocated on the stack into which is stored all the registers
	 that might possibly be used for returning the result of a
	 function.  ARGUMENTS is the value returned by
	 __builtin_apply_args.  ARGSIZE is the number of bytes of
	 arguments that must be copied.  ??? How should this value be
	 computed?  We'll also need a safe worst case value for varargs
	 functions.  */
    case BUILT_IN_APPLY:
      if (!validate_arglist (exp, POINTER_TYPE,
			     POINTER_TYPE, INTEGER_TYPE, VOID_TYPE)
	  && !validate_arglist (exp, REFERENCE_TYPE,
				POINTER_TYPE, INTEGER_TYPE, VOID_TYPE))
	return const0_rtx;
      else
	{
	  rtx ops[3];

	  ops[0] = expand_normal (CALL_EXPR_ARG (exp, 0));
	  ops[1] = expand_normal (CALL_EXPR_ARG (exp, 1));
	  ops[2] = expand_normal (CALL_EXPR_ARG (exp, 2));

	  return expand_builtin_apply (ops[0], ops[1], ops[2]);
	}

      /* __builtin_return (RESULT) causes the function to return the
	 value described by RESULT.  RESULT is address of the block of
	 memory returned by __builtin_apply.  */
    case BUILT_IN_RETURN:
      if (validate_arglist (exp, POINTER_TYPE, VOID_TYPE))
	expand_builtin_return (expand_normal (CALL_EXPR_ARG (exp, 0)));
      return const0_rtx;

    case BUILT_IN_SAVEREGS:
      return expand_builtin_saveregs ();

    case BUILT_IN_VA_ARG_PACK:
      /* All valid uses of __builtin_va_arg_pack () are removed during
	 inlining.  */
      error ("%Kinvalid use of %<__builtin_va_arg_pack ()%>", exp);
      return const0_rtx;

    case BUILT_IN_VA_ARG_PACK_LEN:
      /* All valid uses of __builtin_va_arg_pack_len () are removed during
	 inlining.  */
      error ("%Kinvalid use of %<__builtin_va_arg_pack_len ()%>", exp);
      return const0_rtx;

      /* Return the address of the first anonymous stack arg.  */
    case BUILT_IN_NEXT_ARG:
      if (fold_builtin_next_arg (exp, false))
	return const0_rtx;
      return expand_builtin_next_arg ();

    case BUILT_IN_CLEAR_CACHE:
      target = expand_builtin___clear_cache (exp);
      if (target)
        return target;
      break;

    case BUILT_IN_CLASSIFY_TYPE:
      return expand_builtin_classify_type (exp);

    case BUILT_IN_CONSTANT_P:
      return const0_rtx;

    case BUILT_IN_FRAME_ADDRESS:
    case BUILT_IN_RETURN_ADDRESS:
      return expand_builtin_frame_address (fndecl, exp);

    /* Returns the address of the area where the structure is returned.
       0 otherwise.  */
    case BUILT_IN_AGGREGATE_INCOMING_ADDRESS:
      if (call_expr_nargs (exp) != 0
	  || ! AGGREGATE_TYPE_P (TREE_TYPE (TREE_TYPE (current_function_decl)))
	  || !MEM_P (DECL_RTL (DECL_RESULT (current_function_decl))))
	return const0_rtx;
      else
	return XEXP (DECL_RTL (DECL_RESULT (current_function_decl)), 0);

    case BUILT_IN_ALLOCA:
    case BUILT_IN_ALLOCA_WITH_ALIGN:
      target = expand_builtin_alloca (exp);
      if (target)
	return target;
      break;

    case BUILT_IN_ASAN_ALLOCAS_UNPOISON:
      return expand_asan_emit_allocas_unpoison (exp);

    case BUILT_IN_STACK_SAVE:
      return expand_stack_save ();

    case BUILT_IN_STACK_RESTORE:
      expand_stack_restore (CALL_EXPR_ARG (exp, 0));
      return const0_rtx;

    case BUILT_IN_BSWAP16:
    case BUILT_IN_BSWAP32:
    case BUILT_IN_BSWAP64:
      target = expand_builtin_bswap (target_mode, exp, target, subtarget);
      if (target)
	return target;
      break;

    CASE_INT_FN (BUILT_IN_FFS):
      target = expand_builtin_unop (target_mode, exp, target,
				    subtarget, ffs_optab);
      if (target)
	return target;
      break;

    CASE_INT_FN (BUILT_IN_CLZ):
      target = expand_builtin_unop (target_mode, exp, target,
				    subtarget, clz_optab);
      if (target)
	return target;
      break;

    CASE_INT_FN (BUILT_IN_CTZ):
      target = expand_builtin_unop (target_mode, exp, target,
				    subtarget, ctz_optab);
      if (target)
	return target;
      break;

    CASE_INT_FN (BUILT_IN_CLRSB):
      target = expand_builtin_unop (target_mode, exp, target,
				    subtarget, clrsb_optab);
      if (target)
	return target;
      break;

    CASE_INT_FN (BUILT_IN_POPCOUNT):
      target = expand_builtin_unop (target_mode, exp, target,
				    subtarget, popcount_optab);
      if (target)
	return target;
      break;

    CASE_INT_FN (BUILT_IN_PARITY):
      target = expand_builtin_unop (target_mode, exp, target,
				    subtarget, parity_optab);
      if (target)
	return target;
      break;

    case BUILT_IN_STRLEN:
      target = expand_builtin_strlen (exp, target, target_mode);
      if (target)
	return target;
      break;

    case BUILT_IN_STRCAT:
      target = expand_builtin_strcat (exp, target);
      if (target)
	return target;
      break;

    case BUILT_IN_STRCPY:
      target = expand_builtin_strcpy (exp, target);
      if (target)
	return target;
      break;

    case BUILT_IN_STRNCAT:
      target = expand_builtin_strncat (exp, target);
      if (target)
	return target;
      break;

    case BUILT_IN_STRNCPY:
      target = expand_builtin_strncpy (exp, target);
      if (target)
	return target;
      break;

    case BUILT_IN_STPCPY:
      target = expand_builtin_stpcpy (exp, target, mode);
      if (target)
	return target;
      break;

    case BUILT_IN_STPNCPY:
      target = expand_builtin_stpncpy (exp, target);
      if (target)
	return target;
      break;

    case BUILT_IN_MEMCHR:
      target = expand_builtin_memchr (exp, target);
      if (target)
	return target;
      break;

    case BUILT_IN_MEMCPY:
      target = expand_builtin_memcpy (exp, target);
      if (target)
	return target;
      break;

    case BUILT_IN_MEMMOVE:
      target = expand_builtin_memmove (exp, target);
      if (target)
	return target;
      break;

    case BUILT_IN_MEMPCPY:
      target = expand_builtin_mempcpy (exp, target);
      if (target)
	return target;
      break;

    case BUILT_IN_MEMSET:
      target = expand_builtin_memset (exp, target, mode);
      if (target)
	return target;
      break;

    case BUILT_IN_BZERO:
      target = expand_builtin_bzero (exp);
      if (target)
	return target;
      break;

    case BUILT_IN_STRCMP:
      target = expand_builtin_strcmp (exp, target);
      if (target)
	return target;
      break;

    case BUILT_IN_STRNCMP:
      target = expand_builtin_strncmp (exp, target, mode);
      if (target)
	return target;
      break;

    case BUILT_IN_BCMP:
    case BUILT_IN_MEMCMP:
    case BUILT_IN_MEMCMP_EQ:
      target = expand_builtin_memcmp (exp, target, fcode == BUILT_IN_MEMCMP_EQ);
      if (target)
	return target;
      if (fcode == BUILT_IN_MEMCMP_EQ)
	{
	  tree newdecl = builtin_decl_explicit (BUILT_IN_MEMCMP);
	  TREE_OPERAND (exp, 1) = build_fold_addr_expr (newdecl);
	}
      break;

    case BUILT_IN_SETJMP:
      /* This should have been lowered to the builtins below.  */
      gcc_unreachable ();

    case BUILT_IN_SETJMP_SETUP:
      /* __builtin_setjmp_setup is passed a pointer to an array of five words
          and the receiver label.  */
      if (validate_arglist (exp, POINTER_TYPE, POINTER_TYPE, VOID_TYPE))
	{
	  rtx buf_addr = expand_expr (CALL_EXPR_ARG (exp, 0), subtarget,
				      VOIDmode, EXPAND_NORMAL);
	  tree label = TREE_OPERAND (CALL_EXPR_ARG (exp, 1), 0);
	  rtx_insn *label_r = label_rtx (label);

	  /* This is copied from the handling of non-local gotos.  */
	  expand_builtin_setjmp_setup (buf_addr, label_r);
	  nonlocal_goto_handler_labels
	    = gen_rtx_INSN_LIST (VOIDmode, label_r,
				 nonlocal_goto_handler_labels);
	  /* ??? Do not let expand_label treat us as such since we would
	     not want to be both on the list of non-local labels and on
	     the list of forced labels.  */
	  FORCED_LABEL (label) = 0;
	  return const0_rtx;
	}
      break;

    case BUILT_IN_SETJMP_RECEIVER:
       /* __builtin_setjmp_receiver is passed the receiver label.  */
      if (validate_arglist (exp, POINTER_TYPE, VOID_TYPE))
	{
	  tree label = TREE_OPERAND (CALL_EXPR_ARG (exp, 0), 0);
	  rtx_insn *label_r = label_rtx (label);

	  expand_builtin_setjmp_receiver (label_r);
	  return const0_rtx;
	}
      break;

      /* __builtin_longjmp is passed a pointer to an array of five words.
	 It's similar to the C library longjmp function but works with
	 __builtin_setjmp above.  */
    case BUILT_IN_LONGJMP:
      if (validate_arglist (exp, POINTER_TYPE, INTEGER_TYPE, VOID_TYPE))
	{
	  rtx buf_addr = expand_expr (CALL_EXPR_ARG (exp, 0), subtarget,
				      VOIDmode, EXPAND_NORMAL);
	  rtx value = expand_normal (CALL_EXPR_ARG (exp, 1));

	  if (value != const1_rtx)
	    {
	      error ("%<__builtin_longjmp%> second argument must be 1");
	      return const0_rtx;
	    }

	  expand_builtin_longjmp (buf_addr, value);
	  return const0_rtx;
	}
      break;

    case BUILT_IN_NONLOCAL_GOTO:
      target = expand_builtin_nonlocal_goto (exp);
      if (target)
	return target;
      break;

      /* This updates the setjmp buffer that is its argument with the value
	 of the current stack pointer.  */
    case BUILT_IN_UPDATE_SETJMP_BUF:
      if (validate_arglist (exp, POINTER_TYPE, VOID_TYPE))
	{
	  rtx buf_addr
	    = expand_normal (CALL_EXPR_ARG (exp, 0));

	  expand_builtin_update_setjmp_buf (buf_addr);
	  return const0_rtx;
	}
      break;

    case BUILT_IN_TRAP:
      expand_builtin_trap ();
      return const0_rtx;

    case BUILT_IN_UNREACHABLE:
      expand_builtin_unreachable ();
      return const0_rtx;

    CASE_FLT_FN (BUILT_IN_SIGNBIT):
    case BUILT_IN_SIGNBITD32:
    case BUILT_IN_SIGNBITD64:
    case BUILT_IN_SIGNBITD128:
      target = expand_builtin_signbit (exp, target);
      if (target)
	return target;
      break;

      /* Various hooks for the DWARF 2 __throw routine.  */
    case BUILT_IN_UNWIND_INIT:
      expand_builtin_unwind_init ();
      return const0_rtx;
    case BUILT_IN_DWARF_CFA:
      return virtual_cfa_rtx;
#ifdef DWARF2_UNWIND_INFO
    case BUILT_IN_DWARF_SP_COLUMN:
      return expand_builtin_dwarf_sp_column ();
    case BUILT_IN_INIT_DWARF_REG_SIZES:
      expand_builtin_init_dwarf_reg_sizes (CALL_EXPR_ARG (exp, 0));
      return const0_rtx;
#endif
    case BUILT_IN_FROB_RETURN_ADDR:
      return expand_builtin_frob_return_addr (CALL_EXPR_ARG (exp, 0));
    case BUILT_IN_EXTRACT_RETURN_ADDR:
      return expand_builtin_extract_return_addr (CALL_EXPR_ARG (exp, 0));
    case BUILT_IN_EH_RETURN:
      expand_builtin_eh_return (CALL_EXPR_ARG (exp, 0),
				CALL_EXPR_ARG (exp, 1));
      return const0_rtx;
    case BUILT_IN_EH_RETURN_DATA_REGNO:
      return expand_builtin_eh_return_data_regno (exp);
    case BUILT_IN_EXTEND_POINTER:
      return expand_builtin_extend_pointer (CALL_EXPR_ARG (exp, 0));
    case BUILT_IN_EH_POINTER:
      return expand_builtin_eh_pointer (exp);
    case BUILT_IN_EH_FILTER:
      return expand_builtin_eh_filter (exp);
    case BUILT_IN_EH_COPY_VALUES:
      return expand_builtin_eh_copy_values (exp);

    case BUILT_IN_VA_START:
      return expand_builtin_va_start (exp);
    case BUILT_IN_VA_END:
      return expand_builtin_va_end (exp);
    case BUILT_IN_VA_COPY:
      return expand_builtin_va_copy (exp);
    case BUILT_IN_EXPECT:
      return expand_builtin_expect (exp, target);
    case BUILT_IN_ASSUME_ALIGNED:
      return expand_builtin_assume_aligned (exp, target);
    case BUILT_IN_PREFETCH:
      expand_builtin_prefetch (exp);
      return const0_rtx;

    case BUILT_IN_INIT_TRAMPOLINE:
      return expand_builtin_init_trampoline (exp, true);
    case BUILT_IN_INIT_HEAP_TRAMPOLINE:
      return expand_builtin_init_trampoline (exp, false);
    case BUILT_IN_ADJUST_TRAMPOLINE:
      return expand_builtin_adjust_trampoline (exp);

    case BUILT_IN_INIT_DESCRIPTOR:
      return expand_builtin_init_descriptor (exp);
    case BUILT_IN_ADJUST_DESCRIPTOR:
      return expand_builtin_adjust_descriptor (exp);

    case BUILT_IN_FORK:
    case BUILT_IN_EXECL:
    case BUILT_IN_EXECV:
    case BUILT_IN_EXECLP:
    case BUILT_IN_EXECLE:
    case BUILT_IN_EXECVP:
    case BUILT_IN_EXECVE:
      target = expand_builtin_fork_or_exec (fndecl, exp, target, ignore);
      if (target)
	return target;
      break;

    case BUILT_IN_SYNC_FETCH_AND_ADD_1:
    case BUILT_IN_SYNC_FETCH_AND_ADD_2:
    case BUILT_IN_SYNC_FETCH_AND_ADD_4:
    case BUILT_IN_SYNC_FETCH_AND_ADD_8:
    case BUILT_IN_SYNC_FETCH_AND_ADD_16:
      mode = get_builtin_sync_mode (fcode - BUILT_IN_SYNC_FETCH_AND_ADD_1);
      target = expand_builtin_sync_operation (mode, exp, PLUS, false, target);
      if (target)
	return target;
      break;

    case BUILT_IN_SYNC_FETCH_AND_SUB_1:
    case BUILT_IN_SYNC_FETCH_AND_SUB_2:
    case BUILT_IN_SYNC_FETCH_AND_SUB_4:
    case BUILT_IN_SYNC_FETCH_AND_SUB_8:
    case BUILT_IN_SYNC_FETCH_AND_SUB_16:
      mode = get_builtin_sync_mode (fcode - BUILT_IN_SYNC_FETCH_AND_SUB_1);
      target = expand_builtin_sync_operation (mode, exp, MINUS, false, target);
      if (target)
	return target;
      break;

    case BUILT_IN_SYNC_FETCH_AND_OR_1:
    case BUILT_IN_SYNC_FETCH_AND_OR_2:
    case BUILT_IN_SYNC_FETCH_AND_OR_4:
    case BUILT_IN_SYNC_FETCH_AND_OR_8:
    case BUILT_IN_SYNC_FETCH_AND_OR_16:
      mode = get_builtin_sync_mode (fcode - BUILT_IN_SYNC_FETCH_AND_OR_1);
      target = expand_builtin_sync_operation (mode, exp, IOR, false, target);
      if (target)
	return target;
      break;

    case BUILT_IN_SYNC_FETCH_AND_AND_1:
    case BUILT_IN_SYNC_FETCH_AND_AND_2:
    case BUILT_IN_SYNC_FETCH_AND_AND_4:
    case BUILT_IN_SYNC_FETCH_AND_AND_8:
    case BUILT_IN_SYNC_FETCH_AND_AND_16:
      mode = get_builtin_sync_mode (fcode - BUILT_IN_SYNC_FETCH_AND_AND_1);
      target = expand_builtin_sync_operation (mode, exp, AND, false, target);
      if (target)
	return target;
      break;

    case BUILT_IN_SYNC_FETCH_AND_XOR_1:
    case BUILT_IN_SYNC_FETCH_AND_XOR_2:
    case BUILT_IN_SYNC_FETCH_AND_XOR_4:
    case BUILT_IN_SYNC_FETCH_AND_XOR_8:
    case BUILT_IN_SYNC_FETCH_AND_XOR_16:
      mode = get_builtin_sync_mode (fcode - BUILT_IN_SYNC_FETCH_AND_XOR_1);
      target = expand_builtin_sync_operation (mode, exp, XOR, false, target);
      if (target)
	return target;
      break;

    case BUILT_IN_SYNC_FETCH_AND_NAND_1:
    case BUILT_IN_SYNC_FETCH_AND_NAND_2:
    case BUILT_IN_SYNC_FETCH_AND_NAND_4:
    case BUILT_IN_SYNC_FETCH_AND_NAND_8:
    case BUILT_IN_SYNC_FETCH_AND_NAND_16:
      mode = get_builtin_sync_mode (fcode - BUILT_IN_SYNC_FETCH_AND_NAND_1);
      target = expand_builtin_sync_operation (mode, exp, NOT, false, target);
      if (target)
	return target;
      break;

    case BUILT_IN_SYNC_ADD_AND_FETCH_1:
    case BUILT_IN_SYNC_ADD_AND_FETCH_2:
    case BUILT_IN_SYNC_ADD_AND_FETCH_4:
    case BUILT_IN_SYNC_ADD_AND_FETCH_8:
    case BUILT_IN_SYNC_ADD_AND_FETCH_16:
      mode = get_builtin_sync_mode (fcode - BUILT_IN_SYNC_ADD_AND_FETCH_1);
      target = expand_builtin_sync_operation (mode, exp, PLUS, true, target);
      if (target)
	return target;
      break;

    case BUILT_IN_SYNC_SUB_AND_FETCH_1:
    case BUILT_IN_SYNC_SUB_AND_FETCH_2:
    case BUILT_IN_SYNC_SUB_AND_FETCH_4:
    case BUILT_IN_SYNC_SUB_AND_FETCH_8:
    case BUILT_IN_SYNC_SUB_AND_FETCH_16:
      mode = get_builtin_sync_mode (fcode - BUILT_IN_SYNC_SUB_AND_FETCH_1);
      target = expand_builtin_sync_operation (mode, exp, MINUS, true, target);
      if (target)
	return target;
      break;

    case BUILT_IN_SYNC_OR_AND_FETCH_1:
    case BUILT_IN_SYNC_OR_AND_FETCH_2:
    case BUILT_IN_SYNC_OR_AND_FETCH_4:
    case BUILT_IN_SYNC_OR_AND_FETCH_8:
    case BUILT_IN_SYNC_OR_AND_FETCH_16:
      mode = get_builtin_sync_mode (fcode - BUILT_IN_SYNC_OR_AND_FETCH_1);
      target = expand_builtin_sync_operation (mode, exp, IOR, true, target);
      if (target)
	return target;
      break;

    case BUILT_IN_SYNC_AND_AND_FETCH_1:
    case BUILT_IN_SYNC_AND_AND_FETCH_2:
    case BUILT_IN_SYNC_AND_AND_FETCH_4:
    case BUILT_IN_SYNC_AND_AND_FETCH_8:
    case BUILT_IN_SYNC_AND_AND_FETCH_16:
      mode = get_builtin_sync_mode (fcode - BUILT_IN_SYNC_AND_AND_FETCH_1);
      target = expand_builtin_sync_operation (mode, exp, AND, true, target);
      if (target)
	return target;
      break;

    case BUILT_IN_SYNC_XOR_AND_FETCH_1:
    case BUILT_IN_SYNC_XOR_AND_FETCH_2:
    case BUILT_IN_SYNC_XOR_AND_FETCH_4:
    case BUILT_IN_SYNC_XOR_AND_FETCH_8:
    case BUILT_IN_SYNC_XOR_AND_FETCH_16:
      mode = get_builtin_sync_mode (fcode - BUILT_IN_SYNC_XOR_AND_FETCH_1);
      target = expand_builtin_sync_operation (mode, exp, XOR, true, target);
      if (target)
	return target;
      break;

    case BUILT_IN_SYNC_NAND_AND_FETCH_1:
    case BUILT_IN_SYNC_NAND_AND_FETCH_2:
    case BUILT_IN_SYNC_NAND_AND_FETCH_4:
    case BUILT_IN_SYNC_NAND_AND_FETCH_8:
    case BUILT_IN_SYNC_NAND_AND_FETCH_16:
      mode = get_builtin_sync_mode (fcode - BUILT_IN_SYNC_NAND_AND_FETCH_1);
      target = expand_builtin_sync_operation (mode, exp, NOT, true, target);
      if (target)
	return target;
      break;

    case BUILT_IN_SYNC_BOOL_COMPARE_AND_SWAP_1:
    case BUILT_IN_SYNC_BOOL_COMPARE_AND_SWAP_2:
    case BUILT_IN_SYNC_BOOL_COMPARE_AND_SWAP_4:
    case BUILT_IN_SYNC_BOOL_COMPARE_AND_SWAP_8:
    case BUILT_IN_SYNC_BOOL_COMPARE_AND_SWAP_16:
      if (mode == VOIDmode)
	mode = TYPE_MODE (boolean_type_node);
      if (!target || !register_operand (target, mode))
	target = gen_reg_rtx (mode);

      mode = get_builtin_sync_mode 
				(fcode - BUILT_IN_SYNC_BOOL_COMPARE_AND_SWAP_1);
      target = expand_builtin_compare_and_swap (mode, exp, true, target);
      if (target)
	return target;
      break;

    case BUILT_IN_SYNC_VAL_COMPARE_AND_SWAP_1:
    case BUILT_IN_SYNC_VAL_COMPARE_AND_SWAP_2:
    case BUILT_IN_SYNC_VAL_COMPARE_AND_SWAP_4:
    case BUILT_IN_SYNC_VAL_COMPARE_AND_SWAP_8:
    case BUILT_IN_SYNC_VAL_COMPARE_AND_SWAP_16:
      mode = get_builtin_sync_mode 
				(fcode - BUILT_IN_SYNC_VAL_COMPARE_AND_SWAP_1);
      target = expand_builtin_compare_and_swap (mode, exp, false, target);
      if (target)
	return target;
      break;

    case BUILT_IN_SYNC_LOCK_TEST_AND_SET_1:
    case BUILT_IN_SYNC_LOCK_TEST_AND_SET_2:
    case BUILT_IN_SYNC_LOCK_TEST_AND_SET_4:
    case BUILT_IN_SYNC_LOCK_TEST_AND_SET_8:
    case BUILT_IN_SYNC_LOCK_TEST_AND_SET_16:
      mode = get_builtin_sync_mode (fcode - BUILT_IN_SYNC_LOCK_TEST_AND_SET_1);
      target = expand_builtin_sync_lock_test_and_set (mode, exp, target);
      if (target)
	return target;
      break;

    case BUILT_IN_SYNC_LOCK_RELEASE_1:
    case BUILT_IN_SYNC_LOCK_RELEASE_2:
    case BUILT_IN_SYNC_LOCK_RELEASE_4:
    case BUILT_IN_SYNC_LOCK_RELEASE_8:
    case BUILT_IN_SYNC_LOCK_RELEASE_16:
      mode = get_builtin_sync_mode (fcode - BUILT_IN_SYNC_LOCK_RELEASE_1);
      expand_builtin_sync_lock_release (mode, exp);
      return const0_rtx;

    case BUILT_IN_SYNC_SYNCHRONIZE:
      expand_builtin_sync_synchronize ();
      return const0_rtx;

    case BUILT_IN_ATOMIC_EXCHANGE_1:
    case BUILT_IN_ATOMIC_EXCHANGE_2:
    case BUILT_IN_ATOMIC_EXCHANGE_4:
    case BUILT_IN_ATOMIC_EXCHANGE_8:
    case BUILT_IN_ATOMIC_EXCHANGE_16:
      mode = get_builtin_sync_mode (fcode - BUILT_IN_ATOMIC_EXCHANGE_1);
      target = expand_builtin_atomic_exchange (mode, exp, target);
      if (target)
	return target;
      break;

    case BUILT_IN_ATOMIC_COMPARE_EXCHANGE_1:
    case BUILT_IN_ATOMIC_COMPARE_EXCHANGE_2:
    case BUILT_IN_ATOMIC_COMPARE_EXCHANGE_4:
    case BUILT_IN_ATOMIC_COMPARE_EXCHANGE_8:
    case BUILT_IN_ATOMIC_COMPARE_EXCHANGE_16:
      {
	unsigned int nargs, z;
	vec<tree, va_gc> *vec;

	mode = 
	    get_builtin_sync_mode (fcode - BUILT_IN_ATOMIC_COMPARE_EXCHANGE_1);
	target = expand_builtin_atomic_compare_exchange (mode, exp, target);
	if (target)
	  return target;

	/* If this is turned into an external library call, the weak parameter
	   must be dropped to match the expected parameter list.  */
	nargs = call_expr_nargs (exp);
	vec_alloc (vec, nargs - 1);
	for (z = 0; z < 3; z++)
	  vec->quick_push (CALL_EXPR_ARG (exp, z));
	/* Skip the boolean weak parameter.  */
	for (z = 4; z < 6; z++)
	  vec->quick_push (CALL_EXPR_ARG (exp, z));
	exp = build_call_vec (TREE_TYPE (exp), CALL_EXPR_FN (exp), vec);
	break;
      }

    case BUILT_IN_ATOMIC_LOAD_1:
    case BUILT_IN_ATOMIC_LOAD_2:
    case BUILT_IN_ATOMIC_LOAD_4:
    case BUILT_IN_ATOMIC_LOAD_8:
    case BUILT_IN_ATOMIC_LOAD_16:
      mode = get_builtin_sync_mode (fcode - BUILT_IN_ATOMIC_LOAD_1);
      target = expand_builtin_atomic_load (mode, exp, target);
      if (target)
	return target;
      break;

    case BUILT_IN_ATOMIC_STORE_1:
    case BUILT_IN_ATOMIC_STORE_2:
    case BUILT_IN_ATOMIC_STORE_4:
    case BUILT_IN_ATOMIC_STORE_8:
    case BUILT_IN_ATOMIC_STORE_16:
      mode = get_builtin_sync_mode (fcode - BUILT_IN_ATOMIC_STORE_1);
      target = expand_builtin_atomic_store (mode, exp);
      if (target)
	return const0_rtx;
      break;

    case BUILT_IN_ATOMIC_ADD_FETCH_1:
    case BUILT_IN_ATOMIC_ADD_FETCH_2:
    case BUILT_IN_ATOMIC_ADD_FETCH_4:
    case BUILT_IN_ATOMIC_ADD_FETCH_8:
    case BUILT_IN_ATOMIC_ADD_FETCH_16:
      {
	enum built_in_function lib;
	mode = get_builtin_sync_mode (fcode - BUILT_IN_ATOMIC_ADD_FETCH_1);
	lib = (enum built_in_function)((int)BUILT_IN_ATOMIC_FETCH_ADD_1 + 
				       (fcode - BUILT_IN_ATOMIC_ADD_FETCH_1));
	target = expand_builtin_atomic_fetch_op (mode, exp, target, PLUS, true,
						 ignore, lib);
	if (target)
	  return target;
	break;
      }
    case BUILT_IN_ATOMIC_SUB_FETCH_1:
    case BUILT_IN_ATOMIC_SUB_FETCH_2:
    case BUILT_IN_ATOMIC_SUB_FETCH_4:
    case BUILT_IN_ATOMIC_SUB_FETCH_8:
    case BUILT_IN_ATOMIC_SUB_FETCH_16:
      {
	enum built_in_function lib;
	mode = get_builtin_sync_mode (fcode - BUILT_IN_ATOMIC_SUB_FETCH_1);
	lib = (enum built_in_function)((int)BUILT_IN_ATOMIC_FETCH_SUB_1 + 
				       (fcode - BUILT_IN_ATOMIC_SUB_FETCH_1));
	target = expand_builtin_atomic_fetch_op (mode, exp, target, MINUS, true,
						 ignore, lib);
	if (target)
	  return target;
	break;
      }
    case BUILT_IN_ATOMIC_AND_FETCH_1:
    case BUILT_IN_ATOMIC_AND_FETCH_2:
    case BUILT_IN_ATOMIC_AND_FETCH_4:
    case BUILT_IN_ATOMIC_AND_FETCH_8:
    case BUILT_IN_ATOMIC_AND_FETCH_16:
      {
	enum built_in_function lib;
	mode = get_builtin_sync_mode (fcode - BUILT_IN_ATOMIC_AND_FETCH_1);
	lib = (enum built_in_function)((int)BUILT_IN_ATOMIC_FETCH_AND_1 + 
				       (fcode - BUILT_IN_ATOMIC_AND_FETCH_1));
	target = expand_builtin_atomic_fetch_op (mode, exp, target, AND, true,
						 ignore, lib);
	if (target)
	  return target;
	break;
      }
    case BUILT_IN_ATOMIC_NAND_FETCH_1:
    case BUILT_IN_ATOMIC_NAND_FETCH_2:
    case BUILT_IN_ATOMIC_NAND_FETCH_4:
    case BUILT_IN_ATOMIC_NAND_FETCH_8:
    case BUILT_IN_ATOMIC_NAND_FETCH_16:
      {
	enum built_in_function lib;
	mode = get_builtin_sync_mode (fcode - BUILT_IN_ATOMIC_NAND_FETCH_1);
	lib = (enum built_in_function)((int)BUILT_IN_ATOMIC_FETCH_NAND_1 + 
				       (fcode - BUILT_IN_ATOMIC_NAND_FETCH_1));
	target = expand_builtin_atomic_fetch_op (mode, exp, target, NOT, true,
						 ignore, lib);
	if (target)
	  return target;
	break;
      }
    case BUILT_IN_ATOMIC_XOR_FETCH_1:
    case BUILT_IN_ATOMIC_XOR_FETCH_2:
    case BUILT_IN_ATOMIC_XOR_FETCH_4:
    case BUILT_IN_ATOMIC_XOR_FETCH_8:
    case BUILT_IN_ATOMIC_XOR_FETCH_16:
      {
	enum built_in_function lib;
	mode = get_builtin_sync_mode (fcode - BUILT_IN_ATOMIC_XOR_FETCH_1);
	lib = (enum built_in_function)((int)BUILT_IN_ATOMIC_FETCH_XOR_1 + 
				       (fcode - BUILT_IN_ATOMIC_XOR_FETCH_1));
	target = expand_builtin_atomic_fetch_op (mode, exp, target, XOR, true,
						 ignore, lib);
	if (target)
	  return target;
	break;
      }
    case BUILT_IN_ATOMIC_OR_FETCH_1:
    case BUILT_IN_ATOMIC_OR_FETCH_2:
    case BUILT_IN_ATOMIC_OR_FETCH_4:
    case BUILT_IN_ATOMIC_OR_FETCH_8:
    case BUILT_IN_ATOMIC_OR_FETCH_16:
      {
	enum built_in_function lib;
	mode = get_builtin_sync_mode (fcode - BUILT_IN_ATOMIC_OR_FETCH_1);
	lib = (enum built_in_function)((int)BUILT_IN_ATOMIC_FETCH_OR_1 + 
				       (fcode - BUILT_IN_ATOMIC_OR_FETCH_1));
	target = expand_builtin_atomic_fetch_op (mode, exp, target, IOR, true,
						 ignore, lib);
	if (target)
	  return target;
	break;
      }
    case BUILT_IN_ATOMIC_FETCH_ADD_1:
    case BUILT_IN_ATOMIC_FETCH_ADD_2:
    case BUILT_IN_ATOMIC_FETCH_ADD_4:
    case BUILT_IN_ATOMIC_FETCH_ADD_8:
    case BUILT_IN_ATOMIC_FETCH_ADD_16:
      mode = get_builtin_sync_mode (fcode - BUILT_IN_ATOMIC_FETCH_ADD_1);
      target = expand_builtin_atomic_fetch_op (mode, exp, target, PLUS, false,
					       ignore, BUILT_IN_NONE);
      if (target)
	return target;
      break;
 
    case BUILT_IN_ATOMIC_FETCH_SUB_1:
    case BUILT_IN_ATOMIC_FETCH_SUB_2:
    case BUILT_IN_ATOMIC_FETCH_SUB_4:
    case BUILT_IN_ATOMIC_FETCH_SUB_8:
    case BUILT_IN_ATOMIC_FETCH_SUB_16:
      mode = get_builtin_sync_mode (fcode - BUILT_IN_ATOMIC_FETCH_SUB_1);
      target = expand_builtin_atomic_fetch_op (mode, exp, target, MINUS, false,
					       ignore, BUILT_IN_NONE);
      if (target)
	return target;
      break;

    case BUILT_IN_ATOMIC_FETCH_AND_1:
    case BUILT_IN_ATOMIC_FETCH_AND_2:
    case BUILT_IN_ATOMIC_FETCH_AND_4:
    case BUILT_IN_ATOMIC_FETCH_AND_8:
    case BUILT_IN_ATOMIC_FETCH_AND_16:
      mode = get_builtin_sync_mode (fcode - BUILT_IN_ATOMIC_FETCH_AND_1);
      target = expand_builtin_atomic_fetch_op (mode, exp, target, AND, false,
					       ignore, BUILT_IN_NONE);
      if (target)
	return target;
      break;
  
    case BUILT_IN_ATOMIC_FETCH_NAND_1:
    case BUILT_IN_ATOMIC_FETCH_NAND_2:
    case BUILT_IN_ATOMIC_FETCH_NAND_4:
    case BUILT_IN_ATOMIC_FETCH_NAND_8:
    case BUILT_IN_ATOMIC_FETCH_NAND_16:
      mode = get_builtin_sync_mode (fcode - BUILT_IN_ATOMIC_FETCH_NAND_1);
      target = expand_builtin_atomic_fetch_op (mode, exp, target, NOT, false,
					       ignore, BUILT_IN_NONE);
      if (target)
	return target;
      break;
 
    case BUILT_IN_ATOMIC_FETCH_XOR_1:
    case BUILT_IN_ATOMIC_FETCH_XOR_2:
    case BUILT_IN_ATOMIC_FETCH_XOR_4:
    case BUILT_IN_ATOMIC_FETCH_XOR_8:
    case BUILT_IN_ATOMIC_FETCH_XOR_16:
      mode = get_builtin_sync_mode (fcode - BUILT_IN_ATOMIC_FETCH_XOR_1);
      target = expand_builtin_atomic_fetch_op (mode, exp, target, XOR, false,
					       ignore, BUILT_IN_NONE);
      if (target)
	return target;
      break;
 
    case BUILT_IN_ATOMIC_FETCH_OR_1:
    case BUILT_IN_ATOMIC_FETCH_OR_2:
    case BUILT_IN_ATOMIC_FETCH_OR_4:
    case BUILT_IN_ATOMIC_FETCH_OR_8:
    case BUILT_IN_ATOMIC_FETCH_OR_16:
      mode = get_builtin_sync_mode (fcode - BUILT_IN_ATOMIC_FETCH_OR_1);
      target = expand_builtin_atomic_fetch_op (mode, exp, target, IOR, false,
					       ignore, BUILT_IN_NONE);
      if (target)
	return target;
      break;

    case BUILT_IN_ATOMIC_TEST_AND_SET:
      return expand_builtin_atomic_test_and_set (exp, target);

    case BUILT_IN_ATOMIC_CLEAR:
      return expand_builtin_atomic_clear (exp);
 
    case BUILT_IN_ATOMIC_ALWAYS_LOCK_FREE:
      return expand_builtin_atomic_always_lock_free (exp);

    case BUILT_IN_ATOMIC_IS_LOCK_FREE:
      target = expand_builtin_atomic_is_lock_free (exp);
      if (target)
        return target;
      break;

    case BUILT_IN_ATOMIC_THREAD_FENCE:
      expand_builtin_atomic_thread_fence (exp);
      return const0_rtx;

    case BUILT_IN_ATOMIC_SIGNAL_FENCE:
      expand_builtin_atomic_signal_fence (exp);
      return const0_rtx;

    case BUILT_IN_OBJECT_SIZE:
      return expand_builtin_object_size (exp);

    case BUILT_IN_MEMCPY_CHK:
    case BUILT_IN_MEMPCPY_CHK:
    case BUILT_IN_MEMMOVE_CHK:
    case BUILT_IN_MEMSET_CHK:
      target = expand_builtin_memory_chk (exp, target, mode, fcode);
      if (target)
	return target;
      break;

    case BUILT_IN_STRCPY_CHK:
    case BUILT_IN_STPCPY_CHK:
    case BUILT_IN_STRNCPY_CHK:
    case BUILT_IN_STPNCPY_CHK:
    case BUILT_IN_STRCAT_CHK:
    case BUILT_IN_STRNCAT_CHK:
    case BUILT_IN_SNPRINTF_CHK:
    case BUILT_IN_VSNPRINTF_CHK:
      maybe_emit_chk_warning (exp, fcode);
      break;

    case BUILT_IN_SPRINTF_CHK:
    case BUILT_IN_VSPRINTF_CHK:
      maybe_emit_sprintf_chk_warning (exp, fcode);
      break;

    case BUILT_IN_FREE:
      if (warn_free_nonheap_object)
	maybe_emit_free_warning (exp);
      break;

    case BUILT_IN_THREAD_POINTER:
      return expand_builtin_thread_pointer (exp, target);

    case BUILT_IN_SET_THREAD_POINTER:
      expand_builtin_set_thread_pointer (exp);
      return const0_rtx;

    case BUILT_IN_CILK_DETACH:
      expand_builtin_cilk_detach (exp);
      return const0_rtx;
      
    case BUILT_IN_CILK_POP_FRAME:
      expand_builtin_cilk_pop_frame (exp);
      return const0_rtx;

    case BUILT_IN_CHKP_INIT_PTR_BOUNDS:
    case BUILT_IN_CHKP_NULL_PTR_BOUNDS:
    case BUILT_IN_CHKP_COPY_PTR_BOUNDS:
    case BUILT_IN_CHKP_CHECK_PTR_LBOUNDS:
    case BUILT_IN_CHKP_CHECK_PTR_UBOUNDS:
    case BUILT_IN_CHKP_CHECK_PTR_BOUNDS:
    case BUILT_IN_CHKP_SET_PTR_BOUNDS:
    case BUILT_IN_CHKP_NARROW_PTR_BOUNDS:
    case BUILT_IN_CHKP_STORE_PTR_BOUNDS:
    case BUILT_IN_CHKP_GET_PTR_LBOUND:
    case BUILT_IN_CHKP_GET_PTR_UBOUND:
      /* We allow user CHKP builtins if Pointer Bounds
	 Checker is off.  */
      if (!chkp_function_instrumented_p (current_function_decl))
	{
	  if (fcode == BUILT_IN_CHKP_SET_PTR_BOUNDS
	      || fcode == BUILT_IN_CHKP_NARROW_PTR_BOUNDS
	      || fcode == BUILT_IN_CHKP_INIT_PTR_BOUNDS
	      || fcode == BUILT_IN_CHKP_NULL_PTR_BOUNDS
	      || fcode == BUILT_IN_CHKP_COPY_PTR_BOUNDS)
	    return expand_normal (CALL_EXPR_ARG (exp, 0));
	  else if (fcode == BUILT_IN_CHKP_GET_PTR_LBOUND)
	    return expand_normal (size_zero_node);
	  else if (fcode == BUILT_IN_CHKP_GET_PTR_UBOUND)
	    return expand_normal (size_int (-1));
	  else
	    return const0_rtx;
	}
      /* FALLTHROUGH */

    case BUILT_IN_CHKP_BNDMK:
    case BUILT_IN_CHKP_BNDSTX:
    case BUILT_IN_CHKP_BNDCL:
    case BUILT_IN_CHKP_BNDCU:
    case BUILT_IN_CHKP_BNDLDX:
    case BUILT_IN_CHKP_BNDRET:
    case BUILT_IN_CHKP_INTERSECT:
    case BUILT_IN_CHKP_NARROW:
    case BUILT_IN_CHKP_EXTRACT_LOWER:
    case BUILT_IN_CHKP_EXTRACT_UPPER:
      /* Software implementation of Pointer Bounds Checker is NYI.
	 Target support is required.  */
      error ("Your target platform does not support -fcheck-pointer-bounds");
      break;

    case BUILT_IN_ACC_ON_DEVICE:
      /* Do library call, if we failed to expand the builtin when
	 folding.  */
      break;

    default:	/* just do library call, if unknown builtin */
      break;
    }

  /* The switch statement above can drop through to cause the function
     to be called normally.  */
  return expand_call (exp, target, ignore);
}

/* Similar to expand_builtin but is used for instrumented calls.  */

rtx
expand_builtin_with_bounds (tree exp, rtx target,
			    rtx subtarget ATTRIBUTE_UNUSED,
			    machine_mode mode, int ignore)
{
  tree fndecl = get_callee_fndecl (exp);
  enum built_in_function fcode = DECL_FUNCTION_CODE (fndecl);

  gcc_assert (CALL_WITH_BOUNDS_P (exp));

  if (DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_MD)
    return targetm.expand_builtin (exp, target, subtarget, mode, ignore);

  gcc_assert (fcode > BEGIN_CHKP_BUILTINS
	      && fcode < END_CHKP_BUILTINS);

  switch (fcode)
    {
    case BUILT_IN_CHKP_MEMCPY_NOBND_NOCHK_CHKP:
      target = expand_builtin_memcpy_with_bounds (exp, target);
      if (target)
	return target;
      break;

    case BUILT_IN_CHKP_MEMPCPY_NOBND_NOCHK_CHKP:
      target = expand_builtin_mempcpy_with_bounds (exp, target);
      if (target)
	return target;
      break;

    case BUILT_IN_CHKP_MEMSET_NOBND_NOCHK_CHKP:
      target = expand_builtin_memset_with_bounds (exp, target, mode);
      if (target)
	return target;
      break;

    default:
      break;
    }

  /* The switch statement above can drop through to cause the function
     to be called normally.  */
  return expand_call (exp, target, ignore);
 }

/* Determine whether a tree node represents a call to a built-in
   function.  If the tree T is a call to a built-in function with
   the right number of arguments of the appropriate types, return
   the DECL_FUNCTION_CODE of the call, e.g. BUILT_IN_SQRT.
   Otherwise the return value is END_BUILTINS.  */

enum built_in_function
builtin_mathfn_code (const_tree t)
{
  const_tree fndecl, arg, parmlist;
  const_tree argtype, parmtype;
  const_call_expr_arg_iterator iter;

  if (TREE_CODE (t) != CALL_EXPR
      || TREE_CODE (CALL_EXPR_FN (t)) != ADDR_EXPR)
    return END_BUILTINS;

  fndecl = get_callee_fndecl (t);
  if (fndecl == NULL_TREE
      || TREE_CODE (fndecl) != FUNCTION_DECL
      || ! DECL_BUILT_IN (fndecl)
      || DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_MD)
    return END_BUILTINS;

  parmlist = TYPE_ARG_TYPES (TREE_TYPE (fndecl));
  init_const_call_expr_arg_iterator (t, &iter);
  for (; parmlist; parmlist = TREE_CHAIN (parmlist))
    {
      /* If a function doesn't take a variable number of arguments,
	 the last element in the list will have type `void'.  */
      parmtype = TREE_VALUE (parmlist);
      if (VOID_TYPE_P (parmtype))
	{
	  if (more_const_call_expr_args_p (&iter))
	    return END_BUILTINS;
	  return DECL_FUNCTION_CODE (fndecl);
	}

      if (! more_const_call_expr_args_p (&iter))
	return END_BUILTINS;

      arg = next_const_call_expr_arg (&iter);
      argtype = TREE_TYPE (arg);

      if (SCALAR_FLOAT_TYPE_P (parmtype))
	{
	  if (! SCALAR_FLOAT_TYPE_P (argtype))
	    return END_BUILTINS;
	}
      else if (COMPLEX_FLOAT_TYPE_P (parmtype))
	{
	  if (! COMPLEX_FLOAT_TYPE_P (argtype))
	    return END_BUILTINS;
	}
      else if (POINTER_TYPE_P (parmtype))
	{
	  if (! POINTER_TYPE_P (argtype))
	    return END_BUILTINS;
	}
      else if (INTEGRAL_TYPE_P (parmtype))
	{
	  if (! INTEGRAL_TYPE_P (argtype))
	    return END_BUILTINS;
	}
      else
	return END_BUILTINS;
    }

  /* Variable-length argument list.  */
  return DECL_FUNCTION_CODE (fndecl);
}

/* Fold a call to __builtin_constant_p, if we know its argument ARG will
   evaluate to a constant.  */

static tree
fold_builtin_constant_p (tree arg)
{
  /* We return 1 for a numeric type that's known to be a constant
     value at compile-time or for an aggregate type that's a
     literal constant.  */
  STRIP_NOPS (arg);

  /* If we know this is a constant, emit the constant of one.  */
  if (CONSTANT_CLASS_P (arg)
      || (TREE_CODE (arg) == CONSTRUCTOR
	  && TREE_CONSTANT (arg)))
    return integer_one_node;
  if (TREE_CODE (arg) == ADDR_EXPR)
    {
       tree op = TREE_OPERAND (arg, 0);
       if (TREE_CODE (op) == STRING_CST
	   || (TREE_CODE (op) == ARRAY_REF
	       && integer_zerop (TREE_OPERAND (op, 1))
	       && TREE_CODE (TREE_OPERAND (op, 0)) == STRING_CST))
	 return integer_one_node;
    }

  /* If this expression has side effects, show we don't know it to be a
     constant.  Likewise if it's a pointer or aggregate type since in
     those case we only want literals, since those are only optimized
     when generating RTL, not later.
     And finally, if we are compiling an initializer, not code, we
     need to return a definite result now; there's not going to be any
     more optimization done.  */
  if (TREE_SIDE_EFFECTS (arg)
      || AGGREGATE_TYPE_P (TREE_TYPE (arg))
      || POINTER_TYPE_P (TREE_TYPE (arg))
      || cfun == 0
      || folding_initializer
      || force_folding_builtin_constant_p)
    return integer_zero_node;

  return NULL_TREE;
}

/* Create builtin_expect with PRED and EXPECTED as its arguments and
   return it as a truthvalue.  */

static tree
build_builtin_expect_predicate (location_t loc, tree pred, tree expected,
				tree predictor)
{
  tree fn, arg_types, pred_type, expected_type, call_expr, ret_type;

  fn = builtin_decl_explicit (BUILT_IN_EXPECT);
  arg_types = TYPE_ARG_TYPES (TREE_TYPE (fn));
  ret_type = TREE_TYPE (TREE_TYPE (fn));
  pred_type = TREE_VALUE (arg_types);
  expected_type = TREE_VALUE (TREE_CHAIN (arg_types));

  pred = fold_convert_loc (loc, pred_type, pred);
  expected = fold_convert_loc (loc, expected_type, expected);
  call_expr = build_call_expr_loc (loc, fn, predictor ? 3 : 2, pred, expected,
				   predictor);

  return build2 (NE_EXPR, TREE_TYPE (pred), call_expr,
		 build_int_cst (ret_type, 0));
}

/* Fold a call to builtin_expect with arguments ARG0 and ARG1.  Return
   NULL_TREE if no simplification is possible.  */

tree
fold_builtin_expect (location_t loc, tree arg0, tree arg1, tree arg2)
{
  tree inner, fndecl, inner_arg0;
  enum tree_code code;

  /* Distribute the expected value over short-circuiting operators.
     See through the cast from truthvalue_type_node to long.  */
  inner_arg0 = arg0;
  while (CONVERT_EXPR_P (inner_arg0)
	 && INTEGRAL_TYPE_P (TREE_TYPE (inner_arg0))
	 && INTEGRAL_TYPE_P (TREE_TYPE (TREE_OPERAND (inner_arg0, 0))))
    inner_arg0 = TREE_OPERAND (inner_arg0, 0);

  /* If this is a builtin_expect within a builtin_expect keep the
     inner one.  See through a comparison against a constant.  It
     might have been added to create a thruthvalue.  */
  inner = inner_arg0;

  if (COMPARISON_CLASS_P (inner)
      && TREE_CODE (TREE_OPERAND (inner, 1)) == INTEGER_CST)
    inner = TREE_OPERAND (inner, 0);

  if (TREE_CODE (inner) == CALL_EXPR
      && (fndecl = get_callee_fndecl (inner))
      && DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL
      && DECL_FUNCTION_CODE (fndecl) == BUILT_IN_EXPECT)
    return arg0;

  inner = inner_arg0;
  code = TREE_CODE (inner);
  if (code == TRUTH_ANDIF_EXPR || code == TRUTH_ORIF_EXPR)
    {
      tree op0 = TREE_OPERAND (inner, 0);
      tree op1 = TREE_OPERAND (inner, 1);

      op0 = build_builtin_expect_predicate (loc, op0, arg1, arg2);
      op1 = build_builtin_expect_predicate (loc, op1, arg1, arg2);
      inner = build2 (code, TREE_TYPE (inner), op0, op1);

      return fold_convert_loc (loc, TREE_TYPE (arg0), inner);
    }

  /* If the argument isn't invariant then there's nothing else we can do.  */
  if (!TREE_CONSTANT (inner_arg0))
    return NULL_TREE;

  /* If we expect that a comparison against the argument will fold to
     a constant return the constant.  In practice, this means a true
     constant or the address of a non-weak symbol.  */
  inner = inner_arg0;
  STRIP_NOPS (inner);
  if (TREE_CODE (inner) == ADDR_EXPR)
    {
      do
	{
	  inner = TREE_OPERAND (inner, 0);
	}
      while (TREE_CODE (inner) == COMPONENT_REF
	     || TREE_CODE (inner) == ARRAY_REF);
      if (VAR_OR_FUNCTION_DECL_P (inner) && DECL_WEAK (inner))
	return NULL_TREE;
    }

  /* Otherwise, ARG0 already has the proper type for the return value.  */
  return arg0;
}

/* Fold a call to __builtin_classify_type with argument ARG.  */

static tree
fold_builtin_classify_type (tree arg)
{
  if (arg == 0)
    return build_int_cst (integer_type_node, no_type_class);

  return build_int_cst (integer_type_node, type_to_class (TREE_TYPE (arg)));
}

/* Fold a call to __builtin_strlen with argument ARG.  */

static tree
fold_builtin_strlen (location_t loc, tree type, tree arg)
{
  if (!validate_arg (arg, POINTER_TYPE))
    return NULL_TREE;
  else
    {
      tree len = c_strlen (arg, 0);

      if (len)
	return fold_convert_loc (loc, type, len);

      return NULL_TREE;
    }
}

/* Fold a call to __builtin_inf or __builtin_huge_val.  */

static tree
fold_builtin_inf (location_t loc, tree type, int warn)
{
  REAL_VALUE_TYPE real;

  /* __builtin_inff is intended to be usable to define INFINITY on all
     targets.  If an infinity is not available, INFINITY expands "to a
     positive constant of type float that overflows at translation
     time", footnote "In this case, using INFINITY will violate the
     constraint in 6.4.4 and thus require a diagnostic." (C99 7.12#4).
     Thus we pedwarn to ensure this constraint violation is
     diagnosed.  */
  if (!MODE_HAS_INFINITIES (TYPE_MODE (type)) && warn)
    pedwarn (loc, 0, "target format does not support infinity");

  real_inf (&real);
  return build_real (type, real);
}

/* Fold function call to builtin sincos, sincosf, or sincosl.  Return
   NULL_TREE if no simplification can be made.  */

static tree
fold_builtin_sincos (location_t loc,
		     tree arg0, tree arg1, tree arg2)
{
  tree type;
  tree fndecl, call = NULL_TREE;

  if (!validate_arg (arg0, REAL_TYPE)
      || !validate_arg (arg1, POINTER_TYPE)
      || !validate_arg (arg2, POINTER_TYPE))
    return NULL_TREE;

  type = TREE_TYPE (arg0);

  /* Calculate the result when the argument is a constant.  */
  built_in_function fn = mathfn_built_in_2 (type, CFN_BUILT_IN_CEXPI);
  if (fn == END_BUILTINS)
    return NULL_TREE;

  /* Canonicalize sincos to cexpi.  */
  if (TREE_CODE (arg0) == REAL_CST)
    {
      tree complex_type = build_complex_type (type);
      call = fold_const_call (as_combined_fn (fn), complex_type, arg0);
    }
  if (!call)
    {
      if (!targetm.libc_has_function (function_c99_math_complex)
	  || !builtin_decl_implicit_p (fn))
	return NULL_TREE;
      fndecl = builtin_decl_explicit (fn);
      call = build_call_expr_loc (loc, fndecl, 1, arg0);
      call = builtin_save_expr (call);
    }

  return build2 (COMPOUND_EXPR, void_type_node,
		 build2 (MODIFY_EXPR, void_type_node,
			 build_fold_indirect_ref_loc (loc, arg1),
			 fold_build1_loc (loc, IMAGPART_EXPR, type, call)),
		 build2 (MODIFY_EXPR, void_type_node,
			 build_fold_indirect_ref_loc (loc, arg2),
			 fold_build1_loc (loc, REALPART_EXPR, type, call)));
}

/* Fold function call to builtin memcmp with arguments ARG1 and ARG2.
   Return NULL_TREE if no simplification can be made.  */

static tree
fold_builtin_memcmp (location_t loc, tree arg1, tree arg2, tree len)
{
  if (!validate_arg (arg1, POINTER_TYPE)
      || !validate_arg (arg2, POINTER_TYPE)
      || !validate_arg (len, INTEGER_TYPE))
    return NULL_TREE;

  /* If the LEN parameter is zero, return zero.  */
  if (integer_zerop (len))
    return omit_two_operands_loc (loc, integer_type_node, integer_zero_node,
			      arg1, arg2);

  /* If ARG1 and ARG2 are the same (and not volatile), return zero.  */
  if (operand_equal_p (arg1, arg2, 0))
    return omit_one_operand_loc (loc, integer_type_node, integer_zero_node, len);

  /* If len parameter is one, return an expression corresponding to
     (*(const unsigned char*)arg1 - (const unsigned char*)arg2).  */
  if (tree_fits_uhwi_p (len) && tree_to_uhwi (len) == 1)
    {
      tree cst_uchar_node = build_type_variant (unsigned_char_type_node, 1, 0);
      tree cst_uchar_ptr_node
	= build_pointer_type_for_mode (cst_uchar_node, ptr_mode, true);

      tree ind1
	= fold_convert_loc (loc, integer_type_node,
			    build1 (INDIRECT_REF, cst_uchar_node,
				    fold_convert_loc (loc,
						      cst_uchar_ptr_node,
						      arg1)));
      tree ind2
	= fold_convert_loc (loc, integer_type_node,
			    build1 (INDIRECT_REF, cst_uchar_node,
				    fold_convert_loc (loc,
						      cst_uchar_ptr_node,
						      arg2)));
      return fold_build2_loc (loc, MINUS_EXPR, integer_type_node, ind1, ind2);
    }

  return NULL_TREE;
}

/* Fold a call to builtin isascii with argument ARG.  */

static tree
fold_builtin_isascii (location_t loc, tree arg)
{
  if (!validate_arg (arg, INTEGER_TYPE))
    return NULL_TREE;
  else
    {
      /* Transform isascii(c) -> ((c & ~0x7f) == 0).  */
      arg = fold_build2 (BIT_AND_EXPR, integer_type_node, arg,
			 build_int_cst (integer_type_node,
					~ (unsigned HOST_WIDE_INT) 0x7f));
      return fold_build2_loc (loc, EQ_EXPR, integer_type_node,
			      arg, integer_zero_node);
    }
}

/* Fold a call to builtin toascii with argument ARG.  */

static tree
fold_builtin_toascii (location_t loc, tree arg)
{
  if (!validate_arg (arg, INTEGER_TYPE))
    return NULL_TREE;

  /* Transform toascii(c) -> (c & 0x7f).  */
  return fold_build2_loc (loc, BIT_AND_EXPR, integer_type_node, arg,
			  build_int_cst (integer_type_node, 0x7f));
}

/* Fold a call to builtin isdigit with argument ARG.  */

static tree
fold_builtin_isdigit (location_t loc, tree arg)
{
  if (!validate_arg (arg, INTEGER_TYPE))
    return NULL_TREE;
  else
    {
      /* Transform isdigit(c) -> (unsigned)(c) - '0' <= 9.  */
      /* According to the C standard, isdigit is unaffected by locale.
	 However, it definitely is affected by the target character set.  */
      unsigned HOST_WIDE_INT target_digit0
	= lang_hooks.to_target_charset ('0');

      if (target_digit0 == 0)
	return NULL_TREE;

      arg = fold_convert_loc (loc, unsigned_type_node, arg);
      arg = fold_build2 (MINUS_EXPR, unsigned_type_node, arg,
			 build_int_cst (unsigned_type_node, target_digit0));
      return fold_build2_loc (loc, LE_EXPR, integer_type_node, arg,
			  build_int_cst (unsigned_type_node, 9));
    }
}

/* Fold a call to fabs, fabsf or fabsl with argument ARG.  */

static tree
fold_builtin_fabs (location_t loc, tree arg, tree type)
{
  if (!validate_arg (arg, REAL_TYPE))
    return NULL_TREE;

  arg = fold_convert_loc (loc, type, arg);
  return fold_build1_loc (loc, ABS_EXPR, type, arg);
}

/* Fold a call to abs, labs, llabs or imaxabs with argument ARG.  */

static tree
fold_builtin_abs (location_t loc, tree arg, tree type)
{
  if (!validate_arg (arg, INTEGER_TYPE))
    return NULL_TREE;

  arg = fold_convert_loc (loc, type, arg);
  return fold_build1_loc (loc, ABS_EXPR, type, arg);
}

/* Fold a call to fma, fmaf, or fmal with arguments ARG[012].  */

static tree
fold_builtin_fma (location_t loc, tree arg0, tree arg1, tree arg2, tree type)
{
  /* ??? Only expand to FMA_EXPR if it's directly supported.  */
  if (validate_arg (arg0, REAL_TYPE)
      && validate_arg (arg1, REAL_TYPE)
      && validate_arg (arg2, REAL_TYPE)
      && optab_handler (fma_optab, TYPE_MODE (type)) != CODE_FOR_nothing)
    return fold_build3_loc (loc, FMA_EXPR, type, arg0, arg1, arg2);

  return NULL_TREE;
}

/* Fold a call to builtin carg(a+bi) -> atan2(b,a).  */

static tree
fold_builtin_carg (location_t loc, tree arg, tree type)
{
  if (validate_arg (arg, COMPLEX_TYPE)
      && TREE_CODE (TREE_TYPE (TREE_TYPE (arg))) == REAL_TYPE)
    {
      tree atan2_fn = mathfn_built_in (type, BUILT_IN_ATAN2);

      if (atan2_fn)
        {
  	  tree new_arg = builtin_save_expr (arg);
	  tree r_arg = fold_build1_loc (loc, REALPART_EXPR, type, new_arg);
	  tree i_arg = fold_build1_loc (loc, IMAGPART_EXPR, type, new_arg);
	  return build_call_expr_loc (loc, atan2_fn, 2, i_arg, r_arg);
	}
    }

  return NULL_TREE;
}

/* Fold a call to builtin frexp, we can assume the base is 2.  */

static tree
fold_builtin_frexp (location_t loc, tree arg0, tree arg1, tree rettype)
{
  if (! validate_arg (arg0, REAL_TYPE) || ! validate_arg (arg1, POINTER_TYPE))
    return NULL_TREE;

  STRIP_NOPS (arg0);

  if (!(TREE_CODE (arg0) == REAL_CST && ! TREE_OVERFLOW (arg0)))
    return NULL_TREE;

  arg1 = build_fold_indirect_ref_loc (loc, arg1);

  /* Proceed if a valid pointer type was passed in.  */
  if (TYPE_MAIN_VARIANT (TREE_TYPE (arg1)) == integer_type_node)
    {
      const REAL_VALUE_TYPE *const value = TREE_REAL_CST_PTR (arg0);
      tree frac, exp;

      switch (value->cl)
      {
      case rvc_zero:
	/* For +-0, return (*exp = 0, +-0).  */
	exp = integer_zero_node;
	frac = arg0;
	break;
      case rvc_nan:
      case rvc_inf:
	/* For +-NaN or +-Inf, *exp is unspecified, return arg0.  */
	return omit_one_operand_loc (loc, rettype, arg0, arg1);
      case rvc_normal:
	{
	  /* Since the frexp function always expects base 2, and in
	     GCC normalized significands are already in the range
	     [0.5, 1.0), we have exactly what frexp wants.  */
	  REAL_VALUE_TYPE frac_rvt = *value;
	  SET_REAL_EXP (&frac_rvt, 0);
	  frac = build_real (rettype, frac_rvt);
	  exp = build_int_cst (integer_type_node, REAL_EXP (value));
	}
	break;
      default:
	gcc_unreachable ();
      }

      /* Create the COMPOUND_EXPR (*arg1 = trunc, frac). */
      arg1 = fold_build2_loc (loc, MODIFY_EXPR, rettype, arg1, exp);
      TREE_SIDE_EFFECTS (arg1) = 1;
      return fold_build2_loc (loc, COMPOUND_EXPR, rettype, arg1, frac);
    }

  return NULL_TREE;
}

/* Fold a call to builtin modf.  */

static tree
fold_builtin_modf (location_t loc, tree arg0, tree arg1, tree rettype)
{
  if (! validate_arg (arg0, REAL_TYPE) || ! validate_arg (arg1, POINTER_TYPE))
    return NULL_TREE;

  STRIP_NOPS (arg0);

  if (!(TREE_CODE (arg0) == REAL_CST && ! TREE_OVERFLOW (arg0)))
    return NULL_TREE;

  arg1 = build_fold_indirect_ref_loc (loc, arg1);

  /* Proceed if a valid pointer type was passed in.  */
  if (TYPE_MAIN_VARIANT (TREE_TYPE (arg1)) == TYPE_MAIN_VARIANT (rettype))
    {
      const REAL_VALUE_TYPE *const value = TREE_REAL_CST_PTR (arg0);
      REAL_VALUE_TYPE trunc, frac;

      switch (value->cl)
      {
      case rvc_nan:
      case rvc_zero:
	/* For +-NaN or +-0, return (*arg1 = arg0, arg0).  */
	trunc = frac = *value;
	break;
      case rvc_inf:
	/* For +-Inf, return (*arg1 = arg0, +-0).  */
	frac = dconst0;
	frac.sign = value->sign;
	trunc = *value;
	break;
      case rvc_normal:
	/* Return (*arg1 = trunc(arg0), arg0-trunc(arg0)).  */
	real_trunc (&trunc, VOIDmode, value);
	real_arithmetic (&frac, MINUS_EXPR, value, &trunc);
	/* If the original number was negative and already
	   integral, then the fractional part is -0.0.  */
	if (value->sign && frac.cl == rvc_zero)
	  frac.sign = value->sign;
	break;
      }

      /* Create the COMPOUND_EXPR (*arg1 = trunc, frac). */
      arg1 = fold_build2_loc (loc, MODIFY_EXPR, rettype, arg1,
			  build_real (rettype, trunc));
      TREE_SIDE_EFFECTS (arg1) = 1;
      return fold_build2_loc (loc, COMPOUND_EXPR, rettype, arg1,
			  build_real (rettype, frac));
    }

  return NULL_TREE;
}

/* Given a location LOC, an interclass builtin function decl FNDECL
   and its single argument ARG, return an folded expression computing
   the same, or NULL_TREE if we either couldn't or didn't want to fold
   (the latter happen if there's an RTL instruction available).  */

static tree
fold_builtin_interclass_mathfn (location_t loc, tree fndecl, tree arg)
{
  machine_mode mode;

  if (!validate_arg (arg, REAL_TYPE))
    return NULL_TREE;

  if (interclass_mathfn_icode (arg, fndecl) != CODE_FOR_nothing)
    return NULL_TREE;

  mode = TYPE_MODE (TREE_TYPE (arg));

  bool is_ibm_extended = MODE_COMPOSITE_P (mode);

  /* If there is no optab, try generic code.  */
  switch (DECL_FUNCTION_CODE (fndecl))
    {
      tree result;

    CASE_FLT_FN (BUILT_IN_ISINF):
      {
	/* isinf(x) -> isgreater(fabs(x),DBL_MAX).  */
	tree const isgr_fn = builtin_decl_explicit (BUILT_IN_ISGREATER);
	tree type = TREE_TYPE (arg);
	REAL_VALUE_TYPE r;
	char buf[128];

	if (is_ibm_extended)
	  {
	    /* NaN and Inf are encoded in the high-order double value
	       only.  The low-order value is not significant.  */
	    type = double_type_node;
	    mode = DFmode;
	    arg = fold_build1_loc (loc, NOP_EXPR, type, arg);
	  }
	get_max_float (REAL_MODE_FORMAT (mode), buf, sizeof (buf));
	real_from_string (&r, buf);
	result = build_call_expr (isgr_fn, 2,
				  fold_build1_loc (loc, ABS_EXPR, type, arg),
				  build_real (type, r));
	return result;
      }
    CASE_FLT_FN (BUILT_IN_FINITE):
    case BUILT_IN_ISFINITE:
      {
	/* isfinite(x) -> islessequal(fabs(x),DBL_MAX).  */
	tree const isle_fn = builtin_decl_explicit (BUILT_IN_ISLESSEQUAL);
	tree type = TREE_TYPE (arg);
	REAL_VALUE_TYPE r;
	char buf[128];

	if (is_ibm_extended)
	  {
	    /* NaN and Inf are encoded in the high-order double value
	       only.  The low-order value is not significant.  */
	    type = double_type_node;
	    mode = DFmode;
	    arg = fold_build1_loc (loc, NOP_EXPR, type, arg);
	  }
	get_max_float (REAL_MODE_FORMAT (mode), buf, sizeof (buf));
	real_from_string (&r, buf);
	result = build_call_expr (isle_fn, 2,
				  fold_build1_loc (loc, ABS_EXPR, type, arg),
				  build_real (type, r));
	/*result = fold_build2_loc (loc, UNGT_EXPR,
				  TREE_TYPE (TREE_TYPE (fndecl)),
				  fold_build1_loc (loc, ABS_EXPR, type, arg),
				  build_real (type, r));
	result = fold_build1_loc (loc, TRUTH_NOT_EXPR,
				  TREE_TYPE (TREE_TYPE (fndecl)),
				  result);*/
	return result;
      }
    case BUILT_IN_ISNORMAL:
      {
	/* isnormal(x) -> isgreaterequal(fabs(x),DBL_MIN) &
	   islessequal(fabs(x),DBL_MAX).  */
	tree const isle_fn = builtin_decl_explicit (BUILT_IN_ISLESSEQUAL);
	tree type = TREE_TYPE (arg);
	tree orig_arg, max_exp, min_exp;
	machine_mode orig_mode = mode;
	REAL_VALUE_TYPE rmax, rmin;
	char buf[128];

	orig_arg = arg = builtin_save_expr (arg);
	if (is_ibm_extended)
	  {
	    /* Use double to test the normal range of IBM extended
	       precision.  Emin for IBM extended precision is
	       different to emin for IEEE double, being 53 higher
	       since the low double exponent is at least 53 lower
	       than the high double exponent.  */
	    type = double_type_node;
	    mode = DFmode;
	    arg = fold_build1_loc (loc, NOP_EXPR, type, arg);
	  }
	arg = fold_build1_loc (loc, ABS_EXPR, type, arg);

	get_max_float (REAL_MODE_FORMAT (mode), buf, sizeof (buf));
	real_from_string (&rmax, buf);
	sprintf (buf, "0x1p%d", REAL_MODE_FORMAT (orig_mode)->emin - 1);
	real_from_string (&rmin, buf);
	max_exp = build_real (type, rmax);
	min_exp = build_real (type, rmin);

	max_exp = build_call_expr (isle_fn, 2, arg, max_exp);
	if (is_ibm_extended)
	  {
	    /* Testing the high end of the range is done just using
	       the high double, using the same test as isfinite().
	       For the subnormal end of the range we first test the
	       high double, then if its magnitude is equal to the
	       limit of 0x1p-969, we test whether the low double is
	       non-zero and opposite sign to the high double.  */
	    tree const islt_fn = builtin_decl_explicit (BUILT_IN_ISLESS);
	    tree const isgt_fn = builtin_decl_explicit (BUILT_IN_ISGREATER);
	    tree gt_min = build_call_expr (isgt_fn, 2, arg, min_exp);
	    tree eq_min = fold_build2 (EQ_EXPR, integer_type_node,
				       arg, min_exp);
	    tree as_complex = build1 (VIEW_CONVERT_EXPR,
				      complex_double_type_node, orig_arg);
	    tree hi_dbl = build1 (REALPART_EXPR, type, as_complex);
	    tree lo_dbl = build1 (IMAGPART_EXPR, type, as_complex);
	    tree zero = build_real (type, dconst0);
	    tree hilt = build_call_expr (islt_fn, 2, hi_dbl, zero);
	    tree lolt = build_call_expr (islt_fn, 2, lo_dbl, zero);
	    tree logt = build_call_expr (isgt_fn, 2, lo_dbl, zero);
	    tree ok_lo = fold_build1 (TRUTH_NOT_EXPR, integer_type_node,
				      fold_build3 (COND_EXPR,
						   integer_type_node,
						   hilt, logt, lolt));
	    eq_min = fold_build2 (TRUTH_ANDIF_EXPR, integer_type_node,
				  eq_min, ok_lo);
	    min_exp = fold_build2 (TRUTH_ORIF_EXPR, integer_type_node,
				   gt_min, eq_min);
	  }
	else
	  {
	    tree const isge_fn
	      = builtin_decl_explicit (BUILT_IN_ISGREATEREQUAL);
	    min_exp = build_call_expr (isge_fn, 2, arg, min_exp);
	  }
	result = fold_build2 (BIT_AND_EXPR, integer_type_node,
			      max_exp, min_exp);
	return result;
      }
    default:
      break;
    }

  return NULL_TREE;
}

/* Fold a call to __builtin_isnan(), __builtin_isinf, __builtin_finite.
   ARG is the argument for the call.  */

static tree
fold_builtin_classify (location_t loc, tree fndecl, tree arg, int builtin_index)
{
  tree type = TREE_TYPE (TREE_TYPE (fndecl));

  if (!validate_arg (arg, REAL_TYPE))
    return NULL_TREE;

  switch (builtin_index)
    {
    case BUILT_IN_ISINF:
      if (!HONOR_INFINITIES (arg))
	return omit_one_operand_loc (loc, type, integer_zero_node, arg);

      return NULL_TREE;

    case BUILT_IN_ISINF_SIGN:
      {
	/* isinf_sign(x) -> isinf(x) ? (signbit(x) ? -1 : 1) : 0 */
	/* In a boolean context, GCC will fold the inner COND_EXPR to
	   1.  So e.g. "if (isinf_sign(x))" would be folded to just
	   "if (isinf(x) ? 1 : 0)" which becomes "if (isinf(x))". */
	tree signbit_fn = builtin_decl_explicit (BUILT_IN_SIGNBIT);
	tree isinf_fn = builtin_decl_explicit (BUILT_IN_ISINF);
	tree tmp = NULL_TREE;

	arg = builtin_save_expr (arg);

	if (signbit_fn && isinf_fn)
	  {
	    tree signbit_call = build_call_expr_loc (loc, signbit_fn, 1, arg);
	    tree isinf_call = build_call_expr_loc (loc, isinf_fn, 1, arg);

	    signbit_call = fold_build2_loc (loc, NE_EXPR, integer_type_node,
					signbit_call, integer_zero_node);
	    isinf_call = fold_build2_loc (loc, NE_EXPR, integer_type_node,
				      isinf_call, integer_zero_node);

	    tmp = fold_build3_loc (loc, COND_EXPR, integer_type_node, signbit_call,
			       integer_minus_one_node, integer_one_node);
	    tmp = fold_build3_loc (loc, COND_EXPR, integer_type_node,
			       isinf_call, tmp,
			       integer_zero_node);
	  }

	return tmp;
      }

    case BUILT_IN_ISFINITE:
      if (!HONOR_NANS (arg)
	  && !HONOR_INFINITIES (arg))
	return omit_one_operand_loc (loc, type, integer_one_node, arg);

      return NULL_TREE;

    case BUILT_IN_ISNAN:
      if (!HONOR_NANS (arg))
	return omit_one_operand_loc (loc, type, integer_zero_node, arg);

      {
	bool is_ibm_extended = MODE_COMPOSITE_P (TYPE_MODE (TREE_TYPE (arg)));
	if (is_ibm_extended)
	  {
	    /* NaN and Inf are encoded in the high-order double value
	       only.  The low-order value is not significant.  */
	    arg = fold_build1_loc (loc, NOP_EXPR, double_type_node, arg);
	  }
      }
      arg = builtin_save_expr (arg);
      return fold_build2_loc (loc, UNORDERED_EXPR, type, arg, arg);

    default:
      gcc_unreachable ();
    }
}

/* Fold a call to __builtin_fpclassify(int, int, int, int, int, ...).
   This builtin will generate code to return the appropriate floating
   point classification depending on the value of the floating point
   number passed in.  The possible return values must be supplied as
   int arguments to the call in the following order: FP_NAN, FP_INFINITE,
   FP_NORMAL, FP_SUBNORMAL and FP_ZERO.  The ellipses is for exactly
   one floating point argument which is "type generic".  */

static tree
fold_builtin_fpclassify (location_t loc, tree *args, int nargs)
{
  tree fp_nan, fp_infinite, fp_normal, fp_subnormal, fp_zero,
    arg, type, res, tmp;
  machine_mode mode;
  REAL_VALUE_TYPE r;
  char buf[128];

  /* Verify the required arguments in the original call.  */
  if (nargs != 6
      || !validate_arg (args[0], INTEGER_TYPE)
      || !validate_arg (args[1], INTEGER_TYPE)
      || !validate_arg (args[2], INTEGER_TYPE)
      || !validate_arg (args[3], INTEGER_TYPE)
      || !validate_arg (args[4], INTEGER_TYPE)
      || !validate_arg (args[5], REAL_TYPE))
    return NULL_TREE;

  fp_nan = args[0];
  fp_infinite = args[1];
  fp_normal = args[2];
  fp_subnormal = args[3];
  fp_zero = args[4];
  arg = args[5];
  type = TREE_TYPE (arg);
  mode = TYPE_MODE (type);
  arg = builtin_save_expr (fold_build1_loc (loc, ABS_EXPR, type, arg));

  /* fpclassify(x) ->
       isnan(x) ? FP_NAN :
         (fabs(x) == Inf ? FP_INFINITE :
	   (fabs(x) >= DBL_MIN ? FP_NORMAL :
	     (x == 0 ? FP_ZERO : FP_SUBNORMAL))).  */

  tmp = fold_build2_loc (loc, EQ_EXPR, integer_type_node, arg,
		     build_real (type, dconst0));
  res = fold_build3_loc (loc, COND_EXPR, integer_type_node,
		     tmp, fp_zero, fp_subnormal);

  sprintf (buf, "0x1p%d", REAL_MODE_FORMAT (mode)->emin - 1);
  real_from_string (&r, buf);
  tmp = fold_build2_loc (loc, GE_EXPR, integer_type_node,
		     arg, build_real (type, r));
  res = fold_build3_loc (loc, COND_EXPR, integer_type_node, tmp, fp_normal, res);

  if (HONOR_INFINITIES (mode))
    {
      real_inf (&r);
      tmp = fold_build2_loc (loc, EQ_EXPR, integer_type_node, arg,
			 build_real (type, r));
      res = fold_build3_loc (loc, COND_EXPR, integer_type_node, tmp,
			 fp_infinite, res);
    }

  if (HONOR_NANS (mode))
    {
      tmp = fold_build2_loc (loc, ORDERED_EXPR, integer_type_node, arg, arg);
      res = fold_build3_loc (loc, COND_EXPR, integer_type_node, tmp, res, fp_nan);
    }

  return res;
}

/* Fold a call to an unordered comparison function such as
   __builtin_isgreater().  FNDECL is the FUNCTION_DECL for the function
   being called and ARG0 and ARG1 are the arguments for the call.
   UNORDERED_CODE and ORDERED_CODE are comparison codes that give
   the opposite of the desired result.  UNORDERED_CODE is used
   for modes that can hold NaNs and ORDERED_CODE is used for
   the rest.  */

static tree
fold_builtin_unordered_cmp (location_t loc, tree fndecl, tree arg0, tree arg1,
			    enum tree_code unordered_code,
			    enum tree_code ordered_code)
{
  tree type = TREE_TYPE (TREE_TYPE (fndecl));
  enum tree_code code;
  tree type0, type1;
  enum tree_code code0, code1;
  tree cmp_type = NULL_TREE;

  type0 = TREE_TYPE (arg0);
  type1 = TREE_TYPE (arg1);

  code0 = TREE_CODE (type0);
  code1 = TREE_CODE (type1);

  if (code0 == REAL_TYPE && code1 == REAL_TYPE)
    /* Choose the wider of two real types.  */
    cmp_type = TYPE_PRECISION (type0) >= TYPE_PRECISION (type1)
      ? type0 : type1;
  else if (code0 == REAL_TYPE && code1 == INTEGER_TYPE)
    cmp_type = type0;
  else if (code0 == INTEGER_TYPE && code1 == REAL_TYPE)
    cmp_type = type1;

  arg0 = fold_convert_loc (loc, cmp_type, arg0);
  arg1 = fold_convert_loc (loc, cmp_type, arg1);

  if (unordered_code == UNORDERED_EXPR)
    {
      if (!HONOR_NANS (arg0))
	return omit_two_operands_loc (loc, type, integer_zero_node, arg0, arg1);
      return fold_build2_loc (loc, UNORDERED_EXPR, type, arg0, arg1);
    }

  code = HONOR_NANS (arg0) ? unordered_code : ordered_code;
  return fold_build1_loc (loc, TRUTH_NOT_EXPR, type,
		      fold_build2_loc (loc, code, type, arg0, arg1));
}

/* Fold __builtin_{,s,u}{add,sub,mul}{,l,ll}_overflow, either into normal
   arithmetics if it can never overflow, or into internal functions that
   return both result of arithmetics and overflowed boolean flag in
   a complex integer result, or some other check for overflow.
   Similarly fold __builtin_{add,sub,mul}_overflow_p to just the overflow
   checking part of that.  */

static tree
fold_builtin_arith_overflow (location_t loc, enum built_in_function fcode,
			     tree arg0, tree arg1, tree arg2)
{
  enum internal_fn ifn = IFN_LAST;
  /* The code of the expression corresponding to the type-generic
     built-in, or ERROR_MARK for the type-specific ones.  */
  enum tree_code opcode = ERROR_MARK;
  bool ovf_only = false;

  switch (fcode)
    {
    case BUILT_IN_ADD_OVERFLOW_P:
      ovf_only = true;
      /* FALLTHRU */
    case BUILT_IN_ADD_OVERFLOW:
      opcode = PLUS_EXPR;
      /* FALLTHRU */
    case BUILT_IN_SADD_OVERFLOW:
    case BUILT_IN_SADDL_OVERFLOW:
    case BUILT_IN_SADDLL_OVERFLOW:
    case BUILT_IN_UADD_OVERFLOW:
    case BUILT_IN_UADDL_OVERFLOW:
    case BUILT_IN_UADDLL_OVERFLOW:
      ifn = IFN_ADD_OVERFLOW;
      break;
    case BUILT_IN_SUB_OVERFLOW_P:
      ovf_only = true;
      /* FALLTHRU */
    case BUILT_IN_SUB_OVERFLOW:
      opcode = MINUS_EXPR;
      /* FALLTHRU */
    case BUILT_IN_SSUB_OVERFLOW:
    case BUILT_IN_SSUBL_OVERFLOW:
    case BUILT_IN_SSUBLL_OVERFLOW:
    case BUILT_IN_USUB_OVERFLOW:
    case BUILT_IN_USUBL_OVERFLOW:
    case BUILT_IN_USUBLL_OVERFLOW:
      ifn = IFN_SUB_OVERFLOW;
      break;
    case BUILT_IN_MUL_OVERFLOW_P:
      ovf_only = true;
      /* FALLTHRU */
    case BUILT_IN_MUL_OVERFLOW:
      opcode = MULT_EXPR;
      /* FALLTHRU */
    case BUILT_IN_SMUL_OVERFLOW:
    case BUILT_IN_SMULL_OVERFLOW:
    case BUILT_IN_SMULLL_OVERFLOW:
    case BUILT_IN_UMUL_OVERFLOW:
    case BUILT_IN_UMULL_OVERFLOW:
    case BUILT_IN_UMULLL_OVERFLOW:
      ifn = IFN_MUL_OVERFLOW;
      break;
    default:
      gcc_unreachable ();
    }

  /* For the "generic" overloads, the first two arguments can have different
     types and the last argument determines the target type to use to check
     for overflow.  The arguments of the other overloads all have the same
     type.  */
  tree type = ovf_only ? TREE_TYPE (arg2) : TREE_TYPE (TREE_TYPE (arg2));

  /* For the __builtin_{add,sub,mul}_overflow_p builtins, when the first two
     arguments are constant, attempt to fold the built-in call into a constant
     expression indicating whether or not it detected an overflow.  */
  if (ovf_only
      && TREE_CODE (arg0) == INTEGER_CST
      && TREE_CODE (arg1) == INTEGER_CST)
    /* Perform the computation in the target type and check for overflow.  */
    return omit_one_operand_loc (loc, boolean_type_node,
				 arith_overflowed_p (opcode, type, arg0, arg1)
				 ? boolean_true_node : boolean_false_node,
				 arg2);

  tree ctype = build_complex_type (type);
  tree call = build_call_expr_internal_loc (loc, ifn, ctype,
					    2, arg0, arg1);
  tree tgt = save_expr (call);
  tree intres = build1_loc (loc, REALPART_EXPR, type, tgt);
  tree ovfres = build1_loc (loc, IMAGPART_EXPR, type, tgt);
  ovfres = fold_convert_loc (loc, boolean_type_node, ovfres);

  if (ovf_only)
    return omit_one_operand_loc (loc, boolean_type_node, ovfres, arg2);

  tree mem_arg2 = build_fold_indirect_ref_loc (loc, arg2);
  tree store
    = fold_build2_loc (loc, MODIFY_EXPR, void_type_node, mem_arg2, intres);
  return build2_loc (loc, COMPOUND_EXPR, boolean_type_node, store, ovfres);
}

/* Fold a call to __builtin_FILE to a constant string.  */

static inline tree
fold_builtin_FILE (location_t loc)
{
  if (const char *fname = LOCATION_FILE (loc))
    return build_string_literal (strlen (fname) + 1, fname);

  return build_string_literal (1, "");
}

/* Fold a call to __builtin_FUNCTION to a constant string.  */

static inline tree
fold_builtin_FUNCTION ()
{
  const char *name = "";

  if (current_function_decl)
    name = lang_hooks.decl_printable_name (current_function_decl, 0);

  return build_string_literal (strlen (name) + 1, name);
}

/* Fold a call to __builtin_LINE to an integer constant.  */

static inline tree
fold_builtin_LINE (location_t loc, tree type)
{
  return build_int_cst (type, LOCATION_LINE (loc));
}

/* Fold a call to built-in function FNDECL with 0 arguments.
   This function returns NULL_TREE if no simplification was possible.  */

static tree
fold_builtin_0 (location_t loc, tree fndecl)
{
  tree type = TREE_TYPE (TREE_TYPE (fndecl));
  enum built_in_function fcode = DECL_FUNCTION_CODE (fndecl);
  switch (fcode)
    {
    case BUILT_IN_FILE:
      return fold_builtin_FILE (loc);

    case BUILT_IN_FUNCTION:
      return fold_builtin_FUNCTION ();

    case BUILT_IN_LINE:
      return fold_builtin_LINE (loc, type);

    CASE_FLT_FN (BUILT_IN_INF):
    CASE_FLT_FN_FLOATN_NX (BUILT_IN_INF):
    case BUILT_IN_INFD32:
    case BUILT_IN_INFD64:
    case BUILT_IN_INFD128:
      return fold_builtin_inf (loc, type, true);

    CASE_FLT_FN (BUILT_IN_HUGE_VAL):
    CASE_FLT_FN_FLOATN_NX (BUILT_IN_HUGE_VAL):
      return fold_builtin_inf (loc, type, false);

    case BUILT_IN_CLASSIFY_TYPE:
      return fold_builtin_classify_type (NULL_TREE);

    default:
      break;
    }
  return NULL_TREE;
}

/* Fold a call to built-in function FNDECL with 1 argument, ARG0.
   This function returns NULL_TREE if no simplification was possible.  */

static tree
fold_builtin_1 (location_t loc, tree fndecl, tree arg0)
{
  tree type = TREE_TYPE (TREE_TYPE (fndecl));
  enum built_in_function fcode = DECL_FUNCTION_CODE (fndecl);

  if (TREE_CODE (arg0) == ERROR_MARK)
    return NULL_TREE;

  if (tree ret = fold_const_call (as_combined_fn (fcode), type, arg0))
    return ret;

  switch (fcode)
    {
    case BUILT_IN_CONSTANT_P:
      {
	tree val = fold_builtin_constant_p (arg0);

	/* Gimplification will pull the CALL_EXPR for the builtin out of
	   an if condition.  When not optimizing, we'll not CSE it back.
	   To avoid link error types of regressions, return false now.  */
	if (!val && !optimize)
	  val = integer_zero_node;

	return val;
      }

    case BUILT_IN_CLASSIFY_TYPE:
      return fold_builtin_classify_type (arg0);

    case BUILT_IN_STRLEN:
      return fold_builtin_strlen (loc, type, arg0);

    CASE_FLT_FN (BUILT_IN_FABS):
    CASE_FLT_FN_FLOATN_NX (BUILT_IN_FABS):
    case BUILT_IN_FABSD32:
    case BUILT_IN_FABSD64:
    case BUILT_IN_FABSD128:
      return fold_builtin_fabs (loc, arg0, type);

    case BUILT_IN_ABS:
    case BUILT_IN_LABS:
    case BUILT_IN_LLABS:
    case BUILT_IN_IMAXABS:
      return fold_builtin_abs (loc, arg0, type);

    CASE_FLT_FN (BUILT_IN_CONJ):
      if (validate_arg (arg0, COMPLEX_TYPE)
	&& TREE_CODE (TREE_TYPE (TREE_TYPE (arg0))) == REAL_TYPE)
	return fold_build1_loc (loc, CONJ_EXPR, type, arg0);
    break;

    CASE_FLT_FN (BUILT_IN_CREAL):
      if (validate_arg (arg0, COMPLEX_TYPE)
	&& TREE_CODE (TREE_TYPE (TREE_TYPE (arg0))) == REAL_TYPE)
	return non_lvalue_loc (loc, fold_build1_loc (loc, REALPART_EXPR, type, arg0));
    break;

    CASE_FLT_FN (BUILT_IN_CIMAG):
      if (validate_arg (arg0, COMPLEX_TYPE)
	  && TREE_CODE (TREE_TYPE (TREE_TYPE (arg0))) == REAL_TYPE)
	return non_lvalue_loc (loc, fold_build1_loc (loc, IMAGPART_EXPR, type, arg0));
    break;

    CASE_FLT_FN (BUILT_IN_CARG):
      return fold_builtin_carg (loc, arg0, type);

    case BUILT_IN_ISASCII:
      return fold_builtin_isascii (loc, arg0);

    case BUILT_IN_TOASCII:
      return fold_builtin_toascii (loc, arg0);

    case BUILT_IN_ISDIGIT:
      return fold_builtin_isdigit (loc, arg0);

    CASE_FLT_FN (BUILT_IN_FINITE):
    case BUILT_IN_FINITED32:
    case BUILT_IN_FINITED64:
    case BUILT_IN_FINITED128:
    case BUILT_IN_ISFINITE:
      {
	tree ret = fold_builtin_classify (loc, fndecl, arg0, BUILT_IN_ISFINITE);
	if (ret)
	  return ret;
	return fold_builtin_interclass_mathfn (loc, fndecl, arg0);
      }

    CASE_FLT_FN (BUILT_IN_ISINF):
    case BUILT_IN_ISINFD32:
    case BUILT_IN_ISINFD64:
    case BUILT_IN_ISINFD128:
      {
	tree ret = fold_builtin_classify (loc, fndecl, arg0, BUILT_IN_ISINF);
	if (ret)
	  return ret;
	return fold_builtin_interclass_mathfn (loc, fndecl, arg0);
      }

    case BUILT_IN_ISNORMAL:
      return fold_builtin_interclass_mathfn (loc, fndecl, arg0);

    case BUILT_IN_ISINF_SIGN:
      return fold_builtin_classify (loc, fndecl, arg0, BUILT_IN_ISINF_SIGN);

    CASE_FLT_FN (BUILT_IN_ISNAN):
    case BUILT_IN_ISNAND32:
    case BUILT_IN_ISNAND64:
    case BUILT_IN_ISNAND128:
      return fold_builtin_classify (loc, fndecl, arg0, BUILT_IN_ISNAN);

    case BUILT_IN_FREE:
      if (integer_zerop (arg0))
	return build_empty_stmt (loc);
      break;

    default:
      break;
    }

  return NULL_TREE;

}

/* Fold a call to built-in function FNDECL with 2 arguments, ARG0 and ARG1.
   This function returns NULL_TREE if no simplification was possible.  */

static tree
fold_builtin_2 (location_t loc, tree fndecl, tree arg0, tree arg1)
{
  tree type = TREE_TYPE (TREE_TYPE (fndecl));
  enum built_in_function fcode = DECL_FUNCTION_CODE (fndecl);

  if (TREE_CODE (arg0) == ERROR_MARK
      || TREE_CODE (arg1) == ERROR_MARK)
    return NULL_TREE;

  if (tree ret = fold_const_call (as_combined_fn (fcode), type, arg0, arg1))
    return ret;

  switch (fcode)
    {
    CASE_FLT_FN_REENT (BUILT_IN_GAMMA): /* GAMMA_R */
    CASE_FLT_FN_REENT (BUILT_IN_LGAMMA): /* LGAMMA_R */
      if (validate_arg (arg0, REAL_TYPE)
	  && validate_arg (arg1, POINTER_TYPE))
	return do_mpfr_lgamma_r (arg0, arg1, type);
    break;

    CASE_FLT_FN (BUILT_IN_FREXP):
      return fold_builtin_frexp (loc, arg0, arg1, type);

    CASE_FLT_FN (BUILT_IN_MODF):
      return fold_builtin_modf (loc, arg0, arg1, type);

    case BUILT_IN_STRSPN:
      return fold_builtin_strspn (loc, arg0, arg1);

    case BUILT_IN_STRCSPN:
      return fold_builtin_strcspn (loc, arg0, arg1);

    case BUILT_IN_STRPBRK:
      return fold_builtin_strpbrk (loc, arg0, arg1, type);

    case BUILT_IN_EXPECT:
      return fold_builtin_expect (loc, arg0, arg1, NULL_TREE);

    case BUILT_IN_ISGREATER:
      return fold_builtin_unordered_cmp (loc, fndecl,
					 arg0, arg1, UNLE_EXPR, LE_EXPR);
    case BUILT_IN_ISGREATEREQUAL:
      return fold_builtin_unordered_cmp (loc, fndecl,
					 arg0, arg1, UNLT_EXPR, LT_EXPR);
    case BUILT_IN_ISLESS:
      return fold_builtin_unordered_cmp (loc, fndecl,
					 arg0, arg1, UNGE_EXPR, GE_EXPR);
    case BUILT_IN_ISLESSEQUAL:
      return fold_builtin_unordered_cmp (loc, fndecl,
					 arg0, arg1, UNGT_EXPR, GT_EXPR);
    case BUILT_IN_ISLESSGREATER:
      return fold_builtin_unordered_cmp (loc, fndecl,
					 arg0, arg1, UNEQ_EXPR, EQ_EXPR);
    case BUILT_IN_ISUNORDERED:
      return fold_builtin_unordered_cmp (loc, fndecl,
					 arg0, arg1, UNORDERED_EXPR,
					 NOP_EXPR);

      /* We do the folding for va_start in the expander.  */
    case BUILT_IN_VA_START:
      break;

    case BUILT_IN_OBJECT_SIZE:
      return fold_builtin_object_size (arg0, arg1);

    case BUILT_IN_ATOMIC_ALWAYS_LOCK_FREE:
      return fold_builtin_atomic_always_lock_free (arg0, arg1);

    case BUILT_IN_ATOMIC_IS_LOCK_FREE:
      return fold_builtin_atomic_is_lock_free (arg0, arg1);

    default:
      break;
    }
  return NULL_TREE;
}

/* Fold a call to built-in function FNDECL with 3 arguments, ARG0, ARG1,
   and ARG2.
   This function returns NULL_TREE if no simplification was possible.  */

static tree
fold_builtin_3 (location_t loc, tree fndecl,
		tree arg0, tree arg1, tree arg2)
{
  tree type = TREE_TYPE (TREE_TYPE (fndecl));
  enum built_in_function fcode = DECL_FUNCTION_CODE (fndecl);

  if (TREE_CODE (arg0) == ERROR_MARK
      || TREE_CODE (arg1) == ERROR_MARK
      || TREE_CODE (arg2) == ERROR_MARK)
    return NULL_TREE;

  if (tree ret = fold_const_call (as_combined_fn (fcode), type,
				  arg0, arg1, arg2))
    return ret;

  switch (fcode)
    {

    CASE_FLT_FN (BUILT_IN_SINCOS):
      return fold_builtin_sincos (loc, arg0, arg1, arg2);

    CASE_FLT_FN (BUILT_IN_FMA):
      return fold_builtin_fma (loc, arg0, arg1, arg2, type);

    CASE_FLT_FN (BUILT_IN_REMQUO):
      if (validate_arg (arg0, REAL_TYPE)
	  && validate_arg (arg1, REAL_TYPE)
	  && validate_arg (arg2, POINTER_TYPE))
	return do_mpfr_remquo (arg0, arg1, arg2);
    break;

    case BUILT_IN_MEMCMP:
      return fold_builtin_memcmp (loc, arg0, arg1, arg2);;

    case BUILT_IN_EXPECT:
      return fold_builtin_expect (loc, arg0, arg1, arg2);

    case BUILT_IN_ADD_OVERFLOW:
    case BUILT_IN_SUB_OVERFLOW:
    case BUILT_IN_MUL_OVERFLOW:
    case BUILT_IN_ADD_OVERFLOW_P:
    case BUILT_IN_SUB_OVERFLOW_P:
    case BUILT_IN_MUL_OVERFLOW_P:
    case BUILT_IN_SADD_OVERFLOW:
    case BUILT_IN_SADDL_OVERFLOW:
    case BUILT_IN_SADDLL_OVERFLOW:
    case BUILT_IN_SSUB_OVERFLOW:
    case BUILT_IN_SSUBL_OVERFLOW:
    case BUILT_IN_SSUBLL_OVERFLOW:
    case BUILT_IN_SMUL_OVERFLOW:
    case BUILT_IN_SMULL_OVERFLOW:
    case BUILT_IN_SMULLL_OVERFLOW:
    case BUILT_IN_UADD_OVERFLOW:
    case BUILT_IN_UADDL_OVERFLOW:
    case BUILT_IN_UADDLL_OVERFLOW:
    case BUILT_IN_USUB_OVERFLOW:
    case BUILT_IN_USUBL_OVERFLOW:
    case BUILT_IN_USUBLL_OVERFLOW:
    case BUILT_IN_UMUL_OVERFLOW:
    case BUILT_IN_UMULL_OVERFLOW:
    case BUILT_IN_UMULLL_OVERFLOW:
      return fold_builtin_arith_overflow (loc, fcode, arg0, arg1, arg2);

    default:
      break;
    }
  return NULL_TREE;
}

/* Fold a call to built-in function FNDECL.  ARGS is an array of NARGS
   arguments.  IGNORE is true if the result of the
   function call is ignored.  This function returns NULL_TREE if no
   simplification was possible.  */

tree
fold_builtin_n (location_t loc, tree fndecl, tree *args, int nargs, bool)
{
  tree ret = NULL_TREE;

  switch (nargs)
    {
    case 0:
      ret = fold_builtin_0 (loc, fndecl);
      break;
    case 1:
      ret = fold_builtin_1 (loc, fndecl, args[0]);
      break;
    case 2:
      ret = fold_builtin_2 (loc, fndecl, args[0], args[1]);
      break;
    case 3:
      ret = fold_builtin_3 (loc, fndecl, args[0], args[1], args[2]);
      break;
    default:
      ret = fold_builtin_varargs (loc, fndecl, args, nargs);
      break;
    }
  if (ret)
    {
      ret = build1 (NOP_EXPR, TREE_TYPE (ret), ret);
      SET_EXPR_LOCATION (ret, loc);
      TREE_NO_WARNING (ret) = 1;
      return ret;
    }
  return NULL_TREE;
}

/* Construct a new CALL_EXPR to FNDECL using the tail of the argument
   list ARGS along with N new arguments in NEWARGS.  SKIP is the number
   of arguments in ARGS to be omitted.  OLDNARGS is the number of
   elements in ARGS.  */

static tree
rewrite_call_expr_valist (location_t loc, int oldnargs, tree *args,
			  int skip, tree fndecl, int n, va_list newargs)
{
  int nargs = oldnargs - skip + n;
  tree *buffer;

  if (n > 0)
    {
      int i, j;

      buffer = XALLOCAVEC (tree, nargs);
      for (i = 0; i < n; i++)
	buffer[i] = va_arg (newargs, tree);
      for (j = skip; j < oldnargs; j++, i++)
	buffer[i] = args[j];
    }
  else
    buffer = args + skip;

  return build_call_expr_loc_array (loc, fndecl, nargs, buffer);
}

/* Return true if FNDECL shouldn't be folded right now.
   If a built-in function has an inline attribute always_inline
   wrapper, defer folding it after always_inline functions have
   been inlined, otherwise e.g. -D_FORTIFY_SOURCE checking
   might not be performed.  */

bool
avoid_folding_inline_builtin (tree fndecl)
{
  return (DECL_DECLARED_INLINE_P (fndecl)
	  && DECL_DISREGARD_INLINE_LIMITS (fndecl)
	  && cfun
	  && !cfun->always_inline_functions_inlined
	  && lookup_attribute ("always_inline", DECL_ATTRIBUTES (fndecl)));
}

/* A wrapper function for builtin folding that prevents warnings for
   "statement without effect" and the like, caused by removing the
   call node earlier than the warning is generated.  */

tree
fold_call_expr (location_t loc, tree exp, bool ignore)
{
  tree ret = NULL_TREE;
  tree fndecl = get_callee_fndecl (exp);
  if (fndecl
      && TREE_CODE (fndecl) == FUNCTION_DECL
      && DECL_BUILT_IN (fndecl)
      /* If CALL_EXPR_VA_ARG_PACK is set, the arguments aren't finalized
	 yet.  Defer folding until we see all the arguments
	 (after inlining).  */
      && !CALL_EXPR_VA_ARG_PACK (exp))
    {
      int nargs = call_expr_nargs (exp);

      /* Before gimplification CALL_EXPR_VA_ARG_PACK is not set, but
	 instead last argument is __builtin_va_arg_pack ().  Defer folding
	 even in that case, until arguments are finalized.  */
      if (nargs && TREE_CODE (CALL_EXPR_ARG (exp, nargs - 1)) == CALL_EXPR)
	{
	  tree fndecl2 = get_callee_fndecl (CALL_EXPR_ARG (exp, nargs - 1));
	  if (fndecl2
	      && TREE_CODE (fndecl2) == FUNCTION_DECL
	      && DECL_BUILT_IN_CLASS (fndecl2) == BUILT_IN_NORMAL
	      && DECL_FUNCTION_CODE (fndecl2) == BUILT_IN_VA_ARG_PACK)
	    return NULL_TREE;
	}

      if (avoid_folding_inline_builtin (fndecl))
	return NULL_TREE;

      if (DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_MD)
        return targetm.fold_builtin (fndecl, call_expr_nargs (exp),
				     CALL_EXPR_ARGP (exp), ignore);
      else
	{
	  tree *args = CALL_EXPR_ARGP (exp);
	  ret = fold_builtin_n (loc, fndecl, args, nargs, ignore);
	  if (ret)
	    return ret;
	}
    }
  return NULL_TREE;
}

/* Fold a CALL_EXPR with type TYPE with FN as the function expression.
   N arguments are passed in the array ARGARRAY.  Return a folded
   expression or NULL_TREE if no simplification was possible.  */

tree
fold_builtin_call_array (location_t loc, tree,
			 tree fn,
			 int n,
			 tree *argarray)
{
  if (TREE_CODE (fn) != ADDR_EXPR)
    return NULL_TREE;

  tree fndecl = TREE_OPERAND (fn, 0);
  if (TREE_CODE (fndecl) == FUNCTION_DECL
      && DECL_BUILT_IN (fndecl))
    {
      /* If last argument is __builtin_va_arg_pack (), arguments to this
	 function are not finalized yet.  Defer folding until they are.  */
      if (n && TREE_CODE (argarray[n - 1]) == CALL_EXPR)
	{
	  tree fndecl2 = get_callee_fndecl (argarray[n - 1]);
	  if (fndecl2
	      && TREE_CODE (fndecl2) == FUNCTION_DECL
	      && DECL_BUILT_IN_CLASS (fndecl2) == BUILT_IN_NORMAL
	      && DECL_FUNCTION_CODE (fndecl2) == BUILT_IN_VA_ARG_PACK)
	    return NULL_TREE;
	}
      if (avoid_folding_inline_builtin (fndecl))
	return NULL_TREE;
      if (DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_MD)
	return targetm.fold_builtin (fndecl, n, argarray, false);
      else
	return fold_builtin_n (loc, fndecl, argarray, n, false);
    }

  return NULL_TREE;
}

/* Construct a new CALL_EXPR using the tail of the argument list of EXP
   along with N new arguments specified as the "..." parameters.  SKIP
   is the number of arguments in EXP to be omitted.  This function is used
   to do varargs-to-varargs transformations.  */

static tree
rewrite_call_expr (location_t loc, tree exp, int skip, tree fndecl, int n, ...)
{
  va_list ap;
  tree t;

  va_start (ap, n);
  t = rewrite_call_expr_valist (loc, call_expr_nargs (exp),
				CALL_EXPR_ARGP (exp), skip, fndecl, n, ap);
  va_end (ap);

  return t;
}

/* Validate a single argument ARG against a tree code CODE representing
   a type.  Return true when argument is valid.  */

static bool
validate_arg (const_tree arg, enum tree_code code)
{
  if (!arg)
    return false;
  else if (code == POINTER_TYPE)
    return POINTER_TYPE_P (TREE_TYPE (arg));
  else if (code == INTEGER_TYPE)
    return INTEGRAL_TYPE_P (TREE_TYPE (arg));
  return code == TREE_CODE (TREE_TYPE (arg));
}

/* This function validates the types of a function call argument list
   against a specified list of tree_codes.  If the last specifier is a 0,
   that represents an ellipses, otherwise the last specifier must be a
   VOID_TYPE.

   This is the GIMPLE version of validate_arglist.  Eventually we want to
   completely convert builtins.c to work from GIMPLEs and the tree based
   validate_arglist will then be removed.  */

bool
validate_gimple_arglist (const gcall *call, ...)
{
  enum tree_code code;
  bool res = 0;
  va_list ap;
  const_tree arg;
  size_t i;

  va_start (ap, call);
  i = 0;

  do
    {
      code = (enum tree_code) va_arg (ap, int);
      switch (code)
	{
	case 0:
	  /* This signifies an ellipses, any further arguments are all ok.  */
	  res = true;
	  goto end;
	case VOID_TYPE:
	  /* This signifies an endlink, if no arguments remain, return
	     true, otherwise return false.  */
	  res = (i == gimple_call_num_args (call));
	  goto end;
	default:
	  /* If no parameters remain or the parameter's code does not
	     match the specified code, return false.  Otherwise continue
	     checking any remaining arguments.  */
	  arg = gimple_call_arg (call, i++);
	  if (!validate_arg (arg, code))
	    goto end;
	  break;
	}
    }
  while (1);

  /* We need gotos here since we can only have one VA_CLOSE in a
     function.  */
 end: ;
  va_end (ap);

  return res;
}

/* Default target-specific builtin expander that does nothing.  */

rtx
default_expand_builtin (tree exp ATTRIBUTE_UNUSED,
			rtx target ATTRIBUTE_UNUSED,
			rtx subtarget ATTRIBUTE_UNUSED,
			machine_mode mode ATTRIBUTE_UNUSED,
			int ignore ATTRIBUTE_UNUSED)
{
  return NULL_RTX;
}

/* Returns true is EXP represents data that would potentially reside
   in a readonly section.  */

bool
readonly_data_expr (tree exp)
{
  STRIP_NOPS (exp);

  if (TREE_CODE (exp) != ADDR_EXPR)
    return false;

  exp = get_base_address (TREE_OPERAND (exp, 0));
  if (!exp)
    return false;

  /* Make sure we call decl_readonly_section only for trees it
     can handle (since it returns true for everything it doesn't
     understand).  */
  if (TREE_CODE (exp) == STRING_CST
      || TREE_CODE (exp) == CONSTRUCTOR
      || (VAR_P (exp) && TREE_STATIC (exp)))
    return decl_readonly_section (exp, 0);
  else
    return false;
}

/* Simplify a call to the strpbrk builtin.  S1 and S2 are the arguments
   to the call, and TYPE is its return type.

   Return NULL_TREE if no simplification was possible, otherwise return the
   simplified form of the call as a tree.

   The simplified form may be a constant or other expression which
   computes the same value, but in a more efficient manner (including
   calls to other builtin functions).

   The call may contain arguments which need to be evaluated, but
   which are not useful to determine the result of the call.  In
   this case we return a chain of COMPOUND_EXPRs.  The LHS of each
   COMPOUND_EXPR will be an argument which must be evaluated.
   COMPOUND_EXPRs are chained through their RHS.  The RHS of the last
   COMPOUND_EXPR in the chain will contain the tree for the simplified
   form of the builtin function call.  */

static tree
fold_builtin_strpbrk (location_t loc, tree s1, tree s2, tree type)
{
  if (!validate_arg (s1, POINTER_TYPE)
      || !validate_arg (s2, POINTER_TYPE))
    return NULL_TREE;
  else
    {
      tree fn;
      const char *p1, *p2;

      p2 = c_getstr (s2);
      if (p2 == NULL)
	return NULL_TREE;

      p1 = c_getstr (s1);
      if (p1 != NULL)
	{
	  const char *r = strpbrk (p1, p2);
	  tree tem;

	  if (r == NULL)
	    return build_int_cst (TREE_TYPE (s1), 0);

	  /* Return an offset into the constant string argument.  */
	  tem = fold_build_pointer_plus_hwi_loc (loc, s1, r - p1);
	  return fold_convert_loc (loc, type, tem);
	}

      if (p2[0] == '\0')
	/* strpbrk(x, "") == NULL.
	   Evaluate and ignore s1 in case it had side-effects.  */
	return omit_one_operand_loc (loc, TREE_TYPE (s1), integer_zero_node, s1);

      if (p2[1] != '\0')
	return NULL_TREE;  /* Really call strpbrk.  */

      fn = builtin_decl_implicit (BUILT_IN_STRCHR);
      if (!fn)
	return NULL_TREE;

      /* New argument list transforming strpbrk(s1, s2) to
	 strchr(s1, s2[0]).  */
      return build_call_expr_loc (loc, fn, 2, s1,
				  build_int_cst (integer_type_node, p2[0]));
    }
}

/* Simplify a call to the strspn builtin.  S1 and S2 are the arguments
   to the call.

   Return NULL_TREE if no simplification was possible, otherwise return the
   simplified form of the call as a tree.

   The simplified form may be a constant or other expression which
   computes the same value, but in a more efficient manner (including
   calls to other builtin functions).

   The call may contain arguments which need to be evaluated, but
   which are not useful to determine the result of the call.  In
   this case we return a chain of COMPOUND_EXPRs.  The LHS of each
   COMPOUND_EXPR will be an argument which must be evaluated.
   COMPOUND_EXPRs are chained through their RHS.  The RHS of the last
   COMPOUND_EXPR in the chain will contain the tree for the simplified
   form of the builtin function call.  */

static tree
fold_builtin_strspn (location_t loc, tree s1, tree s2)
{
  if (!validate_arg (s1, POINTER_TYPE)
      || !validate_arg (s2, POINTER_TYPE))
    return NULL_TREE;
  else
    {
      const char *p1 = c_getstr (s1), *p2 = c_getstr (s2);

      /* If either argument is "", return NULL_TREE.  */
      if ((p1 && *p1 == '\0') || (p2 && *p2 == '\0'))
	/* Evaluate and ignore both arguments in case either one has
	   side-effects.  */
	return omit_two_operands_loc (loc, size_type_node, size_zero_node,
				  s1, s2);
      return NULL_TREE;
    }
}

/* Simplify a call to the strcspn builtin.  S1 and S2 are the arguments
   to the call.

   Return NULL_TREE if no simplification was possible, otherwise return the
   simplified form of the call as a tree.

   The simplified form may be a constant or other expression which
   computes the same value, but in a more efficient manner (including
   calls to other builtin functions).

   The call may contain arguments which need to be evaluated, but
   which are not useful to determine the result of the call.  In
   this case we return a chain of COMPOUND_EXPRs.  The LHS of each
   COMPOUND_EXPR will be an argument which must be evaluated.
   COMPOUND_EXPRs are chained through their RHS.  The RHS of the last
   COMPOUND_EXPR in the chain will contain the tree for the simplified
   form of the builtin function call.  */

static tree
fold_builtin_strcspn (location_t loc, tree s1, tree s2)
{
  if (!validate_arg (s1, POINTER_TYPE)
      || !validate_arg (s2, POINTER_TYPE))
    return NULL_TREE;
  else
    {
      /* If the first argument is "", return NULL_TREE.  */
      const char *p1 = c_getstr (s1);
      if (p1 && *p1 == '\0')
	{
	  /* Evaluate and ignore argument s2 in case it has
	     side-effects.  */
	  return omit_one_operand_loc (loc, size_type_node,
				   size_zero_node, s2);
	}

      /* If the second argument is "", return __builtin_strlen(s1).  */
      const char *p2 = c_getstr (s2);
      if (p2 && *p2 == '\0')
	{
	  tree fn = builtin_decl_implicit (BUILT_IN_STRLEN);

	  /* If the replacement _DECL isn't initialized, don't do the
	     transformation.  */
	  if (!fn)
	    return NULL_TREE;

	  return build_call_expr_loc (loc, fn, 1, s1);
	}
      return NULL_TREE;
    }
}

/* Fold the next_arg or va_start call EXP. Returns true if there was an error
   produced.  False otherwise.  This is done so that we don't output the error
   or warning twice or three times.  */

bool
fold_builtin_next_arg (tree exp, bool va_start_p)
{
  tree fntype = TREE_TYPE (current_function_decl);
  int nargs = call_expr_nargs (exp);
  tree arg;
  /* There is good chance the current input_location points inside the
     definition of the va_start macro (perhaps on the token for
     builtin) in a system header, so warnings will not be emitted.
     Use the location in real source code.  */
  source_location current_location =
    linemap_unwind_to_first_non_reserved_loc (line_table, input_location,
					      NULL);

  if (!stdarg_p (fntype))
    {
      error ("%<va_start%> used in function with fixed args");
      return true;
    }

  if (va_start_p)
    {
      if (va_start_p && (nargs != 2))
	{
	  error ("wrong number of arguments to function %<va_start%>");
	  return true;
	}
      arg = CALL_EXPR_ARG (exp, 1);
    }
  /* We use __builtin_va_start (ap, 0, 0) or __builtin_next_arg (0, 0)
     when we checked the arguments and if needed issued a warning.  */
  else
    {
      if (nargs == 0)
	{
	  /* Evidently an out of date version of <stdarg.h>; can't validate
	     va_start's second argument, but can still work as intended.  */
	  warning_at (current_location,
		      OPT_Wvarargs,
		   "%<__builtin_next_arg%> called without an argument");
	  return true;
	}
      else if (nargs > 1)
	{
	  error ("wrong number of arguments to function %<__builtin_next_arg%>");
	  return true;
	}
      arg = CALL_EXPR_ARG (exp, 0);
    }

  if (TREE_CODE (arg) == SSA_NAME)
    arg = SSA_NAME_VAR (arg);

  /* We destructively modify the call to be __builtin_va_start (ap, 0)
     or __builtin_next_arg (0) the first time we see it, after checking
     the arguments and if needed issuing a warning.  */
  if (!integer_zerop (arg))
    {
      tree last_parm = tree_last (DECL_ARGUMENTS (current_function_decl));

      /* Strip off all nops for the sake of the comparison.  This
	 is not quite the same as STRIP_NOPS.  It does more.
	 We must also strip off INDIRECT_EXPR for C++ reference
	 parameters.  */
      while (CONVERT_EXPR_P (arg)
	     || TREE_CODE (arg) == INDIRECT_REF)
	arg = TREE_OPERAND (arg, 0);
      if (arg != last_parm)
	{
	  /* FIXME: Sometimes with the tree optimizers we can get the
	     not the last argument even though the user used the last
	     argument.  We just warn and set the arg to be the last
	     argument so that we will get wrong-code because of
	     it.  */
	  warning_at (current_location,
		      OPT_Wvarargs,
		      "second parameter of %<va_start%> not last named argument");
	}

      /* Undefined by C99 7.15.1.4p4 (va_start):
         "If the parameter parmN is declared with the register storage
         class, with a function or array type, or with a type that is
         not compatible with the type that results after application of
         the default argument promotions, the behavior is undefined."
      */
      else if (DECL_REGISTER (arg))
	{
	  warning_at (current_location,
		      OPT_Wvarargs,
		      "undefined behavior when second parameter of "
		      "%<va_start%> is declared with %<register%> storage");
	}

      /* We want to verify the second parameter just once before the tree
	 optimizers are run and then avoid keeping it in the tree,
	 as otherwise we could warn even for correct code like:
	 void foo (int i, ...)
	 { va_list ap; i++; va_start (ap, i); va_end (ap); }  */
      if (va_start_p)
	CALL_EXPR_ARG (exp, 1) = integer_zero_node;
      else
	CALL_EXPR_ARG (exp, 0) = integer_zero_node;
    }
  return false;
}


/* Expand a call EXP to __builtin_object_size.  */

static rtx
expand_builtin_object_size (tree exp)
{
  tree ost;
  int object_size_type;
  tree fndecl = get_callee_fndecl (exp);

  if (!validate_arglist (exp, POINTER_TYPE, INTEGER_TYPE, VOID_TYPE))
    {
      error ("%Kfirst argument of %qD must be a pointer, second integer constant",
	     exp, fndecl);
      expand_builtin_trap ();
      return const0_rtx;
    }

  ost = CALL_EXPR_ARG (exp, 1);
  STRIP_NOPS (ost);

  if (TREE_CODE (ost) != INTEGER_CST
      || tree_int_cst_sgn (ost) < 0
      || compare_tree_int (ost, 3) > 0)
    {
      error ("%Klast argument of %qD is not integer constant between 0 and 3",
	     exp, fndecl);
      expand_builtin_trap ();
      return const0_rtx;
    }

  object_size_type = tree_to_shwi (ost);

  return object_size_type < 2 ? constm1_rtx : const0_rtx;
}

/* Expand EXP, a call to the __mem{cpy,pcpy,move,set}_chk builtin.
   FCODE is the BUILT_IN_* to use.
   Return NULL_RTX if we failed; the caller should emit a normal call,
   otherwise try to get the result in TARGET, if convenient (and in
   mode MODE if that's convenient).  */

static rtx
expand_builtin_memory_chk (tree exp, rtx target, machine_mode mode,
			   enum built_in_function fcode)
{
  tree dest, src, len, size;

  if (!validate_arglist (exp,
			 POINTER_TYPE,
			 fcode == BUILT_IN_MEMSET_CHK
			 ? INTEGER_TYPE : POINTER_TYPE,
			 INTEGER_TYPE, INTEGER_TYPE, VOID_TYPE))
    return NULL_RTX;

  dest = CALL_EXPR_ARG (exp, 0);
  src = CALL_EXPR_ARG (exp, 1);
  len = CALL_EXPR_ARG (exp, 2);
  size = CALL_EXPR_ARG (exp, 3);

  bool sizes_ok = check_sizes (OPT_Wstringop_overflow_,
			       exp, len, /*maxlen=*/NULL_TREE,
			       /*str=*/NULL_TREE, size);

  if (!tree_fits_uhwi_p (size))
    return NULL_RTX;

  if (tree_fits_uhwi_p (len) || integer_all_onesp (size))
    {
      /* Avoid transforming the checking call to an ordinary one when
	 an overflow has been detected or when the call couldn't be
	 validated because the size is not constant.  */
      if (!sizes_ok && !integer_all_onesp (size) && tree_int_cst_lt (size, len))
	return NULL_RTX;

      tree fn = NULL_TREE;
      /* If __builtin_mem{cpy,pcpy,move,set}_chk is used, assume
	 mem{cpy,pcpy,move,set} is available.  */
      switch (fcode)
	{
	case BUILT_IN_MEMCPY_CHK:
	  fn = builtin_decl_explicit (BUILT_IN_MEMCPY);
	  break;
	case BUILT_IN_MEMPCPY_CHK:
	  fn = builtin_decl_explicit (BUILT_IN_MEMPCPY);
	  break;
	case BUILT_IN_MEMMOVE_CHK:
	  fn = builtin_decl_explicit (BUILT_IN_MEMMOVE);
	  break;
	case BUILT_IN_MEMSET_CHK:
	  fn = builtin_decl_explicit (BUILT_IN_MEMSET);
	  break;
	default:
	  break;
	}

      if (! fn)
	return NULL_RTX;

      fn = build_call_nofold_loc (EXPR_LOCATION (exp), fn, 3, dest, src, len);
      gcc_assert (TREE_CODE (fn) == CALL_EXPR);
      CALL_EXPR_TAILCALL (fn) = CALL_EXPR_TAILCALL (exp);
      return expand_expr (fn, target, mode, EXPAND_NORMAL);
    }
  else if (fcode == BUILT_IN_MEMSET_CHK)
    return NULL_RTX;
  else
    {
      unsigned int dest_align = get_pointer_alignment (dest);

      /* If DEST is not a pointer type, call the normal function.  */
      if (dest_align == 0)
	return NULL_RTX;

      /* If SRC and DEST are the same (and not volatile), do nothing.  */
      if (operand_equal_p (src, dest, 0))
	{
	  tree expr;

	  if (fcode != BUILT_IN_MEMPCPY_CHK)
	    {
	      /* Evaluate and ignore LEN in case it has side-effects.  */
	      expand_expr (len, const0_rtx, VOIDmode, EXPAND_NORMAL);
	      return expand_expr (dest, target, mode, EXPAND_NORMAL);
	    }

	  expr = fold_build_pointer_plus (dest, len);
	  return expand_expr (expr, target, mode, EXPAND_NORMAL);
	}

      /* __memmove_chk special case.  */
      if (fcode == BUILT_IN_MEMMOVE_CHK)
	{
	  unsigned int src_align = get_pointer_alignment (src);

	  if (src_align == 0)
	    return NULL_RTX;

	  /* If src is categorized for a readonly section we can use
	     normal __memcpy_chk.  */
	  if (readonly_data_expr (src))
	    {
	      tree fn = builtin_decl_explicit (BUILT_IN_MEMCPY_CHK);
	      if (!fn)
		return NULL_RTX;
	      fn = build_call_nofold_loc (EXPR_LOCATION (exp), fn, 4,
					  dest, src, len, size);
	      gcc_assert (TREE_CODE (fn) == CALL_EXPR);
	      CALL_EXPR_TAILCALL (fn) = CALL_EXPR_TAILCALL (exp);
	      return expand_expr (fn, target, mode, EXPAND_NORMAL);
	    }
	}
      return NULL_RTX;
    }
}

/* Emit warning if a buffer overflow is detected at compile time.  */

static void
maybe_emit_chk_warning (tree exp, enum built_in_function fcode)
{
  /* The source string.  */
  tree srcstr = NULL_TREE;
  /* The size of the destination object.  */
  tree objsize = NULL_TREE;
  /* The string that is being concatenated with (as in __strcat_chk)
     or null if it isn't.  */
  tree catstr = NULL_TREE;
  /* The maximum length of the source sequence in a bounded operation
     (such as __strncat_chk) or null if the operation isn't bounded
     (such as __strcat_chk).  */
  tree maxlen = NULL_TREE;

  switch (fcode)
    {
    case BUILT_IN_STRCPY_CHK:
    case BUILT_IN_STPCPY_CHK:
      srcstr = CALL_EXPR_ARG (exp, 1);
      objsize = CALL_EXPR_ARG (exp, 2);
      break;

    case BUILT_IN_STRCAT_CHK:
      /* For __strcat_chk the warning will be emitted only if overflowing
	 by at least strlen (dest) + 1 bytes.  */
      catstr = CALL_EXPR_ARG (exp, 0);
      srcstr = CALL_EXPR_ARG (exp, 1);
      objsize = CALL_EXPR_ARG (exp, 2);
      break;

    case BUILT_IN_STRNCAT_CHK:
      catstr = CALL_EXPR_ARG (exp, 0);
      srcstr = CALL_EXPR_ARG (exp, 1);
      maxlen = CALL_EXPR_ARG (exp, 2);
      objsize = CALL_EXPR_ARG (exp, 3);
      break;

    case BUILT_IN_STRNCPY_CHK:
    case BUILT_IN_STPNCPY_CHK:
      srcstr = CALL_EXPR_ARG (exp, 1);
      maxlen = CALL_EXPR_ARG (exp, 2);
      objsize = CALL_EXPR_ARG (exp, 3);
      break;

    case BUILT_IN_SNPRINTF_CHK:
    case BUILT_IN_VSNPRINTF_CHK:
      maxlen = CALL_EXPR_ARG (exp, 1);
      objsize = CALL_EXPR_ARG (exp, 3);
      break;
    default:
      gcc_unreachable ();
    }

  if (catstr && maxlen)
    {
      /* Check __strncat_chk.  There is no way to determine the length
	 of the string to which the source string is being appended so
	 just warn when the length of the source string is not known.  */
      check_strncat_sizes (exp, objsize);
      return;
    }

  check_sizes (OPT_Wstringop_overflow_, exp,
	       /*size=*/NULL_TREE, maxlen, srcstr, objsize);
}

/* Emit warning if a buffer overflow is detected at compile time
   in __sprintf_chk/__vsprintf_chk calls.  */

static void
maybe_emit_sprintf_chk_warning (tree exp, enum built_in_function fcode)
{
  tree size, len, fmt;
  const char *fmt_str;
  int nargs = call_expr_nargs (exp);

  /* Verify the required arguments in the original call.  */

  if (nargs < 4)
    return;
  size = CALL_EXPR_ARG (exp, 2);
  fmt = CALL_EXPR_ARG (exp, 3);

  if (! tree_fits_uhwi_p (size) || integer_all_onesp (size))
    return;

  /* Check whether the format is a literal string constant.  */
  fmt_str = c_getstr (fmt);
  if (fmt_str == NULL)
    return;

  if (!init_target_chars ())
    return;

  /* If the format doesn't contain % args or %%, we know its size.  */
  if (strchr (fmt_str, target_percent) == 0)
    len = build_int_cstu (size_type_node, strlen (fmt_str));
  /* If the format is "%s" and first ... argument is a string literal,
     we know it too.  */
  else if (fcode == BUILT_IN_SPRINTF_CHK
	   && strcmp (fmt_str, target_percent_s) == 0)
    {
      tree arg;

      if (nargs < 5)
	return;
      arg = CALL_EXPR_ARG (exp, 4);
      if (! POINTER_TYPE_P (TREE_TYPE (arg)))
	return;

      len = c_strlen (arg, 1);
      if (!len || ! tree_fits_uhwi_p (len))
	return;
    }
  else
    return;

  /* Add one for the terminating nul.  */
  len = fold_build2 (PLUS_EXPR, TREE_TYPE (len), len, size_one_node);
  check_sizes (OPT_Wstringop_overflow_,
	       exp, /*size=*/NULL_TREE, /*maxlen=*/NULL_TREE, len, size);
}

/* Emit warning if a free is called with address of a variable.  */

static void
maybe_emit_free_warning (tree exp)
{
  tree arg = CALL_EXPR_ARG (exp, 0);

  STRIP_NOPS (arg);
  if (TREE_CODE (arg) != ADDR_EXPR)
    return;

  arg = get_base_address (TREE_OPERAND (arg, 0));
  if (arg == NULL || INDIRECT_REF_P (arg) || TREE_CODE (arg) == MEM_REF)
    return;

  if (SSA_VAR_P (arg))
    warning_at (tree_nonartificial_location (exp), OPT_Wfree_nonheap_object,
		"%Kattempt to free a non-heap object %qD", exp, arg);
  else
    warning_at (tree_nonartificial_location (exp), OPT_Wfree_nonheap_object,
		"%Kattempt to free a non-heap object", exp);
}

/* Fold a call to __builtin_object_size with arguments PTR and OST,
   if possible.  */

static tree
fold_builtin_object_size (tree ptr, tree ost)
{
  unsigned HOST_WIDE_INT bytes;
  int object_size_type;

  if (!validate_arg (ptr, POINTER_TYPE)
      || !validate_arg (ost, INTEGER_TYPE))
    return NULL_TREE;

  STRIP_NOPS (ost);

  if (TREE_CODE (ost) != INTEGER_CST
      || tree_int_cst_sgn (ost) < 0
      || compare_tree_int (ost, 3) > 0)
    return NULL_TREE;

  object_size_type = tree_to_shwi (ost);

  /* __builtin_object_size doesn't evaluate side-effects in its arguments;
     if there are any side-effects, it returns (size_t) -1 for types 0 and 1
     and (size_t) 0 for types 2 and 3.  */
  if (TREE_SIDE_EFFECTS (ptr))
    return build_int_cst_type (size_type_node, object_size_type < 2 ? -1 : 0);

  if (TREE_CODE (ptr) == ADDR_EXPR)
    {
      compute_builtin_object_size (ptr, object_size_type, &bytes);
      if (wi::fits_to_tree_p (bytes, size_type_node))
	return build_int_cstu (size_type_node, bytes);
    }
  else if (TREE_CODE (ptr) == SSA_NAME)
    {
      /* If object size is not known yet, delay folding until
       later.  Maybe subsequent passes will help determining
       it.  */
      if (compute_builtin_object_size (ptr, object_size_type, &bytes)
	  && wi::fits_to_tree_p (bytes, size_type_node))
	return build_int_cstu (size_type_node, bytes);
    }

  return NULL_TREE;
}

/* Builtins with folding operations that operate on "..." arguments
   need special handling; we need to store the arguments in a convenient
   data structure before attempting any folding.  Fortunately there are
   only a few builtins that fall into this category.  FNDECL is the
   function, EXP is the CALL_EXPR for the call.  */

static tree
fold_builtin_varargs (location_t loc, tree fndecl, tree *args, int nargs)
{
  enum built_in_function fcode = DECL_FUNCTION_CODE (fndecl);
  tree ret = NULL_TREE;

  switch (fcode)
    {
    case BUILT_IN_FPCLASSIFY:
      ret = fold_builtin_fpclassify (loc, args, nargs);
      break;

    default:
      break;
    }
  if (ret)
    {
      ret = build1 (NOP_EXPR, TREE_TYPE (ret), ret);
      SET_EXPR_LOCATION (ret, loc);
      TREE_NO_WARNING (ret) = 1;
      return ret;
    }
  return NULL_TREE;
}

/* Initialize format string characters in the target charset.  */

bool
init_target_chars (void)
{
  static bool init;
  if (!init)
    {
      target_newline = lang_hooks.to_target_charset ('\n');
      target_percent = lang_hooks.to_target_charset ('%');
      target_c = lang_hooks.to_target_charset ('c');
      target_s = lang_hooks.to_target_charset ('s');
      if (target_newline == 0 || target_percent == 0 || target_c == 0
	  || target_s == 0)
	return false;

      target_percent_c[0] = target_percent;
      target_percent_c[1] = target_c;
      target_percent_c[2] = '\0';

      target_percent_s[0] = target_percent;
      target_percent_s[1] = target_s;
      target_percent_s[2] = '\0';

      target_percent_s_newline[0] = target_percent;
      target_percent_s_newline[1] = target_s;
      target_percent_s_newline[2] = target_newline;
      target_percent_s_newline[3] = '\0';

      init = true;
    }
  return true;
}

/* Helper function for do_mpfr_arg*().  Ensure M is a normal number
   and no overflow/underflow occurred.  INEXACT is true if M was not
   exactly calculated.  TYPE is the tree type for the result.  This
   function assumes that you cleared the MPFR flags and then
   calculated M to see if anything subsequently set a flag prior to
   entering this function.  Return NULL_TREE if any checks fail.  */

static tree
do_mpfr_ckconv (mpfr_srcptr m, tree type, int inexact)
{
  /* Proceed iff we get a normal number, i.e. not NaN or Inf and no
     overflow/underflow occurred.  If -frounding-math, proceed iff the
     result of calling FUNC was exact.  */
  if (mpfr_number_p (m) && !mpfr_overflow_p () && !mpfr_underflow_p ()
      && (!flag_rounding_math || !inexact))
    {
      REAL_VALUE_TYPE rr;

      real_from_mpfr (&rr, m, type, GMP_RNDN);
      /* Proceed iff GCC's REAL_VALUE_TYPE can hold the MPFR value,
	 check for overflow/underflow.  If the REAL_VALUE_TYPE is zero
	 but the mpft_t is not, then we underflowed in the
	 conversion.  */
      if (real_isfinite (&rr)
	  && (rr.cl == rvc_zero) == (mpfr_zero_p (m) != 0))
        {
	  REAL_VALUE_TYPE rmode;

	  real_convert (&rmode, TYPE_MODE (type), &rr);
	  /* Proceed iff the specified mode can hold the value.  */
	  if (real_identical (&rmode, &rr))
	    return build_real (type, rmode);
	}
    }
  return NULL_TREE;
}

/* Helper function for do_mpc_arg*().  Ensure M is a normal complex
   number and no overflow/underflow occurred.  INEXACT is true if M
   was not exactly calculated.  TYPE is the tree type for the result.
   This function assumes that you cleared the MPFR flags and then
   calculated M to see if anything subsequently set a flag prior to
   entering this function.  Return NULL_TREE if any checks fail, if
   FORCE_CONVERT is true, then bypass the checks.  */

static tree
do_mpc_ckconv (mpc_srcptr m, tree type, int inexact, int force_convert)
{
  /* Proceed iff we get a normal number, i.e. not NaN or Inf and no
     overflow/underflow occurred.  If -frounding-math, proceed iff the
     result of calling FUNC was exact.  */
  if (force_convert
      || (mpfr_number_p (mpc_realref (m)) && mpfr_number_p (mpc_imagref (m))
	  && !mpfr_overflow_p () && !mpfr_underflow_p ()
	  && (!flag_rounding_math || !inexact)))
    {
      REAL_VALUE_TYPE re, im;

      real_from_mpfr (&re, mpc_realref (m), TREE_TYPE (type), GMP_RNDN);
      real_from_mpfr (&im, mpc_imagref (m), TREE_TYPE (type), GMP_RNDN);
      /* Proceed iff GCC's REAL_VALUE_TYPE can hold the MPFR values,
	 check for overflow/underflow.  If the REAL_VALUE_TYPE is zero
	 but the mpft_t is not, then we underflowed in the
	 conversion.  */
      if (force_convert
	  || (real_isfinite (&re) && real_isfinite (&im)
	      && (re.cl == rvc_zero) == (mpfr_zero_p (mpc_realref (m)) != 0)
	      && (im.cl == rvc_zero) == (mpfr_zero_p (mpc_imagref (m)) != 0)))
        {
	  REAL_VALUE_TYPE re_mode, im_mode;

	  real_convert (&re_mode, TYPE_MODE (TREE_TYPE (type)), &re);
	  real_convert (&im_mode, TYPE_MODE (TREE_TYPE (type)), &im);
	  /* Proceed iff the specified mode can hold the value.  */
	  if (force_convert
	      || (real_identical (&re_mode, &re)
		  && real_identical (&im_mode, &im)))
	    return build_complex (type, build_real (TREE_TYPE (type), re_mode),
				  build_real (TREE_TYPE (type), im_mode));
	}
    }
  return NULL_TREE;
}

/* If arguments ARG0 and ARG1 are REAL_CSTs, call mpfr_remquo() to set
   the pointer *(ARG_QUO) and return the result.  The type is taken
   from the type of ARG0 and is used for setting the precision of the
   calculation and results.  */

static tree
do_mpfr_remquo (tree arg0, tree arg1, tree arg_quo)
{
  tree const type = TREE_TYPE (arg0);
  tree result = NULL_TREE;

  STRIP_NOPS (arg0);
  STRIP_NOPS (arg1);

  /* To proceed, MPFR must exactly represent the target floating point
     format, which only happens when the target base equals two.  */
  if (REAL_MODE_FORMAT (TYPE_MODE (type))->b == 2
      && TREE_CODE (arg0) == REAL_CST && !TREE_OVERFLOW (arg0)
      && TREE_CODE (arg1) == REAL_CST && !TREE_OVERFLOW (arg1))
    {
      const REAL_VALUE_TYPE *const ra0 = TREE_REAL_CST_PTR (arg0);
      const REAL_VALUE_TYPE *const ra1 = TREE_REAL_CST_PTR (arg1);

      if (real_isfinite (ra0) && real_isfinite (ra1))
        {
	  const struct real_format *fmt = REAL_MODE_FORMAT (TYPE_MODE (type));
	  const int prec = fmt->p;
	  const mp_rnd_t rnd = fmt->round_towards_zero? GMP_RNDZ : GMP_RNDN;
	  tree result_rem;
	  long integer_quo;
	  mpfr_t m0, m1;

	  mpfr_inits2 (prec, m0, m1, NULL);
	  mpfr_from_real (m0, ra0, GMP_RNDN);
	  mpfr_from_real (m1, ra1, GMP_RNDN);
	  mpfr_clear_flags ();
	  mpfr_remquo (m0, &integer_quo, m0, m1, rnd);
	  /* Remquo is independent of the rounding mode, so pass
	     inexact=0 to do_mpfr_ckconv().  */
	  result_rem = do_mpfr_ckconv (m0, type, /*inexact=*/ 0);
	  mpfr_clears (m0, m1, NULL);
	  if (result_rem)
	    {
	      /* MPFR calculates quo in the host's long so it may
		 return more bits in quo than the target int can hold
		 if sizeof(host long) > sizeof(target int).  This can
		 happen even for native compilers in LP64 mode.  In
		 these cases, modulo the quo value with the largest
		 number that the target int can hold while leaving one
		 bit for the sign.  */
	      if (sizeof (integer_quo) * CHAR_BIT > INT_TYPE_SIZE)
		integer_quo %= (long)(1UL << (INT_TYPE_SIZE - 1));

	      /* Dereference the quo pointer argument.  */
	      arg_quo = build_fold_indirect_ref (arg_quo);
	      /* Proceed iff a valid pointer type was passed in.  */
	      if (TYPE_MAIN_VARIANT (TREE_TYPE (arg_quo)) == integer_type_node)
	        {
		  /* Set the value. */
		  tree result_quo
		    = fold_build2 (MODIFY_EXPR, TREE_TYPE (arg_quo), arg_quo,
				   build_int_cst (TREE_TYPE (arg_quo),
						  integer_quo));
		  TREE_SIDE_EFFECTS (result_quo) = 1;
		  /* Combine the quo assignment with the rem.  */
		  result = non_lvalue (fold_build2 (COMPOUND_EXPR, type,
						    result_quo, result_rem));
		}
	    }
	}
    }
  return result;
}

/* If ARG is a REAL_CST, call mpfr_lgamma() on it and return the
   resulting value as a tree with type TYPE.  The mpfr precision is
   set to the precision of TYPE.  We assume that this mpfr function
   returns zero if the result could be calculated exactly within the
   requested precision.  In addition, the integer pointer represented
   by ARG_SG will be dereferenced and set to the appropriate signgam
   (-1,1) value.  */

static tree
do_mpfr_lgamma_r (tree arg, tree arg_sg, tree type)
{
  tree result = NULL_TREE;

  STRIP_NOPS (arg);

  /* To proceed, MPFR must exactly represent the target floating point
     format, which only happens when the target base equals two.  Also
     verify ARG is a constant and that ARG_SG is an int pointer.  */
  if (REAL_MODE_FORMAT (TYPE_MODE (type))->b == 2
      && TREE_CODE (arg) == REAL_CST && !TREE_OVERFLOW (arg)
      && TREE_CODE (TREE_TYPE (arg_sg)) == POINTER_TYPE
      && TYPE_MAIN_VARIANT (TREE_TYPE (TREE_TYPE (arg_sg))) == integer_type_node)
    {
      const REAL_VALUE_TYPE *const ra = TREE_REAL_CST_PTR (arg);

      /* In addition to NaN and Inf, the argument cannot be zero or a
	 negative integer.  */
      if (real_isfinite (ra)
	  && ra->cl != rvc_zero
	  && !(real_isneg (ra) && real_isinteger (ra, TYPE_MODE (type))))
        {
	  const struct real_format *fmt = REAL_MODE_FORMAT (TYPE_MODE (type));
	  const int prec = fmt->p;
	  const mp_rnd_t rnd = fmt->round_towards_zero? GMP_RNDZ : GMP_RNDN;
	  int inexact, sg;
	  mpfr_t m;
	  tree result_lg;

	  mpfr_init2 (m, prec);
	  mpfr_from_real (m, ra, GMP_RNDN);
	  mpfr_clear_flags ();
	  inexact = mpfr_lgamma (m, &sg, m, rnd);
	  result_lg = do_mpfr_ckconv (m, type, inexact);
	  mpfr_clear (m);
	  if (result_lg)
	    {
	      tree result_sg;

	      /* Dereference the arg_sg pointer argument.  */
	      arg_sg = build_fold_indirect_ref (arg_sg);
	      /* Assign the signgam value into *arg_sg. */
	      result_sg = fold_build2 (MODIFY_EXPR,
				       TREE_TYPE (arg_sg), arg_sg,
				       build_int_cst (TREE_TYPE (arg_sg), sg));
	      TREE_SIDE_EFFECTS (result_sg) = 1;
	      /* Combine the signgam assignment with the lgamma result.  */
	      result = non_lvalue (fold_build2 (COMPOUND_EXPR, type,
						result_sg, result_lg));
	    }
	}
    }

  return result;
}

/* If arguments ARG0 and ARG1 are a COMPLEX_CST, call the two-argument
   mpc function FUNC on it and return the resulting value as a tree
   with type TYPE.  The mpfr precision is set to the precision of
   TYPE.  We assume that function FUNC returns zero if the result
   could be calculated exactly within the requested precision.  If
   DO_NONFINITE is true, then fold expressions containing Inf or NaN
   in the arguments and/or results.  */

tree
do_mpc_arg2 (tree arg0, tree arg1, tree type, int do_nonfinite,
	     int (*func)(mpc_ptr, mpc_srcptr, mpc_srcptr, mpc_rnd_t))
{
  tree result = NULL_TREE;

  STRIP_NOPS (arg0);
  STRIP_NOPS (arg1);

  /* To proceed, MPFR must exactly represent the target floating point
     format, which only happens when the target base equals two.  */
  if (TREE_CODE (arg0) == COMPLEX_CST && !TREE_OVERFLOW (arg0)
      && TREE_CODE (TREE_TYPE (TREE_TYPE (arg0))) == REAL_TYPE
      && TREE_CODE (arg1) == COMPLEX_CST && !TREE_OVERFLOW (arg1)
      && TREE_CODE (TREE_TYPE (TREE_TYPE (arg1))) == REAL_TYPE
      && REAL_MODE_FORMAT (TYPE_MODE (TREE_TYPE (TREE_TYPE (arg0))))->b == 2)
    {
      const REAL_VALUE_TYPE *const re0 = TREE_REAL_CST_PTR (TREE_REALPART (arg0));
      const REAL_VALUE_TYPE *const im0 = TREE_REAL_CST_PTR (TREE_IMAGPART (arg0));
      const REAL_VALUE_TYPE *const re1 = TREE_REAL_CST_PTR (TREE_REALPART (arg1));
      const REAL_VALUE_TYPE *const im1 = TREE_REAL_CST_PTR (TREE_IMAGPART (arg1));

      if (do_nonfinite
	  || (real_isfinite (re0) && real_isfinite (im0)
	      && real_isfinite (re1) && real_isfinite (im1)))
        {
	  const struct real_format *const fmt =
	    REAL_MODE_FORMAT (TYPE_MODE (TREE_TYPE (type)));
	  const int prec = fmt->p;
	  const mp_rnd_t rnd = fmt->round_towards_zero ? GMP_RNDZ : GMP_RNDN;
	  const mpc_rnd_t crnd = fmt->round_towards_zero ? MPC_RNDZZ : MPC_RNDNN;
	  int inexact;
	  mpc_t m0, m1;

	  mpc_init2 (m0, prec);
	  mpc_init2 (m1, prec);
	  mpfr_from_real (mpc_realref (m0), re0, rnd);
	  mpfr_from_real (mpc_imagref (m0), im0, rnd);
	  mpfr_from_real (mpc_realref (m1), re1, rnd);
	  mpfr_from_real (mpc_imagref (m1), im1, rnd);
	  mpfr_clear_flags ();
	  inexact = func (m0, m0, m1, crnd);
	  result = do_mpc_ckconv (m0, type, inexact, do_nonfinite);
	  mpc_clear (m0);
	  mpc_clear (m1);
	}
    }

  return result;
}

/* A wrapper function for builtin folding that prevents warnings for
   "statement without effect" and the like, caused by removing the
   call node earlier than the warning is generated.  */

tree
fold_call_stmt (gcall *stmt, bool ignore)
{
  tree ret = NULL_TREE;
  tree fndecl = gimple_call_fndecl (stmt);
  location_t loc = gimple_location (stmt);
  if (fndecl
      && TREE_CODE (fndecl) == FUNCTION_DECL
      && DECL_BUILT_IN (fndecl)
      && !gimple_call_va_arg_pack_p (stmt))
    {
      int nargs = gimple_call_num_args (stmt);
      tree *args = (nargs > 0
		    ? gimple_call_arg_ptr (stmt, 0)
		    : &error_mark_node);

      if (avoid_folding_inline_builtin (fndecl))
	return NULL_TREE;
      if (DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_MD)
        {
	  return targetm.fold_builtin (fndecl, nargs, args, ignore);
        }
      else
	{
	  ret = fold_builtin_n (loc, fndecl, args, nargs, ignore);
	  if (ret)
	    {
	      /* Propagate location information from original call to
		 expansion of builtin.  Otherwise things like
		 maybe_emit_chk_warning, that operate on the expansion
		 of a builtin, will use the wrong location information.  */
	      if (gimple_has_location (stmt))
                {
		  tree realret = ret;
		  if (TREE_CODE (ret) == NOP_EXPR)
		    realret = TREE_OPERAND (ret, 0);
		  if (CAN_HAVE_LOCATION_P (realret)
		      && !EXPR_HAS_LOCATION (realret))
		    SET_EXPR_LOCATION (realret, loc);
                  return realret;
                }
	      return ret;
	    }
	}
    }
  return NULL_TREE;
}

/* Look up the function in builtin_decl that corresponds to DECL
   and set ASMSPEC as its user assembler name.  DECL must be a
   function decl that declares a builtin.  */

void
set_builtin_user_assembler_name (tree decl, const char *asmspec)
{
  gcc_assert (TREE_CODE (decl) == FUNCTION_DECL
	      && DECL_BUILT_IN_CLASS (decl) == BUILT_IN_NORMAL
	      && asmspec != 0);

  tree builtin = builtin_decl_explicit (DECL_FUNCTION_CODE (decl));
  set_user_assembler_name (builtin, asmspec);

  if (DECL_FUNCTION_CODE (decl) == BUILT_IN_FFS
      && INT_TYPE_SIZE < BITS_PER_WORD)
    {
      scalar_int_mode mode = int_mode_for_size (INT_TYPE_SIZE, 0).require ();
      set_user_assembler_libfunc ("ffs", asmspec);
      set_optab_libfunc (ffs_optab, mode, "ffs");
    }
}

/* Return true if DECL is a builtin that expands to a constant or similarly
   simple code.  */
bool
is_simple_builtin (tree decl)
{
  if (decl && DECL_BUILT_IN_CLASS (decl) == BUILT_IN_NORMAL)
    switch (DECL_FUNCTION_CODE (decl))
      {
	/* Builtins that expand to constants.  */
      case BUILT_IN_CONSTANT_P:
      case BUILT_IN_EXPECT:
      case BUILT_IN_OBJECT_SIZE:
      case BUILT_IN_UNREACHABLE:
	/* Simple register moves or loads from stack.  */
      case BUILT_IN_ASSUME_ALIGNED:
      case BUILT_IN_RETURN_ADDRESS:
      case BUILT_IN_EXTRACT_RETURN_ADDR:
      case BUILT_IN_FROB_RETURN_ADDR:
      case BUILT_IN_RETURN:
      case BUILT_IN_AGGREGATE_INCOMING_ADDRESS:
      case BUILT_IN_FRAME_ADDRESS:
      case BUILT_IN_VA_END:
      case BUILT_IN_STACK_SAVE:
      case BUILT_IN_STACK_RESTORE:
	/* Exception state returns or moves registers around.  */
      case BUILT_IN_EH_FILTER:
      case BUILT_IN_EH_POINTER:
      case BUILT_IN_EH_COPY_VALUES:
	return true;

      default:
	return false;
      }

  return false;
}

/* Return true if DECL is a builtin that is not expensive, i.e., they are
   most probably expanded inline into reasonably simple code.  This is a
   superset of is_simple_builtin.  */
bool
is_inexpensive_builtin (tree decl)
{
  if (!decl)
    return false;
  else if (DECL_BUILT_IN_CLASS (decl) == BUILT_IN_MD)
    return true;
  else if (DECL_BUILT_IN_CLASS (decl) == BUILT_IN_NORMAL)
    switch (DECL_FUNCTION_CODE (decl))
      {
      case BUILT_IN_ABS:
      case BUILT_IN_ALLOCA:
      case BUILT_IN_ALLOCA_WITH_ALIGN:
      case BUILT_IN_BSWAP16:
      case BUILT_IN_BSWAP32:
      case BUILT_IN_BSWAP64:
      case BUILT_IN_CLZ:
      case BUILT_IN_CLZIMAX:
      case BUILT_IN_CLZL:
      case BUILT_IN_CLZLL:
      case BUILT_IN_CTZ:
      case BUILT_IN_CTZIMAX:
      case BUILT_IN_CTZL:
      case BUILT_IN_CTZLL:
      case BUILT_IN_FFS:
      case BUILT_IN_FFSIMAX:
      case BUILT_IN_FFSL:
      case BUILT_IN_FFSLL:
      case BUILT_IN_IMAXABS:
      case BUILT_IN_FINITE:
      case BUILT_IN_FINITEF:
      case BUILT_IN_FINITEL:
      case BUILT_IN_FINITED32:
      case BUILT_IN_FINITED64:
      case BUILT_IN_FINITED128:
      case BUILT_IN_FPCLASSIFY:
      case BUILT_IN_ISFINITE:
      case BUILT_IN_ISINF_SIGN:
      case BUILT_IN_ISINF:
      case BUILT_IN_ISINFF:
      case BUILT_IN_ISINFL:
      case BUILT_IN_ISINFD32:
      case BUILT_IN_ISINFD64:
      case BUILT_IN_ISINFD128:
      case BUILT_IN_ISNAN:
      case BUILT_IN_ISNANF:
      case BUILT_IN_ISNANL:
      case BUILT_IN_ISNAND32:
      case BUILT_IN_ISNAND64:
      case BUILT_IN_ISNAND128:
      case BUILT_IN_ISNORMAL:
      case BUILT_IN_ISGREATER:
      case BUILT_IN_ISGREATEREQUAL:
      case BUILT_IN_ISLESS:
      case BUILT_IN_ISLESSEQUAL:
      case BUILT_IN_ISLESSGREATER:
      case BUILT_IN_ISUNORDERED:
      case BUILT_IN_VA_ARG_PACK:
      case BUILT_IN_VA_ARG_PACK_LEN:
      case BUILT_IN_VA_COPY:
      case BUILT_IN_TRAP:
      case BUILT_IN_SAVEREGS:
      case BUILT_IN_POPCOUNTL:
      case BUILT_IN_POPCOUNTLL:
      case BUILT_IN_POPCOUNTIMAX:
      case BUILT_IN_POPCOUNT:
      case BUILT_IN_PARITYL:
      case BUILT_IN_PARITYLL:
      case BUILT_IN_PARITYIMAX:
      case BUILT_IN_PARITY:
      case BUILT_IN_LABS:
      case BUILT_IN_LLABS:
      case BUILT_IN_PREFETCH:
      case BUILT_IN_ACC_ON_DEVICE:
	return true;

      default:
	return is_simple_builtin (decl);
      }

  return false;
}

/* Return true if T is a constant and the value cast to a target char
   can be represented by a host char.
   Store the casted char constant in *P if so.  */

bool
target_char_cst_p (tree t, char *p)
{
  if (!tree_fits_uhwi_p (t) || CHAR_TYPE_SIZE != HOST_BITS_PER_CHAR)
    return false;

  *p = (char)tree_to_uhwi (t);
  return true;
}
