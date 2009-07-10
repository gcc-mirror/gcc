/* Expand builtin functions.
   Copyright (C) 1988, 1992, 1993, 1994, 1995, 1996, 1997, 1998, 1999,
   2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007
   Free Software Foundation, Inc.

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

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "machmode.h"
#include "real.h"
#include "rtl.h"
#include "tree.h"
#include "tree-gimple.h"
#include "flags.h"
#include "regs.h"
#include "hard-reg-set.h"
#include "except.h"
#include "function.h"
#include "insn-config.h"
#include "expr.h"
#include "optabs.h"
#include "libfuncs.h"
#include "recog.h"
#include "output.h"
#include "typeclass.h"
#include "toplev.h"
#include "predict.h"
#include "tm_p.h"
#include "target.h"
#include "langhooks.h"
#include "basic-block.h"
#include "tree-mudflap.h"
#include "tree-flow.h"
#include "value-prof.h"
#include "diagnostic.h"

#ifndef PAD_VARARGS_DOWN
#define PAD_VARARGS_DOWN BYTES_BIG_ENDIAN
#endif

/* Define the names of the builtin function types and codes.  */
const char *const built_in_class_names[4]
  = {"NOT_BUILT_IN", "BUILT_IN_FRONTEND", "BUILT_IN_MD", "BUILT_IN_NORMAL"};

#define DEF_BUILTIN(X, N, C, T, LT, B, F, NA, AT, IM, COND) #X,
const char * built_in_names[(int) END_BUILTINS] =
{
#include "builtins.def"
};
#undef DEF_BUILTIN

/* Setup an array of _DECL trees, make sure each element is
   initialized to NULL_TREE.  */
tree built_in_decls[(int) END_BUILTINS];
/* Declarations used when constructing the builtin implicitly in the compiler.
   It may be NULL_TREE when this is invalid (for instance runtime is not
   required to implement the function call in all cases).  */
tree implicit_built_in_decls[(int) END_BUILTINS];

static const char *c_getstr (tree);
static rtx c_readstr (const char *, enum machine_mode);
static int target_char_cast (tree, char *);
static rtx get_memory_rtx (tree, tree);
static int apply_args_size (void);
static int apply_result_size (void);
#if defined (HAVE_untyped_call) || defined (HAVE_untyped_return)
static rtx result_vector (int, rtx);
#endif
static void expand_builtin_update_setjmp_buf (rtx);
static void expand_builtin_prefetch (tree);
static rtx expand_builtin_apply_args (void);
static rtx expand_builtin_apply_args_1 (void);
static rtx expand_builtin_apply (rtx, rtx, rtx);
static void expand_builtin_return (rtx);
static enum type_class type_to_class (tree);
static rtx expand_builtin_classify_type (tree);
static void expand_errno_check (tree, rtx);
static rtx expand_builtin_mathfn (tree, rtx, rtx);
static rtx expand_builtin_mathfn_2 (tree, rtx, rtx);
static rtx expand_builtin_mathfn_3 (tree, rtx, rtx);
static rtx expand_builtin_interclass_mathfn (tree, rtx, rtx);
static rtx expand_builtin_sincos (tree);
static rtx expand_builtin_cexpi (tree, rtx, rtx);
static rtx expand_builtin_int_roundingfn (tree, rtx, rtx);
static rtx expand_builtin_int_roundingfn_2 (tree, rtx, rtx);
static rtx expand_builtin_args_info (tree);
static rtx expand_builtin_next_arg (void);
static rtx expand_builtin_va_start (tree);
static rtx expand_builtin_va_end (tree);
static rtx expand_builtin_va_copy (tree);
static rtx expand_builtin_memchr (tree, rtx, enum machine_mode);
static rtx expand_builtin_memcmp (tree, rtx, enum machine_mode);
static rtx expand_builtin_strcmp (tree, rtx, enum machine_mode);
static rtx expand_builtin_strncmp (tree, rtx, enum machine_mode);
static rtx builtin_memcpy_read_str (void *, HOST_WIDE_INT, enum machine_mode);
static rtx expand_builtin_strcat (tree, tree, rtx, enum machine_mode);
static rtx expand_builtin_strncat (tree, rtx, enum machine_mode);
static rtx expand_builtin_strspn (tree, rtx, enum machine_mode);
static rtx expand_builtin_strcspn (tree, rtx, enum machine_mode);
static rtx expand_builtin_memcpy (tree, rtx, enum machine_mode);
static rtx expand_builtin_mempcpy (tree, rtx, enum machine_mode);
static rtx expand_builtin_mempcpy_args (tree, tree, tree, tree, rtx, 
					enum machine_mode, int);
static rtx expand_builtin_memmove (tree, rtx, enum machine_mode, int);
static rtx expand_builtin_memmove_args (tree, tree, tree, tree, rtx, 
					enum machine_mode, int);
static rtx expand_builtin_bcopy (tree, int);
static rtx expand_builtin_strcpy (tree, tree, rtx, enum machine_mode);
static rtx expand_builtin_strcpy_args (tree, tree, tree, rtx, enum machine_mode);
static rtx expand_builtin_stpcpy (tree, rtx, enum machine_mode);
static rtx expand_builtin_strncpy (tree, rtx, enum machine_mode);
static rtx builtin_memset_gen_str (void *, HOST_WIDE_INT, enum machine_mode);
static rtx expand_builtin_memset (tree, rtx, enum machine_mode);
static rtx expand_builtin_memset_args (tree, tree, tree, rtx, enum machine_mode, tree);
static rtx expand_builtin_bzero (tree);
static rtx expand_builtin_strlen (tree, rtx, enum machine_mode);
static rtx expand_builtin_strstr (tree, rtx, enum machine_mode);
static rtx expand_builtin_strpbrk (tree, rtx, enum machine_mode);
static rtx expand_builtin_strchr (tree, rtx, enum machine_mode);
static rtx expand_builtin_strrchr (tree, rtx, enum machine_mode);
static rtx expand_builtin_alloca (tree, rtx);
static rtx expand_builtin_unop (enum machine_mode, tree, rtx, rtx, optab);
static rtx expand_builtin_frame_address (tree, tree);
static rtx expand_builtin_fputs (tree, rtx, bool);
static rtx expand_builtin_printf (tree, rtx, enum machine_mode, bool);
static rtx expand_builtin_fprintf (tree, rtx, enum machine_mode, bool);
static rtx expand_builtin_sprintf (tree, rtx, enum machine_mode);
static tree stabilize_va_list (tree, int);
static rtx expand_builtin_expect (tree, rtx);
static tree fold_builtin_constant_p (tree);
static tree fold_builtin_expect (tree, tree);
static tree fold_builtin_classify_type (tree);
static tree fold_builtin_strlen (tree);
static tree fold_builtin_inf (tree, int);
static tree fold_builtin_nan (tree, tree, int);
static tree rewrite_call_expr (tree, int, tree, int, ...);
static bool validate_arg (const_tree, enum tree_code code);
static bool integer_valued_real_p (tree);
static tree fold_trunc_transparent_mathfn (tree, tree);
static bool readonly_data_expr (tree);
static rtx expand_builtin_fabs (tree, rtx, rtx);
static rtx expand_builtin_signbit (tree, rtx);
static tree fold_builtin_sqrt (tree, tree);
static tree fold_builtin_cbrt (tree, tree);
static tree fold_builtin_pow (tree, tree, tree, tree);
static tree fold_builtin_powi (tree, tree, tree, tree);
static tree fold_builtin_cos (tree, tree, tree);
static tree fold_builtin_cosh (tree, tree, tree);
static tree fold_builtin_tan (tree, tree);
static tree fold_builtin_trunc (tree, tree);
static tree fold_builtin_floor (tree, tree);
static tree fold_builtin_ceil (tree, tree);
static tree fold_builtin_round (tree, tree);
static tree fold_builtin_int_roundingfn (tree, tree);
static tree fold_builtin_bitop (tree, tree);
static tree fold_builtin_memory_op (tree, tree, tree, tree, bool, int);
static tree fold_builtin_strchr (tree, tree, tree);
static tree fold_builtin_memchr (tree, tree, tree, tree);
static tree fold_builtin_memcmp (tree, tree, tree);
static tree fold_builtin_strcmp (tree, tree);
static tree fold_builtin_strncmp (tree, tree, tree);
static tree fold_builtin_signbit (tree, tree);
static tree fold_builtin_copysign (tree, tree, tree, tree);
static tree fold_builtin_isascii (tree);
static tree fold_builtin_toascii (tree);
static tree fold_builtin_isdigit (tree);
static tree fold_builtin_fabs (tree, tree);
static tree fold_builtin_abs (tree, tree);
static tree fold_builtin_unordered_cmp (tree, tree, tree, enum tree_code,
					enum tree_code);
static tree fold_builtin_n (tree, tree *, int, bool);
static tree fold_builtin_0 (tree, bool);
static tree fold_builtin_1 (tree, tree, bool);
static tree fold_builtin_2 (tree, tree, tree, bool);
static tree fold_builtin_3 (tree, tree, tree, tree, bool);
static tree fold_builtin_4 (tree, tree, tree, tree, tree, bool);
static tree fold_builtin_varargs (tree, tree, bool);

static tree fold_builtin_strpbrk (tree, tree, tree);
static tree fold_builtin_strstr (tree, tree, tree);
static tree fold_builtin_strrchr (tree, tree, tree);
static tree fold_builtin_strcat (tree, tree);
static tree fold_builtin_strncat (tree, tree, tree);
static tree fold_builtin_strspn (tree, tree);
static tree fold_builtin_strcspn (tree, tree);
static tree fold_builtin_sprintf (tree, tree, tree, int);

static rtx expand_builtin_object_size (tree);
static rtx expand_builtin_memory_chk (tree, rtx, enum machine_mode,
				      enum built_in_function);
static void maybe_emit_chk_warning (tree, enum built_in_function);
static void maybe_emit_sprintf_chk_warning (tree, enum built_in_function);
static tree fold_builtin_object_size (tree, tree);
static tree fold_builtin_strcat_chk (tree, tree, tree, tree);
static tree fold_builtin_strncat_chk (tree, tree, tree, tree, tree);
static tree fold_builtin_sprintf_chk (tree, enum built_in_function);
static tree fold_builtin_printf (tree, tree, tree, bool, enum built_in_function);
static tree fold_builtin_fprintf (tree, tree, tree, tree, bool,
				  enum built_in_function);
static bool init_target_chars (void);

static unsigned HOST_WIDE_INT target_newline;
static unsigned HOST_WIDE_INT target_percent;
static unsigned HOST_WIDE_INT target_c;
static unsigned HOST_WIDE_INT target_s;
static char target_percent_c[3];
static char target_percent_s[3];
static char target_percent_s_newline[4];
static tree do_mpfr_arg1 (tree, tree, int (*)(mpfr_ptr, mpfr_srcptr, mp_rnd_t),
			  const REAL_VALUE_TYPE *, const REAL_VALUE_TYPE *, bool);
static tree do_mpfr_arg2 (tree, tree, tree,
			  int (*)(mpfr_ptr, mpfr_srcptr, mpfr_srcptr, mp_rnd_t));
static tree do_mpfr_arg3 (tree, tree, tree, tree,
			  int (*)(mpfr_ptr, mpfr_srcptr, mpfr_srcptr, mpfr_srcptr, mp_rnd_t));
static tree do_mpfr_sincos (tree, tree, tree);
#if MPFR_VERSION >= MPFR_VERSION_NUM(2,3,0)
static tree do_mpfr_bessel_n (tree, tree, tree,
			      int (*)(mpfr_ptr, long, mpfr_srcptr, mp_rnd_t),
			      const REAL_VALUE_TYPE *, bool);
static tree do_mpfr_remquo (tree, tree, tree);
static tree do_mpfr_lgamma_r (tree, tree, tree);
#endif

/* Return true if NODE should be considered for inline expansion regardless
   of the optimization level.  This means whenever a function is invoked with
   its "internal" name, which normally contains the prefix "__builtin".  */

static bool called_as_built_in (tree node)
{
  const char *name = IDENTIFIER_POINTER (DECL_NAME (node));
  if (strncmp (name, "__builtin_", 10) == 0)
    return true;
  if (strncmp (name, "__sync_", 7) == 0)
    return true;
  return false;
}

/* Return the alignment in bits of EXP, a pointer valued expression.
   But don't return more than MAX_ALIGN no matter what.
   The alignment returned is, by default, the alignment of the thing that
   EXP points to.  If it is not a POINTER_TYPE, 0 is returned.

   Otherwise, look at the expression to see if we can do better, i.e., if the
   expression is actually pointing at an object whose alignment is tighter.  */

int
get_pointer_alignment (tree exp, unsigned int max_align)
{
  unsigned int align, inner;

  /* We rely on TER to compute accurate alignment information.  */
  if (!(optimize && flag_tree_ter))
    return 0;

  if (!POINTER_TYPE_P (TREE_TYPE (exp)))
    return 0;

  align = TYPE_ALIGN (TREE_TYPE (TREE_TYPE (exp)));
  align = MIN (align, max_align);

  while (1)
    {
      switch (TREE_CODE (exp))
	{
	case NOP_EXPR:
	case CONVERT_EXPR:
	case NON_LVALUE_EXPR:
	  exp = TREE_OPERAND (exp, 0);
	  if (! POINTER_TYPE_P (TREE_TYPE (exp)))
	    return align;

	  inner = TYPE_ALIGN (TREE_TYPE (TREE_TYPE (exp)));
	  align = MIN (inner, max_align);
	  break;

	case POINTER_PLUS_EXPR:
	  /* If sum of pointer + int, restrict our maximum alignment to that
	     imposed by the integer.  If not, we can't do any better than
	     ALIGN.  */
	  if (! host_integerp (TREE_OPERAND (exp, 1), 1))
	    return align;

	  while (((tree_low_cst (TREE_OPERAND (exp, 1), 1))
		  & (max_align / BITS_PER_UNIT - 1))
		 != 0)
	    max_align >>= 1;

	  exp = TREE_OPERAND (exp, 0);
	  break;

	case ADDR_EXPR:
	  /* See what we are pointing at and look at its alignment.  */
	  exp = TREE_OPERAND (exp, 0);
	  inner = max_align;
	  if (handled_component_p (exp))
	    {
	      HOST_WIDE_INT bitsize, bitpos;
	      tree offset;
	      enum machine_mode mode; 
	      int unsignedp, volatilep;

	      exp = get_inner_reference (exp, &bitsize, &bitpos, &offset,
					 &mode, &unsignedp, &volatilep, true);
	      if (bitpos)
		inner = MIN (inner, (unsigned) (bitpos & -bitpos));
	      if (offset && TREE_CODE (offset) == PLUS_EXPR
		  && host_integerp (TREE_OPERAND (offset, 1), 1))
	        {
		  /* Any overflow in calculating offset_bits won't change
		     the alignment.  */
		  unsigned offset_bits
		    = ((unsigned) tree_low_cst (TREE_OPERAND (offset, 1), 1)
		       * BITS_PER_UNIT);

		  if (offset_bits)
		    inner = MIN (inner, (offset_bits & -offset_bits));
		  offset = TREE_OPERAND (offset, 0);
		}
	      if (offset && TREE_CODE (offset) == MULT_EXPR
		  && host_integerp (TREE_OPERAND (offset, 1), 1))
	        {
		  /* Any overflow in calculating offset_factor won't change
		     the alignment.  */
		  unsigned offset_factor
		    = ((unsigned) tree_low_cst (TREE_OPERAND (offset, 1), 1)
		       * BITS_PER_UNIT);

		  if (offset_factor)
		    inner = MIN (inner, (offset_factor & -offset_factor));
		}
	      else if (offset)
		inner = MIN (inner, BITS_PER_UNIT);
	    }
	  if (DECL_P (exp))
	    align = MIN (inner, DECL_ALIGN (exp));
#ifdef CONSTANT_ALIGNMENT
	  else if (CONSTANT_CLASS_P (exp))
	    align = MIN (inner, (unsigned)CONSTANT_ALIGNMENT (exp, align));
#endif
	  else if (TREE_CODE (exp) == VIEW_CONVERT_EXPR
		   || TREE_CODE (exp) == INDIRECT_REF)
	    align = MIN (TYPE_ALIGN (TREE_TYPE (exp)), inner);
	  else
	    align = MIN (align, inner);
	  return MIN (align, max_align);

	default:
	  return align;
	}
    }
}

/* Compute the length of a C string.  TREE_STRING_LENGTH is not the right
   way, because it could contain a zero byte in the middle.
   TREE_STRING_LENGTH is the size of the character array, not the string.

   ONLY_VALUE should be nonzero if the result is not going to be emitted
   into the instruction stream and zero if it is going to be expanded.
   E.g. with i++ ? "foo" : "bar", if ONLY_VALUE is nonzero, constant 3
   is returned, otherwise NULL, since
   len = c_strlen (src, 1); if (len) expand_expr (len, ...); would not
   evaluate the side-effects.

   The value returned is of type `ssizetype'.

   Unfortunately, string_constant can't access the values of const char
   arrays with initializers, so neither can we do so here.  */

tree
c_strlen (tree src, int only_value)
{
  tree offset_node;
  HOST_WIDE_INT offset;
  int max;
  const char *ptr;

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

  src = string_constant (src, &offset_node);
  if (src == 0)
    return NULL_TREE;

  max = TREE_STRING_LENGTH (src) - 1;
  ptr = TREE_STRING_POINTER (src);

  if (offset_node && TREE_CODE (offset_node) != INTEGER_CST)
    {
      /* If the string has an internal zero byte (e.g., "foo\0bar"), we can't
	 compute the offset to the following null if we don't know where to
	 start searching for it.  */
      int i;

      for (i = 0; i < max; i++)
	if (ptr[i] == 0)
	  return NULL_TREE;

      /* We don't know the starting offset, but we do know that the string
	 has no internal zero bytes.  We can assume that the offset falls
	 within the bounds of the string; otherwise, the programmer deserves
	 what he gets.  Subtract the offset from the length of the string,
	 and return that.  This would perhaps not be valid if we were dealing
	 with named arrays in addition to literal string constants.  */

      return size_diffop (size_int (max), offset_node);
    }

  /* We have a known offset into the string.  Start searching there for
     a null character if we can represent it as a single HOST_WIDE_INT.  */
  if (offset_node == 0)
    offset = 0;
  else if (! host_integerp (offset_node, 0))
    offset = -1;
  else
    offset = tree_low_cst (offset_node, 0);

  /* If the offset is known to be out of bounds, warn, and call strlen at
     runtime.  */
  if (offset < 0 || offset > max)
    {
      warning (0, "offset outside bounds of constant string");
      return NULL_TREE;
    }

  /* Use strlen to search for the first zero byte.  Since any strings
     constructed with build_string will have nulls appended, we win even
     if we get handed something like (char[4])"abcd".

     Since OFFSET is our starting index into the string, no further
     calculation is needed.  */
  return ssize_int (strlen (ptr + offset));
}

/* Return a char pointer for a C string if it is a string constant
   or sum of string constant and integer constant.  */

static const char *
c_getstr (tree src)
{
  tree offset_node;

  src = string_constant (src, &offset_node);
  if (src == 0)
    return 0;

  if (offset_node == 0)
    return TREE_STRING_POINTER (src);
  else if (!host_integerp (offset_node, 1)
	   || compare_tree_int (offset_node, TREE_STRING_LENGTH (src) - 1) > 0)
    return 0;

  return TREE_STRING_POINTER (src) + tree_low_cst (offset_node, 1);
}

/* Return a CONST_INT or CONST_DOUBLE corresponding to target reading
   GET_MODE_BITSIZE (MODE) bits from string constant STR.  */

static rtx
c_readstr (const char *str, enum machine_mode mode)
{
  HOST_WIDE_INT c[2];
  HOST_WIDE_INT ch;
  unsigned int i, j;

  gcc_assert (GET_MODE_CLASS (mode) == MODE_INT);

  c[0] = 0;
  c[1] = 0;
  ch = 1;
  for (i = 0; i < GET_MODE_SIZE (mode); i++)
    {
      j = i;
      if (WORDS_BIG_ENDIAN)
	j = GET_MODE_SIZE (mode) - i - 1;
      if (BYTES_BIG_ENDIAN != WORDS_BIG_ENDIAN
	  && GET_MODE_SIZE (mode) > UNITS_PER_WORD)
	j = j + UNITS_PER_WORD - 2 * (j % UNITS_PER_WORD) - 1;
      j *= BITS_PER_UNIT;
      gcc_assert (j <= 2 * HOST_BITS_PER_WIDE_INT);

      if (ch)
	ch = (unsigned char) str[i];
      c[j / HOST_BITS_PER_WIDE_INT] |= ch << (j % HOST_BITS_PER_WIDE_INT);
    }
  return immed_double_const (c[0], c[1], mode);
}

/* Cast a target constant CST to target CHAR and if that value fits into
   host char type, return zero and put that value into variable pointed to by
   P.  */

static int
target_char_cast (tree cst, char *p)
{
  unsigned HOST_WIDE_INT val, hostval;

  if (!host_integerp (cst, 1)
      || CHAR_TYPE_SIZE > HOST_BITS_PER_WIDE_INT)
    return 1;

  val = tree_low_cst (cst, 1);
  if (CHAR_TYPE_SIZE < HOST_BITS_PER_WIDE_INT)
    val &= (((unsigned HOST_WIDE_INT) 1) << CHAR_TYPE_SIZE) - 1;

  hostval = val;
  if (HOST_BITS_PER_CHAR < HOST_BITS_PER_WIDE_INT)
    hostval &= (((unsigned HOST_WIDE_INT) 1) << HOST_BITS_PER_CHAR) - 1;

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
  if (TREE_ADDRESSABLE (exp) == 0
      && (TREE_CODE (exp) == PARM_DECL
	  || (TREE_CODE (exp) == VAR_DECL && !TREE_STATIC (exp))))
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

#ifdef INITIAL_FRAME_ADDRESS_RTX
  rtx tem = INITIAL_FRAME_ADDRESS_RTX;
#else
  rtx tem;

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
      current_function_accesses_prior_frames = 1;
    }
#endif

  /* Some machines need special handling before we can access
     arbitrary frames.  For example, on the SPARC, we must first flush
     all register windows to the stack.  */
#ifdef SETUP_FRAME_ADDRESSES
  if (count > 0)
    SETUP_FRAME_ADDRESSES ();
#endif

  /* On the SPARC, the return address is not in the frame, it is in a
     register.  There is no way to access it off of the current frame
     pointer, but it can be accessed off the previous frame pointer by
     reading the value from the register window save area.  */
#ifdef RETURN_ADDR_IN_PREVIOUS_FRAME
  if (fndecl_code == BUILT_IN_RETURN_ADDRESS)
    count--;
#endif

  /* Scan back COUNT frames to the specified frame.  */
  for (i = 0; i < count; i++)
    {
      /* Assume the dynamic chain pointer is in the word that the
	 frame address points to, unless otherwise specified.  */
#ifdef DYNAMIC_CHAIN_ADDRESS
      tem = DYNAMIC_CHAIN_ADDRESS (tem);
#endif
      tem = memory_address (Pmode, tem);
      tem = gen_frame_mem (Pmode, tem);
      tem = copy_to_reg (tem);
    }

  /* For __builtin_frame_address, return what we've got.  But, on
     the SPARC for example, we may have to add a bias.  */
  if (fndecl_code == BUILT_IN_FRAME_ADDRESS)
#ifdef FRAME_ADDR_RTX
    return FRAME_ADDR_RTX (tem);
#else
    return tem;
#endif

  /* For __builtin_return_address, get the return address from that frame.  */
#ifdef RETURN_ADDR_RTX
  tem = RETURN_ADDR_RTX (count, tem);
#else
  tem = memory_address (Pmode,
			plus_constant (tem, GET_MODE_SIZE (Pmode)));
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
  enum machine_mode sa_mode = STACK_SAVEAREA_MODE (SAVE_NONLOCAL);
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

  mem = gen_rtx_MEM (Pmode, plus_constant (buf_addr, GET_MODE_SIZE (Pmode))),
  set_mem_alias_set (mem, setjmp_alias_set);

  emit_move_insn (validize_mem (mem),
		  force_reg (Pmode, gen_rtx_LABEL_REF (Pmode, receiver_label)));

  stack_save = gen_rtx_MEM (sa_mode,
			    plus_constant (buf_addr,
					   2 * GET_MODE_SIZE (Pmode)));
  set_mem_alias_set (stack_save, setjmp_alias_set);
  emit_stack_save (SAVE_NONLOCAL, &stack_save, NULL_RTX);

  /* If there is further processing to do, do it.  */
#ifdef HAVE_builtin_setjmp_setup
  if (HAVE_builtin_setjmp_setup)
    emit_insn (gen_builtin_setjmp_setup (buf_addr));
#endif

  /* Tell optimize_save_area_alloca that extra work is going to
     need to go on during alloca.  */
  current_function_calls_setjmp = 1;

  /* We have a nonlocal label.   */
  current_function_has_nonlocal_label = 1;
}

/* Construct the trailing part of a __builtin_setjmp call.  This is
   also called directly by the SJLJ exception handling code.  */

void
expand_builtin_setjmp_receiver (rtx receiver_label ATTRIBUTE_UNUSED)
{
  /* Clobber the FP when we get here, so we have to make sure it's
     marked as used by this function.  */
  emit_insn (gen_rtx_USE (VOIDmode, hard_frame_pointer_rtx));

  /* Mark the static chain as clobbered here so life information
     doesn't get messed up for it.  */
  emit_insn (gen_rtx_CLOBBER (VOIDmode, static_chain_rtx));

  /* Now put in the code to restore the frame pointer, and argument
     pointer, if needed.  */
#ifdef HAVE_nonlocal_goto
  if (! HAVE_nonlocal_goto)
#endif
    {
      emit_move_insn (virtual_stack_vars_rtx, hard_frame_pointer_rtx);
      /* This might change the hard frame pointer in ways that aren't
	 apparent to early optimization passes, so force a clobber.  */
      emit_insn (gen_rtx_CLOBBER (VOIDmode, hard_frame_pointer_rtx));
    }

#if ARG_POINTER_REGNUM != HARD_FRAME_POINTER_REGNUM
  if (fixed_regs[ARG_POINTER_REGNUM])
    {
#ifdef ELIMINABLE_REGS
      size_t i;
      static const struct elims {const int from, to;} elim_regs[] = ELIMINABLE_REGS;

      for (i = 0; i < ARRAY_SIZE (elim_regs); i++)
	if (elim_regs[i].from == ARG_POINTER_REGNUM
	    && elim_regs[i].to == HARD_FRAME_POINTER_REGNUM)
	  break;

      if (i == ARRAY_SIZE (elim_regs))
#endif
	{
	  /* Now restore our arg pointer from the address at which it
	     was saved in our stack frame.  */
	  emit_move_insn (virtual_incoming_args_rtx,
			  copy_to_reg (get_arg_pointer_save_area (cfun)));
	}
    }
#endif

#ifdef HAVE_builtin_setjmp_receiver
  if (HAVE_builtin_setjmp_receiver)
    emit_insn (gen_builtin_setjmp_receiver (receiver_label));
  else
#endif
#ifdef HAVE_nonlocal_goto_receiver
    if (HAVE_nonlocal_goto_receiver)
      emit_insn (gen_nonlocal_goto_receiver ());
    else
#endif
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
  rtx fp, lab, stack, insn, last;
  enum machine_mode sa_mode = STACK_SAVEAREA_MODE (SAVE_NONLOCAL);

  if (setjmp_alias_set == -1)
    setjmp_alias_set = new_alias_set ();

  buf_addr = convert_memory_address (Pmode, buf_addr);

  buf_addr = force_reg (Pmode, buf_addr);

  /* We used to store value in static_chain_rtx, but that fails if pointers
     are smaller than integers.  We instead require that the user must pass
     a second argument of 1, because that is what builtin_setjmp will
     return.  This also makes EH slightly more efficient, since we are no
     longer copying around a value that we don't care about.  */
  gcc_assert (value == const1_rtx);

  last = get_last_insn ();
#ifdef HAVE_builtin_longjmp
  if (HAVE_builtin_longjmp)
    emit_insn (gen_builtin_longjmp (buf_addr));
  else
#endif
    {
      fp = gen_rtx_MEM (Pmode, buf_addr);
      lab = gen_rtx_MEM (Pmode, plus_constant (buf_addr,
					       GET_MODE_SIZE (Pmode)));

      stack = gen_rtx_MEM (sa_mode, plus_constant (buf_addr,
						   2 * GET_MODE_SIZE (Pmode)));
      set_mem_alias_set (fp, setjmp_alias_set);
      set_mem_alias_set (lab, setjmp_alias_set);
      set_mem_alias_set (stack, setjmp_alias_set);

      /* Pick up FP, label, and SP from the block and jump.  This code is
	 from expand_goto in stmt.c; see there for detailed comments.  */
#ifdef HAVE_nonlocal_goto
      if (HAVE_nonlocal_goto)
	/* We have to pass a value to the nonlocal_goto pattern that will
	   get copied into the static_chain pointer, but it does not matter
	   what that value is, because builtin_setjmp does not use it.  */
	emit_insn (gen_nonlocal_goto (value, lab, stack, fp));
      else
#endif
	{
	  lab = copy_to_reg (lab);

	  emit_insn (gen_rtx_CLOBBER (VOIDmode,
				      gen_rtx_MEM (BLKmode,
						   gen_rtx_SCRATCH (VOIDmode))));
	  emit_insn (gen_rtx_CLOBBER (VOIDmode,
				      gen_rtx_MEM (BLKmode,
						   hard_frame_pointer_rtx)));

	  emit_move_insn (hard_frame_pointer_rtx, fp);
	  emit_stack_restore (SAVE_NONLOCAL, stack, NULL_RTX);

	  emit_insn (gen_rtx_USE (VOIDmode, hard_frame_pointer_rtx));
	  emit_insn (gen_rtx_USE (VOIDmode, stack_pointer_rtx));
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
	  REG_NOTES (insn) = alloc_EXPR_LIST (REG_NON_LOCAL_GOTO, const0_rtx,
					      REG_NOTES (insn));
	  break;
	}
      else if (CALL_P (insn))
	break;
    }
}

/* Expand a call to __builtin_nonlocal_goto.  We're passed the target label
   and the address of the save area.  */

static rtx
expand_builtin_nonlocal_goto (tree exp)
{
  tree t_label, t_save_area;
  rtx r_label, r_save_area, r_fp, r_sp, insn;

  if (!validate_arglist (exp, POINTER_TYPE, POINTER_TYPE, VOID_TYPE))
    return NULL_RTX;

  t_label = CALL_EXPR_ARG (exp, 0);
  t_save_area = CALL_EXPR_ARG (exp, 1);

  r_label = expand_normal (t_label);
  r_label = convert_memory_address (Pmode, r_label);
  r_save_area = expand_normal (t_save_area);
  r_save_area = convert_memory_address (Pmode, r_save_area);
  r_fp = gen_rtx_MEM (Pmode, r_save_area);
  r_sp = gen_rtx_MEM (STACK_SAVEAREA_MODE (SAVE_NONLOCAL),
		      plus_constant (r_save_area, GET_MODE_SIZE (Pmode)));

  current_function_has_nonlocal_goto = 1;

#ifdef HAVE_nonlocal_goto
  /* ??? We no longer need to pass the static chain value, afaik.  */
  if (HAVE_nonlocal_goto)
    emit_insn (gen_nonlocal_goto (const0_rtx, r_label, r_sp, r_fp));
  else
#endif
    {
      r_label = copy_to_reg (r_label);

      emit_insn (gen_rtx_CLOBBER (VOIDmode,
				  gen_rtx_MEM (BLKmode,
					       gen_rtx_SCRATCH (VOIDmode))));

      emit_insn (gen_rtx_CLOBBER (VOIDmode,
				  gen_rtx_MEM (BLKmode,
					       hard_frame_pointer_rtx)));

      /* Restore frame pointer for containing function.
	 This sets the actual hard register used for the frame pointer
	 to the location of the function's incoming static chain info.
	 The non-local goto handler will then adjust it to contain the
	 proper value and reload the argument pointer, if needed.  */
      emit_move_insn (hard_frame_pointer_rtx, r_fp);
      emit_stack_restore (SAVE_NONLOCAL, r_sp, NULL_RTX);

      /* USE of hard_frame_pointer_rtx added for consistency;
	 not clear if really needed.  */
      emit_insn (gen_rtx_USE (VOIDmode, hard_frame_pointer_rtx));
      emit_insn (gen_rtx_USE (VOIDmode, stack_pointer_rtx));

      /* If the architecture is using a GP register, we must
	 conservatively assume that the target function makes use of it.
	 The prologue of functions with nonlocal gotos must therefore
	 initialize the GP register to the appropriate value, and we
	 must then make sure that this value is live at the point
	 of the jump.  (Note that this doesn't necessarily apply
	 to targets with a nonlocal_goto pattern; they are free
	 to implement it in their own way.  Note also that this is
	 a no-op if the GP register is a global invariant.)  */
      if ((unsigned) PIC_OFFSET_TABLE_REGNUM != INVALID_REGNUM
	  && fixed_regs[PIC_OFFSET_TABLE_REGNUM])
	emit_insn (gen_rtx_USE (VOIDmode, pic_offset_table_rtx));

      emit_indirect_jump (r_label);
    }

  /* Search backwards to the jump insn and mark it as a
     non-local goto.  */
  for (insn = get_last_insn (); insn; insn = PREV_INSN (insn))
    {
      if (JUMP_P (insn))
	{
	  REG_NOTES (insn) = alloc_EXPR_LIST (REG_NON_LOCAL_GOTO,
					      const0_rtx, REG_NOTES (insn));
	  break;
	}
      else if (CALL_P (insn))
	break;
    }

  return const0_rtx;
}

/* __builtin_update_setjmp_buf is passed a pointer to an array of five words
   (not all will be used on all machines) that was passed to __builtin_setjmp.
   It updates the stack pointer in that block to correspond to the current
   stack pointer.  */

static void
expand_builtin_update_setjmp_buf (rtx buf_addr)
{
  enum machine_mode sa_mode = Pmode;
  rtx stack_save;


#ifdef HAVE_save_stack_nonlocal
  if (HAVE_save_stack_nonlocal)
    sa_mode = insn_data[(int) CODE_FOR_save_stack_nonlocal].operand[0].mode;
#endif
#ifdef STACK_SAVEAREA_MODE
  sa_mode = STACK_SAVEAREA_MODE (SAVE_NONLOCAL);
#endif

  stack_save
    = gen_rtx_MEM (sa_mode,
		   memory_address
		   (sa_mode,
		    plus_constant (buf_addr, 2 * GET_MODE_SIZE (Pmode))));

#ifdef HAVE_setjmp
  if (HAVE_setjmp)
    emit_insn (gen_setjmp ());
#endif

  emit_stack_save (SAVE_NONLOCAL, &stack_save, NULL_RTX);
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
    arg2 = build_int_cst (NULL_TREE, 3);

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

#ifdef HAVE_prefetch
  if (HAVE_prefetch)
    {
      if ((! (*insn_data[(int) CODE_FOR_prefetch].operand[0].predicate)
	     (op0,
	      insn_data[(int) CODE_FOR_prefetch].operand[0].mode))
	  || (GET_MODE (op0) != Pmode))
	{
	  op0 = convert_memory_address (Pmode, op0);
	  op0 = force_reg (Pmode, op0);
	}
      emit_insn (gen_prefetch (op0, op1, op2));
    }
#endif

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
  rtx addr = expand_expr (exp, NULL_RTX, ptr_mode, EXPAND_NORMAL);
  rtx mem = gen_rtx_MEM (BLKmode, memory_address (BLKmode, addr));

  /* Get an expression we can use to find the attributes to assign to MEM.
     If it is an ADDR_EXPR, use the operand.  Otherwise, dereference it if
     we can.  First remove any nops.  */
  while ((TREE_CODE (exp) == NOP_EXPR || TREE_CODE (exp) == CONVERT_EXPR
	  || TREE_CODE (exp) == NON_LVALUE_EXPR)
	 && POINTER_TYPE_P (TREE_TYPE (TREE_OPERAND (exp, 0))))
    exp = TREE_OPERAND (exp, 0);

  if (TREE_CODE (exp) == ADDR_EXPR)
    exp = TREE_OPERAND (exp, 0);
  else if (POINTER_TYPE_P (TREE_TYPE (exp)))
    exp = build1 (INDIRECT_REF, TREE_TYPE (TREE_TYPE (exp)), exp);
  else
    exp = NULL;

  /* Honor attributes derived from exp, except for the alias set
     (as builtin stringops may alias with anything) and the size
     (as stringops may access multiple array elements).  */
  if (exp)
    {
      set_mem_attributes (mem, exp, 0);

      /* Allow the string and memory builtins to overflow from one
	 field into another, see http://gcc.gnu.org/PR23561.
	 Thus avoid COMPONENT_REFs in MEM_EXPR unless we know the whole
	 memory accessed by the string or memory builtin will fit
	 within the field.  */
      if (MEM_EXPR (mem) && TREE_CODE (MEM_EXPR (mem)) == COMPONENT_REF)
	{
	  tree mem_expr = MEM_EXPR (mem);
	  HOST_WIDE_INT offset = -1, length = -1;
	  tree inner = exp;

	  while (TREE_CODE (inner) == ARRAY_REF
		 || TREE_CODE (inner) == NOP_EXPR
		 || TREE_CODE (inner) == CONVERT_EXPR
		 || TREE_CODE (inner) == NON_LVALUE_EXPR
		 || TREE_CODE (inner) == VIEW_CONVERT_EXPR
		 || TREE_CODE (inner) == SAVE_EXPR)
	    inner = TREE_OPERAND (inner, 0);

	  gcc_assert (TREE_CODE (inner) == COMPONENT_REF);

	  if (MEM_OFFSET (mem)
	      && GET_CODE (MEM_OFFSET (mem)) == CONST_INT)
	    offset = INTVAL (MEM_OFFSET (mem));

	  if (offset >= 0 && len && host_integerp (len, 0))
	    length = tree_low_cst (len, 0);

	  while (TREE_CODE (inner) == COMPONENT_REF)
	    {
	      tree field = TREE_OPERAND (inner, 1);
	      gcc_assert (! DECL_BIT_FIELD (field));
	      gcc_assert (TREE_CODE (mem_expr) == COMPONENT_REF);
	      gcc_assert (field == TREE_OPERAND (mem_expr, 1));

	      if (length >= 0
		  && TYPE_SIZE_UNIT (TREE_TYPE (inner))
		  && host_integerp (TYPE_SIZE_UNIT (TREE_TYPE (inner)), 0))
		{
		  HOST_WIDE_INT size
		    = tree_low_cst (TYPE_SIZE_UNIT (TREE_TYPE (inner)), 0);
		  /* If we can prove the memory starting at XEXP (mem, 0)
		     and ending at XEXP (mem, 0) + LENGTH will fit into
		     this field, we can keep that COMPONENT_REF in MEM_EXPR.  */
		  if (offset <= size
		      && length <= size
		      && offset + length <= size)
		    break;
		}

	      if (offset >= 0
		  && host_integerp (DECL_FIELD_OFFSET (field), 0))
		offset += tree_low_cst (DECL_FIELD_OFFSET (field), 0)
			  + tree_low_cst (DECL_FIELD_BIT_OFFSET (field), 1)
			    / BITS_PER_UNIT;
	      else
		{
		  offset = -1;
		  length = -1;
		}

	      mem_expr = TREE_OPERAND (mem_expr, 0);
	      inner = TREE_OPERAND (inner, 0);
	    }

	  if (mem_expr == NULL)
	    offset = -1;
	  if (mem_expr != MEM_EXPR (mem))
	    {
	      set_mem_expr (mem, mem_expr);
	      set_mem_offset (mem, offset >= 0 ? GEN_INT (offset) : NULL_RTX);
	    }
	}
      set_mem_alias_set (mem, 0);
      set_mem_size (mem, NULL_RTX);
    }

  return mem;
}

/* Built-in functions to perform an untyped call and return.  */

/* For each register that may be used for calling a function, this
   gives a mode used to copy the register's value.  VOIDmode indicates
   the register is not used for calling a function.  If the machine
   has register windows, this gives only the outbound registers.
   INCOMING_REGNO gives the corresponding inbound register.  */
static enum machine_mode apply_args_mode[FIRST_PSEUDO_REGISTER];

/* For each register that may be used for returning values, this gives
   a mode used to copy the register's value.  VOIDmode indicates the
   register is not used for returning values.  If the machine has
   register windows, this gives only the outbound registers.
   INCOMING_REGNO gives the corresponding inbound register.  */
static enum machine_mode apply_result_mode[FIRST_PSEUDO_REGISTER];

/* For each register that may be used for calling a function, this
   gives the offset of that register into the block returned by
   __builtin_apply_args.  0 indicates that the register is not
   used for calling a function.  */
static int apply_args_reg_offset[FIRST_PSEUDO_REGISTER];

/* Return the size required for the block returned by __builtin_apply_args,
   and initialize apply_args_mode.  */

static int
apply_args_size (void)
{
  static int size = -1;
  int align;
  unsigned int regno;
  enum machine_mode mode;

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
	    mode = reg_raw_mode[regno];

	    gcc_assert (mode != VOIDmode);

	    align = GET_MODE_ALIGNMENT (mode) / BITS_PER_UNIT;
	    if (size % align != 0)
	      size = CEIL (size, align) * align;
	    apply_args_reg_offset[regno] = size;
	    size += GET_MODE_SIZE (mode);
	    apply_args_mode[regno] = mode;
	  }
	else
	  {
	    apply_args_mode[regno] = VOIDmode;
	    apply_args_reg_offset[regno] = 0;
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
  enum machine_mode mode;

  /* The values computed by this function never change.  */
  if (size < 0)
    {
      size = 0;

      for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
	if (FUNCTION_VALUE_REGNO_P (regno))
	  {
	    mode = reg_raw_mode[regno];

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

#if defined (HAVE_untyped_call) || defined (HAVE_untyped_return)
/* Create a vector describing the result block RESULT.  If SAVEP is true,
   the result block is used to save the values; otherwise it is used to
   restore the values.  */

static rtx
result_vector (int savep, rtx result)
{
  int regno, size, align, nelts;
  enum machine_mode mode;
  rtx reg, mem;
  rtx *savevec = alloca (FIRST_PSEUDO_REGISTER * sizeof (rtx));

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
			    ? gen_rtx_SET (VOIDmode, mem, reg)
			    : gen_rtx_SET (VOIDmode, reg, mem));
	size += GET_MODE_SIZE (mode);
      }
  return gen_rtx_PARALLEL (VOIDmode, gen_rtvec_v (nelts, savevec));
}
#endif /* HAVE_untyped_call or HAVE_untyped_return */

/* Save the state required to perform an untyped call with the same
   arguments as were passed to the current function.  */

static rtx
expand_builtin_apply_args_1 (void)
{
  rtx registers, tem;
  int size, align, regno;
  enum machine_mode mode;
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
  tem = copy_to_reg (virtual_incoming_args_rtx);
#ifdef STACK_GROWS_DOWNWARD
  /* We need the pointer as the caller actually passed them to us, not
     as we might have pretended they were passed.  Make sure it's a valid
     operand, as emit_move_insn isn't expected to handle a PLUS.  */
  tem
    = force_operand (plus_constant (tem, current_function_pretend_args_size),
		     NULL_RTX);
#endif
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
    rtx seq;

    start_sequence ();
    temp = expand_builtin_apply_args_1 ();
    seq = get_insns ();
    end_sequence ();

    apply_args_value = temp;

    /* Put the insns after the NOTE that starts the function.
       If this is inside a start_sequence, make the outer-level insn
       chain current, so the code is placed at the start of the
       function.  */
    push_topmost_sequence ();
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
  enum machine_mode mode;
  rtx incoming_args, result, reg, dest, src, call_insn;
  rtx old_stack_level = 0;
  rtx call_fusage = 0;
  rtx struct_value = targetm.calls.struct_value_rtx (cfun ? TREE_TYPE (cfun->decl) : 0, 0);

  arguments = convert_memory_address (Pmode, arguments);

  /* Create a block where the return registers can be saved.  */
  result = assign_stack_local (BLKmode, apply_result_size (), -1);

  /* Fetch the arg pointer from the ARGUMENTS block.  */
  incoming_args = gen_reg_rtx (Pmode);
  emit_move_insn (incoming_args, gen_rtx_MEM (Pmode, arguments));
#ifndef STACK_GROWS_DOWNWARD
  incoming_args = expand_simple_binop (Pmode, MINUS, incoming_args, argsize,
				       incoming_args, 0, OPTAB_LIB_WIDEN);
#endif

  /* Push a new argument block and copy the arguments.  Do not allow
     the (potential) memcpy call below to interfere with our stack
     manipulations.  */
  do_pending_stack_adjust ();
  NO_DEFER_POP;

  /* Save the stack with nonlocal if available.  */
#ifdef HAVE_save_stack_nonlocal
  if (HAVE_save_stack_nonlocal)
    emit_stack_save (SAVE_NONLOCAL, &old_stack_level, NULL_RTX);
  else
#endif
    emit_stack_save (SAVE_BLOCK, &old_stack_level, NULL_RTX);

  /* Allocate a block of memory onto the stack and copy the memory
     arguments to the outgoing arguments address.  */
  allocate_dynamic_stack_space (argsize, 0, BITS_PER_UNIT);
  dest = virtual_outgoing_args_rtx;
#ifndef STACK_GROWS_DOWNWARD
  if (GET_CODE (argsize) == CONST_INT)
    dest = plus_constant (dest, -INTVAL (argsize));
  else
    dest = gen_rtx_PLUS (Pmode, dest, negate_rtx (Pmode, argsize));
#endif
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
  function = prepare_call_address (function, NULL, &call_fusage, 0, 0);

  /* Ensure address is valid.  SYMBOL_REF is already valid, so no need,
     and we don't want to load it into a register as an optimization,
     because prepare_call_address already did it if it should be done.  */
  if (GET_CODE (function) != SYMBOL_REF)
    function = memory_address (FUNCTION_MODE, function);

  /* Generate the actual call instruction and save the return value.  */
#ifdef HAVE_untyped_call
  if (HAVE_untyped_call)
    emit_call_insn (gen_untyped_call (gen_rtx_MEM (FUNCTION_MODE, function),
				      result, result_vector (1, result)));
  else
#endif
#ifdef HAVE_call_value
  if (HAVE_call_value)
    {
      rtx valreg = 0;

      /* Locate the unique return register.  It is not possible to
	 express a call that sets more than one return register using
	 call_value; use untyped_call for that.  In fact, untyped_call
	 only needs to save the return registers in the given block.  */
      for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
	if ((mode = apply_result_mode[regno]) != VOIDmode)
	  {
	    gcc_assert (!valreg); /* HAVE_untyped_call required.  */

	    valreg = gen_rtx_REG (mode, regno);
	  }

      emit_call_insn (GEN_CALL_VALUE (valreg,
				      gen_rtx_MEM (FUNCTION_MODE, function),
				      const0_rtx, NULL_RTX, const0_rtx));

      emit_move_insn (adjust_address (result, GET_MODE (valreg), 0), valreg);
    }
  else
#endif
    gcc_unreachable ();

  /* Find the CALL insn we just emitted, and attach the register usage
     information.  */
  call_insn = last_call_insn ();
  add_function_usage_to (call_insn, call_fusage);

  /* Restore the stack.  */
#ifdef HAVE_save_stack_nonlocal
  if (HAVE_save_stack_nonlocal)
    emit_stack_restore (SAVE_NONLOCAL, old_stack_level, NULL_RTX);
  else
#endif
    emit_stack_restore (SAVE_BLOCK, old_stack_level, NULL_RTX);

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
  enum machine_mode mode;
  rtx reg;
  rtx call_fusage = 0;

  result = convert_memory_address (Pmode, result);

  apply_result_size ();
  result = gen_rtx_MEM (BLKmode, result);

#ifdef HAVE_untyped_return
  if (HAVE_untyped_return)
    {
      emit_jump_insn (gen_untyped_return (result, result_vector (0, result)));
      emit_barrier ();
      return;
    }
#endif

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
	emit_insn (gen_rtx_USE (VOIDmode, reg));
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
#define CASE_MATHFN(BUILT_IN_MATHFN) \
  case BUILT_IN_MATHFN: case BUILT_IN_MATHFN##F: case BUILT_IN_MATHFN##L: \
  fcode = BUILT_IN_MATHFN; fcodef = BUILT_IN_MATHFN##F ; \
  fcodel = BUILT_IN_MATHFN##L ; break;
/* Similar to above, but appends _R after any F/L suffix.  */
#define CASE_MATHFN_REENT(BUILT_IN_MATHFN) \
  case BUILT_IN_MATHFN##_R: case BUILT_IN_MATHFN##F_R: case BUILT_IN_MATHFN##L_R: \
  fcode = BUILT_IN_MATHFN##_R; fcodef = BUILT_IN_MATHFN##F_R ; \
  fcodel = BUILT_IN_MATHFN##L_R ; break;

/* Return mathematic function equivalent to FN but operating directly
   on TYPE, if available.  If we can't do the conversion, return zero.  */
tree
mathfn_built_in (tree type, enum built_in_function fn)
{
  enum built_in_function fcode, fcodef, fcodel;

  switch (fn)
    {
      CASE_MATHFN (BUILT_IN_ACOS)
      CASE_MATHFN (BUILT_IN_ACOSH)
      CASE_MATHFN (BUILT_IN_ASIN)
      CASE_MATHFN (BUILT_IN_ASINH)
      CASE_MATHFN (BUILT_IN_ATAN)
      CASE_MATHFN (BUILT_IN_ATAN2)
      CASE_MATHFN (BUILT_IN_ATANH)
      CASE_MATHFN (BUILT_IN_CBRT)
      CASE_MATHFN (BUILT_IN_CEIL)
      CASE_MATHFN (BUILT_IN_CEXPI)
      CASE_MATHFN (BUILT_IN_COPYSIGN)
      CASE_MATHFN (BUILT_IN_COS)
      CASE_MATHFN (BUILT_IN_COSH)
      CASE_MATHFN (BUILT_IN_DREM)
      CASE_MATHFN (BUILT_IN_ERF)
      CASE_MATHFN (BUILT_IN_ERFC)
      CASE_MATHFN (BUILT_IN_EXP)
      CASE_MATHFN (BUILT_IN_EXP10)
      CASE_MATHFN (BUILT_IN_EXP2)
      CASE_MATHFN (BUILT_IN_EXPM1)
      CASE_MATHFN (BUILT_IN_FABS)
      CASE_MATHFN (BUILT_IN_FDIM)
      CASE_MATHFN (BUILT_IN_FLOOR)
      CASE_MATHFN (BUILT_IN_FMA)
      CASE_MATHFN (BUILT_IN_FMAX)
      CASE_MATHFN (BUILT_IN_FMIN)
      CASE_MATHFN (BUILT_IN_FMOD)
      CASE_MATHFN (BUILT_IN_FREXP)
      CASE_MATHFN (BUILT_IN_GAMMA)
      CASE_MATHFN_REENT (BUILT_IN_GAMMA) /* GAMMA_R */
      CASE_MATHFN (BUILT_IN_HUGE_VAL)
      CASE_MATHFN (BUILT_IN_HYPOT)
      CASE_MATHFN (BUILT_IN_ILOGB)
      CASE_MATHFN (BUILT_IN_INF)
      CASE_MATHFN (BUILT_IN_ISINF)
      CASE_MATHFN (BUILT_IN_J0)
      CASE_MATHFN (BUILT_IN_J1)
      CASE_MATHFN (BUILT_IN_JN)
      CASE_MATHFN (BUILT_IN_LCEIL)
      CASE_MATHFN (BUILT_IN_LDEXP)
      CASE_MATHFN (BUILT_IN_LFLOOR)
      CASE_MATHFN (BUILT_IN_LGAMMA)
      CASE_MATHFN_REENT (BUILT_IN_LGAMMA) /* LGAMMA_R */
      CASE_MATHFN (BUILT_IN_LLCEIL)
      CASE_MATHFN (BUILT_IN_LLFLOOR)
      CASE_MATHFN (BUILT_IN_LLRINT)
      CASE_MATHFN (BUILT_IN_LLROUND)
      CASE_MATHFN (BUILT_IN_LOG)
      CASE_MATHFN (BUILT_IN_LOG10)
      CASE_MATHFN (BUILT_IN_LOG1P)
      CASE_MATHFN (BUILT_IN_LOG2)
      CASE_MATHFN (BUILT_IN_LOGB)
      CASE_MATHFN (BUILT_IN_LRINT)
      CASE_MATHFN (BUILT_IN_LROUND)
      CASE_MATHFN (BUILT_IN_MODF)
      CASE_MATHFN (BUILT_IN_NAN)
      CASE_MATHFN (BUILT_IN_NANS)
      CASE_MATHFN (BUILT_IN_NEARBYINT)
      CASE_MATHFN (BUILT_IN_NEXTAFTER)
      CASE_MATHFN (BUILT_IN_NEXTTOWARD)
      CASE_MATHFN (BUILT_IN_POW)
      CASE_MATHFN (BUILT_IN_POWI)
      CASE_MATHFN (BUILT_IN_POW10)
      CASE_MATHFN (BUILT_IN_REMAINDER)
      CASE_MATHFN (BUILT_IN_REMQUO)
      CASE_MATHFN (BUILT_IN_RINT)
      CASE_MATHFN (BUILT_IN_ROUND)
      CASE_MATHFN (BUILT_IN_SCALB)
      CASE_MATHFN (BUILT_IN_SCALBLN)
      CASE_MATHFN (BUILT_IN_SCALBN)
      CASE_MATHFN (BUILT_IN_SIGNIFICAND)
      CASE_MATHFN (BUILT_IN_SIN)
      CASE_MATHFN (BUILT_IN_SINCOS)
      CASE_MATHFN (BUILT_IN_SINH)
      CASE_MATHFN (BUILT_IN_SQRT)
      CASE_MATHFN (BUILT_IN_TAN)
      CASE_MATHFN (BUILT_IN_TANH)
      CASE_MATHFN (BUILT_IN_TGAMMA)
      CASE_MATHFN (BUILT_IN_TRUNC)
      CASE_MATHFN (BUILT_IN_Y0)
      CASE_MATHFN (BUILT_IN_Y1)
      CASE_MATHFN (BUILT_IN_YN)

      default:
	return NULL_TREE;
      }

  if (TYPE_MAIN_VARIANT (type) == double_type_node)
    return implicit_built_in_decls[fcode];
  else if (TYPE_MAIN_VARIANT (type) == float_type_node)
    return implicit_built_in_decls[fcodef];
  else if (TYPE_MAIN_VARIANT (type) == long_double_type_node)
    return implicit_built_in_decls[fcodel];
  else
    return NULL_TREE;
}

/* If errno must be maintained, expand the RTL to check if the result,
   TARGET, of a built-in function call, EXP, is NaN, and if so set
   errno to EDOM.  */

static void
expand_errno_check (tree exp, rtx target)
{
  rtx lab = gen_label_rtx ();

  /* Test the result; if it is NaN, set errno=EDOM because
     the argument was not in the domain.  */
  emit_cmp_and_jump_insns (target, target, EQ, 0, GET_MODE (target),
			   0, lab);

#ifdef TARGET_EDOM
  /* If this built-in doesn't throw an exception, set errno directly.  */
  if (TREE_NOTHROW (TREE_OPERAND (CALL_EXPR_FN (exp), 0)))
    {
#ifdef GEN_ERRNO_RTX
      rtx errno_rtx = GEN_ERRNO_RTX;
#else
      rtx errno_rtx
	  = gen_rtx_MEM (word_mode, gen_rtx_SYMBOL_REF (Pmode, "errno"));
#endif
      emit_move_insn (errno_rtx, GEN_INT (TARGET_EDOM));
      emit_label (lab);
      return;
    }
#endif

  /* Make sure the library call isn't expanded as a tail call.  */
  CALL_EXPR_TAILCALL (exp) = 0;

  /* We can't set errno=EDOM directly; let the library call do it.
     Pop the arguments right away in case the call gets deleted.  */
  NO_DEFER_POP;
  expand_call (exp, target, 0);
  OK_DEFER_POP;
  emit_label (lab);
}

/* Expand a call to one of the builtin math functions (sqrt, exp, or log).
   Return NULL_RTX if a normal call should be emitted rather than expanding
   the function in-line.  EXP is the expression that is a call to the builtin
   function; if convenient, the result should be placed in TARGET.
   SUBTARGET may be used as the target for computing one of EXP's operands.  */

static rtx
expand_builtin_mathfn (tree exp, rtx target, rtx subtarget)
{
  optab builtin_optab;
  rtx op0, insns, before_call;
  tree fndecl = get_callee_fndecl (exp);
  enum machine_mode mode;
  bool errno_set = false;
  tree arg;

  if (!validate_arglist (exp, REAL_TYPE, VOID_TYPE))
    return NULL_RTX;

  arg = CALL_EXPR_ARG (exp, 0);

  switch (DECL_FUNCTION_CODE (fndecl))
    {
    CASE_FLT_FN (BUILT_IN_SQRT):
      errno_set = ! tree_expr_nonnegative_p (arg);
      builtin_optab = sqrt_optab;
      break;
    CASE_FLT_FN (BUILT_IN_EXP):
      errno_set = true; builtin_optab = exp_optab; break;
    CASE_FLT_FN (BUILT_IN_EXP10):
    CASE_FLT_FN (BUILT_IN_POW10):
      errno_set = true; builtin_optab = exp10_optab; break;
    CASE_FLT_FN (BUILT_IN_EXP2):
      errno_set = true; builtin_optab = exp2_optab; break;
    CASE_FLT_FN (BUILT_IN_EXPM1):
      errno_set = true; builtin_optab = expm1_optab; break;
    CASE_FLT_FN (BUILT_IN_LOGB):
      errno_set = true; builtin_optab = logb_optab; break;
    CASE_FLT_FN (BUILT_IN_LOG):
      errno_set = true; builtin_optab = log_optab; break;
    CASE_FLT_FN (BUILT_IN_LOG10):
      errno_set = true; builtin_optab = log10_optab; break;
    CASE_FLT_FN (BUILT_IN_LOG2):
      errno_set = true; builtin_optab = log2_optab; break;
    CASE_FLT_FN (BUILT_IN_LOG1P):
      errno_set = true; builtin_optab = log1p_optab; break;
    CASE_FLT_FN (BUILT_IN_ASIN):
      builtin_optab = asin_optab; break;
    CASE_FLT_FN (BUILT_IN_ACOS):
      builtin_optab = acos_optab; break;
    CASE_FLT_FN (BUILT_IN_TAN):
      builtin_optab = tan_optab; break;
    CASE_FLT_FN (BUILT_IN_ATAN):
      builtin_optab = atan_optab; break;
    CASE_FLT_FN (BUILT_IN_FLOOR):
      builtin_optab = floor_optab; break;
    CASE_FLT_FN (BUILT_IN_CEIL):
      builtin_optab = ceil_optab; break;
    CASE_FLT_FN (BUILT_IN_TRUNC):
      builtin_optab = btrunc_optab; break;
    CASE_FLT_FN (BUILT_IN_ROUND):
      builtin_optab = round_optab; break;
    CASE_FLT_FN (BUILT_IN_NEARBYINT):
      builtin_optab = nearbyint_optab;
      if (flag_trapping_math)
	break;
      /* Else fallthrough and expand as rint.  */
    CASE_FLT_FN (BUILT_IN_RINT):
      builtin_optab = rint_optab; break;
    default:
      gcc_unreachable ();
    }

  /* Make a suitable register to place result in.  */
  mode = TYPE_MODE (TREE_TYPE (exp));

  if (! flag_errno_math || ! HONOR_NANS (mode))
    errno_set = false;

  /* Before working hard, check whether the instruction is available.  */
  if (optab_handler (builtin_optab, mode)->insn_code != CODE_FOR_nothing)
    {
      target = gen_reg_rtx (mode);

      /* Wrap the computation of the argument in a SAVE_EXPR, as we may
	 need to expand the argument again.  This way, we will not perform
	 side-effects more the once.  */
      CALL_EXPR_ARG (exp, 0) = arg = builtin_save_expr (arg);

      op0 = expand_expr (arg, subtarget, VOIDmode, EXPAND_NORMAL);

      start_sequence ();

      /* Compute into TARGET.
	 Set TARGET to wherever the result comes back.  */
      target = expand_unop (mode, builtin_optab, op0, target, 0);

      if (target != 0)
	{
	  if (errno_set)
	    expand_errno_check (exp, target);

	  /* Output the entire sequence.  */
	  insns = get_insns ();
	  end_sequence ();
	  emit_insn (insns);
	  return target;
	}

      /* If we were unable to expand via the builtin, stop the sequence
	 (without outputting the insns) and call to the library function
	 with the stabilized argument list.  */
      end_sequence ();
    }

  before_call = get_last_insn ();

  target = expand_call (exp, target, target == const0_rtx);

  /* If this is a sqrt operation and we don't care about errno, try to
     attach a REG_EQUAL note with a SQRT rtx to the emitted libcall.
     This allows the semantics of the libcall to be visible to the RTL
     optimizers.  */
  if (builtin_optab == sqrt_optab && !errno_set)
    {
      /* Search backwards through the insns emitted by expand_call looking
	 for the instruction with the REG_RETVAL note.  */
      rtx last = get_last_insn ();
      while (last != before_call)
	{
	  if (find_reg_note (last, REG_RETVAL, NULL))
	    {
	      rtx note = find_reg_note (last, REG_EQUAL, NULL);
	      /* Check that the REQ_EQUAL note is an EXPR_LIST with
		 two elements, i.e. symbol_ref(sqrt) and the operand.  */
	      if (note
		  && GET_CODE (note) == EXPR_LIST
		  && GET_CODE (XEXP (note, 0)) == EXPR_LIST
		  && XEXP (XEXP (note, 0), 1) != NULL_RTX
		  && XEXP (XEXP (XEXP (note, 0), 1), 1) == NULL_RTX)
		{
		  rtx operand = XEXP (XEXP (XEXP (note, 0), 1), 0);
		  /* Check operand is a register with expected mode.  */
		  if (operand
		      && REG_P (operand)
		      && GET_MODE (operand) == mode)
		    {
		      /* Replace the REG_EQUAL note with a SQRT rtx.  */
		      rtx equiv = gen_rtx_SQRT (mode, operand);
		      set_unique_reg_note (last, REG_EQUAL, equiv);
		    }
		}
	      break;
	    }
	  last = PREV_INSN (last);
	}
    }

  return target;
}

/* Expand a call to the builtin binary math functions (pow and atan2).
   Return NULL_RTX if a normal call should be emitted rather than expanding the
   function in-line.  EXP is the expression that is a call to the builtin
   function; if convenient, the result should be placed in TARGET.
   SUBTARGET may be used as the target for computing one of EXP's
   operands.  */

static rtx
expand_builtin_mathfn_2 (tree exp, rtx target, rtx subtarget)
{
  optab builtin_optab;
  rtx op0, op1, insns;
  int op1_type = REAL_TYPE;
  tree fndecl = get_callee_fndecl (exp);
  tree arg0, arg1;
  enum machine_mode mode;
  bool errno_set = true;

  switch (DECL_FUNCTION_CODE (fndecl))
    {
    CASE_FLT_FN (BUILT_IN_SCALBN):
    CASE_FLT_FN (BUILT_IN_SCALBLN):
    CASE_FLT_FN (BUILT_IN_LDEXP):
      op1_type = INTEGER_TYPE;
    default:
      break;
    }

  if (!validate_arglist (exp, REAL_TYPE, op1_type, VOID_TYPE))
    return NULL_RTX;

  arg0 = CALL_EXPR_ARG (exp, 0);
  arg1 = CALL_EXPR_ARG (exp, 1);

  switch (DECL_FUNCTION_CODE (fndecl))
    {
    CASE_FLT_FN (BUILT_IN_POW):
      builtin_optab = pow_optab; break;
    CASE_FLT_FN (BUILT_IN_ATAN2):
      builtin_optab = atan2_optab; break;
    CASE_FLT_FN (BUILT_IN_SCALB):
      if (REAL_MODE_FORMAT (TYPE_MODE (TREE_TYPE (exp)))->b != 2)
	return 0;
      builtin_optab = scalb_optab; break;
    CASE_FLT_FN (BUILT_IN_SCALBN):
    CASE_FLT_FN (BUILT_IN_SCALBLN):
      if (REAL_MODE_FORMAT (TYPE_MODE (TREE_TYPE (exp)))->b != 2)
	return 0;
    /* Fall through... */
    CASE_FLT_FN (BUILT_IN_LDEXP):
      builtin_optab = ldexp_optab; break;
    CASE_FLT_FN (BUILT_IN_FMOD):
      builtin_optab = fmod_optab; break;
    CASE_FLT_FN (BUILT_IN_REMAINDER):
    CASE_FLT_FN (BUILT_IN_DREM):
      builtin_optab = remainder_optab; break;
    default:
      gcc_unreachable ();
    }

  /* Make a suitable register to place result in.  */
  mode = TYPE_MODE (TREE_TYPE (exp));

  /* Before working hard, check whether the instruction is available.  */
  if (optab_handler (builtin_optab, mode)->insn_code == CODE_FOR_nothing)
    return NULL_RTX;

  target = gen_reg_rtx (mode);

  if (! flag_errno_math || ! HONOR_NANS (mode))
    errno_set = false;

  /* Always stabilize the argument list.  */
  CALL_EXPR_ARG (exp, 0) = arg0 = builtin_save_expr (arg0);
  CALL_EXPR_ARG (exp, 1) = arg1 = builtin_save_expr (arg1);

  op0 = expand_expr (arg0, subtarget, VOIDmode, EXPAND_NORMAL);
  op1 = expand_normal (arg1);

  start_sequence ();

  /* Compute into TARGET.
     Set TARGET to wherever the result comes back.  */
  target = expand_binop (mode, builtin_optab, op0, op1,
			 target, 0, OPTAB_DIRECT);

  /* If we were unable to expand via the builtin, stop the sequence
     (without outputting the insns) and call to the library function
     with the stabilized argument list.  */
  if (target == 0)
    {
      end_sequence ();
      return expand_call (exp, target, target == const0_rtx);
    }

  if (errno_set)
    expand_errno_check (exp, target);

  /* Output the entire sequence.  */
  insns = get_insns ();
  end_sequence ();
  emit_insn (insns);

  return target;
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
  rtx op0, insns;
  tree fndecl = get_callee_fndecl (exp);
  enum machine_mode mode;
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
  if (optab_handler (builtin_optab, mode)->insn_code == CODE_FOR_nothing)
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
  if (optab_handler (builtin_optab, mode)->insn_code != CODE_FOR_nothing)
    {
      target = gen_reg_rtx (mode);

      /* Wrap the computation of the argument in a SAVE_EXPR, as we may
	 need to expand the argument again.  This way, we will not perform
	 side-effects more the once.  */
      CALL_EXPR_ARG (exp, 0) = arg = builtin_save_expr (arg);

      op0 = expand_expr (arg, subtarget, VOIDmode, EXPAND_NORMAL);

      start_sequence ();

      /* Compute into TARGET.
	 Set TARGET to wherever the result comes back.  */
      if (builtin_optab == sincos_optab)
	{
	  int result;

	  switch (DECL_FUNCTION_CODE (fndecl))
	    {
	    CASE_FLT_FN (BUILT_IN_SIN):
	      result = expand_twoval_unop (builtin_optab, op0, 0, target, 0);
	      break;
	    CASE_FLT_FN (BUILT_IN_COS):
	      result = expand_twoval_unop (builtin_optab, op0, target, 0, 0);
	      break;
	    default:
	      gcc_unreachable ();
	    }
	  gcc_assert (result);
	}
      else
	{
	  target = expand_unop (mode, builtin_optab, op0, target, 0);
	}

      if (target != 0)
	{
	  /* Output the entire sequence.  */
	  insns = get_insns ();
	  end_sequence ();
	  emit_insn (insns);
	  return target;
	}

      /* If we were unable to expand via the builtin, stop the sequence
	 (without outputting the insns) and call to the library function
	 with the stabilized argument list.  */
      end_sequence ();
    }

  target = expand_call (exp, target, target == const0_rtx);

  return target;
}

/* Expand a call to one of the builtin math functions that operate on
   floating point argument and output an integer result (ilogb, isinf,
   isnan, etc).
   Return 0 if a normal call should be emitted rather than expanding the
   function in-line.  EXP is the expression that is a call to the builtin
   function; if convenient, the result should be placed in TARGET.
   SUBTARGET may be used as the target for computing one of EXP's operands.  */

static rtx
expand_builtin_interclass_mathfn (tree exp, rtx target, rtx subtarget)
{
  optab builtin_optab = 0;
  enum insn_code icode = CODE_FOR_nothing;
  rtx op0;
  tree fndecl = get_callee_fndecl (exp);
  enum machine_mode mode;
  bool errno_set = false;
  tree arg;

  if (!validate_arglist (exp, REAL_TYPE, VOID_TYPE))
    return NULL_RTX;

  arg = CALL_EXPR_ARG (exp, 0);

  switch (DECL_FUNCTION_CODE (fndecl))
    {
    CASE_FLT_FN (BUILT_IN_ILOGB):
      errno_set = true; builtin_optab = ilogb_optab; break;
    CASE_FLT_FN (BUILT_IN_ISINF):
      builtin_optab = isinf_optab; break;
    case BUILT_IN_ISNORMAL:
    case BUILT_IN_ISFINITE:
    CASE_FLT_FN (BUILT_IN_FINITE):
      /* These builtins have no optabs (yet).  */
      break;
    default:
      gcc_unreachable ();
    }

  /* There's no easy way to detect the case we need to set EDOM.  */
  if (flag_errno_math && errno_set)
    return NULL_RTX;

  /* Optab mode depends on the mode of the input argument.  */
  mode = TYPE_MODE (TREE_TYPE (arg));

  if (builtin_optab)
    icode = optab_handler (builtin_optab, mode)->insn_code;
 
  /* Before working hard, check whether the instruction is available.  */
  if (icode != CODE_FOR_nothing)
    {
      /* Make a suitable register to place result in.  */
      if (!target
	  || GET_MODE (target) != TYPE_MODE (TREE_TYPE (exp)))
         target = gen_reg_rtx (TYPE_MODE (TREE_TYPE (exp)));

      gcc_assert (insn_data[icode].operand[0].predicate
		  (target, GET_MODE (target)));

      /* Wrap the computation of the argument in a SAVE_EXPR, as we may
	 need to expand the argument again.  This way, we will not perform
	 side-effects more the once.  */
      CALL_EXPR_ARG (exp, 0) = arg = builtin_save_expr (arg);

      op0 = expand_expr (arg, subtarget, VOIDmode, EXPAND_NORMAL);

      if (mode != GET_MODE (op0))
	op0 = convert_to_mode (mode, op0, 0);

      /* Compute into TARGET.
	 Set TARGET to wherever the result comes back.  */
      emit_unop_insn (icode, target, op0, UNKNOWN);
      return target;
    }

  /* If there is no optab, try generic code.  */
  switch (DECL_FUNCTION_CODE (fndecl))
    {
      tree result;

    CASE_FLT_FN (BUILT_IN_ISINF):
      {
	/* isinf(x) -> isgreater(fabs(x),DBL_MAX).  */
	tree const isgr_fn = built_in_decls[BUILT_IN_ISGREATER];
	tree const type = TREE_TYPE (arg);
	REAL_VALUE_TYPE r;
	char buf[128];

	get_max_float (REAL_MODE_FORMAT (mode), buf, sizeof (buf));
	real_from_string (&r, buf);
	result = build_call_expr (isgr_fn, 2,
				  fold_build1 (ABS_EXPR, type, arg),
				  build_real (type, r));
	return expand_expr (result, target, VOIDmode, EXPAND_NORMAL);
      }
    CASE_FLT_FN (BUILT_IN_FINITE):
    case BUILT_IN_ISFINITE:
      {
	/* isfinite(x) -> islessequal(fabs(x),DBL_MAX).  */
	tree const isle_fn = built_in_decls[BUILT_IN_ISLESSEQUAL];
	tree const type = TREE_TYPE (arg);
	REAL_VALUE_TYPE r;
	char buf[128];

	get_max_float (REAL_MODE_FORMAT (mode), buf, sizeof (buf));
	real_from_string (&r, buf);
	result = build_call_expr (isle_fn, 2,
				  fold_build1 (ABS_EXPR, type, arg),
				  build_real (type, r));
	return expand_expr (result, target, VOIDmode, EXPAND_NORMAL);
      }
    case BUILT_IN_ISNORMAL:
      {
	/* isnormal(x) -> isgreaterequal(fabs(x),DBL_MIN) &
	   islessequal(fabs(x),DBL_MAX).  */
	tree const isle_fn = built_in_decls[BUILT_IN_ISLESSEQUAL];
	tree const isge_fn = built_in_decls[BUILT_IN_ISGREATEREQUAL];
	tree const type = TREE_TYPE (arg);
	REAL_VALUE_TYPE rmax, rmin;
	char buf[128];

	get_max_float (REAL_MODE_FORMAT (mode), buf, sizeof (buf));
	real_from_string (&rmax, buf);
	sprintf (buf, "0x1p%d", REAL_MODE_FORMAT (mode)->emin - 1);
	real_from_string (&rmin, buf);
	arg = builtin_save_expr (fold_build1 (ABS_EXPR, type, arg));
	result = build_call_expr (isle_fn, 2, arg,
				  build_real (type, rmax));
	result = fold_build2 (BIT_AND_EXPR, integer_type_node, result,
			      build_call_expr (isge_fn, 2, arg,
					       build_real (type, rmin)));
	return expand_expr (result, target, VOIDmode, EXPAND_NORMAL);
      }
    default:
      break;
    }

  target = expand_call (exp, target, target == const0_rtx);

  return target;
}

/* Expand a call to the builtin sincos math function.
   Return NULL_RTX if a normal call should be emitted rather than expanding the
   function in-line.  EXP is the expression that is a call to the builtin
   function.  */

static rtx
expand_builtin_sincos (tree exp)
{
  rtx op0, op1, op2, target1, target2;
  enum machine_mode mode;
  tree arg, sinp, cosp;
  int result;

  if (!validate_arglist (exp, REAL_TYPE,
 			 POINTER_TYPE, POINTER_TYPE, VOID_TYPE))
    return NULL_RTX;

  arg = CALL_EXPR_ARG (exp, 0);
  sinp = CALL_EXPR_ARG (exp, 1);
  cosp = CALL_EXPR_ARG (exp, 2);

  /* Make a suitable register to place result in.  */
  mode = TYPE_MODE (TREE_TYPE (arg));

  /* Check if sincos insn is available, otherwise emit the call.  */
  if (optab_handler (sincos_optab, mode)->insn_code == CODE_FOR_nothing)
    return NULL_RTX;

  target1 = gen_reg_rtx (mode);
  target2 = gen_reg_rtx (mode);

  op0 = expand_normal (arg);
  op1 = expand_normal (build_fold_indirect_ref (sinp));
  op2 = expand_normal (build_fold_indirect_ref (cosp));

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
   the result should be placed in TARGET.  SUBTARGET may be used as the target
   for computing one of EXP's operands.  */

static rtx
expand_builtin_cexpi (tree exp, rtx target, rtx subtarget)
{
  tree fndecl = get_callee_fndecl (exp);
  tree arg, type;
  enum machine_mode mode;
  rtx op0, op1, op2;

  if (!validate_arglist (exp, REAL_TYPE, VOID_TYPE))
    return NULL_RTX;

  arg = CALL_EXPR_ARG (exp, 0);
  type = TREE_TYPE (arg);
  mode = TYPE_MODE (TREE_TYPE (arg));

  /* Try expanding via a sincos optab, fall back to emitting a libcall
     to sincos or cexp.  We are sure we have sincos or cexp because cexpi
     is only generated from sincos, cexp or if we have either of them.  */
  if (optab_handler (sincos_optab, mode)->insn_code != CODE_FOR_nothing)
    {
      op1 = gen_reg_rtx (mode);
      op2 = gen_reg_rtx (mode);

      op0 = expand_expr (arg, subtarget, VOIDmode, EXPAND_NORMAL);

      /* Compute into op1 and op2.  */
      expand_twoval_unop (sincos_optab, op0, op2, op1, 0);
    }
  else if (TARGET_HAS_SINCOS)
    {
      tree call, fn = NULL_TREE;
      tree top1, top2;
      rtx op1a, op2a;

      if (DECL_FUNCTION_CODE (fndecl) == BUILT_IN_CEXPIF)
	fn = built_in_decls[BUILT_IN_SINCOSF];
      else if (DECL_FUNCTION_CODE (fndecl) == BUILT_IN_CEXPI)
	fn = built_in_decls[BUILT_IN_SINCOS];
      else if (DECL_FUNCTION_CODE (fndecl) == BUILT_IN_CEXPIL)
	fn = built_in_decls[BUILT_IN_SINCOSL];
      else
	gcc_unreachable ();
 
      op1 = assign_temp (TREE_TYPE (arg), 0, 1, 1);
      op2 = assign_temp (TREE_TYPE (arg), 0, 1, 1);
      op1a = copy_to_mode_reg (Pmode, XEXP (op1, 0));
      op2a = copy_to_mode_reg (Pmode, XEXP (op2, 0));
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
	fn = built_in_decls[BUILT_IN_CEXPF];
      else if (DECL_FUNCTION_CODE (fndecl) == BUILT_IN_CEXPI)
	fn = built_in_decls[BUILT_IN_CEXP];
      else if (DECL_FUNCTION_CODE (fndecl) == BUILT_IN_CEXPIL)
	fn = built_in_decls[BUILT_IN_CEXPL];
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

      narg = fold_build2 (COMPLEX_EXPR, ctype,
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

/* Expand a call to one of the builtin rounding functions gcc defines
   as an extension (lfloor and lceil).  As these are gcc extensions we
   do not need to worry about setting errno to EDOM.
   If expanding via optab fails, lower expression to (int)(floor(x)).
   EXP is the expression that is a call to the builtin function;
   if convenient, the result should be placed in TARGET.  SUBTARGET may
   be used as the target for computing one of EXP's operands.  */

static rtx
expand_builtin_int_roundingfn (tree exp, rtx target, rtx subtarget)
{
  convert_optab builtin_optab;
  rtx op0, insns, tmp;
  tree fndecl = get_callee_fndecl (exp);
  enum built_in_function fallback_fn;
  tree fallback_fndecl;
  enum machine_mode mode;
  tree arg;

  if (!validate_arglist (exp, REAL_TYPE, VOID_TYPE))
    gcc_unreachable ();

  arg = CALL_EXPR_ARG (exp, 0);

  switch (DECL_FUNCTION_CODE (fndecl))
    {
    CASE_FLT_FN (BUILT_IN_LCEIL):
    CASE_FLT_FN (BUILT_IN_LLCEIL):
      builtin_optab = lceil_optab;
      fallback_fn = BUILT_IN_CEIL;
      break;

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

  op0 = expand_expr (arg, subtarget, VOIDmode, EXPAND_NORMAL);

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
	case BUILT_IN_LCEIL:
	case BUILT_IN_LLCEIL:
	  name = "ceil";
	  break;
	case BUILT_IN_LCEILF:
	case BUILT_IN_LLCEILF:
	  name = "ceilf";
	  break;
	case BUILT_IN_LCEILL:
	case BUILT_IN_LLCEILL:
	  name = "ceill";
	  break;
	case BUILT_IN_LFLOOR:
	case BUILT_IN_LLFLOOR:
	  name = "floor";
	  break;
	case BUILT_IN_LFLOORF:
	case BUILT_IN_LLFLOORF:
	  name = "floorf";
	  break;
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

  exp = build_call_expr (fallback_fndecl, 1, arg);

  tmp = expand_normal (exp);

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
   function; if convenient, the result should be placed in TARGET.
   SUBTARGET may be used as the target for computing one of EXP's operands.  */

static rtx
expand_builtin_int_roundingfn_2 (tree exp, rtx target, rtx subtarget)
{
  convert_optab builtin_optab;
  rtx op0, insns;
  tree fndecl = get_callee_fndecl (exp);
  tree arg;
  enum machine_mode mode;

  /* There's no easy way to detect the case we need to set EDOM.  */
  if (flag_errno_math)
    return NULL_RTX;

  if (!validate_arglist (exp, REAL_TYPE, VOID_TYPE))
     gcc_unreachable ();
 
  arg = CALL_EXPR_ARG (exp, 0);

  switch (DECL_FUNCTION_CODE (fndecl))
    {
    CASE_FLT_FN (BUILT_IN_LRINT):
    CASE_FLT_FN (BUILT_IN_LLRINT):
      builtin_optab = lrint_optab; break;
    CASE_FLT_FN (BUILT_IN_LROUND):
    CASE_FLT_FN (BUILT_IN_LLROUND):
      builtin_optab = lround_optab; break;
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

  op0 = expand_expr (arg, subtarget, VOIDmode, EXPAND_NORMAL);

  start_sequence ();

  if (expand_sfix_optab (target, op0, builtin_optab))
    {
      /* Output the entire sequence.  */
      insns = get_insns ();
      end_sequence ();
      emit_insn (insns);
      return target;
    }

  /* If we were unable to expand via the builtin, stop the sequence
     (without outputting the insns) and call to the library function
     with the stabilized argument list.  */
  end_sequence ();

  target = expand_call (exp, target, target == const0_rtx);

  return target;
}

/* To evaluate powi(x,n), the floating point value x raised to the
   constant integer exponent n, we use a hybrid algorithm that
   combines the "window method" with look-up tables.  For an
   introduction to exponentiation algorithms and "addition chains",
   see section 4.6.3, "Evaluation of Powers" of Donald E. Knuth,
   "Seminumerical Algorithms", Vol. 2, "The Art of Computer Programming",
   3rd Edition, 1998, and Daniel M. Gordon, "A Survey of Fast Exponentiation
   Methods", Journal of Algorithms, Vol. 27, pp. 129-146, 1998.  */

/* Provide a default value for POWI_MAX_MULTS, the maximum number of
   multiplications to inline before calling the system library's pow
   function.  powi(x,n) requires at worst 2*bits(n)-2 multiplications,
   so this default never requires calling pow, powf or powl.  */

#ifndef POWI_MAX_MULTS
#define POWI_MAX_MULTS  (2*HOST_BITS_PER_WIDE_INT-2)
#endif

/* The size of the "optimal power tree" lookup table.  All
   exponents less than this value are simply looked up in the
   powi_table below.  This threshold is also used to size the
   cache of pseudo registers that hold intermediate results.  */
#define POWI_TABLE_SIZE 256

/* The size, in bits of the window, used in the "window method"
   exponentiation algorithm.  This is equivalent to a radix of
   (1<<POWI_WINDOW_SIZE) in the corresponding "m-ary method".  */
#define POWI_WINDOW_SIZE 3

/* The following table is an efficient representation of an
   "optimal power tree".  For each value, i, the corresponding
   value, j, in the table states than an optimal evaluation
   sequence for calculating pow(x,i) can be found by evaluating
   pow(x,j)*pow(x,i-j).  An optimal power tree for the first
   100 integers is given in Knuth's "Seminumerical algorithms".  */

static const unsigned char powi_table[POWI_TABLE_SIZE] =
  {
      0,   1,   1,   2,   2,   3,   3,   4,  /*   0 -   7 */
      4,   6,   5,   6,   6,  10,   7,   9,  /*   8 -  15 */
      8,  16,   9,  16,  10,  12,  11,  13,  /*  16 -  23 */
     12,  17,  13,  18,  14,  24,  15,  26,  /*  24 -  31 */
     16,  17,  17,  19,  18,  33,  19,  26,  /*  32 -  39 */
     20,  25,  21,  40,  22,  27,  23,  44,  /*  40 -  47 */
     24,  32,  25,  34,  26,  29,  27,  44,  /*  48 -  55 */
     28,  31,  29,  34,  30,  60,  31,  36,  /*  56 -  63 */
     32,  64,  33,  34,  34,  46,  35,  37,  /*  64 -  71 */
     36,  65,  37,  50,  38,  48,  39,  69,  /*  72 -  79 */
     40,  49,  41,  43,  42,  51,  43,  58,  /*  80 -  87 */
     44,  64,  45,  47,  46,  59,  47,  76,  /*  88 -  95 */
     48,  65,  49,  66,  50,  67,  51,  66,  /*  96 - 103 */
     52,  70,  53,  74,  54, 104,  55,  74,  /* 104 - 111 */
     56,  64,  57,  69,  58,  78,  59,  68,  /* 112 - 119 */
     60,  61,  61,  80,  62,  75,  63,  68,  /* 120 - 127 */
     64,  65,  65, 128,  66, 129,  67,  90,  /* 128 - 135 */
     68,  73,  69, 131,  70,  94,  71,  88,  /* 136 - 143 */
     72, 128,  73,  98,  74, 132,  75, 121,  /* 144 - 151 */
     76, 102,  77, 124,  78, 132,  79, 106,  /* 152 - 159 */
     80,  97,  81, 160,  82,  99,  83, 134,  /* 160 - 167 */
     84,  86,  85,  95,  86, 160,  87, 100,  /* 168 - 175 */
     88, 113,  89,  98,  90, 107,  91, 122,  /* 176 - 183 */
     92, 111,  93, 102,  94, 126,  95, 150,  /* 184 - 191 */
     96, 128,  97, 130,  98, 133,  99, 195,  /* 192 - 199 */
    100, 128, 101, 123, 102, 164, 103, 138,  /* 200 - 207 */
    104, 145, 105, 146, 106, 109, 107, 149,  /* 208 - 215 */
    108, 200, 109, 146, 110, 170, 111, 157,  /* 216 - 223 */
    112, 128, 113, 130, 114, 182, 115, 132,  /* 224 - 231 */
    116, 200, 117, 132, 118, 158, 119, 206,  /* 232 - 239 */
    120, 240, 121, 162, 122, 147, 123, 152,  /* 240 - 247 */
    124, 166, 125, 214, 126, 138, 127, 153,  /* 248 - 255 */
  };


/* Return the number of multiplications required to calculate
   powi(x,n) where n is less than POWI_TABLE_SIZE.  This is a
   subroutine of powi_cost.  CACHE is an array indicating
   which exponents have already been calculated.  */

static int
powi_lookup_cost (unsigned HOST_WIDE_INT n, bool *cache)
{
  /* If we've already calculated this exponent, then this evaluation
     doesn't require any additional multiplications.  */
  if (cache[n])
    return 0;

  cache[n] = true;
  return powi_lookup_cost (n - powi_table[n], cache)
	 + powi_lookup_cost (powi_table[n], cache) + 1;
}

/* Return the number of multiplications required to calculate
   powi(x,n) for an arbitrary x, given the exponent N.  This
   function needs to be kept in sync with expand_powi below.  */

static int
powi_cost (HOST_WIDE_INT n)
{
  bool cache[POWI_TABLE_SIZE];
  unsigned HOST_WIDE_INT digit;
  unsigned HOST_WIDE_INT val;
  int result;

  if (n == 0)
    return 0;

  /* Ignore the reciprocal when calculating the cost.  */
  val = (n < 0) ? -n : n;

  /* Initialize the exponent cache.  */
  memset (cache, 0, POWI_TABLE_SIZE * sizeof (bool));
  cache[1] = true;

  result = 0;

  while (val >= POWI_TABLE_SIZE)
    {
      if (val & 1)
	{
	  digit = val & ((1 << POWI_WINDOW_SIZE) - 1);
	  result += powi_lookup_cost (digit, cache)
		    + POWI_WINDOW_SIZE + 1;
	  val >>= POWI_WINDOW_SIZE;
	}
      else
	{
	  val >>= 1;
	  result++;
	}
    }

  return result + powi_lookup_cost (val, cache);
}

/* Recursive subroutine of expand_powi.  This function takes the array,
   CACHE, of already calculated exponents and an exponent N and returns
   an RTX that corresponds to CACHE[1]**N, as calculated in mode MODE.  */

static rtx
expand_powi_1 (enum machine_mode mode, unsigned HOST_WIDE_INT n, rtx *cache)
{
  unsigned HOST_WIDE_INT digit;
  rtx target, result;
  rtx op0, op1;

  if (n < POWI_TABLE_SIZE)
    {
      if (cache[n])
	return cache[n];

      target = gen_reg_rtx (mode);
      cache[n] = target;

      op0 = expand_powi_1 (mode, n - powi_table[n], cache);
      op1 = expand_powi_1 (mode, powi_table[n], cache);
    }
  else if (n & 1)
    {
      target = gen_reg_rtx (mode);
      digit = n & ((1 << POWI_WINDOW_SIZE) - 1);
      op0 = expand_powi_1 (mode, n - digit, cache);
      op1 = expand_powi_1 (mode, digit, cache);
    }
  else
    {
      target = gen_reg_rtx (mode);
      op0 = expand_powi_1 (mode, n >> 1, cache);
      op1 = op0;
    }

  result = expand_mult (mode, op0, op1, target, 0);
  if (result != target)
    emit_move_insn (target, result);
  return target;
}

/* Expand the RTL to evaluate powi(x,n) in mode MODE.  X is the
   floating point operand in mode MODE, and N is the exponent.  This
   function needs to be kept in sync with powi_cost above.  */

static rtx
expand_powi (rtx x, enum machine_mode mode, HOST_WIDE_INT n)
{
  unsigned HOST_WIDE_INT val;
  rtx cache[POWI_TABLE_SIZE];
  rtx result;

  if (n == 0)
    return CONST1_RTX (mode);

  val = (n < 0) ? -n : n;

  memset (cache, 0, sizeof (cache));
  cache[1] = x;

  result = expand_powi_1 (mode, (n < 0) ? -n : n, cache);

  /* If the original exponent was negative, reciprocate the result.  */
  if (n < 0)
    result = expand_binop (mode, sdiv_optab, CONST1_RTX (mode),
			   result, NULL_RTX, 0, OPTAB_LIB_WIDEN);

  return result;
}

/* Expand a call to the pow built-in mathematical function.  Return NULL_RTX if
   a normal call should be emitted rather than expanding the function
   in-line.  EXP is the expression that is a call to the builtin
   function; if convenient, the result should be placed in TARGET.  */

static rtx
expand_builtin_pow (tree exp, rtx target, rtx subtarget)
{
  tree arg0, arg1;
  tree fn, narg0;
  tree type = TREE_TYPE (exp);
  REAL_VALUE_TYPE cint, c, c2;
  HOST_WIDE_INT n;
  rtx op, op2;
  enum machine_mode mode = TYPE_MODE (type);

  if (! validate_arglist (exp, REAL_TYPE, REAL_TYPE, VOID_TYPE))
    return NULL_RTX;

  arg0 = CALL_EXPR_ARG (exp, 0);
  arg1 = CALL_EXPR_ARG (exp, 1);

  if (TREE_CODE (arg1) != REAL_CST
      || TREE_OVERFLOW (arg1))
    return expand_builtin_mathfn_2 (exp, target, subtarget);

  /* Handle constant exponents.  */

  /* For integer valued exponents we can expand to an optimal multiplication
     sequence using expand_powi.  */
  c = TREE_REAL_CST (arg1);
  n = real_to_integer (&c);
  real_from_integer (&cint, VOIDmode, n, n < 0 ? -1 : 0, 0);
  if (real_identical (&c, &cint)
      && ((n >= -1 && n <= 2)
	  || (flag_unsafe_math_optimizations
	      && !optimize_size
	      && powi_cost (n) <= POWI_MAX_MULTS)))
    {
      op = expand_expr (arg0, subtarget, VOIDmode, EXPAND_NORMAL);
      if (n != 1)
	{
	  op = force_reg (mode, op);
	  op = expand_powi (op, mode, n);
	}
      return op;
    }

  narg0 = builtin_save_expr (arg0);

  /* If the exponent is not integer valued, check if it is half of an integer.
     In this case we can expand to sqrt (x) * x**(n/2).  */
  fn = mathfn_built_in (type, BUILT_IN_SQRT);
  if (fn != NULL_TREE)
    {
      real_arithmetic (&c2, MULT_EXPR, &c, &dconst2);
      n = real_to_integer (&c2);
      real_from_integer (&cint, VOIDmode, n, n < 0 ? -1 : 0, 0);
      if (real_identical (&c2, &cint)
	  && ((flag_unsafe_math_optimizations
	       && !optimize_size
	       && powi_cost (n/2) <= POWI_MAX_MULTS)
	      || n == 1))
	{
	  tree call_expr = build_call_expr (fn, 1, narg0);
	  /* Use expand_expr in case the newly built call expression
	     was folded to a non-call.  */
	  op = expand_expr (call_expr, subtarget, mode, EXPAND_NORMAL);
	  if (n != 1)
	    {
	      op2 = expand_expr (narg0, subtarget, VOIDmode, EXPAND_NORMAL);
	      op2 = force_reg (mode, op2);
	      op2 = expand_powi (op2, mode, abs (n / 2));
	      op = expand_simple_binop (mode, MULT, op, op2, NULL_RTX,
					0, OPTAB_LIB_WIDEN);
	      /* If the original exponent was negative, reciprocate the
		 result.  */
	      if (n < 0)
		op = expand_binop (mode, sdiv_optab, CONST1_RTX (mode),
				   op, NULL_RTX, 0, OPTAB_LIB_WIDEN);
	    }
	  return op;
	}
    }

  /* Try if the exponent is a third of an integer.  In this case
     we can expand to x**(n/3) * cbrt(x)**(n%3).  As cbrt (x) is
     different from pow (x, 1./3.) due to rounding and behavior
     with negative x we need to constrain this transformation to
     unsafe math and positive x or finite math.  */
  fn = mathfn_built_in (type, BUILT_IN_CBRT);
  if (fn != NULL_TREE
      && flag_unsafe_math_optimizations
      && (tree_expr_nonnegative_p (arg0)
	  || !HONOR_NANS (mode)))
    {
      real_arithmetic (&c2, MULT_EXPR, &c, &dconst3);
      real_round (&c2, mode, &c2);
      n = real_to_integer (&c2);
      real_from_integer (&cint, VOIDmode, n, n < 0 ? -1 : 0, 0);
      real_arithmetic (&c2, RDIV_EXPR, &cint, &dconst3);
      real_convert (&c2, mode, &c2);
      if (real_identical (&c2, &c)
	  && ((!optimize_size
	       && powi_cost (n/3) <= POWI_MAX_MULTS)
	      || n == 1))
	{
	  tree call_expr = build_call_expr (fn, 1,narg0);
	  op = expand_builtin (call_expr, NULL_RTX, subtarget, mode, 0);
	  if (abs (n) % 3 == 2)
	    op = expand_simple_binop (mode, MULT, op, op, op,
				      0, OPTAB_LIB_WIDEN);
	  if (n != 1)
	    {
	      op2 = expand_expr (narg0, subtarget, VOIDmode, EXPAND_NORMAL);
	      op2 = force_reg (mode, op2);
	      op2 = expand_powi (op2, mode, abs (n / 3));
	      op = expand_simple_binop (mode, MULT, op, op2, NULL_RTX,
					0, OPTAB_LIB_WIDEN);
	      /* If the original exponent was negative, reciprocate the
		 result.  */
	      if (n < 0)
		op = expand_binop (mode, sdiv_optab, CONST1_RTX (mode),
				   op, NULL_RTX, 0, OPTAB_LIB_WIDEN);
	    }
	  return op;
	}
    }

  /* Fall back to optab expansion.  */
  return expand_builtin_mathfn_2 (exp, target, subtarget);
}

/* Expand a call to the powi built-in mathematical function.  Return NULL_RTX if
   a normal call should be emitted rather than expanding the function
   in-line.  EXP is the expression that is a call to the builtin
   function; if convenient, the result should be placed in TARGET.  */

static rtx
expand_builtin_powi (tree exp, rtx target, rtx subtarget)
{
  tree arg0, arg1;
  rtx op0, op1;
  enum machine_mode mode;
  enum machine_mode mode2;

  if (! validate_arglist (exp, REAL_TYPE, INTEGER_TYPE, VOID_TYPE))
    return NULL_RTX;

  arg0 = CALL_EXPR_ARG (exp, 0);
  arg1 = CALL_EXPR_ARG (exp, 1);
  mode = TYPE_MODE (TREE_TYPE (exp));

  /* Handle constant power.  */

  if (TREE_CODE (arg1) == INTEGER_CST
      && !TREE_OVERFLOW (arg1))
    {
      HOST_WIDE_INT n = TREE_INT_CST_LOW (arg1);

      /* If the exponent is -1, 0, 1 or 2, then expand_powi is exact.
	 Otherwise, check the number of multiplications required.  */
      if ((TREE_INT_CST_HIGH (arg1) == 0
	   || TREE_INT_CST_HIGH (arg1) == -1)
	  && ((n >= -1 && n <= 2)
	      || (! optimize_size
		  && powi_cost (n) <= POWI_MAX_MULTS)))
	{
	  op0 = expand_expr (arg0, subtarget, VOIDmode, EXPAND_NORMAL);
	  op0 = force_reg (mode, op0);
	  return expand_powi (op0, mode, n);
	}
    }

  /* Emit a libcall to libgcc.  */

  /* Mode of the 2nd argument must match that of an int.  */
  mode2 = mode_for_size (INT_TYPE_SIZE, MODE_INT, 0);

  if (target == NULL_RTX)
    target = gen_reg_rtx (mode);

  op0 = expand_expr (arg0, subtarget, mode, EXPAND_NORMAL);
  if (GET_MODE (op0) != mode)
    op0 = convert_to_mode (mode, op0, 0);
  op1 = expand_expr (arg1, NULL_RTX, mode2, EXPAND_NORMAL);
  if (GET_MODE (op1) != mode2)
    op1 = convert_to_mode (mode2, op1, 0);

  target = emit_library_call_value (optab_libfunc (powi_optab, mode),
				    target, LCT_CONST_MAKE_BLOCK, mode, 2,
				    op0, mode, op1, mode2);

  return target;
}

/* Expand expression EXP which is a call to the strlen builtin.  Return 
   NULL_RTX if we failed the caller should emit a normal call, otherwise
   try to get the result in TARGET, if convenient.  */

static rtx
expand_builtin_strlen (tree exp, rtx target,
		       enum machine_mode target_mode)
{
  if (!validate_arglist (exp, POINTER_TYPE, VOID_TYPE))
    return NULL_RTX;
  else
    {
      rtx pat;
      tree len;
      tree src = CALL_EXPR_ARG (exp, 0);
      rtx result, src_reg, char_rtx, before_strlen;
      enum machine_mode insn_mode = target_mode, char_mode;
      enum insn_code icode = CODE_FOR_nothing;
      int align;

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

      align = get_pointer_alignment (src, BIGGEST_ALIGNMENT) / BITS_PER_UNIT;

      /* If SRC is not a pointer type, don't do this operation inline.  */
      if (align == 0)
	return NULL_RTX;

      /* Bail out if we can't compute strlen in the right mode.  */
      while (insn_mode != VOIDmode)
	{
	  icode = optab_handler (strlen_optab, insn_mode)->insn_code;
	  if (icode != CODE_FOR_nothing)
	    break;

	  insn_mode = GET_MODE_WIDER_MODE (insn_mode);
	}
      if (insn_mode == VOIDmode)
	return NULL_RTX;

      /* Make a place to write the result of the instruction.  */
      result = target;
      if (! (result != 0
	     && REG_P (result)
	     && GET_MODE (result) == insn_mode
	     && REGNO (result) >= FIRST_PSEUDO_REGISTER))
	result = gen_reg_rtx (insn_mode);

      /* Make a place to hold the source address.  We will not expand
	 the actual source until we are sure that the expansion will
	 not fail -- there are trees that cannot be expanded twice.  */
      src_reg = gen_reg_rtx (Pmode);

      /* Mark the beginning of the strlen sequence so we can emit the
	 source operand later.  */
      before_strlen = get_last_insn ();

      char_rtx = const0_rtx;
      char_mode = insn_data[(int) icode].operand[2].mode;
      if (! (*insn_data[(int) icode].operand[2].predicate) (char_rtx,
							    char_mode))
	char_rtx = copy_to_mode_reg (char_mode, char_rtx);

      pat = GEN_FCN (icode) (result, gen_rtx_MEM (BLKmode, src_reg),
			     char_rtx, GEN_INT (align));
      if (! pat)
	return NULL_RTX;
      emit_insn (pat);

      /* Now that we are assured of success, expand the source.  */
      start_sequence ();
      pat = expand_expr (src, src_reg, ptr_mode, EXPAND_NORMAL);
      if (pat != src_reg)
	emit_move_insn (src_reg, pat);
      pat = get_insns ();
      end_sequence ();

      if (before_strlen)
	emit_insn_after (pat, before_strlen);
      else
	emit_insn_before (pat, get_insns ());

      /* Return the value in the proper mode for this function.  */
      if (GET_MODE (result) == target_mode)
	target = result;
      else if (target != 0)
	convert_move (target, result, 0);
      else
	target = convert_to_mode (target_mode, result, 0);

      return target;
    }
}

/* Expand a call to the strstr builtin.  Return NULL_RTX if we failed the
   caller should emit a normal call, otherwise try to get the result
   in TARGET, if convenient (and in mode MODE if that's convenient).  */

static rtx
expand_builtin_strstr (tree exp, rtx target, enum machine_mode mode)
{
  if (validate_arglist (exp, POINTER_TYPE, POINTER_TYPE, VOID_TYPE))
    {
      tree type = TREE_TYPE (exp);
      tree result = fold_builtin_strstr (CALL_EXPR_ARG (exp, 0),
					 CALL_EXPR_ARG (exp, 1), type);
      if (result)
	return expand_expr (result, target, mode, EXPAND_NORMAL);
    }
  return NULL_RTX;
}

/* Expand a call to the strchr builtin.  Return NULL_RTX if we failed the
   caller should emit a normal call, otherwise try to get the result
   in TARGET, if convenient (and in mode MODE if that's convenient).  */

static rtx
expand_builtin_strchr (tree exp, rtx target, enum machine_mode mode)
{
  if (validate_arglist (exp, POINTER_TYPE, INTEGER_TYPE, VOID_TYPE))
    {
      tree type = TREE_TYPE (exp);
      tree result = fold_builtin_strchr (CALL_EXPR_ARG (exp, 0),
					 CALL_EXPR_ARG (exp, 1), type);
      if (result)
	return expand_expr (result, target, mode, EXPAND_NORMAL);

      /* FIXME: Should use strchrM optab so that ports can optimize this.  */
    }
  return NULL_RTX;
}

/* Expand a call to the strrchr builtin.  Return NULL_RTX if we failed the
   caller should emit a normal call, otherwise try to get the result
   in TARGET, if convenient (and in mode MODE if that's convenient).  */

static rtx
expand_builtin_strrchr (tree exp, rtx target, enum machine_mode mode)
{
  if (validate_arglist (exp, POINTER_TYPE, INTEGER_TYPE, VOID_TYPE))
    {
      tree type = TREE_TYPE (exp);
      tree result = fold_builtin_strrchr (CALL_EXPR_ARG (exp, 0),
					  CALL_EXPR_ARG (exp, 1), type);
      if (result)
	return expand_expr (result, target, mode, EXPAND_NORMAL);
    }
  return NULL_RTX;
}

/* Expand a call to the strpbrk builtin.  Return NULL_RTX if we failed the
   caller should emit a normal call, otherwise try to get the result
   in TARGET, if convenient (and in mode MODE if that's convenient).  */

static rtx
expand_builtin_strpbrk (tree exp, rtx target, enum machine_mode mode)
{
  if (validate_arglist (exp, POINTER_TYPE, POINTER_TYPE, VOID_TYPE))
    {
      tree type = TREE_TYPE (exp);
      tree result = fold_builtin_strpbrk (CALL_EXPR_ARG (exp, 0),
					  CALL_EXPR_ARG (exp, 1), type);
      if (result)
	return expand_expr (result, target, mode, EXPAND_NORMAL);
    }
  return NULL_RTX;
}

/* Callback routine for store_by_pieces.  Read GET_MODE_BITSIZE (MODE)
   bytes from constant string DATA + OFFSET and return it as target
   constant.  */

static rtx
builtin_memcpy_read_str (void *data, HOST_WIDE_INT offset,
			 enum machine_mode mode)
{
  const char *str = (const char *) data;

  gcc_assert (offset >= 0
	      && ((unsigned HOST_WIDE_INT) offset + GET_MODE_SIZE (mode)
		  <= strlen (str) + 1));

  return c_readstr (str + offset, mode);
}

/* Expand a call EXP to the memcpy builtin.
   Return NULL_RTX if we failed, the caller should emit a normal call,
   otherwise try to get the result in TARGET, if convenient (and in
   mode MODE if that's convenient).  */

static rtx
expand_builtin_memcpy (tree exp, rtx target, enum machine_mode mode)
{
  tree fndecl = get_callee_fndecl (exp);

  if (!validate_arglist (exp,
 			 POINTER_TYPE, POINTER_TYPE, INTEGER_TYPE, VOID_TYPE))
    return NULL_RTX;
  else
    {
      tree dest = CALL_EXPR_ARG (exp, 0);
      tree src = CALL_EXPR_ARG (exp, 1);
      tree len = CALL_EXPR_ARG (exp, 2);
      const char *src_str;
      unsigned int src_align = get_pointer_alignment (src, BIGGEST_ALIGNMENT);
      unsigned int dest_align
	= get_pointer_alignment (dest, BIGGEST_ALIGNMENT);
      rtx dest_mem, src_mem, dest_addr, len_rtx;
      tree result = fold_builtin_memory_op (dest, src, len, 
					    TREE_TYPE (TREE_TYPE (fndecl)),
					    false, /*endp=*/0);
      HOST_WIDE_INT expected_size = -1;
      unsigned int expected_align = 0;

      if (result)
	{
	  while (TREE_CODE (result) == COMPOUND_EXPR)
	    {
	      expand_expr (TREE_OPERAND (result, 0), const0_rtx, VOIDmode,
			   EXPAND_NORMAL);
	      result = TREE_OPERAND (result, 1);
	    }
	  return expand_expr (result, target, mode, EXPAND_NORMAL);
	}

      /* If DEST is not a pointer type, call the normal function.  */
      if (dest_align == 0)
	return NULL_RTX;

      /* If either SRC is not a pointer type, don't do this
	 operation in-line.  */
      if (src_align == 0)
	return NULL_RTX;
 
      stringop_block_profile (exp, &expected_align, &expected_size);
      if (expected_align < dest_align)
	expected_align = dest_align;
      dest_mem = get_memory_rtx (dest, len);
      set_mem_align (dest_mem, dest_align);
      len_rtx = expand_normal (len);
      src_str = c_getstr (src);

      /* If SRC is a string constant and block move would be done
	 by pieces, we can avoid loading the string from memory
	 and only stored the computed constants.  */
      if (src_str
	  && GET_CODE (len_rtx) == CONST_INT
	  && (unsigned HOST_WIDE_INT) INTVAL (len_rtx) <= strlen (src_str) + 1
	  && can_store_by_pieces (INTVAL (len_rtx), builtin_memcpy_read_str,
				  (void *) src_str, dest_align, false))
	{
	  dest_mem = store_by_pieces (dest_mem, INTVAL (len_rtx),
				      builtin_memcpy_read_str,
				      (void *) src_str, dest_align, false, 0);
	  dest_mem = force_operand (XEXP (dest_mem, 0), NULL_RTX);
	  dest_mem = convert_memory_address (ptr_mode, dest_mem);
	  return dest_mem;
	}

      src_mem = get_memory_rtx (src, len);
      set_mem_align (src_mem, src_align);

      /* Copy word part most expediently.  */
      dest_addr = emit_block_move_hints (dest_mem, src_mem, len_rtx,
				         CALL_EXPR_TAILCALL (exp)
				         ? BLOCK_OP_TAILCALL : BLOCK_OP_NORMAL,
					 expected_align, expected_size);

      if (dest_addr == 0)
	{
	  dest_addr = force_operand (XEXP (dest_mem, 0), NULL_RTX);
	  dest_addr = convert_memory_address (ptr_mode, dest_addr);
	}
      return dest_addr;
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
expand_builtin_mempcpy(tree exp, rtx target, enum machine_mode mode)
{
  if (!validate_arglist (exp,
 			 POINTER_TYPE, POINTER_TYPE, INTEGER_TYPE, VOID_TYPE))
    return NULL_RTX;
  else
    {
      tree dest = CALL_EXPR_ARG (exp, 0);
      tree src = CALL_EXPR_ARG (exp, 1);
      tree len = CALL_EXPR_ARG (exp, 2);
      return expand_builtin_mempcpy_args (dest, src, len,
					  TREE_TYPE (exp),
					  target, mode, /*endp=*/ 1);
    }
}

/* Helper function to do the actual work for expand_builtin_mempcpy.  The
   arguments to the builtin_mempcpy call DEST, SRC, and LEN are broken out
   so that this can also be called without constructing an actual CALL_EXPR.
   TYPE is the return type of the call.  The other arguments and return value
   are the same as for expand_builtin_mempcpy.  */

static rtx
expand_builtin_mempcpy_args (tree dest, tree src, tree len, tree type,
			     rtx target, enum machine_mode mode, int endp)
{
    /* If return value is ignored, transform mempcpy into memcpy.  */
  if (target == const0_rtx)
    {
      tree fn = implicit_built_in_decls[BUILT_IN_MEMCPY];

      if (!fn)
	return NULL_RTX;

      return expand_expr (build_call_expr (fn, 3, dest, src, len),
			  target, mode, EXPAND_NORMAL);
    }
  else
    {
      const char *src_str;
      unsigned int src_align = get_pointer_alignment (src, BIGGEST_ALIGNMENT);
      unsigned int dest_align
	= get_pointer_alignment (dest, BIGGEST_ALIGNMENT);
      rtx dest_mem, src_mem, len_rtx;
      tree result = fold_builtin_memory_op (dest, src, len, type, false, endp);

      if (result)
	{
	  while (TREE_CODE (result) == COMPOUND_EXPR)
	    {
	      expand_expr (TREE_OPERAND (result, 0), const0_rtx, VOIDmode,
			   EXPAND_NORMAL);
	      result = TREE_OPERAND (result, 1);
	    }
	  return expand_expr (result, target, mode, EXPAND_NORMAL);
	}

      /* If either SRC or DEST is not a pointer type, don't do this
	 operation in-line.  */
      if (dest_align == 0 || src_align == 0)
	return NULL_RTX;

      /* If LEN is not constant, call the normal function.  */
      if (! host_integerp (len, 1))
	return NULL_RTX;

      len_rtx = expand_normal (len);
      src_str = c_getstr (src);

      /* If SRC is a string constant and block move would be done
	 by pieces, we can avoid loading the string from memory
	 and only stored the computed constants.  */
      if (src_str
	  && GET_CODE (len_rtx) == CONST_INT
	  && (unsigned HOST_WIDE_INT) INTVAL (len_rtx) <= strlen (src_str) + 1
	  && can_store_by_pieces (INTVAL (len_rtx), builtin_memcpy_read_str,
				  (void *) src_str, dest_align, false))
	{
	  dest_mem = get_memory_rtx (dest, len);
	  set_mem_align (dest_mem, dest_align);
	  dest_mem = store_by_pieces (dest_mem, INTVAL (len_rtx),
				      builtin_memcpy_read_str,
				      (void *) src_str, dest_align,
				      false, endp);
	  dest_mem = force_operand (XEXP (dest_mem, 0), NULL_RTX);
	  dest_mem = convert_memory_address (ptr_mode, dest_mem);
	  return dest_mem;
	}

      if (GET_CODE (len_rtx) == CONST_INT
	  && can_move_by_pieces (INTVAL (len_rtx),
				 MIN (dest_align, src_align)))
	{
	  dest_mem = get_memory_rtx (dest, len);
	  set_mem_align (dest_mem, dest_align);
	  src_mem = get_memory_rtx (src, len);
	  set_mem_align (src_mem, src_align);
	  dest_mem = move_by_pieces (dest_mem, src_mem, INTVAL (len_rtx),
				     MIN (dest_align, src_align), endp);
	  dest_mem = force_operand (XEXP (dest_mem, 0), NULL_RTX);
	  dest_mem = convert_memory_address (ptr_mode, dest_mem);
	  return dest_mem;
	}

      return NULL_RTX;
    }
}

/* Expand expression EXP, which is a call to the memmove builtin.  Return 
   NULL_RTX if we failed; the caller should emit a normal call.  */

static rtx
expand_builtin_memmove (tree exp, rtx target, enum machine_mode mode, int ignore)
{
  if (!validate_arglist (exp,
 			 POINTER_TYPE, POINTER_TYPE, INTEGER_TYPE, VOID_TYPE))
    return NULL_RTX;
  else
    {
      tree dest = CALL_EXPR_ARG (exp, 0);
      tree src = CALL_EXPR_ARG (exp, 1);
      tree len = CALL_EXPR_ARG (exp, 2);
      return expand_builtin_memmove_args (dest, src, len, TREE_TYPE (exp), 
					  target, mode, ignore);
    }
}

/* Helper function to do the actual work for expand_builtin_memmove.  The
   arguments to the builtin_memmove call DEST, SRC, and LEN are broken out
   so that this can also be called without constructing an actual CALL_EXPR.
   TYPE is the return type of the call.  The other arguments and return value
   are the same as for expand_builtin_memmove.  */

static rtx
expand_builtin_memmove_args (tree dest, tree src, tree len,
			     tree type, rtx target, enum machine_mode mode, 
                             int ignore)
{
  tree result = fold_builtin_memory_op (dest, src, len, type, ignore, /*endp=*/3);

  if (result)
    {
      STRIP_TYPE_NOPS (result);
      while (TREE_CODE (result) == COMPOUND_EXPR)
	{
	  expand_expr (TREE_OPERAND (result, 0), const0_rtx, VOIDmode,
		       EXPAND_NORMAL);
	  result = TREE_OPERAND (result, 1);
	}
      return expand_expr (result, target, mode, EXPAND_NORMAL);
    }
  
  /* Otherwise, call the normal function.  */
  return NULL_RTX;
}

/* Expand expression EXP, which is a call to the bcopy builtin.  Return 
   NULL_RTX if we failed the caller should emit a normal call.  */

static rtx
expand_builtin_bcopy (tree exp, int ignore)
{
  tree type = TREE_TYPE (exp);
  tree src, dest, size;

  if (!validate_arglist (exp,
 			 POINTER_TYPE, POINTER_TYPE, INTEGER_TYPE, VOID_TYPE))
    return NULL_RTX;

  src = CALL_EXPR_ARG (exp, 0);
  dest = CALL_EXPR_ARG (exp, 1);
  size = CALL_EXPR_ARG (exp, 2);

  /* Transform bcopy(ptr x, ptr y, int z) to memmove(ptr y, ptr x, size_t z).
     This is done this way so that if it isn't expanded inline, we fall
     back to calling bcopy instead of memmove.  */
  return expand_builtin_memmove_args (dest, src,
 				      fold_convert (sizetype, size),
 				      type, const0_rtx, VOIDmode, 
				      ignore);
}

#ifndef HAVE_movstr
# define HAVE_movstr 0
# define CODE_FOR_movstr CODE_FOR_nothing
#endif

/* Expand into a movstr instruction, if one is available.  Return NULL_RTX if
   we failed, the caller should emit a normal call, otherwise try to
   get the result in TARGET, if convenient.  If ENDP is 0 return the
   destination pointer, if ENDP is 1 return the end pointer ala
   mempcpy, and if ENDP is 2 return the end pointer minus one ala
   stpcpy.  */

static rtx
expand_movstr (tree dest, tree src, rtx target, int endp)
{
  rtx end;
  rtx dest_mem;
  rtx src_mem;
  rtx insn;
  const struct insn_data * data;

  if (!HAVE_movstr)
    return NULL_RTX;

  dest_mem = get_memory_rtx (dest, NULL);
  src_mem = get_memory_rtx (src, NULL);
  if (!endp)
    {
      target = force_reg (Pmode, XEXP (dest_mem, 0));
      dest_mem = replace_equiv_address (dest_mem, target);
      end = gen_reg_rtx (Pmode);
    }
  else
    {
      if (target == 0 || target == const0_rtx)
	{
	  end = gen_reg_rtx (Pmode);
	  if (target == 0)
	    target = end;
	}
      else
	end = target;
    }

  data = insn_data + CODE_FOR_movstr;

  if (data->operand[0].mode != VOIDmode)
    end = gen_lowpart (data->operand[0].mode, end);

  insn = data->genfun (end, dest_mem, src_mem);

  gcc_assert (insn);

  emit_insn (insn);

  /* movstr is supposed to set end to the address of the NUL
     terminator.  If the caller requested a mempcpy-like return value,
     adjust it.  */
  if (endp == 1 && target != const0_rtx)
    {
      rtx tem = plus_constant (gen_lowpart (GET_MODE (target), end), 1);
      emit_move_insn (target, force_operand (tem, NULL_RTX));
    }

  return target;
}

/* Expand expression EXP, which is a call to the strcpy builtin.  Return 
   NULL_RTX if we failed the caller should emit a normal call, otherwise 
   try to get the result in TARGET, if convenient (and in mode MODE if that's
   convenient).  */

static rtx
expand_builtin_strcpy (tree fndecl, tree exp, rtx target, enum machine_mode mode)
{
  if (validate_arglist (exp, POINTER_TYPE, POINTER_TYPE, VOID_TYPE))
   {
     tree dest = CALL_EXPR_ARG (exp, 0);
     tree src = CALL_EXPR_ARG (exp, 1);
     return expand_builtin_strcpy_args (fndecl, dest, src, target, mode);
   }
   return NULL_RTX;
}

/* Helper function to do the actual work for expand_builtin_strcpy.  The
   arguments to the builtin_strcpy call DEST and SRC are broken out
   so that this can also be called without constructing an actual CALL_EXPR.
   The other arguments and return value are the same as for
   expand_builtin_strcpy.  */

static rtx
expand_builtin_strcpy_args (tree fndecl, tree dest, tree src,
			    rtx target, enum machine_mode mode)
{
  tree result = fold_builtin_strcpy (fndecl, dest, src, 0);
  if (result)
    return expand_expr (result, target, mode, EXPAND_NORMAL);
  return expand_movstr (dest, src, target, /*endp=*/0);

}

/* Expand a call EXP to the stpcpy builtin.
   Return NULL_RTX if we failed the caller should emit a normal call,
   otherwise try to get the result in TARGET, if convenient (and in
   mode MODE if that's convenient).  */

static rtx
expand_builtin_stpcpy (tree exp, rtx target, enum machine_mode mode)
{
  tree dst, src;

  if (!validate_arglist (exp, POINTER_TYPE, POINTER_TYPE, VOID_TYPE))
    return NULL_RTX;

  dst = CALL_EXPR_ARG (exp, 0);
  src = CALL_EXPR_ARG (exp, 1);

  /* If return value is ignored, transform stpcpy into strcpy.  */
  if (target == const0_rtx)
    {
      tree fn = implicit_built_in_decls[BUILT_IN_STRCPY];
      if (!fn)
	return NULL_RTX;

      return expand_expr (build_call_expr (fn, 2, dst, src),
  			  target, mode, EXPAND_NORMAL);
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

      lenp1 = size_binop (PLUS_EXPR, len, ssize_int (1));
      ret = expand_builtin_mempcpy_args (dst, src, lenp1, TREE_TYPE (exp),
 					 target, mode, /*endp=*/2);

      if (ret)
	return ret;

      if (TREE_CODE (len) == INTEGER_CST)
	{
	  rtx len_rtx = expand_normal (len);

	  if (GET_CODE (len_rtx) == CONST_INT)
	    {
	      ret = expand_builtin_strcpy_args (get_callee_fndecl (exp),
						dst, src, target, mode);

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

		  ret = plus_constant (ret, INTVAL (len_rtx));
		  ret = emit_move_insn (target, force_operand (ret, NULL_RTX));
		  gcc_assert (ret);

		  return target;
		}
	    }
	}

      return expand_movstr (dst, src, target, /*endp=*/2);
    }
}

/* Callback routine for store_by_pieces.  Read GET_MODE_BITSIZE (MODE)
   bytes from constant string DATA + OFFSET and return it as target
   constant.  */

rtx
builtin_strncpy_read_str (void *data, HOST_WIDE_INT offset,
			  enum machine_mode mode)
{
  const char *str = (const char *) data;

  if ((unsigned HOST_WIDE_INT) offset > strlen (str))
    return const0_rtx;

  return c_readstr (str + offset, mode);
}

/* Expand expression EXP, which is a call to the strncpy builtin.  Return 
   NULL_RTX if we failed the caller should emit a normal call.  */

static rtx
expand_builtin_strncpy (tree exp, rtx target, enum machine_mode mode)
{
  tree fndecl = get_callee_fndecl (exp);

  if (validate_arglist (exp,
 			POINTER_TYPE, POINTER_TYPE, INTEGER_TYPE, VOID_TYPE))
    {
      tree dest = CALL_EXPR_ARG (exp, 0);
      tree src = CALL_EXPR_ARG (exp, 1);
      tree len = CALL_EXPR_ARG (exp, 2);
      tree slen = c_strlen (src, 1);
      tree result = fold_builtin_strncpy (fndecl, dest, src, len, slen);

      if (result)
	{
	  while (TREE_CODE (result) == COMPOUND_EXPR)
	    {
	      expand_expr (TREE_OPERAND (result, 0), const0_rtx, VOIDmode,
			   EXPAND_NORMAL);
	      result = TREE_OPERAND (result, 1);
	    }
	  return expand_expr (result, target, mode, EXPAND_NORMAL);
	}

      /* We must be passed a constant len and src parameter.  */
      if (!host_integerp (len, 1) || !slen || !host_integerp (slen, 1))
	return NULL_RTX;

      slen = size_binop (PLUS_EXPR, slen, ssize_int (1));

      /* We're required to pad with trailing zeros if the requested
	 len is greater than strlen(s2)+1.  In that case try to
	 use store_by_pieces, if it fails, punt.  */
      if (tree_int_cst_lt (slen, len))
	{
	  unsigned int dest_align
	    = get_pointer_alignment (dest, BIGGEST_ALIGNMENT);
	  const char *p = c_getstr (src);
	  rtx dest_mem;

	  if (!p || dest_align == 0 || !host_integerp (len, 1)
	      || !can_store_by_pieces (tree_low_cst (len, 1),
				       builtin_strncpy_read_str,
				       (void *) p, dest_align, false))
	    return NULL_RTX;

	  dest_mem = get_memory_rtx (dest, len);
	  store_by_pieces (dest_mem, tree_low_cst (len, 1),
			   builtin_strncpy_read_str,
			   (void *) p, dest_align, false, 0);
	  dest_mem = force_operand (XEXP (dest_mem, 0), NULL_RTX);
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
			 enum machine_mode mode)
{
  const char *c = (const char *) data;
  char *p = alloca (GET_MODE_SIZE (mode));

  memset (p, *c, GET_MODE_SIZE (mode));

  return c_readstr (p, mode);
}

/* Callback routine for store_by_pieces.  Return the RTL of a register
   containing GET_MODE_SIZE (MODE) consecutive copies of the unsigned
   char value given in the RTL register data.  For example, if mode is
   4 bytes wide, return the RTL for 0x01010101*data.  */

static rtx
builtin_memset_gen_str (void *data, HOST_WIDE_INT offset ATTRIBUTE_UNUSED,
			enum machine_mode mode)
{
  rtx target, coeff;
  size_t size;
  char *p;

  size = GET_MODE_SIZE (mode);
  if (size == 1)
    return (rtx) data;

  p = alloca (size);
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
expand_builtin_memset (tree exp, rtx target, enum machine_mode mode)
{
  if (!validate_arglist (exp,
 			 POINTER_TYPE, INTEGER_TYPE, INTEGER_TYPE, VOID_TYPE))
    return NULL_RTX;
  else
    {
      tree dest = CALL_EXPR_ARG (exp, 0);
      tree val = CALL_EXPR_ARG (exp, 1);
      tree len = CALL_EXPR_ARG (exp, 2);
      return expand_builtin_memset_args (dest, val, len, target, mode, exp);
    }
}

/* Helper function to do the actual work for expand_builtin_memset.  The
   arguments to the builtin_memset call DEST, VAL, and LEN are broken out
   so that this can also be called without constructing an actual CALL_EXPR.
   The other arguments and return value are the same as for
   expand_builtin_memset.  */

static rtx
expand_builtin_memset_args (tree dest, tree val, tree len,
			    rtx target, enum machine_mode mode, tree orig_exp)
{
  tree fndecl, fn;
  enum built_in_function fcode;
  char c;
  unsigned int dest_align;
  rtx dest_mem, dest_addr, len_rtx;
  HOST_WIDE_INT expected_size = -1;
  unsigned int expected_align = 0;

  dest_align = get_pointer_alignment (dest, BIGGEST_ALIGNMENT);

  /* If DEST is not a pointer type, don't do this operation in-line.  */
  if (dest_align == 0)
    return NULL_RTX;

  stringop_block_profile (orig_exp, &expected_align, &expected_size);
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
  dest_mem = get_memory_rtx (dest, len);

  if (TREE_CODE (val) != INTEGER_CST)
    {
      rtx val_rtx;

      val_rtx = expand_normal (val);
      val_rtx = convert_to_mode (TYPE_MODE (unsigned_char_type_node),
				 val_rtx, 0);

      /* Assume that we can memset by pieces if we can store
       * the coefficients by pieces (in the required modes).
       * We can't pass builtin_memset_gen_str as that emits RTL.  */
      c = 1;
      if (host_integerp (len, 1)
	  && can_store_by_pieces (tree_low_cst (len, 1),
				  builtin_memset_read_str, &c, dest_align,
				  true))
	{
	  val_rtx = force_reg (TYPE_MODE (unsigned_char_type_node),
			       val_rtx);
	  store_by_pieces (dest_mem, tree_low_cst (len, 1),
			   builtin_memset_gen_str, val_rtx, dest_align,
			   true, 0);
	}
      else if (!set_storage_via_setmem (dest_mem, len_rtx, val_rtx,
					dest_align, expected_align,
					expected_size))
	goto do_libcall;
      
      dest_mem = force_operand (XEXP (dest_mem, 0), NULL_RTX);
      dest_mem = convert_memory_address (ptr_mode, dest_mem);
      return dest_mem;
    }

  if (target_char_cast (val, &c))
    goto do_libcall;

  if (c)
    {
      if (host_integerp (len, 1)
	  && can_store_by_pieces (tree_low_cst (len, 1),
				  builtin_memset_read_str, &c, dest_align,
				  true))
	store_by_pieces (dest_mem, tree_low_cst (len, 1),
			 builtin_memset_read_str, &c, dest_align, true, 0);
      else if (!set_storage_via_setmem (dest_mem, len_rtx, GEN_INT (c),
					dest_align, expected_align,
					expected_size))
	goto do_libcall;
      
      dest_mem = force_operand (XEXP (dest_mem, 0), NULL_RTX);
      dest_mem = convert_memory_address (ptr_mode, dest_mem);
      return dest_mem;
    }

  set_mem_align (dest_mem, dest_align);
  dest_addr = clear_storage_hints (dest_mem, len_rtx,
				   CALL_EXPR_TAILCALL (orig_exp)
				   ? BLOCK_OP_TAILCALL : BLOCK_OP_NORMAL,
				   expected_align, expected_size);

  if (dest_addr == 0)
    {
      dest_addr = force_operand (XEXP (dest_mem, 0), NULL_RTX);
      dest_addr = convert_memory_address (ptr_mode, dest_addr);
    }

  return dest_addr;

 do_libcall:
  fndecl = get_callee_fndecl (orig_exp);
  fcode = DECL_FUNCTION_CODE (fndecl);
  if (fcode == BUILT_IN_MEMSET)
    fn = build_call_expr (fndecl, 3, dest, val, len);
  else if (fcode == BUILT_IN_BZERO)
    fn = build_call_expr (fndecl, 2, dest, len);
  else
    gcc_unreachable ();
  if (TREE_CODE (fn) == CALL_EXPR)
    CALL_EXPR_TAILCALL (fn) = CALL_EXPR_TAILCALL (orig_exp);
  return expand_call (fn, target, target == const0_rtx);
}

/* Expand expression EXP, which is a call to the bzero builtin.  Return 
   NULL_RTX if we failed the caller should emit a normal call.  */

static rtx
expand_builtin_bzero (tree exp)
{
  tree dest, size;

  if (!validate_arglist (exp, POINTER_TYPE, INTEGER_TYPE, VOID_TYPE))
    return NULL_RTX;

  dest = CALL_EXPR_ARG (exp, 0);
  size = CALL_EXPR_ARG (exp, 1);

  /* New argument list transforming bzero(ptr x, int y) to
     memset(ptr x, int 0, size_t y).   This is done this way
     so that if it isn't expanded inline, we fallback to
     calling bzero instead of memset.  */

  return expand_builtin_memset_args (dest, integer_zero_node,
				     fold_convert (sizetype, size),
				     const0_rtx, VOIDmode, exp);
}

/* Expand a call to the memchr builtin.  Return NULL_RTX if we failed the
   caller should emit a normal call, otherwise try to get the result
   in TARGET, if convenient (and in mode MODE if that's convenient).  */

static rtx
expand_builtin_memchr (tree exp, rtx target, enum machine_mode mode)
{
  if (validate_arglist (exp, POINTER_TYPE, INTEGER_TYPE,
			INTEGER_TYPE, VOID_TYPE))
    {
      tree type = TREE_TYPE (exp);
      tree result = fold_builtin_memchr (CALL_EXPR_ARG (exp, 0),
					 CALL_EXPR_ARG (exp, 1),
					 CALL_EXPR_ARG (exp, 2), type);
      if (result)
	return expand_expr (result, target, mode, EXPAND_NORMAL);
    }
  return NULL_RTX;
}

/* Expand expression EXP, which is a call to the memcmp built-in function.
   Return NULL_RTX if we failed and the
   caller should emit a normal call, otherwise try to get the result in
   TARGET, if convenient (and in mode MODE, if that's convenient).  */

static rtx
expand_builtin_memcmp (tree exp, rtx target, enum machine_mode mode)
{
  if (!validate_arglist (exp,
 			 POINTER_TYPE, POINTER_TYPE, INTEGER_TYPE, VOID_TYPE))
    return NULL_RTX;
  else
    {
      tree result = fold_builtin_memcmp (CALL_EXPR_ARG (exp, 0),
 					 CALL_EXPR_ARG (exp, 1),
 					 CALL_EXPR_ARG (exp, 2));
      if (result)
	return expand_expr (result, target, mode, EXPAND_NORMAL);
    }

#if defined HAVE_cmpmemsi || defined HAVE_cmpstrnsi
  {
    rtx arg1_rtx, arg2_rtx, arg3_rtx;
    rtx result;
    rtx insn;
    tree arg1 = CALL_EXPR_ARG (exp, 0);
    tree arg2 = CALL_EXPR_ARG (exp, 1);
    tree len = CALL_EXPR_ARG (exp, 2);

    int arg1_align
      = get_pointer_alignment (arg1, BIGGEST_ALIGNMENT) / BITS_PER_UNIT;
    int arg2_align
      = get_pointer_alignment (arg2, BIGGEST_ALIGNMENT) / BITS_PER_UNIT;
    enum machine_mode insn_mode;

#ifdef HAVE_cmpmemsi
    if (HAVE_cmpmemsi)
      insn_mode = insn_data[(int) CODE_FOR_cmpmemsi].operand[0].mode;
    else
#endif
#ifdef HAVE_cmpstrnsi
    if (HAVE_cmpstrnsi)
      insn_mode = insn_data[(int) CODE_FOR_cmpstrnsi].operand[0].mode;
    else
#endif
      return NULL_RTX;

    /* If we don't have POINTER_TYPE, call the function.  */
    if (arg1_align == 0 || arg2_align == 0)
      return NULL_RTX;

    /* Make a place to write the result of the instruction.  */
    result = target;
    if (! (result != 0
	   && REG_P (result) && GET_MODE (result) == insn_mode
	   && REGNO (result) >= FIRST_PSEUDO_REGISTER))
      result = gen_reg_rtx (insn_mode);

    arg1_rtx = get_memory_rtx (arg1, len);
    arg2_rtx = get_memory_rtx (arg2, len);
    arg3_rtx = expand_normal (fold_convert (sizetype, len));

    /* Set MEM_SIZE as appropriate.  */
    if (GET_CODE (arg3_rtx) == CONST_INT)
      {
	set_mem_size (arg1_rtx, arg3_rtx);
	set_mem_size (arg2_rtx, arg3_rtx);
      }

#ifdef HAVE_cmpmemsi
    if (HAVE_cmpmemsi)
      insn = gen_cmpmemsi (result, arg1_rtx, arg2_rtx, arg3_rtx,
			   GEN_INT (MIN (arg1_align, arg2_align)));
    else
#endif
#ifdef HAVE_cmpstrnsi
    if (HAVE_cmpstrnsi)
      insn = gen_cmpstrnsi (result, arg1_rtx, arg2_rtx, arg3_rtx,
			    GEN_INT (MIN (arg1_align, arg2_align)));
    else
#endif
      gcc_unreachable ();

    if (insn)
      emit_insn (insn);
    else
      emit_library_call_value (memcmp_libfunc, result, LCT_PURE_MAKE_BLOCK,
			       TYPE_MODE (integer_type_node), 3,
			       XEXP (arg1_rtx, 0), Pmode,
			       XEXP (arg2_rtx, 0), Pmode,
			       convert_to_mode (TYPE_MODE (sizetype), arg3_rtx,
						TYPE_UNSIGNED (sizetype)),
			       TYPE_MODE (sizetype));

    /* Return the value in the proper mode for this function.  */
    mode = TYPE_MODE (TREE_TYPE (exp));
    if (GET_MODE (result) == mode)
      return result;
    else if (target != 0)
      {
	convert_move (target, result, 0);
	return target;
      }
    else
      return convert_to_mode (mode, result, 0);
  }
#endif

  return NULL_RTX;
}

/* Expand expression EXP, which is a call to the strcmp builtin.  Return NULL_RTX
   if we failed the caller should emit a normal call, otherwise try to get
   the result in TARGET, if convenient.  */

static rtx
expand_builtin_strcmp (tree exp, rtx target, enum machine_mode mode)
{
  if (!validate_arglist (exp, POINTER_TYPE, POINTER_TYPE, VOID_TYPE))
    return NULL_RTX;
  else
    {
      tree result = fold_builtin_strcmp (CALL_EXPR_ARG (exp, 0),
 					 CALL_EXPR_ARG (exp, 1));
      if (result)
	return expand_expr (result, target, mode, EXPAND_NORMAL);
    }

#if defined HAVE_cmpstrsi || defined HAVE_cmpstrnsi
  if (cmpstr_optab[SImode] != CODE_FOR_nothing
      || cmpstrn_optab[SImode] != CODE_FOR_nothing)
    {
      rtx arg1_rtx, arg2_rtx;
      rtx result, insn = NULL_RTX;
      tree fndecl, fn;
      tree arg1 = CALL_EXPR_ARG (exp, 0);
      tree arg2 = CALL_EXPR_ARG (exp, 1);

      int arg1_align
	= get_pointer_alignment (arg1, BIGGEST_ALIGNMENT) / BITS_PER_UNIT;
      int arg2_align
	= get_pointer_alignment (arg2, BIGGEST_ALIGNMENT) / BITS_PER_UNIT;

      /* If we don't have POINTER_TYPE, call the function.  */
      if (arg1_align == 0 || arg2_align == 0)
	return NULL_RTX;

      /* Stabilize the arguments in case gen_cmpstr(n)si fail.  */
      arg1 = builtin_save_expr (arg1);
      arg2 = builtin_save_expr (arg2);

      arg1_rtx = get_memory_rtx (arg1, NULL);
      arg2_rtx = get_memory_rtx (arg2, NULL);

#ifdef HAVE_cmpstrsi
      /* Try to call cmpstrsi.  */
      if (HAVE_cmpstrsi)
	{
	  enum machine_mode insn_mode
	    = insn_data[(int) CODE_FOR_cmpstrsi].operand[0].mode;

	  /* Make a place to write the result of the instruction.  */
	  result = target;
	  if (! (result != 0
		 && REG_P (result) && GET_MODE (result) == insn_mode
		 && REGNO (result) >= FIRST_PSEUDO_REGISTER))
	    result = gen_reg_rtx (insn_mode);

	  insn = gen_cmpstrsi (result, arg1_rtx, arg2_rtx,
			       GEN_INT (MIN (arg1_align, arg2_align)));
	}
#endif
#ifdef HAVE_cmpstrnsi
      /* Try to determine at least one length and call cmpstrnsi.  */
      if (!insn && HAVE_cmpstrnsi)
	{
	  tree len;
	  rtx arg3_rtx;

	  enum machine_mode insn_mode
	    = insn_data[(int) CODE_FOR_cmpstrnsi].operand[0].mode;
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
	  if (!len || TREE_SIDE_EFFECTS (len))
	    goto do_libcall;

	  arg3_rtx = expand_normal (len);

	  /* Make a place to write the result of the instruction.  */
	  result = target;
	  if (! (result != 0
		 && REG_P (result) && GET_MODE (result) == insn_mode
		 && REGNO (result) >= FIRST_PSEUDO_REGISTER))
	    result = gen_reg_rtx (insn_mode);

	  insn = gen_cmpstrnsi (result, arg1_rtx, arg2_rtx, arg3_rtx,
				GEN_INT (MIN (arg1_align, arg2_align)));
	}
#endif

      if (insn)
	{
	  emit_insn (insn);

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
#ifdef HAVE_cmpstrnsi
    do_libcall:
#endif
      fndecl = get_callee_fndecl (exp);
      fn = build_call_expr (fndecl, 2, arg1, arg2);
      if (TREE_CODE (fn) == CALL_EXPR)
	CALL_EXPR_TAILCALL (fn) = CALL_EXPR_TAILCALL (exp);
      return expand_call (fn, target, target == const0_rtx);
    }
#endif
  return NULL_RTX;
}

/* Expand expression EXP, which is a call to the strncmp builtin. Return 
   NULL_RTX if we failed the caller should emit a normal call, otherwise try to get
   the result in TARGET, if convenient.  */

static rtx
expand_builtin_strncmp (tree exp, rtx target, enum machine_mode mode)
{
  if (!validate_arglist (exp,
 			 POINTER_TYPE, POINTER_TYPE, INTEGER_TYPE, VOID_TYPE))
    return NULL_RTX;
  else
    {
      tree result = fold_builtin_strncmp (CALL_EXPR_ARG (exp, 0),
 					  CALL_EXPR_ARG (exp, 1),
 					  CALL_EXPR_ARG (exp, 2));
      if (result)
	return expand_expr (result, target, mode, EXPAND_NORMAL);
    }

  /* If c_strlen can determine an expression for one of the string
     lengths, and it doesn't have side effects, then emit cmpstrnsi
     using length MIN(strlen(string)+1, arg3).  */
#ifdef HAVE_cmpstrnsi
  if (HAVE_cmpstrnsi)
  {
    tree len, len1, len2;
    rtx arg1_rtx, arg2_rtx, arg3_rtx;
    rtx result, insn;
    tree fndecl, fn;
    tree arg1 = CALL_EXPR_ARG (exp, 0);
    tree arg2 = CALL_EXPR_ARG (exp, 1);
    tree arg3 = CALL_EXPR_ARG (exp, 2);

    int arg1_align
      = get_pointer_alignment (arg1, BIGGEST_ALIGNMENT) / BITS_PER_UNIT;
    int arg2_align
      = get_pointer_alignment (arg2, BIGGEST_ALIGNMENT) / BITS_PER_UNIT;
    enum machine_mode insn_mode
      = insn_data[(int) CODE_FOR_cmpstrnsi].operand[0].mode;

    len1 = c_strlen (arg1, 1);
    len2 = c_strlen (arg2, 1);

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
    if (!len || TREE_SIDE_EFFECTS (len))
      return NULL_RTX;

    /* The actual new length parameter is MIN(len,arg3).  */
    len = fold_build2 (MIN_EXPR, TREE_TYPE (len), len,
		       fold_convert (TREE_TYPE (len), arg3));

    /* If we don't have POINTER_TYPE, call the function.  */
    if (arg1_align == 0 || arg2_align == 0)
      return NULL_RTX;

    /* Make a place to write the result of the instruction.  */
    result = target;
    if (! (result != 0
	   && REG_P (result) && GET_MODE (result) == insn_mode
	   && REGNO (result) >= FIRST_PSEUDO_REGISTER))
      result = gen_reg_rtx (insn_mode);

    /* Stabilize the arguments in case gen_cmpstrnsi fails.  */
    arg1 = builtin_save_expr (arg1);
    arg2 = builtin_save_expr (arg2);
    len = builtin_save_expr (len);

    arg1_rtx = get_memory_rtx (arg1, len);
    arg2_rtx = get_memory_rtx (arg2, len);
    arg3_rtx = expand_normal (len);
    insn = gen_cmpstrnsi (result, arg1_rtx, arg2_rtx, arg3_rtx,
			  GEN_INT (MIN (arg1_align, arg2_align)));
    if (insn)
      {
	emit_insn (insn);

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
    fn = build_call_expr (fndecl, 3, arg1, arg2, len);
    if (TREE_CODE (fn) == CALL_EXPR)
      CALL_EXPR_TAILCALL (fn) = CALL_EXPR_TAILCALL (exp);
    return expand_call (fn, target, target == const0_rtx);
  }
#endif
  return NULL_RTX;
}

/* Expand expression EXP, which is a call to the strcat builtin.
   Return NULL_RTX if we failed the caller should emit a normal call,
   otherwise try to get the result in TARGET, if convenient.  */

static rtx
expand_builtin_strcat (tree fndecl, tree exp, rtx target, enum machine_mode mode)
{
  if (!validate_arglist (exp, POINTER_TYPE, POINTER_TYPE, VOID_TYPE))
    return NULL_RTX;
  else
    {
      tree dst = CALL_EXPR_ARG (exp, 0);
      tree src = CALL_EXPR_ARG (exp, 1);
      const char *p = c_getstr (src);

      /* If the string length is zero, return the dst parameter.  */
      if (p && *p == '\0')
	return expand_expr (dst, target, mode, EXPAND_NORMAL);

      if (!optimize_size)
	{
	  /* See if we can store by pieces into (dst + strlen(dst)).  */
	  tree newsrc, newdst,
	    strlen_fn = implicit_built_in_decls[BUILT_IN_STRLEN];
	  rtx insns;

	  /* Stabilize the argument list.  */
	  newsrc = builtin_save_expr (src);
	  dst = builtin_save_expr (dst);

	  start_sequence ();

	  /* Create strlen (dst).  */
	  newdst = build_call_expr (strlen_fn, 1, dst);
	  /* Create (dst p+ strlen (dst)).  */

	  newdst = fold_build2 (POINTER_PLUS_EXPR, TREE_TYPE (dst), dst, newdst);
	  newdst = builtin_save_expr (newdst);

	  if (!expand_builtin_strcpy_args (fndecl, newdst, newsrc, target, mode))
	    {
	      end_sequence (); /* Stop sequence.  */
	      return NULL_RTX;
	    }

	  /* Output the entire sequence.  */
	  insns = get_insns ();
	  end_sequence ();
	  emit_insn (insns);

	  return expand_expr (dst, target, mode, EXPAND_NORMAL);
	}

      return NULL_RTX;
    }
}

/* Expand expression EXP, which is a call to the strncat builtin.
   Return NULL_RTX if we failed the caller should emit a normal call,
   otherwise try to get the result in TARGET, if convenient.  */

static rtx
expand_builtin_strncat (tree exp, rtx target, enum machine_mode mode)
{
  if (validate_arglist (exp,
 			POINTER_TYPE, POINTER_TYPE, INTEGER_TYPE, VOID_TYPE))
    {
      tree result = fold_builtin_strncat (CALL_EXPR_ARG (exp, 0),
					  CALL_EXPR_ARG (exp, 1),
					  CALL_EXPR_ARG (exp, 2));
      if (result)
	return expand_expr (result, target, mode, EXPAND_NORMAL);
    }
  return NULL_RTX;
}

/* Expand expression EXP, which is a call to the strspn builtin.
   Return NULL_RTX if we failed the caller should emit a normal call,
   otherwise try to get the result in TARGET, if convenient.  */

static rtx
expand_builtin_strspn (tree exp, rtx target, enum machine_mode mode)
{
  if (validate_arglist (exp, POINTER_TYPE, POINTER_TYPE, VOID_TYPE))
    {
      tree result = fold_builtin_strspn (CALL_EXPR_ARG (exp, 0),
					 CALL_EXPR_ARG (exp, 1));
      if (result)
	return expand_expr (result, target, mode, EXPAND_NORMAL);
    }
  return NULL_RTX;
}

/* Expand expression EXP, which is a call to the strcspn builtin.
   Return NULL_RTX if we failed the caller should emit a normal call,
   otherwise try to get the result in TARGET, if convenient.  */

static rtx
expand_builtin_strcspn (tree exp, rtx target, enum machine_mode mode)
{
  if (validate_arglist (exp, POINTER_TYPE, POINTER_TYPE, VOID_TYPE))
    {
      tree result = fold_builtin_strcspn (CALL_EXPR_ARG (exp, 0),
					  CALL_EXPR_ARG (exp, 1));
      if (result)
	return expand_expr (result, target, mode, EXPAND_NORMAL);
    }
  return NULL_RTX;
}

/* Expand a call to __builtin_saveregs, generating the result in TARGET,
   if that's convenient.  */

rtx
expand_builtin_saveregs (void)
{
  rtx val, seq;

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

/* __builtin_args_info (N) returns word N of the arg space info
   for the current function.  The number and meanings of words
   is controlled by the definition of CUMULATIVE_ARGS.  */

static rtx
expand_builtin_args_info (tree exp)
{
  int nwords = sizeof (CUMULATIVE_ARGS) / sizeof (int);
  int *word_ptr = (int *) &current_function_args_info;

  gcc_assert (sizeof (CUMULATIVE_ARGS) % sizeof (int) == 0);

  if (call_expr_nargs (exp) != 0)
    {
      if (!host_integerp (CALL_EXPR_ARG (exp, 0), 0))
	error ("argument of %<__builtin_args_info%> must be constant");
      else
	{
	  HOST_WIDE_INT wordnum = tree_low_cst (CALL_EXPR_ARG (exp, 0), 0);

	  if (wordnum < 0 || wordnum >= nwords)
	    error ("argument of %<__builtin_args_info%> out of range");
	  else
	    return GEN_INT (word_ptr[wordnum]);
	}
    }
  else
    error ("missing argument in %<__builtin_args_info%>");

  return const0_rtx;
}

/* Expand a call to __builtin_next_arg.  */

static rtx
expand_builtin_next_arg (void)
{
  /* Checking arguments is already done in fold_builtin_next_arg
     that must be called before this function.  */
  return expand_binop (ptr_mode, add_optab,
		       current_function_internal_arg_pointer,
		       current_function_arg_offset_rtx,
		       NULL_RTX, 0, OPTAB_LIB_WIDEN);
}

/* Make it easier for the backends by protecting the valist argument
   from multiple evaluations.  */

static tree
stabilize_va_list (tree valist, int needs_lvalue)
{
  if (TREE_CODE (va_list_type_node) == ARRAY_TYPE)
    {
      if (TREE_SIDE_EFFECTS (valist))
	valist = save_expr (valist);

      /* For this case, the backends will be expecting a pointer to
	 TREE_TYPE (va_list_type_node), but it's possible we've
	 actually been given an array (an actual va_list_type_node).
	 So fix it.  */
      if (TREE_CODE (TREE_TYPE (valist)) == ARRAY_TYPE)
	{
	  tree p1 = build_pointer_type (TREE_TYPE (va_list_type_node));
	  valist = build_fold_addr_expr_with_type (valist, p1);
	}
    }
  else
    {
      tree pt;

      if (! needs_lvalue)
	{
	  if (! TREE_SIDE_EFFECTS (valist))
	    return valist;

	  pt = build_pointer_type (va_list_type_node);
	  valist = fold_build1 (ADDR_EXPR, pt, valist);
	  TREE_SIDE_EFFECTS (valist) = 1;
	}

      if (TREE_SIDE_EFFECTS (valist))
	valist = save_expr (valist);
      valist = build_fold_indirect_ref (valist);
    }

  return valist;
}

/* The "standard" definition of va_list is void*.  */

tree
std_build_builtin_va_list (void)
{
  return ptr_type_node;
}

/* The "standard" implementation of va_start: just assign `nextarg' to
   the variable.  */

void
std_expand_builtin_va_start (tree valist, rtx nextarg)
{
  rtx va_r = expand_expr (valist, NULL_RTX, VOIDmode, EXPAND_WRITE);
  convert_move (va_r, nextarg, 0);
}

/* Expand EXP, a call to __builtin_va_start.  */

static rtx
expand_builtin_va_start (tree exp)
{
  rtx nextarg;
  tree valist;

  if (call_expr_nargs (exp) < 2)
    {
      error ("too few arguments to function %<va_start%>");
      return const0_rtx;
    }

  if (fold_builtin_next_arg (exp, true))
    return const0_rtx;

  nextarg = expand_builtin_next_arg ();
  valist = stabilize_va_list (CALL_EXPR_ARG (exp, 0), 1);

  if (targetm.expand_builtin_va_start)
    targetm.expand_builtin_va_start (valist, nextarg);
  else
    std_expand_builtin_va_start (valist, nextarg);

  return const0_rtx;
}

/* The "standard" implementation of va_arg: read the value from the
   current (padded) address and increment by the (padded) size.  */

tree
std_gimplify_va_arg_expr (tree valist, tree type, tree *pre_p, tree *post_p)
{
  tree addr, t, type_size, rounded_size, valist_tmp;
  unsigned HOST_WIDE_INT align, boundary;
  bool indirect;

#ifdef ARGS_GROW_DOWNWARD
  /* All of the alignment and movement below is for args-grow-up machines.
     As of 2004, there are only 3 ARGS_GROW_DOWNWARD targets, and they all
     implement their own specialized gimplify_va_arg_expr routines.  */
  gcc_unreachable ();
#endif

  indirect = pass_by_reference (NULL, TYPE_MODE (type), type, false);
  if (indirect)
    type = build_pointer_type (type);

  align = PARM_BOUNDARY / BITS_PER_UNIT;
  boundary = FUNCTION_ARG_BOUNDARY (TYPE_MODE (type), type) / BITS_PER_UNIT;

  /* Hoist the valist value into a temporary for the moment.  */
  valist_tmp = get_initialized_tmp_var (valist, pre_p, NULL);

  /* va_list pointer is aligned to PARM_BOUNDARY.  If argument actually
     requires greater alignment, we must perform dynamic alignment.  */
  if (boundary > align
      && !integer_zerop (TYPE_SIZE (type)))
    {
      t = build2 (MODIFY_EXPR, TREE_TYPE (valist), valist_tmp,
		  fold_build2 (POINTER_PLUS_EXPR, TREE_TYPE (valist),
			       valist_tmp, size_int (boundary - 1)));
      gimplify_and_add (t, pre_p);

      t = fold_convert (sizetype, valist_tmp);
      t = build2 (MODIFY_EXPR, TREE_TYPE (valist), valist_tmp,
		  fold_convert (TREE_TYPE (valist),
				fold_build2 (BIT_AND_EXPR, sizetype, t,
					     size_int (-boundary))));
      gimplify_and_add (t, pre_p);
    }
  else
    boundary = align;

  /* If the actual alignment is less than the alignment of the type,
     adjust the type accordingly so that we don't assume strict alignment
     when deferencing the pointer.  */
  boundary *= BITS_PER_UNIT;
  if (boundary < TYPE_ALIGN (type))
    {
      type = build_variant_type_copy (type);
      TYPE_ALIGN (type) = boundary;
    }

  /* Compute the rounded size of the type.  */
  type_size = size_in_bytes (type);
  rounded_size = round_up (type_size, align);

  /* Reduce rounded_size so it's sharable with the postqueue.  */
  gimplify_expr (&rounded_size, pre_p, post_p, is_gimple_val, fb_rvalue);

  /* Get AP.  */
  addr = valist_tmp;
  if (PAD_VARARGS_DOWN && !integer_zerop (rounded_size))
    {
      /* Small args are padded downward.  */
      t = fold_build2 (GT_EXPR, sizetype, rounded_size, size_int (align));
      t = fold_build3 (COND_EXPR, sizetype, t, size_zero_node,
		       size_binop (MINUS_EXPR, rounded_size, type_size));
      addr = fold_build2 (POINTER_PLUS_EXPR, TREE_TYPE (addr), addr, t);
    }

  /* Compute new value for AP.  */
  t = build2 (POINTER_PLUS_EXPR, TREE_TYPE (valist), valist_tmp, rounded_size);
  t = build2 (MODIFY_EXPR, TREE_TYPE (valist), valist, t);
  gimplify_and_add (t, pre_p);

  addr = fold_convert (build_pointer_type (type), addr);

  if (indirect)
    addr = build_va_arg_indirect_ref (addr);

  return build_va_arg_indirect_ref (addr);
}

/* Build an indirect-ref expression over the given TREE, which represents a
   piece of a va_arg() expansion.  */
tree
build_va_arg_indirect_ref (tree addr)
{
  addr = build_fold_indirect_ref (addr);

  if (flag_mudflap) /* Don't instrument va_arg INDIRECT_REF.  */
    mf_mark (addr);

  return addr;
}

/* Return a dummy expression of type TYPE in order to keep going after an
   error.  */

static tree
dummy_object (tree type)
{
  tree t = build_int_cst (build_pointer_type (type), 0);
  return build1 (INDIRECT_REF, type, t);
}

/* Gimplify __builtin_va_arg, aka VA_ARG_EXPR, which is not really a
   builtin function, but a very special sort of operator.  */

enum gimplify_status
gimplify_va_arg_expr (tree *expr_p, tree *pre_p, tree *post_p)
{
  tree promoted_type, want_va_type, have_va_type;
  tree valist = TREE_OPERAND (*expr_p, 0);
  tree type = TREE_TYPE (*expr_p);
  tree t;

  /* Verify that valist is of the proper type.  */
  want_va_type = va_list_type_node;
  have_va_type = TREE_TYPE (valist);

  if (have_va_type == error_mark_node)
    return GS_ERROR;

  if (TREE_CODE (want_va_type) == ARRAY_TYPE)
    {
      /* If va_list is an array type, the argument may have decayed
	 to a pointer type, e.g. by being passed to another function.
	 In that case, unwrap both types so that we can compare the
	 underlying records.  */
      if (TREE_CODE (have_va_type) == ARRAY_TYPE
	  || POINTER_TYPE_P (have_va_type))
	{
	  want_va_type = TREE_TYPE (want_va_type);
	  have_va_type = TREE_TYPE (have_va_type);
	}
    }

  if (TYPE_MAIN_VARIANT (want_va_type) != TYPE_MAIN_VARIANT (have_va_type))
    {
      error ("first argument to %<va_arg%> not of type %<va_list%>");
      return GS_ERROR;
    }

  /* Generate a diagnostic for requesting data of a type that cannot
     be passed through `...' due to type promotion at the call site.  */
  else if ((promoted_type = lang_hooks.types.type_promotes_to (type))
	   != type)
    {
      static bool gave_help;

      /* Unfortunately, this is merely undefined, rather than a constraint
	 violation, so we cannot make this an error.  If this call is never
	 executed, the program is still strictly conforming.  */
      warning (0, "%qT is promoted to %qT when passed through %<...%>",
	       type, promoted_type);
      if (! gave_help)
	{
	  gave_help = true;
	  inform ("(so you should pass %qT not %qT to %<va_arg%>)",
		   promoted_type, type);
	}

      /* We can, however, treat "undefined" any way we please.
	 Call abort to encourage the user to fix the program.  */
      inform ("if this code is reached, the program will abort");
      t = build_call_expr (implicit_built_in_decls[BUILT_IN_TRAP], 0);
      append_to_statement_list (t, pre_p);

      /* This is dead code, but go ahead and finish so that the
	 mode of the result comes out right.  */
      *expr_p = dummy_object (type);
      return GS_ALL_DONE;
    }
  else
    {
      /* Make it easier for the backends by protecting the valist argument
	 from multiple evaluations.  */
      if (TREE_CODE (va_list_type_node) == ARRAY_TYPE)
	{
	  /* For this case, the backends will be expecting a pointer to
	     TREE_TYPE (va_list_type_node), but it's possible we've
	     actually been given an array (an actual va_list_type_node).
	     So fix it.  */
	  if (TREE_CODE (TREE_TYPE (valist)) == ARRAY_TYPE)
	    {
	      tree p1 = build_pointer_type (TREE_TYPE (va_list_type_node));
	      valist = build_fold_addr_expr_with_type (valist, p1);
	    }
	  gimplify_expr (&valist, pre_p, post_p, is_gimple_val, fb_rvalue);
	}
      else
	gimplify_expr (&valist, pre_p, post_p, is_gimple_min_lval, fb_lvalue);

      if (!targetm.gimplify_va_arg_expr)
	/* FIXME:Once most targets are converted we should merely
	   assert this is non-null.  */
	return GS_ALL_DONE;

      *expr_p = targetm.gimplify_va_arg_expr (valist, type, pre_p, post_p);
      return GS_OK;
    }
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

  dst = CALL_EXPR_ARG (exp, 0);
  src = CALL_EXPR_ARG (exp, 1);

  dst = stabilize_va_list (dst, 1);
  src = stabilize_va_list (src, 0);

  if (TREE_CODE (va_list_type_node) != ARRAY_TYPE)
    {
      t = build2 (MODIFY_EXPR, va_list_type_node, dst, src);
      TREE_SIDE_EFFECTS (t) = 1;
      expand_expr (t, const0_rtx, VOIDmode, EXPAND_NORMAL);
    }
  else
    {
      rtx dstb, srcb, size;

      /* Evaluate to pointers.  */
      dstb = expand_expr (dst, NULL_RTX, Pmode, EXPAND_NORMAL);
      srcb = expand_expr (src, NULL_RTX, Pmode, EXPAND_NORMAL);
      size = expand_expr (TYPE_SIZE_UNIT (va_list_type_node), NULL_RTX,
			  VOIDmode, EXPAND_NORMAL);

      dstb = convert_memory_address (Pmode, dstb);
      srcb = convert_memory_address (Pmode, srcb);

      /* "Dereference" to BLKmode memories.  */
      dstb = gen_rtx_MEM (BLKmode, dstb);
      set_mem_alias_set (dstb, get_alias_set (TREE_TYPE (TREE_TYPE (dst))));
      set_mem_align (dstb, TYPE_ALIGN (va_list_type_node));
      srcb = gen_rtx_MEM (BLKmode, srcb);
      set_mem_alias_set (srcb, get_alias_set (TREE_TYPE (TREE_TYPE (src))));
      set_mem_align (srcb, TYPE_ALIGN (va_list_type_node));

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
     The value is the return address saved in that frame.  */
  if (call_expr_nargs (exp) == 0)
    /* Warning about missing arg was already issued.  */
    return const0_rtx;
  else if (! host_integerp (CALL_EXPR_ARG (exp, 0), 1))
    {
      if (DECL_FUNCTION_CODE (fndecl) == BUILT_IN_FRAME_ADDRESS)
	error ("invalid argument to %<__builtin_frame_address%>");
      else
	error ("invalid argument to %<__builtin_return_address%>");
      return const0_rtx;
    }
  else
    {
      rtx tem
	= expand_builtin_return_addr (DECL_FUNCTION_CODE (fndecl),
				      tree_low_cst (CALL_EXPR_ARG (exp, 0), 1));

      /* Some ports cannot access arbitrary stack frames.  */
      if (tem == NULL)
	{
	  if (DECL_FUNCTION_CODE (fndecl) == BUILT_IN_FRAME_ADDRESS)
	    warning (0, "unsupported argument to %<__builtin_frame_address%>");
	  else
	    warning (0, "unsupported argument to %<__builtin_return_address%>");
	  return const0_rtx;
	}

      /* For __builtin_frame_address, return what we've got.  */
      if (DECL_FUNCTION_CODE (fndecl) == BUILT_IN_FRAME_ADDRESS)
	return tem;

      if (!REG_P (tem)
	  && ! CONSTANT_P (tem))
	tem = copy_to_mode_reg (Pmode, tem);
      return tem;
    }
}

/* Expand EXP, a call to the alloca builtin.  Return NULL_RTX if
   we failed and the caller should emit a normal call, otherwise try to get
   the result in TARGET, if convenient.  */

static rtx
expand_builtin_alloca (tree exp, rtx target)
{
  rtx op0;
  rtx result;

  /* In -fmudflap-instrumented code, alloca() and __builtin_alloca()
     should always expand to function calls.  These can be intercepted
     in libmudflap.  */
  if (flag_mudflap)
    return NULL_RTX;

  if (!validate_arglist (exp, INTEGER_TYPE, VOID_TYPE))
    return NULL_RTX;

  /* Compute the argument.  */
  op0 = expand_normal (CALL_EXPR_ARG (exp, 0));

  /* Allocate the desired space.  */
  result = allocate_dynamic_stack_space (op0, target, BITS_PER_UNIT);
  result = convert_memory_address (ptr_mode, result);

  return result;
}

/* Expand a call to a bswap builtin with argument ARG0.  MODE
   is the mode to expand with.  */

static rtx
expand_builtin_bswap (tree exp, rtx target, rtx subtarget)
{
  enum machine_mode mode;
  tree arg;
  rtx op0;

  if (!validate_arglist (exp, INTEGER_TYPE, VOID_TYPE))
    return NULL_RTX;

  arg = CALL_EXPR_ARG (exp, 0);
  mode = TYPE_MODE (TREE_TYPE (arg));
  op0 = expand_expr (arg, subtarget, VOIDmode, EXPAND_NORMAL);

  target = expand_unop (mode, bswap_optab, op0, target, 1);

  gcc_assert (target);

  return convert_to_mode (mode, target, 0);
}

/* Expand a call to a unary builtin in EXP.
   Return NULL_RTX if a normal call should be emitted rather than expanding the
   function in-line.  If convenient, the result should be placed in TARGET.
   SUBTARGET may be used as the target for computing one of EXP's operands.  */

static rtx
expand_builtin_unop (enum machine_mode target_mode, tree exp, rtx target,
		     rtx subtarget, optab op_optab)
{
  rtx op0;

  if (!validate_arglist (exp, INTEGER_TYPE, VOID_TYPE))
    return NULL_RTX;

  /* Compute the argument.  */
  op0 = expand_expr (CALL_EXPR_ARG (exp, 0), subtarget,
		     VOIDmode, EXPAND_NORMAL);
  /* Compute op, into TARGET if possible.
     Set TARGET to wherever the result comes back.  */
  target = expand_unop (TYPE_MODE (TREE_TYPE (CALL_EXPR_ARG (exp, 0))),
			op_optab, op0, target, 1);
  gcc_assert (target);

  return convert_to_mode (target_mode, target, 0);
}

/* If the string passed to fputs is a constant and is one character
   long, we attempt to transform this call into __builtin_fputc().  */

static rtx
expand_builtin_fputs (tree exp, rtx target, bool unlocked)
{
  /* Verify the arguments in the original call.  */
  if (validate_arglist (exp, POINTER_TYPE, POINTER_TYPE, VOID_TYPE))
    {
      tree result = fold_builtin_fputs (CALL_EXPR_ARG (exp, 0),
 					CALL_EXPR_ARG (exp, 1),
					(target == const0_rtx),
					unlocked, NULL_TREE);
      if (result)
	return expand_expr (result, target, VOIDmode, EXPAND_NORMAL);
    }
  return NULL_RTX;
}

/* Expand a call to __builtin_expect.  We just return our argument 
   as the builtin_expect semantic should've been already executed by
   tree branch prediction pass. */

static rtx
expand_builtin_expect (tree exp, rtx target)
{
  tree arg, c;

  if (call_expr_nargs (exp) < 2)
    return const0_rtx;
  arg = CALL_EXPR_ARG (exp, 0);
  c = CALL_EXPR_ARG (exp, 1);

  target = expand_expr (arg, target, VOIDmode, EXPAND_NORMAL);
  /* When guessing was done, the hints should be already stripped away.  */
  gcc_assert (!flag_guess_branch_prob
	      || optimize == 0 || errorcount || sorrycount);
  return target;
}

void
expand_builtin_trap (void)
{
#ifdef HAVE_trap
  if (HAVE_trap)
    emit_insn (gen_trap ());
  else
#endif
    emit_library_call (abort_libfunc, LCT_NORETURN, VOIDmode, 0);
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
  enum machine_mode mode;
  tree arg;
  rtx op0;

  if (!validate_arglist (exp, REAL_TYPE, VOID_TYPE))
    return NULL_RTX;

  arg = CALL_EXPR_ARG (exp, 0);
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

/* Create a new constant string literal and return a char* pointer to it.
   The STRING_CST value is the LEN characters at STR.  */
tree
build_string_literal (int len, const char *str)
{
  tree t, elem, index, type;

  t = build_string (len, str);
  elem = build_type_variant (char_type_node, 1, 0);
  index = build_index_type (build_int_cst (NULL_TREE, len - 1));
  type = build_array_type (elem, index);
  TREE_TYPE (t) = type;
  TREE_CONSTANT (t) = 1;
  TREE_INVARIANT (t) = 1;
  TREE_READONLY (t) = 1;
  TREE_STATIC (t) = 1;

  type = build_pointer_type (type);
  t = build1 (ADDR_EXPR, type, t);

  type = build_pointer_type (elem);
  t = build1 (NOP_EXPR, type, t);
  return t;
}

/* Expand EXP, a call to printf or printf_unlocked.
   Return NULL_RTX if a normal call should be emitted rather than transforming
   the function inline.  If convenient, the result should be placed in
   TARGET with mode MODE.  UNLOCKED indicates this is a printf_unlocked
   call.  */
static rtx
expand_builtin_printf (tree exp, rtx target, enum machine_mode mode,
		       bool unlocked)
{
  /* If we're using an unlocked function, assume the other unlocked
     functions exist explicitly.  */
  tree const fn_putchar = unlocked ? built_in_decls[BUILT_IN_PUTCHAR_UNLOCKED]
    : implicit_built_in_decls[BUILT_IN_PUTCHAR];
  tree const fn_puts = unlocked ? built_in_decls[BUILT_IN_PUTS_UNLOCKED]
    : implicit_built_in_decls[BUILT_IN_PUTS];
  const char *fmt_str;
  tree fn = 0;
  tree fmt, arg;
  int nargs = call_expr_nargs (exp);

  /* If the return value is used, don't do the transformation.  */
  if (target != const0_rtx)
    return NULL_RTX;

  /* Verify the required arguments in the original call.  */
  if (nargs == 0)
    return NULL_RTX;
  fmt = CALL_EXPR_ARG (exp, 0);
  if (! POINTER_TYPE_P (TREE_TYPE (fmt)))
    return NULL_RTX;

  /* Check whether the format is a literal string constant.  */
  fmt_str = c_getstr (fmt);
  if (fmt_str == NULL)
    return NULL_RTX;

  if (!init_target_chars ())
    return NULL_RTX;

  /* If the format specifier was "%s\n", call __builtin_puts(arg).  */
  if (strcmp (fmt_str, target_percent_s_newline) == 0)
    {
      if ((nargs != 2)
	  || ! POINTER_TYPE_P (TREE_TYPE (CALL_EXPR_ARG (exp, 1))))
	return NULL_RTX;
      if (fn_puts)
	fn = build_call_expr (fn_puts, 1, CALL_EXPR_ARG (exp, 1));
    }
  /* If the format specifier was "%c", call __builtin_putchar(arg).  */
  else if (strcmp (fmt_str, target_percent_c) == 0)
    {
      if ((nargs != 2)
	  || TREE_CODE (TREE_TYPE (CALL_EXPR_ARG (exp, 1))) != INTEGER_TYPE)
	return NULL_RTX;
      if (fn_putchar)
	fn = build_call_expr (fn_putchar, 1, CALL_EXPR_ARG (exp, 1));
    }
  else
    {
      /* We can't handle anything else with % args or %% ... yet.  */
      if (strchr (fmt_str, target_percent))
	return NULL_RTX;

      if (nargs > 1)
	return NULL_RTX;

      /* If the format specifier was "", printf does nothing.  */
      if (fmt_str[0] == '\0')
	return const0_rtx;
      /* If the format specifier has length of 1, call putchar.  */
      if (fmt_str[1] == '\0')
	{
	  /* Given printf("c"), (where c is any one character,)
	     convert "c"[0] to an int and pass that to the replacement
	     function.  */
	  arg = build_int_cst (NULL_TREE, fmt_str[0]);
	  if (fn_putchar)
	    fn = build_call_expr (fn_putchar, 1, arg);
	}
      else
	{
	  /* If the format specifier was "string\n", call puts("string").  */
	  size_t len = strlen (fmt_str);
	  if ((unsigned char)fmt_str[len - 1] == target_newline)
	    {
	      /* Create a NUL-terminated string that's one char shorter
		 than the original, stripping off the trailing '\n'.  */
	      char *newstr = alloca (len);
	      memcpy (newstr, fmt_str, len - 1);
	      newstr[len - 1] = 0;
	      arg = build_string_literal (len, newstr);
	      if (fn_puts)
		fn = build_call_expr (fn_puts, 1, arg);
	    }
	  else
	    /* We'd like to arrange to call fputs(string,stdout) here,
	       but we need stdout and don't have a way to get it yet.  */
	    return NULL_RTX;
	}
    }

  if (!fn)
    return NULL_RTX;
  if (TREE_CODE (fn) == CALL_EXPR)
    CALL_EXPR_TAILCALL (fn) = CALL_EXPR_TAILCALL (exp);
  return expand_expr (fn, target, mode, EXPAND_NORMAL);
}

/* Expand EXP, a call to fprintf or fprintf_unlocked.
   Return NULL_RTX if a normal call should be emitted rather than transforming
   the function inline.  If convenient, the result should be placed in
   TARGET with mode MODE.  UNLOCKED indicates this is a fprintf_unlocked
   call.  */
static rtx
expand_builtin_fprintf (tree exp, rtx target, enum machine_mode mode,
			bool unlocked)
{
  /* If we're using an unlocked function, assume the other unlocked
     functions exist explicitly.  */
  tree const fn_fputc = unlocked ? built_in_decls[BUILT_IN_FPUTC_UNLOCKED]
    : implicit_built_in_decls[BUILT_IN_FPUTC];
  tree const fn_fputs = unlocked ? built_in_decls[BUILT_IN_FPUTS_UNLOCKED]
    : implicit_built_in_decls[BUILT_IN_FPUTS];
  const char *fmt_str;
  tree fn = 0;
  tree fmt, fp, arg;
  int nargs = call_expr_nargs (exp);

  /* If the return value is used, don't do the transformation.  */
  if (target != const0_rtx)
    return NULL_RTX;

  /* Verify the required arguments in the original call.  */
  if (nargs < 2)
    return NULL_RTX;
  fp = CALL_EXPR_ARG (exp, 0);
  if (! POINTER_TYPE_P (TREE_TYPE (fp)))
    return NULL_RTX;
  fmt = CALL_EXPR_ARG (exp, 1);
  if (! POINTER_TYPE_P (TREE_TYPE (fmt)))
    return NULL_RTX;

  /* Check whether the format is a literal string constant.  */
  fmt_str = c_getstr (fmt);
  if (fmt_str == NULL)
    return NULL_RTX;

  if (!init_target_chars ())
    return NULL_RTX;

  /* If the format specifier was "%s", call __builtin_fputs(arg,fp).  */
  if (strcmp (fmt_str, target_percent_s) == 0)
    {
      if ((nargs != 3)
	  || ! POINTER_TYPE_P (TREE_TYPE (CALL_EXPR_ARG (exp, 2))))
	return NULL_RTX;
      arg = CALL_EXPR_ARG (exp, 2);
      if (fn_fputs)
	fn = build_call_expr (fn_fputs, 2, arg, fp);
    }
  /* If the format specifier was "%c", call __builtin_fputc(arg,fp).  */
  else if (strcmp (fmt_str, target_percent_c) == 0)
    {
      if ((nargs != 3)
	  || TREE_CODE (TREE_TYPE (CALL_EXPR_ARG (exp, 2))) != INTEGER_TYPE)
	return NULL_RTX;
      arg = CALL_EXPR_ARG (exp, 2);
      if (fn_fputc)
	fn = build_call_expr (fn_fputc, 2, arg, fp);
    }
  else
    {
      /* We can't handle anything else with % args or %% ... yet.  */
      if (strchr (fmt_str, target_percent))
	return NULL_RTX;

      if (nargs > 2)
	return NULL_RTX;

      /* If the format specifier was "", fprintf does nothing.  */
      if (fmt_str[0] == '\0')
	{
	  /* Evaluate and ignore FILE* argument for side-effects.  */
	  expand_expr (fp, const0_rtx, VOIDmode, EXPAND_NORMAL);
	  return const0_rtx;
	}

      /* When "string" doesn't contain %, replace all cases of
	 fprintf(stream,string) with fputs(string,stream).  The fputs
	 builtin will take care of special cases like length == 1.  */
      if (fn_fputs)
	fn = build_call_expr (fn_fputs, 2, fmt, fp);
    }

  if (!fn)
    return NULL_RTX;
  if (TREE_CODE (fn) == CALL_EXPR)
    CALL_EXPR_TAILCALL (fn) = CALL_EXPR_TAILCALL (exp);
  return expand_expr (fn, target, mode, EXPAND_NORMAL);
}

/* Expand a call EXP to sprintf.  Return NULL_RTX if
   a normal call should be emitted rather than expanding the function
   inline.  If convenient, the result should be placed in TARGET with
   mode MODE.  */

static rtx
expand_builtin_sprintf (tree exp, rtx target, enum machine_mode mode)
{
  tree dest, fmt;
  const char *fmt_str;
  int nargs = call_expr_nargs (exp);

  /* Verify the required arguments in the original call.  */
  if (nargs < 2)
    return NULL_RTX;
  dest = CALL_EXPR_ARG (exp, 0);
  if (! POINTER_TYPE_P (TREE_TYPE (dest)))
    return NULL_RTX;
  fmt = CALL_EXPR_ARG (exp, 0);
  if (! POINTER_TYPE_P (TREE_TYPE (fmt)))
    return NULL_RTX;

  /* Check whether the format is a literal string constant.  */
  fmt_str = c_getstr (fmt);
  if (fmt_str == NULL)
    return NULL_RTX;

  if (!init_target_chars ())
    return NULL_RTX;

  /* If the format doesn't contain % args or %%, use strcpy.  */
  if (strchr (fmt_str, target_percent) == 0)
    {
      tree fn = implicit_built_in_decls[BUILT_IN_STRCPY];
      tree exp;

      if ((nargs > 2) || ! fn)
	return NULL_RTX;
      expand_expr (build_call_expr (fn, 2, dest, fmt),
		   const0_rtx, VOIDmode, EXPAND_NORMAL);
      if (target == const0_rtx)
	return const0_rtx;
      exp = build_int_cst (NULL_TREE, strlen (fmt_str));
      return expand_expr (exp, target, mode, EXPAND_NORMAL);
    }
  /* If the format is "%s", use strcpy if the result isn't used.  */
  else if (strcmp (fmt_str, target_percent_s) == 0)
    {
      tree fn, arg, len;
      fn = implicit_built_in_decls[BUILT_IN_STRCPY];

      if (! fn)
	return NULL_RTX;
      if (nargs != 3)
	return NULL_RTX;
      arg = CALL_EXPR_ARG (exp, 2);
      if (! POINTER_TYPE_P (TREE_TYPE (arg)))
	return NULL_RTX;

      if (target != const0_rtx)
	{
	  len = c_strlen (arg, 1);
	  if (! len || TREE_CODE (len) != INTEGER_CST)
	    return NULL_RTX;
	}
      else
	len = NULL_TREE;

      expand_expr (build_call_expr (fn, 2, dest, arg),
		   const0_rtx, VOIDmode, EXPAND_NORMAL);

      if (target == const0_rtx)
	return const0_rtx;
      return expand_expr (len, target, mode, EXPAND_NORMAL);
    }

  return NULL_RTX;
}

/* Expand a call to either the entry or exit function profiler.  */

static rtx
expand_builtin_profile_func (bool exitp)
{
  rtx this, which;

  this = DECL_RTL (current_function_decl);
  gcc_assert (MEM_P (this));
  this = XEXP (this, 0);

  if (exitp)
    which = profile_function_exit_libfunc;
  else
    which = profile_function_entry_libfunc;

  emit_library_call (which, LCT_NORMAL, VOIDmode, 2, this, Pmode,
		     expand_builtin_return_addr (BUILT_IN_RETURN_ADDRESS,
						 0),
		     Pmode);

  return const0_rtx;
}

/* Expand a call to __builtin___clear_cache.  */

static rtx
expand_builtin___clear_cache (tree exp ATTRIBUTE_UNUSED)
{
#ifndef HAVE_clear_cache
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
#else
  /* We have a "clear_cache" insn, and it will handle everything.  */
  tree begin, end;
  rtx begin_rtx, end_rtx;
  enum insn_code icode;

  /* We must not expand to a library call.  If we did, any
     fallback library function in libgcc that might contain a call to
     __builtin___clear_cache() would recurse infinitely.  */
  if (!validate_arglist (exp, POINTER_TYPE, POINTER_TYPE, VOID_TYPE))
    {
      error ("both arguments to %<__builtin___clear_cache%> must be pointers");
      return const0_rtx;
    }

  if (HAVE_clear_cache)
    {
      icode = CODE_FOR_clear_cache;

      begin = CALL_EXPR_ARG (exp, 0);
      begin_rtx = expand_expr (begin, NULL_RTX, Pmode, EXPAND_NORMAL);
      begin_rtx = convert_memory_address (Pmode, begin_rtx);
      if (!insn_data[icode].operand[0].predicate (begin_rtx, Pmode))
	begin_rtx = copy_to_mode_reg (Pmode, begin_rtx);

      end = CALL_EXPR_ARG (exp, 1);
      end_rtx = expand_expr (end, NULL_RTX, Pmode, EXPAND_NORMAL);
      end_rtx = convert_memory_address (Pmode, end_rtx);
      if (!insn_data[icode].operand[1].predicate (end_rtx, Pmode))
	end_rtx = copy_to_mode_reg (Pmode, end_rtx);

      emit_insn (gen_clear_cache (begin_rtx, end_rtx));
    }
  return const0_rtx;
#endif /* HAVE_clear_cache */
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
  addend = GEN_INT (TRAMPOLINE_ALIGNMENT / BITS_PER_UNIT - 1);
  mask = GEN_INT (-TRAMPOLINE_ALIGNMENT / BITS_PER_UNIT);

  temp  = expand_simple_binop (Pmode, PLUS, tramp, addend,
			       temp, 0, OPTAB_LIB_WIDEN);
  tramp = expand_simple_binop (Pmode, AND, temp, mask,
			       temp, 0, OPTAB_LIB_WIDEN);

  return tramp;
}

static rtx
expand_builtin_init_trampoline (tree exp)
{
  tree t_tramp, t_func, t_chain;
  rtx r_tramp, r_func, r_chain;
#ifdef TRAMPOLINE_TEMPLATE
  rtx blktramp;
#endif

  if (!validate_arglist (exp, POINTER_TYPE, POINTER_TYPE,
			 POINTER_TYPE, VOID_TYPE))
    return NULL_RTX;

  t_tramp = CALL_EXPR_ARG (exp, 0);
  t_func = CALL_EXPR_ARG (exp, 1);
  t_chain = CALL_EXPR_ARG (exp, 2);

  r_tramp = expand_normal (t_tramp);
  r_func = expand_normal (t_func);
  r_chain = expand_normal (t_chain);

  /* Generate insns to initialize the trampoline.  */
  r_tramp = round_trampoline_addr (r_tramp);
#ifdef TRAMPOLINE_TEMPLATE
  blktramp = gen_rtx_MEM (BLKmode, r_tramp);
  set_mem_align (blktramp, TRAMPOLINE_ALIGNMENT);
  emit_block_move (blktramp, assemble_trampoline_template (),
		   GEN_INT (TRAMPOLINE_SIZE), BLOCK_OP_NORMAL);
#endif
  trampolines_created = 1;
  INITIALIZE_TRAMPOLINE (r_tramp, r_func, r_chain);

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
#ifdef TRAMPOLINE_ADJUST_ADDRESS
  TRAMPOLINE_ADJUST_ADDRESS (tramp);
#endif

  return tramp;
}

/* Expand the call EXP to the built-in signbit, signbitf or signbitl
   function.  The function first checks whether the back end provides
   an insn to implement signbit for the respective mode.  If not, it
   checks whether the floating point format of the value is such that
   the sign bit can be extracted.  If that is not the case, the
   function returns NULL_RTX to indicate that a normal call should be
   emitted rather than expanding the function in-line.  EXP is the
   expression that is a call to the builtin function; if convenient,
   the result should be placed in TARGET.  */
static rtx
expand_builtin_signbit (tree exp, rtx target)
{
  const struct real_format *fmt;
  enum machine_mode fmode, imode, rmode;
  HOST_WIDE_INT hi, lo;
  tree arg;
  int word, bitpos;
  enum insn_code icode;
  rtx temp;

  if (!validate_arglist (exp, REAL_TYPE, VOID_TYPE))
    return NULL_RTX;

  arg = CALL_EXPR_ARG (exp, 0);
  fmode = TYPE_MODE (TREE_TYPE (arg));
  rmode = TYPE_MODE (TREE_TYPE (exp));
  fmt = REAL_MODE_FORMAT (fmode);

  arg = builtin_save_expr (arg);

  /* Expand the argument yielding a RTX expression. */
  temp = expand_normal (arg);

  /* Check if the back end provides an insn that handles signbit for the
     argument's mode. */
  icode = signbit_optab->handlers [(int) fmode].insn_code;
  if (icode != CODE_FOR_nothing)
    {
      target = gen_reg_rtx (TYPE_MODE (TREE_TYPE (exp)));
      emit_unop_insn (icode, target, temp, UNKNOWN);
      return target;
    }

  /* For floating point formats without a sign bit, implement signbit
     as "ARG < 0.0".  */
  bitpos = fmt->signbit_ro;
  if (bitpos < 0)
  {
    /* But we can't do this if the format supports signed zero.  */
    if (fmt->has_signed_zero && HONOR_SIGNED_ZEROS (fmode))
      return NULL_RTX;

    arg = fold_build2 (LT_EXPR, TREE_TYPE (exp), arg,
		       build_real (TREE_TYPE (arg), dconst0));
    return expand_expr (arg, target, VOIDmode, EXPAND_NORMAL);
  }

  if (GET_MODE_SIZE (fmode) <= UNITS_PER_WORD)
    {
      imode = int_mode_for_mode (fmode);
      if (imode == BLKmode)
	return NULL_RTX;
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
      if (bitpos < HOST_BITS_PER_WIDE_INT)
	{
	  hi = 0;
	  lo = (HOST_WIDE_INT) 1 << bitpos;
	}
      else
	{
	  hi = (HOST_WIDE_INT) 1 << (bitpos - HOST_BITS_PER_WIDE_INT);
	  lo = 0;
	}

      if (GET_MODE_SIZE (imode) > GET_MODE_SIZE (rmode))
	temp = gen_lowpart (rmode, temp);
      temp = expand_binop (rmode, and_optab, temp,
			   immed_double_const (lo, hi, rmode),
			   NULL_RTX, 1, OPTAB_LIB_WIDEN);
    }
  else
    {
      /* Perform a logical right shift to place the signbit in the least
	 significant bit, then truncate the result to the desired mode
	 and mask just this bit.  */
      temp = expand_shift (RSHIFT_EXPR, imode, temp,
			   build_int_cst (NULL_TREE, bitpos), NULL_RTX, 1);
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

  decl = build_decl (FUNCTION_DECL, id, TREE_TYPE (fn));
  DECL_EXTERNAL (decl) = 1;
  TREE_PUBLIC (decl) = 1;
  DECL_ARTIFICIAL (decl) = 1;
  TREE_NOTHROW (decl) = 1;
  DECL_VISIBILITY (decl) = VISIBILITY_DEFAULT;
  DECL_VISIBILITY_SPECIFIED (decl) = 1;
  call = rewrite_call_expr (exp, 0, decl, 0);
  return expand_call (call, target, ignore);
 }
  


/* Reconstitute a mode for a __sync intrinsic operation.  Since the type of
   the pointer in these functions is void*, the tree optimizers may remove
   casts.  The mode computed in expand_builtin isn't reliable either, due
   to __sync_bool_compare_and_swap.

   FCODE_DIFF should be fcode - base, where base is the FOO_1 code for the
   group of builtins.  This gives us log2 of the mode size.  */

static inline enum machine_mode
get_builtin_sync_mode (int fcode_diff)
{
  /* The size is not negotiable, so ask not to get BLKmode in return
     if the target indicates that a smaller size would be better.  */
  return mode_for_size (BITS_PER_UNIT << fcode_diff, MODE_INT, 0);
}

/* Expand the memory expression LOC and return the appropriate memory operand
   for the builtin_sync operations.  */

static rtx
get_builtin_sync_mem (tree loc, enum machine_mode mode)
{
  rtx addr, mem;

  addr = expand_expr (loc, NULL_RTX, Pmode, EXPAND_SUM);

  /* Note that we explicitly do not want any alias information for this
     memory, so that we kill all other live memories.  Otherwise we don't
     satisfy the full barrier semantics of the intrinsic.  */
  mem = validize_mem (gen_rtx_MEM (mode, addr));

  set_mem_align (mem, get_pointer_alignment (loc, BIGGEST_ALIGNMENT));
  set_mem_alias_set (mem, ALIAS_SET_MEMORY_BARRIER);
  MEM_VOLATILE_P (mem) = 1;

  return mem;
}

/* Expand the __sync_xxx_and_fetch and __sync_fetch_and_xxx intrinsics.
   EXP is the CALL_EXPR.  CODE is the rtx code
   that corresponds to the arithmetic or logical operation from the name;
   an exception here is that NOT actually means NAND.  TARGET is an optional
   place for us to store the results; AFTER is true if this is the
   fetch_and_xxx form.  IGNORE is true if we don't actually care about
   the result of the operation at all.  */

static rtx
expand_builtin_sync_operation (enum machine_mode mode, tree exp,
			       enum rtx_code code, bool after,
			       rtx target, bool ignore)
{
  rtx val, mem;
  enum machine_mode old_mode;

  /* Expand the operands.  */
  mem = get_builtin_sync_mem (CALL_EXPR_ARG (exp, 0), mode);

  val = expand_expr (CALL_EXPR_ARG (exp, 1), NULL_RTX, mode, EXPAND_NORMAL);
  /* If VAL is promoted to a wider mode, convert it back to MODE.  Take care
     of CONST_INTs, where we know the old_mode only from the call argument.  */
  old_mode = GET_MODE (val);
  if (old_mode == VOIDmode)
    old_mode = TYPE_MODE (TREE_TYPE (CALL_EXPR_ARG (exp, 1)));
  val = convert_modes (mode, old_mode, val, 1);

  if (ignore)
    return expand_sync_operation (mem, val, code);
  else
    return expand_sync_fetch_operation (mem, val, code, after, target);
}

/* Expand the __sync_val_compare_and_swap and __sync_bool_compare_and_swap
   intrinsics. EXP is the CALL_EXPR.  IS_BOOL is
   true if this is the boolean form.  TARGET is a place for us to store the
   results; this is NOT optional if IS_BOOL is true.  */

static rtx
expand_builtin_compare_and_swap (enum machine_mode mode, tree exp,
				 bool is_bool, rtx target)
{
  rtx old_val, new_val, mem;
  enum machine_mode old_mode;

  /* Expand the operands.  */
  mem = get_builtin_sync_mem (CALL_EXPR_ARG (exp, 0), mode);


  old_val = expand_expr (CALL_EXPR_ARG (exp, 1), NULL_RTX,
			 mode, EXPAND_NORMAL);
  /* If VAL is promoted to a wider mode, convert it back to MODE.  Take care
     of CONST_INTs, where we know the old_mode only from the call argument.  */
  old_mode = GET_MODE (old_val);
  if (old_mode == VOIDmode)
    old_mode = TYPE_MODE (TREE_TYPE (CALL_EXPR_ARG (exp, 1)));
  old_val = convert_modes (mode, old_mode, old_val, 1);

  new_val = expand_expr (CALL_EXPR_ARG (exp, 2), NULL_RTX,
			 mode, EXPAND_NORMAL);
  /* If VAL is promoted to a wider mode, convert it back to MODE.  Take care
     of CONST_INTs, where we know the old_mode only from the call argument.  */
  old_mode = GET_MODE (new_val);
  if (old_mode == VOIDmode)
    old_mode = TYPE_MODE (TREE_TYPE (CALL_EXPR_ARG (exp, 2)));
  new_val = convert_modes (mode, old_mode, new_val, 1);

  if (is_bool)
    return expand_bool_compare_and_swap (mem, old_val, new_val, target);
  else
    return expand_val_compare_and_swap (mem, old_val, new_val, target);
}

/* Expand the __sync_lock_test_and_set intrinsic.  Note that the most
   general form is actually an atomic exchange, and some targets only
   support a reduced form with the second argument being a constant 1.
   EXP is the CALL_EXPR; TARGET is an optional place for us to store 
   the results.  */

static rtx
expand_builtin_lock_test_and_set (enum machine_mode mode, tree exp,
				  rtx target)
{
  rtx val, mem;
  enum machine_mode old_mode;

  /* Expand the operands.  */
  mem = get_builtin_sync_mem (CALL_EXPR_ARG (exp, 0), mode);
  val = expand_expr (CALL_EXPR_ARG (exp, 1), NULL_RTX, mode, EXPAND_NORMAL);
  /* If VAL is promoted to a wider mode, convert it back to MODE.  Take care
     of CONST_INTs, where we know the old_mode only from the call argument.  */
  old_mode = GET_MODE (val);
  if (old_mode == VOIDmode)
    old_mode = TYPE_MODE (TREE_TYPE (CALL_EXPR_ARG (exp, 1)));
  val = convert_modes (mode, old_mode, val, 1);

  return expand_sync_lock_test_and_set (mem, val, target);
}

/* Expand the __sync_synchronize intrinsic.  */

static void
expand_builtin_synchronize (void)
{
  tree x;

#ifdef HAVE_memory_barrier
  if (HAVE_memory_barrier)
    {
      emit_insn (gen_memory_barrier ());
      return;
    }
#endif

  /* If no explicit memory barrier instruction is available, create an
     empty asm stmt with a memory clobber.  */
  x = build4 (ASM_EXPR, void_type_node, build_string (0, ""), NULL, NULL,
	      tree_cons (NULL, build_string (6, "memory"), NULL));
  ASM_VOLATILE_P (x) = 1;
  expand_asm_expr (x);
}

/* Expand the __sync_lock_release intrinsic.  EXP is the CALL_EXPR.  */

static void
expand_builtin_lock_release (enum machine_mode mode, tree exp)
{
  enum insn_code icode;
  rtx mem, insn;
  rtx val = const0_rtx;

  /* Expand the operands.  */
  mem = get_builtin_sync_mem (CALL_EXPR_ARG (exp, 0), mode);

  /* If there is an explicit operation in the md file, use it.  */
  icode = sync_lock_release[mode];
  if (icode != CODE_FOR_nothing)
    {
      if (!insn_data[icode].operand[1].predicate (val, mode))
	val = force_reg (mode, val);

      insn = GEN_FCN (icode) (mem, val);
      if (insn)
	{
	  emit_insn (insn);
	  return;
	}
    }

  /* Otherwise we can implement this operation by emitting a barrier
     followed by a store of zero.  */
  expand_builtin_synchronize ();
  emit_move_insn (mem, val);
}

/* Expand an expression EXP that calls a built-in function,
   with result going to TARGET if that's convenient
   (and in mode MODE if that's convenient).
   SUBTARGET may be used as the target for computing one of EXP's operands.
   IGNORE is nonzero if the value is to be ignored.  */

rtx
expand_builtin (tree exp, rtx target, rtx subtarget, enum machine_mode mode,
		int ignore)
{
  tree fndecl = get_callee_fndecl (exp);
  enum built_in_function fcode = DECL_FUNCTION_CODE (fndecl);
  enum machine_mode target_mode = TYPE_MODE (TREE_TYPE (exp));

  if (DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_MD)
    return targetm.expand_builtin (exp, target, subtarget, mode, ignore);

  /* When not optimizing, generate calls to library functions for a certain
     set of builtins.  */
  if (!optimize
      && !called_as_built_in (fndecl)
      && DECL_ASSEMBLER_NAME_SET_P (fndecl)
      && fcode != BUILT_IN_ALLOCA)
    return expand_call (exp, target, ignore);

  /* The built-in function expanders test for target == const0_rtx
     to determine whether the function's result will be ignored.  */
  if (ignore)
    target = const0_rtx;

  /* If the result of a pure or const built-in function is ignored, and
     none of its arguments are volatile, we can avoid expanding the
     built-in call and just evaluate the arguments for side-effects.  */
  if (target == const0_rtx
      && (DECL_IS_PURE (fndecl) || TREE_READONLY (fndecl)))
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

  switch (fcode)
    {
    CASE_FLT_FN (BUILT_IN_FABS):
      target = expand_builtin_fabs (exp, target, subtarget);
      if (target)
	return target;
      break;

    CASE_FLT_FN (BUILT_IN_COPYSIGN):
      target = expand_builtin_copysign (exp, target, subtarget);
      if (target)
	return target;
      break;

      /* Just do a normal library call if we were unable to fold
	 the values.  */
    CASE_FLT_FN (BUILT_IN_CABS):
      break;

    CASE_FLT_FN (BUILT_IN_EXP):
    CASE_FLT_FN (BUILT_IN_EXP10):
    CASE_FLT_FN (BUILT_IN_POW10):
    CASE_FLT_FN (BUILT_IN_EXP2):
    CASE_FLT_FN (BUILT_IN_EXPM1):
    CASE_FLT_FN (BUILT_IN_LOGB):
    CASE_FLT_FN (BUILT_IN_LOG):
    CASE_FLT_FN (BUILT_IN_LOG10):
    CASE_FLT_FN (BUILT_IN_LOG2):
    CASE_FLT_FN (BUILT_IN_LOG1P):
    CASE_FLT_FN (BUILT_IN_TAN):
    CASE_FLT_FN (BUILT_IN_ASIN):
    CASE_FLT_FN (BUILT_IN_ACOS):
    CASE_FLT_FN (BUILT_IN_ATAN):
      /* Treat these like sqrt only if unsafe math optimizations are allowed,
	 because of possible accuracy problems.  */
      if (! flag_unsafe_math_optimizations)
	break;
    CASE_FLT_FN (BUILT_IN_SQRT):
    CASE_FLT_FN (BUILT_IN_FLOOR):
    CASE_FLT_FN (BUILT_IN_CEIL):
    CASE_FLT_FN (BUILT_IN_TRUNC):
    CASE_FLT_FN (BUILT_IN_ROUND):
    CASE_FLT_FN (BUILT_IN_NEARBYINT):
    CASE_FLT_FN (BUILT_IN_RINT):
      target = expand_builtin_mathfn (exp, target, subtarget);
      if (target)
	return target;
      break;

    CASE_FLT_FN (BUILT_IN_ILOGB):
      if (! flag_unsafe_math_optimizations)
	break;
    CASE_FLT_FN (BUILT_IN_ISINF):
    CASE_FLT_FN (BUILT_IN_FINITE):
    case BUILT_IN_ISFINITE:
    case BUILT_IN_ISNORMAL:
      target = expand_builtin_interclass_mathfn (exp, target, subtarget);
      if (target)
	return target;
      break;

    CASE_FLT_FN (BUILT_IN_LCEIL):
    CASE_FLT_FN (BUILT_IN_LLCEIL):
    CASE_FLT_FN (BUILT_IN_LFLOOR):
    CASE_FLT_FN (BUILT_IN_LLFLOOR):
      target = expand_builtin_int_roundingfn (exp, target, subtarget);
      if (target)
	return target;
      break;

    CASE_FLT_FN (BUILT_IN_LRINT):
    CASE_FLT_FN (BUILT_IN_LLRINT):
    CASE_FLT_FN (BUILT_IN_LROUND):
    CASE_FLT_FN (BUILT_IN_LLROUND):
      target = expand_builtin_int_roundingfn_2 (exp, target, subtarget);
      if (target)
	return target;
      break;

    CASE_FLT_FN (BUILT_IN_POW):
      target = expand_builtin_pow (exp, target, subtarget);
      if (target)
	return target;
      break;

    CASE_FLT_FN (BUILT_IN_POWI):
      target = expand_builtin_powi (exp, target, subtarget);
      if (target)
	return target;
      break;

    CASE_FLT_FN (BUILT_IN_ATAN2):
    CASE_FLT_FN (BUILT_IN_LDEXP):
    CASE_FLT_FN (BUILT_IN_SCALB):
    CASE_FLT_FN (BUILT_IN_SCALBN):
    CASE_FLT_FN (BUILT_IN_SCALBLN):
      if (! flag_unsafe_math_optimizations)
	break;

    CASE_FLT_FN (BUILT_IN_FMOD):
    CASE_FLT_FN (BUILT_IN_REMAINDER):
    CASE_FLT_FN (BUILT_IN_DREM):
      target = expand_builtin_mathfn_2 (exp, target, subtarget);
      if (target)
	return target;
      break;

    CASE_FLT_FN (BUILT_IN_CEXPI):
      target = expand_builtin_cexpi (exp, target, subtarget);
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

    case BUILT_IN_ARGS_INFO:
      return expand_builtin_args_info (exp);

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
      target = expand_builtin_alloca (exp, target);
      if (target)
	return target;
      break;

    case BUILT_IN_STACK_SAVE:
      return expand_stack_save ();

    case BUILT_IN_STACK_RESTORE:
      expand_stack_restore (CALL_EXPR_ARG (exp, 0));
      return const0_rtx;

    case BUILT_IN_BSWAP32:
    case BUILT_IN_BSWAP64:
      target = expand_builtin_bswap (exp, target, subtarget);

      if (target)
	return target;
      break;

    CASE_INT_FN (BUILT_IN_FFS):
    case BUILT_IN_FFSIMAX:
      target = expand_builtin_unop (target_mode, exp, target,
				    subtarget, ffs_optab);
      if (target)
	return target;
      break;

    CASE_INT_FN (BUILT_IN_CLZ):
    case BUILT_IN_CLZIMAX:
      target = expand_builtin_unop (target_mode, exp, target,
				    subtarget, clz_optab);
      if (target)
	return target;
      break;

    CASE_INT_FN (BUILT_IN_CTZ):
    case BUILT_IN_CTZIMAX:
      target = expand_builtin_unop (target_mode, exp, target,
				    subtarget, ctz_optab);
      if (target)
	return target;
      break;

    CASE_INT_FN (BUILT_IN_POPCOUNT):
    case BUILT_IN_POPCOUNTIMAX:
      target = expand_builtin_unop (target_mode, exp, target,
				    subtarget, popcount_optab);
      if (target)
	return target;
      break;

    CASE_INT_FN (BUILT_IN_PARITY):
    case BUILT_IN_PARITYIMAX:
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

    case BUILT_IN_STRCPY:
      target = expand_builtin_strcpy (fndecl, exp, target, mode);
      if (target)
	return target;
      break;

    case BUILT_IN_STRNCPY:
      target = expand_builtin_strncpy (exp, target, mode);
      if (target)
	return target;
      break;

    case BUILT_IN_STPCPY:
      target = expand_builtin_stpcpy (exp, target, mode);
      if (target)
	return target;
      break;

    case BUILT_IN_STRCAT:
      target = expand_builtin_strcat (fndecl, exp, target, mode);
      if (target)
	return target;
      break;

    case BUILT_IN_STRNCAT:
      target = expand_builtin_strncat (exp, target, mode);
      if (target)
	return target;
      break;

    case BUILT_IN_STRSPN:
      target = expand_builtin_strspn (exp, target, mode);
      if (target)
	return target;
      break;

    case BUILT_IN_STRCSPN:
      target = expand_builtin_strcspn (exp, target, mode);
      if (target)
	return target;
      break;

    case BUILT_IN_STRSTR:
      target = expand_builtin_strstr (exp, target, mode);
      if (target)
	return target;
      break;

    case BUILT_IN_STRPBRK:
      target = expand_builtin_strpbrk (exp, target, mode);
      if (target)
	return target;
      break;

    case BUILT_IN_INDEX:
    case BUILT_IN_STRCHR:
      target = expand_builtin_strchr (exp, target, mode);
      if (target)
	return target;
      break;

    case BUILT_IN_RINDEX:
    case BUILT_IN_STRRCHR:
      target = expand_builtin_strrchr (exp, target, mode);
      if (target)
	return target;
      break;

    case BUILT_IN_MEMCPY:
      target = expand_builtin_memcpy (exp, target, mode);
      if (target)
	return target;
      break;

    case BUILT_IN_MEMPCPY:
      target = expand_builtin_mempcpy (exp, target, mode);
      if (target)
	return target;
      break;

    case BUILT_IN_MEMMOVE:
      target = expand_builtin_memmove (exp, target, mode, ignore);
      if (target)
	return target;
      break;

    case BUILT_IN_BCOPY:
      target = expand_builtin_bcopy (exp, ignore);
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
      target = expand_builtin_strcmp (exp, target, mode);
      if (target)
	return target;
      break;

    case BUILT_IN_STRNCMP:
      target = expand_builtin_strncmp (exp, target, mode);
      if (target)
	return target;
      break;

    case BUILT_IN_MEMCHR:
      target = expand_builtin_memchr (exp, target, mode);
      if (target)
	return target;
      break;

    case BUILT_IN_BCMP:
    case BUILT_IN_MEMCMP:
      target = expand_builtin_memcmp (exp, target, mode);
      if (target)
	return target;
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
	  rtx label_r = label_rtx (label);

	  /* This is copied from the handling of non-local gotos.  */
	  expand_builtin_setjmp_setup (buf_addr, label_r);
	  nonlocal_goto_handler_labels
	    = gen_rtx_EXPR_LIST (VOIDmode, label_r,
				 nonlocal_goto_handler_labels);
	  /* ??? Do not let expand_label treat us as such since we would
	     not want to be both on the list of non-local labels and on
	     the list of forced labels.  */
	  FORCED_LABEL (label) = 0;
	  return const0_rtx;
	}
      break;

    case BUILT_IN_SETJMP_DISPATCHER:
       /* __builtin_setjmp_dispatcher is passed the dispatcher label.  */
      if (validate_arglist (exp, POINTER_TYPE, VOID_TYPE))
	{
	  tree label = TREE_OPERAND (CALL_EXPR_ARG (exp, 0), 0);
	  rtx label_r = label_rtx (label);

	  /* Remove the dispatcher label from the list of non-local labels
	     since the receiver labels have been added to it above.  */
	  remove_node_from_expr_list (label_r, &nonlocal_goto_handler_labels);
	  return const0_rtx;
	}
      break;

    case BUILT_IN_SETJMP_RECEIVER:
       /* __builtin_setjmp_receiver is passed the receiver label.  */
      if (validate_arglist (exp, POINTER_TYPE, VOID_TYPE))
	{
	  tree label = TREE_OPERAND (CALL_EXPR_ARG (exp, 0), 0);
	  rtx label_r = label_rtx (label);

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

    case BUILT_IN_PRINTF:
      target = expand_builtin_printf (exp, target, mode, false);
      if (target)
	return target;
      break;

    case BUILT_IN_PRINTF_UNLOCKED:
      target = expand_builtin_printf (exp, target, mode, true);
      if (target)
	return target;
      break;

    case BUILT_IN_FPUTS:
      target = expand_builtin_fputs (exp, target, false);
      if (target)
	return target;
      break;
    case BUILT_IN_FPUTS_UNLOCKED:
      target = expand_builtin_fputs (exp, target, true);
      if (target)
	return target;
      break;

    case BUILT_IN_FPRINTF:
      target = expand_builtin_fprintf (exp, target, mode, false);
      if (target)
	return target;
      break;

    case BUILT_IN_FPRINTF_UNLOCKED:
      target = expand_builtin_fprintf (exp, target, mode, true);
      if (target)
	return target;
      break;

    case BUILT_IN_SPRINTF:
      target = expand_builtin_sprintf (exp, target, mode);
      if (target)
	return target;
      break;

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
#ifdef EH_RETURN_DATA_REGNO
    case BUILT_IN_EH_RETURN_DATA_REGNO:
      return expand_builtin_eh_return_data_regno (exp);
#endif
    case BUILT_IN_EXTEND_POINTER:
      return expand_builtin_extend_pointer (CALL_EXPR_ARG (exp, 0));

    case BUILT_IN_VA_START:
    case BUILT_IN_STDARG_START:
      return expand_builtin_va_start (exp);
    case BUILT_IN_VA_END:
      return expand_builtin_va_end (exp);
    case BUILT_IN_VA_COPY:
      return expand_builtin_va_copy (exp);
    case BUILT_IN_EXPECT:
      return expand_builtin_expect (exp, target);
    case BUILT_IN_PREFETCH:
      expand_builtin_prefetch (exp);
      return const0_rtx;

    case BUILT_IN_PROFILE_FUNC_ENTER:
      return expand_builtin_profile_func (false);
    case BUILT_IN_PROFILE_FUNC_EXIT:
      return expand_builtin_profile_func (true);

    case BUILT_IN_INIT_TRAMPOLINE:
      return expand_builtin_init_trampoline (exp);
    case BUILT_IN_ADJUST_TRAMPOLINE:
      return expand_builtin_adjust_trampoline (exp);

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

    case BUILT_IN_FETCH_AND_ADD_1:
    case BUILT_IN_FETCH_AND_ADD_2:
    case BUILT_IN_FETCH_AND_ADD_4:
    case BUILT_IN_FETCH_AND_ADD_8:
    case BUILT_IN_FETCH_AND_ADD_16:
      mode = get_builtin_sync_mode (fcode - BUILT_IN_FETCH_AND_ADD_1);
      target = expand_builtin_sync_operation (mode, exp, PLUS,
					      false, target, ignore);
      if (target)
	return target;
      break;

    case BUILT_IN_FETCH_AND_SUB_1:
    case BUILT_IN_FETCH_AND_SUB_2:
    case BUILT_IN_FETCH_AND_SUB_4:
    case BUILT_IN_FETCH_AND_SUB_8:
    case BUILT_IN_FETCH_AND_SUB_16:
      mode = get_builtin_sync_mode (fcode - BUILT_IN_FETCH_AND_SUB_1);
      target = expand_builtin_sync_operation (mode, exp, MINUS,
					      false, target, ignore);
      if (target)
	return target;
      break;

    case BUILT_IN_FETCH_AND_OR_1:
    case BUILT_IN_FETCH_AND_OR_2:
    case BUILT_IN_FETCH_AND_OR_4:
    case BUILT_IN_FETCH_AND_OR_8:
    case BUILT_IN_FETCH_AND_OR_16:
      mode = get_builtin_sync_mode (fcode - BUILT_IN_FETCH_AND_OR_1);
      target = expand_builtin_sync_operation (mode, exp, IOR,
					      false, target, ignore);
      if (target)
	return target;
      break;

    case BUILT_IN_FETCH_AND_AND_1:
    case BUILT_IN_FETCH_AND_AND_2:
    case BUILT_IN_FETCH_AND_AND_4:
    case BUILT_IN_FETCH_AND_AND_8:
    case BUILT_IN_FETCH_AND_AND_16:
      mode = get_builtin_sync_mode (fcode - BUILT_IN_FETCH_AND_AND_1);
      target = expand_builtin_sync_operation (mode, exp, AND,
					      false, target, ignore);
      if (target)
	return target;
      break;

    case BUILT_IN_FETCH_AND_XOR_1:
    case BUILT_IN_FETCH_AND_XOR_2:
    case BUILT_IN_FETCH_AND_XOR_4:
    case BUILT_IN_FETCH_AND_XOR_8:
    case BUILT_IN_FETCH_AND_XOR_16:
      mode = get_builtin_sync_mode (fcode - BUILT_IN_FETCH_AND_XOR_1);
      target = expand_builtin_sync_operation (mode, exp, XOR,
					      false, target, ignore);
      if (target)
	return target;
      break;

    case BUILT_IN_FETCH_AND_NAND_1:
    case BUILT_IN_FETCH_AND_NAND_2:
    case BUILT_IN_FETCH_AND_NAND_4:
    case BUILT_IN_FETCH_AND_NAND_8:
    case BUILT_IN_FETCH_AND_NAND_16:
      mode = get_builtin_sync_mode (fcode - BUILT_IN_FETCH_AND_NAND_1);
      target = expand_builtin_sync_operation (mode, exp, NOT,
					      false, target, ignore);
      if (target)
	return target;
      break;

    case BUILT_IN_ADD_AND_FETCH_1:
    case BUILT_IN_ADD_AND_FETCH_2:
    case BUILT_IN_ADD_AND_FETCH_4:
    case BUILT_IN_ADD_AND_FETCH_8:
    case BUILT_IN_ADD_AND_FETCH_16:
      mode = get_builtin_sync_mode (fcode - BUILT_IN_ADD_AND_FETCH_1);
      target = expand_builtin_sync_operation (mode, exp, PLUS,
					      true, target, ignore);
      if (target)
	return target;
      break;

    case BUILT_IN_SUB_AND_FETCH_1:
    case BUILT_IN_SUB_AND_FETCH_2:
    case BUILT_IN_SUB_AND_FETCH_4:
    case BUILT_IN_SUB_AND_FETCH_8:
    case BUILT_IN_SUB_AND_FETCH_16:
      mode = get_builtin_sync_mode (fcode - BUILT_IN_SUB_AND_FETCH_1);
      target = expand_builtin_sync_operation (mode, exp, MINUS,
					      true, target, ignore);
      if (target)
	return target;
      break;

    case BUILT_IN_OR_AND_FETCH_1:
    case BUILT_IN_OR_AND_FETCH_2:
    case BUILT_IN_OR_AND_FETCH_4:
    case BUILT_IN_OR_AND_FETCH_8:
    case BUILT_IN_OR_AND_FETCH_16:
      mode = get_builtin_sync_mode (fcode - BUILT_IN_OR_AND_FETCH_1);
      target = expand_builtin_sync_operation (mode, exp, IOR,
					      true, target, ignore);
      if (target)
	return target;
      break;

    case BUILT_IN_AND_AND_FETCH_1:
    case BUILT_IN_AND_AND_FETCH_2:
    case BUILT_IN_AND_AND_FETCH_4:
    case BUILT_IN_AND_AND_FETCH_8:
    case BUILT_IN_AND_AND_FETCH_16:
      mode = get_builtin_sync_mode (fcode - BUILT_IN_AND_AND_FETCH_1);
      target = expand_builtin_sync_operation (mode, exp, AND,
					      true, target, ignore);
      if (target)
	return target;
      break;

    case BUILT_IN_XOR_AND_FETCH_1:
    case BUILT_IN_XOR_AND_FETCH_2:
    case BUILT_IN_XOR_AND_FETCH_4:
    case BUILT_IN_XOR_AND_FETCH_8:
    case BUILT_IN_XOR_AND_FETCH_16:
      mode = get_builtin_sync_mode (fcode - BUILT_IN_XOR_AND_FETCH_1);
      target = expand_builtin_sync_operation (mode, exp, XOR,
					      true, target, ignore);
      if (target)
	return target;
      break;

    case BUILT_IN_NAND_AND_FETCH_1:
    case BUILT_IN_NAND_AND_FETCH_2:
    case BUILT_IN_NAND_AND_FETCH_4:
    case BUILT_IN_NAND_AND_FETCH_8:
    case BUILT_IN_NAND_AND_FETCH_16:
      mode = get_builtin_sync_mode (fcode - BUILT_IN_NAND_AND_FETCH_1);
      target = expand_builtin_sync_operation (mode, exp, NOT,
					      true, target, ignore);
      if (target)
	return target;
      break;

    case BUILT_IN_BOOL_COMPARE_AND_SWAP_1:
    case BUILT_IN_BOOL_COMPARE_AND_SWAP_2:
    case BUILT_IN_BOOL_COMPARE_AND_SWAP_4:
    case BUILT_IN_BOOL_COMPARE_AND_SWAP_8:
    case BUILT_IN_BOOL_COMPARE_AND_SWAP_16:
      if (mode == VOIDmode)
	mode = TYPE_MODE (boolean_type_node);
      if (!target || !register_operand (target, mode))
	target = gen_reg_rtx (mode);

      mode = get_builtin_sync_mode (fcode - BUILT_IN_BOOL_COMPARE_AND_SWAP_1);
      target = expand_builtin_compare_and_swap (mode, exp, true, target);
      if (target)
	return target;
      break;

    case BUILT_IN_VAL_COMPARE_AND_SWAP_1:
    case BUILT_IN_VAL_COMPARE_AND_SWAP_2:
    case BUILT_IN_VAL_COMPARE_AND_SWAP_4:
    case BUILT_IN_VAL_COMPARE_AND_SWAP_8:
    case BUILT_IN_VAL_COMPARE_AND_SWAP_16:
      mode = get_builtin_sync_mode (fcode - BUILT_IN_VAL_COMPARE_AND_SWAP_1);
      target = expand_builtin_compare_and_swap (mode, exp, false, target);
      if (target)
	return target;
      break;

    case BUILT_IN_LOCK_TEST_AND_SET_1:
    case BUILT_IN_LOCK_TEST_AND_SET_2:
    case BUILT_IN_LOCK_TEST_AND_SET_4:
    case BUILT_IN_LOCK_TEST_AND_SET_8:
    case BUILT_IN_LOCK_TEST_AND_SET_16:
      mode = get_builtin_sync_mode (fcode - BUILT_IN_LOCK_TEST_AND_SET_1);
      target = expand_builtin_lock_test_and_set (mode, exp, target);
      if (target)
	return target;
      break;

    case BUILT_IN_LOCK_RELEASE_1:
    case BUILT_IN_LOCK_RELEASE_2:
    case BUILT_IN_LOCK_RELEASE_4:
    case BUILT_IN_LOCK_RELEASE_8:
    case BUILT_IN_LOCK_RELEASE_16:
      mode = get_builtin_sync_mode (fcode - BUILT_IN_LOCK_RELEASE_1);
      expand_builtin_lock_release (mode, exp);
      return const0_rtx;

    case BUILT_IN_SYNCHRONIZE:
      expand_builtin_synchronize ();
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

    default:	/* just do library call, if unknown builtin */
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
      || folding_initializer)
    return integer_zero_node;

  return NULL_TREE;
}

/* Create builtin_expect with PRED and EXPECTED as its arguments and
   return it as a truthvalue.  */

static tree
build_builtin_expect_predicate (tree pred, tree expected)
{
  tree fn, arg_types, pred_type, expected_type, call_expr, ret_type;

  fn = built_in_decls[BUILT_IN_EXPECT];
  arg_types = TYPE_ARG_TYPES (TREE_TYPE (fn));
  ret_type = TREE_TYPE (TREE_TYPE (fn));
  pred_type = TREE_VALUE (arg_types);
  expected_type = TREE_VALUE (TREE_CHAIN (arg_types));

  pred = fold_convert (pred_type, pred);
  expected = fold_convert (expected_type, expected);
  call_expr = build_call_expr (fn, 2, pred, expected);

  return build2 (NE_EXPR, TREE_TYPE (pred), call_expr,
		 build_int_cst (ret_type, 0));
}

/* Fold a call to builtin_expect with arguments ARG0 and ARG1.  Return
   NULL_TREE if no simplification is possible.  */

static tree
fold_builtin_expect (tree arg0, tree arg1)
{
  tree inner, fndecl;
  enum tree_code code;

  /* If this is a builtin_expect within a builtin_expect keep the
     inner one.  See through a comparison against a constant.  It
     might have been added to create a thruthvalue.  */
  inner = arg0;
  if (COMPARISON_CLASS_P (inner)
      && TREE_CODE (TREE_OPERAND (inner, 1)) == INTEGER_CST)
    inner = TREE_OPERAND (inner, 0);

  if (TREE_CODE (inner) == CALL_EXPR
      && (fndecl = get_callee_fndecl (inner))
      && DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_NORMAL
      && DECL_FUNCTION_CODE (fndecl) == BUILT_IN_EXPECT)
    return arg0;

  /* Distribute the expected value over short-circuiting operators.
     See through the cast from truthvalue_type_node to long.  */
  inner = arg0;
  while (TREE_CODE (inner) == NOP_EXPR
	 && INTEGRAL_TYPE_P (TREE_TYPE (inner))
	 && INTEGRAL_TYPE_P (TREE_TYPE (TREE_OPERAND (inner, 0))))
    inner = TREE_OPERAND (inner, 0);

  code = TREE_CODE (inner);
  if (code == TRUTH_ANDIF_EXPR || code == TRUTH_ORIF_EXPR)
    {
      tree op0 = TREE_OPERAND (inner, 0);
      tree op1 = TREE_OPERAND (inner, 1);

      op0 = build_builtin_expect_predicate (op0, arg1);
      op1 = build_builtin_expect_predicate (op1, arg1);
      inner = build2 (code, TREE_TYPE (inner), op0, op1);

      return fold_convert (TREE_TYPE (arg0), inner);
    }

  /* If the argument isn't invariant then there's nothing else we can do.  */
  if (!TREE_INVARIANT (arg0))
    return NULL_TREE;

  /* If we expect that a comparison against the argument will fold to
     a constant return the constant.  In practice, this means a true
     constant or the address of a non-weak symbol.  */
  inner = arg0;
  STRIP_NOPS (inner);
  if (TREE_CODE (inner) == ADDR_EXPR)
    {
      do
	{
	  inner = TREE_OPERAND (inner, 0);
	}
      while (TREE_CODE (inner) == COMPONENT_REF
	     || TREE_CODE (inner) == ARRAY_REF);
      if (DECL_P (inner) && DECL_WEAK (inner))
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
    return build_int_cst (NULL_TREE, no_type_class);

  return build_int_cst (NULL_TREE, type_to_class (TREE_TYPE (arg)));
}

/* Fold a call to __builtin_strlen with argument ARG.  */

static tree
fold_builtin_strlen (tree arg)
{
  if (!validate_arg (arg, POINTER_TYPE))
    return NULL_TREE;
  else
    {
      tree len = c_strlen (arg, 0);

      if (len)
	{
	  /* Convert from the internal "sizetype" type to "size_t".  */
	  if (size_type_node)
	    len = fold_convert (size_type_node, len);
	  return len;
	}

      return NULL_TREE;
    }
}

/* Fold a call to __builtin_inf or __builtin_huge_val.  */

static tree
fold_builtin_inf (tree type, int warn)
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
    pedwarn ("target format does not support infinity");

  real_inf (&real);
  return build_real (type, real);
}

/* Fold a call to __builtin_nan or __builtin_nans with argument ARG.  */

static tree
fold_builtin_nan (tree arg, tree type, int quiet)
{
  REAL_VALUE_TYPE real;
  const char *str;

  if (!validate_arg (arg, POINTER_TYPE))
    return NULL_TREE;
  str = c_getstr (arg);
  if (!str)
    return NULL_TREE;

  if (!real_nan (&real, str, quiet, TYPE_MODE (type)))
    return NULL_TREE;

  return build_real (type, real);
}

/* Return true if the floating point expression T has an integer value.
   We also allow +Inf, -Inf and NaN to be considered integer values.  */

static bool
integer_valued_real_p (tree t)
{
  switch (TREE_CODE (t))
    {
    case FLOAT_EXPR:
      return true;

    case ABS_EXPR:
    case SAVE_EXPR:
    case NON_LVALUE_EXPR:
      return integer_valued_real_p (TREE_OPERAND (t, 0));

    case COMPOUND_EXPR:
    case MODIFY_EXPR:
    case BIND_EXPR:
      return integer_valued_real_p (GENERIC_TREE_OPERAND (t, 1));

    case PLUS_EXPR:
    case MINUS_EXPR:
    case MULT_EXPR:
    case MIN_EXPR:
    case MAX_EXPR:
      return integer_valued_real_p (TREE_OPERAND (t, 0))
	     && integer_valued_real_p (TREE_OPERAND (t, 1));

    case COND_EXPR:
      return integer_valued_real_p (TREE_OPERAND (t, 1))
	     && integer_valued_real_p (TREE_OPERAND (t, 2));

    case REAL_CST:
      return real_isinteger (TREE_REAL_CST_PTR (t), TYPE_MODE (TREE_TYPE (t)));

    case NOP_EXPR:
      {
	tree type = TREE_TYPE (TREE_OPERAND (t, 0));
	if (TREE_CODE (type) == INTEGER_TYPE)
	  return true;
	if (TREE_CODE (type) == REAL_TYPE)
	  return integer_valued_real_p (TREE_OPERAND (t, 0));
	break;
      }

    case CALL_EXPR:
      switch (builtin_mathfn_code (t))
	{
	CASE_FLT_FN (BUILT_IN_CEIL):
	CASE_FLT_FN (BUILT_IN_FLOOR):
	CASE_FLT_FN (BUILT_IN_NEARBYINT):
	CASE_FLT_FN (BUILT_IN_RINT):
	CASE_FLT_FN (BUILT_IN_ROUND):
	CASE_FLT_FN (BUILT_IN_TRUNC):
	  return true;

	CASE_FLT_FN (BUILT_IN_FMIN):
	CASE_FLT_FN (BUILT_IN_FMAX):
	  return integer_valued_real_p (CALL_EXPR_ARG (t, 0))
 	    && integer_valued_real_p (CALL_EXPR_ARG (t, 1));

	default:
	  break;
	}
      break;

    default:
      break;
    }
  return false;
}

/* FNDECL is assumed to be a builtin where truncation can be propagated
   across (for instance floor((double)f) == (double)floorf (f).
   Do the transformation for a call with argument ARG.  */

static tree
fold_trunc_transparent_mathfn (tree fndecl, tree arg)
{
  enum built_in_function fcode = DECL_FUNCTION_CODE (fndecl);

  if (!validate_arg (arg, REAL_TYPE))
    return NULL_TREE;

  /* Integer rounding functions are idempotent.  */
  if (fcode == builtin_mathfn_code (arg))
    return arg;

  /* If argument is already integer valued, and we don't need to worry
     about setting errno, there's no need to perform rounding.  */
  if (! flag_errno_math && integer_valued_real_p (arg))
    return arg;

  if (optimize)
    {
      tree arg0 = strip_float_extensions (arg);
      tree ftype = TREE_TYPE (TREE_TYPE (fndecl));
      tree newtype = TREE_TYPE (arg0);
      tree decl;

      if (TYPE_PRECISION (newtype) < TYPE_PRECISION (ftype)
	  && (decl = mathfn_built_in (newtype, fcode)))
	return fold_convert (ftype,
			     build_call_expr (decl, 1,
					      fold_convert (newtype, arg0)));
    }
  return NULL_TREE;
}

/* FNDECL is assumed to be builtin which can narrow the FP type of
   the argument, for instance lround((double)f) -> lroundf (f).
   Do the transformation for a call with argument ARG.  */

static tree
fold_fixed_mathfn (tree fndecl, tree arg)
{
  enum built_in_function fcode = DECL_FUNCTION_CODE (fndecl);

  if (!validate_arg (arg, REAL_TYPE))
    return NULL_TREE;

  /* If argument is already integer valued, and we don't need to worry
     about setting errno, there's no need to perform rounding.  */
  if (! flag_errno_math && integer_valued_real_p (arg))
    return fold_build1 (FIX_TRUNC_EXPR, TREE_TYPE (TREE_TYPE (fndecl)), arg);

  if (optimize)
    {
      tree ftype = TREE_TYPE (arg);
      tree arg0 = strip_float_extensions (arg);
      tree newtype = TREE_TYPE (arg0);
      tree decl;

      if (TYPE_PRECISION (newtype) < TYPE_PRECISION (ftype)
	  && (decl = mathfn_built_in (newtype, fcode)))
	return build_call_expr (decl, 1, fold_convert (newtype, arg0));
    }

  /* Canonicalize llround (x) to lround (x) on LP64 targets where
     sizeof (long long) == sizeof (long).  */
  if (TYPE_PRECISION (long_long_integer_type_node)
      == TYPE_PRECISION (long_integer_type_node))
    {
      tree newfn = NULL_TREE;
      switch (fcode)
	{
	CASE_FLT_FN (BUILT_IN_LLCEIL):
	  newfn = mathfn_built_in (TREE_TYPE (arg), BUILT_IN_LCEIL);
	  break;

	CASE_FLT_FN (BUILT_IN_LLFLOOR):
	  newfn = mathfn_built_in (TREE_TYPE (arg), BUILT_IN_LFLOOR);
	  break;

	CASE_FLT_FN (BUILT_IN_LLROUND):
	  newfn = mathfn_built_in (TREE_TYPE (arg), BUILT_IN_LROUND);
	  break;

	CASE_FLT_FN (BUILT_IN_LLRINT):
	  newfn = mathfn_built_in (TREE_TYPE (arg), BUILT_IN_LRINT);
	  break;

	default:
	  break;
	}

      if (newfn)
	{
	  tree newcall = build_call_expr(newfn, 1, arg);
	  return fold_convert (TREE_TYPE (TREE_TYPE (fndecl)), newcall);
	}
    }

  return NULL_TREE;
}

/* Fold call to builtin cabs, cabsf or cabsl with argument ARG.  TYPE is the
   return type.  Return NULL_TREE if no simplification can be made.  */

static tree
fold_builtin_cabs (tree arg, tree type, tree fndecl)
{
  tree res;

  if (TREE_CODE (TREE_TYPE (arg)) != COMPLEX_TYPE
      || TREE_CODE (TREE_TYPE (TREE_TYPE (arg))) != REAL_TYPE)
    return NULL_TREE;

  /* Calculate the result when the argument is a constant.  */
  if (TREE_CODE (arg) == COMPLEX_CST
      && (res = do_mpfr_arg2 (TREE_REALPART (arg), TREE_IMAGPART (arg),
			      type, mpfr_hypot)))
    return res;
  
  if (TREE_CODE (arg) == COMPLEX_EXPR)
    {
      tree real = TREE_OPERAND (arg, 0);
      tree imag = TREE_OPERAND (arg, 1);
      
      /* If either part is zero, cabs is fabs of the other.  */
      if (real_zerop (real))
	return fold_build1 (ABS_EXPR, type, imag);
      if (real_zerop (imag))
	return fold_build1 (ABS_EXPR, type, real);

      /* cabs(x+xi) -> fabs(x)*sqrt(2).  */
      if (flag_unsafe_math_optimizations
	  && operand_equal_p (real, imag, OEP_PURE_SAME))
        {
	  const REAL_VALUE_TYPE sqrt2_trunc
	    = real_value_truncate (TYPE_MODE (type), dconstsqrt2);
	  STRIP_NOPS (real);
	  return fold_build2 (MULT_EXPR, type,
			      fold_build1 (ABS_EXPR, type, real),
			      build_real (type, sqrt2_trunc));
	}
    }

  /* Optimize cabs(-z) and cabs(conj(z)) as cabs(z).  */
  if (TREE_CODE (arg) == NEGATE_EXPR
      || TREE_CODE (arg) == CONJ_EXPR)
    return build_call_expr (fndecl, 1, TREE_OPERAND (arg, 0));

  /* Don't do this when optimizing for size.  */
  if (flag_unsafe_math_optimizations
      && optimize && !optimize_size)
    {
      tree sqrtfn = mathfn_built_in (type, BUILT_IN_SQRT);

      if (sqrtfn != NULL_TREE)
	{
	  tree rpart, ipart, result;

	  arg = builtin_save_expr (arg);

	  rpart = fold_build1 (REALPART_EXPR, type, arg);
	  ipart = fold_build1 (IMAGPART_EXPR, type, arg);

	  rpart = builtin_save_expr (rpart);
	  ipart = builtin_save_expr (ipart);

	  result = fold_build2 (PLUS_EXPR, type,
				fold_build2 (MULT_EXPR, type,
					     rpart, rpart),
				fold_build2 (MULT_EXPR, type,
					     ipart, ipart));

	  return build_call_expr (sqrtfn, 1, result);
	}
    }

  return NULL_TREE;
}

/* Fold a builtin function call to sqrt, sqrtf, or sqrtl with argument ARG.
   Return NULL_TREE if no simplification can be made.  */

static tree
fold_builtin_sqrt (tree arg, tree type)
{

  enum built_in_function fcode;
  tree res;

  if (!validate_arg (arg, REAL_TYPE))
    return NULL_TREE;

  /* Calculate the result when the argument is a constant.  */
  if ((res = do_mpfr_arg1 (arg, type, mpfr_sqrt, &dconst0, NULL, true)))
    return res;
  
  /* Optimize sqrt(expN(x)) = expN(x*0.5).  */
  fcode = builtin_mathfn_code (arg);
  if (flag_unsafe_math_optimizations && BUILTIN_EXPONENT_P (fcode))
    {
      tree expfn = TREE_OPERAND (CALL_EXPR_FN (arg), 0);
      arg = fold_build2 (MULT_EXPR, type,
			 CALL_EXPR_ARG (arg, 0),
			 build_real (type, dconsthalf));
      return build_call_expr (expfn, 1, arg);
    }

  /* Optimize sqrt(Nroot(x)) -> pow(x,1/(2*N)).  */
  if (flag_unsafe_math_optimizations && BUILTIN_ROOT_P (fcode))
    {
      tree powfn = mathfn_built_in (type, BUILT_IN_POW);

      if (powfn)
	{
	  tree arg0 = CALL_EXPR_ARG (arg, 0);
	  tree tree_root;
	  /* The inner root was either sqrt or cbrt.  */
	  REAL_VALUE_TYPE dconstroot =
	    BUILTIN_SQRT_P (fcode) ? dconsthalf : dconstthird;

	  /* Adjust for the outer root.  */
	  SET_REAL_EXP (&dconstroot, REAL_EXP (&dconstroot) - 1);
	  dconstroot = real_value_truncate (TYPE_MODE (type), dconstroot);
	  tree_root = build_real (type, dconstroot);
	  return build_call_expr (powfn, 2, arg0, tree_root);
	}
    }

  /* Optimize sqrt(pow(x,y)) = pow(|x|,y*0.5).  */
  if (flag_unsafe_math_optimizations
      && (fcode == BUILT_IN_POW
	  || fcode == BUILT_IN_POWF
	  || fcode == BUILT_IN_POWL))
    {
      tree powfn = TREE_OPERAND (CALL_EXPR_FN (arg), 0);
      tree arg0 = CALL_EXPR_ARG (arg, 0);
      tree arg1 = CALL_EXPR_ARG (arg, 1);
      tree narg1;
      if (!tree_expr_nonnegative_p (arg0))
	arg0 = build1 (ABS_EXPR, type, arg0);
      narg1 = fold_build2 (MULT_EXPR, type, arg1,
			   build_real (type, dconsthalf));
      return build_call_expr (powfn, 2, arg0, narg1);
    }

  return NULL_TREE;
}

/* Fold a builtin function call to cbrt, cbrtf, or cbrtl with argument ARG.
   Return NULL_TREE if no simplification can be made.  */

static tree
fold_builtin_cbrt (tree arg, tree type)
{
  const enum built_in_function fcode = builtin_mathfn_code (arg);
  tree res;

  if (!validate_arg (arg, REAL_TYPE))
    return NULL_TREE;

  /* Calculate the result when the argument is a constant.  */
  if ((res = do_mpfr_arg1 (arg, type, mpfr_cbrt, NULL, NULL, 0)))
    return res;

  if (flag_unsafe_math_optimizations)
    {
      /* Optimize cbrt(expN(x)) -> expN(x/3).  */
      if (BUILTIN_EXPONENT_P (fcode))
	{
	  tree expfn = TREE_OPERAND (CALL_EXPR_FN (arg), 0);
	  const REAL_VALUE_TYPE third_trunc =
	    real_value_truncate (TYPE_MODE (type), dconstthird);
	  arg = fold_build2 (MULT_EXPR, type,
			     CALL_EXPR_ARG (arg, 0),
			     build_real (type, third_trunc));
	  return build_call_expr (expfn, 1, arg);
	}

      /* Optimize cbrt(sqrt(x)) -> pow(x,1/6).  */
      if (BUILTIN_SQRT_P (fcode))
	{
	  tree powfn = mathfn_built_in (type, BUILT_IN_POW);

	  if (powfn)
	    {
	      tree arg0 = CALL_EXPR_ARG (arg, 0);
	      tree tree_root;
	      REAL_VALUE_TYPE dconstroot = dconstthird;

	      SET_REAL_EXP (&dconstroot, REAL_EXP (&dconstroot) - 1);
	      dconstroot = real_value_truncate (TYPE_MODE (type), dconstroot);
	      tree_root = build_real (type, dconstroot);
	      return build_call_expr (powfn, 2, arg0, tree_root);
	    }
	}

      /* Optimize cbrt(cbrt(x)) -> pow(x,1/9) iff x is nonnegative.  */
      if (BUILTIN_CBRT_P (fcode))
	{
	  tree arg0 = CALL_EXPR_ARG (arg, 0);
	  if (tree_expr_nonnegative_p (arg0))
	    {
	      tree powfn = mathfn_built_in (type, BUILT_IN_POW);

	      if (powfn)
		{
		  tree tree_root;
		  REAL_VALUE_TYPE dconstroot;

		  real_arithmetic (&dconstroot, MULT_EXPR, &dconstthird, &dconstthird);
		  dconstroot = real_value_truncate (TYPE_MODE (type), dconstroot);
		  tree_root = build_real (type, dconstroot);
		  return build_call_expr (powfn, 2, arg0, tree_root);
		}
	    }
	}

      /* Optimize cbrt(pow(x,y)) -> pow(x,y/3) iff x is nonnegative.  */
      if (fcode == BUILT_IN_POW 
          || fcode == BUILT_IN_POWF
	  || fcode == BUILT_IN_POWL)
	{
	  tree arg00 = CALL_EXPR_ARG (arg, 0);
	  tree arg01 = CALL_EXPR_ARG (arg, 1);
	  if (tree_expr_nonnegative_p (arg00))
	    {
	      tree powfn = TREE_OPERAND (CALL_EXPR_FN (arg), 0);
	      const REAL_VALUE_TYPE dconstroot
		= real_value_truncate (TYPE_MODE (type), dconstthird);
	      tree narg01 = fold_build2 (MULT_EXPR, type, arg01,
					 build_real (type, dconstroot));
	      return build_call_expr (powfn, 2, arg00, narg01);
	    }
	}
    }
  return NULL_TREE;
}

/* Fold function call to builtin cos, cosf, or cosl with argument ARG.
   TYPE is the type of the return value.  Return NULL_TREE if no
   simplification can be made.  */

static tree
fold_builtin_cos (tree arg, tree type, tree fndecl)
{
  tree res, narg;

  if (!validate_arg (arg, REAL_TYPE))
    return NULL_TREE;

  /* Calculate the result when the argument is a constant.  */
  if ((res = do_mpfr_arg1 (arg, type, mpfr_cos, NULL, NULL, 0)))
    return res;
  
  /* Optimize cos(-x) into cos (x).  */
  if ((narg = fold_strip_sign_ops (arg)))
    return build_call_expr (fndecl, 1, narg);

  return NULL_TREE;
}

/* Fold function call to builtin cosh, coshf, or coshl with argument ARG.
   Return NULL_TREE if no simplification can be made.  */

static tree
fold_builtin_cosh (tree arg, tree type, tree fndecl)
{
  if (validate_arg (arg, REAL_TYPE))
    {
      tree res, narg;

      /* Calculate the result when the argument is a constant.  */
      if ((res = do_mpfr_arg1 (arg, type, mpfr_cosh, NULL, NULL, 0)))
	return res;
  
      /* Optimize cosh(-x) into cosh (x).  */
      if ((narg = fold_strip_sign_ops (arg)))
	return build_call_expr (fndecl, 1, narg);
    }
  
  return NULL_TREE;
}

/* Fold function call to builtin tan, tanf, or tanl with argument ARG.
   Return NULL_TREE if no simplification can be made.  */

static tree
fold_builtin_tan (tree arg, tree type)
{
  enum built_in_function fcode;
  tree res;

  if (!validate_arg (arg, REAL_TYPE))
    return NULL_TREE;

  /* Calculate the result when the argument is a constant.  */
  if ((res = do_mpfr_arg1 (arg, type, mpfr_tan, NULL, NULL, 0)))
    return res;
  
  /* Optimize tan(atan(x)) = x.  */
  fcode = builtin_mathfn_code (arg);
  if (flag_unsafe_math_optimizations
      && (fcode == BUILT_IN_ATAN
	  || fcode == BUILT_IN_ATANF
	  || fcode == BUILT_IN_ATANL))
    return CALL_EXPR_ARG (arg, 0);

  return NULL_TREE;
}

/* Fold function call to builtin sincos, sincosf, or sincosl.  Return
   NULL_TREE if no simplification can be made.  */

static tree
fold_builtin_sincos (tree arg0, tree arg1, tree arg2)
{
  tree type;
  tree res, fn, call;

  if (!validate_arg (arg0, REAL_TYPE)
      || !validate_arg (arg1, POINTER_TYPE)
      || !validate_arg (arg2, POINTER_TYPE))
    return NULL_TREE;

  type = TREE_TYPE (arg0);

  /* Calculate the result when the argument is a constant.  */
  if ((res = do_mpfr_sincos (arg0, arg1, arg2)))
    return res;

  /* Canonicalize sincos to cexpi.  */
  if (!TARGET_C99_FUNCTIONS)
    return NULL_TREE;
  fn = mathfn_built_in (type, BUILT_IN_CEXPI);
  if (!fn)
    return NULL_TREE;

  call = build_call_expr (fn, 1, arg0);
  call = builtin_save_expr (call);

  return build2 (COMPOUND_EXPR, type,
		 build2 (MODIFY_EXPR, void_type_node,
			 build_fold_indirect_ref (arg1),
			 build1 (IMAGPART_EXPR, type, call)),
		 build2 (MODIFY_EXPR, void_type_node,
			 build_fold_indirect_ref (arg2),
			 build1 (REALPART_EXPR, type, call)));
}

/* Fold function call to builtin cexp, cexpf, or cexpl.  Return
   NULL_TREE if no simplification can be made.  */

static tree
fold_builtin_cexp (tree arg0, tree type)
{
  tree rtype;
  tree realp, imagp, ifn;

  if (!validate_arg (arg0, COMPLEX_TYPE))
    return NULL_TREE;

  rtype = TREE_TYPE (TREE_TYPE (arg0));

  /* In case we can figure out the real part of arg0 and it is constant zero
     fold to cexpi.  */
  if (!TARGET_C99_FUNCTIONS)
    return NULL_TREE;
  ifn = mathfn_built_in (rtype, BUILT_IN_CEXPI);
  if (!ifn)
    return NULL_TREE;

  if ((realp = fold_unary (REALPART_EXPR, rtype, arg0))
      && real_zerop (realp))
    {
      tree narg = fold_build1 (IMAGPART_EXPR, rtype, arg0);
      return build_call_expr (ifn, 1, narg);
    }

  /* In case we can easily decompose real and imaginary parts split cexp
     to exp (r) * cexpi (i).  */
  if (flag_unsafe_math_optimizations
      && realp)
    {
      tree rfn, rcall, icall;

      rfn = mathfn_built_in (rtype, BUILT_IN_EXP);
      if (!rfn)
	return NULL_TREE;

      imagp = fold_unary (IMAGPART_EXPR, rtype, arg0);
      if (!imagp)
	return NULL_TREE;

      icall = build_call_expr (ifn, 1, imagp);
      icall = builtin_save_expr (icall);
      rcall = build_call_expr (rfn, 1, realp);
      rcall = builtin_save_expr (rcall);
      return fold_build2 (COMPLEX_EXPR, type,
			  fold_build2 (MULT_EXPR, rtype,
				       rcall,
			 	       fold_build1 (REALPART_EXPR, rtype, icall)),
			  fold_build2 (MULT_EXPR, rtype,
				       rcall,
				       fold_build1 (IMAGPART_EXPR, rtype, icall)));
    }

  return NULL_TREE;
}

/* Fold function call to builtin trunc, truncf or truncl with argument ARG.
   Return NULL_TREE if no simplification can be made.  */

static tree
fold_builtin_trunc (tree fndecl, tree arg)
{
  if (!validate_arg (arg, REAL_TYPE))
    return NULL_TREE;

  /* Optimize trunc of constant value.  */
  if (TREE_CODE (arg) == REAL_CST && !TREE_OVERFLOW (arg))
    {
      REAL_VALUE_TYPE r, x;
      tree type = TREE_TYPE (TREE_TYPE (fndecl));

      x = TREE_REAL_CST (arg);
      real_trunc (&r, TYPE_MODE (type), &x);
      return build_real (type, r);
    }

  return fold_trunc_transparent_mathfn (fndecl, arg);
}

/* Fold function call to builtin floor, floorf or floorl with argument ARG.
   Return NULL_TREE if no simplification can be made.  */

static tree
fold_builtin_floor (tree fndecl, tree arg)
{
  if (!validate_arg (arg, REAL_TYPE))
    return NULL_TREE;

  /* Optimize floor of constant value.  */
  if (TREE_CODE (arg) == REAL_CST && !TREE_OVERFLOW (arg))
    {
      REAL_VALUE_TYPE x;

      x = TREE_REAL_CST (arg);
      if (! REAL_VALUE_ISNAN (x) || ! flag_errno_math)
	{
	  tree type = TREE_TYPE (TREE_TYPE (fndecl));
	  REAL_VALUE_TYPE r;

	  real_floor (&r, TYPE_MODE (type), &x);
	  return build_real (type, r);
	}
    }

  /* Fold floor (x) where x is nonnegative to trunc (x).  */
  if (tree_expr_nonnegative_p (arg))
    {
      tree truncfn = mathfn_built_in (TREE_TYPE (arg), BUILT_IN_TRUNC);
      if (truncfn)
	return build_call_expr (truncfn, 1, arg);
    }

  return fold_trunc_transparent_mathfn (fndecl, arg);
}

/* Fold function call to builtin ceil, ceilf or ceill with argument ARG.
   Return NULL_TREE if no simplification can be made.  */

static tree
fold_builtin_ceil (tree fndecl, tree arg)
{
  if (!validate_arg (arg, REAL_TYPE))
    return NULL_TREE;

  /* Optimize ceil of constant value.  */
  if (TREE_CODE (arg) == REAL_CST && !TREE_OVERFLOW (arg))
    {
      REAL_VALUE_TYPE x;

      x = TREE_REAL_CST (arg);
      if (! REAL_VALUE_ISNAN (x) || ! flag_errno_math)
	{
	  tree type = TREE_TYPE (TREE_TYPE (fndecl));
	  REAL_VALUE_TYPE r;

	  real_ceil (&r, TYPE_MODE (type), &x);
	  return build_real (type, r);
	}
    }

  return fold_trunc_transparent_mathfn (fndecl, arg);
}

/* Fold function call to builtin round, roundf or roundl with argument ARG.
   Return NULL_TREE if no simplification can be made.  */

static tree
fold_builtin_round (tree fndecl, tree arg)
{
  if (!validate_arg (arg, REAL_TYPE))
    return NULL_TREE;

  /* Optimize round of constant value.  */
  if (TREE_CODE (arg) == REAL_CST && !TREE_OVERFLOW (arg))
    {
      REAL_VALUE_TYPE x;

      x = TREE_REAL_CST (arg);
      if (! REAL_VALUE_ISNAN (x) || ! flag_errno_math)
	{
	  tree type = TREE_TYPE (TREE_TYPE (fndecl));
	  REAL_VALUE_TYPE r;

	  real_round (&r, TYPE_MODE (type), &x);
	  return build_real (type, r);
	}
    }

  return fold_trunc_transparent_mathfn (fndecl, arg);
}

/* Fold function call to builtin lround, lroundf or lroundl (or the
   corresponding long long versions) and other rounding functions.  ARG
   is the argument to the call.  Return NULL_TREE if no simplification
   can be made.  */

static tree
fold_builtin_int_roundingfn (tree fndecl, tree arg)
{
  if (!validate_arg (arg, REAL_TYPE))
    return NULL_TREE;

  /* Optimize lround of constant value.  */
  if (TREE_CODE (arg) == REAL_CST && !TREE_OVERFLOW (arg))
    {
      const REAL_VALUE_TYPE x = TREE_REAL_CST (arg);

      if (real_isfinite (&x))
	{
	  tree itype = TREE_TYPE (TREE_TYPE (fndecl));
	  tree ftype = TREE_TYPE (arg);
	  unsigned HOST_WIDE_INT lo2;
	  HOST_WIDE_INT hi, lo;
	  REAL_VALUE_TYPE r;

	  switch (DECL_FUNCTION_CODE (fndecl))
	    {
	    CASE_FLT_FN (BUILT_IN_LFLOOR):
	    CASE_FLT_FN (BUILT_IN_LLFLOOR):
	      real_floor (&r, TYPE_MODE (ftype), &x);
	      break;

	    CASE_FLT_FN (BUILT_IN_LCEIL):
	    CASE_FLT_FN (BUILT_IN_LLCEIL):
	      real_ceil (&r, TYPE_MODE (ftype), &x);
	      break;

	    CASE_FLT_FN (BUILT_IN_LROUND):
	    CASE_FLT_FN (BUILT_IN_LLROUND):
	      real_round (&r, TYPE_MODE (ftype), &x);
	      break;

	    default:
	      gcc_unreachable ();
	    }

	  REAL_VALUE_TO_INT (&lo, &hi, r);
	  if (!fit_double_type (lo, hi, &lo2, &hi, itype))
	    return build_int_cst_wide (itype, lo2, hi);
	}
    }

  switch (DECL_FUNCTION_CODE (fndecl))
    {
    CASE_FLT_FN (BUILT_IN_LFLOOR):
    CASE_FLT_FN (BUILT_IN_LLFLOOR):
      /* Fold lfloor (x) where x is nonnegative to FIX_TRUNC (x).  */
      if (tree_expr_nonnegative_p (arg))
	return fold_build1 (FIX_TRUNC_EXPR, TREE_TYPE (TREE_TYPE (fndecl)),
			    arg);
      break;
    default:;
    }

  return fold_fixed_mathfn (fndecl, arg);
}

/* Fold function call to builtin ffs, clz, ctz, popcount and parity
   and their long and long long variants (i.e. ffsl and ffsll).  ARG is
   the argument to the call.  Return NULL_TREE if no simplification can
   be made.  */

static tree
fold_builtin_bitop (tree fndecl, tree arg)
{
  if (!validate_arg (arg, INTEGER_TYPE))
    return NULL_TREE;

  /* Optimize for constant argument.  */
  if (TREE_CODE (arg) == INTEGER_CST && !TREE_OVERFLOW (arg))
    {
      HOST_WIDE_INT hi, width, result;
      unsigned HOST_WIDE_INT lo;
      tree type;

      type = TREE_TYPE (arg);
      width = TYPE_PRECISION (type);
      lo = TREE_INT_CST_LOW (arg);

      /* Clear all the bits that are beyond the type's precision.  */
      if (width > HOST_BITS_PER_WIDE_INT)
	{
	  hi = TREE_INT_CST_HIGH (arg);
	  if (width < 2 * HOST_BITS_PER_WIDE_INT)
	    hi &= ~((HOST_WIDE_INT) (-1) >> (width - HOST_BITS_PER_WIDE_INT));
	}
      else
	{
	  hi = 0;
	  if (width < HOST_BITS_PER_WIDE_INT)
	    lo &= ~((unsigned HOST_WIDE_INT) (-1) << width);
	}

      switch (DECL_FUNCTION_CODE (fndecl))
	{
	CASE_INT_FN (BUILT_IN_FFS):
	  if (lo != 0)
	    result = exact_log2 (lo & -lo) + 1;
	  else if (hi != 0)
	    result = HOST_BITS_PER_WIDE_INT + exact_log2 (hi & -hi) + 1;
	  else
	    result = 0;
	  break;

	CASE_INT_FN (BUILT_IN_CLZ):
	  if (hi != 0)
	    result = width - floor_log2 (hi) - 1 - HOST_BITS_PER_WIDE_INT;
	  else if (lo != 0)
	    result = width - floor_log2 (lo) - 1;
	  else if (! CLZ_DEFINED_VALUE_AT_ZERO (TYPE_MODE (type), result))
	    result = width;
	  break;

	CASE_INT_FN (BUILT_IN_CTZ):
	  if (lo != 0)
	    result = exact_log2 (lo & -lo);
	  else if (hi != 0)
	    result = HOST_BITS_PER_WIDE_INT + exact_log2 (hi & -hi);
	  else if (! CTZ_DEFINED_VALUE_AT_ZERO (TYPE_MODE (type), result))
	    result = width;
	  break;

	CASE_INT_FN (BUILT_IN_POPCOUNT):
	  result = 0;
	  while (lo)
	    result++, lo &= lo - 1;
	  while (hi)
	    result++, hi &= hi - 1;
	  break;

	CASE_INT_FN (BUILT_IN_PARITY):
	  result = 0;
	  while (lo)
	    result++, lo &= lo - 1;
	  while (hi)
	    result++, hi &= hi - 1;
	  result &= 1;
	  break;

	default:
	  gcc_unreachable ();
	}

      return build_int_cst (TREE_TYPE (TREE_TYPE (fndecl)), result);
    }

  return NULL_TREE;
}

/* Fold function call to builtin_bswap and the long and long long
   variants.  Return NULL_TREE if no simplification can be made.  */
static tree
fold_builtin_bswap (tree fndecl, tree arg)
{
  if (! validate_arg (arg, INTEGER_TYPE))
    return NULL_TREE;

  /* Optimize constant value.  */
  if (TREE_CODE (arg) == INTEGER_CST && !TREE_OVERFLOW (arg))
    {
      HOST_WIDE_INT hi, width, r_hi = 0;
      unsigned HOST_WIDE_INT lo, r_lo = 0;
      tree type;

      type = TREE_TYPE (arg);
      width = TYPE_PRECISION (type);
      lo = TREE_INT_CST_LOW (arg);
      hi = TREE_INT_CST_HIGH (arg);

      switch (DECL_FUNCTION_CODE (fndecl))
	{
	  case BUILT_IN_BSWAP32:
	  case BUILT_IN_BSWAP64:
	    {
	      int s;

	      for (s = 0; s < width; s += 8)
		{
		  int d = width - s - 8;
		  unsigned HOST_WIDE_INT byte;

		  if (s < HOST_BITS_PER_WIDE_INT)
		    byte = (lo >> s) & 0xff;
		  else
		    byte = (hi >> (s - HOST_BITS_PER_WIDE_INT)) & 0xff;

		  if (d < HOST_BITS_PER_WIDE_INT)
		    r_lo |= byte << d;
		  else
		    r_hi |= byte << (d - HOST_BITS_PER_WIDE_INT);
		}
	    }

	    break;

	default:
	  gcc_unreachable ();
	}

      if (width < HOST_BITS_PER_WIDE_INT)
	return build_int_cst (TREE_TYPE (TREE_TYPE (fndecl)), r_lo);
      else
	return build_int_cst_wide (TREE_TYPE (TREE_TYPE (fndecl)), r_lo, r_hi);
    }

  return NULL_TREE;
}

/* Return true if EXPR is the real constant contained in VALUE.  */

static bool
real_dconstp (tree expr, const REAL_VALUE_TYPE *value)
{
  STRIP_NOPS (expr);

  return ((TREE_CODE (expr) == REAL_CST
	   && !TREE_OVERFLOW (expr)
	   && REAL_VALUES_EQUAL (TREE_REAL_CST (expr), *value))
	  || (TREE_CODE (expr) == COMPLEX_CST
	      && real_dconstp (TREE_REALPART (expr), value)
	      && real_zerop (TREE_IMAGPART (expr))));
}

/* A subroutine of fold_builtin to fold the various logarithmic
   functions.  Return NULL_TREE if no simplification can me made.
   FUNC is the corresponding MPFR logarithm function.  */

static tree
fold_builtin_logarithm (tree fndecl, tree arg,
			int (*func)(mpfr_ptr, mpfr_srcptr, mp_rnd_t))
{
  if (validate_arg (arg, REAL_TYPE))
    {
      tree type = TREE_TYPE (TREE_TYPE (fndecl));
      tree res;
      const enum built_in_function fcode = builtin_mathfn_code (arg);

      /* Optimize log(e) = 1.0.  We're never passed an exact 'e',
	 instead we'll look for 'e' truncated to MODE.  So only do
	 this if flag_unsafe_math_optimizations is set.  */
      if (flag_unsafe_math_optimizations && func == mpfr_log)
        {
	  const REAL_VALUE_TYPE e_truncated =
	    real_value_truncate (TYPE_MODE (type), dconste);
	  if (real_dconstp (arg, &e_truncated))
	    return build_real (type, dconst1);
	}

      /* Calculate the result when the argument is a constant.  */
      if ((res = do_mpfr_arg1 (arg, type, func, &dconst0, NULL, false)))
	return res;

      /* Special case, optimize logN(expN(x)) = x.  */
      if (flag_unsafe_math_optimizations
	  && ((func == mpfr_log
	       && (fcode == BUILT_IN_EXP
		   || fcode == BUILT_IN_EXPF
		   || fcode == BUILT_IN_EXPL))
	      || (func == mpfr_log2
		  && (fcode == BUILT_IN_EXP2
		      || fcode == BUILT_IN_EXP2F
		      || fcode == BUILT_IN_EXP2L))
	      || (func == mpfr_log10 && (BUILTIN_EXP10_P (fcode)))))
	return fold_convert (type, CALL_EXPR_ARG (arg, 0));

      /* Optimize logN(func()) for various exponential functions.  We
	 want to determine the value "x" and the power "exponent" in
	 order to transform logN(x**exponent) into exponent*logN(x).  */
      if (flag_unsafe_math_optimizations)
	{
	  tree exponent = 0, x = 0;

	  switch (fcode)
	  {
	  CASE_FLT_FN (BUILT_IN_EXP):
	    /* Prepare to do logN(exp(exponent) -> exponent*logN(e).  */
	    x = build_real (type,
			    real_value_truncate (TYPE_MODE (type), dconste));
	    exponent = CALL_EXPR_ARG (arg, 0);
	    break;
	  CASE_FLT_FN (BUILT_IN_EXP2):
	    /* Prepare to do logN(exp2(exponent) -> exponent*logN(2).  */
	    x = build_real (type, dconst2);
	    exponent = CALL_EXPR_ARG (arg, 0);
	    break;
	  CASE_FLT_FN (BUILT_IN_EXP10):
	  CASE_FLT_FN (BUILT_IN_POW10):
	    /* Prepare to do logN(exp10(exponent) -> exponent*logN(10).  */
	    x = build_real (type, dconst10);
	    exponent = CALL_EXPR_ARG (arg, 0);
	    break;
	  CASE_FLT_FN (BUILT_IN_SQRT):
	    /* Prepare to do logN(sqrt(x) -> 0.5*logN(x).  */
	    x = CALL_EXPR_ARG (arg, 0);
	    exponent = build_real (type, dconsthalf);
	    break;
	  CASE_FLT_FN (BUILT_IN_CBRT):
	    /* Prepare to do logN(cbrt(x) -> (1/3)*logN(x).  */
	    x = CALL_EXPR_ARG (arg, 0);
	    exponent = build_real (type, real_value_truncate (TYPE_MODE (type),
							      dconstthird));
	    break;
	  CASE_FLT_FN (BUILT_IN_POW):
	    /* Prepare to do logN(pow(x,exponent) -> exponent*logN(x).  */
	    x = CALL_EXPR_ARG (arg, 0);
	    exponent = CALL_EXPR_ARG (arg, 1);
	    break;
	  default:
	    break;
	  }

	  /* Now perform the optimization.  */
	  if (x && exponent)
	    {
	      tree logfn = build_call_expr (fndecl, 1, x);
	      return fold_build2 (MULT_EXPR, type, exponent, logfn);
	    }
	}
    }

  return NULL_TREE;
}

/* Fold a builtin function call to hypot, hypotf, or hypotl.  Return
   NULL_TREE if no simplification can be made.  */

static tree
fold_builtin_hypot (tree fndecl, tree arg0, tree arg1, tree type)
{
  tree res, narg0, narg1;

  if (!validate_arg (arg0, REAL_TYPE)
      || !validate_arg (arg1, REAL_TYPE))
    return NULL_TREE;

  /* Calculate the result when the argument is a constant.  */
  if ((res = do_mpfr_arg2 (arg0, arg1, type, mpfr_hypot)))
    return res;
  
  /* If either argument to hypot has a negate or abs, strip that off.
     E.g. hypot(-x,fabs(y)) -> hypot(x,y).  */
  narg0 = fold_strip_sign_ops (arg0);
  narg1 = fold_strip_sign_ops (arg1);
  if (narg0 || narg1)
    {
      return build_call_expr (fndecl, 2, narg0 ? narg0 : arg0, 
			      narg1 ? narg1 : arg1);
    }
  
  /* If either argument is zero, hypot is fabs of the other.  */
  if (real_zerop (arg0))
    return fold_build1 (ABS_EXPR, type, arg1);
  else if (real_zerop (arg1))
    return fold_build1 (ABS_EXPR, type, arg0);
      
  /* hypot(x,x) -> fabs(x)*sqrt(2).  */
  if (flag_unsafe_math_optimizations
      && operand_equal_p (arg0, arg1, OEP_PURE_SAME))
    {
      const REAL_VALUE_TYPE sqrt2_trunc
	= real_value_truncate (TYPE_MODE (type), dconstsqrt2);
      return fold_build2 (MULT_EXPR, type,
			  fold_build1 (ABS_EXPR, type, arg0),
			  build_real (type, sqrt2_trunc));
    }

  return NULL_TREE;
}


/* Fold a builtin function call to pow, powf, or powl.  Return
   NULL_TREE if no simplification can be made.  */
static tree
fold_builtin_pow (tree fndecl, tree arg0, tree arg1, tree type)
{
  tree res;

  if (!validate_arg (arg0, REAL_TYPE)
       || !validate_arg (arg1, REAL_TYPE))
    return NULL_TREE;

  /* Calculate the result when the argument is a constant.  */
  if ((res = do_mpfr_arg2 (arg0, arg1, type, mpfr_pow)))
    return res;

  /* Optimize pow(1.0,y) = 1.0.  */
  if (real_onep (arg0))
    return omit_one_operand (type, build_real (type, dconst1), arg1);

  if (TREE_CODE (arg1) == REAL_CST
      && !TREE_OVERFLOW (arg1))
    {
      REAL_VALUE_TYPE cint;
      REAL_VALUE_TYPE c;
      HOST_WIDE_INT n;

      c = TREE_REAL_CST (arg1);

      /* Optimize pow(x,0.0) = 1.0.  */
      if (REAL_VALUES_EQUAL (c, dconst0))
	return omit_one_operand (type, build_real (type, dconst1),
				 arg0);

      /* Optimize pow(x,1.0) = x.  */
      if (REAL_VALUES_EQUAL (c, dconst1))
	return arg0;

      /* Optimize pow(x,-1.0) = 1.0/x.  */
      if (REAL_VALUES_EQUAL (c, dconstm1))
	return fold_build2 (RDIV_EXPR, type,
			    build_real (type, dconst1), arg0);

      /* Optimize pow(x,0.5) = sqrt(x).  */
      if (flag_unsafe_math_optimizations
	  && REAL_VALUES_EQUAL (c, dconsthalf))
	{
	  tree sqrtfn = mathfn_built_in (type, BUILT_IN_SQRT);

	  if (sqrtfn != NULL_TREE)
	    return build_call_expr (sqrtfn, 1, arg0);
	}

      /* Optimize pow(x,1.0/3.0) = cbrt(x).  */
      if (flag_unsafe_math_optimizations)
	{
	  const REAL_VALUE_TYPE dconstroot
	    = real_value_truncate (TYPE_MODE (type), dconstthird);

	  if (REAL_VALUES_EQUAL (c, dconstroot))
	    {
	      tree cbrtfn = mathfn_built_in (type, BUILT_IN_CBRT);
	      if (cbrtfn != NULL_TREE)
		  return build_call_expr (cbrtfn, 1, arg0);
	    }
	}

      /* Check for an integer exponent.  */
      n = real_to_integer (&c);
      real_from_integer (&cint, VOIDmode, n, n < 0 ? -1 : 0, 0);
      if (real_identical (&c, &cint))
	{
	  /* Attempt to evaluate pow at compile-time.  */
	  if (TREE_CODE (arg0) == REAL_CST
	      && !TREE_OVERFLOW (arg0))
	    {
	      REAL_VALUE_TYPE x;
	      bool inexact;

	      x = TREE_REAL_CST (arg0);
	      inexact = real_powi (&x, TYPE_MODE (type), &x, n);
	      if (flag_unsafe_math_optimizations || !inexact)
		return build_real (type, x);
	    }

	  /* Strip sign ops from even integer powers.  */
	  if ((n & 1) == 0 && flag_unsafe_math_optimizations)
	    {
	      tree narg0 = fold_strip_sign_ops (arg0);
	      if (narg0)
		return build_call_expr (fndecl, 2, narg0, arg1);
	    }
	}
    }

  if (flag_unsafe_math_optimizations)
    {
      const enum built_in_function fcode = builtin_mathfn_code (arg0);

      /* Optimize pow(expN(x),y) = expN(x*y).  */
      if (BUILTIN_EXPONENT_P (fcode))
	{
	  tree expfn = TREE_OPERAND (CALL_EXPR_FN (arg0), 0);
	  tree arg = CALL_EXPR_ARG (arg0, 0);
	  arg = fold_build2 (MULT_EXPR, type, arg, arg1);
	  return build_call_expr (expfn, 1, arg);
	}

      /* Optimize pow(sqrt(x),y) = pow(x,y*0.5).  */
      if (BUILTIN_SQRT_P (fcode))
	{
	  tree narg0 = CALL_EXPR_ARG (arg0, 0);
	  tree narg1 = fold_build2 (MULT_EXPR, type, arg1,
				    build_real (type, dconsthalf));
	  return build_call_expr (fndecl, 2, narg0, narg1);
	}

      /* Optimize pow(cbrt(x),y) = pow(x,y/3) iff x is nonnegative.  */
      if (BUILTIN_CBRT_P (fcode))
	{
	  tree arg = CALL_EXPR_ARG (arg0, 0);
	  if (tree_expr_nonnegative_p (arg))
	    {
	      const REAL_VALUE_TYPE dconstroot
		= real_value_truncate (TYPE_MODE (type), dconstthird);
	      tree narg1 = fold_build2 (MULT_EXPR, type, arg1,
					build_real (type, dconstroot));
	      return build_call_expr (fndecl, 2, arg, narg1);
	    }
	}

      /* Optimize pow(pow(x,y),z) = pow(x,y*z).  */
      if (fcode == BUILT_IN_POW
	  || fcode == BUILT_IN_POWF
	  || fcode == BUILT_IN_POWL)
	{
	  tree arg00 = CALL_EXPR_ARG (arg0, 0);
	  tree arg01 = CALL_EXPR_ARG (arg0, 1);
	  tree narg1 = fold_build2 (MULT_EXPR, type, arg01, arg1);
	  return build_call_expr (fndecl, 2, arg00, narg1);
	}
    }

  return NULL_TREE;
}

/* Fold a builtin function call to powi, powif, or powil with argument ARG.
   Return NULL_TREE if no simplification can be made.  */
static tree
fold_builtin_powi (tree fndecl ATTRIBUTE_UNUSED,
		   tree arg0, tree arg1, tree type)
{
  if (!validate_arg (arg0, REAL_TYPE)
      || !validate_arg (arg1, INTEGER_TYPE))
    return NULL_TREE;

  /* Optimize pow(1.0,y) = 1.0.  */
  if (real_onep (arg0))
    return omit_one_operand (type, build_real (type, dconst1), arg1);

  if (host_integerp (arg1, 0))
    {
      HOST_WIDE_INT c = TREE_INT_CST_LOW (arg1);

      /* Evaluate powi at compile-time.  */
      if (TREE_CODE (arg0) == REAL_CST
	  && !TREE_OVERFLOW (arg0))
	{
	  REAL_VALUE_TYPE x;
	  x = TREE_REAL_CST (arg0);
	  real_powi (&x, TYPE_MODE (type), &x, c);
	  return build_real (type, x);
	}

      /* Optimize pow(x,0) = 1.0.  */
      if (c == 0)
	return omit_one_operand (type, build_real (type, dconst1),
				 arg0);

      /* Optimize pow(x,1) = x.  */
      if (c == 1)
	return arg0;

      /* Optimize pow(x,-1) = 1.0/x.  */
      if (c == -1)
	return fold_build2 (RDIV_EXPR, type,
			   build_real (type, dconst1), arg0);
    }

  return NULL_TREE;
}

/* A subroutine of fold_builtin to fold the various exponent
   functions.  Return NULL_TREE if no simplification can be made.
   FUNC is the corresponding MPFR exponent function.  */

static tree
fold_builtin_exponent (tree fndecl, tree arg,
		       int (*func)(mpfr_ptr, mpfr_srcptr, mp_rnd_t))
{
  if (validate_arg (arg, REAL_TYPE))
    {
      tree type = TREE_TYPE (TREE_TYPE (fndecl));
      tree res;
      
      /* Calculate the result when the argument is a constant.  */
      if ((res = do_mpfr_arg1 (arg, type, func, NULL, NULL, 0)))
	return res;

      /* Optimize expN(logN(x)) = x.  */
      if (flag_unsafe_math_optimizations)
	{
	  const enum built_in_function fcode = builtin_mathfn_code (arg);

	  if ((func == mpfr_exp
	       && (fcode == BUILT_IN_LOG
		   || fcode == BUILT_IN_LOGF
		   || fcode == BUILT_IN_LOGL))
	      || (func == mpfr_exp2
		  && (fcode == BUILT_IN_LOG2
		      || fcode == BUILT_IN_LOG2F
		      || fcode == BUILT_IN_LOG2L))
	      || (func == mpfr_exp10
		  && (fcode == BUILT_IN_LOG10
		      || fcode == BUILT_IN_LOG10F
		      || fcode == BUILT_IN_LOG10L)))
	    return fold_convert (type, CALL_EXPR_ARG (arg, 0));
	}
    }

  return NULL_TREE;
}

/* Return true if VAR is a VAR_DECL or a component thereof.  */

static bool
var_decl_component_p (tree var)
{
  tree inner = var;
  while (handled_component_p (inner))
    inner = TREE_OPERAND (inner, 0);
  return SSA_VAR_P (inner);
}

/* Fold function call to builtin memset.  Return
   NULL_TREE if no simplification can be made.  */

static tree
fold_builtin_memset (tree dest, tree c, tree len, tree type, bool ignore)
{
  tree var, ret;
  unsigned HOST_WIDE_INT length, cval;

  if (! validate_arg (dest, POINTER_TYPE)
      || ! validate_arg (c, INTEGER_TYPE)
      || ! validate_arg (len, INTEGER_TYPE))
    return NULL_TREE;

  if (! host_integerp (len, 1))
    return NULL_TREE;

  /* If the LEN parameter is zero, return DEST.  */
  if (integer_zerop (len))
    return omit_one_operand (type, dest, c);

  if (! host_integerp (c, 1) || TREE_SIDE_EFFECTS (dest))
    return NULL_TREE;

  var = dest;
  STRIP_NOPS (var);
  if (TREE_CODE (var) != ADDR_EXPR)
    return NULL_TREE;

  var = TREE_OPERAND (var, 0);
  if (TREE_THIS_VOLATILE (var))
    return NULL_TREE;

  if (!INTEGRAL_TYPE_P (TREE_TYPE (var))
      && !POINTER_TYPE_P (TREE_TYPE (var)))
    return NULL_TREE;

  if (! var_decl_component_p (var))
    return NULL_TREE;

  length = tree_low_cst (len, 1);
  if (GET_MODE_SIZE (TYPE_MODE (TREE_TYPE (var))) != length
      || get_pointer_alignment (dest, BIGGEST_ALIGNMENT) / BITS_PER_UNIT
	 < (int) length)
    return NULL_TREE;

  if (length > HOST_BITS_PER_WIDE_INT / BITS_PER_UNIT)
    return NULL_TREE;

  if (integer_zerop (c))
    cval = 0;
  else
    {
      if (CHAR_BIT != 8 || BITS_PER_UNIT != 8 || HOST_BITS_PER_WIDE_INT > 64)
	return NULL_TREE;

      cval = tree_low_cst (c, 1);
      cval &= 0xff;
      cval |= cval << 8;
      cval |= cval << 16;
      cval |= (cval << 31) << 1;
    }

  ret = build_int_cst_type (TREE_TYPE (var), cval);
  ret = build2 (MODIFY_EXPR, TREE_TYPE (var), var, ret);
  if (ignore)
    return ret;

  return omit_one_operand (type, dest, ret);
}

/* Fold function call to builtin memset.  Return
   NULL_TREE if no simplification can be made.  */

static tree
fold_builtin_bzero (tree dest, tree size, bool ignore)
{
  if (! validate_arg (dest, POINTER_TYPE)
      || ! validate_arg (size, INTEGER_TYPE))
    return NULL_TREE;

  if (!ignore)
    return NULL_TREE;

  /* New argument list transforming bzero(ptr x, int y) to
     memset(ptr x, int 0, size_t y).   This is done this way
     so that if it isn't expanded inline, we fallback to
     calling bzero instead of memset.  */

  return fold_builtin_memset (dest, integer_zero_node,
			      fold_convert (sizetype, size),
			      void_type_node, ignore);
}

/* Fold function call to builtin mem{{,p}cpy,move}.  Return
   NULL_TREE if no simplification can be made.
   If ENDP is 0, return DEST (like memcpy).
   If ENDP is 1, return DEST+LEN (like mempcpy).
   If ENDP is 2, return DEST+LEN-1 (like stpcpy).
   If ENDP is 3, return DEST, additionally *SRC and *DEST may overlap
   (memmove).   */

static tree
fold_builtin_memory_op (tree dest, tree src, tree len, tree type, bool ignore, int endp)
{
  tree destvar, srcvar, expr;

  if (! validate_arg (dest, POINTER_TYPE)
      || ! validate_arg (src, POINTER_TYPE)
      || ! validate_arg (len, INTEGER_TYPE))
    return NULL_TREE;

  /* If the LEN parameter is zero, return DEST.  */
  if (integer_zerop (len))
    return omit_one_operand (type, dest, src);

  /* If SRC and DEST are the same (and not volatile), return
     DEST{,+LEN,+LEN-1}.  */
  if (operand_equal_p (src, dest, 0))
    expr = len;
  else
    {
      tree srctype, desttype;
      if (endp == 3)
	{
          int src_align = get_pointer_alignment (src, BIGGEST_ALIGNMENT);
          int dest_align = get_pointer_alignment (dest, BIGGEST_ALIGNMENT);

	  /* Both DEST and SRC must be pointer types. 
	     ??? This is what old code did.  Is the testing for pointer types
	     really mandatory?

	     If either SRC is readonly or length is 1, we can use memcpy.  */
	  if (dest_align && src_align
	      && (readonly_data_expr (src)
	          || (host_integerp (len, 1)
		      && (MIN (src_align, dest_align) / BITS_PER_UNIT >=
			  tree_low_cst (len, 1)))))
	    {
	      tree fn = implicit_built_in_decls[BUILT_IN_MEMCPY];
	      if (!fn)
		return NULL_TREE;
              return build_call_expr (fn, 3, dest, src, len);
	    }
	  return NULL_TREE;
	}

      if (!host_integerp (len, 0))
	return NULL_TREE;
      /* FIXME:
         This logic lose for arguments like (type *)malloc (sizeof (type)),
         since we strip the casts of up to VOID return value from malloc.
	 Perhaps we ought to inherit type from non-VOID argument here?  */
      STRIP_NOPS (src);
      STRIP_NOPS (dest);
      srctype = TREE_TYPE (TREE_TYPE (src));
      desttype = TREE_TYPE (TREE_TYPE (dest));
      if (!srctype || !desttype
	  || !TYPE_SIZE_UNIT (srctype)
	  || !TYPE_SIZE_UNIT (desttype)
	  || TREE_CODE (TYPE_SIZE_UNIT (srctype)) != INTEGER_CST
	  || TREE_CODE (TYPE_SIZE_UNIT (desttype)) != INTEGER_CST
	  || !tree_int_cst_equal (TYPE_SIZE_UNIT (srctype), len)
	  || !tree_int_cst_equal (TYPE_SIZE_UNIT (desttype), len))
	return NULL_TREE;

      if (get_pointer_alignment (dest, BIGGEST_ALIGNMENT) 
	  < (int) TYPE_ALIGN (desttype)
	  || (get_pointer_alignment (src, BIGGEST_ALIGNMENT) 
	      < (int) TYPE_ALIGN (srctype)))
	return NULL_TREE;

      if (!ignore)
        dest = builtin_save_expr (dest);

      srcvar = build_fold_indirect_ref (src);
      if (TREE_THIS_VOLATILE (srcvar))
	return NULL_TREE;
      if (!tree_int_cst_equal (lang_hooks.expr_size (srcvar), len))
	return NULL_TREE;
      /* With memcpy, it is possible to bypass aliasing rules, so without
         this check i. e. execute/20060930-2.c would be misoptimized, because
	 it use conflicting alias set to hold argument for the memcpy call.
	 This check is probably unnecesary with -fno-strict-aliasing. 
	 Similarly for destvar.  See also PR29286.  */
      if (!var_decl_component_p (srcvar)
	  /* Accept: memcpy (*char_var, "test", 1); that simplify
	     to char_var='t';  */
	  || is_gimple_min_invariant (srcvar)
	  || readonly_data_expr (src))
	return NULL_TREE;

      destvar = build_fold_indirect_ref (dest);
      if (TREE_THIS_VOLATILE (destvar))
	return NULL_TREE;
      if (!tree_int_cst_equal (lang_hooks.expr_size (destvar), len))
	return NULL_TREE;
      if (!var_decl_component_p (destvar))
	return NULL_TREE;

      if (srctype == desttype
	  || (gimple_in_ssa_p (cfun)
	      && useless_type_conversion_p (desttype, srctype)))
	expr = srcvar;
      else if ((INTEGRAL_TYPE_P (TREE_TYPE (srcvar))
	   || POINTER_TYPE_P (TREE_TYPE (srcvar)))
	  && (INTEGRAL_TYPE_P (TREE_TYPE (destvar))
	      || POINTER_TYPE_P (TREE_TYPE (destvar))))
	expr = fold_convert (TREE_TYPE (destvar), srcvar);
      else
	expr = fold_build1 (VIEW_CONVERT_EXPR, TREE_TYPE (destvar), srcvar);
      expr = build2 (MODIFY_EXPR, TREE_TYPE (destvar), destvar, expr);
    }

  if (ignore)
    return expr;

  if (endp == 0 || endp == 3)
    return omit_one_operand (type, dest, expr);

  if (expr == len)
    expr = NULL_TREE;

  if (endp == 2)
    len = fold_build2 (MINUS_EXPR, TREE_TYPE (len), len,
		       ssize_int (1));

  dest = fold_build2 (POINTER_PLUS_EXPR, TREE_TYPE (dest), dest, len);
  dest = fold_convert (type, dest);
  if (expr)
    dest = omit_one_operand (type, dest, expr);
  return dest;
}

/* Fold function call to builtin strcpy with arguments DEST and SRC.
   If LEN is not NULL, it represents the length of the string to be
   copied.  Return NULL_TREE if no simplification can be made.  */

tree
fold_builtin_strcpy (tree fndecl, tree dest, tree src, tree len)
{
  tree fn;

  if (!validate_arg (dest, POINTER_TYPE)
      || !validate_arg (src, POINTER_TYPE))
    return NULL_TREE;

  /* If SRC and DEST are the same (and not volatile), return DEST.  */
  if (operand_equal_p (src, dest, 0))
    return fold_convert (TREE_TYPE (TREE_TYPE (fndecl)), dest);

  if (optimize_size)
    return NULL_TREE;

  fn = implicit_built_in_decls[BUILT_IN_MEMCPY];
  if (!fn)
    return NULL_TREE;

  if (!len)
    {
      len = c_strlen (src, 1);
      if (! len || TREE_SIDE_EFFECTS (len))
	return NULL_TREE;
    }

  len = size_binop (PLUS_EXPR, len, ssize_int (1));
  return fold_convert (TREE_TYPE (TREE_TYPE (fndecl)),
		       build_call_expr (fn, 3, dest, src, len));
}

/* Fold function call to builtin strncpy with arguments DEST, SRC, and LEN.
   If SLEN is not NULL, it represents the length of the source string.
   Return NULL_TREE if no simplification can be made.  */

tree
fold_builtin_strncpy (tree fndecl, tree dest, tree src, tree len, tree slen)
{
  tree fn;

  if (!validate_arg (dest, POINTER_TYPE)
      || !validate_arg (src, POINTER_TYPE)
      || !validate_arg (len, INTEGER_TYPE))
    return NULL_TREE;

  /* If the LEN parameter is zero, return DEST.  */
  if (integer_zerop (len))
    return omit_one_operand (TREE_TYPE (TREE_TYPE (fndecl)), dest, src);

  /* We can't compare slen with len as constants below if len is not a
     constant.  */
  if (len == 0 || TREE_CODE (len) != INTEGER_CST)
    return NULL_TREE;

  if (!slen)
    slen = c_strlen (src, 1);

  /* Now, we must be passed a constant src ptr parameter.  */
  if (slen == 0 || TREE_CODE (slen) != INTEGER_CST)
    return NULL_TREE;

  slen = size_binop (PLUS_EXPR, slen, ssize_int (1));

  /* We do not support simplification of this case, though we do
     support it when expanding trees into RTL.  */
  /* FIXME: generate a call to __builtin_memset.  */
  if (tree_int_cst_lt (slen, len))
    return NULL_TREE;

  /* OK transform into builtin memcpy.  */
  fn = implicit_built_in_decls[BUILT_IN_MEMCPY];
  if (!fn)
    return NULL_TREE;
  return fold_convert (TREE_TYPE (TREE_TYPE (fndecl)),
		       build_call_expr (fn, 3, dest, src, len));
}

/* Fold function call to builtin memchr.  ARG1, ARG2 and LEN are the
   arguments to the call, and TYPE is its return type.
   Return NULL_TREE if no simplification can be made.  */

static tree
fold_builtin_memchr (tree arg1, tree arg2, tree len, tree type)
{
  if (!validate_arg (arg1, POINTER_TYPE)
      || !validate_arg (arg2, INTEGER_TYPE)
      || !validate_arg (len, INTEGER_TYPE))
    return NULL_TREE;
  else
    {
      const char *p1;

      if (TREE_CODE (arg2) != INTEGER_CST
	  || !host_integerp (len, 1))
	return NULL_TREE;

      p1 = c_getstr (arg1);
      if (p1 && compare_tree_int (len, strlen (p1) + 1) <= 0)
	{
	  char c;
	  const char *r;
	  tree tem;

	  if (target_char_cast (arg2, &c))
	    return NULL_TREE;

	  r = memchr (p1, c, tree_low_cst (len, 1));

	  if (r == NULL)
	    return build_int_cst (TREE_TYPE (arg1), 0);

	  tem = fold_build2 (POINTER_PLUS_EXPR, TREE_TYPE (arg1), arg1,
			     size_int (r - p1));
	  return fold_convert (type, tem);
	}
      return NULL_TREE;
    }
}

/* Fold function call to builtin memcmp with arguments ARG1 and ARG2.
   Return NULL_TREE if no simplification can be made.  */

static tree
fold_builtin_memcmp (tree arg1, tree arg2, tree len)
{
  const char *p1, *p2;

  if (!validate_arg (arg1, POINTER_TYPE)
      || !validate_arg (arg2, POINTER_TYPE)
      || !validate_arg (len, INTEGER_TYPE))
    return NULL_TREE;

  /* If the LEN parameter is zero, return zero.  */
  if (integer_zerop (len))
    return omit_two_operands (integer_type_node, integer_zero_node,
			      arg1, arg2);

  /* If ARG1 and ARG2 are the same (and not volatile), return zero.  */
  if (operand_equal_p (arg1, arg2, 0))
    return omit_one_operand (integer_type_node, integer_zero_node, len);

  p1 = c_getstr (arg1);
  p2 = c_getstr (arg2);

  /* If all arguments are constant, and the value of len is not greater
     than the lengths of arg1 and arg2, evaluate at compile-time.  */
  if (host_integerp (len, 1) && p1 && p2
      && compare_tree_int (len, strlen (p1) + 1) <= 0
      && compare_tree_int (len, strlen (p2) + 1) <= 0)
    {
      const int r = memcmp (p1, p2, tree_low_cst (len, 1));

      if (r > 0)
	return integer_one_node;
      else if (r < 0)
	return integer_minus_one_node;
      else
	return integer_zero_node;
    }

  /* If len parameter is one, return an expression corresponding to
     (*(const unsigned char*)arg1 - (const unsigned char*)arg2).  */
  if (host_integerp (len, 1) && tree_low_cst (len, 1) == 1)
    {
      tree cst_uchar_node = build_type_variant (unsigned_char_type_node, 1, 0);
      tree cst_uchar_ptr_node
	= build_pointer_type_for_mode (cst_uchar_node, ptr_mode, true);

      tree ind1 = fold_convert (integer_type_node,
				build1 (INDIRECT_REF, cst_uchar_node,
					fold_convert (cst_uchar_ptr_node,
						      arg1)));
      tree ind2 = fold_convert (integer_type_node,
				build1 (INDIRECT_REF, cst_uchar_node,
					fold_convert (cst_uchar_ptr_node,
						      arg2)));
      return fold_build2 (MINUS_EXPR, integer_type_node, ind1, ind2);
    }

  return NULL_TREE;
}

/* Fold function call to builtin strcmp with arguments ARG1 and ARG2.
   Return NULL_TREE if no simplification can be made.  */

static tree
fold_builtin_strcmp (tree arg1, tree arg2)
{
  const char *p1, *p2;

  if (!validate_arg (arg1, POINTER_TYPE)
      || !validate_arg (arg2, POINTER_TYPE))
    return NULL_TREE;

  /* If ARG1 and ARG2 are the same (and not volatile), return zero.  */
  if (operand_equal_p (arg1, arg2, 0))
    return integer_zero_node;

  p1 = c_getstr (arg1);
  p2 = c_getstr (arg2);

  if (p1 && p2)
    {
      const int i = strcmp (p1, p2);
      if (i < 0)
	return integer_minus_one_node;
      else if (i > 0)
	return integer_one_node;
      else
	return integer_zero_node;
    }

  /* If the second arg is "", return *(const unsigned char*)arg1.  */
  if (p2 && *p2 == '\0')
    {
      tree cst_uchar_node = build_type_variant (unsigned_char_type_node, 1, 0);
      tree cst_uchar_ptr_node
	= build_pointer_type_for_mode (cst_uchar_node, ptr_mode, true);

      return fold_convert (integer_type_node,
			   build1 (INDIRECT_REF, cst_uchar_node,
				   fold_convert (cst_uchar_ptr_node,
						 arg1)));
    }

  /* If the first arg is "", return -*(const unsigned char*)arg2.  */
  if (p1 && *p1 == '\0')
    {
      tree cst_uchar_node = build_type_variant (unsigned_char_type_node, 1, 0);
      tree cst_uchar_ptr_node
	= build_pointer_type_for_mode (cst_uchar_node, ptr_mode, true);

      tree temp = fold_convert (integer_type_node,
				build1 (INDIRECT_REF, cst_uchar_node,
					fold_convert (cst_uchar_ptr_node,
						      arg2)));
      return fold_build1 (NEGATE_EXPR, integer_type_node, temp);
    }

  return NULL_TREE;
}

/* Fold function call to builtin strncmp with arguments ARG1, ARG2, and LEN.
   Return NULL_TREE if no simplification can be made.  */

static tree
fold_builtin_strncmp (tree arg1, tree arg2, tree len)
{
  const char *p1, *p2;

  if (!validate_arg (arg1, POINTER_TYPE)
      || !validate_arg (arg2, POINTER_TYPE)
      || !validate_arg (len, INTEGER_TYPE))
    return NULL_TREE;

  /* If the LEN parameter is zero, return zero.  */
  if (integer_zerop (len))
    return omit_two_operands (integer_type_node, integer_zero_node,
			      arg1, arg2);

  /* If ARG1 and ARG2 are the same (and not volatile), return zero.  */
  if (operand_equal_p (arg1, arg2, 0))
    return omit_one_operand (integer_type_node, integer_zero_node, len);

  p1 = c_getstr (arg1);
  p2 = c_getstr (arg2);

  if (host_integerp (len, 1) && p1 && p2)
    {
      const int i = strncmp (p1, p2, tree_low_cst (len, 1));
      if (i > 0)
	return integer_one_node;
      else if (i < 0)
	return integer_minus_one_node;
      else
	return integer_zero_node;
    }

  /* If the second arg is "", and the length is greater than zero,
     return *(const unsigned char*)arg1.  */
  if (p2 && *p2 == '\0'
      && TREE_CODE (len) == INTEGER_CST
      && tree_int_cst_sgn (len) == 1)
    {
      tree cst_uchar_node = build_type_variant (unsigned_char_type_node, 1, 0);
      tree cst_uchar_ptr_node
	= build_pointer_type_for_mode (cst_uchar_node, ptr_mode, true);

      return fold_convert (integer_type_node,
			   build1 (INDIRECT_REF, cst_uchar_node,
				   fold_convert (cst_uchar_ptr_node,
						 arg1)));
    }

  /* If the first arg is "", and the length is greater than zero,
     return -*(const unsigned char*)arg2.  */
  if (p1 && *p1 == '\0'
      && TREE_CODE (len) == INTEGER_CST
      && tree_int_cst_sgn (len) == 1)
    {
      tree cst_uchar_node = build_type_variant (unsigned_char_type_node, 1, 0);
      tree cst_uchar_ptr_node
	= build_pointer_type_for_mode (cst_uchar_node, ptr_mode, true);

      tree temp = fold_convert (integer_type_node,
				build1 (INDIRECT_REF, cst_uchar_node,
					fold_convert (cst_uchar_ptr_node,
						      arg2)));
      return fold_build1 (NEGATE_EXPR, integer_type_node, temp);
    }

  /* If len parameter is one, return an expression corresponding to
     (*(const unsigned char*)arg1 - (const unsigned char*)arg2).  */
  if (host_integerp (len, 1) && tree_low_cst (len, 1) == 1)
    {
      tree cst_uchar_node = build_type_variant (unsigned_char_type_node, 1, 0);
      tree cst_uchar_ptr_node
	= build_pointer_type_for_mode (cst_uchar_node, ptr_mode, true);

      tree ind1 = fold_convert (integer_type_node,
				build1 (INDIRECT_REF, cst_uchar_node,
					fold_convert (cst_uchar_ptr_node,
						      arg1)));
      tree ind2 = fold_convert (integer_type_node,
				build1 (INDIRECT_REF, cst_uchar_node,
					fold_convert (cst_uchar_ptr_node,
						      arg2)));
      return fold_build2 (MINUS_EXPR, integer_type_node, ind1, ind2);
    }

  return NULL_TREE;
}

/* Fold function call to builtin signbit, signbitf or signbitl with argument
   ARG.  Return NULL_TREE if no simplification can be made.  */

static tree
fold_builtin_signbit (tree arg, tree type)
{
  tree temp;

  if (!validate_arg (arg, REAL_TYPE))
    return NULL_TREE;

  /* If ARG is a compile-time constant, determine the result.  */
  if (TREE_CODE (arg) == REAL_CST
      && !TREE_OVERFLOW (arg))
    {
      REAL_VALUE_TYPE c;

      c = TREE_REAL_CST (arg);
      temp = REAL_VALUE_NEGATIVE (c) ? integer_one_node : integer_zero_node;
      return fold_convert (type, temp);
    }

  /* If ARG is non-negative, the result is always zero.  */
  if (tree_expr_nonnegative_p (arg))
    return omit_one_operand (type, integer_zero_node, arg);

  /* If ARG's format doesn't have signed zeros, return "arg < 0.0".  */
  if (!HONOR_SIGNED_ZEROS (TYPE_MODE (TREE_TYPE (arg))))
    return fold_build2 (LT_EXPR, type, arg,
			build_real (TREE_TYPE (arg), dconst0));

  return NULL_TREE;
}

/* Fold function call to builtin copysign, copysignf or copysignl with
   arguments ARG1 and ARG2.  Return NULL_TREE if no simplification can
   be made.  */

static tree
fold_builtin_copysign (tree fndecl, tree arg1, tree arg2, tree type)
{
  tree tem;

  if (!validate_arg (arg1, REAL_TYPE)
      || !validate_arg (arg2, REAL_TYPE))
    return NULL_TREE;

  /* copysign(X,X) is X.  */
  if (operand_equal_p (arg1, arg2, 0))
    return fold_convert (type, arg1);

  /* If ARG1 and ARG2 are compile-time constants, determine the result.  */
  if (TREE_CODE (arg1) == REAL_CST
      && TREE_CODE (arg2) == REAL_CST
      && !TREE_OVERFLOW (arg1)
      && !TREE_OVERFLOW (arg2))
    {
      REAL_VALUE_TYPE c1, c2;

      c1 = TREE_REAL_CST (arg1);
      c2 = TREE_REAL_CST (arg2);
      /* c1.sign := c2.sign.  */
      real_copysign (&c1, &c2);
      return build_real (type, c1);
    }

  /* copysign(X, Y) is fabs(X) when Y is always non-negative.
     Remember to evaluate Y for side-effects.  */
  if (tree_expr_nonnegative_p (arg2))
    return omit_one_operand (type,
			     fold_build1 (ABS_EXPR, type, arg1),
			     arg2);

  /* Strip sign changing operations for the first argument.  */
  tem = fold_strip_sign_ops (arg1);
  if (tem)
    return build_call_expr (fndecl, 2, tem, arg2);

  return NULL_TREE;
}

/* Fold a call to builtin isascii with argument ARG.  */

static tree
fold_builtin_isascii (tree arg)
{
  if (!validate_arg (arg, INTEGER_TYPE))
    return NULL_TREE;
  else
    {
      /* Transform isascii(c) -> ((c & ~0x7f) == 0).  */
      arg = build2 (BIT_AND_EXPR, integer_type_node, arg,
		    build_int_cst (NULL_TREE,
				   ~ (unsigned HOST_WIDE_INT) 0x7f));
      return fold_build2 (EQ_EXPR, integer_type_node,
			  arg, integer_zero_node);
    }
}

/* Fold a call to builtin toascii with argument ARG.  */

static tree
fold_builtin_toascii (tree arg)
{
  if (!validate_arg (arg, INTEGER_TYPE))
    return NULL_TREE;
      
  /* Transform toascii(c) -> (c & 0x7f).  */
  return fold_build2 (BIT_AND_EXPR, integer_type_node, arg,
		      build_int_cst (NULL_TREE, 0x7f));
}

/* Fold a call to builtin isdigit with argument ARG.  */

static tree
fold_builtin_isdigit (tree arg)
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

      arg = fold_convert (unsigned_type_node, arg);
      arg = build2 (MINUS_EXPR, unsigned_type_node, arg,
		    build_int_cst (unsigned_type_node, target_digit0));
      return fold_build2 (LE_EXPR, integer_type_node, arg,
			  build_int_cst (unsigned_type_node, 9));
    }
}

/* Fold a call to fabs, fabsf or fabsl with argument ARG.  */

static tree
fold_builtin_fabs (tree arg, tree type)
{
  if (!validate_arg (arg, REAL_TYPE))
    return NULL_TREE;

  arg = fold_convert (type, arg);
  if (TREE_CODE (arg) == REAL_CST)
    return fold_abs_const (arg, type);
  return fold_build1 (ABS_EXPR, type, arg);
}

/* Fold a call to abs, labs, llabs or imaxabs with argument ARG.  */

static tree
fold_builtin_abs (tree arg, tree type)
{
  if (!validate_arg (arg, INTEGER_TYPE))
    return NULL_TREE;

  arg = fold_convert (type, arg);
  if (TREE_CODE (arg) == INTEGER_CST)
    return fold_abs_const (arg, type);
  return fold_build1 (ABS_EXPR, type, arg);
}

/* Fold a call to builtin fmin or fmax.  */

static tree
fold_builtin_fmin_fmax (tree arg0, tree arg1, tree type, bool max)
{
  if (validate_arg (arg0, REAL_TYPE) && validate_arg (arg1, REAL_TYPE))
    {
      /* Calculate the result when the argument is a constant.  */
      tree res = do_mpfr_arg2 (arg0, arg1, type, (max ? mpfr_max : mpfr_min));

      if (res)
	return res;

      /* If either argument is NaN, return the other one.  Avoid the
	 transformation if we get (and honor) a signalling NaN.  Using
	 omit_one_operand() ensures we create a non-lvalue.  */
      if (TREE_CODE (arg0) == REAL_CST
	  && real_isnan (&TREE_REAL_CST (arg0))
	  && (! HONOR_SNANS (TYPE_MODE (TREE_TYPE (arg0)))
	      || ! TREE_REAL_CST (arg0).signalling))
	return omit_one_operand (type, arg1, arg0);
      if (TREE_CODE (arg1) == REAL_CST
	  && real_isnan (&TREE_REAL_CST (arg1))
	  && (! HONOR_SNANS (TYPE_MODE (TREE_TYPE (arg1)))
	      || ! TREE_REAL_CST (arg1).signalling))
	return omit_one_operand (type, arg0, arg1);

      /* Transform fmin/fmax(x,x) -> x.  */
      if (operand_equal_p (arg0, arg1, OEP_PURE_SAME))
	return omit_one_operand (type, arg0, arg1);
      
      /* Convert fmin/fmax to MIN_EXPR/MAX_EXPR.  C99 requires these
	 functions to return the numeric arg if the other one is NaN.
	 These tree codes don't honor that, so only transform if
	 -ffinite-math-only is set.  C99 doesn't require -0.0 to be
	 handled, so we don't have to worry about it either.  */
      if (flag_finite_math_only)
	return fold_build2 ((max ? MAX_EXPR : MIN_EXPR), type,
			    fold_convert (type, arg0),
			    fold_convert (type, arg1));
    }
  return NULL_TREE;
}

/* Fold a call to builtin carg(a+bi) -> atan2(b,a).  */

static tree
fold_builtin_carg (tree arg, tree type)
{
  if (validate_arg (arg, COMPLEX_TYPE))
    {
      tree atan2_fn = mathfn_built_in (type, BUILT_IN_ATAN2);
      
      if (atan2_fn)
        {
  	  tree new_arg = builtin_save_expr (arg);
	  tree r_arg = fold_build1 (REALPART_EXPR, type, new_arg);
	  tree i_arg = fold_build1 (IMAGPART_EXPR, type, new_arg);
	  return build_call_expr (atan2_fn, 2, i_arg, r_arg);
	}
    }
  
  return NULL_TREE;
}

/* Fold a call to builtin logb/ilogb.  */

static tree
fold_builtin_logb (tree arg, tree rettype)
{
  if (! validate_arg (arg, REAL_TYPE))
    return NULL_TREE;
  
  STRIP_NOPS (arg);
      
  if (TREE_CODE (arg) == REAL_CST && ! TREE_OVERFLOW (arg))
    {
      const REAL_VALUE_TYPE *const value = TREE_REAL_CST_PTR (arg);
	  
      switch (value->cl)
      {
      case rvc_nan:
      case rvc_inf:
	/* If arg is Inf or NaN and we're logb, return it.  */
	if (TREE_CODE (rettype) == REAL_TYPE)
	  return fold_convert (rettype, arg);
	/* Fall through... */
      case rvc_zero:
	/* Zero may set errno and/or raise an exception for logb, also
	   for ilogb we don't know FP_ILOGB0.  */
	return NULL_TREE;
      case rvc_normal:
	/* For normal numbers, proceed iff radix == 2.  In GCC,
	   normalized significands are in the range [0.5, 1.0).  We
	   want the exponent as if they were [1.0, 2.0) so get the
	   exponent and subtract 1.  */
	if (REAL_MODE_FORMAT (TYPE_MODE (TREE_TYPE (arg)))->b == 2)
	  return fold_convert (rettype, build_int_cst (NULL_TREE,
						       REAL_EXP (value)-1));
	break;
      }
    }
  
  return NULL_TREE;
}

/* Fold a call to builtin significand, if radix == 2.  */

static tree
fold_builtin_significand (tree arg, tree rettype)
{
  if (! validate_arg (arg, REAL_TYPE))
    return NULL_TREE;
  
  STRIP_NOPS (arg);
      
  if (TREE_CODE (arg) == REAL_CST && ! TREE_OVERFLOW (arg))
    {
      const REAL_VALUE_TYPE *const value = TREE_REAL_CST_PTR (arg);
	  
      switch (value->cl)
      {
      case rvc_zero:
      case rvc_nan:
      case rvc_inf:
	/* If arg is +-0, +-Inf or +-NaN, then return it.  */
	return fold_convert (rettype, arg);
      case rvc_normal:
	/* For normal numbers, proceed iff radix == 2.  */
	if (REAL_MODE_FORMAT (TYPE_MODE (TREE_TYPE (arg)))->b == 2)
	  {
	    REAL_VALUE_TYPE result = *value;
	    /* In GCC, normalized significands are in the range [0.5,
	       1.0).  We want them to be [1.0, 2.0) so set the
	       exponent to 1.  */
	    SET_REAL_EXP (&result, 1);
	    return build_real (rettype, result);
	  }
	break;
      }
    }
  
  return NULL_TREE;
}

/* Fold a call to builtin frexp, we can assume the base is 2.  */

static tree
fold_builtin_frexp (tree arg0, tree arg1, tree rettype)
{
  if (! validate_arg (arg0, REAL_TYPE) || ! validate_arg (arg1, POINTER_TYPE))
    return NULL_TREE;
  
  STRIP_NOPS (arg0);
      
  if (!(TREE_CODE (arg0) == REAL_CST && ! TREE_OVERFLOW (arg0)))
    return NULL_TREE;
  
  arg1 = build_fold_indirect_ref (arg1);

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
	return omit_one_operand (rettype, arg0, arg1);
      case rvc_normal:
	{
	  /* Since the frexp function always expects base 2, and in
	     GCC normalized significands are already in the range
	     [0.5, 1.0), we have exactly what frexp wants.  */
	  REAL_VALUE_TYPE frac_rvt = *value;
	  SET_REAL_EXP (&frac_rvt, 0);
	  frac = build_real (rettype, frac_rvt);
	  exp = build_int_cst (NULL_TREE, REAL_EXP (value));
	}
	break;
      default:
	gcc_unreachable ();
      }
		
      /* Create the COMPOUND_EXPR (*arg1 = trunc, frac). */
      arg1 = fold_build2 (MODIFY_EXPR, rettype, arg1, exp);
      TREE_SIDE_EFFECTS (arg1) = 1;
      return fold_build2 (COMPOUND_EXPR, rettype, arg1, frac);
    }

  return NULL_TREE;
}

/* Fold a call to builtin ldexp or scalbn/scalbln.  If LDEXP is true
   then we can assume the base is two.  If it's false, then we have to
   check the mode of the TYPE parameter in certain cases.  */

static tree
fold_builtin_load_exponent (tree arg0, tree arg1, tree type, bool ldexp)
{
  if (validate_arg (arg0, REAL_TYPE) && validate_arg (arg1, INTEGER_TYPE))
    {
      STRIP_NOPS (arg0);
      STRIP_NOPS (arg1);

      /* If arg0 is 0, Inf or NaN, or if arg1 is 0, then return arg0.  */
      if (real_zerop (arg0) || integer_zerop (arg1)
	  || (TREE_CODE (arg0) == REAL_CST
	      && !real_isfinite (&TREE_REAL_CST (arg0))))
	return omit_one_operand (type, arg0, arg1);
      
      /* If both arguments are constant, then try to evaluate it.  */
      if ((ldexp || REAL_MODE_FORMAT (TYPE_MODE (type))->b == 2)
	  && TREE_CODE (arg0) == REAL_CST && !TREE_OVERFLOW (arg0)
	  && host_integerp (arg1, 0))
        {
	  /* Bound the maximum adjustment to twice the range of the
	     mode's valid exponents.  Use abs to ensure the range is
	     positive as a sanity check.  */
	  const long max_exp_adj = 2 * 
	    labs (REAL_MODE_FORMAT (TYPE_MODE (type))->emax
		 - REAL_MODE_FORMAT (TYPE_MODE (type))->emin);

	  /* Get the user-requested adjustment.  */
	  const HOST_WIDE_INT req_exp_adj = tree_low_cst (arg1, 0);
	  
	  /* The requested adjustment must be inside this range.  This
	     is a preliminary cap to avoid things like overflow, we
	     may still fail to compute the result for other reasons.  */
	  if (-max_exp_adj < req_exp_adj && req_exp_adj < max_exp_adj)
	    {
	      REAL_VALUE_TYPE initial_result;
	      
	      real_ldexp (&initial_result, &TREE_REAL_CST (arg0), req_exp_adj);

	      /* Ensure we didn't overflow.  */
	      if (! real_isinf (&initial_result))
	        {
		  const REAL_VALUE_TYPE trunc_result
		    = real_value_truncate (TYPE_MODE (type), initial_result);
		  
		  /* Only proceed if the target mode can hold the
		     resulting value.  */
		  if (REAL_VALUES_EQUAL (initial_result, trunc_result))
		    return build_real (type, trunc_result);
		}
	    }
	}
    }

  return NULL_TREE;
}

/* Fold a call to builtin modf.  */

static tree
fold_builtin_modf (tree arg0, tree arg1, tree rettype)
{
  if (! validate_arg (arg0, REAL_TYPE) || ! validate_arg (arg1, POINTER_TYPE))
    return NULL_TREE;
  
  STRIP_NOPS (arg0);
      
  if (!(TREE_CODE (arg0) == REAL_CST && ! TREE_OVERFLOW (arg0)))
    return NULL_TREE;
  
  arg1 = build_fold_indirect_ref (arg1);

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
      arg1 = fold_build2 (MODIFY_EXPR, rettype, arg1,
			  build_real (rettype, trunc));
      TREE_SIDE_EFFECTS (arg1) = 1;
      return fold_build2 (COMPOUND_EXPR, rettype, arg1,
			  build_real (rettype, frac));
    }
  
  return NULL_TREE;
}

/* Fold a call to __builtin_isnan(), __builtin_isinf, __builtin_finite.
   ARG is the argument for the call.  */

static tree
fold_builtin_classify (tree fndecl, tree arg, int builtin_index)
{
  tree type = TREE_TYPE (TREE_TYPE (fndecl));
  REAL_VALUE_TYPE r;

  if (!validate_arg (arg, REAL_TYPE))
    {
      error ("non-floating-point argument to function %qs",
	     IDENTIFIER_POINTER (DECL_NAME (fndecl)));
      return error_mark_node;
    }

  switch (builtin_index)
    {
    case BUILT_IN_ISINF:
      if (!HONOR_INFINITIES (TYPE_MODE (TREE_TYPE (arg))))
	return omit_one_operand (type, integer_zero_node, arg);

      if (TREE_CODE (arg) == REAL_CST)
	{
	  r = TREE_REAL_CST (arg);
	  if (real_isinf (&r))
	    return real_compare (GT_EXPR, &r, &dconst0)
		   ? integer_one_node : integer_minus_one_node;
	  else
	    return integer_zero_node;
	}

      return NULL_TREE;

    case BUILT_IN_ISFINITE:
      if (!HONOR_NANS (TYPE_MODE (TREE_TYPE (arg)))
	  && !HONOR_INFINITIES (TYPE_MODE (TREE_TYPE (arg))))
	return omit_one_operand (type, integer_one_node, arg);

      if (TREE_CODE (arg) == REAL_CST)
	{
	  r = TREE_REAL_CST (arg);
	  return real_isfinite (&r) ? integer_one_node : integer_zero_node;
	}

      return NULL_TREE;

    case BUILT_IN_ISNAN:
      if (!HONOR_NANS (TYPE_MODE (TREE_TYPE (arg))))
	return omit_one_operand (type, integer_zero_node, arg);

      if (TREE_CODE (arg) == REAL_CST)
	{
	  r = TREE_REAL_CST (arg);
	  return real_isnan (&r) ? integer_one_node : integer_zero_node;
	}

      arg = builtin_save_expr (arg);
      return fold_build2 (UNORDERED_EXPR, type, arg, arg);

    default:
      gcc_unreachable ();
    }
}

/* Fold a call to an unordered comparison function such as
   __builtin_isgreater().  FNDECL is the FUNCTION_DECL for the function
   being called and ARG0 and ARG1 are the arguments for the call.
   UNORDERED_CODE and ORDERED_CODE are comparison codes that give
   the opposite of the desired result.  UNORDERED_CODE is used
   for modes that can hold NaNs and ORDERED_CODE is used for
   the rest.  */

static tree
fold_builtin_unordered_cmp (tree fndecl, tree arg0, tree arg1,
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
  else
    {
      error ("non-floating-point argument to function %qs",
		 IDENTIFIER_POINTER (DECL_NAME (fndecl)));
      return error_mark_node;
    }

  arg0 = fold_convert (cmp_type, arg0);
  arg1 = fold_convert (cmp_type, arg1);

  if (unordered_code == UNORDERED_EXPR)
    {
      if (!HONOR_NANS (TYPE_MODE (TREE_TYPE (arg0))))
	return omit_two_operands (type, integer_zero_node, arg0, arg1);
      return fold_build2 (UNORDERED_EXPR, type, arg0, arg1);
    }

  code = HONOR_NANS (TYPE_MODE (TREE_TYPE (arg0))) ? unordered_code
						   : ordered_code;
  return fold_build1 (TRUTH_NOT_EXPR, type,
		      fold_build2 (code, type, arg0, arg1));
}

/* Fold a call to built-in function FNDECL with 0 arguments.
   IGNORE is true if the result of the function call is ignored.  This
   function returns NULL_TREE if no simplification was possible.  */

static tree
fold_builtin_0 (tree fndecl, bool ignore ATTRIBUTE_UNUSED)
{
  tree type = TREE_TYPE (TREE_TYPE (fndecl));
  enum built_in_function fcode = DECL_FUNCTION_CODE (fndecl);
  switch (fcode)
    {
    CASE_FLT_FN (BUILT_IN_INF):
    case BUILT_IN_INFD32:
    case BUILT_IN_INFD64:
    case BUILT_IN_INFD128:
      return fold_builtin_inf (type, true);

    CASE_FLT_FN (BUILT_IN_HUGE_VAL):
      return fold_builtin_inf (type, false);

    case BUILT_IN_CLASSIFY_TYPE:
      return fold_builtin_classify_type (NULL_TREE);

    default:
      break;
    }
  return NULL_TREE;
}

/* Fold a call to built-in function FNDECL with 1 argument, ARG0.
   IGNORE is true if the result of the function call is ignored.  This
   function returns NULL_TREE if no simplification was possible.  */

static tree
fold_builtin_1 (tree fndecl, tree arg0, bool ignore)
{
  tree type = TREE_TYPE (TREE_TYPE (fndecl));
  enum built_in_function fcode = DECL_FUNCTION_CODE (fndecl);
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
      return fold_builtin_strlen (arg0);

    CASE_FLT_FN (BUILT_IN_FABS):
      return fold_builtin_fabs (arg0, type);

    case BUILT_IN_ABS:
    case BUILT_IN_LABS:
    case BUILT_IN_LLABS:
    case BUILT_IN_IMAXABS:
      return fold_builtin_abs (arg0, type);

    CASE_FLT_FN (BUILT_IN_CONJ):
      if (validate_arg (arg0, COMPLEX_TYPE))
	return fold_build1 (CONJ_EXPR, type, arg0);
    break;

    CASE_FLT_FN (BUILT_IN_CREAL):
      if (validate_arg (arg0, COMPLEX_TYPE))
	return non_lvalue (fold_build1 (REALPART_EXPR, type, arg0));;
    break;

    CASE_FLT_FN (BUILT_IN_CIMAG):
      if (validate_arg (arg0, COMPLEX_TYPE))
	return non_lvalue (fold_build1 (IMAGPART_EXPR, type, arg0));
    break;

    CASE_FLT_FN (BUILT_IN_CCOS):
    CASE_FLT_FN (BUILT_IN_CCOSH):
      /* These functions are "even", i.e. f(x) == f(-x).  */
      if (validate_arg (arg0, COMPLEX_TYPE))
	{
	  tree narg = fold_strip_sign_ops (arg0);
	  if (narg)
	    return build_call_expr (fndecl, 1, narg);
	}
    break;

    CASE_FLT_FN (BUILT_IN_CABS):
      return fold_builtin_cabs (arg0, type, fndecl);

    CASE_FLT_FN (BUILT_IN_CARG):
      return fold_builtin_carg (arg0, type);

    CASE_FLT_FN (BUILT_IN_SQRT):
      return fold_builtin_sqrt (arg0, type);

    CASE_FLT_FN (BUILT_IN_CBRT):
      return fold_builtin_cbrt (arg0, type);

    CASE_FLT_FN (BUILT_IN_ASIN):
      if (validate_arg (arg0, REAL_TYPE))
	return do_mpfr_arg1 (arg0, type, mpfr_asin,
			     &dconstm1, &dconst1, true);
    break;

    CASE_FLT_FN (BUILT_IN_ACOS):
      if (validate_arg (arg0, REAL_TYPE))
	return do_mpfr_arg1 (arg0, type, mpfr_acos,
			     &dconstm1, &dconst1, true);
    break;

    CASE_FLT_FN (BUILT_IN_ATAN):
      if (validate_arg (arg0, REAL_TYPE))
	return do_mpfr_arg1 (arg0, type, mpfr_atan, NULL, NULL, 0);
    break;

    CASE_FLT_FN (BUILT_IN_ASINH):
      if (validate_arg (arg0, REAL_TYPE))
	return do_mpfr_arg1 (arg0, type, mpfr_asinh, NULL, NULL, 0);
    break;

    CASE_FLT_FN (BUILT_IN_ACOSH):
      if (validate_arg (arg0, REAL_TYPE))
	return do_mpfr_arg1 (arg0, type, mpfr_acosh,
			     &dconst1, NULL, true);
    break;

    CASE_FLT_FN (BUILT_IN_ATANH):
      if (validate_arg (arg0, REAL_TYPE))
	return do_mpfr_arg1 (arg0, type, mpfr_atanh,
			     &dconstm1, &dconst1, false);
    break;

    CASE_FLT_FN (BUILT_IN_SIN):
      if (validate_arg (arg0, REAL_TYPE))
	return do_mpfr_arg1 (arg0, type, mpfr_sin, NULL, NULL, 0);
    break;

    CASE_FLT_FN (BUILT_IN_COS):
      return fold_builtin_cos (arg0, type, fndecl);
    break;

    CASE_FLT_FN (BUILT_IN_TAN):
      return fold_builtin_tan (arg0, type);

    CASE_FLT_FN (BUILT_IN_CEXP):
      return fold_builtin_cexp (arg0, type);

    CASE_FLT_FN (BUILT_IN_CEXPI):
      if (validate_arg (arg0, REAL_TYPE))
	return do_mpfr_sincos (arg0, NULL_TREE, NULL_TREE);
    break;

    CASE_FLT_FN (BUILT_IN_SINH):
      if (validate_arg (arg0, REAL_TYPE))
	return do_mpfr_arg1 (arg0, type, mpfr_sinh, NULL, NULL, 0);
    break;

    CASE_FLT_FN (BUILT_IN_COSH):
      return fold_builtin_cosh (arg0, type, fndecl);

    CASE_FLT_FN (BUILT_IN_TANH):
      if (validate_arg (arg0, REAL_TYPE))
	return do_mpfr_arg1 (arg0, type, mpfr_tanh, NULL, NULL, 0);
    break;

    CASE_FLT_FN (BUILT_IN_ERF):
      if (validate_arg (arg0, REAL_TYPE))
	return do_mpfr_arg1 (arg0, type, mpfr_erf, NULL, NULL, 0);
    break;

    CASE_FLT_FN (BUILT_IN_ERFC):
      if (validate_arg (arg0, REAL_TYPE))
	return do_mpfr_arg1 (arg0, type, mpfr_erfc, NULL, NULL, 0);
    break;

    CASE_FLT_FN (BUILT_IN_TGAMMA):
      if (validate_arg (arg0, REAL_TYPE))
	return do_mpfr_arg1 (arg0, type, mpfr_gamma, NULL, NULL, 0);
    break;
 
    CASE_FLT_FN (BUILT_IN_EXP):
      return fold_builtin_exponent (fndecl, arg0, mpfr_exp);

    CASE_FLT_FN (BUILT_IN_EXP2):
      return fold_builtin_exponent (fndecl, arg0, mpfr_exp2);

    CASE_FLT_FN (BUILT_IN_EXP10):
    CASE_FLT_FN (BUILT_IN_POW10):
      return fold_builtin_exponent (fndecl, arg0, mpfr_exp10);

    CASE_FLT_FN (BUILT_IN_EXPM1):
      if (validate_arg (arg0, REAL_TYPE))
	return do_mpfr_arg1 (arg0, type, mpfr_expm1, NULL, NULL, 0);
    break;
 
    CASE_FLT_FN (BUILT_IN_LOG):
      return fold_builtin_logarithm (fndecl, arg0, mpfr_log);

    CASE_FLT_FN (BUILT_IN_LOG2):
      return fold_builtin_logarithm (fndecl, arg0, mpfr_log2);

    CASE_FLT_FN (BUILT_IN_LOG10):
      return fold_builtin_logarithm (fndecl, arg0, mpfr_log10);

    CASE_FLT_FN (BUILT_IN_LOG1P):
      if (validate_arg (arg0, REAL_TYPE))
	return do_mpfr_arg1 (arg0, type, mpfr_log1p,
			     &dconstm1, NULL, false);
    break;

#if MPFR_VERSION >= MPFR_VERSION_NUM(2,3,0)
    CASE_FLT_FN (BUILT_IN_J0):
      if (validate_arg (arg0, REAL_TYPE))
	return do_mpfr_arg1 (arg0, type, mpfr_j0,
			     NULL, NULL, 0);
    break;

    CASE_FLT_FN (BUILT_IN_J1):
      if (validate_arg (arg0, REAL_TYPE))
	return do_mpfr_arg1 (arg0, type, mpfr_j1,
			     NULL, NULL, 0);
    break;

    CASE_FLT_FN (BUILT_IN_Y0):
      if (validate_arg (arg0, REAL_TYPE))
	return do_mpfr_arg1 (arg0, type, mpfr_y0,
			     &dconst0, NULL, false);
    break;

    CASE_FLT_FN (BUILT_IN_Y1):
      if (validate_arg (arg0, REAL_TYPE))
	return do_mpfr_arg1 (arg0, type, mpfr_y1,
			     &dconst0, NULL, false);
    break;
#endif

    CASE_FLT_FN (BUILT_IN_NAN):
    case BUILT_IN_NAND32:
    case BUILT_IN_NAND64:
    case BUILT_IN_NAND128:
      return fold_builtin_nan (arg0, type, true);

    CASE_FLT_FN (BUILT_IN_NANS):
      return fold_builtin_nan (arg0, type, false);

    CASE_FLT_FN (BUILT_IN_FLOOR):
      return fold_builtin_floor (fndecl, arg0);

    CASE_FLT_FN (BUILT_IN_CEIL):
      return fold_builtin_ceil (fndecl, arg0);

    CASE_FLT_FN (BUILT_IN_TRUNC):
      return fold_builtin_trunc (fndecl, arg0);

    CASE_FLT_FN (BUILT_IN_ROUND):
      return fold_builtin_round (fndecl, arg0);

    CASE_FLT_FN (BUILT_IN_NEARBYINT):
    CASE_FLT_FN (BUILT_IN_RINT):
      return fold_trunc_transparent_mathfn (fndecl, arg0);

    CASE_FLT_FN (BUILT_IN_LCEIL):
    CASE_FLT_FN (BUILT_IN_LLCEIL):
    CASE_FLT_FN (BUILT_IN_LFLOOR):
    CASE_FLT_FN (BUILT_IN_LLFLOOR):
    CASE_FLT_FN (BUILT_IN_LROUND):
    CASE_FLT_FN (BUILT_IN_LLROUND):
      return fold_builtin_int_roundingfn (fndecl, arg0);

    CASE_FLT_FN (BUILT_IN_LRINT):
    CASE_FLT_FN (BUILT_IN_LLRINT):
      return fold_fixed_mathfn (fndecl, arg0);

    case BUILT_IN_BSWAP32:
    case BUILT_IN_BSWAP64:
      return fold_builtin_bswap (fndecl, arg0);

    CASE_INT_FN (BUILT_IN_FFS):
    CASE_INT_FN (BUILT_IN_CLZ):
    CASE_INT_FN (BUILT_IN_CTZ):
    CASE_INT_FN (BUILT_IN_POPCOUNT):
    CASE_INT_FN (BUILT_IN_PARITY):
      return fold_builtin_bitop (fndecl, arg0);

    CASE_FLT_FN (BUILT_IN_SIGNBIT):
      return fold_builtin_signbit (arg0, type);

    CASE_FLT_FN (BUILT_IN_SIGNIFICAND):
      return fold_builtin_significand (arg0, type);

    CASE_FLT_FN (BUILT_IN_ILOGB):
    CASE_FLT_FN (BUILT_IN_LOGB):
      return fold_builtin_logb (arg0, type);

    case BUILT_IN_ISASCII:
      return fold_builtin_isascii (arg0);

    case BUILT_IN_TOASCII:
      return fold_builtin_toascii (arg0);

    case BUILT_IN_ISDIGIT:
      return fold_builtin_isdigit (arg0);

    CASE_FLT_FN (BUILT_IN_FINITE):
    case BUILT_IN_FINITED32:
    case BUILT_IN_FINITED64:
    case BUILT_IN_FINITED128:
    case BUILT_IN_ISFINITE:
      return fold_builtin_classify (fndecl, arg0, BUILT_IN_ISFINITE);

    CASE_FLT_FN (BUILT_IN_ISINF):
    case BUILT_IN_ISINFD32:
    case BUILT_IN_ISINFD64:
    case BUILT_IN_ISINFD128:
      return fold_builtin_classify (fndecl, arg0, BUILT_IN_ISINF);

    CASE_FLT_FN (BUILT_IN_ISNAN):
    case BUILT_IN_ISNAND32:
    case BUILT_IN_ISNAND64:
    case BUILT_IN_ISNAND128:
      return fold_builtin_classify (fndecl, arg0, BUILT_IN_ISNAN);

    case BUILT_IN_ISNORMAL:
      if (!validate_arg (arg0, REAL_TYPE))
	{
	  error ("non-floating-point argument to function %qs",
		 IDENTIFIER_POINTER (DECL_NAME (fndecl)));
	  return error_mark_node;
	}
      break;

    case BUILT_IN_PRINTF:
    case BUILT_IN_PRINTF_UNLOCKED:
    case BUILT_IN_VPRINTF:
      return fold_builtin_printf (fndecl, arg0, NULL_TREE, ignore, fcode);

    default:
      break;
    }

  return NULL_TREE;

}

/* Fold a call to built-in function FNDECL with 2 arguments, ARG0 and ARG1.
   IGNORE is true if the result of the function call is ignored.  This
   function returns NULL_TREE if no simplification was possible.  */

static tree
fold_builtin_2 (tree fndecl, tree arg0, tree arg1, bool ignore)
{
  tree type = TREE_TYPE (TREE_TYPE (fndecl));
  enum built_in_function fcode = DECL_FUNCTION_CODE (fndecl);

  switch (fcode)
    {
#if MPFR_VERSION >= MPFR_VERSION_NUM(2,3,0)
    CASE_FLT_FN (BUILT_IN_JN):
      if (validate_arg (arg0, INTEGER_TYPE)
	  && validate_arg (arg1, REAL_TYPE))
	return do_mpfr_bessel_n (arg0, arg1, type, mpfr_jn, NULL, 0);
    break;

    CASE_FLT_FN (BUILT_IN_YN):
      if (validate_arg (arg0, INTEGER_TYPE)
	  && validate_arg (arg1, REAL_TYPE))
	return do_mpfr_bessel_n (arg0, arg1, type, mpfr_yn,
				 &dconst0, false);
    break;

    CASE_FLT_FN (BUILT_IN_DREM):
    CASE_FLT_FN (BUILT_IN_REMAINDER):
      if (validate_arg (arg0, REAL_TYPE)
          && validate_arg(arg1, REAL_TYPE))
        return do_mpfr_arg2 (arg0, arg1, type, mpfr_remainder);
    break;

    CASE_FLT_FN_REENT (BUILT_IN_GAMMA): /* GAMMA_R */
    CASE_FLT_FN_REENT (BUILT_IN_LGAMMA): /* LGAMMA_R */
      if (validate_arg (arg0, REAL_TYPE)
	  && validate_arg(arg1, POINTER_TYPE))
	return do_mpfr_lgamma_r (arg0, arg1, type);
    break;
#endif

    CASE_FLT_FN (BUILT_IN_ATAN2):
      if (validate_arg (arg0, REAL_TYPE)
	  && validate_arg(arg1, REAL_TYPE))
	return do_mpfr_arg2 (arg0, arg1, type, mpfr_atan2);
    break;

    CASE_FLT_FN (BUILT_IN_FDIM):
      if (validate_arg (arg0, REAL_TYPE)
	  && validate_arg(arg1, REAL_TYPE))
	return do_mpfr_arg2 (arg0, arg1, type, mpfr_dim);
    break;

    CASE_FLT_FN (BUILT_IN_HYPOT):
      return fold_builtin_hypot (fndecl, arg0, arg1, type);

    CASE_FLT_FN (BUILT_IN_LDEXP):
      return fold_builtin_load_exponent (arg0, arg1, type, /*ldexp=*/true);
    CASE_FLT_FN (BUILT_IN_SCALBN):
    CASE_FLT_FN (BUILT_IN_SCALBLN):
      return fold_builtin_load_exponent (arg0, arg1, type, /*ldexp=*/false);

    CASE_FLT_FN (BUILT_IN_FREXP):
      return fold_builtin_frexp (arg0, arg1, type);

    CASE_FLT_FN (BUILT_IN_MODF):
      return fold_builtin_modf (arg0, arg1, type);

    case BUILT_IN_BZERO:
      return fold_builtin_bzero (arg0, arg1, ignore);

    case BUILT_IN_FPUTS:
      return fold_builtin_fputs (arg0, arg1, ignore, false, NULL_TREE);

    case BUILT_IN_FPUTS_UNLOCKED:
      return fold_builtin_fputs (arg0, arg1, ignore, true, NULL_TREE);

    case BUILT_IN_STRSTR:
      return fold_builtin_strstr (arg0, arg1, type);

    case BUILT_IN_STRCAT:
      return fold_builtin_strcat (arg0, arg1);

    case BUILT_IN_STRSPN:
      return fold_builtin_strspn (arg0, arg1);

    case BUILT_IN_STRCSPN:
      return fold_builtin_strcspn (arg0, arg1);

    case BUILT_IN_STRCHR:
    case BUILT_IN_INDEX:
      return fold_builtin_strchr (arg0, arg1, type);

    case BUILT_IN_STRRCHR:
    case BUILT_IN_RINDEX:
      return fold_builtin_strrchr (arg0, arg1, type);

    case BUILT_IN_STRCPY:
      return fold_builtin_strcpy (fndecl, arg0, arg1, NULL_TREE);

    case BUILT_IN_STRCMP:
      return fold_builtin_strcmp (arg0, arg1);

    case BUILT_IN_STRPBRK:
      return fold_builtin_strpbrk (arg0, arg1, type);

    case BUILT_IN_EXPECT:
      return fold_builtin_expect (arg0, arg1);

    CASE_FLT_FN (BUILT_IN_POW):
      return fold_builtin_pow (fndecl, arg0, arg1, type);

    CASE_FLT_FN (BUILT_IN_POWI):
      return fold_builtin_powi (fndecl, arg0, arg1, type);

    CASE_FLT_FN (BUILT_IN_COPYSIGN):
      return fold_builtin_copysign (fndecl, arg0, arg1, type);

    CASE_FLT_FN (BUILT_IN_FMIN):
      return fold_builtin_fmin_fmax (arg0, arg1, type, /*max=*/false);

    CASE_FLT_FN (BUILT_IN_FMAX):
      return fold_builtin_fmin_fmax (arg0, arg1, type, /*max=*/true);

    case BUILT_IN_ISGREATER:
      return fold_builtin_unordered_cmp (fndecl, arg0, arg1, UNLE_EXPR, LE_EXPR);
    case BUILT_IN_ISGREATEREQUAL:
      return fold_builtin_unordered_cmp (fndecl, arg0, arg1, UNLT_EXPR, LT_EXPR);
    case BUILT_IN_ISLESS:
      return fold_builtin_unordered_cmp (fndecl, arg0, arg1, UNGE_EXPR, GE_EXPR);
    case BUILT_IN_ISLESSEQUAL:
      return fold_builtin_unordered_cmp (fndecl, arg0, arg1, UNGT_EXPR, GT_EXPR);
    case BUILT_IN_ISLESSGREATER:
      return fold_builtin_unordered_cmp (fndecl, arg0, arg1, UNEQ_EXPR, EQ_EXPR);
    case BUILT_IN_ISUNORDERED:
      return fold_builtin_unordered_cmp (fndecl, arg0, arg1, UNORDERED_EXPR,
					 NOP_EXPR);

      /* We do the folding for va_start in the expander.  */
    case BUILT_IN_VA_START:
      break;

    case BUILT_IN_SPRINTF:
      return fold_builtin_sprintf (arg0, arg1, NULL_TREE, ignore);

    case BUILT_IN_OBJECT_SIZE:
      return fold_builtin_object_size (arg0, arg1);

    case BUILT_IN_PRINTF:
    case BUILT_IN_PRINTF_UNLOCKED:
    case BUILT_IN_VPRINTF:
      return fold_builtin_printf (fndecl, arg0, arg1, ignore, fcode);

    case BUILT_IN_PRINTF_CHK:
    case BUILT_IN_VPRINTF_CHK:
      if (!validate_arg (arg0, INTEGER_TYPE)
	  || TREE_SIDE_EFFECTS (arg0))
	return NULL_TREE;
      else
	return fold_builtin_printf (fndecl, arg1, NULL_TREE, ignore, fcode);
    break;

    case BUILT_IN_FPRINTF:
    case BUILT_IN_FPRINTF_UNLOCKED:
    case BUILT_IN_VFPRINTF:
      return fold_builtin_fprintf (fndecl, arg0, arg1, NULL_TREE,
				   ignore, fcode);

    default:
      break;
    }
  return NULL_TREE;
}

/* Fold a call to built-in function FNDECL with 3 arguments, ARG0, ARG1,
   and ARG2.  IGNORE is true if the result of the function call is ignored.
   This function returns NULL_TREE if no simplification was possible.  */

static tree
fold_builtin_3 (tree fndecl, tree arg0, tree arg1, tree arg2, bool ignore)
{
  tree type = TREE_TYPE (TREE_TYPE (fndecl));
  enum built_in_function fcode = DECL_FUNCTION_CODE (fndecl);
  switch (fcode)
    {

    CASE_FLT_FN (BUILT_IN_SINCOS):
      return fold_builtin_sincos (arg0, arg1, arg2);

    CASE_FLT_FN (BUILT_IN_FMA):
      if (validate_arg (arg0, REAL_TYPE)
	  && validate_arg(arg1, REAL_TYPE)
	  && validate_arg(arg2, REAL_TYPE))
	return do_mpfr_arg3 (arg0, arg1, arg2, type, mpfr_fma);
    break;

#if MPFR_VERSION >= MPFR_VERSION_NUM(2,3,0)
    CASE_FLT_FN (BUILT_IN_REMQUO):
      if (validate_arg (arg0, REAL_TYPE)
	  && validate_arg(arg1, REAL_TYPE)
	  && validate_arg(arg2, POINTER_TYPE))
	return do_mpfr_remquo (arg0, arg1, arg2);
    break;
#endif

    case BUILT_IN_MEMSET:
      return fold_builtin_memset (arg0, arg1, arg2, type, ignore);

    case BUILT_IN_BCOPY:
	return fold_builtin_memory_op (arg1, arg0, arg2, void_type_node, true, /*endp=*/3);

    case BUILT_IN_MEMCPY:
      return fold_builtin_memory_op (arg0, arg1, arg2, type, ignore, /*endp=*/0);

    case BUILT_IN_MEMPCPY:
      return fold_builtin_memory_op (arg0, arg1, arg2, type, ignore, /*endp=*/1);

    case BUILT_IN_MEMMOVE:
      return fold_builtin_memory_op (arg0, arg1, arg2, type, ignore, /*endp=*/3);

    case BUILT_IN_STRNCAT:
      return fold_builtin_strncat (arg0, arg1, arg2);

    case BUILT_IN_STRNCPY:
      return fold_builtin_strncpy (fndecl, arg0, arg1, arg2, NULL_TREE);

    case BUILT_IN_STRNCMP:
      return fold_builtin_strncmp (arg0, arg1, arg2);

    case BUILT_IN_MEMCHR:
      return fold_builtin_memchr (arg0, arg1, arg2, type);

    case BUILT_IN_BCMP:
    case BUILT_IN_MEMCMP:
      return fold_builtin_memcmp (arg0, arg1, arg2);;

    case BUILT_IN_SPRINTF:
      return fold_builtin_sprintf (arg0, arg1, arg2, ignore);

    case BUILT_IN_STRCPY_CHK:
    case BUILT_IN_STPCPY_CHK:
      return fold_builtin_stxcpy_chk (fndecl, arg0, arg1, arg2, NULL_TREE,
				      ignore, fcode);

    case BUILT_IN_STRCAT_CHK:
      return fold_builtin_strcat_chk (fndecl, arg0, arg1, arg2);

    case BUILT_IN_PRINTF_CHK:
    case BUILT_IN_VPRINTF_CHK:
      if (!validate_arg (arg0, INTEGER_TYPE)
	  || TREE_SIDE_EFFECTS (arg0))
	return NULL_TREE;
      else
	return fold_builtin_printf (fndecl, arg1, arg2, ignore, fcode);
    break;

    case BUILT_IN_FPRINTF:
    case BUILT_IN_FPRINTF_UNLOCKED:
    case BUILT_IN_VFPRINTF:
      return fold_builtin_fprintf (fndecl, arg0, arg1, arg2, ignore, fcode);

    case BUILT_IN_FPRINTF_CHK:
    case BUILT_IN_VFPRINTF_CHK:
      if (!validate_arg (arg1, INTEGER_TYPE)
	  || TREE_SIDE_EFFECTS (arg1))
	return NULL_TREE;
      else
	return fold_builtin_fprintf (fndecl, arg0, arg2, NULL_TREE,
				     ignore, fcode);

    default:
      break;
    }
  return NULL_TREE;
}

/* Fold a call to built-in function FNDECL with 4 arguments, ARG0, ARG1,
   ARG2, and ARG3.  IGNORE is true if the result of the function call is
   ignored.  This function returns NULL_TREE if no simplification was
   possible.  */
 
static tree
fold_builtin_4 (tree fndecl, tree arg0, tree arg1, tree arg2, tree arg3,
		bool ignore)
{
  enum built_in_function fcode = DECL_FUNCTION_CODE (fndecl);

  switch (fcode)
    {
    case BUILT_IN_MEMCPY_CHK:
    case BUILT_IN_MEMPCPY_CHK:
    case BUILT_IN_MEMMOVE_CHK:
    case BUILT_IN_MEMSET_CHK:
      return fold_builtin_memory_chk (fndecl, arg0, arg1, arg2, arg3,
				      NULL_TREE, ignore,
				      DECL_FUNCTION_CODE (fndecl));

    case BUILT_IN_STRNCPY_CHK:
      return fold_builtin_strncpy_chk (arg0, arg1, arg2, arg3, NULL_TREE);

    case BUILT_IN_STRNCAT_CHK:
      return fold_builtin_strncat_chk (fndecl, arg0, arg1, arg2, arg3);

    case BUILT_IN_FPRINTF_CHK:
    case BUILT_IN_VFPRINTF_CHK:
      if (!validate_arg (arg1, INTEGER_TYPE)
	  || TREE_SIDE_EFFECTS (arg1))
	return NULL_TREE;
      else
	return fold_builtin_fprintf (fndecl, arg0, arg2, arg3,
				     ignore, fcode);
    break;

    default:
      break;
    }
  return NULL_TREE;
}

/* Fold a call to built-in function FNDECL.  ARGS is an array of NARGS
    arguments, where NARGS <= 4.  IGNORE is true if the result of the
    function call is ignored.  This function returns NULL_TREE if no
    simplification was possible.  Note that this only folds builtins with
    fixed argument patterns.  Foldings that do varargs-to-varargs
    transformations, or that match calls with more than 4 arguments,
    need to be handled with fold_builtin_varargs instead.  */
 
#define MAX_ARGS_TO_FOLD_BUILTIN 4
 
static tree
fold_builtin_n (tree fndecl, tree *args, int nargs, bool ignore)
{
  enum built_in_function fcode = DECL_FUNCTION_CODE (fndecl);
  tree ret = NULL_TREE;

  /* Verify the number of arguments for type-generic and thus variadic
     builtins.  */
  switch (fcode)
    {
    case BUILT_IN_ISFINITE:
    case BUILT_IN_ISINF:
    case BUILT_IN_ISNAN:
    case BUILT_IN_ISNORMAL:
      if (nargs < 1)
	{
	  error ("too few arguments to function %qs",
		 IDENTIFIER_POINTER (DECL_NAME (fndecl)));
	  return error_mark_node;
	}
      else if (nargs > 1)
	{
	  error ("too many arguments to function %qs",
		 IDENTIFIER_POINTER (DECL_NAME (fndecl)));
	  return error_mark_node;
	}
      break;

    case BUILT_IN_ISGREATER:
    case BUILT_IN_ISGREATEREQUAL:
    case BUILT_IN_ISLESS:
    case BUILT_IN_ISLESSEQUAL:
    case BUILT_IN_ISLESSGREATER:
    case BUILT_IN_ISUNORDERED:
      if (nargs < 2)
	{
	  error ("too few arguments to function %qs",
		 IDENTIFIER_POINTER (DECL_NAME (fndecl)));
	  return error_mark_node;
	}
      else if (nargs > 2)
	{
	  error ("too many arguments to function %qs",
		 IDENTIFIER_POINTER (DECL_NAME (fndecl)));
	  return error_mark_node;
	}
      break;

    default:
      break;
    }

  switch (nargs)
    {
    case 0:
      ret = fold_builtin_0 (fndecl, ignore);
      break;
    case 1:
      ret = fold_builtin_1 (fndecl, args[0], ignore);
      break;
    case 2:
      ret = fold_builtin_2 (fndecl, args[0], args[1], ignore);
      break;
    case 3:
      ret = fold_builtin_3 (fndecl, args[0], args[1], args[2], ignore);
      break;
    case 4:
      ret = fold_builtin_4 (fndecl, args[0], args[1], args[2], args[3],
 			    ignore);
      break;
    default:
      break;
    }
  if (ret)
    {
      ret = build1 (NOP_EXPR, GENERIC_TREE_TYPE (ret), ret);
      TREE_NO_WARNING (ret) = 1;
      return ret;
    }
  return NULL_TREE;
}

/* Builtins with folding operations that operate on "..." arguments
   need special handling; we need to store the arguments in a convenient
   data structure before attempting any folding.  Fortunately there are
   only a few builtins that fall into this category.  FNDECL is the
   function, EXP is the CALL_EXPR for the call, and IGNORE is true if the
   result of the function call is ignored.  */

static tree
fold_builtin_varargs (tree fndecl, tree exp, bool ignore ATTRIBUTE_UNUSED)
{
  enum built_in_function fcode = DECL_FUNCTION_CODE (fndecl);
  tree ret = NULL_TREE;

  switch (fcode)
    {
    case BUILT_IN_SPRINTF_CHK:
    case BUILT_IN_VSPRINTF_CHK:
      ret = fold_builtin_sprintf_chk (exp, fcode);
      break;

    case BUILT_IN_SNPRINTF_CHK:
    case BUILT_IN_VSNPRINTF_CHK:
      ret = fold_builtin_snprintf_chk (exp, NULL_TREE, fcode);

    default:
      break;
    }
  if (ret)
    {
      ret = build1 (NOP_EXPR, TREE_TYPE (ret), ret);
      TREE_NO_WARNING (ret) = 1;
      return ret;
    }
  return NULL_TREE;
}

/* A wrapper function for builtin folding that prevents warnings for
   "statement without effect" and the like, caused by removing the
   call node earlier than the warning is generated.  */

tree
fold_call_expr (tree exp, bool ignore)
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

      /* FIXME: Don't use a list in this interface.  */
      if (DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_MD)
	  return targetm.fold_builtin (fndecl, CALL_EXPR_ARGS (exp), ignore);
      else
	{
	  if (nargs <= MAX_ARGS_TO_FOLD_BUILTIN)
	    {
	      tree *args = CALL_EXPR_ARGP (exp);
	      ret = fold_builtin_n (fndecl, args, nargs, ignore);
	    }
	  if (!ret)
	    ret = fold_builtin_varargs (fndecl, exp, ignore);
	  if (ret)
	    {
	      /* Propagate location information from original call to
		 expansion of builtin.  Otherwise things like
		 maybe_emit_chk_warning, that operate on the expansion
		 of a builtin, will use the wrong location information.  */
	      if (CAN_HAVE_LOCATION_P (exp) && EXPR_HAS_LOCATION (exp))
		{
		  tree realret = ret;
		  if (TREE_CODE (ret) == NOP_EXPR)
		    realret = TREE_OPERAND (ret, 0);
		  if (CAN_HAVE_LOCATION_P (realret)
		      && !EXPR_HAS_LOCATION (realret))
		    SET_EXPR_LOCATION (realret, EXPR_LOCATION (exp));
		}
	      return ret;
	    }
	}
    }
  return NULL_TREE;
}
 
/* Conveniently construct a function call expression.  FNDECL names the
    function to be called and ARGLIST is a TREE_LIST of arguments.  */
 
tree
build_function_call_expr (tree fndecl, tree arglist)
{
  tree fntype = TREE_TYPE (fndecl);
  tree fn = build1 (ADDR_EXPR, build_pointer_type (fntype), fndecl);
  int n = list_length (arglist);
  tree *argarray = (tree *) alloca (n * sizeof (tree));
  int i;
  
  for (i = 0; i < n; i++, arglist = TREE_CHAIN (arglist))
    argarray[i] = TREE_VALUE (arglist);
  return fold_builtin_call_array (TREE_TYPE (fntype), fn, n, argarray);
}

/* Conveniently construct a function call expression.  FNDECL names the
   function to be called, N is the number of arguments, and the "..."
   parameters are the argument expressions.  */
 
tree
build_call_expr (tree fndecl, int n, ...)
{
  va_list ap;
  tree fntype = TREE_TYPE (fndecl);
  tree fn = build1 (ADDR_EXPR, build_pointer_type (fntype), fndecl);
  tree *argarray = (tree *) alloca (n * sizeof (tree));
  int i;

  va_start (ap, n);
  for (i = 0; i < n; i++)
    argarray[i] = va_arg (ap, tree);
  va_end (ap);
  return fold_builtin_call_array (TREE_TYPE (fntype), fn, n, argarray);
}

/* Construct a CALL_EXPR with type TYPE with FN as the function expression.
   N arguments are passed in the array ARGARRAY.  */

tree
fold_builtin_call_array (tree type,
			 tree fn,
			 int n,
			 tree *argarray)
{
  tree ret = NULL_TREE;
  int i;
   tree exp;

  if (TREE_CODE (fn) == ADDR_EXPR)
  {
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
	      return build_call_array (type, fn, n, argarray);
	  }
        if (DECL_BUILT_IN_CLASS (fndecl) == BUILT_IN_MD)
          {
            tree arglist = NULL_TREE;
	    for (i = n - 1; i >= 0; i--)
	      arglist = tree_cons (NULL_TREE, argarray[i], arglist);
            ret = targetm.fold_builtin (fndecl, arglist, false);
            if (ret)
              return ret;
          }
        else if (n <= MAX_ARGS_TO_FOLD_BUILTIN)
          {
            /* First try the transformations that don't require consing up
               an exp.  */
            ret = fold_builtin_n (fndecl, argarray, n, false);
            if (ret)
              return ret;
          }

        /* If we got this far, we need to build an exp.  */
        exp = build_call_array (type, fn, n, argarray);
        ret = fold_builtin_varargs (fndecl, exp, false);
        return ret ? ret : exp;
      }
  }

  return build_call_array (type, fn, n, argarray);
}

/* Construct a new CALL_EXPR using the tail of the argument list of EXP
   along with N new arguments specified as the "..." parameters.  SKIP
   is the number of arguments in EXP to be omitted.  This function is used
   to do varargs-to-varargs transformations.  */

static tree
rewrite_call_expr (tree exp, int skip, tree fndecl, int n, ...)
{
  int oldnargs = call_expr_nargs (exp);
  int nargs = oldnargs - skip + n;
  tree fntype = TREE_TYPE (fndecl);
  tree fn = build1 (ADDR_EXPR, build_pointer_type (fntype), fndecl);
  tree *buffer;

  if (n > 0)
    {
      int i, j;
      va_list ap;

      buffer = alloca (nargs * sizeof (tree));
      va_start (ap, n);
      for (i = 0; i < n; i++)
	buffer[i] = va_arg (ap, tree);
      va_end (ap);
      for (j = skip; j < oldnargs; j++, i++)
	buffer[i] = CALL_EXPR_ARG (exp, j);
    }
  else 
    buffer = CALL_EXPR_ARGP (exp) + skip;

  return fold (build_call_array (TREE_TYPE (exp), fn, nargs, buffer));
}

/* Validate a single argument ARG against a tree code CODE representing
   a type.  */
  
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
   VOID_TYPE.  */

bool
validate_arglist (const_tree callexpr, ...)
{
  enum tree_code code;
  bool res = 0;
  va_list ap;
  const_call_expr_arg_iterator iter;
  const_tree arg;

  va_start (ap, callexpr);
  init_const_call_expr_arg_iterator (callexpr, &iter);

  do
    {
      code = va_arg (ap, enum tree_code);
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
			enum machine_mode mode ATTRIBUTE_UNUSED,
			int ignore ATTRIBUTE_UNUSED)
{
  return NULL_RTX;
}

/* Returns true is EXP represents data that would potentially reside
   in a readonly section.  */

static bool
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
      || (TREE_CODE (exp) == VAR_DECL && TREE_STATIC (exp)))
    return decl_readonly_section (exp, 0);
  else
    return false;
}

/* Simplify a call to the strstr builtin.  S1 and S2 are the arguments
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
fold_builtin_strstr (tree s1, tree s2, tree type)
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
	  const char *r = strstr (p1, p2);
	  tree tem;

	  if (r == NULL)
	    return build_int_cst (TREE_TYPE (s1), 0);

	  /* Return an offset into the constant string argument.  */
	  tem = fold_build2 (POINTER_PLUS_EXPR, TREE_TYPE (s1),
			     s1, size_int (r - p1));
	  return fold_convert (type, tem);
	}

      /* The argument is const char *, and the result is char *, so we need
	 a type conversion here to avoid a warning.  */
      if (p2[0] == '\0')
	return fold_convert (type, s1);

      if (p2[1] != '\0')
	return NULL_TREE;

      fn = implicit_built_in_decls[BUILT_IN_STRCHR];
      if (!fn)
	return NULL_TREE;

      /* New argument list transforming strstr(s1, s2) to
	 strchr(s1, s2[0]).  */
      return build_call_expr (fn, 2, s1, build_int_cst (NULL_TREE, p2[0]));
    }
}

/* Simplify a call to the strchr builtin.  S1 and S2 are the arguments to
   the call, and TYPE is its return type.

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
fold_builtin_strchr (tree s1, tree s2, tree type)
{
  if (!validate_arg (s1, POINTER_TYPE)
      || !validate_arg (s2, INTEGER_TYPE))
    return NULL_TREE;
  else
    {
      const char *p1;

      if (TREE_CODE (s2) != INTEGER_CST)
	return NULL_TREE;

      p1 = c_getstr (s1);
      if (p1 != NULL)
	{
	  char c;
	  const char *r;
	  tree tem;

	  if (target_char_cast (s2, &c))
	    return NULL_TREE;

	  r = strchr (p1, c);

	  if (r == NULL)
	    return build_int_cst (TREE_TYPE (s1), 0);

	  /* Return an offset into the constant string argument.  */
	  tem = fold_build2 (POINTER_PLUS_EXPR, TREE_TYPE (s1),
			     s1, size_int (r - p1));
	  return fold_convert (type, tem);
	}
      return NULL_TREE;
    }
}

/* Simplify a call to the strrchr builtin.  S1 and S2 are the arguments to
   the call, and TYPE is its return type.

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
fold_builtin_strrchr (tree s1, tree s2, tree type)
{
  if (!validate_arg (s1, POINTER_TYPE)
      || !validate_arg (s2, INTEGER_TYPE))
    return NULL_TREE;
  else
    {
      tree fn;
      const char *p1;

      if (TREE_CODE (s2) != INTEGER_CST)
	return NULL_TREE;

      p1 = c_getstr (s1);
      if (p1 != NULL)
	{
	  char c;
	  const char *r;
	  tree tem;

	  if (target_char_cast (s2, &c))
	    return NULL_TREE;

	  r = strrchr (p1, c);

	  if (r == NULL)
	    return build_int_cst (TREE_TYPE (s1), 0);

	  /* Return an offset into the constant string argument.  */
	  tem = fold_build2 (POINTER_PLUS_EXPR, TREE_TYPE (s1),
			     s1, size_int (r - p1));
	  return fold_convert (type, tem);
	}

      if (! integer_zerop (s2))
	return NULL_TREE;

      fn = implicit_built_in_decls[BUILT_IN_STRCHR];
      if (!fn)
	return NULL_TREE;

      /* Transform strrchr(s1, '\0') to strchr(s1, '\0').  */
      return build_call_expr (fn, 2, s1, s2);
    }
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
fold_builtin_strpbrk (tree s1, tree s2, tree type)
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
	  tem = fold_build2 (POINTER_PLUS_EXPR, TREE_TYPE (s1),
			     s1, size_int (r - p1));
	  return fold_convert (type, tem);
	}

      if (p2[0] == '\0')
	/* strpbrk(x, "") == NULL.
	   Evaluate and ignore s1 in case it had side-effects.  */
	return omit_one_operand (TREE_TYPE (s1), integer_zero_node, s1);

      if (p2[1] != '\0')
	return NULL_TREE;  /* Really call strpbrk.  */

      fn = implicit_built_in_decls[BUILT_IN_STRCHR];
      if (!fn)
	return NULL_TREE;

      /* New argument list transforming strpbrk(s1, s2) to
	 strchr(s1, s2[0]).  */
      return build_call_expr (fn, 2, s1, build_int_cst (NULL_TREE, p2[0]));
    }
}

/* Simplify a call to the strcat builtin.  DST and SRC are the arguments
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
fold_builtin_strcat (tree dst, tree src)
{
  if (!validate_arg (dst, POINTER_TYPE)
      || !validate_arg (src, POINTER_TYPE))
    return NULL_TREE;
  else
    {
      const char *p = c_getstr (src);

      /* If the string length is zero, return the dst parameter.  */
      if (p && *p == '\0')
	return dst;

      return NULL_TREE;
    }
}

/* Simplify a call to the strncat builtin.  DST, SRC, and LEN are the
   arguments to the call.

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
fold_builtin_strncat (tree dst, tree src, tree len)
{
  if (!validate_arg (dst, POINTER_TYPE)
      || !validate_arg (src, POINTER_TYPE)
      || !validate_arg (len, INTEGER_TYPE))
    return NULL_TREE;
  else
    {
      const char *p = c_getstr (src);

      /* If the requested length is zero, or the src parameter string
	 length is zero, return the dst parameter.  */
      if (integer_zerop (len) || (p && *p == '\0'))
	return omit_two_operands (TREE_TYPE (dst), dst, src, len);

      /* If the requested len is greater than or equal to the string
	 length, call strcat.  */
      if (TREE_CODE (len) == INTEGER_CST && p
	  && compare_tree_int (len, strlen (p)) >= 0)
	{
	  tree fn = implicit_built_in_decls[BUILT_IN_STRCAT];

	  /* If the replacement _DECL isn't initialized, don't do the
	     transformation.  */
	  if (!fn)
	    return NULL_TREE;

	  return build_call_expr (fn, 2, dst, src);
	}
      return NULL_TREE;
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
fold_builtin_strspn (tree s1, tree s2)
{
  if (!validate_arg (s1, POINTER_TYPE)
      || !validate_arg (s2, POINTER_TYPE))
    return NULL_TREE;
  else
    {
      const char *p1 = c_getstr (s1), *p2 = c_getstr (s2);

      /* If both arguments are constants, evaluate at compile-time.  */
      if (p1 && p2)
	{
	  const size_t r = strspn (p1, p2);
	  return size_int (r);
	}

      /* If either argument is "", return NULL_TREE.  */
      if ((p1 && *p1 == '\0') || (p2 && *p2 == '\0'))
	/* Evaluate and ignore both arguments in case either one has
	   side-effects.  */
	return omit_two_operands (integer_type_node, integer_zero_node,
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
fold_builtin_strcspn (tree s1, tree s2)
{
  if (!validate_arg (s1, POINTER_TYPE)
      || !validate_arg (s2, POINTER_TYPE))
    return NULL_TREE;
  else
    {
      const char *p1 = c_getstr (s1), *p2 = c_getstr (s2);

      /* If both arguments are constants, evaluate at compile-time.  */
      if (p1 && p2)
	{
	  const size_t r = strcspn (p1, p2);
	  return size_int (r);
	}

      /* If the first argument is "", return NULL_TREE.  */
      if (p1 && *p1 == '\0')
	{
	  /* Evaluate and ignore argument s2 in case it has
	     side-effects.  */
	  return omit_one_operand (integer_type_node,
				   integer_zero_node, s2);
	}

      /* If the second argument is "", return __builtin_strlen(s1).  */
      if (p2 && *p2 == '\0')
	{
	  tree fn = implicit_built_in_decls[BUILT_IN_STRLEN];

	  /* If the replacement _DECL isn't initialized, don't do the
	     transformation.  */
	  if (!fn)
	    return NULL_TREE;

	  return build_call_expr (fn, 1, s1);
	}
      return NULL_TREE;
    }
}

/* Fold a call to the fputs builtin.  ARG0 and ARG1 are the arguments
   to the call.  IGNORE is true if the value returned
   by the builtin will be ignored.  UNLOCKED is true is true if this
   actually a call to fputs_unlocked.  If LEN in non-NULL, it represents
   the known length of the string.  Return NULL_TREE if no simplification
   was possible.  */

tree
fold_builtin_fputs (tree arg0, tree arg1, bool ignore, bool unlocked, tree len)
{
  /* If we're using an unlocked function, assume the other unlocked
     functions exist explicitly.  */
  tree const fn_fputc = unlocked ? built_in_decls[BUILT_IN_FPUTC_UNLOCKED]
    : implicit_built_in_decls[BUILT_IN_FPUTC];
  tree const fn_fwrite = unlocked ? built_in_decls[BUILT_IN_FWRITE_UNLOCKED]
    : implicit_built_in_decls[BUILT_IN_FWRITE];

  /* If the return value is used, don't do the transformation.  */
  if (!ignore)
    return NULL_TREE;

  /* Verify the arguments in the original call.  */
  if (!validate_arg (arg0, POINTER_TYPE)
      || !validate_arg (arg1, POINTER_TYPE))
    return NULL_TREE;

  if (! len)
    len = c_strlen (arg0, 0);

  /* Get the length of the string passed to fputs.  If the length
     can't be determined, punt.  */
  if (!len
      || TREE_CODE (len) != INTEGER_CST)
    return NULL_TREE;

  switch (compare_tree_int (len, 1))
    {
    case -1: /* length is 0, delete the call entirely .  */
      return omit_one_operand (integer_type_node, integer_zero_node, arg1);;

    case 0: /* length is 1, call fputc.  */
      {
	const char *p = c_getstr (arg0);

	if (p != NULL)
	  {
 	    if (fn_fputc)
	      return build_call_expr (fn_fputc, 2,
				      build_int_cst (NULL_TREE, p[0]), arg1);
	    else
	      return NULL_TREE;
	  }
      }
      /* FALLTHROUGH */
    case 1: /* length is greater than 1, call fwrite.  */
      {
	/* If optimizing for size keep fputs.  */
	if (optimize_size)
	  return NULL_TREE;
	/* New argument list transforming fputs(string, stream) to
	   fwrite(string, 1, len, stream).  */
	if (fn_fwrite)
	  return build_call_expr (fn_fwrite, 4, arg0, size_one_node, len, arg1);
	else
	  return NULL_TREE;
      }
    default:
      gcc_unreachable ();
    }
  return NULL_TREE;
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

  if (TYPE_ARG_TYPES (fntype) == 0
      || (TREE_VALUE (tree_last (TYPE_ARG_TYPES (fntype)))
	  == void_type_node))
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
	  warning (0, "%<__builtin_next_arg%> called without an argument");
	  return true;
	}
      else if (nargs > 1)
	{
	  error ("wrong number of arguments to function %<__builtin_next_arg%>");
	  return true;
	}
      arg = CALL_EXPR_ARG (exp, 0);
    }

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
      while (TREE_CODE (arg) == NOP_EXPR
	     || TREE_CODE (arg) == CONVERT_EXPR
	     || TREE_CODE (arg) == NON_LVALUE_EXPR
	     || TREE_CODE (arg) == INDIRECT_REF)
	arg = TREE_OPERAND (arg, 0);
      if (arg != last_parm)
	{
	  /* FIXME: Sometimes with the tree optimizers we can get the
	     not the last argument even though the user used the last
	     argument.  We just warn and set the arg to be the last
	     argument so that we will get wrong-code because of
	     it.  */
	  warning (0, "second parameter of %<va_start%> not last named argument");
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


/* Simplify a call to the sprintf builtin with arguments DEST, FMT, and ORIG.
   ORIG may be null if this is a 2-argument call.  We don't attempt to
   simplify calls with more than 3 arguments.

   Return NULL_TREE if no simplification was possible, otherwise return the
   simplified form of the call as a tree.  If IGNORED is true, it means that
   the caller does not use the returned value of the function.  */

static tree
fold_builtin_sprintf (tree dest, tree fmt, tree orig, int ignored)
{
  tree call, retval;
  const char *fmt_str = NULL;

  /* Verify the required arguments in the original call.  We deal with two
     types of sprintf() calls: 'sprintf (str, fmt)' and
     'sprintf (dest, "%s", orig)'.  */
  if (!validate_arg (dest, POINTER_TYPE)
      || !validate_arg (fmt, POINTER_TYPE))
    return NULL_TREE;
  if (orig && !validate_arg (orig, POINTER_TYPE))
    return NULL_TREE;

  /* Check whether the format is a literal string constant.  */
  fmt_str = c_getstr (fmt);
  if (fmt_str == NULL)
    return NULL_TREE;

  call = NULL_TREE;
  retval = NULL_TREE;

  if (!init_target_chars ())
    return NULL_TREE;

  /* If the format doesn't contain % args or %%, use strcpy.  */
  if (strchr (fmt_str, target_percent) == NULL)
    {
      tree fn = implicit_built_in_decls[BUILT_IN_STRCPY];

      if (!fn)
	return NULL_TREE;

      /* Don't optimize sprintf (buf, "abc", ptr++).  */
      if (orig)
	return NULL_TREE;

      /* Convert sprintf (str, fmt) into strcpy (str, fmt) when
	 'format' is known to contain no % formats.  */
      call = build_call_expr (fn, 2, dest, fmt);
      if (!ignored)
	retval = build_int_cst (NULL_TREE, strlen (fmt_str));
    }

  /* If the format is "%s", use strcpy if the result isn't used.  */
  else if (fmt_str && strcmp (fmt_str, target_percent_s) == 0)
    {
      tree fn;
      fn = implicit_built_in_decls[BUILT_IN_STRCPY];

      if (!fn)
	return NULL_TREE;

      /* Don't crash on sprintf (str1, "%s").  */
      if (!orig)
	return NULL_TREE;

      /* Convert sprintf (str1, "%s", str2) into strcpy (str1, str2).  */
      if (!ignored)
	{
	  retval = c_strlen (orig, 1);
	  if (!retval || TREE_CODE (retval) != INTEGER_CST)
	    return NULL_TREE;
	}
      call = build_call_expr (fn, 2, dest, orig);
    }

  if (call && retval)
    {
      retval = fold_convert
	(TREE_TYPE (TREE_TYPE (implicit_built_in_decls[BUILT_IN_SPRINTF])),
	 retval);
      return build2 (COMPOUND_EXPR, TREE_TYPE (retval), call, retval);
    }
  else
    return call;
}

/* Expand a call EXP to __builtin_object_size.  */

rtx
expand_builtin_object_size (tree exp)
{
  tree ost;
  int object_size_type;
  tree fndecl = get_callee_fndecl (exp);

  if (!validate_arglist (exp, POINTER_TYPE, INTEGER_TYPE, VOID_TYPE))
    {
      error ("%Kfirst argument of %D must be a pointer, second integer constant",
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
      error ("%Klast argument of %D is not integer constant between 0 and 3",
	     exp, fndecl);
      expand_builtin_trap ();
      return const0_rtx;
    }

  object_size_type = tree_low_cst (ost, 0);

  return object_size_type < 2 ? constm1_rtx : const0_rtx;
}

/* Expand EXP, a call to the __mem{cpy,pcpy,move,set}_chk builtin.
   FCODE is the BUILT_IN_* to use.
   Return NULL_RTX if we failed; the caller should emit a normal call,
   otherwise try to get the result in TARGET, if convenient (and in
   mode MODE if that's convenient).  */

static rtx
expand_builtin_memory_chk (tree exp, rtx target, enum machine_mode mode,
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

  if (! host_integerp (size, 1))
    return NULL_RTX;

  if (host_integerp (len, 1) || integer_all_onesp (size))
    {
      tree fn;

      if (! integer_all_onesp (size) && tree_int_cst_lt (size, len))
	{
	  warning (0, "%Kcall to %D will always overflow destination buffer",
		   exp, get_callee_fndecl (exp));
	  return NULL_RTX;
	}

      fn = NULL_TREE;
      /* If __builtin_mem{cpy,pcpy,move,set}_chk is used, assume
	 mem{cpy,pcpy,move,set} is available.  */
      switch (fcode)
	{
	case BUILT_IN_MEMCPY_CHK:
	  fn = built_in_decls[BUILT_IN_MEMCPY];
	  break;
	case BUILT_IN_MEMPCPY_CHK:
	  fn = built_in_decls[BUILT_IN_MEMPCPY];
	  break;
	case BUILT_IN_MEMMOVE_CHK:
	  fn = built_in_decls[BUILT_IN_MEMMOVE];
	  break;
	case BUILT_IN_MEMSET_CHK:
	  fn = built_in_decls[BUILT_IN_MEMSET];
	  break;
	default:
	  break;
	}

      if (! fn)
	return NULL_RTX;

      fn = build_call_expr (fn, 3, dest, src, len);
      STRIP_TYPE_NOPS (fn);
      while (TREE_CODE (fn) == COMPOUND_EXPR)
	{
	  expand_expr (TREE_OPERAND (fn, 0), const0_rtx, VOIDmode,
		       EXPAND_NORMAL);
	  fn = TREE_OPERAND (fn, 1);
	}
      if (TREE_CODE (fn) == CALL_EXPR)
	CALL_EXPR_TAILCALL (fn) = CALL_EXPR_TAILCALL (exp);
      return expand_expr (fn, target, mode, EXPAND_NORMAL);
    }
  else if (fcode == BUILT_IN_MEMSET_CHK)
    return NULL_RTX;
  else
    {
      unsigned int dest_align
	= get_pointer_alignment (dest, BIGGEST_ALIGNMENT);

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

	  expr = fold_build2 (POINTER_PLUS_EXPR, TREE_TYPE (dest), dest, len);
	  return expand_expr (expr, target, mode, EXPAND_NORMAL);
	}

      /* __memmove_chk special case.  */
      if (fcode == BUILT_IN_MEMMOVE_CHK)
	{
	  unsigned int src_align
	    = get_pointer_alignment (src, BIGGEST_ALIGNMENT);

	  if (src_align == 0)
	    return NULL_RTX;

	  /* If src is categorized for a readonly section we can use
	     normal __memcpy_chk.  */
	  if (readonly_data_expr (src))
	    {
	      tree fn = built_in_decls[BUILT_IN_MEMCPY_CHK];
	      if (!fn)
		return NULL_RTX;
	      fn = build_call_expr (fn, 4, dest, src, len, size);
	      STRIP_TYPE_NOPS (fn);
	      while (TREE_CODE (fn) == COMPOUND_EXPR)
		{
		  expand_expr (TREE_OPERAND (fn, 0), const0_rtx, VOIDmode,
			       EXPAND_NORMAL);
		  fn = TREE_OPERAND (fn, 1);
		}
	      if (TREE_CODE (fn) == CALL_EXPR)
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
  int is_strlen = 0;
  tree len, size;

  switch (fcode)
    {
    case BUILT_IN_STRCPY_CHK:
    case BUILT_IN_STPCPY_CHK:
    /* For __strcat_chk the warning will be emitted only if overflowing
       by at least strlen (dest) + 1 bytes.  */
    case BUILT_IN_STRCAT_CHK:
      len = CALL_EXPR_ARG (exp, 1);
      size = CALL_EXPR_ARG (exp, 2);
      is_strlen = 1;
      break;
    case BUILT_IN_STRNCAT_CHK:
    case BUILT_IN_STRNCPY_CHK:
      len = CALL_EXPR_ARG (exp, 2);
      size = CALL_EXPR_ARG (exp, 3);
      break;
    case BUILT_IN_SNPRINTF_CHK:
    case BUILT_IN_VSNPRINTF_CHK:
      len = CALL_EXPR_ARG (exp, 1);
      size = CALL_EXPR_ARG (exp, 3);
      break;
    default:
      gcc_unreachable ();
    }

  if (!len || !size)
    return;

  if (! host_integerp (size, 1) || integer_all_onesp (size))
    return;

  if (is_strlen)
    {
      len = c_strlen (len, 1);
      if (! len || ! host_integerp (len, 1) || tree_int_cst_lt (len, size))
	return;
    }
  else if (fcode == BUILT_IN_STRNCAT_CHK)
    {
      tree src = CALL_EXPR_ARG (exp, 1);
      if (! src || ! host_integerp (len, 1) || tree_int_cst_lt (len, size))
	return;
      src = c_strlen (src, 1);
      if (! src || ! host_integerp (src, 1))
	{
	  warning (0, "%Kcall to %D might overflow destination buffer",
		   exp, get_callee_fndecl (exp));
	  return;
	}
      else if (tree_int_cst_lt (src, size))
	return;
    }
  else if (! host_integerp (len, 1) || ! tree_int_cst_lt (size, len))
    return;

  warning (0, "%Kcall to %D will always overflow destination buffer",
	   exp, get_callee_fndecl (exp));
}

/* Emit warning if a buffer overflow is detected at compile time
   in __sprintf_chk/__vsprintf_chk calls.  */

static void
maybe_emit_sprintf_chk_warning (tree exp, enum built_in_function fcode)
{
  tree dest, size, len, fmt, flag;
  const char *fmt_str;
  int nargs = call_expr_nargs (exp);

  /* Verify the required arguments in the original call.  */
  
  if (nargs < 4)
    return;
  dest = CALL_EXPR_ARG (exp, 0);
  flag = CALL_EXPR_ARG (exp, 1);
  size = CALL_EXPR_ARG (exp, 2);
  fmt = CALL_EXPR_ARG (exp, 3);

  if (! host_integerp (size, 1) || integer_all_onesp (size))
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
      if (!len || ! host_integerp (len, 1))
	return;
    }
  else
    return;

  if (! tree_int_cst_lt (len, size))
    {
      warning (0, "%Kcall to %D will always overflow destination buffer",
	       exp, get_callee_fndecl (exp));
    }
}

/* Fold a call to __builtin_object_size with arguments PTR and OST,
   if possible.  */

tree
fold_builtin_object_size (tree ptr, tree ost)
{
  tree ret = NULL_TREE;
  int object_size_type;

  if (!validate_arg (ptr, POINTER_TYPE)
      || !validate_arg (ost, INTEGER_TYPE))
    return NULL_TREE;

  STRIP_NOPS (ost);

  if (TREE_CODE (ost) != INTEGER_CST
      || tree_int_cst_sgn (ost) < 0
      || compare_tree_int (ost, 3) > 0)
    return NULL_TREE;

  object_size_type = tree_low_cst (ost, 0);

  /* __builtin_object_size doesn't evaluate side-effects in its arguments;
     if there are any side-effects, it returns (size_t) -1 for types 0 and 1
     and (size_t) 0 for types 2 and 3.  */
  if (TREE_SIDE_EFFECTS (ptr))
    return build_int_cst_type (size_type_node, object_size_type < 2 ? -1 : 0);

  if (TREE_CODE (ptr) == ADDR_EXPR)
    ret = build_int_cstu (size_type_node,
			  compute_builtin_object_size (ptr, object_size_type));

  else if (TREE_CODE (ptr) == SSA_NAME)
    {
      unsigned HOST_WIDE_INT bytes;

      /* If object size is not known yet, delay folding until
       later.  Maybe subsequent passes will help determining
       it.  */
      bytes = compute_builtin_object_size (ptr, object_size_type);
      if (bytes != (unsigned HOST_WIDE_INT) (object_size_type < 2
					     ? -1 : 0))
	ret = build_int_cstu (size_type_node, bytes);
    }

  if (ret)
    {
      unsigned HOST_WIDE_INT low = TREE_INT_CST_LOW (ret);
      HOST_WIDE_INT high = TREE_INT_CST_HIGH (ret);
      if (fit_double_type (low, high, &low, &high, TREE_TYPE (ret)))
	ret = NULL_TREE;
    }

  return ret;
}

/* Fold a call to the __mem{cpy,pcpy,move,set}_chk builtin.
   DEST, SRC, LEN, and SIZE are the arguments to the call.
   IGNORE is true, if return value can be ignored.  FCODE is the BUILT_IN_*
   code of the builtin.  If MAXLEN is not NULL, it is maximum length
   passed as third argument.  */

tree
fold_builtin_memory_chk (tree fndecl,
			 tree dest, tree src, tree len, tree size,
			 tree maxlen, bool ignore,
			 enum built_in_function fcode)
{
  tree fn;

  if (!validate_arg (dest, POINTER_TYPE)
      || !validate_arg (src,
			(fcode == BUILT_IN_MEMSET_CHK
			 ? INTEGER_TYPE : POINTER_TYPE))
      || !validate_arg (len, INTEGER_TYPE)
      || !validate_arg (size, INTEGER_TYPE))
    return NULL_TREE;

  /* If SRC and DEST are the same (and not volatile), return DEST
     (resp. DEST+LEN for __mempcpy_chk).  */
  if (fcode != BUILT_IN_MEMSET_CHK && operand_equal_p (src, dest, 0))
    {
      if (fcode != BUILT_IN_MEMPCPY_CHK)
	return omit_one_operand (TREE_TYPE (TREE_TYPE (fndecl)), dest, len);
      else
	{
	  tree temp = fold_build2 (POINTER_PLUS_EXPR, TREE_TYPE (dest), dest, len);
	  return fold_convert (TREE_TYPE (TREE_TYPE (fndecl)), temp);
	}
    }

  if (! host_integerp (size, 1))
    return NULL_TREE;

  if (! integer_all_onesp (size))
    {
      if (! host_integerp (len, 1))
	{
	  /* If LEN is not constant, try MAXLEN too.
	     For MAXLEN only allow optimizing into non-_ocs function
	     if SIZE is >= MAXLEN, never convert to __ocs_fail ().  */
	  if (maxlen == NULL_TREE || ! host_integerp (maxlen, 1))
	    {
	      if (fcode == BUILT_IN_MEMPCPY_CHK && ignore)
		{
		  /* (void) __mempcpy_chk () can be optimized into
		     (void) __memcpy_chk ().  */
		  fn = built_in_decls[BUILT_IN_MEMCPY_CHK];
		  if (!fn)
		    return NULL_TREE;

		  return build_call_expr (fn, 4, dest, src, len, size);
		}
	      return NULL_TREE;
	    }
	}
      else
	maxlen = len;

      if (tree_int_cst_lt (size, maxlen))
	return NULL_TREE;
    }

  fn = NULL_TREE;
  /* If __builtin_mem{cpy,pcpy,move,set}_chk is used, assume
     mem{cpy,pcpy,move,set} is available.  */
  switch (fcode)
    {
    case BUILT_IN_MEMCPY_CHK:
      fn = built_in_decls[BUILT_IN_MEMCPY];
      break;
    case BUILT_IN_MEMPCPY_CHK:
      fn = built_in_decls[BUILT_IN_MEMPCPY];
      break;
    case BUILT_IN_MEMMOVE_CHK:
      fn = built_in_decls[BUILT_IN_MEMMOVE];
      break;
    case BUILT_IN_MEMSET_CHK:
      fn = built_in_decls[BUILT_IN_MEMSET];
      break;
    default:
      break;
    }

  if (!fn)
    return NULL_TREE;

  return build_call_expr (fn, 3, dest, src, len);
}

/* Fold a call to the __st[rp]cpy_chk builtin.
   DEST, SRC, and SIZE are the arguments to the call.
   IGNORE is true if return value can be ignored.  FCODE is the BUILT_IN_*
   code of the builtin.  If MAXLEN is not NULL, it is maximum length of
   strings passed as second argument.  */

tree
fold_builtin_stxcpy_chk (tree fndecl, tree dest, tree src, tree size,
			 tree maxlen, bool ignore,
			 enum built_in_function fcode)
{
  tree len, fn;

  if (!validate_arg (dest, POINTER_TYPE)
      || !validate_arg (src, POINTER_TYPE)
      || !validate_arg (size, INTEGER_TYPE))
    return NULL_TREE;

  /* If SRC and DEST are the same (and not volatile), return DEST.  */
  if (fcode == BUILT_IN_STRCPY_CHK && operand_equal_p (src, dest, 0))
    return fold_convert (TREE_TYPE (TREE_TYPE (fndecl)), dest);

  if (! host_integerp (size, 1))
    return NULL_TREE;

  if (! integer_all_onesp (size))
    {
      len = c_strlen (src, 1);
      if (! len || ! host_integerp (len, 1))
	{
	  /* If LEN is not constant, try MAXLEN too.
	     For MAXLEN only allow optimizing into non-_ocs function
	     if SIZE is >= MAXLEN, never convert to __ocs_fail ().  */
	  if (maxlen == NULL_TREE || ! host_integerp (maxlen, 1))
	    {
	      if (fcode == BUILT_IN_STPCPY_CHK)
		{
		  if (! ignore)
		    return NULL_TREE;

		  /* If return value of __stpcpy_chk is ignored,
		     optimize into __strcpy_chk.  */
		  fn = built_in_decls[BUILT_IN_STRCPY_CHK];
		  if (!fn)
		    return NULL_TREE;

		  return build_call_expr (fn, 3, dest, src, size);
		}

	      if (! len || TREE_SIDE_EFFECTS (len))
		return NULL_TREE;

	      /* If c_strlen returned something, but not a constant,
		 transform __strcpy_chk into __memcpy_chk.  */
	      fn = built_in_decls[BUILT_IN_MEMCPY_CHK];
	      if (!fn)
		return NULL_TREE;

	      len = size_binop (PLUS_EXPR, len, ssize_int (1));
	      return fold_convert (TREE_TYPE (TREE_TYPE (fndecl)),
				   build_call_expr (fn, 4,
						    dest, src, len, size));
	    }
	}
      else
	maxlen = len;

      if (! tree_int_cst_lt (maxlen, size))
	return NULL_TREE;
    }

  /* If __builtin_st{r,p}cpy_chk is used, assume st{r,p}cpy is available.  */
  fn = built_in_decls[fcode == BUILT_IN_STPCPY_CHK
		      ? BUILT_IN_STPCPY : BUILT_IN_STRCPY];
  if (!fn)
    return NULL_TREE;

  return build_call_expr (fn, 2, dest, src);
}

/* Fold a call to the __strncpy_chk builtin.  DEST, SRC, LEN, and SIZE
   are the arguments to the call.  If MAXLEN is not NULL, it is maximum
   length passed as third argument.  */

tree
fold_builtin_strncpy_chk (tree dest, tree src, tree len, tree size,
			  tree maxlen)
{
  tree fn;

  if (!validate_arg (dest, POINTER_TYPE)
      || !validate_arg (src, POINTER_TYPE)
      || !validate_arg (len, INTEGER_TYPE)
      || !validate_arg (size, INTEGER_TYPE))
    return NULL_TREE;

  if (! host_integerp (size, 1))
    return NULL_TREE;

  if (! integer_all_onesp (size))
    {
      if (! host_integerp (len, 1))
	{
	  /* If LEN is not constant, try MAXLEN too.
	     For MAXLEN only allow optimizing into non-_ocs function
	     if SIZE is >= MAXLEN, never convert to __ocs_fail ().  */
	  if (maxlen == NULL_TREE || ! host_integerp (maxlen, 1))
	    return NULL_TREE;
	}
      else
	maxlen = len;

      if (tree_int_cst_lt (size, maxlen))
	return NULL_TREE;
    }

  /* If __builtin_strncpy_chk is used, assume strncpy is available.  */
  fn = built_in_decls[BUILT_IN_STRNCPY];
  if (!fn)
    return NULL_TREE;

  return build_call_expr (fn, 3, dest, src, len);
}

/* Fold a call to the __strcat_chk builtin FNDECL.  DEST, SRC, and SIZE
   are the arguments to the call.  */

static tree
fold_builtin_strcat_chk (tree fndecl, tree dest, tree src, tree size)
{
  tree fn;
  const char *p;

  if (!validate_arg (dest, POINTER_TYPE)
      || !validate_arg (src, POINTER_TYPE)
      || !validate_arg (size, INTEGER_TYPE))
    return NULL_TREE;

  p = c_getstr (src);
  /* If the SRC parameter is "", return DEST.  */
  if (p && *p == '\0')
    return omit_one_operand (TREE_TYPE (TREE_TYPE (fndecl)), dest, src);

  if (! host_integerp (size, 1) || ! integer_all_onesp (size))
    return NULL_TREE;

  /* If __builtin_strcat_chk is used, assume strcat is available.  */
  fn = built_in_decls[BUILT_IN_STRCAT];
  if (!fn)
    return NULL_TREE;

  return build_call_expr (fn, 2, dest, src);
}

/* Fold a call to the __strncat_chk builtin with arguments DEST, SRC,
   LEN, and SIZE.  */

static tree
fold_builtin_strncat_chk (tree fndecl,
			  tree dest, tree src, tree len, tree size)
{
  tree fn;
  const char *p;

  if (!validate_arg (dest, POINTER_TYPE)
      || !validate_arg (src, POINTER_TYPE)
      || !validate_arg (size, INTEGER_TYPE)
      || !validate_arg (size, INTEGER_TYPE))
    return NULL_TREE;

  p = c_getstr (src);
  /* If the SRC parameter is "" or if LEN is 0, return DEST.  */
  if (p && *p == '\0')
    return omit_one_operand (TREE_TYPE (TREE_TYPE (fndecl)), dest, len);
  else if (integer_zerop (len))
    return omit_one_operand (TREE_TYPE (TREE_TYPE (fndecl)), dest, src);

  if (! host_integerp (size, 1))
    return NULL_TREE;

  if (! integer_all_onesp (size))
    {
      tree src_len = c_strlen (src, 1);
      if (src_len
	  && host_integerp (src_len, 1)
	  && host_integerp (len, 1)
	  && ! tree_int_cst_lt (len, src_len))
	{
	  /* If LEN >= strlen (SRC), optimize into __strcat_chk.  */
	  fn = built_in_decls[BUILT_IN_STRCAT_CHK];
	  if (!fn)
	    return NULL_TREE;

	  return build_call_expr (fn, 3, dest, src, size);
	}
      return NULL_TREE;
    }

  /* If __builtin_strncat_chk is used, assume strncat is available.  */
  fn = built_in_decls[BUILT_IN_STRNCAT];
  if (!fn)
    return NULL_TREE;

  return build_call_expr (fn, 3, dest, src, len);
}

/* Fold a call EXP to __{,v}sprintf_chk.  Return NULL_TREE if
   a normal call should be emitted rather than expanding the function
   inline.  FCODE is either BUILT_IN_SPRINTF_CHK or BUILT_IN_VSPRINTF_CHK.  */

static tree
fold_builtin_sprintf_chk (tree exp, enum built_in_function fcode)
{
  tree dest, size, len, fn, fmt, flag;
  const char *fmt_str;
  int nargs = call_expr_nargs (exp);

  /* Verify the required arguments in the original call.  */
  if (nargs < 4)
    return NULL_TREE;
  dest = CALL_EXPR_ARG (exp, 0);
  if (!validate_arg (dest, POINTER_TYPE))
    return NULL_TREE;
  flag = CALL_EXPR_ARG (exp, 1);
  if (!validate_arg (flag, INTEGER_TYPE))
    return NULL_TREE;
  size = CALL_EXPR_ARG (exp, 2);
  if (!validate_arg (size, INTEGER_TYPE))
    return NULL_TREE;
  fmt = CALL_EXPR_ARG (exp, 3);
  if (!validate_arg (fmt, POINTER_TYPE))
    return NULL_TREE;

  if (! host_integerp (size, 1))
    return NULL_TREE;

  len = NULL_TREE;

  if (!init_target_chars ())
    return NULL_TREE;

  /* Check whether the format is a literal string constant.  */
  fmt_str = c_getstr (fmt);
  if (fmt_str != NULL)
    {
      /* If the format doesn't contain % args or %%, we know the size.  */
      if (strchr (fmt_str, target_percent) == 0)
	{
	  if (fcode != BUILT_IN_SPRINTF_CHK || nargs == 4)
	    len = build_int_cstu (size_type_node, strlen (fmt_str));
	}
      /* If the format is "%s" and first ... argument is a string literal,
	 we know the size too.  */
      else if (fcode == BUILT_IN_SPRINTF_CHK
	       && strcmp (fmt_str, target_percent_s) == 0)
	{
	  tree arg;

	  if (nargs == 5)
	    {
	      arg = CALL_EXPR_ARG (exp, 4);
	      if (validate_arg (arg, POINTER_TYPE))
		{
		  len = c_strlen (arg, 1);
		  if (! len || ! host_integerp (len, 1))
		    len = NULL_TREE;
		}
	    }
	}
    }

  if (! integer_all_onesp (size))
    {
      if (! len || ! tree_int_cst_lt (len, size))
	return NULL_TREE;
    }

  /* Only convert __{,v}sprintf_chk to {,v}sprintf if flag is 0
     or if format doesn't contain % chars or is "%s".  */
  if (! integer_zerop (flag))
    {
      if (fmt_str == NULL)
	return NULL_TREE;
      if (strchr (fmt_str, target_percent) != NULL
	  && strcmp (fmt_str, target_percent_s))
	return NULL_TREE;
    }

  /* If __builtin_{,v}sprintf_chk is used, assume {,v}sprintf is available.  */
  fn = built_in_decls[fcode == BUILT_IN_VSPRINTF_CHK
		      ? BUILT_IN_VSPRINTF : BUILT_IN_SPRINTF];
  if (!fn)
    return NULL_TREE;

  return rewrite_call_expr (exp, 4, fn, 2, dest, fmt);
}

/* Fold a call EXP to {,v}snprintf.  Return NULL_TREE if
   a normal call should be emitted rather than expanding the function
   inline.  FCODE is either BUILT_IN_SNPRINTF_CHK or
   BUILT_IN_VSNPRINTF_CHK.  If MAXLEN is not NULL, it is maximum length
   passed as second argument.  */

tree
fold_builtin_snprintf_chk (tree exp, tree maxlen,
			   enum built_in_function fcode)
{
  tree dest, size, len, fn, fmt, flag;
  const char *fmt_str;

  /* Verify the required arguments in the original call.  */
  if (call_expr_nargs (exp) < 5)
    return NULL_TREE;
  dest = CALL_EXPR_ARG (exp, 0);
  if (!validate_arg (dest, POINTER_TYPE))
    return NULL_TREE;
  len = CALL_EXPR_ARG (exp, 1);
  if (!validate_arg (len, INTEGER_TYPE))
    return NULL_TREE;
  flag = CALL_EXPR_ARG (exp, 2);
  if (!validate_arg (flag, INTEGER_TYPE))
    return NULL_TREE;
  size = CALL_EXPR_ARG (exp, 3);
  if (!validate_arg (size, INTEGER_TYPE))
    return NULL_TREE;
  fmt = CALL_EXPR_ARG (exp, 4);
  if (!validate_arg (fmt, POINTER_TYPE))
    return NULL_TREE;

  if (! host_integerp (size, 1))
    return NULL_TREE;

  if (! integer_all_onesp (size))
    {
      if (! host_integerp (len, 1))
	{
	  /* If LEN is not constant, try MAXLEN too.
	     For MAXLEN only allow optimizing into non-_ocs function
	     if SIZE is >= MAXLEN, never convert to __ocs_fail ().  */
	  if (maxlen == NULL_TREE || ! host_integerp (maxlen, 1))
	    return NULL_TREE;
	}
      else
	maxlen = len;

      if (tree_int_cst_lt (size, maxlen))
	return NULL_TREE;
    }

  if (!init_target_chars ())
    return NULL_TREE;

  /* Only convert __{,v}snprintf_chk to {,v}snprintf if flag is 0
     or if format doesn't contain % chars or is "%s".  */
  if (! integer_zerop (flag))
    {
      fmt_str = c_getstr (fmt);
      if (fmt_str == NULL)
	return NULL_TREE;
      if (strchr (fmt_str, target_percent) != NULL
	  && strcmp (fmt_str, target_percent_s))
	return NULL_TREE;
    }

  /* If __builtin_{,v}snprintf_chk is used, assume {,v}snprintf is
     available.  */
  fn = built_in_decls[fcode == BUILT_IN_VSNPRINTF_CHK
		      ? BUILT_IN_VSNPRINTF : BUILT_IN_SNPRINTF];
  if (!fn)
    return NULL_TREE;

  return rewrite_call_expr (exp, 5, fn, 3, dest, len, fmt);
}

/* Fold a call to the {,v}printf{,_unlocked} and __{,v}printf_chk builtins.
   FMT and ARG are the arguments to the call; we don't fold cases with
   more than 2 arguments, and ARG may be null if this is a 1-argument case.

   Return NULL_TREE if no simplification was possible, otherwise return the
   simplified form of the call as a tree.  FCODE is the BUILT_IN_*
   code of the function to be simplified.  */

static tree
fold_builtin_printf (tree fndecl, tree fmt, tree arg, bool ignore,
		     enum built_in_function fcode)
{
  tree fn_putchar, fn_puts, newarg, call = NULL_TREE;
  const char *fmt_str = NULL;

  /* If the return value is used, don't do the transformation.  */
  if (! ignore)
    return NULL_TREE;

  /* Verify the required arguments in the original call.  */
  if (!validate_arg (fmt, POINTER_TYPE))
    return NULL_TREE;

  /* Check whether the format is a literal string constant.  */
  fmt_str = c_getstr (fmt);
  if (fmt_str == NULL)
    return NULL_TREE;

  if (fcode == BUILT_IN_PRINTF_UNLOCKED)
    {
      /* If we're using an unlocked function, assume the other
	 unlocked functions exist explicitly.  */
      fn_putchar = built_in_decls[BUILT_IN_PUTCHAR_UNLOCKED];
      fn_puts = built_in_decls[BUILT_IN_PUTS_UNLOCKED];
    }
  else
    {
      fn_putchar = implicit_built_in_decls[BUILT_IN_PUTCHAR];
      fn_puts = implicit_built_in_decls[BUILT_IN_PUTS];
    }

  if (!init_target_chars ())
    return NULL_TREE;

  if (strcmp (fmt_str, target_percent_s) == 0
      || strchr (fmt_str, target_percent) == NULL)
    {
      const char *str;

      if (strcmp (fmt_str, target_percent_s) == 0)
	{
	  if (fcode == BUILT_IN_VPRINTF || fcode == BUILT_IN_VPRINTF_CHK)
	    return NULL_TREE;

	  if (!arg || !validate_arg (arg, POINTER_TYPE))
	    return NULL_TREE;

	  str = c_getstr (arg);
	  if (str == NULL)
	    return NULL_TREE;
	}
      else
	{
	  /* The format specifier doesn't contain any '%' characters.  */
	  if (fcode != BUILT_IN_VPRINTF && fcode != BUILT_IN_VPRINTF_CHK
	      && arg)
	    return NULL_TREE;
	  str = fmt_str;
	}

      /* If the string was "", printf does nothing.  */
      if (str[0] == '\0')
	return build_int_cst (TREE_TYPE (TREE_TYPE (fndecl)), 0);

      /* If the string has length of 1, call putchar.  */
      if (str[1] == '\0')
	{
	  /* Given printf("c"), (where c is any one character,)
	     convert "c"[0] to an int and pass that to the replacement
	     function.  */
	  newarg = build_int_cst (NULL_TREE, str[0]);
	  if (fn_putchar)
	    call = build_call_expr (fn_putchar, 1, newarg);
	}
      else
	{
	  /* If the string was "string\n", call puts("string").  */
	  size_t len = strlen (str);
	  if ((unsigned char)str[len - 1] == target_newline)
	    {
	      /* Create a NUL-terminated string that's one char shorter
		 than the original, stripping off the trailing '\n'.  */
	      char *newstr = alloca (len);
	      memcpy (newstr, str, len - 1);
	      newstr[len - 1] = 0;

	      newarg = build_string_literal (len, newstr);
	      if (fn_puts)
		call = build_call_expr (fn_puts, 1, newarg);
	    }
	  else
	    /* We'd like to arrange to call fputs(string,stdout) here,
	       but we need stdout and don't have a way to get it yet.  */
	    return NULL_TREE;
	}
    }

  /* The other optimizations can be done only on the non-va_list variants.  */
  else if (fcode == BUILT_IN_VPRINTF || fcode == BUILT_IN_VPRINTF_CHK)
    return NULL_TREE;

  /* If the format specifier was "%s\n", call __builtin_puts(arg).  */
  else if (strcmp (fmt_str, target_percent_s_newline) == 0)
    {
      if (!arg || !validate_arg (arg, POINTER_TYPE))
	return NULL_TREE;
      if (fn_puts)
	call = build_call_expr (fn_puts, 1, arg);
    }

  /* If the format specifier was "%c", call __builtin_putchar(arg).  */
  else if (strcmp (fmt_str, target_percent_c) == 0)
    {
      if (!arg || !validate_arg (arg, INTEGER_TYPE))
	return NULL_TREE;
      if (fn_putchar)
	call = build_call_expr (fn_putchar, 1, arg);
    }

  if (!call)
    return NULL_TREE;

  return fold_convert (TREE_TYPE (TREE_TYPE (fndecl)), call);
}

/* Fold a call to the {,v}fprintf{,_unlocked} and __{,v}printf_chk builtins.
   FP, FMT, and ARG are the arguments to the call.  We don't fold calls with
   more than 3 arguments, and ARG may be null in the 2-argument case.

   Return NULL_TREE if no simplification was possible, otherwise return the
   simplified form of the call as a tree.  FCODE is the BUILT_IN_*
   code of the function to be simplified.  */

static tree
fold_builtin_fprintf (tree fndecl, tree fp, tree fmt, tree arg, bool ignore,
		      enum built_in_function fcode)
{
  tree fn_fputc, fn_fputs, call = NULL_TREE;
  const char *fmt_str = NULL;

  /* If the return value is used, don't do the transformation.  */
  if (! ignore)
    return NULL_TREE;

  /* Verify the required arguments in the original call.  */
  if (!validate_arg (fp, POINTER_TYPE))
    return NULL_TREE;
  if (!validate_arg (fmt, POINTER_TYPE))
    return NULL_TREE;

  /* Check whether the format is a literal string constant.  */
  fmt_str = c_getstr (fmt);
  if (fmt_str == NULL)
    return NULL_TREE;

  if (fcode == BUILT_IN_FPRINTF_UNLOCKED)
    {
      /* If we're using an unlocked function, assume the other
	 unlocked functions exist explicitly.  */
      fn_fputc = built_in_decls[BUILT_IN_FPUTC_UNLOCKED];
      fn_fputs = built_in_decls[BUILT_IN_FPUTS_UNLOCKED];
    }
  else
    {
      fn_fputc = implicit_built_in_decls[BUILT_IN_FPUTC];
      fn_fputs = implicit_built_in_decls[BUILT_IN_FPUTS];
    }

  if (!init_target_chars ())
    return NULL_TREE;

  /* If the format doesn't contain % args or %%, use strcpy.  */
  if (strchr (fmt_str, target_percent) == NULL)
    {
      if (fcode != BUILT_IN_VFPRINTF && fcode != BUILT_IN_VFPRINTF_CHK
	  && arg)
	return NULL_TREE;

      /* If the format specifier was "", fprintf does nothing.  */
      if (fmt_str[0] == '\0')
	{
	  /* If FP has side-effects, just wait until gimplification is
	     done.  */
	  if (TREE_SIDE_EFFECTS (fp))
	    return NULL_TREE;

	  return build_int_cst (TREE_TYPE (TREE_TYPE (fndecl)), 0);
	}

      /* When "string" doesn't contain %, replace all cases of
	 fprintf (fp, string) with fputs (string, fp).  The fputs
	 builtin will take care of special cases like length == 1.  */
      if (fn_fputs)
	call = build_call_expr (fn_fputs, 2, fmt, fp);
    }

  /* The other optimizations can be done only on the non-va_list variants.  */
  else if (fcode == BUILT_IN_VFPRINTF || fcode == BUILT_IN_VFPRINTF_CHK)
    return NULL_TREE;

  /* If the format specifier was "%s", call __builtin_fputs (arg, fp).  */
  else if (strcmp (fmt_str, target_percent_s) == 0)
    {
      if (!arg || !validate_arg (arg, POINTER_TYPE))
	return NULL_TREE;
      if (fn_fputs)
	call = build_call_expr (fn_fputs, 2, arg, fp);
    }

  /* If the format specifier was "%c", call __builtin_fputc (arg, fp).  */
  else if (strcmp (fmt_str, target_percent_c) == 0)
    {
      if (!arg || !validate_arg (arg, INTEGER_TYPE))
	return NULL_TREE;
      if (fn_fputc)
	call = build_call_expr (fn_fputc, 2, arg, fp);
    }

  if (!call)
    return NULL_TREE;
  return fold_convert (TREE_TYPE (TREE_TYPE (fndecl)), call);
}

/* Initialize format string characters in the target charset.  */

static bool
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

/* If argument ARG is a REAL_CST, call the one-argument mpfr function
   FUNC on it and return the resulting value as a tree with type TYPE.
   If MIN and/or MAX are not NULL, then the supplied ARG must be
   within those bounds.  If INCLUSIVE is true, then MIN/MAX are
   acceptable values, otherwise they are not.  The mpfr precision is
   set to the precision of TYPE.  We assume that function FUNC returns
   zero if the result could be calculated exactly within the requested
   precision.  */

static tree
do_mpfr_arg1 (tree arg, tree type, int (*func)(mpfr_ptr, mpfr_srcptr, mp_rnd_t),
	      const REAL_VALUE_TYPE *min, const REAL_VALUE_TYPE *max,
	      bool inclusive)
{
  tree result = NULL_TREE;
  
  STRIP_NOPS (arg);

  /* To proceed, MPFR must exactly represent the target floating point
     format, which only happens when the target base equals two.  */
  if (REAL_MODE_FORMAT (TYPE_MODE (type))->b == 2
      && TREE_CODE (arg) == REAL_CST && !TREE_OVERFLOW (arg))
    {
      const REAL_VALUE_TYPE *const ra = &TREE_REAL_CST (arg);

      if (real_isfinite (ra)
	  && (!min || real_compare (inclusive ? GE_EXPR: GT_EXPR , ra, min))
	  && (!max || real_compare (inclusive ? LE_EXPR: LT_EXPR , ra, max)))
        {
	  const struct real_format *fmt = REAL_MODE_FORMAT (TYPE_MODE (type));
	  const int prec = fmt->p;
	  const mp_rnd_t rnd = fmt->round_towards_zero? GMP_RNDZ : GMP_RNDN;
	  int inexact;
	  mpfr_t m;

	  mpfr_init2 (m, prec);
	  mpfr_from_real (m, ra, GMP_RNDN);
	  mpfr_clear_flags ();
	  inexact = func (m, m, rnd);
	  result = do_mpfr_ckconv (m, type, inexact);
	  mpfr_clear (m);
	}
    }
  
  return result;
}

/* If argument ARG is a REAL_CST, call the two-argument mpfr function
   FUNC on it and return the resulting value as a tree with type TYPE.
   The mpfr precision is set to the precision of TYPE.  We assume that
   function FUNC returns zero if the result could be calculated
   exactly within the requested precision.  */

static tree
do_mpfr_arg2 (tree arg1, tree arg2, tree type,
	      int (*func)(mpfr_ptr, mpfr_srcptr, mpfr_srcptr, mp_rnd_t))
{
  tree result = NULL_TREE;
  
  STRIP_NOPS (arg1);
  STRIP_NOPS (arg2);

  /* To proceed, MPFR must exactly represent the target floating point
     format, which only happens when the target base equals two.  */
  if (REAL_MODE_FORMAT (TYPE_MODE (type))->b == 2
      && TREE_CODE (arg1) == REAL_CST && !TREE_OVERFLOW (arg1)
      && TREE_CODE (arg2) == REAL_CST && !TREE_OVERFLOW (arg2))
    {
      const REAL_VALUE_TYPE *const ra1 = &TREE_REAL_CST (arg1);
      const REAL_VALUE_TYPE *const ra2 = &TREE_REAL_CST (arg2);

      if (real_isfinite (ra1) && real_isfinite (ra2))
        {
	  const struct real_format *fmt = REAL_MODE_FORMAT (TYPE_MODE (type));
	  const int prec = fmt->p;
	  const mp_rnd_t rnd = fmt->round_towards_zero? GMP_RNDZ : GMP_RNDN;
	  int inexact;
	  mpfr_t m1, m2;

	  mpfr_inits2 (prec, m1, m2, NULL);
	  mpfr_from_real (m1, ra1, GMP_RNDN);
	  mpfr_from_real (m2, ra2, GMP_RNDN);
	  mpfr_clear_flags ();
	  inexact = func (m1, m1, m2, rnd);
	  result = do_mpfr_ckconv (m1, type, inexact);
	  mpfr_clears (m1, m2, NULL);
	}
    }
  
  return result;
}

/* If argument ARG is a REAL_CST, call the three-argument mpfr function
   FUNC on it and return the resulting value as a tree with type TYPE.
   The mpfr precision is set to the precision of TYPE.  We assume that
   function FUNC returns zero if the result could be calculated
   exactly within the requested precision.  */

static tree
do_mpfr_arg3 (tree arg1, tree arg2, tree arg3, tree type,
	      int (*func)(mpfr_ptr, mpfr_srcptr, mpfr_srcptr, mpfr_srcptr, mp_rnd_t))
{
  tree result = NULL_TREE;
  
  STRIP_NOPS (arg1);
  STRIP_NOPS (arg2);
  STRIP_NOPS (arg3);

  /* To proceed, MPFR must exactly represent the target floating point
     format, which only happens when the target base equals two.  */
  if (REAL_MODE_FORMAT (TYPE_MODE (type))->b == 2
      && TREE_CODE (arg1) == REAL_CST && !TREE_OVERFLOW (arg1)
      && TREE_CODE (arg2) == REAL_CST && !TREE_OVERFLOW (arg2)
      && TREE_CODE (arg3) == REAL_CST && !TREE_OVERFLOW (arg3))
    {
      const REAL_VALUE_TYPE *const ra1 = &TREE_REAL_CST (arg1);
      const REAL_VALUE_TYPE *const ra2 = &TREE_REAL_CST (arg2);
      const REAL_VALUE_TYPE *const ra3 = &TREE_REAL_CST (arg3);

      if (real_isfinite (ra1) && real_isfinite (ra2) && real_isfinite (ra3))
        {
	  const struct real_format *fmt = REAL_MODE_FORMAT (TYPE_MODE (type));
	  const int prec = fmt->p;
	  const mp_rnd_t rnd = fmt->round_towards_zero? GMP_RNDZ : GMP_RNDN;
	  int inexact;
	  mpfr_t m1, m2, m3;

	  mpfr_inits2 (prec, m1, m2, m3, NULL);
	  mpfr_from_real (m1, ra1, GMP_RNDN);
	  mpfr_from_real (m2, ra2, GMP_RNDN);
	  mpfr_from_real (m3, ra3, GMP_RNDN);
	  mpfr_clear_flags ();
	  inexact = func (m1, m1, m2, m3, rnd);
	  result = do_mpfr_ckconv (m1, type, inexact);
	  mpfr_clears (m1, m2, m3, NULL);
	}
    }
  
  return result;
}

/* If argument ARG is a REAL_CST, call mpfr_sin_cos() on it and set
   the pointers *(ARG_SINP) and *(ARG_COSP) to the resulting values.
   If ARG_SINP and ARG_COSP are NULL then the result is returned
   as a complex value.
   The type is taken from the type of ARG and is used for setting the
   precision of the calculation and results.  */

static tree
do_mpfr_sincos (tree arg, tree arg_sinp, tree arg_cosp)
{
  tree const type = TREE_TYPE (arg);
  tree result = NULL_TREE;
  
  STRIP_NOPS (arg);
  
  /* To proceed, MPFR must exactly represent the target floating point
     format, which only happens when the target base equals two.  */
  if (REAL_MODE_FORMAT (TYPE_MODE (type))->b == 2
      && TREE_CODE (arg) == REAL_CST
      && !TREE_OVERFLOW (arg))
    {
      const REAL_VALUE_TYPE *const ra = &TREE_REAL_CST (arg);

      if (real_isfinite (ra))
        {
	  const struct real_format *fmt = REAL_MODE_FORMAT (TYPE_MODE (type));
	  const int prec = fmt->p;
	  const mp_rnd_t rnd = fmt->round_towards_zero? GMP_RNDZ : GMP_RNDN;
	  tree result_s, result_c;
	  int inexact;
	  mpfr_t m, ms, mc;

	  mpfr_inits2 (prec, m, ms, mc, NULL);
	  mpfr_from_real (m, ra, GMP_RNDN);
	  mpfr_clear_flags ();
	  inexact = mpfr_sin_cos (ms, mc, m, rnd);
	  result_s = do_mpfr_ckconv (ms, type, inexact);
	  result_c = do_mpfr_ckconv (mc, type, inexact);
	  mpfr_clears (m, ms, mc, NULL);
	  if (result_s && result_c)
	    {
	      /* If we are to return in a complex value do so.  */
	      if (!arg_sinp && !arg_cosp)
		return build_complex (build_complex_type (type),
				      result_c, result_s);

	      /* Dereference the sin/cos pointer arguments.  */
	      arg_sinp = build_fold_indirect_ref (arg_sinp);
	      arg_cosp = build_fold_indirect_ref (arg_cosp);
	      /* Proceed if valid pointer type were passed in.  */
	      if (TYPE_MAIN_VARIANT (TREE_TYPE (arg_sinp)) == TYPE_MAIN_VARIANT (type)
		  && TYPE_MAIN_VARIANT (TREE_TYPE (arg_cosp)) == TYPE_MAIN_VARIANT (type))
	        {
		  /* Set the values. */
		  result_s = fold_build2 (MODIFY_EXPR, type, arg_sinp,
		      			  result_s);
		  TREE_SIDE_EFFECTS (result_s) = 1;
		  result_c = fold_build2 (MODIFY_EXPR, type, arg_cosp,
		      			  result_c);
		  TREE_SIDE_EFFECTS (result_c) = 1;
		  /* Combine the assignments into a compound expr.  */
		  result = non_lvalue (fold_build2 (COMPOUND_EXPR, type,
						    result_s, result_c));
		}
	    }
	}
    }
  return result;
}

#if MPFR_VERSION >= MPFR_VERSION_NUM(2,3,0)
/* If argument ARG1 is an INTEGER_CST and ARG2 is a REAL_CST, call the
   two-argument mpfr order N Bessel function FUNC on them and return
   the resulting value as a tree with type TYPE.  The mpfr precision
   is set to the precision of TYPE.  We assume that function FUNC
   returns zero if the result could be calculated exactly within the
   requested precision.  */
static tree
do_mpfr_bessel_n (tree arg1, tree arg2, tree type,
		  int (*func)(mpfr_ptr, long, mpfr_srcptr, mp_rnd_t),
		  const REAL_VALUE_TYPE *min, bool inclusive)
{
  tree result = NULL_TREE;

  STRIP_NOPS (arg1);
  STRIP_NOPS (arg2);

  /* To proceed, MPFR must exactly represent the target floating point
     format, which only happens when the target base equals two.  */
  if (REAL_MODE_FORMAT (TYPE_MODE (type))->b == 2
      && host_integerp (arg1, 0)
      && TREE_CODE (arg2) == REAL_CST && !TREE_OVERFLOW (arg2))
    {
      const HOST_WIDE_INT n = tree_low_cst(arg1, 0);
      const REAL_VALUE_TYPE *const ra = &TREE_REAL_CST (arg2);

      if (n == (long)n
	  && real_isfinite (ra)
	  && (!min || real_compare (inclusive ? GE_EXPR: GT_EXPR , ra, min)))
        {
	  const struct real_format *fmt = REAL_MODE_FORMAT (TYPE_MODE (type));
	  const int prec = fmt->p;
	  const mp_rnd_t rnd = fmt->round_towards_zero? GMP_RNDZ : GMP_RNDN;
	  int inexact;
	  mpfr_t m;

	  mpfr_init2 (m, prec);
	  mpfr_from_real (m, ra, GMP_RNDN);
	  mpfr_clear_flags ();
	  inexact = func (m, n, m, rnd);
	  result = do_mpfr_ckconv (m, type, inexact);
	  mpfr_clear (m);
	}
    }
  
  return result;
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
		  tree result_quo = fold_build2 (MODIFY_EXPR,
						 TREE_TYPE (arg_quo), arg_quo,
						 build_int_cst (NULL, integer_quo));
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
	  && !(real_isneg(ra) && real_isinteger(ra, TYPE_MODE (type))))
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
				       build_int_cst (NULL, sg));
	      TREE_SIDE_EFFECTS (result_sg) = 1;
	      /* Combine the signgam assignment with the lgamma result.  */
	      result = non_lvalue (fold_build2 (COMPOUND_EXPR, type,
						result_sg, result_lg));
	    }
	}
    }

  return result;
}
#endif
