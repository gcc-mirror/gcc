/* Lowering routines for all things related to BITS values.
   Copyright (C) 2025 Jose E. Marchesi.

   Written by Jose E. Marchesi.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3, or (at your option)
   any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#define INCLUDE_MEMORY
#include "config.h"
#include "system.h"
#include "coretypes.h"

#include "tree.h"
#include "fold-const.h"
#include "diagnostic.h"
#include "langhooks.h"
#include "tm.h"
#include "function.h"
#include "cgraph.h"
#include "toplev.h"
#include "varasm.h"
#include "predict.h"
#include "stor-layout.h"
#include "tree-iterator.h"
#include "stringpool.h"
#include "print-tree.h"
#include "gimplify.h"
#include "dumpfile.h"
#include "convert.h"

#include "a68.h"

/* Return a tree with the yielind of SKIP for the given BITS mode.  */

tree
a68_get_bits_skip_tree (MOID_T *m)
{
  tree type;

  if (m == M_BITS)
    type = a68_bits_type;
  else if (m == M_LONG_BITS)
    type = a68_long_bits_type;
  else if (m == M_LONG_LONG_BITS)
    type = a68_long_long_bits_type;
  else if (m == M_SHORT_BITS)
    type = a68_short_bits_type;
  else if (m == M_SHORT_SHORT_BITS)
    type = a68_short_short_bits_type;
  else
    gcc_unreachable ();

  return build_int_cst (type, 0);
}

/* Given a BITS type, compute the number of bits that fit in a value of that
   type.  The result is an INT.  */

tree
a68_bits_width (tree type)
{
  return fold_convert (a68_int_type, TYPE_SIZE (type));
}

/* Given a BITS type, compute the maximum value that can be expressed with that
   type.  */

tree
a68_bits_maxbits (tree type)
{
  return fold_convert (type, TYPE_MAX_VALUE (type));
}

/* Given a SIZETY INT value VAL, compute and return a SIZETY BITS reflecting
   its constituent bits.

   In strict Algol 68 the BIN of a negative value is BITS (SKIP).

   In GNU 68 the BIN of a negative value is the constituent bits of the two's
   complement of the value.  */

tree
a68_bits_bin (MOID_T *m, tree val)
{
  tree type = CTYPE (m);

  if (OPTION_STRICT (&A68_JOB))
    return a68_get_bits_skip_tree (m);
  else
    return fold_convert (type, val);
}

/* Given a SIZETY BITS value BITS, compute and return the corresponding SIZETY
   INT.

   In strict Algol 68 the ABS of a BITS value reflecting a bit pattern that
   would correspond a negative integral value is INT (SKIP).

   In GNU 68 the ABS of a BITS value reflecting a bit pattern that would
   correspond a negative integral value is that negative integral value.  */

tree
a68_bits_abs (MOID_T *m, tree bits)
{
  tree type = CTYPE (m);

  if (OPTION_STRICT (&A68_JOB))
    {
      tree integral_val = save_expr (fold_convert (type, bits));
      return fold_build3 (COND_EXPR,
			  type,
			  fold_build2 (LT_EXPR, type, integral_val,
				       build_int_cst (type, 0)),
			  a68_get_int_skip_tree (m),
			  integral_val);
    }
  else
    return fold_convert (type, bits);
}

/* Given a SIZETY BITS value BITS, shorten it into a SIZETY BITS whose tree
   type is TYPE.  */

tree
a68_bits_shorten (tree type, tree bits)
{
  /* This will truncate at the left, which is what is intended.  */
  return fold_convert (type, bits);
}

/* Given a SIZETY BITS value BITS, length it into a SIZETY BITS whose tree type
   is TYPE.  */

tree
a68_bits_leng (tree type, tree bits)
{
  /* This will add zeroes to the left, which is what is intended.  */
  return fold_convert (type, bits);
}

/* Given a SIZETY BITS value BITS, compute and return a new SIZETY BITS whose
   bits are the logical negation of the bits of BITS.  */

tree
a68_bits_not (tree bits)
{
  return fold_build1 (BIT_NOT_EXPR, TREE_TYPE (bits), bits);
}

/* Given two SIZETY BITS values BITS1 and BITS2, compute and return a new
   SIZETY BITS whose bits are the `and' of the bits of BITS1 and
   BITS2.  */

tree
a68_bits_and (tree bits1, tree bits2)
{
  return fold_build2 (BIT_AND_EXPR, TREE_TYPE (bits1), bits1, bits2);
}

/* Given two SIZETY BITS values BITS1 and BITS2, compute and return a new
   SIZETY BITS whose bits are the inclusive-or of the bits of BITS1 and
   BITS2.  */

tree
a68_bits_ior (tree bits1, tree bits2)
{
  return fold_build2 (BIT_IOR_EXPR, TREE_TYPE (bits1), bits1, bits2);
}

/* Given two SIZETY BITS values BITS1 and BITS2, compute and return a new
   SIZETY BITS whose bits are the exclusive-or of the bits of BITS1 and
   BITS2.  */

tree
a68_bits_xor (tree bits1, tree bits2)
{
  return fold_build2 (BIT_XOR_EXPR, TREE_TYPE (bits1), bits1, bits2);
}

/* Given a position POS of mode INT and a BITS of mode SIZETY BITS, return a
   BOOL reflecting the state of the bit occupying the position POS in BITS.

   If POS is out of range a run-time error is emitted.  */

tree
a68_bits_elem (NODE_T *p, tree pos, tree bits)
{
  pos = save_expr (pos);
  tree one = build_int_cst (TREE_TYPE (bits), 1);

  tree shift = fold_build2 (MINUS_EXPR, bitsizetype,
			    TYPE_SIZE (TREE_TYPE (bits)),
			    fold_convert (bitsizetype, pos));
  tree elem = fold_build2 (EQ_EXPR,
			   a68_bool_type,
			   fold_build2 (BIT_AND_EXPR,
					TREE_TYPE (bits),
					fold_build2 (RSHIFT_EXPR,
						     TREE_TYPE (bits),
						     bits, shift),
					one),
			   one);

  /* Do bounds checking if requested.  */
  if (OPTION_BOUNDS_CHECKING (&A68_JOB))
    {
      unsigned int lineno = NUMBER (LINE (INFO (p)));
      const char *filename_str = FILENAME (LINE (INFO (p)));
      tree filename = build_string_literal (strlen (filename_str) + 1,
					    filename_str);
      tree call = a68_build_libcall (A68_LIBCALL_BITSBOUNDSERROR,
				     void_type_node, 3,
				     filename,
				     build_int_cst (unsigned_type_node, lineno),
				     fold_convert (ssizetype, pos));
      tree check = fold_build2 (TRUTH_AND_EXPR, integer_type_node,
				fold_build2 (GT_EXPR, integer_type_node,
					     pos, fold_convert (TREE_TYPE (pos), integer_zero_node)),
				fold_build2 (LE_EXPR, integer_type_node,
					     fold_convert (bitsizetype, pos),
					     TYPE_SIZE (TREE_TYPE (bits))));

      check = fold_build2_loc (a68_get_node_location (p),
			       TRUTH_ORIF_EXPR,
			       ssizetype,
			       check,
			       fold_build2 (COMPOUND_EXPR, a68_bool_type,
					    call, boolean_false_node));
      elem = fold_build2 (COMPOUND_EXPR, a68_bool_type,
			  check, elem);
    }

  return elem;
}

/* Given two SIZETY BITS values BITS1 and BITS2, return a BOOL value indicating
   whether all the bits set in BITS1 are also set in BITS2.  */

tree
a68_bits_subset (tree bits1, tree bits2)
{
  /* We compute this operation with `A | B == B' as specified by the Report */
  bits2 = save_expr (bits2);
  return fold_build2 (EQ_EXPR, a68_bool_type,
		      fold_build2 (BIT_IOR_EXPR, TREE_TYPE (bits1), bits1, bits2),
		      bits2);
}

/* Rotate the bits in BITS SHIFT bits to the left if SHIFT is positive, or ABS
   (SHIFT) bits to the right if SHIFT is negative.

   A run-time error is raised if the count overflows the BITS value.  */

tree
a68_bits_shift (tree shift, tree bits)
{
  shift = save_expr (shift);
  bits = save_expr (bits);
  return fold_build3 (COND_EXPR,
		      TREE_TYPE (bits),
		      fold_build2 (GE_EXPR, TREE_TYPE (shift),
				   shift, build_int_cst (TREE_TYPE (shift), 0)),
		      fold_build2 (LSHIFT_EXPR, TREE_TYPE (bits),
				   bits, shift),
		      fold_build2 (RSHIFT_EXPR, TREE_TYPE (bits),
				   bits,
				   fold_build1 (ABS_EXPR, TREE_TYPE (shift), shift)));
}

/* Given two bits values, build an expression that calculates whether A = B.  */

tree
a68_bits_eq (tree a, tree b, location_t loc)
{
  return fold_build2_loc (loc, EQ_EXPR, boolean_type_node, a, b);
}

/* Given two bits values, build an expression that calculates whether A /=
   B.  */

tree
a68_bits_ne (tree a, tree b, location_t loc)
{
  return fold_build2_loc (loc, NE_EXPR, boolean_type_node, a, b);
}
