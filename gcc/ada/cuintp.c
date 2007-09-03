/****************************************************************************
 *                                                                          *
 *                        GNAT COMPILER COMPONENTS                          *
 *                                                                          *
 *                               C U I N T P                                *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *          Copyright (C) 1992-2007, Free Software Foundation, Inc.         *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 3,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License *
 * for  more details.  You should have received a copy of the GNU General   *
 * Public License along with GCC; see the file COPYING3.  If not see        *
 * <http://www.gnu.org/licenses/>.                                          *
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/

/* This file corresponds to the Ada package body Uintp. It was created
   manually from the files uintp.ads and uintp.adb. */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "tree.h"
#include "ada.h"
#include "types.h"
#include "uintp.h"
#include "atree.h"
#include "elists.h"
#include "nlists.h"
#include "stringt.h"
#include "fe.h"
#include "gigi.h"
#include "ada-tree.h"

/* Universal integers are represented by the Uint type which is an index into
   the Uints_Ptr table containing Uint_Entry values.  A Uint_Entry contains an
   index and length for getting the "digits" of the universal integer from the
   Udigits_Ptr table.

   For efficiency, this method is used only for integer values larger than the
   constant Uint_Bias.  If a Uint is less than this constant, then it contains
   the integer value itself.  The origin of the Uints_Ptr table is adjusted so
   that a Uint value of Uint_Bias indexes the first element.

   First define a utility function that operates like build_int_cst for
   integral types and does a conversion to floating-point for real types.  */

static tree
build_cst_from_int (tree type, HOST_WIDE_INT low)
{
  if (TREE_CODE (type) == REAL_TYPE)
    return convert (type, build_int_cst (NULL_TREE, low));
  else
    return build_int_cst_type (type, low);
}

/* Similar to UI_To_Int, but return a GCC INTEGER_CST or REAL_CST node,
   depending on whether TYPE is an integral or real type.  Overflow is tested
   by the constant-folding used to build the node.  TYPE is the GCC type of
   the resulting node.  */

tree
UI_To_gnu (Uint Input, tree type)
{
  tree gnu_ret;

  /* We might have a TYPE with biased representation and be passed an
     unbiased value that doesn't fit.  We always use an unbiased type able
     to hold any such possible value for intermediate computations, and
     then rely on a conversion back to TYPE to perform the bias adjustment
     when need be.  */

  int biased_type_p
    = (TREE_CODE (type) == INTEGER_TYPE
       && TYPE_BIASED_REPRESENTATION_P (type));

  tree comp_type = biased_type_p ? get_base_type (type) : type;

  if (Input <= Uint_Direct_Last)
    gnu_ret = build_cst_from_int (comp_type, Input - Uint_Direct_Bias);
  else
    {
      Int Idx = Uints_Ptr[Input].Loc;
      Pos Length = Uints_Ptr[Input].Length;
      Int First = Udigits_Ptr[Idx];
      tree gnu_base;

      gcc_assert (Length > 0);

      /* The computations we perform below always require a type at least as
	 large as an integer not to overflow.  REAL types are always fine, but
	 INTEGER or ENUMERAL types we are handed may be too short.  We use a
	 base integer type node for the computations in this case and will
	 convert the final result back to the incoming type later on.  */

      if (TREE_CODE (comp_type) != REAL_TYPE
	  && TYPE_PRECISION (comp_type) < TYPE_PRECISION (integer_type_node))
	comp_type = integer_type_node;

      gnu_base = build_cst_from_int (comp_type, Base);

      gnu_ret = build_cst_from_int (comp_type, First);
      if (First < 0)
	for (Idx++, Length--; Length; Idx++, Length--)
	  gnu_ret = fold_build2 (MINUS_EXPR, comp_type,
				 fold_build2 (MULT_EXPR, comp_type,
					      gnu_ret, gnu_base),
				 build_cst_from_int (comp_type,
						     Udigits_Ptr[Idx]));
      else
	for (Idx++, Length--; Length; Idx++, Length--)
	  gnu_ret = fold_build2 (PLUS_EXPR, comp_type,
				 fold_build2 (MULT_EXPR, comp_type,
					      gnu_ret, gnu_base),
				 build_cst_from_int (comp_type,
						     Udigits_Ptr[Idx]));
    }

  gnu_ret = convert (type, gnu_ret);

  /* We don't need any NOP_EXPR or NON_LVALUE_EXPR on GNU_RET.  */
  while ((TREE_CODE (gnu_ret) == NOP_EXPR
	  || TREE_CODE (gnu_ret) == NON_LVALUE_EXPR)
	 && TREE_TYPE (TREE_OPERAND (gnu_ret, 0)) == TREE_TYPE (gnu_ret))
    gnu_ret = TREE_OPERAND (gnu_ret, 0);

  return gnu_ret;
}
