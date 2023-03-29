/****************************************************************************
 *                                                                          *
 *                        GNAT COMPILER COMPONENTS                          *
 *                                                                          *
 *                               C U I N T P                                *
 *                                                                          *
 *                          C Implementation File                           *
 *                                                                          *
 *          Copyright (C) 1992-2023, Free Software Foundation, Inc.         *
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

/* This file corresponds to the Ada package body Uintp.  It was created
   manually from the files uintp.ads and uintp.adb.  */

#include "config.h"
#include "system.h"
#include "coretypes.h"
#include "tm.h"
#include "vec.h"
#include "alias.h"
#include "tree.h"
#include "inchash.h"
#include "fold-const.h"

#include "ada.h"
#include "types.h"
#include "uintp.h"
#include "sinfo.h"
#include "ada-tree.h"
#include "gigi.h"

/* Universal integers are represented by the Uint type which is an index into
   the Uints_Ptr table containing Uint_Entry values.  A Uint_Entry contains an
   index and length for getting the "digits" of the universal integer from the
   Udigits_Ptr table.

   For efficiency, this method is used only for integer values larger than the
   constant Uint_Bias.  If a Uint is less than this constant, then it contains
   the integer value itself.

   First define a utility function that is build_int_cst for integral types and
   does a conversion for floating-point types.  */

static tree
build_cst_from_int (tree type, HOST_WIDE_INT low)
{
  if (SCALAR_FLOAT_TYPE_P (type))
    return convert (type, build_int_cst (gnat_type_for_size (32, 0), low));
  else
    return build_int_cst (type, low);
}

/* Similar to UI_To_Int, but return a GCC INTEGER_CST or REAL_CST node,
   depending on whether TYPE is an integral or real type.  Overflow is tested
   by the constant-folding used to build the node.  TYPE is the GCC type of
   the resulting node.  */

tree
UI_To_gnu (Uint Input, tree type)
{
  /* We might have a TYPE with biased representation and be passed an unbiased
     value that doesn't fit.  We always use an unbiased type to be able to hold
     any such possible value for intermediate computations and then rely on a
     conversion back to TYPE to perform the bias adjustment when need be.  */
  tree comp_type
    = TREE_CODE (type) == INTEGER_TYPE && TYPE_BIASED_REPRESENTATION_P (type)
      ? get_base_type (type) : type;
  tree gnu_ret;

  if (Input <= Uint_Direct_Last)
    gnu_ret = build_cst_from_int (comp_type, Input - Uint_Direct_Bias);
  else
    {
      Int Idx = (*Uints_Ptr)[Input - Uint_Table_Start].Loc;
      Pos Length = (*Uints_Ptr)[Input - Uint_Table_Start].Length;
      Int First = (*Udigits_Ptr)[Idx];
      tree gnu_base;

      gcc_assert (Length > 0);

      /* The computations we perform below always require a type at least as
	 large as an integer not to overflow.  FP types are always fine, but
	 INTEGER or ENUMERAL types we are handed may be too short.  We use a
	 base integer type node for the computations in this case and will
	 convert the final result back to the incoming type later on.  */
      if (!SCALAR_FLOAT_TYPE_P (comp_type) && TYPE_PRECISION (comp_type) < 32)
	comp_type = gnat_type_for_size (32, 0);

      gnu_base = build_cst_from_int (comp_type, Base);

      gnu_ret = build_cst_from_int (comp_type, First);
      if (First < 0)
	for (Idx++, Length--; Length; Idx++, Length--)
	  gnu_ret = fold_build2 (MINUS_EXPR, comp_type,
				 fold_build2 (MULT_EXPR, comp_type,
					      gnu_ret, gnu_base),
				 build_cst_from_int (comp_type,
						     (*Udigits_Ptr)[Idx]));
      else
	for (Idx++, Length--; Length; Idx++, Length--)
	  gnu_ret = fold_build2 (PLUS_EXPR, comp_type,
				 fold_build2 (MULT_EXPR, comp_type,
					      gnu_ret, gnu_base),
				 build_cst_from_int (comp_type,
						     (*Udigits_Ptr)[Idx]));
    }

  gnu_ret = convert (type, gnu_ret);

  /* We don't need any NOP_EXPR or NON_LVALUE_EXPR on GNU_RET.  */
  while ((TREE_CODE (gnu_ret) == NOP_EXPR
	  || TREE_CODE (gnu_ret) == NON_LVALUE_EXPR)
	 && TREE_TYPE (TREE_OPERAND (gnu_ret, 0)) == TREE_TYPE (gnu_ret))
    gnu_ret = TREE_OPERAND (gnu_ret, 0);

  return gnu_ret;
}

/* Similar to UI_From_Int, but take a GCC INTEGER_CST.  We use UI_From_Int
   when possible, i.e. for a 32-bit signed value, to take advantage of its
   built-in caching mechanism.  For values of larger magnitude, we compute
   digits into a vector and call Vector_To_Uint.  */

Uint
UI_From_gnu (tree Input)
{
  tree gnu_type = TREE_TYPE (Input), gnu_base, gnu_temp;
  /* UI_Base is defined so that 5 Uint digits is sufficient to hold the
     largest possible signed 64-bit value.  */
  const int Max_For_Dint = 5;
  int v[Max_For_Dint];
  Vector_Template temp;
  Int_Vector vec;

#if HOST_BITS_PER_WIDE_INT < 64
#error unsupported HOST_BITS_PER_WIDE_INT setting
#endif

  /* On 64-bit hosts, tree_fits_shwi_p tells whether the input fits in
     a signed 64-bit integer.  Then a truncation tells whether it fits
     in a signed 32-bit integer.  */
  if (tree_fits_shwi_p (Input))
    {
      HOST_WIDE_INT hw_input = tree_to_shwi (Input);
      if (hw_input == (int) hw_input)
	return UI_From_Int (hw_input);
    }
  else
    return No_Uint;

  gnu_base = build_int_cst (gnu_type, UI_Base);
  gnu_temp = Input;

  for (int i = Max_For_Dint - 1; i >= 0; i--)
    {
      v[i] = tree_to_shwi (fold_build1 (ABS_EXPR, gnu_type,
					fold_build2 (TRUNC_MOD_EXPR, gnu_type,
						     gnu_temp, gnu_base)));
      gnu_temp = fold_build2 (TRUNC_DIV_EXPR, gnu_type, gnu_temp, gnu_base);
    }

  temp.Low_Bound = 1;
  temp.High_Bound = Max_For_Dint;
  vec.Bounds = &temp;
  vec.Array = v;
  return Vector_To_Uint (vec, tree_int_cst_sgn (Input) < 0);
}
