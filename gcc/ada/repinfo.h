/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                              R E P I N F O                               *
 *                                                                          *
 *                              C Header File                               *
 *                                                                          *
 *          Copyright (C) 1999-2011, Free Software Foundation, Inc.         *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 3,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.                                     *
 *                                                                          *
 * As a special exception under Section 7 of GPL version 3, you are granted *
 * additional permissions described in the GCC Runtime Library Exception,   *
 * version 3.1, as published by the Free Software Foundation.               *
 *                                                                          *
 * You should have received a copy of the GNU General Public License and    *
 * a copy of the GCC Runtime Library Exception along with this program;     *
 * see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    *
 * <http://www.gnu.org/licenses/>.                                          *
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/

/* This file corresponds to the Ada file repinfo.ads.  */

#ifdef __cplusplus
extern "C" {
#endif

typedef Uint Node_Ref;
typedef Uint Node_Ref_Or_Val;
typedef char TCode;

/* These are the values of TCcode that correspond to tree codes in tree.def,
   except for the first, which is how we encode discriminants.  */

#define Discrim_Val       0
#define Cond_Expr         1
#define Plus_Expr         2
#define Minus_Expr        3
#define Mult_Expr         4
#define Trunc_Div_Expr    5
#define Ceil_Div_Expr     6
#define Floor_Div_Expr    7
#define Trunc_Mod_Expr    8
#define Ceil_Mod_Expr     9
#define Floor_Mod_Expr   10
#define Exact_Div_Expr   11
#define Negate_Expr      12
#define Min_Expr         13
#define Max_Expr         14
#define Abs_Expr         15
#define Truth_Andif_Expr 16
#define Truth_Orif_Expr  17
#define Truth_And_Expr   18
#define Truth_Or_Expr    19
#define Truth_Xor_Expr   20
#define Truth_Not_Expr   21
#define Lt_Expr          22
#define Le_Expr          23
#define Gt_Expr          24
#define Ge_Expr          25
#define Eq_Expr          26
#define Ne_Expr          27
#define Bit_And_Expr     28

/* Creates a node using the tree code defined by Expr and from 1-3
   operands as required (unused operands set as shown to No_Uint) Note
   that this call can be used to create a discriminant reference by
   using (Expr => Discrim_Val, Op1 => discriminant_number).  */
#define Create_Node repinfo__create_node
extern Node_Ref Create_Node	(TCode, Node_Ref_Or_Val,
				 Node_Ref_Or_Val, Node_Ref_Or_Val);

#ifdef __cplusplus
}
#endif
