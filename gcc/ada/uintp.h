/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                                U I N T P                                 *
 *                                                                          *
 *                              C Header File                               *
 *                                                                          *
 *            Copyright (C) 1992-2020, Free Software Foundation, Inc.       *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 3,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License *
 * for  more details.  You should have  received  a copy of the GNU General *
 * Public License  distributed with GNAT; see file COPYING3.  If not, go to *
 * http://www.gnu.org/licenses for a complete copy of the license.          *
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/

/* This is the C header that corresponds to the Ada package specification for
   Uintp.  It was created manually from uintp.ads and must be kept synchronized
   with changes in this file.  */

#ifdef __cplusplus
extern "C" {
#endif

/* Support for universal integer arithmetic */

struct Uint_Entry
{
  Pos Length;
  Int Loc;
};

/* See if a Uint is within the range of an integer.  */
#define UI_Is_In_Int_Range  uintp__ui_is_in_int_range
extern Boolean UI_Is_In_Int_Range	(Uint);

/* Obtain Char_Code value from Uint input.  Value must be in range.  */
#define UI_To_CC uintp__ui_to_cc
extern Char_Code UI_To_CC		(Uint);

/* Convert a Char_Code into a Uint.  */
#define UI_From_CC uintp__ui_from_cc
extern Uint UI_From_CC			(Char_Code);

/* Obtain Int value from Uint input.  Abort if the result is out of range.  */
#define UI_To_Int uintp__ui_to_int
extern Int UI_To_Int			(Uint);

/* Similarly, but return a GCC INTEGER_CST.  */
extern tree UI_To_gnu			(Uint, tree);

/* Convert an Int into a Uint.  */
#define UI_From_Int uintp__ui_from_int
extern Uint UI_From_Int			(int);

/* Similarly, but take a GCC INTEGER_CST.  */
extern Uint UI_From_gnu			(tree);

/* A constant value indicating a missing or unset Uint value.  */
#define UI_No_Uint uintp__no_uint
extern const Uint UI_No_Uint;

/* Uint values are represented as multiple precision integers stored in a
   multi-digit format using UI_Base as the base.  This value is chosen so
   that the product UI_Base*UI_Base is within the range of Int values.  */
#define UI_Base uintp__base
extern const int UI_Base;

/* Types for the fat pointer of Int vectors and the template it points to.  */
typedef struct {int Low_Bound, High_Bound; } Vector_Template;
typedef struct {const int *Array; Vector_Template *Bounds; }
	__attribute ((aligned (sizeof (char *) * 2))) Int_Vector;

/* Create and return the Uint value from the Int vector.  */
#define Vector_To_Uint uintp__vector_to_uint
extern Uint Vector_To_Uint		(Int_Vector, Boolean);

/* Compare integer values for equality.  */
#define UI_Eq uintp__ui_eq
extern Boolean UI_Eq			(Uint, Uint);

/* Compare integer values for less than.  */
#define UI_Lt uintp__ui_lt
extern Boolean UI_Lt			(Uint, Uint);

/* Universal integers are represented by the Uint type which is an index into
   the Uints_Ptr table containing Uint_Entry values.  A Uint_Entry contains an
   index and length for getting the "digits" of the universal integer from the
   Udigits_Ptr table.

   For efficiency, this method is used only for integer values larger than the
   constant Uint_Bias.  If a Uint is less than this constant, then it contains
   the integer value itself.  The origin of the Uints_Ptr table is adjusted so
   that a Uint value of Uint_Bias indexes the first element.  */

#define Uints_Ptr (uintp__uints__table - Uint_Table_Start)
extern struct Uint_Entry *uintp__uints__table;

#define Udigits_Ptr uintp__udigits__table
extern int *uintp__udigits__table;

#ifdef __cplusplus
}
#endif
