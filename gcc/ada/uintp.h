/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                                U I N T P                                 *
 *                                                                          *
 *                              C Header File                               *
 *                                                                          *
 *          Copyright (C) 1992-2005, Free Software Foundation, Inc.         *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 2,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License *
 * for  more details.  You should have  received  a copy of the GNU General *
 * Public License  distributed with GNAT;  see file COPYING.  If not, write *
 * to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, *
 * MA 02111-1307, USA.                                                      *
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/

/* This file corresponds to the Ada package specification Uintp. It was
   created manually from the files uintp.ads and uintp.adb  */

/* Support for universal integer arithmetic */

struct Uint_Entry
{
  Pos Length;
  Int Loc;
};

/* See if a Uint is within the range of an integer.  */
#define UI_Is_In_Int_Range  uintp__ui_is_in_int_range
extern Boolean UI_Is_In_Int_Range	(Uint);

/* Obtain Char_Code value from Uint input. Value must be in range.  */
#define UI_To_CC uintp__ui_to_cc
extern Char_Code UI_To_CC               (Uint);

/* Obtain Int value from Uint input. This will abort if the result is
   out of range.  */
#define UI_To_Int uintp__ui_to_int
extern Int UI_To_Int			(Uint);

/* Convert an Int into a Uint.  */
#define UI_From_Int uintp__ui_from_int
extern Uint UI_From_Int			(int);

/* Convert a Char_Code into a Uint.  */
#define UI_From_CC uintp__ui_from_cc
extern Uint UI_From_CC                  (Char_Code);

/* Similarly, but return a GCC INTEGER_CST.  Overflow is tested by the
   constant-folding used to build the node.  TYPE is the GCC type of the
   resulting node.  */
extern tree UI_To_gnu			(Uint, tree);

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

#define Uint_0 (Uint_Direct_Bias + 0)
#define Uint_1 (Uint_Direct_Bias + 1)
