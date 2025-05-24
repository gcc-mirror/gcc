/*
-- CXB30040.C
--
--                             Grant of Unlimited Rights
--
--     Under contracts F33600-87-D-0337, F33600-84-D-0280, MDA903-79-C-0687,
--     F08630-91-C-0015, and DCA100-97-D-0025, the U.S. Government obtained
--     unlimited rights in the software and documentation contained herein.
--     Unlimited rights are defined in DFAR 252.227-7013(a)(19).  By making
--     this public release, the Government intends to confer upon all
--     recipients unlimited rights  equal to those held by the Government.
--     These rights include rights to use, duplicate, release or disclose the
--     released technical data and computer software in whole or in part, in
--     any manner and for any purpose whatsoever, and to have or permit others
--     to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS.  THE GOVERNMENT MAKES NO EXPRESS OR IMPLIED
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--*
--
-- FUNCTION NAME: CXB30040   ("char_gen")
--
-- FUNCTION DESCRIPTION:
--      This C function returns the value of type char corresponding to the
--      value of its parameter, where
--          Val  0 ..  9 ==> '0' .. '9'
--          Val 10 .. 19 ==> 'A' .. 'J'
--          Val 20 .. 29 ==> 'k' .. 't'
--          Val 30       ==> ' '
--          Val 31       ==> '.'
--          Val 32       ==> ','
--
-- INPUT:
--      This function requires that one int parameter be passed to it.
--
-- OUTPUT:
--      The function will return the appropriate value of type char.
--
-- CHANGE HISTORY:
--      13 Sep 99   RLB     Created function to replace incorrect
--                          Unchecked_Conversion.
--
--!
*/

char CXB30040 (int val)

/* NOTE: The above function definition should be accepted by an ANSI-C   */
/*       compiler.  Older C compilers may reject it; they may, however   */
/*       accept the following two lines.  An implementation may comment  */
/*       out the above function definition and uncomment the following   */
/*       one.  Otherwise, an implementation must provide the necessary   */
/*       modifications to this C code to satisfy the function            */
/*       requirements (see Function Description).                        */
/*                                                                       */
/*  char CXB30040 (val)                                                  */
/*     int     val;                                                      */
/*                                                                       */

{  char return_value = ';';

   switch (val)
   {
      case 0:
         return_value = '0';
         break;
      case 1:
         return_value = '1';
         break;
      case 2:
         return_value = '2';
         break;
      case 3:
         return_value = '3';
         break;
      case 4:
         return_value = '4';
         break;
      case 5:
         return_value = '5';
         break;
      case 6:
         return_value = '6';
         break;
      case 7:
         return_value = '7';
         break;
      case 8:
         return_value = '8';
         break;
      case 9:
         return_value = '9';
         break;
      case 10:
         return_value = 'A';
         break;
      case 11:
         return_value = 'B';
         break;
      case 12:
         return_value = 'C';
         break;
      case 13:
         return_value = 'D';
         break;
      case 14:
         return_value = 'E';
         break;
      case 15:
         return_value = 'F';
         break;
      case 16:
         return_value = 'G';
         break;
      case 17:
         return_value = 'H';
         break;
      case 18:
         return_value = 'I';
         break;
      case 19:
         return_value = 'J';
         break;
      case 20:
         return_value = 'k';
         break;
      case 21:
         return_value = 'l';
         break;
      case 22:
         return_value = 'm';
         break;
      case 23:
         return_value = 'n';
         break;
      case 24:
         return_value = 'o';
         break;
      case 25:
         return_value = 'p';
         break;
      case 26:
         return_value = 'q';
         break;
      case 27:
         return_value = 'r';
         break;
      case 28:
         return_value = 's';
         break;
      case 29:
         return_value = 't';
         break;
      case 30:
         return_value = ' ';
         break;
      case 31:
         return_value = '.';
         break;
      case 32:
         return_value = ',';
         break;
   }

   return (return_value); /* Return character value */
}
