/*
-- CXB30131.C                                                           
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
-- FUNCTION NAME: CXB30131   ("combine_two_strings")                    
--                                                                      
-- FUNCTION DESCRIPTION:                                                
--      This C function returns a pointer to the combination of two     
--      input strings. 
--                                                                      
-- INPUTS:
--      This function requires that two parameters be passed to it.
--      The type of both of these parameters are pointer to char (which
--      is used to reference an array of chars).
--
-- PROCESSING:
--      The function will create a char array that is equal to the combined
--      length of the char arrays referenced by the two input parameters.
--      The char elements contained in the char arrays specified by the 
--      parameters will be combined (in order) into this new char array.
--
-- OUTPUTS:                        
--      The newly created char array will be returned as the function
--      result through the function name.  The char arrays referenced by the
--      two parameters will be unaffected.
--                                                                       
-- CHANGE HISTORY:                                                      
--      12 Oct 95   SAIC    Initial prerelease version. 
--      26 Oct 96   SAIC    Modified temp array initialization.                
--      15 Feb 99   RLB     Repaired to remove non-standard function strdup.
--!
*/

#include <string.h>
#include <stdlib.h>

char *stringdup (char *s)
{
  char *result = malloc(sizeof(char)*(strlen(s)+1));
  return strcpy(result,s);
}

char *CXB30131 (char *string1, char *string2)

/* NOTE: The above function definition should be accepted by an ANSI-C    */
/*       compiler.  Older C compilers may reject it; they may, however    */
/*       accept the following three lines.  An implementation may comment */
/*       out the above function definition and uncomment the following    */
/*       one.  Otherwise, an implementation must provide the necessary    */
/*       modifications to this C code to satisfy the function             */
/*       requirements (see Function Description).                         */
/*                                                                        */
/*   char *CXB30131 (string1, string2)                                    */
/*      char *string1;                                                    */
/*      char *string2;                                                    */

{
   char temp[100];  /* Local array that holds the combined strings */
   int  index;      /* Loop counter */
   int  length = 0; /* Variable that holds the length of the strings */

   /* Initialize the local array */
   for (index = 0; index < 100; index++)
   { temp[index] = 0; }

   /* Use the library function strcpy to copy the contents of string1 
      into temp.  */
   strcpy (temp, string1);

   /* Use the library function strlen to determine the number of 
      characters in the temp array (without the trailing nul). */
   length = strlen (temp);

   /* Add each character in string2 into the temp array, add nul 
      to the end of the array. */
   for (index = length; *string2 != '\0'; index++)
   { temp[index] = *string2++; }
   temp[index] = '\0';

   /* Use the library function strdup to return a pointer to temp. */
   return (stringdup(temp));
}
