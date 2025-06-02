/*
-- CXB30130.C                                                            
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
-- FUNCTION NAME: CXB30130   ("square_it")                               
--                                                                       
-- FUNCTION DESCRIPTION:                                                 
--      This C function returns the square of num1 through the function  
--      name, and returns the square of parameters num2, num3, and num4  
--      through the argument list (modifying the objects pointed to by   
--      the parameters).                         
--
-- INPUTS:
--      This function requires that four parameters be passed to it.
--      The types of these parameters are, in order: int, pointer to short, 
--      pointer to float, and pointer to double.
--
-- PROCESSING:
--      The function will calculate the square of the int parameter (num1),
--      and return this value as the function result through the function
--      name.  The function will also calculate the square of the values
--      pointed to by the remaining three parameters (num2, num3, num4), 
--      and will modify the referenced memory locations to contain the 
--      squared values.
--
-- OUTPUTS:                        
--      The square of num1 is returned through function name. 
--      Parameters num2-num4 now point to values that are the squared results
--      of the originally referenced values (i.e., the original values are
--      modified as a result of this function).
--                                                                       
-- CHANGE HISTORY:                                                       
--      12 Oct 95   SAIC    Initial prerelease version.                  
--                                                                       
--!
*/
                                                                         
int CXB30130 (int num1, short* num2, float* num3, double* num4)          
                                                                         
/* NOTE: The above function definition should be accepted by an ANSI-C   */
/*       compiler.  Older C compilers may reject it; they may, however   */
/*       accept the following five lines.  An implementation may comment */
/*       out the above function definition and uncomment the following   */
/*       one.  Otherwise, an implementation must provide the necessary   */
/*       modifications to this C code to satisfy the function            */
/*       requirements (see Function Description).                        */
/*                                                                       */
/*  int CXB30130 (num1, num2, num3, num4)                                */
/*     int     num1;                                                     */
/*     short*  num2;                                                     */
/*     float*  num3;                                                     */
/*     double* num4;                                                     */
/*                                                                       */

{
   int return_value = 0;

   return_value = num1 * num1;
   *num2 = *num2 * *num2; /* Return square of these parameters through   */
   *num3 = *num3 * *num3; /* the parameter list.                         */
   *num4 = *num4 * *num4;

   return (return_value); /* Return square of num1 through function name */
}
