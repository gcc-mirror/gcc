/*
-- CD30051.C                                                            
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
-- FUNCTION NAME: _cd3005_1
--                                                                       
-- FUNCTION DESCRIPTION:                                                 
--      This C function returns the sum of its parameter and 1 through
--      the function name.  The parameter is unchanged.
--
-- INPUTS:
--      This function requires that one parameter, of type int, be passed 
--      to it.
--
-- PROCESSING:
--      The function will calculate the sum of its parameter and 1
--      and return this value as the function result through the function
--      name.
--
-- OUTPUTS:                        
--      The sum of the parameter and 1 is returned through function name. 
--                                                                       
-- CHANGE HISTORY:                                                       
--      12 Oct 95   SAIC    Initial prerelease version.                  
--      14 Feb 97   PWB.CTA Created this file from code appearing in
--                          CD30005.A (as comments).
--!
*/
            int _cd30005_1( Value )
            {
                /* int Value */

               return Value + 1;
            }
                                                                         
