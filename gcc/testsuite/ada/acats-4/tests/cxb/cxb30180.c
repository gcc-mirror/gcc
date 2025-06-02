/*
-- CXB30180.C
--
--                             Grant of Unlimited Rights
--
--     The Ada Conformity Assessment Authority (ACAA) holds unlimited
--     rights in the software and documentation contained herein. Unlimited
--     rights are the same as those granted by the U.S. Government for older
--     parts of the Ada Conformity Assessment Test Suite, and are defined
--     in DFAR 252.227-7013(a)(19). By making this public release, the ACAA
--     intends to confer upon all recipients unlimited rights equal to those
--     held by the ACAA. These rights include rights to use, duplicate,
--     release or disclose the released technical data and computer software
--     in whole or in part, in any manner and for any purpose whatsoever, and
--     to have or permit others to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS. THE ACAA MAKES NO EXPRESS OR IMPLIED
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--
--                                     Notice
--
--     The ACAA has created and maintains the Ada Conformity Assessment Test
--     Suite for the purpose of conformity assessments conducted in accordance
--     with the International Standard ISO/IEC 18009 - Ada: Conformity
--     assessment of a language processor. This test suite should not be used
--     to make claims of conformance unless used in accordance with
--     ISO/IEC 18009 and any applicable ACAA procedures.
--*
--
-- FUNCTION NAME: CXB30180
--
-- FUNCTION DESCRIPTION:
--      This C function calls back into Ada passing through the parameters.
--      It returns the value of the first parameter plus CXB3018_Global.
--
-- INPUTS:
--      This function requires that four parameters be passed to it.
--      The types of these parameters are, in order: int, pointer to short,
--      pointer to float, and pointer to double.
--      This also reads the value of the global CXB3018_Global.
--
-- PROCESSING:
--      The function will call an Ada procedure which will update the
--      parameters.
--
-- OUTPUTS:
--      The value of num1 after the call to the Ada routine CXB3018_Ada_Proc
--      summed with CXB3018_Global is returned through function name.
--      Parameters num2-num4 now point to values that are the values of
--      parameters num2-numb4 modified by the Ada routine CXB3018_Ada_Proc.
--
-- OBJECTIVE
--      See CXB30182.AM.
--
-- TEST DESCRIPTION
--      See CXB30182.AM.
--
-- TEST FILES:
--      This test consists of the following files:
--      -> CXB30180.C
--         CXB30181.A
--         CXB30182.AM
--
-- CHANGE HISTORY:
--      24 Mar 2014    STT     Created initial version.
--      26 Mar 2014    RLB     Changed naming consistent with ACATS.
--
--!
*/

extern int CXB30181_Global;
/* Ada declaration:
 *   Global : Interfaces.C.Int := 0
 *      with Export, Convention => C, External_Name => "CXB30181_Global";
 */
int CXB30180 (int num1, short* num2, float* num3, double* num4)

/* NOTE: The above function definition should be accepted by an ANSI-C   */
/*       compiler.  Older C compilers may reject it; they may, however   */
/*       accept the following five lines.  An implementation may comment */
/*       out the above function definition and uncomment the following   */
/*       one.  Otherwise, an implementation must provide the necessary   */
/*       modifications to this C code to satisfy the function            */
/*       requirements (see Function Description).                        */
/*                                                                       */
/*  int CXB30180 (num1, num2, num3, num4)                                */
/*     int     num1;                                                     */
/*     short*  num2;                                                     */
/*     float*  num3;                                                     */
/*     double* num4;                                                     */
/*                                                                       */

{
   int return_value = 0;
   extern void CXB30181_Ada_Doubler
      (int* out1, short* out2, float* out3, double* out4);
   /* Ada declaration:
    *  procedure Ada_Doubler
    *   (InOut1 : in out C.int;     InOut2 : in out C.Short;
    *    InOut3 : in out C.C_Float; InOut4 : in out C.Double)
    *      with Export, Convention => C, External_Name => "CXB30181_Ada_Doubler";
    */

   CXB30181_Ada_Doubler(&num1, num2, num3, num4);
   return num1 + CXB30181_Global;
}
