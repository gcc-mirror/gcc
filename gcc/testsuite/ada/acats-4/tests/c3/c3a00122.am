-- C3A00122.AM
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
-- OBJECTIVE:
--      Check that an access-to-subprogram object can be used to invoke a
--      subprogram when the subprogram body had been declared and implemented
--      as a subunit.
--
-- TEST DESCRIPTION:
--      Declare an access to procedure type in a main program.  Declare 
--      three different log subprogram body stubs that can be referred to by 
--      the access to procedure type.
--
--      Complete bodies of the log procedures.
--
--      In the main program, each procedure will be called indirectly by 
--      dereferencing the access value.
-- 
-- TEST FILES:
--      The following files comprise this test:
--
--         C3A00120.A
--         C3A00121.A
--      => C3A00122.AM
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!
 
 with Report;
 
 with C3A0012_0; 
 
 procedure C3A00122 is
 
    function "="( A,B: C3A0012_0.Call_Kind ) return Boolean
             renames C3A0012_0."=";
 
    Log_Access  : C3A0012_0.Log_Procedure_Ptr;
    Theta       : Float := 0.0;
    Method      : C3A0012_0.Call_Kind := C3A0012_0.No_Call_Made;
 
 
 
   function Due_Process( LA: C3A0012_0.Log_Procedure_Ptr )
            return C3A0012_0.Call_Kind is
     Result : C3A0012_0.Call_Kind := C3A0012_0.No_Call_Made;
   begin
     LA( Theta, Result );
     return Result;
   end Due_Process;
 
 begin
 
    Report.Test ("C3A0012", "Check that an access to a subprogram object " &
                 "can be used to select and invoke an operation with " &
                 "appropriate arguments");
 
    Log_Access := C3A0012_0.Log_Calc_Fast'Access;
 
    -- Invoking Log procedure designated by access value
    Method := Due_Process( Log_Access );
 
    If Method /= C3A0012_0.Fast_Call then
       Report.Failed ("Incorrect Log_Calc_Fast result");
    end if;
 
    Log_Access := C3A0012_0.Log_Calc_Acc'Access;
 
    -- Invoking Log procedure designated by access value
    Method := Due_Process( Log_Access );
 
    If Method /= C3A0012_0.Accurate_Call then
       Report.Failed ("Incorrect Log_Calc_Acc result");
    end if;
 
    Log_Access := C3A0012_0.Log_Calc_Table'Access;
 
    -- Invoking Log procedure designated by access value
    Method := Due_Process( Log_Access );
 
    If Method /= C3A0012_0.Table_Lookup_Call then
       Report.Failed ("Incorrect Log_Calc_Table result");
    end if;
 
    Report.Result;
 
 end C3A00122;
 
