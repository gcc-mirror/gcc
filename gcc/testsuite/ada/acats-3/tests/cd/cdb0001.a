-- CDB0001.A

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
-- OBJECTIVE:
--     If Storage_Size is specified for an access type, check that
--     Storage_Error is raised by an allocator when insufficient storage
--     is available.

-- HISTORY:
--     DHH 09/23/87 Created original test.
--     PMW 09/19/88 Modified withdrawn test.
--     THS 03/21/90 Changed extension from '.dep' to '.ada' and
--                  completely revised the test to prevent optimization.
--     LDC 09/20/90 Removed unused variable, changed Fail calls to
--                  Comment for 'Storage_Size /= Specified_Size,
--                  moved loop for check values to exception handler.
--     RLB 07/20/12 Changed test format and name to "modern"; revised
--                  to allow the Storage_Size value to be within the
--                  the range allowed by the
--                  Maximum_Adjustment_To_Specified_Storage_Size constant.
--     RLB 04/01/13 Corrected mistakenly redundant comment.


with ImpDef;
with Report; Use Report;
with System;
procedure CDB0001 is

     Specified_Size : constant := 1000;
     Max_Storage_Size : constant := Specified_Size +
                       ImpDef.Maximum_Adjustment_To_Specified_Storage_Size;

     type Check_Type is access Integer;
     for Check_Type'Storage_Size use Specified_Size;

     Units_Per_Integer : constant :=
         (Integer'Size + System.Storage_Unit - 1) / System.Storage_Unit;

     type Acc_Array_Type is array
         (Integer range 1 .. (Check_Type'Storage_Size /
          Units_Per_Integer) + 1) of Check_Type;
     Acc_Array : Acc_Array_Type;

     Place_I_Stopped : Natural := 0;

begin

     Report.Test ("CDB0001", "If Storage_Size is specified for an access " &
                             "type, check that Storage_Error is raised by " &
                             "an allocator when insufficient storage is " &
                             "available");

     if Check_Type'Storage_Size < Ident_Int (Specified_Size) Then
          Report.Failed ("Check_Type'Storage_Size is less than the value " &
                  "specified in the attribute definition clause");

     elsif Check_Type'Storage_Size > Ident_Int (Max_Storage_Size) Then
          Report.Failed ("Check_Type'Storage_Size is greater than the value " &
                  "specified in the attribute definition clause with " &
                  "maximum rounding");

     elsif Check_Type'Storage_Size /= Ident_Int (Specified_Size) Then
          Report.Comment ("Check_Type'Storage_Size is rounded up");

     else
          Report.Comment ("Check_Type'Storage_Size is exactly as specified");

     end if;

     begin

          for I in Acc_Array'range loop
               Acc_Array (I) := new Integer'(Ident_Int (I));
               Place_I_Stopped := I;
          end loop;

          Report.Failed ("No exception raised when reserved space " &
                  "exceeded");

     exception
          when Storage_Error =>
               for I in 1 .. Place_I_Stopped loop
                    if Acc_Array (I).all /= Ident_Int (I) then
                        Report.Failed ("Incorrect value for Acc_Array (" &
                                Integer'Image (I) & ")");
                    end if;
               end loop;
          when others =>
               Report.Failed ("Wrong exception raised when reserved space " &
                       "exceeded");
     end;

     Report.Result;

end CDB0001;
