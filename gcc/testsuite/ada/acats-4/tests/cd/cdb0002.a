-- CDB0002.A

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
--     Check that operations on values of an access type are not affected
--     if Storage_Size is specified for the type.

-- HISTORY:
--     BCB 11/01/88  Created original test.
--     RJW 05/17/89  Changed from '.dep' test to '.ada' test.
--                   Added check for Unchecked_Deallocation.
--     RLB 07/20/12  Changed test format and name to "modern"; revised
--                   to allow the Storage_Size value to be within the
--                   the range allowed by the
--                   Maximum_Adjustment_To_Specified_Storage_Size constant.

with Ada.Unchecked_Deallocation;
with ImpDef;
with Report; Use Report;
with System;
procedure CDB0002 is

     Basic_Size : constant := 1024;

     type Maintype is array (Integer range <>) of Integer;
     type Acc_Type is access Maintype;
     subtype Acc_Range is Acc_Type (1 .. 3);

     for Acc_Type'Storage_Size use Basic_Size;

     type Record_Type is record
          Comp : Acc_Type;
     end record;

     Check_Type1 : Acc_Type;
     Check_Type2 : Acc_Type;
     Check_Type3 : Acc_Type(1..3);

     Check_Array : Array (1..2) of Acc_Type;

     Check_Record1 : Record_Type;
     Check_Record2 : Record_Type;

     Check_Param1 : Acc_Type;
     Check_Param2 : Acc_Type;

     Check_null : Acc_Type := null;

     procedure Proc (Acc1,Acc2 : in out Acc_Type) is

     begin

          if (Acc1.all /= Acc2.all) then
               Report.Failed ("Incorrect values for designated objects " &
                       "- 1");
          end if;

          if Report.Equal (3,3) then
               Acc2 := Acc1;
          end if;

          if Acc2 /= Acc1 then
               Report.Failed ("Incorrect results for relational operators " &
                       "-1");
          end if;

          if (Acc1 in Acc_Range) then
               Report.Failed ("Incorrect results for membership test - 1");
          end if;

     end Proc;

begin

     Report.Test ("CDB0002", "Check that operations on values of an access " &
                             "Type are not affected if Storage_Size is " &
                             "specified for the Type");

     Check_Param1 := new Maintype'(25,35,45);
     Check_Param2 := new Maintype'(25,35,45);

     Proc (Check_Param1, Check_Param2);

     if Acc_Type'Storage_Size < Basic_Size then
          Report.Failed ("Incorrect value for access type Storage_Size - 1");
     elsif Acc_Type'Storage_Size > Basic_Size +
                       ImpDef.Maximum_Adjustment_To_Specified_Storage_Size then
          Report.Failed ("Incorrect value for access type Storage_Size - 2");
     end if;

     Check_Type1 := new Maintype'(25,35,45);
     Check_Type2 := new Maintype'(25,35,45);
     Check_Type3 := new Maintype'(1 => 1,2 => 2,3 => 3);

     Check_Array (1) := new Maintype'(25,35,45);
     Check_Array (2) := new Maintype'(25,35,45);

     Check_Record1.Comp := new Maintype'(25,35,45);
     Check_Record2.Comp := new Maintype'(25,35,45);

     if (Check_Type1.all /= Check_Type2.all) then
          Report.Failed ("Incorrect values for designated objects - 2");
     end if;

     if Report.Equal (3,3) then
          Check_Type2 := Check_Type1;
     end if;

     if Check_Type2 /= Check_Type1 then
          Report.Failed ("Incorrect results for relational operators - 2");
     end if;

     if (Check_Type1 in Acc_Range) then
          Report.Failed ("Incorrect results for membership test - 2");
     end if;

     if (Check_Array (1).all /= Check_Array (2).all) then
          Report.Failed ("Incorrect values for designated objects - 3");
     end if;

     if Report.Equal (3,3) then
          Check_Array (2) := Check_Array (1);
     end if;

     if Check_Array (2) /= Check_Array (1) then
          Report.Failed ("Incorrect results for relational operators - 3");
     end if;

     if (Check_Array (1) in Acc_Range) then
          Report.Failed ("Incorrect results for membership test - 3");
     end if;

     if (Check_Record1.Comp.all /= Check_Record2.Comp.all) then
          Report.Failed ("Incorrect values for designated objects - 4");
     end if;

     if Report.Equal (3,3) then
          Check_Record2 := Check_Record1;
     end if;

     if Check_Record2 /= Check_Record1 then
          Report.Failed ("Incorrect results for relational operators - 4");
     end if;

     if (Check_Record1.Comp in Acc_Range) then
          Report.Failed ("Incorrect results for membership test - 4");
     end if;

     if Check_Type3'First /= Ident_Int (1) then
          Report.Failed ("Incorrect value for Check_Type3'First");
     end if;

     if Check_Type3'Last /= Ident_Int (3) then
          Report.Failed ("Incorrect value for Check_Type3'Last");
     end if;

     declare
          type Acc_Char is access Character;
          for Acc_Char'Storage_Size use 128;

          Limit : Integer :=
           (Acc_Char'Storage_Size * System.Storage_Unit)/Character'Size;

          Acc_Array : Array (1 .. Limit + 1) of Acc_Char;
          Place : Integer;

          procedure Free is
               new Ada.Unchecked_Deallocation (Character, Acc_Char);
     begin
          for I in Acc_Array'range loop
               ACC_Array (Ident_Int (I)) :=
                    new Character'
                         (Ident_Char ((Character'Val (I mod 128))));
               Place := I;
          end loop;
          Report.Failed ("No exception raised when Storage_Size exceeded");
     exception
          when Storage_Error =>
               begin
                    for I in 1 .. Place loop
                         if I mod 2 = 0 then
                              Free (Acc_Array (Ident_Int (I)));
                         end if;
                    end loop;

                    for I in 1 .. Place loop
                         if I mod 2 = 1 and then
                            Ident_Char (Acc_Array (I).all) /=
                            Character'Val (I mod Ident_Int (128)) then
                              Report.Failed ("Incorrect value in array");
                         end if;
                    end loop;
               end;
          when others =>
               Report.Failed ("Wrong exception raised");
     end;

     Report.Result;
end CDB0002;
