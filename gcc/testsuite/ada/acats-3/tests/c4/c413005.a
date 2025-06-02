-- C413005.A
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
-- OBJECTIVE:
--     Check that a prefixed view is the name of a subprogram (with the first
--     parameter omitted from the profile) that can be renamed and passed as
--     a generic formal parameter.
--
-- CHANGE HISTORY:
--     27 Sep 2007 RLB Created test.
--     25 Oct 2007 RLB Corrected test errors.
--
--!
package C413005P is
   type TP is tagged record
      Data  : Integer := 999;
   end record;

   procedure Clear  (X : in out TP);
   procedure Set    (X : in out TP; Value : in Integer);
   function  Get    (X : TP) return Integer;
   function  Prod   (X : TP; Value : Integer) return Integer;

   procedure Class_Wide_Clear (X : in out TP'Class);
   procedure Class_Wide_Set   (X : in out TP'Class; Value : Integer);
   function  Class_Wide_Get   (X : TP'Class) return Integer;
   function  Class_Wide_Prod  (X : TP'Class; Value : Integer) return Integer;

end C413005P;

package body C413005P is

   procedure Clear (X : in out TP) is
   begin
      X.Data := 0;
   end Clear;

   procedure Set  (X : in out TP; Value : Integer) is
   begin
      X.Data := Value;
   end Set;

   function Get (X : TP) return Integer is
   begin
      return X.Data;
   end Get;

   function  Prod (X : TP; Value : Integer) return Integer is
   begin
      return X.Data*Value;
   end Prod;

   procedure Class_Wide_Clear (X: in out TP'Class) is
   begin
      X.Data := 0;
   end Class_Wide_Clear;

   procedure Class_Wide_Set (X: in out TP'Class; Value : Integer) is
   begin
      X.Data := Value;
   end Class_Wide_Set;

   function Class_Wide_Get (X : TP'Class) return Integer is
   begin
      return X.Data;
   end Class_Wide_Get;

   function Class_Wide_Prod
     (X : TP'Class; Value : Integer) return Integer is
   begin
      return X.Data * Value;
   end Class_Wide_Prod;
end C413005P;

with C413005P;
with Report;
procedure C413005 is

   Count : Natural := 1;

   generic
      with function Get return Integer;
      with procedure Set (Value : in Integer);
   procedure Check (Message : in String);

   procedure Check (Message : in String) is
   begin
      Set (Count);
      if Get /= Count then
         Report.Failed (Message & " - value wrong:" & Integer'Image(Get) &
            " should be" & Integer'Image(Count));
      end if;
      Count := Count + 2;
   end Check;

   Result1 : aliased C413005P.TP;
   Result2 : aliased C413005P.TP;
   Result_Count : Natural := 1;
   function Func return access C413005P.TP is
      -- This function changes the object it returns every other call;
      -- if extra calls are made to this function, the wrong object
      -- will be returned.
   begin
      Result_Count := Result_Count + 1;
      case Result_Count mod 4 is
         when 0 | 1 => return Result1'access;
         when 2 | 3 => return Result2'access;
         when others => raise Program_Error;
      end case;
   end Func;

begin
   Report.Test ("C413005", "Check that a prefixed view is the name of a " &
                           "subprogram (with the first parameter omitted " &
                           "from the profile) that can be renamed and " &
                           "passed as a generic formal parameter.");

   declare
      Obj1 : C413005P.TP;
      Obj2 : C413005P.TP;
   begin
      if Obj1.Get /= 999 then
         Report.Failed ("Simple func call - value wrong");
      end if;
      Obj1.Clear;
      Obj2.Class_Wide_Clear;
      if Obj1.Data /= 0 then
         Report.Failed ("Simple proc call - value wrong");
      end if;
      if Obj2.Data /= 0 then
         Report.Failed ("Simple class-wide proc call - value wrong");
      end if;
      Func.Clear;   -- Result2
      if Func.Get /= 0 then  -- Result2
         Report.Failed ("Func prefix calls - result wrong");
      end if;
      declare
         function My_Get return Integer renames Obj1.Get;
         function My_Prod (Value : Integer) return Integer renames Obj1.Prod;
         procedure My_Set (Value : in Integer) renames Obj1.Set;
         function Other_Get return Integer renames Obj2.Class_Wide_Get;
         procedure Other_Set (Value : in Integer) renames Obj2.Class_Wide_Set;
         function Func_Get return Integer renames Func.Get;
         procedure Func_Set (Value : in Integer) renames Func.Set;
      begin
         if My_Get /= 0 then
            Report.Failed ("Renames get - value wrong");
         end if;
         if Other_Get /= 0 then
            Report.Failed ("Renames class-wide get - value wrong");
         end if;
         if Func_Get /= 999 then -- Result1, initial value. (No call to
                                 -- Func here.)
            Report.Failed ("Renames func get - value wrong");
         end if;
         My_Set (14);
         Other_Set (92);
         Func_Set (31);
         if My_Get /= 14 then
            Report.Failed ("Renames set - value wrong");
         end if;
         if Other_Get /= 92 then
            Report.Failed ("Renames class-wide get - value wrong");
         end if;
         if Func_Get /= 31 then
            Report.Failed ("Renames func get - value wrong");
         end if;
         if My_Prod (3) /= 42 then
            Report.Failed ("Renames prod - value wrong");
         end if;
      end;
      declare
          procedure Check1 is new Check (Obj1.Class_Wide_Get,
                                         Obj1.Class_Wide_Set);
          procedure Check2 is new Check (Obj2.Get, Obj2.Set);
          procedure Check3 is new Check (Func.Class_Wide_Get,
                                         Func.Class_Wide_Set);
      begin
          Check1 ("Generic Obj1, try 1");
          Check2 ("Generic Obj2, try 1");
          Check3 ("Generic Func, try 1");
          Check2 ("Generic Obj2, try 2");
          Check3 ("Generic Func, try 2");
          Check1 ("Generic Obj1, try 2");
          Check2 ("Generic Obj2, try 3");
      end;
   end;
   Report.Result;
end C413005;
