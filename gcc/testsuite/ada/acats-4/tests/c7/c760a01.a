-- C760A01.A
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
--    The ACAA has created and maintains the Ada Conformity Assessment Test
--    Suite for the purpose of conformity assessments conducted in accordance
--    with the International Standard ISO/IEC 18009 - Ada: Conformity
--    assessment of a language processor. This test suite should not be used
--    to make claims of conformance unless used in accordance with
--    ISO/IEC 18009 and any applicable ACAA procedures.
--*
--
-- OBJECTIVE:
--     Check that no separate anonymous object is used for a limited aggregate
--     initializing an object.
--
-- TEST DESCRIPTION:
--     We initialize various limited objects with aggregates that contain
--     a special checking component. This component records the operations
--     performed on it, in particular whether any assignment occurs.
--     This allows us to check for the visible semantic effects of
--     an anonymous object: the extra assignment operations that are required.
--
-- CHANGE HISTORY:
--      30 Apr 07   RLB   Created.
--      17 Aug 07   RLB   Corrected test errors.
--      28 Feb 14   RLB   Renamed test as rules have moved.
--
--!

with Report;
with F760A00.Child;
procedure C760A01 is
begin
   Report.Test ("C760A01",
                    "Check that no separate anonymous object is used for a " &
                    "limited aggregate initializing an object.");

   F760A00.Check_TC_Copy_Check;

   -- Object declarations:
   declare
      My_Obj : F760A00.Lim_Rec := (A => 1, B => False, TC => <>);
   begin
      F760A00.Check_Init (My_Obj, "Lim_Rec obj init");
      F760A00.Use_It (My_Obj);
   end;
   F760A00.Check_Fin_Lim_Rec ("Lim_Rec obj finalization");

   declare
      My_Prot : F760A00.Lim_Comp := (N => Report.Ident_Int(1), others => <>);
   begin
      F760A00.Check_Init (My_Prot, "Lim_Comp obj init");
      F760A00.Use_It (My_Prot);
   end;
   F760A00.Check_Fin_Lim_Comp ("Lim_Comp obj init");

   declare
      My_Arr : F760A00.Lim_Array := (1 .. Report.Ident_Int(5) =>
		(A => Report.Ident_Int(10), B => True, TC => <>));
         -- Also the expression of an array aggregate association.
   begin
      F760A00.Check_Init (My_Arr, "Lim_Array obj init");
      F760A00.Use_It (My_Arr);
   end;
   F760A00.Check_Fin_Lim_Array (Length => 5, Message => "Lim_Array obj init");

   declare
      My_Ext : F760A00.Child.Lim_Ext := (F760A00.Lim_Tagged with
                                         G => 10, TC2 => <>);
   begin
      F760A00.Child.Check_Init (My_Ext, "Lim_Ext obj init");
      F760A00.Child.Use_It (My_Ext);
   end;
   F760A00.Child.Check_Fin_Lim_Ext ("Lim_Ext obj init");

   -- Defaults for components:
   declare
      type A_Rec is record
         C : F760A00.Lim_Rec := (A => Report.Ident_Int(12),
                                 B => True, TC => <>);
         N : Natural := 0;
      end record;
      My_Rec : A_Rec;
   begin
      F760A00.Check_Init (My_Rec.C, "Lim_Rec component def init");
      F760A00.Use_It (My_Rec.C);
   end;
   F760A00.Check_Fin_Lim_Rec ("Lim_Rec component def finalization");

   declare
      type A_Rec is record
         C : F760A00.Child.Lim_Ext := (F760A00.Lim_Tagged with
                                       G => 20, TC2 => <>);
         D : Character := 'A';
      end record;
      My_Rec : A_Rec;
   begin
      F760A00.Child.Check_Init (My_Rec.C, "Lim_Ext component def init");
      F760A00.Child.Use_It (My_Rec.C);
   end;
   F760A00.Child.Check_Fin_Lim_Ext ("Lim_Ext component def finalization");

   declare
      type A_Rec is record
         C : F760A00.Lim_Comp := (N => Report.Ident_Int(3), others => <>);
         A : Natural := 10;
      end record;
      My_Rec : A_Rec;
   begin
      F760A00.Check_Init (My_Rec.C, "Lim_Comp component def init");
      F760A00.Use_It (My_Rec.C);
   end;
   F760A00.Check_Fin_Lim_Comp ("Lim_Comp component def finalization");

   -- A record component in an aggregate:
   declare
      My_Tag : F760A00.Lim_Tagged := (R => (A => Report.Ident_Int(12),
                                            B => True, TC => <>),
                                      TC => <>);
   begin
      F760A00.Check_Init (My_Tag, "Lim_Rec aggregate init");
      F760A00.Use_It (My_Tag);
   end;
   F760A00.Check_Fin_Lim_Tagged ("Lim_Rec aggregate finalization");

   -- The expression of an extension aggregate:
   declare
      My_Ext : F760A00.Child.Lim_Ext :=
        (F760A00.Lim_Tagged'(R => (A => Report.Ident_Int(8),
                                   B => False, TC => <>),
                             TC => <>) with
         G => Report.Ident_Int(20), TC2 => <>);
   begin
      F760A00.Child.Check_Init (My_Ext, "Lim_Ext ext agg init");
      F760A00.Child.Use_It (My_Ext);
   end;
   F760A00.Child.Check_Fin_Lim_Ext ("Lim_Ext ext agg finalization");

   -- The expression of a positional array aggregate:
   declare
      My_Arr : F760A00.Lim_Array := (
        (A => Report.Ident_Int(10), B => True, TC => <>),
        (A => Report.Ident_Int(2), B => False, TC => <>));
   begin
      F760A00.Check_Init (My_Arr, "Lim_Array pos agg init");
      F760A00.Use_It (My_Arr);
   end;
   F760A00.Check_Fin_Lim_Array (Length => 2, Message => "Lim_Array pos agg init");

   -- The expression of an initialized allocator:
   declare
       type Acc_Comp is access F760A00.Lim_Comp;
       Obj : Acc_Comp := new F760A00.Lim_Comp'(N => Report.Ident_Int(4),
                                               others => <>);
   begin
      F760A00.Check_Init (Obj.all, "Lim_Comp allocator def init");
      F760A00.Use_It (Obj.all);
   end;
   F760A00.Check_Fin_Lim_Comp ("Lim_Comp allocator finalization");

   -- We'll test function returns with function calls.

   -- Generic in parameters:
   declare
      generic
         Test : in F760A00.Lim_Rec := (A => 3, B => False, TC => <>);
      procedure Check_It;

      procedure Check_It is
      begin
         F760A00.Check_Init (Test, "Lim_Rec def generic in init");
         F760A00.Use_Cnst (Test);
      end Check_It;

      procedure My_Check_It is new Check_It;
   begin
      My_Check_It;
   end;
   F760A00.Check_Fin_Lim_Rec ("Lim_Rec def generic in finalization");

   declare
      generic
         Test : in F760A00.Lim_Rec;
      procedure Check_It;

      procedure Check_It is
      begin
         F760A00.Check_Init (Test, "Lim_Rec generic in init");
         F760A00.Use_Cnst (Test);
      end Check_It;

      procedure My_Check_It is new Check_It ((A => 6, B => True, TC => <>));
   begin
      My_Check_It;
   end;
   F760A00.Check_Fin_Lim_Rec ("Lim_Rec generic in finalization");

   Report.Result;

end C760A01;
