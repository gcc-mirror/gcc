-- C760A03.A
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
--     Check that no separate anonymous object is used for a function call
--     initializing an object of an immutably limited type.
--
-- TEST DESCRIPTION:
--     We initialize various limited objects with aggregates that contain
--     a special checking component. This component records the operations
--     performed on it, in particular whether any assignment occurs.
--     This allows us to check for the visible semantic effects of
--     an anonymous object: the extra assignment operations that are required.
--
--     The calls to Use_It are intended to emulate usage of the object and
--     are designed to ensure that the objects cannot be optimized away.
--
--     The test also checks that limited aggregates in expression functions are
--     built-in-place. If test C760A01 fails, this test will also fail as
--     it uses similar aggregates.
--
--     The test also provides an existence test that expression functions can
--     successfully return limited types. And it provides an example that
--     the syntax changes of AI12-0157-1 (Ada 2012 Corrigendum 1) are
--     supported. Whew!
--
-- CHANGE HISTORY:
--      15 Mar 18   RLB   Created test from C760A01.
--
--!

with Report;
with F760A00.Child;
procedure C760A03 is


   function Func_Lim_Rec (Int : in Integer) return F760A00.Lim_Rec is
       (A => Int, B => False, TC => <>);


   function Func_Lim_Comp (Int : in Integer) return F760A00.Lim_Comp is
       (N => Int, others => <>);

   function Func_Lim_Array (Int : in Integer) return F760A00.Lim_Array is
--       (1 .. Int => (A => 10, B => True, TC => <>));
       (1 .. Report.Ident_Int(5) => (A => 10, B => True, TC => <>));

   function Func_Lim_Ext (Int : in Integer) return F760A00.Child.Lim_Ext is
      (F760A00.Lim_Tagged with G => Int, TC2 => <>);

begin
   Report.Test ("C760A03",
                    "Check that no separate anonymous object is used for a " &
                    "function call initializing an object of an immutably " &
                    "limited type.");

   F760A00.Check_TC_Copy_Check;

   -- Object declarations:
   declare
      My_Obj : F760A00.Lim_Rec := Func_Lim_Rec (Int => 1);
   begin
      F760A00.Check_Init (My_Obj, "Lim_Rec obj init");
      F760A00.Use_It (My_Obj);
   end;
   F760A00.Check_Fin_Lim_Rec ("Lim_Rec obj finalization");

   declare
      My_Prot : F760A00.Lim_Comp :=
         Func_Lim_Comp (Int => Report.Ident_Int(1));
   begin
      F760A00.Check_Init (My_Prot, "Lim_Comp obj init");
      F760A00.Use_It (My_Prot);
   end;
   F760A00.Check_Fin_Lim_Comp ("Lim_Comp obj finalization");

   declare
      My_Arr : F760A00.Lim_Array :=
         Func_Lim_Array (Int => Report.Ident_Int(5));
   begin
      F760A00.Check_Init (My_Arr, "Lim_Array obj init");
      F760A00.Use_It (My_Arr);
   end;
   F760A00.Check_Fin_Lim_Array (Length => 5,
                                Message => "Lim_Array obj finalization");

   declare
      My_Ext : F760A00.Child.Lim_Ext :=
          Func_Lim_Ext (Int => Report.Ident_Int(10));
   begin
      F760A00.Child.Check_Init (My_Ext, "Lim_Ext obj init");
      F760A00.Child.Use_It (My_Ext);
   end;
   F760A00.Child.Check_Fin_Lim_Ext ("Lim_Ext obj finalization");

   -- Defaults for components:
   declare
      type A_Rec is record
         C : F760A00.Lim_Rec :=
            Func_Lim_Rec (Int => Report.Ident_Int(12));
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
         C : F760A00.Child.Lim_Ext := Func_Lim_Ext (Int => 20);
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
         C : F760A00.Lim_Comp :=
             Func_Lim_Comp (Int => Report.Ident_Int(3));
         A : Natural := 10;
      end record;
      My_Rec : A_Rec;
   begin
      F760A00.Check_Init (My_Rec.C, "Lim_Comp component def init");
      F760A00.Use_It (My_Rec.C);
   end;
   F760A00.Check_Fin_Lim_Comp ("Lim_Comp component def finalization");

   -- An expression of an array aggregate association.
   declare
      My_Arr : F760A00.Lim_Array := (1 .. Report.Ident_Int(4) =>
                Func_Lim_Rec (Int => Report.Ident_Int(10)));
   begin
      F760A00.Check_Init (My_Arr, "Lim_Array agg init");
      F760A00.Use_It (My_Arr);
   end;
   F760A00.Check_Fin_Lim_Array (Length => 4,
                                Message => "Lim_Array agg finalization");

   -- A record component in an aggregate:
   declare
      My_Tag : F760A00.Lim_Tagged := (R => Func_Lim_Rec (Int => 12),
                                      TC => <>);
   begin
      F760A00.Check_Init (My_Tag, "Lim_Rec aggregate init");
      F760A00.Use_It (My_Tag);
   end;
   F760A00.Check_Fin_Lim_Tagged ("Lim_Rec aggregate finalization");

   -- The expression of an extension aggregate:
   declare
      My_Ext : F760A00.Child.Lim_Ext :=
        (F760A00.Lim_Tagged'(R => Func_Lim_Rec (Int => 8),
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
         Func_Lim_Rec (Int => 4),
         Func_Lim_Rec (Int => 6));
   begin
      F760A00.Check_Init (My_Arr, "Lim_Array pos agg init");
      F760A00.Use_It (My_Arr);
   end;
   F760A00.Check_Fin_Lim_Array (Length => 2,
                                Message => "Lim_Array pos agg finalization");

   -- The expression of an initialized allocator:
   declare
       type Acc_Comp is access F760A00.Lim_Comp;
       Obj : Acc_Comp := new F760A00.Lim_Comp'(
            Func_Lim_Comp (Int => Report.Ident_Int(4)));
   begin
      F760A00.Check_Init (Obj.all, "Lim_Comp allocator def init");
      F760A00.Use_It (Obj.all);
   end;
   F760A00.Check_Fin_Lim_Comp ("Lim_Comp allocator finalization");

   -- We tested function returns in the definitions of the functions used here.

   -- Generic in parameters:
   declare
      generic
         Test : in F760A00.Lim_Rec := Func_Lim_Rec (Int => 3);
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

      procedure My_Check_It is new Check_It (Func_Lim_Rec (Int => 7));
   begin
      My_Check_It;
   end;
   F760A00.Check_Fin_Lim_Rec ("Lim_Rec generic in finalization");

   Report.Result;

end C760A03;
