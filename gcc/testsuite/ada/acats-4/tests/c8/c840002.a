-- C840002.A
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
--
--
-- OBJECTIVE:
--      Check that the construct "use all type" is correctly implemented.
--
-- TEST DESCRIPTION:
--      This Ada 2012 test tests that "use all type" has the correct
--      effect on name resolution.

--
-- CHANGE HISTORY:
--      27 Sep 13   SWB   Initial version.
--      17 Oct 13   RLB   Corrected header, corrected syntax error as requested,
--                        renamed test as the original name is that of an existing
--                        ACATS test.
--      23 Mar 14   SWB   Corrected test errors.
--      24 Mar 14   SWB   Changes for ACATS 4.0: test name changed.
--!

with Report;
use Report;

procedure C840002 is
begin
   Report.Test ("C840002", "Check that 'use all type' is correctly implemented");

   declare
      package Pkg_1 is
         type Enum is (Aaa, Bbb, Ccc);
         procedure Prim_Proc (X : Enum := Aaa);
         function Prim_Func return Enum;

         type Other_Enum is (Bbb, Ccc, Ddd);
         procedure Prim_Proc (X : Other_Enum := Ddd);
         function Prim_Func return Other_Enum;
      end Pkg_1;

      package body Pkg_1 is
         procedure Prim_Proc (X : Enum := Aaa) is null;
         function Prim_Func return Enum is (Bbb);

         procedure Prim_Proc (X : Other_Enum := Ddd) is null;
         function Prim_Func return Other_Enum is (Ccc);
      end Pkg_1;

      package Pkg_2 is
         type Derived_Enum is new Pkg_1.Enum;
      end Pkg_2;

   begin
      declare
         use all type Pkg_1.Enum;
         use type Pkg_2.Derived_Enum;
      begin
         if Prim_Func /= Bbb then
            Report.Failed ("Enum case");
         end if;
         Prim_Proc;
         Prim_Proc (Aaa);
      end;

      declare
         use type Pkg_1.Enum;
         use all type Pkg_2.Derived_Enum;
      begin
         if Prim_Func /= Bbb then
            Report.Failed ("Derived Enum case");
         end if;
         Prim_Proc;
         Prim_Proc (Aaa);
      end;
   end;

   declare
      package T1_Pkg is
         type T1 is tagged null record;
         procedure Prim_Proc (X : T1);
         function  Prim_Func return T1;
         T1_Var : T1;
         procedure Cw_Operand (X : T1'Class := T1_Var);
      end T1_Pkg;

      package T2_Pkg is
         type T2 is new T1_Pkg.T1 with null record;
         T2_Var : T2;
         procedure Cw_Operand (X : T2'Class := T2_Var);
      end T2_Pkg;

      package T3_Pkg is
         type T3 is new T2_Pkg.T2 with null record;
         T3_Var : T3;
         procedure Cw_Operand (X : T3'Class := T3_Var);
      end T3_Pkg;

      package body T1_Pkg is
         procedure Prim_Proc (X : T1) is null;
         function  Prim_Func return T1 is (T1_Var);
         procedure Cw_Operand (X : T1'Class := T1_Var) is null;
      end T1_Pkg;

      package body T2_Pkg is
         procedure Cw_Operand (X : T2'Class := T2_Var) is null;
      end T2_Pkg;

      package body T3_Pkg is
         procedure Cw_Operand (X : T3'Class := T3_Var) is null;
      end T3_Pkg;

      package Late_Resolver is
         procedure Proc
            with Pre => (Prim_Func = Prim_Func);

         use type T1_Pkg.T1;
         use all type T2_Pkg.T2;
         use type T3_Pkg.T3;
      end Late_Resolver;

      package body Late_Resolver is
         procedure Proc is null;
      end Late_Resolver;

      use type T1_Pkg.T1;
      use all type T2_Pkg.T2;
      use type T3_Pkg.T3;

      procedure Ren1 (X : T1_Pkg.T1'Class) renames Cw_Operand;
      -- preceding "use all type" makes T1_Pkg.Cw_Operand use-visible
      -- because T1 is an ancestor of T2.

      procedure Ren2 (X : T2_Pkg.T2'Class) renames Cw_Operand;
   begin
      if Prim_Func /= Prim_Func then
         Report.Failed ("Tagged");
      end if;
      Prim_Proc (T2_Pkg.T2'Class (T3_Pkg.T3_Var));
      Late_Resolver.Proc;
   end;

   Report.Result;
end C840002;
