-- C460002.A
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
--      Check that if the target type of a type conversion is a general
--      access type, Program_Error is raised if the accessibility level
--      of the operand type is deeper than that of the target type.
--      Check for the case where the operand is an access parameter,
--      and the actual corresponding to the access parameter is another
--      access parameter.
--
-- TEST DESCRIPTION:
--      In order to satisfy accessibility requirements, the operand type
--      must be at the same or a less deep nesting level than the target
--      type -- the operand type must "live" as long as the target type.
--      Nesting levels are the run-time nestings of masters: block statements;
--      subprogram, task, and entry bodies; and accept statements. Packages
--      are invisible to accessibility rules.
--
--      This test declares subprograms with access parameters, within which
--      a type conversion is attempted on the access parameter to an access
--      type A declared at some nesting level. The test verifies that
--      Program_Error is raised if the actual corresponding to the access
--      parameter is another access parameter, and the actual corresponding
--      to this second access parameter is:
--
--         (1) an expression of a named access type, and the accessibility
--             level of the named access type is deeper than that of the
--             access type A.
--
--         (2) a reference to the Access attribute (e.g., X'Access), and
--             the accessibility level of X is deeper than that of the
--             access type A.
--
--      Note that the static nesting level of the actual corresponding to the
--      access parameter can be deeper than that of the target type -- it is
--      the run-time nesting that matters for accessibility rules. Consider
--      the case where the access type A is declared within the called
--      subprogram. The accessibility check will never fail, even if the
--      actual happens to have a deeper static nesting level:
--
--         procedure P (X: access T) is
--            type A is access all T;   -- Static level = 2, e.g.
--            Acc : A := A(X);          -- Check should never fail.
--         begin null; end;
--         . . .
--         procedure Q (Y: access T) is
--         begin
--            P(Y);
--         end;
--         . . .
--         declare
--            Actual : aliased T;       -- Static level = 3, e.g.
--         begin
--            Q (Actual'Access);
--         end;
--
--         For the execution of Q (and hence P), the accessibility level of
--         type A will always be deeper than that of Actual, so there is no
--         danger of a dangling reference arising from the assignment to
--         Acc. Thus, the type conversion is safe, even though the static
--         nesting level of Actual is deeper than that of A.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      19 Dec 94   SAIC    Changed maintenance documentation.
--      15 Jul 98   EDS     Avoid Optimization
--      28 Jun 02   RLB     Added pragma Elaborate_All.
--!

with Report; use Report; pragma Elaborate_All (Report);
package C460002_0 is

   type Component is array (1 .. 10) of Natural;

   type Desig is record
      C: Component;
   end record;

   X0 : aliased Desig := (C=>(others => Ident_Int(3)));    -- Level = 0.

   type Acc_L0 is access all Desig;                               -- Level = 0.
   A0 : Acc_L0;

   type Result_Kind is (OK, P_E, O_E);

   procedure Target_Is_Level_0_Nest (Y: access Desig; S: out Result_Kind);
   procedure Never_Fails_Nest       (Y: access Desig; S: out Result_Kind);
   procedure Never_Fails_Same       (Y: access Desig; S: out Result_Kind);

end C460002_0;


     --==================================================================--


package body C460002_0 is

   procedure Target_Is_Level_0_Nest (Y: access Desig; S: out Result_Kind) is

      procedure Nested (X: access Desig; R: out Result_Kind) is
      -- This procedure attempts a type conversion on the access parameter to
      -- an access type declared at some nesting level.  Program_Error is
      -- raised if the accessibility level of the operand type is deeper than
      -- that of the target type.

      begin
         -- The accessibility level of type Acc_L0 is 0.
         A0 := Acc_L0(X);
         R  := OK;
      exception
         when Program_Error =>
            R := P_E;
         when others        =>
            R := O_E;
      end Nested;

   begin
      Nested (Y, S);
   end Target_Is_Level_0_Nest;

   -------------------------------------------------------------

   procedure Never_Fails_Nest (Y: access Desig; S: out Result_Kind) is

      type Acc_Deeper is access all Desig;
      AD : Acc_Deeper;

      function Nested (X: access Desig) return Result_Kind is
      begin
         -- The type conversion below will always be safe, since the
         -- accessibility level (although not necessarily the static nesting
         -- depth) of Acc_Deeper will always be deeper than or the same as that
         -- of the actual corresponding to Y.
         AD := Acc_Deeper(X);
         if Natural(Ident_Int(AD.C(1))) /= 3 then            --Avoid Optimization of AD
           Report.Failed ("Initial Values not correct.");
         end if;
         return OK;
      exception
         when Program_Error =>
            return P_E;
         when others        =>
            return O_E;
      end Nested;

   begin
      S := Nested (Y);
   end Never_Fails_Nest;

   -------------------------------------------------------------

   procedure Called_By_Never_Fails_Same
     (X: access Desig; R: out Result_Kind) is
      type Acc_Local is access all Desig;
      AL : Acc_Local;
   begin
      -- The type conversion below will always be safe, since the
      -- accessibility level (although not necessarily the static nesting
      -- depth) of Acc_Local will always be deeper than or the same as that
      -- of the actual corresponding to X.
      AL := Acc_Local(X);
      if Natural(Ident_Int(AL.C(1))) /= 3 then       --Avoid Optimization of AL
        Report.Failed ("Initial Values not correct.");
      end if;
      R  := OK;
   exception
      when Program_Error =>
         R := P_E;
      when others        =>
         R := O_E;
   end Called_By_Never_Fails_Same;

   -------------------------------------------------------------

   procedure Never_Fails_Same (Y: access Desig; S: out Result_Kind) is
   begin
      Called_By_Never_Fails_Same (Y, S);
   end Never_Fails_Same;

end C460002_0;


     --==================================================================--


with C460002_0;
use  C460002_0;

with Report; use Report;

procedure C460002 is

   type Acc_L1 is access all Desig;                               -- Level = 1.
   A1 : Acc_L1;
   X1 : aliased Desig := (C=>(others => Ident_Int(3)));
   Res : Result_Kind;



   procedure Called_By_Target_L1 (X: access Desig; R: out Result_Kind) is
   begin
      -- The accessibility level of type Acc_L1 is 1.
      A1 := Acc_L1(X);
      if Natural(Ident_Int(A1.C(1))) /= 3 then       --Avoid Optimization of A1
        Report.Failed ("Initial Values not correct.");
      end if;
      R  := OK;
   exception
      when Program_Error =>
         R := P_E;
      when others        =>
         R := O_E;
   end Called_By_Target_L1;

   -------------------------------------------------------------

   function Target_Is_Level_1_Same (Y: access Desig) return Result_Kind is
      S : Result_Kind;
   begin
      Called_By_Target_L1 (Y, S);
      return S;
   end Target_Is_Level_1_Same;

   -------------------------------------------------------------

   procedure Display_Results (Result  : in Result_Kind;
                              Expected: in Result_Kind;
                              Msg     : in String) is
   begin
      if Result /= Expected then
         case Result is
            when OK  => Report.Failed ("No exception raised: "         & Msg);
            when P_E => Report.Failed ("Program_Error raised: "        & Msg);
            when O_E => Report.Failed ("Unexpected exception raised: " & Msg);
         end case;
      end if;
   end Display_Results;

begin -- C460002.

   Report.Test ("C460002", "Check that if the target type of a type "      &
                "conversion is a general access type, Program_Error is "   &
                "raised if the accessibility level of the operand type "   &
                "is deeper than that of the target type: operand is an "   &
                "access parameter; corresponding actual is another "       &
                "access parameter");


   -- Accessibility level of actual is 0 (actual is X'Access):

   Never_Fails_Same (X0'Access, Res);
   Display_Results (Res, OK, "Never_Fails_Same, level 0 actual");

   Never_Fails_Nest (X0'Access, Res);
   Display_Results (Res, OK, "Target_L1_Nest, level 0 actual");

   Target_Is_Level_0_Nest (X0'Access, Res);
   Display_Results (Res, OK, "Target_L0_Nest, level 0 actual");

   Res := Target_Is_Level_1_Same (X0'Access);
   Display_Results (Res, OK, "Target_L1_Same, level 0 actual");


   -- Accessibility level of actual is 1 (actual is X'Access):

   Never_Fails_Same (X1'Access, Res);
   Display_Results (Res, OK, "Never_Fails_Same, level 1 actual");

   Never_Fails_Nest (X1'Access, Res);
   Display_Results (Res, OK, "Target_L1_Nest, level 1 actual");

   Target_Is_Level_0_Nest (X1'Access, Res);
   Display_Results (Res, P_E, "Target_L0_Nest, level 1 actual");

   Res := Target_Is_Level_1_Same (X1'Access);
   Display_Results (Res, OK, "Target_L1_Same, level 1 actual");


   Block_L2:
   declare
      X2 : aliased Desig := (C=>(others => Ident_Int(3)));
      type Acc_L2 is access all Desig;                            -- Level = 2.
      Expr_L2 : Acc_L2 := X2'Access;
   begin

      -- Accessibility level of actual is 2 (actual is expression of named
      -- access type):

      Never_Fails_Same (Expr_L2, Res);
      Display_Results (Res, OK, "Never_Fails_Same, level 2 actual");

      Never_Fails_Nest (Expr_L2, Res);
      Display_Results (Res, OK, "Target_L1_Nest, level 2 actual");

      Target_Is_Level_0_Nest (Expr_L2, Res);
      Display_Results (Res, P_E, "Target_L0_Nest, level 2 actual");

      Res := Target_Is_Level_1_Same (Expr_L2);
      Display_Results (Res, P_E, "Target_L1_Same, level 2 actual");

   end Block_L2;


   Report.Result;

end C460002;
