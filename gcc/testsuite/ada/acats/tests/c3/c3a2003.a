-- C3A2003.A
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
--      Check that, for X'Access of a general access type A, Program_Error is
--      raised if the accessibility level of X is deeper than that of A.
--      Check for the case where X denotes a view that is a dereference of an
--      access parameter, or a rename thereof. Check for the case where X is
--      an access parameter and the corresponding actual is another access
--      parameter.
--
-- TEST DESCRIPTION:
--      In order to satisfy accessibility requirements, the designated
--      object X must be at the same or a less deep nesting level than the
--      general access type A -- X must "live" as long as A. Nesting
--      levels are the run-time nestings of masters: block statements;
--      subprogram, task, and entry bodies; and accept statements. Packages
--      are invisible to accessibility rules.
--
--      This test declares subprograms with access parameters, within which
--      'Access is attempted on a dereference of an access parameter, and
--      assigned to an access object whose type A is declared at some nesting
--      level. The test verifies that Program_Error is raised if the actual
--      corresponding to the access parameter is another access parameter,
--      and the actual corresponding to this second access parameter is:
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
--      access parameter can be deeper than that of the type A -- it is
--      the run-time nesting that matters for accessibility rules. Consider
--      the case where the access type A is declared within the called
--      subprogram. The accessibility check will never fail, even if the
--      actual happens to have a deeper static nesting level:
--
--         procedure P (X: access T) is
--            type A is access all T;   -- Static level = 2, e.g.
--            Acc : A := X.all'Access;  -- Check should never fail.
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
--         Acc. Thus, X.all'Access is safe, even though the static nesting
--         level of Actual is deeper than that of A.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC   ACVC 2.0
--      15 Jul 98   EDS    Avoid optimization.
--      28 Jun 02   RLB    Added pragma Elaborate_All (Report);.
--!

with report; use report; pragma Elaborate_All (report);
package C3A2003_0 is

   type Desig is array (1 .. 10) of Integer;

   X0 : aliased Desig := (Desig'Range => Ident_Int(3));           -- Level = 0.

   type Acc_L0 is access all Desig;                               -- Level = 0.
   A0 : Acc_L0;

   type Result_Kind is (OK, P_E, O_E);

   procedure Target_Is_Level_0_Nest (Y: access Desig; S: out Result_Kind);
   procedure Never_Fails_Nest       (Y: access Desig; S: out Result_Kind);
   procedure Never_Fails_Same       (Y: access Desig; S: out Result_Kind);

end C3A2003_0;


     --==================================================================--


package body C3A2003_0 is

   procedure Target_Is_Level_0_Nest (Y: access Desig; S: out Result_Kind) is


      -- This procedure utilizes 'Access on a dereference of an access
      -- parameter, and assigned to an access object whose type A is
      -- declared at some nesting level.  Program_Error is raised if
      -- the accessibility level of the operand type is deeper than that
      -- of the target type.

      procedure Nested (X: access Desig; R: out Result_Kind) is
         -- Dereference of an access_to_object value is aliased.
         Ren : Desig renames X.all;                -- Renaming of a dereference
      begin                                        -- of an access parameter.
         -- The accessibility level of type A0 is 0.
         A0 := Ren'Access;
         R  := OK;
      exception
         when Program_Error =>
            R := P_E;
         when others        =>
            R := O_E;
      end Nested;

   begin   -- Target_Is_Level_0_Nest
      Nested (Y, S);
   end Target_Is_Level_0_Nest;

   ------------------------------------------------------------------

   procedure Never_Fails_Nest (Y: access Desig; S: out Result_Kind) is

      type Acc_Deeper is access all Desig;
      AD : Acc_Deeper;

      function Nested (X: access Desig) return Result_Kind is
      begin
         -- X.all'Access below will always be safe, since the accessibility
         -- level (although not necessarily the static nesting depth) of the
         -- type of AD will always be deeper than or the same as that of the
         -- actual corresponding to Y.
         AD := X.all'Access;
         if Ident_Int (AD(4)) /= 3 then            --Avoid Optimization of AD
           FAILED ("Initial Values not correct.");
         end if;
         return OK;
      exception
         when Program_Error =>
            return P_E;
         when others        =>
            return O_E;
      end Nested;

   begin  -- Never_Fails_Nest
      S := Nested (Y);
   end Never_Fails_Nest;

   ------------------------------------------------------------------

   procedure Called_By_Never_Fails_Same
     (X: access Desig; R: out Result_Kind) is
      type Acc_Local is access all Desig;
      AL : Acc_Local;

      -- Dereference of an access_to_object value is aliased.
      Ren : Desig renames X.all;                   -- Renaming of a dereference
   begin                                           -- of an access parameter.
      -- Ren'Access below will always be safe, since the accessibility
      -- level (although not necessarily the static nesting depth) of
      -- type of AL will always be deeper than or the same as that of the
      -- actual corresponding to Y.
      AL := Ren'Access;
      if Ident_Int (AL(4)) /= 3 then       --Avoid Optimization of AL
        FAILED ("Initial Values not correct.");
      end if;
      R  := OK;
   exception
      when Program_Error =>
         R := P_E;
      when others        =>
         R := O_E;
   end Called_By_Never_Fails_Same;

   ------------------------------------------------------------------

   procedure Never_Fails_Same (Y: access Desig; S: out Result_Kind) is
   begin
      Called_By_Never_Fails_Same (Y, S);
   end Never_Fails_Same;

end C3A2003_0;


     --==================================================================--


with C3A2003_0;
use  C3A2003_0;

with Report; use report;

procedure C3A2003 is

   type Acc_L1 is access all Desig;                               -- Level = 1.
   A1  : Acc_L1;
   X1 : aliased Desig := (Desig'Range => Ident_Int(3));
   Res : Result_Kind;


   procedure Called_By_Target_L1 (X: access Desig; R: out Result_Kind) is
   begin
      -- The accessibility level of the type of A1 is 1.
      A1 := X.all'Access;
      if IDENT_INT (A1(4)) /= 3 then       --Avoid optimization of A1
        FAILED ("Initial values not correct.");
      end if;
      R  := OK;
   exception
      when Program_Error =>
         R := P_E;
      when others        =>
         R := O_E;
   end Called_By_Target_L1;

   ------------------------------------------------------------------

   function Target_Is_Level_1_Same (Y: access Desig) return Result_Kind is
      S : Result_Kind;
   begin
      Called_By_Target_L1 (Y, S);
      return S;
   end Target_Is_Level_1_Same;

   ------------------------------------------------------------------

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

begin -- C3A2003

   Report.Test ("C3A2003", "Check that, for X'Access of general access "   &
                "type A, Program_Error is raised if the accessibility "    &
                "level of X is deeper than that of A: X is an access "     &
                "parameter; corresponding actual is another access "       &
                "parameter");


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
      X2 : aliased Desig := (Desig'Range => Ident_Int(3));
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

end C3A2003;
