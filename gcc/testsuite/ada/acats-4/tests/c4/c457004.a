-- C457004.A
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
-- OBJECTIVE:
--     Check that the selecting_expression of a case expression can be resolved
--     if it is an overloaded function call, of which exactly one has a discrete
--     type.
--
-- TEST DESCRIPTION:
--     The test tries an overloaded function of each kind of discrete type
--     allowed in the selecting_expression of a legal case expression.
--     (Note that a formal discrete type would also be allowed, but it would
--     not be possible to get static case choices for such a type - meaning
--     any case expression would be illegal.)
--
--     The test is based on test C540002.
--
-- CHANGE HISTORY:
--      21 May 14   RLB     Created test.
--
--!

With Report;

procedure C457004 is

   -- A more global function.
   function Value (A : Positive) return String is
   begin
      return Integer'Image(A);
   end Value;

begin
   Report.Test ("C457004", "Check that the selecting_expression of a case " &
      "expression can be resolved if it is an overloaded function call, of " &
      "which exactly one has a discrete type");

   declare
      function Zero return Float is
      begin
         return 0.0;
      end Zero;

      type Short is range -10 .. 10;
      function Zero return Short is
      begin
         return 0;
      end Zero;

      function Check return Boolean is
      begin
          return
             (case Zero is
                 when -10 .. -1 => False,
                 when 0         => True,
                 when 1 .. 10   => False);
      end Check;

   begin
      if not Check then
         Report.Failed ("Wrong case limb selected (1)");
      end if;
   end;

   declare
      type Colors is (White, Red, Green, Blue, Black);

      type Rec is record
         C : Colors;
      end record;

      function Get_Color (P : access Colors) return Colors is
      begin
         if P = null then
            return Black;
         else
            return P.all;
         end if;
      end Get_Color;

      function Get_Color (P : access Colors) return Rec is
      begin
         if P = null then
            return (C => White);
         else
            return (C => P.all);
         end if;
      end Get_Color;

      function Check return Boolean is
      begin
          return
             (case Get_Color (null) is
                when White              => False,
                when Red | Green | Blue => False,
                when Black              => True);
      end Check;
   begin
      if not Check then
         Report.Failed ("Wrong case limb selected (2)");
      end if;
   end;

   declare
      type Byte is mod 2**8;

      function Value (A : Integer) return Byte is
      begin
         return Byte(A);
      end Value;

      function Check return Boolean is
      begin
          return
             -- Note: The more global Value is visible here and has the correct
             -- parameter type, but should not be part of the final solution.
             (case Value (2) is
                when 0 | 1    => False,
                when 2        => True,
                when 3 .. 255 => False);
      end Check;

   begin
      if not Check then
         Report.Failed ("Wrong case limb selected (3)");
      end if;
   end;

   declare
      generic
         type Formal_Int is range <>;
         with function Value (A : Integer) return Formal_Int;
      procedure GenI (A : Integer);

      procedure GenI (A : Integer) is
      begin
         -- Note: The more global Value is visible here and has the correct
         -- parameter type, but should not be part of the final solution.
         if (case Value (A) is
               when 0 | 1 | 2 => True,
               when 3         => False,
               when others    => True) then
            Report.Failed ("Wrong case limb selected (4)");
         end if;
      end GenI;

      type Small is range 0 .. 10;
      function Foo (A : Integer) return Small is
      begin
         if A in 0 .. 10 then
            return Small(A);
         else
            return 10;
         end if;
      end Foo;

      procedure GenI_Test is new GenI (Small, Value => Foo);
   begin
      GenI_Test (3);
   end;

   declare
      type Nibble is mod 2**4;

      function Convert (Sec : Integer) return Duration is
      begin
         return Duration(Sec);
      end Convert;

      generic
         type Formal_Mod is mod <>;
         with function Convert (A : Integer) return Formal_Mod;
      procedure GenM (A : Integer);

      procedure GenM (A : Integer) is
      begin
         if (case Convert (A) is
               when 0 | 1 | 2 => True,
               when 3 .. 10   => True,
               when others    => False) then
            Report.Failed ("Wrong case limb selected (5)");
         end if;
      end GenM;

      function Bar (A : Integer) return Nibble is
      begin
         if A in 0 .. 15 then
            return Nibble(A);
         else
            return 15;
         end if;
      end Bar;

      procedure GenM_Test is new GenM (Nibble, Convert => Bar);
   begin
      GenM_Test (33);
   end;

   Report.Result;
end C457004;
