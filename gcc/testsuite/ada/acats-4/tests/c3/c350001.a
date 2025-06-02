-- C350001.A
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
-- OBJECTIVES:
--     Check that the object form of the Image attribute works as defined.
--
-- TEST DESCRIPTION:
--     The object form was added to Ada by Corrigendum 1 for Ada 2012.
--
--     We use test cases similar to those found in tests C35502C, C35502E,
--     and C35503C. We try various kinds of prefixes to the attribute.
--     Since this attribute is primarily used for debugging, we assume that
--     almost any prefix will appear in actual usageand thus we don't try to
--     determine specific usage scenarios.
--
-- CHANGE HISTORY:
--     22 Mar 17   RLB     Created test.
--
--!
with Report;
procedure C350001 is

   generic
      type Disc is (<>);
      Tst_Id : String;
   procedure Tst (D1 : Disc; Expected : String);

   procedure Tst (D1 : Disc; Expected : String) is
   begin
      if D1'Image /= Expected then -- Prefix is parameter.
         Report.Failed ("Incorrect D1'Image for " & Expected & " in " &
                        Tst_Id);
      end if;
      if D1'Image'First /= 1 then
         Report.Failed ("Incorrect lower bound for " & Expected & " in " &
                        Tst_Id);
      end if;
   end Tst;

begin

   Report.Test
     ("C350001",
      "Check that the object form of the Image attribute works as defined");

   -- Enumeration cases:
   declare
      type Enum is (A, BC, ABC, A_B_C, abcd, 'd');
      type New_Enum is new Enum;

      function Ident (X : Enum) return Enum is
      begin
         if Report.Equal (Enum'Pos (X), Enum'Pos (X)) then
            return X;
         end if;
         return Enum'First;
      end Ident;

      E1 : New_Enum := New_Enum (Ident (BC));

      A1 : array (0 .. 5) of Enum := (A, BC, ABC, A_B_C, abcd, 'd');

      procedure Tst_Enum is new Tst (Enum, "Enum");
      procedure Tst_New_Enum is new Tst (New_Enum, "New_Enum");

   begin
      if Ident(ABC)'Image /= "ABC" then -- Function call prefix
         Report.Failed ("Incorrect Enum'Image for ABC" );
      end if;
      if Ident(ABC)'Image'First /= 1 then
         Report.Failed ("Incorrect lower bound for ABC in Enum");
      end if;

      if Ident(A_B_C)'Image /= "A_B_C" then
         Report.Failed ("Incorrect Enum'Image for A_B_C");
      end if;
      if Ident('d')'Image /= "'d'" then
         Report.Failed ("Incorrect Enum'Image for 'd'");
      end if;

      if New_Enum'(E1)'Image /=               -- Qualified expr. prefix
            Report.Ident_STR("BC") then
         Report.Failed ("Incorrect New_Enum'Image for BC");
      end if;
      --if Enum(E1)'Image /=
      --   Report.Ident_STR("BC") then        -- Type conversion prefix
      --   Report.Failed ("Incorrect New_Enum'Image for BC");
      --end if;
      -- Illegal by strict reading of 3.5(51.1/4), but probably not intended.
      -- Now ARG question.

      if A1(Report.Ident_Int(2))'Image /=      -- Array component prefix.
                 Report.Ident_STR("ABC") then
         Report.Failed ("Incorrect Enum'Image for ABC");
      end if;
      if A1(4)'Image /=                        -- Array component prefix.
                 Report.Ident_STR("ABCD") then
         Report.Failed ("Incorrect Enum'Image for abcd");
      end if;

      if New_Enum'Pred(E1)'Image /=            -- Attribute prefix
                 Report.Ident_STR("A") then
         Report.Failed ("Incorrect New_Enum'Image for A");
      end if;

      Tst_Enum (ABC, "ABC");
      Tst_Enum (Ident(A_B_C), "A_B_C");
      Tst_Enum (A1(5), "'d'");
      Tst_New_Enum (E1, "BC");
   end;

   -- Integer cases:
   declare
      type Tiny is range -99 .. 99;
      type Der is new Tiny;

      function Double (X : Tiny) return Tiny is
      begin
         if Report.Equal (Tiny'Pos (X), Tiny'Pos (X)) then
            return X*2;
         else
            return Tiny'Last;
         end if;
      end Double;

      V1 : Der := Der (Double (12));

      A1 : array (0 .. 3) of Tiny := (4, 12, 15, 16);

      procedure Tst_Tiny is new Tst (Tiny, "Tiny");
      procedure Tst_Int  is new Tst (Integer, "Integer");
      procedure Tst_Der is new Tst (Der, "Der");

   begin
      if Double(2 ** 5)'Image /= " 64" then
         Report.Failed ("Incorrect 'Image of '2 ** 6'" );
      end if;
      if Double(-7)'Image /= "-14" then
         Report.Failed ("Incorrect 'Image of '-14'" );
      end if;
      if Double(-7)'Image'First /= 1 then
         Report.Failed ("Incorrect lower bound for -14 in Tiny");
      end if;

      if V1'Image /= " 24" then
         Report.Failed ("Incorrect 'Image of '24'" );
      end if;
      --if Integer(V1-1)'Image /= " 23" then
      --   Report.Failed ("Incorrect 'Image of '23'" );
      --end if;
      -- Illegal by strict reading of 3.5(51.1/4), but probably not intended.
      -- Now ARG question.
      if A1(0)'Image /= " 4" then
         Report.Failed ("Incorrect 'Image of '4'" );
      end if;
      if A1(Report.Ident_Int(2))'Image /= " 15" then
         Report.Failed ("Incorrect 'Image of '15'" );
      end if;

      if Der'(V1 - 88)'Image /= "-64" then
         Report.Failed ("Incorrect 'Image of '-64'" );
      end if;

      Tst_Tiny (-99, "-99");
      Tst_Tiny ( 99, " 99");
      Tst_Int  (-666, "-666");
      Tst_Int  ( 975, " 975");
      Tst_Int  (1025, " 1025");
      Tst_Der (V1+17, " 41");
      Tst_Der ( 0, " 0");
      Tst_Der (-1, "-1");
      Tst_Der ( 1, " 1");
   end;

   Report.Result;

end C350001;
