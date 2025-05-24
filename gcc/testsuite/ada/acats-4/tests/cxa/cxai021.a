-- CXAI021.A
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
--      Check iterators, indexing, and references for package
--      Ada.Containers.Ordered_Maps.
--
-- TEST DESCRIPTION:
--      This test verifies that an implementation supports the iterator and
--      reference subprograms contained in package
--      Ada.Containers.Ordered_Maps. Each of the subprograms is
--      exercised in a general sense, to ensure that
--      it is available, and that it provides the expected results in a known
--      test environment.
--
-- CHANGE HISTORY:
--      23 Sep 13   JAC     Initial pre-release version.
--       6 Dec 13   JAC     Second pre-release version.
--      30 Dec 13   JAC     Third pre-release version.
--      31 Jan 14   JAC     Fourth pre-release version.
--      02 Apr 14   RLB     Created separate test from part of submitted test.
--                          Added additional tests of indexing and references.
--      14 Apr 14   RLB     Changed early exit failed message to be unique.
--
--!
with Ada.Containers.Ordered_Maps;
with Report;

procedure CXAI021 is

   My_Default_Value : constant := 999.0;

   type My_Float is new Float
     with Default_Value => My_Default_Value;

   type My_Key_Type is new Integer;

   package My_Ordered_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => My_Key_Type,
      Element_Type => My_Float); -- Default < and Default =

   My_Map_1 : My_Ordered_Maps.Map;

   Num_Tests : constant := 10;

   subtype Array_Bounds_Type is Ada.Containers.Count_Type range 1 .. Num_Tests;

   -- No fractional parts so that can compare values for equality.
   -- In ascending order so that can check that iterators have proceeded in the
   -- correct order

   Value_In_Array  : constant array (Array_Bounds_Type) of My_Float :=
     (12.0, 23.0, 34.0, 45.0, 56.0, 67.0, 78.0, 89.0, 100.0, 111.0);

   My_Cursor_1 : My_Ordered_Maps.Cursor;
   My_Cursor_2 : My_Ordered_Maps.Cursor;

   My_Inserted : Boolean;

   procedure Tampering_Check
     (Container : in out My_Ordered_Maps.Map;
      Where     : in     String) is

      Program_Error_Raised : Boolean := False;

   begin

      declare
      begin

         -- Use a key not already in map

         Container.Insert
           (Key      => Num_Tests + 1,
            New_Item => Value_In_Array (1));

      exception

         when Program_Error =>

            Program_Error_Raised := True;

      end;

      if not Program_Error_Raised then

         Report.Failed ("Tampering should have raised error in " & Where);

      end if;

   end Tampering_Check;

   use type Ada.Containers.Count_Type;
   use type My_Ordered_Maps.Cursor;
   use type My_Ordered_Maps.Map;


begin

   Report.Test
     ("CXAI021",
      "Check iterators, indexing, and references for package " &
      "Ada.Containers.Ordered_Maps");

   -- Test Constant_Reference and Reference (2 forms each) explicitly.

   My_Map_1.Clear;

   My_Map_1.Insert
     (Key      => My_Key_Type (1),
      New_Item => Value_In_Array (1));

   declare

      My_Constant_Reference : My_Ordered_Maps.Constant_Reference_Type :=
                                My_Map_1.Constant_Reference
                                  (Key => My_Key_Type (1));

   begin

      Tampering_Check
        (Container => My_Map_1,
         Where     => "Constant_Reference (key form)");

   end;

   declare

      My_Reference : My_Ordered_Maps.Reference_Type :=
                       My_Map_1.Reference (Key => My_Key_Type (1));

   begin

      Tampering_Check
        (Container => My_Map_1,
         Where     => "Reference (key form)");

   end;

   declare

      My_Constant_Reference : My_Ordered_Maps.Constant_Reference_Type :=
                                My_Map_1.Constant_Reference
                                  (Position => My_Map_1.First);

   begin

      Tampering_Check
        (Container => My_Map_1,
         Where     => "Constant_Reference (cursor form)");

   end;

   declare

      My_Reference : My_Ordered_Maps.Reference_Type :=
                       My_Map_1.Reference (Position => My_Map_1.First);

   begin

      Tampering_Check
        (Container => My_Map_1,
         Where     => "Reference (cursor form)");

   end;


   -- Test Constant_Reference and Reference explicitly using
   -- a generalized reference and implicitly via Constant_Indexing and
   -- Variable_Indexing -- in a way that corresponds to likely usage.

   declare
      procedure Test (Value    : in My_Float;
                      Expected : in My_Float;
                      Test_Case: in String) is
      begin
         Tampering_Check
           (Container => My_Map_1,
            Where     => Test_Case);
            -- The tampering check here prevents the
            -- Value parameter to this subprogram from
            -- disappearing (if passed by-reference) while
            -- it is still in use.

         if Value /= Expected then
            Report.Failed ("Wrong value for " & Test_Case);
         end if;
      end Test;

      procedure Test_and_Mod (Value    : in out My_Float;
                              Expected : in     My_Float;
                              New_Item : in     My_Float;
                              Test_Case: in     String) is
      begin
         Tampering_Check
           (Container => My_Map_1,
            Where     => Test_Case);
            -- The tampering check here prevents the
            -- Value parameter to this subprogram from
            -- disappearing (if passed by-reference) while
            -- it is still in use.

         if Value /= Expected then
            Report.Failed ("Wrong value for " & Test_Case);
         end if;
         Value := New_Item;
      end Test_and_Mod;

   begin
      My_Cursor_1 := My_Map_1.First;
      -- Normal call:
      Test (Value    =>
               My_Ordered_Maps.Constant_Reference(My_Map_1,
                   Key => My_Key_Type(1)).Element.all,
            Expected => Value_In_Array (1),
            Test_Case=> "Constant_Reference normal key");

      Test_and_Mod (
            Value    =>
               My_Ordered_Maps.Reference(My_Map_1,
                   Key => My_Key_Type(1)).Element.all,
            Expected => Value_In_Array (1),
            New_Item => Value_In_Array (2),
            Test_Case=> "Reference normal key");

      Test (Value    =>
               My_Ordered_Maps.Constant_Reference(My_Map_1,
                   Position => My_Cursor_1).Element.all,
            Expected => Value_In_Array (2),
            Test_Case=> "Constant_Reference normal cursor");

      Test_and_Mod (
            Value    =>
               My_Ordered_Maps.Reference(My_Map_1,
                   Position => My_Cursor_1).Element.all,
            Expected => Value_In_Array (2),
            New_Item => Value_In_Array (3),
            Test_Case=> "Reference normal cursor");

      -- Prefix call with all components explicit:
      Test (Value    =>
               My_Map_1.Constant_Reference(
                   Key => My_Key_Type(1)).Element.all,
            Expected => Value_In_Array (3),
            Test_Case=> "Constant_Reference prefix key");

      Test_and_Mod (
            Value    =>
               My_Map_1.Reference(Key => My_Key_Type(1)).Element.all,
            Expected => Value_In_Array (3),
            New_Item => Value_In_Array (4),
            Test_Case=> "Reference prefix key");

      Test (Value    =>
               My_Map_1.Constant_Reference(
                   Position => My_Cursor_1).Element.all,
            Expected => Value_In_Array (4),
            Test_Case=> "Constant_Reference prefix cursor");

      Test_and_Mod (
            Value    =>
               My_Map_1.Reference(Position => My_Cursor_1).Element.all,
            Expected => Value_In_Array (4),
            New_Item => Value_In_Array (5),
            Test_Case=> "Reference prefix cursor");

      -- Prefix call using a generalized reference (implicit dereference):
      Test (Value    => My_Map_1.Constant_Reference(Key => My_Key_Type(1)),
            Expected => Value_In_Array (5),
            Test_Case=> "Constant_Reference generalized key");

      Test_and_Mod (
            Value    => My_Map_1.Reference(Key => My_Key_Type(1)),
            Expected => Value_In_Array (5),
            New_Item => Value_In_Array (6),
            Test_Case=> "Reference generalized key");

      Test (Value    => My_Map_1.Constant_Reference(Position => My_Cursor_1),
            Expected => Value_In_Array (6),
            Test_Case=> "Constant_Reference generalized cursor");

      Test_and_Mod (
            Value    => My_Map_1.Reference(Position => My_Cursor_1),
            Expected => Value_In_Array (6),
            New_Item => Value_In_Array (7),
            Test_Case=> "Reference generalized cursor");

      -- Object indexing, everything implicit.
      Test (Value    => My_Map_1(My_Key_Type(1)), -- Constant_Indexing
            Expected => Value_In_Array (7),
            Test_Case=> "Constant object indexing by key");

      Test_and_Mod (
            Value    => My_Map_1(My_Key_Type(1)), -- Variable_Indexing
            Expected => Value_In_Array (7),
            New_Item => Value_In_Array (8),
            Test_Case=> "Object indexing by key");

      Test (Value    => My_Map_1(My_Cursor_1), -- Constant_Indexing
            Expected => Value_In_Array (8),
            Test_Case=> "Constant object indexing by cursor");

      Test_and_Mod (
            Value    => My_Map_1(My_Cursor_1), -- Variable_Indexing
            Expected => Value_In_Array (8),
            New_Item => Value_In_Array (1),
            Test_Case=> "Object indexing by cursor");

   end;


   -- Test Ada 2012 Iterate (2 forms) and (implicitly) Constant_Reference and
   -- Reference.

   declare

      Lower_Total_In  : My_Float := 0.0;
      Lower_Total_Out : My_Float;
      Previous_Value  : My_Float;
      Total_In        : My_Float := 0.0;
      Total_Out       : My_Float;
      Upper_Total_In  : My_Float := 0.0;
      Upper_Total_Out : My_Float;
      Early_Total_In  : My_Float;

   begin

      My_Map_1.Clear;

      -- Set with Ada 2005-style loop

      for I in Array_Bounds_Type loop

         My_Map_1.Insert
           (Key      => My_Key_Type (I),
            New_Item => Value_In_Array (I));

         Total_In := Total_In + Value_In_Array (I);

         if I <= Num_Tests / 2 then

            Lower_Total_In := Lower_Total_In + Value_In_Array (I);

         end if;

         if I >= Num_Tests / 2 then

            Upper_Total_In := Upper_Total_In + Value_In_Array (I);

         end if;

      end loop;

      Previous_Value := Value_In_Array (1) - 1.0;
      Total_Out      := 0.0;

      for E of My_Map_1 loop

         Tampering_Check
           (Container => My_Map_1,
            Where     => "reading forward of loop");

         Total_Out := Total_Out + E;

         if not (E > Previous_Value) then

            Report.Failed ("Forward of loop not in ascending order");

         end if;

         Previous_Value := E;

      end loop;

      if not (Total_Out = Total_In) then

         Report.Failed ("Total after forward of loop not as expected");

      end if;

      Previous_Value := Value_In_Array (Num_Tests) + 1.0;
      Total_Out      := 0.0;

      for E of reverse My_Map_1 loop

         Tampering_Check
           (Container => My_Map_1,
            Where     => "reading reverse of loop");

         Total_Out := Total_Out + E;

         if not (E < Previous_Value) then

            Report.Failed ("Reverse of loop not in descending order");

         end if;

         Previous_Value := E;

      end loop;

      if not (Total_Out = Total_In) then

         Report.Failed ("Total after reverse of loop not as expected");

      end if;

      Previous_Value := Value_In_Array (1) - 1.0;
      Total_Out      := 0.0;

      for C in My_Map_1.Iterate loop

         Tampering_Check
           (Container => My_Map_1,
            Where     => "reading forward in loop");

         Total_Out := Total_Out + My_Map_1 (C);

         if not (My_Map_1 (C) > Previous_Value) then

            Report.Failed ("Forward in loop not in ascending order");

         end if;

         Previous_Value := My_Map_1 (C);

      end loop;

      if not (Total_Out = Total_In) then

         Report.Failed ("Total after forward in loop not as expected");

      end if;

      Previous_Value := Value_In_Array (Num_Tests) + 1.0;
      Total_Out      := 0.0;

      for C in reverse My_Map_1.Iterate loop

         Tampering_Check
           (Container => My_Map_1,
            Where     => "reading reverse in loop");

         Total_Out := Total_Out + My_Map_1 (C);

         if not (My_Map_1 (C) < Previous_Value) then

            Report.Failed ("Reverse in loop not in descending order");

         end if;

         Previous_Value := My_Map_1 (C);

      end loop;

      if not (Total_Out = Total_In) then

         Report.Failed ("Total after reverse in loop not as expected");

      end if;

      My_Cursor_1 := My_Map_1.Find (Key => Num_Tests / 2);

      Previous_Value  := Value_In_Array (Num_Tests / 2) - 1.0;
      Upper_Total_Out := 0.0;

      for C in My_Map_1.Iterate (My_Cursor_1) loop

         Tampering_Check
           (Container => My_Map_1,
            Where     => "reading partial forward in loop");

         Upper_Total_Out := Upper_Total_Out + My_Map_1 (C);

         if not (My_Map_1 (C) > Previous_Value) then

            Report.Failed ("Partial forward in loop not in ascending order");

         end if;

         Previous_Value := My_Map_1 (C);

      end loop;

      if not (Upper_Total_Out = Upper_Total_In) then

         Report.Failed ("Total after partial forward in loop not as expected");

      end if;

      My_Cursor_1 := My_Map_1.Find (Key => Num_Tests / 2);

      Previous_Value  := Value_In_Array (Num_Tests / 2) + 1.0;
      Lower_Total_Out := 0.0;

      for C in reverse My_Map_1.Iterate (My_Cursor_1) loop

         Tampering_Check
           (Container => My_Map_1,
            Where     => "reading partial reverse in loop");

         Lower_Total_Out := Lower_Total_Out + My_Map_1 (C);

         if not (My_Map_1 (C) < Previous_Value) then

            Report.Failed ("Partial reverse in loop not in descending order");

         end if;

         Previous_Value := My_Map_1 (C);

      end loop;

      if not (Lower_Total_Out = Lower_Total_In) then

         Report.Failed ("Total after partial reverse in loop not as expected");

      end if;

      -- Check early exit loop as described in AARM A.18.6(94.a-c/3):

      My_Cursor_1 := My_Map_1.Find (Key => My_Key_Type (3));

      Previous_Value  := Value_In_Array (1) - 1.0;
      Early_Total_In := 0.0;

      for C in My_Map_1.Iterate loop

         Early_Total_In := Early_Total_In + My_Map_1 (C);

         if not (My_Map_1 (C) > Previous_Value) then

            Report.Failed ("Early exit forward in loop not in ascending order");

         end if;

         Previous_Value := My_Map_1 (C);

         exit when C = My_Cursor_1;

      end loop;

      if not (Early_Total_In = (Value_In_Array(1) + Value_In_Array(2) +
                                Value_In_Array(3))) then

         Report.Failed ("Total after early exit in loop not as expected");

      end if;


      -- Try Variable_Indexing as a change from Constant_Indexing

      for E of My_Map_1 loop

         Tampering_Check
           (Container => My_Map_1,
            Where     => "writing of loop");

         E := 0.0;

      end loop;

      -- Check with Ada 2005-style loop

      My_Cursor_1 := My_Map_1.First;

      for I in Array_Bounds_Type loop

         if My_Ordered_Maps.Element (Position => My_Cursor_1) /= 0.0 then

            Report.Failed ("Data set by of loop not as expected");

         end if;

         My_Ordered_Maps.Next (Position => My_Cursor_1);

      end loop;

   end;


   Report.Result;

end CXAI021;

