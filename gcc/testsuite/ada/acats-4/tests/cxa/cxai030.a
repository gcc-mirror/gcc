-- CXAI030.A
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
--      Ada.Containers.Bounded_Ordered_Sets.
--
-- TEST DESCRIPTION:
--      This test verifies that an implementation supports the iterator and
--      reference subprograms contained in package
--      Ada.Containers.Bounded_Ordered_Sets. Each of the subprograms is
--      exercised in a general sense, to ensure that
--      it is available, and that it provides the expected results in a known
--      test environment.
--
-- CHANGE HISTORY:
--      23 Sep 13   JAC     Initial pre-release version.
--       6 Dec 13   JAC     Second pre-release version.
--      29 Dec 13   JAC     Third pre-release version.
--      31 Jan 14   JAC     Fourth pre-release version.
--      12 Feb 14   JAC     Fifth pre-release version.
--       3 Apr 14   RLB     Created separate test from part of submitted test.
--                          Added additional tests of indexing and references.
--      14 Apr 14   RLB     Changed early exit failed message to be unique.
--
--!
with Ada.Containers.Bounded_Ordered_Sets;
with Report;
with Ada.Exceptions;

procedure CXAI030 is

   My_Default_Value : constant := 999.0;

   type My_Float is new Float
     with Default_Value => My_Default_Value;

   package My_Bounded_Ordered_Sets is new
     Ada.Containers.Bounded_Ordered_Sets
     (Element_Type => My_Float); -- Default < and Default =

   type My_Key_Type is new Integer;

   function My_Key (Element : My_Float) return My_Key_Type is
   begin

      return My_Key_Type (Element);

   end My_Key;

   package My_Keys is new My_Bounded_Ordered_Sets.Generic_Keys
     (Key_Type => My_Key_Type,
      Key      => My_Key); -- Predefined <

   Num_Tests : constant := 10;

   Capacity_Reqd : constant := Num_Tests + 1;

   My_Set_1 : My_Bounded_Ordered_Sets.Set (Capacity => Capacity_Reqd);

   subtype Array_Bounds_Type is Ada.Containers.Count_Type range 1 .. Num_Tests;

   -- No fractional parts so that can compare values for equality.
   -- Values in ascending order as this is what determines the order for a set
   -- and so that can check that iterators have proceeded in the correct order.

   Value_In_Array  : constant array (Array_Bounds_Type) of My_Float :=
     (12.0, 23.0, 34.0, 45.0, 56.0, 67.0, 78.0, 89.0, 100.0, 111.0);

   My_Cursor_1 : My_Bounded_Ordered_Sets.Cursor;

   procedure Tampering_Check
     (Container : in out My_Bounded_Ordered_Sets.Set;
      Where     : in     String)
     with Pre => not Container.Is_Empty is

      Program_Error_Raised : Boolean := False;

   begin

      declare
      begin

         -- Don't try to insert into a full bounded container as it's a grey
         -- area as to whether Capacity_Error or Program_Error should be raised

         Container.Delete (Item => Value_In_Array (1));

      exception

         when Program_Error =>

            Program_Error_Raised := True;

      end;

      if not Program_Error_Raised then

         Report.Failed ("Tampering should have raised error in " & Where);

      end if;

   end Tampering_Check;

   use type Ada.Containers.Count_Type;
   use type My_Bounded_Ordered_Sets.Cursor;
   use type My_Bounded_Ordered_Sets.Set;


begin

   Report.Test
     ("CXAI030",
      "Check iterators, indexing, and references for package " &
      "Ada.Containers.Bounded_Ordered_Sets");

   -- Test Constant_Reference and Reference_Preserving_Key explicitly.

   My_Set_1.Clear;

   My_Set_1.Insert (New_Item => Value_In_Array (1));

   declare

      My_Constant_Reference : My_Bounded_Ordered_Sets.Constant_Reference_Type :=
                                My_Set_1.Constant_Reference
                                  (Position => My_Set_1.First);

   begin

      Tampering_Check
        (Container => My_Set_1,
         Where     => "Constant_Reference (cursor form)");

   end;

   declare

      My_Constant_Reference : My_Bounded_Ordered_Sets.Constant_Reference_Type :=
                                My_Keys.Constant_Reference
                                  (Container => My_Set_1,
                                   Key       => My_Key_Type(Value_In_Array(1)));

   begin

      Tampering_Check
        (Container => My_Set_1,
         Where     => "Constant_Reference (key form)");

   end;

   begin
      declare

          My_Reference : My_Keys.Reference_Type :=
                                My_Keys.Reference_Preserving_Key
                                  (Container => My_Set_1,
                                   Position  => My_Set_1.First);

      begin

         Tampering_Check
           (Container => My_Set_1,
            Where     => "Reference_Preserving_Key (cursor form)");
      end;

   exception
      when Err:others =>
         Report.Failed
            ("Unexpected exception in " &
               "Reference_Preserving_Key (cursor form)");
            Report.Comment
              ("Raised " & Ada.Exceptions.Exception_Information(Err));
   end;

   begin
      declare

         My_Reference : My_Keys.Reference_Type :=
                                My_Keys.Reference_Preserving_Key
                                  (Container => My_Set_1,
                                   Key       => My_Key_Type(Value_In_Array(1)));

      begin

         Tampering_Check
           (Container => My_Set_1,
            Where     => "Reference_Preserving_Key (key form)");
      end;

   exception
      when Err:others =>
         Report.Failed
            ("Unexpected exception in " &
               "Reference_Preserving_Key (key form)");
            Report.Comment
              ("Raised " & Ada.Exceptions.Exception_Information(Err));
   end;


   -- Test Constant_Reference and Reference_Preserving_Key explicitly using
   -- a generalized reference and implicitly via Constant_Indexing (no
   -- Variable_Indexing here) -- in a way that corresponds to likely usage.

   declare
      procedure Test (Value    : in My_Float;
                      Expected : in My_Float;
                      Test_Case: in String) is
      begin
         Tampering_Check
           (Container => My_Set_1,
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
           (Container => My_Set_1,
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
      My_Cursor_1 := My_Set_1.First;
      -- Normal call:
      Test (Value    =>
               My_Keys.Constant_Reference(My_Set_1,
                   Key => My_Key_Type(Value_In_Array (1))).Element.all,
            Expected => Value_In_Array (1),
            Test_Case=> "Constant_Reference normal key");

      Test_and_Mod (
            Value    =>
               My_Keys.Reference_Preserving_Key(My_Set_1,
                   Key => My_Key_Type(Value_In_Array (1))).Element.all,
            Expected => Value_In_Array (1),
            New_Item => Value_In_Array (1)+0.125, -- Can't change Key.
            Test_Case=> "Reference normal key");

      Test (Value    =>
               My_Bounded_Ordered_Sets.Constant_Reference(My_Set_1,
                   Position => My_Cursor_1).Element.all,
            Expected => Value_In_Array (1)+0.125,
            Test_Case=> "Constant_Reference normal cursor");

      Test_and_Mod (
            Value    =>
               My_Keys.Reference_Preserving_Key(My_Set_1,
                   Position => My_Cursor_1).Element.all,
            Expected => Value_In_Array (1)+0.125,
            New_Item => Value_In_Array (1),
            Test_Case=> "Reference normal cursor");

      -- Normal call using generalized reference:
      Test (Value    =>
               My_Keys.Constant_Reference(My_Set_1,
                   Key => My_Key_Type(Value_In_Array (1))),
            Expected => Value_In_Array (1),
            Test_Case=> "Constant_Reference normal generalized key");

      Test_and_Mod (
            Value    =>
               My_Keys.Reference_Preserving_Key(My_Set_1,
                   Key => My_Key_Type(Value_In_Array (1))),
            Expected => Value_In_Array (1),
            New_Item => Value_In_Array (1)+0.25, -- Can't change Key.
            Test_Case=> "Reference normal generalized key");

      Test (Value    =>
               My_Bounded_Ordered_Sets.Constant_Reference(My_Set_1,
                   Position => My_Cursor_1),
            Expected => Value_In_Array (1)+0.25,
            Test_Case=> "Constant_Reference normal generalized cursor");

      Test_and_Mod (
            Value    =>
               My_Keys.Reference_Preserving_Key(My_Set_1,
                   Position => My_Cursor_1),
            Expected => Value_In_Array (1)+0.25,
            New_Item => Value_In_Array (1),
            Test_Case=> "Reference normal generalized cursor");

      -- Prefix call with all components explicit (only possible for
      -- cursor form of Constant_Reference):
      Test (Value    =>
               My_Set_1.Constant_Reference(
                   Position => My_Cursor_1).Element.all,
            Expected => Value_In_Array (1),
            Test_Case=> "Constant_Reference prefix cursor");

      -- Prefix call using a generalized reference (implicit dereference):
      Test (Value    => My_Set_1.Constant_Reference(Position => My_Cursor_1),
            Expected => Value_In_Array (1),
            Test_Case=> "Constant_Reference generalized cursor");

      -- Object indexing, everything implicit.
      Test (Value    => My_Set_1(My_Cursor_1), -- Constant_Indexing
            Expected => Value_In_Array (1),
            Test_Case=> "Constant object indexing by cursor");

      -- Check what happens when the key is changed:
      begin
         Test_and_Mod (
               Value    =>
                  My_Keys.Reference_Preserving_Key(My_Set_1,
                      Key => My_Key_Type(Value_In_Array (1))).Element.all,
               Expected => Value_In_Array (1),
               New_Item => Value_In_Array (3), -- Changes Key.
               Test_Case=> "Reference changed key");
         Report.Failed ("No exception raised when key changed for " &
                        "Reference_Preserving_Key (key)");
      exception
         when Program_Error =>
            Report.Comment ("Reference_Preserving_Key detected key change");
            if not My_Set_1.Is_Empty then
               Report.Failed ("Changed element not removed by " &
                              "Reference_Preserving_Key");
            end if;
         when Err:others =>
            Report.Failed
              ("Wrong exception raised when key change in " &
               "Reference_Preserving_Key");
            Report.Comment
              ("Raised " & Ada.Exceptions.Exception_Information(Err));
      end;
   exception
      when Crash:others =>
         Report.Failed
           ("Unexpected exception raised in " &
            "Constant_Reference/Reference_Preserving_Key block");
         Report.Comment
           ("Raised " & Ada.Exceptions.Exception_Information(Crash));
   end;


   -- Test Ada 2012 Iterate (2 forms) and (implicitly) Constant_Reference

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

      My_Set_1.Clear;

      -- Set with Ada 2005-style loop

      for I in Array_Bounds_Type loop

         My_Set_1.Insert (New_Item => Value_In_Array (I));

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

      for E of My_Set_1 loop

         Tampering_Check
           (Container => My_Set_1,
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

      for E of reverse My_Set_1 loop

         Tampering_Check
           (Container => My_Set_1,
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

      for C in My_Set_1.Iterate loop

         Tampering_Check
           (Container => My_Set_1,
            Where     => "reading forward in loop");

         Total_Out := Total_Out + My_Set_1 (C);

         if not (My_Set_1 (C) > Previous_Value) then

            Report.Failed ("Forward in loop not in ascending order");

         end if;

         Previous_Value := My_Set_1 (C);

      end loop;

      if not (Total_Out = Total_In) then

         Report.Failed ("Total after forward in loop not as expected");

      end if;

      Previous_Value := Value_In_Array (Num_Tests) + 1.0;
      Total_Out      := 0.0;

      for C in reverse My_Set_1.Iterate loop

         Tampering_Check
           (Container => My_Set_1,
            Where     => "reading reverse in loop");

         Total_Out := Total_Out + My_Set_1 (C);

         if not (My_Set_1 (C) < Previous_Value) then

            Report.Failed ("Reverse in loop not in descending order");

         end if;

         Previous_Value := My_Set_1 (C);

      end loop;

      if not (Total_Out = Total_In) then

         Report.Failed ("Total after reverse in loop not as expected");

      end if;

      My_Cursor_1 := My_Keys.Find
        (Container => My_Set_1,
         Key       => My_Key_Type (Value_In_Array (Num_Tests / 2)));

      Previous_Value  := Value_In_Array (Num_Tests / 2) - 1.0;
      Upper_Total_Out := 0.0;

      for C in My_Set_1.Iterate (My_Cursor_1) loop

         Tampering_Check
           (Container => My_Set_1,
            Where     => "reading partial forward in loop");

         Upper_Total_Out := Upper_Total_Out + My_Set_1 (C);

         if not (My_Set_1 (C) > Previous_Value) then

            Report.Failed ("Partial forward in loop not in ascending order");

         end if;

         Previous_Value := My_Set_1 (C);

      end loop;

      if not (Upper_Total_Out = Upper_Total_In) then

         Report.Failed ("Total after partial forward in loop not as expected");

      end if;

      My_Cursor_1 := My_Keys.Find
        (Container => My_Set_1,
         Key       => My_Key_Type (Value_In_Array (Num_Tests / 2)));

      Previous_Value  := Value_In_Array (Num_Tests / 2) + 1.0;
      Lower_Total_Out := 0.0;

      for C in reverse My_Set_1.Iterate (My_Cursor_1) loop

         Tampering_Check
           (Container => My_Set_1,
            Where     => "reading partial reverse in loop");

         Lower_Total_Out := Lower_Total_Out + My_Set_1 (C);

         if not (My_Set_1 (C) < Previous_Value) then

            Report.Failed ("Partial reverse in loop not in descending order");

         end if;

         Previous_Value := My_Set_1 (C);

      end loop;

      if not (Lower_Total_Out = Lower_Total_In) then

         Report.Failed ("Total after partial reverse in loop not as expected");

      end if;

      -- Check early exit loop as described in AARM A.18.9(113.a-c/3):

      My_Cursor_1 := My_Set_1.Find (Item => Value_In_Array (3));

      Previous_Value  := Value_In_Array (1) - 1.0;
      Early_Total_In := 0.0;

      for C in My_Set_1.Iterate loop

         Early_Total_In := Early_Total_In + My_Set_1 (C);

         if not (My_Set_1 (C) > Previous_Value) then

            Report.Failed ("Early exit forward in loop not in ascending order");

         end if;

         Previous_Value := My_Set_1 (C);

         exit when C = My_Cursor_1;

      end loop;

      if not (Early_Total_In = (Value_In_Array(1) + Value_In_Array(2) +
                                Value_In_Array(3))) then

         Report.Failed ("Total after early exit in loop not as expected");

      end if;

      -- No Variable_Indexing for Sets

   end;


   Report.Result;

end CXAI030;
