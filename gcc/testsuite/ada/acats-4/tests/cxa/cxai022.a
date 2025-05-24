-- CXAI022.A
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
--      Ada.Containers.Hashed_Sets.
--
-- TEST DESCRIPTION:
--      This test verifies that an implementation supports the iterator and
--      reference subprograms contained in package
--      Ada.Containers.Hashed_Sets. Each of the subprograms is
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
--
with Ada.Containers.Hashed_Sets;
with Report;
with Ada.Exceptions;

procedure CXAI022 is

   My_Default_Value : constant := 999.0;

   type My_Float is new Float
     with Default_Value => My_Default_Value;

   use type Ada.Containers.Hash_Type;

   function My_Element_Hash (Element : My_Float)
     return Ada.Containers.Hash_Type is
   begin

      return Ada.Containers.Hash_Type (Element) * 17; -- Some prime

   end My_Element_Hash;

   function My_Equivalent_Elements (Left, Right : My_Float) return Boolean
     is
   begin

      return Left = Right;

   end My_Equivalent_Elements;

   package My_Hashed_Sets is new Ada.Containers.Hashed_Sets
     (Element_Type        => My_Float,
      Hash                => My_Element_Hash,
      Equivalent_Elements => My_Equivalent_Elements); -- Default =

   type My_Key_Type is new Integer;

   function My_Key (Element : My_Float) return My_Key_Type is
   begin

      return My_Key_Type (Element);

   end My_Key;

   function My_Key_Hash (Key : My_Key_Type) return Ada.Containers.Hash_Type is
   begin

      return Ada.Containers.Hash_Type (Key) * 17; -- Some prime

   end My_Key_Hash;

   function My_Equivalent_Keys (Left, Right : My_Key_Type) return Boolean is
   begin

      return Left = Right;

   end My_Equivalent_Keys;

   package My_Keys is new My_Hashed_Sets.Generic_Keys
     (Key_Type        => My_Key_Type,
      Key             => My_Key,
      Hash            => My_Key_Hash,
      Equivalent_Keys => My_Equivalent_Keys); -- Predefined <

   My_Set_1 : My_Hashed_Sets.Set;

   Num_Tests : constant := 10;

   subtype Array_Bounds_Type is Ada.Containers.Count_Type range 1 .. Num_Tests;

   -- No fractional parts so that can compare values for equality.
   -- Values in ascending order as this is what determines the order for a set

   Value_In_Array  : constant array (Array_Bounds_Type) of My_Float :=
     (12.0, 23.0, 34.0, 45.0, 56.0, 67.0, 78.0, 89.0, 100.0, 111.0);

   My_Cursor_1 : My_Hashed_Sets.Cursor;

   procedure Tampering_Check
     (Container : in out My_Hashed_Sets.Set;
      Where     : in     String) is

      Program_Error_Raised : Boolean := False;

   begin

      declare
      begin

         -- Use a value not already in set

         Container.Insert (New_Item => My_Default_Value * 3.0);

      exception

         when Program_Error =>

            Program_Error_Raised := True;

      end;

      if not Program_Error_Raised then

         Report.Failed ("Tampering should have raised error in " & Where);

      end if;

   end Tampering_Check;

   use type Ada.Containers.Count_Type;
   use type My_Hashed_Sets.Cursor;
   use type My_Hashed_Sets.Set;


begin

   Report.Test
     ("CXAI022",
      "Check iterators, indexing, and references for package " &
      "Ada.Containers.Hashed_Sets");

   -- Test Constant_Reference and Reference_Preserving_Key explicitly.

   My_Set_1.Clear;

   My_Set_1.Insert (New_Item => Value_In_Array (1));

   declare

      My_Constant_Reference : My_Hashed_Sets.Constant_Reference_Type :=
                                My_Set_1.Constant_Reference
                                  (Position => My_Set_1.First);

   begin

      Tampering_Check
        (Container => My_Set_1,
         Where     => "Constant_Reference (cursor form)");

   end;

   declare

      My_Constant_Reference : My_Hashed_Sets.Constant_Reference_Type :=
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
               My_Hashed_Sets.Constant_Reference(My_Set_1,
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
               My_Hashed_Sets.Constant_Reference(My_Set_1,
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

      Total_In  : My_Float := 0.0;
      Total_Out : My_Float;

   begin

      My_Set_1.Clear;

      -- Set with Ada 2005-style loop

      for I in Array_Bounds_Type loop

         My_Set_1.Insert (New_Item => Value_In_Array (I));

         Total_In := Total_In + Value_In_Array (I);

      end loop;

      Total_Out := 0.0;

      for E of My_Set_1 loop

         Tampering_Check
           (Container => My_Set_1,
            Where     => "reading of loop");

         Total_Out := Total_Out + E;

      end loop;

      if not (Total_Out = Total_In) then

         Report.Failed ("Total after of loop not as expected");

      end if;

      -- No reverse form for Hashed Containers

      Total_Out := 0.0;

      for C in My_Set_1.Iterate loop

         Tampering_Check
           (Container => My_Set_1,
            Where     => "reading in loop");

         Total_Out := Total_Out + My_Set_1 (C);

      end loop;

      if not (Total_Out = Total_In) then

         Report.Failed ("Total after in loop not as expected");

      end if;

      -- No reverse or start value forms for Hashed Containers

      -- No Variable_Indexing for Sets

   end;


   Report.Result;

end CXAI022;

