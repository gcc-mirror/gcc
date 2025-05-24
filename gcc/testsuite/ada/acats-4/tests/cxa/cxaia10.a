-- CXAIA10.A
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
--      Ada.Containers.Indefinite_Hashed_Maps.
--
-- TEST DESCRIPTION:
--      This test verifies that an implementation supports the iterator and
--      reference subprograms contained in package
--      Ada.Containers.Indefinite_Hashed_Maps. Each of the subprograms
--      is exercised in a general sense, to ensure that
--      it is available, and that it provides the expected results in a known
--      test environment.
--
-- CHANGE HISTORY:
--      23 Sep 13   JAC     Initial pre-release version.
--       6 Dec 13   JAC     Second pre-release version.
--      30 Dec 13   JAC     Third pre-release version.
--      31 Jan 14   JAC     Fourth pre-release version.
--       3 Apr 14   RLB     Created separate test from part of submitted test.
--                          Added additional tests of indexing and references.
--
--!
with Ada.Containers.Indefinite_Hashed_Maps;
with Report;
with FXAIA00; -- Foundation.

procedure CXAIA10 is

   My_Default_Value : constant String := "zzz";

   Value_In_Ptr_Array : FXAIA00.Value_In_Ptr_Array_Type
      renames FXAIA00.Value_In_Ptr_Array;

   type My_Key_Type is new Integer;

   use type Ada.Containers.Hash_Type;

   function My_Hash (Key : My_Key_Type) return Ada.Containers.Hash_Type is
   begin

      return Ada.Containers.Hash_Type (Key) * 17; -- Some prime

   end My_Hash;

   function My_Equivalent_Keys (Left, Right : My_Key_Type) return Boolean is
   begin

      return Left = Right;

   end My_Equivalent_Keys;

   package My_Indefinite_Hashed_Maps is new
     Ada.Containers.Indefinite_Hashed_Maps
     (Key_Type        => My_Key_Type,
      Element_Type    => String,
      Hash            => My_Hash,
      Equivalent_Keys => My_Equivalent_Keys); -- Default =

   My_Map_1 : My_Indefinite_Hashed_Maps.Map;

   My_Cursor_1 : My_Indefinite_Hashed_Maps.Cursor;

   procedure Tampering_Check
     (Container : in out My_Indefinite_Hashed_Maps.Map;
      Where     : in     String) is

      Program_Error_Raised : Boolean := False;

   begin

      declare
      begin

         -- Use a key not already in map

         Container.Insert
           (Key      => FXAIA00.Num_Tests + 1,
            New_Item => Value_In_Ptr_Array (1).all);

      exception

         when Program_Error =>

            Program_Error_Raised := True;

      end;

      if not Program_Error_Raised then

         Report.Failed ("Tampering should have raised error in " & Where);

      end if;

   end Tampering_Check;

   use type Ada.Containers.Count_Type;
   use type My_Indefinite_Hashed_Maps.Cursor;
   use type My_Indefinite_Hashed_Maps.Map;


begin

   Report.Test
     ("CXAIA10",
      "Check iterators, indexing, and references for package " &
      "Ada.Containers.Indefinite_Hashed_Maps");

   -- Test Constant_Reference and Reference (2 forms each) explicitly.

   My_Map_1.Clear;

   My_Map_1.Insert
     (Key      => My_Key_Type (1),
      New_Item => Value_In_Ptr_Array (1).all);

   declare

      My_Constant_Reference : My_Indefinite_Hashed_Maps.Constant_Reference_Type
                                := My_Map_1.Constant_Reference
                                     (Key => My_Key_Type (1));

   begin

      Tampering_Check
        (Container => My_Map_1,
         Where     => "Constant_Reference (key form)");

   end;

   declare

      My_Reference : My_Indefinite_Hashed_Maps.Reference_Type :=
                       My_Map_1.Reference (Key => My_Key_Type (1));

   begin

      Tampering_Check
        (Container => My_Map_1,
         Where     => "Reference (key form)");

   end;

   declare

      My_Constant_Reference : My_Indefinite_Hashed_Maps.Constant_Reference_Type
                                := My_Map_1.Constant_Reference
                                     (Position => My_Map_1.First);

   begin

      Tampering_Check
        (Container => My_Map_1,
         Where     => "Constant_Reference (cursor form)");

   end;

   declare

      My_Reference : My_Indefinite_Hashed_Maps.Reference_Type :=
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
      procedure Test (Value    : in String;
                      Expected : in String;
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

      procedure Test_and_Mod (Value    : in out String;
                              Expected : in     String;
                              New_Item : in     String;
                              Test_Case: in     String)
         with Pre => Value'Length = New_Item'Length is
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
               My_Indefinite_Hashed_Maps.Constant_Reference(My_Map_1,
                   Key => My_Key_Type(1)).Element.all,
            Expected => "00", -- Value_In_Ptr_Array (1).all
            Test_Case=> "Constant_Reference normal key");

      Test_and_Mod (
            Value    =>
               My_Indefinite_Hashed_Maps.Reference(My_Map_1,
                   Key => My_Key_Type(1)).Element.all,
            Expected => "00",
            New_Item => "AA",
            Test_Case=> "Reference normal key");

      Test (Value    =>
               My_Indefinite_Hashed_Maps.Constant_Reference(My_Map_1,
                   Position => My_Cursor_1).Element.all,
            Expected => "AA",
            Test_Case=> "Constant_Reference normal cursor");

      Test_and_Mod (
            Value    =>
               My_Indefinite_Hashed_Maps.Reference(My_Map_1,
                   Position => My_Cursor_1).Element.all,
            Expected => "AA",
            New_Item => "CC",
            Test_Case=> "Reference normal cursor");

      -- Prefix call with all components explicit:
      Test (Value    =>
               My_Map_1.Constant_Reference(
                   Key => My_Key_Type(1)).Element.all,
            Expected => "CC",
            Test_Case=> "Constant_Reference prefix key");

      Test_and_Mod (
            Value    =>
               My_Map_1.Reference(Key => My_Key_Type(1)).Element.all,
            Expected => "CC",
            New_Item => "EE",
            Test_Case=> "Reference prefix key");

      Test (Value    =>
               My_Map_1.Constant_Reference(
                   Position => My_Cursor_1).Element.all,
            Expected => "EE",
            Test_Case=> "Constant_Reference prefix cursor");

      Test_and_Mod (
            Value    =>
               My_Map_1.Reference(Position => My_Cursor_1).Element.all,
            Expected => "EE",
            New_Item => "GG",
            Test_Case=> "Reference prefix cursor");

      -- Prefix call using a generalized reference (implicit dereference):
      Test (Value    => My_Map_1.Constant_Reference(Key => My_Key_Type(1)),
            Expected => "GG",
            Test_Case=> "Constant_Reference generalized key");

      Test_and_Mod (
            Value    => My_Map_1.Reference(Key => My_Key_Type(1)),
            Expected => "GG",
            New_Item => "JJ",
            Test_Case=> "Reference generalized key");

      Test (Value    => My_Map_1.Constant_Reference(Position => My_Cursor_1),
            Expected => "JJ",
            Test_Case=> "Constant_Reference generalized cursor");

      Test_and_Mod (
            Value    => My_Map_1.Reference(Position => My_Cursor_1),
            Expected => "JJ",
            New_Item => "MM",
            Test_Case=> "Reference generalized cursor");

      -- Object indexing, everything implicit.
      Test (Value    => My_Map_1(My_Key_Type(1)), -- Constant_Indexing
            Expected => "MM",
            Test_Case=> "Constant object indexing by key");

      Test_and_Mod (
            Value    => My_Map_1(My_Key_Type(1)), -- Variable_Indexing
            Expected => "MM",
            New_Item => "PP",
            Test_Case=> "Object indexing by key");

      Test (Value    => My_Map_1(My_Cursor_1), -- Constant_Indexing
            Expected => "PP",
            Test_Case=> "Constant object indexing by cursor");

      Test_and_Mod (
            Value    => My_Map_1(My_Cursor_1), -- Variable_Indexing
            Expected => "PP",
            New_Item => "00",
            Test_Case=> "Object indexing by cursor");

   end;


   -- Test Ada 2012 Iterate (2 forms) and (implicitly) Constant_Reference and
   -- Reference

   declare

      Pos_Of_First_Char : Natural;
      Total_In          : Natural := 0;
      Total_Out         : Natural;

   begin

      My_Map_1.Clear;

      -- Set with Ada 2005-style loop

      for I in FXAIA00.Array_Bounds_Type loop

         My_Map_1.Insert
           (Key      => My_Key_Type (I),
            New_Item => Value_In_Ptr_Array (I).all);

         Pos_Of_First_Char := Character'Pos (Value_In_Ptr_Array (I).all (1));

         Total_In := Total_In + Pos_Of_First_Char;

      end loop;

      Total_Out := 0;

      for E of My_Map_1 loop

         Tampering_Check
           (Container => My_Map_1,
            Where     => "reading of loop");

         Pos_Of_First_Char := Character'Pos (E (1));

         Total_Out := Total_Out + Pos_Of_First_Char;

      end loop;

      if not (Total_Out = Total_In) then

         Report.Failed ("Total after of loop not as expected");

      end if;

      -- No reverse form for Hashed Containers

      Total_Out := 0;

      for C in My_Map_1.Iterate loop

         Tampering_Check
           (Container => My_Map_1,
            Where     => "reading in loop");

         Pos_Of_First_Char := Character'Pos (My_Map_1 (C) (1));

         Total_Out := Total_Out + Pos_Of_First_Char;

      end loop;

      if not (Total_Out = Total_In) then

         Report.Failed ("Total after in loop not as expected");

      end if;

      -- No reverse or start value forms for Hashed Containers

      -- Try Variable_Indexing as a change from Constant_Indexing

      -- Initialise with something of the same length as the value to be set by
      -- the of loop

      My_Map_1.Clear;

      for I in FXAIA00.Array_Bounds_Type loop

         My_Map_1.Insert
           (Key      => My_Key_Type (I),
            New_Item => "abc");

      end loop;

      for E of My_Map_1 loop

         Tampering_Check
           (Container => My_Map_1,
            Where     => "writing of loop");

         -- Note that have to write something of the same length as the existing
         -- element otherwise will fail length check, only Replace_Element can
         -- change the length

         E := My_Default_Value;

      end loop;

      -- Check with Ada 2005-style loop

      My_Cursor_1 := My_Map_1.First;

      for I in FXAIA00.Array_Bounds_Type loop

         if My_Indefinite_Hashed_Maps.Element (Position => My_Cursor_1) /=
           My_Default_Value then

            Report.Failed ("Data set by of loop not as expected");

         end if;

         My_Indefinite_Hashed_Maps.Next (Position => My_Cursor_1);

      end loop;

   end;


   Report.Result;

end CXAIA10;
