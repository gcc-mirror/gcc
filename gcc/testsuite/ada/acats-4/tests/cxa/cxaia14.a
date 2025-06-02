-- CXAIA14.A
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
--      Ada.Containers.Indefinite_Multiway_Trees.
--
-- TEST DESCRIPTION:
--      This test verifies that an implementation supports the iterator and
--      reference subprograms contained in package
--      Ada.Containers.Indefinite_Multiway_Trees. Each of the subprograms is
--      exercised in a general sense, to ensure that
--      it is available, and that it provides the expected results in a known
--      test environment.
--
-- CHANGE HISTORY:
--      23 Sep 13   JAC     Initial pre-release version.
--       6 Dec 13   JAC     Second pre-release version.
--      31 Jan 14   JAC     Third pre-release version.
--       4 Apr 14   RLB     Created separate test from part of submitted test.
--                          Added additional tests of indexing and references.
--      14 Apr 14   RLB     Corrected error in early loop exit result check.
--
--!
with Ada.Containers.Indefinite_Multiway_Trees;
with Report;
with Ada.Exceptions;
with FXAIA00; -- Foundation.

procedure CXAIA14 is

   My_Default_Value : constant String := "zzz";

   package My_Indefinite_Multiway_Trees is new
     Ada.Containers.Indefinite_Multiway_Trees
       (Element_Type => String); -- Default =

   My_Tree_1 : My_Indefinite_Multiway_Trees.Tree;

   -- Test Root

   Root_1 : constant My_Indefinite_Multiway_Trees.Cursor := My_Tree_1.Root;

   Value_In_Ptr_Array : FXAIA00.Value_In_Ptr_Array_Type
      renames FXAIA00.Value_In_Ptr_Array;

   My_Cursor_1               : My_Indefinite_Multiway_Trees.Cursor;

   procedure Tampering_Check
     (Container : in out My_Indefinite_Multiway_Trees.Tree;
      Where     : in     String) is

      Program_Error_Raised : Boolean := False;

   begin

      declare
      begin

         Container.Append_Child
           (Parent   => Root_1,
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
   use type My_Indefinite_Multiway_Trees.Cursor;
   use type My_Indefinite_Multiway_Trees.Tree;


begin

   Report.Test
     ("CXAIA14",
      "Check iterators, indexing, and references for package " &
      "Ada.Containers.Multiway_Trees");

   -- Test Constant_Reference and Reference explicitly.

   My_Tree_1.Clear;

   My_Tree_1.Append_Child
     (Parent   => Root_1,
      New_Item => Value_In_Ptr_Array (1).all);

   declare

      My_Constant_Reference :
                       My_Indefinite_Multiway_Trees.Constant_Reference_Type :=
                         My_Tree_1.Constant_Reference
                           (Position => My_Indefinite_Multiway_Trees.First_Child
                                          (Parent => Root_1));

   begin

      Tampering_Check
        (Container => My_Tree_1,
         Where     => "Constant_Reference");

   end;

   declare

      My_Reference : My_Indefinite_Multiway_Trees.Reference_Type :=
                       My_Tree_1.Reference
                         (Position => My_Indefinite_Multiway_Trees.First_Child
                                        (Parent => Root_1));

   begin

      Tampering_Check
        (Container => My_Tree_1,
         Where     => "Reference");

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
           (Container => My_Tree_1,
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
                              Test_Case: in     String) is
      begin
         Tampering_Check
           (Container => My_Tree_1,
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
      My_Cursor_1 := My_Indefinite_Multiway_Trees.First_Child
                                   (Parent => Root_1);

      -- Normal call:
      Test (Value    =>
               My_Indefinite_Multiway_Trees.Constant_Reference(My_Tree_1,
                   Position => My_Cursor_1).Element.all,
            Expected => "00", -- Value_In_Ptr_Array (I).all
            Test_Case=> "Constant_Reference normal cursor");

      Test_and_Mod (
            Value    =>
               My_Indefinite_Multiway_Trees.Reference(My_Tree_1,
                   Position => My_Cursor_1).Element.all,
            Expected => "00",
            New_Item => "37",
            Test_Case=> "Reference normal cursor");

      -- Prefix call with all components explicit:
      Test (Value    =>
               My_Tree_1.Constant_Reference(
                   Position => My_Cursor_1).Element.all,
            Expected => "37",
            Test_Case=> "Constant_Reference prefix cursor");

      Test_and_Mod (
            Value    =>
               My_Tree_1.Reference(Position => My_Cursor_1).Element.all,
            Expected => "37",
            New_Item => "64",
            Test_Case=> "Reference prefix cursor");

      -- Prefix call using a generalized reference (implicit dereference):
      Test (Value    => My_Tree_1.Constant_Reference(Position => My_Cursor_1),
            Expected => "64",
            Test_Case=> "Constant_Reference generalized cursor");

      Test_and_Mod (
            Value    => My_Tree_1.Reference(Position => My_Cursor_1),
            Expected => "64",
            New_Item => "91",
            Test_Case=> "Reference generalized cursor");

      -- Object indexing, everything implicit.
      Test (Value    => My_Tree_1(My_Cursor_1), -- Constant_Indexing
            Expected => "91",
            Test_Case=> "Constant object indexing by cursor");

      Test_and_Mod (
            Value    => My_Tree_1(My_Cursor_1), -- Variable_Indexing
            Expected => "91",
            New_Item => "00",
            Test_Case=> "Object indexing by cursor");

   end;


   -- Test Ada 2012 Iterate (2 forms) and (implicitly) Constant_Reference and
   -- Reference

   declare

      Pos_Of_First_Char : Natural;
      Previous_Value    : Natural;
      Total_In          : Natural := 0;
      Total_Out         : Natural;
      Early_Total_In    : Natural;
      Child_Total_In    : Natural;
      Child_Total_Out   : Natural;

   begin

      My_Tree_1.Clear;

      -- Set with Ada 2005-style loop

      for I in FXAIA00.Array_Bounds_Type loop

         My_Tree_1.Append_Child
           (Parent   => Root_1,
            New_Item => Value_In_Ptr_Array (I).all);

         Pos_Of_First_Char := Character'Pos (Value_In_Ptr_Array (I).all (1));

         Total_In := Total_In + Pos_Of_First_Char;

      end loop;

      Previous_Value := Natural'First;
      Total_Out      := 0;

      for E of My_Tree_1 loop

         Tampering_Check
           (Container => My_Tree_1,
            Where     => "reading of loop");

         Pos_Of_First_Char := Character'Pos (E (1));

         Total_Out := Total_Out + Pos_Of_First_Char;

         if not (Pos_Of_First_Char > Previous_Value) then

            Report.Failed ("Of loop not in ascending order");

         end if;

         Previous_Value := Pos_Of_First_Char;

      end loop;

      if not (Total_Out = Total_In) then

         Report.Failed ("Total after of loop not as expected");

      end if;

      -- No reverse form for Multiway_Trees

      Previous_Value := Natural'First;
      Total_Out      := 0;

      for C in My_Tree_1.Iterate loop

         Tampering_Check
           (Container => My_Tree_1,
            Where     => "reading in loop");

         Pos_Of_First_Char := Character'Pos (My_Tree_1 (C) (1));

         Total_Out := Total_Out + Pos_Of_First_Char;

         if not (Pos_Of_First_Char > Previous_Value) then

            Report.Failed ("Forward in loop not in ascending order");

         end if;

         Previous_Value := Pos_Of_First_Char;

      end loop;

      if not (Total_Out = Total_In) then

         Report.Failed ("Total after in loop not as expected");

      end if;

      -- No reverse form for Multiway_Trees

      Previous_Value := Natural'First;
      Total_Out      := 0;

      for C in My_Indefinite_Multiway_Trees.Iterate_Subtree (Root_1) loop

         Tampering_Check
           (Container => My_Tree_1,
            Where     => "reading in loop subtree");

         Pos_Of_First_Char := Character'Pos (My_Tree_1 (C) (1));

         Total_Out := Total_Out + Pos_Of_First_Char;

         if not (Pos_Of_First_Char > Previous_Value) then

            Report.Failed ("In loop subtree not in ascending order");

         end if;

         Previous_Value := Pos_Of_First_Char;

      end loop;

      if not (Total_Out = Total_In) then

         Report.Failed ("Total after in loop subtree not as expected");

      end if;

      -- No reverse form for Multiway_Trees

      -- Try Iterate_Children:

      Previous_Value  := Natural'First;
      Child_Total_In  := 0;
      Child_Total_Out := 0;

      for C in My_Tree_1.Iterate_Children (Parent => Root_1) loop

         Tampering_Check
           (Container => My_Tree_1,
            Where     => "reading child in loop");

         Pos_Of_First_Char := Character'Pos (My_Tree_1 (C) (1));

         Child_Total_In := Child_Total_In + Pos_Of_First_Char;

         if not (Pos_Of_First_Char > Previous_Value) then

            Report.Failed ("Child In loop not in ascending order");

         end if;

         Previous_Value := Pos_Of_First_Char;

      end loop;

      if not (Total_In = Child_Total_In) then

         Report.Failed ("Total after child in loop not as expected");

      end if;

      Previous_Value  := 1000;

      for C in reverse My_Tree_1.Iterate_Children (Parent => Root_1) loop

         Tampering_Check
           (Container => My_Tree_1,
            Where     => "reading child in loop");

         Pos_Of_First_Char := Character'Pos (My_Tree_1 (C) (1));

         Child_Total_Out := Child_Total_Out + Pos_Of_First_Char;

         if not (Pos_Of_First_Char < Previous_Value) then

            Report.Failed ("Child In reverse loop not in descending order");

         end if;

         Previous_Value := Pos_Of_First_Char;

      end loop;


      if not (Child_Total_Out = Child_Total_In) then

         Report.Failed ("Total after child in reverse loop not as expected");

      end if;


      -- Check early exit loop as described in AARM A.18.10(157.a-c/3):

      My_Cursor_1 := My_Tree_1.Find
                       (Item => Value_In_Ptr_Array (3).all);

      Previous_Value := Natural'First;
      Early_Total_In := 0;

      for C in My_Tree_1.Iterate loop

         Pos_Of_First_Char := Character'Pos (My_Tree_1 (C) (1));

         Early_Total_In := Early_Total_In + Pos_Of_First_Char;

         if not (Pos_Of_First_Char > Previous_Value) then

            Report.Failed ("Early exit forward in loop not in ascending order");

         end if;

         Previous_Value := Pos_Of_First_Char;

         exit when C = My_Cursor_1;

      end loop;

      if not (Early_Total_In = (Character'Pos(Value_In_Ptr_Array(1)(1)) +
                                Character'Pos(Value_In_Ptr_Array(2)(1)) +
                                Character'Pos(Value_In_Ptr_Array(3)(1)))) then

         Report.Failed ("Total after early exit in loop not as expected");

      end if;


      -- Try Variable_Indexing as a change from Constant_Indexing

      -- Initialise with something of the same length as the value to be set by
      -- the of loop

      My_Tree_1.Clear;

      for I in FXAIA00.Array_Bounds_Type loop

         My_Tree_1.Append_Child
           (Parent   => Root_1,
            New_Item => "abc");

      end loop;

      for E of My_Tree_1 loop

         Tampering_Check
           (Container => My_Tree_1,
            Where     => "writing of loop");

         -- Note that have to write something of the same length as the existing
         -- element otherwise will fail length check, only Replace_Element can
         -- change the length

         E := My_Default_Value;

      end loop;

      -- Check with Ada 2005-style loop

      My_Cursor_1 := My_Indefinite_Multiway_Trees.First_Child
                       (Parent => Root_1);

      for I in FXAIA00.Array_Bounds_Type loop

         if My_Indefinite_Multiway_Trees.Element (Position => My_Cursor_1) /=
           My_Default_Value then

            Report.Failed ("Data set by of loop not as expected");

         end if;

         My_Indefinite_Multiway_Trees.Next_Sibling (Position => My_Cursor_1);

      end loop;

   end;


   Report.Result;

end CXAIA14;
