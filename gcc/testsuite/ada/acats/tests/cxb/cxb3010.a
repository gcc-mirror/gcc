-- CXB3010.A
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
--      Check that the Procedure Free resets the parameter Item to
--      Null_Ptr.  Check that Free has no effect if Item is Null_Ptr.
--
--      Check that the version of Function Value with a chars_ptr parameter
--      returning a char_array result returns the prefix of an array of
--      chars.
--
--      Check that the version of Function Value with a chars_ptr parameter
--      and a size_t parameter returning a char_array result returns
--      the shorter of:
--        1) the first size_t number of characters, or
--        2) the characters up to and including the first nul.
--
--      Check that both of the above versions of Function Value propagate
--      Dereference_Error if the Item parameter is Null_Ptr.
--
-- TEST DESCRIPTION:
--      This test validates the Procedure Free and two versions of Function
--      Value.  A variety of char_array and char_ptr values are provided as
--      input, and results are compared for both length and content.
--
--      This test assumes that the following characters are all included
--      in the implementation defined type Interfaces.C.char:
--      ' ', 'a'..'z', and 'A'..'Z'.
--
-- APPLICABILITY CRITERIA:
--      This test is applicable to all implementations that provide
--      package Interfaces.C.Strings.  If an implementation provides
--      package Interfaces.C.Strings, this test must compile, execute,
--      and report "PASSED".
--
--
-- CHANGE HISTORY:
--      27 Sep 95   SAIC    Initial prerelease version.
--      13 May 96   SAIC    Incorporated reviewer comments for ACVC 2.1.
--      26 Oct 96   SAIC    Incorporated reviewer comments.
--      01 DEC 97   EDS     Replicate line 199 at line 256, to ensure that
--                          TC_chars_ptr has a valid pointer.
--      08 JUL 99   RLB     Added a test case to check that Value raises
--                          Constraint_Error when Length = 0. (From Technical
--                          Corrigendum 1).
--      25 JAN 01   RLB     Repaired previous test case to avoid raising
--                          Constraint_Error in test case code.
--      26 JAN 01   RLB     Added an Ident_Int to the test case to prevent
--                          optimization.

--!

with Report;
with Interfaces.C.Strings;                                    -- N/A => ERROR

procedure CXB3010 is
begin

   Report.Test ("CXB3010", "Check that Procedure Free and versions of " &
                           "Function Value produce correct results");

   Test_Block:
   declare

      package IC  renames Interfaces.C;
      package ICS renames Interfaces.C.Strings;

      use type IC.char_array;
      use type IC.size_t;
      use type ICS.chars_ptr;
      use type IC.char;

      Null_Char_Array_Access : constant ICS.char_array_access := null;

      TC_String_1            : constant String       := "Nonul";
      TC_String_2            : constant String       := "AbCdE";
      TC_Blank_String        : constant String(1..5) := (others => ' ');

      -- The initialization of the following char_array objects
      -- includes the appending of a terminating nul char, in order to
      -- prevent the erroneous execution of Function Value.

      TC_char_array          : IC.char_array :=
                                 IC.To_C(TC_Blank_String, True);
      TC_char_array_1        : constant IC.char_array :=
                                          IC.To_C(TC_String_1, True);
      TC_char_array_2        : constant IC.char_array :=
                                          IC.To_C(TC_String_2, True);
      TC_Blank_char_array    : constant IC.char_array :=
                                          IC.To_C(TC_Blank_String, True);

      -- This chars_ptr is initialized via the use of New_Chars_Array to
      -- avoid erroneous execution of procedure Free.
      TC_chars_ptr           : ICS.chars_ptr :=
                                 ICS.New_Char_Array(TC_Blank_char_array);

   begin

      -- Check that the Procedure Free resets the parameter Item
      -- to Null_Ptr.

      if TC_chars_ptr = ICS.Null_Ptr then
         Report.Failed("TC_chars_ptr is currently null; it should not be " &
                       "null since it was given default initialization");
      end if;

      ICS.Free(TC_chars_ptr);

      if TC_chars_ptr /= ICS.Null_Ptr then
         Report.Failed("TC_chars_ptr was not set to Null_Ptr by " &
                       "Procedure Free");
      end if;

      -- Check that Free has no effect if Item is Null_Ptr.

      begin
         TC_chars_ptr := ICS.Null_Ptr;  -- Ensure pointer is null.
         ICS.Free(TC_chars_ptr);
         if TC_chars_ptr /= ICS.Null_Ptr then
            Report.Failed("TC_chars_ptr was set to a non-Null_Ptr value "  &
                          "by Procedure Free.  It was provided as a null " &
                          "parameter to Free, and there should have been " &
                          "no effect from a call to Procedure Free");
         end if;
      exception
         when others =>
           Report.Failed("Unexpected exception raised by Procedure Free " &
                         "when parameter Item is Null_Ptr");
      end;


      -- Check that the version of Function Value with a chars_ptr parameter
      -- that returns a char_array result returns an array of chars (up to
      -- and including the first nul).

      TC_chars_ptr  := ICS.New_Char_Array(TC_char_array_1);
      TC_char_array := ICS.Value(Item => TC_chars_ptr);

      if TC_char_array                  /= TC_char_array_1            or
         IC.To_Ada(TC_char_array, True) /= IC.To_Ada(TC_char_array_1)
      then
         Report.Failed("Incorrect result from Function Value - 1");
      end if;

      TC_chars_ptr  := ICS.New_Char_Array(TC_char_array_2);
      TC_char_array := ICS.Value(Item => TC_chars_ptr);

      if TC_char_array                  /= TC_char_array_2            or
         IC.To_Ada(TC_char_array, True) /= IC.To_Ada(TC_char_array_2)
      then
         Report.Failed("Incorrect result from Function Value - 2");
      end if;

      if ICS.Value(Item => ICS.New_String("A little longer string")) /=
         IC.To_C("A little longer string")
      then
         Report.Failed("Incorrect result from Function Value - 3");
      end if;


      -- Check that the version of Function Value with a chars_ptr parameter
      -- and a size_t parameter that returns a char_array result returns
      -- the shorter of:
      --   1) the first size_t number of characters, or
      --   2) the characters up to and including the first nul.

      -- Case 1: the first size_t number of characters (less than the
      --         total length).

      begin
         TC_chars_ptr        := ICS.New_Char_Array(TC_char_array_1);
         TC_char_array(0..2) := ICS.Value(Item => TC_chars_ptr, Length => 3);

         if TC_char_array(0..2) /= TC_char_array_1(0..2)
         then
            Report.Failed
              ("Incorrect result from Function Value with Length " &
               "parameter - 1");
         end if;
      exception
         when others =>
           Report.Failed("Exception raised during Case 1 evaluation");
      end;

      -- Case 2: the characters up to and including the first nul.

      TC_chars_ptr  := ICS.New_Char_Array(TC_char_array_2);

      -- The length supplied as a parameter exceeds the total length of
      -- TC_char_array_2.  The result should be the entire TC_char_array_2
      -- including the terminating nul.

      TC_char_array := ICS.Value(Item => TC_chars_ptr, Length => 7);

      if TC_char_array            /= TC_char_array_2            or
         IC.To_Ada(TC_char_array) /= IC.To_Ada(TC_char_array_2) or
         not (IC.Is_Nul_Terminated(TC_char_array))
      then
         Report.Failed("Incorrect result from Function Value with Length " &
                       "parameter - 2");
      end if;


      -- Check that both of the above versions of Function Value propagate
      -- Dereference_Error if the Item parameter is Null_Ptr.

      declare

         -- Declare a dummy function to demonstrate one way that a chars_ptr
         -- variable could inadvertantly be set to Null_Ptr prior to a call
         -- to Value (below).
         function Freedom (Condition : Boolean := False;
                           Ptr       : ICS.chars_ptr) return ICS.chars_ptr is
            Pointer : ICS.chars_ptr := Ptr;
         begin
            if Condition then
               ICS.Free(Pointer);
            else
               null; -- An activity that doesn't set the chars_ptr value to
                     -- Null_Ptr.
            end if;
            return Pointer;
         end Freedom;

      begin

         begin
            TC_char_array := ICS.Value(Item => Freedom(True, TC_chars_ptr));
            Report.Failed
              ("Function Value (without Length parameter) did not " &
               "raise Dereference_Error when provided a null Item " &
               "parameter input value");
            if TC_char_array(0) = '6' then   -- Defeat optimization.
               Report.Comment("Should never be printed");
            end if;
         exception
            when ICS.Dereference_Error => null;  -- OK, expected exception.
            when others                =>
              Report.Failed("Incorrect exception raised by Function Value " &
                            "with Item parameter, when the Item parameter " &
                            "is Null_Ptr");
         end;

         TC_chars_ptr  := ICS.New_Char_Array(TC_char_array_2);
         begin
            TC_char_array := ICS.Value(Item   => Freedom(True, TC_chars_ptr),
                                       Length => 4);
            Report.Failed
              ("Function Value (with Length parameter) did not "    &
               "raise Dereference_Error when provided a null Item " &
               "parameter input value");
            if TC_char_array(0) = '6' then   -- Defeat optimization.
               Report.Comment("Should never be printed");
            end if;
         exception
            when ICS.Dereference_Error => null;  -- OK, expected exception.
            when others                =>
              Report.Failed("Incorrect exception raised by Function Value " &
                            "with both Item and Length parameters, when "   &
                            "the Item parameter is Null_Ptr");
         end;
      end;

      -- Check that Function Value with two parameters propagates
      -- Constraint_Error if Length is 0.

      begin
         TC_chars_ptr        := ICS.New_Char_Array(TC_char_array_1);
	 declare
	    TC : IC.char_array := ICS.Value(Item => TC_chars_ptr, Length =>
		IC.Size_T(Report.Ident_Int(0)));
	 begin
             Report.Failed
                 ("Function Value (with Length parameter) did not "    &
                  "raise Constraint_Error when Length = 0");
             if TC'Length <= TC_char_array'Length then
                TC_char_array(1..TC'Length) := TC; -- Block optimization of TC.
             end if;
         end;

         Report.Failed
              ("Function Value (with Length parameter) did not "    &
               "raise Constraint_Error when Length = 0");
      exception
         when Constraint_Error => null;  -- OK, expected exception.
         when others =>
            Report.Failed("Incorrect exception raised by Function Value " &
                          "with both Item and Length parameters, when "   &
                          "Length = 0");
      end;

   exception
      when others => Report.Failed ("Exception raised in Test_Block");
   end Test_Block;

   Report.Result;

end CXB3010;
