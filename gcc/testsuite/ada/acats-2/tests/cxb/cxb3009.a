-- CXB3009.A
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
--      Check that the function To_Chars_Ptr will return a Null_Ptr value
--      when the parameter Item is null.  If the parameter Item is not null,
--      and references a chars_array object that does contain the char nul,
--      and parameter Nul_Check is True, check that To_Chars_Ptr performs a
--      pointer conversion from char_array_access type to chars_ptr type.
--      Check that if parameter Item is not null, and references a
--      chars_array object that does not contain nul, and parameter Nul_Check
--      is True, the To_Chars_Ptr function will propagate Terminator_Error.
--      Check that if parameter Item is not null, and parameter Nul_Check
--      is False, check that To_Chars_Ptr performs a pointer conversion from
--      char_array_access type to chars_ptr type.
--
--      Check that the New_Char_Array function will return a chars_ptr type
--      pointer to an allocated object that has been initialized with
--      the value of parameter Chars.
--
--      Check that the function New_String returns a chars_ptr initialized
--      to a nul-terminated string having the value of the Str parameter.
--
-- TEST DESCRIPTION:
--      This test uses a variety of of string, char_array,
--      char_array_access and char_ptr values in order to validate the
--      functions under test, and results are compared for both length
--      and content.
--
--      This test assumes that the following characters are all included
--      in the implementation defined type Interfaces.C.char:
--      ' ', 'a'..'z', and 'A'.. 'Z'.
--
-- APPLICABILITY CRITERIA:
--      This test is applicable to all implementations that provide
--      package Interfaces.C.Strings.  If an implementation provides
--      package Interfaces.C.Strings, this test must compile, execute, and
--      report "PASSED".
--
--
-- CHANGE HISTORY:
--      20 Sep 95   SAIC    Initial prerelease version.
--      09 May 96   SAIC    Incorporated reviewer comments for ACVC 2.1.
--      01 DEC 97   EDS     Remove incorrect block of code (previously
--                          lines 264-287)
--      14 Sep 99   RLB     Added check for behavior of To_Chars_Ptr when
--                          Nul_Check => False. (From Technical
--                          Corrigendum 1).
--!

with Report;
with Interfaces.C.Strings;                                   -- N/A => ERROR
with Ada.Characters.Latin_1;
with Ada.Exceptions;
with Ada.Strings.Fixed;

procedure CXB3009 is
begin

   Report.Test ("CXB3009", "Check that functions To_Chars_Ptr, "      &
                           "New_Chars_Array, and New_String produce " &
                           "correct results");

   Test_Block:
   declare

      package IC  renames Interfaces.C;
      package ICS renames Interfaces.C.Strings;
      use Ada.Exceptions;

      use type IC.char_array;
      use type IC.size_t;
      use type ICS.chars_ptr;

      Null_Char_Array_Access : constant ICS.char_array_access := null;

      Test_String            : constant String := "Test String";
      String_With_nul        : String(1..6)    := "Addnul";
      String_Without_nul     : String(1..6)    := "No nul";

      Char_Array_With_nul    : IC.char_array(0..6)   :=
                                 IC.To_C(String_With_nul, True);
      Char_Array_Without_nul : IC.char_array(0..5)   :=
                                 IC.To_C(String_Without_nul, False);
      Char_Array_W_nul_Ptr   : ICS.char_array_access :=
                                 new IC.char_array'(Char_Array_With_nul);
      Char_Array_WO_nul_Ptr  : ICS.char_array_access :=
                                 new IC.char_array'(Char_Array_Without_nul);

      TC_chars_ptr           : ICS.chars_ptr;

      TC_size_t              : IC.size_t := IC.size_t'First;


   begin

      -- Check that the function To_Chars_Ptr will return a Null_Ptr value
      -- when the parameter Item is null.

      if ICS.To_Chars_Ptr(Item      => Null_Char_Array_Access,
                          Nul_Check => False)     /= ICS.Null_Ptr   or
         ICS.To_Chars_Ptr(Null_Char_Array_Access,
                          Nul_Check => True)      /= ICS.Null_Ptr   or
         ICS.To_Chars_Ptr(Null_Char_Array_Access) /= ICS.Null_Ptr
      then
         Report.Failed("Incorrect result from function To_Chars_Ptr " &
                       "with parameter Item being a null value");
      end if;


      -- Check that if the parameter Item is not null, and references a
      -- chars_array object that does contain the nul char, and parameter
      -- Nul_Check is True, function To_Chars_Ptr performs a pointer
      -- conversion from char_array_access type to chars_ptr type.

      begin
         TC_chars_ptr := ICS.To_Chars_Ptr(Item      => Char_Array_W_nul_Ptr,
                                          Nul_Check => True);

         if ICS.Value(TC_chars_ptr) /= String_With_nul     or
            ICS.Value(TC_chars_ptr) /= Char_Array_With_nul
         then
            Report.Failed("Incorrect result from function To_Chars_Ptr " &
                          "with parameter Item being non-null and "      &
                          "containing the nul char");
         end if;
      exception
         when IC.Terminator_Error =>
           Report.Failed("Terminator_Error raised during the validation " &
                         "of Function To_Chars_Ptr");
         when others           =>
           Report.Failed("Unexpected exception raised during the " &
                         "validation of Function To_Chars_Ptr");
      end;

      -- Check that if parameter Item is not null, and references a
      -- chars_array object that does not contain nul, and parameter
      -- Nul_Check is True, the To_Chars_Ptr function will propagate
      -- Terminator_Error.

      begin
         TC_chars_ptr   := ICS.To_Chars_Ptr(Char_Array_WO_nul_Ptr, True);
         Report.Failed("Terminator_Error was not raised by function "   &
                       "To_Chars_Ptr when given a parameter Item that " &
                       "is non-null, and does not contain the nul "     &
                       "char, but parameter Nul_Check is True");
         TC_size_t := ICS.Strlen(TC_chars_ptr); -- Use TC_chars_ptr to
                                                -- defeat optimization;
      exception
          when IC.Terminator_Error => null;     -- Expected exception.
          when others              =>
             Report.Failed("Incorrect exception raised when function "    &
                           "To_Chars_Ptr is given a parameter Item that " &
                           "is non-null, and does not contain the nul "   &
                           "char, but parameter Nul_Check is True");
      end;

      -- Check that if the parameter Item is not null, and parameter
      -- Nul_Check is False, function To_Chars_Ptr performs a pointer
      -- conversion from char_array_access type to chars_ptr type.

      begin
         TC_chars_ptr := ICS.To_Chars_Ptr(Item      => Char_Array_WO_nul_Ptr,
                                          Nul_Check => False);

         if ICS.Value(TC_chars_ptr, 6) /= String_Without_nul     or
            ICS.Value(TC_chars_ptr, 6) /= Char_Array_Without_nul
         then
            Report.Failed("Incorrect result from function To_Chars_Ptr " &
                          "with parameter Item being non-null and "      &
                          "Nul_Check False");
         end if;
      exception
         when IC.Terminator_Error =>
           Report.Failed("Terminator_Error raised during the validation " &
                         "of Function To_Chars_Ptr");
         when others           =>
           Report.Failed("Unexpected exception raised during the " &
                         "validation of Function To_Chars_Ptr");
      end;


      -- Check that the New_Char_Array function will return a chars_ptr type
      -- pointer to an allocated object that has been initialized with
      -- the value of parameter Chars.
      TC_chars_ptr := ICS.New_String("");
      ICS.Free(TC_chars_ptr);   -- Reset the chars_ptr to Null_Ptr;

      if TC_chars_ptr /= ICS.Null_Ptr then
         Report.Failed("Reset of TC_chars_ptr to Null not successful - 1");
      end if;

      TC_chars_ptr := ICS.New_Char_Array(Chars => Char_Array_With_nul);

      if TC_chars_ptr = ICS.Null_Ptr then    -- Check allocation.
         Report.Failed
           ("No allocation took place in call to New_Char_Array " &
            "with a non-null char_array parameter containing a "  &
            "terminating nul char");
      end if;

      -- Length of allocated array is determined using Strlen since array
      -- is nul terminated.  Contents of array are validated using Value.

      if ICS.Value (TC_chars_ptr, Length => 7) /= Char_Array_With_nul  or
         ICS.Strlen(Item => TC_chars_ptr)      /= 6
      then
         Report.Failed
           ("Incorrect length of allocated char_array resulting " &
            "from call of New_Char_Array with a non-null "        &
            "char_array parameter containing a terminating nul char");
      end if;

      ICS.Free(TC_chars_ptr);   -- Reset the chars_ptr to Null_Ptr;
      if TC_chars_ptr /= ICS.Null_Ptr then
         Report.Failed("Reset of TC_chars_ptr to Null not successful - 2");
      end if;

      TC_chars_ptr := ICS.New_Char_Array(Chars => Char_Array_Without_nul);

      if TC_chars_ptr = ICS.Null_Ptr then    -- Check allocation.
         Report.Failed
           ("No allocation took place in call to New_Char_Array " &
            "with a non-null char_array parameter that did not "  &
            "contain a terminating nul char");
      end if;

      -- Function Value is used with the total length of the
      -- Char_Array_Without_nul as a parameter to verify the allocation.

      if ICS.Value(Item => TC_chars_ptr, Length => 6) /=
         Char_Array_Without_nul                            or
         ICS.Strlen(Item => TC_chars_ptr)             /= 6
      then
         Report.Failed("Incorrect length of allocated char_array "     &
                       "resulting from call of New_Char_Array with "   &
                       "a non-null char_array parameter that did not " &
                       "contain a terminating nul char");
      end if;


      -- Check that the function New_String returns a chars_ptr specifying
      -- an allocated object initialized to the value of parameter Str.

      ICS.Free(TC_chars_ptr);   -- Reset the chars_ptr to Null_Ptr;
      if TC_chars_ptr /= ICS.Null_Ptr then
         Report.Failed("Reset of TC_chars_ptr to Null not successful - 3");
      end if;

      TC_chars_ptr := ICS.New_String(Str => Test_String);

      if ICS.Value(TC_chars_ptr) /= Test_String or
         ICS.Value(ICS.New_Char_Array(IC.To_C(Test_String,True))) /=
         Test_String
      then
         Report.Failed("Incorrect allocation resulting from function " &
                       "New_String with a string parameter value");
      end if;

      ICS.Free(TC_chars_ptr);   -- Reset the chars_ptr to Null_Ptr;
      if TC_chars_ptr /= ICS.Null_Ptr then
         Report.Failed("Reset of TC_chars_ptr to Null not successful - 4");
      end if;

      if ICS.Value(ICS.New_String(String_Without_nul)) /=
         String_Without_nul                               or
         ICS.Value(ICS.New_Char_Array(IC.To_C(String_Without_nul,False))) /=
         String_Without_nul
      then
         Report.Failed("Incorrect allocation resulting from function " &
                       "New_String with parameter value String_Without_nul");
      end if;


   exception
      when The_Error : others =>
         Report.Failed ("The following exception was raised in the " &
                        "Test_Block: " & Exception_Name(The_Error));
   end Test_Block;

   Report.Result;

end CXB3009;
