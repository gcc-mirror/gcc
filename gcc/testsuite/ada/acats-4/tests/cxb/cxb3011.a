-- CXB3011.A
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
--      Check that the version of Function Value with a chars_ptr parameter
--      that returns a String result returns an Ada string containing the
--      characters pointed to by the chars_ptr parameter, up to (but not 
--      including) the terminating nul.
--
--      Check that the version of Function Value with a chars_ptr parameter
--      and a size_t parameter that returns a String result returns the
--      shorter of: 
--        1) a String of the first size_t number of characters, or 
--        2) a String of characters up to (but not including) the
--           terminating nul.
--
--      Check that the Function Strlen returns a size_t result that
--      corresponds to the number of chars in the array pointed to by Item,
--      up to but not including the terminating nul.
--
--      Check that both of the above versions of Function Value and
--      Function Strlen propagate Dereference_Error if the Item parameter
--      is Null_Ptr.
--      
-- TEST DESCRIPTION:
--      This test validates two versions of Function Value, and the Function
--      Strlen.  A series of char_ptr values are provided as input, and 
--      results are compared for length or content.  
--      
--      This test assumes that the following characters are all included
--      in the implementation defined type Interfaces.C.char:
--      ' ', 'a'..'z', 'A'..'Z', '0'..'9', '*' and '.'.
--      
-- APPLICABILITY CRITERIA: 
--      This test is applicable to all implementations that provide 
--      package Interfaces.C.Strings.  If an implementation provides
--      package Interfaces.C.Strings, this test must compile, execute, 
--      and report "PASSED".
--
--       
-- CHANGE HISTORY:
--      28 Sep 95   SAIC    Initial prerelease version.
--      13 May 96   SAIC    Incorporated reviewer comments for ACVC 2.1.
--      26 Oct 96   SAIC    Incorporated reviewer comments.
--
--!

with Report;
with Ada.Characters.Latin_1;
with Interfaces.C.Strings;                                    -- N/A => ERROR

procedure CXB3011 is
begin

   Report.Test ("CXB3011", "Check that the two versions of Function Value " &
                           "returning a String result, and the Function "   &
                           "Strlen, produce correct results");

   Test_Block:
   declare

      package IC   renames Interfaces.C;
      package ICS  renames Interfaces.C.Strings;
      package ACL1 renames Ada.Characters.Latin_1;

      use type IC.char_array;
      use type IC.size_t;
      use type ICS.chars_ptr;

      Null_Char_Array_Access : constant ICS.char_array_access := null;

      TC_String              : String(1..5)          := (others => 'X');
      TC_String_1            : constant String       := "*.3*0";
      TC_String_2            : constant String       := "Two";
      TC_String_3            : constant String       := "Five5";
      TC_Blank_String        : constant String(1..5) := (others => ' ');

      TC_char_array          : IC.char_array := 
                                 IC.To_C(TC_Blank_String, True);
      TC_char_array_1        : constant IC.char_array := 
                                 IC.To_C(TC_String_1, True);
      TC_char_array_2        : constant IC.char_array := 
                                 IC.To_C(TC_String_2, True);
      TC_char_array_3        : constant IC.char_array := 
                                 IC.To_C(TC_String_3, True);
      TC_Blank_char_array    : constant IC.char_array := 
                                 IC.To_C(TC_Blank_String, True);

      TC_chars_ptr           : ICS.chars_ptr :=
                                 ICS.New_Char_Array(TC_Blank_char_array); 

      TC_size_t              : IC.size_t := IC.size_t'First;


   begin

      -- Check that the version of Function Value with a chars_ptr parameter
      -- that returns a String result returns an Ada string containing the 
      -- characters pointed to by the chars_ptr parameter, up to (but not 
      -- including) the terminating nul.

      TC_chars_ptr := ICS.New_Char_Array(TC_char_array_1);
      TC_String    := ICS.Value(Item => TC_chars_ptr);

      if TC_String                 /= TC_String_1 or
         TC_String(TC_String'Last)  = ACL1.NUL     
      then
         Report.Failed("Incorrect result from Function Value - 1");
      end if;

      TC_chars_ptr := ICS.New_Char_Array(TC_char_array_2);

      if ICS.Value(Item => TC_chars_ptr) /= 
         IC.To_Ada(ICS.Value(TC_chars_ptr), Trim_Nul => True)
      then
         Report.Failed("Incorrect result from Function Value - 2");
      end if;

      TC_chars_ptr := ICS.New_Char_Array(TC_char_array_3);
      TC_String    := ICS.Value(TC_chars_ptr);

      if TC_String                 /= TC_String_3 or
         TC_String(TC_String'Last)  = ACL1.NUL    
      then
         Report.Failed("Incorrect result from Function Value - 3");
      end if;


      -- Check that the version of Function Value with a chars_ptr parameter
      -- and a size_t parameter that returns a String result returns the
      -- shorter of: 
      --   1) a String of the first size_t number of characters, or 
      --   2) a String of characters up to (but not including) the 
      --      terminating nul.
      --

      -- Case 1 : Length parameter specifies a length shorter than total
      --          length.

      TC_chars_ptr    := ICS.New_Char_Array(TC_char_array_1);
      TC_String       := "XXXXX";  -- Reinitialize all characters in string.
      TC_String(1..5) := ICS.Value(Item => TC_chars_ptr, Length => 6);

      if TC_String(1..4)           /= TC_String_1(1..4) or
         TC_String(TC_String'Last)  = ACL1.NUL          
      then
         Report.Failed("Incorrect result from Function Value - 4");
      end if;

      -- Case 2 : Length parameter specifies total length.

      TC_chars_ptr := ICS.New_Char_Array(TC_char_array_2);

      if ICS.Value(TC_chars_ptr, Length => 5) /= 
         IC.To_Ada(ICS.Value(TC_chars_ptr), Trim_Nul => True)
      then
         Report.Failed("Incorrect result from Function Value - 5");
      end if;

      -- Case 3 : Length parameter specifies a length longer than total
      --          length.

      TC_chars_ptr := ICS.New_Char_Array(TC_char_array_3);
      TC_String    := "XXXXX";  -- Reinitialize all characters in string.
      TC_String    := ICS.Value(TC_chars_ptr, 7);

      if TC_String                 /= TC_String_3 or
         TC_String(TC_String'Last)  = ACL1.NUL    
      then
         Report.Failed("Incorrect result from Function Value - 6");
      end if;


      -- Check that the Function Strlen returns a size_t result that
      -- corresponds to the number of chars in the array pointed to by
      -- parameter Item, up to but not including the terminating nul.

      TC_chars_ptr := ICS.New_Char_Array(IC.To_C("A longer string value"));
      TC_size_t    := ICS.Strlen(TC_chars_ptr);

      if TC_size_t /= 21 then
         Report.Failed("Incorrect result from Function Strlen - 1"); 
      end if;

      TC_chars_ptr := ICS.New_Char_Array(TC_char_array_2);
      TC_size_t    := ICS.Strlen(TC_chars_ptr);

      if TC_size_t /= 3 then  -- Nul not included in length.
         Report.Failed("Incorrect result from Function Strlen - 2"); 
      end if;

      TC_chars_ptr := ICS.New_Char_Array(IC.To_C(""));
      TC_size_t    := ICS.Strlen(TC_chars_ptr);

      if TC_size_t /= 0 then
         Report.Failed("Incorrect result from Function Strlen - 3"); 
      end if;


      -- Check that both of the above versions of Function Value and
      -- function Strlen propagate Dereference_Error if the Item parameter
      -- is Null_Ptr.

      begin
         TC_chars_ptr := ICS.Null_Ptr;
         TC_String    := ICS.Value(Item => TC_chars_ptr);
         Report.Failed("Function Value (without Length parameter) did not " &
                       "raise Dereference_Error when provided a null Item " &
                       "parameter input value");
         if TC_String(1) = '1' then   -- Defeat optimization.
            Report.Comment("Should never be printed");
         end if;
      exception
         when ICS.Dereference_Error => null;  -- OK, expected exception.
         when others                => 
           Report.Failed("Incorrect exception raised by Function Value " &
                         "with Item parameter, when the Item parameter " &
                         "is Null_Ptr");
      end;

      begin
         TC_chars_ptr := ICS.Null_Ptr;
         TC_String    := ICS.Value(Item => TC_chars_ptr, Length => 4);
         Report.Failed("Function Value (with Length parameter) did not "    &
                       "raise Dereference_Error when provided a null Item " &
                       "parameter input value");
         if TC_String(1) = '1' then   -- Defeat optimization.
            Report.Comment("Should never be printed");
         end if;
      exception
         when ICS.Dereference_Error => null;  -- OK, expected exception.
         when others                => 
           Report.Failed("Incorrect exception raised by Function Value " &
                         "with both Item and Length parameters, when "   &
                         "the Item parameter is Null_Ptr");
      end;

      begin
         TC_chars_ptr := ICS.Null_Ptr;
         TC_size_t    := ICS.Strlen(Item => TC_chars_ptr);
         Report.Failed("Function Strlen did not raise Dereference_Error" &
                       "when provided a null Item parameter input value");
         if TC_size_t = 35 then   -- Defeat optimization.
            Report.Comment("Should never be printed");
         end if;
      exception
         when ICS.Dereference_Error => null;  -- OK, expected exception.
         when others                => 
           Report.Failed("Incorrect exception raised by Function Strlen " &
                         "when the Item parameter is Null_Ptr");
      end;


   exception
      when others => Report.Failed ("Exception raised in Test_Block");
   end Test_Block;

   Report.Result;

end CXB3011;
