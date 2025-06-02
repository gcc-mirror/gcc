-- CXB3008.A
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
--      Check that functions imported from the C language <string.h> and
--      <stdlib.h> libraries can be called from an Ada program.
--      
-- TEST DESCRIPTION:
--      This test checks that C language functions from the <string.h> and
--      <stdlib.h> libraries can be used as completions of Ada subprograms.
--      A pragma Import with convention identifier "C" is used to complete
--      the Ada subprogram specifications.
--      The three subprogram cases tested are as follows:
--      1) A C function that returns an int value (strcpy) is used as the
--         completion of an Ada procedure specification.  The return value
--         is discarded; parameter modification is the desired effect.
--      2) A C function that returns an int value (strlen) is used as the
--         completion of an Ada function specification.
--      3) A C function that returns a double value (strtod) is used as the
--         completion of an Ada function specification.
--
--      This test assumes that the following characters are all included
--      in the implementation defined type Interfaces.C.char:
--      ' ', 'a'..'z', 'A'..'Z', '0'..'9', and '$'.
--      
-- APPLICABILITY CRITERIA: 
--      This test is applicable to all implementations that provide 
--      packages Interfaces.C and Interfaces.C.Strings.  If an 
--      implementation provides these packages, this test must compile, 
--      execute, and report "PASSED".
--
-- SPECIAL REQUIREMENTS:
--      The C language library functions used by this test must be 
--      available for importing into the test.
--
--       
-- CHANGE HISTORY:
--      12 Oct 95   SAIC    Initial prerelease version.
--      09 May 96   SAIC    Incorporated reviewer comments for ACVC 2.1.
--      01 DEC 97   EDS     Replaced all references of C function atof with
--                          C function strtod.
--      29 JUN 98   EDS     Give Ada function corresponding to strtod a 
--                          second parameter.
--!

with Report;
with Ada.Exceptions;
with Interfaces.C;                                            -- N/A => ERROR
with Interfaces.C.Strings;                                    -- N/A => ERROR
with Interfaces.C.Pointers;

procedure CXB3008 is
begin

   Report.Test ("CXB3008", "Check that functions imported from the " &
                           "C language predefined libraries can be " &
                           "called from an Ada program");

   Test_Block:
   declare

      package IC  renames Interfaces.C;
      package ICS renames Interfaces.C.Strings;
      package ICP is new Interfaces.C.Pointers
         ( Index => IC.size_t,
           Element => IC.char,
           Element_Array => IC.char_array,
           Default_Terminator => IC.nul );
      use Ada.Exceptions;

      use type IC.char;
      use type IC.char_array;
      use type IC.size_t;
      use type IC.double;

      -- The String_Copy procedure copies the string pointed to by Source, 
      -- including the terminating nul char, into the char_array pointed
      -- to by Target.

      procedure String_Copy (Target : out IC.char_array;
                             Source : in  IC.char_array);

      -- The String_Length function returns the length of the nul-terminated 
      -- string pointed to by The_String.  The nul is not included in
      -- the count.

      function String_Length (The_String : in IC.char_array) 
        return IC.size_t;

      -- The String_To_Double function converts the char_array pointed to
      -- by The_String into a double value returned through the function
      -- name.  The_String must contain a valid floating-point number; if
      -- not, the value returned is zero.

--      type Acc_ptr is access IC.char_array;
      function String_To_Double (The_String : in IC.char_array ; 
                                 End_Ptr    : ICP.Pointer := null) 
        return IC.double;


      -- Use the <string.h> strcpy function as a completion to the procedure
      -- specification.  Note that the Ada interface to this C function is
      -- in the form of a procedure (C function return value is not used).

      pragma Import (C, String_Copy, "strcpy");

      -- Use the <string.h> strlen function as a completion to the
      -- String_Length function specification.

      pragma Import (C, String_Length, "strlen");

      -- Use the <stdlib.h> strtod function as a completion to the 
      -- String_To_Double function specification.

      pragma Import (C, String_To_Double, "strtod");


      TC_String     : constant String := "Just a Test";
      Char_Source   : IC.char_array(0..30);
      Char_Target   : IC.char_array(0..30);
      Double_Result : IC.double;
      Source_Ptr,
      Target_Ptr    : ICS.chars_ptr;

   begin

      -- Check that the imported version of C function strcpy produces 
      -- the correct results.

      Char_Source(0..21) := "Test of Pragma Import" & IC.nul;

      String_Copy(Char_Target, Char_Source);

      if Char_Target(0..21) /= Char_Source(0..21) then
         Report.Failed("Incorrect result from the imported version of " &
                       "strcpy - 1");
      end if;

      if String_Length(Char_Target) /= 21 then
         Report.Failed("Incorrect result from the imported version of " &
                       "strlen - 1");
      end if;

      Char_Source(0) := IC.nul;

      String_Copy(Char_Target, Char_Source);

      if Char_Target(0) /= Char_Source(0) then
         Report.Failed("Incorrect result from the imported version of " &
                       "strcpy - 2");
      end if;

      if String_Length(Char_Target) /= 0 then
         Report.Failed("Incorrect result from the imported version of " &
                       "strlen - 2");
      end if;

      -- The following chars_ptr designates a char_array of 12 chars 
      -- (including the terminating nul char).
      Source_Ptr := ICS.New_Char_Array(IC.To_C(TC_String));  

      String_Copy(Char_Target, ICS.Value(Source_Ptr));

      Target_Ptr := ICS.New_Char_Array(Char_Target);

      if ICS.Value(Target_Ptr) /= TC_String then
         Report.Failed("Incorrect result from the imported version of " &
                       "strcpy - 3");
      end if;
         
      if String_Length(ICS.Value(Target_Ptr)) /= TC_String'Length then
         Report.Failed("Incorrect result from the imported version of " &
                       "strlen - 3");
      end if;


      Char_Source(0..9) := "100.00only";

      Double_Result := String_To_Double(Char_Source);

      Char_Source(0..13) := "5050.00$$$$$$$";

      if Double_Result + String_To_Double(Char_Source) /= 5150.00 then
         Report.Failed("Incorrect result returned from the imported " &
                       "version of function strtod - 1");
      end if;

      Char_Source(0..9) := "xxx$10.00x";  -- String doesn't contain a
                                          -- valid floating point value.
      if String_To_Double(Char_Source) /= 0.0 then
         Report.Failed("Incorrect result returned from the imported " &
                       "version of function strtod - 2");
      end if;


   exception
      when The_Error : others => 
         Report.Failed ("The following exception was raised in the " &
                        "Test_Block: " & Exception_Name(The_Error));
   end Test_Block;

   Report.Result;

end CXB3008;
