-- CXB30132.AM
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
--      Check that imported, user-defined C language functions can be 
--      called from an Ada program.
--      
-- TEST DESCRIPTION:
--      This test checks that user-defined C language functions can be
--      imported and referenced from an Ada program.  Two C language
--      functions are specified in files CXB30130.C and CXB30131.C.  
--      These two functions are imported to this test program, using two
--      calls to Pragma Import.  Each function is then called in this test,
--      and the results of the call are verified.
--      
--      This test assumes that the following characters are all included
--      in the implementation defined type Interfaces.C.char:
--      ' ', 'a'..'z', and 'A'..'Z'.
--      
-- APPLICABILITY CRITERIA: 
--      This test is applicable to all implementations that provide 
--      packages Interfaces.C and Interfaces.C.Strings.  If an 
--      implementation provides packages Interfaces.C and 
--      Interfaces.C.Strings, this test must compile, execute, and 
--      report "PASSED".
--
-- SPECIAL REQUIREMENTS:
--      The files CXB30130.C and CXB30131.C must be compiled with a C 
--      compiler.  Implementation dialects of C may require alteration of 
--      the C program syntax (see individual C files).
--     
--      Note that the compiled C code must be bound with the compiled Ada
--      code to create an executable image.  An implementation must provide
--      the necessary commands to accomplish this.
--     
--      Note that the C code included in CXB30130.C and CXB30131.C conforms
--      to ANSI-C.  Modifications to these files may be required for other
--      C compilers.  An implementation must provide the necessary 
--      modifications to satisfy the function requirements.
--     
-- TEST FILES:
--      The following files comprise this test:
--
--         CXB30130.C
--         CXB30131.C
--         CXB30132.AM
--
--       
-- CHANGE HISTORY:
--      13 Oct 95   SAIC    Initial prerelease version.
--      13 May 96   SAIC    Incorporated reviewer comments for ACVC 2.1.
--      26 Oct 96   SAIC    Incorporated reviewer comments.
--
--!

with Report;
with Impdef;
with Interfaces.C;                                            -- N/A => ERROR
with Interfaces.C.Strings;                                    -- N/A => ERROR

procedure CXB30132 is
begin

   Report.Test ("CXB3013", "Check that user-defined C functions can " &
                           "be imported into an Ada program");

   Test_Block:
   declare

      package IC  renames Interfaces.C;
      package ICS renames Interfaces.C.Strings;

      use type IC.char_array;
      use type IC.int;
      use type IC.short;
      use type IC.C_float;
      use type IC.double;

      type Short_Ptr          is access all IC.short;
      type Float_Ptr          is access all IC.C_float;
      type Double_Ptr         is access all IC.double;
      subtype Char_Array_Type is IC.char_array(0..20);

      TC_Default_int      : IC.int             :=   49;
      TC_Default_short    : IC.short           :=    3;
      TC_Default_float    : IC.C_float         :=   50.0;
      TC_Default_double   : IC.double          := 1209.0; 

      An_Int_Value        : IC.int             := TC_Default_int;
      A_Short_Value       : aliased IC.short   := TC_Default_short;
      A_Float_Value       : aliased IC.C_float := TC_Default_float; 
      A_Double_Value      : aliased IC.double  := TC_Default_double;

      A_Short_Int_Pointer : Short_Ptr          := A_Short_Value'access;
      A_Float_Pointer     : Float_Ptr          := A_Float_Value'access;
      A_Double_Pointer    : Double_Ptr         := A_Double_Value'access;

      Char_Array_1        : Char_Array_Type;
      Char_Array_2        : Char_Array_Type;
      Char_Pointer        : ICS.chars_ptr;

      TC_Char_Array       : constant Char_Array_Type := 
                              "Look before you leap" & IC.nul;
      TC_Return_int       : IC.int := 0;

      -- The Square_It function returns the square of the value The_Int 
      -- through the function name, and returns the square of the other
      -- parameters through the parameter list (the last three parameters 
      -- are access values).

      function Square_It (The_Int    : in IC.int;
                          The_Short  : in Short_Ptr;
                          The_Float  : in Float_Ptr;
                          The_Double : in Double_Ptr) return IC.int;

      -- The Combine_Strings function returns the result of the catenation
      -- of the two string parameters through the function name.

      function Combine_Strings (First_Part  : in IC.char_array;
                                Second_Part : in IC.char_array) 
        return ICS.chars_ptr;


      -- Use the user-defined C function square_it as a completion to the
      -- function specification above.

     pragma Import (Convention    => C, 
                    Entity        => Square_It, 
                    External_Name => Impdef.CXB30130_External_Name);

      -- Use the user-defined C function combine_two_strings as a completion
      -- to the function specification above.

     pragma Import (C, Combine_Strings, Impdef.CXB30131_External_Name);


   begin

      -- Check that the imported version of C function CXB30130 produces 
      -- the correct results.

      TC_Return_int := Square_It (The_Int    => An_Int_Value,
                                  The_Short  => A_Short_Int_Pointer,
                                  The_Float  => A_Float_Pointer,
                                  The_Double => A_Double_Pointer);

      -- Compare the results with the expected results.  Note that in the
      -- case of the three "pointer" parameters, the objects being pointed
      -- to have been modified as a result of the function.

      if TC_Return_int           /= An_Int_Value      * An_Int_Value      or
         A_Short_Int_Pointer.all /= TC_Default_short  * TC_Default_Short  or
         A_Short_Value           /= TC_Default_short  * TC_Default_Short  or
         A_Float_Pointer.all     /= TC_Default_float  * TC_Default_float  or
         A_Float_Value           /= TC_Default_float  * TC_Default_float  or
         A_Double_Pointer.all    /= TC_Default_double * TC_Default_double or
         A_Double_Value          /= TC_Default_double * TC_Default_double 
      then
         Report.Failed("Incorrect results returned from function square_it");
      end if;


      -- Check that two char_array values are combined by the imported 
      -- C function CXB30131.

      Char_Array_1(0..12) := "Look before " & IC.nul;
      Char_Array_2(0..8)  := "you leap"     & IC.nul;

      Char_Pointer := Combine_Strings (Char_Array_1, Char_Array_2);

      if ICS.Value(Char_Pointer) /= TC_Char_Array then
         Report.Failed("Incorrect value returned from imported function " &
                       "combine_two_strings");
      end if;


   exception
      when others => Report.Failed ("Exception raised in Test_Block");
   end Test_Block;

   Report.Result;

end CXB30132;
