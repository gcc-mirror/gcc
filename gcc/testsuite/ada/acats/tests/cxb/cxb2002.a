-- CXB2002.A
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
--      Check that subprograms Shift_Left, Shift_Right, 
--      Shift_Right_Arithmetic, Rotate_Left, and Rotate_Right are available
--      and produce correct results for values of signed and modular
--      integer types of 16 bits.  
--
-- TEST DESCRIPTION:
--      This test uses the shift and rotate functions of package Interfaces
--      with a modular type representative of 16 bits.  The functions
--      are used as the right hand of assignment statements, as part of
--      conditional statements, and as arguments in other function calls.
--
-- APPLICABILITY CRITERIA: 
--      This test is applicable to all implementations that support signed
--      and modular integer types of 16 bits.
--
--       
-- CHANGE HISTORY:
--      21 Aug 95   SAIC    Initial prerelease version.
--      07 May 96   SAIC    Incorporated reviewer comments for ACVC 2.1.
--      26 Oct 96   SAIC    Removed subtests based on Big/Little Endian.
--      17 Feb 97   PWB.CTA Corrected "-" to "+" in parenthesized expressions.
--!

with Report;
with Interfaces;
with Ada.Exceptions;

procedure CXB2002 is
begin

   Report.Test ("CXB2002", 
                "Check that subprograms Shift_Left, Shift_Right, "       &
                "Shift_Right_Arithmetic, Rotate_Left, and Rotate_Right " &
                "produce correct results for values of signed and "      &
                "modular integer types of 16 bits");

   Test_Block:
   declare

      use Ada.Exceptions;
      use Interfaces;

      TC_Amount             : Natural     := Natural'First;

      -- Range of type Unsigned_16 is 0..65535 (0..Modulus-1).
      TC_Val_Unsigned_16,
      TC_Result_Unsigned_16 : Unsigned_16 := Unsigned_16'First;

   begin

      -- Note: The shifting and rotating subprograms operate on a bit-by-bit
      --       basis, using the binary representation of the value of the 
      --       operands to yield a binary representation for the result.

      -- Function Shift_Left.

         TC_Amount             := 3;
         TC_Val_Unsigned_16    := Unsigned_16'Last;  -- 65535.
         TC_Result_Unsigned_16 := Shift_Left(TC_Val_Unsigned_16, TC_Amount);

         if TC_Result_Unsigned_16 /= Unsigned_16'Last - (2**0 + 2**1 + 2**2)
         then
            Report.Failed("Incorrect result from Shift_Left - 1");
         end if;

         if Shift_Left(TC_Val_Unsigned_16,  0) /= Unsigned_16'Last  or
            Shift_Left(TC_Val_Unsigned_16,  5) /= 
            Unsigned_16'Last - (2**0 + 2**1 + 2**2 + 2**3 +2**4)    or
            Shift_Left(TC_Val_Unsigned_16, 16) /=     0  
         then
            Report.Failed("Incorrect result from Shift_Left - 2");
         end if;


      -- Function Shift_Right.

         TC_Amount             := 3;
         TC_Val_Unsigned_16    := Unsigned_16'Last;  -- 65535.
         TC_Result_Unsigned_16 := Shift_Right(Value  => TC_Val_Unsigned_16, 
                                              Amount => TC_Amount);

         if TC_Result_Unsigned_16 /= Unsigned_16'Last-(2**15 + 2**14 + 2**13)
         then
            Report.Failed("Incorrect result from Shift_Right - 1");
         end if;

         if Shift_Right(TC_Val_Unsigned_16,  0) /= Unsigned_16'Last  or
            Shift_Right(TC_Val_Unsigned_16,  5) /= 
            Unsigned_16'Last-(2**15 + 2**14 + 2**13 + 2**12 + 2**11) or
            Shift_Right(TC_Val_Unsigned_16, 16) /=    0  
         then
            Report.Failed("Incorrect result from Shift_Right - 2");
         end if;


      -- Tests of Shift_Left and Shift_Right in combination.

         TC_Val_Unsigned_16 := Unsigned_16'Last;

         if Shift_Left(Shift_Right(TC_Val_Unsigned_16, 4),  4) /= 
            Unsigned_16'Last-(2**0 + 2**1 + 2**2 + 2**3)         or
            Shift_Left(Shift_Right(TC_Val_Unsigned_16, 1),  3) /= 
            Unsigned_16'Last-(2**0 + 2**1 + 2**2)                or
            Shift_Right(Shift_Left(TC_Val_Unsigned_16, 2),  4) /=  
            Unsigned_16'Last-(2**15+ 2**14 + 2**13 + 2**12)      or
            Shift_Right(Shift_Left(TC_Val_Unsigned_16, 2), 16) /= 0
         then
            Report.Failed("Incorrect result from Shift_Left - " &
                          "Shift_Right functions used in combination");
         end if;


      -- Function Shift_Right_Arithmetic.

         -- Case where the parameter Value is less than 
         -- one half of the modulus.  Zero bits will be shifted in.
         -- Modulus of type Unsigned_16 is 2**16; one half is 2**15.

         TC_Amount             := 3;
         TC_Val_Unsigned_16    := 2**15 - 1; -- Less than one half of modulus.
         TC_Result_Unsigned_16 := Shift_Right_Arithmetic(TC_Val_Unsigned_16,
                                                         TC_Amount);
         if TC_Result_Unsigned_16 /= 
            TC_Val_Unsigned_16 - (2**14 + 2**13 + 2**12) 
         then
            Report.Failed
              ("Incorrect result from Shift_Right_Arithmetic - 1");
         end if;

         if Shift_Right_Arithmetic(TC_Val_Unsigned_16, 0)  /= 
            TC_Val_Unsigned_16                                           or
            Shift_Right_Arithmetic(TC_Val_Unsigned_16, 5)  /=
            TC_Val_Unsigned_16 - (2**14 + 2**13 + 2**12 + 2**11 + 2**10) or 
            Shift_Right_Arithmetic(TC_Val_Unsigned_16, 16) /=    0  
         then
            Report.Failed
              ("Incorrect result from Shift_Right_Arithmetic - 2");
         end if;

         -- Case where the parameter Value is greater than or equal to
         -- one half of the modulus.  One bits will be shifted in.

         TC_Amount             := 1;
         TC_Val_Unsigned_16    := 2**15;  -- One half of modulus.
         TC_Result_Unsigned_16 := Shift_Right_Arithmetic(TC_Val_Unsigned_16,
                                                         TC_Amount);
         if TC_Result_Unsigned_16 /= TC_Val_Unsigned_16 + 2**14 then
            Report.Failed
              ("Incorrect result from Shift_Right_Arithmetic - 3");
         end if;

         TC_Amount             := 1;
         TC_Val_Unsigned_16    := 2**15 + 1; -- Greater than half of modulus.
         TC_Result_Unsigned_16 := Shift_Right_Arithmetic(TC_Val_Unsigned_16,
                                                         TC_Amount);
         if TC_Result_Unsigned_16 /= TC_Val_Unsigned_16 + 2**14 - 2**0 then
            Report.Failed
              ("Incorrect result from Shift_Right_Arithmetic - 4");
         end if;

         if Shift_Right_Arithmetic(TC_Val_Unsigned_16, 0)  /= 
            TC_Val_Unsigned_16                                        or
            Shift_Right_Arithmetic(TC_Val_Unsigned_16, 4)  /= 
            TC_Val_Unsigned_16 - 2**0 + 2**14 + 2**13 + 2**12 + 2**11 or
            Shift_Right_Arithmetic(TC_Val_Unsigned_16, 16) /= Unsigned_16'Last
         then
            Report.Failed
              ("Incorrect result from Shift_Right_Arithmetic - 5");
         end if;


      -- Function Rotate_Left.

         TC_Amount             := 3;
         TC_Val_Unsigned_16    := Unsigned_16'Last;  -- 65535.
         TC_Result_Unsigned_16 := Rotate_Left(Value  => TC_Val_Unsigned_16, 
                                              Amount => TC_Amount);
         if TC_Result_Unsigned_16 /= Unsigned_16'Last then
            Report.Failed("Incorrect result from Rotate_Left - 1");
         end if;

         TC_Val_Unsigned_16    := 2**15 + 2**14 + 2**1 + 2**0;
         if Rotate_Left(TC_Val_Unsigned_16,  0) /= 
            2**15 + 2**14 + 2**1 + 2**0            or
            Rotate_Left(TC_Val_Unsigned_16,  5) /=    
            2**6 + 2**5 + 2**4 + 2**3              or
            Rotate_Left(TC_Val_Unsigned_16, 16) /= TC_Val_Unsigned_16  
         then
            Report.Failed("Incorrect result from Rotate_Left - 2");
         end if;


      -- Function Rotate_Right.

         TC_Amount             := 1;
         TC_Val_Unsigned_16    := 2**1 + 2**0;
         TC_Result_Unsigned_16 := Rotate_Right(Value  => TC_Val_Unsigned_16, 
                                               Amount => TC_Amount);
         if TC_Result_Unsigned_16 /= 2**15 + 2**0 then
            Report.Failed("Incorrect result from Rotate_Right - 1");
         end if;

         if Rotate_Right(TC_Val_Unsigned_16,  0) /= 2**1 + 2**0   or
            Rotate_Right(TC_Val_Unsigned_16,  5) /= 2**12 + 2**11 or
            Rotate_Right(TC_Val_Unsigned_16, 16) /= 2**1 + 2**0 
         then
            Report.Failed("Incorrect result from Rotate_Right - 2");
         end if;


      -- Tests of Rotate_Left and Rotate_Right in combination.

         TC_Val_Unsigned_16 := 32769;

         if Rotate_Left(Rotate_Right(TC_Val_Unsigned_16, 4),  3) /= 49152 or
            Rotate_Left(Rotate_Right(TC_Val_Unsigned_16, 1),  3) /=     6 or
            Rotate_Right(Rotate_Left(TC_Val_Unsigned_16, 3),  7) /=  6144 or
            Rotate_Right(Rotate_Left(TC_Val_Unsigned_16, 1), 16) /=     3
         then
            Report.Failed("Incorrect result from Rotate_Left - " &
                          "Rotate_Right functions used in combination");
         end if;


   exception
      when The_Error : others => 
         Report.Failed ("The following exception was raised in the " &
                        "Test_Block: " & Exception_Name(The_Error));
   end Test_Block;

   Report.Result;

end CXB2002;
