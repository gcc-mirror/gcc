-- CXB2001.A
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
--      integer types of 8 bits. 
--
-- TEST DESCRIPTION:
--      This test uses the shift and rotate functions of package Interfaces
--      with a modular type representative of 8 bits.  The functions
--      are used as the right hand of assignment statements, as part of
--      conditional statements, and as arguments in other function calls.
--
--      A check is performed in the test to determine whether the bit 
--      ordering method used by the machine/implementation is high-order
--      first ("Big Endian") or low-order first ("Little Endian").  The
--      specific subtests use this information to evaluate the results of
--      each of the functions under test.
--
--      Note: In the string associated with each Report.Failed statement, the
--            acronym BE refers to Big Endian, LE refers to Little Endian.
--
-- APPLICABILITY CRITERIA: 
--      This test is applicable to all implementations that support signed
--      and modular integer types of 8 bits.
--
--       
-- CHANGE HISTORY:
--      21 Aug 95   SAIC    Initial prerelease version.
--      07 May 96   SAIC    Incorporated reviewer comments for ACVC 2.1.
--
--!

with Report;
with Interfaces;
with Ada.Exceptions;

procedure CXB2001 is
begin

   Report.Test ("CXB2001", 
                "Check that subprograms Shift_Left, Shift_Right, "       &
                "Shift_Right_Arithmetic, Rotate_Left, and Rotate_Right " &
                "produce correct results for values of signed and "      &
                "modular integer types of 8 bits");

   Test_Block:
   declare

      use Ada.Exceptions;
      use Interfaces;

      TC_Amount             : Natural     := Natural'First;
      Big_Endian            : Boolean     := False;

      -- Range of type Unsigned_8 is 0..255 (0..Modulus-1).
      TC_Val_Unsigned_8,
      TC_Result_Unsigned_8  : Unsigned_8  := Unsigned_8'First;

   begin

      -- Determine whether the machine uses high-order first or low-order
      -- first bit ordering.
      -- On a high-order first machine, bit zero of a storage element is
      -- the most significant bit (interpreting the sequence of bits that
      -- represent a component as an unsigned integer value).  
      -- On a low-order first machine, bit zero is the least significant.
      -- In this check, a right shift of one place on a Big Endian machine
      -- will yield a result of one, while on a Little Endian machine the
      -- result would be four.

      TC_Val_Unsigned_8 := 2;
      Big_Endian := (Shift_Right(TC_Val_Unsigned_8, 1) = 1);


      -- Note: The shifting and rotating subprograms operate on a bit-by-bit
      --       basis, using the binary representation of the value of the 
      --       operands to yield a binary representation for the result.

      -- Function Shift_Left.

      if Big_Endian then   -- High-order first bit ordering.

         TC_Amount            := 1;
         TC_Val_Unsigned_8    := Unsigned_8'Last;  -- 255.
         TC_Result_Unsigned_8 := Shift_Left(Value  => TC_Val_Unsigned_8, 
                                            Amount => TC_Amount);
         if TC_Result_Unsigned_8 /= 254 then
            Report.Failed("Incorrect result from BE Shift_Left - 1");
         end if;

         if Shift_Left(TC_Val_Unsigned_8, 2) /= 252  or
            Shift_Left(TC_Val_Unsigned_8, 3) /= 248  or
            Shift_Left(TC_Val_Unsigned_8, 5) /= 224  or
            Shift_Left(TC_Val_Unsigned_8, 8) /=   0  or
            Shift_Left(TC_Val_Unsigned_8, 9) /=   0  or
            Shift_Left(TC_Val_Unsigned_8, 0) /= TC_Val_Unsigned_8  
         then
            Report.Failed("Incorrect result from BE Shift_Left - 2");
         end if;

         TC_Val_Unsigned_8 := 1;
         if Shift_Left(TC_Val_Unsigned_8, 1)           /=  2 or
            Shift_Left(TC_Val_Unsigned_8, Amount => 3) /=  8 
         then
            Report.Failed("Incorrect result from BE Shift_Left - 3");
         end if;

         TC_Val_Unsigned_8 := 7;
         if Shift_Left(TC_Val_Unsigned_8, Amount => 4)      /= 112  or
            Shift_Left(Shift_Left(TC_Val_Unsigned_8, 7), 1) /=   0
         then
            Report.Failed("Incorrect result from BE Shift_Left - 4");
         end if;

      else  -- Low-order first bit ordering.

         TC_Amount            := 1;
         TC_Val_Unsigned_8    := Unsigned_8'Last;  -- 255.
         TC_Result_Unsigned_8 := Shift_Left(TC_Val_Unsigned_8, TC_Amount);

         if TC_Result_Unsigned_8 /= 127 then
            Report.Failed("Incorrect result from LE Shift_Left - 1");
         end if;

         if Shift_Left(TC_Val_Unsigned_8, 2) /=  63  or
            Shift_Left(TC_Val_Unsigned_8, 3) /=  31  or
            Shift_Left(TC_Val_Unsigned_8, 5) /=   7  or
            Shift_Left(TC_Val_Unsigned_8, 8) /=   0  or
            Shift_Left(TC_Val_Unsigned_8, 0) /= TC_Val_Unsigned_8  
         then
            Report.Failed("Incorrect result from LE Shift_Left - 2");
         end if;

         TC_Val_Unsigned_8 := 1;
         if Shift_Left(TC_Val_Unsigned_8, 1) /= 0 or
            Shift_Left(TC_Val_Unsigned_8, 7) /= 0  
         then
            Report.Failed("Incorrect result from LE Shift_Left - 3");
         end if;

         TC_Val_Unsigned_8 := 129;
         if Shift_Left(TC_Val_Unsigned_8, 4)                /= 8  or
            Shift_Left(Shift_Left(TC_Val_Unsigned_8, 7), 1) /= 0
         then
            Report.Failed("Incorrect result from LE Shift_Left - 4");
         end if;

      end if;



      -- Function Shift_Right.

      if Big_Endian then   -- High-order first bit ordering.

         TC_Amount            := 1;
         TC_Val_Unsigned_8    := Unsigned_8'Last;  -- 255.
         TC_Result_Unsigned_8 := Shift_Right(TC_Val_Unsigned_8, TC_Amount);

         if TC_Result_Unsigned_8 /= 127 then
            Report.Failed("Incorrect result from BE Shift_Right - 1");
         end if;

         if Shift_Right(TC_Val_Unsigned_8, 2) /=  63  or
            Shift_Right(TC_Val_Unsigned_8, 3) /=  31  or
            Shift_Right(TC_Val_Unsigned_8, 5) /=   7  or
            Shift_Right(TC_Val_Unsigned_8, 8) /=   0  or
            Shift_Right(TC_Val_Unsigned_8, 0) /= TC_Val_Unsigned_8  
         then
            Report.Failed("Incorrect result from BE Shift_Right - 2");
         end if;

         TC_Val_Unsigned_8 := 1;
         if Shift_Right(TC_Val_Unsigned_8, 1) /= 0 or
            Shift_Right(TC_Val_Unsigned_8, 7) /= 0  
         then
            Report.Failed("Incorrect result from BE Shift_Right - 3");
         end if;

         TC_Val_Unsigned_8 := 129;
         if Shift_Right(TC_Val_Unsigned_8, 4)                 /= 8  or
            Shift_Right(Shift_Right(TC_Val_Unsigned_8, 7), 1) /= 0
         then
            Report.Failed("Incorrect result from BE Shift_Right - 4");
         end if;

      else  -- Low-order first bit ordering.

         TC_Amount            := 1;
         TC_Val_Unsigned_8    := Unsigned_8'Last;  -- 255.
         TC_Result_Unsigned_8 := Shift_Right(Value  => TC_Val_Unsigned_8, 
                                             Amount => TC_Amount);
         if TC_Result_Unsigned_8 /= 254 then
            Report.Failed("Incorrect result from LE Shift_Right - 1");
         end if;

         if Shift_Right(TC_Val_Unsigned_8, 2) /= 252  or
            Shift_Right(TC_Val_Unsigned_8, 3) /= 248  or
            Shift_Right(TC_Val_Unsigned_8, 5) /= 224  or
            Shift_Right(TC_Val_Unsigned_8, 8) /=   0  or
            Shift_Right(TC_Val_Unsigned_8, 9) /=   0  or
            Shift_Right(TC_Val_Unsigned_8, 0) /= TC_Val_Unsigned_8  
         then
            Report.Failed("Incorrect result from LE Shift_Right - 2");
         end if;

         TC_Val_Unsigned_8 := 1;
         if Shift_Right(TC_Val_Unsigned_8, 1)           /=  2 or
            Shift_Right(TC_Val_Unsigned_8, Amount => 3) /=  8 
         then
            Report.Failed("Incorrect result from LE Shift_Right - 3");
         end if;

         TC_Val_Unsigned_8 := 7;
         if Shift_Right(TC_Val_Unsigned_8, Amount => 4)       /= 112  or
            Shift_Right(Shift_Right(TC_Val_Unsigned_8, 7), 1) /=   0
         then
            Report.Failed("Incorrect result from LE Shift_Right - 4");
         end if;

      end if;



      -- Tests of Shift_Left and Shift_Right in combination.

      if Big_Endian then   -- High-order first bit ordering.

         TC_Val_Unsigned_8  := 32;

         if Shift_Left(Shift_Right(TC_Val_Unsigned_8, 2), 2) /= 
            TC_Val_Unsigned_8                                       or
            Shift_Left(Shift_Right(TC_Val_Unsigned_8, 1), 3) /= 128 or
            Shift_Right(Shift_Left(TC_Val_Unsigned_8, 2), 6) /=   2 or
            Shift_Right(Shift_Left(TC_Val_Unsigned_8, 2), 8) /=   0
         then
            Report.Failed("Incorrect result from BE Shift_Left - " &
                          "Shift_Right functions used in combination");
         end if;

      else  -- Low-order first bit ordering.

         TC_Val_Unsigned_8  := 32;

         if Shift_Left(Shift_Right(TC_Val_Unsigned_8, 2), 2) /= 
            TC_Val_Unsigned_8                                       or
            Shift_Left(Shift_Right(TC_Val_Unsigned_8, 1), 3) /=   8 or
            Shift_Right(Shift_Left(TC_Val_Unsigned_8, 2), 3) /=  64 or
            Shift_Right(Shift_Left(TC_Val_Unsigned_8, 2), 4) /= 128
         then
            Report.Failed("Incorrect result from LE Shift_Left - " &
                          "Shift_Right functions used in combination");
         end if;

      end if;



      -- Function Shift_Right_Arithmetic.

      if Big_Endian then   -- High-order first bit ordering.

         -- Case where the parameter Value is less than
         -- one half of the modulus.  Zero bits will be shifted in.
         -- Modulus of type Unsigned_8 is 256; half of the modulus is 128.

         TC_Amount            := 1;
         TC_Val_Unsigned_8    := 127;  -- Less than one half of modulus.
         TC_Result_Unsigned_8 := Shift_Right_Arithmetic(TC_Val_Unsigned_8,
                                                        TC_Amount);
         if TC_Result_Unsigned_8 /= 63 then
            Report.Failed
              ("Incorrect result from BE Shift_Right_Arithmetic - 1");
         end if;

         if Shift_Right_Arithmetic(TC_Val_Unsigned_8, 2) /=  31  or
            Shift_Right_Arithmetic(TC_Val_Unsigned_8, 3) /=  15  or
            Shift_Right_Arithmetic(TC_Val_Unsigned_8, 5) /=   3  or
            Shift_Right_Arithmetic(TC_Val_Unsigned_8, 8) /=   0  or
            Shift_Right_Arithmetic(TC_Val_Unsigned_8, 0) /= TC_Val_Unsigned_8  
         then
            Report.Failed
              ("Incorrect result from BE Shift_Right_Arithmetic - 2");
         end if;

         TC_Val_Unsigned_8 := 1;
         if Shift_Right_Arithmetic(TC_Val_Unsigned_8, Amount => 1) /= 0 or
            Shift_Right_Arithmetic(TC_Val_Unsigned_8, 3)           /= 0  
         then
            Report.Failed
              ("Incorrect result from BE Shift_Right_Arithmetic - 3");
         end if;

         -- Case where the parameter Value is greater than or equal to
         -- one half of the modulus.  One bits will be shifted in.
       
         TC_Amount            := 1;
         TC_Val_Unsigned_8    := 128;  -- One half of modulus.
         TC_Result_Unsigned_8 := Shift_Right_Arithmetic(TC_Val_Unsigned_8,
                                                        Amount => TC_Amount);
         if TC_Result_Unsigned_8 /= 192 then
            Report.Failed
              ("Incorrect result from BE Shift_Right_Arithmetic - 4");
         end if;

         TC_Amount            := 1;
         TC_Val_Unsigned_8    := 129;  -- Greater than one half of modulus.
         TC_Result_Unsigned_8 := Shift_Right_Arithmetic(TC_Val_Unsigned_8,
                                                        Amount => TC_Amount);
         if TC_Result_Unsigned_8 /= 192 then
            Report.Failed
              ("Incorrect result from BE Shift_Right_Arithmetic - 5");
         end if;

         if Shift_Right_Arithmetic(TC_Val_Unsigned_8, 2) /= 224  or
            Shift_Right_Arithmetic(TC_Val_Unsigned_8, 3) /= 240  or
            Shift_Right_Arithmetic(TC_Val_Unsigned_8, 5) /= 252  or
            Shift_Right_Arithmetic(TC_Val_Unsigned_8, 7) /= Unsigned_8'Last or
            Shift_Right_Arithmetic(TC_Val_Unsigned_8, 0) /= TC_Val_Unsigned_8  
         then
            Report.Failed
              ("Incorrect result from BE Shift_Right_Arithmetic - 6");
         end if;

         TC_Val_Unsigned_8 := Unsigned_8'Last;
         if Shift_Right_Arithmetic(TC_Val_Unsigned_8, 1) /= 
            Unsigned_8'Last 
         then
            Report.Failed
              ("Incorrect result from BE Shift_Right_Arithmetic - 7");
         end if;

      else  -- Low-order first bit ordering

         -- Case where the parameter Value is less than 
         -- one half of the modulus.  Zero bits will be shifted in.

         TC_Amount            := 1;
         TC_Val_Unsigned_8    := 127;  -- Less than one half of modulus.
         TC_Result_Unsigned_8 := Shift_Right_Arithmetic(TC_Val_Unsigned_8,
                                                        TC_Amount);
         if TC_Result_Unsigned_8 /= 254 then
            Report.Failed
              ("Incorrect result from LE Shift_Right_Arithmetic - 1");
         end if;

         TC_Val_Unsigned_8 := 2;
         if Shift_Right_Arithmetic(TC_Val_Unsigned_8, 2) /=   8  or
            Shift_Right_Arithmetic(TC_Val_Unsigned_8, 3) /=  16  or
            Shift_Right_Arithmetic(TC_Val_Unsigned_8, 5) /=  64  or
            Shift_Right_Arithmetic(TC_Val_Unsigned_8, 8) /=   0  or
            Shift_Right_Arithmetic(TC_Val_Unsigned_8, 0) /= TC_Val_Unsigned_8  
         then
            Report.Failed
              ("Incorrect result from LE Shift_Right_Arithmetic - 2");
         end if;

         TC_Val_Unsigned_8 := 64;
         if Shift_Right_Arithmetic(TC_Val_Unsigned_8, Amount => 1) /= 128 or
            Shift_Right_Arithmetic(TC_Val_Unsigned_8, 3)           /=   0  
         then
            Report.Failed
              ("Incorrect result from LE Shift_Right_Arithmetic - 3");
         end if;

         -- Case where the parameter Value is greater than or equal to
         -- one half of the modulus.  One bits will be shifted in.
       
         TC_Amount            := 1;
         TC_Val_Unsigned_8    := 128;  -- One half of modulus.
         TC_Result_Unsigned_8 := Shift_Right_Arithmetic(TC_Val_Unsigned_8,
                                                        Amount => TC_Amount);

         if TC_Result_Unsigned_8 /= 3 then
            Report.Failed
              ("Incorrect result from LE Shift_Right_Arithmetic - 4");
         end if;

         TC_Amount            := 1;
         TC_Val_Unsigned_8    := 129;  -- Greater than one half of modulus.
         TC_Result_Unsigned_8 := Shift_Right_Arithmetic(TC_Val_Unsigned_8,
                                                        Amount => TC_Amount);

         if TC_Result_Unsigned_8 /= 3 then
            Report.Failed
              ("Incorrect result from LE Shift_Right_Arithmetic - 5");
         end if;

         TC_Val_Unsigned_8    := 135;  -- Greater than one half of modulus.
         if Shift_Right_Arithmetic(TC_Val_Unsigned_8, 2) /= 31  or
            Shift_Right_Arithmetic(TC_Val_Unsigned_8, 3) /= 63  or
            Shift_Right_Arithmetic(TC_Val_Unsigned_8, 5) /= Unsigned_8'Last or
            Shift_Right_Arithmetic(TC_Val_Unsigned_8, 7) /= Unsigned_8'Last or
            Shift_Right_Arithmetic(TC_Val_Unsigned_8, 0) /= TC_Val_Unsigned_8  
         then
            Report.Failed
              ("Incorrect result from LE Shift_Right_Arithmetic - 6");
         end if;

         TC_Val_Unsigned_8 := Unsigned_8'Last;
         if Shift_Right_Arithmetic(TC_Val_Unsigned_8, 1) /= 
            Unsigned_8'Last 
         then
            Report.Failed
              ("Incorrect result from LE Shift_Right_Arithmetic - 7");
         end if;

      end if;



      -- Function Rotate_Left.

      if Big_Endian then  -- High-order first bit ordering.

         TC_Amount            := 1;
         TC_Val_Unsigned_8    := 129;  
         TC_Result_Unsigned_8 := Rotate_Left(Value  => TC_Val_Unsigned_8, 
                                             Amount => TC_Amount);
         if TC_Result_Unsigned_8 /= 3 then
            Report.Failed("Incorrect result from BE Rotate_Left - 1");
         end if;

         if Rotate_Left(TC_Val_Unsigned_8, 2) /=   6  or
            Rotate_Left(TC_Val_Unsigned_8, 3) /=  12  or
            Rotate_Left(TC_Val_Unsigned_8, 5) /=  48  or
            Rotate_Left(TC_Val_Unsigned_8, 8) /= 129  or
            Rotate_Left(TC_Val_Unsigned_8, 0) /= TC_Val_Unsigned_8  
         then
            Report.Failed("Incorrect result from BE Rotate_Left - 2");
         end if;

         TC_Val_Unsigned_8 := 1;
         if Rotate_Left(Value => TC_Val_Unsigned_8, Amount => 1) /= 2 or
            Rotate_Left(TC_Val_Unsigned_8, Amount => 3)          /= 8 
         then 
            Report.Failed("Incorrect result from BE Rotate_Left - 3");
         end if;

         TC_Val_Unsigned_8 := 82;
         if Rotate_Left(TC_Val_Unsigned_8, Amount => 4)       /=  37  or
            Rotate_Left(Rotate_Left(TC_Val_Unsigned_8, 7), 1) /=  82
         then
            Report.Failed("Incorrect result from BE Rotate_Left - 4");
         end if;

      else   -- Low-order first bit ordering.

         TC_Amount            := 1;
         TC_Val_Unsigned_8    := 1;
         TC_Result_Unsigned_8 := Rotate_Left(TC_Val_Unsigned_8, TC_Amount);

         if TC_Result_Unsigned_8 /= 128 then
            Report.Failed("Incorrect result from LE Rotate_Left - 1");
         end if;

         TC_Val_Unsigned_8    := 15;
         if Rotate_Left(TC_Val_Unsigned_8, 2) /=  195  or
            Rotate_Left(TC_Val_Unsigned_8, 3) /=  225  or
            Rotate_Left(TC_Val_Unsigned_8, 5) /=  120  or
            Rotate_Left(TC_Val_Unsigned_8, 8) /= TC_Val_Unsigned_8 or
            Rotate_Left(TC_Val_Unsigned_8, 0) /= TC_Val_Unsigned_8  
         then
            Report.Failed("Incorrect result from LE Rotate_Left - 2");
         end if;

         TC_Val_Unsigned_8 := Unsigned_8'Last;
         if Rotate_Left(TC_Val_Unsigned_8, 1) /= Unsigned_8'Last then
            Report.Failed("Incorrect result from LE Rotate_Left - 3");
         end if;

         TC_Val_Unsigned_8 := 12;
         if Rotate_Left(TC_Val_Unsigned_8, 1) /=   6 or
            Rotate_Left(TC_Val_Unsigned_8, 3) /= 129  
         then
            Report.Failed("Incorrect result from LE Rotate_Left - 4");
         end if;

         TC_Val_Unsigned_8 := 129;
         if Rotate_Left(TC_Val_Unsigned_8, 4)                 /=  24  or
            Rotate_Left(Rotate_Left(TC_Val_Unsigned_8, 7), 1) /= 129
         then
            Report.Failed("Incorrect result from LE Rotate_Left - 5");
         end if;

      end if;



      -- Function Rotate_Right.

      if Big_Endian then  -- High-order first bit ordering.

         TC_Amount            := 1;
         TC_Val_Unsigned_8    := 1;
         TC_Result_Unsigned_8 := Rotate_Right(TC_Val_Unsigned_8, TC_Amount);

         if TC_Result_Unsigned_8 /= 128 then
            Report.Failed("Incorrect result from BE Rotate_Right - 1");
         end if;

         TC_Val_Unsigned_8    := 15;
         if Rotate_Right(TC_Val_Unsigned_8, 2) /=  195  or
            Rotate_Right(TC_Val_Unsigned_8, 3) /=  225  or
            Rotate_Right(TC_Val_Unsigned_8, 5) /=  120  or
            Rotate_Right(TC_Val_Unsigned_8, 8) /= TC_Val_Unsigned_8 or
            Rotate_Right(TC_Val_Unsigned_8, 0) /= TC_Val_Unsigned_8  
         then
            Report.Failed("Incorrect result from BE Rotate_Right - 2");
         end if;

         TC_Val_Unsigned_8 := Unsigned_8'Last;
         if Rotate_Right(TC_Val_Unsigned_8, 1) /= Unsigned_8'Last then
            Report.Failed("Incorrect result from BE Rotate_Right - 3");
         end if;

         TC_Val_Unsigned_8 := 12;
         if Rotate_Right(TC_Val_Unsigned_8, 1) /=   6 or
            Rotate_Right(TC_Val_Unsigned_8, 3) /= 129  
         then
            Report.Failed("Incorrect result from BE Rotate_Right - 4");
         end if;

         TC_Val_Unsigned_8 := 129;
         if Rotate_Right(TC_Val_Unsigned_8, 4)                  /=  24  or
            Rotate_Right(Rotate_Right(TC_Val_Unsigned_8, 7), 1) /= 129
         then
            Report.Failed("Incorrect result from BE Rotate_Right - 5");
         end if;

      else  -- Low-order first bit ordering.

         TC_Amount            := 1;
         TC_Val_Unsigned_8    := 129;  
         TC_Result_Unsigned_8 := Rotate_Right(Value  => TC_Val_Unsigned_8, 
                                              Amount => TC_Amount);
         if TC_Result_Unsigned_8 /= 3 then
            Report.Failed("Incorrect result from LE Rotate_Right - 1");
         end if;

         if Rotate_Right(TC_Val_Unsigned_8, 2) /=   6  or
            Rotate_Right(TC_Val_Unsigned_8, 3) /=  12  or
            Rotate_Right(TC_Val_Unsigned_8, 5) /=  48  or
            Rotate_Right(TC_Val_Unsigned_8, 8) /= 129  or
            Rotate_Right(TC_Val_Unsigned_8, 0) /= TC_Val_Unsigned_8  
         then
            Report.Failed("Incorrect result from LE Rotate_Right - 2");
         end if;

         TC_Val_Unsigned_8 := 1;
         if Rotate_Right(Value => TC_Val_Unsigned_8, Amount => 1) /= 2 or
            Rotate_Right(TC_Val_Unsigned_8, Amount => 3)          /= 8 
         then 
            Report.Failed("Incorrect result from LE Rotate_Right - 3");
         end if;

         TC_Val_Unsigned_8 := 82;
         if Rotate_Right(TC_Val_Unsigned_8, Amount => 4)        /=  37  or
            Rotate_Right(Rotate_Right(TC_Val_Unsigned_8, 7), 1) /=  82
         then
            Report.Failed("Incorrect result from LE Rotate_Right - 4");
         end if;

      end if;



      -- Tests of Rotate_Left and Rotate_Right in combination.

      if Big_Endian then  -- High-order first bit ordering.

         TC_Val_Unsigned_8  := 17;

         if Rotate_Left(Rotate_Right(TC_Val_Unsigned_8, 2), 2) /= 
            TC_Val_Unsigned_8                                        or
            Rotate_Left(Rotate_Right(TC_Val_Unsigned_8, 1), 3) /= 68 or
            Rotate_Right(Rotate_Left(TC_Val_Unsigned_8, 3), 7) /= 17 or
            Rotate_Right(Rotate_Left(TC_Val_Unsigned_8, 2), 8) /= 68 
         then
            Report.Failed("Incorrect result from BE Rotate_Left - " &
                          "Rotate_Right functions used in combination");
         end if;

      else  -- Low-order first bit ordering.

         TC_Val_Unsigned_8 := 4;

         if Rotate_Left(Rotate_Right(TC_Val_Unsigned_8, 2), 2) /= 
            TC_Val_Unsigned_8                                        or
            Rotate_Left(Rotate_Right(TC_Val_Unsigned_8, 1), 3) /=  1 or
            Rotate_Right(Rotate_Left(TC_Val_Unsigned_8, 3), 7) /= 64 or
            Rotate_Right(Rotate_Left(TC_Val_Unsigned_8, 2), 8) /=  1 
         then
            Report.Failed("Incorrect result from LE Rotate_Left - " &
                          "Rotate_Right functions used in combination");
         end if;

      end if;

   exception
      when The_Error : others => 
         Report.Failed ("The following exception was raised in the " &
                        "Test_Block: " & Exception_Name(The_Error));
   end Test_Block;

   Report.Result;

end CXB2001;
