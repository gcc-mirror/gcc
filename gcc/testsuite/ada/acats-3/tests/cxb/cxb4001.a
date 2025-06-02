-- CXB4001.A
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
--      Check that the specifications of the package Interfaces.COBOL
--      are available for use
--
-- TEST DESCRIPTION: 
--      This test verifies that the type and the subprograms specified for
--      the interface are present.
--
-- APPLICABILITY CRITERIA: 
--      This test is applicable to all implementations that provide 
--      package Interfaces.COBOL.  If an implementation provides
--      package Interfaces.COBOL, this test must compile, execute, and 
--      report "PASSED".
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      15 Nov 95   SAIC    Corrected visibility errors for ACVC 2.0.1.
--      28 Feb 96   SAIC    Added applicability criteria.
--      27 Oct 96   SAIC    Incorporated reviewer comments.
--      01 DEC 97   EDS     Change "To_Comp" to "To_Binary".
--!

with Report;
with Interfaces.COBOL;                                        -- N/A => ERROR

procedure CXB4001 is

   package COBOL renames Interfaces.COBOL;
   use type COBOL.Byte;
   use type COBOL.Decimal_Element;

begin

   Report.Test ("CXB4001", "Check the specification of Interfaces.COBOL");


   declare  -- encapsulate the test

      --  Types and operations for internal data representations
 
      TST_Floating : COBOL.Floating;
      TST_Long_Floating : COBOL.Long_Floating;

      TST_Binary        : COBOL.Binary;
      TST_Long_Binary   : COBOL.Long_Binary;

      TST_Max_Digits_Binary      : constant := COBOL.Max_Digits_Binary; 
      TST_Max_Digits_Long_Binary : constant := COBOL.Max_Digits_Long_Binary;

      TST_Decimal_Element : COBOL.Decimal_Element;

      TST_Packed_Decimal : COBOL.Packed_Decimal (1..5) := 
                                    (others => COBOL.Decimal_Element'First);

      --  initialize it so it can reasonably be used later
      TST_COBOL_Character : COBOL.COBOL_Character :=
                                                COBOL.COBOL_Character'First;

      TST_Ada_To_COBOL : COBOL.COBOL_Character :=  
                                          COBOL.Ada_To_COBOL (Character'First);

      TST_COBOL_To_Ada : Character :=  
                              COBOL.COBOL_To_Ada (COBOL.COBOL_Character'First);

      --  assignment to make sure it is an array of  COBOL_Character
      TST_Alphanumeric : COBOL.Alphanumeric (1..5) := 
                                             (others => TST_COBOL_Character);

        
      --  assignment to make sure it is an array of  COBOL_Character
      TST_Numeric : COBOL.Numeric (1..5) := (others => TST_COBOL_Character);


      procedure Collect_All_Calls is

         CAC_Alphanumeric : COBOL.Alphanumeric(1..5) :=
                              COBOL.To_COBOL("abcde");
         CAC_String       : String (1..5) := "vwxyz";
         CAC_Natural      : natural       := 0;

      begin
   
         CAC_Alphanumeric := COBOL.To_COBOL (CAC_String);
         CAC_String := COBOL.To_Ada  (CAC_Alphanumeric);

         COBOL.To_COBOL (CAC_String, CAC_Alphanumeric, CAC_Natural);
         COBOL.To_Ada (CAC_Alphanumeric, CAC_String, CAC_Natural);

         raise COBOL.Conversion_Error;

      end Collect_All_Calls;



      --  Formats for COBOL data representations

      TST_Unsigned   : COBOL.Display_Format := COBOL.Unsigned;
      TST_Leading_Separate : COBOL.Display_Format := COBOL.Leading_Separate;
      TST_Trailing_Separate : COBOL.Display_Format := COBOL.Trailing_Separate;
      TST_Leading_Nonseparate  : COBOL.Display_Format :=  
                                                   COBOL.Leading_Nonseparate;
      TST_Trailing_Nonseparate : COBOL.Display_Format :=  
                                                   COBOL.Trailing_Nonseparate;


      TST_High_Order_First  : COBOL.Binary_Format := COBOL.High_Order_First;
      TST_Low_Order_First   : COBOL.Binary_Format := COBOL.Low_Order_First;
      TST_Native_Binary     : COBOL.Binary_Format := COBOL.Native_Binary;


      TST_Packed_Unsigned   : COBOL.Packed_Format := COBOL.Packed_Unsigned;
      TST_Packed_Signed     : COBOL.Packed_Format := COBOL.Packed_Signed;


      --  Types for external representation of COBOL binary data

      TST_Byte_Array : COBOL.Byte_Array(1..5) := (others => COBOL.Byte'First);

      -- Now instantiate one version of the generic
      --
      type bx4001_Decimal is delta 0.1 digits 5;
      package bx4001_conv is new COBOL.Decimal_Conversions (bx4001_Decimal);

      procedure Collect_All_Generic_Calls is
         CAGC_natural        : natural;
         CAGC_Display_Format : COBOL.Display_Format;
         CAGC_Boolean        : Boolean;
         CAGC_Numeric        : COBOL.Numeric(1..5);
         CAGC_Num            : bx4001_Decimal;
         CAGC_Packed_Decimal : COBOL.Packed_Decimal (1..5);
         CAGC_Packed_Format  : COBOL.Packed_Format; 
         CAGC_Byte_Array     : COBOL.Byte_Array (1..5);
         CAGC_Binary_Format  : COBOL.Binary_Format;
         CAGC_Binary         : COBOL.Binary;
         CAGC_Long_Binary    : COBOL.Long_Binary;
      begin

         --  Display Formats: data values are represented as Numeric
         
         CAGC_Boolean := bx4001_conv.Valid (CAGC_Numeric, CAGC_Display_Format);
         CAGC_Natural := bx4001_conv.Length (CAGC_Display_Format);

         CAGC_Num := bx4001_conv.To_Decimal 
                                       (CAGC_Numeric, CAGC_Display_Format);
         CAGC_Numeric := bx4001_conv.To_Display 
                                       (CAGC_Num, CAGC_Display_Format);


         --  Packed Formats: data values are represented as Packed_Decimal
         
         CAGC_Boolean := bx4001_conv.Valid 
                                    (CAGC_Packed_Decimal, CAGC_Packed_Format);

         CAGC_Natural := bx4001_conv.Length (CAGC_Packed_Format);

         CAGC_Num := bx4001_conv.To_Decimal 
                                    (CAGC_Packed_Decimal, CAGC_Packed_Format);

         CAGC_Packed_Decimal := bx4001_conv.To_Packed 
                                    (CAGC_Num, CAGC_Packed_Format);


         --  Binary Formats: external data values are represented as 
         --  Byte_Array

         CAGC_Boolean := bx4001_conv.Valid 
                                    (CAGC_Byte_Array, CAGC_Binary_Format);
         
         CAGC_Natural := bx4001_conv.Length (CAGC_Binary_Format);
         CAGC_Num := bx4001_conv.To_Decimal 
                                    (CAGC_Byte_Array, CAGC_Binary_Format);

         CAGC_Byte_Array := bx4001_conv.To_Binary (CAGC_Num, CAGC_Binary_Format);


         --  Internal Binary formats: data values are of type 
         --  Binary/Long_Binary

         CAGC_Num := bx4001_conv.To_Decimal (CAGC_Binary);
         CAGC_Num := bx4001_conv.To_Decimal (CAGC_Long_Binary);

         CAGC_Binary       := bx4001_conv.To_Binary (CAGC_Num);
         CAGC_Long_Binary  := bx4001_conv.To_Long_Binary (CAGC_Num);
         

      end Collect_All_Generic_Calls;


   begin    -- encapsulation

      if COBOL.Byte'First /= 0     or
         COBOL.Byte'Last  /=  (2 ** COBOL.COBOL_Character'Size) - 1 then
            Report.Failed ("Byte is incorrectly defined");
      end if;

      if  COBOL.Decimal_Element'First /= 0 then
         Report.Failed ("Decimal_Element is incorrectly defined");
      end if;

   end;     -- encapsulation

   Report.Result;

end CXB4001;
