-- CD30003.A
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
--      Check that a Size clause for an object is supported if the specified
--      size is at least as large as the subtype's size, and correspond to a
--      size in storage elements that is a multiple of the object's (non-zero)
--      Alignment. RM 13.3(43)
--
-- TEST DESCRIPTION:
--      This test defines several types and then asserts specific sizes for
--      the, it then checks that the size set is reported back.
--
-- APPLICABILITY CRITERIA:
--      All implementations must attempt to compile this test.
--
--      For implementations validating against Systems Programming Annex (C):
--        this test must execute and report PASSED.
--
--      For implementations not validating against Annex C:
--        this test may report compile time errors at one or more points
--        indicated by "-- ANX-C RQMT", in which case it may be graded as inapplicable.
--        Otherwise, the test must execute and report PASSED.
--
--
-- CHANGE HISTORY:
--      22 JUL 95   SAIC   Initial version
--      08 MAY 96   SAIC   Corrected and strengthened for 2.1
--      14 FEB 97   PWB.CTA Changed 'Size specifications to multiples
--                         of System.Storage_Unit; restricted 'Size spec
--                         for enumeration object to max integer size.
--      16 FEB 98   EDS    Modify Documentation.
--      25 JAN 99   RLB    Repaired to properly set and check sizes.
--      29 JAN 99   RLB    Added Pack pragma needed for some implementations.
--                         Corrected to support a Storage_Unit size < 8.
--!

------------------------------------------------------------------- CD30003

with Report;
with System;
procedure CD30003 is

  ---------------------------------------------------------------------------
  -- types and subtypes
  ---------------------------------------------------------------------------

  type Bit is mod 2**1;
  for Bit'Size use 1;                                         -- ANX-C RQMT.

  type Byte is mod 2**8;
  for Byte'Size use 8;                                        -- ANX-C RQMT.

  type Smallword is mod 2**8;
  for Smallword'size use 16;                                  -- ANX-C RQMT.

  type Byte_Array is array(1..4) of Byte;
  pragma Pack(Byte_Array);                                    -- ANX-C RQMT.
  -- size should be 32

  type Smallword_Array is array(1..4) of Smallword;
  pragma Pack(Smallword_Array); -- Required if Storage_Unit > 16. -- ANX-C RQMT.

  -- Use to calulate maximum required size:
  type Max_Modular is mod System.Max_Binary_Modulus;
  type Max_Integer is range System.Min_Int .. System.Max_Int;
  Enum_Size : constant := Integer'Min (32,
      Integer'Min (Max_Modular'Size, Max_Integer'Size));
  type Transmission_Data is ( Empty, Input, Output, IO, Control );
  for Transmission_Data'Size use Enum_Size;                   -- ANX-C RQMT.

  -- Sizes to try:

  -- The basic sizes are based on a "normal" Storage_Unit = 8 implementation.
  -- We then use formulas to insure that the specified sizes meet the
  -- the minimum level of support and AI-0051.

  Modular_Single_Size : constant := Integer'Min (((8 + (System.Storage_Unit-1))
     /System.Storage_Unit)*System.Storage_Unit, Max_Modular'Size);
     -- Calulate an appropriate, legal, and required to be supported size to
     -- try, which is the size of Byte. Note that object sizes must be
     -- a multiple of the storage unit for the compiler.

  Modular_Double_Size : constant := Integer'Min (((16 + (System.Storage_Unit-1))
     /System.Storage_Unit)*System.Storage_Unit, Max_Modular'Size);

  Modular_Quad_Size : constant := Integer'Min (((32 + (System.Storage_Unit-1))
     /System.Storage_Unit)*System.Storage_Unit, Max_Modular'Size);

  Array_Quad_Size : constant := ((4 * 8 + (System.Storage_Unit-1))
     /System.Storage_Unit)*System.Storage_Unit;

  Array_Octo_Size : constant := ((4 * 16 + (System.Storage_Unit-1))
     /System.Storage_Unit)*System.Storage_Unit;

  Rounded_Enum_Size : constant := ((Enum_Size + (System.Storage_Unit-1))
     /System.Storage_Unit)*System.Storage_Unit;

  Enum_Quad_Size : constant := Integer'Min (((32 + (System.Storage_Unit-1))
     /System.Storage_Unit)*System.Storage_Unit,
     Integer'Min (Max_Modular'Size, Max_Integer'Size));


  ---------------------------------------------------------------------------
  -- objects
  ---------------------------------------------------------------------------

  Bit_8 : Bit :=0;
  for Bit_8'Size use System.Storage_Unit;                     -- ANX-C RQMT.

  Bit_G : Bit :=0;
  for Bit_G'Size use Modular_Double_Size;                     -- ANX-C RQMT.

  Byte_8 : Byte :=0;
  for Byte_8'Size use Modular_Single_Size;                    -- ANX-C RQMT.

  Byte_G : Byte :=0;
  for Byte_G'Size use Modular_Double_Size;                    -- ANX-C RQMT.

  Smallword_1 : Smallword :=0;
  for Smallword_1'Size use Modular_Double_Size;               -- ANX-C RQMT.

  Smallword_2 : Smallword :=0;
  for Smallword_2'Size use Modular_Quad_Size;                 -- ANX-C RQMT.

  Byte_Array_1 : Byte_Array := (others=>0);
  for Byte_Array_1'Size use Array_Quad_Size;                  -- ANX-C RQMT.

  Smallword_Array_1 : Smallword_Array := (others=>0);
  for Smallword_Array_1'Size use Array_Octo_Size;             -- ANX-C RQMT.

  Transmission_Data_1 : aliased Transmission_Data := Empty;

  Transmission_Data_2 : Transmission_Data := Control;
  for Transmission_Data_2'Size use Enum_Quad_Size;            -- ANX-C RQMT.

begin  -- Main test procedure.

  Report.Test ("CD30003", "Check that Size clauses are supported for " &
                          "values at least as large as the subtypes " &
                          "size, and correspond to a size in storage " &
                          "elements that is a multiple of the objects " &
                          "(non-zero) Alignment" );
   
  if Bit_8'Size /= System.Storage_Unit then
    Report.Failed("Expected Bit_8'Size =" & Integer'Image(System.Storage_Unit)
                      & " , actually =" & Integer'Image(Bit_8'Size));
  end if;

  if Bit_G'Size /= Modular_Double_Size then
    Report.Failed("Expected Bit_G'Size =" & Integer'Image(Modular_Double_Size)
                      & " , actually =" & Integer'Image(Bit_G'Size));
  end if;

  if Byte_8'Size /= Modular_Single_Size then
    Report.Failed("Expected Byte_8'Size =" & Integer'Image(Modular_Single_Size)
                      & " , actually =" & Integer'Image(Byte_8'Size));
  end if;

  if Byte_G'Size /= Modular_Double_Size then
    Report.Failed("Expected Bit_G'Size =" & Integer'Image(Modular_Double_Size)
                      & " , actually =" & Integer'Image(Byte_G'Size));
  end if;

  if Smallword_1'Size /= Modular_Double_Size then
    Report.Failed("Expected Smallword_1'Size =" &
                      Integer'Image(Modular_Double_Size) &
                      ", actually =" & Integer'Image(Smallword_1'Size));
  end if;

  if Smallword_2'Size /= Modular_Quad_Size then
    Report.Failed("Expected Smallword_2'Size =" &
                      Integer'Image(Modular_Quad_Size) &
                      ", actually =" & Integer'Image(Smallword_2'Size));
  end if;

  if Byte_Array_1'Size /= Array_Quad_Size then
    Report.Failed("Expected Byte_Array_1'Size =" &
                      Integer'Image(Array_Quad_Size) &
                      ", actually =" & Integer'Image(Byte_Array_1'Size));
  end if;

  if Smallword_Array_1'Size /= Array_Octo_Size then
    Report.Failed(
      "Expected Smallword_Array_1'Size =" &
                      Integer'Image(Array_Octo_Size) &
                      ", actually =" & Integer'Image(Smallword_Array_1'Size));
  end if;

  if Transmission_Data_1'Size /= Enum_Size and then
     Transmission_Data_1'Size /= Rounded_Enum_Size then
    Report.Failed(
      "Expected Transmission_Data_1'Size =" & Integer'Image(Rounded_Enum_Size) &
                      ", actually =" & Integer'Image(Transmission_Data_1'Size));
  end if;

  if Transmission_Data_2'Size /= Enum_Quad_Size then
    Report.Failed(
      "Expected Transmission_Data_2'Size =" & Integer'Image(Enum_Quad_Size) &
                      ", actually =" & Integer'Image(Transmission_Data_2'Size));
  end if;

  Report.Result;

end CD30003;
