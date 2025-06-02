-- CD92001.A
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
--      Check that if X denotes a scalar object, X'Valid
--      yields true if an only if the object denoted by X is normal and
--      has a valid representation.
--
-- TEST DESCRIPTION:
--      Using Unchecked_Conversion, Image and Value attributes, combined
--      with string manipulation, cause valid and invalid values to be
--      stored in various objects.  Check their validity with the
--      attribute 'Valid.  Invalid objects are created in a loop which
--      performs a simplistic check to ensure that the values being used
--      are indeed not valid, then assigns the value using an instance of
--      Unchecked_Conversion.  The creation of the tables of valid values
--      is trivial.
--
-- APPLICABILITY CRITERIA:
--      All implementations must attempt to compile this test.
--
--      For implementations validating against Systems Programming Annex (C):
--        this test must execute and report PASSED.
--
--      For implementations not validating against Annex C:
--        this test may report compile time errors at one or more points
--        indicated by "-- N/A => ERROR", in which case it may be graded as
--        inapplicable. Otherwise, the test must execute and report PASSED.
--
--
-- CHANGE HISTORY:
--      10 MAY 95   SAIC    Initial version
--      07 MAY 96   SAIC    Changed U_C to Ada.U_C for 2.1
--      05 JAN 99   RLB     Added Component_Size clauses to compensate
--                          for the fact that there is no required size
--                          for either the enumeration or modular components.
--!

with Report;
with Ada.Unchecked_Conversion;
with System;
procedure CD92001 is

  type Sparse_Enumerated is
       ( Help, Home, Page_Up, Del, EndK,
         Page_Down, Up, Left, Down, Right );

  for Sparse_Enumerated use ( Help      =>    2,
                              Home      =>    4,
                              Page_Up   =>    8,
                              Del       =>   16,
                              EndK      =>   32,
                              Page_Down =>   64,
                              Up        =>  128,
                              Left      =>  256,
                              Down      =>  512,
                              Right     => 1024 );

  type Mod_10 is mod 10;

  type Default_Enumerated is ( Zero,  One, Two,   Three, Four,
                               Five,  Six, Seven, Eight, Nine,
                               Clear, '=', '/',   '*',   '-',
                               '+',   Enter );
    for Default_Enumerated'Size use 8;

  Default_Enumerated_Count : constant := 17;

  type Mod_By_Enum_Items is mod Default_Enumerated_Count;

  type Mod_Same_Size_As_Sparse_Enum is mod 2**12;
                                        -- Sparse_Enumerated 'Size;

  type Mod_Same_Size_As_Def_Enum is mod 2**8;
                                     -- Default_Enumerated'Size;

  subtype Test_Width is Positive range 1..100;

  -- Note: There is no required relationship between 'Size and 'Component_Size,
  -- so we must use component_size clauses here.
  -- We use the following expressions to insure that the component size is a
  -- multiple of the Storage_Unit.
  Sparse_Component_Size : constant := ((Sparse_Enumerated'Size / System.Storage_Unit) +
        Boolean'Pos((Sparse_Enumerated'Size mod System.Storage_Unit) /= 0)) *
        System.Storage_Unit;
  Default_Component_Size : constant := ((Default_Enumerated'Size / System.Storage_Unit) +
        Boolean'Pos((Sparse_Enumerated'Size mod System.Storage_Unit) /= 0)) *
        System.Storage_Unit;

  type Sparse_Enum_Table is array(Test_Width) of Sparse_Enumerated;
  for Sparse_Enum_Table'Component_Size use Sparse_Component_Size;  -- N/A => ERROR.
  type Def_Enum_Table is array(Test_Width) of Default_Enumerated;
  for Def_Enum_Table'Component_Size use Default_Component_Size;  -- N/A => ERROR.

  type Sparse_Mod_Table  is
       array(Test_Width) of Mod_Same_Size_As_Sparse_Enum;
  for Sparse_Mod_Table'Component_Size use Sparse_Component_Size;  -- N/A => ERROR.

  type Default_Mod_Table is
       array(Test_Width) of Mod_Same_Size_As_Def_Enum;
  for Default_Mod_Table'Component_Size use Default_Component_Size;  -- N/A => ERROR.

  function UC_Sparse_Mod_Enum is
    new Ada.Unchecked_Conversion( Sparse_Mod_Table, Sparse_Enum_Table );

  function UC_Def_Mod_Enum is
    new Ada.Unchecked_Conversion( Default_Mod_Table, Def_Enum_Table );

  Valid_Sparse_Values : Sparse_Enum_Table;
  Valid_Def_Values    : Def_Enum_Table;

  Sample_Enum_Value_Table : Sparse_Mod_Table;
  Sample_Def_Value_Table  : Default_Mod_Table;


  -- fill the Valid tables with valid values for conversion
  procedure Fill_Valid is
    K : Mod_10 := 0;
    P : Mod_By_Enum_Items := 0;
  begin
    for I in Test_Width loop
      Valid_Sparse_Values(I) := Sparse_Enumerated'Val( K );
      Valid_Def_Values(I)    := Default_Enumerated'Val( Integer(P) );
      K := K +1;
      P := P +1;
    end loop;
  end Fill_Valid;

  -- fill the Sample tables with invalid values for conversion
  procedure Fill_Invalid is
    K : Mod_Same_Size_As_Sparse_Enum := 1;
    P : Mod_Same_Size_As_Def_Enum    := 1;
  begin
    for I in Test_Width loop
      K := K +13;
      if K mod 2 = 0 then  -- oops, that would be a valid value
        K := K +1;
      end if;
      if P = Mod_Same_Size_As_Def_Enum'Last
         or P < Default_Enumerated_Count then -- that would be valid
        P := Default_Enumerated_Count + 1;
      else
        P := P +1;
      end if;
      Sample_Enum_Value_Table(I) := K;
      Sample_Def_Value_Table(I)  := P;
    end loop;

    Valid_Sparse_Values := UC_Sparse_Mod_Enum(Sample_Enum_Value_Table);
    Valid_Def_Values    := UC_Def_Mod_Enum(Sample_Def_Value_Table);

  end Fill_Invalid;

  -- fill the tables with second set of valid values for conversion
  procedure Refill_Valid is
    K : Mod_10 := 0;
    P : Mod_By_Enum_Items := 0;

    Table : Array(Mod_10) of Mod_Same_Size_As_Sparse_Enum
          := ( 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024 );

  begin
    for I in Test_Width loop
      Sample_Enum_Value_Table(I) := Table(K);
      Sample_Def_Value_Table(I)  := Mod_Same_Size_As_Def_Enum(P);
      K := K +1;
      P := P +1;
    end loop;
    Valid_Sparse_Values := UC_Sparse_Mod_Enum(Sample_Enum_Value_Table);
    Valid_Def_Values    := UC_Def_Mod_Enum(Sample_Def_Value_Table);
  end Refill_Valid;

  procedure Validate(Expect_Valid: Boolean) is
  begin  -- here's where we actually use the tested attribute

    for K in Test_Width loop
      if Valid_Sparse_Values(K)'Valid /= Expect_Valid then
        Report.Failed("Expected 'Valid =" & Boolean'Image(Expect_Valid)
                    & " for Sparse item " & Integer'Image(K) );
      end if;
    end loop;

    for P in Test_Width loop
      if Valid_Def_Values(P)'Valid /= Expect_Valid then
        Report.Failed("Expected 'Valid =" & Boolean'Image(Expect_Valid)
                    & " for Default item " & Integer'Image(P) );
      end if;
    end loop;

  end Validate;

begin  -- Main test procedure.

  Report.Test ("CD92001", "Check object attribute: X'Valid" );
   
  Fill_Valid;
  Validate(True);

  Fill_Invalid;
  Validate(False);

  Refill_Valid;
  Validate(True);

  Report.Result;

end CD92001;
