-- C460008.A
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
--     Check that conversion to a modular type raises Constraint_Error
--     when the operand value is outside the base range of the modular type.
--
-- TEST DESCRIPTION:
--      Test conversion from integer, float, fixed and decimal types to
--      modular types.  Test conversion to mod 255, mod 256 and mod 258
--      to test the boundaries of 8 bit (+/-) unsigned numbers.
--      Test operand values that are negative, the value of the mod,
--      and greater than the value of the mod.
--      Declare a generic test procedure and instantiate it for each of the
--      unsigned types for each operand type.
--
--
-- CHANGE HISTORY:
--      04 OCT 95   SAIC   Initial version
--      15 MAY 96   SAIC   Revised for 2.1
--      24 NOV 98   RLB    Moved decimal cases into new test, C460011, to
--                         prevent this test from being inapplicable to
--                         implementations not supporting decimal types.
--
--!

------------------------------------------------------------------- C460008

with Report;

procedure C460008 is

  Shy_By_One   : constant := 2**8-1;
  Heavy_By_Two : constant := 2**8+2;

  type Unsigned_Edge_8 is mod Shy_By_One;
  type Unsigned_8_Bit  is mod 2**8;
  type Unsigned_Over_8 is mod Heavy_By_Two;

  NPC : constant String := " not properly converted";

  procedure Assert( Truth: Boolean; Message: String ) is
  begin
    if not Truth then
      Report.Failed(Message);
    end if;
  end Assert;

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

  generic
    type Source is range <>;
    type Target is mod <>;
  procedure Integer_Conversion_Check( For_The_Value : Source;
                                      Message       : String );

  procedure Integer_Conversion_Check( For_The_Value : Source;
                                      Message       : String ) is

    Item : Target;

  begin
    Item := Target( For_The_Value );
    Report.Failed("Int expected Constraint_Error " & Message);
    -- the call to Comment is to make the otherwise dead assignment to
    -- Item live.
    -- To avoid invoking C_E on a call to 'Image in Report.Failed that
    -- could cause a false pass
   Report.Comment("Value of" & Target'Image(Item) & NPC);
  exception
    when Constraint_Error => null; -- expected case
    when others => Report.Failed("Int Raised wrong exception " & Message);
  end Integer_Conversion_Check;

  procedure Int_To_Short is
    new Integer_Conversion_Check( Integer, Unsigned_Edge_8 );

  procedure Int_To_Eight is
    new Integer_Conversion_Check( Integer, Unsigned_8_Bit );

  procedure Int_To_Wide is
    new Integer_Conversion_Check( Integer, Unsigned_Over_8 );

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

  generic
    type Source is digits <>;
    type Target is mod <>;
  procedure Float_Conversion_Check( For_The_Value : Source;
                                    Message       : String );

  procedure Float_Conversion_Check( For_The_Value : Source;
                                    Message       : String ) is

    Item : Target;

  begin
    Item := Target( For_The_Value );
    Report.Failed("Flt expected Constraint_Error " & Message);
    Report.Comment("Value of" & Target'Image(Item) & NPC);
  exception
    when Constraint_Error => null; -- expected case
    when others => Report.Failed("Flt raised wrong exception " & Message);
  end Float_Conversion_Check;

  procedure Float_To_Short is
    new Float_Conversion_Check( Float, Unsigned_Edge_8 );

  procedure Float_To_Eight is
    new Float_Conversion_Check( Float, Unsigned_8_Bit );

  procedure Float_To_Wide is
    new Float_Conversion_Check( Float, Unsigned_Over_8 );

  function Identity( Root_Beer: Float ) return Float is
    -- a knockoff of Report.Ident_Int for type Float
    Nothing : constant Float := 0.0;
  begin
    if Report.Ident_Bool( Root_Beer = Nothing ) then
      return Nothing;
    else
      return Root_Beer;
    end if;
  end Identity;

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

  generic
    type Source is delta <>;
    type Target is mod <>;
  procedure Fixed_Conversion_Check( For_The_Value : Source;
                                    Message       : String );

  procedure Fixed_Conversion_Check( For_The_Value : Source;
                                    Message       : String ) is

    Item : Target;

  begin
    Item := Target( For_The_Value );
    Report.Failed("Fix expected Constraint_Error " & Message);
    Report.Comment("Value of" & Target'Image(Item) & NPC);
  exception
    when Constraint_Error => null; -- expected case
    when others => Report.Failed("Fix raised wrong exception " & Message);
  end Fixed_Conversion_Check;

  procedure Fixed_To_Short is
    new Fixed_Conversion_Check( Duration, Unsigned_Edge_8 );

  procedure Fixed_To_Eight is
    new Fixed_Conversion_Check( Duration, Unsigned_8_Bit );

  procedure Fixed_To_Wide is
    new Fixed_Conversion_Check( Duration, Unsigned_Over_8 );

  function Identity( A_Stitch: Duration ) return Duration is
    Threadbare : constant Duration := 0.0;
  begin
    if Report.Ident_Bool( A_Stitch = Threadbare ) then
      return Threadbare;
    else
      return A_Stitch;
    end if;
  end Identity;

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

begin  -- Main test procedure.

  Report.Test ("C460008", "Check that conversion to " &
                          "a modular type raises Constraint_Error when " &
                          "the operand value is outside the base range " &
                          "of the modular type" );


  -- Integer Error cases

  Int_To_Short( Report.Ident_Int( -1 ), "I2S Dynamic, Negative" );
  Int_To_Short( Report.Ident_Int( Shy_By_One ), "I2S Dynamic, At_Mod" );
  Int_To_Short( Report.Ident_Int( Heavy_By_Two+1 ), "I2S Dynamic, Over_Mod" );

  Int_To_Eight( -Shy_By_One, "I28 Static,  Negative" );
  Int_To_Eight( 2**8, "I28 Static,  At_Mod" );
  Int_To_Eight( Heavy_By_Two+1, "I28 Static,  Over_Mod" );

  Int_To_Wide ( Report.Ident_Int( -(Heavy_By_Two*2) ),
                "I2W Dynamic, Negative" );
  Int_To_Wide ( Heavy_By_Two, "I2W Static,  At_Mod" );
  Int_To_Wide ( Report.Ident_Int( Heavy_By_Two*2 ), "I2W Dynamic, Over_Mod" );

  -- Float Error cases

  Float_To_Short( -13.31, "F2S Static,  Negative" );
  Float_To_Short( Identity ( Float(Shy_By_One)), "F2S Dynamic, At_Mod" );
  Float_To_Short( 6378.388, "F2S Static,  Over_Mod" );

  Float_To_Eight( Identity( -99.3574 ), "F28 Dynamic, Negative" );
  Float_To_Eight( 2.0**8, "F28 Static,  At_Mod" );
  Float_To_Eight( 2.0**9, "F28 Static,  Over_Mod" );

  Float_To_Wide ( -0.54953_93129_81644, "FTW Static,  Negative" );
  Float_To_Wide ( Identity( 2.0**8 +2.0 ), "FTW Dynamic, At_Mod" );
  Float_To_Wide ( Identity( 2.0**8 +2.5001 ), "FTW Dynamic, Over_Mod" );
  Float_To_Wide ( Identity( Float'Last ), "FTW Dynamic, Over_Mod" );

  -- Fixed Error cases

  Fixed_To_Short( Identity( -5.00 ), "D2S Dynamic, Negative" );
  Fixed_To_Short( Shy_By_One * 1.0, "D2S Static,  At_Mod" );
  Fixed_To_Short( 1995.9, "D2S Static,  Over_Mod" );

  Fixed_To_Eight( -0.5, "D28 Static, Negative" );
  Fixed_To_Eight( 2.0*128, "D28 Static,  At_Mod" );
  Fixed_To_Eight( Identity( 2001.2 ), "D28 Dynamic, Over_Mod" );

  Fixed_To_Wide ( Duration'First, "D2W Static,  Negative" );
  Fixed_To_Wide ( Identity( 2*128.0 +2.0 ), "D2W Dynamic, At_Mod" );
  Fixed_To_Wide ( Duration'Last, "D2W Static,  Over_Mod" );

  -- having made it this far, the rest is downhill...
  -- check a few, correct, edge cases, and we're done

  Eye_Dew: declare
    A_Float   : Float    := 0.0;
    Your_Time : Duration := 0.0;
    Number    : Integer  := 0;

    Little   : Unsigned_Edge_8;
    Moderate : Unsigned_8_Bit;
    Big      : Unsigned_Over_8;

  begin
    Little := Unsigned_Edge_8(A_Float);
    Assert( Little = 0, "Float => Little, 0");


    Moderate := Unsigned_8_Bit (Your_Time);
    Assert( Moderate = 0, "Your_Time => Moderate, 0");

    Big := Unsigned_Over_8 (Number);
    Assert( Big = 0, "Number => Big, 0");

    A_Float   := 2.0**8-2.0;
    Your_Time := 2.0*128-2.0;
    Number    := 2**8;

    Little := Unsigned_Edge_8(A_Float);
    Assert( Little = 254, "Float => Little, 254");

    Little := Unsigned_Edge_8(Your_Time);
    Assert( Little = 254, "Your_Time => Little, 254");

    Big := Unsigned_Over_8 (A_Float + 2.0);
    Assert( Big = 256, "Sense => Big, 256");

    Big := Unsigned_Over_8 (Number);
    Assert( Big = 256, "Number => Big, 256");

  end Eye_Dew;

  Report.Result;

end C460008;
