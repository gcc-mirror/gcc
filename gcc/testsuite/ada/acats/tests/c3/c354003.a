-- C354003.A
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
--      Check that the Wide_String attributes of modular types yield
--      correct values/results.  The attributes checked are:
--
--      Wide_Image
--      Wide_Value
--
-- TEST DESCRIPTION:
--      This test is split from C354002.  It tests only the attributes:
--
--      Wide_Image, Wide_Value
--
--      This test defines several modular types.  One type defined at
--      each of System.Max_Binary_Modulus, System.Max_Nonbinary_Modulus,
--      a power of two half that of System.Max_Binary_Modulus, one less
--      than that power of two; one more than that power of two, two
--      less than a (large) power of two.  For each of these types,
--      determine the correct operation of the Wide_String attributes.
--
--
-- CHANGE HISTORY:
--      13 DEC 94   SAIC    Initial version
--      06 JAN 94   SAIC    Promoted to future release
--      19 APR 95   SAIC    Revised in accord with reviewer comments
--      01 DEC 95   SAIC    Corrected for 2.0.1
--      27 JAN 96   SAIC    Eliminated potential 32/64 bit conflict for 2.1
--      24 FEB 97   PWB.CTA Corrected out-of-range value
--!

with Report;
with System;
with TCTouch;
with Ada.Characters.Handling;
procedure C354003 is

  function ID(Local_Value: Integer) return Integer renames Report.Ident_Int;
  function ID(Local_Value: String)  return String renames  Report.Ident_Str;

  function ID(Local_Value: String) return Wide_String is
  begin
    return Ada.Characters.Handling.To_Wide_String( ID( Local_Value ) );
  end ID;

  Half_Max_Binary_Value : constant := System.Max_Binary_Modulus / 2;

  type Max_Binary      is mod System.Max_Binary_Modulus;
  type Max_NonBinary   is mod System.Max_Nonbinary_Modulus;
  type Half_Max_Binary is mod Half_Max_Binary_Value;

  type Medium          is mod 2048;
  type Medium_Plus     is mod 2042;
  type Medium_Minus    is mod 2111;

  type Small  is mod 2;
  type Finger is mod 5;

  type Finger_Id is (Thumb, Index, Middle, Ring, Pinkie);

  subtype Midrange is Medium_Minus range 222 .. 1111;

  AMB,   BMB   : Max_Binary;
  AHMB,  BHMB  : Half_Max_Binary;
  AM,    BM    : Medium;
  AMP,   BMP   : Medium_Plus;
  AMM,   BMM   : Medium_Minus;
  AS,    BS    : Small;
  AF,    BF    : Finger;

  procedure Wide_Value_Fault( S: Wide_String ) is
  -- check 'Wide_Value for failure modes
  begin
    -- the evaluation of the 'Wide_Value expression should raise C_E
    TCTouch.Assert_Not( Midrange'Wide_Value(S) = 0, "Wide_Value_Fault" );
    if Midrange'Wide_Value(S) not in Midrange'Base then
      Report.Failed("'Wide_Value  raised no exception");
    end if;
  exception
    when Constraint_Error => null; -- expected case
    when others =>
         Report.Failed("'Wide_Value raised wrong exception");
  end Wide_Value_Fault;


  The_Cap, The_Toe : Natural;

  procedure Check_Non_Static_Cases( Lower_Bound,Upper_Bound : Medium ) is
    subtype Non_Static is Medium range Lower_Bound..Upper_Bound;
  begin
  -- First, Last, Range, Min, Max, Succ, Pred, Pos, and Val

    TCTouch.Assert( Non_Static'First = Medium(The_Toe), "Non_Static'First" );
    TCTouch.Assert( Non_Static'Last = Non_Static(The_Cap),
                    "Non_Static'Last" );
    TCTouch.Assert( Non_Static(The_Cap/2) in Non_Static'Range,
                    "Non_Static'Range" );
    TCTouch.Assert( Non_Static'Min(Medium(Report.Ident_Int(100)),
                                   Medium(Report.Ident_Int(200))) = 100,
                    "Non_Static'Min" );
    TCTouch.Assert( Non_Static'Max(Medium(Report.Ident_Int(100)),
                                   Medium(Report.Ident_Int(200))) = 200,
                    "Non_Static'Max" );
    TCTouch.Assert( Non_Static'Succ(Non_Static(The_Cap))
                    = Medium'Succ(Upper_Bound),
                    "Non_Static'Succ" );
    TCTouch.Assert( Non_Static'Pred(Medium(Report.Ident_Int(The_Cap)))
                    = Non_Static(Report.Ident_Int(The_Cap-1)),
                    "Non_Static'Pred" );
    TCTouch.Assert( Non_Static'Pos(Upper_Bound) = Non_Static(The_Cap),
                    "Non_Static'Pos" );
    TCTouch.Assert( Non_Static'Val(Non_Static(The_Cap)) = Upper_Bound,
                    "Non_Static'Val" );

  end Check_Non_Static_Cases;


begin  -- Main test procedure.

  Report.Test ("C354003", "Check Wide_String attributes of modular types" );

  Wide_Strings_Needed: declare

    Max_Bin_Mod_Div_3 : constant := Max_Binary'Modulus/3;
    Max_Non_Mod_Div_4 : constant := Max_NonBinary'Modulus/4;

  begin

-- Wide_Image

    TCTouch.Assert( Half_Max_Binary'Wide_Image(255) = " 255",
                   "Half_Max_Binary'Wide_Image" );

    TCTouch.Assert( Medium'Wide_Image(0) = " 0",  "Medium'Wide_Image" );

    TCTouch.Assert( Medium_Plus'Wide_Image(Medium_Plus'Last) = " 2041",
                   "Medium_Plus'Wide_Image" );

    TCTouch.Assert( Medium_Minus'Wide_Image(Medium_Minus(ID(1024))) = " 1024",
                   "Medium_Minus'Wide_Image" );

    TCTouch.Assert( Small'Wide_Image(1) = " 1",   "Small'Wide_Image" );

    TCTouch.Assert( Midrange'Wide_Image(Midrange(ID(333))) = " 333",
                   "Midrange'Wide_Image" );

-- Wide_Value

    TCTouch.Assert( Half_Max_Binary'Wide_Value("255") = 255,
                   "Half_Max_Binary'Wide_Value" );

    TCTouch.Assert( Medium'Wide_Value(" 0 ")  = 0,   "Medium'Wide_Value" );

    TCTouch.Assert( Medium_Plus'Wide_Value(ID("2041")) = Medium_Plus'Last,
                   "Medium_Plus'Wide_Value" );

    TCTouch.Assert( Medium_Minus'Wide_Value("+1_4 ") = 14,
                   "Medium_Minus'Wide_Value" );

    TCTouch.Assert( Small'Wide_Value("+1") = 1,      "Small'Wide_Value" );

    TCTouch.Assert( Midrange'Wide_Value(ID("333")) = 333,
                   "Midrange'Wide_Value" );

    TCTouch.Assert( Midrange'Wide_Value(ID("1E3")) = 1000,
                   "Midrange'Wide_Value(""1E3"")" );

    Wide_Value_Fault( "bad input" );
    Wide_Value_Fault( "-333" );
    Wide_Value_Fault( "9999" );
    Wide_Value_Fault( ".1" );
    Wide_Value_Fault( "1e-1" );

  end Wide_Strings_Needed;

  The_Toe := Report.Ident_Int(25);
  The_Cap := Report.Ident_Int(256);
  Check_Non_Static_Cases( Medium(Report.Ident_Int(The_Toe)),
                          Medium(Report.Ident_Int(The_Cap)) );

  The_Toe := Report.Ident_Int(40);
  The_Cap := Report.Ident_Int(2047);
  Check_Non_Static_Cases( Medium(Report.Ident_Int(The_Toe)),
                          Medium(Report.Ident_Int(The_Cap)) );

  Report.Result;

end C354003;
