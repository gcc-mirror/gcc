--
-- C354002.A
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
--      Check that the attributes of modular types yield
--      correct values/results.  The attributes checked are:
--
--      First, Last, Range, Base, Min, Max, Succ, Pred, 
--      Image, Width, Value, Pos, and Val
--
-- TEST DESCRIPTION:
--      This test defines several modular types.  One type defined at
--      each of System.Max_Binary_Modulus, System.Max_Nonbinary_Modulus,
--      a power of two half that of System.Max_Binary_Modulus, one less
--      than that power of two; one more than that power of two, two
--      less than a (large) power of two.  For each of these types,
--      determine the correct operation of the following attributes:
--
--      First, Last, Range, Base, Min, Max, Succ, Pred, Image, Width,
--      Value, Pos, Val, and Modulus
--
--      The attributes Wide_Image and Wide_Value are deferred to C354003.
--
--
--
-- CHANGE HISTORY:
--      08 SEP 94   SAIC    Initial version
--      17 NOV 94   SAIC    Revised version
--      13 DEC 94   SAIC    split off Wide_String attributes into C354003
--      06 JAN 95   SAIC    Promoted to next release
--      19 APR 95   SAIC    Revised in accord with reviewer comments
--      27 JAN 96   SAIC    Eliminated 32/64 bit potential conflict for 2.1
--
--!

with Report;
with System;
with TCTouch;
procedure C354002 is

  function ID(Local_Value: Integer) return Integer renames Report.Ident_Int;
  function ID(Local_Value: String)  return String renames  Report.Ident_Str;

  Power_2_Bits          : constant := System.Storage_Unit;
  Half_Max_Binary_Value : constant := System.Max_Binary_Modulus / 2;

  type Max_Binary      is mod System.Max_Binary_Modulus;
  type Max_NonBinary   is mod System.Max_Nonbinary_Modulus;
  type Half_Max_Binary is mod Half_Max_Binary_Value;

  type Medium          is mod 2048;
  type Medium_Plus     is mod 2042;
  type Medium_Minus    is mod 2111;

  type Small  is mod 2;
  type Finger is mod 5;

  MBL  : constant := Max_NonBinary'Last;
  MNBM : constant := Max_NonBinary'Modulus;

  Ones_Complement_Permission : constant Boolean := MBL = MNBM;

  type Finger_Id is (Thumb, Index, Middle, Ring, Pinkie);

  subtype Midrange is Medium_Minus range 222 .. 1111;

-- a few numbers for testing purposes
  Max_Binary_Mod_Over_3      : constant := Max_Binary'Modulus / 3;
  Max_NonBinary_Mod_Over_4   : constant := Max_NonBinary'Modulus / 4;
  System_Max_Bin_Mod_Pred    : constant := System.Max_Binary_Modulus - 1;
  System_Max_NonBin_Mod_Pred : constant := System.Max_Nonbinary_Modulus - 1;
  Half_Max_Bin_Value_Pred    : constant := Half_Max_Binary_Value - 1;

  AMB,   BMB   : Max_Binary;
  AHMB,  BHMB  : Half_Max_Binary;
  AM,    BM    : Medium;
  AMP,   BMP   : Medium_Plus;
  AMM,   BMM   : Medium_Minus;
  AS,    BS    : Small;
  AF,    BF    : Finger;

  TC_Pass_Case : Boolean := True;

  procedure Value_Fault( S: String ) is
  -- check 'Value for failure modes
  begin
    -- the evaluation of the 'Value expression should raise C_E
    TCTouch.Assert_Not( Midrange'Value(S) = 0, "Value_Fault" );
    if Midrange'Value(S) not in Midrange'Base then
      Report.Failed("'Value(" & S & ") raised no exception");
    end if;
  exception
    when Constraint_Error => null; -- expected case
    when others =>
         Report.Failed("'Value(" & S & ") raised wrong exception");
  end Value_Fault;

begin  -- Main test procedure.

  Report.Test ("C354002", "Check attributes of modular types" );

-- Base
  TCTouch.Assert( Midrange'Base'First = 0, "Midrange'Base'First" );
  TCTouch.Assert( Midrange'Base'Last  = Medium_Minus'Last,
                  "Midrange'Base'Last" );

-- First
  TCTouch.Assert( Max_Binary'First = 0,         "Max_Binary'First" );
  TCTouch.Assert( Max_NonBinary'First = 0,      "Max_NonBinary'First" );
  TCTouch.Assert( Half_Max_Binary'First = 0,    "Half_Max_Binary'First" );

  TCTouch.Assert( Medium'First = Medium(ID(0)), "Medium'First" );
  TCTouch.Assert( Medium_Plus'First = Medium_Plus(ID(0)),
                                                "Medium_Plus'First" );
  TCTouch.Assert( Medium_Minus'First = Medium_Minus(ID(0)),
                                                "Medium_Minus'First" );

  TCTouch.Assert( Small'First = Small(ID(0)),   "Small'First" );
  TCTouch.Assert( Finger'First = Finger(ID(0)), "Finger'First" );
  TCTouch.Assert( Midrange'First = Midrange(ID(222)),
                                                "Midrange'First" );

-- Image
  TCTouch.Assert( Half_Max_Binary'Image(255) = " 255",
                 "Half_Max_Binary'Image" );
  TCTouch.Assert( Medium'Image(0) = ID(" 0"),  "Medium'Image" );
  TCTouch.Assert( Medium_Plus'Image(Medium_Plus'Last) = " 2041",
                 "Medium_Plus'Image" );
  TCTouch.Assert( Medium_Minus'Image(Medium_Minus(ID(1024))) = " 1024",
                 "Medium_Minus'Image" );
  TCTouch.Assert( Small'Image(Small(ID(1))) = " 1", "Small'Image" );
  TCTouch.Assert( Midrange'Image(Midrange(ID(333))) = " 333",
                  "Midrange'Image" );

-- Last
  TCTouch.Assert( Max_Binary'Last      = System_Max_Bin_Mod_Pred,
                 "Max_Binary'Last");
  if Ones_Complement_Permission then
    TCTouch.Assert( Max_NonBinary'Last >= System_Max_NonBin_Mod_Pred,
                   "Max_NonBinary'Last (ones comp)");
  else
    TCTouch.Assert( Max_NonBinary'Last   = System_Max_NonBin_Mod_Pred,
                   "Max_NonBinary'Last");
  end if;
  TCTouch.Assert( Half_Max_Binary'Last = Half_Max_Bin_Value_Pred,
                 "Half_Max_Binary'Last");

  TCTouch.Assert( Medium'Last          = Medium(ID(2047)), "Medium'Last");
  TCTouch.Assert( Medium_Plus'Last     = Medium_Plus(ID(2041)),
                  "Medium_Plus'Last");
  TCTouch.Assert( Medium_Minus'Last    = Medium_Minus(ID(2110)),
                  "Medium_Minus'Last");
  TCTouch.Assert( Small'Last    = Small(ID(1)), "Small'Last");
  TCTouch.Assert( Finger'Last   = Finger(ID(4)), "Finger'Last");
  TCTouch.Assert( Midrange'Last = Midrange(ID(1111)), "Midrange'Last");

-- Max
  TCTouch.Assert( Max_Binary'Max(Power_2_Bits, Max_Binary'Last)
                  = Max_Binary'Last,                     "Max_Binary'Max");
  TCTouch.Assert( Max_NonBinary'Max(100,2000) = 2000, "Max_NonBinary'Max");
  TCTouch.Assert( Half_Max_Binary'Max(123,456) = 456,
                                                    "Half_Max_Binary'Max");

  TCTouch.Assert( Medium'Max(0,2040) = 2040,                 "Medium'Max");
  TCTouch.Assert( Medium_Plus'Max(0,1) = 1,             "Medium_Plus'Max");
  TCTouch.Assert( Medium_Minus'Max(2001,1995) = 2001,  "Medium_Minus'Max");
  TCTouch.Assert( Small'Max(1,0) = 1,                         "Small'Max");
  TCTouch.Assert( Finger'Max(Finger'Last+1,4) = 4,           "Finger'Max");
  TCTouch.Assert( Midrange'Max(Midrange'First+1,222) = Midrange'First+1,
                                                          "Midrange'Max");

-- Min
  TCTouch.Assert( Max_Binary'Min(Power_2_Bits, Max_Binary'Last)
                  = Power_2_Bits,                        "Max_Binary'Min");
  TCTouch.Assert( Max_NonBinary'Min(100,2000) = 100,  "Max_NonBinary'Min");
  TCTouch.Assert( Half_Max_Binary'Min(123,456) = 123,
                                                    "Half_Max_Binary'Min");

  TCTouch.Assert( Medium'Min(0,Medium(ID(2040))) = 0,        "Medium'Min");
  TCTouch.Assert( Medium_Plus'Min(0,1) = 0,             "Medium_Plus'Min");
  TCTouch.Assert( Medium_Minus'Min(2001,1995) = 1995,  "Medium_Minus'Min");
  TCTouch.Assert( Small'Min(1,0) = 0,                         "Small'Min");
  TCTouch.Assert( Finger'Min(Finger'Last+1,4) /= 4,          "Finger'Min");
  TCTouch.Assert( Midrange'Min(Midrange'First+1,222) = 222,
                                                          "Midrange'Min");
-- Modulus
  TCTouch.Assert( Max_Binary'Modulus = System.Max_Binary_Modulus,
                 "Max_Binary'Modulus");
  TCTouch.Assert( Max_NonBinary'Modulus = System.Max_Nonbinary_Modulus,
                 "Max_NonBinary'Modulus");
  TCTouch.Assert( Half_Max_Binary'Modulus = Half_Max_Binary_Value,
                 "Half_Max_Binary'Modulus");

  TCTouch.Assert( Medium'Modulus       = 2048, "Medium'Modulus");
  TCTouch.Assert( Medium_Plus'Modulus  = 2042, "Medium_Plus'Modulus");
  TCTouch.Assert( Medium_Minus'Modulus = 2111, "Medium_Minus'Modulus");
  TCTouch.Assert( Small'Modulus        =    2, "Small'Modulus");
  TCTouch.Assert( Finger'Modulus       =    5, "Finger'Modulus");
  TCTouch.Assert( Midrange'Modulus = ID(2111), "Midrange'Modulus");

-- Pos
  declare
    Int : Natural := 222;
  begin
    for I in Midrange loop
      TC_Pass_Case := TC_Pass_Case and Midrange'Pos(I) = Int;
                    
      Int := Int +1;
    end loop;
  end;

  TCTouch.Assert( TC_Pass_Case, "Midrange'Pos");

-- Pred
  TCTouch.Assert( Max_Binary'Pred(0)      = System_Max_Bin_Mod_Pred,
                 "Max_Binary'Pred(0)");
  if Ones_Complement_Permission then
    TCTouch.Assert( Max_NonBinary'Pred(0) >= System_Max_NonBin_Mod_Pred,
                   "Max_NonBinary'Pred(0) (ones comp)");
  else
    TCTouch.Assert( Max_NonBinary'Pred(0)   = System_Max_NonBin_Mod_Pred,
                   "Max_NonBinary'Pred(0)");
  end if;
  TCTouch.Assert( Half_Max_Binary'Pred(0) = Half_Max_Bin_Value_Pred,
                 "Half_Max_Binary'Pred(0)");

  TCTouch.Assert( Medium'Pred(Medium(ID(0))) = 2047, "Medium'Pred(0)");
  TCTouch.Assert( Medium_Plus'Pred(0)     = 2041, "Medium_Plus'Pred(0)");
  TCTouch.Assert( Medium_Minus'Pred(0)    = 2110, "Medium_Minus'Pred(0)");
  TCTouch.Assert( Small'Pred(0)  = 1, "Small'Pred(0)");
  TCTouch.Assert( Finger'Pred(Finger(ID(0))) = 4, "Finger'Pred(0)");
  TCTouch.Assert( Midrange'Pred(222) = 221, "Midrange'Pred('First)");

-- Range
  for I in Midrange'Range loop
    if I not in Midrange then
      Report.Failed("Midrange loop test");
    end if;
  end loop;
  for I in Medium'Range loop
    if I not in Medium then
      Report.Failed("Medium loop test");
    end if;
  end loop;
  for I in Medium_Minus'Range loop
    if I not in 0..2110 then
      Report.Failed("Medium loop test");
    end if;
  end loop;

-- Succ
  TCTouch.Assert( Max_Binary'Succ(System_Max_Bin_Mod_Pred)         = 0,
                 "Max_Binary'Succ('Last)");
  if Ones_Complement_Permission then
    TCTouch.Assert( (Max_NonBinary'Succ(System_Max_NonBin_Mod_Pred) = 0)
                or (Max_NonBinary'Succ(System_Max_NonBin_Mod_Pred)
                    = Max_NonBinary'Last),
                   "Max_NonBinary'Succ('Last) (ones comp)");
  else
    TCTouch.Assert( Max_NonBinary'Succ(System_Max_NonBin_Mod_Pred)   = 0,
                   "Max_NonBinary'Succ('Last)");
  end if;
 TCTouch.Assert( Half_Max_Binary'Succ(Half_Max_Bin_Value_Pred)    = 0,
                 "Half_Max_Binary'Succ('Last)");

  TCTouch.Assert( Medium'Succ(2047)       = 0, "Medium'Succ('Last)");
  TCTouch.Assert( Medium_Plus'Succ(2041)  = 0, "Medium_Plus'Succ('Last)");
  TCTouch.Assert( Medium_Minus'Succ(2110) = 0, "Medium_Minus'Succ('Last)");
  TCTouch.Assert( Small'Succ(1)           = 0, "Small'Succ('Last)");
  TCTouch.Assert( Finger'Succ(4)          = 0, "Finger'Succ('Last)");
  TCTouch.Assert( Midrange'Succ(Midrange(ID(1111))) = 1112,
                  "Midrange'Succ('Last)");

-- Val
  for I in Natural range ID(222)..ID(1111) loop
    TCTouch.Assert( Midrange'Val(I) = Medium_Minus(I), "Midrange'Val");
  end loop;

-- Value

  TCTouch.Assert( Half_Max_Binary'Value("255") = 255,
                 "Half_Max_Binary'Value" );

  TCTouch.Assert( Medium'Value(" 1e2") = 100,   "Medium'Value(""1e2"")" );
  TCTouch.Assert( Medium'Value(" 0 ")  =   0,   "Medium'Value" );
  TCTouch.Assert( Medium_Plus'Value(ID("2041")) = 2041,
                 "Medium_Plus'Value" );
  TCTouch.Assert( Medium_Minus'Value(ID("+10_24")) = 1024,
                 "Medium_Minus'Value" );

  TCTouch.Assert( Small'Value("+1") = 1,            "Small'Value" );
  TCTouch.Assert( Midrange'Value(ID("333")) = 333,  "Midrange'Value" );
  TCTouch.Assert( Midrange'Value("1E3") = 1000,
                 "Midrange'Value(""1E3"")" );

  Value_Fault( "bad input" );
  Value_Fault( "-333" );
  Value_Fault( "9999" );
  Value_Fault( ".1" );
  Value_Fault( "1e-1" );

-- Width
  TCTouch.Assert( Medium'Width       = 5, "Medium'Width");
  TCTouch.Assert( Medium_Plus'Width  = 5, "Medium_Plus'Width");
  TCTouch.Assert( Medium_Minus'Width = 5, "Medium_Minus'Width");
  TCTouch.Assert( Small'Width        = 2, "Small'Width");
  TCTouch.Assert( Finger'Width       = 2, "Finger'Width");
  TCTouch.Assert( Midrange'Width     = 5, "Midrange'Width");

  Report.Result;

end C354002;
