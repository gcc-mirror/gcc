-- { dg-do run }
-- { dg-options "-O" }

with Interfaces; use Interfaces;
with Unchecked_Conversion;

procedure Opt31 is

  type Unsigned_24 is new Unsigned_32 range 0 .. 2**24 - 1;
  subtype Time_T is Unsigned_24 range 0 .. 24 * 60 * 60 * 128 - 1;

  type Messages_T is array (Positive range <>) of Unsigned_8;
  subtype T_3Bytes is Messages_T (1 .. 3);

  type Rec1 is record
    F : Time_T;
  end record;
  for Rec1 use record
    F at 0 range 0 .. 23;
  end record;
  for Rec1'Size use 24;

  type Rec2 is record
    I1,I2,I3,I4 : Integer;
    R1 : Rec1;
  end record;

  function Conv is new Unchecked_Conversion (T_3Bytes, Rec1);

  procedure Decode (M : Messages_T) is
    My_Rec2 : Rec2;
  begin
    My_Rec2.R1 := Conv (M (1 .. 3));
    if not My_Rec2.R1.F'Valid then
      raise Program_Error;
    end if;
  end;

  Message : Messages_T (1 .. 4) := (16#18#, 16#0C#, 16#0C#, 16#18#);

begin
  Decode (Message);
end;
