-- { dg-do run }
-- { dg-options "-gnatws" }

with Ada.Unchecked_Conversion;

procedure Unchecked_Convert1 is
  type Byte is mod 2**8;

  type Stream is array (Natural range <>) of Byte;

  type Rec is record
    I1, I2 : Integer;
  end record;

  function Do_Sum (R : Rec) return Integer is
  begin
    return R.I1 + R.I2;
  end;

  function Sum (S : Stream) return Integer is
    subtype Chunk is Stream (1 .. Rec'Size / 8);
    function To_Chunk is new Ada.Unchecked_Conversion (Chunk, Rec);
  begin
    return Do_Sum (To_Chunk (S(S'First ..  S'First + Rec'Size / 8 - 1)));
  end;

  A : Stream (1..9);
  I : Integer;

begin
  I := Sum (A(1..8));
end;
