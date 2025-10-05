-- { dg-do compile }
-- { dg-options "-gnat2022" }

procedure Reduce2 is

  subtype Value is Natural range 0 .. 255;

  function Do_Something (Accumulator : Value; Symbol : Character) return Value
    is (((Accumulator + Character'Pos (Symbol)) * 17) mod 256);

  function Do_It_By_Reduction (S : String) return Value is
    (S'Reduce (Do_Something, 0));

  Test_It : constant Value := Do_It_By_Reduction ("Hello, world!");

begin
  null;
end;
