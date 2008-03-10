-- { dg-do compile }
-- { dg-options "-O2" }

procedure Range_Check2 is

  subtype Block_Subtype is String(1 .. 6);
  type Color is (Black, Red, Green, Yellow, Blue, Magenta, Cyan, White);
  Foregrnd_Color : Color := White;
  Block : Block_Subtype := "123456";

begin
  Foregrnd_Color := Color'Val(Integer'Value(Block(5 .. 6)));
end;
