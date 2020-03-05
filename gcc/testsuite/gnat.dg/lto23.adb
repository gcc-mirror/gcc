-- { dg-do compile }
-- { dg-options "-flto" { target lto } }

procedure Lto23 (N : Natural) is

  type Root is tagged null record;

  type Vec is array (Positive range <>) of Root;

  type Rec is record
    V : Vec (1 .. N);
  end record;

  type Arr is array (Positive range <>) of Rec;

  A : Arr (1 .. 4);

begin
  null;
end;
