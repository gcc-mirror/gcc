-- { dg-do run }
-- { dg-options "-O" }

procedure Opt89 is

  type Rec is record
    I : Integer := 3;
  end record;

  subtype Index is Natural range 0..0;

  type Arr is array (Index range <>) of Rec;

  X : Arr (0 .. -1);

begin
  null;
end;
