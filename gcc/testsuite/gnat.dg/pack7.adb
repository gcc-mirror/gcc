-- { dg-do compile }
-- { dg-options "-gnatws" }

procedure Pack7 is

  type R is record
     I : Integer;
     a, b : Character;
  end record;

  type Ar1 is array (1..4) of R;
  type Ar2 is array (1..4) of R;
  pragma Pack (Ar2);

  type R2 is record
    A : Ar2;
  end record;
  for R2 use record
    A at 0 range 0 .. 48*4-1;
  end record;

  X : Ar1;
  Y : Ar2;

begin
  Y (1) := X (1);
end;
