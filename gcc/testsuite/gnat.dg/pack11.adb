-- { dg-do run }
-- { dg-options "-gnatws" }

with System;

procedure Pack11 is

  type R1 is record
    A1, A2, A3 : System.Address;
  end record;

  type R2 is record
    C : Character;
    R : R1;
  end record;
  pragma Pack (R2);

  procedure Dummy (R : R1) is begin null; end;

  procedure Init (X : R2) is
  begin
    Dummy (X.R);
  end;

  My_R2 : R2;

begin
  Init (My_R2);
end;
