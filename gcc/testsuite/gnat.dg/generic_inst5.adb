--  { dg-do compile }

procedure Generic_Inst5 is
  generic
  package G1 is
  end G1;

  generic
     with package I1 is new G1;
  package G2 is
  end G2;

  package body G1 is
     package I2 is new G2 (I1 => G1);
  end G1;

  package I1 is new G1;
begin
  null;
end;
