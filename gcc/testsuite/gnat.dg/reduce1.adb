-- { dg-do compile }
-- { dg-options "-gnat2022" }

procedure Reduce1 is

  type Arr is array (Positive range <>) of Positive;

  A: Arr := (2, 87);

  B: Positive := A'Reduce (1, Positive'Max); --  { dg-error "reducer must be a subprogram, an operator, or an attribute|possible swap of reducer and initial value" }
begin
  null;
end;
