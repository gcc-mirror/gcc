-- { dg-do run }
-- { dg-options "-gnatVaM" }

procedure Loop_Boolean is

  type R is record
    B : Boolean;
  end record;

  procedure proc (X : R) is
    B : Boolean;
  begin
    B := X.B;
  end;

begin
  for I in reverse Boolean loop
    Proc ((B => I));
  end loop;
end;
