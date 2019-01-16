--  { dg-do compile }

with Atomic11_Pkg1; use Atomic11_Pkg1;

procedure Atomic11 is

  R1 : Rec1;
  pragma Atomic (R1);

  R2 : Rec2;
  pragma Volatile (R2);

begin
  R1.I := 0;
  Proc1 (R1);    -- { dg-warning "atomic actual passed by copy" }
  R2.A(1) := 0;
  Proc1 (R1);    -- { dg-warning "atomic actual passed by copy" }
  Proc2 (R2);    -- { dg-warning "volatile actual passed by copy" }
end;
