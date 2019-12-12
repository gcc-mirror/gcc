--  { dg-do run }
--  { dg-options "-O0" }

procedure Fixedpnt6 is

  type T is delta 0.125 range -2.0 .. 1.875;

  function Mult (A, B : T) return T is
  begin
    return T (A * B);
  end;

  R : T;

begin
  R := Mult (T'Last, T'Last);
  raise Program_Error;
exception
   when Constraint_Error =>
      null;
end;
