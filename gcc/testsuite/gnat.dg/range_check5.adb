--  { dg-do run }
--  { dg-options "-gnateF -O0" }

procedure Range_Check5 is

  subtype Small_Float is Float range -100.0 .. 100.0;

  function Conv (F : Long_Float) return Small_Float is
  begin
    return Small_Float (F);
  end;

  R : Small_Float;

begin
  R := Conv (4.0E+38);
  raise Program_Error;
exception
   when Constraint_Error =>
      null;
end;
