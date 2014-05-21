-- { dg-do run }
-- { dg-options "-O2" }

with Opt35_Pkg; use Opt35_Pkg;

procedure Opt36 is
  I : Integer := -1;
  N : Natural := 0;
begin
  loop
     begin
       I := I + 1;
       I := I + F(0);
     exception
       when E => N := N + 1;
     end;
     exit when I = 1;
  end loop;

  if N /= 2 or I = 0 then
    raise Program_Error;
  end if;
end;
