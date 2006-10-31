-- { dg-do run }

procedure capture_value is
   x : integer := 0;
begin
   declare
      z : integer renames x;
   begin
      z := 3;
      x := 5;
      z := z + 1;
      if z /= 6 then
         raise Program_Error;
      end if;
   end;
end;
