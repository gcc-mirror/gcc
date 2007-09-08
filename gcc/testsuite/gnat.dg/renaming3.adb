-- { dg-do run }

with Renaming4; use Renaming4;

procedure Renaming3 is
   type A is array(1..16) of Integer;
   Filler : A := (others => 0);
begin
   if B(1) /= 1 then
      raise Program_Error;
   end if;
end;
