--  { dg-do compile }

procedure equal_access is
   PA, PB  : access procedure := null;
begin
   if PA /= PB then
      null;
   end if;
end;
