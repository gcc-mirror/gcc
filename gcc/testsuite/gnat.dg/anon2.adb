-- { dg-do compile }

with anon1;
procedure anon2 is
begin
   if anon1.F /= null then
      null;
   end if;
end anon2;
