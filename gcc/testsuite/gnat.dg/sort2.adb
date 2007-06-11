--  { dg-do run }

with sort1;
procedure sort2 is
begin
   if Sort1 ("hello world") /= " dehllloorw" then
      raise Program_Error;
   end if;
end sort2;
