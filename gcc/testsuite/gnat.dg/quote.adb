--  { dg-do run }

with GNAT.Regpat; use GNAT.Regpat;
procedure Quote is
begin   
  if Quote (".+") /= "\.\+" then
     raise Program_Error;
  end if;
end Quote;
