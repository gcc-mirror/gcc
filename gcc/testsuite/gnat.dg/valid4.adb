--  { dg-do run }
--  { dg-options "-gnatVa" }

with Valid4_Pkg; use Valid4_Pkg;

procedure Valid4 is
begin
   Proc (Global);

   if Global then
      raise Program_Error;
   end if;
end Valid4;
