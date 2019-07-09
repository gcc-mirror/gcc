--  { dg-do run }
with Text_IO; use Text_IO;
with Generic_Inst6_I2;
procedure Generic_Inst6 is
begin
   if Generic_Inst6_I2.Check /= 49 then
      raise Program_Error;
   end if;
end;
