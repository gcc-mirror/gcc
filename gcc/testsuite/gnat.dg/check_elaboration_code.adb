-- { dg-do run }
with Bug_Elaboration_Code; use Bug_Elaboration_Code;

procedure Check_Elaboration_Code is
begin
   if I /= J then
      raise Program_Error;
   end if;
end Check_Elaboration_Code;
