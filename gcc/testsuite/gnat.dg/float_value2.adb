--  { dg-do run }

procedure Float_Value2 is
   F1 : Long_Long_Float := Long_Long_Float'Value ("1.e40");
   F2 : Long_Long_Float := Long_Long_Float'Value ("1.0e40");
begin
   if F1 /= F2 then
      raise Program_Error;
   end if;
end Float_Value2;