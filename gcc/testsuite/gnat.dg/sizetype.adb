-- { dg-do run }

with Interfaces.C; use Interfaces.C;

procedure Sizetype is

   TC_String : String(1..8) := "abcdefgh";
   TC_No_nul : constant char_array := To_C(TC_String, False);
 
begin
   if TC_No_nul(0) /= To_C('a') then
      raise Program_Error;
   end if;
end;
