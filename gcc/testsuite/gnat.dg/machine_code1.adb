-- { dg-do compile }
-- { dg-options "-gnatws" }

with System.Machine_Code; use System.Machine_Code;
procedure machine_code1 is
   A_Float        : Float;
   An_Other_Float : Float := -99999.0;
begin
   An_Other_Float := An_Other_Float - A_Float;
   Asm("", Inputs => (Float'Asm_Input ("m", A_Float)));
end;
