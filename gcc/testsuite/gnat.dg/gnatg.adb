-- { dg-do compile }
-- { dg-options "-gnatD" }

with System; 
with Ada.Unchecked_Conversion;
procedure gnatg is
   subtype Address is System.Address;
   type T is access procedure;
   function Cvt is new Ada.Unchecked_Conversion (Address, T);
   X : T;  
begin   
   X := Cvt (Gnatg'Address);
end gnatg;
