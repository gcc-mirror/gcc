-- { dg-do run }
-- { dg-options "-O2" }

with System.Address_To_Access_Conversions;

procedure address_conversion is

   type Integer_type1 is new Integer;
   type Integer_type2 is new Integer;

   package AA is new System.Address_To_Access_Conversions (Integer_type1);

   K1 : Integer_type1;
   K2 : Integer_type2;

begin
   K1 := 1;
   K2 := 2;

   AA.To_Pointer(K2'Address).all := K1;
   if K2 /= 1 then
      raise Program_Error;
   end if;
end;
