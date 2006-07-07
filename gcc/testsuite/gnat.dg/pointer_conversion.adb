-- { dg-do run }
-- { dg-options "-O2" }

with Unchecked_Conversion;

procedure pointer_conversion is

   type int1 is new integer;
   type int2 is new integer;
   type a1 is access int1;
   type a2 is access int2;

   function to_a2 is new Unchecked_Conversion (a1, a2);

   v1 : a1 := new int1;
   v2 : a2 := to_a2 (v1);

begin
   v1.all := 1;
   v2.all := 0;

   if v1.all /= 0 then
      raise Program_Error;
   end if;
end;
