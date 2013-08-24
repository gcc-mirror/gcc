-- { dg-do run }

with Loop_Optimization16_Pkg; use Loop_Optimization16_Pkg;

procedure Loop_Optimization16 is

   Counter : Natural := 0;

   C : constant Natural := F;

   subtype Index_T is Index_Base range 1 .. Index_Base (C);

begin

   for I in Index_T'First .. Index_T'Last loop
      Counter := Counter + 1;
      exit when Counter > 200;
   end loop;

   if Counter > 200 then
      raise Program_Error;
   end if;

end Loop_Optimization16;
