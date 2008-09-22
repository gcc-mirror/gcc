-- { dg-do compile }
-- { dg-options "-O2" }

procedure volatile3 is

   v1 : Integer := 0;
   v2 : Integer := 0;
   pragma Volatile (v1);
   pragma Volatile (v2);
begin
   if v1 /= v2 then
      raise Program_Error;
   end if;
end;

-- { dg-final { scan-assembler "__gnat_rcheck" } }
