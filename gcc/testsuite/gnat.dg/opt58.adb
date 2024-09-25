-- { dg-do compile }
-- { dg-options "-O -gnatws" }

with Unchecked_Conversion;
with System; use System;
with Opt58_Pkg; use Opt58_Pkg;

procedure Opt58 is

   function Convert is new Unchecked_Conversion (Integer, Rec);

   Dword : Integer := 0;
   I : Small_Int := F1 (Convert (Dword));

begin
   if F2 (Null_Address, I = 0) then
      null;
   end if;
end Opt58;
