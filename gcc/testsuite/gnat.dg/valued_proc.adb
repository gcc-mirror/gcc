-- { dg-do compile }
-- { dg-options "-gnatdm -gnatws" }

with Valued_Proc_Pkg; use Valued_Proc_Pkg;
with System; use System;

procedure Valued_Proc is
   Status : UNSIGNED_LONGWORD;
   Length : POSITIVE;
begin
   GetMsg (Status, UNSIGNED_WORD(Length));
end;
