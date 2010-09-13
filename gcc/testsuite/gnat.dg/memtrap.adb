-- { dg-do compile }
-- { dg-options "-O2" }

with System;

procedure Memtrap is
  X : integer;
  for X'address use System.Null_Address;
begin
  X := 12;
exception
   when others => null;
end;

-- { dg-final { scan-assembler "__gnat_begin_handler|__gnat_raise_nodefer" } }
