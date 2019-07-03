--  { dg-do compile }
--  { dg-options "-gnata" }

with System.Assertions; use System.Assertions;
with Predicate4_Pkg;
procedure Predicate4 is
   type V is new Float;
   package MXI2 is new Predicate4_Pkg (V);
   use MXI2;
   OK : Lt := (Has => False);
begin
   declare
      Wrong : Lt := (Has => True, MX => 3.14);
   begin
      raise Program_Error;
   end;
exception
   when Assert_Failure => null;
end;
