--  { dg-do run }
--  { dg-options "-gnatVa" }

with Range_Check3_Pkg; use Range_Check3_Pkg;
procedure Range_Check3 is
   Ptr : Array_Access;
begin
   Ptr := Allocate;
   raise Program_Error;
exception
   when Constraint_Error => null;
end Range_Check3;

