--  { dg-do run }
--  { dg-options "-O2" }
with Wrap_Raise_From_Pure; use Wrap_Raise_From_Pure;
procedure test_raise_from_pure is
begin
   Wrap_Raise_From_Pure.Check;
exception
   when Constraint_Error => null;
end;
