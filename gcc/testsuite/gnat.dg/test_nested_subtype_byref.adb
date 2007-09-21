-- { dg-do run }
-- { dg-options "-O2" }

with Nested_Subtype_Byref;
procedure Test_Nested_Subtype_Byref is
begin
   Nested_Subtype_Byref.Check;
end;
