-- PR ada/52735
-- Reported by Per Sandberg <per.sandberg@bredband.net>

-- { dg-do compile }

with Nested_Generic1_Pkg;

procedure Nested_Generic1 is
   package P is new Nested_Generic1_Pkg;
begin
   null;
end;
