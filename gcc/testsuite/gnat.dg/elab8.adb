--  { dg-do compile }
--  { dg-options "-gnatN" }

with Elab8_Gen;

procedure Elab8 is

  package My_G is new Elab8_Gen  (Integer);

begin
  My_G.Compare (0, 1);
end;
