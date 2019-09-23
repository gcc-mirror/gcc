--  { dg-do compile }

with Generic_Inst13_Pkg;
with Generic_Inst13_Pkg.Nested_G;

procedure Generic_Inst13 is

  type Item_T is range 1 .. 16;

  package My_Inst is new Generic_Inst13_Pkg (Item_T);

  package My_Nested is new My_Inst.Nested_G;

  procedure Proc (Left, Right : My_Nested.T) is
    R : constant My_Nested.List_T := My_Nested."or" (Left, Right);
  begin
    null;
  end;

begin
  null;
end;
