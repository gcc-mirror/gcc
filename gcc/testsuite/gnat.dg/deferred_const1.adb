-- { dg-do compile }

with Text_IO; use Text_IO;

procedure Deferred_Const1 is
  I : Integer := 16#20_3A_2D_28#;
  S : constant string(1..4);
  for S'address use I'address; -- { dg-warning "constant overlays a variable" } 
  pragma Import (Ada, S);
begin
  Put_Line (S);
end;
