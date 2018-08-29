with Ada.Text_IO; use Ada.Text_IO;
with Class_Wide3_Pkg; use Class_Wide3_Pkg;

procedure Class_Wide3 is
   DC : Disc_Child := (N => 1, I => 3, J => 5);
begin
   DC.Put_Line;
end Class_Wide3;
