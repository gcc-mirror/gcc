--  { dg-do compile }

procedure Slice_Enum is
   Pos : array (Boolean) of Integer;
begin
   Pos (Boolean) := (others => 0);
end;

