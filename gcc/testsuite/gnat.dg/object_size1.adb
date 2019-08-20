--  { dg-do compile }

with Text_IO; use Text_IO;

procedure Object_Size1 is
  type Zero_Size_Type is (Solo);

  for Zero_Size_Type'Size use 0;
  for Zero_Size_Type'Object_Size use 0;  --  { dg-error "Object_Size must be positive" }
begin
  Put_Line (Zero_Size_Type'Size'Image);
  Put_Line (Zero_Size_Type'Object_Size'Image);
end Object_Size1;
