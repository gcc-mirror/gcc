-- PR middle-end/35823
-- { dg-do compile ]

procedure Size_Attribute (Arg : in String) is
   Size : constant Natural := Arg'Size;
begin
   null;
end;
