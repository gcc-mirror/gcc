--  { dg-do compile }

procedure Alignment14 is

  type My_Int1 is new Integer;
  for My_Int1'Alignment use 8;

  type Arr1 is array (1 .. 2) of My_Int1;

begin
   null;
end Alignment14;
