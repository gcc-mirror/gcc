--  { dg-do compile }

procedure Alignment14 is

  type My_Int1 is new Integer;
  for My_Int1'Alignment use 8;

  type Arr1 is array (1 .. 2) of My_Int1;

  type My_Int2 is new Integer;
  for My_Int2'Alignment use 16;

  type Arr2 is array (1 .. 2) of My_Int2;

begin
   null;
end Alignment14;
