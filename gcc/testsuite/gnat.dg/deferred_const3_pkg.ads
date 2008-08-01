package Deferred_Const3_Pkg is

  C : constant Natural := 1;

  C1 : constant Natural := 1;
  for C1'Address use C'Address;

  C2 : constant Natural;
  for C2'Address use C'Address;

  C3 : constant Natural;

  procedure Dummy;

private
  C2 : constant Natural := 1;

  C3 : constant Natural := 1;
  for C3'Address use C'Address;

end Deferred_Const3_Pkg;
