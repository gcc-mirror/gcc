--  { dg-do run }
--  { dg-options "-gnatws -gnatVa" }

pragma Initialize_Scalars;
procedure init_scalar1 is
  type Fixed_3T is delta 2.0 ** (- 4)
      range - 2.0 ** 19 ..  (2.0 ** 19 - 2.0 ** (- 4));
  for Fixed_3T'Size use 3*8;

  Write_Value : constant Fixed_3T := Fixed_3T(524287.875);
  type singleton is array (1 .. 1) of Fixed_3T;
  pragma Pack (singleton);
  it : Singleton;
begin
   null;
end;
