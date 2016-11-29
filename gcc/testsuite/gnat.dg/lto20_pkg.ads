package Lto20_Pkg is

  type Arr is private;

  Null_Arr : constant Arr;

  procedure Proc (A : Arr);

private

  type Obj;

  type Handle is access Obj;

  Null_Handle : constant Handle := null;

  type Arr is array (1 .. 2) of Handle;

  Null_Arr : constant Arr := (others => Null_Handle);

end Lto20_Pkg;
