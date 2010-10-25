package Loop_Optimization8_Pkg2 is

  type Array_T is array (Natural range <>) of Integer;

  type Obj_T (Length : Natural) is
    record
      Elements : Array_T (1 .. Length);
    end record;

  type T is access Obj_T;

  function Length (Set : T) return Natural;
  function Index (Set : T; Position : Natural) return Integer;
  pragma Inline (Length, Index);

end Loop_Optimization8_Pkg2;
