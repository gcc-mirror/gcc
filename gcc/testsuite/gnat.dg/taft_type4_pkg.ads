package Taft_Type4_Pkg is

  type T is private;

  procedure Proc (L : in out T);
  pragma Inline (Proc);

private

  type Obj_T;
  type T is access Obj_T;

end Taft_Type4_Pkg;
