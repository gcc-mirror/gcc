generic

  N : Natural;

package Discr25_Pkg is

  type T is private;

  procedure Proc1 (Set : in out T);

private
  type Obj_T (Size_Max : Natural);
  type T is access Obj_T;

end Discr25_Pkg;
