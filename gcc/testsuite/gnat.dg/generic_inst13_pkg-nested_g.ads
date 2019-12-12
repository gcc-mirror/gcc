with Generic_Inst13_Pkg.Ops_G;

generic
package Generic_Inst13_Pkg.Nested_G is

  type T is new Generic_Inst13_Pkg.T;

  package My_Operations is new Generic_Inst13_Pkg.Ops_G (T);

  subtype List_T is My_Operations.List_T;

  function "or" (Left, Right : T) return List_T renames My_Operations."or";

end Generic_Inst13_Pkg.Nested_G;
