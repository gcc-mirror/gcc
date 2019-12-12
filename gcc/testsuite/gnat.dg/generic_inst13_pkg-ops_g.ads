generic
  type Data_T is private;
package Generic_Inst13_Pkg.Ops_G is

  type List_T is array (Positive range <>) of Data_T;

  function "or" (Left, Right : Data_T) return List_T is ((Left, Right));

end Generic_Inst13_Pkg.Ops_G;