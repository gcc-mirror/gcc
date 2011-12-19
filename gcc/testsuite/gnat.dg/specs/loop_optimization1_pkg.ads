-- { dg-excess-errors "no code generated" }

generic

  type Element_Type is private;
  type Index_Type is (<>);
  type Constrained_Array_Type is array (Index_Type) of Element_Type;

package Loop_Optimization1_Pkg is

  procedure Proc (CA : in out Constrained_Array_Type);

end Loop_Optimization1_Pkg;
