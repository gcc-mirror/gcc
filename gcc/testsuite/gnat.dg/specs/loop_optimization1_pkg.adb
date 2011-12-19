package body Loop_Optimization1_Pkg is

  type Unconstrained_Array_Type
    is array (Index_Type range <>) of Element_Type;

  procedure Local (UA : in out Unconstrained_Array_Type) is
  begin
    null;
  end;

  procedure Proc (CA : in out Constrained_Array_Type) is
  begin
    Local (Unconstrained_Array_Type (CA));
  end;

end Loop_Optimization1_Pkg;
