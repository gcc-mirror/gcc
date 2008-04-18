with Dynamic_Elab_Pkg; use Dynamic_Elab_Pkg;

package Dynamic_Elab2 is

  type Plot is record
    B : Boolean;
    Data : R;
  end record;
  pragma Pack (Plot);

  function Get_Plot return Plot;

end Dynamic_Elab2;
