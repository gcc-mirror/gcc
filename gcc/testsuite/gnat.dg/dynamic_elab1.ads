with Dynamic_Elab_Pkg; use Dynamic_Elab_Pkg;

package Dynamic_Elab1 is

  type Plot is record
    Data : R;
  end record;
  pragma Pack (Plot);

  function Get_Plot return Plot;

end Dynamic_Elab1;
