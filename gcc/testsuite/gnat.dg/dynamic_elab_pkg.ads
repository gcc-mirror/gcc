package Dynamic_Elab_Pkg is

  type R is record
    Code : Integer;
    Val  : Boolean;
  end record;

  function Get_R return R;

end Dynamic_Elab_Pkg;
