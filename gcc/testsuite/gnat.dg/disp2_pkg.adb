package body Disp2_Pkg is

  function Impl_Of (Self : access Object) return Object_Ptr is
  begin
    return Object_Ptr (Self);
  end Impl_Of;

end Disp2_Pkg;
