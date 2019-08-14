package body Inline18_Pkg1 is

  procedure Proc (R : in out Rec) is
  begin
    R.Comp := My_G2.Func (Inline18_Pkg2.Child.General.Val);
  end;

end Inline18_Pkg1;
