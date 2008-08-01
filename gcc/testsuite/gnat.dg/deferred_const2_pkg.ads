package Deferred_Const2_Pkg is

  I : Integer := 16#20_3A_2D_28#;

  pragma Warnings (Off);
  S : constant string(1..4);
  for S'address use I'address;
  pragma Import (Ada, S);

  procedure Dummy;

end Deferred_Const2_Pkg;
