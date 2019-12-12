
-- { dg-final { scan-tree-dump "= 1145258561|= 1094861636" "optimized" } }

package Opt82_Pkg is

  type Rec is record
    A, B, C, D : Character;
  end record;

end Opt82_Pkg;
