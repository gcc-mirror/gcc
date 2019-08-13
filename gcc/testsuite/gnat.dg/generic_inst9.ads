with Generic_Inst9_Pkg2;
with Generic_Inst9_Pkg1; use Generic_Inst9_Pkg1;

package Generic_Inst9 is

  package Partition is new Generic_Inst9_Pkg2
    (Item_T => Generic_Inst9_Pkg1.R, Bound_T => Generic_Inst9_Pkg1.R);

  procedure Dummy;

end Generic_Inst9;
