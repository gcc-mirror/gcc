with Generic_Inst9_Pkg1.Operator;

package body Generic_Inst9_Pkg2 is

  package My_Operator is new Generic_Inst9_Pkg1.Operator (Bound_T);

  procedure Dummy is begin null; end;

end Generic_Inst9_Pkg2;
