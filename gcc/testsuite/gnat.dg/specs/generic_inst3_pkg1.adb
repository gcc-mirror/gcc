with Generic_Inst3_Pkg2; use Generic_Inst3_Pkg2;
with Generic_Inst3_Pkg3, Generic_Inst3_Pkg3.Child;

package body Generic_Inst3_Pkg1 is

  package Pkg3 is new Generic_Inst3_Pkg3 (T);

  use Pkg3;

  package Child is new Pkg3.Child;

  procedure Proc is null;

end Generic_Inst3_Pkg1;
