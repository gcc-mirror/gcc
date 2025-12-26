with Aggr34_Pkg3;
with Aggr34_Pkg2;

generic
   with package My_Config is new Aggr34_Pkg3;
package Aggr34_Pkg1 is
   package My_Module_Basic_Config is new Aggr34_Pkg2 (My_Config);
   procedure Proc;
end Aggr34_Pkg1;
