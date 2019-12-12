with Prot9_Pkg2;

package Prot9_Pkg1 is

   type Prot_Type is limited private;

private

   type Prot_Type is new Prot9_Pkg2.Prot_Type;

end Prot9_Pkg1;
