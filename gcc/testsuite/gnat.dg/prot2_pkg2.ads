generic

   type T is private;

package Prot2_Pkg2 is

   type Id is private;

   procedure Create (B : out Id);
   procedure Delete (B : in out Id);

private

   type Rec;
   type Id is access Rec;

end Prot2_Pkg2;
