with Ada.Containers.Vectors;
with Lto21_Pkg2;

package Lto21_Pkg1 is

   pragma Suppress (Tampering_Check);

   package Vect1 is new Ada.Containers.Vectors (Positive, Natural);

end Lto21_Pkg1;
