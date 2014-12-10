-- { dg-excess-errors "cannot generate code" }

package Concat1_Pkg is

   function Id_For (Name : String) return Natural;

   function Image_Of return String;

end Concat1_Pkg;
