with Vect3_Pkg;

package Vect3 is

   -- Unconstrained array types are vectorizable, possibly with special
   -- help for the programmer
   type Varray is array (Vect3_Pkg.Index_Type range <>) of Long_Float;
   for Varray'Alignment use 16;

   function "+" (X, Y : Varray) return Varray;
   procedure Add (X, Y : not null access Varray; R : not null access Varray);


   -- Constrained array types are vectorizable
   type Sarray is array (Vect3_Pkg.Index_Type(1) .. Vect3_Pkg.Index_Type(4))
     of Long_Float;
   for Sarray'Alignment use 16;

   function "+" (X, Y : Sarray) return Sarray;
   procedure Add (X, Y : not null access Sarray; R : not null access Sarray);


   type Darray1 is array (Vect3_Pkg.Index_Type(1) .. Vect3_Pkg.N) of Long_Float;
   for Darray1'Alignment use 16;

   function "+" (X, Y : Darray1) return Darray1;
   procedure Add (X, Y : not null access Darray1; R : not null access Darray1);


   type Darray2 is array (Vect3_Pkg.K .. Vect3_Pkg.Index_Type(4)) of Long_Float;
   for Darray2'Alignment use 16;

   function "+" (X, Y : Darray2) return Darray2;
   procedure Add (X, Y : not null access Darray2; R : not null access Darray2);


   type Darray3 is array (Vect3_Pkg.K .. Vect3_Pkg.N) of Long_Float;
   for Darray3'Alignment use 16;

   function "+" (X, Y : Darray3) return Darray3;
   procedure Add (X, Y : not null access Darray3; R : not null access Darray3);

end Vect3;
