with Vect6_Pkg;

package Vect6 is

   -- Unconstrained array types are vectorizable, possibly with special
   -- help for the programmer
   type Varray is array (Vect6_Pkg.Index_Type range <>) of Long_Float;
   for Varray'Alignment use 16;

   function "+" (X : Varray; Y : Long_Float) return Varray;
   procedure Add (X : not null access Varray; Y : Long_Float; R : not null access Varray);


   -- Constrained array types are vectorizable
   type Sarray is array (Vect6_Pkg.Index_Type(1) .. Vect6_Pkg.Index_Type(4))
     of Long_Float;
   for Sarray'Alignment use 16;

   function "+" (X : Sarray; Y : Long_Float) return Sarray;
   procedure Add (X : not null access Sarray; Y : Long_Float; R : not null access Sarray);


   type Darray1 is array (Vect6_Pkg.Index_Type(1) .. Vect6_Pkg.N) of Long_Float;
   for Darray1'Alignment use 16;

   function "+" (X : Darray1; Y : Long_Float) return Darray1;
   procedure Add (X : not null access Darray1; Y : Long_Float; R : not null access Darray1);


   type Darray2 is array (Vect6_Pkg.K .. Vect6_Pkg.Index_Type(4)) of Long_Float;
   for Darray2'Alignment use 16;

   function "+" (X : Darray2; Y : Long_Float) return Darray2;
   procedure Add (X : not null access Darray2; Y : Long_Float; R : not null access Darray2);


   type Darray3 is array (Vect6_Pkg.K .. Vect6_Pkg.N) of Long_Float;
   for Darray3'Alignment use 16;

   function "+" (X : Darray3; Y : Long_Float) return Darray3;
   procedure Add (X : not null access Darray3; Y : Long_Float; R : not null access Darray3);

end Vect6;
