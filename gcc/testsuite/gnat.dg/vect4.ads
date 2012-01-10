with Vect4_Pkg;

package Vect4 is

   -- Unconstrained array types are vectorizable, possibly with special
   -- help for the programmer
   type Varray is array (Integer range <>) of Long_Float;
   for Varray'Alignment use 16;

   function "+" (X : Varray; Y : Long_Float) return Varray;
   procedure Add (X : Varray; Y : Long_Float; R : out Varray);
   procedure Add (X : not null access Varray; Y : Long_Float; R : not null access Varray);


   -- Constrained array types are vectorizable
   type Sarray is array (1 .. 4) of Long_Float;
   for Sarray'Alignment use 16;

   function "+" (X : Sarray; Y : Long_Float) return Sarray;
   procedure Add (X : Sarray; Y : Long_Float; R : out Sarray);
   procedure Add (X : not null access Sarray; Y : Long_Float; R : not null access Sarray);


   type Darray1 is array (1 .. Vect4_Pkg.N) of Long_Float;
   for Darray1'Alignment use 16;

   function "+" (X : Darray1; Y : Long_Float) return Darray1;
   procedure Add (X : Darray1; Y : Long_Float; R : out Darray1);
   procedure Add (X : not null access Darray1; Y : Long_Float; R : not null access Darray1);


   type Darray2 is array (Vect4_Pkg.K .. 4) of Long_Float;
   for Darray2'Alignment use 16;

   function "+" (X : Darray2; Y : Long_Float) return Darray2;
   procedure Add (X : Darray2; Y : Long_Float; R : out Darray2);
   procedure Add (X : not null access Darray2; Y : Long_Float; R : not null access Darray2);


   type Darray3 is array (Vect4_Pkg.K .. Vect4_Pkg.N) of Long_Float;
   for Darray3'Alignment use 16;

   function "+" (X : Darray3; Y : Long_Float) return Darray3;
   procedure Add (X : Darray3; Y : Long_Float; R : out Darray3);
   procedure Add (X : not null access Darray3; Y : Long_Float; R : not null access Darray3);

end Vect4;
