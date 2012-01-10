with Vect2_Pkg;

package Vect2 is

   -- Unconstrained array types are vectorizable, possibly with special
   -- help for the programmer
   type Varray is array (Positive range <>) of Long_Float;
   for Varray'Alignment use 16;

   function "+" (X, Y : Varray) return Varray;
   procedure Add (X, Y : Varray; R : out Varray);
   procedure Add (X, Y : not null access Varray; R : not null access Varray);


   -- Constrained array types are vectorizable
   type Sarray is array (Positive(1) .. Positive(4)) of Long_Float;
   for Sarray'Alignment use 16;

   function "+" (X, Y : Sarray) return Sarray;
   procedure Add (X, Y : Sarray; R : out Sarray);
   procedure Add (X, Y : not null access Sarray; R : not null access Sarray);


   type Darray1 is array (Positive(1) .. Vect2_Pkg.N) of Long_Float;
   for Darray1'Alignment use 16;

   function "+" (X, Y : Darray1) return Darray1;
   procedure Add (X, Y : Darray1; R : out Darray1);
   procedure Add (X, Y : not null access Darray1; R : not null access Darray1);


   type Darray2 is array (Vect2_Pkg.K .. Positive(4)) of Long_Float;
   for Darray2'Alignment use 16;

   function "+" (X, Y : Darray2) return Darray2;
   procedure Add (X, Y : Darray2; R : out Darray2);
   procedure Add (X, Y : not null access Darray2; R : not null access Darray2);


   type Darray3 is array (Vect2_Pkg.K .. Vect2_Pkg.N) of Long_Float;
   for Darray3'Alignment use 16;

   function "+" (X, Y : Darray3) return Darray3;
   procedure Add (X, Y : Darray3; R : out Darray3);
   procedure Add (X, Y : not null access Darray3; R : not null access Darray3);

end Vect2;
