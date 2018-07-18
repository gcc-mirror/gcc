-- { dg-do compile { target i?86-*-* x86_64-*-* } }
-- { dg-options "-O3 -msse2 -fno-vect-cost-model -fdump-tree-vect-details" }

package body Vect6 is

   function "+" (X : Varray; Y : Long_Float) return Varray is
      R : Varray (X'Range);
   begin
      for I in X'Range loop
         R(I) := X(I) + Y;
      end loop;
      return R;
   end;

   procedure Add (X : Varray; Y : Long_Float; R : out Varray) is
   begin
      for I in X'Range loop
         R(I) := X(I) + Y;
      end loop;
   end;

   procedure Add (X : not null access Varray; Y : Long_Float; R : not null access Varray) is
   begin
      for I in X'Range loop
         R(I) := X(I) + Y;
      end loop;
   end;


   function "+" (X : Sarray; Y : Long_Float) return Sarray is
      R : Sarray;
   begin
      for I in Sarray'Range loop
         R(I) := X(I) + Y;
      end loop;
      return R;
   end;

   procedure Add (X : Sarray; Y : Long_Float; R : out Sarray) is
   begin
      for I in Sarray'Range loop
         R(I) := X(I) + Y;
      end loop;
   end;

   procedure Add (X : not null access Sarray; Y : Long_Float; R : not null access Sarray) is
   begin
      for I in Sarray'Range loop
         R(I) := X(I) + Y;
      end loop;
   end;


   function "+" (X : Darray1; Y : Long_Float) return Darray1 is
      R : Darray1;
   begin
      for I in Darray1'Range loop
         R(I) := X(I) + Y;
      end loop;
      return R;
   end;

   procedure Add (X : Darray1; Y : Long_Float; R : out Darray1) is
   begin
      for I in Darray1'Range loop
         R(I) := X(I) + Y;
      end loop;
   end;

   procedure Add (X : not null access Darray1; Y : Long_Float; R : not null access Darray1) is
   begin
      for I in Darray1'Range loop
         R(I) := X(I) + Y;
      end loop;
   end;


   function "+" (X : Darray2; Y : Long_Float) return Darray2 is
      R : Darray2;
   begin
      for I in Darray2'Range loop
         R(I) := X(I) + Y;
      end loop;
      return R;
   end;

   procedure Add (X : Darray2; Y : Long_Float; R : out Darray2) is
   begin
      for I in Darray2'Range loop
         R(I) := X(I) + Y;
      end loop;
   end;

   procedure Add (X : not null access Darray2; Y : Long_Float; R : not null access Darray2) is
   begin
      for I in Darray2'Range loop
         R(I) := X(I) + Y;
      end loop;
   end;


   function "+" (X : Darray3; Y : Long_Float) return Darray3 is
      R : Darray3;
   begin
      for I in Darray3'Range loop
         R(I) := X(I) + Y;
      end loop;
      return R;
   end;

   procedure Add (X : Darray3; Y : Long_Float; R : out Darray3) is
   begin
      for I in Darray3'Range loop
         R(I) := X(I) + Y;
      end loop;
   end;

   procedure Add (X : not null access Darray3; Y : Long_Float; R : not null access Darray3) is
   begin
      for I in Darray3'Range loop
         R(I) := X(I) + Y;
      end loop;
   end;

end Vect6;

-- { dg-final { scan-tree-dump-times "vectorized 1 loops" 15 "vect"  } }
