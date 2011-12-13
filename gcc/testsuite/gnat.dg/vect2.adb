-- { dg-do compile { target i?86-*-* x86_64-*-* } }
-- { dg-options "-O3 -msse2 -fdump-tree-vect-details" }

package body Vect2 is

   function "+" (X, Y : Varray) return Varray is
      R : Varray (X'Range);
   begin
      for I in X'Range loop
         R(I) := X(I) + Y(I);
      end loop;
      return R;
   end;

   procedure Add (X, Y : Varray; R : out Varray) is
   begin
      for I in X'Range loop
         R(I) := X(I) + Y(I);
      end loop;
   end;

   procedure Add (X, Y : not null access Varray; R : not null access Varray) is
   begin
      for I in X'Range loop
         R(I) := X(I) + Y(I);
      end loop;
   end;


   function "+" (X, Y : Sarray) return Sarray is
      R : Sarray;
   begin
      for I in Sarray'Range loop
         R(I) := X(I) + Y(I);
      end loop;
      return R;
   end;

   procedure Add (X, Y : Sarray; R : out Sarray) is
   begin
      for I in Sarray'Range loop
         R(I) := X(I) + Y(I);
      end loop;
   end;

   procedure Add (X, Y : not null access Sarray; R : not null access Sarray) is
   begin
      for I in Sarray'Range loop
         R(I) := X(I) + Y(I);
      end loop;
   end;


   function "+" (X, Y : Darray1) return Darray1 is
      R : Darray1;
   begin
      for I in Darray1'Range loop
         R(I) := X(I) + Y(I);
      end loop;
      return R;
   end;

   procedure Add (X, Y : Darray1; R : out Darray1) is
   begin
      for I in Darray1'Range loop
         R(I) := X(I) + Y(I);
      end loop;
   end;

   procedure Add (X, Y : not null access Darray1; R : not null access Darray1) is
   begin
      for I in Darray1'Range loop
         R(I) := X(I) + Y(I);
      end loop;
   end;


   function "+" (X, Y : Darray2) return Darray2 is
      R : Darray2;
   begin
      for I in Darray2'Range loop
         R(I) := X(I) + Y(I);
      end loop;
      return R;
   end;

   procedure Add (X, Y : Darray2; R : out Darray2) is
   begin
      for I in Darray2'Range loop
         R(I) := X(I) + Y(I);
      end loop;
   end;

   procedure Add (X, Y : not null access Darray2; R : not null access Darray2) is
   begin
      for I in Darray2'Range loop
         R(I) := X(I) + Y(I);
      end loop;
   end;


   function "+" (X, Y : Darray3) return Darray3 is
      R : Darray3;
   begin
      for I in Darray3'Range loop
         R(I) := X(I) + Y(I);
      end loop;
      return R;
   end;

   procedure Add (X, Y : Darray3; R : out Darray3) is
   begin
      for I in Darray3'Range loop
         R(I) := X(I) + Y(I);
      end loop;
   end;

   procedure Add (X, Y : not null access Darray3; R : not null access Darray3) is
   begin
      for I in Darray3'Range loop
         R(I) := X(I) + Y(I);
      end loop;
   end;

end Vect2;

-- { dg-final { scan-tree-dump-times "vectorized 1 loops" 15 "vect"  } }
-- { dg-final { cleanup-tree-dump "vect" } }
