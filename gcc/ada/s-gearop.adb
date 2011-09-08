------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--       S Y S T E M . G E N E R I C _ A R R A Y _ O P E R A T I O N S      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2006-2011, Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

package body System.Generic_Array_Operations is

   --  The local function Check_Unit_Last computes the index
   --  of the last element returned by Unit_Vector or Unit_Matrix.
   --  A separate function is needed to allow raising Constraint_Error
   --  before declaring the function result variable. The result variable
   --  needs to be declared first, to allow front-end inlining.

   function Check_Unit_Last
     (Index : Integer;
      Order : Positive;
      First : Integer) return Integer;
   pragma Inline_Always (Check_Unit_Last);

   --------------
   -- Diagonal --
   --------------

   function Diagonal (A : Matrix) return Vector is

      N : constant Natural := Natural'Min (A'Length (1), A'Length (2));
      R : Vector (A'First (1) .. A'First (1) + N - 1);

   begin
      for J in 0 .. N - 1 loop
         R (R'First + J) := A (A'First (1) + J, A'First (2) + J);
      end loop;

      return R;
   end Diagonal;

   --------------------------
   -- Square_Matrix_Length --
   --------------------------

   function Square_Matrix_Length (A : Matrix) return Natural is
   begin
      if A'Length (1) /= A'Length (2) then
         raise Constraint_Error with "matrix is not square";
      end if;

      return A'Length (1);
   end Square_Matrix_Length;

   ---------------------
   -- Check_Unit_Last --
   ---------------------

   function Check_Unit_Last
      (Index : Integer;
       Order : Positive;
       First : Integer) return Integer is
   begin
      --  Order the tests carefully to avoid overflow

      if Index < First
           or else First > Integer'Last - Order + 1
           or else Index > First + (Order - 1)
      then
         raise Constraint_Error;
      end if;

      return First + (Order - 1);
   end Check_Unit_Last;

   ---------------------
   -- Back_Substitute --
   ---------------------

   procedure Back_Substitute (M, N : in out Matrix) is
      pragma Assert (M'First (1) = N'First (1) and then
                     M'Last  (1) = N'Last (1));

      Max_Col : Integer := M'Last (2);

      procedure Sub_Row
        (M      : in out Matrix;
         Target : Integer;
         Source : Integer;
         Factor : Scalar);
      --  Needs comments ???

      procedure Sub_Row
        (M      : in out Matrix;
         Target : Integer;
         Source : Integer;
         Factor : Scalar)
      is
      begin
         for J in M'Range (2) loop
            M (Target, J) := M (Target, J) - Factor * M (Source, J);
         end loop;
      end Sub_Row;

   --  Start of processing for Back_Substitute

   begin
      for Row in reverse M'Range (1) loop
         Find_Non_Zero : for Col in M'First (2) .. Max_Col loop
            if Is_Non_Zero (M (Row, Col)) then

               --  Found first non-zero element, so subtract a multiple
               --  of this row from all higher rows, to reduce all other
               --  elements in this column to zero.

               for J in M'First (1) .. Row - 1 loop
                  Sub_Row (N, J, Row, (M (J, Col) / M (Row, Col)));
                  Sub_Row (M, J, Row, (M (J, Col) / M (Row, Col)));
               end loop;

               Max_Col := Col - 1;
               exit Find_Non_Zero;
            end if;
         end loop Find_Non_Zero;
      end loop;
   end Back_Substitute;

   -----------------------
   -- Forward_Eliminate --
   -----------------------

   procedure Forward_Eliminate
     (M   : in out Matrix;
      N   : in out Matrix;
      Det : out Scalar)
   is
      pragma Assert (M'First (1) = N'First (1) and then
                     M'Last  (1) = N'Last (1));

      function "abs" (X : Scalar) return Scalar is
        (if X < Zero then Zero - X else X);

      procedure Sub_Row
        (M : in out Matrix;
         Target : Integer;
         Source : Integer;
         Factor : Scalar);
      --  Needs commenting ???

      procedure Divide_Row
        (M, N  : in out Matrix;
         Row   : Integer;
         Scale : Scalar);
      --  Needs commenting ???

      procedure Switch_Row
        (M, N  : in out Matrix;
         Row_1 : Integer;
         Row_2 : Integer);
      --  Needs commenting ???

      -------------
      -- Sub_Row --
      -------------

      procedure Sub_Row
        (M      : in out Matrix;
         Target : Integer;
         Source : Integer;
         Factor : Scalar)
         is
      begin
         for J in M'Range (2) loop
            M (Target, J) := M (Target, J) - Factor * M (Source, J);
         end loop;
      end Sub_Row;

      ----------------
      -- Divide_Row --
      ----------------

      procedure Divide_Row
        (M, N  : in out Matrix;
         Row   : Integer;
         Scale : Scalar)
      is
      begin
         Det := Det * Scale;

         for J in M'Range (2) loop
            M (Row, J) := M (Row, J) / Scale;
         end loop;

         for J in N'Range (2) loop
            N (Row - M'First (1) + N'First (1), J)
               := N (Row - M'First (1) + N'First (1), J) / Scale;
         end loop;
      end Divide_Row;

      ----------------
      -- Switch_Row --
      ----------------

      procedure Switch_Row
        (M, N  : in out Matrix;
         Row_1 : Integer;
         Row_2 : Integer)
      is
         procedure Swap (X, Y : in out Scalar);
         --  Exchange the values of X and Y

         procedure Swap (X, Y : in out Scalar) is
            T : constant Scalar := X;
         begin
            X := Y;
            Y := T;
         end Swap;

      --  Start of processing for Switch_Row

      begin
         if Row_1 /= Row_2 then
            Det := Zero - Det;

            for J in M'Range (2) loop
               Swap (M (Row_1, J), M (Row_2, J));
            end loop;

            for J in N'Range (2) loop
               Swap (N (Row_1 - M'First (1) + N'First (1), J),
                     N (Row_2 - M'First (1) + N'First (1), J));
            end loop;
         end if;
      end Switch_Row;

      I : Integer := M'First (1);
      --  Avoid use of I ???

   --  Start of processing for Forward_Eliminate

   begin
      Det := One;

      for J in M'Range (2) loop
         declare
            Max_I   : Integer := I;
            Max_Abs : Scalar := Zero;

         begin
            --  Find best pivot in column J, starting in row I

            for K in I .. M'Last (1) loop
               declare
                  New_Abs : constant Scalar := abs M (K, J);
               begin
                  if Max_Abs < New_Abs then
                     Max_Abs := New_Abs;
                     Max_I := K;
                  end if;
               end;
            end loop;

            if Zero < Max_Abs then
               Switch_Row (M, N, I, Max_I);
               Divide_Row (M, N, I, M (I, J));

               for U in I + 1 .. M'Last (1) loop
                  Sub_Row (N, U, I, M (U, J));
                  Sub_Row (M, U, I, M (U, J));
               end loop;

               exit when I >= M'Last (1);

               I := I + 1;

            else
               Det := Zero; --  Zero, but we don't have literals
            end if;
         end;
      end loop;
   end Forward_Eliminate;

   -------------------
   -- Inner_Product --
   -------------------

   function Inner_Product
     (Left  : Left_Vector;
      Right : Right_Vector)
      return  Result_Scalar
   is
      R : Result_Scalar := Zero;

   begin
      if Left'Length /= Right'Length then
         raise Constraint_Error with
            "vectors are of different length in inner product";
      end if;

      for J in Left'Range loop
         R := R + Left (J) * Right (J - Left'First + Right'First);
      end loop;

      return R;
   end Inner_Product;

   -------------
   -- L2_Norm --
   -------------

   function L2_Norm (X : Vector) return Scalar is
   begin
      return Sqrt (Inner_Product (X, X));
   end L2_Norm;

   ----------------------------------
   -- Matrix_Elementwise_Operation --
   ----------------------------------

   function Matrix_Elementwise_Operation (X : X_Matrix) return Result_Matrix is
      R : Result_Matrix (X'Range (1), X'Range (2));

   begin
      for J in R'Range (1) loop
         for K in R'Range (2) loop
            R (J, K) := Operation (X (J, K));
         end loop;
      end loop;

      return R;
   end Matrix_Elementwise_Operation;

   ----------------------------------
   -- Vector_Elementwise_Operation --
   ----------------------------------

   function Vector_Elementwise_Operation (X : X_Vector) return Result_Vector is
      R : Result_Vector (X'Range);

   begin
      for J in R'Range loop
         R (J) := Operation (X (J));
      end loop;

      return R;
   end Vector_Elementwise_Operation;

   -----------------------------------------
   -- Matrix_Matrix_Elementwise_Operation --
   -----------------------------------------

   function Matrix_Matrix_Elementwise_Operation
     (Left  : Left_Matrix;
      Right : Right_Matrix)
      return Result_Matrix
   is
      R : Result_Matrix (Left'Range (1), Left'Range (2));

   begin
      if Left'Length (1) /= Right'Length (1)
        or else Left'Length (2) /= Right'Length (2)
      then
         raise Constraint_Error with
            "matrices are of different dimension in elementwise operation";
      end if;

      for J in R'Range (1) loop
         for K in R'Range (2) loop
            R (J, K) :=
              Operation
                (Left (J, K),
                 Right
                   (J - R'First (1) + Right'First (1),
                    K - R'First (2) + Right'First (2)));
         end loop;
      end loop;

      return R;
   end Matrix_Matrix_Elementwise_Operation;

   ------------------------------------------------
   -- Matrix_Matrix_Scalar_Elementwise_Operation --
   ------------------------------------------------

   function Matrix_Matrix_Scalar_Elementwise_Operation
     (X    : X_Matrix;
      Y    : Y_Matrix;
      Z    : Z_Scalar) return Result_Matrix
   is
      R : Result_Matrix (X'Range (1), X'Range (2));

   begin
      if X'Length (1) /= Y'Length (1)
        or else X'Length (2) /= Y'Length (2)
      then
         raise Constraint_Error with
            "matrices are of different dimension in elementwise operation";
      end if;

      for J in R'Range (1) loop
         for K in R'Range (2) loop
            R (J, K) :=
              Operation
                (X (J, K),
                 Y (J - R'First (1) + Y'First (1),
                    K - R'First (2) + Y'First (2)),
                 Z);
         end loop;
      end loop;

      return R;
   end Matrix_Matrix_Scalar_Elementwise_Operation;

   -----------------------------------------
   -- Vector_Vector_Elementwise_Operation --
   -----------------------------------------

   function Vector_Vector_Elementwise_Operation
     (Left  : Left_Vector;
      Right : Right_Vector) return Result_Vector
   is
      R : Result_Vector (Left'Range);

   begin
      if Left'Length /= Right'Length then
         raise Constraint_Error with
            "vectors are of different length in elementwise operation";
      end if;

      for J in R'Range loop
         R (J) := Operation (Left (J), Right (J - R'First + Right'First));
      end loop;

      return R;
   end Vector_Vector_Elementwise_Operation;

   ------------------------------------------------
   -- Vector_Vector_Scalar_Elementwise_Operation --
   ------------------------------------------------

   function Vector_Vector_Scalar_Elementwise_Operation
     (X : X_Vector;
      Y : Y_Vector;
      Z : Z_Scalar) return Result_Vector
   is
      R : Result_Vector (X'Range);

   begin
      if X'Length /= Y'Length then
         raise Constraint_Error with
            "vectors are of different length in elementwise operation";
      end if;

      for J in R'Range loop
         R (J) := Operation (X (J), Y (J - X'First + Y'First), Z);
      end loop;

      return R;
   end Vector_Vector_Scalar_Elementwise_Operation;

   -----------------------------------------
   -- Matrix_Scalar_Elementwise_Operation --
   -----------------------------------------

   function Matrix_Scalar_Elementwise_Operation
     (Left  : Left_Matrix;
      Right : Right_Scalar) return Result_Matrix
   is
      R : Result_Matrix (Left'Range (1), Left'Range (2));

   begin
      for J in R'Range (1) loop
         for K in R'Range (2) loop
            R (J, K) := Operation (Left (J, K), Right);
         end loop;
      end loop;

      return R;
   end Matrix_Scalar_Elementwise_Operation;

   -----------------------------------------
   -- Vector_Scalar_Elementwise_Operation --
   -----------------------------------------

   function Vector_Scalar_Elementwise_Operation
     (Left  : Left_Vector;
      Right : Right_Scalar) return Result_Vector
   is
      R : Result_Vector (Left'Range);

   begin
      for J in R'Range loop
         R (J) := Operation (Left (J), Right);
      end loop;

      return R;
   end Vector_Scalar_Elementwise_Operation;

   -----------------------------------------
   -- Scalar_Matrix_Elementwise_Operation --
   -----------------------------------------

   function Scalar_Matrix_Elementwise_Operation
     (Left  : Left_Scalar;
      Right : Right_Matrix) return Result_Matrix
   is
      R : Result_Matrix (Right'Range (1), Right'Range (2));

   begin
      for J in R'Range (1) loop
         for K in R'Range (2) loop
            R (J, K) := Operation (Left, Right (J, K));
         end loop;
      end loop;

      return R;
   end Scalar_Matrix_Elementwise_Operation;

   -----------------------------------------
   -- Scalar_Vector_Elementwise_Operation --
   -----------------------------------------

   function Scalar_Vector_Elementwise_Operation
     (Left  : Left_Scalar;
      Right : Right_Vector) return Result_Vector
   is
      R : Result_Vector (Right'Range);

   begin
      for J in R'Range loop
         R (J) := Operation (Left, Right (J));
      end loop;

      return R;
   end Scalar_Vector_Elementwise_Operation;

   ---------------------------
   -- Matrix_Matrix_Product --
   ---------------------------

   function Matrix_Matrix_Product
     (Left  : Left_Matrix;
      Right : Right_Matrix) return Result_Matrix
   is
      R : Result_Matrix (Left'Range (1), Right'Range (2));

   begin
      if Left'Length (2) /= Right'Length (1) then
         raise Constraint_Error with
            "incompatible dimensions in matrix multiplication";
      end if;

      for J in R'Range (1) loop
         for K in R'Range (2) loop
            declare
               S : Result_Scalar := Zero;

            begin
               for M in Left'Range (2) loop
                  S := S + Left (J, M)
                            * Right (M - Left'First (2) + Right'First (1), K);
               end loop;

               R (J, K) := S;
            end;
         end loop;
      end loop;

      return R;
   end  Matrix_Matrix_Product;

   ---------------------------
   -- Matrix_Vector_Product --
   ---------------------------

   function Matrix_Vector_Product
     (Left  : Matrix;
      Right : Right_Vector) return Result_Vector
   is
      R : Result_Vector (Left'Range (1));

   begin
      if Left'Length (2) /= Right'Length then
         raise Constraint_Error with
            "incompatible dimensions in matrix-vector multiplication";
      end if;

      for J in Left'Range (1) loop
         declare
            S : Result_Scalar := Zero;

         begin
            for K in Left'Range (2) loop
               S := S + Left (J, K) * Right (K - Left'First (2) + Right'First);
            end loop;

            R (J) := S;
         end;
      end loop;

      return R;
   end Matrix_Vector_Product;

   -------------------
   -- Outer_Product --
   -------------------

   function Outer_Product
     (Left  : Left_Vector;
      Right : Right_Vector) return Matrix
   is
      R : Matrix (Left'Range, Right'Range);

   begin
      for J in R'Range (1) loop
         for K in R'Range (2) loop
            R (J, K) := Left (J) * Right (K);
         end loop;
      end loop;

      return R;
   end Outer_Product;

   -----------------
   -- Swap_Column --
   -----------------

   procedure Swap_Column (A : in out Matrix; Left, Right : Integer) is
      Temp : Scalar;
   begin
      for J in A'Range (1) loop
         Temp := A (J, Left);
         A (J, Left) := A (J, Right);
         A (J, Right) := Temp;
      end loop;
   end Swap_Column;

   ---------------
   -- Transpose --
   ---------------

   procedure Transpose (A : Matrix; R : out Matrix) is
   begin
      for J in R'Range (1) loop
         for K in R'Range (2) loop
            R (J, K) := A (K - R'First (2) + A'First (1),
                           J - R'First (1) + A'First (2));
         end loop;
      end loop;
   end Transpose;

   -------------------------------
   -- Update_Matrix_With_Matrix --
   -------------------------------

   procedure Update_Matrix_With_Matrix (X : in out X_Matrix; Y : Y_Matrix) is
   begin
      if X'Length (1) /= Y'Length (1)
        or else X'Length (2) /= Y'Length (2)
      then
         raise Constraint_Error with
            "matrices are of different dimension in update operation";
      end if;

      for J in X'Range (1) loop
         for K in X'Range (2) loop
            Update (X (J, K), Y (J - X'First (1) + Y'First (1),
                                 K - X'First (2) + Y'First (2)));
         end loop;
      end loop;
   end Update_Matrix_With_Matrix;

   -------------------------------
   -- Update_Vector_With_Vector --
   -------------------------------

   procedure Update_Vector_With_Vector (X : in out X_Vector; Y : Y_Vector) is
   begin
      if X'Length /= Y'Length then
         raise Constraint_Error with
            "vectors are of different length in update operation";
      end if;

      for J in X'Range loop
         Update (X (J), Y (J - X'First + Y'First));
      end loop;
   end Update_Vector_With_Vector;

   -----------------
   -- Unit_Matrix --
   -----------------

   function Unit_Matrix
     (Order   : Positive;
      First_1 : Integer := 1;
      First_2 : Integer := 1) return Matrix
   is
      R : Matrix (First_1 .. Check_Unit_Last (First_1, Order, First_1),
                  First_2 .. Check_Unit_Last (First_2, Order, First_2));

   begin
      R := (others => (others => Zero));

      for J in 0 .. Order - 1 loop
         R (First_1 + J, First_2 + J) := One;
      end loop;

      return R;
   end Unit_Matrix;

   -----------------
   -- Unit_Vector --
   -----------------

   function Unit_Vector
     (Index : Integer;
      Order : Positive;
      First : Integer := 1) return Vector
   is
      R : Vector (First .. Check_Unit_Last (Index, Order, First));
   begin
      R := (others => Zero);
      R (Index) := One;
      return R;
   end Unit_Vector;

   ---------------------------
   -- Vector_Matrix_Product --
   ---------------------------

   function Vector_Matrix_Product
     (Left  : Left_Vector;
      Right : Matrix) return Result_Vector
   is
      R : Result_Vector (Right'Range (2));

   begin
      if Left'Length /= Right'Length (2) then
         raise Constraint_Error with
            "incompatible dimensions in vector-matrix multiplication";
      end if;

      for J in Right'Range (2) loop
         declare
            S : Result_Scalar := Zero;

         begin
            for K in Right'Range (1) loop
               S := S + Left (J - Right'First (1) + Left'First) * Right (K, J);
            end loop;

            R (J) := S;
         end;
      end loop;

      return R;
   end Vector_Matrix_Product;

end System.Generic_Array_Operations;
