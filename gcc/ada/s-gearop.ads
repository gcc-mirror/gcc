------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                     SYSTEM.GENERIC_ARRAY_OPERATIONS                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--            Copyright (C) 2006, Free Software Foundation, Inc.            --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

package System.Generic_Array_Operations is
pragma Pure (Generic_Array_Operations);

   --------------------------
   -- Square_Matrix_Length --
   --------------------------

   generic
      type Scalar is private;
      type Matrix is array (Integer range <>, Integer range <>) of Scalar;
   function Square_Matrix_Length (A : Matrix) return Natural;
   --  If A is non-square, raise Constraint_Error,  else return its dimension

   ----------------------------------
   -- Vector_Elementwise_Operation --
   ----------------------------------

   generic
      type X_Scalar is private;
      type Result_Scalar is private;
      type X_Vector is array (Integer range <>) of X_Scalar;
      type Result_Vector is array (Integer range <>) of Result_Scalar;
      with function Operation (X : X_Scalar) return Result_Scalar;
   function Vector_Elementwise_Operation (X : X_Vector) return Result_Vector;

   ----------------------------------
   -- Matrix_Elementwise_Operation --
   ----------------------------------

   generic
      type X_Scalar is private;
      type Result_Scalar is private;
      type X_Matrix is array (Integer range <>, Integer range <>) of X_Scalar;
      type Result_Matrix is array (Integer range <>, Integer range <>)
        of Result_Scalar;
      with function Operation (X : X_Scalar) return Result_Scalar;
   function Matrix_Elementwise_Operation (X : X_Matrix) return Result_Matrix;

   -----------------------------------------
   -- Vector_Vector_Elementwise_Operation --
   -----------------------------------------

   generic
      type Left_Scalar is private;
      type Right_Scalar is private;
      type Result_Scalar is private;
      type Left_Vector is array (Integer range <>) of Left_Scalar;
      type Right_Vector is array (Integer range <>) of Right_Scalar;
      type Result_Vector is array (Integer range <>) of Result_Scalar;
      with function Operation
             (Left  : Left_Scalar;
              Right : Right_Scalar) return Result_Scalar;
   function Vector_Vector_Elementwise_Operation
     (Left  : Left_Vector;
      Right : Right_Vector) return Result_Vector;

   ------------------------------------------------
   -- Vector_Vector_Scalar_Elementwise_Operation --
   ------------------------------------------------

   generic
      type X_Scalar is private;
      type Y_Scalar is private;
      type Z_Scalar is private;
      type Result_Scalar is private;
      type X_Vector is array (Integer range <>) of X_Scalar;
      type Y_Vector is array (Integer range <>) of Y_Scalar;
      type Result_Vector is array (Integer range <>) of Result_Scalar;
      with function Operation
             (X : X_Scalar;
              Y : Y_Scalar;
              Z : Z_Scalar) return Result_Scalar;
   function Vector_Vector_Scalar_Elementwise_Operation
     (X : X_Vector;
      Y : Y_Vector;
      Z : Z_Scalar) return Result_Vector;

   -----------------------------------------
   -- Matrix_Matrix_Elementwise_Operation --
   -----------------------------------------

   generic
      type Left_Scalar is private;
      type Right_Scalar is private;
      type Result_Scalar is private;
      type Left_Matrix is array (Integer range <>, Integer range <>)
        of Left_Scalar;
      type Right_Matrix is array (Integer range <>, Integer range <>)
        of Right_Scalar;
      type Result_Matrix is array (Integer range <>, Integer range <>)
        of Result_Scalar;
      with function Operation
             (Left  : Left_Scalar;
              Right : Right_Scalar) return Result_Scalar;
   function Matrix_Matrix_Elementwise_Operation
     (Left  : Left_Matrix;
      Right : Right_Matrix) return Result_Matrix;

   ------------------------------------------------
   -- Matrix_Matrix_Scalar_Elementwise_Operation --
   ------------------------------------------------

   generic
      type X_Scalar is private;
      type Y_Scalar is private;
      type Z_Scalar is private;
      type Result_Scalar is private;
      type X_Matrix is array (Integer range <>, Integer range <>) of X_Scalar;
      type Y_Matrix is array (Integer range <>, Integer range <>) of Y_Scalar;
      type Result_Matrix is array (Integer range <>, Integer range <>)
        of Result_Scalar;
      with function Operation
             (X : X_Scalar;
              Y : Y_Scalar;
              Z : Z_Scalar) return Result_Scalar;
   function Matrix_Matrix_Scalar_Elementwise_Operation
     (X : X_Matrix;
      Y : Y_Matrix;
      Z : Z_Scalar) return Result_Matrix;

   -----------------------------------------
   -- Vector_Scalar_Elementwise_Operation --
   -----------------------------------------

   generic
      type Left_Scalar is private;
      type Right_Scalar is private;
      type Result_Scalar is private;
      type Left_Vector is array (Integer range <>) of Left_Scalar;
      type Result_Vector is array (Integer range <>) of Result_Scalar;
      with function Operation
             (Left  : Left_Scalar;
              Right : Right_Scalar) return Result_Scalar;
   function Vector_Scalar_Elementwise_Operation
     (Left  : Left_Vector;
      Right : Right_Scalar) return Result_Vector;

   -----------------------------------------
   -- Matrix_Scalar_Elementwise_Operation --
   -----------------------------------------

   generic
      type Left_Scalar is private;
      type Right_Scalar is private;
      type Result_Scalar is private;
      type Left_Matrix is array (Integer range <>, Integer range <>)
        of Left_Scalar;
      type Result_Matrix is array (Integer range <>, Integer range <>)
        of Result_Scalar;
      with function Operation
             (Left  : Left_Scalar;
              Right : Right_Scalar) return Result_Scalar;
   function Matrix_Scalar_Elementwise_Operation
     (Left  : Left_Matrix;
      Right : Right_Scalar) return Result_Matrix;

   -----------------------------------------
   -- Scalar_Vector_Elementwise_Operation --
   -----------------------------------------

   generic
      type Left_Scalar is private;
      type Right_Scalar is private;
      type Result_Scalar is private;
      type Right_Vector is array (Integer range <>) of Right_Scalar;
      type Result_Vector is array (Integer range <>) of Result_Scalar;
      with function Operation
             (Left  : Left_Scalar;
              Right : Right_Scalar) return Result_Scalar;
   function Scalar_Vector_Elementwise_Operation
     (Left  : Left_Scalar;
      Right : Right_Vector) return Result_Vector;

   -----------------------------------------
   -- Scalar_Matrix_Elementwise_Operation --
   -----------------------------------------

   generic
      type Left_Scalar is private;
      type Right_Scalar is private;
      type Result_Scalar is private;
      type Right_Matrix is array (Integer range <>, Integer range <>)
        of Right_Scalar;
      type Result_Matrix is array (Integer range <>, Integer range <>)
        of Result_Scalar;
      with function Operation
             (Left  : Left_Scalar;
              Right : Right_Scalar) return Result_Scalar;
   function Scalar_Matrix_Elementwise_Operation
     (Left  : Left_Scalar;
      Right : Right_Matrix) return Result_Matrix;

   -------------------
   -- Inner_Product --
   -------------------

   generic
      type Left_Scalar is private;
      type Right_Scalar is private;
      type Result_Scalar is private;
      type Left_Vector is array (Integer range <>) of Left_Scalar;
      type Right_Vector is array (Integer range <>) of Right_Scalar;
      Zero : Result_Scalar;
      with function "*"
             (Left  : Left_Scalar;
              Right : Right_Scalar) return Result_Scalar is <>;
      with function "+"
             (Left  : Result_Scalar;
              Right : Result_Scalar) return Result_Scalar is <>;
   function Inner_Product
     (Left  : Left_Vector;
      Right : Right_Vector) return Result_Scalar;

   -------------------
   -- Outer_Product --
   -------------------

   generic
      type Left_Scalar is private;
      type Right_Scalar is private;
      type Result_Scalar is private;
      type Left_Vector is array (Integer range <>) of Left_Scalar;
      type Right_Vector is array (Integer range <>) of Right_Scalar;
      type Matrix is array (Integer range <>, Integer range <>)
        of Result_Scalar;
      with function "*"
             (Left  : Left_Scalar;
              Right : Right_Scalar) return Result_Scalar is <>;
   function Outer_Product
     (Left  : Left_Vector;
      Right : Right_Vector) return Matrix;

   ---------------------------
   -- Matrix_Vector_Product --
   ---------------------------

   generic
      type Left_Scalar is private;
      type Right_Scalar is private;
      type Result_Scalar is private;
      type Matrix is array (Integer range <>, Integer range <>)
        of Left_Scalar;
      type Right_Vector is array (Integer range <>) of Right_Scalar;
      type Result_Vector is array (Integer range <>) of Result_Scalar;
      Zero : Result_Scalar;
      with function "*"
             (Left  : Left_Scalar;
              Right : Right_Scalar) return Result_Scalar is <>;
      with function "+"
             (Left  : Result_Scalar;
              Right : Result_Scalar) return Result_Scalar is <>;
   function Matrix_Vector_Product
     (Left  : Matrix;
      Right : Right_Vector) return Result_Vector;

   ---------------------------
   -- Vector_Matrix_Product --
   ---------------------------

   generic
      type Left_Scalar is private;
      type Right_Scalar is private;
      type Result_Scalar is private;
      type Left_Vector is array (Integer range <>) of Left_Scalar;
      type Matrix is array (Integer range <>, Integer range <>)
        of Right_Scalar;
      type Result_Vector is array (Integer range <>) of Result_Scalar;
      Zero : Result_Scalar;
      with function "*"
             (Left  : Left_Scalar;
              Right : Right_Scalar) return Result_Scalar is <>;
      with function "+"
             (Left  : Result_Scalar;
              Right : Result_Scalar) return Result_Scalar is <>;
   function Vector_Matrix_Product
     (Left  : Left_Vector;
      Right : Matrix) return Result_Vector;

   ---------------------------
   -- Matrix_Matrix_Product --
   ---------------------------

   generic
      type Left_Scalar is private;
      type Right_Scalar is private;
      type Result_Scalar is private;
      type Left_Matrix is array (Integer range <>, Integer range <>)
        of Left_Scalar;
      type Right_Matrix is array (Integer range <>, Integer range <>)
        of Right_Scalar;
      type Result_Matrix is array (Integer range <>, Integer range <>)
        of Result_Scalar;
      Zero : Result_Scalar;
      with function "*"
             (Left  : Left_Scalar;
              Right : Right_Scalar) return Result_Scalar is <>;
      with function "+"
             (Left  : Result_Scalar;
              Right : Result_Scalar) return Result_Scalar is <>;
   function Matrix_Matrix_Product
     (Left  : Left_Matrix;
      Right : Right_Matrix) return Result_Matrix;

   ---------------
   -- Transpose --
   ---------------

   generic
      type Scalar is private;
      type Matrix is array (Integer range <>, Integer range <>) of Scalar;
   procedure Transpose (A : Matrix; R : out Matrix);

   -------------------------------
   -- Update_Vector_With_Vector --
   -------------------------------

   generic
      type X_Scalar is private;
      type Y_Scalar is private;
      type X_Vector is array (Integer range <>) of X_Scalar;
      type Y_Vector is array (Integer range <>) of Y_Scalar;
      with procedure Update (X : in out X_Scalar; Y : Y_Scalar);
   procedure Update_Vector_With_Vector (X : in out X_Vector; Y : Y_Vector);

   -------------------------------
   -- Update_Matrix_With_Matrix --
   -------------------------------

   generic
      type X_Scalar is private;
      type Y_Scalar is private;
      type X_Matrix is array (Integer range <>, Integer range <>) of X_Scalar;
      type Y_Matrix is array (Integer range <>, Integer range <>) of Y_Scalar;
      with procedure Update (X : in out X_Scalar; Y : Y_Scalar);
   procedure Update_Matrix_With_Matrix (X : in out X_Matrix; Y : Y_Matrix);

   -----------------
   -- Unit_Matrix --
   -----------------

   generic
      type Scalar is private;
      type Matrix is array (Integer range <>, Integer range <>) of Scalar;
      Zero : Scalar;
      One  : Scalar;
   function Unit_Matrix
     (Order   : Positive;
      First_1 : Integer := 1;
      First_2 : Integer := 1) return Matrix;

   -----------------
   -- Unit_Vector --
   -----------------

   generic
      type Scalar is private;
      type Vector is array (Integer range <>) of Scalar;
      Zero : Scalar;
      One  : Scalar;
   function Unit_Vector
     (Index : Integer;
      Order : Positive;
      First : Integer := 1) return Vector;

end System.Generic_Array_Operations;
