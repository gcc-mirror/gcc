------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                   I N T E R F A C E S . F O R T R A N                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Numerics.Generic_Complex_Types;
pragma Elaborate_All (Ada.Numerics.Generic_Complex_Types);

package Interfaces.Fortran is
   pragma Pure;

   type Fortran_Integer  is new Integer;
   type Real             is new Float;
   type Double_Precision is new Long_Float;

   type Logical is new Boolean;
   for Logical'Size use Integer'Size;
   pragma Convention (Fortran, Logical);
   --  As required by Fortran standard, logical allocates same space as
   --  an integer. The convention is important, since in Fortran, Booleans
   --  are implemented with zero/non-zero semantics for False/True, and the
   --  pragma Convention (Fortran) activates the special handling required
   --  in this case.

   package Single_Precision_Complex_Types is
      new Ada.Numerics.Generic_Complex_Types (Real);

   package Double_Precision_Complex_Types is
      new Ada.Numerics.Generic_Complex_Types (Double_Precision);

   type Complex is new Single_Precision_Complex_Types.Complex;

   type Double_Complex is new Double_Precision_Complex_Types.Complex;

   subtype Imaginary is Single_Precision_Complex_Types.Imaginary;
   i : Imaginary renames Single_Precision_Complex_Types.i;
   j : Imaginary renames Single_Precision_Complex_Types.j;

   type Character_Set is new Character;

   type Fortran_Character is array (Positive range <>) of Character_Set;

   --  Additional declarations as permitted by Ada 2012, p.608, paragraph 21.
   --  Interoperability with Fortran 77's vendor extension using star
   --  notation and Fortran 90's intrinsic types with kind=n parameter.
   --  The following assumes that `n' matches the byte size, which
   --  most Fortran compiler, including GCC's follow.

   type Integer_Star_1  is new Integer_8;
   type Integer_Kind_1  is new Integer_8;
   type Integer_Star_2  is new Integer_16;
   type Integer_Kind_2  is new Integer_16;
   type Integer_Star_4  is new Integer_32;
   type Integer_Kind_4  is new Integer_32;
   type Integer_Star_8  is new Integer_64;
   type Integer_Kind_8  is new Integer_64;

   type Logical_Star_1  is new Boolean;
   type Logical_Star_2  is new Boolean;
   type Logical_Star_4  is new Boolean;
   type Logical_Star_8  is new Boolean;
   type Logical_Kind_1  is new Boolean;
   type Logical_Kind_2  is new Boolean;
   type Logical_Kind_4  is new Boolean;
   type Logical_Kind_8  is new Boolean;
   for Logical_Star_1'Size use Integer_8'Size;
   for Logical_Star_2'Size use Integer_16'Size;
   for Logical_Star_4'Size use Integer_32'Size;
   for Logical_Star_8'Size use Integer_64'Size;
   for Logical_Kind_1'Size use Integer_8'Size;
   for Logical_Kind_2'Size use Integer_16'Size;
   for Logical_Kind_4'Size use Integer_32'Size;
   for Logical_Kind_8'Size use Integer_64'Size;
   pragma Convention (Fortran, Logical_Star_1);
   pragma Convention (Fortran, Logical_Star_2);
   pragma Convention (Fortran, Logical_Star_4);
   pragma Convention (Fortran, Logical_Star_8);
   pragma Convention (Fortran, Logical_Kind_1);
   pragma Convention (Fortran, Logical_Kind_2);
   pragma Convention (Fortran, Logical_Kind_4);
   pragma Convention (Fortran, Logical_Kind_8);

   type Real_Star_4  is new Float;
   type Real_Kind_4  is new Float;
   type Real_Star_8  is new Long_Float;
   type Real_Kind_8  is new Long_Float;

   --  In the kind syntax, n is the same as the associated real kind.
   --  In the star syntax, n is twice as large (real+imaginary size)
   type Complex_Star_8  is new Complex;
   type Complex_Kind_4  is new Complex;
   type Complex_Star_16 is new Double_Complex;
   type Complex_Kind_8  is new Double_Complex;

   type Character_Kind_n is new Fortran_Character;

   function To_Fortran (Item : Character)     return Character_Set;
   function To_Ada     (Item : Character_Set) return Character;

   function To_Fortran (Item : String)            return Fortran_Character;
   function To_Ada     (Item : Fortran_Character) return String;

   procedure To_Fortran
     (Item   : String;
      Target : out Fortran_Character;
      Last   : out Natural);

   procedure To_Ada
     (Item   : Fortran_Character;
      Target : out String;
      Last   : out Natural);

end Interfaces.Fortran;
