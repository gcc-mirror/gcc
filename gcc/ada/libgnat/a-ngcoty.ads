------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--   A D A . N U M E R I C S . G E N E R I C _ C O M P L E X _ T Y P E S    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2025, Free Software Foundation, Inc.         --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
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

generic
   type Real is digits <>;

package Ada.Numerics.Generic_Complex_Types is
   pragma Pure;

   type Complex is record
      Re, Im : Real'Base;
   end record;

   pragma Complex_Representation (Complex);

   type Imaginary is private
   with Preelaborable_Initialization;

   i : constant Imaginary;
   j : constant Imaginary;

   function Re (X : Complex)   return Real'Base;
   function Im (X : Complex)   return Real'Base;
   function Im (X : Imaginary) return Real'Base;

   procedure Set_Re (X : in out Complex; Re : Real'Base);
   procedure Set_Im (X : in out Complex; Im : Real'Base);
   procedure Set_Im (X : out Imaginary;  Im : Real'Base);

   function Compose_From_Cartesian (Re, Im : Real'Base) return Complex;
   function Compose_From_Cartesian (Re     : Real'Base) return Complex;
   function Compose_From_Cartesian (Im     : Imaginary) return Complex;

   function Modulus (X     : Complex) return Real'Base;
   function "abs"   (Right : Complex) return Real'Base renames Modulus;

   function Argument (X : Complex)                    return Real'Base;
   function Argument (X : Complex; Cycle : Real'Base) return Real'Base;

   function Compose_From_Polar (
     Modulus, Argument : Real'Base)
     return Complex;

   function Compose_From_Polar (
     Modulus, Argument, Cycle : Real'Base)
     return Complex;

   function "+"       (Right : Complex) return Complex;
   function "-"       (Right : Complex) return Complex;
   function Conjugate (X     : Complex) return Complex;

   function "+"       (Left, Right : Complex) return Complex;
   function "-"       (Left, Right : Complex) return Complex;
   function "*"       (Left, Right : Complex) return Complex;
   function "/"       (Left, Right : Complex) return Complex;

   function "**"      (Left : Complex; Right : Integer) return Complex;

   function "+"       (Right : Imaginary) return Imaginary;
   function "-"       (Right : Imaginary) return Imaginary;
   function Conjugate (X     : Imaginary) return Imaginary renames "-";
   function "abs"     (Right : Imaginary) return Real'Base;

   function "+"       (Left, Right : Imaginary) return Imaginary;
   function "-"       (Left, Right : Imaginary) return Imaginary;
   function "*"       (Left, Right : Imaginary) return Real'Base;
   function "/"       (Left, Right : Imaginary) return Real'Base;

   function "**"      (Left : Imaginary; Right : Integer) return Complex;

   function "<"       (Left, Right : Imaginary) return Boolean;
   function "<="      (Left, Right : Imaginary) return Boolean;
   function ">"       (Left, Right : Imaginary) return Boolean;
   function ">="      (Left, Right : Imaginary) return Boolean;

   function "+"       (Left : Complex;   Right : Real'Base) return Complex;
   function "+"       (Left : Real'Base; Right : Complex)   return Complex;
   function "-"       (Left : Complex;   Right : Real'Base) return Complex;
   function "-"       (Left : Real'Base; Right : Complex)   return Complex;
   function "*"       (Left : Complex;   Right : Real'Base) return Complex;
   function "*"       (Left : Real'Base; Right : Complex)   return Complex;
   function "/"       (Left : Complex;   Right : Real'Base) return Complex;
   function "/"       (Left : Real'Base; Right : Complex)   return Complex;

   function "+"       (Left : Complex;   Right : Imaginary) return Complex;
   function "+"       (Left : Imaginary; Right : Complex)   return Complex;
   function "-"       (Left : Complex;   Right : Imaginary) return Complex;
   function "-"       (Left : Imaginary; Right : Complex)   return Complex;
   function "*"       (Left : Complex;   Right : Imaginary) return Complex;
   function "*"       (Left : Imaginary; Right : Complex)   return Complex;
   function "/"       (Left : Complex;   Right : Imaginary) return Complex;
   function "/"       (Left : Imaginary; Right : Complex)   return Complex;

   function "+"       (Left : Imaginary; Right : Real'Base) return Complex;
   function "+"       (Left : Real'Base; Right : Imaginary) return Complex;
   function "-"       (Left : Imaginary; Right : Real'Base) return Complex;
   function "-"       (Left : Real'Base; Right : Imaginary) return Complex;

   function "*"       (Left : Imaginary; Right : Real'Base) return Imaginary;
   function "*"       (Left : Real'Base; Right : Imaginary) return Imaginary;
   function "/"       (Left : Imaginary; Right : Real'Base) return Imaginary;
   function "/"       (Left : Real'Base; Right : Imaginary) return Imaginary;

private
   type Imaginary is new Real'Base;

   i : constant Imaginary := 1.0;
   j : constant Imaginary := 1.0;

   pragma Inline ("+");
   pragma Inline ("-");
   pragma Inline ("*");
   pragma Inline ("<");
   pragma Inline ("<=");
   pragma Inline (">");
   pragma Inline (">=");
   pragma Inline ("abs");
   pragma Inline (Compose_From_Cartesian);
   pragma Inline (Conjugate);
   pragma Inline (Im);
   pragma Inline (Re);
   pragma Inline (Set_Im);
   pragma Inline (Set_Re);

end Ada.Numerics.Generic_Complex_Types;
