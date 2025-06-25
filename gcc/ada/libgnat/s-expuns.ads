------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . E X P _ U N S                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2025, Free Software Foundation, Inc.         --
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

--  This function performs exponentiation of unsigned types (with binary
--  modulus values up to and including that of Unsigned_Types.Unsigned).
--
--  The result is always full width, the caller must do a masking operation
--  the modulus is less than 2 ** (Unsigned'Size).

with System.Exponu;
with System.Unsigned_Types;

package System.Exp_Uns
  with SPARK_Mode
is
   subtype Unsigned is Unsigned_Types.Unsigned;

   function Exp_Unsigned is new Exponu (Unsigned);
   pragma Pure_Function (Exp_Unsigned);
   --  Return the power of ``Left`` by ``Right`` where ``Left`` is an
   --  Unsigned.
   --
   --  This function is implemented using the standard logarithmic approach:
   --  ``Right`` gets shifted right testing successive low order bits, and
   --  ``Left`` is raised to the next power of 2.
   --
   --  In case of overflow, Constraint_Error is raised.

end System.Exp_Uns;
