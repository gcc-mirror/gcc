------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                           S Y S T E M . D I M                            --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--             Copyright (C) 2012, Free Software Foundation, Inc.           --
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

--  Defines the dimension terminology

---------------------------
-- Dimension Terminology --
---------------------------

--  * Dimensioned type

--    A dimensioned type is a type (more accurately a first subtype) to which
--    the aspect Dimension_System applies to.

--      type Mks_Type is new Long_Long_Float
--        with
--         Dimension_System => ((Meter, 'm'),
--           (Kilogram, "kg"),
--           (Second,   's'),
--           (Ampere,   'A'),
--           (Kelvin,   'K'),
--           (Mole,     "mol"),
--           (Candela,  "cd"));

--      'm' is the symbolic name of dimension Meter

--  * Dimensioned subtype

--    A dimensioned subtype is a subtype directly defined from the dimensioned
--    type and to which the aspect Dimension applies to.

--      subtype Length is Mks_Type
--        with
--         Dimension => ('m',
--           Meter =>  1,
--           others => 0);

--      'm' is the symbolic name of dimensioned subtype Length

package System.Dim is
   pragma Pure;

end System.Dim;
