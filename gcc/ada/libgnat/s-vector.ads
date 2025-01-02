------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                       S Y S T E M . V E C T O R S                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2002-2025, Free Software Foundation, Inc.         --
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

--  This package defines a datatype which is most efficient for performing
--  logical operations on large arrays. See
--  :ref:`System.Generic_Vector_Operations`.

--  In the future this package may also define operations such as element-wise
--  addition, subtraction, multiplication, minimum and maximum of vector-sized
--  packed arrays of Unsigned_8, Unsigned_16 and Unsigned_32 values. These
--  operations could be implemented as system intrinsics on platforms with
--  direct processor support for them.

package System.Vectors is
   pragma Pure;

   type Vector is mod 2**System.Word_Size;
   for Vector'Alignment use Integer'Min
     (Standard'Maximum_Alignment, System.Word_Size / System.Storage_Unit);
   for Vector'Size use System.Word_Size;
   --  The *Vector* type uses an alignment that allows translating it into
   --  words.

end System.Vectors;
