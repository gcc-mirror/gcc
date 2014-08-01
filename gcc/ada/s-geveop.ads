------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--      S Y S T E M . G E N E R I C _ V E C T O R _ O P E R A T I O N S     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2002-2014, Free Software Foundation, Inc.         --
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

--  This package contains generic procedures for vector operations on arrays.
--  If the arguments are aligned on word boundaries and the word size is a
--  multiple M of the element size, the operations will be done M elements
--  at a time using vector operations on a word.

--  All routines assume argument arrays have the same length, and arguments
--  with mode "in" do not alias arguments with mode "out" or "in out".
--  If the number N of elements to be processed is not a multiple of M
--  the final N rem M elements will be processed one item at a time.

with System.Vectors;
with System.Storage_Elements;

generic
   type Element is (<>);
   type Index is (<>);
   type Element_Array is array (Index range <>) of Element;

package System.Generic_Vector_Operations is
   pragma Pure;

   generic
      with function Element_Op (X, Y : Element) return Element;
      with function Vector_Op (X, Y : Vectors.Vector) return Vectors.Vector;
   procedure Binary_Operation
     (R, X, Y : System.Address;
      Length  : System.Storage_Elements.Storage_Count);

   generic
      with function Element_Op (X : Element) return Element;
      with function Vector_Op (X : Vectors.Vector) return Vectors.Vector;
   procedure Unary_Operation
     (R, X    : System.Address;
      Length  : System.Storage_Elements.Storage_Count);
end System.Generic_Vector_Operations;
