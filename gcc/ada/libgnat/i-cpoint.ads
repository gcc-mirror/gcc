------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                I N T E R F A C E S . C . P O I N T E R S                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1993-2023, Free Software Foundation, Inc.         --
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

with System.Parameters;

generic
   type Index is (<>);
   type Element is private;
   type Element_Array is array (Index range <>) of aliased Element;
   Default_Terminator : Element;

package Interfaces.C.Pointers is
   pragma Preelaborate;

   type Pointer is access all Element;
   for Pointer'Size use System.Parameters.ptr_bits;

   pragma No_Strict_Aliasing (Pointer);
   --  We turn off any strict aliasing assumptions for the pointer type,
   --  since it is possible to create "improperly" aliased values.

   function Value
     (Ref        : Pointer;
      Terminator : Element := Default_Terminator) return Element_Array;

   function Value
     (Ref    : Pointer;
      Length : ptrdiff_t) return Element_Array;

   Pointer_Error : exception;

   --------------------------------
   -- C-style Pointer Arithmetic --
   --------------------------------

   function "+" (Left : Pointer;   Right : ptrdiff_t) return Pointer;
   function "+" (Left : ptrdiff_t; Right : Pointer)   return Pointer;
   function "-" (Left : Pointer;   Right : ptrdiff_t) return Pointer;
   function "-" (Left : Pointer;   Right : Pointer)   return ptrdiff_t;

   procedure Increment (Ref : in out Pointer);
   procedure Decrement (Ref : in out Pointer);

   pragma Convention (Intrinsic, "+");
   pragma Convention (Intrinsic, "-");
   pragma Convention (Intrinsic, Increment);
   pragma Convention (Intrinsic, Decrement);

   function Virtual_Length
     (Ref        : Pointer;
      Terminator : Element := Default_Terminator) return ptrdiff_t;

   procedure Copy_Terminated_Array
     (Source     : Pointer;
      Target     : Pointer;
      Limit      : ptrdiff_t := ptrdiff_t'Last;
      Terminator : Element := Default_Terminator);

   procedure Copy_Array
     (Source  : Pointer;
      Target  : Pointer;
      Length  : ptrdiff_t);

private
   pragma Inline ("+");
   pragma Inline ("-");
   pragma Inline (Decrement);
   pragma Inline (Increment);

end Interfaces.C.Pointers;
