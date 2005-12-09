------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                I N T E R F A C E S . C . P O I N T E R S                 --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1993-2005, Free Software Foundation, Inc.         --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
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

generic
   type Index is (<>);
   type Element is private;
   type Element_Array is array (Index range <>) of aliased Element;
   Default_Terminator : Element;

package Interfaces.C.Pointers is
   pragma Preelaborate;

   type Pointer is access all Element;

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
