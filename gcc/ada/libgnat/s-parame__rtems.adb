------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    S Y S T E M . P A R A M E T E R S                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1997-2009 Free Software Foundation, Inc.          --
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

--  This is the RTEMS specific version

with Interfaces.C;

package body System.Parameters is

   function ada_pthread_minimum_stack_size return Interfaces.C.size_t;
   pragma Import (C, ada_pthread_minimum_stack_size,
     "_ada_pthread_minimum_stack_size");

   ------------------------
   -- Default_Stack_Size --
   ------------------------

   function Default_Stack_Size return Size_Type is
   begin
      return Size_Type (ada_pthread_minimum_stack_size);
   end Default_Stack_Size;

   ------------------------
   -- Minimum_Stack_Size --
   ------------------------

   function Minimum_Stack_Size return Size_Type is

   begin
      return Size_Type (ada_pthread_minimum_stack_size);
   end Minimum_Stack_Size;

   -------------------------
   -- Adjust_Storage_Size --
   -------------------------

   function Adjust_Storage_Size (Size : Size_Type) return Size_Type is
   begin
      if Size = Unspecified_Size then
         return Default_Stack_Size;

      elsif Size < Minimum_Stack_Size then
         return Minimum_Stack_Size;

      else
         return Size;
      end if;
   end Adjust_Storage_Size;

end System.Parameters;
