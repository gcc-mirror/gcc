------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    S Y S T E M . P A R A M E T E R S                     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1995-2025, Free Software Foundation, Inc.         --
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

--  This is the version for AArch64 QNX

package body System.Parameters is

   -------------------------
   -- Adjust_Storage_Size --
   -------------------------

   function Adjust_Storage_Size (Size : Size_Type) return Size_Type is
   begin
      return
         (if Size = Unspecified_Size then Default_Stack_Size
          elsif Size < Minimum_Stack_Size then Minimum_Stack_Size
          else Size
         );
   end Adjust_Storage_Size;

   ------------------------
   -- Default_Stack_Size --
   ------------------------

   function Default_Stack_Size return Size_Type is
      Default_Stack_Size : constant Integer;
      pragma Import (C, Default_Stack_Size, "__gl_default_stack_size");
   begin
      return
         (if Default_Stack_Size = -1
          then
             (256 * 1024) --  256K is the default stack size on aarch64 QNX
          elsif Size_Type (Default_Stack_Size) < Minimum_Stack_Size
          then
             Minimum_Stack_Size
          else
             Size_Type (Default_Stack_Size)
         );
   end Default_Stack_Size;

   ------------------------
   -- Minimum_Stack_Size --
   ------------------------

   function Minimum_Stack_Size return Size_Type is
   begin
      --  256 is the value of PTHREAD_STACK_MIN on QNX and
      --  12K is required for stack-checking. The value is
      --  rounded up to a multiple of a 4K page.
      return 16 * 1024;
   end Minimum_Stack_Size;

end System.Parameters;
