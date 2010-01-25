------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                 S Y S T E M . C O M M U N I C A T I O N                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2001-2009, AdaCore                     --
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

package body System.Communication is

   subtype SEO is Ada.Streams.Stream_Element_Offset;

   ----------------
   -- Last_Index --
   ----------------

   function Last_Index
     (First : Ada.Streams.Stream_Element_Offset;
      Count : CRTL.size_t) return Ada.Streams.Stream_Element_Offset
   is
      use type Ada.Streams.Stream_Element_Offset;
      use type System.CRTL.size_t;
   begin
      if First = SEO'First and then Count = 0 then
         raise Constraint_Error with
           "last index out of range (no element transferred)";
      else
         return First + SEO (Count) - 1;
      end if;
   end Last_Index;

end System.Communication;
