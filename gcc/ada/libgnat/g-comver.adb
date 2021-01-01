------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                G N A T . C O M P I L E R _ V E R S I O N                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2002-2021, AdaCore                     --
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

--  This package provides a routine for obtaining the version number of the
--  GNAT compiler used to compile the program. It relies on the generated
--  constant in the binder generated package that records this information.

package body GNAT.Compiler_Version is

   Ver_Len_Max : constant := 256;
   --  This is logically a reference to Gnatvsn.Ver_Len_Max but we cannot
   --  import this directly since run-time units cannot WITH compiler units.

   Ver_Prefix : constant String := "GNAT Version: ";
   --  This is logically a reference to Gnatvsn.Ver_Prefix but we cannot
   --  import this directly since run-time units cannot WITH compiler units.

   GNAT_Version : constant String (1 .. Ver_Len_Max + Ver_Prefix'Length);
   pragma Import (C, GNAT_Version, "__gnat_version");

   -------------
   -- Version --
   -------------

   function Version return String is
   begin
      --  Search for terminating right paren or NUL ending the string

      for J in Ver_Prefix'Length + 1 .. GNAT_Version'Last loop
         if GNAT_Version (J) = ')' then
            return GNAT_Version (Ver_Prefix'Length + 1 .. J);
         end if;

         if GNAT_Version (J) = Character'Val (0) then
            return GNAT_Version (Ver_Prefix'Length + 1 .. J - 1);
         end if;
      end loop;

      --  This should not happen (no right paren or NUL found)

      return GNAT_Version;
   end Version;

end GNAT.Compiler_Version;
