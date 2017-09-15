------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--  G N A T . T R A C E B A C K . S Y M B O L I C . M O D U L E _ N A M E   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2012-2017, AdaCore                     --
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

--  This is the default version of this package

separate (System.Traceback.Symbolic)

package body Module_Name is

   ---------------------------------
   -- Build_Cache_For_All_Modules --
   ---------------------------------

   procedure Build_Cache_For_All_Modules is
   begin
      null;
   end Build_Cache_For_All_Modules;

   ---------
   -- Get --
   ---------

   function Get (Addr : access System.Address) return String is
      pragma Unreferenced (Addr);

   begin
      return "";
   end Get;

   ------------------
   -- Is_Supported --
   ------------------

   function Is_Supported return Boolean is
   begin
      return False;
   end Is_Supported;

end Module_Name;
