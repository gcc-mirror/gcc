------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--               E R R I D . S W I T C H _ R E P O S I T O R Y              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2026, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

package body Errid.Switch_Repository is

   -------------------
   -- Get_Switch_Id --
   -------------------

   function Get_Switch_Id (Name : String) return Switch_Id is
      Trimmed_Name : constant String :=
        (if Name (Name'Last) = ' ' then Name (Name'First .. Name'Last - 1)
         else Name);
   begin
      for I in Active_Switch_Id loop
         if Switches (I).Short_Name.all = Trimmed_Name then
            return I;
         end if;
      end loop;

      return No_Switch_Id;
   end Get_Switch_Id;

end Errid.Switch_Repository;
