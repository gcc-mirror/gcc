------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       E X P _ A T T R _ L I G H T                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--              Copyright (C) 2011, Free Software Foundation, Inc.          --
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

with Exp_Attr; use Exp_Attr;
with Sinfo;    use Sinfo;
with Snames;   use Snames;

package body Exp_Attr_Light is

   ----------------------------------------
   -- Expand_Light_N_Attribute_Reference --
   ----------------------------------------

   procedure Expand_Light_N_Attribute_Reference (N : Node_Id) is
      Id : constant Attribute_Id := Get_Attribute_Id (Attribute_Name (N));

   begin
      case Id is
         when Attribute_Old    |
              Attribute_Result =>
            Expand_N_Attribute_Reference (N);

         when others =>
            null;
      end case;
   end Expand_Light_N_Attribute_Reference;

end Exp_Attr_Light;
