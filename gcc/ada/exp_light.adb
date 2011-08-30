------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                            E X P _ L I G H T                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2011, Free Software Foundation, Inc.           --
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

with Atree;          use Atree;
with Exp_Attr_Light; use Exp_Attr_Light;
with Exp_Ch6_Light;  use Exp_Ch6_Light;
with Exp_Ch7_Light;  use Exp_Ch7_Light;
with Sinfo;          use Sinfo;

package body Exp_Light is

   ------------------
   -- Expand_Light --
   ------------------

   procedure Expand_Light (N : Node_Id) is
   begin
      case Nkind (N) is

         when N_Package_Declaration =>
            Expand_Light_N_Package_Declaration (N);

         when N_Simple_Return_Statement =>
            Expand_Light_N_Simple_Return_Statement (N);

         when N_Subprogram_Body =>
            Expand_Light_N_Subprogram_Body (N);

         when N_Function_Call            |
              N_Procedure_Call_Statement =>
            Expand_Light_Call (N);

         when N_Attribute_Reference =>
            Expand_Light_N_Attribute_Reference (N);

         when others =>
            null;

      end case;
   end Expand_Light;

end Exp_Light;
