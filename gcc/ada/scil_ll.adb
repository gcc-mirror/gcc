------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S C I L _ L L                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2010, Free Software Foundation, Inc.           --
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

with Alloc; use Alloc;
with Atree; use Atree;
with Opt;   use Opt;
with Sinfo; use Sinfo;
with Table;

package body SCIL_LL is

   procedure Copy_SCIL_Node (Target : Node_Id; Source : Node_Id);
   --  Copy the SCIL field from Source to Target (it is used as the argument
   --  for a call to Set_Reporting_Proc in package atree).

   function SCIL_Nodes_Table_Size return Pos;
   --  Used to initialize the table of SCIL nodes because we do not want
   --  to consume memory for this table if it is not required.

   ----------------------------
   --  SCIL_Nodes_Table_Size --
   ----------------------------

   function SCIL_Nodes_Table_Size return Pos is
   begin
      if Generate_SCIL then
         return Alloc.Orig_Nodes_Initial;
      else
         return 1;
      end if;
   end SCIL_Nodes_Table_Size;

   package SCIL_Nodes is new Table.Table (
      Table_Component_Type => Node_Id,
      Table_Index_Type     => Node_Id'Base,
      Table_Low_Bound      => First_Node_Id,
      Table_Initial        => SCIL_Nodes_Table_Size,
      Table_Increment      => Alloc.Orig_Nodes_Increment,
      Table_Name           => "SCIL_Nodes");
   --  This table records the value of attribute SCIL_Node of all the
   --  tree nodes.

   --------------------
   -- Copy_SCIL_Node --
   --------------------

   procedure Copy_SCIL_Node (Target : Node_Id; Source : Node_Id) is
   begin
      Set_SCIL_Node (Target, Get_SCIL_Node (Source));
   end Copy_SCIL_Node;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      SCIL_Nodes.Init;
      Set_Reporting_Proc (Copy_SCIL_Node'Access);
   end Initialize;

   -------------------
   -- Get_SCIL_Node --
   -------------------

   function Get_SCIL_Node (N : Node_Id) return Node_Id is
   begin
      if Generate_SCIL
        and then Present (N)
      then
         return SCIL_Nodes.Table (N);
      else
         return Empty;
      end if;
   end Get_SCIL_Node;

   -------------------
   -- Set_SCIL_Node --
   -------------------

   procedure Set_SCIL_Node (N : Node_Id; Value : Node_Id) is
   begin
      pragma Assert (Generate_SCIL);

      if Present (Value) then
         case Nkind (Value) is
            when N_SCIL_Dispatch_Table_Tag_Init =>
               pragma Assert (Nkind (N) = N_Object_Declaration);
               null;

            when N_SCIL_Dispatching_Call =>
               pragma Assert (Nkind_In (N, N_Function_Call,
                                           N_Procedure_Call_Statement));
               null;

            when N_SCIL_Membership_Test =>
               pragma Assert (Nkind_In (N, N_Identifier,
                                           N_And_Then,
                                           N_Or_Else,
                                           N_Expression_With_Actions));
               null;

            when others =>
               pragma Assert (False);
               raise Program_Error;
         end case;
      end if;

      if Atree.Last_Node_Id > SCIL_Nodes.Last then
         SCIL_Nodes.Set_Last (Atree.Last_Node_Id);
      end if;

      SCIL_Nodes.Set_Item (N, Value);
   end Set_SCIL_Node;

end SCIL_LL;
