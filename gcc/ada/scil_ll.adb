------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S C I L _ L L                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2010-2016, Free Software Foundation, Inc.         --
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

with Atree;         use Atree;
with Opt;           use Opt;
with Sinfo;         use Sinfo;
with System.HTable; use System.HTable;

package body SCIL_LL is

   procedure Copy_SCIL_Node (Target : Node_Id; Source : Node_Id);
   --  Copy the SCIL field from Source to Target (it is used as the argument
   --  for a call to Set_Reporting_Proc in package atree).

   type Header_Num is range 1 .. 4096;

   function Hash (N : Node_Id) return Header_Num;
   --  Hash function for Node_Ids

   --------------------------
   -- Internal Hash Tables --
   --------------------------

   package Contract_Only_Body_Flag is new Simple_HTable
     (Header_Num => Header_Num,
      Element    => Boolean,
      No_Element => False,
      Key        => Node_Id,
      Hash       => Hash,
      Equal      => "=");
   --  This table records the value of flag Is_Contract_Only_Flag of tree nodes

   package Contract_Only_Body_Nodes is new Simple_HTable
     (Header_Num => Header_Num,
      Element    => Node_Id,
      No_Element => Empty,
      Key        => Node_Id,
      Hash       => Hash,
      Equal      => "=");
   --  This table records the value of attribute Contract_Only_Body of tree
   --  nodes.

   package SCIL_Nodes is new Simple_HTable
     (Header_Num => Header_Num,
      Element    => Node_Id,
      No_Element => Empty,
      Key        => Node_Id,
      Hash       => Hash,
      Equal      => "=");
   --  This table records the value of attribute SCIL_Node of tree nodes

   --------------------
   -- Copy_SCIL_Node --
   --------------------

   procedure Copy_SCIL_Node (Target : Node_Id; Source : Node_Id) is
   begin
      Set_SCIL_Node (Target, Get_SCIL_Node (Source));
   end Copy_SCIL_Node;

   ----------------------------
   -- Get_Contract_Only_Body --
   ----------------------------

   function Get_Contract_Only_Body (N : Node_Id) return Node_Id is
   begin
      if CodePeer_Mode
        and then Present (N)
      then
         return Contract_Only_Body_Nodes.Get (N);
      else
         return Empty;
      end if;
   end Get_Contract_Only_Body;

   -------------------
   -- Get_SCIL_Node --
   -------------------

   function Get_SCIL_Node (N : Node_Id) return Node_Id is
   begin
      if Generate_SCIL
        and then Present (N)
      then
         return SCIL_Nodes.Get (N);
      else
         return Empty;
      end if;
   end Get_SCIL_Node;

   ----------
   -- Hash --
   ----------

   function Hash (N : Node_Id) return Header_Num is
   begin
      return Header_Num (1 + N mod Node_Id (Header_Num'Last));
   end Hash;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      SCIL_Nodes.Reset;
      Contract_Only_Body_Nodes.Reset;
      Contract_Only_Body_Flag.Reset;
      Set_Reporting_Proc (Copy_SCIL_Node'Access);
   end Initialize;

   ---------------------------
   -- Is_Contract_Only_Body --
   ---------------------------

   function Is_Contract_Only_Body (E : Entity_Id) return Boolean is
   begin
      return Contract_Only_Body_Flag.Get (E);
   end Is_Contract_Only_Body;

   ----------------------------
   -- Set_Contract_Only_Body --
   ----------------------------

   procedure Set_Contract_Only_Body (N : Node_Id; Value : Node_Id) is
   begin
      pragma Assert (CodePeer_Mode
        and then Present (N)
        and then Is_Contract_Only_Body (Value));

      Contract_Only_Body_Nodes.Set (N, Value);
   end Set_Contract_Only_Body;

   -------------------------------
   -- Set_Is_Contract_Only_Body --
   -------------------------------

   procedure Set_Is_Contract_Only_Body (E : Entity_Id) is
   begin
      Contract_Only_Body_Flag.Set (E, True);
   end Set_Is_Contract_Only_Body;

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
               pragma Assert (Nkind (N) in N_Subprogram_Call);
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

      SCIL_Nodes.Set (N, Value);
   end Set_SCIL_Node;

end SCIL_LL;
