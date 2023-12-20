------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--        S Y S T E M . F I N A L I Z A T I O N _ P R I M I T I V E S       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2023, Free Software Foundation, Inc.            --
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

with Ada.Exceptions; use Ada.Exceptions;

with System.Soft_Links; use System.Soft_Links;

package body System.Finalization_Primitives is

   -----------------------------
   -- Attach_Object_To_Master --
   -----------------------------

   procedure Attach_Object_To_Master
     (Object_Address   : System.Address;
      Finalize_Address : not null Finalize_Address_Ptr;
      Node             : not null Master_Node_Ptr;
      Master           : in out Finalization_Scope_Master)
   is
   begin
      Attach_Object_To_Node (Object_Address, Finalize_Address, Node.all);
      Chain_Node_To_Master (Node, Master);
   end Attach_Object_To_Master;

   ---------------------------
   -- Attach_Object_To_Node --
   ---------------------------

   procedure Attach_Object_To_Node
     (Object_Address   : System.Address;
      Finalize_Address : not null Finalize_Address_Ptr;
      Node             : in out Master_Node)
   is
   begin
      pragma Assert (Node.Object_Address = System.Null_Address
        and then Node.Finalize_Address = null);

      Node.Object_Address   := Object_Address;
      Node.Finalize_Address := Finalize_Address;
   end Attach_Object_To_Node;

   --------------------------
   -- Chain_Node_To_Master --
   --------------------------

   procedure Chain_Node_To_Master
     (Node   : not null Master_Node_Ptr;
      Master : in out Finalization_Scope_Master)
   is
   begin
      Node.Next   := Master.Head;
      Master.Head := Node;
   end Chain_Node_To_Master;

   ---------------------
   -- Finalize_Master --
   ---------------------

   procedure Finalize_Master (Master : in out Finalization_Scope_Master) is
      procedure Raise_From_Controlled_Operation (X : Exception_Occurrence);
      pragma Import (Ada, Raise_From_Controlled_Operation,
                                 "__gnat_raise_from_controlled_operation");

      Finalization_Exception_Raised : Boolean := False;
      Exc_Occur                     : Exception_Occurrence;
      Node                          : Master_Node_Ptr;

   begin
      Node := Master.Head;

      --  If exceptions are enabled, we catch them locally and reraise one
      --  once all the finalization actions have been completed.

      if Master.Exceptions_OK then
         while Node /= null loop
            begin
               Finalize_Object (Node.all);

            exception
               when Exc : others =>
                  if not Finalization_Exception_Raised then
                     Finalization_Exception_Raised := True;

                     if Master.Library_Level then
                        if Master.Extra_Info then
                           Save_Library_Occurrence (Exc'Unrestricted_Access);
                        else
                           Save_Library_Occurrence (null);
                        end if;

                     elsif Master.Extra_Info then
                        Save_Occurrence (Exc_Occur, Exc);
                     end if;
                  end if;
            end;

            Node := Node.Next;
         end loop;

      --  Otherwise we call finalization procedures without protection

      else
         while Node /= null loop
            Finalize_Object (Node.all);

            Node := Node.Next;
         end loop;
      end if;

      Master.Head := null;

      --  If one of the finalization actions raised an exception, and we are
      --  not at library level, then reraise the exception.

      if Finalization_Exception_Raised and then not Master.Library_Level then
         if Master.Extra_Info then
            Raise_From_Controlled_Operation (Exc_Occur);
         else
            raise Program_Error with "finalize/adjust raised exception";
         end if;
      end if;
   end Finalize_Master;

   ---------------------
   -- Finalize_Object --
   ---------------------

   procedure Finalize_Object (Node : in out Master_Node) is
      FA : constant Finalize_Address_Ptr := Node.Finalize_Address;

   begin
      if FA /= null then
         pragma Assert (Node.Object_Address /= System.Null_Address);

         Node.Finalize_Address := null;

         FA (Node.Object_Address);
      end if;
   end Finalize_Object;

   -------------------------------------
   -- Suppress_Object_Finalize_At_End --
   -------------------------------------

   procedure Suppress_Object_Finalize_At_End (Node : in out Master_Node) is
   begin
      Node.Finalize_Address := null;
   end Suppress_Object_Finalize_At_End;

end System.Finalization_Primitives;
