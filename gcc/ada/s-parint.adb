------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--            S Y S T E M . P A R T I T I O N _ I N T E R F A C E           --
--                                                                          --
--                                  B o d y                                 --
--                   (Dummy body for non-distributed case)                  --
--                                                                          --
--                             $Revision: 1.1 $
--                                                                          --
--          Copyright (C) 1995-2000 Free Software Foundation, Inc.          --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNARL; see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

package body System.Partition_Interface is

   M : constant := 7;

   type String_Access is access String;

   --  To have a minimal implementation of U'Partition_ID.

   type Pkg_Node;
   type Pkg_List is access Pkg_Node;
   type Pkg_Node is record
      Name : String_Access;
      Next : Pkg_List;
   end record;

   Pkg_Head : Pkg_List;
   Pkg_Tail : Pkg_List;

   function getpid return Integer;
   pragma Import (C, getpid);

   PID : constant Integer := getpid;

   function Lower (S : String) return String;

   Passive_Prefix : constant String := "SP__";
   --  String prepended in top of shared passive packages

   procedure Check
     (Name    : in Unit_Name;
      Version : in String;
      RCI     : in Boolean := True)
   is
   begin
      null;
   end Check;

   -----------------------------
   -- Get_Active_Partition_Id --
   -----------------------------

   function Get_Active_Partition_ID
     (Name : Unit_Name)
      return System.RPC.Partition_ID
   is
      P : Pkg_List := Pkg_Head;
      N : String   := Lower (Name);

   begin
      while P /= null loop
         if P.Name.all = N then
            return Get_Local_Partition_ID;
         end if;

         P := P.Next;
      end loop;

      return M;
   end Get_Active_Partition_ID;

   ------------------------
   -- Get_Active_Version --
   ------------------------

   function Get_Active_Version
     (Name : Unit_Name)
      return String
   is
   begin
      return "";
   end Get_Active_Version;

   ----------------------------
   -- Get_Local_Partition_Id --
   ----------------------------

   function Get_Local_Partition_ID return System.RPC.Partition_ID is
   begin
      return System.RPC.Partition_ID (PID mod M);
   end Get_Local_Partition_ID;

   ------------------------------
   -- Get_Passive_Partition_ID --
   ------------------------------

   function Get_Passive_Partition_ID
     (Name : Unit_Name)
      return System.RPC.Partition_ID
   is
   begin
      return Get_Local_Partition_ID;
   end Get_Passive_Partition_ID;

   -------------------------
   -- Get_Passive_Version --
   -------------------------

   function Get_Passive_Version
     (Name : Unit_Name)
      return String
   is
   begin
      return "";
   end Get_Passive_Version;

   ------------------------------
   -- Get_RCI_Package_Receiver --
   ------------------------------

   function Get_RCI_Package_Receiver
     (Name : Unit_Name)
      return Interfaces.Unsigned_64
   is
   begin
      return 0;
   end Get_RCI_Package_Receiver;

   -------------------------------
   -- Get_Unique_Remote_Pointer --
   -------------------------------

   procedure Get_Unique_Remote_Pointer
     (Handler : in out RACW_Stub_Type_Access)
   is
   begin
      null;
   end Get_Unique_Remote_Pointer;

   ------------
   -- Launch --
   ------------

   procedure Launch
     (Rsh_Command  : in String;
      Name_Is_Host : in Boolean;
      General_Name : in String;
      Command_Line : in String)
   is
   begin
      null;
   end Launch;

   -----------
   -- Lower --
   -----------

   function Lower (S : String) return String is
      T : String := S;

   begin
      for J in T'Range loop
         if T (J) in 'A' .. 'Z' then
            T (J) := Character'Val (Character'Pos (T (J)) -
                                    Character'Pos ('A') +
                                    Character'Pos ('a'));
         end if;
      end loop;

      return T;
   end Lower;

   ------------------------------------
   -- Raise_Program_Error_For_E_4_18 --
   ------------------------------------

   procedure Raise_Program_Error_For_E_4_18 is
   begin
      Ada.Exceptions.Raise_Exception
        (Program_Error'Identity,
        "Illegal usage of remote access to class-wide type. See RM E.4(18)");
   end Raise_Program_Error_For_E_4_18;

   -------------------------------------
   -- Raise_Program_Error_Unknown_Tag --
   -------------------------------------

   procedure Raise_Program_Error_Unknown_Tag
     (E : in Ada.Exceptions.Exception_Occurrence)
   is
   begin
      Ada.Exceptions.Raise_Exception
        (Program_Error'Identity, Ada.Exceptions.Exception_Message (E));
   end Raise_Program_Error_Unknown_Tag;

   --------------
   -- RCI_Info --
   --------------

   package body RCI_Info is

      -----------------------------
      -- Get_Active_Partition_ID --
      -----------------------------

      function Get_Active_Partition_ID return System.RPC.Partition_ID is
         P : Pkg_List := Pkg_Head;
         N : String   := Lower (RCI_Name);

      begin
         while P /= null loop
            if P.Name.all = N then
               return Get_Local_Partition_ID;
            end if;

            P := P.Next;
         end loop;

         return M;
      end Get_Active_Partition_ID;

      ------------------------------
      -- Get_RCI_Package_Receiver --
      ------------------------------

      function Get_RCI_Package_Receiver return Interfaces.Unsigned_64 is
      begin
         return 0;
      end Get_RCI_Package_Receiver;

   end RCI_Info;

   ------------------------------
   -- Register_Passive_Package --
   ------------------------------

   procedure Register_Passive_Package
     (Name    : in Unit_Name;
      Version : in String := "")
   is
   begin
      Register_Receiving_Stub (Passive_Prefix & Name, null, Version);
   end Register_Passive_Package;

   -----------------------------
   -- Register_Receiving_Stub --
   -----------------------------

   procedure Register_Receiving_Stub
     (Name     : in Unit_Name;
      Receiver : in RPC.RPC_Receiver;
      Version  : in String := "")
   is
   begin
      if Pkg_Tail = null then
         Pkg_Head := new Pkg_Node'(new String'(Lower (Name)), null);
         Pkg_Tail := Pkg_Head;

      else
         Pkg_Tail.Next := new Pkg_Node'(new String'(Lower (Name)), null);
         Pkg_Tail := Pkg_Tail.Next;
      end if;
   end Register_Receiving_Stub;

   ---------
   -- Run --
   ---------

   procedure Run
     (Main : in Main_Subprogram_Type := null)
   is
   begin
      if Main /= null then
         Main.all;
      end if;
   end Run;

end System.Partition_Interface;
