------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--            S Y S T E M . P A R T I T I O N _ I N T E R F A C E           --
--                                                                          --
--                                  B o d y                                 --
--                   (Dummy body for non-distributed case)                  --
--                                                                          --
--          Copyright (C) 1995-2017, Free Software Foundation, Inc.         --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
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

package body System.Partition_Interface is

   pragma Warnings (Off); -- suppress warnings for unreferenced formals

   M : constant := 7;

   type String_Access is access String;

   --  To have a minimal implementation of U'Partition_ID

   type Pkg_Node;
   type Pkg_List is access Pkg_Node;
   type Pkg_Node is record
      Name          : String_Access;
      Subp_Info     : System.Address;
      Subp_Info_Len : Integer;
      Next          : Pkg_List;
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
     (Name    : Unit_Name;
      Version : String;
      RCI     : Boolean := True)
   is
   begin
      null;
   end Check;

   -----------------------------
   -- Get_Active_Partition_Id --
   -----------------------------

   function Get_Active_Partition_ID
     (Name : Unit_Name) return System.RPC.Partition_ID
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

   function Get_Active_Version (Name : Unit_Name) return String is
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
     (Name : Unit_Name) return System.RPC.Partition_ID
   is
   begin
      return Get_Local_Partition_ID;
   end Get_Passive_Partition_ID;

   -------------------------
   -- Get_Passive_Version --
   -------------------------

   function Get_Passive_Version (Name : Unit_Name) return String is
   begin
      return "";
   end Get_Passive_Version;

   ------------------
   -- Get_RAS_Info --
   ------------------

   procedure Get_RAS_Info
     (Name          :  Unit_Name;
      Subp_Id       :  Subprogram_Id;
      Proxy_Address : out Interfaces.Unsigned_64)
   is
      LName : constant String := Lower (Name);
      N : Pkg_List;
   begin
      N := Pkg_Head;
      while N /= null loop
         if N.Name.all = LName then
            declare
               subtype Subprogram_Array is RCI_Subp_Info_Array
                 (First_RCI_Subprogram_Id ..
                  First_RCI_Subprogram_Id + N.Subp_Info_Len - 1);
               Subprograms : Subprogram_Array;
               for Subprograms'Address use N.Subp_Info;
               pragma Import (Ada, Subprograms);
            begin
               Proxy_Address :=
                 Interfaces.Unsigned_64 (Subprograms (Integer (Subp_Id)).Addr);
               return;
            end;
         end if;
         N := N.Next;
      end loop;
      Proxy_Address := 0;
   end Get_RAS_Info;

   ------------------------------
   -- Get_RCI_Package_Receiver --
   ------------------------------

   function Get_RCI_Package_Receiver
     (Name : Unit_Name) return Interfaces.Unsigned_64
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

   -------------------------------------
   -- Raise_Program_Error_Unknown_Tag --
   -------------------------------------

   procedure Raise_Program_Error_Unknown_Tag
     (E : Ada.Exceptions.Exception_Occurrence)
   is
   begin
      raise Program_Error with Ada.Exceptions.Exception_Message (E);
   end Raise_Program_Error_Unknown_Tag;

   -----------------
   -- RCI_Locator --
   -----------------

   package body RCI_Locator is

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

   end RCI_Locator;

   ------------------------------
   -- Register_Passive_Package --
   ------------------------------

   procedure Register_Passive_Package
     (Name    : Unit_Name;
      Version : String := "")
   is
   begin
      Register_Receiving_Stub
        (Passive_Prefix & Name, null, Version, System.Null_Address, 0);
   end Register_Passive_Package;

   -----------------------------
   -- Register_Receiving_Stub --
   -----------------------------

   procedure Register_Receiving_Stub
     (Name          : Unit_Name;
      Receiver      : RPC_Receiver;
      Version       : String := "";
      Subp_Info     : System.Address;
      Subp_Info_Len : Integer)
   is
      N : constant Pkg_List :=
            new Pkg_Node'(new String'(Lower (Name)),
                          Subp_Info, Subp_Info_Len,
                          Next => null);
   begin
      if Pkg_Tail = null then
         Pkg_Head := N;
      else
         Pkg_Tail.Next := N;
      end if;
      Pkg_Tail := N;
   end Register_Receiving_Stub;

   ---------
   -- Run --
   ---------

   procedure Run
     (Main : Main_Subprogram_Type := null)
   is
   begin
      if Main /= null then
         Main.all;
      end if;
   end Run;

   --------------------
   -- Same_Partition --
   --------------------

   function Same_Partition
      (Left  : not null access RACW_Stub_Type;
       Right : not null access RACW_Stub_Type) return Boolean
   is
      pragma Unreferenced (Left);
      pragma Unreferenced (Right);
   begin
      return True;
   end Same_Partition;

end System.Partition_Interface;
