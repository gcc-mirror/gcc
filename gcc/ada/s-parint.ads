------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--            S Y S T E M . P A R T I T I O N _ I N T E R F A C E           --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--          Copyright (C) 1995-2003 Free Software Foundation, Inc.          --
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

--  This unit may be used directly from an application program by providing
--  an appropriate WITH, and the interface can be expected to remain stable.

with Ada.Exceptions;
with Interfaces;
with System.RPC;

package System.Partition_Interface is

   pragma Elaborate_Body;

   type Subprogram_Id is new Natural;
   --  This type is used exclusively by stubs

   subtype Unit_Name is String;
   --  Name of Ada units

   type Main_Subprogram_Type is access procedure;

   type RACW_Stub_Type is tagged record
      Origin       : RPC.Partition_ID;
      Receiver     : Interfaces.Unsigned_64;
      Addr         : Interfaces.Unsigned_64;
      Asynchronous : Boolean;
   end record;
   type RACW_Stub_Type_Access is access RACW_Stub_Type;
   --  This type is used by the expansion to implement distributed objects.
   --  Do not change its definition or its layout without updating
   --  exp_dist.adb.

   procedure Check
     (Name    : in Unit_Name;
      Version : in String;
      RCI     : in Boolean := True);
   --  Use by the main subprogram to check that a remote receiver
   --  unit has has the same version than the caller's one.

   function Get_Active_Partition_ID
     (Name : Unit_Name)
      return RPC.Partition_ID;
   --  Similar in some respects to RCI_Info.Get_Active_Partition_ID

   function Get_Active_Version
      (Name : Unit_Name)
       return String;
   --  Similar in some respects to Get_Active_Partition_ID

   function Get_Local_Partition_ID return RPC.Partition_ID;
   --  Return the Partition_ID of the current partition

   function Get_Passive_Partition_ID
     (Name : Unit_Name)
     return RPC.Partition_ID;
   --  Return the Partition_ID of the given shared passive partition

   function Get_Passive_Version (Name : Unit_Name) return String;
   --  Return the version corresponding to a shared passive unit

   function Get_RCI_Package_Receiver
     (Name : Unit_Name)
      return Interfaces.Unsigned_64;
   --  Similar in some respects to RCI_Info.Get_RCI_Package_Receiver

   procedure Get_Unique_Remote_Pointer
     (Handler : in out RACW_Stub_Type_Access);
   --  Get a unique pointer on a remote object

   procedure Launch
     (Rsh_Command  : in String;
      Name_Is_Host : in Boolean;
      General_Name : in String;
      Command_Line : in String);
   --  General_Name represents the name of the machine or the name of the
   --  partition (depending on the value of Name_Is_Host). Command_Line
   --  holds the extra options that will be given on the command line.
   --  Rsh_Command is typically "rsh", that will be used to launch the
   --  other partition.

   procedure Raise_Program_Error_For_E_4_18;
   pragma No_Return (Raise_Program_Error_For_E_4_18);
   --  Raise Program_Error with an error message explaining why it has been
   --  raised. The rule in E.4 (18) is tricky and misleading for most users
   --  of the distributed systems annex.

   procedure Raise_Program_Error_Unknown_Tag
     (E : in Ada.Exceptions.Exception_Occurrence);
   pragma No_Return (Raise_Program_Error_Unknown_Tag);
   --  Raise Program_Error with the same message as E one

   procedure Register_Receiving_Stub
     (Name     : in Unit_Name;
      Receiver : in RPC.RPC_Receiver;
      Version  : in String := "");
   --  Register the fact that the Name receiving stub is now elaborated.
   --  Register the access value to the package RPC_Receiver procedure.

   procedure Register_Passive_Package
     (Name    : in Unit_Name;
      Version : in String := "");
   --  Register a passive package

   generic
      RCI_Name : String;
   package RCI_Info is
      function Get_RCI_Package_Receiver return Interfaces.Unsigned_64;
      function Get_Active_Partition_ID return RPC.Partition_ID;
   end RCI_Info;
   --  RCI package information caching

   procedure Run (Main : in Main_Subprogram_Type := null);
   --  Run the main subprogram

end System.Partition_Interface;
