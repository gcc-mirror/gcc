------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             R E S T R I C T                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--          Copyright (C) 1992-2002 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package deals with the implementation of the Restrictions pragma

with Rident;
with Types;  use Types;
with Uintp;  use Uintp;

package Restrict is

   type Restriction_Id is new Rident.Restriction_Id;
   --  The type Restriction_Id defines the set of restriction identifiers,
   --  which take no parameter (i.e. they are either present or not present).
   --  The actual definition is in the separate package Rident, so that
   --  it can easily be accessed by the binder without dragging in lots of
   --  stuff.

   subtype All_Restrictions is
     Restriction_Id range
       Restriction_Id (Rident.All_Restrictions'First) ..
       Restriction_Id (Rident.All_Restrictions'Last);
   --  All restriction identifiers

   subtype Partition_Restrictions is
     Restriction_Id range
       Restriction_Id (Rident.Partition_Restrictions'First) ..
       Restriction_Id (Rident.Partition_Restrictions'Last);
   --  Range of restriction identifiers that are checked by the binder

   subtype Compilation_Unit_Restrictions is
     Restriction_Id range
       Restriction_Id (Rident.Compilation_Unit_Restrictions'First) ..
       Restriction_Id (Rident.Compilation_Unit_Restrictions'Last);
   --  Range of restriction identifiers not checked by binder

   type Restriction_Parameter_Id is new Rident.Restriction_Parameter_Id;
   --  The type Restriction_Parameter_Id records cases where a parameter is
   --  present in the corresponding pragma. These cases are not checked for
   --  consistency by the binder. The actual definition is in the separate
   --  package Rident for consistency.

   type Restrictions_Flags is array (Restriction_Id) of Boolean;
   --  Type used for arrays indexed by Restriction_Id.

   Restrictions : Restrictions_Flags := (others => False);
   --  Corresponding entry is False if restriction is not active, and
   --  True if the restriction is active, i.e. if a pragma Restrictions
   --  has been seen anywhere. Note that we are happy to pick up any
   --  restrictions pragmas in with'ed units, since we are required to
   --  be consistent at link time, and we might as well find the error
   --  at compile time. Clients must NOT use this array for checking to
   --  see if a restriction is violated, instead it is required that the
   --  Check_Restriction subprograms be used for this purpose. The only
   --  legitimate direct use of this array is when the code is modified
   --  as a result of the restriction in some way.

   Restrictions_Loc : array (Restriction_Id) of Source_Ptr;
   --  Locations of Restrictions pragmas for error message purposes.
   --  Valid only if corresponding entry in Restrictions is set.

   Main_Restrictions : Restrictions_Flags := (others => False);
   --  This variable saves the cumulative restrictions in effect compiling
   --  any unit that is part of the extended main unit (i.e. the compiled
   --  unit, its spec if any, and its subunits if any). The reason we keep
   --  track of this is for the information that goes to the binder about
   --  restrictions that are set. The binder will identify a unit that has
   --  a restrictions pragma for error message purposes, and we do not want
   --  to pick up a restrictions pragma in a with'ed unit for this purpose.

   Violations : Restrictions_Flags := (others => False);
   --  Corresponding entry is False if the restriction has not been
   --  violated in the current main unit, and True if it has been violated.

   Restriction_Parameters :
     array (Restriction_Parameter_Id) of Uint := (others => No_Uint);
   --  This array indicates the setting of restriction parameter identifier
   --  values. All values are initially set to No_Uint indicating that the
   --  parameter is not set, and are set to the appropriate non-negative
   --  value if a Restrictions pragma specifies the corresponding
   --  restriction parameter identifier with an appropriate value.

   Restriction_Parameters_Loc :
     array (Restriction_Parameter_Id) of Source_Ptr;
   --  Locations of Restrictions pragmas for error message purposes.
   --  Valid only if corresponding entry in Restriction_Parameters is
   --  set to a value other than No_Uint.

   type Unit_Entry is record
      Res_Id : Restriction_Id;
      Filenm : String (1 .. 8);
   end record;

   type Unit_Array_Type is array (Positive range <>) of Unit_Entry;

   Unit_Array : constant Unit_Array_Type := (
     (No_Asynchronous_Control,    "a-astaco"),
     (No_Calendar,                "a-calend"),
     (No_Calendar,                "calendar"),
     (No_Delay,                   "a-calend"),
     (No_Delay,                   "calendar"),
     (No_Dynamic_Priorities,      "a-dynpri"),
     (No_IO,                      "a-direio"),
     (No_IO,                      "directio"),
     (No_IO,                      "a-sequio"),
     (No_IO,                      "sequenio"),
     (No_IO,                      "a-ststio"),
     (No_IO,                      "a-textio"),
     (No_IO,                      "text_io "),
     (No_IO,                      "a-witeio"),
     (No_Task_Attributes,         "a-tasatt"),
     (No_Streams,                 "a-stream"),
     (No_Unchecked_Conversion,    "a-unccon"),
     (No_Unchecked_Conversion,    "unchconv"),
     (No_Unchecked_Deallocation,  "a-uncdea"),
     (No_Unchecked_Deallocation,  "unchdeal"));
   --  This array defines the mapping between restriction identifiers and
   --  predefined language files containing units for which the identifier
   --  forbids semantic dependence.

   type Save_Compilation_Unit_Restrictions is private;
   --  Type used for saving and restoring compilation unit restrictions.
   --  See Compilation_Unit_Restrictions_[Save|Restore] subprograms.

   --  The following map has True for all GNAT pragmas. It is used to
   --  implement pragma Restrictions (No_Implementation_Restrictions)
   --  (which is why this restriction itself is excluded from the list).

   Implementation_Restriction : Restrictions_Flags :=
     (Boolean_Entry_Barriers             => True,
      No_Calendar                        => True,
      No_Dynamic_Interrupts              => True,
      No_Enumeration_Maps                => True,
      No_Entry_Calls_In_Elaboration_Code => True,
      No_Entry_Queue                     => True,
      No_Exception_Handlers              => True,
      No_Implicit_Conditionals           => True,
      No_Implicit_Dynamic_Code           => True,
      No_Implicit_Loops                  => True,
      No_Local_Protected_Objects         => True,
      No_Protected_Type_Allocators       => True,
      No_Relative_Delay                  => True,
      No_Requeue                         => True,
      No_Secondary_Stack                 => True,
      No_Select_Statements               => True,
      No_Standard_Storage_Pools          => True,
      No_Streams                         => True,
      No_Task_Attributes                 => True,
      No_Task_Termination                => True,
      No_Tasking                         => True,
      No_Wide_Characters                 => True,
      Static_Priorities                  => True,
      Static_Storage_Size                => True,
      No_Implementation_Attributes       => True,
      No_Implementation_Pragmas          => True,
      No_Elaboration_Code                => True,
      others                             => False);

   -----------------
   -- Subprograms --
   -----------------

   procedure Check_Restricted_Unit (U : Unit_Name_Type; N : Node_Id);
   --  Checks if loading of unit U is prohibited by the setting of some
   --  restriction (e.g. No_IO restricts the loading of unit Ada.Text_IO).
   --  If a restriction exists post error message at the given node.

   procedure Check_Restriction (R : Restriction_Id; N : Node_Id);
   --  Checks that the given restriction is not set, and if it is set, an
   --  appropriate message is posted on the given node. Also records the
   --  violation in the violations array. Note that it is mandatory to
   --  always use this routine to check if a restriction is violated. Such
   --  checks must never be done directly by the caller, since otherwise
   --  they are not properly recorded in the violations array.

   procedure Check_Restriction
     (R : Restriction_Parameter_Id;
      N : Node_Id);
   --  Checks that the given restriction parameter identifier is not set to
   --  zero. If it is set to zero, then the node N is replaced by a node
   --  that raises Storage_Error, and a warning is issued.

   procedure Check_Restriction
     (R : Restriction_Parameter_Id;
      V : Uint;
      N : Node_Id);
   --  Checks that the count in V does not exceed the maximum value of the
   --  restriction parameter value corresponding to the given restriction
   --  parameter identifier (if it has been set). If the count in V exceeds
   --  the maximum, then post an error message on node N.

   procedure Check_Elaboration_Code_Allowed (N : Node_Id);
   --  Tests to see if elaboration code is allowed by the current restrictions
   --  settings. This function is called by Gigi when it needs to define
   --  an elaboration routine. If elaboration code is not allowed, an error
   --  message is posted on the node given as argument.

   function No_Exception_Handlers_Set return Boolean;
   --  Test to see if current restrictions settings specify that no exception
   --  handlers are present. This function is called by Gigi when it needs to
   --  expand an AT END clean up identifier with no exception handler.

   function Compilation_Unit_Restrictions_Save
     return Save_Compilation_Unit_Restrictions;
   --  This function saves the compilation unit restriction settings, and
   --  resets them to False. This is used e.g. when compiling a with'ed
   --  unit to avoid incorrectly propagating restrictions. Note that it
   --  would not be wrong to also save and reset the partition restrictions,
   --  since the binder would catch inconsistencies, but actually it is a
   --  good thing to acquire restrictions from with'ed units if they are
   --  required to be partition wide, because it allows the restriction
   --  violation message to be given at compile time instead of link time.

   procedure Compilation_Unit_Restrictions_Restore
     (R : Save_Compilation_Unit_Restrictions);
   --  This is the corresponding restore procedure to restore restrictions
   --  previously saved by Compilation_Unit_Restrictions_Save.

   procedure Disallow_In_No_Run_Time_Mode (Enode : Node_Id);
   --  If in No_Run_Time mode, then the construct represented by Enode is
   --  not permitted, and will be appropriately flagged.

   procedure Set_No_Run_Time_Mode;
   --  Set the no run time mode, and associated restriction pragmas.

   function Get_Restriction_Id
     (N    : Name_Id)
      return Restriction_Id;
   --  Given an identifier name, determines if it is a valid restriction
   --  identifier, and if so returns the corresponding Restriction_Id
   --  value, otherwise returns Not_A_Restriction_Id.

   function Get_Restriction_Parameter_Id
     (N    : Name_Id)
      return Restriction_Parameter_Id;
   --  Given an identifier name, determines if it is a valid restriction
   --  parameter identifier, and if so returns the corresponding
   --  Restriction_Parameter_Id value, otherwise returns
   --  Not_A_Restriction_Parameter_Id.

   function Abort_Allowed return Boolean;
   pragma Inline (Abort_Allowed);
   --  Tests to see if abort is allowed by the current restrictions settings.
   --  For abort to be allowed, either No_Abort_Statements must be False,
   --  or Max_Asynchronous_Select_Nesting must be non-zero.

   function Restricted_Profile return Boolean;
   --  Tests to see if tasking operations follow the GNAT restricted run time
   --  profile.

   procedure Set_Ravenscar;
   --  Sets the set of rerstrictions fro Ravenscar

   procedure Set_Restricted_Profile;
   --  Sets the set of restrictions for pragma Restricted_Run_Time

   function Tasking_Allowed return Boolean;
   pragma Inline (Tasking_Allowed);
   --  Tests to see if tasking operations are allowed by the current
   --  restrictions settings. For tasking to be allowed Max_Tasks must
   --  be non-zero.

private
   type Save_Compilation_Unit_Restrictions is
     array (Compilation_Unit_Restrictions) of Boolean;
   --  Type used for saving and restoring compilation unit restrictions.
   --  See Compilation_Unit_Restrictions_[Save|Restore] subprograms.

end Restrict;
