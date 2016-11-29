------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             R E S T R I C T                              --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2016, Free Software Foundation, Inc.         --
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

--  This package deals with the implementation of the Restrictions pragma

with Aspects; use Aspects;
with Namet;   use Namet;
with Rident;  use Rident;
with Snames;  use Snames;
with Table;
with Types;   use Types;
with Uintp;   use Uintp;

package Restrict is

   Restrictions : Restrictions_Info := No_Restrictions;
   --  This variable records restrictions found in any units in the main
   --  extended unit, and in the case of restrictions checked for partition
   --  consistency, restrictions found in any with'ed units, parent specs
   --  etc., since we may as well check as much as we can at compile time.
   --  These variables should not be referenced directly by clients. Instead
   --  use Check_Restriction to record a violation of a restriction, and
   --  Restriction_Active to test if a given restriction is active.

   Restrictions_Loc : array (All_Restrictions) of Source_Ptr :=
                       (others => No_Location);
   --  Locations of Restrictions pragmas for error message purposes.
   --  Valid only if corresponding entry in Restrictions is set. A value
   --  of No_Location is used for implicit restrictions set by another
   --  pragma, and a value of System_Location is used for restrictions
   --  set from package Standard by the processing in Targparm.

   Restriction_Profile_Name : array (All_Restrictions) of Profile_Name;
   --  Entries in this array are valid only if the corresponding restriction in
   --  Restrictions is set. The value is the corresponding profile name if the
   --  restriction was set by a Profile or Profile_Warnings pragma. The value
   --  is No_Profile in all other cases.

   Main_Restrictions : Restrictions_Info := No_Restrictions;
   --  This variable records only restrictions found in any units of the
   --  main extended unit. These are the variables used for ali file output,
   --  since we want the binder to be able to accurately diagnose inter-unit
   --  restriction violations.

   Restriction_Warnings : Rident.Restriction_Flags := (others => False);
   --  If one of these flags is set, then it means that violation of the
   --  corresponding restriction results only in a warning message, not
   --  in an error message, and the restriction is not otherwise enforced.
   --  Note that the flags in Restrictions are set to indicate that the
   --  restriction is set in this case, but Main_Restrictions is never
   --  set if Restriction_Warnings is set, so this does not look like a
   --  restriction to the binder.

   --  The following declarations establish a mapping between restriction
   --  identifiers, and the names of corresponding restricted library units.

   type Unit_Entry is record
      Res_Id : Restriction_Id;
      Filenm : String (1 .. 8);
   end record;

   Unit_Array : constant array (Positive range <>) of Unit_Entry := (
     (No_Asynchronous_Control,     "a-astaco"),
     (No_Calendar,                 "a-calend"),
     (No_Calendar,                 "calendar"),
     (No_Delay,                    "a-calend"),
     (No_Delay,                    "calendar"),
     (No_Dynamic_Priorities,       "a-dynpri"),
     (No_Finalization,             "a-finali"),
     (No_IO,                       "a-direio"),
     (No_IO,                       "directio"),
     (No_IO,                       "a-sequio"),
     (No_IO,                       "sequenio"),
     (No_IO,                       "a-ststio"),
     (No_IO,                       "a-textio"),
     (No_IO,                       "text_io "),
     (No_IO,                       "a-witeio"),
     (No_Task_Attributes_Package,  "a-tasatt"),
     (No_Unchecked_Conversion,     "a-unccon"),
     (No_Unchecked_Conversion,     "unchconv"),
     (No_Unchecked_Deallocation,   "a-uncdea"),
     (No_Unchecked_Deallocation,   "unchdeal"));

   --  The following map has True for all GNAT-defined Restrictions. It is used
   --  to implement pragma Restrictions (No_Implementation_Restrictions) (which
   --  is why this restriction itself is excluded from the list).

   Implementation_Restriction : constant array (All_Restrictions) of Boolean :=
     (Simple_Barriers                    => True,
      No_Calendar                        => True,
      No_Default_Initialization          => True,
      No_Direct_Boolean_Operators        => True,
      No_Dispatching_Calls               => True,
      No_Dynamic_Attachment              => True,
      No_Elaboration_Code                => True,
      No_Enumeration_Maps                => True,
      No_Entry_Calls_In_Elaboration_Code => True,
      No_Entry_Queue                     => True,
      No_Exception_Handlers              => True,
      No_Exception_Propagation           => True,
      No_Exception_Registration          => True,
      No_Finalization                    => True,
      No_Fixed_IO                        => True,
      No_Implementation_Attributes       => True,
      No_Implementation_Pragmas          => True,
      No_Implicit_Conditionals           => True,
      No_Implicit_Aliasing               => True,
      No_Implicit_Dynamic_Code           => True,
      No_Implicit_Loops                  => True,
      No_Initialize_Scalars              => True,
      No_Local_Protected_Objects         => True,
      No_Long_Long_Integers              => True,
      No_Multiple_Elaboration            => True,
      No_Protected_Type_Allocators       => True,
      No_Relative_Delay                  => True,
      No_Requeue_Statements              => True,
      No_Secondary_Stack                 => True,
      No_Select_Statements               => True,
      No_Standard_Storage_Pools          => True,
      No_Stream_Optimizations            => True,
      No_Streams                         => True,
      No_Task_Attributes_Package         => True,
      No_Task_Termination                => True,
      No_Tasking                         => True,
      No_Wide_Characters                 => True,
      Static_Priorities                  => True,
      Static_Storage_Size                => True,
      Pure_Barriers                      => True,
      SPARK_05                           => True,
      others                             => False);

   --------------------------
   -- No_Dependences Table --
   --------------------------

   --  The following table records entries made by Restrictions pragmas
   --  that specify a parameter for No_Dependence. Each such pragma makes
   --  an entry in this table.

   --  Note: we have chosen to implement this restriction in the "syntactic"
   --  form, where we do not check that the named package is a language defined
   --  package, but instead we allow arbitrary package names. The discussion of
   --  this issue is not complete in the ARG, but the sense seems to be leaning
   --  in this direction, which makes more sense to us, since it is much more
   --  useful, and much easier to implement.

   type ND_Entry is record
      Unit : Node_Id;
      --  The unit parameter from the No_Dependence pragma

      Warn : Boolean;
      --  True if from Restriction_Warnings, False if from Restrictions

      Profile : Profile_Name;
      --  Set to name of profile from which No_Dependence entry came, or to
      --  No_Profile if a pragma Restriction set the No_Dependence entry.
   end record;

   package No_Dependences is new Table.Table (
     Table_Component_Type => ND_Entry,
     Table_Index_Type     => Int,
     Table_Low_Bound      => 0,
     Table_Initial        => 200,
     Table_Increment      => 200,
     Table_Name           => "Name_No_Dependences");

   ----------------------------
   -- No_Use_Of_Entity Table --
   ----------------------------

   --  The following table records entries made by Restrictions pragmas
   --  that specify a parameter for No_Use_Of_Entity. Each such pragma makes
   --  an entry in this table.

   --  Note: we have chosen to implement this restriction in the "syntactic"
   --  form, where we allow arbitrary fully qualified names to be specified.

   type NE_Entry is record
      Entity : Node_Id;
      --  The entity parameter from the No_Use_Of_Entity pragma. This is in
      --  the form of a selected component, since that is the way the parser
      --  processes it, and we don't further analyze it.

      Warn : Boolean;
      --  True if from Restriction_Warnings, False if from Restrictions

      Profile : Profile_Name;
      --  Set to name of profile from which No_Use_Of_Entity entry came, or to
      --  No_Profile if a pragma Restriction set the No_Use_Of_Entity entry.
   end record;

   package No_Use_Of_Entity is new Table.Table (
     Table_Component_Type => NE_Entry,
     Table_Index_Type     => Int,
     Table_Low_Bound      => 0,
     Table_Initial        => 200,
     Table_Increment      => 200,
     Table_Name           => "Name_No_Use_Of_Entity");

   --  Note that in addition to making an entry in this table, we also set the
   --  Boolean2 flag of the Name_Table entry for the simple name of the entity.
   --  This is used to avoid most useless searches of this table.

   -----------------
   -- Subprograms --
   -----------------

   --  Note: several of these subprograms can generate error messages (e.g.
   --  Check_Restriction). These routines should be called in the analyzer
   --  rather than the expander, so that the associated error messages are
   --  correctly generated in semantics only (-gnatc) mode.

   function Abort_Allowed return Boolean;
   pragma Inline (Abort_Allowed);
   --  Tests to see if abort is allowed by the current restrictions settings.
   --  For abort to be allowed, either No_Abort_Statements must be False,
   --  or Max_Asynchronous_Select_Nesting must be non-zero.

   procedure Check_Compiler_Unit (Feature : String; N : Node_Id);
   --  If unit N is in a unit that has a pragma Compiler_Unit_Warning, then
   --  a message is posted on node N noting use of the given feature is not
   --  permitted in the compiler (bootstrap considerations).

   procedure Check_Compiler_Unit (Feature : String; Loc : Source_Ptr);
   --  If unit N is in a unit that has a pragma Compiler_Unit_Warning, then a
   --  message is posted at location Loc noting use of the given feature is not
   --  permitted in the compiler (bootstrap considerations).

   procedure Check_Restricted_Unit (U : Unit_Name_Type; N : Node_Id);
   --  Checks if loading of unit U is prohibited by the setting of some
   --  restriction (e.g. No_IO restricts the loading of unit Ada.Text_IO).
   --  If a restriction exists post error message at the given node.

   procedure Check_Restriction
     (Msg_Issued : out Boolean;
      R          : Restriction_Id;
      N          : Node_Id;
      V          : Uint := Uint_Minus_1);
   --  Checks that the given restriction is not set, and if it is set, an
   --  appropriate message is posted on the given node, in which case
   --  Msg_Issued is set to True (and False otherwise). Also records the
   --  violation in the appropriate internal arrays. Note that it is mandatory
   --  to always use this routine to check if a restriction is violated. Such
   --  checks must never be done directly by the caller, since otherwise
   --  violations in the absence of restrictions are not properly recorded. The
   --  value of V is relevant only for parameter restrictions, and in this case
   --  indicates the exact count for the violation. If the exact count is not
   --  known, V is left at its default of -1 which indicates an unknown count.

   procedure Check_Restriction
     (R : Restriction_Id;
      N : Node_Id;
      V : Uint := Uint_Minus_1);
   --  Wrapper on Check_Restriction with Msg_Issued, with the out-parameter
   --  being ignored here.

   procedure Check_Restriction_No_Dependence (U : Node_Id; Err : Node_Id);
   --  Called when a dependence on a unit is created (either implicitly, or by
   --  an explicit WITH clause). U is a node for the unit involved, and Err is
   --  the node to which an error will be attached if necessary.

   procedure Check_Restriction_No_Specification_Of_Aspect (N : Node_Id);
   --  N is the node id for an N_Aspect_Specification. An error message
   --  (warning) will be issued if a restriction (warning) was previously set
   --  for this aspect using Set_No_Specification_Of_Aspect.

   procedure Check_Restriction_No_Use_Of_Attribute (N : Node_Id);
   --  N denotes an attribute definition clause or an attribute reference. An
   --  error message (warning) will be issued if a restriction (warning) was
   --  previously set for this attribute using Set_No_Use_Of_Attribute.

   procedure Check_Restriction_No_Use_Of_Entity (N : Node_Id);
   --  N is the node id for an entity reference. An error message (warning)
   --  will be issued if a restriction (warning) was previously set for this
   --  entity name using Set_No_Use_Of_Entity.

   procedure Check_Restriction_No_Use_Of_Pragma (N : Node_Id);
   --  N is the node of a pragma. An error message (warning) will be issued
   --  if a restriction (warning) was previously set for this pragma using
   --  Set_No_Use_Of_Pragma.

   procedure Check_Elaboration_Code_Allowed (N : Node_Id);
   --  Tests to see if elaboration code is allowed by the current restrictions
   --  settings. This function is called by Gigi when it needs to define an
   --  elaboration routine. If elaboration code is not allowed, an error
   --  message is posted on the node given as argument.

   procedure Check_SPARK_05_Restriction
     (Msg   : String;
      N     : Node_Id;
      Force : Boolean := False);
   --  Node N represents a construct not allowed in SPARK_05 mode. If this is
   --  a source node, or if the restriction is forced (Force = True), and
   --  the SPARK_05 restriction is set, then an error is issued on N. Msg
   --  is appended to the restriction failure message.

   procedure Check_SPARK_05_Restriction
     (Msg1 : String;
      Msg2 : String;
      N    : Node_Id);
   --  Same as Check_SPARK_05_Restriction except there is a continuation
   --  message Msg2 following the initial message Msg1.

   procedure Check_No_Implicit_Aliasing (Obj : Node_Id);
   --  Obj is a node for which Is_Aliased_View is True, which is being used in
   --  a context (e.g. 'Access) where no implicit aliasing is allowed if the
   --  restriction No_Implicit_Aliasing is set. This procedure checks for the
   --  case where the restriction is active and Obj does not meet the required
   --  rules for avoiding implicit aliases, and issues a restriction message.

   procedure Check_Implicit_Dynamic_Code_Allowed (N : Node_Id);
   --  Tests to see if dynamic code generation (dynamically generated
   --  trampolines, in particular) is allowed by the current restrictions
   --  settings. This function is called by Gigi when it needs to generate code
   --  that generates a trampoline. If not allowed, an error message is posted
   --  on the node given as argument.

   procedure Check_No_Implicit_Heap_Alloc (N : Node_Id);
   --  Equivalent to Check_Restriction (No_Implicit_Heap_Allocations, N).
   --  Provided for easy use by back end, which has to check this restriction.

   procedure Check_No_Implicit_Task_Alloc (N : Node_Id);
   --  Equivalent to Check_Restriction (No_Implicit_Task_Allocations, N).
   --  Provided for easy use by back end, which has to check this restriction.

   procedure Check_No_Implicit_Protected_Alloc (N : Node_Id);
   --  Equivalent to:
   --    Check_Restriction (No_Implicit_Protected_Object_Allocations, N)
   --  Provided for easy use by back end, which has to check this restriction.

   procedure Check_Obsolescent_2005_Entity (E : Entity_Id; N : Node_Id);
   --  This routine checks if the entity E is one of the obsolescent entries
   --  in Ada.Characters.Handling in Ada 2005 and No_Obsolescent_Features
   --  restriction is active. If so an appropriate message is given. N is
   --  the node on which the message is to be placed. It's a bit kludgy to
   --  have this highly specialized routine rather than some wonderful general
   --  mechanism (e.g. a special pragma) to handle this case, but there are
   --  only six cases, and it is not worth the effort to do something general.

   procedure Check_Wide_Character_Restriction (E : Entity_Id; N : Node_Id);
   --  This procedure checks if the No_Wide_Character restriction is active,
   --  and if so, if N Comes_From_Source, and the root type of E is one of
   --  [Wide_]Wide_Character or [Wide_]Wide_String, then the restriction
   --  violation is recorded, and an appropriate message given.

   function Get_Restriction_Id
     (N : Name_Id) return Restriction_Id;
   --  Given an identifier name, determines if it is a valid restriction
   --  identifier, and if so returns the corresponding Restriction_Id value,
   --  otherwise returns Not_A_Restriction_Id.

   function OK_No_Dependence_Unit_Name (N : Node_Id) return Boolean;
   --  Used in checking No_Dependence argument of pragma Restrictions or
   --  pragma Restrictions_Warning, or attribute Restriction_Set. Returns
   --  True if N has the proper form for a unit name, False otherwise.

   function OK_No_Use_Of_Entity_Name (N : Node_Id) return Boolean;
   --  Used in checking No_Use_Of_Entity argument of pragma Restrictions or
   --  pragma Restrictions_Warning, or attribute Restriction_Set. Returns
   --  True if N has the proper form for an entity name, False otherwise.

   function Is_In_Hidden_Part_In_SPARK (Loc : Source_Ptr) return Boolean;
   --  Determine if given location is covered by a hidden region range in the
   --  SPARK hides table.

   function No_Exception_Handlers_Set return Boolean;
   --  Test to see if current restrictions settings specify that no exception
   --  handlers are present. This function is called by Gigi when it needs to
   --  expand an AT END clean up identifier with no exception handler. True
   --  will be returned if the configurable run-time is activated, and either
   --  of the restrictions No_Exception_Handlers or No_Exception_Propagation is
   --  set. In the latter case, the source may contain handlers but they either
   --  get converted using the local goto transformation or deleted.

   function No_Exception_Propagation_Active return Boolean;
   --  Test to see if current restrictions settings specify that no
   --  exception propagation is activated.

   function Process_Restriction_Synonyms (N : Node_Id) return Name_Id;
   --  Id is a node whose Chars field contains the name of a restriction.
   --  If it is one of synonyms that we allow for historical purposes (for
   --  list see System.Rident), then the proper official name is returned.

   function Restriction_Active (R : All_Restrictions) return Boolean;
   pragma Inline (Restriction_Active);
   --  Determines if a given restriction is active. This call should only be
   --  used where the compiled code depends on whether the restriction is
   --  active. Always use Check_Restriction to record a violation. Note that
   --  this returns False if we only have a Restriction_Warnings set, since
   --  restriction warnings should never affect generated code. If you want
   --  to know if a call to Check_Restriction is needed then use the function
   --  Restriction_Check_Required instead.

   function Restriction_Check_Required (R : All_Restrictions) return Boolean;
   pragma Inline (Restriction_Check_Required);
   --  Determines if either a Restriction_Warnings or Restrictions pragma has
   --  been given for the specified restriction. If true, then a subsequent
   --  call to Check_Restriction is required if the restriction is violated.
   --  This must not be used to guard code generation that depends on whether
   --  a restriction is active (see Restriction_Active above). Typically it
   --  is used to avoid complex code to determine if a restriction is violated,
   --  executing this code only if needed.

   function Restricted_Profile return Boolean;
   --  Tests if set of restrictions corresponding to Restricted_Tasking profile
   --  is currently in effect (set by pragma Profile, or by an appropriate set
   --  of individual Restrictions pragmas). Returns True only if all the
   --  required restrictions are set.

   procedure Set_Hidden_Part_In_SPARK (Loc1, Loc2 : Source_Ptr);
   --  Insert a new hidden region range in the SPARK hides table. The effect
   --  is to hide any SPARK violation messages which are in the range Loc1 to
   --  Loc2-1 (i.e. Loc2 is the first location for reenabling checks).

   procedure Set_Profile_Restrictions
     (P    : Profile_Name;
      N    : Node_Id;
      Warn : Boolean);
   --  Sets the set of restrictions associated with the given profile name. N
   --  is the node of the construct to which error messages are to be attached
   --  as required. Warn is set True for the case of Profile_Warnings where the
   --  restrictions are set as warnings rather than legality requirements, and
   --  is also True for Profile if the Treat_Restrictions_As_Warnings flag is
   --  set. It is false for Profile if this flag is not set.

   procedure Set_Restriction
     (R : All_Boolean_Restrictions;
      N : Node_Id);
   --  N is a node (typically a pragma node) that has the effect of setting
   --  Boolean restriction R. The restriction is set in Restrictions, and
   --  also in Main_Restrictions if this is the main unit.

   procedure Set_Restriction
     (R : All_Parameter_Restrictions;
      N : Node_Id;
      V : Integer);
   --  Similar to the above, except that this is used for the case of a
   --  parameter restriction, and the corresponding value V is given.

   procedure Set_Restriction_No_Dependence
     (Unit    : Node_Id;
      Warn    : Boolean;
      Profile : Profile_Name := No_Profile);
   --  Sets given No_Dependence restriction in table if not there already. Warn
   --  is True if from Restriction_Warnings, or for Restrictions if the flag
   --  Treat_Restrictions_As_Warnings is set. False if from Restrictions and
   --  this flag is not set. Profile is set to a non-default value if the
   --  No_Dependence restriction comes from a Profile pragma.

   procedure Set_Restriction_No_Specification_Of_Aspect
     (N       : Node_Id;
      Warning : Boolean);
   --  N is the node id for an identifier from a pragma Restrictions for the
   --  No_Specification_Of_Aspect pragma. An error message will be issued if
   --  the identifier is not a valid aspect name. Warning is set True for the
   --  case of a Restriction_Warnings pragma specifying this restriction and
   --  False for a Restrictions pragma specifying this restriction.

   procedure Set_Restriction_No_Specification_Of_Aspect (A_Id : Aspect_Id);
   --  Version used by Get_Target_Parameters (via Tbuild)

   procedure Set_Restriction_No_Use_Of_Attribute
     (N       : Node_Id;
      Warning : Boolean);
   --  N is the node id for the identifier in a pragma Restrictions for
   --  No_Use_Of_Attribute. Caller has verified that this is a valid attribute
   --  designator.

   procedure Set_Restriction_No_Use_Of_Attribute (A_Id : Attribute_Id);
   --  Version used by Get_Target_Parameters (via Tbuild)

   procedure Set_Restriction_No_Use_Of_Entity
     (Entity  : Node_Id;
      Warning : Boolean;
      Profile : Profile_Name := No_Profile);
   --  Sets given No_Use_Of_Entity restriction in table if not there already.
   --  Warn is True if from Restriction_Warnings, or for Restrictions if the
   --  flag Treat_Restrictions_As_Warnings is set. False if from Restrictions
   --  and this flag is not set. Profile is set to a non-default value if the
   --  No_Dependence restriction comes from a Profile pragma. This procedure
   --  also takes care of setting the Boolean2 flag of the simple name for
   --  the entity (to optimize table searches).

   procedure Set_Restriction_No_Use_Of_Pragma
     (N       : Node_Id;
      Warning : Boolean);
   --  N is the node id for the identifier in a pragma Restrictions for
   --  No_Use_Of_Pragma. Caller has verified that this is a valid pragma id.

   procedure Set_Restriction_No_Use_Of_Pragma (A_Id : Pragma_Id);
   --  Version used in call from Get_Target_Parameters (via Tbuild).

   function Tasking_Allowed return Boolean;
   pragma Inline (Tasking_Allowed);
   --  Tests if tasking operations are allowed by the current restrictions
   --  settings. For tasking to be allowed Max_Tasks must be non-zero.

   ----------------------------------------------
   -- Handling of Boolean Compilation Switches --
   ----------------------------------------------

   --  The following declarations are used for proper saving and restoring of
   --  restrictions for separate compilation units. There are two cases:

   --    For partition-wide restrictions, we just let the restrictions pragmas
   --    pile up, and we never reset them. We might as well detect what we can
   --    at compile time. If e.g. a with'ed unit has a restriction for one of
   --    the partition-wide restrictions, then the binder will enforce it on
   --    all units in the partition, including the unit with the WITH. Although
   --    it would not be wrong to leave this till bind time, we might as well
   --    flag it earlier at compile time.

   --    For non-partition-wide restrictions, we have quite a different state
   --    of affairs. Here it would be quite wrong to carry a restriction from
   --    a with'ed unit to another with'ed unit, or from a package spec to the
   --    package body. This means that we have to reset these non-partition
   --    wide restrictions at the start of each separate compilation unit. For
   --    units in the extended main program, we need to reset them all to the
   --    values set by the configuration pragma file(s). For units not in the
   --    extended main program, e.g. with'ed units, we might as well reset all
   --    of these restrictions to off (False). The actual initial values will
   --    be taken from the config files active when those units are compiled
   --    as main units.

   type Save_Cunit_Boolean_Restrictions is private;
   --  Type used for saving and restoring compilation unit restrictions.

   function Cunit_Boolean_Restrictions_Save
     return Save_Cunit_Boolean_Restrictions;
   --  This function saves the compilation unit restriction settings, leaving
   --  them unchanged. This is used e.g. at the start of processing a context
   --  clause, so that the main unit restrictions can be restored after all
   --  the with'ed units have been processed.

   procedure Cunit_Boolean_Restrictions_Restore
     (R : Save_Cunit_Boolean_Restrictions);
   --  This is the corresponding restore procedure to restore restrictions
   --  previously saved by Cunit_Boolean_Restrictions_Save. However it does
   --  not reset No_Elaboration_Code, this stays set if it was set before
   --  the call, and also if it is set before the call, then the Config
   --  setting is also updated to include this restriction. This is what
   --  implements the special handling of No_Elaboration_Code.

   procedure Save_Config_Cunit_Boolean_Restrictions;
   --  This saves the current compilation unit restrictions in an internal
   --  variable, and leaves them unchanged. This is called immediately after
   --  processing the configuration file pragmas, to record the restrictions
   --  set by these configuration file pragmas.

   procedure Restore_Config_Cunit_Boolean_Restrictions;
   --  This restores the value saved by the previous call to save config values
   --  saved by Save_Config_Cunit_Boolean_Restrictions. It is called at the
   --  start of processing a new unit that is part of the main sources (e.g.
   --  a package spec when the main unit is a package body).

   procedure Reset_Cunit_Boolean_Restrictions;
   --  Turns off all non-partition-wide boolean restrictions

   procedure Add_To_Config_Boolean_Restrictions (R : Restriction_Id);
   --  Add specified restriction to stored configuration boolean restrictions.
   --  This is used for handling the special case of No_Elaboration_Code.

private
   type Save_Cunit_Boolean_Restrictions is
     array (Cunit_Boolean_Restrictions) of Boolean;
   --  Type used for saving and restoring compilation unit restrictions.
   --  See Compilation_Unit_Restrictions_[Save|Restore] subprograms.

end Restrict;
