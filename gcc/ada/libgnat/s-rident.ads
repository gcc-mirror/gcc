------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                         S Y S T E M . R I D E N T                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2020, Free Software Foundation, Inc.         --
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

--  This package defines the set of restriction identifiers. It is a generic
--  package that is instantiated by the compiler/binder in package Rident, and
--  is instantiated in package System.Restrictions for use at run-time.

--  The reason that we make this a generic package is so that in the case of
--  the instantiation in Rident for use at compile time and bind time, we can
--  generate normal image tables for the enumeration types, which are needed
--  for diagnostic and informational messages. At run-time we really do not
--  want to waste the space for these image tables, and they are not needed,
--  so we can do the instantiation under control of Discard_Names to remove
--  the tables.

---------------------------------------------------
-- Note On Compile/Run-Time Consistency Checking --
---------------------------------------------------

--  This unit is with'ed by the run-time (to make System.Restrictions which is
--  used for run-time access to restriction information), by the compiler (to
--  determine what restrictions are implemented and what their category is) and
--  by the binder (in processing ali files, and generating the information used
--  at run-time to access restriction information).

--  Normally the version of System.Rident referenced in all three contexts
--  should be the same. However, problems could arise in certain inconsistent
--  builds that used inconsistent versions of the compiler and run-time. This
--  sort of thing is not strictly correct, but it does arise when short-cuts
--  are taken in build procedures.

--  Previously, this kind of inconsistency could cause a significant problem.
--  If versions of System.Rident accessed by the compiler and binder differed,
--  then the binder could fail to recognize the R (restrictions line) in the
--  ali file, leading to bind errors when restrictions were added or removed.

--  The latest implementation avoids this problem by using a named scheme
--  for recording restrictions, rather than a positional scheme that fails
--  completely if restrictions are added or subtracted. Now the worst that
--  happens at bind time in inconsistent builds is that unrecognized
--  restrictions are ignored, and the consistency checking for restrictions
--  might be incomplete, which is no big deal.

pragma Compiler_Unit_Warning;

generic
package System.Rident is
   pragma Preelaborate;

   --  The following enumeration type defines the set of restriction
   --  identifiers that are implemented in GNAT.

   --  To add a new restriction identifier, add an entry with the name to be
   --  used in the pragma, and add calls to the Restrict.Check_Restriction
   --  routine as appropriate.

   type Restriction_Id is

      --  The following cases are checked for consistency in the binder. The
      --  binder will check that every unit either has the restriction set, or
      --  does not violate the restriction.

     (Simple_Barriers,                           -- Ada 2012 (D.7 (10.9/3))
      Pure_Barriers,                             -- GNAT
      No_Abort_Statements,                       -- (RM D.7(5), H.4(3))
      No_Access_Parameter_Allocators,            -- Ada 2012 (RM H.4 (8.3/3))
      No_Access_Subprograms,                     -- (RM H.4(17))
      No_Allocators,                             -- (RM H.4(7))
      No_Anonymous_Allocators,                   -- Ada 2012 (RM H.4(8/1))
      No_Asynchronous_Control,                   -- (RM J.13(3/2)
      No_Calendar,                               -- GNAT
      No_Coextensions,                           -- Ada 2012 (RM H.4(8.2/3))
      No_Default_Stream_Attributes,              -- Ada 2012 (RM 13.12.1(4/2))
      No_Delay,                                  -- (RM H.4(21))
      No_Direct_Boolean_Operators,               -- GNAT
      No_Dispatch,                               -- (RM H.4(19))
      No_Dispatching_Calls,                      -- GNAT
      No_Dynamic_Attachment,                     -- Ada 2012 (RM E.7(10/3))
      No_Dynamic_CPU_Assignment,                 -- Ada 202x (RM D.7(10/3))
      No_Dynamic_Priorities,                     -- (RM D.9(9))
      No_Enumeration_Maps,                       -- GNAT
      No_Entry_Calls_In_Elaboration_Code,        -- GNAT
      No_Entry_Queue,                            -- GNAT (Ravenscar)
      No_Exception_Handlers,                     -- GNAT
      No_Exception_Propagation,                  -- GNAT
      No_Exception_Registration,                 -- GNAT
      No_Exceptions,                             -- (RM H.4(12))
      No_Finalization,                           -- GNAT
      No_Fixed_IO,                               -- GNAT
      No_Fixed_Point,                            -- (RM H.4(15))
      No_Floating_Point,                         -- (RM H.4(14))
      No_IO,                                     -- (RM H.4(20))
      No_Implicit_Conditionals,                  -- GNAT
      No_Implicit_Dynamic_Code,                  -- GNAT
      No_Implicit_Heap_Allocations,              -- (RM D.8(8), H.4(3))
      No_Implicit_Task_Allocations,              -- GNAT
      No_Implicit_Protected_Object_Allocations,  -- GNAT
      No_Initialize_Scalars,                     -- GNAT
      No_Local_Allocators,                       -- (RM H.4(8))
      No_Local_Timing_Events,                    -- (RM D.7(10.2/2))
      No_Local_Protected_Objects,                -- Ada 2012 (D.7(10/1.3))
      No_Long_Long_Integers,                     -- GNAT
      No_Multiple_Elaboration,                   -- GNAT
      No_Nested_Finalization,                    -- (RM D.7(4))
      No_Protected_Type_Allocators,              -- Ada 2012 (D.7 (10.3/2))
      No_Protected_Types,                        -- (RM H.4(5))
      No_Recursion,                              -- (RM H.4(22))
      No_Reentrancy,                             -- (RM H.4(23))
      No_Relative_Delay,                         -- Ada 2012 (D.7 (10.5/3))
      No_Requeue_Statements,                     -- Ada 2012 (D.7 (10.6/3))
      No_Secondary_Stack,                        -- GNAT
      No_Select_Statements,                      -- Ada 2012 (D.7 (10.7/4))
      No_Specific_Termination_Handlers,          -- (RM D.7(10.7/2))
      No_Standard_Allocators_After_Elaboration,  -- Ada 2012 (RM D.7(19.1/2))
      No_Standard_Storage_Pools,                 -- GNAT
      No_Stream_Optimizations,                   -- GNAT
      No_Streams,                                -- GNAT
      No_Task_Allocators,                        -- (RM D.7(7))
      No_Task_Attributes_Package,                -- GNAT
      No_Task_At_Interrupt_Priority,             -- GNAT
      No_Task_Hierarchy,                         -- (RM D.7(3), H.4(3))
      No_Task_Termination,                       -- GNAT (Ravenscar)
      No_Tasks_Unassigned_To_CPU,                -- Ada 202x (D.7(10.10/4))
      No_Tasking,                                -- GNAT
      No_Terminate_Alternatives,                 -- (RM D.7(6))
      No_Unchecked_Access,                       -- (RM H.4(18))
      No_Unchecked_Conversion,                   -- (RM J.13(4/2))
      No_Unchecked_Deallocation,                 -- (RM J.13(5/2))
      Static_Priorities,                         -- GNAT
      Static_Storage_Size,                       -- GNAT

      --  The following require consistency checking with special rules. See
      --  individual routines in unit Bcheck for details of what is required.

      No_Default_Initialization,                 -- GNAT

      --  The following cases do not require consistency checking and if used
      --  as a configuration pragma within a specific unit, apply only to that
      --  unit (e.g. if used in the package spec, do not apply to the body)

      --  Note: No_Elaboration_Code is handled specially. Like the other
      --  non-partition-wide restrictions, it can only be set in a unit that
      --  is part of the extended main source unit (body/spec/subunits). But
      --  it is sticky, in that if it is found anywhere within any of these
      --  units, it applies to all units in this extended main source.

      Immediate_Reclamation,                     -- (RM H.4(10))
      No_Dynamic_Sized_Objects,                  -- GNAT
      No_Implementation_Aspect_Specifications,   -- Ada 2012 AI-241
      No_Implementation_Attributes,              -- Ada 2005 AI-257
      No_Implementation_Identifiers,             -- Ada 2012 AI-246
      No_Implementation_Pragmas,                 -- Ada 2005 AI-257
      No_Implementation_Restrictions,            -- GNAT
      No_Implementation_Units,                   -- Ada 2012 AI-242
      No_Implicit_Aliasing,                      -- GNAT
      No_Implicit_Loops,                         -- GNAT
      No_Elaboration_Code,                       -- GNAT
      No_Obsolescent_Features,                   -- Ada 2005 AI-368
      No_Wide_Characters,                        -- GNAT
      Static_Dispatch_Tables,                    -- GNAT
      SPARK_05,                                  -- GNAT

      --  The following cases require a parameter value

      No_Specification_Of_Aspect,                -- 2012 (RM 13.12.1 (6.1/3))
      No_Use_Of_Attribute,                       -- 2012 (RM 13.12.1 (6.2/3))
      No_Use_Of_Pragma,                          -- 2012 (RM 13.12.1 (6.3/3))

      --  The following entries are fully checked at compile/bind time, which
      --  means that the compiler can in general tell the minimum value which
      --  could be used with a restrictions pragma. The binder can deduce the
      --  appropriate minimum value for the partition by taking the maximum
      --  value required by any unit.

      Max_Protected_Entries,                     -- (RM D.7(14))
      Max_Select_Alternatives,                   -- (RM D.7(12))
      Max_Task_Entries,                          -- (RM D.7(13), H.4(3))

      --  The following entries are also fully checked at compile/bind time,
      --  and the compiler can also at least in some cases tell the minimum
      --  value which could be used with a restriction pragma. The difference
      --  is that the contributions are additive, so the binder deduces this
      --  value by adding the unit contributions.

      Max_Tasks,                                 -- (RM D.7(19), H.4(3))

      --  The following entries are checked at compile time only for zero/
      --  nonzero entries. This means that the compiler can tell at compile
      --  time if a restriction value of zero is (would be) violated, but that
      --  the compiler cannot distinguish between different non-zero values.

      Max_Asynchronous_Select_Nesting,           -- (RM D.7(18), H.4(3))
      Max_Entry_Queue_Length,                    -- Ada 2012 (RM D.7 (19.1/2))

      --  The remaining entries are not checked at compile/bind time

      Max_Storage_At_Blocking,                   -- (RM D.7(17))

      Not_A_Restriction_Id);

   --  Synonyms permitted for historical purposes of compatibility.
   --  Must be coordinated with Restrict.Process_Restriction_Synonym.

   Boolean_Entry_Barriers : Restriction_Id renames Simple_Barriers;
   Max_Entry_Queue_Depth  : Restriction_Id renames Max_Entry_Queue_Length;
   No_Dynamic_Interrupts  : Restriction_Id renames No_Dynamic_Attachment;
   No_Requeue             : Restriction_Id renames No_Requeue_Statements;
   No_Task_Attributes     : Restriction_Id renames No_Task_Attributes_Package;

   subtype All_Restrictions is Restriction_Id range
     Simple_Barriers .. Max_Storage_At_Blocking;
   --  All restrictions (excluding only Not_A_Restriction_Id)

   subtype All_Boolean_Restrictions is Restriction_Id range
     Simple_Barriers .. SPARK_05;
   --  All restrictions which do not take a parameter

   subtype Partition_Boolean_Restrictions is All_Boolean_Restrictions range
     Simple_Barriers .. Static_Storage_Size;
   --  Boolean restrictions that are checked for partition consistency.
   --  Note that all parameter restrictions are checked for partition
   --  consistency by default, so this distinction is only needed in the
   --  case of Boolean restrictions.

   subtype Cunit_Boolean_Restrictions is All_Boolean_Restrictions range
     Immediate_Reclamation .. SPARK_05;
   --  Boolean restrictions that are not checked for partition consistency
   --  and that thus apply only to the current unit. Note that for these
   --  restrictions, the compiler does not apply restrictions found in
   --  with'ed units, parent specs etc. to the main unit, and vice versa.

   subtype All_Parameter_Restrictions is
     Restriction_Id range
       No_Specification_Of_Aspect .. Max_Storage_At_Blocking;
   --  All restrictions that take a parameter

   subtype Integer_Parameter_Restrictions is
     Restriction_Id range
       Max_Protected_Entries .. Max_Storage_At_Blocking;
   --  All restrictions taking an integer parameter

   subtype Checked_Parameter_Restrictions is
     All_Parameter_Restrictions range
       Max_Protected_Entries .. Max_Entry_Queue_Length;
   --  These are the parameter restrictions that can be at least partially
   --  checked at compile/binder time. Minimally, the compiler can detect
   --  violations of a restriction pragma with a value of zero reliably.

   subtype Checked_Max_Parameter_Restrictions is
     Checked_Parameter_Restrictions range
       Max_Protected_Entries .. Max_Task_Entries;
   --  Restrictions with parameters that can be checked in some cases by
   --  maximizing among statically detected instances where the compiler
   --  can determine the count.

   subtype Checked_Add_Parameter_Restrictions is
     Checked_Parameter_Restrictions range
       Max_Tasks .. Max_Tasks;
   --  Restrictions with parameters that can be checked in some cases by
   --  summing the statically detected instances where the compiler can
   --  determine the count.

   subtype Checked_Val_Parameter_Restrictions is
     Checked_Parameter_Restrictions range
       Max_Protected_Entries .. Max_Tasks;
   --  Restrictions with parameter where the count is known at least in some
   --  cases by the compiler/binder.

   subtype Checked_Zero_Parameter_Restrictions is
     Checked_Parameter_Restrictions range
       Max_Asynchronous_Select_Nesting .. Max_Entry_Queue_Length;
   --  Restrictions with parameters where the compiler can detect the use of
   --  the feature, and hence violations of a restriction specifying a value
   --  of zero, but cannot detect specific values other than zero/nonzero.

   subtype Unchecked_Parameter_Restrictions is
     All_Parameter_Restrictions range
       Max_Storage_At_Blocking .. Max_Storage_At_Blocking;
   --  Restrictions with parameters where the compiler cannot ever detect
   --  corresponding compile time usage, so the binder and compiler never
   --  detect violations of any restriction.

   -------------------------------------
   -- Restriction Status Declarations --
   -------------------------------------

   --  The following declarations are used to record the current status or
   --  restrictions (for the current unit, or related units, at compile time,
   --  and for all units in a partition at bind time or run time).

   type Restriction_Flags  is array (All_Restrictions)           of Boolean;
   type Restriction_Values is array (All_Parameter_Restrictions) of Natural;
   type Parameter_Flags    is array (All_Parameter_Restrictions) of Boolean;

   type Restrictions_Info is record
      Set : Restriction_Flags;
      --  An entry is True in the Set array if a restrictions pragma has been
      --  encountered for the given restriction. If the value is True for a
      --  parameter restriction, then the corresponding entry in the Value
      --  array gives the minimum value encountered for any such restriction.

      Value : Restriction_Values;
      --  If the entry for a parameter restriction in Set is True (i.e. a
      --  restrictions pragma for the restriction has been encountered), then
      --  the corresponding entry in the Value array is the minimum value
      --  specified by any such restrictions pragma. Note that a restrictions
      --  pragma specifying a value greater than Int'Last is simply ignored.

      Violated : Restriction_Flags;
      --  An entry is True in the violations array if the compiler has detected
      --  a violation of the restriction. For a parameter restriction, the
      --  Count and Unknown arrays have additional information.

      Count : Restriction_Values;
      --  If an entry for a parameter restriction is True in Violated, the
      --  corresponding entry in the Count array may record additional
      --  information. If the actual minimum count is known (by taking
      --  maximums, or sums, depending on the restriction), it will be
      --  recorded in this array. If not, then the value will remain zero.
      --  The value is also zero for a non-violated restriction.

      Unknown : Parameter_Flags;
      --  If an entry for a parameter restriction is True in Violated, the
      --  corresponding entry in the Unknown array may record additional
      --  information. If the actual count is not known by the compiler (but
      --  is known to be non-zero), then the entry in Unknown will be True.
      --  This indicates that the value in Count is not known to be exact,
      --  and the actual violation count may be higher.

      --  Note: If Violated (K) is True, then either Count (K) > 0 or
      --  Unknown (K) = True. It is possible for both these to be set.
      --  For example, if Count (K) = 3 and Unknown (K) is True, it means
      --  that the actual violation count is at least 3 but might be higher.
   end record;

   No_Restrictions : constant Restrictions_Info :=
     (Set      => (others => False),
      Value    => (others => 0),
      Violated => (others => False),
      Count    => (others => 0),
      Unknown  => (others => False));
   --  Used to initialize Restrictions_Info variables

   ----------------------------------
   -- Profile Definitions and Data --
   ----------------------------------

   --  Note: to add a profile, modify the following declarations appropriately,
   --  add Name_xxx to Snames, and add a branch to the conditions for pragmas
   --  Profile and Profile_Warnings in the body of Sem_Prag.

   type Profile_Name is
     (No_Profile,
      No_Implementation_Extensions,
      Restricted_Tasking,
      Restricted,
      Ravenscar,
      Jorvik,
      GNAT_Extended_Ravenscar,
      GNAT_Ravenscar_EDF);
   --  Names of recognized profiles. No_Profile is used to indicate that a
   --  restriction came from pragma Restrictions[_Warning], as opposed to
   --  pragma Profile[_Warning]. Restricted_Tasking is a non-user profile that
   --  contaings the minimal set of restrictions to trigger the user of the
   --  restricted tasking runtime. Restricted is the corresponding user profile
   --  that also restrict protected types.

   subtype Profile_Name_Actual is Profile_Name
     range No_Implementation_Extensions .. Profile_Name'Last;
   --  Actual used profile names

   type Profile_Data is record
      Set : Restriction_Flags;
      --  Set to True if given restriction must be set for the profile, and
      --  False if it need not be set (False does not mean that it must not be
      --  set, just that it need not be set). If the flag is True for a
      --  parameter restriction, then the Value array gives the maximum value
      --  permitted by the profile.

      Value : Restriction_Values;
      --  An entry in this array is meaningful only if the corresponding flag
      --  in Set is True. In that case, the value in this array is the maximum
      --  value of the parameter permitted by the profile.
   end record;

   Profile_Info : constant array (Profile_Name_Actual) of Profile_Data := (

                     --  No_Implementation_Extensions profile

                     No_Implementation_Extensions =>

                       (Set   =>
                          (No_Implementation_Aspect_Specifications => True,
                           No_Implementation_Attributes            => True,
                           No_Implementation_Identifiers           => True,
                           No_Implementation_Pragmas               => True,
                           No_Implementation_Units                 => True,
                           others                                  => False),

                        --  Value settings for Restricted profile (none

                        Value =>
                          (others                          => 0)),

                     --  Restricted_Tasking Profile

                     Restricted_Tasking =>

                        --  Restrictions for Restricted_Tasking profile

                       (Set   =>
                          (No_Abort_Statements             => True,
                           No_Asynchronous_Control         => True,
                           No_Dynamic_Attachment           => True,
                           No_Dynamic_CPU_Assignment       => True,
                           No_Dynamic_Priorities           => True,
                           No_Local_Protected_Objects      => True,
                           No_Protected_Type_Allocators    => True,
                           No_Requeue_Statements           => True,
                           No_Task_Allocators              => True,
                           No_Task_Attributes_Package      => True,
                           No_Task_Hierarchy               => True,
                           No_Terminate_Alternatives       => True,
                           Max_Asynchronous_Select_Nesting => True,
                           Max_Select_Alternatives         => True,
                           Max_Task_Entries                => True,
                           others                          => False),

                        --  Value settings for Restricted_Tasking profile

                        Value =>
                          (Max_Asynchronous_Select_Nesting => 0,
                           Max_Select_Alternatives         => 0,
                           Max_Task_Entries                => 0,
                           others                          => 0)),

                     --  Restricted Profile

                     Restricted =>

                        --  Restrictions for Restricted profile

                       (Set   =>
                          (No_Abort_Statements             => True,
                           No_Asynchronous_Control         => True,
                           No_Dynamic_Attachment           => True,
                           No_Dynamic_CPU_Assignment       => True,
                           No_Dynamic_Priorities           => True,
                           No_Entry_Queue                  => True,
                           No_Local_Protected_Objects      => True,
                           No_Protected_Type_Allocators    => True,
                           No_Requeue_Statements           => True,
                           No_Task_Allocators              => True,
                           No_Task_Attributes_Package      => True,
                           No_Task_Hierarchy               => True,
                           No_Terminate_Alternatives       => True,
                           Max_Asynchronous_Select_Nesting => True,
                           Max_Protected_Entries           => True,
                           Max_Select_Alternatives         => True,
                           Max_Task_Entries                => True,
                           others                          => False),

                        --  Value settings for Restricted profile

                        Value =>
                          (Max_Asynchronous_Select_Nesting => 0,
                           Max_Protected_Entries           => 1,
                           Max_Select_Alternatives         => 0,
                           Max_Task_Entries                => 0,
                           others                          => 0)),

                     --  Ravenscar Profile

                     --  Note: the table entries here only represent the
                     --  required restriction profile for Ravenscar. The
                     --  full Ravenscar profile also requires:

                     --    pragma Dispatching_Policy (FIFO_Within_Priorities);
                     --    pragma Locking_Policy (Ceiling_Locking);
                     --    pragma Detect_Blocking;

                     Ravenscar  =>

                     --  Restrictions for Ravenscar = Restricted profile ..

                       (Set   =>
                          (No_Abort_Statements             => True,
                           No_Asynchronous_Control         => True,
                           No_Dynamic_Attachment           => True,
                           No_Dynamic_CPU_Assignment       => True,
                           No_Dynamic_Priorities           => True,
                           No_Entry_Queue                  => True,
                           No_Local_Protected_Objects      => True,
                           No_Protected_Type_Allocators    => True,
                           No_Requeue_Statements           => True,
                           No_Task_Allocators              => True,
                           No_Task_Attributes_Package      => True,
                           No_Task_Hierarchy               => True,
                           No_Terminate_Alternatives       => True,
                           Max_Asynchronous_Select_Nesting => True,
                           Max_Protected_Entries           => True,
                           Max_Select_Alternatives         => True,
                           Max_Task_Entries                => True,

                           --  plus these additional restrictions:

                           No_Calendar                      => True,
                           No_Implicit_Heap_Allocations     => True,
                           No_Local_Timing_Events           => True,
                           No_Relative_Delay                => True,
                           No_Select_Statements             => True,
                           No_Specific_Termination_Handlers => True,
                           No_Task_Termination              => True,
                           Simple_Barriers                  => True,
                           others                           => False),

                        --  Value settings for Ravenscar (same as Restricted)

                        Value =>
                          (Max_Asynchronous_Select_Nesting => 0,
                           Max_Protected_Entries           => 1,
                           Max_Select_Alternatives         => 0,
                           Max_Task_Entries                => 0,
                           others                          => 0)),

                     Jorvik  =>

                     --  Restrictions for Jorvik profile ..

                     --  Note: the table entries here only represent the
                     --  required restriction profile for Jorvik. The
                     --  full Jorvik profile also requires:

                     --    pragma Dispatching_Policy (FIFO_Within_Priorities);
                     --    pragma Locking_Policy (Ceiling_Locking);
                     --    pragma Detect_Blocking;

                     --  The differences between Ravenscar and Jorvik are
                     --  as follows:
                     --     1) Ravenscar includes restriction Simple_Barriers;
                     --        Jorvik includes Pure_Barriers instead.
                     --     2) The following 6 restrictions are included in
                     --        Ravenscar but not in Jorvik:
                     --          No_Implicit_Heap_Allocations
                     --          No_Relative_Delay
                     --          Max_Entry_Queue_Length => 1
                     --          Max_Protected_Entries => 1
                     --          No_Dependence => Ada.Calendar
                     --          No_Dependence => Ada.Synchronous_Barriers
                     --
                     --  The last of those 7 (i.e., No_Dep => Ada.Synch_Bars)
                     --  is not reflected here (see sem_prag.adb).

                       (Set   =>
                          (No_Abort_Statements             => True,
                           No_Asynchronous_Control         => True,
                           No_Dynamic_Attachment           => True,
                           No_Dynamic_CPU_Assignment       => True,
                           No_Dynamic_Priorities           => True,
                           No_Local_Protected_Objects      => True,
                           No_Protected_Type_Allocators    => True,
                           No_Requeue_Statements           => True,
                           No_Task_Allocators              => True,
                           No_Task_Attributes_Package      => True,
                           No_Task_Hierarchy               => True,
                           No_Terminate_Alternatives       => True,
                           Max_Asynchronous_Select_Nesting => True,
                           Max_Select_Alternatives         => True,
                           Max_Task_Entries                => True,

                           --  plus these additional restrictions:

                           No_Local_Timing_Events           => True,
                           No_Select_Statements             => True,
                           No_Specific_Termination_Handlers => True,
                           No_Task_Termination              => True,
                           Pure_Barriers                    => True,
                           others                           => False),

                        --  Value settings for Ravenscar (same as Restricted)

                        Value =>
                          (Max_Asynchronous_Select_Nesting => 0,
                           Max_Select_Alternatives         => 0,
                           Max_Task_Entries                => 0,
                           others                          => 0)),

                     GNAT_Extended_Ravenscar  =>

                     --  Restrictions for GNAT_Extended_Ravenscar =
                     --    Restricted profile ..

                       (Set   =>
                          (No_Abort_Statements             => True,
                           No_Asynchronous_Control         => True,
                           No_Dynamic_Attachment           => True,
                           No_Dynamic_CPU_Assignment       => True,
                           No_Dynamic_Priorities           => True,
                           No_Local_Protected_Objects      => True,
                           No_Protected_Type_Allocators    => True,
                           No_Requeue_Statements           => True,
                           No_Task_Allocators              => True,
                           No_Task_Attributes_Package      => True,
                           No_Task_Hierarchy               => True,
                           No_Terminate_Alternatives       => True,
                           Max_Asynchronous_Select_Nesting => True,
                           Max_Select_Alternatives         => True,
                           Max_Task_Entries                => True,

                           --  plus these additional restrictions:

                           No_Implicit_Task_Allocations     => True,
                           No_Implicit_Protected_Object_Allocations
                                                            => True,
                           No_Local_Timing_Events           => True,
                           No_Select_Statements             => True,
                           No_Specific_Termination_Handlers => True,
                           No_Task_Termination              => True,
                           Pure_Barriers                    => True,
                           others                           => False),

                        --  Value settings for Ravenscar (same as Restricted)

                        Value =>
                          (Max_Asynchronous_Select_Nesting => 0,
                           Max_Select_Alternatives         => 0,
                           Max_Task_Entries                => 0,
                           others                          => 0)),

                     --  GNAT_Ravenscar_EDF Profile

                     --  Note: the table entries here only represent the
                     --  required restriction profile for GNAT_Ravenscar_EDF.
                     --  The full GNAT_Ravenscar_EDF profile also requires:

                     --    pragma Dispatching_Policy (EDF_Across_Priorities);
                     --    pragma Locking_Policy (Ceiling_Locking);
                     --    pragma Detect_Blocking;

                     GNAT_Ravenscar_EDF  =>

                     --  Restrictions for Ravenscar = Restricted profile ..

                       (Set   =>
                          (No_Abort_Statements             => True,
                           No_Asynchronous_Control         => True,
                           No_Dynamic_Attachment           => True,
                           No_Dynamic_CPU_Assignment       => True,
                           No_Dynamic_Priorities           => True,
                           No_Entry_Queue                  => True,
                           No_Local_Protected_Objects      => True,
                           No_Protected_Type_Allocators    => True,
                           No_Requeue_Statements           => True,
                           No_Task_Allocators              => True,
                           No_Task_Attributes_Package      => True,
                           No_Task_Hierarchy               => True,
                           No_Terminate_Alternatives       => True,
                           Max_Asynchronous_Select_Nesting => True,
                           Max_Protected_Entries           => True,
                           Max_Select_Alternatives         => True,
                           Max_Task_Entries                => True,

                           --  plus these additional restrictions:

                           No_Calendar                      => True,
                           No_Implicit_Heap_Allocations     => True,
                           No_Local_Timing_Events           => True,
                           No_Relative_Delay                => True,
                           No_Select_Statements             => True,
                           No_Specific_Termination_Handlers => True,
                           No_Task_Termination              => True,
                           Simple_Barriers                  => True,
                           others                           => False),

                        --  Value settings for Ravenscar (same as Restricted)

                        Value =>
                          (Max_Asynchronous_Select_Nesting => 0,
                           Max_Protected_Entries           => 1,
                           Max_Select_Alternatives         => 0,
                           Max_Task_Entries                => 0,
                           others                          => 0)));

end System.Rident;
