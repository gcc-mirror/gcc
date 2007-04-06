------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                         S Y S T E M . R I D E N T                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2006, Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the GNU Public License.                                       --
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

generic
package System.Rident is
   pragma Preelaborate;

   --  The following enumeration type defines the set of restriction
   --  identifiers that are implemented in GNAT.

   --  To add a new restriction identifier, add an entry with the name
   --  to be used in the pragma, and add appropriate calls to the
   --  Restrict.Check_Restriction routine.

   type Restriction_Id is

      --  The following cases are checked for consistency in the binder

     (Simple_Barriers,                         -- GNAT (Ravenscar)
      No_Abort_Statements,                     -- (RM D.7(5), H.4(3))
      No_Access_Subprograms,                   -- (RM H.4(17))
      No_Allocators,                           -- (RM H.4(7))
      No_Asynchronous_Control,                 -- (RM D.7(10))
      No_Calendar,                             -- GNAT
      No_Delay,                                -- (RM H.4(21))
      No_Direct_Boolean_Operators,             -- GNAT
      No_Dispatch,                             -- (RM H.4(19))
      No_Dispatching_Calls,                    -- GNAT
      No_Dynamic_Attachment,                   -- GNAT
      No_Dynamic_Priorities,                   -- (RM D.9(9))
      No_Enumeration_Maps,                     -- GNAT
      No_Entry_Calls_In_Elaboration_Code,      -- GNAT
      No_Entry_Queue,                          -- GNAT (Ravenscar)
      No_Exception_Handlers,                   -- GNAT
      No_Exception_Propagation,                -- GNAT
      No_Exception_Registration,               -- GNAT
      No_Exceptions,                           -- (RM H.4(12))
      No_Finalization,                         -- GNAT
      No_Fixed_Point,                          -- (RM H.4(15))
      No_Floating_Point,                       -- (RM H.4(14))
      No_IO,                                   -- (RM H.4(20))
      No_Implicit_Conditionals,                -- GNAT
      No_Implicit_Dynamic_Code,                -- GNAT
      No_Implicit_Heap_Allocations,            -- (RM D.8(8), H.4(3))
      No_Implicit_Loops,                       -- GNAT
      No_Initialize_Scalars,                   -- GNAT
      No_Local_Allocators,                     -- (RM H.4(8))
      No_Local_Protected_Objects,              -- GNAT
      No_Nested_Finalization,                  -- (RM D.7(4))
      No_Protected_Type_Allocators,            -- GNAT
      No_Protected_Types,                      -- (RM H.4(5))
      No_Recursion,                            -- (RM H.4(22))
      No_Reentrancy,                           -- (RM H.4(23))
      No_Relative_Delay,                       -- GNAT (Ravenscar)
      No_Requeue_Statements,                   -- GNAT
      No_Secondary_Stack,                      -- GNAT
      No_Select_Statements,                    -- GNAT (Ravenscar)
      No_Standard_Storage_Pools,               -- GNAT
      No_Streams,                              -- GNAT
      No_Task_Allocators,                      -- (RM D.7(7))
      No_Task_Attributes_Package,              -- GNAT
      No_Task_Hierarchy,                       -- (RM D.7(3), H.4(3))
      No_Task_Termination,                     -- GNAT (Ravenscar)
      No_Tasking,                              -- GNAT
      No_Terminate_Alternatives,               -- (RM D.7(6))
      No_Unchecked_Access,                     -- (RM H.4(18))
      No_Unchecked_Conversion,                 -- (RM H.4(16))
      No_Unchecked_Deallocation,               -- (RM H.4(9))
      Static_Priorities,                       -- GNAT
      Static_Storage_Size,                     -- GNAT

      --  The following cases do not require partition-wide checks

      Immediate_Reclamation,                   -- (RM H.4(10))
      No_Implementation_Attributes,            -- Ada 2005 AI-257
      No_Implementation_Pragmas,               -- Ada 2005 AI-257
      No_Implementation_Restrictions,          -- GNAT
      No_Elaboration_Code,                     -- GNAT
      No_Obsolescent_Features,                 -- Ada 2005 AI-368
      No_Wide_Characters,                      -- GNAT

      --  The following cases require a parameter value

      --  The following entries are fully checked at compile/bind time,
      --  which means that the compiler can in general tell the minimum
      --  value which could be used with a restrictions pragma. The binder
      --  can deduce the appropriate minimum value for the partition by
      --  taking the maximum value required by any unit.

      Max_Protected_Entries,                   -- (RM D.7(14))
      Max_Select_Alternatives,                 -- (RM D.7(12))
      Max_Task_Entries,                        -- (RM D.7(13), H.4(3))

      --  The following entries are also fully checked at compile/bind
      --  time, and the compiler can also at least in some cases tell
      --  the minimum value which could be used with a restriction pragma.
      --  The difference is that the contributions are additive, so the
      --  binder deduces this value by adding the unit contributions.

      Max_Tasks,                               -- (RM D.7(19), H.4(3))

      --  The following entries are checked at compile time only for
      --  zero/nonzero entries. This means that the compiler can tell
      --  at compile time if a restriction value of zero is (would be)
      --  violated, but that is all. The compiler cannot distinguish
      --  between different non-zero values.

      Max_Asynchronous_Select_Nesting,         -- (RM D.7(18), H.4(3))
      Max_Entry_Queue_Length,                  -- GNAT

      --  The remaining entries are not checked at compile/bind time

      Max_Storage_At_Blocking,                 -- (RM D.7(17))

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
     Simple_Barriers .. No_Wide_Characters;
   --  All restrictions which do not take a parameter

   subtype Partition_Boolean_Restrictions is All_Boolean_Restrictions range
     Simple_Barriers .. Static_Storage_Size;
   --  Boolean restrictions that are checked for partition consistency.
   --  Note that all parameter restrictions are checked for partition
   --  consistency by default, so this distinction is only needed in the
   --  case of Boolean restrictions.

   subtype Cunit_Boolean_Restrictions is All_Boolean_Restrictions range
     Immediate_Reclamation .. No_Wide_Characters;
   --  Boolean restrictions that are not checked for partition consistency
   --  and that thus apply only to the current unit. Note that for these
   --  restrictions, the compiler does not apply restrictions found in
   --  with'ed units, parent specs etc to the main unit.

   subtype All_Parameter_Restrictions is
     Restriction_Id range
       Max_Protected_Entries .. Max_Storage_At_Blocking;
   --  All restrictions that are take a parameter

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
   --  Restrictions with parameter where the count is known at least in
   --  some cases by the compiler/binder.

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

   --  The following declarations are used to record the current status
   --  or restrictions (for the current unit, or related units, at compile
   --  time, and for all units in a partition at bind time or run time).

   type Restriction_Flags  is array (All_Restrictions)           of Boolean;
   type Restriction_Values is array (All_Parameter_Restrictions) of Natural;
   type Parameter_Flags    is array (All_Parameter_Restrictions) of Boolean;

   type Restrictions_Info is record
      Set : Restriction_Flags;
      --  An entry is True in the Set array if a restrictions pragma has
      --  been encountered for the given restriction. If the value is
      --  True for a parameter restriction, then the corresponding entry
      --  in the Value array gives the minimum value encountered for any
      --  such restriction.

      Value : Restriction_Values;
      --  If the entry for a parameter restriction in Set is True (i.e. a
      --  restrictions pragma for the restriction has been encountered), then
      --  the corresponding entry in the Value array is the minimum value
      --  specified by any such restrictions pragma. Note that a restrictions
      --  pragma specifying a value greater than Int'Last is simply ignored.

      Violated : Restriction_Flags;
      --  An entry is True in the violations array if the compiler has
      --  detected a violation of the restriction. For a parameter
      --  restriction, the Count and Unknown arrays have additional
      --  information.

      Count : Restriction_Values;
      --  If an entry for a parameter restriction is True in Violated,
      --  the corresponding entry in the Count array may record additional
      --  information. If the actual minimum count is known (by taking
      --  maximums, or sums, depending on the restriction), it will be
      --  recorded in this array. If not, then the value will remain zero.

      Unknown : Parameter_Flags;
      --  If an entry for a parameter restriction is True in Violated,
      --  the corresponding entry in the Unknown array may record additional
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

   type Profile_Name is (Ravenscar, Restricted);
   --  Names of recognized pfofiles

   type Profile_Data is record
      Set : Restriction_Flags;
      --  Set to True if given restriction must be set for the profile,
      --  and False if it need not be set (False does not mean that it
      --  must not be set, just that it need not be set). If the flag
      --  is True for a parameter restriction, then the Value array
      --  gives the maximum value permitted by the profile.

      Value : Restriction_Values;
      --  An entry in this array is meaningful only if the corresponding
      --  flag in Set is True. In that case, the value in this array is
      --  the maximum value of the parameter permitted by the profile.
   end record;

   Profile_Info : array (Profile_Name) of Profile_Data :=

                     --  Restricted Profile

                    (Restricted =>

                        --  Restrictions for Restricted profile

                       (Set   =>
                          (No_Abort_Statements             => True,
                           No_Asynchronous_Control         => True,
                           No_Dynamic_Attachment           => True,
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
                     --    pragma Detect_Blocking

                     Ravenscar  =>

                     --  Restrictions for Ravenscar = Restricted profile ..

                       (Set   =>
                          (No_Abort_Statements             => True,
                           No_Asynchronous_Control         => True,
                           No_Dynamic_Attachment           => True,
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

                           No_Calendar                     => True,
                           No_Implicit_Heap_Allocations    => True,
                           No_Relative_Delay               => True,
                           No_Select_Statements            => True,
                           No_Task_Termination             => True,
                           Simple_Barriers                 => True,
                           others                          => False),

                        --  Value settings for Ravenscar (same as Restricted)

                        Value =>
                          (Max_Asynchronous_Select_Nesting => 0,
                           Max_Protected_Entries           => 1,
                           Max_Select_Alternatives         => 0,
                           Max_Task_Entries                => 0,
                           others                          => 0)));

end System.Rident;
