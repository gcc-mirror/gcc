------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               R I D E N T                                --
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

--  This package defines the set of restriction identifiers. It is in a
--  separate package from Restrict so that it can be easily used by the
--  binder without dragging in a lot of stuff.

package Rident is

   --  The following enumeration type defines the set of restriction
   --  identifiers not taking a parameter that are implemented in GNAT.
   --  To add a new restriction identifier, add an entry with the name
   --  to be used in the pragma, and add appropriate calls to the
   --  Restrict.Check_Restriction routine.

   type Restriction_Id is (

      --  The following cases are checked for consistency in the binder

      Boolean_Entry_Barriers,                  -- GNAT (Ravenscar)
      No_Abort_Statements,                     -- (RM D.7(5), H.4(3))
      No_Access_Subprograms,                   -- (RM H.4(17))
      No_Allocators,                           -- (RM H.4(7))
      No_Asynchronous_Control,                 -- (RM D.7(10))
      No_Calendar,                             -- GNAT
      No_Delay,                                -- (RM H.4(21))
      No_Dispatch,                             -- (RM H.4(19))
      No_Dynamic_Interrupts,                   -- GNAT
      No_Dynamic_Priorities,                   -- (RM D.9(9))
      No_Enumeration_Maps,                     -- GNAT
      No_Entry_Calls_In_Elaboration_Code,      -- GNAT
      No_Entry_Queue,                          -- GNAT
      No_Exception_Handlers,                   -- GNAT
      No_Exceptions,                           -- (RM H.4(12))
      No_Fixed_Point,                          -- (RM H.4(15))
      No_Floating_Point,                       -- (RM H.4(14))
      No_IO,                                   -- (RM H.4(20))
      No_Implicit_Conditionals,                -- GNAT
      No_Implicit_Dynamic_Code,                -- GNAT
      No_Implicit_Heap_Allocations,            -- (RM D.8(8), H.4(3))
      No_Implicit_Loops,                       -- GNAT
      No_Local_Allocators,                     -- (RM H.4(8))
      No_Local_Protected_Objects,              -- GNAT
      No_Nested_Finalization,                  -- (RM D.7(4))
      No_Protected_Type_Allocators,            -- GNAT
      No_Protected_Types,                      -- (RM H.4(5))
      No_Recursion,                            -- (RM H.4(22))
      No_Reentrancy,                           -- (RM H.4(23))
      No_Relative_Delay,                       -- GNAT
      No_Requeue,                              -- GNAT
      No_Secondary_Stack,                      -- GNAT
      No_Select_Statements,                    -- GNAT (Ravenscar)
      No_Standard_Storage_Pools,               -- GNAT
      No_Streams,                              -- GNAT
      No_Task_Allocators,                      -- (RM D.7(7))
      No_Task_Attributes,                      -- GNAT
      No_Task_Hierarchy,                       -- (RM D.7(3), H.4(3))
      No_Task_Termination,                     -- GNAT
      No_Tasking,                              -- GNAT
      No_Terminate_Alternatives,               -- (RM D.7(6))
      No_Unchecked_Access,                     -- (RM H.4(18))
      No_Unchecked_Conversion,                 -- (RM H.4(16))
      No_Unchecked_Deallocation,               -- (RM H.4(9))
      No_Wide_Characters,                      -- GNAT
      Static_Priorities,                       -- GNAT
      Static_Storage_Size,                     -- GNAT

      --  The following cases do not require partition-wide checks

      Immediate_Reclamation,                   -- (RM H.4(10))
      No_Implementation_Attributes,            -- GNAT
      No_Implementation_Pragmas,               -- GNAT
      No_Implementation_Restrictions,          -- GNAT
      No_Elaboration_Code,                     -- GNAT

      Not_A_Restriction_Id);

   subtype All_Restrictions is
     Restriction_Id range Boolean_Entry_Barriers .. No_Elaboration_Code;
   --  All restrictions except Not_A_Restriction_Id

   --  The following range of Restriction identifiers is checked for
   --  consistency across a partition. The generated ali file is marked
   --  for each entry to show one of three possibilities:
   --
   --    Corresponding restriction is set (so unit does not violate it)
   --    Corresponding restriction is not violated
   --    Corresponding restriction is violated

   subtype Partition_Restrictions is
     Restriction_Id range Boolean_Entry_Barriers .. Static_Storage_Size;

   --  The following set of Restriction identifiers is not checked for
   --  consistency across a partition. The generated ali file still
   --  contains indications of the above three possibilities for the
   --  purposes of listing applicable restrictions.

   subtype Compilation_Unit_Restrictions is
     Restriction_Id range Immediate_Reclamation .. No_Elaboration_Code;

   --  The following enumeration type defines the set of restriction
   --  parameter identifiers taking a parameter that are implemented in
   --  GNAT. To add a new restriction parameter identifier, add an entry
   --  with the name to be used in the pragma, and add appropriate
   --  calls to Restrict.Check_Restriction.

   --  Note: the GNAT implementation currently only accomodates restriction
   --  parameter identifiers whose expression value is a non-negative
   --  integer. This is true for all language defined parameters.

   type Restriction_Parameter_Id is (
     Max_Asynchronous_Select_Nesting,         -- (RM D.7(18), H.4(3))
     Max_Entry_Queue_Depth,                   -- GNAT
     Max_Protected_Entries,                   -- (RM D.7(14))
     Max_Select_Alternatives,                 -- (RM D.7(12))
     Max_Storage_At_Blocking,                 -- (RM D.7(17))
     Max_Task_Entries,                        -- (RM D.7(13), H.4(3))
     Max_Tasks,                               -- (RM D.7(19), H.4(3))
     Not_A_Restriction_Parameter_Id);

end Rident;
