------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    S Y S T E M . P A R A M E T E R S                     --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2003 Free Software Foundation, Inc.          --
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

--  This is the HP version of this package
--  Blank line intentional so that it lines up exactly with default.

--  This package defines some system dependent parameters for GNAT. These
--  are values that are referenced by the runtime library and are therefore
--  relevant to the target machine.

--  The parameters whose value is defined in the spec are not generally
--  expected to be changed. If they are changed, it will be necessary to
--  recompile the run-time library.

--  The parameters which are defined by functions can be changed by modifying
--  the body of System.Parameters in file s-parame.adb. A change to this body
--  requires only rebinding and relinking of the application.

--  Note: do not introduce any pragma Inline statements into this unit, since
--  otherwise the relinking and rebinding capability would be deactivated.

package System.Parameters is
pragma Pure (Parameters);

   ---------------------------------------
   -- Task And Stack Allocation Control --
   ---------------------------------------

   type Task_Storage_Size is new Integer;
   --  Type used in tasking units for task storage size

   type Size_Type is new Task_Storage_Size;
   --  Type used to provide task storage size to runtime

   Unspecified_Size : constant Size_Type := Size_Type'First;
   --  Value used to indicate that no size type is set

   subtype Ratio is Size_Type range -1 .. 100;
   Dynamic : constant Size_Type := -1;
   --  The secondary stack ratio is a constant between 0 and 100 which
   --  determines the percentage of the allocated task stack that is
   --  used by the secondary stack (the rest being the primary stack).
   --  The special value of minus one indicates that the secondary
   --  stack is to be allocated from the heap instead.

   Sec_Stack_Ratio : constant Ratio := Dynamic;
   --  This constant defines the handling of the secondary stack

   Sec_Stack_Dynamic : constant Boolean := Sec_Stack_Ratio = Dynamic;
   --  Convenient Boolean for testing for dynamic secondary stack

   function Default_Stack_Size return Size_Type;
   --  Default task stack size used if none is specified

   function Minimum_Stack_Size return Size_Type;
   --  Minimum task stack size permitted

   function Adjust_Storage_Size (Size : Size_Type) return Size_Type;
   --  Given the storage size stored in the TCB, return the Storage_Size
   --  value required by the RM for the Storage_Size attribute. The
   --  required adjustment is as follows:
   --
   --    when Size = Unspecified_Size, return Default_Stack_Size
   --    when Size < Minimum_Stack_Size, return Minimum_Stack_Size
   --    otherwise return given Size

   Default_Env_Stack_Size : constant Size_Type := 8_192_000;
   --  Assumed size of the environment task, if no other information
   --  is available. This value is used when stack checking is
   --  enabled and no GNAT_STACK_LIMIT environment variable is set.

   Stack_Grows_Down  : constant Boolean := False;
   --  This constant indicates whether the stack grows up (False) or
   --  down (True) in memory as functions are called. It is used for
   --  proper implementation of the stack overflow check.

   ----------------------------------------------
   -- Characteristics of Types in Interfaces.C --
   ----------------------------------------------

   long_bits : constant := Long_Integer'Size;
   --  Number of bits in type long and unsigned_long. The normal convention
   --  is that this is the same as type Long_Integer, but this is not true
   --  of all targets. For example, in OpenVMS long /= Long_Integer.

   ----------------------------------------------
   -- Behavior of Pragma Finalize_Storage_Only --
   ----------------------------------------------

   --  Garbage_Collected is a Boolean constant whose value indicates the
   --  effect of the pragma Finalize_Storage_Entry on a controlled type.

   --    Garbage_Collected = False

   --      The system releases all storage on program termination only,
   --      but not other garbage collection occurs, so finalization calls
   --      are ommitted only for outer level onjects can be omitted if
   --      pragma Finalize_Storage_Only is used.

   --    Garbage_Collected = True

   --      The system provides full garbage collection, so it is never
   --      necessary to release storage for controlled objects for which
   --      a pragma Finalize_Storage_Only is used.

   Garbage_Collected : constant Boolean := False;
   --  The storage mode for this system (release on program exit)

   ---------------------
   -- Tasking Profile --
   ---------------------

   --  In the following sections, constant parameters are defined to
   --  allow some optimizations and fine tuning within the tasking run time
   --  based on restrictions on the tasking features.

   ----------------------
   -- Locking Strategy --
   ----------------------

   Single_Lock : constant Boolean := False;
   --  Indicates whether a single lock should be used within the tasking
   --  run-time to protect internal structures. If True, a single lock
   --  will be used, meaning less locking/unlocking operations, but also
   --  more global contention. In general, Single_Lock should be set to
   --  True on single processor machines, and to False to multi-processor
   --  systems, but this can vary from application to application and also
   --  depends on the scheduling policy.

   -------------------
   -- Task Abortion --
   -------------------

   No_Abort : constant Boolean := False;
   --  This constant indicates whether abort statements and asynchronous
   --  transfer of control (ATC) are disallowed. If set to True, it is
   --  assumed that neither construct is used, and the run time does not
   --  need to defer/undefer abort and check for pending actions at
   --  completion points. A value of True for No_Abort corresponds to:
   --  pragma Restrictions (No_Abort_Statements);
   --  pragma Restrictions (Max_Asynchronous_Select_Nesting => 0);

   ----------------------
   -- Dynamic Priority --
   ----------------------

   Dynamic_Priority_Support : constant Boolean := True;
   --  This constant indicates whether dynamic changes of task priorities
   --  are allowed (True means normal RM mode in which such changes are
   --  allowed). In particular, if this is False, then we do not need to
   --  poll for pending base priority changes at every abort completion
   --  point. A value of False for Dynamic_Priority_Support corresponds
   --  to pragma Restrictions (No_Dynamic_Priorities);

   ---------------------
   -- Task Attributes --
   ---------------------

   Default_Attribute_Count : constant := 4;
   --  Number of pre-allocated Address-sized task attributes stored in the
   --  task control block.

   --------------------
   -- Runtime Traces --
   --------------------

   Runtime_Traces : constant Boolean := False;
   --  This constant indicates whether the runtime outputs traces to a
   --  predefined output or not (True means that traces are output).
   --  See System.Traces for more details.

end System.Parameters;
