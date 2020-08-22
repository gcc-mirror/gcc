------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                    S Y S T E M . P A R A M E T E R S                     --
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

--  This is the HP version of this package

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
   pragma Pure;

   ---------------------------------------
   -- Task And Stack Allocation Control --
   ---------------------------------------

   type Size_Type is range
     -(2 ** (Integer'(Standard'Address_Size) - 1)) ..
     +(2 ** (Integer'(Standard'Address_Size) - 1)) - 1;
   --  Type used to provide task stack sizes to the runtime. Sized to permit
   --  stack sizes of up to half the total addressable memory space. This may
   --  seem excessively large (even for 32-bit systems), however there are many
   --  instances of users requiring large stack sizes (for example string
   --  processing).

   Unspecified_Size : constant Size_Type := Size_Type'First;
   --  Value used to indicate that no size type is set

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

   Runtime_Default_Sec_Stack_Size : constant Size_Type := 10 * 1024;
   --  The run-time chosen default size for secondary stacks that may be
   --  overridden by the user with the use of binder -D switch.

   Sec_Stack_Dynamic : constant Boolean := True;
   --  Indicates if secondary stacks can grow and shrink at run-time. If False,
   --  the size of a secondary stack is fixed at the point of its creation.

   ----------------------------------------------
   -- Characteristics of Types in Interfaces.C --
   ----------------------------------------------

   long_bits : constant := Long_Integer'Size;
   --  Number of bits in type long and unsigned_long. The normal convention
   --  is that this is the same as type Long_Integer, but this may not be true
   --  of all targets.

   ptr_bits  : constant := Standard'Address_Size;
   subtype C_Address is System.Address;
   --  Number of bits in Interfaces.C pointers, normally a standard address

   C_Malloc_Linkname : constant String := "__gnat_malloc";
   --  Name of runtime function used to allocate such a pointer

   ----------------------------------------------
   -- Behavior of Pragma Finalize_Storage_Only --
   ----------------------------------------------

   --  Garbage_Collected is a Boolean constant whose value indicates the
   --  effect of the pragma Finalize_Storage_Entry on a controlled type.

   --    Garbage_Collected = False

   --      The system releases all storage on program termination only,
   --      but not other garbage collection occurs, so finalization calls
   --      are omitted only for outer level objects can be omitted if
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

   ---------------------
   -- Task Attributes --
   ---------------------

   Max_Attribute_Count : constant := 32;
   --  Number of task attributes stored in the task control block

   -----------------------
   -- Task Image Length --
   -----------------------

   Max_Task_Image_Length : constant := 256;
   --  This constant specifies the maximum length of a task's image

   ------------------------------
   -- Exception Message Length --
   ------------------------------

   Default_Exception_Msg_Max_Length : constant := 200;
   --  This constant specifies the default number of characters to allow
   --  in an exception message (200 is minimum required by RM 11.4.1(18)).

end System.Parameters;
