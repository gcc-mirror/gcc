------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       G N A T . D E B U G _ P O O L S                    --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2005 Free Software Foundation, Inc.          --
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
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This packages provides a special implementation of the Ada95 storage pools.

--  The goal of this debug pool is to detect incorrect uses of memory
--  (multiple deallocations, access to invalid memory,...). Errors are reported
--  in one of two ways: either by immediately raising an exception, or by
--  printing a message on standard output.

--  You need to instrument your code to use this package: for each access type
--  you want to monitor, you need to add a clause similar to:

--      type Integer_Access is access Integer;
--      for Integer_Access'Storage_Pool use Pool;

--  where Pool is a tagged object declared with
--
--      Pool : GNAT.Debug_Pools.Debug_Pool;

--  This package was designed to be as efficient as possible, but still has an
--  impact on the performance of your code, which depends on the number of
--  allocations, deallocations and, somewhat less, dereferences that your
--  application performs.

--  For each faulty memory use, this debug pool will print several lines
--  of information, including things like the location where the memory
--  was initially allocated, the location where it was freed etc.

--  Physical allocations and deallocations are done through the usual system
--  calls. However, in order to provide proper checks, the debug pool will not
--  release the memory immediately. It keeps released memory around (the amount
--  kept around is configurable) so that it can distinguish between memory that
--  has not been allocated and memory that has been allocated but freed. This
--  also means that this memory cannot be reallocated, preventing what would
--  otherwise be a false indication that freed memory is now allocated.

--  In addition, this package presents several subprograms that help analyze
--  the behavior of your program, by reporting memory leaks, the total amount
--  of memory that was allocated. The pool is also designed to work correctly
--  in conjunction with gnatmem.

--  Finally, a subprogram Print_Pool is provided for use from the debugger.

--  Limitations
--  ===========

--  Current limitation of this debug pool: if you use this debug pool for a
--  general access type ("access all"), the pool might report invalid
--  dereferences if the access object is pointing to another object on the
--  stack which was not allocated through a call to "new".

--  This debug pool will respect all alignments specified in your code, but
--  it does that by aligning all objects using Standard'Maximum_Alignment.
--  This allows faster checks, and limits the performance impact of using
--  this pool.

with System;                  use System;
with System.Storage_Elements; use System.Storage_Elements;
with System.Checked_Pools;

package GNAT.Debug_Pools is

   type Debug_Pool is new System.Checked_Pools.Checked_Pool with private;
   --  The new debug pool

   subtype SSC is System.Storage_Elements.Storage_Count;

   Default_Max_Freed         : constant SSC     := 50_000_000;
   Default_Stack_Trace_Depth : constant Natural := 20;
   Default_Reset_Content     : constant Boolean := False;
   Default_Raise_Exceptions  : constant Boolean := True;
   Default_Advanced_Scanning : constant Boolean := False;
   Default_Min_Freed         : constant SSC     := 0;
   --  The above values are constants used for the parameters to Configure
   --  if not overridden in the call. See description of Configure for full
   --  details on these parameters. If these defaults are not satisfactory,
   --  then you need to call Configure to change the default values.

   procedure Configure
     (Pool                           : in out Debug_Pool;
      Stack_Trace_Depth              : Natural := Default_Stack_Trace_Depth;
      Maximum_Logically_Freed_Memory : SSC     := Default_Max_Freed;
      Minimum_To_Free                : SSC     := Default_Min_Freed;
      Reset_Content_On_Free          : Boolean := Default_Reset_Content;
      Raise_Exceptions               : Boolean := Default_Raise_Exceptions;
      Advanced_Scanning              : Boolean := Default_Advanced_Scanning);
   --  Subprogram used to configure the debug pool.
   --
   --    Stack_Trace_Depth. This parameter controls the maximum depth of stack
   --    traces that are output to indicate locations of actions for error
   --    conditions such as bad allocations. If set to zero, the debug pool
   --    will not try to compute backtraces. This is more efficient but gives
   --    less information on problem locations
   --
   --    Maximum_Logically_Freed_Memory: maximum amount of memory (bytes)
   --    that should be kept before starting to physically deallocate some.
   --    This value should be non-zero, since having memory that is logically
   --    but not physically freed helps to detect invalid memory accesses.
   --
   --    Minimum_To_Free is the minimum amount of memory that should be freed
   --    every time the pool starts physically releasing memory. The algorithm
   --    to compute which block should be physically released needs some
   --    expensive initialization (see Advanced_Scanning below), and this
   --    parameter can be used to limit the performance impact by ensuring
   --    that a reasonable amount of memory is freed each time. Even in the
   --    advanced scanning mode, marked blocks may be released to match this
   --    Minimum_To_Free parameter.
   --
   --    Reset_Content_On_Free: If true, then the contents of the freed memory
   --    is reset to the pattern 16#DEADBEEF#, following an old IBM convention.
   --    This helps in detecting invalid memory references from the debugger.
   --
   --    Raise_Exceptions: If true, the exceptions below will be raised every
   --    time an error is detected. If you set this to False, then the action
   --    is to generate output on standard error, noting the errors, but to
   --    keep running if possible (of course if storage is badly damaged, this
   --    attempt may fail. This helps to detect more than one error in a run.
   --
   --    Advanced_Scanning: If true, the pool will check the contents of all
   --    allocated blocks before physically releasing memory. Any possible
   --    reference to a logically free block will prevent its deallocation.
   --    Note that this algorithm is approximate, and it is recommended
   --    that you set Minimum_To_Free to a non-zero value to save time.
   --
   --  All instantiations of this pool use the same internal tables. However,
   --  they do not store the same amount of information for the tracebacks,
   --  and they have different counters for maximum logically freed memory.

   Accessing_Not_Allocated_Storage : exception;
   --  Exception raised if Raise_Exception is True, and an attempt is made
   --  to access storage that was never allocated.

   Accessing_Deallocated_Storage : exception;
   --  Exception raised if Raise_Exception is True, and an attempt is made
   --  to access storage that was allocated but has been deallocated.

   Freeing_Not_Allocated_Storage : exception;
   --  Exception raised if Raise_Exception is True, and an attempt is made
   --  to free storage that had not been previously allocated.

   Freeing_Deallocated_Storage : exception;
   --  Exception raised if Raise_Exception is True, and an attempt is made
   --  to free storage that had already been freed.

   --  Note on the above exceptions. The distinction between not allocated
   --  and deallocated storage is not guaranteed to be accurate in the case
   --  where storage is allocated, and then physically freed. Larger values
   --  of the parameter Maximum_Logically_Freed_Memory will help to guarantee
   --  that this distinction is made more accurately.

   generic
      with procedure Put_Line (S : String) is <>;
      with procedure Put      (S : String) is <>;
   procedure Print_Info
     (Pool          : Debug_Pool;
      Cumulate      : Boolean := False;
      Display_Slots : Boolean := False;
      Display_Leaks : Boolean := False);
   --  Print out information about the High Water Mark, the current and
   --  total number of bytes allocated and the total number of bytes
   --  deallocated.
   --
   --  If Display_Slots is true, this subprogram prints a list of all the
   --  locations in the application that have done at least one allocation or
   --  deallocation. The result might be used to detect places in the program
   --  where lots of allocations are taking place. This output is not in any
   --  defined order.
   --
   --  If Cumulate if True, then each stack trace will display the number of
   --  allocations that were done either directly, or by the subprograms called
   --  at that location (e.g: if there were two physical allocations at a->b->c
   --  and a->b->d, then a->b would be reported as performing two allocations).
   --
   --  If Display_Leaks is true, then each block that has not been deallocated
   --  (often called a "memory leak") will be listed, along with the traceback
   --  showing where it was allocated. Not that no grouping of the blocks is
   --  done, you should use the Dump_Gnatmem procedure below in conjunction
   --  with the gnatmem utility.

   procedure Print_Info_Stdout
     (Pool          : Debug_Pool;
      Cumulate      : Boolean := False;
      Display_Slots : Boolean := False;
      Display_Leaks : Boolean := False);
   --  Standard instantiation of Print_Info to print on standard_output. More
   --  convenient to use where this is the intended location, and in particular
   --  easier to use from the debugger.

   procedure Dump_Gnatmem (Pool : Debug_Pool; File_Name : String);
   --  Create an external file on the disk, which can be processed by gnatmem
   --  to display the location of memory leaks.
   --
   --  This provides a nicer output that Print_Info above, and groups similar
   --  stack traces together. This also provides an easy way to save the memory
   --  status of your program for post-mortem analysis.
   --
   --  To use this file, use the following command line:
   --     gnatmem 5 -i <File_Name> <Executable_Name>
   --  If you want all the stack traces to be displayed with 5 levels.

   procedure Print_Pool (A : System.Address);
   pragma Export (C, Print_Pool, "print_pool");
   --  This subprogram is meant to be used from a debugger. Given an address in
   --  memory, it will print on standard output the known information about
   --  this address (provided, of course, the matching pointer is handled by
   --  the Debug_Pool).
   --
   --  The information includes the stacktrace for the allocation or
   --  deallocation of that memory chunck, its current status (allocated or
   --  logically freed), etc.

private
   --  The following are the standard primitive subprograms for a pool

   procedure Allocate
     (Pool                     : in out Debug_Pool;
      Storage_Address          : out Address;
      Size_In_Storage_Elements : Storage_Count;
      Alignment                : Storage_Count);
   --  Allocate a new chunk of memory, and set it up so that the debug pool
   --  can check accesses to its data, and report incorrect access later on.
   --  The parameters have the same semantics as defined in the ARM95.

   procedure Deallocate
     (Pool                     : in out Debug_Pool;
      Storage_Address          : Address;
      Size_In_Storage_Elements : Storage_Count;
      Alignment                : Storage_Count);
   --  Mark a block of memory as invalid. It might not be physically removed
   --  immediately, depending on the setup of the debug pool, so that checks
   --  are still possible. The parameters have the same semantics as defined
   --  in the RM.

   function Storage_Size (Pool : Debug_Pool) return SSC;
   --  Return the maximal size of data that can be allocated through Pool.
   --  Since Pool uses the malloc() system call, all the memory is accessible
   --  through the pool

   procedure Dereference
     (Pool                     : in out Debug_Pool;
      Storage_Address          : System.Address;
      Size_In_Storage_Elements : Storage_Count;
      Alignment                : Storage_Count);
   --  Check whether a derefence statement is valid, ie whether the pointer
   --  was allocated through Pool. As documented above, errors will be
   --  reported either by a special error message or an exception, depending
   --  on the setup of the storage pool.
   --  The parameters have the same semantics as defined in the ARM95.

   type Byte_Count is mod System.Max_Binary_Modulus;
   --  Type used for maintaining byte counts, needs to be large enough
   --  to accomodate counts allowing for repeated use of the same memory.

   type Debug_Pool is new System.Checked_Pools.Checked_Pool with record
      Stack_Trace_Depth              : Natural := Default_Stack_Trace_Depth;
      Maximum_Logically_Freed_Memory : SSC     := Default_Max_Freed;
      Reset_Content_On_Free          : Boolean := Default_Reset_Content;
      Raise_Exceptions               : Boolean := Default_Raise_Exceptions;
      Minimum_To_Free                : SSC     := Default_Min_Freed;
      Advanced_Scanning              : Boolean := Default_Advanced_Scanning;

      Allocated : Byte_Count := 0;
      --  Total number of bytes allocated in this pool

      Logically_Deallocated : Byte_Count := 0;
      --  Total number of bytes logically deallocated in this pool. This is the
      --  memory that the application has released, but that the pool has not
      --  yet physically released through a call to free(), to detect later
      --  accesed to deallocated memory.

      Physically_Deallocated : Byte_Count := 0;
      --  Total number of bytes that were free()-ed.

      Marked_Blocks_Deallocated : Boolean := False;
      --  Set to true if some mark blocks had to be deallocated in the advanced
      --  scanning scheme. Since this is potentially dangereous, this is
      --  reported to the user, who might want to rerun his program with a
      --  lower Minimum_To_Free value.

      High_Water : Byte_Count := 0;
      --  Maximum of Allocated - Logically_Deallocated - Physically_Deallocated

      First_Free_Block : System.Address := System.Null_Address;
      Last_Free_Block  : System.Address := System.Null_Address;
      --  Pointers to the first and last logically freed blocks.

      First_Used_Block : System.Address := System.Null_Address;
      --  Pointer to the list of currently allocated blocks. This list is
      --  used to list the memory leaks in the application on exit, as well as
      --  for the advanced freeing algorithms that needs to traverse all these
      --  blocks to find possible references to the block being physically
      --  freed.
   end record;
end GNAT.Debug_Pools;
