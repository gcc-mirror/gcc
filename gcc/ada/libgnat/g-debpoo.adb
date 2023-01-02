------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       G N A T . D E B U G _ P O O L S                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2023, Free Software Foundation, Inc.         --
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

with GNAT.IO; use GNAT.IO;

with System.CRTL;
with System.Memory;     use System.Memory;
with System.Soft_Links; use System.Soft_Links;

with System.Traceback_Entries;

with GNAT.Debug_Utilities; use GNAT.Debug_Utilities;
with GNAT.HTable;
with GNAT.Traceback; use GNAT.Traceback;

with Ada.Finalization;
with Ada.Unchecked_Conversion;

package body GNAT.Debug_Pools is

   Storage_Alignment : constant := Standard'Maximum_Alignment;
   --  Alignment enforced for all the memory chunks returned by Allocate,
   --  maximized to make sure that it will be compatible with all types.
   --
   --  The addresses returned by the underlying low-level allocator (be it
   --  'new' or a straight 'malloc') aren't guaranteed to be that much aligned
   --  on some targets, so we manage the needed alignment padding ourselves
   --  systematically. Use of a common value for every allocation allows
   --  significant simplifications in the code, nevertheless, for improved
   --  robustness and efficiency overall.

   --  We combine a few internal devices to offer the pool services:
   --
   --  * A management header attached to each allocated memory block, located
   --    right ahead of it, like so:
   --
   --        Storage Address returned by the pool,
   --        aligned on Storage_Alignment
   --                       v
   --      +------+--------+---------------------
   --      | ~~~~ | HEADER | USER DATA ... |
   --      +------+--------+---------------------
   --       <---->
   --       alignment
   --       padding
   --
   --    The alignment padding is required
   --
   --  * A validity bitmap, which holds a validity bit for blocks managed by
   --    the pool. Enforcing Storage_Alignment on those blocks allows efficient
   --    validity management.
   --
   --  * A list of currently used blocks.

   Max_Ignored_Levels : constant Natural := 10;
   --  Maximum number of levels that will be ignored in backtraces. This is so
   --  that we still have enough significant levels in the tracebacks returned
   --  to the user.
   --
   --  The value 10 is chosen as being greater than the maximum callgraph
   --  in this package. Its actual value is not really relevant, as long as it
   --  is high enough to make sure we still have enough frames to return to
   --  the user after we have hidden the frames internal to this package.

   Disable : Boolean := False;
   --  This variable is used to avoid infinite loops, where this package would
   --  itself allocate memory and then call itself recursively, forever. Useful
   --  when System_Memory_Debug_Pool_Enabled is True.

   System_Memory_Debug_Pool_Enabled : Boolean := False;
   --  If True, System.Memory allocation uses Debug_Pool

   Allow_Unhandled_Memory : Boolean := False;
   --  If True, protects Deallocate against releasing memory allocated before
   --  System_Memory_Debug_Pool_Enabled was set.

   Traceback_Count : Byte_Count := 0;
   --  Total number of traceback elements

   ---------------------------
   -- Back Trace Hash Table --
   ---------------------------

   --  This package needs to store one set of tracebacks for each allocation
   --  point (when was it allocated or deallocated). This would use too much
   --  memory,  so the tracebacks are actually stored in a hash table, and
   --  we reference elements in this hash table instead.

   --  This hash-table will remain empty if the discriminant Stack_Trace_Depth
   --  for the pools is set to 0.

   --  This table is a global table, that can be shared among all debug pools
   --  with no problems.

   type Header is range 1 .. 1023;
   --  Number of elements in the hash-table

   type Tracebacks_Array_Access is access Tracebacks_Array;

   type Traceback_Kind is (Alloc, Dealloc, Indirect_Alloc, Indirect_Dealloc);

   type Traceback_Htable_Elem;
   type Traceback_Htable_Elem_Ptr
      is access Traceback_Htable_Elem;

   type Traceback_Htable_Elem is record
      Traceback   : Tracebacks_Array_Access;
      Kind        : Traceback_Kind;
      Count       : Natural;
      --  Size of the memory allocated/freed at Traceback since last Reset call

      Total       : Byte_Count;
      --  Number of chunk of memory allocated/freed at Traceback since last
      --  Reset call.

      Frees       : Natural;
      --  Number of chunk of memory allocated at Traceback, currently freed
      --  since last Reset call. (only for Alloc & Indirect_Alloc elements)

      Total_Frees : Byte_Count;
      --  Size of the memory allocated at Traceback, currently freed since last
      --  Reset call. (only for Alloc & Indirect_Alloc elements)

      Next        : Traceback_Htable_Elem_Ptr;
   end record;

   --  Subprograms used for the Backtrace_Htable instantiation

   procedure Set_Next
     (E    : Traceback_Htable_Elem_Ptr;
      Next : Traceback_Htable_Elem_Ptr);
   pragma Inline (Set_Next);

   function Next
     (E : Traceback_Htable_Elem_Ptr) return Traceback_Htable_Elem_Ptr;
   pragma Inline (Next);

   function Get_Key
     (E : Traceback_Htable_Elem_Ptr) return Tracebacks_Array_Access;
   pragma Inline (Get_Key);

   function Hash (T : Tracebacks_Array_Access) return Header;
   pragma Inline (Hash);

   function Equal (K1, K2 : Tracebacks_Array_Access) return Boolean;
   --  Why is this not inlined???

   --  The hash table for back traces

   package Backtrace_Htable is new GNAT.HTable.Static_HTable
     (Header_Num => Header,
      Element    => Traceback_Htable_Elem,
      Elmt_Ptr   => Traceback_Htable_Elem_Ptr,
      Null_Ptr   => null,
      Set_Next   => Set_Next,
      Next       => Next,
      Key        => Tracebacks_Array_Access,
      Get_Key    => Get_Key,
      Hash       => Hash,
      Equal      => Equal);

   -----------------------
   -- Allocations table --
   -----------------------

   type Allocation_Header;
   type Allocation_Header_Access is access Allocation_Header;

   type Traceback_Ptr_Or_Address is new System.Address;
   --  A type that acts as a C union, and is either a System.Address or a
   --  Traceback_Htable_Elem_Ptr.

   --  The following record stores extra information that needs to be
   --  memorized for each block allocated with the special debug pool.

   type Allocation_Header is record
      Allocation_Address : System.Address;
      --  Address of the block returned by malloc, possibly unaligned

      Block_Size : Storage_Offset;
      --  Needed only for advanced freeing algorithms (traverse all allocated
      --  blocks for potential references). This value is negated when the
      --  chunk of memory has been logically freed by the application. This
      --  chunk has not been physically released yet.

      Alloc_Traceback : Traceback_Htable_Elem_Ptr;
      --  ??? comment required

      Dealloc_Traceback : Traceback_Ptr_Or_Address;
      --  Pointer to the traceback for the allocation (if the memory chunk is
      --  still valid), or to the first deallocation otherwise. Make sure this
      --  is a thin pointer to save space.
      --
      --  Dealloc_Traceback is also for blocks that are still allocated to
      --  point to the previous block in the list. This saves space in this
      --  header, and make manipulation of the lists of allocated pointers
      --  faster.

      Next : System.Address;
      --  Point to the next block of the same type (either allocated or
      --  logically freed) in memory. This points to the beginning of the user
      --  data, and does not include the header of that block.
   end record;

   function Header_Of
     (Address : System.Address) return Allocation_Header_Access;
   pragma Inline (Header_Of);
   --  Return the header corresponding to a previously allocated address

   function To_Address is new Ada.Unchecked_Conversion
     (Traceback_Ptr_Or_Address, System.Address);

   function To_Address is new Ada.Unchecked_Conversion
     (System.Address, Traceback_Ptr_Or_Address);

   function To_Traceback is new Ada.Unchecked_Conversion
     (Traceback_Ptr_Or_Address, Traceback_Htable_Elem_Ptr);

   function To_Traceback is new Ada.Unchecked_Conversion
     (Traceback_Htable_Elem_Ptr, Traceback_Ptr_Or_Address);

   Header_Offset : constant Storage_Count :=
     (Allocation_Header'Object_Size / System.Storage_Unit);
   --  Offset, in bytes, from start of allocation Header to start of User
   --  data.  The start of user data is assumed to be aligned at least as much
   --  as what the header type requires, so applying this offset yields a
   --  suitably aligned address as well.

   Extra_Allocation : constant Storage_Count :=
     (Storage_Alignment - 1 + Header_Offset);
   --  Amount we need to secure in addition to the user data for a given
   --  allocation request: room for the allocation header plus worst-case
   --  alignment padding.

   -----------------------
   -- Local subprograms --
   -----------------------

   function Align (Addr : Integer_Address) return Integer_Address;
   pragma Inline (Align);
   --  Return the next address aligned on Storage_Alignment from Addr.

   function Find_Or_Create_Traceback
     (Pool                : Debug_Pool;
      Kind                : Traceback_Kind;
      Size                : Storage_Count;
      Ignored_Frame_Start : System.Address;
      Ignored_Frame_End   : System.Address) return Traceback_Htable_Elem_Ptr;
   --  Return an element matching the current traceback (omitting the frames
   --  that are in the current package). If this traceback already existed in
   --  the htable, a pointer to this is returned to spare memory. Null is
   --  returned if the pool is set not to store tracebacks. If the traceback
   --  already existed in the table, the count is incremented so that
   --  Dump_Tracebacks returns useful results. All addresses up to, and
   --  including, an address between Ignored_Frame_Start .. Ignored_Frame_End
   --  are ignored.

   function Output_File (Pool : Debug_Pool) return File_Type;
   pragma Inline (Output_File);
   --  Returns file_type on which error messages have to be generated for Pool

   procedure Put_Line
     (File                : File_Type;
      Depth               : Natural;
      Traceback           : Tracebacks_Array_Access;
      Ignored_Frame_Start : System.Address := System.Null_Address;
      Ignored_Frame_End   : System.Address := System.Null_Address);
   --  Print Traceback to File. If Traceback is null, print the call_chain
   --  at the current location, up to Depth levels, ignoring all addresses
   --  up to the first one in the range:
   --    Ignored_Frame_Start .. Ignored_Frame_End

   procedure Stdout_Put (S : String);
   --  Wrapper for Put that ensures we always write to stdout instead of the
   --  current output file defined in GNAT.IO.

   procedure Stdout_Put_Line (S : String);
   --  Wrapper for Put_Line that ensures we always write to stdout instead of
   --  the current output file defined in GNAT.IO.

   procedure Print_Traceback
     (Output_File : File_Type;
      Prefix      : String;
      Traceback   : Traceback_Htable_Elem_Ptr);
   --  Output Prefix & Traceback & EOL. Print nothing if Traceback is null.

   procedure Print_Address (File : File_Type; Addr : Address);
   --  Output System.Address without using secondary stack.
   --  When System.Memory uses Debug_Pool, secondary stack cannot be used
   --  during Allocate calls, as some Allocate calls are done to
   --  register/initialize a secondary stack for a foreign thread.
   --  During these calls, the secondary stack is not available yet.

   package Validity is
      function Is_Handled (Storage : System.Address) return Boolean;
      pragma Inline (Is_Handled);
      --  Return True if Storage is the address of a block that the debug pool
      --  already had under its control. Used to allow System.Memory to use
      --  Debug_Pools

      function Is_Valid (Storage : System.Address) return Boolean;
      pragma Inline (Is_Valid);
      --  Return True if Storage is the address of a block that the debug pool
      --  has under its control, in which case Header_Of may be used to access
      --  the associated allocation header.

      procedure Set_Valid (Storage : System.Address; Value : Boolean);
      pragma Inline (Set_Valid);
      --  Mark the address Storage as being under control of the memory pool
      --  (if Value is True), or not (if Value is False).

      Validity_Count : Byte_Count := 0;
      --  Total number of validity elements

   end Validity;

   use Validity;

   procedure Set_Dead_Beef
     (Storage_Address          : System.Address;
      Size_In_Storage_Elements : Storage_Count);
   --  Set the contents of the memory block pointed to by Storage_Address to
   --  the 16#DEADBEEF# pattern. If Size_In_Storage_Elements is not a multiple
   --  of the length of this pattern, the last instance may be partial.

   procedure Free_Physically (Pool : in out Debug_Pool);
   --  Start to physically release some memory to the system, until the amount
   --  of logically (but not physically) freed memory is lower than the
   --  expected amount in Pool.

   procedure Allocate_End;
   procedure Deallocate_End;
   procedure Dereference_End;
   --  These procedures are used as markers when computing the stacktraces,
   --  so that addresses in the debug pool itself are not reported to the user.

   Code_Address_For_Allocate_End    : System.Address := System.Null_Address;
   Code_Address_For_Deallocate_End  : System.Address;
   Code_Address_For_Dereference_End : System.Address;
   --  Taking the address of the above procedures will not work on some
   --  architectures (HPUX for instance). Thus we do the same thing that
   --  is done in a-except.adb, and get the address of labels instead.

   procedure Skip_Levels
     (Depth               : Natural;
      Trace               : Tracebacks_Array;
      Start               : out Natural;
      Len                 : in out Natural;
      Ignored_Frame_Start : System.Address;
      Ignored_Frame_End   : System.Address);
   --  Set Start .. Len to the range of values from Trace that should be output
   --  to the user. This range of values excludes any address prior to the
   --  first one in Ignored_Frame_Start .. Ignored_Frame_End (basically
   --  addresses internal to this package). Depth is the number of levels that
   --  the user is interested in.

   package STBE renames System.Traceback_Entries;

   function PC_For (TB_Entry : STBE.Traceback_Entry) return System.Address
     renames STBE.PC_For;

   type Scope_Lock is
     new Ada.Finalization.Limited_Controlled with null record;
   --  Used to handle Lock_Task/Unlock_Task calls

   overriding procedure Initialize (This : in out Scope_Lock);
   --  Lock task on initialization

   overriding procedure Finalize   (This : in out Scope_Lock);
   --  Unlock task on finalization

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (This : in out Scope_Lock) is
      pragma Unreferenced (This);
   begin
      Lock_Task.all;
   end Initialize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (This : in out Scope_Lock) is
      pragma Unreferenced (This);
   begin
      Unlock_Task.all;
   end Finalize;

   -----------
   -- Align --
   -----------

   function Align (Addr : Integer_Address) return Integer_Address is
      Factor : constant Integer_Address := Storage_Alignment;
   begin
      return ((Addr + Factor - 1) / Factor) * Factor;
   end Align;

   ---------------
   -- Header_Of --
   ---------------

   function Header_Of
     (Address : System.Address) return Allocation_Header_Access
   is
      function Convert is
        new Ada.Unchecked_Conversion
                  (System.Address,
                   Allocation_Header_Access);
   begin
      return Convert (Address - Header_Offset);
   end Header_Of;

   --------------
   -- Set_Next --
   --------------

   procedure Set_Next
     (E    : Traceback_Htable_Elem_Ptr;
      Next : Traceback_Htable_Elem_Ptr)
   is
   begin
      E.Next := Next;
   end Set_Next;

   ----------
   -- Next --
   ----------

   function Next
     (E : Traceback_Htable_Elem_Ptr) return Traceback_Htable_Elem_Ptr
   is
   begin
      return E.Next;
   end Next;

   -----------
   -- Equal --
   -----------

   function Equal (K1, K2 : Tracebacks_Array_Access) return Boolean is
      use type Tracebacks_Array;
   begin
      return K1.all = K2.all;
   end Equal;

   -------------
   -- Get_Key --
   -------------

   function Get_Key
     (E : Traceback_Htable_Elem_Ptr) return Tracebacks_Array_Access
   is
   begin
      return E.Traceback;
   end Get_Key;

   ----------
   -- Hash --
   ----------

   function Hash (T : Tracebacks_Array_Access) return Header is
      Result : Integer_Address := 0;

   begin
      for X in T'Range loop
         Result := Result + To_Integer (PC_For (T (X)));
      end loop;

      return Header (1 + Result mod Integer_Address (Header'Last));
   end Hash;

   -----------------
   -- Output_File --
   -----------------

   function Output_File (Pool : Debug_Pool) return File_Type is
   begin
      if Pool.Errors_To_Stdout then
         return Standard_Output;
      else
         return Standard_Error;
      end if;
   end Output_File;

   -------------------
   -- Print_Address --
   -------------------

   procedure Print_Address (File : File_Type; Addr : Address) is
   begin
      --  Warning: secondary stack cannot be used here. When System.Memory
      --  implementation uses Debug_Pool, Print_Address can be called during
      --  secondary stack creation for foreign threads.

      Put (File, Image_C (Addr));
   end Print_Address;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line
     (File                : File_Type;
      Depth               : Natural;
      Traceback           : Tracebacks_Array_Access;
      Ignored_Frame_Start : System.Address := System.Null_Address;
      Ignored_Frame_End   : System.Address := System.Null_Address)
   is
      procedure Print (Tr : Tracebacks_Array);
      --  Print the traceback to standard_output

      -----------
      -- Print --
      -----------

      procedure Print (Tr : Tracebacks_Array) is
      begin
         for J in Tr'Range loop
            Print_Address (File, PC_For (Tr (J)));
            Put (File, ' ');
         end loop;
         Put (File, ASCII.LF);
      end Print;

   --  Start of processing for Put_Line

   begin
      if Traceback = null then
         declare
            Len   : Natural;
            Start : Natural;
            Trace : aliased Tracebacks_Array (1 .. Depth + Max_Ignored_Levels);

         begin
            Call_Chain (Trace, Len);
            Skip_Levels
              (Depth               => Depth,
               Trace               => Trace,
               Start               => Start,
               Len                 => Len,
               Ignored_Frame_Start => Ignored_Frame_Start,
               Ignored_Frame_End   => Ignored_Frame_End);
            Print (Trace (Start .. Len));
         end;

      else
         Print (Traceback.all);
      end if;
   end Put_Line;

   -----------------
   -- Skip_Levels --
   -----------------

   procedure Skip_Levels
     (Depth               : Natural;
      Trace               : Tracebacks_Array;
      Start               : out Natural;
      Len                 : in out Natural;
      Ignored_Frame_Start : System.Address;
      Ignored_Frame_End   : System.Address)
   is
   begin
      Start := Trace'First;

      while Start <= Len
        and then (PC_For (Trace (Start)) < Ignored_Frame_Start
                    or else PC_For (Trace (Start)) > Ignored_Frame_End)
      loop
         Start := Start + 1;
      end loop;

      Start := Start + 1;

      --  Just in case: make sure we have a traceback even if Ignore_Till
      --  wasn't found.

      if Start > Len then
         Start := 1;
      end if;

      if Len - Start + 1 > Depth then
         Len := Depth + Start - 1;
      end if;
   end Skip_Levels;

   ------------------------------
   -- Find_Or_Create_Traceback --
   ------------------------------

   function Find_Or_Create_Traceback
     (Pool                : Debug_Pool;
      Kind                : Traceback_Kind;
      Size                : Storage_Count;
      Ignored_Frame_Start : System.Address;
      Ignored_Frame_End   : System.Address) return Traceback_Htable_Elem_Ptr
   is
   begin
      if Pool.Stack_Trace_Depth = 0 then
         return null;
      end if;

      declare
         Disable_Exit_Value : constant Boolean := Disable;

         Elem  : Traceback_Htable_Elem_Ptr;
         Len   : Natural;
         Start : Natural;
         Trace : aliased Tracebacks_Array
                   (1 .. Integer (Pool.Stack_Trace_Depth) +
                      Max_Ignored_Levels);

      begin
         Disable := True;
         Call_Chain (Trace, Len);
         Skip_Levels
           (Depth               => Pool.Stack_Trace_Depth,
            Trace               => Trace,
            Start               => Start,
            Len                 => Len,
            Ignored_Frame_Start => Ignored_Frame_Start,
            Ignored_Frame_End   => Ignored_Frame_End);

         --  Check if the traceback is already in the table

         Elem :=
           Backtrace_Htable.Get (Trace (Start .. Len)'Unrestricted_Access);

         --  If not, insert it

         if Elem = null then
            Elem :=
              new Traceback_Htable_Elem'
                    (Traceback   =>
                       new Tracebacks_Array'(Trace (Start .. Len)),
                     Count       => 1,
                     Kind        => Kind,
                     Total       => Byte_Count (Size),
                     Frees       => 0,
                     Total_Frees => 0,
                     Next        => null);
            Traceback_Count := Traceback_Count + 1;
            Backtrace_Htable.Set (Elem);

         else
            Elem.Count := Elem.Count + 1;
            Elem.Total := Elem.Total + Byte_Count (Size);
         end if;

         Disable := Disable_Exit_Value;
         return Elem;
      exception
         when others =>
            Disable := Disable_Exit_Value;
            raise;
      end;
   end Find_Or_Create_Traceback;

   --------------
   -- Validity --
   --------------

   package body Validity is

      --  The validity bits of the allocated blocks are kept in a has table.
      --  Each component of the hash table contains the validity bits for a
      --  16 Mbyte memory chunk.

      --  The reason the validity bits are kept for chunks of memory rather
      --  than in a big array is that on some 64 bit platforms, it may happen
      --  that two chunk of allocated data are very far from each other.

      Memory_Chunk_Size : constant Integer_Address := 2 ** 24; --  16 MB
      Validity_Divisor  : constant := Storage_Alignment * System.Storage_Unit;

      Max_Validity_Byte_Index : constant :=
                                  Memory_Chunk_Size / Validity_Divisor;

      subtype Validity_Byte_Index is
        Integer_Address range 0 .. Max_Validity_Byte_Index - 1;

      type Byte is mod 2 ** System.Storage_Unit;

      type Validity_Bits_Part is array (Validity_Byte_Index) of Byte;
      type Validity_Bits_Part_Ref is access all Validity_Bits_Part;
      No_Validity_Bits_Part : constant Validity_Bits_Part_Ref := null;

      type Validity_Bits is record
         Valid : Validity_Bits_Part_Ref := No_Validity_Bits_Part;
         --  True if chunk of memory at this address is currently allocated

         Handled : Validity_Bits_Part_Ref := No_Validity_Bits_Part;
         --  True if chunk of memory at this address was allocated once after
         --  Allow_Unhandled_Memory was set to True. Used to know on Deallocate
         --  if chunk of memory should be handled a block allocated by this
         --  package.

      end record;

      type Validity_Bits_Ref is access all Validity_Bits;
      No_Validity_Bits : constant Validity_Bits_Ref := null;

      Max_Header_Num : constant := 1023;

      type Header_Num is range 0 .. Max_Header_Num - 1;

      function Hash (F : Integer_Address) return Header_Num;

      function Is_Valid_Or_Handled
        (Storage : System.Address;
         Valid   : Boolean) return Boolean;
      pragma Inline (Is_Valid_Or_Handled);
      --  Internal implementation of Is_Valid and Is_Handled.
      --  Valid is used to select Valid or Handled arrays.

      package Validy_Htable is new GNAT.HTable.Simple_HTable
        (Header_Num => Header_Num,
         Element    => Validity_Bits_Ref,
         No_Element => No_Validity_Bits,
         Key        => Integer_Address,
         Hash       => Hash,
         Equal      => "=");
      --  Table to keep the validity and handled bit blocks for the allocated
      --  data.

      function To_Pointer is new Ada.Unchecked_Conversion
        (System.Address, Validity_Bits_Part_Ref);

      procedure Memset (A : Address; C : Integer; N : size_t);
      pragma Import (C, Memset, "memset");

      ----------
      -- Hash --
      ----------

      function Hash (F : Integer_Address) return Header_Num is
      begin
         return Header_Num (F mod Max_Header_Num);
      end Hash;

      -------------------------
      -- Is_Valid_Or_Handled --
      -------------------------

      function Is_Valid_Or_Handled
        (Storage : System.Address;
         Valid   : Boolean) return Boolean is
         Int_Storage : constant Integer_Address := To_Integer (Storage);

      begin
         --  The pool only returns addresses aligned on Storage_Alignment so
         --  anything off cannot be a valid block address and we can return
         --  early in this case. We actually have to since our data structures
         --  map validity bits for such aligned addresses only.

         if Int_Storage mod Storage_Alignment /= 0 then
            return False;
         end if;

         declare
            Block_Number : constant Integer_Address :=
                             Int_Storage / Memory_Chunk_Size;
            Ptr          : constant Validity_Bits_Ref :=
                             Validy_Htable.Get (Block_Number);
            Offset       : constant Integer_Address :=
                             (Int_Storage -
                               (Block_Number * Memory_Chunk_Size)) /
                                  Storage_Alignment;
            Bit          : constant Byte :=
                             2 ** Natural (Offset mod System.Storage_Unit);
         begin
            if Ptr = No_Validity_Bits then
               return False;
            else
               if Valid then
                  return (Ptr.Valid (Offset / System.Storage_Unit)
                             and Bit) /= 0;
               else
                  if Ptr.Handled = No_Validity_Bits_Part then
                     return False;
                  else
                     return (Ptr.Handled (Offset / System.Storage_Unit)
                                and Bit) /= 0;
                  end if;
               end if;
            end if;
         end;
      end Is_Valid_Or_Handled;

      --------------
      -- Is_Valid --
      --------------

      function Is_Valid (Storage : System.Address) return Boolean is
      begin
         return Is_Valid_Or_Handled (Storage => Storage, Valid => True);
      end Is_Valid;

      -----------------
      -- Is_Handled --
      -----------------

      function Is_Handled (Storage : System.Address) return Boolean is
      begin
         return Is_Valid_Or_Handled (Storage => Storage, Valid => False);
      end Is_Handled;

      ---------------
      -- Set_Valid --
      ---------------

      procedure Set_Valid (Storage : System.Address; Value : Boolean) is
         Int_Storage  : constant Integer_Address := To_Integer (Storage);
         Block_Number : constant Integer_Address :=
                          Int_Storage / Memory_Chunk_Size;
         Ptr          : Validity_Bits_Ref := Validy_Htable.Get (Block_Number);
         Offset       : constant Integer_Address :=
                          (Int_Storage - (Block_Number * Memory_Chunk_Size)) /
                             Storage_Alignment;
         Bit          : constant Byte :=
                          2 ** Natural (Offset mod System.Storage_Unit);

         procedure Set_Handled;
         pragma Inline (Set_Handled);
         --  if Allow_Unhandled_Memory set Handled bit in table.

         -----------------
         -- Set_Handled --
         -----------------

         procedure Set_Handled is
         begin
            if Allow_Unhandled_Memory then
               if Ptr.Handled = No_Validity_Bits_Part then
                  Ptr.Handled :=
                    To_Pointer (Alloc (size_t (Max_Validity_Byte_Index)));
                  Memset
                    (A => Ptr.Handled.all'Address,
                     C => 0,
                     N => size_t (Max_Validity_Byte_Index));
               end if;

               Ptr.Handled (Offset / System.Storage_Unit) :=
                 Ptr.Handled (Offset / System.Storage_Unit) or Bit;
            end if;
         end Set_Handled;

      --  Start of processing for Set_Valid

      begin
         if Ptr = No_Validity_Bits then

            --  First time in this memory area: allocate a new block and put
            --  it in the table.

            if Value then
               Ptr := new Validity_Bits;
               Validity_Count := Validity_Count + 1;
               Ptr.Valid :=
                 To_Pointer (Alloc (size_t (Max_Validity_Byte_Index)));
               Validy_Htable.Set (Block_Number, Ptr);
               Memset
                 (A => Ptr.Valid.all'Address,
                  C => 0,
                  N => size_t (Max_Validity_Byte_Index));
               Ptr.Valid (Offset / System.Storage_Unit) := Bit;
               Set_Handled;
            end if;

         else
            if Value then
               Ptr.Valid (Offset / System.Storage_Unit) :=
                 Ptr.Valid (Offset / System.Storage_Unit) or Bit;
               Set_Handled;
            else
               Ptr.Valid (Offset / System.Storage_Unit) :=
                 Ptr.Valid (Offset / System.Storage_Unit) and (not Bit);
            end if;
         end if;
      end Set_Valid;
   end Validity;

   --------------
   -- Allocate --
   --------------

   procedure Allocate
     (Pool                     : in out Debug_Pool;
      Storage_Address          : out Address;
      Size_In_Storage_Elements : Storage_Count;
      Alignment                : Storage_Count)
   is
      pragma Unreferenced (Alignment);
      --  Ignored, we always force Storage_Alignment

      type Local_Storage_Array is new Storage_Array
        (1 .. Size_In_Storage_Elements + Extra_Allocation);

      type Ptr is access Local_Storage_Array;
      --  On some systems, we might want to physically protect pages against
      --  writing when they have been freed (of course, this is expensive in
      --  terms of wasted memory). To do that, all we should have to do it to
      --  set the size of this array to the page size. See mprotect().

      Current : Byte_Count;
      P       : Ptr;
      Trace   : Traceback_Htable_Elem_Ptr;

      Reset_Disable_At_Exit : Boolean := False;

      Lock : Scope_Lock;
      pragma Unreferenced (Lock);

   begin
      <<Allocate_Label>>

      if Disable then
         Storage_Address :=
           System.CRTL.malloc (System.CRTL.size_t (Size_In_Storage_Elements));
         return;
      end if;

      Reset_Disable_At_Exit := True;
      Disable := True;

      Pool.Alloc_Count := Pool.Alloc_Count + 1;

      --  If necessary, start physically releasing memory. The reason this is
      --  done here, although Pool.Logically_Deallocated has not changed above,
      --  is so that we do this only after a series of deallocations (e.g loop
      --  that deallocates a big array). If we were doing that in Deallocate,
      --  we might be physically freeing memory several times during the loop,
      --  which is expensive.

      if Pool.Logically_Deallocated >
           Byte_Count (Pool.Maximum_Logically_Freed_Memory)
      then
         Free_Physically (Pool);
      end if;

      --  Use standard (i.e. through malloc) allocations. This automatically
      --  raises Storage_Error if needed. We also try once more to physically
      --  release memory, so that even marked blocks, in the advanced scanning,
      --  are freed. Note that we do not initialize the storage array since it
      --  is not necessary to do so (however this will cause bogus valgrind
      --  warnings, which should simply be ignored).

      begin
         P := new Local_Storage_Array;

      exception
         when Storage_Error =>
            Free_Physically (Pool);
            P := new Local_Storage_Array;
      end;

      --  Compute Storage_Address, aimed at receiving user data. We need room
      --  for the allocation header just ahead of the user data space plus
      --  alignment padding so Storage_Address is aligned on Storage_Alignment,
      --  like so:
      --
      --                         Storage_Address, aligned
      --                         on Storage_Alignment
      --                           v
      --          | ~~~~ | Header | User data ... |
      --                  ^........^
      --                  Header_Offset
      --
      --  Header_Offset is fixed so moving back and forth between user data
      --  and allocation header is straightforward. The value is also such
      --  that the header type alignment is honored when starting from
      --  Default_alignment.

      --  For the purpose of computing Storage_Address, we just do as if the
      --  header was located first, followed by the alignment padding:

      Storage_Address :=
        To_Address (Align (To_Integer (P.all'Address) +
                      Integer_Address (Header_Offset)));
      --  Computation is done in Integer_Address, not Storage_Offset, because
      --  the range of Storage_Offset may not be large enough.

      pragma Assert ((Storage_Address - System.Null_Address)
                     mod Storage_Alignment = 0);
      pragma Assert (Storage_Address + Size_In_Storage_Elements
                     <= P.all'Address + P'Length);

      Trace :=
        Find_Or_Create_Traceback
          (Pool                => Pool,
           Kind                => Alloc,
           Size                => Size_In_Storage_Elements,
           Ignored_Frame_Start => Allocate_Label'Address,
           Ignored_Frame_End   => Code_Address_For_Allocate_End);

      pragma Warnings (Off);
      --  Turn warning on alignment for convert call off. We know that in fact
      --  this conversion is safe since P itself is always aligned on
      --  Storage_Alignment.

      Header_Of (Storage_Address).all :=
        (Allocation_Address => P.all'Address,
         Alloc_Traceback    => Trace,
         Dealloc_Traceback  => To_Traceback (null),
         Next               => Pool.First_Used_Block,
         Block_Size         => Size_In_Storage_Elements);

      pragma Warnings (On);

      --  Link this block in the list of used blocks. This will be used to list
      --  memory leaks in Print_Info, and for the advanced schemes of
      --  Physical_Free, where we want to traverse all allocated blocks and
      --  search for possible references.

      --  We insert in front, since most likely we'll be freeing the most
      --  recently allocated blocks first (the older one might stay allocated
      --  for the whole life of the application).

      if Pool.First_Used_Block /= System.Null_Address then
         Header_Of (Pool.First_Used_Block).Dealloc_Traceback :=
           To_Address (Storage_Address);
      end if;

      Pool.First_Used_Block := Storage_Address;

      --  Mark the new address as valid

      Set_Valid (Storage_Address, True);

      if Pool.Low_Level_Traces then
         Put (Output_File (Pool),
              "info: Allocated"
              & Storage_Count'Image (Size_In_Storage_Elements)
              & " bytes at ");
         Print_Address (Output_File (Pool), Storage_Address);
         Put (Output_File (Pool),
              " (physically:"
              & Storage_Count'Image (Local_Storage_Array'Length)
              & " bytes at ");
         Print_Address (Output_File (Pool), P.all'Address);
         Put (Output_File (Pool),
              "), at ");
         Put_Line (Output_File (Pool), Pool.Stack_Trace_Depth, null,
                   Allocate_Label'Address,
                   Code_Address_For_Deallocate_End);
      end if;

      --  Update internal data

      Pool.Allocated :=
        Pool.Allocated + Byte_Count (Size_In_Storage_Elements);

      Current := Pool.Current_Water_Mark;

      if Current > Pool.High_Water then
         Pool.High_Water := Current;
      end if;

      Disable := False;

   exception
      when others =>
         if Reset_Disable_At_Exit then
            Disable := False;
         end if;
         raise;
   end Allocate;

   ------------------
   -- Allocate_End --
   ------------------

   --  DO NOT MOVE, this must be right after Allocate. This is similar to what
   --  is done in a-except, so that we can hide the traceback frames internal
   --  to this package

   procedure Allocate_End is
   begin
      <<Allocate_End_Label>>
      Code_Address_For_Allocate_End := Allocate_End_Label'Address;
   end Allocate_End;

   -------------------
   -- Set_Dead_Beef --
   -------------------

   procedure Set_Dead_Beef
     (Storage_Address          : System.Address;
      Size_In_Storage_Elements : Storage_Count)
   is
      Dead_Bytes : constant := 4;

      type Data is mod 2 ** (Dead_Bytes * 8);
      for Data'Size use Dead_Bytes * 8;

      Dead : constant Data := 16#DEAD_BEEF#;

      type Dead_Memory is array
        (1 .. Size_In_Storage_Elements / Dead_Bytes) of Data;
      type Mem_Ptr is access Dead_Memory;

      type Byte is mod 2 ** 8;
      for Byte'Size use 8;

      type Dead_Memory_Bytes is array (0 .. 2) of Byte;
      type Dead_Memory_Bytes_Ptr is access Dead_Memory_Bytes;

      function From_Ptr is new Ada.Unchecked_Conversion
        (System.Address, Mem_Ptr);

      function From_Ptr is new Ada.Unchecked_Conversion
        (System.Address, Dead_Memory_Bytes_Ptr);

      M      : constant Mem_Ptr := From_Ptr (Storage_Address);
      M2     : Dead_Memory_Bytes_Ptr;
      Modulo : constant Storage_Count :=
                 Size_In_Storage_Elements mod Dead_Bytes;
   begin
      M.all := [others => Dead];

      --  Any bytes left (up to three of them)

      if Modulo /= 0 then
         M2 := From_Ptr (Storage_Address + M'Length * Dead_Bytes);

         M2 (0) := 16#DE#;
         if Modulo >= 2 then
            M2 (1) := 16#AD#;

            if Modulo >= 3 then
               M2 (2) := 16#BE#;
            end if;
         end if;
      end if;
   end Set_Dead_Beef;

   ---------------------
   -- Free_Physically --
   ---------------------

   procedure Free_Physically (Pool : in out Debug_Pool) is
      type Byte is mod 256;
      type Byte_Access is access Byte;

      function To_Byte is new Ada.Unchecked_Conversion
        (System.Address, Byte_Access);

      type Address_Access is access System.Address;

      function To_Address_Access is new Ada.Unchecked_Conversion
        (System.Address, Address_Access);

      In_Use_Mark : constant Byte := 16#D#;
      Free_Mark   : constant Byte := 16#F#;

      Total_Freed : Storage_Count := 0;

      procedure Reset_Marks;
      --  Unmark all the logically freed blocks, so that they are considered
      --  for physical deallocation

      procedure Mark
        (H : Allocation_Header_Access; A : System.Address; In_Use : Boolean);
      --  Mark the user data block starting at A. For a block of size zero,
      --  nothing is done. For a block with a different size, the first byte
      --  is set to either "D" (in use) or "F" (free).

      function Marked (A : System.Address) return Boolean;
      --  Return true if the user data block starting at A might be in use
      --  somewhere else

      procedure Mark_Blocks;
      --  Traverse all allocated blocks, and search for possible references
      --  to logically freed blocks. Mark them appropriately

      procedure Free_Blocks (Ignore_Marks : Boolean);
      --  Physically release blocks. Only the blocks that haven't been marked
      --  will be released, unless Ignore_Marks is true.

      -----------------
      -- Free_Blocks --
      -----------------

      procedure Free_Blocks (Ignore_Marks : Boolean) is
         Header   : Allocation_Header_Access;
         Tmp      : System.Address := Pool.First_Free_Block;
         Next     : System.Address;
         Previous : System.Address := System.Null_Address;

      begin
         while Tmp /= System.Null_Address
           and then
             not (Total_Freed > Pool.Minimum_To_Free
                   and Pool.Logically_Deallocated <
                         Byte_Count (Pool.Maximum_Logically_Freed_Memory))
         loop
            Header := Header_Of (Tmp);

            --  If we know, or at least assume, the block is no longer
            --  referenced anywhere, we can free it physically.

            if Ignore_Marks or else not Marked (Tmp) then
               declare
                  pragma Suppress (All_Checks);
                  --  Suppress the checks on this section. If they are overflow
                  --  errors, it isn't critical, and we'd rather avoid a
                  --  Constraint_Error in that case.

               begin
                  --  Note that block_size < zero for freed blocks

                  Pool.Physically_Deallocated :=
                    Pool.Physically_Deallocated -
                      Byte_Count (Header.Block_Size);

                  Pool.Logically_Deallocated :=
                    Pool.Logically_Deallocated +
                      Byte_Count (Header.Block_Size);

                  Total_Freed := Total_Freed - Header.Block_Size;
               end;

               Next := Header.Next;

               if Pool.Low_Level_Traces then
                  Put
                    (Output_File (Pool),
                     "info: Freeing physical memory "
                     & Storage_Count'Image
                       ((abs Header.Block_Size) + Extra_Allocation)
                     & " bytes at ");
                  Print_Address (Output_File (Pool),
                                 Header.Allocation_Address);
                  Put_Line (Output_File (Pool), "");
               end if;

               if System_Memory_Debug_Pool_Enabled then
                  System.CRTL.free (Header.Allocation_Address);
               else
                  System.Memory.Free (Header.Allocation_Address);
               end if;

               Set_Valid (Tmp, False);

               --  Remove this block from the list

               if Previous = System.Null_Address then
                  Pool.First_Free_Block := Next;
               else
                  Header_Of (Previous).Next := Next;
               end if;

               Tmp := Next;

            else
               Previous := Tmp;
               Tmp := Header.Next;
            end if;
         end loop;
      end Free_Blocks;

      ----------
      -- Mark --
      ----------

      procedure Mark
        (H      : Allocation_Header_Access;
         A      : System.Address;
         In_Use : Boolean)
      is
      begin
         if H.Block_Size /= 0 then
            To_Byte (A).all := (if In_Use then In_Use_Mark else Free_Mark);
         end if;
      end Mark;

      -----------------
      -- Mark_Blocks --
      -----------------

      procedure Mark_Blocks is
         Tmp      : System.Address := Pool.First_Used_Block;
         Previous : System.Address;
         Last     : System.Address;
         Pointed  : System.Address;
         Header   : Allocation_Header_Access;

      begin
         --  For each allocated block, check its contents. Things that look
         --  like a possible address are used to mark the blocks so that we try
         --  and keep them, for better detection in case of invalid access.
         --  This mechanism is far from being fool-proof: it doesn't check the
         --  stacks of the threads, doesn't check possible memory allocated not
         --  under control of this debug pool. But it should allow us to catch
         --  more cases.

         while Tmp /= System.Null_Address loop
            Previous := Tmp;
            Last     := Tmp + Header_Of (Tmp).Block_Size;
            while Previous < Last loop
               --  ??? Should we move byte-per-byte, or consider that addresses
               --  are always aligned on 4-bytes boundaries ? Let's use the
               --  fastest for now.

               Pointed := To_Address_Access (Previous).all;
               if Is_Valid (Pointed) then
                  Header := Header_Of (Pointed);

                  --  Do not even attempt to mark blocks in use. That would
                  --  screw up the whole application, of course.

                  if Header.Block_Size < 0 then
                     Mark (Header, Pointed, In_Use => True);
                  end if;
               end if;

               Previous := Previous + System.Address'Size;
            end loop;

            Tmp := Header_Of (Tmp).Next;
         end loop;
      end Mark_Blocks;

      ------------
      -- Marked --
      ------------

      function Marked (A : System.Address) return Boolean is
      begin
         return To_Byte (A).all = In_Use_Mark;
      end Marked;

      -----------------
      -- Reset_Marks --
      -----------------

      procedure Reset_Marks is
         Current : System.Address := Pool.First_Free_Block;
         Header  : Allocation_Header_Access;

      begin
         while Current /= System.Null_Address loop
            Header := Header_Of (Current);
            Mark (Header, Current, False);
            Current := Header.Next;
         end loop;
      end Reset_Marks;

      Lock : Scope_Lock;
      pragma Unreferenced (Lock);

   --  Start of processing for Free_Physically

   begin
      if Pool.Advanced_Scanning then

         --  Reset the mark for each freed block

         Reset_Marks;

         Mark_Blocks;
      end if;

      Free_Blocks (Ignore_Marks => not Pool.Advanced_Scanning);

      --  The contract is that we need to free at least Minimum_To_Free bytes,
      --  even if this means freeing marked blocks in the advanced scheme.

      if Total_Freed < Pool.Minimum_To_Free
        and then Pool.Advanced_Scanning
      then
         Pool.Marked_Blocks_Deallocated := True;
         Free_Blocks (Ignore_Marks => True);
      end if;
   end Free_Physically;

   --------------
   -- Get_Size --
   --------------

   procedure Get_Size
     (Storage_Address          : Address;
      Size_In_Storage_Elements : out Storage_Count;
      Valid                    : out Boolean)
   is
      Lock : Scope_Lock;
      pragma Unreferenced (Lock);

   begin
      Valid := Is_Valid (Storage_Address);
      Size_In_Storage_Elements := Storage_Count'First;

      if Is_Valid (Storage_Address) then
         declare
            Header : constant Allocation_Header_Access :=
                       Header_Of (Storage_Address);

         begin
            if Header.Block_Size >= 0 then
               Valid := True;
               Size_In_Storage_Elements := Header.Block_Size;
            else
               Valid := False;
            end if;
         end;
      else
         Valid := False;
      end if;
   end Get_Size;

   ---------------------
   -- Print_Traceback --
   ---------------------

   procedure Print_Traceback
     (Output_File : File_Type;
      Prefix      : String;
      Traceback   : Traceback_Htable_Elem_Ptr)
   is
   begin
      if Traceback /= null then
         Put (Output_File, Prefix);
         Put_Line (Output_File, 0, Traceback.Traceback);
      end if;
   end Print_Traceback;

   ----------------
   -- Deallocate --
   ----------------

   procedure Deallocate
     (Pool                     : in out Debug_Pool;
      Storage_Address          : Address;
      Size_In_Storage_Elements : Storage_Count;
      Alignment                : Storage_Count)
   is
      pragma Unreferenced (Alignment);

      Header   : constant Allocation_Header_Access :=
                   Header_Of (Storage_Address);
      Previous : System.Address;
      Valid    : Boolean;

      Header_Block_Size_Was_Less_Than_0 : Boolean := True;

   begin
      <<Deallocate_Label>>

      declare
         Lock : Scope_Lock;
         pragma Unreferenced (Lock);

      begin
         Valid := Is_Valid (Storage_Address);

         if Valid and then not (Header.Block_Size < 0) then
            Header_Block_Size_Was_Less_Than_0 := False;

            --  Some sort of codegen problem or heap corruption caused the
            --  Size_In_Storage_Elements to be wrongly computed. The code
            --  below is all based on the assumption that Header.all is not
            --  corrupted, such that the error is non-fatal.

            if Header.Block_Size /= Size_In_Storage_Elements and then
              Size_In_Storage_Elements /= Storage_Count'Last
            then
               Put_Line (Output_File (Pool),
                         "error: Deallocate size "
                         & Storage_Count'Image (Size_In_Storage_Elements)
                         & " does not match allocate size "
                         & Storage_Count'Image (Header.Block_Size));
            end if;

            if Pool.Low_Level_Traces then
               Put (Output_File (Pool),
                    "info: Deallocated"
                    & Storage_Count'Image (Header.Block_Size)
                    & " bytes at ");
               Print_Address (Output_File (Pool), Storage_Address);
               Put (Output_File (Pool),
                    " (physically"
                    & Storage_Count'Image
                      (Header.Block_Size + Extra_Allocation)
                    & " bytes at ");
               Print_Address (Output_File (Pool), Header.Allocation_Address);
               Put (Output_File (Pool), "), at ");

               Put_Line (Output_File (Pool), Pool.Stack_Trace_Depth, null,
                         Deallocate_Label'Address,
                         Code_Address_For_Deallocate_End);
               Print_Traceback (Output_File (Pool),
                                "   Memory was allocated at ",
                                Header.Alloc_Traceback);
            end if;

            --  Remove this block from the list of used blocks

            Previous :=
              To_Address (Header.Dealloc_Traceback);

            if Previous = System.Null_Address then
               Pool.First_Used_Block := Header_Of (Pool.First_Used_Block).Next;

               if Pool.First_Used_Block /= System.Null_Address then
                  Header_Of (Pool.First_Used_Block).Dealloc_Traceback :=
                    To_Traceback (null);
               end if;

            else
               Header_Of (Previous).Next := Header.Next;

               if Header.Next /= System.Null_Address then
                  Header_Of
                    (Header.Next).Dealloc_Traceback := To_Address (Previous);
               end if;
            end if;

            --  Update the Alloc_Traceback Frees/Total_Frees members
            --  (if present)

            if Header.Alloc_Traceback /= null then
               Header.Alloc_Traceback.Frees :=
                 Header.Alloc_Traceback.Frees + 1;
               Header.Alloc_Traceback.Total_Frees :=
                 Header.Alloc_Traceback.Total_Frees +
                   Byte_Count (Header.Block_Size);
            end if;

            Pool.Free_Count := Pool.Free_Count + 1;

            --  Update the header

            Header.all :=
              (Allocation_Address => Header.Allocation_Address,
               Alloc_Traceback    => Header.Alloc_Traceback,
               Dealloc_Traceback  => To_Traceback
                 (Find_Or_Create_Traceback
                      (Pool, Dealloc,
                       Header.Block_Size,
                       Deallocate_Label'Address,
                       Code_Address_For_Deallocate_End)),
               Next               => System.Null_Address,
               Block_Size         => -Header.Block_Size);

            if Pool.Reset_Content_On_Free then
               Set_Dead_Beef (Storage_Address, -Header.Block_Size);
            end if;

            Pool.Logically_Deallocated :=
              Pool.Logically_Deallocated + Byte_Count (-Header.Block_Size);

            --  Link this free block with the others (at the end of the list,
            --  so that we can start releasing the older blocks first later on)

            if Pool.First_Free_Block = System.Null_Address then
               Pool.First_Free_Block := Storage_Address;
               Pool.Last_Free_Block := Storage_Address;

            else
               Header_Of (Pool.Last_Free_Block).Next := Storage_Address;
               Pool.Last_Free_Block := Storage_Address;
            end if;

            --  Do not physically release the memory here, but in Alloc.
            --  See comment there for details.
         end if;
      end;

      if not Valid then
         if Storage_Address = System.Null_Address then
            if Pool.Raise_Exceptions and then
              Size_In_Storage_Elements /= Storage_Count'Last
            then
               raise Freeing_Not_Allocated_Storage;
            else
               Put (Output_File (Pool),
                    "error: Freeing Null_Address, at ");
               Put_Line (Output_File (Pool), Pool.Stack_Trace_Depth, null,
                         Deallocate_Label'Address,
                         Code_Address_For_Deallocate_End);
               return;
            end if;
         end if;

         if Allow_Unhandled_Memory
           and then not Is_Handled (Storage_Address)
         then
            System.CRTL.free (Storage_Address);
            return;
         end if;

         if Pool.Raise_Exceptions
           and then Size_In_Storage_Elements /= Storage_Count'Last
         then
            raise Freeing_Not_Allocated_Storage;
         else
            Put (Output_File (Pool),
                 "error: Freeing not allocated storage, at ");
            Put_Line (Output_File (Pool), Pool.Stack_Trace_Depth, null,
                      Deallocate_Label'Address,
                      Code_Address_For_Deallocate_End);
         end if;

      elsif Header_Block_Size_Was_Less_Than_0 then
         if Pool.Raise_Exceptions then
            raise Freeing_Deallocated_Storage;
         else
            Put (Output_File (Pool),
                 "error: Freeing already deallocated storage, at ");
            Put_Line (Output_File (Pool), Pool.Stack_Trace_Depth, null,
                      Deallocate_Label'Address,
                      Code_Address_For_Deallocate_End);
            Print_Traceback (Output_File (Pool),
                             "   Memory already deallocated at ",
                            To_Traceback (Header.Dealloc_Traceback));
            Print_Traceback (Output_File (Pool), "   Memory was allocated at ",
                             Header.Alloc_Traceback);
         end if;
      end if;
   end Deallocate;

   --------------------
   -- Deallocate_End --
   --------------------

   --  DO NOT MOVE, this must be right after Deallocate

   --  See Allocate_End

   --  This is making assumptions about code order that may be invalid ???

   procedure Deallocate_End is
   begin
      <<Deallocate_End_Label>>
      Code_Address_For_Deallocate_End := Deallocate_End_Label'Address;
   end Deallocate_End;

   -----------------
   -- Dereference --
   -----------------

   procedure Dereference
     (Pool                     : in out Debug_Pool;
      Storage_Address          : Address;
      Size_In_Storage_Elements : Storage_Count;
      Alignment                : Storage_Count)
   is
      pragma Unreferenced (Alignment, Size_In_Storage_Elements);

      Valid   : constant Boolean := Is_Valid (Storage_Address);
      Header  : Allocation_Header_Access;

   begin
      --  Locking policy: we do not do any locking in this procedure. The
      --  tables are only read, not written to, and although a problem might
      --  appear if someone else is modifying the tables at the same time, this
      --  race condition is not intended to be detected by this storage_pool (a
      --  now invalid pointer would appear as valid). Instead, we prefer
      --  optimum performance for dereferences.

      <<Dereference_Label>>

      if not Valid then
         if Pool.Raise_Exceptions then
            raise Accessing_Not_Allocated_Storage;
         else
            Put (Output_File (Pool),
                 "error: Accessing not allocated storage, at ");
            Put_Line (Output_File (Pool), Pool.Stack_Trace_Depth, null,
                      Dereference_Label'Address,
                      Code_Address_For_Dereference_End);
         end if;

      else
         Header := Header_Of (Storage_Address);

         if Header.Block_Size < 0 then
            if Pool.Raise_Exceptions then
               raise Accessing_Deallocated_Storage;
            else
               Put (Output_File (Pool),
                    "error: Accessing deallocated storage, at ");
               Put_Line
                 (Output_File (Pool), Pool.Stack_Trace_Depth, null,
                  Dereference_Label'Address,
                  Code_Address_For_Dereference_End);
               Print_Traceback (Output_File (Pool), "  First deallocation at ",
                                To_Traceback (Header.Dealloc_Traceback));
               Print_Traceback (Output_File (Pool), "  Initial allocation at ",
                                Header.Alloc_Traceback);
            end if;
         end if;
      end if;
   end Dereference;

   ---------------------
   -- Dereference_End --
   ---------------------

   --  DO NOT MOVE: this must be right after Dereference

   --  See Allocate_End

   --  This is making assumptions about code order that may be invalid ???

   procedure Dereference_End is
   begin
      <<Dereference_End_Label>>
      Code_Address_For_Dereference_End := Dereference_End_Label'Address;
   end Dereference_End;

   ----------------
   -- Print_Info --
   ----------------

   procedure Print_Info
     (Pool          : Debug_Pool;
      Cumulate      : Boolean := False;
      Display_Slots : Boolean := False;
      Display_Leaks : Boolean := False)
   is
      package Backtrace_Htable_Cumulate is new GNAT.HTable.Static_HTable
        (Header_Num => Header,
         Element    => Traceback_Htable_Elem,
         Elmt_Ptr   => Traceback_Htable_Elem_Ptr,
         Null_Ptr   => null,
         Set_Next   => Set_Next,
         Next       => Next,
         Key        => Tracebacks_Array_Access,
         Get_Key    => Get_Key,
         Hash       => Hash,
         Equal      => Equal);
      --  This needs a comment ??? probably some of the ones below do too???

      Current : System.Address;
      Data    : Traceback_Htable_Elem_Ptr;
      Elem    : Traceback_Htable_Elem_Ptr;
      Header  : Allocation_Header_Access;
      K       : Traceback_Kind;

   begin
      Put_Line
        ("Total allocated bytes : " &
         Byte_Count'Image (Pool.Allocated));

      Put_Line
        ("Total logically deallocated bytes : " &
         Byte_Count'Image (Pool.Logically_Deallocated));

      Put_Line
        ("Total physically deallocated bytes : " &
         Byte_Count'Image (Pool.Physically_Deallocated));

      if Pool.Marked_Blocks_Deallocated then
         Put_Line ("Marked blocks were physically deallocated. This is");
         Put_Line ("potentially dangerous, and you might want to run");
         Put_Line ("again with a lower value of Minimum_To_Free");
      end if;

      Put_Line
        ("Current Water Mark: " &
         Byte_Count'Image (Pool.Current_Water_Mark));

      Put_Line
        ("High Water Mark: " &
          Byte_Count'Image (Pool.High_Water));

      Put_Line ("");

      if Display_Slots then
         Data := Backtrace_Htable.Get_First;
         while Data /= null loop
            if Data.Kind in Alloc .. Dealloc then
               Elem :=
                 new Traceback_Htable_Elem'
                       (Traceback => new Tracebacks_Array'(Data.Traceback.all),
                        Count       => Data.Count,
                        Kind        => Data.Kind,
                        Total       => Data.Total,
                        Frees       => Data.Frees,
                        Total_Frees => Data.Total_Frees,
                        Next        => null);
               Backtrace_Htable_Cumulate.Set (Elem);

               if Cumulate then
                  K := (if Data.Kind = Alloc then Indirect_Alloc
                                             else Indirect_Dealloc);

                  --  Propagate the direct call to all its parents

                  for T in Data.Traceback'First + 1 .. Data.Traceback'Last loop
                     Elem := Backtrace_Htable_Cumulate.Get
                       (Data.Traceback
                          (T .. Data.Traceback'Last)'Unrestricted_Access);

                     --  If not, insert it

                     if Elem = null then
                        Elem :=
                          new Traceback_Htable_Elem'
                                (Traceback =>
                                   new Tracebacks_Array'
                                         (Data.Traceback
                                           (T .. Data.Traceback'Last)),
                                 Count       => Data.Count,
                                 Kind        => K,
                                 Total       => Data.Total,
                                 Frees       => Data.Frees,
                                 Total_Frees => Data.Total_Frees,
                                 Next        => null);
                        Backtrace_Htable_Cumulate.Set (Elem);

                        --  Properly take into account that the subprograms
                        --  indirectly called might be doing either allocations
                        --  or deallocations. This needs to be reflected in the
                        --  counts.

                     else
                        Elem.Count := Elem.Count + Data.Count;

                        if K = Elem.Kind then
                           Elem.Total := Elem.Total + Data.Total;

                        elsif Elem.Total > Data.Total then
                           Elem.Total := Elem.Total - Data.Total;

                        else
                           Elem.Kind  := K;
                           Elem.Total := Data.Total - Elem.Total;
                        end if;
                     end if;
                  end loop;
               end if;

               Data := Backtrace_Htable.Get_Next;
            end if;
         end loop;

         Put_Line ("List of allocations/deallocations: ");

         Data := Backtrace_Htable_Cumulate.Get_First;
         while Data /= null loop
            case Data.Kind is
               when Alloc            => Put ("alloc (count:");
               when Indirect_Alloc   => Put ("indirect alloc (count:");
               when Dealloc          => Put ("free  (count:");
               when Indirect_Dealloc => Put ("indirect free  (count:");
            end case;

            Put (Natural'Image (Data.Count) & ", total:" &
                 Byte_Count'Image (Data.Total) & ") ");

            for T in Data.Traceback'Range loop
               Put (Image_C (PC_For (Data.Traceback (T))) & ' ');
            end loop;

            Put_Line ("");

            Data := Backtrace_Htable_Cumulate.Get_Next;
         end loop;

         Backtrace_Htable_Cumulate.Reset;
      end if;

      if Display_Leaks then
         Put_Line ("");
         Put_Line ("List of not deallocated blocks:");

         --  Do not try to group the blocks with the same stack traces
         --  together. This is done by the gnatmem output.

         Current := Pool.First_Used_Block;
         while Current /= System.Null_Address loop
            Header := Header_Of (Current);

            Put ("Size: " & Storage_Count'Image (Header.Block_Size) & " at: ");

            if Header.Alloc_Traceback /= null then
               for T in Header.Alloc_Traceback.Traceback'Range loop
                  Put (Image_C
                       (PC_For (Header.Alloc_Traceback.Traceback (T))) & ' ');
               end loop;
            end if;

            Put_Line ("");
            Current := Header.Next;
         end loop;
      end if;
   end Print_Info;

   ----------
   -- Dump --
   ----------

   procedure Dump
     (Pool   : Debug_Pool;
      Size   : Positive;
      Report : Report_Type := All_Reports)
   is
      procedure Do_Report (Sort : Report_Type);
      --  Do a specific type of report

      ---------------
      -- Do_Report --
      ---------------

      procedure Do_Report (Sort : Report_Type) is
         Elem        : Traceback_Htable_Elem_Ptr;
         Bigger      : Boolean;
         Grand_Total : Float;

         Max  : array (1 .. Size) of Traceback_Htable_Elem_Ptr :=
           [others => null];
         --  Sorted array for the biggest memory users

         Allocated_In_Pool : Byte_Count;
         --  safe thread Pool.Allocated

         Elem_Safe : Traceback_Htable_Elem;
         --  safe thread current elem.all;

         Max_M_Safe : Traceback_Htable_Elem;
         --  safe thread Max(M).all

      begin
         Put_Line ("");

         case Sort is
            when All_Reports
               | Memory_Usage
            =>
               Put_Line (Size'Img & " biggest memory users at this time:");
               Put_Line ("Results include bytes and chunks still allocated");
               Grand_Total := Float (Pool.Current_Water_Mark);

            when Allocations_Count =>
               Put_Line (Size'Img & " biggest number of live allocations:");
               Put_Line ("Results include bytes and chunks still allocated");
               Grand_Total := Float (Pool.Current_Water_Mark);

            when Sort_Total_Allocs =>
               Put_Line (Size'Img & " biggest number of allocations:");
               Put_Line ("Results include total bytes and chunks allocated,");
               Put_Line ("even if no longer allocated - Deallocations are"
                         & " ignored");

               declare
                  Lock : Scope_Lock;
                  pragma Unreferenced (Lock);
               begin
                  Allocated_In_Pool := Pool.Allocated;
               end;

               Grand_Total := Float (Allocated_In_Pool);

            when Marked_Blocks =>
               Put_Line ("Special blocks marked by Mark_Traceback");
               Grand_Total := 0.0;
         end case;

         declare
            Lock : Scope_Lock;
            pragma Unreferenced (Lock);
         begin
            Elem := Backtrace_Htable.Get_First;
         end;

         while Elem /= null loop
            declare
               Lock : Scope_Lock;
               pragma Unreferenced (Lock);
            begin
               Elem_Safe := Elem.all;
            end;

            --  Handle only alloc elememts
            if Elem_Safe.Kind = Alloc then
               --  Ignore small blocks (depending on the sorting criteria) to
               --  gain speed.

               if (Sort = Memory_Usage
                    and then Elem_Safe.Total - Elem_Safe.Total_Frees >= 1_000)
                 or else (Sort = Allocations_Count
                           and then Elem_Safe.Count - Elem_Safe.Frees >= 1)
                 or else (Sort = Sort_Total_Allocs
                           and then Elem_Safe.Count > 1)
                 or else (Sort = Marked_Blocks
                           and then Elem_Safe.Total = 0)
               then
                  if Sort = Marked_Blocks then
                     Grand_Total := Grand_Total + Float (Elem_Safe.Count);
                  end if;

                  for M in Max'Range loop
                     Bigger := Max (M) = null;
                     if not Bigger then
                        declare
                           Lock : Scope_Lock;
                           pragma Unreferenced (Lock);
                        begin
                           Max_M_Safe := Max (M).all;
                        end;

                        case Sort is
                           when All_Reports
                              | Memory_Usage
                           =>
                              Bigger :=
                                Max_M_Safe.Total - Max_M_Safe.Total_Frees
                                  < Elem_Safe.Total - Elem_Safe.Total_Frees;

                           when Allocations_Count =>
                              Bigger :=
                                Max_M_Safe.Count - Max_M_Safe.Frees
                                  < Elem_Safe.Count - Elem_Safe.Frees;

                           when Marked_Blocks
                              | Sort_Total_Allocs
                           =>
                              Bigger := Max_M_Safe.Count < Elem_Safe.Count;
                        end case;
                     end if;

                     if Bigger then
                        Max (M + 1 .. Max'Last) := Max (M .. Max'Last - 1);
                        Max (M) := Elem;
                        exit;
                     end if;
                  end loop;
               end if;
            end if;

            declare
               Lock : Scope_Lock;
               pragma Unreferenced (Lock);
            begin
               Elem := Backtrace_Htable.Get_Next;
            end;
         end loop;

         if Grand_Total = 0.0 then
            Grand_Total := 1.0;
         end if;

         for M in Max'Range loop
            exit when Max (M) = null;
            declare
               type Percent is delta 0.1 range 0.0 .. 100.0;

               P     : Percent;
               Total : Byte_Count;

            begin
               declare
                  Lock : Scope_Lock;
                  pragma Unreferenced (Lock);
               begin
                  Max_M_Safe := Max (M).all;
               end;

               case Sort is
                  when All_Reports
                     | Allocations_Count
                     | Memory_Usage
                  =>
                     Total := Max_M_Safe.Total - Max_M_Safe.Total_Frees;

                  when Sort_Total_Allocs =>
                     Total := Max_M_Safe.Total;

                  when Marked_Blocks =>
                     Total := Byte_Count (Max_M_Safe.Count);
               end case;

               declare
                  Normalized_Total : constant Float := Float (Total);
                  --  In multi tasking configuration, memory deallocations
                  --  during Do_Report processing can lead to Total >
                  --  Grand_Total. As Percent requires Total <= Grand_Total

               begin
                  if Normalized_Total > Grand_Total then
                     P := 100.0;
                  else
                     P := Percent (100.0 * Normalized_Total / Grand_Total);
                  end if;
               end;

               case Sort is
                  when All_Reports
                     | Allocations_Count
                     | Memory_Usage
                  =>
                     declare
                        Count : constant Natural :=
                          Max_M_Safe.Count - Max_M_Safe.Frees;
                     begin
                        Put (P'Img & "%:" & Total'Img & " bytes in"
                             & Count'Img & " chunks at");
                     end;

                  when Sort_Total_Allocs =>
                     Put (P'Img & "%:" & Total'Img & " bytes in"
                          & Max_M_Safe.Count'Img & " chunks at");

                  when Marked_Blocks =>
                     Put (P'Img & "%:"
                          & Max_M_Safe.Count'Img & " chunks /"
                          & Integer (Grand_Total)'Img & " at");
               end case;
            end;

            for J in Max (M).Traceback'Range loop
               Put (" " & Image_C (PC_For (Max (M).Traceback (J))));
            end loop;

            Put_Line ("");
         end loop;
      end Do_Report;

      --  Local variables

      Total_Freed : Byte_Count;
      --  safe thread pool logically & physically deallocated

      Traceback_Elements_Allocated : Byte_Count;
      --  safe thread Traceback_Count

      Validity_Elements_Allocated : Byte_Count;
      --  safe thread Validity_Count

      Ada_Allocs_Bytes : Byte_Count;
      --  safe thread pool Allocated

      Ada_Allocs_Chunks : Byte_Count;
      --  safe thread pool Alloc_Count

      Ada_Free_Chunks : Byte_Count;
      --  safe thread pool Free_Count

   --  Start of processing for Dump

   begin
      declare
         Lock : Scope_Lock;
         pragma Unreferenced (Lock);
      begin
         Total_Freed :=
           Pool.Logically_Deallocated + Pool.Physically_Deallocated;
         Traceback_Elements_Allocated := Traceback_Count;
         Validity_Elements_Allocated := Validity_Count;
         Ada_Allocs_Bytes := Pool.Allocated;
         Ada_Allocs_Chunks := Pool.Alloc_Count;
         Ada_Free_Chunks := Pool.Free_Count;
      end;

      Put_Line
        ("Traceback elements allocated: " & Traceback_Elements_Allocated'Img);
      Put_Line
        ("Validity elements allocated: " & Validity_Elements_Allocated'Img);
      Put_Line ("");

      Put_Line ("Ada Allocs:" & Ada_Allocs_Bytes'Img
                & " bytes in" & Ada_Allocs_Chunks'Img & " chunks");
      Put_Line ("Ada Free:" & Total_Freed'Img & " bytes in" &
                  Ada_Free_Chunks'Img
                & " chunks");
      Put_Line ("Ada Current watermark: "
                & Byte_Count'Image (Pool.Current_Water_Mark)
                & " in" & Byte_Count'Image (Ada_Allocs_Chunks -
                    Ada_Free_Chunks) & " chunks");
      Put_Line ("Ada High watermark: " & Pool.High_Water_Mark'Img);

      case Report is
         when All_Reports =>
            for Sort in Report_Type loop
               if Sort /= All_Reports then
                  Do_Report (Sort);
               end if;
            end loop;

         when others =>
            Do_Report (Report);
      end case;
   end Dump;

   -----------------
   -- Dump_Stdout --
   -----------------

   procedure Dump_Stdout
     (Pool   : Debug_Pool;
      Size   : Positive;
      Report : Report_Type := All_Reports)
   is
      procedure Internal is new Dump
        (Put_Line => Stdout_Put_Line,
         Put      => Stdout_Put);

   --  Start of processing for Dump_Stdout

   begin
      Internal (Pool, Size, Report);
   end Dump_Stdout;

   -----------
   -- Reset --
   -----------

   procedure Reset is
      Elem : Traceback_Htable_Elem_Ptr;
      Lock : Scope_Lock;
      pragma Unreferenced (Lock);
   begin
      Elem := Backtrace_Htable.Get_First;
      while Elem /= null loop
         Elem.Count := 0;
         Elem.Frees := 0;
         Elem.Total := 0;
         Elem.Total_Frees := 0;
         Elem := Backtrace_Htable.Get_Next;
      end loop;
   end Reset;

   ------------------
   -- Storage_Size --
   ------------------

   function Storage_Size (Pool : Debug_Pool) return Storage_Count is
      pragma Unreferenced (Pool);
   begin
      return Storage_Count'Last;
   end Storage_Size;

   ---------------------
   -- High_Water_Mark --
   ---------------------

   function High_Water_Mark (Pool : Debug_Pool) return Byte_Count is
      Lock : Scope_Lock;
      pragma Unreferenced (Lock);
   begin
      return Pool.High_Water;
   end High_Water_Mark;

   ------------------------
   -- Current_Water_Mark --
   ------------------------

   function Current_Water_Mark (Pool : Debug_Pool) return Byte_Count is
      Lock : Scope_Lock;
      pragma Unreferenced (Lock);
   begin
      return Pool.Allocated - Pool.Logically_Deallocated -
        Pool.Physically_Deallocated;
   end Current_Water_Mark;

   ------------------------------
   -- System_Memory_Debug_Pool --
   ------------------------------

   procedure System_Memory_Debug_Pool
     (Has_Unhandled_Memory : Boolean := True)
   is
      Lock : Scope_Lock;
      pragma Unreferenced (Lock);
   begin
      System_Memory_Debug_Pool_Enabled := True;
      Allow_Unhandled_Memory := Has_Unhandled_Memory;
   end System_Memory_Debug_Pool;

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (Pool                           : in out Debug_Pool;
      Stack_Trace_Depth              : Natural := Default_Stack_Trace_Depth;
      Maximum_Logically_Freed_Memory : SSC     := Default_Max_Freed;
      Minimum_To_Free                : SSC     := Default_Min_Freed;
      Reset_Content_On_Free          : Boolean := Default_Reset_Content;
      Raise_Exceptions               : Boolean := Default_Raise_Exceptions;
      Advanced_Scanning              : Boolean := Default_Advanced_Scanning;
      Errors_To_Stdout               : Boolean := Default_Errors_To_Stdout;
      Low_Level_Traces               : Boolean := Default_Low_Level_Traces)
   is
      Lock : Scope_Lock;
      pragma Unreferenced (Lock);
   begin
      Pool.Stack_Trace_Depth              := Stack_Trace_Depth;
      Pool.Maximum_Logically_Freed_Memory := Maximum_Logically_Freed_Memory;
      Pool.Reset_Content_On_Free          := Reset_Content_On_Free;
      Pool.Raise_Exceptions               := Raise_Exceptions;
      Pool.Minimum_To_Free                := Minimum_To_Free;
      Pool.Advanced_Scanning              := Advanced_Scanning;
      Pool.Errors_To_Stdout               := Errors_To_Stdout;
      Pool.Low_Level_Traces               := Low_Level_Traces;
   end Configure;

   ----------------
   -- Print_Pool --
   ----------------

   procedure Print_Pool (A : System.Address) is
      Storage : constant Address := A;
      Valid   : constant Boolean := Is_Valid (Storage);
      Header  : Allocation_Header_Access;

   begin
      --  We might get Null_Address if the call from gdb was done incorrectly.
      --  For instance, doing a "print_pool(my_var)" passes 0x0, instead of
      --  passing the value of my_var.

      if A = System.Null_Address then
         Put_Line
            (Standard_Output, "Memory not under control of the storage pool");
         return;
      end if;

      if not Valid then
         Put_Line
            (Standard_Output, "Memory not under control of the storage pool");

      else
         Header := Header_Of (Storage);
         Print_Address (Standard_Output, A);
         Put_Line (Standard_Output, " allocated at:");
         Print_Traceback (Standard_Output, "", Header.Alloc_Traceback);

         if To_Traceback (Header.Dealloc_Traceback) /= null then
            Print_Address (Standard_Output, A);
            Put_Line (Standard_Output,
                      " logically freed memory, deallocated at:");
            Print_Traceback (Standard_Output, "",
                             To_Traceback (Header.Dealloc_Traceback));
         end if;
      end if;
   end Print_Pool;

   -----------------------
   -- Print_Info_Stdout --
   -----------------------

   procedure Print_Info_Stdout
     (Pool          : Debug_Pool;
      Cumulate      : Boolean := False;
      Display_Slots : Boolean := False;
      Display_Leaks : Boolean := False)
   is
      procedure Internal is new Print_Info
        (Put_Line => Stdout_Put_Line,
         Put      => Stdout_Put);

   --  Start of processing for Print_Info_Stdout

   begin
      Internal (Pool, Cumulate, Display_Slots, Display_Leaks);
   end Print_Info_Stdout;

   ------------------
   -- Dump_Gnatmem --
   ------------------

   procedure Dump_Gnatmem (Pool : Debug_Pool; File_Name : String) is
      type File_Ptr is new System.Address;

      function fopen (Path : String; Mode : String) return File_Ptr;
      pragma Import (C, fopen);

      procedure fwrite
        (Ptr    : System.Address;
         Size   : size_t;
         Nmemb  : size_t;
         Stream : File_Ptr);

      procedure fwrite
        (Str    : String;
         Size   : size_t;
         Nmemb  : size_t;
         Stream : File_Ptr);
      pragma Import (C, fwrite);

      procedure fputc (C : Integer; Stream : File_Ptr);
      pragma Import (C, fputc);

      procedure fclose (Stream : File_Ptr);
      pragma Import (C, fclose);

      Address_Size : constant size_t :=
                       System.Address'Max_Size_In_Storage_Elements;
      --  Size in bytes of a pointer

      File        : File_Ptr;
      Current     : System.Address;
      Header      : Allocation_Header_Access;
      Actual_Size : size_t;
      Num_Calls   : Integer;
      Tracebk     : Tracebacks_Array_Access;
      Dummy_Time  : Duration := 1.0;

   begin
      File := fopen (File_Name & ASCII.NUL, "wb" & ASCII.NUL);
      fwrite ("GMEM DUMP" & ASCII.LF, 10, 1, File);

      fwrite
        (Ptr    => Dummy_Time'Address,
         Size   => Duration'Max_Size_In_Storage_Elements,
         Nmemb  => 1,
         Stream => File);

      --  List of not deallocated blocks (see Print_Info)

      Current := Pool.First_Used_Block;
      while Current /= System.Null_Address loop
         Header := Header_Of (Current);

         Actual_Size := size_t (Header.Block_Size);

         if Header.Alloc_Traceback /= null then
            Tracebk   := Header.Alloc_Traceback.Traceback;
            Num_Calls := Tracebk'Length;

            --  (Code taken from memtrack.adb in GNAT's sources)

            --  Logs allocation call using the format:

            --  'A' <mem addr> <size chunk> <len backtrace> <addr1> ... <addrn>

            fputc (Character'Pos ('A'), File);
            fwrite (Current'Address, Address_Size, 1, File);

            fwrite
              (Ptr    => Actual_Size'Address,
               Size   => size_t'Max_Size_In_Storage_Elements,
               Nmemb  => 1,
               Stream => File);

            fwrite
              (Ptr    => Dummy_Time'Address,
               Size   => Duration'Max_Size_In_Storage_Elements,
               Nmemb  => 1,
               Stream => File);

            fwrite
              (Ptr    => Num_Calls'Address,
               Size   => Integer'Max_Size_In_Storage_Elements,
               Nmemb  => 1,
               Stream => File);

            for J in Tracebk'First .. Tracebk'First + Num_Calls - 1 loop
               declare
                  Ptr : System.Address := PC_For (Tracebk (J));
               begin
                  fwrite (Ptr'Address, Address_Size, 1, File);
               end;
            end loop;
         end if;

         Current := Header.Next;
      end loop;

      fclose (File);
   end Dump_Gnatmem;

   ----------------
   -- Stdout_Put --
   ----------------

   procedure Stdout_Put (S : String) is
   begin
      Put (Standard_Output, S);
   end Stdout_Put;

   ---------------------
   -- Stdout_Put_Line --
   ---------------------

   procedure Stdout_Put_Line (S : String) is
   begin
      Put_Line (Standard_Output, S);
   end Stdout_Put_Line;

--  Package initialization

begin
   Allocate_End;
   Deallocate_End;
   Dereference_End;
end GNAT.Debug_Pools;
