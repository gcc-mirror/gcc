------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                         S Y S T E M . M E M O R Y                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2001-2011, Free Software Foundation, Inc.         --
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

--  This is the VMS 64 bit implementation of this package

--  This implementation assumes that the underlying malloc/free/realloc
--  implementation is thread safe, and thus, no additional lock is required.
--  Note that we still need to defer abort because on most systems, an
--  asynchronous signal (as used for implementing asynchronous abort of
--  task) cannot safely be handled while malloc is executing.

--  If you are not using Ada constructs containing the "abort" keyword, then
--  you can remove the calls to Abort_Defer.all and Abort_Undefer.all from
--  this unit.

pragma Compiler_Unit;

with Ada.Exceptions;
with System.Soft_Links;
with System.Parameters;
with System.CRTL;

package body System.Memory is

   use Ada.Exceptions;
   use System.Soft_Links;

   function c_malloc (Size : System.CRTL.size_t) return System.Address
    renames System.CRTL.malloc;

   procedure c_free (Ptr : System.Address)
     renames System.CRTL.free;

   function c_realloc
     (Ptr : System.Address; Size : System.CRTL.size_t) return System.Address
     renames System.CRTL.realloc;

   Gnat_Heap_Size : Integer;
   pragma Import (C, Gnat_Heap_Size, "__gl_heap_size");
   --  Set by Feature logical GNAT$NO_MALLOC_64 and/or Binder switch -Hnn

   -----------
   -- Alloc --
   -----------

   function Alloc (Size : size_t) return System.Address is
      Result      : System.Address;
      Actual_Size : size_t := Size;

   begin
      if Gnat_Heap_Size = 32 then
         return Alloc32 (Size);
      end if;

      if Size = size_t'Last then
         Raise_Exception (Storage_Error'Identity, "object too large");
      end if;

      --  Change size from zero to non-zero. We still want a proper pointer
      --  for the zero case because pointers to zero length objects have to
      --  be distinct, but we can't just go ahead and allocate zero bytes,
      --  since some malloc's return zero for a zero argument.

      if Size = 0 then
         Actual_Size := 1;
      end if;

      if Parameters.No_Abort then
         Result := c_malloc (System.CRTL.size_t (Actual_Size));
      else
         Abort_Defer.all;
         Result := c_malloc (System.CRTL.size_t (Actual_Size));
         Abort_Undefer.all;
      end if;

      if Result = System.Null_Address then
         Raise_Exception (Storage_Error'Identity, "heap exhausted");
      end if;

      return Result;
   end Alloc;

   -------------
   -- Alloc32 --
   -------------

   function Alloc32 (Size : size_t) return System.Address is
      Result      : System.Address;
      Actual_Size : size_t := Size;

   begin
      if Size = size_t'Last then
         Raise_Exception (Storage_Error'Identity, "object too large");
      end if;

      --  Change size from zero to non-zero. We still want a proper pointer
      --  for the zero case because pointers to zero length objects have to
      --  be distinct, but we can't just go ahead and allocate zero bytes,
      --  since some malloc's return zero for a zero argument.

      if Size = 0 then
         Actual_Size := 1;
      end if;

      if Parameters.No_Abort then
         Result := C_malloc32 (Actual_Size);
      else
         Abort_Defer.all;
         Result := C_malloc32 (Actual_Size);
         Abort_Undefer.all;
      end if;

      if Result = System.Null_Address then
         Raise_Exception (Storage_Error'Identity, "heap exhausted");
      end if;

      return Result;
   end Alloc32;

   ----------
   -- Free --
   ----------

   procedure Free (Ptr : System.Address) is
   begin
      if Parameters.No_Abort then
         c_free (Ptr);
      else
         Abort_Defer.all;
         c_free (Ptr);
         Abort_Undefer.all;
      end if;
   end Free;

   -------------
   -- Realloc --
   -------------

   function Realloc
     (Ptr  : System.Address;
      Size : size_t)
      return System.Address
   is
      Result      : System.Address;
      Actual_Size : constant size_t := Size;

   begin
      if Gnat_Heap_Size = 32 then
         return Realloc32 (Ptr, Size);
      end if;

      if Size = size_t'Last then
         Raise_Exception (Storage_Error'Identity, "object too large");
      end if;

      if Parameters.No_Abort then
         Result := c_realloc (Ptr, System.CRTL.size_t (Actual_Size));
      else
         Abort_Defer.all;
         Result := c_realloc (Ptr, System.CRTL.size_t (Actual_Size));
         Abort_Undefer.all;
      end if;

      if Result = System.Null_Address then
         Raise_Exception (Storage_Error'Identity, "heap exhausted");
      end if;

      return Result;
   end Realloc;

   ---------------
   -- Realloc32 --
   ---------------

   function Realloc32
     (Ptr  : System.Address;
      Size : size_t)
      return System.Address
   is
      Result      : System.Address;
      Actual_Size : constant size_t := Size;

   begin
      if Size = size_t'Last then
         Raise_Exception (Storage_Error'Identity, "object too large");
      end if;

      if Parameters.No_Abort then
         Result := C_realloc32 (Ptr, Actual_Size);
      else
         Abort_Defer.all;
         Result := C_realloc32 (Ptr, Actual_Size);
         Abort_Undefer.all;
      end if;

      if Result = System.Null_Address then
         Raise_Exception (Storage_Error'Identity, "heap exhausted");
      end if;

      return Result;
   end Realloc32;
end System.Memory;
