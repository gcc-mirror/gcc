------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                         S Y S T E M . M E M O R Y                        --
--                                                                          --
--                                 B o d y                                 --
--                                                                          --
--          Copyright (C) 2001-2003 Free Software Foundation, Inc.          --
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

--  This version provides ways to limit the amount of used memory for systems
--  that do not have OS support for that.

--  The amount of available memory available for dynamic allocation is limited
--  by setting the environment variable GNAT_MEMORY_LIMIT to the number of
--  kilobytes that can be used.
--
--  Windows is currently using this version.

with Ada.Exceptions;
with System.Soft_Links;

package body System.Memory is

   use Ada.Exceptions;
   use System.Soft_Links;

   function c_malloc (Size : size_t) return System.Address;
   pragma Import (C, c_malloc, "malloc");

   procedure c_free (Ptr : System.Address);
   pragma Import (C, c_free, "free");

   function c_realloc
     (Ptr : System.Address; Size : size_t) return System.Address;
   pragma Import (C, c_realloc, "realloc");

   function msize (Ptr : System.Address) return size_t;
   pragma Import (C, msize, "_msize");

   function getenv (Str : String) return System.Address;
   pragma Import (C, getenv);

   function atoi (Str : System.Address) return Integer;
   pragma Import (C, atoi);

   Available_Memory : size_t := 0;
   --  Amount of memory that is available for heap allocations.
   --  A value of 0 means that the amount is not yet initialized.

   Msize_Accuracy   : constant := 4096;
   --  Defines the amount of memory to add to requested allocation sizes,
   --  because malloc may return a bigger block than requested. As msize
   --  is used when by Free, it must be used on allocation as well. To
   --  prevent underflow of available_memory we need to use a reserve.

   procedure Check_Available_Memory (Size : size_t);
   --  This routine must be called while holding the task lock. When the
   --  memory limit is not yet initialized, it will be set to the value of
   --  the GNAT_MEMORY_LIMIT environment variable or to unlimited if that
   --  does not exist. If the size is larger than the amount of available
   --  memory, the task lock will be freed and a storage_error exception
   --  will be raised.

   -----------
   -- Alloc --
   -----------

   function Alloc (Size : size_t) return System.Address is
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

      Lock_Task.all;

      if Actual_Size + Msize_Accuracy >= Available_Memory then
         Check_Available_Memory (Size + Msize_Accuracy);
      end if;

      Result := c_malloc (Actual_Size);

      if Result /= System.Null_Address then
         Available_Memory := Available_Memory - msize (Result);
      end if;

      Unlock_Task.all;

      if Result = System.Null_Address then
         Raise_Exception (Storage_Error'Identity, "heap exhausted");
      end if;

      return Result;
   end Alloc;

   ----------------------------
   -- Check_Available_Memory --
   ----------------------------

   procedure Check_Available_Memory (Size : size_t) is
      Gnat_Memory_Limit : System.Address;

   begin
      if Available_Memory = 0 then

         --  The amount of available memory hasn't been initialized yet

         Gnat_Memory_Limit := getenv ("GNAT_MEMORY_LIMIT" & ASCII.NUL);

         if Gnat_Memory_Limit /= System.Null_Address then
            Available_Memory :=
              size_t (atoi (Gnat_Memory_Limit)) * 1024 + Msize_Accuracy;
         else
            Available_Memory := size_t'Last;
         end if;
      end if;

      if Size >= Available_Memory then

         --  There is a memory overflow

         Unlock_Task.all;
         Raise_Exception
           (Storage_Error'Identity, "heap memory limit exceeded");
      end if;
   end Check_Available_Memory;

   ----------
   -- Free --
   ----------

   procedure Free (Ptr : System.Address) is
   begin
      Lock_Task.all;

      if Ptr /= System.Null_Address then
         Available_Memory := Available_Memory + msize (Ptr);
      end if;

      c_free (Ptr);

      Unlock_Task.all;
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
      Old_Size    : size_t;

   begin
      if Size = size_t'Last then
         Raise_Exception (Storage_Error'Identity, "object too large");
      end if;

      Lock_Task.all;

      Old_Size := msize (Ptr);

      --  Conservative check - no need to try to be precise here

      if Size + Msize_Accuracy >= Available_Memory then
         Check_Available_Memory (Size + Msize_Accuracy);
      end if;

      Result := c_realloc (Ptr, Actual_Size);

      if Result /= System.Null_Address then
         Available_Memory := Available_Memory + Old_Size - msize (Result);
      end if;

      Unlock_Task.all;

      if Result = System.Null_Address then
         Raise_Exception (Storage_Error'Identity, "heap exhausted");
      end if;

      return Result;
   end Realloc;

end System.Memory;
