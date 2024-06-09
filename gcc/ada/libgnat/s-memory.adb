------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                         S Y S T E M . M E M O R Y                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2001-2024, Free Software Foundation, Inc.         --
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

--  This is the default implementation of this package

--  This implementation assumes that the underlying malloc/free/realloc
--  implementation is thread safe, and thus, no additional lock is required.
--  Note that when using sjlj exception handling, we still need to defer abort
--  because an asynchronous signal (as used for implementing asynchronous abort
--  of task on sjlj runtimes) cannot safely be handled while malloc is
--  executing.

with System.CRTL;
with System.Parameters;
with System.Soft_Links;

package body System.Memory is

   use System.Soft_Links;

   function c_malloc (Size : System.CRTL.size_t) return System.Address
     renames System.CRTL.malloc;

   procedure c_free (Ptr : System.Address)
     renames System.CRTL.free;

   function c_realloc
     (Ptr : System.Address; Size : System.CRTL.size_t) return System.Address
     renames System.CRTL.realloc;

   -----------
   -- Alloc --
   -----------

   function Alloc (Size : size_t) return System.Address is
      Result : System.Address;
   begin
      --  A previous version moved the check for size_t'Last below, into the
      --  "if Result = System.Null_Address...". So malloc(size_t'Last) should
      --  return Null_Address, and then we can check for that special value.
      --  However, that doesn't work on VxWorks, because malloc(size_t'Last)
      --  prints an unwanted warning message before returning Null_Address.
      --  Note that the branch is correctly predicted on modern hardware, so
      --  there is negligible overhead.

      if Size = size_t'Last then
         raise Storage_Error with "object too large";
      end if;

      if ZCX_By_Default or else Parameters.No_Abort then
         Result := c_malloc (System.CRTL.size_t (Size));
      else
         Abort_Defer.all;
         Result := c_malloc (System.CRTL.size_t (Size));
         Abort_Undefer.all;
      end if;

      if Result = System.Null_Address then

         --  If Size = 0, we can't allocate 0 bytes, because then two different
         --  allocators, one of which has Size = 0, could return pointers that
         --  compare equal, which is wrong. (Nonnull pointers compare equal if
         --  and only if they designate the same object, and two different
         --  allocators allocate two different objects).

         --  malloc(0) is defined to allocate a non-zero-sized object (in which
         --  case we won't get here, and all is well) or NULL, in which case we
         --  get here. We also get here in case of error. So check for the
         --  zero-size case, and allocate 1 byte. Otherwise, raise
         --  Storage_Error.

         --  We check for zero size here, rather than at the start, for
         --  efficiency.

         if Size = 0 then
            return Alloc (1);
         end if;

         raise Storage_Error with "heap exhausted";
      end if;

      return Result;
   end Alloc;

   ----------
   -- Free --
   ----------

   procedure Free (Ptr : System.Address) is
   begin
      if ZCX_By_Default or else Parameters.No_Abort then
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
      Result : System.Address;
   begin
      if Size = size_t'Last then
         raise Storage_Error with "object too large";
      end if;

      if ZCX_By_Default or else Parameters.No_Abort then
         Result := c_realloc (Ptr, System.CRTL.size_t (Size));
      else
         Abort_Defer.all;
         Result := c_realloc (Ptr, System.CRTL.size_t (Size));
         Abort_Undefer.all;
      end if;

      if Result = System.Null_Address then
         raise Storage_Error with "heap exhausted";
      end if;

      return Result;
   end Realloc;

end System.Memory;
