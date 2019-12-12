------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                         S Y S T E M . M E M O R Y                        --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 2001-2019, Free Software Foundation, Inc.         --
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

--  This package provides the low level memory allocation/deallocation
--  mechanisms used by GNAT.

--  To provide an alternate implementation, simply recompile the modified
--  body of this package with gnatmake -u -a -g s-memory.adb and make sure
--  that the ali and object files for this unit are found in the object
--  search path.

--  This unit may be used directly from an application program by providing
--  an appropriate WITH, and the interface can be expected to remain stable.

pragma Compiler_Unit_Warning;

package System.Memory is
   pragma Elaborate_Body;

   type size_t is mod 2 ** Standard'Address_Size;
   --  Note: the reason we redefine this here instead of using the
   --  definition in Interfaces.C is that we do not want to drag in
   --  all of Interfaces.C just because System.Memory is used.

   function Alloc (Size : size_t) return System.Address;
   --  This is the low level allocation routine. Given a size in storage
   --  units, it returns the address of a maximally aligned block of
   --  memory. The implementation of this routine is guaranteed to be
   --  task safe, and also aborts are deferred if necessary.
   --
   --  If Size is set to size_t'Last on entry, then a Storage_Error
   --  exception is raised with a message "object too large".
   --
   --  If Size is set to zero on entry, then a minimal (but non-zero)
   --  size block is allocated.
   --
   --  Note: this is roughly equivalent to the standard C malloc call
   --  with the additional semantics as described above.

   procedure Free (Ptr : System.Address);
   --  This is the low level free routine. It frees a block previously
   --  allocated with a call to Alloc. As in the case of Alloc, this
   --  call is guaranteed task safe, and aborts are deferred.
   --
   --  Note: this is roughly equivalent to the standard C free call
   --  with the additional semantics as described above.

   function Realloc
     (Ptr  : System.Address;
      Size : size_t) return System.Address;
   --  This is the low level reallocation routine. It takes an existing
   --  block address returned by a previous call to Alloc or Realloc,
   --  and reallocates the block. The size can either be increased or
   --  decreased. If possible the reallocation is done in place, so that
   --  the returned result is the same as the value of Ptr on entry.
   --  However, it may be necessary to relocate the block to another
   --  address, in which case the information is copied to the new
   --  block, and the old block is freed. The implementation of this
   --  routine is guaranteed to be task safe, and also aborts are
   --  deferred as necessary.
   --
   --  If Size is set to size_t'Last on entry, then a Storage_Error
   --  exception is raised with a message "object too large".
   --
   --  If Size is set to zero on entry, then a minimal (but non-zero)
   --  size block is allocated.
   --
   --  Note: this is roughly equivalent to the standard C realloc call
   --  with the additional semantics as described above.

private

   --  The following names are used from the generated compiler code

   pragma Export (C, Alloc,   "__gnat_malloc");
   pragma Export (C, Free,    "__gnat_free");
   pragma Export (C, Realloc, "__gnat_realloc");

end System.Memory;
