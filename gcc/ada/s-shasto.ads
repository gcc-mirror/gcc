------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                 S Y S T E M . S H A R E D _ S T O R A G E                --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1998-2003 Free Software Foundation, Inc.          --
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

--  This package manages the shared/persistant storage required for
--  full implementation of variables in Shared_Passive packages, more
--  precisely variables whose enclosing dynamic scope is a shared
--  passive package. This implementation is specific to GNAT and GLADE
--  provides a more general implementation not dedicated to file
--  storage.

--  This unit (and shared passive partitions) are supported on all
--  GNAT implementations except on OpenVMS (where problems arise from
--  trying to share files, and with version numbers of files)

--  --------------------------
--  -- Shared Storage Model --
--  --------------------------

--  The basic model used is that each partition that references the
--  Shared_Passive package has a local copy of the package data that
--  is initialized in accordance with the declarations of the package
--  in the normal manner. The routines in System.Shared_Storage are
--  then used to ensure that the values in these separate copies are
--  properly synchronized with the state of the overall system.

--  In the GNAT implementation, this synchronization is ensured by
--  maintaining a set of files, in a designated directory. The
--  directory is designated by setting the environment variable
--  SHARED_MEMORY_DIRECTORY. This variable must be set for all
--  partitions. If the environment variable is not defined, then the
--  current directory is used.

--  There is one storage for each variable. The name is the fully
--  qualified name of the variable with all letters forced to lower
--  case. For example, the variable Var in the shared passive package
--  Pkg results in the storage name pkg.var.

--  If the storage does not exist, it indicates that no partition has
--  assigned a new value, so that the initial value is the correct
--  one. This is the critical component of the model. It means that
--  there is no system-wide synchronization required for initializing
--  the package, since the shared storages need not (and do not)
--  reflect the initial state. There is therefore no issue of
--  synchronizing initialization and read/write access.

--  -----------------------
--  -- Read/Write Access --
--  -----------------------

--  The approach is as follows:

--    For each shared variable, var, an access routine varR is created whose
--    body has the following form (this example is for Pkg.Var):

--      procedure varR is
--         S : Ada.Streams.Stream_IO.Stream_Access;
--      begin
--         S := Shared_Var_ROpen ("pkg.var");
--         if S /= null then
--            typ'Read (S);
--            Shared_Var_Close (S);
--         end if;
--      end varR;

--    The routine Shared_Var_ROpen in package System.Shared_Storage
--    either returns null if the storage does not exist, or otherwise a
--    Stream_Access value that references the corresponding shared
--    storage, ready to read the current value.

--    Each reference to the shared variable, var, is preceded by a
--    call to the corresponding varR procedure, which either leaves the
--    initial value unchanged if the storage does not exist, or reads
--    the current value from the shared storage.

--    In addition, for each shared variable, var, an assignment routine
--    is created whose body has the following form (again for Pkg.Var)

--      procedure VarA is
--         S : Ada.Streams.Stream_IO.Stream_Access;
--      begin
--         S := Shared_Var_WOpen ("pkg.var");
--         typ'Write (S, var);
--         Shared_Var_Close (S);
--      end VarA;

--    The routine Shared_Var_WOpen in package System.Shared_Storage
--    returns a Stream_Access value that references the corresponding
--    shared storage, ready to write the new value.

--    Each assignment to the shared variable, var, is followed by a call
--    to the corresponding varA procedure, which writes the new value to
--    the shared storage.

--    Note that there is no general synchronization for these storage
--    read and write operations, since it is assumed that a correctly
--    operating programs will provide appropriate synchronization. In
--    particular, variables can be protected using protected types with
--    no entries.

--    The routine Shared_Var_Close is called to indicate the end of a
--    read/write operations. This can be useful even in the context of
--    the GNAT implementation. For instance, when a read operation and a
--    write operation occur at the same time on the same partition, as
--    the same stream is used simultaneously, both operations can
--    terminate abruptly by raising exception Mode_Error because the
--    stream has been opened in read mode and then in write mode and at
--    least used by the read opartion. To avoid this unexpected
--    behaviour, we introduce a synchronization at the partition level.

--  Note: a special circuit allows the use of stream attributes Read and
--  Write for limited types (using the corresponding attribute for the
--  full type), but there are limitations on the data that can be placed
--  in shared passive partitions. See sem_smem.ads/adb for details.

--  ----------------------------------------------------------------
--  -- Handling of Protected Objects in Shared Passive Partitions --
--  ----------------------------------------------------------------

--  In the context of GNAT, during the execution of a protected
--  subprogram call, access is locked out using a locking mechanism
--  per protected object, as provided by the GNAT.Lock_Files
--  capability in the specific case of GNAT. This package contains the
--  lock and unlock calls, and the expander generates a call to the
--  lock routine before the protected call and a call to the unlock
--  routine after the protected call.

--  Within the code of the protected subprogram, the access to the
--  protected object itself uses the local copy, without any special
--  synchronization.  Since global access is locked out, no other task
--  or partition can attempt to read or write this data as long as the
--  lock is held.

--  The data in the local copy does however need synchronizing with
--  the global values in the shared storage. This is achieved as
--  follows:

--    The protected object generates a read and assignment routine as
--    described for other shared passive variables. The code for the
--    'Read and 'Write attributes (not normally allowed, but allowed
--    in this special case) simply reads or writes the values of the
--    components in the protected record.

--    The lock call is followed by a call to the shared read routine to
--    synchronize the local copy to contain the proper global value.

--    The unlock call in the procedure case only is preceded by a call
--    to the shared assign routine to synchronize the global shared
--    storages with the (possibly modified) local copy.

--    These calls to the read and assign routines, as well as the lock
--    and unlock routines, are inserted by the expander (see exp_smem.adb).

with Ada.Streams.Stream_IO;

package System.Shared_Storage is

   package SIO renames Ada.Streams.Stream_IO;

   function Shared_Var_ROpen (Var : String) return SIO.Stream_Access;
   --  As described above, this routine returns null if the
   --  corresponding shared storage does not exist, and otherwise, if
   --  the storage does exist, a Stream_Access value that references
   --  the shared storage, ready to read the current value.

   function Shared_Var_WOpen (Var : String) return SIO.Stream_Access;
   --  As described above, this routine returns a Stream_Access value
   --  that references the shared storage, ready to write the new
   --  value. The storage is created by this call if it does not
   --  already exist.

   procedure Shared_Var_Close (Var : in SIO.Stream_Access);
   --  This routine signals the end of a read/assign operation. It can
   --  be useful to embrace a read/write operation between a call to
   --  open and a call to close which protect the whole operation.
   --  Otherwise, two simultaneous operations can result in the
   --  raising of exception Data_Error by setting the access mode of
   --  the variable in an incorrect mode.

   procedure Shared_Var_Lock (Var : String);
   --  This procedure claims the shared storage lock. It is used for
   --  protected types in shared passive packages. A call to this
   --  locking routine is generated as the first operation in the code
   --  for the body of a protected subprogram, and it busy waits if
   --  the lock is busy.

   procedure Shared_Var_Unlock (Var : String);
   --  This procedure releases the shared storage lock obtaind by a
   --  prior call to the Shared_Mem_Lock procedure, and is to be
   --  generated as the last operation in the body of a protected
   --  subprogram.

end System.Shared_Storage;
