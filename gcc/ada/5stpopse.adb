------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                   SYSTEM.TASK_PRIMITIVES.OPERATIONS.SELF                 --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                             $Revision: 1.1 $
--                                                                          --
--            Copyright (C) 1991-1998, Florida State University             --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNARL; see file COPYING.  If not, write --
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
-- GNARL was developed by the GNARL team at Florida State University. It is --
-- now maintained by Ada Core Technologies Inc. in cooperation with Florida --
-- State University (http://www.gnat.com).                                  --
--                                                                          --
------------------------------------------------------------------------------

--  This is a Solaris Sparc (native) version of this package.

with System.Machine_Code;
--  used for Asm

separate (System.Task_Primitives.Operations)

----------
-- Self --
----------

--  For Solaris version of RTS, we use a short cut to get the self
--  information faster:

--  We have noticed that on Sparc Solaris, the register g7 always
--  contains the address near the frame pointer (fp) of the active
--  thread (fixed offset). This means, if we declare a variable near
--  the top of the stack for each threads (in our case in the task wrapper)
--  and let the variable hold the Task_ID information, we can get the
--  value without going through the thr_getspecific kernel call.
--
--  There are two things to take care in this trick.
--
--  1) We need to calculate the offset between the g7 value and the
--     local variable address.
--     Possible Solutions :
--        a) Use gdb to figure out the offset.
--        b) Figure it out during the elaboration of RTS by, say,
--           creating a dummy task.
--     We used solution a) mainly because it is more efficient and keeps
--     the RTS from being cluttered with stuff that we won't be used
--     for all environments (i.e., we would have to at least introduce
--     new interfaces).
--
--     On Sparc Solaris the offset was #10#108# (= #16#6b#) with gcc 2.7.2.
--     With gcc 2.8.0, the offset is #10#116# (= #16#74#).
--
--  2) We can not use the same offset business for the main thread
--     because we do not use a wrapper for the main thread.
--     Previousely, we used the difference between g7 and fp to determine
--     wether a task was the main task or not. But this was obviousely
--     wrong since it worked only for tasks that use small amount of
--     stack.
--     So, we now take advantage of the code that recognizes foreign
--     threads (see below) for the main task.
--
--  NOTE: What we are doing here is ABSOLUTELY for Solaris 2.4, 2.5 and 2.6
--        on Sun.

--        We need to make sure this is OK when we move to other versions
--        of the same OS.

--        We always can go back to the old way of doing this and we include
--        the code which use thr_getspecifics. Also, look for %%%%%
--        in comments for other necessary modifications.

--        This code happens to work with Solaris 2.5.1 too, but with gcc
--        2.8.0, this offset is different.

--        ??? Try to rethink the approach here to get a more flexible
--        solution at run time ?

--        One other solution (close to 1-b) would be to add some scanning
--        routine in Enter_Task to compute the offset since now we have
--        a magic number at the beginning of the task code.

--  function Self return Task_ID is
--     Temp   : aliased System.Address;
--     Result : Interfaces.C.int;
--
--  begin
--     Result := thr_getspecific (ATCB_Key, Temp'Unchecked_Access);
--     pragma Assert (Result = 0);
--     return To_Task_ID (Temp);
--  end Self;

--  To make Ada tasks and C threads interoperate better, we have
--  added some functionality to Self.  Suppose a C main program
--  (with threads) calls an Ada procedure and the Ada procedure
--  calls the tasking run-time system.  Eventually, a call will be
--  made to self.  Since the call is not coming from an Ada task,
--  there will be no corresponding ATCB.

--  (The entire Ada run-time system may not have been elaborated,
--  either, but that is a different problem, that we will need to
--  solve another way.)

--  What we do in Self is to catch references that do not come
--  from recognized Ada tasks, and create an ATCB for the calling
--  thread.

--  The new ATCB will be "detached" from the normal Ada task
--  master hierarchy, much like the existing implicitly created
--  signal-server tasks.

--  We will also use such points to poll for disappearance of the
--  threads associated with any implicit ATCBs that we created
--  earlier, and take the opportunity to recover them.

--  A nasty problem here is the limitations of the compilation
--  order dependency, and in particular the GNARL/GNULLI layering.
--  To initialize an ATCB we need to assume System.Tasking has
--  been elaborated.

function Self return Task_ID is
   X      : Ptr;
   Result : Interfaces.C.int;

   function Get_G7 return Interfaces.C.unsigned;
   pragma Inline (Get_G7);

   use System.Machine_Code;

   ------------
   -- Get_G7 --
   ------------

   function Get_G7 return Interfaces.C.unsigned is
      Result : Interfaces.C.unsigned;

   begin
      Asm ("mov %%g7,%0", Interfaces.C.unsigned'Asm_Output ("=r", Result));
      return Result;
   end Get_G7;

--  Start of processing for Self

begin
   if To_Iptr (Get_G7 - 120).all /=
     Interfaces.C.unsigned (ATCB_Magic_Code)
   then
      --  Check whether this is a thread we have seen before (e.g the
      --  main task).
      --  120 = 116 + Magic_Type'Size/System.Storage_Unit

      declare
         Unknown_Task : aliased System.Address;

      begin
         Result :=
           thr_getspecific (ATCB_Key, Unknown_Task'Unchecked_Access);

         pragma Assert (Result = 0);

         if Unknown_Task = System.Null_Address then

            --  We are seeing this thread for the first time.

            return New_Fake_ATCB (Get_G7);

         else
            return To_Task_ID (Unknown_Task);
         end if;
      end;
   end if;

   X := To_Ptr (Get_G7 - 116);
   return X.all;

end Self;
