------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--    S Y S T E M . T A S K _ P R I M I T I V E S . O P E R A T I O N S .   --
--                              S P E C I F I C                             --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                             $Revision: 1.1 $
--                                                                          --
--            Copyright (C) 1991-2001, Florida State University             --
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

--  This is a POSIX version of this package where foreign threads are
--  recognized.
--  Currently, DEC Unix, SCO UnixWare, Solaris pthread, HPUX pthread and RTEMS
--  use this version.

with System.Soft_Links;
--  used to initialize TSD for a C thread, in function Self

separate (System.Task_Primitives.Operations)
package body Specific is

   ------------------
   --  Local Data  --
   ------------------

   --  The followings are logically constants, but need to be initialized
   --  at run time.

   ATCB_Key : aliased pthread_key_t;
   --  Key used to find the Ada Task_ID associated with a thread

   --  The following are used to allow the Self function to
   --  automatically generate ATCB's for C threads that happen to call
   --  Ada procedure, which in turn happen to call the Ada runtime system.

   type Fake_ATCB;
   type Fake_ATCB_Ptr is access Fake_ATCB;
   type Fake_ATCB is record
      Stack_Base : Interfaces.C.unsigned := 0;
      --  A value of zero indicates the node is not in use.
      Next : Fake_ATCB_Ptr;
      Real_ATCB : aliased Ada_Task_Control_Block (0);
   end record;

   Fake_ATCB_List : Fake_ATCB_Ptr;
   --  A linear linked list.
   --  The list is protected by All_Tasks_L;
   --  Nodes are added to this list from the front.
   --  Once a node is added to this list, it is never removed.

   Fake_Task_Elaborated : aliased Boolean := True;
   --  Used to identified fake tasks (i.e., non-Ada Threads).

   Next_Fake_ATCB : Fake_ATCB_Ptr;
   --  Used to allocate one Fake_ATCB in advance. See comment in New_Fake_ATCB

   -----------------------
   -- Local Subprograms --
   -----------------------

   ---------------------------------
   --  Support for New_Fake_ATCB  --
   ---------------------------------

   function New_Fake_ATCB return Task_ID;
   --  Allocate and Initialize a new ATCB. This code can safely be called from
   --  a foreign thread, as it doesn't access implicitly or explicitly
   --  "self" before having initialized the new ATCB.

   -------------------
   -- New_Fake_ATCB --
   -------------------

   function New_Fake_ATCB return Task_ID is
      Self_ID   : Task_ID;
      P, Q      : Fake_ATCB_Ptr;
      Succeeded : Boolean;
      Result    : Interfaces.C.int;

   begin
      --  This section is ticklish.
      --  We dare not call anything that might require an ATCB, until
      --  we have the new ATCB in place.

      Write_Lock (All_Tasks_L'Access);
      Q := null;
      P := Fake_ATCB_List;

      while P /= null loop
         if P.Stack_Base = 0 then
            Q := P;
         end if;

         P := P.Next;
      end loop;

      if Q = null then

         --  Create a new ATCB with zero entries.

         Self_ID := Next_Fake_ATCB.Real_ATCB'Access;
         Next_Fake_ATCB.Stack_Base := 1;
         Next_Fake_ATCB.Next := Fake_ATCB_List;
         Fake_ATCB_List := Next_Fake_ATCB;
         Next_Fake_ATCB := null;

      else
         --  Reuse an existing fake ATCB.

         Self_ID := Q.Real_ATCB'Access;
         Q.Stack_Base := 1;
      end if;

      --  Record this as the Task_ID for the current thread.

      Self_ID.Common.LL.Thread := pthread_self;
      Result := pthread_setspecific (ATCB_Key, To_Address (Self_ID));
      pragma Assert (Result = 0);

      --  Do the standard initializations

      System.Tasking.Initialize_ATCB
        (Self_ID, null, Null_Address, Null_Task, Fake_Task_Elaborated'Access,
         System.Priority'First, Task_Info.Unspecified_Task_Info, 0, Self_ID,
         Succeeded);
      pragma Assert (Succeeded);

      --  Finally, it is safe to use an allocator in this thread.

      if Next_Fake_ATCB = null then
         Next_Fake_ATCB := new Fake_ATCB;
      end if;

      Self_ID.Master_of_Task := 0;
      Self_ID.Master_Within := Self_ID.Master_of_Task + 1;

      for L in Self_ID.Entry_Calls'Range loop
         Self_ID.Entry_Calls (L).Self := Self_ID;
         Self_ID.Entry_Calls (L).Level := L;
      end loop;

      Self_ID.Common.State := Runnable;
      Self_ID.Awake_Count := 1;

      --  Since this is not an ordinary Ada task, we will start out undeferred

      Self_ID.Deferral_Level := 0;

      System.Soft_Links.Create_TSD (Self_ID.Common.Compiler_Data);

      --  ????
      --  The following call is commented out to avoid dependence on
      --  the System.Tasking.Initialization package.
      --  It seems that if we want Ada.Task_Attributes to work correctly
      --  for C threads we will need to raise the visibility of this soft
      --  link to System.Soft_Links.
      --  We are putting that off until this new functionality is otherwise
      --  stable.
      --  System.Tasking.Initialization.Initialize_Attributes_Link.all (T);

      for J in Known_Tasks'Range loop
         if Known_Tasks (J) = null then
            Known_Tasks (J) := Self_ID;
            Self_ID.Known_Tasks_Index := J;
            exit;
         end if;
      end loop;

      --  Must not unlock until Next_ATCB is again allocated.

      Unlock (All_Tasks_L'Access);
      return Self_ID;
   end New_Fake_ATCB;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Environment_Task : Task_ID) is
      Result : Interfaces.C.int;

   begin
      Result := pthread_key_create (ATCB_Key'Access, null);
      pragma Assert (Result = 0);
      Result := pthread_setspecific (ATCB_Key, To_Address (Environment_Task));
      pragma Assert (Result = 0);

      --  Create a free ATCB for use on the Fake_ATCB_List.

      Next_Fake_ATCB := new Fake_ATCB;
   end Initialize;

   ---------
   -- Set --
   ---------

   procedure Set (Self_Id : Task_ID) is
      Result  : Interfaces.C.int;

   begin
      Result := pthread_setspecific (ATCB_Key, To_Address (Self_Id));
      pragma Assert (Result = 0);
   end Set;

   ----------
   -- Self --
   ----------

   --  To make Ada tasks and C threads interoperate better, we have
   --  added some functionality to Self.  Suppose a C main program
   --  (with threads) calls an Ada procedure and the Ada procedure
   --  calls the tasking runtime system.  Eventually, a call will be
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
      Result : System.Address;

   begin
      Result := pthread_getspecific (ATCB_Key);

      --  If the key value is Null, then it is a non-Ada task.

      if Result = System.Null_Address then
         return New_Fake_ATCB;
      end if;

      return To_Task_ID (Result);
   end Self;

end Specific;
