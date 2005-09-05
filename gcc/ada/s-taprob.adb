------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--      S Y S T E M . T A S K I N G . P R O T E C T E D _ O B J E C T S     --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--             Copyright (C) 1991-1994, Florida State University            --
--                     Copyright (C) 1995-2005, AdaCore                     --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNARL; see file COPYING.  If not, write --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------

pragma Polling (Off);
--  Turn off polling, we do not want ATC polling to take place during
--  tasking operations. It causes infinite loops and other problems.

with System.Task_Primitives.Operations;
--  used for Write_Lock
--           Unlock
--           Self

with System.Parameters;
--  used for Runtime_Traces

with System.Traces;
--  used for Send_Trace_Info

with System.Soft_Links.Tasking;
--  Used for Init_Tasking_Soft_Links

package body System.Tasking.Protected_Objects is

   use System.Task_Primitives.Operations;
   use System.Traces;

   -------------------------
   -- Finalize_Protection --
   -------------------------

   procedure Finalize_Protection (Object : in out Protection) is
   begin
      Finalize_Lock (Object.L'Unrestricted_Access);
   end Finalize_Protection;

   ---------------------------
   -- Initialize_Protection --
   ---------------------------

   procedure Initialize_Protection
     (Object           : Protection_Access;
      Ceiling_Priority : Integer)
   is
      Init_Priority : Integer := Ceiling_Priority;

   begin
      if Init_Priority = Unspecified_Priority then
         Init_Priority  := System.Priority'Last;
      end if;

      Initialize_Lock (Init_Priority, Object.L'Access);
      Object.Ceiling := System.Any_Priority (Init_Priority);
      Object.Owner := Null_Task;
   end Initialize_Protection;

   ----------
   -- Lock --
   ----------

   procedure Lock (Object : Protection_Access) is
      Ceiling_Violation : Boolean;

   begin
      --  The lock is made without defering abort

      --  Therefore the abort has to be deferred before calling this routine.
      --  This means that the compiler has to generate a Defer_Abort call
      --  before the call to Lock.

      --  The caller is responsible for undeferring abort, and compiler
      --  generated calls must be protected with cleanup handlers to ensure
      --  that abort is undeferred in all cases.

      --  If pragma Detect_Blocking is active then, as described in the ARM
      --  9.5.1, par. 15, we must check whether this is an external call on a
      --  protected subprogram with the same target object as that of the
      --  protected action that is currently in progress (i.e., if the caller
      --  is already the protected object's owner). If this is the case hence
      --  Program_Error must be raised.

      if Detect_Blocking and then Object.Owner = Self then
         raise Program_Error;
      end if;

      Write_Lock (Object.L'Access, Ceiling_Violation);

      if Parameters.Runtime_Traces then
         Send_Trace_Info (PO_Lock);
      end if;

      if Ceiling_Violation then
         raise Program_Error;
      end if;

      --  We are entering in a protected action, so that we increase the
      --  protected object nesting level (if pragma Detect_Blocking is
      --  active), and update the protected object's owner.

      if Detect_Blocking then
         declare
            Self_Id : constant Task_Id := Self;
         begin
            --  Update the protected object's owner

            Object.Owner := Self_Id;

            --  Increase protected object nesting level

            Self_Id.Common.Protected_Action_Nesting :=
              Self_Id.Common.Protected_Action_Nesting + 1;
         end;
      end if;
   end Lock;

   --------------------
   -- Lock_Read_Only --
   --------------------

   procedure Lock_Read_Only (Object : Protection_Access) is
      Ceiling_Violation : Boolean;

   begin
      --  If pragma Detect_Blocking is active then, as described in the ARM
      --  9.5.1, par. 15, we must check whether this is an external call on
      --  protected subprogram with the same target object as that of the
      --  protected action that is currently in progress (i.e., if the caller
      --  is already the protected object's owner). If this is the case hence
      --  Program_Error must be raised.
      --
      --  Note that in this case (getting read access), several tasks may have
      --  read ownership of the protected object, so that this method of
      --  storing the (single) protected object's owner does not work reliably
      --  for read locks. However, this is the approach taken for two major
      --  reasosn: first, this function is not currently being used (it is
      --  provided for possible future use), and second, it largely simplifies
      --  the implementation.

      if Detect_Blocking and then Object.Owner = Self then
         raise Program_Error;
      end if;

      Read_Lock (Object.L'Access, Ceiling_Violation);

      if Parameters.Runtime_Traces then
         Send_Trace_Info (PO_Lock);
      end if;

      if Ceiling_Violation then
         raise Program_Error;
      end if;

      --  We are entering in a protected action, so we increase the protected
      --  object nesting level (if pragma Detect_Blocking is active).

      if Detect_Blocking then
         declare
            Self_Id : constant Task_Id := Self;
         begin
            --  Update the protected object's owner

            Object.Owner := Self_Id;

            --  Increase protected object nesting level

            Self_Id.Common.Protected_Action_Nesting :=
              Self_Id.Common.Protected_Action_Nesting + 1;
         end;
      end if;
   end Lock_Read_Only;

   ------------
   -- Unlock --
   ------------

   procedure Unlock (Object : Protection_Access) is
   begin
      --  We are exiting from a protected action, so that we decrease the
      --  protected object nesting level (if pragma Detect_Blocking is
      --  active), and remove ownership of the protected object.

      if Detect_Blocking then
         declare
            Self_Id : constant Task_Id := Self;

         begin
            --  Calls to this procedure can only take place when being within
            --  a protected action and when the caller is the protected
            --  object's owner.

            pragma Assert (Self_Id.Common.Protected_Action_Nesting > 0
                             and then Object.Owner = Self_Id);

            --  Remove ownership of the protected object

            Object.Owner := Null_Task;

            --  We are exiting from a protected action, so we decrease the
            --  protected object nesting level.

            Self_Id.Common.Protected_Action_Nesting :=
              Self_Id.Common.Protected_Action_Nesting - 1;
         end;
      end if;

      Unlock (Object.L'Access);

      if Parameters.Runtime_Traces then
         Send_Trace_Info (PO_Unlock);
      end if;
   end Unlock;

begin
   --  Ensure that tasking is initialized, as well as tasking soft links
   --  when using protected objects.

   Tasking.Initialize;
   System.Soft_Links.Tasking.Init_Tasking_Soft_Links;
end System.Tasking.Protected_Objects;
