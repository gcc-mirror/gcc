------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--      S Y S T E M . T A S K I N G . P R O T E C T E D _ O B J E C T S     --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--             Copyright (C) 1991-1994, Florida State University            --
--             Copyright (C) 1995-2003, Ada Core Technologies               --
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

with System.Parameters;
--  used for Runtime_Traces

with System.Traces;
--  used for Send_Trace_Info

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
   end Initialize_Protection;

   ----------
   -- Lock --
   ----------

   procedure Lock (Object : Protection_Access) is
      Ceiling_Violation : Boolean;
   begin
      --  The lock is made without defering abortion.

      --  Therefore the abortion has to be deferred before calling this
      --  routine. This means that the compiler has to generate a Defer_Abort
      --  call before the call to Lock.

      --  The caller is responsible for undeferring abortion, and compiler
      --  generated calls must be protected with cleanup handlers to ensure
      --  that abortion is undeferred in all cases.

      Write_Lock (Object.L'Access, Ceiling_Violation);

      if Parameters.Runtime_Traces then
         Send_Trace_Info (PO_Lock);
      end if;

      if Ceiling_Violation then
         raise Program_Error;
      end if;
   end Lock;

   --------------------
   -- Lock_Read_Only --
   --------------------

   procedure Lock_Read_Only (Object : Protection_Access) is
      Ceiling_Violation : Boolean;
   begin
      Read_Lock (Object.L'Access, Ceiling_Violation);

      if Parameters.Runtime_Traces then
         Send_Trace_Info (PO_Lock);
      end if;

      if Ceiling_Violation then
         raise Program_Error;
      end if;
   end Lock_Read_Only;

   ------------
   -- Unlock --
   ------------

   procedure Unlock (Object : Protection_Access) is
   begin
      Unlock (Object.L'Access);

      if Parameters.Runtime_Traces then
         Send_Trace_Info (PO_Unlock);
      end if;
   end Unlock;

end System.Tasking.Protected_Objects;
