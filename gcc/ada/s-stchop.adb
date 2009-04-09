------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--     S Y S T E M . S T A C K _ C H E C K I N G . O P E R A T I O N S      --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--          Copyright (C) 1999-2009, Free Software Foundation, Inc.         --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------

--  This is the general implementation of this package. There is a VxWorks
--  specific version of this package (s-stchop-vxworks.adb). This file should
--  be kept synchronized with it.

pragma Restrictions (No_Elaboration_Code);
--  We want to guarantee the absence of elaboration code because the
--  binder does not handle references to this package.

with System.Storage_Elements; use System.Storage_Elements;
with System.Parameters; use System.Parameters;
with System.Soft_Links;
with System.CRTL;

package body System.Stack_Checking.Operations is

   Kilobyte : constant := 1024;

   function Set_Stack_Info
     (Stack : not null access Stack_Access) return Stack_Access;

   --  The function Set_Stack_Info is the actual function that updates
   --  the cache containing a pointer to the Stack_Info. It may also
   --  be used for detecting asynchronous abort in combination with
   --  Invalidate_Self_Cache.

   --  Set_Stack_Info should do the following things in order:
   --     1) Get the Stack_Access value for the current task
   --     2) Set Stack.all to the value obtained in 1)
   --     3) Optionally Poll to check for asynchronous abort

   --  This order is important because if at any time a write to
   --  the stack cache is pending, that write should be followed
   --  by a Poll to prevent loosing signals.

   --  Note: This function must be compiled with Polling turned off

   --  Note: on systems like VxWorks and OS/2 with real thread-local storage,
   --        Set_Stack_Info should return an access value for such local
   --        storage. In those cases the cache will always be up-to-date.

   --  The following constants should be imported from some system-specific
   --  constants package. The constants must be static for performance reasons.

   ----------------------------
   -- Invalidate_Stack_Cache --
   ----------------------------

   procedure Invalidate_Stack_Cache (Any_Stack : Stack_Access) is
      pragma Warnings (Off, Any_Stack);
   begin
      Cache := Null_Stack;
   end Invalidate_Stack_Cache;

   -----------------------------
   -- Notify_Stack_Attributes --
   -----------------------------

   procedure Notify_Stack_Attributes
     (Initial_SP : System.Address;
      Size       : System.Storage_Elements.Storage_Offset)
   is
      My_Stack : constant Stack_Access := Soft_Links.Get_Stack_Info.all;

      --  We piggyback on the 'Limit' field to store what will be used as the
      --  'Base' and leave the 'Size' alone to not interfere with the logic in
      --  Set_Stack_Info below.

      pragma Unreferenced (Size);

   begin
      My_Stack.Limit := Initial_SP;
   end Notify_Stack_Attributes;

   --------------------
   -- Set_Stack_Info --
   --------------------

   function Set_Stack_Info
     (Stack : not null access Stack_Access) return Stack_Access
   is
      type Frame_Mark is null record;
      Frame_Location : Frame_Mark;
      Frame_Address  : constant Address := Frame_Location'Address;

      My_Stack    : Stack_Access;
      Limit_Chars : System.Address;
      Limit       : Integer;

   begin
      --  The order of steps 1 .. 3 is important, see specification

      --  1) Get the Stack_Access value for the current task

      My_Stack := Soft_Links.Get_Stack_Info.all;

      if My_Stack.Base = Null_Address then

         --  First invocation, initialize based on the assumption that
         --  there are Environment_Stack_Size bytes available beyond
         --  the current frame address.

         if My_Stack.Size = 0 then
            My_Stack.Size := Storage_Offset (Default_Env_Stack_Size);

            --  When the environment variable GNAT_STACK_LIMIT is set,
            --  set Environment_Stack_Size to that number of kB.

            Limit_Chars := System.CRTL.getenv ("GNAT_STACK_LIMIT" & ASCII.NUL);

            if Limit_Chars /= Null_Address then
               Limit := System.CRTL.atoi (Limit_Chars);

               if Limit >= 0 then
                  My_Stack.Size := Storage_Offset (Limit) * Kilobyte;
               end if;
            end if;
         end if;

         --  If a stack base address has been registered, honor it.
         --  Fallback to the address of a local object otherwise.

         if My_Stack.Limit /= System.Null_Address then
            My_Stack.Base := My_Stack.Limit;
         else
            My_Stack.Base := Frame_Address;
         end if;

         if Stack_Grows_Down then

            --  Prevent wrap-around on too big stack sizes

            My_Stack.Limit := My_Stack.Base - My_Stack.Size;

            if My_Stack.Limit > My_Stack.Base then
               My_Stack.Limit := Address'First;
            end if;

         else
            My_Stack.Limit := My_Stack.Base + My_Stack.Size;

            --  Prevent wrap-around on too big stack sizes

            if My_Stack.Limit < My_Stack.Base then
               My_Stack.Limit := Address'Last;
            end if;
         end if;
      end if;

      --  2) Set Stack.all to the value obtained in 1)

      Stack.all := My_Stack;

      --  3) Optionally Poll to check for asynchronous abort

      if Soft_Links.Check_Abort_Status.all /= 0 then
         raise Standard'Abort_Signal;
      end if;

      return My_Stack; -- Never trust the cached value, but return local copy!
   end Set_Stack_Info;

   -----------------
   -- Stack_Check --
   -----------------

   function Stack_Check
     (Stack_Address : System.Address) return Stack_Access
   is
      type Frame_Marker is null record;
      Marker        : Frame_Marker;
      Cached_Stack  : constant Stack_Access := Cache;
      Frame_Address : constant System.Address := Marker'Address;

   begin
      --  The parameter may have wrapped around in System.Address arithmetics.
      --  In that case, we have no other choices than raising the exception.

      if (Stack_Grows_Down and then
            Stack_Address > Frame_Address)
        or else
         (not Stack_Grows_Down and then
            Stack_Address < Frame_Address)
      then
         raise Storage_Error with "stack overflow detected";
      end if;

      --  This function first does a "cheap" check which is correct
      --  if it succeeds. In case of failure, the full check is done.
      --  Ideally the cheap check should be done in an optimized manner,
      --  or be inlined.

      if (Stack_Grows_Down and then
            (Frame_Address <= Cached_Stack.Base
               and
             Stack_Address > Cached_Stack.Limit))
        or else
         (not Stack_Grows_Down and then
            (Frame_Address >= Cached_Stack.Base
               and
             Stack_Address < Cached_Stack.Limit))
      then
         --  Cached_Stack is valid as it passed the stack check
         return Cached_Stack;
      end if;

      Full_Check :
      declare
         My_Stack : constant Stack_Access := Set_Stack_Info (Cache'Access);
         --  At this point Stack.all might already be invalid, so
         --  it is essential to use our local copy of Stack!

      begin
         if (Stack_Grows_Down and then
               (not (Frame_Address <= My_Stack.Base)))
           or else
            (not Stack_Grows_Down and then
               (not (Frame_Address >= My_Stack.Base)))
         then
            --  The returned Base is lower than the stored one,
            --  so assume that the original one wasn't right and use the
            --  current Frame_Address as new one. This allows initializing
            --  Base with the Frame_Address as approximation.
            --  During initialization the Frame_Address will be close to
            --  the stack base anyway: the difference should be compensated
            --  for in the stack reserve.

            My_Stack.Base := Frame_Address;
         end if;

         if (Stack_Grows_Down and then
                  Stack_Address < My_Stack.Limit)
           or else
            (not Stack_Grows_Down and then
                  Stack_Address > My_Stack.Limit)
         then
            raise Storage_Error with "stack overflow detected";
         end if;

         return My_Stack;
      end Full_Check;
   end Stack_Check;

   ------------------------
   -- Update_Stack_Cache --
   ------------------------

   procedure Update_Stack_Cache (Stack : Stack_Access) is
   begin
      if not Multi_Processor then
         Cache := Stack;
      end if;
   end Update_Stack_Cache;

end System.Stack_Checking.Operations;
