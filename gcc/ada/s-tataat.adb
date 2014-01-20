------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--         S Y S T E M . T A S K I N G . T A S K _ A T T R I B U T E S      --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--             Copyright (C) 1991-1994, Florida State University            --
--                     Copyright (C) 1995-2013, AdaCore                     --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------

with Ada.Unchecked_Conversion;

with System.Task_Primitives.Operations;
with System.Tasking.Initialization;

package body System.Tasking.Task_Attributes is

   use Task_Primitives.Operations;
   use Tasking.Initialization;

   function To_Access_Address is new Ada.Unchecked_Conversion
     (Access_Node, Access_Address);
   --  Store pointer to indirect attribute list

   --------------
   -- Finalize --
   --------------

   procedure Finalize (X : in out Instance) is
      Q, To_Be_Freed : Access_Node;
      Self_Id        : constant Task_Id := Self;

   begin
      --  Defer abort. Note that we use the nestable versions of Defer_Abort
      --  and Undefer_Abort, because abort can already deferred when this is
      --  called during finalization, which would cause an assert failure
      --  in Defer_Abort.

      Defer_Abort_Nestable (Self_Id);
      Lock_RTS;

      --  Remove this instantiation from the list of all instantiations

      declare
         P : Access_Instance;
         Q : Access_Instance := All_Attributes;

      begin
         while Q /= null and then Q /= X'Unchecked_Access loop
            P := Q; Q := Q.Next;
         end loop;

         pragma Assert (Q /= null);

         if P = null then
            All_Attributes := Q.Next;
         else
            P.Next := Q.Next;
         end if;
      end;

      if X.Index /= 0 then

         --  Free location of this attribute, for reuse

         In_Use := In_Use and not (2**Natural (X.Index));

         --  There is no need for finalization in this case, since controlled
         --  types are too big to fit in the TCB.

      else
         --  Remove nodes for this attribute from the lists of all tasks,
         --  and deallocate the nodes. Deallocation does finalization, if
         --  necessary.

         declare
            C : System.Tasking.Task_Id := All_Tasks_List;
            P : Access_Node;

         begin
            while C /= null loop
               Write_Lock (C);

               Q := To_Access_Node (C.Indirect_Attributes);
               while Q /= null
                 and then Q.Instance /= X'Unchecked_Access
               loop
                  P := Q;
                  Q := Q.Next;
               end loop;

               if Q /= null then
                  if P = null then
                     C.Indirect_Attributes := To_Access_Address (Q.Next);
                  else
                     P.Next := Q.Next;
                  end if;

                  --  Can't Deallocate now since we are holding RTS_Lock

                  Q.Next := To_Be_Freed;
                  To_Be_Freed := Q;
               end if;

               Unlock (C);
               C := C.Common.All_Tasks_Link;
            end loop;
         end;
      end if;

      Unlock_RTS;

      while To_Be_Freed /= null loop
         Q := To_Be_Freed;
         To_Be_Freed := To_Be_Freed.Next;
         X.Deallocate.all (Q);
      end loop;

      Undefer_Abort_Nestable (Self_Id);

   exception
      when others =>
         null;
         pragma Assert (False,
           "Exception in task attribute instance finalization");
   end Finalize;

   -------------------------
   -- Finalize Attributes --
   -------------------------

   --  This is to be called just before the ATCB is deallocated.
   --  It relies on the caller holding T.L write-lock on entry.

   procedure Finalize_Attributes (T : Task_Id) is
      P : Access_Node;
      Q : Access_Node := To_Access_Node (T.Indirect_Attributes);

   begin
      --  Deallocate all the indirect attributes of this task

      while Q /= null loop
         P := Q;
         Q := Q.Next; P.Instance.Deallocate.all (P);
      end loop;

      T.Indirect_Attributes := null;

   exception
      when others =>
         null;
         pragma Assert (False,
           "Exception in per-task attributes finalization");
   end Finalize_Attributes;

   ---------------------------
   -- Initialize Attributes --
   ---------------------------

   --  This is to be called by System.Tasking.Stages.Create_Task

   procedure Initialize_Attributes (T : Task_Id) is
      P       : Access_Instance;
      Self_Id : constant Task_Id := Self;

   begin
      --  Note: we call [Un]Defer_Abort_Nestable, rather than [Un]Defer_Abort,
      --  because Abort might already be deferred in Create_Task.

      Defer_Abort_Nestable (Self_Id);
      Lock_RTS;

      --  Initialize all the direct-access attributes of this task

      P := All_Attributes;

      while P /= null loop
         if P.Index /= 0 then
            T.Direct_Attributes (P.Index) :=
              Direct_Attribute_Element
                (System.Storage_Elements.To_Address (P.Initial_Value));
         end if;

         P := P.Next;
      end loop;

      Unlock_RTS;
      Undefer_Abort_Nestable (Self_Id);

   exception
      when others =>
         null;
         pragma Assert (False);
   end Initialize_Attributes;

end System.Tasking.Task_Attributes;
