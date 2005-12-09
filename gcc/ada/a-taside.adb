------------------------------------------------------------------------------
--                                                                          --
--                        GNAT RUN-TIME COMPONENTS                          --
--                                                                          --
--              A D A . T A S K _ I D E N T I F I C A T I O N               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2005, Free Software Foundation, Inc.         --
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

with System.Address_Image;
with System.Parameters;
with System.Soft_Links;
with System.Task_Primitives.Operations;
with System.Tasking;

with Unchecked_Conversion;

pragma Warnings (Off);
--  Allow withing of non-Preelaborated units in Ada 2005 mode where this
--  package will be categorized as Preelaborate. See AI-362 for details.
--  It is safe in the context of the run-time to violate the rules!

with System.Tasking.Utilities;
--  Used for Abort_Tasks

pragma Warnings (On);

package body Ada.Task_Identification is

   use System.Parameters;

   package STPO renames System.Task_Primitives.Operations;

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Convert_Ids (T : Task_Id) return System.Tasking.Task_Id;
   function Convert_Ids (T : System.Tasking.Task_Id) return Task_Id;
   pragma Inline (Convert_Ids);
   --  Conversion functions between different forms of Task_Id

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Task_Id) return Boolean is
   begin
      return System.Tasking."=" (Convert_Ids (Left), Convert_Ids (Right));
   end "=";

   -----------------
   -- Abort_Task --
   ----------------

   procedure Abort_Task (T : Task_Id) is
   begin
      if T = Null_Task_Id then
         raise Program_Error;
      else
         System.Tasking.Utilities.Abort_Tasks
           (System.Tasking.Task_List'(1 => Convert_Ids (T)));
      end if;
   end Abort_Task;

   -----------------
   -- Convert_Ids --
   -----------------

   function Convert_Ids (T : Task_Id) return System.Tasking.Task_Id is
   begin
      return System.Tasking.Task_Id (T);
   end Convert_Ids;

   function Convert_Ids (T : System.Tasking.Task_Id) return Task_Id is
   begin
      return Task_Id (T);
   end Convert_Ids;

   ------------------
   -- Current_Task --
   ------------------

   function Current_Task return Task_Id is
   begin
      return Convert_Ids (System.Task_Primitives.Operations.Self);
   end Current_Task;

   -----------
   -- Image --
   -----------

   function Image (T : Task_Id) return String is
      function To_Address is new
        Unchecked_Conversion (Task_Id, System.Address);

   begin
      if T = Null_Task_Id then
         return "";

      elsif T.Common.Task_Image_Len = 0 then
         return System.Address_Image (To_Address (T));

      else
         return T.Common.Task_Image (1 .. T.Common.Task_Image_Len)
            & "_" &  System.Address_Image (To_Address (T));
      end if;
   end Image;

   -----------------
   -- Is_Callable --
   -----------------

   function Is_Callable (T : Task_Id) return Boolean is
      Result : Boolean;
      Id     : constant System.Tasking.Task_Id := Convert_Ids (T);
   begin
      if T = Null_Task_Id then
         raise Program_Error;
      else
         System.Soft_Links.Abort_Defer.all;

         if Single_Lock then
            STPO.Lock_RTS;
         end if;

         STPO.Write_Lock (Id);
         Result := Id.Callable;
         STPO.Unlock (Id);

         if Single_Lock then
            STPO.Unlock_RTS;
         end if;

         System.Soft_Links.Abort_Undefer.all;
         return Result;
      end if;
   end Is_Callable;

   -------------------
   -- Is_Terminated --
   -------------------

   function Is_Terminated (T : Task_Id) return Boolean is
      Result : Boolean;
      Id     : constant System.Tasking.Task_Id := Convert_Ids (T);

      use System.Tasking;

   begin
      if T = Null_Task_Id then
         raise Program_Error;
      else
         System.Soft_Links.Abort_Defer.all;

         if Single_Lock then
            STPO.Lock_RTS;
         end if;

         STPO.Write_Lock (Id);
         Result := Id.Common.State = Terminated;
         STPO.Unlock (Id);

         if Single_Lock then
            STPO.Unlock_RTS;
         end if;

         System.Soft_Links.Abort_Undefer.all;
         return Result;
      end if;
   end Is_Terminated;

end Ada.Task_Identification;
