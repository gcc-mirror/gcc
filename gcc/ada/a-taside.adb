------------------------------------------------------------------------------
--                                                                          --
--                        GNAT RUN-TIME COMPONENTS                          --
--                                                                          --
--              A D A . T A S K _ I D E N T I F I C A T I O N               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.1 $
--                                                                          --
--           Copyright (C) 1992-2001 Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with System.Address_Image;
--  used for the function itself

with System.Tasking;
--  used for Task_List

with System.Tasking.Stages;
--  used for Terminated
--           Abort_Tasks

with System.Tasking.Rendezvous;
--  used for Callable

with System.Task_Primitives.Operations;
--  used for Self

with System.Task_Info;
use type System.Task_Info.Task_Image_Type;

with Unchecked_Conversion;

package body Ada.Task_Identification is

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Convert_Ids (T : Task_Id) return System.Tasking.Task_ID;
   function Convert_Ids (T : System.Tasking.Task_ID) return Task_Id;
   pragma Inline (Convert_Ids);
   --  Conversion functions between different forms of Task_Id

   ---------
   -- "=" --
   ---------

   function  "=" (Left, Right : Task_Id) return Boolean is
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
         System.Tasking.Stages.Abort_Tasks
           (System.Tasking.Task_List'(1 => Convert_Ids (T)));
      end if;
   end Abort_Task;

   -----------------
   -- Convert_Ids --
   -----------------

   function Convert_Ids (T : Task_Id) return System.Tasking.Task_ID is
   begin
      return System.Tasking.Task_ID (T);
   end Convert_Ids;

   function Convert_Ids (T : System.Tasking.Task_ID) return Task_Id is
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
      use System.Task_Info;
      function To_Address is new
        Unchecked_Conversion (Task_Id, System.Address);

   begin
      if T = Null_Task_Id then
         return "";

      elsif T.Common.Task_Image = null then
         return System.Address_Image (To_Address (T));

      else
         return T.Common.Task_Image.all
            & "_" &  System.Address_Image (To_Address (T));
      end if;
   end Image;

   -----------------
   -- Is_Callable --
   -----------------

   function Is_Callable (T : Task_Id) return Boolean is
   begin
      if T = Null_Task_Id then
         raise Program_Error;
      else
         return System.Tasking.Rendezvous.Callable (Convert_Ids (T));
      end if;
   end Is_Callable;

   -------------------
   -- Is_Terminated --
   -------------------

   function Is_Terminated (T : Task_Id) return Boolean is
   begin
      if T = Null_Task_Id then
         raise Program_Error;
      else
         return System.Tasking.Stages.Terminated (Convert_Ids (T));
      end if;
   end Is_Terminated;

end Ada.Task_Identification;
