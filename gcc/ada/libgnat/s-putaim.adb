------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                          SYSTEM.PUT_TASK_IMAGES                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2020, Free Software Foundation, Inc.            --
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

pragma Ada_2020;
with Unchecked_Conversion;
with Ada.Strings.Text_Output.Utils;
use Ada.Strings.Text_Output;
use Ada.Strings.Text_Output.Utils;

package body System.Put_Task_Images is

   procedure Put_Image_Protected (S : in out Sink'Class) is
   begin
      Put_UTF_8 (S, "(protected object)");
   end Put_Image_Protected;

   procedure Put_Image_Task
     (S : in out Sink'Class; Id : Ada.Task_Identification.Task_Id)
   is
   begin
      Put_UTF_8 (S, "(task " & Ada.Task_Identification.Image (Id) & ")");
   end Put_Image_Task;

end System.Put_Task_Images;
