------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                          SYSTEM.PUT_TASK_IMAGES                          --
--                                                                          --
--                                 S p e c                                  --
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
with Ada.Strings.Text_Output;
with Ada.Task_Identification;
package System.Put_Task_Images is

   --  This package contains subprograms that are called by the generated code
   --  for the 'Put_Image attribute for protected and task types. This is
   --  separate from System.Put_Images to avoid dragging the tasking runtimes
   --  into nontasking programs.

   subtype Sink is Ada.Strings.Text_Output.Sink;

   procedure Put_Image_Protected (S : in out Sink'Class);
   procedure Put_Image_Task
     (S : in out Sink'Class; Id : Ada.Task_Identification.Task_Id);

end System.Put_Task_Images;
