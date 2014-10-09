------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                         S Y S T E M . T R A C E S                        --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--          Copyright (C) 2001-2014, Free Software Foundation, Inc.         --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with System.Soft_Links;
with System.Parameters;
with System.Traces.Format;

package body System.Traces is

   package SSL renames System.Soft_Links;
   use System.Traces.Format;

   ----------------------
   -- Send_Trace_Info  --
   ----------------------

   procedure Send_Trace_Info (Id : Trace_T) is
      Task_S  : constant String := SSL.Task_Name.all;
      Trace_S : String (1 .. 3 + Task_S'Length);

   begin
      if Parameters.Runtime_Traces then
         Trace_S (1 .. 3) := "/N:";
         Trace_S (4 .. Trace_S'Last) := Task_S;
         Send_Trace (Id, Trace_S);
      end if;
   end Send_Trace_Info;

   procedure Send_Trace_Info (Id : Trace_T; Timeout : Duration) is
      Task_S    : constant String := SSL.Task_Name.all;
      Timeout_S : constant String := Duration'Image (Timeout);
      Trace_S   : String (1 .. 6 + Task_S'Length + Timeout_S'Length);

   begin
      if Parameters.Runtime_Traces then
         Trace_S (1 .. 3) := "/N:";
         Trace_S (4 .. 3 + Task_S'Length) := Task_S;
         Trace_S (4 + Task_S'Length .. 6 + Task_S'Length) := "/T:";
         Trace_S (7 + Task_S'Length .. Trace_S'Last) := Timeout_S;
         Send_Trace (Id, Trace_S);
      end if;
   end Send_Trace_Info;
end System.Traces;
