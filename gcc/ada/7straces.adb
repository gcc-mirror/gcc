------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                         S Y S T E M . T R A C E S                        --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--             Copyright (C) 2001 Free Software Foundation, Inc.            --
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
      Task_S  : String := SSL.Task_Name.all;
      Trace_S : String (1 .. 3 + Task_S'Length);

   begin
      if Parameters.Runtime_Traces then
         Trace_S (1 .. 3) := "/N:";
         Trace_S (4 .. Trace_S'Last) := Task_S;
         Send_Trace (Id, Trace_S);
      end if;
   end Send_Trace_Info;

   procedure Send_Trace_Info (Id : Trace_T; Timeout : Duration) is
      Task_S    : String := SSL.Task_Name.all;
      Timeout_S : String := Duration'Image (Timeout);
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
