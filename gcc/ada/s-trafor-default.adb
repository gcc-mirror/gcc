------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                  S Y S T E M . T R A C E S . F O R M A T                 --
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

with System.Parameters;

package body System.Traces.Format is

   procedure Send_Trace (Id : Trace_T; Info : String) is separate;

   ------------------
   -- Format_Trace --
   ------------------

   function Format_Trace (Source : in String) return String_Trace is
      Length : Integer      := Source'Length;
      Result : String_Trace := (others => ' ');

   begin
      --  If run-time tracing active, then fill the string

      if Parameters.Runtime_Traces then
         if Max_Size - Length > 0 then
            Result (1 .. Length) := Source (1 .. Length);
            Result (Length + 1 .. Max_Size) := (others => ' ');
            Result (Length + 1) := ASCII.NUL;
         else
            Result (1 .. Max_Size - 1) := Source (1 .. Max_Size - 1);
            Result (Max_Size) := ASCII.NUL;
         end if;
      end if;

      return Result;
   end Format_Trace;

   ------------
   -- Append --
   ------------

   function Append
     (Source : String_Trace;
      Annex  : String)
      return   String_Trace
   is
      Result        : String_Trace := (others => ' ');
      Source_Length : Integer := 1;
      Annex_Length  : Integer := Annex'Length;

   begin
      if Parameters.Runtime_Traces then

         --  First we determine the size used, without the spaces at the
         --  end, if a String_Trace is present. Look at
         --  System.Traces.Tasking for examples.

         while Source (Source_Length) /= ASCII.NUL loop
            Source_Length := Source_Length + 1;
         end loop;

         --  Then we fill the string.

         if Source_Length - 1 + Annex_Length <= Max_Size then
            Result (1 .. Source_Length - 1) :=
              Source (1 .. Source_Length - 1);

            Result (Source_Length .. Source_Length - 1 + Annex_Length) :=
              Annex (1 ..  Annex_Length);

            Result (Source_Length + Annex_Length) := ASCII.NUL;

            Result (Source_Length + Annex_Length + 1 .. Max_Size) :=
              (others => ' ');
         else
            Result (1 .. Source_Length - 1) := Source (1 .. Source_Length - 1);
            Result (Source_Length .. Max_Size - 1) :=
              Annex (1 .. Max_Size - Source_Length);
            Result (Max_Size) := ASCII.NUL;
         end if;
      end if;

      return Result;
   end Append;

end System.Traces.Format;
