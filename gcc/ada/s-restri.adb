------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                  S Y S T E M . R E S T R I C T I O N S                   --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2004 Free Software Foundation, Inc.            --
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
-- covered by the GNU Public License.                                       --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

package body System.Restrictions is
   use Rident;

   -------------------
   -- Abort_Allowed --
   -------------------

   function Abort_Allowed return Boolean is
   begin
      return Run_Time_Restrictions.Violated (No_Abort_Statements)
               or else
             Run_Time_Restrictions.Violated (Max_Asynchronous_Select_Nesting);
   end Abort_Allowed;

   ---------------------
   -- Tasking_Allowed --
   ---------------------

   function Tasking_Allowed return Boolean is
   begin
      return Run_Time_Restrictions.Violated (Max_Tasks)
               or else
             Run_Time_Restrictions.Violated (No_Tasking);
   end Tasking_Allowed;

--  Package elaboration code (acquire restrictions)

begin
   Acquire_Restrictions : declare

      subtype Big_String is String (Positive);
      type Big_String_Ptr is access all Big_String;

      RString : Big_String_Ptr;
      pragma Import (C, RString, "__gl_restrictions");

      P : Natural := 1;
      --  Pointer to scan string

      C : Character;
      --  Next character from string

      function Get_Char return Character;
      --  Get next character from string

      function Get_Natural return Natural;
      --  Scan out natural value known to be in range, updating P past it

      --------------
      -- Get_Char --
      --------------

      function Get_Char return Character is
      begin
         P := P + 1;
         return RString (P - 1);
      end Get_Char;

      -----------------
      -- Get_Natural --
      -----------------

      function Get_Natural return Natural is
         N : Natural := 0;

      begin
         while RString (P) in '0' .. '9' loop
            N := N * 10 + (Character'Pos (Get_Char) - Character'Pos ('0'));
         end loop;

         return N;
      end Get_Natural;

   --  Start of processing for Acquire_Restrictions

   begin
      --  Acquire data corresponding to first R line

      for R in All_Boolean_Restrictions loop
         C := Get_Char;

         if C = 'v' then
            Run_Time_Restrictions.Violated (R) := True;

         elsif C = 'r' then
            Run_Time_Restrictions.Set (R) := True;
         end if;
      end loop;

      --  Acquire data corresponding to second R line

      for RP in All_Parameter_Restrictions loop

         --  Acquire restrictions pragma information

         if Get_Char = 'r' then
            Run_Time_Restrictions.Set (RP) := True;
            Run_Time_Restrictions.Value (RP) := Get_Natural;
         end if;

         --  Acquire restrictions violations information

         if Get_Char = 'v' then
            Run_Time_Restrictions.Violated (RP) := True;
            Run_Time_Restrictions.Count (RP) := Get_Natural;

            if RString (P) = '+' then
               Run_Time_Restrictions.Unknown (RP) := True;
               P := P + 1;
            end if;
         end if;
      end loop;
   end Acquire_Restrictions;
end System.Restrictions;

