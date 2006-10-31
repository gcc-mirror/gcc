------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--          A D A . D I S P A T C H I N G . R O U N D _ R O B I N           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--            Copyright (C) 2006, Free Software Foundation, Inc.            --
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

package body Ada.Dispatching.Round_Robin is

   -----------------
   -- Set_Quantum --
   -----------------

   procedure Set_Quantum
     (Pri     : System.Priority;
      Quantum : Ada.Real_Time.Time_Span)
   is
      pragma Unreferenced (Quantum);
   begin
      if not Is_Round_Robin (Pri) then
         raise Dispatching_Policy_Error;
      end if;
   end Set_Quantum;

   -----------------
   -- Set_Quantum --
   -----------------

   procedure Set_Quantum
     (Low, High : System.Priority;
      Quantum   : Ada.Real_Time.Time_Span)
   is
      pragma Unreferenced (Quantum);
   begin
      for Index in Low .. High loop
         if not Is_Round_Robin (Index) then
            raise Dispatching_Policy_Error;
         end if;
      end loop;
   end Set_Quantum;

   --------------------
   -- Actual_Quantum --
   --------------------

   function Actual_Quantum
     (Pri : System.Priority) return Ada.Real_Time.Time_Span
   is
   begin
      if Is_Round_Robin (Pri) then
         return Default_Quantum;
      else
         raise Dispatching_Policy_Error;
      end if;
   end Actual_Quantum;

   --------------------
   -- Is_Round_Robin --
   --------------------

   function Is_Round_Robin (Pri : System.Priority) return Boolean is
      function Get_Policy (Prio : System.Any_Priority) return Character;
      pragma Import (C, Get_Policy, "__gnat_get_specific_dispatching");
   begin
      return Get_Policy (Pri) = 'R';
   end Is_Round_Robin;

end Ada.Dispatching.Round_Robin;
