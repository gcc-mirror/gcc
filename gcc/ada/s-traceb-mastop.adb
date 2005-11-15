------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     S Y S T E M . T R A C E B A C K                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 1999-2005, AdaCore                     --
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

--  This version uses System.Machine_State_Operations routines

with System.Machine_State_Operations;

package body System.Traceback is

   use System.Machine_State_Operations;

   ----------------
   -- Call_Chain --
   ----------------

   procedure Call_Chain
     (Traceback : System.Address;
      Max_Len   : Natural;
      Len       : out Natural;
      Exclude_Min : System.Address := System.Null_Address;
      Exclude_Max : System.Address := System.Null_Address;
      Skip_Frames : Natural := 1)
   is
      type Tracebacks_Array is array (1 .. Max_Len) of Code_Loc;
      pragma Suppress_Initialization (Tracebacks_Array);

      M     : Machine_State;
      Code  : Code_Loc;

      Trace : Tracebacks_Array;
      for Trace'Address use Traceback;

      N_Skips  : Natural := 0;

   begin
      M := Allocate_Machine_State;
      Set_Machine_State (M);

      --  Skip the requested number of frames

      loop
         Code := Get_Code_Loc (M);
         exit when Code = Null_Address or else N_Skips = Skip_Frames;

         Pop_Frame (M);
         N_Skips := N_Skips + 1;
      end loop;

      --  Now, record the frames outside the exclusion bounds, updating
      --  the Len output value along the way.

      Len := 0;
      loop
         Code := Get_Code_Loc (M);
         exit when Code = Null_Address or else Len = Max_Len;

         if Code < Exclude_Min or else Code > Exclude_Max then
            Len := Len + 1;
            Trace (Len) := Code;
         end if;

         Pop_Frame (M);
      end loop;

      Free_Machine_State (M);
   end Call_Chain;

   ------------------
   -- C_Call_Chain --
   ------------------

   function C_Call_Chain
     (Traceback : System.Address;
      Max_Len   : Natural) return Natural
   is
      Val : Natural;
   begin
      Call_Chain (Traceback, Max_Len, Val);
      return Val;
   end C_Call_Chain;

end System.Traceback;
