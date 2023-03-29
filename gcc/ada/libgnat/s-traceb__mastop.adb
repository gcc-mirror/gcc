------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     S Y S T E M . T R A C E B A C K                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 1999-2023, AdaCore                     --
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

--  This version uses System.Machine_State_Operations routines

with System.Machine_State_Operations;

package body System.Traceback is

   use System.Machine_State_Operations;

   procedure Call_Chain
     (Traceback   : System.Address;
      Max_Len     : Natural;
      Len         : out Natural;
      Exclude_Min : System.Address := System.Null_Address;
      Exclude_Max : System.Address := System.Null_Address;
      Skip_Frames : Natural := 1);
   --  Same as the exported version, but takes Traceback as an Address

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

   procedure Call_Chain
     (Traceback   : in out System.Traceback_Entries.Tracebacks_Array;
      Max_Len     : Natural;
      Len         : out Natural;
      Exclude_Min : System.Address := System.Null_Address;
      Exclude_Max : System.Address := System.Null_Address;
      Skip_Frames : Natural := 1)
   is
   begin
      Call_Chain
        (Traceback'Address, Max_Len, Len,
         Exclude_Min, Exclude_Max,

         --  Skip one extra frame to skip the other Call_Chain entry as well

         Skip_Frames => Skip_Frames + 1);
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
