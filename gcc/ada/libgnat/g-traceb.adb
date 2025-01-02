------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                       G N A T . T R A C E B A C K                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                      Copyright (C) 1999-2025, AdaCore                    --
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

--  Run-time non-symbolic traceback support

with System.Traceback;

package body GNAT.Traceback is

   ----------------
   -- Call_Chain --
   ----------------

   procedure Call_Chain
     (Traceback : out Tracebacks_Array;
      Len       : out Natural)
   is
   begin
      System.Traceback.Call_Chain (Traceback, Traceback'Length, Len);
   end Call_Chain;

   function Call_Chain
     (Max_Len     : Positive;
      Skip_Frames : Natural := 1) return Tracebacks_Array
   is
      Traceback : Tracebacks_Array (1 .. Max_Len);
      Len       : Natural;
   begin
      System.Traceback.Call_Chain
        (Traceback, Max_Len, Len, Skip_Frames => Skip_Frames + 1);
      return Traceback (1 .. Len);
   end Call_Chain;

end GNAT.Traceback;
