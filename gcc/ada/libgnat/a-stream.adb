------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                           A D A . S T R E A M S                          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2013-2020, Free Software Foundation, Inc.      --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT. The copyright notice above, and the license provisions that follow --
-- apply solely to the  contents of the part following the private keyword. --
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

with Ada.IO_Exceptions;

package body Ada.Streams is

   --------------
   -- Read_SEA --
   --------------

   procedure Read_SEA
     (S : access Root_Stream_Type'Class;
      V : out Stream_Element_Array)
   is
      Last : Stream_Element_Offset;

   begin
      Read (S.all, V, Last);

      if Last /= V'Last then
         raise Ada.IO_Exceptions.End_Error;
      end if;
   end Read_SEA;

   ---------------
   -- Write_SEA --
   ---------------

   procedure Write_SEA
     (S : access Root_Stream_Type'Class;
      V : Stream_Element_Array)
   is
   begin
      Write (S.all, V);
   end Write_SEA;

end Ada.Streams;
