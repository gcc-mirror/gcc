------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--           S Y S T E M . T R A C E B A C K . S Y M B O L I C              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 1999-2014, AdaCore                     --
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

--  This is the default implementation for platforms where the full capability
--  is not supported. It returns tracebacks as lists of LF separated strings of
--  the form "0x..." corresponding to the addresses.

with Ada.Exceptions.Traceback; use Ada.Exceptions.Traceback;
with System.Address_Image;

package body System.Traceback.Symbolic is

   ------------------------
   -- Symbolic_Traceback --
   ------------------------

   function Symbolic_Traceback
     (Traceback : System.Traceback_Entries.Tracebacks_Array) return String
   is
   begin
      if Traceback'Length = 0 then
         return "";

      else
         declare
            Img : String := System.Address_Image (Traceback (Traceback'First));

            Result : String (1 .. (Img'Length + 3) * Traceback'Length);
            Last   : Natural := 0;

         begin
            for J in Traceback'Range loop
               Img := System.Address_Image (Traceback (J));
               Result (Last + 1 .. Last + 2) := "0x";
               Last := Last + 2;
               Result (Last + 1 .. Last + Img'Length) := Img;
               Last := Last + Img'Length + 1;
               Result (Last) := ASCII.LF;
            end loop;

            return Result (1 .. Last);
         end;
      end if;
   end Symbolic_Traceback;

   function Symbolic_Traceback
     (E : Ada.Exceptions.Exception_Occurrence) return String
   is
   begin
      return Symbolic_Traceback (Ada.Exceptions.Traceback.Tracebacks (E));
   end Symbolic_Traceback;

end System.Traceback.Symbolic;
