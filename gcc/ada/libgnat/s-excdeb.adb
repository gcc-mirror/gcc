------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--               S Y S T E M . E X C E P T I O N S _ D E B U G              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2006-2018, Free Software Foundation, Inc.          --
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

pragma Compiler_Unit_Warning;

package body System.Exceptions_Debug is

   ---------------------------
   -- Debug_Raise_Exception --
   ---------------------------

   procedure Debug_Raise_Exception
     (E : SSL.Exception_Data_Ptr; Message : String)
   is
      pragma Inspection_Point (E, Message);
   begin
      null;
   end Debug_Raise_Exception;

   -------------------------------
   -- Debug_Unhandled_Exception --
   -------------------------------

   procedure Debug_Unhandled_Exception (E : SSL.Exception_Data_Ptr) is
      pragma Inspection_Point (E);
   begin
      null;
   end Debug_Unhandled_Exception;

   --------------------------------
   -- Debug_Raise_Assert_Failure --
   --------------------------------

   procedure Debug_Raise_Assert_Failure is
   begin
      null;
   end Debug_Raise_Assert_Failure;

   -----------------
   -- Local_Raise --
   -----------------

   procedure Local_Raise (Excep : System.Address) is
      pragma Warnings (Off, Excep);
   begin
      return;
   end Local_Raise;

end System.Exceptions_Debug;
