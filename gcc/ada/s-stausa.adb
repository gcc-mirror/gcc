------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                   S Y S T E M - S T A C K _ U S A G E                    --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--         Copyright (C) 2004-2005, Free Software Foundation, Inc.          --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNARL; see file COPYING.  If not, write --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------
--  Dummy implementation.

package body System.Stack_Usage is

   --------------------
   -- Compute_Result --
   --------------------

   procedure Compute_Result (Analyzer : in out Stack_Analyzer) is
      pragma Unreferenced (Analyzer);
   begin
      null;
   end Compute_Result;

   ----------------
   -- Fill_Stack --
   ----------------

   procedure Fill_Stack (Analyzer : in out Stack_Analyzer) is
      pragma Unreferenced (Analyzer);
   begin
      null;
   end Fill_Stack;

   -------------------------
   -- Initialize_Analyzer --
   -------------------------

   procedure Initialize_Analyzer
     (Analyzer  : in out Stack_Analyzer;
      Task_Name : String;
      Size      : Natural;
      Bottom    : Stack_Address;
      Pattern   : Word_32 := 16#DEAD_BEEF#)
   is
      pragma Unreferenced (Analyzer);
      pragma Unreferenced (Task_Name);
      pragma Unreferenced (Size);
      pragma Unreferenced (Pattern);
      pragma Unreferenced (Bottom);
   begin
      null;
   end Initialize_Analyzer;

   ---------------------
   --  Output_Results --
   ---------------------

   procedure Output_Results is
   begin
      null;
   end Output_Results;

   -------------------
   -- Report_Result --
   -------------------

   procedure Report_Result (Analyzer : Stack_Analyzer) is
      pragma Unreferenced (Analyzer);
   begin
      null;
   end Report_Result;

end System.Stack_Usage;
