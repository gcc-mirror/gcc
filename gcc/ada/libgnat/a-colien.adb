------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--         A D A . C O M M A N D _ L I N E . E N V I R O N M E N T          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1996-2025, Free Software Foundation, Inc.         --
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

with System;

package body Ada.Command_Line.Environment is

   -----------------------
   -- Environment_Count --
   -----------------------

   function Environment_Count return Natural is
      function Env_Count return Natural;
      pragma Import (C, Env_Count, "__gnat_env_count");

   begin
      return Env_Count;
   end Environment_Count;

   -----------------------
   -- Environment_Value --
   -----------------------

   function Environment_Value (Number : Positive) return String is
      procedure Fill_Env (E : System.Address; Env_Num : Integer);
      pragma Import (C, Fill_Env, "__gnat_fill_env");

      function Len_Env (Env_Num : Integer) return Integer;
      pragma Import (C, Len_Env, "__gnat_len_env");

   begin
      if Number > Environment_Count then
         raise Constraint_Error;
      end if;

      declare
         Env : aliased String (1 .. Len_Env (Number - 1));
      begin
         Fill_Env (Env'Address, Number - 1);
         return Env;
      end;
   end Environment_Value;

end Ada.Command_Line.Environment;
