------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--         A D A . C O M M A N D _ L I N E . E N V I R O N M E N T          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.1.16.1 $
--                                                                          --
--          Copyright (C) 1996-2001 Free Software Foundation, Inc.          --
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

   function Environment_Value (Number : in Positive) return String is
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
