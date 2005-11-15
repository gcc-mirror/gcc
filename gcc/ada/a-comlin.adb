------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                     A D A . C O M M A N D _ L I N E                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2005, Free Software Foundation, Inc.         --
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

with System;                 use System;

package body Ada.Command_Line is

   function Arg_Count return Natural;
   pragma Import (C, Arg_Count, "__gnat_arg_count");

   procedure Fill_Arg (A : System.Address; Arg_Num : Integer);
   pragma Import (C, Fill_Arg, "__gnat_fill_arg");

   function Len_Arg (Arg_Num : Integer) return Integer;
   pragma Import (C, Len_Arg, "__gnat_len_arg");

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Initialized return Boolean;
   --  Checks to ensure that gnat_argc and gnat_argv have been properly
   --  initialized.  Returns false if not, or if argv / argc are
   --  unsupported on the target (e.g. VxWorks).

   --------------
   -- Argument --
   --------------

   function Argument (Number : in Positive) return String is
      Num : Positive;

   begin
      if Number > Argument_Count then
         raise Constraint_Error;
      end if;

      if Remove_Args = null then
         Num := Number;
      else
         Num := Remove_Args (Number);
      end if;

      declare
         Arg : aliased String (1 .. Len_Arg (Num));

      begin
         Fill_Arg (Arg'Address, Num);
         return Arg;
      end;
   end Argument;

   --------------------
   -- Argument_Count --
   --------------------

   function Argument_Count return Natural is
   begin
      if not Initialized then
         --  RM A.15 (11)
         return 0;
      end if;

      if Remove_Args = null then
         return Arg_Count - 1;
      else
         return Remove_Count;
      end if;
   end Argument_Count;

   -----------------
   -- Initialized --
   -----------------

   function Initialized return Boolean is
      gnat_argv : System.Address;
      pragma Import (C, gnat_argv, "gnat_argv");

   begin
      return gnat_argv /= System.Null_Address;
   end Initialized;

   ------------------
   -- Command_Name --
   ------------------

   function Command_Name return String is
   begin
      if not Initialized then
         return "";
      end if;

      declare
         Arg : aliased String (1 .. Len_Arg (0));

      begin
         Fill_Arg (Arg'Address, 0);
         return Arg;
      end;
   end Command_Name;

end Ada.Command_Line;
