------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                         B A C K E N D _ U T I L S                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2021-2021, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Lib;
with Opt;    use Opt;
with Switch; use Switch;

package body Backend_Utils is

   ---------------------------------
   -- Scan_Common_Back_End_Switch --
   ---------------------------------

   function Scan_Common_Back_End_Switch (Switch_Chars : String) return Boolean
   is
      First : constant Positive := Switch_Chars'First + 1;
      Last  : constant Natural  := Switch_Last (Switch_Chars);
   begin

      --  Recognize -gxxx switches

      if Switch_Chars (First) = 'g' then
         Debugger_Level := 2;

         if First < Last then
            case Switch_Chars (First + 1) is
               when '0' =>
                  Debugger_Level := 0;
               when '1' =>
                  Debugger_Level := 1;
               when '2' =>
                  Debugger_Level := 2;
               when '3' =>
                  Debugger_Level := 3;
               when others =>
                  null;
            end case;
         end if;

      --  Back end switch -fdiagnostics-format=json tells the frontend to
      --  output its error and warning messages in the same format GCC
      --  uses when passed -fdiagnostics-format=json.

      elsif Switch_Chars (First .. Last) = "fdiagnostics-format=json" then
         Opt.JSON_Output := True;

      --  Back-end switch -fno-inline also sets the front end flags to entirely
      --  inhibit all inlining. So we store it and set the appropriate
      --  flags.
      --  For gcc back ends, -fno-inline disables Inline pragmas only,
      --  not Inline_Always to remain consistent with the always_inline
      --  attribute behavior.

      elsif Switch_Chars (First .. Last) = "fno-inline" then
         Opt.Disable_FE_Inline := True;

      --  Back end switch -fpreserve-control-flow also sets the front end
      --  flag that inhibits improper control flow transformations.

      elsif Switch_Chars (First .. Last) = "fpreserve-control-flow" then
         Opt.Suppress_Control_Flow_Optimizations := True;

      elsif Switch_Chars (First .. Last) = "S" then
         Generate_Asm := True;

      else
         return False;
      end if;

      Lib.Store_Compilation_Switch (Switch_Chars);
      return True;
   end Scan_Common_Back_End_Switch;

end Backend_Utils;
