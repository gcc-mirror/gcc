------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . W C H _ C O N                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2005-2007, Free Software Foundation, Inc.         --
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

pragma Warnings (Off);
pragma Compiler_Unit;
pragma Warnings (On);

package body System.WCh_Con is

   ----------------------------
   -- Get_WC_Encoding_Method --
   ----------------------------

   function Get_WC_Encoding_Method (C : Character) return WC_Encoding_Method is
   begin
      for Method in WC_Encoding_Method loop
         if C = WC_Encoding_Letters (Method) then
            return Method;
         end if;
      end loop;

      raise Constraint_Error;
   end Get_WC_Encoding_Method;

   function Get_WC_Encoding_Method (S : String) return WC_Encoding_Method is
   begin
      if    S = "hex"       then
         return WCEM_Hex;
      elsif S = "upper"     then
         return WCEM_Upper;
      elsif S = "shift_jis" then
         return WCEM_Shift_JIS;
      elsif S = "euc"       then
         return WCEM_EUC;
      elsif S = "utf8"      then
         return WCEM_UTF8;
      elsif S = "brackets"  then
         return WCEM_Brackets;
      else
         raise Constraint_Error;
      end if;
   end Get_WC_Encoding_Method;

   --------------------------
   -- Is_Start_Of_Encoding --
   --------------------------

   function Is_Start_Of_Encoding
     (C  : Character;
      EM : WC_Encoding_Method) return Boolean
   is
   begin
      return (EM in WC_Upper_Half_Encoding_Method
               and then Character'Pos (C) >= 16#80#)
        or else (EM in WC_ESC_Encoding_Method and then C = ASCII.ESC);
   end Is_Start_Of_Encoding;

end System.WCh_Con;
