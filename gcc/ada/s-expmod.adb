------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                       S Y S T E M . E X P _ M O D                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--     Copyright (C) 1992,1993,1994,1995 Free Software Foundation, Inc.     --
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

package body System.Exp_Mod is

   -----------------
   -- Exp_Modular --
   -----------------

   function Exp_Modular
     (Left    : Integer;
      Modulus : Integer;
      Right   : Natural)
      return    Integer
   is
      Result : Integer := 1;
      Factor : Integer := Left;
      Exp    : Natural := Right;

      function Mult (X, Y : Integer) return Integer;
      pragma Inline (Mult);
      --  Modular multiplication. Note that we can't take advantage of the
      --  compiler's circuit, because the modulus is not known statically.

      function Mult (X, Y : Integer) return Integer is
      begin
         return Integer
           (Long_Long_Integer (X) * Long_Long_Integer (Y)
             mod Long_Long_Integer (Modulus));
      end Mult;

   --  Start of processing for Exp_Modular

   begin
      --  We use the standard logarithmic approach, Exp gets shifted right
      --  testing successive low order bits and Factor is the value of the
      --  base raised to the next power of 2.

      --  Note: it is not worth special casing the cases of base values -1,0,+1
      --  since the expander does this when the base is a literal, and other
      --  cases will be extremely rare.

      if Exp /= 0 then
         loop
            if Exp rem 2 /= 0 then
               Result := Mult (Result, Factor);
            end if;

            Exp := Exp / 2;
            exit when Exp = 0;
            Factor := Mult (Factor, Factor);
         end loop;
      end if;

      return Result;

   end Exp_Modular;

end System.Exp_Mod;
