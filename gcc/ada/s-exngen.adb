------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                       S Y S T E M . E X N _ G E N                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.1 $
--                                                                          --
--          Copyright (C) 1992-2001, Free Software Foundation, Inc.         --
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

package body System.Exn_Gen is

   --------------------
   -- Exn_Float_Type --
   --------------------

   function Exn_Float_Type
     (Left  : Type_Of_Base;
      Right : Integer)
      return  Type_Of_Base
   is
      pragma Suppress (Division_Check);
      pragma Suppress (Overflow_Check);
      pragma Suppress (Range_Check);

      Result : Type_Of_Base := 1.0;
      Factor : Type_Of_Base := Left;
      Exp    : Integer := Right;

   begin
      --  We use the standard logarithmic approach, Exp gets shifted right
      --  testing successive low order bits and Factor is the value of the
      --  base raised to the next power of 2. For positive exponents we
      --  multiply the result by this factor, for negative exponents, we
      --  Division by this factor.

      if Exp >= 0 then
         loop
            if Exp rem 2 /= 0 then
               Result := Result * Factor;
            end if;

            Exp := Exp / 2;
            exit when Exp = 0;
            Factor := Factor * Factor;
         end loop;

         return Result;

      --  Negative exponent. For a zero base, we should arguably return an
      --  infinity of the right sign, but it is not clear that there is
      --  proper authorization to do so, so for now raise Constraint_Error???

      elsif Factor = 0.0 then
         raise Constraint_Error;

      --  Here we have a non-zero base and a negative exponent

      else
         --  For the negative exponent case, a constraint error during this
         --  calculation happens if Factor gets too large, and the proper
         --  response is to return 0.0, since what we essentially have is
         --  1.0 / infinity, and the closest model number will be zero.

         begin
            loop
               if Exp rem 2 /= 0 then
                  Result := Result * Factor;
               end if;

               Exp := Exp / 2;
               exit when Exp = 0;
               Factor := Factor * Factor;
            end loop;

            return 1.0 / Result;

         exception

            when Constraint_Error =>
               return 0.0;
         end;
      end if;
   end Exn_Float_Type;

   ----------------------
   -- Exn_Integer_Type --
   ----------------------

   --  Note that negative exponents get a constraint error because the
   --  subtype of the Right argument (the exponent) is Natural.

   function Exn_Integer_Type
     (Left  : Type_Of_Base;
      Right : Natural)
      return  Type_Of_Base
   is
      pragma Suppress (Division_Check);
      pragma Suppress (Overflow_Check);

      Result : Type_Of_Base := 1;
      Factor : Type_Of_Base := Left;
      Exp    : Natural := Right;

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
               Result := Result * Factor;
            end if;

            Exp := Exp / 2;
            exit when Exp = 0;
            Factor := Factor * Factor;
         end loop;
      end if;

      return Result;
   end Exn_Integer_Type;

end System.Exn_Gen;
