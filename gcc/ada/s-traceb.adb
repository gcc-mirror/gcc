------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     S Y S T E M . T R A C E B A C K                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--          Copyright (C) 1999-2001 Free Software Foundation, Inc.          --
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

--  This is the default version of this package

package body System.Traceback is

   ------------------
   -- C_Call_Chain --
   ------------------

   function C_Call_Chain
     (Traceback : System.Address;
      Max_Len   : Natural)
      return      Natural
   is
      Val : Natural;

   begin
      Call_Chain (Traceback, Max_Len, Val);
      return Val;
   end C_Call_Chain;

   ----------------
   -- Call_Chain --
   ----------------

   function Backtrace
     (Traceback   : System.Address;
      Len         : Integer;
      Exclude_Min : System.Address;
      Exclude_Max : System.Address)
      return        Integer;
   pragma Import (C, Backtrace, "__gnat_backtrace");

   procedure Call_Chain
     (Traceback : System.Address;
      Max_Len   : Natural;
      Len       : out Natural;
      Exclude_Min,
      Exclude_Max : System.Address := System.Null_Address)
   is
   begin
      Len := Backtrace (Traceback, Max_Len, Exclude_Min, Exclude_Max);
   end Call_Chain;

end System.Traceback;
