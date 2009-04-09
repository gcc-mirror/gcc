------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     S Y S T E M . T R A C E B A C K                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1999-2009, Free Software Foundation, Inc.         --
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

--  This is the default version of this package

--  Note: this unit must be compiled using -fno-optimize-sibling-calls.
--  See comment below in body of Call_Chain for details on the reason.

pragma Warnings (Off);
pragma Compiler_Unit;
pragma Warnings (On);

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
      Exclude_Max : System.Address;
      Skip_Frames : Integer)
      return        Integer;
   pragma Import (C, Backtrace, "__gnat_backtrace");

   procedure Call_Chain
     (Traceback   : System.Address;
      Max_Len     : Natural;
      Len         : out Natural;
      Exclude_Min : System.Address := System.Null_Address;
      Exclude_Max : System.Address := System.Null_Address;
      Skip_Frames : Natural := 1)
   is
   begin
      --  Note: Backtrace relies on the following call actually creating a
      --  stack frame. To ensure that this is the case, it is essential to
      --  compile this unit without sibling call optimization.

      --  We want the underlying engine to skip its own frame plus the
      --  ones we have been requested to skip ourselves.

      Len := Backtrace (Traceback   => Traceback,
                        Len         => Max_Len,
                        Exclude_Min => Exclude_Min,
                        Exclude_Max => Exclude_Max,
                        Skip_Frames => Skip_Frames + 1);
   end Call_Chain;

end System.Traceback;
