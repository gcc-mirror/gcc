------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--      G N A T . E X C E P T I O N _ A C T I O N S . C O R E _ D U M P     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2003-2012, Free Software Foundation, Inc.         --
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

--  This is the VMS version

with System;
with System.Aux_DEC;
separate (GNAT.Exception_Actions)
procedure Core_Dump (Occurrence : Exception_Occurrence) is

   use System;
   use System.Aux_DEC;

   pragma Unreferenced (Occurrence);

   SS_IMGDMP : constant := 1276;

   subtype Cond_Value_Type is Unsigned_Longword;
   subtype Access_Mode_Type is
      Unsigned_Word range 0 .. 3;
   Access_Mode_Zero : constant Access_Mode_Type := 0;

   Status : Cond_Value_Type;

   procedure Setexv (
     Status : out Cond_Value_Type;
     Vector : Unsigned_Longword := 0;
     Addres : Address           := Address_Zero;
     Acmode : Access_Mode_Type  := Access_Mode_Zero;
     Prvhnd : Unsigned_Longword := 0);
   pragma Import (External, Setexv);
   pragma Import_Valued_Procedure (Setexv, "SYS$SETEXV",
     (Cond_Value_Type, Unsigned_Longword, Address, Access_Mode_Type,
      Unsigned_Longword),
     (Value, Value, Value, Value, Value));

   procedure Lib_Signal (I : Integer);
   pragma Import (C, Lib_Signal);
   pragma Import_Procedure (Lib_Signal, "LIB$SIGNAL", Mechanism => (Value));
begin
   Setexv (Status, 1, Address_Zero, 3);
   Lib_Signal (SS_IMGDMP);
end Core_Dump;
