------------------------------------------------------------------------------
--                                                                          --
--                          GNAT SYSTEM UTILITIES                           --
--                                                                          --
--                            G N A T P S Y S                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.1.16.1 $
--                                                                          --
--             Copyright (C) 1997 Free Software Foundation, Inc.            --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  Program to print out listing of System package with all constants
--  appearing explicitly.

with Ada.Text_IO;
with System; use System;
with Gnatvsn;

procedure GnatPsys is
   pragma Ident (Gnatvsn.Gnat_Version_String);

   procedure P (Item : String) renames Ada.Text_IO.Put_Line;

begin
   P ("package System is");

   P ("pragma Pure (System);");

   P ("");

   P ("   type Name is (SYSTEM_NAME_GNAT);");

   P ("   System_Name : constant Name := SYSTEM_NAME_GNAT;");

   P ("");

   P ("   --  System-Dependent Named Numbers");

   P ("");

   P ("   Min_Int                : constant := -(2 **" &
        Long_Long_Integer'Image (Long_Long_Integer'Size - 1) & ");");

   P ("   Max_Int                : constant := 2 **" &
        Long_Long_Integer'Image (Long_Long_Integer'Size - 1) & " - 1;");

   P ("");

   P ("   Max_Binary_Modulus     : constant := 2 **" &
        Long_Long_Integer'Image (Long_Long_Integer'Size) & ";");

   P ("   Max_Nonbinary_Modulus  : constant :=" &
        Integer'Image (Integer'Last) & ";");

   P ("");

   P ("   Max_Base_Digits        : constant :=" &
        Natural'Image (Long_Long_Float'Digits) & ";");

   P ("   Max_Digits             : constant :=" &
        Natural'Image (Long_Long_Float'Digits) & ";");

   P ("");

   P ("   Max_Mantissa           : constant := 63;");

   P ("   Fine_Delta             : constant := 2.0 ** (-Max_Mantissa);");

   P ("");

   P ("   Tick                   : constant :=" &
          Duration'Image (Duration (Standard'Tick)) & ";");

   P ("");

   P ("   --  Storage-related Declarations");

   P ("");

   P ("   type Address is private;");

   P ("   Null_Address : constant Address;");

   P ("");

   P ("   Storage_Unit           : constant :=" &
        Natural'Image (Standard'Storage_Unit) & ";");

   P ("   Word_Size              : constant :=" &
        Natural'Image (Standard'Word_Size) & ";");

   P ("   Memory_Size            : constant := 2 **" &
        Natural'Image (Standard'Address_Size) & ";");

   P ("");
   P ("   --  Address comparison");
   P ("");
   P ("   function ""<""  (Left, Right : Address) return Boolean;");
   P ("   function ""<="" (Left, Right : Address) return Boolean;");
   P ("   function "">""  (Left, Right : Address) return Boolean;");
   P ("   function "">="" (Left, Right : Address) return Boolean;");
   P ("   function ""=""  (Left, Right : Address) return Boolean;");
   P ("");
   P ("   pragma Import (Intrinsic, ""<""); ");
   P ("   pragma Import (Intrinsic, ""<="");");
   P ("   pragma Import (Intrinsic, "">""); ");
   P ("   pragma Import (Intrinsic, "">="");");
   P ("   pragma Import (Intrinsic, ""=""); ");
   P ("");
   P ("   --  Other System-Dependent Declarations");
   P ("");
   P ("   type Bit_Order is (High_Order_First, Low_Order_First);");
   P ("   Default_Bit_Order : constant Bit_Order;");
   P ("");
   P ("   --  Priority-related Declarations (RM D.1)");
   P ("");
   P ("   subtype Any_Priority is Integer range 0 .." &
        Natural'Image (Standard'Max_Interrupt_Priority) & ";");

   P ("");

   P ("   subtype Priority is Any_Priority range 0 .." &
        Natural'Image (Standard'Max_Priority) & ";");

   P ("");

   P ("   subtype Interrupt_Priority is Any_Priority range" &
        Natural'Image (Standard'Max_Priority + 1) & " .." &
        Natural'Image (Standard'Max_Interrupt_Priority) & ";");

   P ("");

   P ("   Default_Priority : constant Priority :=" &
        Natural'Image ((Priority'First + Priority'Last) / 2) & ";");

   P ("");

   P ("private");

   P ("");

   P ("   type Address is mod Memory_Size;                                  ");

   P ("   Null_Address : constant Address := 0;                             ");

   P ("                                                                     ");

   P ("   Default_Bit_Order : constant Bit_Order := " &
         Bit_Order'Image (Bit_Order'Val (Standard'Default_Bit_Order)) & ";");

   P ("");

   P ("end System;");
end GnatPsys;
