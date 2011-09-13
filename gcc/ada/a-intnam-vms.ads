------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                   A D A . I N T E R R U P T S . N A M E S                --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--          Copyright (C) 1991-2011, Free Software Foundation, Inc.         --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------

--  This is a OpenVMS/Alpha version of this package

--  This target-dependent package spec contains names of interrupts
--  supported by the local system.

with System.OS_Interface;

package Ada.Interrupts.Names is

   --  All identifiers in this unit are implementation defined

   pragma Implementation_Defined;

   package OS renames System.OS_Interface;

   Interrupt_ID_0   : constant Interrupt_ID := OS.Interrupt_ID_0;
   Interrupt_ID_1   : constant Interrupt_ID := OS.Interrupt_ID_1;
   Interrupt_ID_2   : constant Interrupt_ID := OS.Interrupt_ID_2;
   Interrupt_ID_3   : constant Interrupt_ID := OS.Interrupt_ID_3;
   Interrupt_ID_4   : constant Interrupt_ID := OS.Interrupt_ID_4;
   Interrupt_ID_5   : constant Interrupt_ID := OS.Interrupt_ID_5;
   Interrupt_ID_6   : constant Interrupt_ID := OS.Interrupt_ID_6;
   Interrupt_ID_7   : constant Interrupt_ID := OS.Interrupt_ID_7;
   Interrupt_ID_8   : constant Interrupt_ID := OS.Interrupt_ID_8;
   Interrupt_ID_9   : constant Interrupt_ID := OS.Interrupt_ID_9;
   Interrupt_ID_10  : constant Interrupt_ID := OS.Interrupt_ID_10;
   Interrupt_ID_11  : constant Interrupt_ID := OS.Interrupt_ID_11;
   Interrupt_ID_12  : constant Interrupt_ID := OS.Interrupt_ID_12;
   Interrupt_ID_13  : constant Interrupt_ID := OS.Interrupt_ID_13;
   Interrupt_ID_14  : constant Interrupt_ID := OS.Interrupt_ID_14;
   Interrupt_ID_15  : constant Interrupt_ID := OS.Interrupt_ID_15;
   Interrupt_ID_16  : constant Interrupt_ID := OS.Interrupt_ID_16;
   Interrupt_ID_17  : constant Interrupt_ID := OS.Interrupt_ID_17;
   Interrupt_ID_18  : constant Interrupt_ID := OS.Interrupt_ID_18;
   Interrupt_ID_19  : constant Interrupt_ID := OS.Interrupt_ID_19;
   Interrupt_ID_20  : constant Interrupt_ID := OS.Interrupt_ID_20;
   Interrupt_ID_21  : constant Interrupt_ID := OS.Interrupt_ID_21;
   Interrupt_ID_22  : constant Interrupt_ID := OS.Interrupt_ID_22;
   Interrupt_ID_23  : constant Interrupt_ID := OS.Interrupt_ID_23;
   Interrupt_ID_24  : constant Interrupt_ID := OS.Interrupt_ID_24;
   Interrupt_ID_25  : constant Interrupt_ID := OS.Interrupt_ID_25;
   Interrupt_ID_26  : constant Interrupt_ID := OS.Interrupt_ID_26;
   Interrupt_ID_27  : constant Interrupt_ID := OS.Interrupt_ID_27;
   Interrupt_ID_28  : constant Interrupt_ID := OS.Interrupt_ID_28;
   Interrupt_ID_29  : constant Interrupt_ID := OS.Interrupt_ID_29;
   Interrupt_ID_30  : constant Interrupt_ID := OS.Interrupt_ID_30;
   Interrupt_ID_31  : constant Interrupt_ID := OS.Interrupt_ID_31;

end Ada.Interrupts.Names;
