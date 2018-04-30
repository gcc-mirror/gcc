------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                   A D A . I N T E R R U P T S . N A M E S                --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--          Copyright (C) 1997-2018, Free Software Foundation, Inc.         --
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

--  This is a NT (native) version of this package

--  This target-dependent package spec contains names of interrupts supported
--  by the local system.

with System.OS_Interface;

package Ada.Interrupts.Names is

   --  All identifiers in this unit are implementation defined

   pragma Implementation_Defined;

   --  Beware that the mapping of names to signals may be many-to-one. There
   --  may be aliases. Also, for all signal names that are not supported on the
   --  current system the value of the corresponding constant will be zero.

   SIGINT  : constant Interrupt_ID :=  -- interrupt (rubout)
               System.OS_Interface.SIGINT;

   SIGILL  : constant Interrupt_ID :=  -- illegal instruction (not reset)
               System.OS_Interface.SIGILL;

   SIGABRT : constant Interrupt_ID :=  -- used by abort (use SIGIOT in future)
               System.OS_Interface.SIGABRT;

   SIGFPE  : constant Interrupt_ID :=  -- floating point exception
               System.OS_Interface.SIGFPE;

   SIGSEGV : constant Interrupt_ID :=  -- segmentation violation
               System.OS_Interface.SIGSEGV;

   SIGTERM : constant Interrupt_ID :=  -- software termination signal from kill
               System.OS_Interface.SIGTERM;

end Ada.Interrupts.Names;
