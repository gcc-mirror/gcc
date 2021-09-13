------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--     I N T E R F A C E S . V X W O R K S . I N T  _ C O N N E C T I O N   --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                        Copyright (C) 2016-2021, AdaCore                  --
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

--  This package provides users with the ability to use a custom routine for
--  connecting hardware interrupts for VxWorks environments that support the
--  capability to handle them. The custom routine must have the same profile
--  as the VxWorks intConnect() routine.

with System;

package Interfaces.VxWorks.Int_Connection is

   type Interrupt_Connector is access function
     (Vector    : Interrupt_Vector;
      Handler   : VOIDFUNCPTR;
      Parameter : System.Address := System.Null_Address) return STATUS;
   pragma Convention (C, Interrupt_Connector);
   --  Convention C for compatibility with intConnect(). User alternatives are
   --  likely to be imports of C routines anyway.

   procedure Connect (Connector : Interrupt_Connector);
   --  Set user-defined interrupt connection routine. Must precede calls to
   --  Ada.Interrupts.Attach_Handler, or the default connector from
   --  System.OS_Interface (or Interfaces.VxWorks for Ravenscar Cert) will be
   --  used. Can be called multiple times to change the connection routine for
   --  subsequent calls to Attach_Handler.

end Interfaces.VxWorks.Int_Connection;
