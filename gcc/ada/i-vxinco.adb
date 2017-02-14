------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--     I N T E R F A C E S . V X W O R K S . I N T  _ C O N N E C T I O N   --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                        Copyright (C) 2016, AdaCore
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

package body Interfaces.VxWorks.Int_Connection is

   Connection_Routine : Interrupt_Connector;
   pragma Import (C, Connection_Routine, "__gnat_user_int_connect");
   --  Declared in System.Interrupts. Defaults to the standard OS connector in
   --  System.OS_Interface (or Interfaces.VxWorks for restricted runtimes).

   -------------
   -- Connect --
   -------------

   procedure Connect (Connector : Interrupt_Connector) is
   begin
      Connection_Routine := Connector;
   end Connect;

end Interfaces.VxWorks.Int_Connection;
