------------------------------------------------------------------------------
--                                                                          --
--               GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS                --
--                                                                          --
--               S Y S T E M . E R R O R _ R E P O R T I N G                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 1991-1994, Florida State University            --
--             Copyright (C) 1995-2003, Ada Core Technologies               --
--                                                                          --
-- GNARL is free software; you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion. GNARL is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNARL; see file COPYING.  If not, write --
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
-- GNARL was developed by the GNARL team at Florida State University.       --
-- Extensive contributions were provided by Ada Core Technologies, Inc.     --
--                                                                          --
------------------------------------------------------------------------------

--  This package must not depend on anything else, since it may be
--  called during elaboration of other packages.

package body System.Error_Reporting is

   procedure Write (fildes : Integer; buf : System.Address; nbyte : Integer);
   pragma Import (C, Write, "write");

   procedure Prog_Exit (Status : Integer);
   pragma No_Return (Prog_Exit);
   pragma Import (C, Prog_Exit, "exit");

   Shutdown_Message : String := "failed run-time assertion : ";
   End_Of_Line : String := "" & ASCII.LF;

   --------------
   -- Shutdown --
   --------------

   function Shutdown (M : in String) return Boolean is
   begin
      Write (2, Shutdown_Message'Address, Shutdown_Message'Length);
      Write (2, M'Address, M'Length);
      Write (2, End_Of_Line'Address, End_Of_Line'Length);

      --  This call should never return

      Prog_Exit (1);

      --  Return is just to keep Ada happy (return required)

      return False;
   end Shutdown;

end System.Error_Reporting;
