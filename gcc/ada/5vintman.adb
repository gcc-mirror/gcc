------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--           S Y S T E M . I N T E R R U P T _ M A N A G E M E N T          --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                             $Revision: 1.1 $
--                                                                          --
--           Copyright (C) 1991-2000, Florida State University              --
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
-- GNARL was developed by the GNARL team at Florida State University. It is --
-- now maintained by Ada Core Technologies Inc. in cooperation with Florida --
-- State University (http://www.gnat.com).                                  --
--                                                                          --
------------------------------------------------------------------------------

--  This is a OpenVMS/Alpha version of this package.

--  PLEASE DO NOT add any dependences on other packages.
--  This package is designed to work with or without tasking support.

--  See the other warnings in the package specification before making
--  any modifications to this file.

with System.OS_Interface;
--  used for various Constants, Signal and types

package body System.Interrupt_Management is

   use System.OS_Interface;
   use type unsigned_long;

   type Interrupt_List is array (Interrupt_ID range <>) of Interrupt_ID;

   ---------------------------
   -- Initialize_Interrupts --
   ---------------------------

   procedure Initialize_Interrupts is
      Status : Cond_Value_Type;
   begin
      Sys_Crembx
        (Status => Status,
         Prmflg => False,
         Chan   => Rcv_Interrupt_Chan,
         Maxmsg => Interrupt_ID'Size,
         Bufquo => Interrupt_Bufquo,
         Lognam => "GNAT_Interrupt_Mailbox",
         Flags  => CMB_M_READONLY);

      pragma Assert ((Status and 1) = 1);

      Sys_Assign
        (Status => Status,
         Devnam => "GNAT_Interrupt_Mailbox",
         Chan   => Snd_Interrupt_Chan,
         Flags  => AGN_M_WRITEONLY);

      pragma Assert ((Status and 1) = 1);

   end Initialize_Interrupts;

begin
   --  Unused
   Abort_Task_Interrupt := Interrupt_ID_0;

   Reserve := Reserve or Keep_Unmasked or Keep_Masked;

   Reserve (Interrupt_ID_0) := True;

   Initialize_Interrupts;

end System.Interrupt_Management;
