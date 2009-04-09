------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--           S Y S T E M . I N T E R R U P T _ M A N A G E M E N T          --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--         Copyright (C) 1992-2009, Free Software Foundation, Inc.          --
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

package body System.Interrupt_Management is

   ----------------
   -- Initialize --
   ----------------

   Initialized : Boolean := False;

   procedure Initialize is
      use System.OS_Interface;
      Status : Cond_Value_Type;

   begin
      if Initialized then
         return;
      end if;

      Initialized := True;
      Abort_Task_Interrupt := Interrupt_ID_0;
      --  Unused

      Reserve := Reserve or Keep_Unmasked or Keep_Masked;
      Reserve (Interrupt_ID_0) := True;

      Sys_Crembx
        (Status => Status,
         Prmflg => 0,
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
   end Initialize;

end System.Interrupt_Management;
