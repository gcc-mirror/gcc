------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--            S Y S T E M . I N T E R R U P T _ M A N A G E M E N T .       --
--                             O P E R A T I O N S                          --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--                             $Revision: 1.1.16.1 $
--                                                                          --
--          Copyright (C) 1992-1998, Free Software Foundation, Inc.         --
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
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

package System.Interrupt_Management.Operations is

   procedure Thread_Block_Interrupt (Interrupt : Interrupt_ID);
   --  Mask the calling thread for the interrupt
   pragma Inline (Thread_Block_Interrupt);

   procedure Thread_Unblock_Interrupt (Interrupt : Interrupt_ID);
   --  Unmask the calling thread for the interrupt
   pragma Inline (Thread_Unblock_Interrupt);

   procedure Set_Interrupt_Mask (Mask : access Interrupt_Mask);
   --  Set the interrupt mask of the calling thread
   procedure Set_Interrupt_Mask
     (Mask  : access Interrupt_Mask;
      OMask : access Interrupt_Mask);
   --  Set the interrupt mask of the calling thread while returning the
   --  previous Mask.
   pragma Inline (Set_Interrupt_Mask);

   procedure Get_Interrupt_Mask (Mask : access Interrupt_Mask);
   --  Get the interrupt mask of the calling thread
   pragma Inline (Get_Interrupt_Mask);

   function Interrupt_Wait (Mask : access Interrupt_Mask) return Interrupt_ID;
   --  Wait for the interrupts specified in Mask and return
   --  the interrupt received. Upon error it return 0.
   pragma Inline (Interrupt_Wait);

   procedure Install_Default_Action (Interrupt : Interrupt_ID);
   --  Set the sigaction of the Interrupt to default (SIG_DFL).
   pragma Inline (Install_Default_Action);

   procedure Install_Ignore_Action (Interrupt : Interrupt_ID);
   --  Set the sigaction of the Interrupt to ignore (SIG_IGN).
   pragma Inline (Install_Ignore_Action);

   procedure Fill_Interrupt_Mask (Mask : access Interrupt_Mask);
   --  Get a Interrupt_Mask with all the interrupt masked
   pragma Inline (Fill_Interrupt_Mask);

   procedure Empty_Interrupt_Mask (Mask : access Interrupt_Mask);
   --  Get a Interrupt_Mask with all the interrupt unmasked
   pragma Inline (Empty_Interrupt_Mask);

   procedure Add_To_Interrupt_Mask
     (Mask      : access Interrupt_Mask;
      Interrupt : Interrupt_ID);
   --  Mask the given interrupt in the Interrupt_Mask
   pragma Inline (Add_To_Interrupt_Mask);

   procedure Delete_From_Interrupt_Mask
     (Mask      : access Interrupt_Mask;
      Interrupt : Interrupt_ID);
   --  Unmask the given interrupt in the Interrupt_Mask
   pragma Inline (Delete_From_Interrupt_Mask);

   function Is_Member
     (Mask      : access Interrupt_Mask;
      Interrupt : Interrupt_ID) return Boolean;
   --  See if a given interrupt is masked in the Interrupt_Mask
   pragma Inline (Is_Member);

   procedure Copy_Interrupt_Mask (X : out Interrupt_Mask; Y : Interrupt_Mask);
   --  Assigment needed for limited private type Interrupt_Mask.
   pragma Inline (Copy_Interrupt_Mask);

   procedure Interrupt_Self_Process (Interrupt : Interrupt_ID);
   --  raise an Interrupt process-level
   pragma Inline (Interrupt_Self_Process);

   --  The following objects serve as constants, but are initialized
   --  in the body to aid portability.  These actually belong to the
   --  System.Interrupt_Management but since Interrupt_Mask is a
   --  private type we can not have them declared there.

   Environment_Mask : aliased Interrupt_Mask;
   --  This mask represents the mask of Environment task when this package
   --  is being elaborated, except the signals being
   --  forced to be unmasked by RTS (items in Keep_Unmasked)

   All_Tasks_Mask : aliased Interrupt_Mask;
   --  This is the mask of all tasks created in RTS. Only one task in RTS
   --  is responsible for masking/unmasking signals (see s-interr.adb).

end System.Interrupt_Management.Operations;
