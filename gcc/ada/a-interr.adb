------------------------------------------------------------------------------
--                                                                          --
--                GNU ADA RUN-TIME LIBRARY (GNARL) COMPONENTS               --
--                                                                          --
--                         A D A . I N T E R R U P T S                      --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--                                                                          --
--             Copyright (C) 1991-2001 Florida State University             --
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

with System.Interrupts;
--  used for Interrupt_ID
--           Parameterless_Handler
--           Is_Reserved
--           Is_Handler_Attached
--           Current_Handler
--           Attach_Handler
--           Exchange_Handler
--           Detach_Handler
--           Reference

with Unchecked_Conversion;

package body Ada.Interrupts is

   package SI renames System.Interrupts;

   function To_System is new Unchecked_Conversion
     (Parameterless_Handler, SI.Parameterless_Handler);

   function To_Ada is new Unchecked_Conversion
     (SI.Parameterless_Handler, Parameterless_Handler);

   --------------------
   -- Attach_Handler --
   --------------------

   procedure Attach_Handler
     (New_Handler : Parameterless_Handler;
      Interrupt   : Interrupt_ID)
   is
   begin
      SI.Attach_Handler
        (To_System (New_Handler), SI.Interrupt_ID (Interrupt), False);
   end Attach_Handler;

   ---------------------
   -- Current_Handler --
   ---------------------

   function Current_Handler
     (Interrupt : Interrupt_ID)
      return      Parameterless_Handler
   is
   begin
      return To_Ada (SI.Current_Handler (SI.Interrupt_ID (Interrupt)));
   end Current_Handler;

   --------------------
   -- Detach_Handler --
   --------------------

   procedure Detach_Handler (Interrupt : in Interrupt_ID) is
   begin
      SI.Detach_Handler (SI.Interrupt_ID (Interrupt), False);
   end Detach_Handler;

   ----------------------
   -- Exchange_Handler --
   ----------------------

   procedure Exchange_Handler
     (Old_Handler : out Parameterless_Handler;
      New_Handler : Parameterless_Handler;
      Interrupt   : Interrupt_ID)
   is
      H : SI.Parameterless_Handler;

   begin
      SI.Exchange_Handler
        (H, To_System (New_Handler),
         SI.Interrupt_ID (Interrupt), False);
      Old_Handler := To_Ada (H);
   end Exchange_Handler;

   -----------------
   -- Is_Attached --
   -----------------

   function Is_Attached (Interrupt : Interrupt_ID) return Boolean is
   begin
      return SI.Is_Handler_Attached (SI.Interrupt_ID (Interrupt));
   end Is_Attached;

   -----------------
   -- Is_Reserved --
   -----------------

   function Is_Reserved (Interrupt : Interrupt_ID) return Boolean is
   begin
      return SI.Is_Reserved (SI.Interrupt_ID (Interrupt));
   end Is_Reserved;

   ---------------
   -- Reference --
   ---------------

   function Reference (Interrupt : Interrupt_ID) return System.Address is
   begin
      return SI.Reference (SI.Interrupt_ID (Interrupt));
   end Reference;

end Ada.Interrupts;
