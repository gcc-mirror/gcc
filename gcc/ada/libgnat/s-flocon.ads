------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                 S Y S T E M . F L O A T _ C O N T R O L                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                     Copyright (C) 2000-2020, AdaCore                     --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  Control functions for floating-point unit

package System.Float_Control is
   pragma Pure;
   --  This is not fully correct, but this unit is with-ed by pure units
   --  (eg s-imgrea).

   procedure Reset;
   pragma Inline (Reset);
   --  Reset the floating-point processor to the default state needed to get
   --  correct Ada semantics for the target. Some third party tools change
   --  the settings for the floating-point processor. Reset can be called
   --  to reset the floating-point processor into the mode required by GNAT
   --  for correct operation. Use this call after a call to foreign code if
   --  you suspect incorrect floating-point operation after the call.
   --
   --  For example under Windows NT some system DLL calls change the default
   --  FPU arithmetic to 64 bit precision mode. However, since in Ada 95 it
   --  is required to provide full access to the floating-point types of the
   --  architecture, GNAT requires full 80-bit precision mode, and Reset makes
   --  sure this mode is established.
   --
   --  Similarly on the PPC processor, it is important that overflow and
   --  underflow exceptions be disabled.
   --
   --  The call to Reset simply has no effect if the target environment
   --  does not give rise to such concerns.
end System.Float_Control;
