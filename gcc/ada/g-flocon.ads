------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                    G N A T . F L O A T _ C O N T R O L                   --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                            $Revision: 1.5 $
--                                                                          --
--              Copyright (C) 2000 Ada Core Technologies, Inc.              --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- It is now maintained by Ada Core Technologies Inc (http://www.gnat.com). --
--                                                                          --
------------------------------------------------------------------------------

--  Control functions for floating-point unit

package GNAT.Float_Control is

   procedure Reset;
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

private
   pragma Import (C, Reset, "__gnat_init_float");

end GNAT.Float_Control;
