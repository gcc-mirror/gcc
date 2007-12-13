------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                          G N A T . C T R L _ C                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                      Copyright (C) 2002-2007, AdaCore                    --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

package body GNAT.Ctrl_C is

   type C_Handler_Type is access procedure;
   pragma Convention (C, C_Handler_Type);

   Ada_Handler : Handler_Type;

   procedure C_Handler;
   pragma Convention (C, C_Handler);

   procedure C_Handler is
   begin
      Ada_Handler.all;
   end C_Handler;

   procedure Install_Handler (Handler : Handler_Type) is
      procedure Internal (Handler : C_Handler_Type);
      pragma Import (C, Internal, "__gnat_install_int_handler");
   begin
      Ada_Handler := Handler;
      Internal (C_Handler'Access);
   end Install_Handler;

end GNAT.Ctrl_C;
