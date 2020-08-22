------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                          G N A T . C T R L _ C                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                      Copyright (C) 2002-2020, AdaCore                    --
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

package body GNAT.Ctrl_C is

   type C_Handler_Type is access procedure;
   pragma Convention (C, C_Handler_Type);

   Ada_Handler : Handler_Type;

   procedure C_Handler;
   pragma Convention (C, C_Handler);

   ---------------
   -- C_Handler --
   ---------------

   procedure C_Handler is
   begin
      Ada_Handler.all;
   end C_Handler;

   ---------------------
   -- Install_Handler --
   ---------------------

   procedure Install_Handler (Handler : Handler_Type) is
      procedure Internal (Handler : C_Handler_Type);
      pragma Import (C, Internal, "__gnat_install_int_handler");
   begin
      Ada_Handler := Handler;
      Internal (C_Handler'Access);
   end Install_Handler;

end GNAT.Ctrl_C;
