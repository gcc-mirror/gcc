------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                   GNAT.SOCKETS.THIN.HOST_ERROR_MESSAGES                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2007-2010, AdaCore                     --
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

--  This is the default implementation of this unit, providing explicit
--  literal messages (we do not use hstrerror from the standard C library,
--  as this function is obsolete).

separate (GNAT.Sockets.Thin)
package body Host_Error_Messages is

   package Messages is
      HOST_NOT_FOUND : aliased char_array := "Host not found" & nul;
      TRY_AGAIN      : aliased char_array := "Try again"      & nul;
      NO_RECOVERY    : aliased char_array := "No recovery"    & nul;
      NO_DATA        : aliased char_array := "No address"     & nul;
      Unknown_Error  : aliased char_array := "Unknown error"  & nul;
   end Messages;

   function Host_Error_Message (H_Errno : Integer) return C.Strings.chars_ptr
   is
      use Interfaces.C.Strings;
      function TCP
        (P : char_array_access; Nul_Check : Boolean := False) return chars_ptr
         renames To_Chars_Ptr;

   begin
      case H_Errno is
         when SOSC.HOST_NOT_FOUND =>
            return TCP (Messages.HOST_NOT_FOUND'Access);

         when SOSC.TRY_AGAIN      =>
            return TCP (Messages.TRY_AGAIN'Access);

         when SOSC.NO_RECOVERY    =>
            return TCP (Messages.NO_RECOVERY'Access);

         when SOSC.NO_DATA        =>
            return TCP (Messages.NO_DATA'Access);

         when others              =>
            return TCP (Messages.Unknown_Error'Access);

      end case;
   end Host_Error_Message;

end Host_Error_Messages;
