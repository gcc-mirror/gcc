------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                   GNAT.SOCKETS.THIN.HOST_ERROR_MESSAGES                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2007-2008, AdaCore                     --
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
