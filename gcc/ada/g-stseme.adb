------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                  GNAT.SOCKETS.THIN.SOCKET_ERROR_MESSAGE                  --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                     Copyright (C) 2007-2009, AdaCore                     --
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

--  This is the default implementation of this unit, using the standard C
--  library's strerror(3) function. It is used on all platforms except Windows,
--  since on that platform socket errno values are distinct from the system
--  ones: there is a specific variant of this function in g-socthi-mingw.adb.

with Ada.Unchecked_Conversion;

with System.CRTL;

separate (GNAT.Sockets.Thin)

--------------------------
-- Socket_Error_Message --
--------------------------

function Socket_Error_Message
  (Errno : Integer) return C.Strings.chars_ptr
is
   use type Interfaces.C.Strings.chars_ptr;

   pragma Warnings (Off);
   function To_Chars_Ptr is
     new Ada.Unchecked_Conversion
       (System.Address, Interfaces.C.Strings.chars_ptr);
   --  On VMS, the compiler warns because System.Address is 64 bits, but
   --  chars_ptr is 32 bits. It should be safe, though, because strerror
   --  will return a 32-bit pointer.
   pragma Warnings (On);

   C_Msg : C.Strings.chars_ptr;

begin
   C_Msg := To_Chars_Ptr (System.CRTL.strerror (Errno));

   if C_Msg = C.Strings.Null_Ptr then
      return Unknown_System_Error;
   else
      return C_Msg;
   end if;
end Socket_Error_Message;
