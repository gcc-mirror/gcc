------------------------------------------------------------------------------
--                                                                          --
--                 GNAT RUN-TIME LIBRARY (GNARL) COMPONENTS                 --
--                                                                          --
--                 I N T E R F A C E S . V X W O R K S . I O                --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--           Copyright (C) 2002-2025, Free Software Foundation, Inc.        --
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

package body Interfaces.VxWorks.IO is

   --------------------------
   -- Enable_Get_Immediate --
   --------------------------

   procedure Enable_Get_Immediate
     (File    : Interfaces.C_Streams.FILEs;
      Success : out Boolean)
   is
      Status : int;
      Fd     : int;

   begin
      Fd := fileno (File);
      Status := ioctl (Fd, FIOSETOPTIONS, OPT_RAW);

      if Status /= int (ERROR) then
         Success := True;
      else
         Success := False;
      end if;
   end Enable_Get_Immediate;

   ---------------------------
   -- Disable_Get_Immediate --
   ---------------------------

   procedure Disable_Get_Immediate
     (File    : Interfaces.C_Streams.FILEs;
      Success : out Boolean)
   is
      Status : int;
      Fd     : int;
   begin
      Fd := fileno (File);
      Status := ioctl (Fd, FIOSETOPTIONS, OPT_TERMINAL);
      Success := (if Status /= int (ERROR) then True else False);
   end Disable_Get_Immediate;

end Interfaces.VxWorks.IO;
