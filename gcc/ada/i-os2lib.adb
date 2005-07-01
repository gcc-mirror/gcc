------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     I N T E R F A C E S . O S 2 L I B                    --
--                                                                          --
--                                  B o d y                                 --
--                                                                          --
--          Copyright (C) 1993-1999 Free Software Foundation, Inc.          --
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

with Interfaces.OS2Lib.Errors;

package body Interfaces.OS2Lib is

   pragma Warnings (Off, Errors);
   package IOE renames Interfaces.OS2Lib.Errors;

   -------------------
   -- Must_Not_Fail --
   -------------------

   procedure Must_Not_Fail (Return_Code : APIRET) is
   begin
      pragma Assert (Return_Code = IOE.NO_ERROR);
      null;
   end Must_Not_Fail;

   -----------------------
   -- Sem_Must_Not_Fail --
   -----------------------

   procedure Sem_Must_Not_Fail (Return_Code : OS2Lib.APIRET) is
   begin
      pragma Assert
        (Return_Code = IOE.NO_ERROR
           or else
         Return_Code = IOE.ERROR_ALREADY_POSTED
           or else
         Return_Code = IOE.ERROR_ALREADY_RESET);
      null;
   end Sem_Must_Not_Fail;

end Interfaces.OS2Lib;
