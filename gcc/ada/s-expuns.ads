------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                       S Y S T E M . E X P _ U N S                         --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--                                                                          --
--          Copyright (C) 1992-1997 Free Software Foundation, Inc.          --
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
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This procedure performs exponentiation of unsigned types (with binary
--  modulus values up to and including that of Unsigned_Types.Unsigned).
--  The result is always full width, the caller must do a masking operation
--  the modulus is less than 2 ** (Unsigned'Size).

with System.Unsigned_Types;

package System.Exp_Uns is
pragma Pure (Exp_Uns);

   function Exp_Unsigned
     (Left  : System.Unsigned_Types.Unsigned;
      Right : Natural)
      return  System.Unsigned_Types.Unsigned;

end System.Exp_Uns;
