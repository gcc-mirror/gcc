------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                     S Y S T E M . W W D _ W C H A R                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2005 Free Software Foundation, Inc.          --
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

--  This package contains routines for [Wide_]Wide_Character'[Wide_]Wide_Width

package System.Wwd_WChar is
pragma Pure (Wwd_WChar);

   function Wide_Width_Wide_Character
     (Lo, Hi : Wide_Character) return Natural;
   --  Compute Wide_Width attribute for non-static type derived from
   --  Wide_Character. The arguments are the low and high bounds for
   --  the type. EM is the wide-character encoding method.

   function Wide_Width_Wide_Wide_Character
     (Lo, Hi : Wide_Wide_Character) return Natural;
   --  Compute Wide_Width attribute for non-static type derived from
   --  Wide_Wide_Character. The arguments are the low and high bounds for
   --  the type. EM is the wide-character encoding method.

   function Wide_Wide_Width_Wide_Character
     (Lo, Hi : Wide_Character) return Natural;
   --  Compute Wide_Wide_Width attribute for non-static type derived from
   --  Wide_Character. The arguments are the low and high bounds for
   --  the type. EM is the wide-character encoding method.

   function Wide_Wide_Width_Wide_Wide_Char
     (Lo, Hi : Wide_Wide_Character) return Natural;
   --  Compute Wide_Wide_Width attribute for non-static type derived from
   --  Wide_Wide_Character. The arguments are the low and high bounds for
   --  the type. EM is the wide-character encoding method.

end System.Wwd_WChar;
