------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                     S Y S T E M . W W D _ W C H A R                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2009 Free Software Foundation, Inc.          --
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

--  This package contains routines for [Wide_]Wide_Character'[Wide_]Wide_Width

package System.Wwd_WChar is
   pragma Pure;

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
