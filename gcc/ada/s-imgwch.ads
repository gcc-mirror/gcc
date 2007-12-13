------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                     S Y S T E M . I M G _ W C H A R                      --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2007, Free Software Foundation, Inc.         --
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

--  Wide_[Wide_]Character'Image

package System.Img_WChar is
   pragma Pure;

   procedure Image_Wide_Character
     (V        : Wide_Character;
      S        : in out String;
      P        : out Natural;
      Ada_2005 : Boolean);
   --  Computes Wide_Character'Image (V) and stores the result in S (1 .. P)
   --  setting the resulting value of P. The caller guarantees that S is long
   --  enough to hold the result, and that S'First is 1. The parameter Ada_2005
   --  is True if operating in Ada 2005 mode (or beyond). This is required to
   --  deal with the annoying FFFE/FFFF incompatibility.

   procedure Image_Wide_Wide_Character
     (V : Wide_Wide_Character;
      S : in out String;
      P : out Natural);
   --  Computes Wide_Wide_Character'Image (V) and stores the result in
   --  S (1 .. P) setting the resulting value of P. The caller guarantees
   --  that S is long enough to hold the result, and that S'First is 1.

end System.Img_WChar;
