------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--                     S Y S T E M . I M G _ W C H A R                      --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2005, Free Software Foundation, Inc.         --
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

with System.Img_Char; use System.Img_Char;
with System.WCh_Con;  use System.WCh_Con;
with System.WCh_WtS;  use System.WCh_WtS;

package body System.Img_WChar is

   --------------------------
   -- Image_Wide_Character --
   --------------------------

   function Image_Wide_Character
     (V  : Wide_Character;
      EM : WC_Encoding_Method) return String
   is
      Val : constant Natural := Wide_Character'Pos (V);
      WS  : Wide_String (1 .. 3);

   begin
      --  If in range of standard character, use standard character routine

      if Val < 16#80#
        or else (Val <= 16#FF#
                  and then EM not in WC_Upper_Half_Encoding_Method)
      then
         return Image_Character (Character'Val (Val));

      --  if the value is one of the last two characters in the type, use
      --  their language-defined names (3.5.2(3)).

      elsif Val = 16#FFFE# then
         return "FFFE";

      elsif Val = 16#FFFF# then
         return "FFFF";

      --  Otherwise return an appropriate escape sequence (i.e. one matching
      --  the convention implemented by Scn.Wide_Char). The easiest thing is
      --  to build a wide string for the result, and then use the Wide_Value
      --  function to build the resulting String.

      else
         WS (1) := ''';
         WS (2) := V;
         WS (3) := ''';

         return Wide_String_To_String (WS, EM);
      end if;
   end Image_Wide_Character;

   -------------------------------
   -- Image_Wide_Wide_Character --
   -------------------------------

   function Image_Wide_Wide_Character
     (V  : Wide_Wide_Character;
      EM : WC_Encoding_Method) return String
   is
      Val : constant Natural := Wide_Wide_Character'Pos (V);
      WS  : Wide_Wide_String (1 .. 3);

   begin
      --  If in range of standard Wide_Character, then we use the
      --  Wide_Character routine

      if Val <= 16#FFFF# then
         return Image_Wide_Character (Wide_Character'Val (Val), EM);

      --  Otherwise return an appropriate escape sequence (i.e. one matching
      --  the convention implemented by Scn.Wide_Wide_Char). The easiest thing
      --  is to build a wide string for the result, and then use the
      --  Wide_Wide_Value function to build the resulting String.

      else
         WS (1) := ''';
         WS (2) := V;
         WS (3) := ''';

         return Wide_Wide_String_To_String (WS, EM);
      end if;
   end Image_Wide_Wide_Character;

end System.Img_WChar;
