------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                      S Y S T E M . I M G _ C H A R                       --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.1.16.1 $
--                                                                          --
--     Copyright (C) 1992,1993,1994,1995 Free Software Foundation, Inc.     --
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

package body System.Img_Char is

   ---------------------
   -- Image_Character --
   ---------------------

   function Image_Character (V : Character) return String is
      subtype Cname is String (1 .. 3);

      S : Cname;

      subtype C0_Range is Character
        range Character'Val (16#00#) .. Character'Val (16#1F#);

      C0 : constant array (C0_Range) of Cname :=
              (Character'Val (16#00#) => "NUL",
               Character'Val (16#01#) => "SOH",
               Character'Val (16#02#) => "STX",
               Character'Val (16#03#) => "ETX",
               Character'Val (16#04#) => "EOT",
               Character'Val (16#05#) => "ENQ",
               Character'Val (16#06#) => "ACK",
               Character'Val (16#07#) => "BEL",
               Character'Val (16#08#) => "BS ",
               Character'Val (16#09#) => "HT ",
               Character'Val (16#0A#) => "LF ",
               Character'Val (16#0B#) => "VT ",
               Character'Val (16#0C#) => "FF ",
               Character'Val (16#0D#) => "CR ",
               Character'Val (16#0E#) => "SO ",
               Character'Val (16#0F#) => "SI ",
               Character'Val (16#10#) => "DLE",
               Character'Val (16#11#) => "DC1",
               Character'Val (16#12#) => "DC2",
               Character'Val (16#13#) => "DC3",
               Character'Val (16#14#) => "DC4",
               Character'Val (16#15#) => "NAK",
               Character'Val (16#16#) => "SYN",
               Character'Val (16#17#) => "ETB",
               Character'Val (16#18#) => "CAN",
               Character'Val (16#19#) => "EM ",
               Character'Val (16#1A#) => "SUB",
               Character'Val (16#1B#) => "ESC",
               Character'Val (16#1C#) => "FS ",
               Character'Val (16#1D#) => "GS ",
               Character'Val (16#1E#) => "RS ",
               Character'Val (16#1F#) => "US ");

      subtype C1_Range is Character
        range Character'Val (16#7F#) .. Character'Val (16#9F#);

      C1 : constant array (C1_Range) of Cname :=
              (Character'Val (16#7F#) => "DEL",
               Character'Val (16#80#) => "res",
               Character'Val (16#81#) => "res",
               Character'Val (16#82#) => "BPH",
               Character'Val (16#83#) => "NBH",
               Character'Val (16#84#) => "res",
               Character'Val (16#85#) => "NEL",
               Character'Val (16#86#) => "SSA",
               Character'Val (16#87#) => "ESA",
               Character'Val (16#88#) => "HTS",
               Character'Val (16#89#) => "HTJ",
               Character'Val (16#8A#) => "VTS",
               Character'Val (16#8B#) => "PLD",
               Character'Val (16#8C#) => "PLU",
               Character'Val (16#8D#) => "RI ",
               Character'Val (16#8E#) => "SS2",
               Character'Val (16#8F#) => "SS3",
               Character'Val (16#90#) => "DCS",
               Character'Val (16#91#) => "PU1",
               Character'Val (16#92#) => "PU2",
               Character'Val (16#93#) => "STS",
               Character'Val (16#94#) => "CCH",
               Character'Val (16#95#) => "MW ",
               Character'Val (16#96#) => "SPA",
               Character'Val (16#97#) => "EPA",
               Character'Val (16#98#) => "SOS",
               Character'Val (16#99#) => "res",
               Character'Val (16#9A#) => "SCI",
               Character'Val (16#9B#) => "CSI",
               Character'Val (16#9C#) => "ST ",
               Character'Val (16#9D#) => "OSC",
               Character'Val (16#9E#) => "PM ",
               Character'Val (16#9F#) => "APC");

   begin
      --  Control characters are represented by their names (RM 3.5(32))

      if V in C0_Range then
         S := C0 (V);

         if S (3) = ' ' then
            return S (1 .. 2);
         else
            return S;
         end if;

      elsif V in C1_Range then
         S := C1 (V);

         if S (1) /= 'r' then
            if S (3) = ' ' then
               return S (1 .. 2);
            else
               return S;
            end if;

         --  Special case, res means RESERVED_nnn where nnn is the three digit
         --  decimal value corresponding to the code position (more efficient
         --  to compute than to store!)

         else
            declare
               VP : constant Natural := Character'Pos (V);
               St : String (1 .. 12) := "RESERVED_xxx";

            begin
               St (10) := Character'Val (48 + VP / 100);
               St (11) := Character'Val (48 + (VP / 10) mod 10);
               St (12) := Character'Val (48 + VP mod 10);
               return St;
            end;
         end if;

      --  Normal characters yield the character enlosed in quotes (RM 3.5(32))

      else
         S (1) := ''';
         S (2) := V;
         S (3) := ''';
         return S;
      end if;
   end Image_Character;

end System.Img_Char;
