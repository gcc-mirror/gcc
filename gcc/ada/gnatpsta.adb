------------------------------------------------------------------------------
--                                                                          --
--                          GNAT SYSTEM UTILITIES                           --
--                                                                          --
--                            G N A T P S T A                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.1 $
--                                                                          --
--          Copyright (C) 1997-2001 Free Software Foundation, Inc.          --
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
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  Program to print out listing of Standard package for the target (not
--  the host) with all constants appearing explicitly. This is not really
--  valid Ada, since one cannot really define new base types, but it is a
--  helpful listing from a documentation point of view.

--  Note that special care has been taken to use the host parameters for
--  integer and floating point sizes.

with Ada.Text_IO; use Ada.Text_IO;
with Gnatvsn;
with Ttypef;      use Ttypef;
with Ttypes;      use Ttypes;
with Types;       use Types;

procedure GnatPsta is
   pragma Ident (Gnatvsn.Gnat_Version_String);

   procedure P (Item : String) renames Ada.Text_IO.Put_Line;

   procedure P_Int_Range   (Size : Pos; Put_First : Boolean := True);
   --  Prints the range of an integer based on its Size. If Put_First is
   --  False, then skip the first bound.

   procedure P_Float_Range (Nb_Digits : Pos);
   --  Prints the maximum range of a Float whose 'Digits is given by Nb_Digits

   -------------------
   -- P_Float_Range --
   -------------------

   procedure P_Float_Range (Nb_Digits : Pos) is
   begin
      --  This routine assumes only IEEE floats.
      --  ??? Should the following be adapted for OpenVMS ?

      case Nb_Digits is
         when IEEES_Digits =>
            P ("     range " & IEEES_First'Universal_Literal_String & " .. " &
                               IEEES_Last'Universal_Literal_String & ";");
         when IEEEL_Digits =>
            P ("     range " & IEEEL_First'Universal_Literal_String & " .. " &
                               IEEEL_Last'Universal_Literal_String & ";");
         when IEEEX_Digits =>
            P ("     range " & IEEEX_First'Universal_Literal_String & " .. " &
                               IEEEX_Last'Universal_Literal_String & ";");

         when others =>
            P (";");
      end case;

      --  If one of the floating point types of the host computer has the
      --  same digits as the target float we are processing, then print out
      --  the float range using the host computer float type.

      if Nb_Digits = Short_Float'Digits then
         P ("     --    " &
            Short_Float'First'Img & " .. " & Short_Float'Last'Img);

      elsif Nb_Digits = Float'Digits then
         P ("     --    " &
            Float'First'Img & " .. " & Float'Last'Img);

      elsif Nb_Digits = Long_Float'Digits then
         P ("     --    " &
            Long_Float'First'Img & " .. " & Long_Float'Last'Img);

      elsif Nb_Digits = Long_Long_Float'Digits then
         P ("     --    " &
            Long_Long_Float'First'Img & " .. " & Long_Long_Float'Last'Img);
      end if;

      New_Line;
   end P_Float_Range;

   -----------------
   -- P_Int_Range --
   -----------------

   procedure P_Int_Range (Size : Pos; Put_First : Boolean := True) is
   begin
      if Put_First then
         Put (" is range -(2 **" & Pos'Image (Size - 1) & ")");
      end if;
      P (" .. +(2 **" & Pos'Image (Size - 1) & " - 1);");
   end P_Int_Range;

--  Start of processing for GnatPsta

begin
   P ("package Standard is");
   P ("pragma Pure(Standard);");
   New_Line;

   P ("   type Boolean is (False, True);");
   New_Line;

   --  Integer types

   Put ("   type Integer");
   P_Int_Range (Standard_Integer_Size);
   New_Line;

   Put ("   subtype Natural  is Integer range 0");
   P_Int_Range (Standard_Integer_Size, Put_First => False);

   Put ("   subtype Positive is Integer range 1");
   P_Int_Range (Standard_Integer_Size, Put_First => False);
   New_Line;

   Put ("   type Short_Short_Integer");
   P_Int_Range (Standard_Short_Short_Integer_Size);

   Put ("   type Short_Integer      ");
   P_Int_Range (Standard_Short_Integer_Size);

   Put ("   type Long_Integer       ");
   P_Int_Range (Standard_Long_Integer_Size);

   Put ("   type Long_Long_Integer  ");
   P_Int_Range (Standard_Long_Long_Integer_Size);
   New_Line;

   --  Floating point types

   P ("   type Short_Float     is digits"
      & Standard_Short_Float_Digits'Img);
   P_Float_Range (Standard_Short_Float_Digits);

   P ("   type Float           is digits"
      & Standard_Float_Digits'Img);
   P_Float_Range (Standard_Float_Digits);

   P ("   type Long_Float      is digits"
      & Standard_Long_Float_Digits'Img);
   P_Float_Range (Standard_Long_Float_Digits);

   P ("   type Long_Long_Float is digits"
      & Standard_Long_Long_Float_Digits'Img);
   P_Float_Range (Standard_Long_Long_Float_Digits);

   P ("   --  function ""*"" (Left : root_integer; Right : root_real)");
   P ("   --    return root_real;");
   New_Line;

   P ("   --  function ""*"" (Left : root_real;    Right : root_integer)");
   P ("   --    return root_real;");
   New_Line;

   P ("   --  function ""/"" (Left : root_real;    Right : root_integer)");
   P ("   --    return root_real;");
   New_Line;

   P ("   --  function ""*"" (Left : universal_fixed; " &
                                                "Right : universal_fixed)");
   P ("   --    return universal_fixed;");
   New_Line;

   P ("   --  function ""/"" (Left : universal_fixed; " &
                                                "Right : universal_fixed)");
   P ("   --    return universal_fixed;");
   New_Line;

   P ("   --  The declaration of type Character is based on the standard");
   P ("   --  ISO 8859-1 character set.");
   New_Line;

   P ("   --  There are no character literals corresponding to the positions");
   P ("   --  for control characters. They are indicated by lower case");
   P ("   --  identifiers in the following list.");
   New_Line;

   P ("   --  Note: this type cannot be represented accurately in Ada");
   New_Line;

   P ("   --  type Character is");
   New_Line;

   P ("   --    (nul,  soh,  stx,  etx,     eot,  enq,  ack,  bel,");
   P ("   --     bs,   ht,   lf,   vt,      ff,   cr,   so,   si,");
   New_Line;

   P ("   --     dle,  dc1,  dc2,  dc3,     dc4,  nak,  syn,  etb,");
   P ("   --     can,  em,   sub,  esc,     fs,   gs,   rs,   us,");
   New_Line;

   P ("   --     ' ',  '!',  '""', '#',     '$',  '%',  '&',  ''',");
   P ("   --     '(',  ')',  '*',  '+',     ',',  '-',  '.',  '/',");
   New_Line;

   P ("   --     '0',  '1',  '2',  '3',     '4',  '5',  '6',  '7',");
   P ("   --     '8',  '9',  ':',  ';',     '<',  '=',  '>',  '?',");
   New_Line;

   P ("   --     '@',  'A',  'B',  'C',     'D',  'E',  'F',  'G',");
   P ("   --     'H',  'I',  'J',  'K',     'L',  'M',  'N',  'O',");
   New_Line;

   P ("   --     'P',  'Q',  'R',  'S',     'T',  'U',  'V',  'W',");
   P ("   --     'X',  'Y',  'Z',  '[',     '\',  ']',  '^',  '_',");
   New_Line;

   P ("   --     '`',  'a',  'b',  'c',     'd',  'e',  'f',  'g',");
   P ("   --     'h',  'i',  'j',  'k',     'l',  'm',  'n',  'o',");
   New_Line;

   P ("   --     'p',  'q',  'r',  's',     't',  'u',  'v',  'w',");
   P ("   --     'x',  'y',  'z',  '{',     '|',  '}',  '~',  del,");
   New_Line;

   P ("   --     reserved_128,     reserved_129,  bph,  nbh,");
   P ("   --     reserved_132,     nel,     ssa,  esa,");
   New_Line;

   P ("   --     hts,  htj,  vts,  pld,     plu,  ri,   ss2,  ss3,");
   New_Line;

   P ("   --     dcs,  pu1,  pu2,  sts,     cch,  mw,   spa,  epa,");
   New_Line;

   P ("   --     sos, reserved_153, sci, csi,");
   P ("   --     st,   osc,  pm,   apc,");
   New_Line;

   P ("   --   ... );");
   New_Line;

   P ("   --  The declaration of type Wide_Character is based " &
                                                        "on the standard");
   P ("   --  ISO 10646 BMP character set.");
   New_Line;

   P ("   --  Note: this type cannot be represented accurately in Ada");
   New_Line;

   P ("   --  The first 256 positions have the same contents as " &
                                                        "type Character");
   New_Line;

   P ("   --  type Wide_Character is (nul, soh ... FFFE, FFFF);");
   New_Line;

   P ("   package ASCII is");
   New_Line;

   P ("      --  Control characters:");
   New_Line;

   P ("      NUL   : constant Character := Character'Val (16#00#);");
   P ("      SOH   : constant Character := Character'Val (16#01#);");
   P ("      STX   : constant Character := Character'Val (16#02#);");
   P ("      ETX   : constant Character := Character'Val (16#03#);");
   P ("      EOT   : constant Character := Character'Val (16#04#);");
   P ("      ENQ   : constant Character := Character'Val (16#05#);");
   P ("      ACK   : constant Character := Character'Val (16#06#);");
   P ("      BEL   : constant Character := Character'Val (16#07#);");
   P ("      BS    : constant Character := Character'Val (16#08#);");
   P ("      HT    : constant Character := Character'Val (16#09#);");
   P ("      LF    : constant Character := Character'Val (16#0A#);");
   P ("      VT    : constant Character := Character'Val (16#0B#);");
   P ("      FF    : constant Character := Character'Val (16#0C#);");
   P ("      CR    : constant Character := Character'Val (16#0D#);");
   P ("      SO    : constant Character := Character'Val (16#0E#);");
   P ("      SI    : constant Character := Character'Val (16#0F#);");
   P ("      DLE   : constant Character := Character'Val (16#10#);");
   P ("      DC1   : constant Character := Character'Val (16#11#);");
   P ("      DC2   : constant Character := Character'Val (16#12#);");
   P ("      DC3   : constant Character := Character'Val (16#13#);");
   P ("      DC4   : constant Character := Character'Val (16#14#);");
   P ("      NAK   : constant Character := Character'Val (16#15#);");
   P ("      SYN   : constant Character := Character'Val (16#16#);");
   P ("      ETB   : constant Character := Character'Val (16#17#);");
   P ("      CAN   : constant Character := Character'Val (16#18#);");
   P ("      EM    : constant Character := Character'Val (16#19#);");
   P ("      SUB   : constant Character := Character'Val (16#1A#);");
   P ("      ESC   : constant Character := Character'Val (16#1B#);");
   P ("      FS    : constant Character := Character'Val (16#1C#);");
   P ("      GS    : constant Character := Character'Val (16#1D#);");
   P ("      RS    : constant Character := Character'Val (16#1E#);");
   P ("      US    : constant Character := Character'Val (16#1F#);");
   P ("      DEL   : constant Character := Character'Val (16#7F#);");
   New_Line;

   P ("      -- Other characters:");
   New_Line;

   P ("      Exclam     : constant Character := '!';");
   P ("      Quotation  : constant Character := '""';");
   P ("      Sharp      : constant Character := '#';");
   P ("      Dollar     : constant Character := '$';");
   P ("      Percent    : constant Character := '%';");
   P ("      Ampersand  : constant Character := '&';");
   P ("      Colon      : constant Character := ':';");
   P ("      Semicolon  : constant Character := ';';");
   P ("      Query      : constant Character := '?';");
   P ("      At_Sign    : constant Character := '@';");
   P ("      L_Bracket  : constant Character := '[';");
   P ("      Back_Slash : constant Character := '\';");
   P ("      R_Bracket  : constant Character := ']';");
   P ("      Circumflex : constant Character := '^';");
   P ("      Underline  : constant Character := '_';");
   P ("      Grave      : constant Character := '`';");
   P ("      L_Brace    : constant Character := '{';");
   P ("      Bar        : constant Character := '|';");
   P ("      R_Brace    : constant Character := '}';");
   P ("      Tilde      : constant Character := '~';");
   New_Line;

   P ("      -- Lower case letters:");
   New_Line;

   for C in Character range 'a' .. 'z' loop
      P ("      LC_" & Character'Val (Character'Pos (C) - 32) &
                  " : constant Character := '" & C & "';");
   end loop;
   New_Line;

   P ("   end ASCII;");
   New_Line;

   P ("   type String is array (Positive range <>) of Character;");
   P ("   pragma Pack (String);");
   New_Line;

   P ("   type Wide_String is array (Positive range <>) of Wide_Character;");
   P ("   pragma Pack (Wide_String);");
   New_Line;

   --  Here it's OK to use the Duration type of the host compiler since
   --  the implementation of Duration in GNAT is target independent.

   P ("   type Duration is delta" &
            Duration'Image (Duration'Delta));
   P ("     range -((2 **" & Natural'Image (Duration'Size - 1) &
              " - 1) *" & Duration'Image (Duration'Delta) & ") ..");
   P ("           +((2 **" & Natural'Image (Duration'Size - 1) &
              " - 1) *" & Duration'Image (Duration'Delta) & ");");
   P ("   for Duration'Small use" & Duration'Image (Duration'Small) & ";");
   New_Line;

   P ("   Constraint_Error : exception;");
   P ("   Program_Error    : exception;");
   P ("   Storage_Error    : exception;");
   P ("   Tasking_Error    : exception;");
   New_Line;

   P ("end Standard;");
end GnatPsta;
