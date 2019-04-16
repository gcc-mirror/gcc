------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--      A D A . W I D E _ T E X T _ I O . E N U M E R A T I O N _ A U X     --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2019, Free Software Foundation, Inc.         --
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

with Ada.Wide_Text_IO.Generic_Aux; use Ada.Wide_Text_IO.Generic_Aux;
with Ada.Characters.Handling;      use Ada.Characters.Handling;
with Interfaces.C_Streams;         use Interfaces.C_Streams;
with System.WCh_Con;               use System.WCh_Con;

package body Ada.Wide_Text_IO.Enumeration_Aux is

   subtype TFT is Ada.Wide_Text_IO.File_Type;
   --  File type required for calls to routines in Aux

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Store_Char
     (WC  : Wide_Character;
      Buf : out Wide_String;
      Ptr : in out Integer);
   --  Store a single character in buffer, checking for overflow

   --  These definitions replace the ones in Ada.Characters.Handling, which
   --  do not seem to work for some strange not understood reason ??? at
   --  least in the OS/2 version.

   function To_Lower (C : Character) return Character;

   ------------------
   -- Get_Enum_Lit --
   ------------------

   procedure Get_Enum_Lit
     (File   : File_Type;
      Buf    : out Wide_String;
      Buflen : out Natural)
   is
      ch  : int;
      WC  : Wide_Character;

   begin
      Buflen := 0;
      Load_Skip (TFT (File));
      ch := Nextc (TFT (File));

      --  Character literal case. If the initial character is a quote, then
      --  we read as far as we can without backup (see ACVC test CE3905L)

      if ch = Character'Pos (''') then
         Get (File, WC);
         Store_Char (WC, Buf, Buflen);

         ch := Nextc (TFT (File));

         if ch = LM or else ch = EOF then
            return;
         end if;

         Get (File, WC);
         Store_Char (WC, Buf, Buflen);

         ch := Nextc (TFT (File));

         if ch /= Character'Pos (''') then
            return;
         end if;

         Get (File, WC);
         Store_Char (WC, Buf, Buflen);

      --  Similarly for identifiers, read as far as we can, in particular,
      --  do read a trailing underscore (again see ACVC test CE3905L to
      --  understand why we do this, although it seems somewhat peculiar).

      else
         --  Identifier must start with a letter. Any wide character value
         --  outside the normal Latin-1 range counts as a letter for this.

         if ch < 255 and then not Is_Letter (Character'Val (ch)) then
            return;
         end if;

         --  If we do have a letter, loop through the characters quitting on
         --  the first non-identifier character (note that this includes the
         --  cases of hitting a line mark or page mark).

         loop
            Get (File, WC);
            Store_Char (WC, Buf, Buflen);

            ch := Nextc (TFT (File));

            exit when ch = EOF;

            if ch = Character'Pos ('_') then
               exit when Buf (Buflen) = '_';

            elsif ch = Character'Pos (ASCII.ESC) then
               null;

            elsif File.WC_Method in WC_Upper_Half_Encoding_Method
              and then ch > 127
            then
               null;

            else
               exit when not Is_Letter (Character'Val (ch))
                           and then
                         not Is_Digit (Character'Val (ch));
            end if;
         end loop;
      end if;
   end Get_Enum_Lit;

   ---------
   -- Put --
   ---------

   procedure Put
     (File  : File_Type;
      Item  : Wide_String;
      Width : Field;
      Set   : Type_Set)
   is
      Actual_Width : constant Integer :=
        Integer'Max (Integer (Width), Item'Length);

   begin
      Check_On_One_Line (TFT (File), Actual_Width);

      if Set = Lower_Case and then Item (Item'First) /= ''' then
         declare
            Iteml : Wide_String (Item'First .. Item'Last);

         begin
            for J in Item'Range loop
               if Is_Character (Item (J)) then
                  Iteml (J) :=
                    To_Wide_Character (To_Lower (To_Character (Item (J))));
               else
                  Iteml (J) := Item (J);
               end if;
            end loop;

            Put (File, Iteml);
         end;

      else
         Put (File, Item);
      end if;

      for J in 1 .. Actual_Width - Item'Length loop
         Put (File, ' ');
      end loop;
   end Put;

   ----------
   -- Puts --
   ----------

   procedure Puts
     (To   : out Wide_String;
      Item : Wide_String;
      Set  : Type_Set)
   is
      Ptr : Natural;

   begin
      if Item'Length > To'Length then
         raise Layout_Error;

      else
         Ptr := To'First;
         for J in Item'Range loop
            if Set = Lower_Case
              and then Item (Item'First) /= '''
              and then Is_Character (Item (J))
            then
               To (Ptr) :=
                 To_Wide_Character (To_Lower (To_Character (Item (J))));
            else
               To (Ptr) := Item (J);
            end if;

            Ptr := Ptr + 1;
         end loop;

         while Ptr <= To'Last loop
            To (Ptr) := ' ';
            Ptr := Ptr + 1;
         end loop;
      end if;
   end Puts;

   -------------------
   -- Scan_Enum_Lit --
   -------------------

   procedure Scan_Enum_Lit
     (From  : Wide_String;
      Start : out Natural;
      Stop  : out Natural)
   is
      WC  : Wide_Character;

   --  Processing for Scan_Enum_Lit

   begin
      Start := From'First;

      loop
         if Start > From'Last then
            raise End_Error;

         elsif Is_Character (From (Start))
           and then not Is_Blank (To_Character (From (Start)))
         then
            exit;

         else
            Start := Start + 1;
         end if;
      end loop;

      --  Character literal case. If the initial character is a quote, then
      --  we read as far as we can without backup (see ACVC test CE3905L
      --  which is for the analogous case for reading from a file).

      if From (Start) = ''' then
         Stop := Start;

         if Stop = From'Last then
            raise Data_Error;
         else
            Stop := Stop + 1;
         end if;

         if From (Stop) in ' ' .. '~'
           or else From (Stop) >= Wide_Character'Val (16#80#)
         then
            if Stop = From'Last then
               raise Data_Error;
            else
               Stop := Stop + 1;

               if From (Stop) = ''' then
                  return;
               end if;
            end if;
         end if;

         raise Data_Error;

      --  Similarly for identifiers, read as far as we can, in particular,
      --  do read a trailing underscore (again see ACVC test CE3905L to
      --  understand why we do this, although it seems somewhat peculiar).

      else
         --  Identifier must start with a letter, any wide character outside
         --  the normal Latin-1 range is considered a letter for this test.

         if Is_Character (From (Start))
           and then not Is_Letter (To_Character (From (Start)))
         then
            raise Data_Error;
         end if;

         --  If we do have a letter, loop through the characters quitting on
         --  the first non-identifier character (note that this includes the
         --  cases of hitting a line mark or page mark).

         Stop := Start + 1;
         while Stop < From'Last loop
            WC := From (Stop + 1);

            exit when
              Is_Character (WC)
                and then
                  not Is_Letter (To_Character (WC))
                and then
                  (WC /= '_' or else From (Stop - 1) = '_');

            Stop := Stop + 1;
         end loop;
      end if;

   end Scan_Enum_Lit;

   ----------------
   -- Store_Char --
   ----------------

   procedure Store_Char
     (WC  : Wide_Character;
      Buf : out Wide_String;
      Ptr : in out Integer)
   is
   begin
      if Ptr = Buf'Last then
         raise Data_Error;
      else
         Ptr := Ptr + 1;
         Buf (Ptr) := WC;
      end if;
   end Store_Char;

   --------------
   -- To_Lower --
   --------------

   function To_Lower (C : Character) return Character is
   begin
      if C in 'A' .. 'Z' then
         return Character'Val (Character'Pos (C) + 32);
      else
         return C;
      end if;
   end To_Lower;

end Ada.Wide_Text_IO.Enumeration_Aux;
