------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--          A D A . T E X T _ I O . E N U M E R A T I O N _ A U X           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2025, Free Software Foundation, Inc.         --
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

with Ada.Text_IO.Generic_Aux; use Ada.Text_IO.Generic_Aux;
with Ada.Characters.Handling; use Ada.Characters.Handling;

--  Note: this package does not yet deal properly with wide characters ???

package body Ada.Text_IO.Enumeration_Aux is

   ------------------
   -- Get_Enum_Lit --
   ------------------

   procedure Get_Enum_Lit
     (File   : File_Type;
      Buf    : out String;
      Buflen : out Natural)
   is
      ch  : Integer;
      C   : Character;

   begin
      Buflen := 0;
      Load_Skip (File);
      ch := Getc (File);
      C := Character'Val (ch);

      --  Character literal case. If the initial character is a quote, then
      --  we read as far as we can without backup (see ACVC test CE3905L)

      if C = ''' then
         Store_Char (File, ch, Buf, Buflen);

         ch := Getc (File);

         if ch in 16#20# .. 16#7E# or else ch >= 16#80# then
            Store_Char (File, ch, Buf, Buflen);

            ch := Getc (File);

            if ch = Character'Pos (''') then
               Store_Char (File, ch, Buf, Buflen);
            else
               Ungetc (ch, File);
            end if;

         else
            Ungetc (ch, File);
         end if;

      --  Similarly for identifiers, read as far as we can, in particular,
      --  do read a trailing underscore (again see ACVC test CE3905L to
      --  understand why we do this, although it seems somewhat peculiar).

      else
         --  Identifier must start with a letter

         if not Is_Letter (C) then
            Ungetc (ch, File);
            return;
         end if;

         --  If we do have a letter, loop through the characters quitting on
         --  the first non-identifier character (note that this includes the
         --  cases of hitting a line mark or page mark).

         loop
            C := Character'Val (ch);
            Store_Char (File, Character'Pos (To_Upper (C)), Buf, Buflen);

            ch := Getc (File);
            exit when ch = EOF_Char;
            C := Character'Val (ch);

            exit when not Is_Letter (C)
              and then not Is_Digit (C)
              and then C /= '_';

            exit when C = '_'
              and then Buf (Buflen) = '_';
         end loop;

         Ungetc (ch, File);
      end if;
   end Get_Enum_Lit;

   ---------
   -- Put --
   ---------

   procedure Put
     (File  : File_Type;
      Item  : String;
      Width : Field;
      Set   : Type_Set)
   is
      Actual_Width : constant Count := Count'Max (Count (Width), Item'Length);

   begin
      --  Deal with limited line length of output file

      if Line_Length (File) /= 0 then

         --  If actual width exceeds line length, raise Layout_Error

         if Actual_Width > Line_Length (File) then
            raise Layout_Error;
         end if;

         --  If full width cannot fit on current line move to new line

         if Actual_Width + (Col (File) - 1) > Line_Length (File) then
            New_Line (File);
         end if;
      end if;

      --  Output in lower case if necessary

      if Set = Lower_Case and then Item (Item'First) /= ''' then
         declare
            Iteml : String (Item'First .. Item'Last);

         begin
            for J in Item'Range loop
               Iteml (J) := To_Lower (Item (J));
            end loop;

            Put_Item (File, Iteml);
         end;

      --  Otherwise output in upper case

      else
         Put_Item (File, Item);
      end if;

      --  Fill out item with spaces to width

      for J in 1 .. Actual_Width - Item'Length loop
         Put (File, ' ');
      end loop;
   end Put;

   ----------
   -- Puts --
   ----------

   procedure Puts
     (To   : out String;
      Item : String;
      Set  : Type_Set)
   is
      Ptr : Natural;

   begin
      if Item'Length > To'Length then
         raise Layout_Error;

      else
         Ptr := To'First;
         for J in Item'Range loop
            if Set = Lower_Case and then Item (Item'First) /= ''' then
               To (Ptr) := To_Lower (Item (J));
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
     (From  : String;
      Start : out Natural;
      Stop  : out Natural)
   is
      C : Character;

   --  Processing for Scan_Enum_Lit

   begin
      String_Skip (From, Start);

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
           or else From (Stop) >= Character'Val (16#80#)
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
         --  Identifier must start with a letter

         if not Is_Letter (From (Start)) then
            raise Data_Error;
         end if;

         --  If we do have a letter, loop through the characters quitting on
         --  the first non-identifier character (note that this includes the
         --  cases of hitting a line mark or page mark).

         Stop := Start;
         while Stop < From'Last loop
            C := From (Stop + 1);

            exit when not Is_Letter (C)
              and then not Is_Digit (C)
              and then C /= '_';

            exit when C = '_'
              and then From (Stop) = '_';

            Stop := Stop + 1;
         end loop;
      end if;
   end Scan_Enum_Lit;

end Ada.Text_IO.Enumeration_Aux;
