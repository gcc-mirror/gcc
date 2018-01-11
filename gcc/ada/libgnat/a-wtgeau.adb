------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--         A D A . W I D E _ T E X T _ I O . G E N E R I C _ A U X          --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2018, Free Software Foundation, Inc.         --
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

with Interfaces.C_Streams; use Interfaces.C_Streams;
with System.File_IO;
with System.File_Control_Block;

package body Ada.Wide_Text_IO.Generic_Aux is

   package FIO renames System.File_IO;
   package FCB renames System.File_Control_Block;
   subtype AP is FCB.AFCB_Ptr;

   ------------------------
   -- Check_End_Of_Field --
   ------------------------

   procedure Check_End_Of_Field
     (Buf   : String;
      Stop  : Integer;
      Ptr   : Integer;
      Width : Field)
   is
   begin
      if Ptr > Stop then
         return;

      elsif Width = 0 then
         raise Data_Error;

      else
         for J in Ptr .. Stop loop
            if not Is_Blank (Buf (J)) then
               raise Data_Error;
            end if;
         end loop;
      end if;
   end Check_End_Of_Field;

   -----------------------
   -- Check_On_One_Line --
   -----------------------

   procedure Check_On_One_Line
     (File   : File_Type;
      Length : Integer)
   is
   begin
      FIO.Check_Write_Status (AP (File));

      if File.Line_Length /= 0 then
         if Count (Length) > File.Line_Length then
            raise Layout_Error;
         elsif File.Col + Count (Length) > File.Line_Length + 1 then
            New_Line (File);
         end if;
      end if;
   end Check_On_One_Line;

   --------------
   -- Is_Blank --
   --------------

   function Is_Blank (C : Character) return Boolean is
   begin
      return C = ' ' or else C = ASCII.HT;
   end Is_Blank;

   ----------
   -- Load --
   ----------

   procedure Load
     (File   : File_Type;
      Buf    : out String;
      Ptr    : in out Integer;
      Char   : Character;
      Loaded : out Boolean)
   is
      ch : int;

   begin
      if File.Before_Wide_Character then
         Loaded := False;
         return;

      else
         ch := Getc (File);

         if ch = Character'Pos (Char) then
            Store_Char (File, ch, Buf, Ptr);
            Loaded := True;
         else
            Ungetc (ch, File);
            Loaded := False;
         end if;
      end if;
   end Load;

   procedure Load
     (File   : File_Type;
      Buf    : out String;
      Ptr    : in out Integer;
      Char   : Character)
   is
      ch : int;

   begin
      if File.Before_Wide_Character then
         null;

      else
         ch := Getc (File);

         if ch = Character'Pos (Char) then
            Store_Char (File, ch, Buf, Ptr);
         else
            Ungetc (ch, File);
         end if;
      end if;
   end Load;

   procedure Load
     (File   : File_Type;
      Buf    : out String;
      Ptr    : in out Integer;
      Char1  : Character;
      Char2  : Character;
      Loaded : out Boolean)
   is
      ch : int;

   begin
      if File.Before_Wide_Character then
         Loaded := False;
         return;

      else
         ch := Getc (File);

         if ch = Character'Pos (Char1)
           or else ch = Character'Pos (Char2)
         then
            Store_Char (File, ch, Buf, Ptr);
            Loaded := True;
         else
            Ungetc (ch, File);
            Loaded := False;
         end if;
      end if;
   end Load;

   procedure Load
     (File   : File_Type;
      Buf    : out String;
      Ptr    : in out Integer;
      Char1  : Character;
      Char2  : Character)
   is
      ch : int;

   begin
      if File.Before_Wide_Character then
         null;

      else
         ch := Getc (File);

         if ch = Character'Pos (Char1)
           or else ch = Character'Pos (Char2)
         then
            Store_Char (File, ch, Buf, Ptr);
         else
            Ungetc (ch, File);
         end if;
      end if;
   end Load;

   -----------------
   -- Load_Digits --
   -----------------

   procedure Load_Digits
     (File   : File_Type;
      Buf    : out String;
      Ptr    : in out Integer;
      Loaded : out Boolean)
   is
      ch          : int;
      After_Digit : Boolean;

   begin
      if File.Before_Wide_Character then
         Loaded := False;
         return;

      else
         ch := Getc (File);

         if ch not in Character'Pos ('0') .. Character'Pos ('9') then
            Loaded := False;

         else
            Loaded := True;
            After_Digit := True;

            loop
               Store_Char (File, ch, Buf, Ptr);
               ch := Getc (File);

               if ch in Character'Pos ('0') .. Character'Pos ('9') then
                  After_Digit := True;

               elsif ch = Character'Pos ('_') and then After_Digit then
                  After_Digit := False;

               else
                  exit;
               end if;
            end loop;
         end if;

         Ungetc (ch, File);
      end if;
   end Load_Digits;

   procedure Load_Digits
     (File   : File_Type;
      Buf    : out String;
      Ptr    : in out Integer)
   is
      ch          : int;
      After_Digit : Boolean;

   begin
      if File.Before_Wide_Character then
         return;

      else
         ch := Getc (File);

         if ch in Character'Pos ('0') .. Character'Pos ('9') then
            After_Digit := True;

            loop
               Store_Char (File, ch, Buf, Ptr);
               ch := Getc (File);

               if ch in Character'Pos ('0') .. Character'Pos ('9') then
                  After_Digit := True;

               elsif ch = Character'Pos ('_') and then After_Digit then
                  After_Digit := False;

               else
                  exit;
               end if;
            end loop;
         end if;

         Ungetc (ch, File);
      end if;
   end Load_Digits;

   --------------------------
   -- Load_Extended_Digits --
   --------------------------

   procedure Load_Extended_Digits
     (File   : File_Type;
      Buf    : out String;
      Ptr    : in out Integer;
      Loaded : out Boolean)
   is
      ch          : int;
      After_Digit : Boolean := False;

   begin
      if File.Before_Wide_Character then
         Loaded := False;
         return;

      else
         Loaded := False;

         loop
            ch := Getc (File);

            if ch in Character'Pos ('0') .. Character'Pos ('9')
                 or else
               ch in Character'Pos ('a') .. Character'Pos ('f')
                 or else
               ch in Character'Pos ('A') .. Character'Pos ('F')
            then
               After_Digit := True;

            elsif ch = Character'Pos ('_') and then After_Digit then
               After_Digit := False;

            else
               exit;
            end if;

            Store_Char (File, ch, Buf, Ptr);
            Loaded := True;
         end loop;

         Ungetc (ch, File);
      end if;
   end Load_Extended_Digits;

   procedure Load_Extended_Digits
     (File   : File_Type;
      Buf    : out String;
      Ptr    : in out Integer)
   is
      Junk : Boolean;
      pragma Unreferenced (Junk);
   begin
      Load_Extended_Digits (File, Buf, Ptr, Junk);
   end Load_Extended_Digits;

   ---------------
   -- Load_Skip --
   ---------------

   procedure Load_Skip (File  : File_Type) is
      C : Character;

   begin
      FIO.Check_Read_Status (AP (File));

      --  We need to explicitly test for the case of being before a wide
      --  character (greater than 16#7F#). Since no such character can
      --  ever legitimately be a valid numeric character, we can
      --  immediately signal Data_Error.

      if File.Before_Wide_Character then
         raise Data_Error;
      end if;

      --  Otherwise loop till we find a non-blank character (note that as
      --  usual in Wide_Text_IO, blank includes horizontal tab). Note that
      --  Get_Character deals with Before_LM/Before_LM_PM flags appropriately.

      loop
         Get_Character (File, C);
         exit when not Is_Blank (C);
      end loop;

      Ungetc (Character'Pos (C), File);
      File.Col := File.Col - 1;
   end Load_Skip;

   ----------------
   -- Load_Width --
   ----------------

   procedure Load_Width
     (File  : File_Type;
      Width : Field;
      Buf   : out String;
      Ptr   : in out Integer)
   is
      ch : int;
      WC : Wide_Character;

      Bad_Wide_C : Boolean := False;
      --  Set True if one of the characters read is not in range of type
      --  Character. This is always a Data_Error, but we do not signal it
      --  right away, since we have to read the full number of characters.

   begin
      FIO.Check_Read_Status (AP (File));

      --  If we are immediately before a line mark, then we have no characters.
      --  This is always a data error, so we may as well raise it right away.

      if File.Before_LM then
         raise Data_Error;

      else
         for J in 1 .. Width loop
            if File.Before_Wide_Character then
               Bad_Wide_C := True;
               Store_Char (File, 0, Buf, Ptr);
               File.Before_Wide_Character := False;

            else
               ch := Getc (File);

               if ch = EOF then
                  exit;

               elsif ch = LM then
                  Ungetc (ch, File);
                  exit;

               else
                  WC := Get_Wide_Char (Character'Val (ch), File);
                  ch := Wide_Character'Pos (WC);

                  if ch > 255 then
                     Bad_Wide_C := True;
                     ch := 0;
                  end if;

                  Store_Char (File, ch, Buf, Ptr);
               end if;
            end if;
         end loop;

         if Bad_Wide_C then
            raise Data_Error;
         end if;
      end if;
   end Load_Width;

   --------------
   -- Put_Item --
   --------------

   procedure Put_Item (File : File_Type; Str : String) is
   begin
      Check_On_One_Line (File, Str'Length);

      for J in Str'Range loop
         Put (File, Wide_Character'Val (Character'Pos (Str (J))));
      end loop;
   end Put_Item;

   ----------------
   -- Store_Char --
   ----------------

   procedure Store_Char
     (File : File_Type;
      ch   : Integer;
      Buf  : out String;
      Ptr  : in out Integer)
   is
   begin
      File.Col := File.Col + 1;

      if Ptr = Buf'Last then
         raise Data_Error;
      else
         Ptr := Ptr + 1;
         Buf (Ptr) := Character'Val (ch);
      end if;
   end Store_Char;

   -----------------
   -- String_Skip --
   -----------------

   procedure String_Skip (Str : String; Ptr : out Integer) is
   begin
      --  Routines calling String_Skip malfunction if Str'Last = Positive'Last.
      --  It's too much trouble to make this silly case work, so we just raise
      --  Program_Error with an appropriate message. We raise Program_Error
      --  rather than Constraint_Error because we don't want this case to be
      --  converted to Data_Error.

      if Str'Last = Positive'Last then
         raise Program_Error with
           "string upper bound is Positive'Last, not supported";
      end if;

      --  Normal case where Str'Last < Positive'Last

      Ptr := Str'First;

      loop
         if Ptr > Str'Last then
            raise End_Error;

         elsif not Is_Blank (Str (Ptr)) then
            return;

         else
            Ptr := Ptr + 1;
         end if;
      end loop;
   end String_Skip;

   ------------
   -- Ungetc --
   ------------

   procedure Ungetc (ch : int; File : File_Type) is
   begin
      if ch /= EOF then
         if ungetc (ch, File.Stream) = EOF then
            raise Device_Error;
         end if;
      end if;
   end Ungetc;

end Ada.Wide_Text_IO.Generic_Aux;
