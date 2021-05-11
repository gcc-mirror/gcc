------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                N A M E T                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2021, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  WARNING: There is a C version of this package. Any changes to this
--  source file must be properly reflected in the C header file namet.h
--  which is created manually from namet.ads and namet.adb.

with Debug;  use Debug;
with Opt;    use Opt;
with Output; use Output;
with Widechar;

with Interfaces; use Interfaces;

package body Namet is

   Name_Chars_Reserve   : constant := 5000;
   Name_Entries_Reserve : constant := 100;
   --  The names table is locked during gigi processing, since gigi assumes
   --  that the table does not move. After returning from gigi, the names
   --  table is unlocked again, since writing library file information needs
   --  to generate some extra names. To avoid the inefficiency of always
   --  reallocating during this second unlocked phase, we reserve a bit of
   --  extra space before doing the release call.

   Hash_Num : constant Int := 2**16;
   --  Number of headers in the hash table. Current hash algorithm is closely
   --  tailored to this choice, so it can only be changed if a corresponding
   --  change is made to the hash algorithm.

   Hash_Max : constant Int := Hash_Num - 1;
   --  Indexes in the hash header table run from 0 to Hash_Num - 1

   subtype Hash_Index_Type is Int range 0 .. Hash_Max;
   --  Range of hash index values

   Hash_Table : array (Hash_Index_Type) of Name_Id;
   --  The hash table is used to locate existing entries in the names table.
   --  The entries point to the first names table entry whose hash value
   --  matches the hash code. Then subsequent names table entries with the
   --  same hash code value are linked through the Hash_Link fields.

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Hash (Buf : Bounded_String) return Hash_Index_Type;
   pragma Inline (Hash);
   --  Compute hash code for name stored in Buf

   procedure Strip_Qualification_And_Suffixes (Buf : in out Bounded_String);
   --  Given an encoded entity name in Buf, remove package body
   --  suffix as described for Strip_Package_Body_Suffix, and also remove
   --  all qualification, i.e. names followed by two underscores.

   -----------------------------
   -- Add_Char_To_Name_Buffer --
   -----------------------------

   procedure Add_Char_To_Name_Buffer (C : Character) is
   begin
      Append (Global_Name_Buffer, C);
   end Add_Char_To_Name_Buffer;

   ----------------------------
   -- Add_Nat_To_Name_Buffer --
   ----------------------------

   procedure Add_Nat_To_Name_Buffer (V : Nat) is
   begin
      Append (Global_Name_Buffer, V);
   end Add_Nat_To_Name_Buffer;

   ----------------------------
   -- Add_Str_To_Name_Buffer --
   ----------------------------

   procedure Add_Str_To_Name_Buffer (S : String) is
   begin
      Append (Global_Name_Buffer, S);
   end Add_Str_To_Name_Buffer;

   ------------
   -- Append --
   ------------

   procedure Append (Buf : in out Bounded_String; C : Character) is
   begin
      Buf.Length := Buf.Length + 1;

      if Buf.Length > Buf.Chars'Last then
         Write_Str ("Name buffer overflow; Max_Length = ");
         Write_Int (Int (Buf.Max_Length));
         Write_Line ("");
         raise Program_Error;
      end if;

      Buf.Chars (Buf.Length) := C;
   end Append;

   procedure Append (Buf : in out Bounded_String; V : Nat) is
   begin
      if V >= 10 then
         Append (Buf, V / 10);
      end if;

      Append (Buf, Character'Val (Character'Pos ('0') + V rem 10));
   end Append;

   procedure Append (Buf : in out Bounded_String; S : String) is
      First : constant Natural := Buf.Length + 1;
   begin
      Buf.Length := Buf.Length + S'Length;

      if Buf.Length > Buf.Chars'Last then
         Write_Str ("Name buffer overflow; Max_Length = ");
         Write_Int (Int (Buf.Max_Length));
         Write_Line ("");
         raise Program_Error;
      end if;

      Buf.Chars (First .. Buf.Length) := S;
      --  A loop calling Append(Character) would be cleaner, but this slice
      --  assignment is substantially faster.
   end Append;

   procedure Append (Buf : in out Bounded_String; Buf2 : Bounded_String) is
   begin
      Append (Buf, Buf2.Chars (1 .. Buf2.Length));
   end Append;

   procedure Append (Buf : in out Bounded_String; Id : Valid_Name_Id) is
      pragma Assert (Is_Valid_Name (Id));

      Index : constant Int   := Name_Entries.Table (Id).Name_Chars_Index;
      Len   : constant Short := Name_Entries.Table (Id).Name_Len;
      Chars : Name_Chars.Table_Type renames
                Name_Chars.Table (Index + 1 .. Index + Int (Len));
   begin
      Append (Buf, String (Chars));
   end Append;

   --------------------
   -- Append_Decoded --
   --------------------

   procedure Append_Decoded
     (Buf : in out Bounded_String;
      Id  : Valid_Name_Id)
   is
      C    : Character;
      P    : Natural;
      Temp : Bounded_String;

   begin
      Append (Temp, Id);

      --  Skip scan if we already know there are no encodings

      if Name_Entries.Table (Id).Name_Has_No_Encodings then
         goto Done;
      end if;

      --  Quick loop to see if there is anything special to do

      P := 1;
      loop
         if P = Temp.Length then
            Name_Entries.Table (Id).Name_Has_No_Encodings := True;
            goto Done;

         else
            C := Temp.Chars (P);

            exit when
              C = 'U' or else
              C = 'W' or else
              C = 'Q' or else
              C = 'O';

            P := P + 1;
         end if;
      end loop;

      --  Here we have at least some encoding that we must decode

      Decode : declare
         New_Len : Natural;
         Old     : Positive;
         New_Buf : String (1 .. Temp.Chars'Last);

         procedure Copy_One_Character;
         --  Copy a character from Temp.Chars to New_Buf. Includes case
         --  of copying a Uhh,Whhhh,WWhhhhhhhh sequence and decoding it.

         function Hex (N : Natural) return Word;
         --  Scans past N digits using Old pointer and returns hex value

         procedure Insert_Character (C : Character);
         --  Insert a new character into output decoded name

         ------------------------
         -- Copy_One_Character --
         ------------------------

         procedure Copy_One_Character is
            C : Character;

         begin
            C := Temp.Chars (Old);

            --  U (upper half insertion case)

            if C = 'U'
              and then Old < Temp.Length
              and then Temp.Chars (Old + 1) not in 'A' .. 'Z'
              and then Temp.Chars (Old + 1) /= '_'
            then
               Old := Old + 1;

               --  If we have upper half encoding, then we have to set an
               --  appropriate wide character sequence for this character.

               if Upper_Half_Encoding then
                  Widechar.Set_Wide (Char_Code (Hex (2)), New_Buf, New_Len);

                  --  For other encoding methods, upper half characters can
                  --  simply use their normal representation.

               else
                  declare
                     W2 : constant Word := Hex (2);
                  begin
                     pragma Assert (W2 <= 255);
                     --  Add assumption to facilitate static analysis. Note
                     --  that we cannot use pragma Assume for bootstrap
                     --  reasons.
                     Insert_Character (Character'Val (W2));
                  end;
               end if;

            --  WW (wide wide character insertion)

            elsif C = 'W'
              and then Old < Temp.Length
              and then Temp.Chars (Old + 1) = 'W'
            then
               Old := Old + 2;
               Widechar.Set_Wide (Char_Code (Hex (8)), New_Buf, New_Len);

            --  W (wide character insertion)

            elsif C = 'W'
              and then Old < Temp.Length
              and then Temp.Chars (Old + 1) not in 'A' .. 'Z'
              and then Temp.Chars (Old + 1) /= '_'
            then
               Old := Old + 1;
               Widechar.Set_Wide (Char_Code (Hex (4)), New_Buf, New_Len);

            --  Any other character is copied unchanged

            else
               Insert_Character (C);
               Old := Old + 1;
            end if;
         end Copy_One_Character;

         ---------
         -- Hex --
         ---------

         function Hex (N : Natural) return Word is
            T : Word := 0;
            C : Character;

         begin
            for J in 1 .. N loop
               C := Temp.Chars (Old);
               Old := Old + 1;

               pragma Assert (C in '0' .. '9' or else C in 'a' .. 'f');

               if C <= '9' then
                  T := 16 * T + Character'Pos (C) - Character'Pos ('0');
               else -- C in 'a' .. 'f'
                  T := 16 * T + Character'Pos (C) - (Character'Pos ('a') - 10);
               end if;
            end loop;

            return T;
         end Hex;

         ----------------------
         -- Insert_Character --
         ----------------------

         procedure Insert_Character (C : Character) is
         begin
            New_Len := New_Len + 1;
            New_Buf (New_Len) := C;
         end Insert_Character;

      --  Start of processing for Decode

      begin
         New_Len := 0;
         Old := 1;

         --  Loop through characters of name

         while Old <= Temp.Length loop

            --  Case of character literal, put apostrophes around character

            if Temp.Chars (Old) = 'Q'
              and then Old < Temp.Length
            then
               Old := Old + 1;
               Insert_Character (''');
               Copy_One_Character;
               Insert_Character (''');

            --  Case of operator name

            elsif Temp.Chars (Old) = 'O'
              and then Old < Temp.Length
              and then Temp.Chars (Old + 1) not in 'A' .. 'Z'
              and then Temp.Chars (Old + 1) /= '_'
            then
               Old := Old + 1;

               declare
                  --  This table maps the 2nd and 3rd characters of the name
                  --  into the required output. Two blanks means leave the
                  --  name alone

                  Map : constant String :=
                     "ab  " &               --  Oabs         => "abs"
                     "ad+ " &               --  Oadd         => "+"
                     "an  " &               --  Oand         => "and"
                     "co& " &               --  Oconcat      => "&"
                     "di/ " &               --  Odivide      => "/"
                     "eq= " &               --  Oeq          => "="
                     "ex**" &               --  Oexpon       => "**"
                     "gt> " &               --  Ogt          => ">"
                     "ge>=" &               --  Oge          => ">="
                     "le<=" &               --  Ole          => "<="
                     "lt< " &               --  Olt          => "<"
                     "mo  " &               --  Omod         => "mod"
                     "mu* " &               --  Omutliply    => "*"
                     "ne/=" &               --  One          => "/="
                     "no  " &               --  Onot         => "not"
                     "or  " &               --  Oor          => "or"
                     "re  " &               --  Orem         => "rem"
                     "su- " &               --  Osubtract    => "-"
                     "xo  ";                --  Oxor         => "xor"

                  J : Integer;

               begin
                  Insert_Character ('"');

                  --  Search the map. Note that this loop must terminate, if
                  --  not we have some kind of internal error, and a constraint
                  --  error may be raised.

                  J := Map'First;
                  loop
                     exit when Temp.Chars (Old) = Map (J)
                       and then Temp.Chars (Old + 1) = Map (J + 1);
                     J := J + 4;
                  end loop;

                  --  Special operator name

                  if Map (J + 2) /= ' ' then
                     Insert_Character (Map (J + 2));

                     if Map (J + 3) /= ' ' then
                        Insert_Character (Map (J + 3));
                     end if;

                     Insert_Character ('"');

                     --  Skip past original operator name in input

                     while Old <= Temp.Length
                       and then Temp.Chars (Old) in 'a' .. 'z'
                     loop
                        Old := Old + 1;
                     end loop;

                  --  For other operator names, leave them in lower case,
                  --  surrounded by apostrophes

                  else
                     --  Copy original operator name from input to output

                     while Old <= Temp.Length
                        and then Temp.Chars (Old) in 'a' .. 'z'
                     loop
                        Copy_One_Character;
                     end loop;

                     Insert_Character ('"');
                  end if;
               end;

            --  Else copy one character and keep going

            else
               Copy_One_Character;
            end if;
         end loop;

         --  Copy new buffer as result

         Temp.Length := New_Len;
         Temp.Chars (1 .. New_Len) := New_Buf (1 .. New_Len);
      end Decode;

      <<Done>>
      Append (Buf, Temp);
   end Append_Decoded;

   ----------------------------------
   -- Append_Decoded_With_Brackets --
   ----------------------------------

   procedure Append_Decoded_With_Brackets
     (Buf : in out Bounded_String;
      Id  : Valid_Name_Id)
   is
      P : Natural;

   begin
      --  Case of operator name, normal decoding is fine

      if Buf.Chars (1) = 'O' then
         Append_Decoded (Buf, Id);

      --  For character literals, normal decoding is fine

      elsif Buf.Chars (1) = 'Q' then
         Append_Decoded (Buf, Id);

      --  Only remaining issue is U/W/WW sequences

      else
         declare
            Temp : Bounded_String;
         begin
            Append (Temp, Id);

            P := 1;
            while P < Temp.Length loop
               if Temp.Chars (P + 1) in 'A' .. 'Z' then
                  P := P + 1;

               --  Uhh encoding

               elsif Temp.Chars (P) = 'U' then
                  for J in reverse P + 3 .. P + Temp.Length loop
                     Temp.Chars (J + 3) := Temp.Chars (J);
                  end loop;

                  Temp.Length := Temp.Length + 3;
                  Temp.Chars (P + 3) := Temp.Chars (P + 2);
                  Temp.Chars (P + 2) := Temp.Chars (P + 1);
                  Temp.Chars (P)     := '[';
                  Temp.Chars (P + 1) := '"';
                  Temp.Chars (P + 4) := '"';
                  Temp.Chars (P + 5) := ']';
                  P := P + 6;

               --  WWhhhhhhhh encoding

               elsif Temp.Chars (P) = 'W'
                 and then P + 9 <= Temp.Length
                 and then Temp.Chars (P + 1) = 'W'
                 and then Temp.Chars (P + 2) not in 'A' .. 'Z'
                 and then Temp.Chars (P + 2) /= '_'
               then
                  Temp.Chars (P + 12 .. Temp.Length + 2) :=
                    Temp.Chars (P + 10 .. Temp.Length);
                  Temp.Chars (P)     := '[';
                  Temp.Chars (P + 1) := '"';
                  Temp.Chars (P + 10) := '"';
                  Temp.Chars (P + 11) := ']';
                  Temp.Length := Temp.Length + 2;
                  P := P + 12;

               --  Whhhh encoding

               elsif Temp.Chars (P) = 'W'
                 and then P < Temp.Length
                 and then Temp.Chars (P + 1) not in 'A' .. 'Z'
                 and then Temp.Chars (P + 1) /= '_'
               then
                  Temp.Chars (P + 8 .. P + Temp.Length + 3) :=
                    Temp.Chars (P + 5 .. Temp.Length);
                  Temp.Chars (P + 2 .. P + 5) := Temp.Chars (P + 1 .. P + 4);
                  Temp.Chars (P)     := '[';
                  Temp.Chars (P + 1) := '"';
                  Temp.Chars (P + 6) := '"';
                  Temp.Chars (P + 7) := ']';
                  Temp.Length := Temp.Length + 3;
                  P := P + 8;

               else
                  P := P + 1;
               end if;
            end loop;

            Append (Buf, Temp);
         end;
      end if;
   end Append_Decoded_With_Brackets;

   --------------------
   -- Append_Encoded --
   --------------------

   procedure Append_Encoded (Buf : in out Bounded_String; C : Char_Code) is
      procedure Set_Hex_Chars (C : Char_Code);
      --  Stores given value, which is in the range 0 .. 255, as two hex
      --  digits (using lower case a-f) in Buf.Chars, incrementing Buf.Length.

      -------------------
      -- Set_Hex_Chars --
      -------------------

      procedure Set_Hex_Chars (C : Char_Code) is
         Hexd : constant String := "0123456789abcdef";
         N    : constant Natural := Natural (C);
      begin
         Buf.Chars (Buf.Length + 1) := Hexd (N / 16 + 1);
         Buf.Chars (Buf.Length + 2) := Hexd (N mod 16 + 1);
         Buf.Length := Buf.Length + 2;
      end Set_Hex_Chars;

   --  Start of processing for Append_Encoded

   begin
      Buf.Length := Buf.Length + 1;

      if In_Character_Range (C) then
         declare
            CC : constant Character := Get_Character (C);
         begin
            if CC in 'a' .. 'z' or else CC in '0' .. '9' then
               Buf.Chars (Buf.Length) := CC;
            else
               Buf.Chars (Buf.Length) := 'U';
               Set_Hex_Chars (C);
            end if;
         end;

      elsif In_Wide_Character_Range (C) then
         Buf.Chars (Buf.Length) := 'W';
         Set_Hex_Chars (C / 256);
         Set_Hex_Chars (C mod 256);

      else
         Buf.Chars (Buf.Length) := 'W';
         Buf.Length := Buf.Length + 1;
         Buf.Chars (Buf.Length) := 'W';
         Set_Hex_Chars (C / 2 ** 24);
         Set_Hex_Chars ((C / 2 ** 16) mod 256);
         Set_Hex_Chars ((C / 256) mod 256);
         Set_Hex_Chars (C mod 256);
      end if;
   end Append_Encoded;

   ------------------------
   -- Append_Unqualified --
   ------------------------

   procedure Append_Unqualified
     (Buf : in out Bounded_String;
      Id  : Valid_Name_Id)
   is
      Temp : Bounded_String;
   begin
      Append (Temp, Id);
      Strip_Qualification_And_Suffixes (Temp);
      Append (Buf, Temp);
   end Append_Unqualified;

   --------------------------------
   -- Append_Unqualified_Decoded --
   --------------------------------

   procedure Append_Unqualified_Decoded
     (Buf : in out Bounded_String;
      Id  : Valid_Name_Id)
   is
      Temp : Bounded_String;
   begin
      Append_Decoded (Temp, Id);
      Strip_Qualification_And_Suffixes (Temp);
      Append (Buf, Temp);
   end Append_Unqualified_Decoded;

   --------------
   -- Finalize --
   --------------

   procedure Finalize is
      F : array (Int range 0 .. 50) of Int;
      --  N'th entry is the number of chains of length N, except last entry,
      --  which is the number of chains of length F'Last or more.

      Max_Chain_Length : Nat := 0;
      --  Maximum length of all chains

      Probes : Nat := 0;
      --  Used to compute average number of probes

      Nsyms : Nat := 0;
      --  Number of symbols in table

      Verbosity : constant Int range 1 .. 3 := 1;
      pragma Warnings (Off, Verbosity);
      --  This constant indicates the level of verbosity in the output from
      --  this procedure. Currently this can only be changed by editing the
      --  declaration above and recompiling. That's good enough in practice,
      --  since we very rarely need to use this debug option. Settings are:
      --
      --    1 => print basic summary information
      --    2 => in addition print number of entries per hash chain
      --    3 => in addition print content of entries

      Zero : constant Int := Character'Pos ('0');

   begin
      if not Debug_Flag_H then
         return;
      end if;

      for J in F'Range loop
         F (J) := 0;
      end loop;

      for J in Hash_Index_Type loop
         if Hash_Table (J) = No_Name then
            F (0) := F (0) + 1;

         else
            declare
               C : Nat;
               N : Name_Id;
               S : Int;

            begin
               C := 0;
               N := Hash_Table (J);

               while N /= No_Name loop
                  N := Name_Entries.Table (N).Hash_Link;
                  C := C + 1;
               end loop;

               Nsyms := Nsyms + 1;
               Probes := Probes + (1 + C) * 100;

               if C > Max_Chain_Length then
                  Max_Chain_Length := C;
               end if;

               if Verbosity >= 2 then
                  Write_Str ("Hash_Table (");
                  Write_Int (J);
                  Write_Str (") has ");
                  Write_Int (C);
                  Write_Str (" entries");
                  Write_Eol;
               end if;

               if C < F'Last then
                  F (C) := F (C) + 1;
               else
                  F (F'Last) := F (F'Last) + 1;
               end if;

               if Verbosity >= 3 then
                  N := Hash_Table (J);
                  while N /= No_Name loop
                     S := Name_Entries.Table (N).Name_Chars_Index;

                     Write_Str ("      ");

                     for J in 1 .. Name_Entries.Table (N).Name_Len loop
                        Write_Char (Name_Chars.Table (S + Int (J)));
                     end loop;

                     Write_Eol;

                     N := Name_Entries.Table (N).Hash_Link;
                  end loop;
               end if;
            end;
         end if;
      end loop;

      Write_Eol;

      for J in F'Range loop
         if F (J) /= 0 then
            Write_Str ("Number of hash chains of length ");

            if J < 10 then
               Write_Char (' ');
            end if;

            Write_Int (J);

            if J = F'Last then
               Write_Str (" or greater");
            end if;

            Write_Str (" = ");
            Write_Int (F (J));
            Write_Eol;
         end if;
      end loop;

      --  Print out average number of probes, in the case where Name_Find is
      --  called for a string that is already in the table.

      Write_Eol;
      Write_Str ("Average number of probes for lookup = ");
      pragma Assert (Nsyms /= 0);
      --  Add assumption to facilitate static analysis. Here Nsyms cannot be
      --  zero because many symbols are added to the table by default.
      Probes := Probes / Nsyms;
      Write_Int (Probes / 200);
      Write_Char ('.');
      Probes := (Probes mod 200) / 2;
      Write_Char (Character'Val (Zero + Probes / 10));
      Write_Char (Character'Val (Zero + Probes mod 10));
      Write_Eol;

      Write_Str ("Max_Chain_Length = ");
      Write_Int (Max_Chain_Length);
      Write_Eol;
      Write_Str ("Name_Chars'Length = ");
      Write_Int (Name_Chars.Last - Name_Chars.First + 1);
      Write_Eol;
      Write_Str ("Name_Entries'Length = ");
      Write_Int (Int (Name_Entries.Last - Name_Entries.First + 1));
      Write_Eol;
      Write_Str ("Nsyms = ");
      Write_Int (Nsyms);
      Write_Eol;
   end Finalize;

   -----------------------------
   -- Get_Decoded_Name_String --
   -----------------------------

   procedure Get_Decoded_Name_String (Id : Valid_Name_Id) is
   begin
      Global_Name_Buffer.Length := 0;
      Append_Decoded (Global_Name_Buffer, Id);
   end Get_Decoded_Name_String;

   -------------------------------------------
   -- Get_Decoded_Name_String_With_Brackets --
   -------------------------------------------

   procedure Get_Decoded_Name_String_With_Brackets (Id : Valid_Name_Id) is
   begin
      Global_Name_Buffer.Length := 0;
      Append_Decoded_With_Brackets (Global_Name_Buffer, Id);
   end Get_Decoded_Name_String_With_Brackets;

   ------------------------
   -- Get_Last_Two_Chars --
   ------------------------

   procedure Get_Last_Two_Chars
     (N  : Valid_Name_Id;
      C1 : out Character;
      C2 : out Character)
   is
      NE  : Name_Entry renames Name_Entries.Table (N);
      NEL : constant Int := Int (NE.Name_Len);

   begin
      if NEL >= 2 then
         C1 := Name_Chars.Table (NE.Name_Chars_Index + NEL - 1);
         C2 := Name_Chars.Table (NE.Name_Chars_Index + NEL - 0);
      else
         C1 := ASCII.NUL;
         C2 := ASCII.NUL;
      end if;
   end Get_Last_Two_Chars;

   ---------------------
   -- Get_Name_String --
   ---------------------

   procedure Get_Name_String (Id : Valid_Name_Id) is
   begin
      Global_Name_Buffer.Length := 0;
      Append (Global_Name_Buffer, Id);
   end Get_Name_String;

   function Get_Name_String (Id : Valid_Name_Id) return String is
      Buf : Bounded_String (Max_Length => Natural (Length_Of_Name (Id)));
   begin
      Append (Buf, Id);
      return +Buf;
   end Get_Name_String;

   --------------------------------
   -- Get_Name_String_And_Append --
   --------------------------------

   procedure Get_Name_String_And_Append (Id : Valid_Name_Id) is
   begin
      Append (Global_Name_Buffer, Id);
   end Get_Name_String_And_Append;

   -----------------------------
   -- Get_Name_Table_Boolean1 --
   -----------------------------

   function Get_Name_Table_Boolean1 (Id : Valid_Name_Id) return Boolean is
   begin
      pragma Assert (Is_Valid_Name (Id));
      return Name_Entries.Table (Id).Boolean1_Info;
   end Get_Name_Table_Boolean1;

   -----------------------------
   -- Get_Name_Table_Boolean2 --
   -----------------------------

   function Get_Name_Table_Boolean2 (Id : Valid_Name_Id) return Boolean is
   begin
      pragma Assert (Is_Valid_Name (Id));
      return Name_Entries.Table (Id).Boolean2_Info;
   end Get_Name_Table_Boolean2;

   -----------------------------
   -- Get_Name_Table_Boolean3 --
   -----------------------------

   function Get_Name_Table_Boolean3 (Id : Valid_Name_Id) return Boolean is
   begin
      pragma Assert (Is_Valid_Name (Id));
      return Name_Entries.Table (Id).Boolean3_Info;
   end Get_Name_Table_Boolean3;

   -------------------------
   -- Get_Name_Table_Byte --
   -------------------------

   function Get_Name_Table_Byte (Id : Valid_Name_Id) return Byte is
   begin
      pragma Assert (Is_Valid_Name (Id));
      return Name_Entries.Table (Id).Byte_Info;
   end Get_Name_Table_Byte;

   -------------------------
   -- Get_Name_Table_Int --
   -------------------------

   function Get_Name_Table_Int (Id : Valid_Name_Id) return Int is
   begin
      pragma Assert (Is_Valid_Name (Id));
      return Name_Entries.Table (Id).Int_Info;
   end Get_Name_Table_Int;

   -----------------------------------------
   -- Get_Unqualified_Decoded_Name_String --
   -----------------------------------------

   procedure Get_Unqualified_Decoded_Name_String (Id : Valid_Name_Id) is
   begin
      Global_Name_Buffer.Length := 0;
      Append_Unqualified_Decoded (Global_Name_Buffer, Id);
   end Get_Unqualified_Decoded_Name_String;

   ---------------------------------
   -- Get_Unqualified_Name_String --
   ---------------------------------

   procedure Get_Unqualified_Name_String (Id : Valid_Name_Id) is
   begin
      Global_Name_Buffer.Length := 0;
      Append_Unqualified (Global_Name_Buffer, Id);
   end Get_Unqualified_Name_String;

   ----------
   -- Hash --
   ----------

   function Hash (Buf : Bounded_String) return Hash_Index_Type is

      --  This hash function looks at every character, in order to make it
      --  likely that similar strings get different hash values. The rotate by
      --  7 bits has been determined empirically to be good, and it doesn't
      --  lose bits like a shift would. The final conversion can't overflow,
      --  because the table is 2**16 in size. This function probably needs to
      --  be changed if the hash table size is changed.

      --  Note that we could get some speed improvement by aligning the string
      --  to 32 or 64 bits, and doing word-wise xor's. We could also implement
      --  a growable table. It doesn't seem worth the trouble to do those
      --  things, for now.

      Result : Unsigned_16 := 0;

   begin
      for J in 1 .. Buf.Length loop
         Result := Rotate_Left (Result, 7) xor Character'Pos (Buf.Chars (J));
      end loop;

      return Hash_Index_Type (Result);
   end Hash;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      null;
   end Initialize;

   ----------------
   -- Insert_Str --
   ----------------

   procedure Insert_Str
     (Buf   : in out Bounded_String;
      S     : String;
      Index : Positive)
   is
      SL : constant Natural := S'Length;

   begin
      Buf.Chars (Index + SL .. Buf.Length + SL) :=
        Buf.Chars (Index .. Buf.Length);
      Buf.Chars (Index .. Index + SL - 1) := S;
      Buf.Length := Buf.Length + SL;
   end Insert_Str;

   -------------------------------
   -- Insert_Str_In_Name_Buffer --
   -------------------------------

   procedure Insert_Str_In_Name_Buffer (S : String; Index : Positive) is
   begin
      Insert_Str (Global_Name_Buffer, S, Index);
   end Insert_Str_In_Name_Buffer;

   ----------------------
   -- Is_Internal_Name --
   ----------------------

   function Is_Internal_Name (Buf : Bounded_String) return Boolean is
      J : Natural;

   begin
      --  Any name starting or ending with underscore is internal

      if Buf.Chars (1) = '_'
        or else Buf.Chars (Buf.Length) = '_'
      then
         return True;

      --  Allow quoted character

      elsif Buf.Chars (1) = ''' then
         return False;

      --  All other cases, scan name

      else
         --  Test backwards, because we only want to test the last entity
         --  name if the name we have is qualified with other entities.

         J := Buf.Length;
         while J /= 0 loop

            --  Skip stuff between brackets (A-F OK there)

            if Buf.Chars (J) = ']' then
               loop
                  J := J - 1;
                  exit when J = 1 or else Buf.Chars (J) = '[';
               end loop;

            --  Test for internal letter

            elsif Is_OK_Internal_Letter (Buf.Chars (J)) then
               return True;

            --  Quit if we come to terminating double underscore (note that
            --  if the current character is an underscore, we know that
            --  there is a previous character present, since we already
            --  filtered out the case of Buf.Chars (1) = '_' above.

            elsif Buf.Chars (J) = '_'
              and then Buf.Chars (J - 1) = '_'
              and then Buf.Chars (J - 2) /= '_'
            then
               return False;
            end if;

            J := J - 1;
         end loop;
      end if;

      return False;
   end Is_Internal_Name;

   function Is_Internal_Name (Id : Valid_Name_Id) return Boolean is
      Buf : Bounded_String (Max_Length => Natural (Length_Of_Name (Id)));
   begin
      Append (Buf, Id);
      return Is_Internal_Name (Buf);
   end Is_Internal_Name;

   function Is_Internal_Name return Boolean is
   begin
      return Is_Internal_Name (Global_Name_Buffer);
   end Is_Internal_Name;

   ---------------------------
   -- Is_OK_Internal_Letter --
   ---------------------------

   function Is_OK_Internal_Letter (C : Character) return Boolean is
   begin
      return C in 'A' .. 'Z'
        and then C /= 'O'
        and then C /= 'Q'
        and then C /= 'U'
        and then C /= 'W'
        and then C /= 'X';
   end Is_OK_Internal_Letter;

   ----------------------
   -- Is_Operator_Name --
   ----------------------

   function Is_Operator_Name (Id : Valid_Name_Id) return Boolean is
      S : Int;
   begin
      pragma Assert (Is_Valid_Name (Id));
      S := Name_Entries.Table (Id).Name_Chars_Index;
      return Name_Chars.Table (S + 1) = 'O';
   end Is_Operator_Name;

   -------------------
   -- Is_Valid_Name --
   -------------------

   function Is_Valid_Name (Id : Name_Id) return Boolean is
   begin
      return Id in Name_Entries.First .. Name_Entries.Last;
   end Is_Valid_Name;

   ------------------
   -- Last_Name_Id --
   ------------------

   function Last_Name_Id return Name_Id is
   begin
      return Name_Id (Int (First_Name_Id) + Name_Entries_Count - 1);
   end Last_Name_Id;

   --------------------
   -- Length_Of_Name --
   --------------------

   function Length_Of_Name (Id : Valid_Name_Id) return Nat is
   begin
      return Int (Name_Entries.Table (Id).Name_Len);
   end Length_Of_Name;

   ----------
   -- Lock --
   ----------

   procedure Lock is
   begin
      Name_Chars.Set_Last (Name_Chars.Last + Name_Chars_Reserve);
      Name_Entries.Set_Last (Name_Entries.Last + Name_Entries_Reserve);
      Name_Chars.Release;
      Name_Chars.Locked := True;
      Name_Entries.Release;
      Name_Entries.Locked := True;
   end Lock;

   ----------------
   -- Name_Enter --
   ----------------

   function Name_Enter
     (Buf : Bounded_String := Global_Name_Buffer) return Valid_Name_Id
   is
   begin
      Name_Entries.Append
        ((Name_Chars_Index      => Name_Chars.Last,
          Name_Len              => Short (Buf.Length),
          Byte_Info             => 0,
          Int_Info              => 0,
          Hash_Link             => No_Name,
          Name_Has_No_Encodings => False,
          Boolean1_Info         => False,
          Boolean2_Info         => False,
          Boolean3_Info         => False,
          Spare                 => False));

      --  Set corresponding string entry in the Name_Chars table

      for J in 1 .. Buf.Length loop
         Name_Chars.Append (Buf.Chars (J));
      end loop;

      Name_Chars.Append (ASCII.NUL);

      return Name_Entries.Last;
   end Name_Enter;

   function Name_Enter (S : String) return Valid_Name_Id is
      Buf : Bounded_String (Max_Length => S'Length);
   begin
      Append (Buf, S);
      return Name_Enter (Buf);
   end Name_Enter;

   ------------------------
   -- Name_Entries_Count --
   ------------------------

   function Name_Entries_Count return Nat is
   begin
      return Int (Name_Entries.Last - Name_Entries.First + 1);
   end Name_Entries_Count;

   ---------------
   -- Name_Find --
   ---------------

   function Name_Find
     (Buf : Bounded_String := Global_Name_Buffer) return Valid_Name_Id
   is
      New_Id : Name_Id;
      --  Id of entry in hash search, and value to be returned

      S : Int;
      --  Pointer into string table

      Hash_Index : Hash_Index_Type;
      --  Computed hash index

      Result : Valid_Name_Id;

   begin
      --  Quick handling for one character names

      if Buf.Length = 1 then
         Result := First_Name_Id + Character'Pos (Buf.Chars (1));

      --  Otherwise search hash table for existing matching entry

      else
         Hash_Index := Namet.Hash (Buf);
         New_Id := Hash_Table (Hash_Index);

         if New_Id = No_Name then
            Hash_Table (Hash_Index) := Name_Entries.Last + 1;

         else
            Search : loop
               if Buf.Length /=
                 Integer (Name_Entries.Table (New_Id).Name_Len)
               then
                  goto No_Match;
               end if;

               S := Name_Entries.Table (New_Id).Name_Chars_Index;

               for J in 1 .. Buf.Length loop
                  if Name_Chars.Table (S + Int (J)) /= Buf.Chars (J) then
                     goto No_Match;
                  end if;
               end loop;

               Result := New_Id;
               goto Done;

               --  Current entry in hash chain does not match

               <<No_Match>>
                  if Name_Entries.Table (New_Id).Hash_Link /= No_Name then
                     New_Id := Name_Entries.Table (New_Id).Hash_Link;
                  else
                     Name_Entries.Table (New_Id).Hash_Link :=
                       Name_Entries.Last + 1;
                     exit Search;
                  end if;
            end loop Search;
         end if;

         --  We fall through here only if a matching entry was not found in the
         --  hash table. We now create a new entry in the names table. The hash
         --  link pointing to the new entry (Name_Entries.Last+1) has been set.

         Name_Entries.Append
           ((Name_Chars_Index      => Name_Chars.Last,
             Name_Len              => Short (Buf.Length),
             Hash_Link             => No_Name,
             Int_Info              => 0,
             Byte_Info             => 0,
             Name_Has_No_Encodings => False,
             Boolean1_Info         => False,
             Boolean2_Info         => False,
             Boolean3_Info         => False,
             Spare                 => False));

         --  Set corresponding string entry in the Name_Chars table

         for J in 1 .. Buf.Length loop
            Name_Chars.Append (Buf.Chars (J));
         end loop;

         Name_Chars.Append (ASCII.NUL);

         Result := Name_Entries.Last;
      end if;

      <<Done>>
      return Result;
   end Name_Find;

   function Name_Find (S : String) return Valid_Name_Id is
      Buf : Bounded_String (Max_Length => S'Length);
   begin
      Append (Buf, S);
      return Name_Find (Buf);
   end Name_Find;

   -----------------
   -- Name_Equals --
   -----------------

   function Name_Equals
     (N1 : Valid_Name_Id;
      N2 : Valid_Name_Id) return Boolean
   is
   begin
      return N1 = N2 or else Get_Name_String (N1) = Get_Name_String (N2);
   end Name_Equals;

   -------------
   -- Present --
   -------------

   function Present (Nam : File_Name_Type) return Boolean is
   begin
      return Nam /= No_File;
   end Present;

   -------------
   -- Present --
   -------------

   function Present (Nam : Name_Id) return Boolean is
   begin
      return Nam /= No_Name;
   end Present;

   -------------
   -- Present --
   -------------

   function Present (Nam : Unit_Name_Type) return Boolean is
   begin
      return Nam /= No_Unit_Name;
   end Present;

   ------------------
   -- Reinitialize --
   ------------------

   procedure Reinitialize is
   begin
      Name_Chars.Init;
      Name_Entries.Init;

      --  Initialize entries for one character names

      for C in Character loop
         Name_Entries.Append
           ((Name_Chars_Index      => Name_Chars.Last,
             Name_Len              => 1,
             Byte_Info             => 0,
             Int_Info              => 0,
             Hash_Link             => No_Name,
             Name_Has_No_Encodings => True,
             Boolean1_Info         => False,
             Boolean2_Info         => False,
             Boolean3_Info         => False,
             Spare                 => False));

         Name_Chars.Append (C);
         Name_Chars.Append (ASCII.NUL);
      end loop;

      --  Clear hash table

      for J in Hash_Index_Type loop
         Hash_Table (J) := No_Name;
      end loop;
   end Reinitialize;

   ----------------------
   -- Reset_Name_Table --
   ----------------------

   procedure Reset_Name_Table is
   begin
      for J in First_Name_Id .. Name_Entries.Last loop
         Name_Entries.Table (J).Int_Info  := 0;
         Name_Entries.Table (J).Byte_Info := 0;
      end loop;
   end Reset_Name_Table;

   --------------------------------
   -- Set_Character_Literal_Name --
   --------------------------------

   procedure Set_Character_Literal_Name
     (Buf : in out Bounded_String;
      C   : Char_Code)
   is
   begin
      Buf.Length := 0;
      Append (Buf, 'Q');
      Append_Encoded (Buf, C);
   end Set_Character_Literal_Name;

   procedure Set_Character_Literal_Name (C : Char_Code) is
   begin
      Set_Character_Literal_Name (Global_Name_Buffer, C);
   end Set_Character_Literal_Name;

   -----------------------------
   -- Set_Name_Table_Boolean1 --
   -----------------------------

   procedure Set_Name_Table_Boolean1 (Id : Valid_Name_Id; Val : Boolean) is
   begin
      pragma Assert (Is_Valid_Name (Id));
      Name_Entries.Table (Id).Boolean1_Info := Val;
   end Set_Name_Table_Boolean1;

   -----------------------------
   -- Set_Name_Table_Boolean2 --
   -----------------------------

   procedure Set_Name_Table_Boolean2 (Id : Valid_Name_Id; Val : Boolean) is
   begin
      pragma Assert (Is_Valid_Name (Id));
      Name_Entries.Table (Id).Boolean2_Info := Val;
   end Set_Name_Table_Boolean2;

   -----------------------------
   -- Set_Name_Table_Boolean3 --
   -----------------------------

   procedure Set_Name_Table_Boolean3 (Id : Valid_Name_Id; Val : Boolean) is
   begin
      pragma Assert (Is_Valid_Name (Id));
      Name_Entries.Table (Id).Boolean3_Info := Val;
   end Set_Name_Table_Boolean3;

   -------------------------
   -- Set_Name_Table_Byte --
   -------------------------

   procedure Set_Name_Table_Byte (Id : Valid_Name_Id; Val : Byte) is
   begin
      pragma Assert (Is_Valid_Name (Id));
      Name_Entries.Table (Id).Byte_Info := Val;
   end Set_Name_Table_Byte;

   -------------------------
   -- Set_Name_Table_Int --
   -------------------------

   procedure Set_Name_Table_Int (Id : Valid_Name_Id; Val : Int) is
   begin
      pragma Assert (Is_Valid_Name (Id));
      Name_Entries.Table (Id).Int_Info := Val;
   end Set_Name_Table_Int;

   -----------------------------
   -- Store_Encoded_Character --
   -----------------------------

   procedure Store_Encoded_Character (C : Char_Code) is
   begin
      Append_Encoded (Global_Name_Buffer, C);
   end Store_Encoded_Character;

   --------------------------------------
   -- Strip_Qualification_And_Suffixes --
   --------------------------------------

   procedure Strip_Qualification_And_Suffixes (Buf : in out Bounded_String) is
      J : Integer;

   begin
      --  Strip package body qualification string off end

      for J in reverse 2 .. Buf.Length loop
         if Buf.Chars (J) = 'X' then
            Buf.Length := J - 1;
            exit;
         end if;

         exit when Buf.Chars (J) /= 'b'
           and then Buf.Chars (J) /= 'n'
           and then Buf.Chars (J) /= 'p';
      end loop;

      --  Find rightmost __ or $ separator if one exists. First we position
      --  to start the search. If we have a character constant, position
      --  just before it, otherwise position to last character but one

      if Buf.Chars (Buf.Length) = ''' then
         J := Buf.Length - 2;
         while J > 0 and then Buf.Chars (J) /= ''' loop
            J := J - 1;
         end loop;

      else
         J := Buf.Length - 1;
      end if;

      --  Loop to search for rightmost __ or $ (homonym) separator

      while J > 1 loop

         --  If $ separator, homonym separator, so strip it and keep looking

         if Buf.Chars (J) = '$' then
            Buf.Length := J - 1;
            J := Buf.Length - 1;

         --  Else check for __ found

         elsif Buf.Chars (J) = '_' and then Buf.Chars (J + 1) = '_' then

            --  Found __ so see if digit follows, and if so, this is a
            --  homonym separator, so strip it and keep looking.

            if Buf.Chars (J + 2) in '0' .. '9' then
               Buf.Length := J - 1;
               J := Buf.Length - 1;

            --  If not a homonym separator, then we simply strip the
            --  separator and everything that precedes it, and we are done

            else
               Buf.Chars (1 .. Buf.Length - J - 1) :=
                 Buf.Chars (J + 2 .. Buf.Length);
               Buf.Length := Buf.Length - J - 1;
               exit;
            end if;

         else
            J := J - 1;
         end if;
      end loop;
   end Strip_Qualification_And_Suffixes;

   ---------------
   -- To_String --
   ---------------

   function To_String (Buf : Bounded_String) return String is
   begin
      return Buf.Chars (1 .. Buf.Length);
   end To_String;

   ------------
   -- Unlock --
   ------------

   procedure Unlock is
   begin
      Name_Chars.Locked := False;
      Name_Chars.Set_Last (Name_Chars.Last - Name_Chars_Reserve);
      Name_Chars.Release;
      Name_Entries.Locked := False;
      Name_Entries.Set_Last (Name_Entries.Last - Name_Entries_Reserve);
      Name_Entries.Release;
   end Unlock;

   --------
   -- wn --
   --------

   procedure wn (Id : Name_Id) is
   begin
      if Is_Valid_Name (Id) then
         declare
            Buf : Bounded_String (Max_Length => Natural (Length_Of_Name (Id)));
         begin
            Append (Buf, Id);
            Write_Str (Buf.Chars (1 .. Buf.Length));
         end;

      elsif Id = No_Name then
         Write_Str ("<No_Name>");

      elsif Id = Error_Name then
         Write_Str ("<Error_Name>");

      else
         Write_Str ("<invalid name_id>");
         Write_Int (Int (Id));
      end if;

      Write_Eol;
   end wn;

   ----------------
   -- Write_Name --
   ----------------

   procedure Write_Name (Id : Valid_Name_Id) is
      Buf : Bounded_String (Max_Length => Natural (Length_Of_Name (Id)));
   begin
      Append (Buf, Id);
      Write_Str (Buf.Chars (1 .. Buf.Length));
   end Write_Name;

   ------------------------
   -- Write_Name_Decoded --
   ------------------------

   procedure Write_Name_Decoded (Id : Valid_Name_Id) is
      Buf : Bounded_String;
   begin
      Append_Decoded (Buf, Id);
      Write_Str (Buf.Chars (1 .. Buf.Length));
   end Write_Name_Decoded;

--  Package initialization, initialize tables

begin
   Reinitialize;
end Namet;
