------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                                N A M E T                                 --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2003 Free Software Foundation, Inc.          --
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

--  WARNING: There is a C version of this package. Any changes to this
--  source file must be properly reflected in the C header file a-namet.h
--  which is created manually from namet.ads and namet.adb.

with Debug;    use Debug;
with Output;   use Output;
with Tree_IO;  use Tree_IO;
with Widechar; use Widechar;

package body Namet is

   Name_Chars_Reserve   : constant := 5000;
   Name_Entries_Reserve : constant := 100;
   --  The names table is locked during gigi processing, since gigi assumes
   --  that the table does not move. After returning from gigi, the names
   --  table is unlocked again, since writing library file information needs
   --  to generate some extra names. To avoid the inefficiency of always
   --  reallocating during this second unlocked phase, we reserve a bit of
   --  extra space before doing the release call.

   Hash_Num : constant Int := 2**12;
   --  Number of headers in the hash table. Current hash algorithm is closely
   --  tailored to this choice, so it can only be changed if a corresponding
   --  change is made to the hash alogorithm.

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

   function Hash return Hash_Index_Type;
   pragma Inline (Hash);
   --  Compute hash code for name stored in Name_Buffer (length in Name_Len)

   procedure Strip_Qualification_And_Suffixes;
   --  Given an encoded entity name in Name_Buffer, remove package body
   --  suffix as described for Strip_Package_Body_Suffix, and also remove
   --  all qualification, i.e. names followed by two underscores. The
   --  contents of Name_Buffer is modified by this call, and on return
   --  Name_Buffer and Name_Len reflect the stripped name.

   -----------------------------
   -- Add_Char_To_Name_Buffer --
   -----------------------------

   procedure Add_Char_To_Name_Buffer (C : Character) is
   begin
      if Name_Len < Name_Buffer'Last then
         Name_Len := Name_Len + 1;
         Name_Buffer (Name_Len) := C;
      end if;
   end Add_Char_To_Name_Buffer;

   ----------------------------
   -- Add_Nat_To_Name_Buffer --
   ----------------------------

   procedure Add_Nat_To_Name_Buffer (V : Nat) is
   begin
      if V >= 10 then
         Add_Nat_To_Name_Buffer (V / 10);
      end if;

      Add_Char_To_Name_Buffer (Character'Val (Character'Pos ('0') + V rem 10));
   end Add_Nat_To_Name_Buffer;

   ----------------------------
   -- Add_Str_To_Name_Buffer --
   ----------------------------

   procedure Add_Str_To_Name_Buffer (S : String) is
   begin
      for J in S'Range loop
         Add_Char_To_Name_Buffer (S (J));
      end loop;
   end Add_Str_To_Name_Buffer;


   --------------
   -- Finalize --
   --------------

   procedure Finalize is
      Max_Chain_Length : constant := 50;
      --  Max length of chains for which specific information is output

      F : array (Int range 0 .. Max_Chain_Length) of Int;
      --  N'th entry is number of chains of length N

      Probes : Int := 0;
      --  Used to compute average number of probes

      Nsyms : Int := 0;
      --  Number of symbols in table

   begin
      if Debug_Flag_H then

         for J in F'Range loop
            F (J) := 0;
         end loop;

         for I in Hash_Index_Type loop
            if Hash_Table (I) = No_Name then
               F (0) := F (0) + 1;

            else
               Write_Str ("Hash_Table (");
               Write_Int (Int (I));
               Write_Str (") has ");

               declare
                  C : Int := 1;
                  N : Name_Id;
                  S : Int;

               begin
                  C := 0;
                  N := Hash_Table (I);

                  while N /= No_Name loop
                     N := Name_Entries.Table (N).Hash_Link;
                     C := C + 1;
                  end loop;

                  Write_Int (C);
                  Write_Str (" entries");
                  Write_Eol;

                  if C < Max_Chain_Length then
                     F (C) := F (C) + 1;
                  else
                     F (Max_Chain_Length) := F (Max_Chain_Length) + 1;
                  end if;

                  N := Hash_Table (I);

                  while N /= No_Name loop
                     S := Name_Entries.Table (N).Name_Chars_Index;
                     Write_Str ("      ");

                     for J in 1 .. Name_Entries.Table (N).Name_Len loop
                        Write_Char (Name_Chars.Table (S + Int (J)));
                     end loop;

                     Write_Eol;
                     N := Name_Entries.Table (N).Hash_Link;
                  end loop;
               end;
            end if;
         end loop;

         Write_Eol;

         for I in Int range 0 .. Max_Chain_Length loop
            if F (I) /= 0 then
               Write_Str ("Number of hash chains of length ");

               if I < 10 then
                  Write_Char (' ');
               end if;

               Write_Int (I);

               if I = Max_Chain_Length then
                  Write_Str (" or greater");
               end if;

               Write_Str (" = ");
               Write_Int (F (I));
               Write_Eol;

               if I /= 0 then
                  Nsyms := Nsyms + F (I);
                  Probes := Probes + F (I) * (1 + I) * 100;
               end if;
            end if;
         end loop;

         Write_Eol;
         Write_Str ("Average number of probes for lookup = ");
         Probes := Probes / Nsyms;
         Write_Int (Probes / 200);
         Write_Char ('.');
         Probes := (Probes mod 200) / 2;
         Write_Char (Character'Val (48 + Probes / 10));
         Write_Char (Character'Val (48 + Probes mod 10));
         Write_Eol;
         Write_Eol;
      end if;
   end Finalize;

   -----------------------------
   -- Get_Decoded_Name_String --
   -----------------------------

   procedure Get_Decoded_Name_String (Id : Name_Id) is
      C : Character;
      P : Natural;

   begin
      Get_Name_String (Id);

      --  Quick loop to see if there is anything special to do

      P := 1;
      loop
         if P = Name_Len then
            return;

         else
            C := Name_Buffer (P);

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
         New_Buf : String (1 .. Name_Buffer'Last);

         procedure Copy_One_Character;
         --  Copy a character from Name_Buffer to New_Buf. Includes case
         --  of copying a Uhh or Whhhh sequence and decoding it.

         function Hex (N : Natural) return Natural;
         --  Scans past N digits using Old pointer and returns hex value

         procedure Insert_Character (C : Character);
         --  Insert a new character into output decoded name

         ------------------------
         -- Copy_One_Character --
         ------------------------

         procedure Copy_One_Character is
            C : Character;

         begin
            C := Name_Buffer (Old);

            --  U (upper half insertion case)

            if C = 'U'
              and then Old < Name_Len
              and then Name_Buffer (Old + 1) not in 'A' .. 'Z'
              and then Name_Buffer (Old + 1) /= '_'
            then
               Old := Old + 1;
               Insert_Character (Character'Val (Hex (2)));

            --  W (wide character insertion)

            elsif C = 'W'
              and then Old < Name_Len
              and then Name_Buffer (Old + 1) not in 'A' .. 'Z'
              and then Name_Buffer (Old + 1) /= '_'
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

         function Hex (N : Natural) return Natural is
            T : Natural := 0;
            C : Character;

         begin
            for J in 1 .. N loop
               C := Name_Buffer (Old);
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

         while Old <= Name_Len loop

            --  Case of character literal, put apostrophes around character

            if Name_Buffer (Old) = 'Q'
              and then Old < Name_Len
            then
               Old := Old + 1;
               Insert_Character (''');
               Copy_One_Character;
               Insert_Character (''');

            --  Case of operator name

            elsif Name_Buffer (Old) = 'O'
              and then Old < Name_Len
              and then Name_Buffer (Old + 1) not in 'A' .. 'Z'
              and then Name_Buffer (Old + 1) /= '_'
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
                  --  constraint error may be raised.

                  J := Map'First;
                  loop
                     exit when Name_Buffer (Old) = Map (J)
                       and then Name_Buffer (Old + 1) = Map (J + 1);
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

                     while Old <= Name_Len
                       and then Name_Buffer (Old) in 'a' .. 'z'
                     loop
                        Old := Old + 1;
                     end loop;

                  --  For other operator names, leave them in lower case,
                  --  surrounded by apostrophes

                  else
                     --  Copy original operator name from input to output

                     while Old <= Name_Len
                        and then Name_Buffer (Old) in 'a' .. 'z'
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

         Name_Len := New_Len;
         Name_Buffer (1 .. New_Len) := New_Buf (1 .. New_Len);
      end Decode;
   end Get_Decoded_Name_String;

   -------------------------------------------
   -- Get_Decoded_Name_String_With_Brackets --
   -------------------------------------------

   procedure Get_Decoded_Name_String_With_Brackets (Id : Name_Id) is
      P : Natural;

   begin
      --  Case of operator name, normal decoding is fine

      if Name_Buffer (1) = 'O' then
         Get_Decoded_Name_String (Id);

      --  For character literals, normal decoding is fine

      elsif Name_Buffer (1) = 'Q' then
         Get_Decoded_Name_String (Id);

      --  Only remaining issue is U/W sequences

      else
         Get_Name_String (Id);

         P := 1;
         while P < Name_Len loop
            if Name_Buffer (P + 1) in 'A' .. 'Z' then
               P := P + 1;

            elsif Name_Buffer (P) = 'U' then
               for J in reverse P + 3 .. P + Name_Len loop
                  Name_Buffer (J + 3) := Name_Buffer (J);
               end loop;

               Name_Len := Name_Len + 3;
               Name_Buffer (P + 3) := Name_Buffer (P + 2);
               Name_Buffer (P + 2) := Name_Buffer (P + 1);
               Name_Buffer (P)     := '[';
               Name_Buffer (P + 1) := '"';
               Name_Buffer (P + 4) := '"';
               Name_Buffer (P + 5) := ']';
               P := P + 6;

            elsif Name_Buffer (P) = 'W' then
               Name_Buffer (P + 8 .. P + Name_Len + 5) :=
                 Name_Buffer (P + 5 .. Name_Len);
               Name_Buffer (P + 5) := Name_Buffer (P + 4);
               Name_Buffer (P + 4) := Name_Buffer (P + 3);
               Name_Buffer (P + 3) := Name_Buffer (P + 2);
               Name_Buffer (P + 2) := Name_Buffer (P + 1);
               Name_Buffer (P)     := '[';
               Name_Buffer (P + 1) := '"';
               Name_Buffer (P + 6) := '"';
               Name_Buffer (P + 7) := ']';
               Name_Len := Name_Len + 5;
               P := P + 8;

            else
               P := P + 1;
            end if;
         end loop;
      end if;
   end Get_Decoded_Name_String_With_Brackets;

   ------------------------
   -- Get_Last_Two_Chars --
   ------------------------

   procedure Get_Last_Two_Chars (N : Name_Id; C1, C2 : out Character) is
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

   procedure Get_Name_String (Id : Name_Id) is
      S : Int;

   begin
      pragma Assert (Id in Name_Entries.First .. Name_Entries.Last);

      S := Name_Entries.Table (Id).Name_Chars_Index;
      Name_Len := Natural (Name_Entries.Table (Id).Name_Len);

      for J in 1 .. Name_Len loop
         Name_Buffer (J) := Name_Chars.Table (S + Int (J));
      end loop;
   end Get_Name_String;

   function Get_Name_String (Id : Name_Id) return String is
      S : Int;

   begin
      pragma Assert (Id in Name_Entries.First .. Name_Entries.Last);
      S := Name_Entries.Table (Id).Name_Chars_Index;

      declare
         R : String (1 .. Natural (Name_Entries.Table (Id).Name_Len));

      begin
         for J in R'Range loop
            R (J) := Name_Chars.Table (S + Int (J));
         end loop;

         return R;
      end;
   end Get_Name_String;

   --------------------------------
   -- Get_Name_String_And_Append --
   --------------------------------

   procedure Get_Name_String_And_Append (Id : Name_Id) is
      S : Int;

   begin
      pragma Assert (Id in Name_Entries.First .. Name_Entries.Last);

      S := Name_Entries.Table (Id).Name_Chars_Index;

      for J in 1 .. Natural (Name_Entries.Table (Id).Name_Len) loop
         Name_Len := Name_Len + 1;
         Name_Buffer (Name_Len) := Name_Chars.Table (S + Int (J));
      end loop;
   end Get_Name_String_And_Append;

   -------------------------
   -- Get_Name_Table_Byte --
   -------------------------

   function Get_Name_Table_Byte (Id : Name_Id) return Byte is
   begin
      pragma Assert (Id in Name_Entries.First .. Name_Entries.Last);
      return Name_Entries.Table (Id).Byte_Info;
   end Get_Name_Table_Byte;

   -------------------------
   -- Get_Name_Table_Info --
   -------------------------

   function Get_Name_Table_Info (Id : Name_Id) return Int is
   begin
      pragma Assert (Id in Name_Entries.First .. Name_Entries.Last);
      return Name_Entries.Table (Id).Int_Info;
   end Get_Name_Table_Info;

   -----------------------------------------
   -- Get_Unqualified_Decoded_Name_String --
   -----------------------------------------

   procedure Get_Unqualified_Decoded_Name_String (Id : Name_Id) is
   begin
      Get_Decoded_Name_String (Id);
      Strip_Qualification_And_Suffixes;
   end Get_Unqualified_Decoded_Name_String;

   ---------------------------------
   -- Get_Unqualified_Name_String --
   ---------------------------------

   procedure Get_Unqualified_Name_String (Id : Name_Id) is
   begin
      Get_Name_String (Id);
      Strip_Qualification_And_Suffixes;
   end Get_Unqualified_Name_String;

   ----------
   -- Hash --
   ----------

   function Hash return Hash_Index_Type is
      subtype Int_0_12 is Int range 0 .. 12;
      --  Used to avoid when others on case jump below

      Even_Name_Len : Integer;
      --  Last even numbered position (used for >12 case)

   begin

      --  Special test for 12 (rather than counting on a when others for the
      --  case statement below) avoids some Ada compilers converting the case
      --  statement into successive jumps.

      --  The case of a name longer than 12 characters is handled by taking
      --  the first 6 odd numbered characters and the last 6 even numbered
      --  characters

      if Name_Len > 12 then
         Even_Name_Len := (Name_Len) / 2 * 2;

         return ((((((((((((
           Character'Pos (Name_Buffer (01))) * 2 +
           Character'Pos (Name_Buffer (Even_Name_Len - 10))) * 2 +
           Character'Pos (Name_Buffer (03))) * 2 +
           Character'Pos (Name_Buffer (Even_Name_Len - 08))) * 2 +
           Character'Pos (Name_Buffer (05))) * 2 +
           Character'Pos (Name_Buffer (Even_Name_Len - 06))) * 2 +
           Character'Pos (Name_Buffer (07))) * 2 +
           Character'Pos (Name_Buffer (Even_Name_Len - 04))) * 2 +
           Character'Pos (Name_Buffer (09))) * 2 +
           Character'Pos (Name_Buffer (Even_Name_Len - 02))) * 2 +
           Character'Pos (Name_Buffer (11))) * 2 +
           Character'Pos (Name_Buffer (Even_Name_Len))) mod Hash_Num;
      end if;

      --  For the cases of 1-12 characters, all characters participate in the
      --  hash. The positioning is randomized, with the bias that characters
      --  later on participate fully (i.e. are added towards the right side).

      case Int_0_12 (Name_Len) is

         when 0 =>
            return 0;

         when 1 =>
            return
               Character'Pos (Name_Buffer (1));

         when 2 =>
            return ((
              Character'Pos (Name_Buffer (1))) * 64 +
              Character'Pos (Name_Buffer (2))) mod Hash_Num;

         when 3 =>
            return (((
              Character'Pos (Name_Buffer (1))) * 16 +
              Character'Pos (Name_Buffer (3))) * 16 +
              Character'Pos (Name_Buffer (2))) mod Hash_Num;

         when 4 =>
            return ((((
              Character'Pos (Name_Buffer (1))) * 8 +
              Character'Pos (Name_Buffer (2))) * 8 +
              Character'Pos (Name_Buffer (3))) * 8 +
              Character'Pos (Name_Buffer (4))) mod Hash_Num;

         when 5 =>
            return (((((
              Character'Pos (Name_Buffer (4))) * 8 +
              Character'Pos (Name_Buffer (1))) * 4 +
              Character'Pos (Name_Buffer (3))) * 4 +
              Character'Pos (Name_Buffer (5))) * 8 +
              Character'Pos (Name_Buffer (2))) mod Hash_Num;

         when 6 =>
            return ((((((
              Character'Pos (Name_Buffer (5))) * 4 +
              Character'Pos (Name_Buffer (1))) * 4 +
              Character'Pos (Name_Buffer (4))) * 4 +
              Character'Pos (Name_Buffer (2))) * 4 +
              Character'Pos (Name_Buffer (6))) * 4 +
              Character'Pos (Name_Buffer (3))) mod Hash_Num;

         when 7 =>
            return (((((((
              Character'Pos (Name_Buffer (4))) * 4 +
              Character'Pos (Name_Buffer (3))) * 4 +
              Character'Pos (Name_Buffer (1))) * 4 +
              Character'Pos (Name_Buffer (2))) * 2 +
              Character'Pos (Name_Buffer (5))) * 2 +
              Character'Pos (Name_Buffer (7))) * 2 +
              Character'Pos (Name_Buffer (6))) mod Hash_Num;

         when 8 =>
            return ((((((((
              Character'Pos (Name_Buffer (2))) * 4 +
              Character'Pos (Name_Buffer (1))) * 4 +
              Character'Pos (Name_Buffer (3))) * 2 +
              Character'Pos (Name_Buffer (5))) * 2 +
              Character'Pos (Name_Buffer (7))) * 2 +
              Character'Pos (Name_Buffer (6))) * 2 +
              Character'Pos (Name_Buffer (4))) * 2 +
              Character'Pos (Name_Buffer (8))) mod Hash_Num;

         when 9 =>
            return (((((((((
              Character'Pos (Name_Buffer (2))) * 4 +
              Character'Pos (Name_Buffer (1))) * 4 +
              Character'Pos (Name_Buffer (3))) * 4 +
              Character'Pos (Name_Buffer (4))) * 2 +
              Character'Pos (Name_Buffer (8))) * 2 +
              Character'Pos (Name_Buffer (7))) * 2 +
              Character'Pos (Name_Buffer (5))) * 2 +
              Character'Pos (Name_Buffer (6))) * 2 +
              Character'Pos (Name_Buffer (9))) mod Hash_Num;

         when 10 =>
            return ((((((((((
              Character'Pos (Name_Buffer (01))) * 2 +
              Character'Pos (Name_Buffer (02))) * 2 +
              Character'Pos (Name_Buffer (08))) * 2 +
              Character'Pos (Name_Buffer (03))) * 2 +
              Character'Pos (Name_Buffer (04))) * 2 +
              Character'Pos (Name_Buffer (09))) * 2 +
              Character'Pos (Name_Buffer (06))) * 2 +
              Character'Pos (Name_Buffer (05))) * 2 +
              Character'Pos (Name_Buffer (07))) * 2 +
              Character'Pos (Name_Buffer (10))) mod Hash_Num;

         when 11 =>
            return (((((((((((
              Character'Pos (Name_Buffer (05))) * 2 +
              Character'Pos (Name_Buffer (01))) * 2 +
              Character'Pos (Name_Buffer (06))) * 2 +
              Character'Pos (Name_Buffer (09))) * 2 +
              Character'Pos (Name_Buffer (07))) * 2 +
              Character'Pos (Name_Buffer (03))) * 2 +
              Character'Pos (Name_Buffer (08))) * 2 +
              Character'Pos (Name_Buffer (02))) * 2 +
              Character'Pos (Name_Buffer (10))) * 2 +
              Character'Pos (Name_Buffer (04))) * 2 +
              Character'Pos (Name_Buffer (11))) mod Hash_Num;

         when 12 =>
            return ((((((((((((
              Character'Pos (Name_Buffer (03))) * 2 +
              Character'Pos (Name_Buffer (02))) * 2 +
              Character'Pos (Name_Buffer (05))) * 2 +
              Character'Pos (Name_Buffer (01))) * 2 +
              Character'Pos (Name_Buffer (06))) * 2 +
              Character'Pos (Name_Buffer (04))) * 2 +
              Character'Pos (Name_Buffer (08))) * 2 +
              Character'Pos (Name_Buffer (11))) * 2 +
              Character'Pos (Name_Buffer (07))) * 2 +
              Character'Pos (Name_Buffer (09))) * 2 +
              Character'Pos (Name_Buffer (10))) * 2 +
              Character'Pos (Name_Buffer (12))) mod Hash_Num;

      end case;
   end Hash;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is

   begin
      Name_Chars.Init;
      Name_Entries.Init;

      --  Initialize entries for one character names

      for C in Character loop
         Name_Entries.Increment_Last;
         Name_Entries.Table (Name_Entries.Last).Name_Chars_Index :=
           Name_Chars.Last;
         Name_Entries.Table (Name_Entries.Last).Name_Len  := 1;
         Name_Entries.Table (Name_Entries.Last).Hash_Link := No_Name;
         Name_Entries.Table (Name_Entries.Last).Int_Info  := 0;
         Name_Entries.Table (Name_Entries.Last).Byte_Info := 0;
         Name_Chars.Increment_Last;
         Name_Chars.Table (Name_Chars.Last) := C;
         Name_Chars.Increment_Last;
         Name_Chars.Table (Name_Chars.Last) := ASCII.NUL;
      end loop;

      --  Clear hash table

      for J in Hash_Index_Type loop
         Hash_Table (J) := No_Name;
      end loop;
   end Initialize;

   ----------------------
   -- Is_Internal_Name --
   ----------------------

   function Is_Internal_Name (Id : Name_Id) return Boolean is
   begin
      Get_Name_String (Id);
      return Is_Internal_Name;
   end Is_Internal_Name;

   function Is_Internal_Name return Boolean is
   begin
      if Name_Buffer (1) = '_'
        or else Name_Buffer (Name_Len) = '_'
      then
         return True;

      else
         --  Test backwards, because we only want to test the last entity
         --  name if the name we have is qualified with other entities.

         for J in reverse 1 .. Name_Len loop
            if Is_OK_Internal_Letter (Name_Buffer (J)) then
               return True;

            --  Quit if we come to terminating double underscore (note that
            --  if the current character is an underscore, we know that
            --  there is a previous character present, since we already
            --  filtered out the case of Name_Buffer (1) = '_' above.

            elsif Name_Buffer (J) = '_'
              and then Name_Buffer (J - 1) = '_'
              and then Name_Buffer (J - 2) /= '_'
            then
               return False;
            end if;
         end loop;
      end if;

      return False;
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

   --------------------
   -- Length_Of_Name --
   --------------------

   function Length_Of_Name (Id : Name_Id) return Nat is
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
      Name_Chars.Locked := True;
      Name_Entries.Locked := True;
      Name_Chars.Release;
      Name_Entries.Release;
   end Lock;

   ------------------------
   -- Name_Chars_Address --
   ------------------------

   function Name_Chars_Address return System.Address is
   begin
      return Name_Chars.Table (0)'Address;
   end Name_Chars_Address;

   ----------------
   -- Name_Enter --
   ----------------

   function Name_Enter return Name_Id is
   begin
      Name_Entries.Increment_Last;
      Name_Entries.Table (Name_Entries.Last).Name_Chars_Index :=
        Name_Chars.Last;
      Name_Entries.Table (Name_Entries.Last).Name_Len  := Short (Name_Len);
      Name_Entries.Table (Name_Entries.Last).Hash_Link := No_Name;
      Name_Entries.Table (Name_Entries.Last).Int_Info  := 0;
      Name_Entries.Table (Name_Entries.Last).Byte_Info := 0;

      --  Set corresponding string entry in the Name_Chars table

      for J in 1 .. Name_Len loop
         Name_Chars.Increment_Last;
         Name_Chars.Table (Name_Chars.Last) := Name_Buffer (J);
      end loop;

      Name_Chars.Increment_Last;
      Name_Chars.Table (Name_Chars.Last) := ASCII.NUL;

      return Name_Entries.Last;
   end Name_Enter;

   --------------------------
   -- Name_Entries_Address --
   --------------------------

   function Name_Entries_Address return System.Address is
   begin
      return Name_Entries.Table (First_Name_Id)'Address;
   end Name_Entries_Address;

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

   function Name_Find return Name_Id is
      New_Id : Name_Id;
      --  Id of entry in hash search, and value to be returned

      S : Int;
      --  Pointer into string table

      Hash_Index : Hash_Index_Type;
      --  Computed hash index

   begin
      --  Quick handling for one character names

      if Name_Len = 1 then
         return Name_Id (First_Name_Id + Character'Pos (Name_Buffer (1)));

      --  Otherwise search hash table for existing matching entry

      else
         Hash_Index := Namet.Hash;
         New_Id := Hash_Table (Hash_Index);

         if New_Id = No_Name then
            Hash_Table (Hash_Index) := Name_Entries.Last + 1;

         else
            Search : loop
               if Name_Len /=
                 Integer (Name_Entries.Table (New_Id).Name_Len)
               then
                  goto No_Match;
               end if;

               S := Name_Entries.Table (New_Id).Name_Chars_Index;

               for I in 1 .. Name_Len loop
                  if Name_Chars.Table (S + Int (I)) /= Name_Buffer (I) then
                     goto No_Match;
                  end if;
               end loop;

               return New_Id;

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

         Name_Entries.Increment_Last;
         Name_Entries.Table (Name_Entries.Last).Name_Chars_Index :=
           Name_Chars.Last;
         Name_Entries.Table (Name_Entries.Last).Name_Len  := Short (Name_Len);
         Name_Entries.Table (Name_Entries.Last).Hash_Link := No_Name;
         Name_Entries.Table (Name_Entries.Last).Int_Info  := 0;
         Name_Entries.Table (Name_Entries.Last).Byte_Info := 0;

         --  Set corresponding string entry in the Name_Chars table

         for I in 1 .. Name_Len loop
            Name_Chars.Increment_Last;
            Name_Chars.Table (Name_Chars.Last) := Name_Buffer (I);
         end loop;

         Name_Chars.Increment_Last;
         Name_Chars.Table (Name_Chars.Last) := ASCII.NUL;

         return Name_Entries.Last;
      end if;
   end Name_Find;

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

   procedure Set_Character_Literal_Name (C : Char_Code) is
   begin
      Name_Buffer (1) := 'Q';
      Name_Len := 1;
      Store_Encoded_Character (C);
   end Set_Character_Literal_Name;

   -------------------------
   -- Set_Name_Table_Byte --
   -------------------------

   procedure Set_Name_Table_Byte (Id : Name_Id; Val : Byte) is
   begin
      pragma Assert (Id in Name_Entries.First .. Name_Entries.Last);
      Name_Entries.Table (Id).Byte_Info := Val;
   end Set_Name_Table_Byte;

   -------------------------
   -- Set_Name_Table_Info --
   -------------------------

   procedure Set_Name_Table_Info (Id : Name_Id; Val : Int) is
   begin
      pragma Assert (Id in Name_Entries.First .. Name_Entries.Last);
      Name_Entries.Table (Id).Int_Info := Val;
   end Set_Name_Table_Info;

   -----------------------------
   -- Store_Encoded_Character --
   -----------------------------

   procedure Store_Encoded_Character (C : Char_Code) is

      procedure Set_Hex_Chars (N : Natural);
      --  Stores given value, which is in the range 0 .. 255, as two hex
      --  digits (using lower case a-f) in Name_Buffer, incrementing Name_Len

      procedure Set_Hex_Chars (N : Natural) is
         Hexd : constant String := "0123456789abcdef";

      begin
         Name_Buffer (Name_Len + 1) := Hexd (N / 16 + 1);
         Name_Buffer (Name_Len + 2) := Hexd (N mod 16 + 1);
         Name_Len := Name_Len + 2;
      end Set_Hex_Chars;

   begin
      Name_Len := Name_Len + 1;

      if In_Character_Range (C) then
         declare
            CC : constant Character := Get_Character (C);

         begin
            if CC in 'a' .. 'z' or else CC in '0' .. '9' then
               Name_Buffer (Name_Len) := CC;

            else
               Name_Buffer (Name_Len) := 'U';
               Set_Hex_Chars (Natural (C));
            end if;
         end;

      else
         Name_Buffer (Name_Len) := 'W';
         Set_Hex_Chars (Natural (C) / 256);
         Set_Hex_Chars (Natural (C) mod 256);
      end if;

   end Store_Encoded_Character;

   --------------------------------------
   -- Strip_Qualification_And_Suffixes --
   --------------------------------------

   procedure Strip_Qualification_And_Suffixes is
      J : Integer;

   begin
      --  Strip package body qualification string off end

      for J in reverse 2 .. Name_Len loop
         if Name_Buffer (J) = 'X' then
            Name_Len := J - 1;
            exit;
         end if;

         exit when Name_Buffer (J) /= 'b'
           and then Name_Buffer (J) /= 'n'
           and then Name_Buffer (J) /= 'p';
      end loop;

      --  Find rightmost __ or $ separator if one exists. First we position
      --  to start the search. If we have a character constant, position
      --  just before it, otherwise position to last character but one

      if Name_Buffer (Name_Len) = ''' then
         J := Name_Len - 2;
         while J > 0 and then Name_Buffer (J) /= ''' loop
            J := J - 1;
         end loop;

      else
         J := Name_Len - 1;
      end if;

      --  Loop to search for rightmost __ or $ (homonym) separator

      while J > 1 loop

         --  If $ separator, homonym separator, so strip it and keep looking

         if Name_Buffer (J) = '$' then
            Name_Len := J - 1;
            J := Name_Len - 1;

         --  Else check for __ found

         elsif Name_Buffer (J) = '_' and then Name_Buffer (J + 1) = '_' then

            --  Found __ so see if digit follows, and if so, this is a
            --  homonym separator, so strip it and keep looking.

            if Name_Buffer (J + 2) in '0' .. '9' then
               Name_Len := J - 1;
               J := Name_Len - 1;

            --  If not a homonym separator, then we simply strip the
            --  separator and everything that precedes it, and we are done

            else
               Name_Buffer (1 .. Name_Len - J - 1) :=
                 Name_Buffer (J + 2 .. Name_Len);
               Name_Len := Name_Len - J - 1;
               exit;
            end if;

         else
            J := J - 1;
         end if;
      end loop;
   end Strip_Qualification_And_Suffixes;

   ---------------
   -- Tree_Read --
   ---------------

   procedure Tree_Read is
   begin
      Name_Chars.Tree_Read;
      Name_Entries.Tree_Read;

      Tree_Read_Data
        (Hash_Table'Address,
         Hash_Table'Length * (Hash_Table'Component_Size / Storage_Unit));
   end Tree_Read;

   ----------------
   -- Tree_Write --
   ----------------

   procedure Tree_Write is
   begin
      Name_Chars.Tree_Write;
      Name_Entries.Tree_Write;

      Tree_Write_Data
        (Hash_Table'Address,
         Hash_Table'Length * (Hash_Table'Component_Size / Storage_Unit));
   end Tree_Write;

   ------------
   -- Unlock --
   ------------

   procedure Unlock is
   begin
      Name_Chars.Set_Last (Name_Chars.Last - Name_Chars_Reserve);
      Name_Entries.Set_Last (Name_Entries.Last - Name_Entries_Reserve);
      Name_Chars.Locked := False;
      Name_Entries.Locked := False;
      Name_Chars.Release;
      Name_Entries.Release;
   end Unlock;

   --------
   -- wn --
   --------

   procedure wn (Id : Name_Id) is
   begin
      Write_Name (Id);
      Write_Eol;
   end wn;

   ----------------
   -- Write_Name --
   ----------------

   procedure Write_Name (Id : Name_Id) is
   begin
      if Id >= First_Name_Id then
         Get_Name_String (Id);
         Write_Str (Name_Buffer (1 .. Name_Len));
      end if;
   end Write_Name;

   ------------------------
   -- Write_Name_Decoded --
   ------------------------

   procedure Write_Name_Decoded (Id : Name_Id) is
   begin
      if Id >= First_Name_Id then
         Get_Decoded_Name_String (Id);
         Write_Str (Name_Buffer (1 .. Name_Len));
      end if;
   end Write_Name_Decoded;

end Namet;
