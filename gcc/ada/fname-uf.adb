------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             F N A M E . U F                              --
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
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT; see file COPYING3.  If not, go to --
-- http://www.gnu.org/licenses for a complete copy of the license.          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Alloc;
with Debug;    use Debug;
with Fmap;     use Fmap;
with Krunch;
with Opt;      use Opt;
with Osint;    use Osint;
with Table;
with Uname;    use Uname;
with Widechar; use Widechar;

with GNAT.HTable;

package body Fname.UF is

   --------------------------------------------------------
   -- Declarations for Handling Source_File_Name pragmas --
   --------------------------------------------------------

   type SFN_Entry is record
      U     : Unit_Name_Type; -- Unit name
      F     : File_Name_Type; -- Spec/Body file name
      Index : Nat;            -- Index from SFN pragma (0 if none)
   end record;
   --  Record single Unit_Name type call to Set_File_Name

   package SFN_Table is new Table.Table (
     Table_Component_Type => SFN_Entry,
     Table_Index_Type     => Int,
     Table_Low_Bound      => 0,
     Table_Initial        => Alloc.SFN_Table_Initial,
     Table_Increment      => Alloc.SFN_Table_Increment,
     Table_Name           => "SFN_Table");
   --  Table recording all Unit_Name calls to Set_File_Name

   type SFN_Header_Num is range 0 .. 100;

   function SFN_Hash (F : Unit_Name_Type) return SFN_Header_Num;
   --  Compute hash index for use by Simple_HTable

   No_Entry : constant Int := -1;
   --  Signals no entry in following table

   package SFN_HTable is new GNAT.HTable.Simple_HTable (
     Header_Num => SFN_Header_Num,
     Element    => Int,
     No_Element => No_Entry,
     Key        => Unit_Name_Type,
     Hash       => SFN_Hash,
     Equal      => "=");
   --  Hash table allowing rapid access to SFN_Table, the element value is an
   --  index into this table.

   type SFN_Pattern_Entry is record
      Pat : String_Ptr;   -- File name pattern (with asterisk in it)
      Typ : Character;    -- 'S'/'B'/'U' for spec/body/subunit
      Dot : String_Ptr;   -- Dot_Separator string
      Cas : Casing_Type;  -- Upper/Lower/Mixed
   end record;
   --  Records single call to Set_File_Name_Patterm

   package SFN_Patterns is new Table.Table (
     Table_Component_Type => SFN_Pattern_Entry,
     Table_Index_Type     => Int,
     Table_Low_Bound      => 1,
     Table_Initial        => 10,
     Table_Increment      => 100,
     Table_Name           => "SFN_Patterns");
   --  Table recording calls to Set_File_Name_Pattern. Note that the last two
   --  entries are set to represent the standard GNAT rules for file naming;
   --  that invariant is maintained by Set_File_Name_Pattern.

   procedure Instantiate_SFN_Pattern
     (Pattern   : SFN_Pattern_Entry;
      Buf       : in out Bounded_String;
      Is_Predef : Boolean := False);
   --  On entry, Buf must contain a unit name. After returning, Buf contains
   --  the file name corresponding to the unit following the naming pattern
   --  described by Pattern. Is_Predef must be whether the unit name in Buf
   --  is a predefined unit name as defined by Is_Predefined_Unit_Name.

   -----------------------
   -- File_Name_Of_Body --
   -----------------------

   function File_Name_Of_Body (Name : Name_Id) return File_Name_Type is
   begin
      Get_Name_String (Name);
      Name_Buffer (Name_Len + 1 .. Name_Len + 2) := "%b";
      Name_Len := Name_Len + 2;
      return Get_File_Name (Name_Enter, Subunit => False);
   end File_Name_Of_Body;

   -----------------------
   -- File_Name_Of_Spec --
   -----------------------

   function File_Name_Of_Spec (Name : Name_Id) return File_Name_Type is
   begin
      Get_Name_String (Name);
      Name_Buffer (Name_Len + 1 .. Name_Len + 2) := "%s";
      Name_Len := Name_Len + 2;
      return Get_File_Name (Name_Enter, Subunit => False);
   end File_Name_Of_Spec;

   ----------------------------
   -- Get_Expected_Unit_Type --
   ----------------------------

   function Get_Expected_Unit_Type
     (Fname : File_Name_Type) return Expected_Unit_Type
   is
   begin
      --  In syntax checking only mode or in multiple unit per file mode, there
      --  can be more than one unit in a file, so the file name is not a useful
      --  guide to the nature of the unit.

      if Operating_Mode = Check_Syntax
        or else Multiple_Unit_Index /= 0
      then
         return Unknown;
      end if;

      --  Search the file mapping table, if we find an entry for this file we
      --  know whether it is a spec or a body.

      for J in SFN_Table.First .. SFN_Table.Last loop
         if Fname = SFN_Table.Table (J).F then
            if Is_Body_Name (SFN_Table.Table (J).U) then
               return Expect_Body;
            else
               return Expect_Spec;
            end if;
         end if;
      end loop;

      --  If no entry in file naming table, assume .ads/.adb for spec/body and
      --  return unknown if we have neither of these two cases.

      Get_Name_String (Fname);

      if Name_Len > 4 then
         if Name_Buffer (Name_Len - 3 .. Name_Len) = ".ads" then
            return Expect_Spec;
         elsif Name_Buffer (Name_Len - 3 .. Name_Len) = ".adb" then
            return Expect_Body;
         end if;
      end if;

      return Unknown;
   end Get_Expected_Unit_Type;

   ---------------------------
   -- Get_Default_File_Name --
   ---------------------------

   function Get_Default_File_Name (Uname : Unit_Name_Type) return String is
      L : constant Int := SFN_Patterns.Last;

      Buf : Bounded_String;

      Pattern : SFN_Pattern_Entry;
   begin
      Get_Unit_Name_String (Buf, Uname, False);

      if Is_Spec_Name (Uname) then
         Pattern := SFN_Patterns.Table (L - 1);
      else
         pragma Assert (Is_Body_Name (Uname));
         Pattern := SFN_Patterns.Table (L);
      end if;

      Instantiate_SFN_Pattern (Pattern, Buf);

      return To_String (Buf);
   end Get_Default_File_Name;

   -------------------
   -- Get_File_Name --
   -------------------

   function Get_File_Name
     (Uname    : Unit_Name_Type;
      Subunit  : Boolean;
      May_Fail : Boolean := False) return File_Name_Type
   is
      Unit_Char : Character;
      --  Set to 's' or 'b' for spec or body or to 'u' for a subunit

      Unit_Char_Search : Character;
      --  Same as Unit_Char, except that in the case of 'u' for a subunit, we
      --  set Unit_Char_Search to 'b' if we do not find a subunit match.

      N : Int;

      Pname : File_Name_Type := No_File;
      Fname : File_Name_Type := No_File;
      --  Path name and File name for mapping

      Unit_Buf : Bounded_String;

   begin
      --  Null or error name means that some previous error occurred. This is
      --  an unrecoverable error, so signal it.

      if Uname in Error_Unit_Name_Or_No_Unit_Name then
         raise Unrecoverable_Error;
      end if;

      --  Look in the map from unit names to file names

      Fname := Mapped_File_Name (Uname);

      --  If the unit name is already mapped, return the corresponding file
      --  name from the map.

      if Fname /= No_File then
         return Fname;
      end if;

      --  If there is a specific SFN pragma, return the corresponding file name

      N := SFN_HTable.Get (Uname);

      if N /= No_Entry then
         return SFN_Table.Table (N).F;
      end if;

      --  Here for the case where the name was not found in the table

      Append_Decoded (Unit_Buf, Uname);

      --  A special fudge, normally we don't have operator symbols present,
      --  since it is always an error to do so. However, if we do, at this
      --  stage it has a leading double quote.

      --  What we do in this case is to go back to the undecoded name, which
      --  is of the form, for example:

      --    Oand%s

      --  and build a file name that looks like:

      --    _and_.ads

      --  which is bit peculiar, but we keep it that way. This means that we
      --  avoid bombs due to writing a bad file name, and we get expected error
      --  processing downstream, e.g. a compilation following gnatchop.

      if Unit_Buf.Chars (1) = '"' then
         Unit_Buf.Length := 0;
         Append (Unit_Buf, Uname);
         Unit_Buf.Length := Unit_Buf.Length + 1;
         Unit_Buf.Chars (Unit_Buf.Length) :=
           Unit_Buf.Chars (Unit_Buf.Length - 1);
         Unit_Buf.Chars (Unit_Buf.Length - 1) :=
           Unit_Buf.Chars (Unit_Buf.Length - 2);
         Unit_Buf.Chars (Unit_Buf.Length - 2) := '_';
         Unit_Buf.Chars (1) := '_';
      end if;

      --  Deal with spec or body suffix

      Unit_Char := Unit_Buf.Chars (Unit_Buf.Length);
      pragma Assert (Unit_Char = 'b' or else Unit_Char = 's');
      pragma
        Assert
          (Unit_Buf.Length >= 3
             and then Unit_Buf.Chars (Unit_Buf.Length - 1) = '%');
      Unit_Buf.Length := Unit_Buf.Length - 2;

      if Subunit then
         Unit_Char := 'u';
      end if;

      --  Now we need to find the proper translation of the name

      declare
         Pent : Nat;
         Fnam : File_Name_Type := No_File;

      --  Start of search through pattern table

      begin
         --  Search pattern table to find a matching entry. In the general case
         --  we do two complete searches. The first time through we stop only
         --  if a matching file is found, the second time through we accept the
         --  first match regardless. Note that there will always be a match the
         --  second time around, because of the default entries at the end of
         --  the table.

         for No_File_Check in False .. True loop
            Unit_Char_Search := Unit_Char;

         <<Repeat_Search>>
         --  The search is repeated with Unit_Char_Search set to b, if an
         --  initial search for the subunit case fails to find any match.

            Pent := SFN_Patterns.First;
            while Pent <= SFN_Patterns.Last loop
               if SFN_Patterns.Table (Pent).Typ = Unit_Char_Search then
                  --  Found a match, execute the pattern
                  declare
                     Is_Predef : constant Boolean :=
                       Is_Predefined_Unit_Name
                         (Uname, Renamings_Included => True);

                     Buf : Bounded_String;
                  begin
                     Append (Buf, Unit_Buf);

                     Instantiate_SFN_Pattern
                       (SFN_Patterns.Table (Pent), Buf, Is_Predef);

                     Fnam := Name_Find (Buf);
                  end;

                  --  If we are in the second search of the table, we accept
                  --  the file name without checking, because we know that the
                  --  file does not exist, except when May_Fail is True, in
                  --  which case we return No_File.

                  if No_File_Check then
                     if May_Fail then
                        return No_File;
                     else
                        return Fnam;
                     end if;

                  --  Otherwise we check if the file exists

                  else
                     Pname := Find_File (Fnam, Source);

                     --  If it does exist, we add it to the mappings and return
                     --  the file name.

                     if Pname /= No_File then

                        --  Add to mapping, so that we don't do another path
                        --  search in Find_File for this file name and, if we
                        --  use a mapping file, we are ready to update it at
                        --  the end of this compilation for the benefit of
                        --  other compilation processes.

                        Add_To_File_Map (Get_File_Name.Uname, Fnam, Pname);
                        return Fnam;

                     --  If there are only two entries, they are those of the
                     --  default GNAT naming scheme. The file does not exist,
                     --  but there is no point doing the second search, because
                     --  we will end up with the same file name. Just return
                     --  the file name, or No_File if May_Fail is True.

                     elsif SFN_Patterns.Last = 2 then
                        if May_Fail then
                           return No_File;
                        else
                           return Fnam;
                        end if;

                     --  The file does not exist, but there may be other naming
                     --  scheme. Keep on searching.

                     else
                        Fnam := No_File;
                     end if;
                  end if;
               end if;

               Pent := Pent + 1;
            end loop;

            --  If search failed, and was for a subunit, repeat the search with
            --  Unit_Char_Search reset to 'b', since in the normal case we
            --  simply treat subunits as bodies.

            if Fnam = No_File and then Unit_Char_Search = 'u' then
               Unit_Char_Search := 'b';
               goto Repeat_Search;
            end if;

            --  Repeat entire search in No_File_Check mode if necessary

         end loop;

         --  Something is wrong if search fails completely, since the default
         --  entries should catch all possibilities at this stage.

         raise Program_Error;
      end;
   end Get_File_Name;

   --------------------
   -- Get_Unit_Index --
   --------------------

   function Get_Unit_Index (Uname : Unit_Name_Type) return Nat is
      N : constant Int := SFN_HTable.Get (Uname);
   begin
      if N /= No_Entry then
         return SFN_Table.Table (N).Index;
      else
         return 0;
      end if;
   end Get_Unit_Index;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      SFN_Table.Init;
      SFN_Patterns.Init;

      --  Add default entries to SFN_Patterns.Table to represent the standard
      --  default GNAT rules for file name translation.

      SFN_Patterns.Append (New_Val =>
        (Pat => new String'("*.ads"),
         Typ => 's',
         Dot => new String'("-"),
         Cas => All_Lower_Case));

      SFN_Patterns.Append (New_Val =>
        (Pat => new String'("*.adb"),
         Typ => 'b',
         Dot => new String'("-"),
         Cas => All_Lower_Case));
   end Initialize;

   -----------------------------
   -- Instantiate_SFN_Pattern --
   -----------------------------

   procedure Instantiate_SFN_Pattern
     (Pattern   : SFN_Pattern_Entry;
      Buf       : in out Bounded_String;
      Is_Predef : Boolean := False)
   is
      function C (N : Natural) return Character;
      --  Return N'th character of pattern

      function C (N : Natural) return Character is
      begin
         return Pattern.Pat (N);
      end C;

      Dot : constant String_Ptr := Pattern.Dot;

      Dotl : constant Natural := Dot.all'Length;

      Plen : constant Natural := Pattern.Pat'Length;

      J : Natural;
   begin
      --  Apply casing, except that we do not do this for the case
      --  of a predefined library file. For the latter, we always
      --  use the all lower case name, regardless of the setting.

      if not Is_Predef then
         Set_Casing (Buf, Pattern.Cas);
      end if;

      --  If dot translation required do it

      if Dot.all /= "." then
         J := 1;

         while J <= Buf.Length loop
            if Buf.Chars (J) = '.' then

               if Dotl = 1 then
                  Buf.Chars (J) := Dot (Dot'First);

               else
                  Buf.Chars (J + Dotl .. Buf.Length + Dotl - 1) :=
                    Buf.Chars (J + 1 .. Buf.Length);
                  Buf.Chars (J .. J + Dotl - 1) := Dot.all;
                  Buf.Length := Buf.Length + Dotl - 1;
               end if;

               J := J + Dotl;

            --  Skip past wide char sequences to avoid messing with
            --  dot characters that are part of a sequence.

            elsif Buf.Chars (J) = ASCII.ESC
              or else (Upper_Half_Encoding
                        and then
                          Buf.Chars (J) in Upper_Half_Character)
            then
               Skip_Wide (Buf.Chars, J);
            else
               J := J + 1;
            end if;
         end loop;
      end if;

      --  Here move result to right if preinsertion before *

      for K in 1 .. Plen loop
         if C (K) = '*' then
            if K /= 1 then
               Buf.Chars (1 + K - 1 .. Buf.Length + K - 1) :=
                 Buf.Chars (1 .. Buf.Length);

               for L in 1 .. K - 1 loop
                  Buf.Chars (L) := C (L);
               end loop;

               Buf.Length := Buf.Length + K - 1;
            end if;

            for L in K + 1 .. Plen loop
               Buf.Length := Buf.Length + 1;
               Buf.Chars (Buf.Length) := C (L);
            end loop;

            exit;
         end if;
      end loop;

      --  Execute possible crunch on constructed name. The krunch
      --  operation excludes any extension that may be present.

      J := Buf.Length;
      while J > 1 loop
         exit when Buf.Chars (J) = '.';
         J := J - 1;
      end loop;

      --  Case of extension present

      if J > 1 then
         declare
            Ext : constant String := Buf.Chars (J .. Buf.Length);

         begin
            --  Remove extension

            Buf.Length := J - 1;

            --  Krunch what's left

            Krunch
              (Buf.Chars,
               Buf.Length,
               Integer (Maximum_File_Name_Length),
               Debug_Flag_4);

            --  Replace extension

            Buf.Chars
              (Buf.Length + 1 .. Buf.Length + Ext'Length) := Ext;
            Buf.Length := Buf.Length + Ext'Length;
         end;

      --  Case of no extension present, straight krunch on the
      --  entire file name.

      else
         Krunch
           (Buf.Chars,
            Buf.Length,
            Integer (Maximum_File_Name_Length),
            Debug_Flag_4);
      end if;
   end Instantiate_SFN_Pattern;

   ----------
   -- Lock --
   ----------

   procedure Lock is
   begin
      SFN_Table.Release;
      SFN_Table.Locked := True;
   end Lock;

   -------------------
   -- Set_File_Name --
   -------------------

   procedure Set_File_Name
     (U     : Unit_Name_Type;
      F     : File_Name_Type;
      Index : Nat)
   is
   begin
      SFN_Table.Increment_Last;
      SFN_Table.Table (SFN_Table.Last) := (U, F, Index);
      SFN_HTable.Set (U, SFN_Table.Last);
   end Set_File_Name;

   ---------------------------
   -- Set_File_Name_Pattern --
   ---------------------------

   procedure Set_File_Name_Pattern
     (Pat : String_Ptr;
      Typ : Character;
      Dot : String_Ptr;
      Cas : Casing_Type)
   is
      L : constant Nat := SFN_Patterns.Last;

   begin
      SFN_Patterns.Increment_Last;

      --  Move up the last two entries (the default ones) and then put the new
      --  entry into the table just before them (we always have the default
      --  entries be the last ones).

      SFN_Patterns.Table (L + 1) := SFN_Patterns.Table (L);
      SFN_Patterns.Table (L)     := SFN_Patterns.Table (L - 1);
      SFN_Patterns.Table (L - 1) := (Pat, Typ, Dot, Cas);
   end Set_File_Name_Pattern;

   --------------
   -- SFN_Hash --
   --------------

   function SFN_Hash (F : Unit_Name_Type) return SFN_Header_Num is
   begin
      return SFN_Header_Num (Int (F) mod SFN_Header_Num'Range_Length);
   end SFN_Hash;

begin

   --  We call the initialization routine from the package body, so that
   --  Fname.Init only needs to be called explicitly to reinitialize.

   Fname.UF.Initialize;
end Fname.UF;
