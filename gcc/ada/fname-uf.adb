------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             F N A M E . U F                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.4 $
--                                                                          --
--          Copyright (C) 1992-2001, Free Software Foundation, Inc.         --
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

with Alloc;
with Debug;    use Debug;
with Fmap;     use Fmap;
with Krunch;
with Namet;    use Namet;
with Opt;      use Opt;
with Osint;    use Osint;
with Table;
with Widechar; use Widechar;

with GNAT.HTable;

package body Fname.UF is

   --------------------------------------------------------
   -- Declarations for Handling Source_File_Name pragmas --
   --------------------------------------------------------

   type SFN_Entry is record
      U : Unit_Name_Type; -- Unit name
      F : File_Name_Type; -- Spec/Body file name
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
   --  Hash table allowing rapid access to SFN_Table, the element value
   --  is an index into this table.

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
   --  Table recording all calls to Set_File_Name_Pattern. Note that the
   --  first two entries are set to represent the standard GNAT rules
   --  for file naming.

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

   -------------------
   -- Get_File_Name --
   -------------------

   function Get_File_Name
     (Uname   : Unit_Name_Type;
      Subunit : Boolean)
      return    File_Name_Type
   is
      Unit_Char : Character;
      --  Set to 's' or 'b' for spec or body or to 'u' for a subunit

      Unit_Char_Search : Character;
      --  Same as Unit_Char, except that in the case of 'u' for a subunit,
      --  we set Unit_Char_Search to 'b' if we do not find a subunit match.

      N : Int;

      Pname : File_Name_Type := No_File;
      Fname : File_Name_Type := No_File;
      --  Path name and File name for mapping

   begin
      --  Null or error name means that some previous error occurred
      --  This is an unrecoverable error, so signal it.

      if Uname <= Error_Name then
         raise Unrecoverable_Error;
      end if;

      --  Look in the map from unit names to file names

      Fname := Mapped_File_Name (Uname);

      --  If the unit name is already mapped, return the corresponding
      --  file name from the map.

      if Fname /= No_File then
         return Fname;
      end if;

      --  If there is a specific SFN pragma, return the corresponding file name

      N := SFN_HTable.Get (Uname);

      if N /= No_Entry then
         return SFN_Table.Table (N).F;
      end if;

      --  Here for the case where the name was not found in the table

      Get_Decoded_Name_String (Uname);

      --  A special fudge, normally we don't have operator symbols present,
      --  since it is always an error to do so. However, if we do, at this
      --  stage it has a leading double quote.

      --  What we do in this case is to go back to the undecoded name, which
      --  is of the form, for example:

      --    Oand%s

      --  and build a file name that looks like:

      --    _and_.ads

      --  which is bit peculiar, but we keep it that way. This means that
      --  we avoid bombs due to writing a bad file name, and w get expected
      --  error processing downstream, e.g. a compilation following gnatchop.

      if Name_Buffer (1) = '"' then
         Get_Name_String (Uname);
         Name_Len := Name_Len + 1;
         Name_Buffer (Name_Len)     := Name_Buffer (Name_Len - 1);
         Name_Buffer (Name_Len - 1) := Name_Buffer (Name_Len - 2);
         Name_Buffer (Name_Len - 2) := '_';
         Name_Buffer (1)            := '_';
      end if;

      --  Deal with spec or body suffix

      Unit_Char := Name_Buffer (Name_Len);
      pragma Assert (Unit_Char = 'b' or else Unit_Char = 's');
      pragma Assert (Name_Len >= 3 and then Name_Buffer (Name_Len - 1) = '%');
      Name_Len := Name_Len - 2;

      if Subunit then
         Unit_Char := 'u';
      end if;

      --  Now we need to find the proper translation of the name

      declare
         Uname : constant String (1 .. Name_Len) :=
                   Name_Buffer (1 .. Name_Len);

         Pent : Nat;
         Plen : Natural;
         Fnam : File_Name_Type := No_File;
         J    : Natural;
         Dot  : String_Ptr;
         Dotl : Natural;

         function C (N : Natural) return Character;
         --  Return N'th character of pattern

         function C (N : Natural) return Character is
         begin
            return SFN_Patterns.Table (Pent).Pat (N);
         end C;

      --  Start of search through pattern table

      begin
         --  Search pattern table to find a matching entry. In the general
         --  case we do two complete searches. The first time through we
         --  stop only if a matching file is found, the second time through
         --  we accept the first match regardless. Note that there will
         --  always be a match the second time around, because of the
         --  default entries at the end of the table.

         for No_File_Check in False .. True loop
            Unit_Char_Search := Unit_Char;

         <<Repeat_Search>>
         --  The search is repeated with Unit_Char_Search set to b, if an
         --  initial search for the subunit case fails to find any match.

            Pent := SFN_Patterns.First;
            while Pent <= SFN_Patterns.Last loop
               if SFN_Patterns.Table (Pent).Typ = Unit_Char_Search then
                  Name_Len := 0;

                  --  Found a match, execute the pattern

                  Name_Len := Uname'Length;
                  Name_Buffer (1 .. Name_Len) := Uname;
                  Set_Casing (SFN_Patterns.Table (Pent).Cas);

                  --  If dot translation required do it

                  Dot  := SFN_Patterns.Table (Pent).Dot;
                  Dotl := Dot.all'Length;

                  if Dot.all /= "." then
                     J := 1;

                     while J <= Name_Len loop
                        if Name_Buffer (J) = '.' then

                           if Dotl = 1 then
                              Name_Buffer (J) := Dot (Dot'First);

                           else
                              Name_Buffer (J + Dotl .. Name_Len + Dotl - 1) :=
                                Name_Buffer (J + 1 .. Name_Len);
                              Name_Buffer (J .. J + Dotl - 1) := Dot.all;
                              Name_Len := Name_Len + Dotl - 1;
                           end if;

                           J := J + Dotl;

                        --  Skip past wide char sequences to avoid messing
                        --  with dot characters that are part of a sequence.

                        elsif Name_Buffer (J) = ASCII.ESC
                          or else (Upper_Half_Encoding
                                    and then
                                      Name_Buffer (J) in Upper_Half_Character)
                        then
                           Skip_Wide (Name_Buffer, J);
                        else
                           J := J + 1;
                        end if;
                     end loop;
                  end if;

                  --  Here move result to right if preinsertion before *

                  Plen := SFN_Patterns.Table (Pent).Pat'Length;
                  for K in 1 .. Plen loop
                     if C (K) = '*' then
                        if K /= 1 then
                           Name_Buffer (1 + K - 1 .. Name_Len + K - 1) :=
                             Name_Buffer (1 .. Name_Len);

                           for L in 1 .. K - 1 loop
                              Name_Buffer (L) := C (L);
                           end loop;

                           Name_Len := Name_Len + K - 1;
                        end if;

                        for L in K + 1 .. Plen loop
                           Name_Len := Name_Len + 1;
                           Name_Buffer (Name_Len) := C (L);
                        end loop;

                        exit;
                     end if;
                  end loop;

                  --  Execute possible crunch on constructed name. The krunch
                  --  operation excludes any extension that may be present.

                  J := Name_Len;
                  while J > 1 loop
                     exit when Name_Buffer (J) = '.';
                     J := J - 1;
                  end loop;

                  --  Case of extension present

                  if J > 1 then
                     declare
                        Ext : constant String := Name_Buffer (J .. Name_Len);

                     begin
                        --  Remove extension

                        Name_Len := J - 1;

                        --  Krunch what's left

                        Krunch
                          (Name_Buffer,
                           Name_Len,
                           Integer (Maximum_File_Name_Length),
                           Debug_Flag_4);

                        --  Replace extension

                        Name_Buffer
                          (Name_Len + 1 .. Name_Len + Ext'Length) := Ext;
                        Name_Len := Name_Len + Ext'Length;
                     end;

                  --  Case of no extension present, straight krunch on
                  --  the entire file name.

                  else
                     Krunch
                       (Name_Buffer,
                        Name_Len,
                        Integer (Maximum_File_Name_Length),
                        Debug_Flag_4);
                  end if;

                  Fnam := File_Name_Type (Name_Find);

                  --  If we are in the first search of the table, then
                  --  we check if the file is present, and only accept
                  --  the entry if it is indeed present. For the second
                  --  search, we accept the entry without this check.

                  --  If we only have two entries in the table, then there
                  --  is no point in seeing if the file exists, since we
                  --  will end up accepting it anyway on the second search,
                  --  so just quit and accept it now to save time.

                  if No_File_Check or else SFN_Patterns.Last = 2 then
                     return Fnam;

                  --  Check if file exists and if so, return the entry

                  else
                     Pname := Find_File (Fnam, Source);

                  --  Check if file exists and if so, return the entry

                     if Pname /= No_File then

                        --  Add to mapping, so that we don't do another
                        --  path search in Find_File for this file name

                        Add_To_File_Map (Get_File_Name.Uname, Fnam, Pname);
                        return Fnam;

                     --  This entry does not match after all, because this is
                     --  the first search loop, and the file does not exist.

                     else
                        Fnam := No_File;
                     end if;
                  end if;
               end if;

               Pent := Pent + 1;
            end loop;

            --  If search failed, and was for a subunit, repeat the search
            --  with Unit_Char_Search reset to 'b', since in the normal case
            --  we simply treat subunits as bodies.

            if Fnam = No_File and then Unit_Char_Search = 'u' then
               Unit_Char_Search := 'b';
               goto Repeat_Search;
            end if;

            --  Repeat entire search in No_File_Check mode if necessary

         end loop;

         --  Something is wrong if search fails completely, since the
         --  default entries should catch all possibilities at this stage.

         raise Program_Error;
      end;
   end Get_File_Name;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      SFN_Table.Init;
      SFN_Patterns.Init;

      --  Add default entries to SFN_Patterns.Table to represent the
      --  standard default GNAT rules for file name translation.

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

   ----------
   -- Lock --
   ----------

   procedure Lock is
   begin
      SFN_Table.Locked := True;
      SFN_Table.Release;
   end Lock;

   -------------------
   -- Set_File_Name --
   -------------------

   procedure Set_File_Name (U : Unit_Name_Type; F : File_Name_Type) is
   begin
      SFN_Table.Increment_Last;
      SFN_Table.Table (SFN_Table.Last) := (U, F);
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

      --  Move up the last two entries (the default ones) and then
      --  put the new entry into the table just before them (we
      --  always have the default entries be the last ones).

      SFN_Patterns.Table (L + 1) := SFN_Patterns.Table (L);
      SFN_Patterns.Table (L)     := SFN_Patterns.Table (L - 1);
      SFN_Patterns.Table (L - 1) := (Pat, Typ, Dot, Cas);
   end Set_File_Name_Pattern;

   --------------
   -- SFN_Hash --
   --------------

   function SFN_Hash (F : Unit_Name_Type) return SFN_Header_Num is
   begin
      return SFN_Header_Num (Int (F) rem SFN_Header_Num'Range_Length);
   end SFN_Hash;

begin

   --  We call the initialization routine from the package body, so that
   --  Fname.Init only needs to be called explicitly to reinitialize.

   Fname.UF.Initialize;
end Fname.UF;
