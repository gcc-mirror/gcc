------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             X R  _ T A B L S                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1998-2008, Free Software Foundation, Inc.         --
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

with Types;    use Types;
with Osint;
with Hostparm;

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with Ada.Strings.Fixed;
with Ada.Strings;
with Ada.Text_IO;
with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Ada.Strings.Unbounded;     use Ada.Strings.Unbounded;

with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.HTable;               use GNAT.HTable;
with GNAT.Heap_Sort_G;

package body Xr_Tabls is

   type HTable_Headers is range 1 .. 10000;

   procedure Set_Next (E : File_Reference; Next : File_Reference);
   function  Next (E : File_Reference) return File_Reference;
   function  Get_Key (E : File_Reference) return Cst_String_Access;
   function  Hash (F : Cst_String_Access) return HTable_Headers;
   function  Equal (F1, F2 : Cst_String_Access) return Boolean;
   --  The five subprograms above are used to instantiate the static
   --  htable to store the files that should be processed.

   package File_HTable is new GNAT.HTable.Static_HTable
     (Header_Num => HTable_Headers,
      Element    => File_Record,
      Elmt_Ptr   => File_Reference,
      Null_Ptr   => null,
      Set_Next   => Set_Next,
      Next       => Next,
      Key        => Cst_String_Access,
      Get_Key    => Get_Key,
      Hash       => Hash,
      Equal      => Equal);
   --  A hash table to store all the files referenced in the
   --  application.  The keys in this htable are the name of the files
   --  themselves, therefore it is assumed that the source path
   --  doesn't contain twice the same source or ALI file name

   type Unvisited_Files_Record;
   type Unvisited_Files_Access is access Unvisited_Files_Record;
   type Unvisited_Files_Record is record
      File : File_Reference;
      Next : Unvisited_Files_Access;
   end record;
   --  A special list, in addition to File_HTable, that only stores
   --  the files that haven't been visited so far. Note that the File
   --  list points to some data in File_HTable, and thus should never be freed.

   function Next (E : Declaration_Reference) return Declaration_Reference;
   procedure Set_Next (E, Next : Declaration_Reference);
   function  Get_Key (E : Declaration_Reference) return Cst_String_Access;
   --  The subprograms above are used to instantiate the static
   --  htable to store the entities that have been found in the application

   package Entities_HTable is new GNAT.HTable.Static_HTable
     (Header_Num => HTable_Headers,
      Element    => Declaration_Record,
      Elmt_Ptr   => Declaration_Reference,
      Null_Ptr   => null,
      Set_Next   => Set_Next,
      Next       => Next,
      Key        => Cst_String_Access,
      Get_Key    => Get_Key,
      Hash       => Hash,
      Equal      => Equal);
   --  A hash table to store all the entities defined in the
   --  application. For each entity, we store a list of its reference
   --  locations as well.
   --  The keys in this htable should be created with Key_From_Ref,
   --  and are the file, line and column of the declaration, which are
   --  unique for every entity.

   Entities_Count : Natural := 0;
   --  Number of entities in Entities_HTable. This is used in the end
   --  when sorting the table.

   Longest_File_Name_In_Table : Natural := 0;
   Unvisited_Files            : Unvisited_Files_Access := null;
   Directories                : Project_File_Ptr;
   Default_Match              : Boolean := False;
   --  The above need commenting ???

   function Parse_Gnatls_Src return String;
   --  Return the standard source directories (taking into account the
   --  ADA_INCLUDE_PATH environment variable, if Osint.Add_Default_Search_Dirs
   --  was called first).

   function Parse_Gnatls_Obj return String;
   --  Return the standard object directories (taking into account the
   --  ADA_OBJECTS_PATH environment variable).

   function Key_From_Ref
     (File_Ref  : File_Reference;
      Line      : Natural;
      Column    : Natural)
      return      String;
   --  Return a key for the symbol declared at File_Ref, Line,
   --  Column. This key should be used for lookup in Entity_HTable

   function Is_Less_Than (Decl1, Decl2 : Declaration_Reference) return Boolean;
   --  Compare two declarations (the comparison is case-insensitive)

   function Is_Less_Than (Ref1, Ref2 : Reference) return Boolean;
   --  Compare two references

   procedure Store_References
     (Decl            : Declaration_Reference;
      Get_Writes      : Boolean := False;
      Get_Reads       : Boolean := False;
      Get_Bodies      : Boolean := False;
      Get_Declaration : Boolean := False;
      Arr             : in out Reference_Array;
      Index           : in out Natural);
   --  Store in Arr, starting at Index, all the references to Decl. The Get_*
   --  parameters can be used to indicate which references should be stored.
   --  Constraint_Error will be raised if Arr is not big enough.

   procedure Sort (Arr : in out Reference_Array);
   --  Sort an array of references (Arr'First must be 1)

   --------------
   -- Set_Next --
   --------------

   procedure Set_Next (E : File_Reference; Next : File_Reference) is
   begin
      E.Next := Next;
   end Set_Next;

   procedure Set_Next
     (E : Declaration_Reference; Next : Declaration_Reference) is
   begin
      E.Next := Next;
   end Set_Next;

   -------------
   -- Get_Key --
   -------------

   function Get_Key (E : File_Reference) return Cst_String_Access is
   begin
      return E.File;
   end Get_Key;

   function Get_Key (E : Declaration_Reference) return Cst_String_Access is
   begin
      return E.Key;
   end Get_Key;

   ----------
   -- Hash --
   ----------

   function Hash (F : Cst_String_Access) return HTable_Headers is
      function H is new GNAT.HTable.Hash (HTable_Headers);

   begin
      return H (F.all);
   end Hash;

   -----------
   -- Equal --
   -----------

   function Equal (F1, F2 : Cst_String_Access) return Boolean is
   begin
      return F1.all = F2.all;
   end Equal;

   ------------------
   -- Key_From_Ref --
   ------------------

   function Key_From_Ref
     (File_Ref : File_Reference;
      Line     : Natural;
      Column   : Natural)
      return     String
   is
   begin
      return File_Ref.File.all & Natural'Image (Line) & Natural'Image (Column);
   end Key_From_Ref;

   ---------------------
   -- Add_Declaration --
   ---------------------

   function Add_Declaration
     (File_Ref     : File_Reference;
      Symbol       : String;
      Line         : Natural;
      Column       : Natural;
      Decl_Type    : Character;
      Remove_Only  : Boolean := False;
      Symbol_Match : Boolean := True)
      return         Declaration_Reference
   is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Declaration_Record, Declaration_Reference);

      Key : aliased constant String := Key_From_Ref (File_Ref, Line, Column);

      New_Decl : Declaration_Reference :=
                   Entities_HTable.Get (Key'Unchecked_Access);

      Is_Parameter : Boolean := False;

   begin
      --  Insert the Declaration in the table. There might already be a
      --  declaration in the table if the entity is a parameter, so we
      --  need to check that first.

      if New_Decl /= null and then New_Decl.Symbol_Length = 0 then
         Is_Parameter := New_Decl.Is_Parameter;
         Entities_HTable.Remove (Key'Unrestricted_Access);
         Entities_Count := Entities_Count - 1;
         Free (New_Decl.Key);
         Unchecked_Free (New_Decl);
         New_Decl := null;
      end if;

      --  The declaration might also already be there for parent types. In
      --  this case, we should keep the entry, since some other entries are
      --  pointing to it.

      if New_Decl = null
        and then not Remove_Only
      then
         New_Decl :=
           new Declaration_Record'
             (Symbol_Length => Symbol'Length,
              Symbol        => Symbol,
              Key           => new String'(Key),
              Decl          => new Reference_Record'
                                     (File          => File_Ref,
                                      Line          => Line,
                                      Column        => Column,
                                      Source_Line   => null,
                                      Next          => null),
              Is_Parameter  => Is_Parameter,
              Decl_Type     => Decl_Type,
              Body_Ref      => null,
              Ref_Ref       => null,
              Modif_Ref     => null,
              Match         => Symbol_Match
                                 and then
                                   (Default_Match
                                     or else Match (File_Ref, Line, Column)),
              Par_Symbol    => null,
              Next          => null);

         Entities_HTable.Set (New_Decl);
         Entities_Count := Entities_Count + 1;

         if New_Decl.Match then
            Longest_File_Name_In_Table :=
              Natural'Max (File_Ref.File'Length, Longest_File_Name_In_Table);
         end if;

      elsif New_Decl /= null
        and then not New_Decl.Match
      then
         New_Decl.Match := Default_Match
           or else Match (File_Ref, Line, Column);
      end if;

      return New_Decl;
   end Add_Declaration;

   ----------------------
   -- Add_To_Xref_File --
   ----------------------

   function Add_To_Xref_File
     (File_Name       : String;
      Visited         : Boolean := True;
      Emit_Warning    : Boolean := False;
      Gnatchop_File   : String  := "";
      Gnatchop_Offset : Integer := 0) return File_Reference
   is
      Base    : aliased constant String := Base_Name (File_Name);
      Dir     : constant String := Dir_Name (File_Name);
      Dir_Acc : GNAT.OS_Lib.String_Access   := null;
      Ref     : File_Reference;

   begin
      --  Do we have a directory name as well?

      if File_Name /= Base then
         Dir_Acc := new String'(Dir);
      end if;

      Ref := File_HTable.Get (Base'Unchecked_Access);
      if Ref = null then
         Ref := new File_Record'
           (File            => new String'(Base),
            Dir             => Dir_Acc,
            Lines           => null,
            Visited         => Visited,
            Emit_Warning    => Emit_Warning,
            Gnatchop_File   => new String'(Gnatchop_File),
            Gnatchop_Offset => Gnatchop_Offset,
            Next            => null);
         File_HTable.Set (Ref);

         if not Visited then

            --  Keep a separate list for faster access

            Set_Unvisited (Ref);
         end if;
      end if;
      return Ref;
   end Add_To_Xref_File;

   --------------
   -- Add_Line --
   --------------

   procedure Add_Line
     (File   : File_Reference;
      Line   : Natural;
      Column : Natural)
   is
   begin
      File.Lines := new Ref_In_File'(Line   => Line,
                                     Column => Column,
                                     Next   => File.Lines);
   end Add_Line;

   ----------------
   -- Add_Parent --
   ----------------

   procedure Add_Parent
     (Declaration : in out Declaration_Reference;
      Symbol      : String;
      Line        : Natural;
      Column      : Natural;
      File_Ref    : File_Reference)
   is
   begin
      Declaration.Par_Symbol :=
        Add_Declaration
          (File_Ref, Symbol, Line, Column,
           Decl_Type    => ' ',
           Symbol_Match => False);
   end Add_Parent;

   -------------------
   -- Add_Reference --
   -------------------

   procedure Add_Reference
     (Declaration   : Declaration_Reference;
      File_Ref      : File_Reference;
      Line          : Natural;
      Column        : Natural;
      Ref_Type      : Character;
      Labels_As_Ref : Boolean)
   is
      New_Ref : Reference;

   begin
      case Ref_Type is
         when 'b' | 'c' | 'm' | 'r' | 'R' | 'i' | ' ' | 'x' =>
            null;

         when 'l' | 'w' =>
            if not Labels_As_Ref then
               return;
            end if;

         when '=' | '<' | '>' | '^' =>

            --  Create a dummy declaration in the table to report it as a
            --  parameter. Note that the current declaration for the subprogram
            --  comes before the declaration of the parameter.

            declare
               Key      : constant String :=
                            Key_From_Ref (File_Ref, Line, Column);
               New_Decl : Declaration_Reference;

            begin
               New_Decl := new Declaration_Record'
                 (Symbol_Length => 0,
                  Symbol        => "",
                  Key           => new String'(Key),
                  Decl          => null,
                  Is_Parameter  => True,
                  Decl_Type     => ' ',
                  Body_Ref      => null,
                  Ref_Ref       => null,
                  Modif_Ref     => null,
                  Match         => False,
                  Par_Symbol    => null,
                  Next          => null);
               Entities_HTable.Set (New_Decl);
               Entities_Count := Entities_Count + 1;
            end;

         when 'e' | 'z' | 't' | 'p' | 'P' | 'k' | 'd' =>
            return;

         when others    =>
            Ada.Text_IO.Put_Line ("Unknown reference type: " & Ref_Type);
            return;
      end case;

      New_Ref := new Reference_Record'
        (File        => File_Ref,
         Line        => Line,
         Column      => Column,
         Source_Line => null,
         Next        => null);

      --  We can insert the reference in the list directly, since all
      --  the references will appear only once in the ALI file
      --  corresponding to the file where they are referenced.
      --  This saves a lot of time compared to checking the list to check
      --  if it exists.

      case Ref_Type is
         when 'b' | 'c' =>
            New_Ref.Next          := Declaration.Body_Ref;
            Declaration.Body_Ref  := New_Ref;

         when 'r' | 'R' | 'i' | 'l' | ' ' | 'x' | 'w' =>
            New_Ref.Next          := Declaration.Ref_Ref;
            Declaration.Ref_Ref   := New_Ref;

         when 'm' =>
            New_Ref.Next          := Declaration.Modif_Ref;
            Declaration.Modif_Ref := New_Ref;

         when others =>
            null;
      end case;

      if not Declaration.Match then
         Declaration.Match := Match (File_Ref, Line, Column);
      end if;

      if Declaration.Match then
         Longest_File_Name_In_Table :=
           Natural'Max (File_Ref.File'Length, Longest_File_Name_In_Table);
      end if;
   end Add_Reference;

   -------------------
   -- ALI_File_Name --
   -------------------

   function ALI_File_Name (Ada_File_Name : String) return String is

      --  ??? Should ideally be based on the naming scheme defined in
      --  project files.

      Index : constant Natural :=
                Ada.Strings.Fixed.Index
                  (Ada_File_Name, ".", Going => Ada.Strings.Backward);

   begin
      if Index /= 0 then
         return Ada_File_Name (Ada_File_Name'First .. Index) & "ali";
      else
         return Ada_File_Name & ".ali";
      end if;
   end ALI_File_Name;

   ------------------
   -- Is_Less_Than --
   ------------------

   function Is_Less_Than (Ref1, Ref2 : Reference) return Boolean is
   begin
      if Ref1 = null then
         return False;
      elsif Ref2 = null then
         return True;
      end if;

      if Ref1.File.File.all < Ref2.File.File.all then
         return True;

      elsif Ref1.File.File.all = Ref2.File.File.all then
         return (Ref1.Line < Ref2.Line
                 or else (Ref1.Line = Ref2.Line
                          and then Ref1.Column < Ref2.Column));
      end if;

      return False;
   end Is_Less_Than;

   ------------------
   -- Is_Less_Than --
   ------------------

   function Is_Less_Than (Decl1, Decl2 : Declaration_Reference) return Boolean
   is
      --  We cannot store the data case-insensitive in the table,
      --  since we wouldn't be able to find the right casing for the
      --  display later on.

      S1 : constant String := To_Lower (Decl1.Symbol);
      S2 : constant String := To_Lower (Decl2.Symbol);

   begin
      if S1 < S2 then
         return True;
      elsif S1 > S2 then
         return False;
      end if;

      return Decl1.Key.all < Decl2.Key.all;
   end Is_Less_Than;

   -------------------------
   -- Create_Project_File --
   -------------------------

   procedure Create_Project_File (Name : String) is
      Obj_Dir     : Unbounded_String := Null_Unbounded_String;
      Src_Dir     : Unbounded_String := Null_Unbounded_String;
      Build_Dir   : GNAT.OS_Lib.String_Access := new String'("");

      F           : File_Descriptor;
      Len         : Positive;
      File_Name   : aliased String := Name & ASCII.NUL;

   begin
      --  Read the size of the file

      F := Open_Read (File_Name'Address, Text);

      --  Project file not found

      if F /= Invalid_FD then
         Len := Positive (File_Length (F));

         declare
            Buffer : String (1 .. Len);
            Index  : Positive := Buffer'First;
            Last   : Positive;

         begin
            Len := Read (F, Buffer'Address, Len);
            Close (F);

            --  First, look for Build_Dir, since all the source and object
            --  path are relative to it.

            while Index <= Buffer'Last loop

               --  Find the end of line

               Last := Index;
               while Last <= Buffer'Last
                 and then Buffer (Last) /= ASCII.LF
                 and then Buffer (Last) /= ASCII.CR
               loop
                  Last := Last + 1;
               end loop;

               if Index <= Buffer'Last - 9
                 and then Buffer (Index .. Index + 9) = "build_dir="
               then
                  Index := Index + 10;
                  while Index <= Last
                    and then (Buffer (Index) = ' '
                              or else Buffer (Index) = ASCII.HT)
                  loop
                     Index := Index + 1;
                  end loop;

                  Free (Build_Dir);
                  Build_Dir := new String'(Buffer (Index .. Last - 1));
               end if;

               Index := Last + 1;

               --  In case we had a ASCII.CR/ASCII.LF end of line, skip the
               --  remaining symbol

               if Index <= Buffer'Last
                 and then Buffer (Index) = ASCII.LF
               then
                  Index := Index + 1;
               end if;
            end loop;

            --  Now parse the source and object paths

            Index := Buffer'First;
            while Index <= Buffer'Last loop

               --  Find the end of line

               Last := Index;
               while Last <= Buffer'Last
                 and then Buffer (Last) /= ASCII.LF
                 and then Buffer (Last) /= ASCII.CR
               loop
                  Last := Last + 1;
               end loop;

               if Index <= Buffer'Last - 7
                 and then Buffer (Index .. Index + 7) = "src_dir="
               then
                  Append (Src_Dir, Normalize_Pathname
                          (Name      => Ada.Strings.Fixed.Trim
                           (Buffer (Index + 8 .. Last - 1), Ada.Strings.Both),
                           Directory => Build_Dir.all) & Path_Separator);

               elsif Index <= Buffer'Last - 7
                 and then Buffer (Index .. Index + 7) = "obj_dir="
               then
                  Append (Obj_Dir, Normalize_Pathname
                          (Name      => Ada.Strings.Fixed.Trim
                           (Buffer (Index + 8 .. Last - 1), Ada.Strings.Both),
                           Directory => Build_Dir.all) & Path_Separator);
               end if;

               --  In case we had a ASCII.CR/ASCII.LF end of line, skip the
               --  remaining symbol
               Index := Last + 1;

               if Index <= Buffer'Last
                 and then Buffer (Index) = ASCII.LF
               then
                  Index := Index + 1;
               end if;
            end loop;
         end;
      end if;

      Osint.Add_Default_Search_Dirs;

      declare
         Src : constant String := Parse_Gnatls_Src;
         Obj : constant String := Parse_Gnatls_Obj;

      begin
         Directories := new Project_File'
           (Src_Dir_Length     => Length (Src_Dir) + Src'Length,
            Obj_Dir_Length     => Length (Obj_Dir) + Obj'Length,
            Src_Dir            => To_String (Src_Dir) & Src,
            Obj_Dir            => To_String (Obj_Dir) & Obj,
            Src_Dir_Index      => 1,
            Obj_Dir_Index      => 1,
            Last_Obj_Dir_Start => 0);
      end;

      Free (Build_Dir);
   end Create_Project_File;

   ---------------------
   -- Current_Obj_Dir --
   ---------------------

   function Current_Obj_Dir return String is
   begin
      return Directories.Obj_Dir
        (Directories.Last_Obj_Dir_Start .. Directories.Obj_Dir_Index - 2);
   end Current_Obj_Dir;

   ----------------
   -- Get_Column --
   ----------------

   function Get_Column (Decl : Declaration_Reference) return String is
   begin
      return Ada.Strings.Fixed.Trim (Natural'Image (Decl.Decl.Column),
                                     Ada.Strings.Left);
   end Get_Column;

   function Get_Column (Ref : Reference) return String is
   begin
      return Ada.Strings.Fixed.Trim (Natural'Image (Ref.Column),
                                     Ada.Strings.Left);
   end Get_Column;

   ---------------------
   -- Get_Declaration --
   ---------------------

   function Get_Declaration
     (File_Ref : File_Reference;
      Line     : Natural;
      Column   : Natural)
      return     Declaration_Reference
   is
      Key : aliased constant String := Key_From_Ref (File_Ref, Line, Column);

   begin
      return Entities_HTable.Get (Key'Unchecked_Access);
   end Get_Declaration;

   ----------------------
   -- Get_Emit_Warning --
   ----------------------

   function Get_Emit_Warning (File : File_Reference) return Boolean is
   begin
      return File.Emit_Warning;
   end Get_Emit_Warning;

   --------------
   -- Get_File --
   --------------

   function Get_File
     (Decl     : Declaration_Reference;
      With_Dir : Boolean := False) return String
   is
   begin
      return Get_File (Decl.Decl.File, With_Dir);
   end Get_File;

   function Get_File
     (Ref      : Reference;
      With_Dir : Boolean := False) return String
   is
   begin
      return Get_File (Ref.File, With_Dir);
   end Get_File;

   function Get_File
     (File     : File_Reference;
      With_Dir : Boolean := False;
      Strip    : Natural    := 0) return String
   is
      Tmp : GNAT.OS_Lib.String_Access;

      function Internal_Strip (Full_Name : String) return String;
      --  Internal function to process the Strip parameter

      --------------------
      -- Internal_Strip --
      --------------------

      function Internal_Strip (Full_Name : String) return String is
         Unit_End        : Natural;
         Extension_Start : Natural;
         S               : Natural;

      begin
         if Strip = 0 then
            return Full_Name;
         end if;

         --  Isolate the file extension

         Extension_Start := Full_Name'Last;
         while Extension_Start >= Full_Name'First
           and then Full_Name (Extension_Start) /= '.'
         loop
            Extension_Start := Extension_Start - 1;
         end loop;

         --  Strip the right number of subunit_names

         S := Strip;
         Unit_End := Extension_Start - 1;
         while Unit_End >= Full_Name'First
           and then S > 0
         loop
            if Full_Name (Unit_End) = '-' then
               S := S - 1;
            end if;

            Unit_End := Unit_End - 1;
         end loop;

         if Unit_End < Full_Name'First then
            return "";
         else
            return Full_Name (Full_Name'First .. Unit_End)
              & Full_Name (Extension_Start .. Full_Name'Last);
         end if;
      end Internal_Strip;

   --  Start of processing for Get_File;

   begin
      --  If we do not want the full path name

      if not With_Dir then
         return Internal_Strip (File.File.all);
      end if;

      if File.Dir = null then
         if Ada.Strings.Fixed.Tail (File.File.all, 3) = "ali" then
            Tmp := Locate_Regular_File
              (Internal_Strip (File.File.all), Directories.Obj_Dir);
         else
            Tmp := Locate_Regular_File
              (File.File.all, Directories.Src_Dir);
         end if;

         if Tmp = null then
            File.Dir := new String'("");
         else
            File.Dir := new String'(Dir_Name (Tmp.all));
            Free (Tmp);
         end if;
      end if;

      return Internal_Strip (File.Dir.all & File.File.all);
   end Get_File;

   ------------------
   -- Get_File_Ref --
   ------------------

   function Get_File_Ref (Ref : Reference) return File_Reference is
   begin
      return Ref.File;
   end Get_File_Ref;

   -----------------------
   -- Get_Gnatchop_File --
   -----------------------

   function Get_Gnatchop_File
     (File     : File_Reference;
      With_Dir : Boolean := False)
      return     String
   is
   begin
      if File.Gnatchop_File.all = "" then
         return Get_File (File, With_Dir);
      else
         return File.Gnatchop_File.all;
      end if;
   end Get_Gnatchop_File;

   function Get_Gnatchop_File
     (Ref      : Reference;
      With_Dir : Boolean := False)
      return     String
   is
   begin
      return Get_Gnatchop_File (Ref.File, With_Dir);
   end Get_Gnatchop_File;

   function Get_Gnatchop_File
     (Decl     : Declaration_Reference;
      With_Dir : Boolean := False)
      return     String
   is
   begin
      return Get_Gnatchop_File (Decl.Decl.File, With_Dir);
   end Get_Gnatchop_File;

   --------------
   -- Get_Line --
   --------------

   function Get_Line (Decl : Declaration_Reference) return String is
   begin
      return Ada.Strings.Fixed.Trim (Natural'Image (Decl.Decl.Line),
                                     Ada.Strings.Left);
   end Get_Line;

   function Get_Line (Ref : Reference) return String is
   begin
      return Ada.Strings.Fixed.Trim (Natural'Image (Ref.Line),
                                     Ada.Strings.Left);
   end Get_Line;

   ----------------
   -- Get_Parent --
   ----------------

   function Get_Parent
     (Decl : Declaration_Reference)
      return Declaration_Reference
   is
   begin
      return Decl.Par_Symbol;
   end Get_Parent;

   ---------------------
   -- Get_Source_Line --
   ---------------------

   function Get_Source_Line (Ref : Reference) return String is
   begin
      if Ref.Source_Line /= null then
         return Ref.Source_Line.all;
      else
         return "";
      end if;
   end Get_Source_Line;

   function Get_Source_Line (Decl : Declaration_Reference) return String is
   begin
      if Decl.Decl.Source_Line /= null then
         return Decl.Decl.Source_Line.all;
      else
         return "";
      end if;
   end Get_Source_Line;

   ----------------
   -- Get_Symbol --
   ----------------

   function Get_Symbol (Decl : Declaration_Reference) return String is
   begin
      return Decl.Symbol;
   end Get_Symbol;

   --------------
   -- Get_Type --
   --------------

   function Get_Type (Decl : Declaration_Reference) return Character is
   begin
      return Decl.Decl_Type;
   end Get_Type;

   ----------
   -- Sort --
   ----------

   procedure Sort (Arr : in out Reference_Array) is
      Tmp : Reference;

      function Lt (Op1, Op2 : Natural) return Boolean;
      procedure Move (From, To : Natural);
      --  See GNAT.Heap_Sort_G

      --------
      -- Lt --
      --------

      function Lt (Op1, Op2 : Natural) return Boolean is
      begin
         if Op1 = 0 then
            return Is_Less_Than (Tmp, Arr (Op2));
         elsif Op2 = 0 then
            return Is_Less_Than (Arr (Op1), Tmp);
         else
            return Is_Less_Than (Arr (Op1), Arr (Op2));
         end if;
      end Lt;

      ----------
      -- Move --
      ----------

      procedure Move (From, To : Natural) is
      begin
         if To = 0 then
            Tmp := Arr (From);
         elsif From = 0 then
            Arr (To) := Tmp;
         else
            Arr (To) := Arr (From);
         end if;
      end Move;

      package Ref_Sort is new GNAT.Heap_Sort_G (Move, Lt);

   --  Start of processing for Sort

   begin
      Ref_Sort.Sort (Arr'Last);
   end Sort;

   -----------------------
   -- Grep_Source_Files --
   -----------------------

   procedure Grep_Source_Files is
      Length       : Natural := 0;
      Decl         : Declaration_Reference := Entities_HTable.Get_First;
      Arr          : Reference_Array_Access;
      Index        : Natural;
      End_Index    : Natural;
      Current_File : File_Reference;
      Current_Line : Cst_String_Access;
      Buffer       : GNAT.OS_Lib.String_Access;
      Ref          : Reference;
      Line         : Natural;

   begin
      --  Create a temporary array, where all references will be
      --  sorted by files. This way, we only have to read the source
      --  files once.

      while Decl /= null loop

         --  Add 1 for the declaration itself

         Length := Length + References_Count (Decl, True, True, True) + 1;
         Decl := Entities_HTable.Get_Next;
      end loop;

      Arr := new Reference_Array (1 .. Length);
      Index := Arr'First;

      Decl := Entities_HTable.Get_First;
      while Decl /= null loop
         Store_References (Decl, True, True, True, True, Arr.all, Index);
         Decl := Entities_HTable.Get_Next;
      end loop;

      Sort (Arr.all);

      --  Now traverse the whole array and find the appropriate source
      --  lines.

      for R in Arr'Range loop
         Ref := Arr (R);

         if Ref.File /= Current_File then
            Free (Buffer);
            begin
               Read_File (Get_File (Ref.File, With_Dir => True), Buffer);
               End_Index := Buffer'First - 1;
               Line := 0;
            exception
               when Ada.Text_IO.Name_Error | Ada.Text_IO.End_Error =>
                  Line := Natural'Last;
            end;
            Current_File := Ref.File;
         end if;

         if Ref.Line > Line then

            --  Do not free Current_Line, it is referenced by the last
            --  Ref we processed.

            loop
               Index := End_Index + 1;

               loop
                  End_Index := End_Index + 1;
                  exit when End_Index > Buffer'Last
                    or else Buffer (End_Index) = ASCII.LF;
               end loop;

               --  Skip spaces at beginning of line

               while Index < End_Index and then
                 (Buffer (Index) = ' ' or else Buffer (Index) = ASCII.HT)
               loop
                  Index := Index + 1;
               end loop;

               Line := Line + 1;
               exit when Ref.Line = Line;
            end loop;

            Current_Line := new String'(Buffer (Index .. End_Index - 1));
         end if;

         Ref.Source_Line := Current_Line;
      end loop;

      Free (Buffer);
      Free (Arr);
   end Grep_Source_Files;

   ---------------
   -- Read_File --
   ---------------

   procedure Read_File
     (File_Name : String;
      Contents  : out GNAT.OS_Lib.String_Access)
   is
      Name_0 : constant String := File_Name & ASCII.NUL;
      FD     : constant File_Descriptor := Open_Read (Name_0'Address, Binary);
      Length : Natural;

   begin
      if FD = Invalid_FD then
         raise Ada.Text_IO.Name_Error;
      end if;

      --  Include room for EOF char

      Length := Natural (File_Length (FD));

      declare
         Buffer    : String (1 .. Length + 1);
         This_Read : Integer;
         Read_Ptr  : Natural := 1;

      begin
         loop
            This_Read := Read (FD,
                               A => Buffer (Read_Ptr)'Address,
                               N => Length + 1 - Read_Ptr);
            Read_Ptr := Read_Ptr + Integer'Max (This_Read, 0);
            exit when This_Read <= 0;
         end loop;

         Buffer (Read_Ptr) := EOF;
         Contents := new String'(Buffer (1 .. Read_Ptr));

         --  Things are not simple on VMS due to the plethora of file types
         --  and organizations. It seems clear that there shouldn't be more
         --  bytes read than are contained in the file though.

         if (Hostparm.OpenVMS and then Read_Ptr > Length + 1)
           or else (not Hostparm.OpenVMS and then Read_Ptr /= Length + 1)
         then
            raise Ada.Text_IO.End_Error;
         end if;

         Close (FD);
      end;
   end Read_File;

   -----------------------
   -- Longest_File_Name --
   -----------------------

   function Longest_File_Name return Natural is
   begin
      return Longest_File_Name_In_Table;
   end Longest_File_Name;

   -----------
   -- Match --
   -----------

   function Match
     (File   : File_Reference;
      Line   : Natural;
      Column : Natural)
      return   Boolean
   is
      Ref : Ref_In_File_Ptr := File.Lines;

   begin
      while Ref /= null loop
         if (Ref.Line = 0 or else Ref.Line = Line)
           and then (Ref.Column = 0 or else Ref.Column = Column)
         then
            return True;
         end if;

         Ref := Ref.Next;
      end loop;

      return False;
   end Match;

   -----------
   -- Match --
   -----------

   function Match (Decl : Declaration_Reference) return Boolean is
   begin
      return Decl.Match;
   end Match;

   ----------
   -- Next --
   ----------

   function Next (E : File_Reference) return File_Reference is
   begin
      return E.Next;
   end Next;

   function Next (E : Declaration_Reference) return Declaration_Reference is
   begin
      return E.Next;
   end Next;

   ------------------
   -- Next_Obj_Dir --
   ------------------

   function Next_Obj_Dir return String is
      First : constant Integer := Directories.Obj_Dir_Index;
      Last  : Integer;

   begin
      Last := Directories.Obj_Dir_Index;

      if Last > Directories.Obj_Dir_Length then
         return String'(1 .. 0 => ' ');
      end if;

      while Directories.Obj_Dir (Last) /= Path_Separator loop
         Last := Last + 1;
      end loop;

      Directories.Obj_Dir_Index := Last + 1;
      Directories.Last_Obj_Dir_Start := First;
      return Directories.Obj_Dir (First .. Last - 1);
   end Next_Obj_Dir;

   -------------------------
   -- Next_Unvisited_File --
   -------------------------

   function Next_Unvisited_File return File_Reference is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Unvisited_Files_Record, Unvisited_Files_Access);

      Ref : File_Reference;
      Tmp : Unvisited_Files_Access;

   begin
      if Unvisited_Files = null then
         return Empty_File;
      else
         Tmp := Unvisited_Files;
         Ref := Unvisited_Files.File;
         Unvisited_Files := Unvisited_Files.Next;
         Unchecked_Free (Tmp);
         return Ref;
      end if;
   end Next_Unvisited_File;

   ----------------------
   -- Parse_Gnatls_Src --
   ----------------------

   function Parse_Gnatls_Src return String is
      Length : Natural;

   begin
      Length := 0;
      for J in 1 .. Osint.Nb_Dir_In_Src_Search_Path loop
         if Osint.Dir_In_Src_Search_Path (J)'Length = 0 then
            Length := Length + 2;
         else
            Length := Length + Osint.Dir_In_Src_Search_Path (J)'Length + 1;
         end if;
      end loop;

      declare
         Result : String (1 .. Length);
         L      : Natural;

      begin
         L := Result'First;
         for J in 1 .. Osint.Nb_Dir_In_Src_Search_Path loop
            if Osint.Dir_In_Src_Search_Path (J)'Length = 0 then
               Result (L .. L + 1) := "." & Path_Separator;
               L := L + 2;

            else
               Result (L .. L + Osint.Dir_In_Src_Search_Path (J)'Length - 1) :=
                 Osint.Dir_In_Src_Search_Path (J).all;
               L := L + Osint.Dir_In_Src_Search_Path (J)'Length;
               Result (L) := Path_Separator;
               L := L + 1;
            end if;
         end loop;

         return Result;
      end;
   end Parse_Gnatls_Src;

   ----------------------
   -- Parse_Gnatls_Obj --
   ----------------------

   function Parse_Gnatls_Obj return String is
      Length : Natural;

   begin
      Length := 0;
      for J in 1 .. Osint.Nb_Dir_In_Obj_Search_Path loop
         if Osint.Dir_In_Obj_Search_Path (J)'Length = 0 then
            Length := Length + 2;
         else
            Length := Length + Osint.Dir_In_Obj_Search_Path (J)'Length + 1;
         end if;
      end loop;

      declare
         Result : String (1 .. Length);
         L      : Natural;

      begin
         L := Result'First;
         for J in 1 .. Osint.Nb_Dir_In_Obj_Search_Path loop
            if Osint.Dir_In_Obj_Search_Path (J)'Length = 0 then
               Result (L .. L + 1) := "." & Path_Separator;
               L := L + 2;
            else
               Result (L .. L + Osint.Dir_In_Obj_Search_Path (J)'Length - 1) :=
                 Osint.Dir_In_Obj_Search_Path (J).all;
               L := L + Osint.Dir_In_Obj_Search_Path (J)'Length;
               Result (L) := Path_Separator;
               L := L + 1;
            end if;
         end loop;

         return Result;
      end;
   end Parse_Gnatls_Obj;

   -------------------
   -- Reset_Obj_Dir --
   -------------------

   procedure Reset_Obj_Dir is
   begin
      Directories.Obj_Dir_Index := 1;
   end Reset_Obj_Dir;

   -----------------------
   -- Set_Default_Match --
   -----------------------

   procedure Set_Default_Match (Value : Boolean) is
   begin
      Default_Match := Value;
   end Set_Default_Match;

   ----------
   -- Free --
   ----------

   procedure Free (Str : in out Cst_String_Access) is
      function Convert is new Ada.Unchecked_Conversion
        (Cst_String_Access, GNAT.OS_Lib.String_Access);

      S : GNAT.OS_Lib.String_Access := Convert (Str);

   begin
      Free (S);
      Str := null;
   end Free;

   ---------------------
   -- Reset_Directory --
   ---------------------

   procedure Reset_Directory (File : File_Reference) is
   begin
      Free (File.Dir);
   end Reset_Directory;

   -------------------
   -- Set_Unvisited --
   -------------------

   procedure Set_Unvisited (File_Ref : File_Reference) is
      F : constant String := Get_File (File_Ref, With_Dir => False);

   begin
      File_Ref.Visited := False;

      --  ??? Do not add a source file to the list. This is true at
      --  least for gnatxref, and probably for gnatfind as well

      if F'Length > 4
        and then F (F'Last - 3 .. F'Last) = ".ali"
      then
         Unvisited_Files := new Unvisited_Files_Record'
           (File => File_Ref,
            Next => Unvisited_Files);
      end if;
   end Set_Unvisited;

   ----------------------
   -- Get_Declarations --
   ----------------------

   function Get_Declarations
     (Sorted : Boolean := True)
      return   Declaration_Array_Access
   is
      Arr   : constant Declaration_Array_Access :=
                new Declaration_Array (1 .. Entities_Count);
      Decl  : Declaration_Reference := Entities_HTable.Get_First;
      Index : Natural               := Arr'First;
      Tmp   : Declaration_Reference;

      procedure Move (From : Natural; To : Natural);
      function Lt (Op1, Op2 : Natural) return Boolean;
      --  See GNAT.Heap_Sort_G

      --------
      -- Lt --
      --------

      function Lt (Op1, Op2 : Natural) return Boolean is
      begin
         if Op1 = 0 then
            return Is_Less_Than (Tmp, Arr (Op2));
         elsif Op2 = 0 then
            return Is_Less_Than (Arr (Op1), Tmp);
         else
            return Is_Less_Than (Arr (Op1), Arr (Op2));
         end if;
      end Lt;

      ----------
      -- Move --
      ----------

      procedure Move (From : Natural; To : Natural) is
      begin
         if To = 0 then
            Tmp := Arr (From);
         elsif From = 0 then
            Arr (To) := Tmp;
         else
            Arr (To) := Arr (From);
         end if;
      end Move;

      package Decl_Sort is new GNAT.Heap_Sort_G (Move, Lt);

   --  Start of processing for Get_Declarations

   begin
      while Decl /= null loop
         Arr (Index) := Decl;
         Index := Index + 1;
         Decl := Entities_HTable.Get_Next;
      end loop;

      if Sorted and then Arr'Length /= 0 then
         Decl_Sort.Sort (Entities_Count);
      end if;

      return Arr;
   end Get_Declarations;

   ----------------------
   -- References_Count --
   ----------------------

   function References_Count
     (Decl       : Declaration_Reference;
      Get_Reads  : Boolean := False;
      Get_Writes : Boolean := False;
      Get_Bodies : Boolean := False)
      return       Natural
   is
      function List_Length (E : Reference) return Natural;
      --  Return the number of references in E

      -----------------
      -- List_Length --
      -----------------

      function List_Length (E : Reference) return Natural is
         L  : Natural := 0;
         E1 : Reference := E;

      begin
         while E1 /= null loop
            L := L + 1;
            E1 := E1.Next;
         end loop;

         return L;
      end List_Length;

      Length : Natural := 0;

   --  Start of processing for References_Count

   begin
      if Get_Reads then
         Length := List_Length (Decl.Ref_Ref);
      end if;

      if Get_Writes then
         Length := Length + List_Length (Decl.Modif_Ref);
      end if;

      if Get_Bodies then
         Length := Length + List_Length (Decl.Body_Ref);
      end if;

      return Length;
   end References_Count;

   ----------------------
   -- Store_References --
   ----------------------

   procedure Store_References
     (Decl            : Declaration_Reference;
      Get_Writes      : Boolean := False;
      Get_Reads       : Boolean := False;
      Get_Bodies      : Boolean := False;
      Get_Declaration : Boolean := False;
      Arr             : in out Reference_Array;
      Index           : in out Natural)
   is
      procedure Add (List : Reference);
      --  Add all the references in List to Arr

      ---------
      -- Add --
      ---------

      procedure Add (List : Reference) is
         E : Reference := List;
      begin
         while E /= null loop
            Arr (Index) := E;
            Index := Index + 1;
            E := E.Next;
         end loop;
      end Add;

   --  Start of processing for Store_References

   begin
      if Get_Declaration then
         Add (Decl.Decl);
      end if;

      if Get_Reads then
         Add (Decl.Ref_Ref);
      end if;

      if Get_Writes then
         Add (Decl.Modif_Ref);
      end if;

      if Get_Bodies then
         Add (Decl.Body_Ref);
      end if;
   end Store_References;

   --------------------
   -- Get_References --
   --------------------

   function Get_References
     (Decl : Declaration_Reference;
      Get_Reads  : Boolean := False;
      Get_Writes : Boolean := False;
      Get_Bodies : Boolean := False)
      return       Reference_Array_Access
   is
      Length : constant Natural :=
                 References_Count (Decl, Get_Reads, Get_Writes, Get_Bodies);

      Arr : constant Reference_Array_Access :=
              new Reference_Array (1 .. Length);

      Index : Natural := Arr'First;

   begin
      Store_References
        (Decl            => Decl,
         Get_Writes      => Get_Writes,
         Get_Reads       => Get_Reads,
         Get_Bodies      => Get_Bodies,
         Get_Declaration => False,
         Arr             => Arr.all,
         Index           => Index);

      if Arr'Length /= 0 then
         Sort (Arr.all);
      end if;

      return Arr;
   end Get_References;

   ----------
   -- Free --
   ----------

   procedure Free (Arr : in out Reference_Array_Access) is
      procedure Internal is new Ada.Unchecked_Deallocation
        (Reference_Array, Reference_Array_Access);
   begin
      Internal (Arr);
   end Free;

   ------------------
   -- Is_Parameter --
   ------------------

   function Is_Parameter (Decl : Declaration_Reference) return Boolean is
   begin
      return Decl.Is_Parameter;
   end Is_Parameter;

end Xr_Tabls;
