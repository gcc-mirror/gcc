------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             X R  _ T A B L S                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--          Copyright (C) 1998-2002 Free Software Foundation, Inc.          --
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
-- Extensive contributions were provided by Ada Core Technologies Inc.   --
--                                                                          --
------------------------------------------------------------------------------

with Osint;
with Unchecked_Deallocation;

with Ada.IO_Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings;
with Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNAT.IO_Aux;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;

package body Xr_Tabls is

   function Base_File_Name (File : String) return String;
   --  Return the base file name for File (ie not including the directory)

   function Dir_Name (File : String; Base : String := "") return String;
   --  Return the directory name of File, or "" if there is no directory part
   --  in File.
   --  This includes the last separator at the end, and always return an
   --  absolute path name (directories are relative to Base, or the current
   --  directory if Base is "")

   Dir_Sep       : Character renames GNAT.OS_Lib.Directory_Separator;

   Files         : File_Table;
   Entities      : Entity_Table;
   Directories   : Project_File_Ptr;
   Default_Match : Boolean := False;

   ---------------------
   -- Add_Declaration --
   ---------------------

   function Add_Declaration
     (File_Ref  : File_Reference;
      Symbol    : String;
      Line      : Natural;
      Column    : Natural;
      Decl_Type : Character)
      return      Declaration_Reference
   is
      The_Entities : Declaration_Reference := Entities.Table;
      New_Decl     : Declaration_Reference;
      Result       : Compare_Result;
      Prev         : Declaration_Reference := null;

   begin
      --  Check if the identifier already exists in the table

      while The_Entities /= null loop
         Result := Compare (The_Entities, File_Ref, Line, Column, Symbol);
         exit when Result = GreaterThan;

         if Result = Equal then
            return The_Entities;
         end if;

         Prev := The_Entities;
         The_Entities  := The_Entities.Next;
      end loop;

      --  Insert the Declaration in the table

      New_Decl :=
        new Declaration_Record'
          (Symbol_Length => Symbol'Length,
           Symbol        => Symbol,
           Decl          => (File          => File_Ref,
                             Line          => Line,
                             Column        => Column,
                             Source_Line   => Null_Unbounded_String,
                             Next          => null),
           Decl_Type     => Decl_Type,
           Body_Ref      => null,
           Ref_Ref       => null,
           Modif_Ref     => null,
           Match         => Default_Match
                              or else Match (File_Ref, Line, Column),
           Par_Symbol    => null,
           Next          => null);

      if Prev = null then
         New_Decl.Next  := Entities.Table;
         Entities.Table := New_Decl;
      else
         New_Decl.Next  := Prev.Next;
         Prev.Next      := New_Decl;
      end if;

      if New_Decl.Match then
         Files.Longest_Name := Natural'Max (File_Ref.File'Length,
                                            Files.Longest_Name);
      end if;

      return New_Decl;
   end Add_Declaration;

   ----------------------
   -- Add_To_Xref_File --
   ----------------------

   procedure Add_To_Xref_File
     (File_Name       : String;
      File_Existed    : out Boolean;
      Ref             : out File_Reference;
      Visited         : Boolean := True;
      Emit_Warning    : Boolean := False;
      Gnatchop_File   : String  := "";
      Gnatchop_Offset : Integer := 0)
   is
      The_Files : File_Reference  := Files.Table;
      Base      : constant String := Base_File_Name (File_Name);
      Dir       : constant String := Xr_Tabls.Dir_Name (File_Name);
      Dir_Acc   : String_Access   := null;

   begin
      --  Do we have a directory name as well?

      if Dir /= "" then
         Dir_Acc := new String' (Dir);
      end if;

      --  Check if the file already exists in the table

      while The_Files /= null loop

         if The_Files.File = File_Name then
            File_Existed      := True;
            Ref               := The_Files;
            return;
         end if;

         The_Files := The_Files.Next;
      end loop;

      Ref := new File_Record'
        (File_Length     => Base'Length,
         File            => Base,
         Dir             => Dir_Acc,
         Lines           => null,
         Visited         => Visited,
         Emit_Warning    => Emit_Warning,
         Gnatchop_File   => new String' (Gnatchop_File),
         Gnatchop_Offset => Gnatchop_Offset,
         Next            => Files.Table);
      Files.Table := Ref;
      File_Existed := False;
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
      Declaration.Par_Symbol := new Declaration_Record'
        (Symbol_Length => Symbol'Length,
         Symbol        => Symbol,
         Decl          => (File         => File_Ref,
                           Line         => Line,
                           Column       => Column,
                           Source_Line  => Null_Unbounded_String,
                           Next         => null),
         Decl_Type     => ' ',
         Body_Ref      => null,
         Ref_Ref       => null,
         Modif_Ref     => null,
         Match         => False,
         Par_Symbol    => null,
         Next          => null);
   end Add_Parent;

   -------------------
   -- Add_Reference --
   -------------------

   procedure Add_Reference
     (Declaration : Declaration_Reference;
      File_Ref    : File_Reference;
      Line        : Natural;
      Column      : Natural;
      Ref_Type    : Character)
   is
      procedure Free is new Unchecked_Deallocation
        (Reference_Record, Reference);

      Ref     : Reference;
      Prev    : Reference := null;
      Result  : Compare_Result;
      New_Ref : Reference := new Reference_Record'
        (File   => File_Ref,
         Line   => Line,
         Column => Column,
         Source_Line => Null_Unbounded_String,
         Next   => null);

   begin
      case Ref_Type is
         when 'b' | 'c' =>
            Ref := Declaration.Body_Ref;

         when 'r' | 'i' | 'l' | ' ' | 'x' =>
            Ref := Declaration.Ref_Ref;

         when 'm'       =>
            Ref := Declaration.Modif_Ref;

         when 'e' | 't' | 'p' =>
            return;

         when others    =>
            Ada.Text_IO.Put_Line ("Unknown reference type: " & Ref_Type);
            return;
      end case;

      --  Check if the reference already exists

      while Ref /= null loop
         Result := Compare (New_Ref, Ref);
         exit when Result = LessThan;

         if Result = Equal then
            Free (New_Ref);
            return;
         end if;

         Prev := Ref;
         Ref  := Ref.Next;
      end loop;

      --  Insert it in the list

      if Prev /= null then
         New_Ref.Next := Prev.Next;
         Prev.Next := New_Ref;

      else
         case Ref_Type is
            when 'b' | 'c' =>
               New_Ref.Next          := Declaration.Body_Ref;
               Declaration.Body_Ref  := New_Ref;

            when 'r' | 'i' | 'l' | ' ' | 'x' =>
               New_Ref.Next          := Declaration.Ref_Ref;
               Declaration.Ref_Ref   := New_Ref;

            when 'm' =>
               New_Ref.Next          := Declaration.Modif_Ref;
               Declaration.Modif_Ref := New_Ref;

            when others =>
               null;
         end case;
      end if;

      if not Declaration.Match then
         Declaration.Match := Match (File_Ref, Line, Column);
      end if;

      if Declaration.Match then
         Files.Longest_Name := Natural'Max (File_Ref.File'Length,
                                            Files.Longest_Name);
      end if;
   end Add_Reference;

   -------------------
   -- ALI_File_Name --
   -------------------

   function ALI_File_Name (Ada_File_Name : String) return String is
      Index : Natural := Ada.Strings.Fixed.Index
                          (Ada_File_Name, ".", Going => Ada.Strings.Backward);

   begin
      if Index /= 0 then
         return Ada_File_Name (Ada_File_Name'First .. Index)
           & "ali";
      else
         return Ada_File_Name & ".ali";
      end if;
   end ALI_File_Name;

   --------------------
   -- Base_File_Name --
   --------------------

   function Base_File_Name (File : String) return String is
   begin
      for J in reverse File'Range loop
         if File (J) = '/' or else File (J) = Dir_Sep then
            return File (J + 1 .. File'Last);
         end if;
      end loop;

      return File;
   end Base_File_Name;

   -------------
   -- Compare --
   -------------

   function Compare
     (Ref1 : Reference;
      Ref2 : Reference)
      return Compare_Result
   is
   begin
      if Ref1 = null then
         return GreaterThan;
      elsif Ref2 = null then
         return LessThan;
      end if;

      if Ref1.File.File < Ref2.File.File then
         return LessThan;

      elsif Ref1.File.File = Ref2.File.File then
         if Ref1.Line < Ref2.Line then
            return LessThan;

         elsif Ref1.Line = Ref2.Line then
            if Ref1.Column < Ref2.Column then
               return LessThan;
            elsif Ref1.Column = Ref2.Column then
               return Equal;
            else
               return GreaterThan;
            end if;

         else
            return GreaterThan;
         end if;

      else
         return GreaterThan;
      end if;
   end Compare;

   -------------
   -- Compare --
   -------------

   function Compare
     (Decl1 : Declaration_Reference;
      File2 : File_Reference;
      Line2 : Integer;
      Col2  : Integer;
      Symb2 : String)
      return  Compare_Result
   is
   begin
      if Decl1 = null then
         return GreaterThan;
      end if;

      if Decl1.Symbol < Symb2 then
         return LessThan;
      elsif Decl1.Symbol > Symb2 then
         return GreaterThan;
      end if;

      if Decl1.Decl.File.File < Get_File (File2) then
         return LessThan;

      elsif Decl1.Decl.File.File = Get_File (File2) then
         if Decl1.Decl.Line < Line2 then
            return LessThan;

         elsif Decl1.Decl.Line = Line2 then
            if Decl1.Decl.Column < Col2 then
               return LessThan;

            elsif Decl1.Decl.Column = Col2 then
               return Equal;

            else
               return GreaterThan;
            end if;

         else
            return GreaterThan;
         end if;

      else
         return GreaterThan;
      end if;
   end Compare;

   -------------------------
   -- Create_Project_File --
   -------------------------

   procedure Create_Project_File
     (Name           : String)
   is
      use Ada.Strings.Unbounded;

      Obj_Dir     : Unbounded_String := Null_Unbounded_String;
      Src_Dir     : Unbounded_String := Null_Unbounded_String;
      Build_Dir   : Unbounded_String;

      Gnatls_Src_Cache : Unbounded_String;
      Gnatls_Obj_Cache : Unbounded_String;

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

               --  find the end of line

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

                  Build_Dir :=
                    To_Unbounded_String (Buffer (Index .. Last - 1));
                  if Buffer (Last - 1) /= Dir_Sep then
                     Append (Build_Dir, Dir_Sep);
                  end if;
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

               --  find the end of line

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
                  declare
                     S : String := Ada.Strings.Fixed.Trim
                       (Buffer (Index + 8 .. Last - 1), Ada.Strings.Both);
                  begin
                     --  A relative directory ?
                     if S (S'First) /= Dir_Sep then
                        Append (Src_Dir, Build_Dir);
                     end if;

                     if S (S'Last) = Dir_Sep then
                        Append (Src_Dir, S & " ");
                     else
                        Append (Src_Dir, S & Dir_Sep & " ");
                     end if;
                  end;

               elsif Index <= Buffer'Last - 7
                 and then Buffer (Index .. Index + 7) = "obj_dir="
               then
                  declare
                     S : String := Ada.Strings.Fixed.Trim
                       (Buffer (Index + 8 .. Last - 1), Ada.Strings.Both);
                  begin
                     --  A relative directory ?
                     if S (S'First) /= Dir_Sep then
                        Append (Obj_Dir, Build_Dir);
                     end if;

                     if S (S'Last) = Dir_Sep then
                        Append (Obj_Dir, S & " ");
                     else
                        Append (Obj_Dir, S & Dir_Sep & " ");
                     end if;
                  end;
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

      Parse_Gnatls (Gnatls_Src_Cache, Gnatls_Obj_Cache);

      Directories := new Project_File'
        (Src_Dir_Length     => Length (Src_Dir) + Length (Gnatls_Src_Cache),
         Obj_Dir_Length     => Length (Obj_Dir) + Length (Gnatls_Obj_Cache),
         Src_Dir            => To_String (Src_Dir & Gnatls_Src_Cache),
         Obj_Dir            => To_String (Obj_Dir & Gnatls_Obj_Cache),
         Src_Dir_Index      => 1,
         Obj_Dir_Index      => 1,
         Last_Obj_Dir_Start => 0);
   end Create_Project_File;

   ---------------------
   -- Current_Obj_Dir --
   ---------------------

   function Current_Obj_Dir return String is
   begin
      return Directories.Obj_Dir (Directories.Last_Obj_Dir_Start
                                  .. Directories.Obj_Dir_Index - 2);
   end Current_Obj_Dir;

   --------------
   -- Dir_Name --
   --------------

   function Dir_Name (File : String; Base : String := "") return String is
   begin
      for J in reverse File'Range loop
         if File (J) = '/' or else File (J) = Dir_Sep then

            --  Is this an absolute directory ?
            if File (File'First) = '/'
              or else File (File'First) = Dir_Sep
            then
               return File (File'First .. J);

            --  Else do we know the base directory ?
            elsif Base /= "" then
               return Base & File (File'First .. J);

            else
               declare
                  Max_Path : Integer;
                  pragma Import (C, Max_Path, "__gnat_max_path_len");

                  Base2 : Dir_Name_Str (1 .. Max_Path);
                  Last  : Natural;
               begin
                  Get_Current_Dir (Base2, Last);
                  return Base2 (Base2'First .. Last) & File (File'First .. J);
               end;
            end if;
         end if;
      end loop;
      return "";
   end Dir_Name;

   -------------------
   -- Find_ALI_File --
   -------------------

   function Find_ALI_File (Short_Name  : String) return String is
      use type Ada.Strings.Unbounded.String_Access;
      Old_Obj_Dir : constant Integer := Directories.Obj_Dir_Index;

   begin
      Reset_Obj_Dir;

      loop
         declare
            Obj_Dir : String := Next_Obj_Dir;
         begin
            exit when Obj_Dir'Length = 0;
            if GNAT.IO_Aux.File_Exists (Obj_Dir & Short_Name) then
               Directories.Obj_Dir_Index := Old_Obj_Dir;
               return Obj_Dir;
            end if;
         end;
      end loop;

      --  Finally look in the standard directories

      Directories.Obj_Dir_Index := Old_Obj_Dir;
      return "";
   end Find_ALI_File;

   ----------------------
   -- Find_Source_File --
   ----------------------

   function Find_Source_File (Short_Name  : String) return String is
      use type Ada.Strings.Unbounded.String_Access;

   begin
      Reset_Src_Dir;
      loop
         declare
            Src_Dir : String := Next_Src_Dir;
         begin
            exit when Src_Dir'Length = 0;

            if GNAT.IO_Aux.File_Exists (Src_Dir & Short_Name) then
               return Src_Dir;
            end if;
         end;
      end loop;

      --  Finally look in the standard directories

      return "";
   end Find_Source_File;

   ----------------
   -- First_Body --
   ----------------

   function First_Body (Decl : Declaration_Reference) return Reference is
   begin
      return Decl.Body_Ref;
   end First_Body;

   -----------------------
   -- First_Declaration --
   -----------------------

   function First_Declaration return Declaration_Reference is
   begin
      return Entities.Table;
   end First_Declaration;

   -----------------
   -- First_Modif --
   -----------------

   function First_Modif (Decl : Declaration_Reference) return Reference is
   begin
      return Decl.Modif_Ref;
   end First_Modif;

   ---------------------
   -- First_Reference --
   ---------------------

   function First_Reference (Decl : Declaration_Reference) return Reference is
   begin
      return Decl.Ref_Ref;
   end First_Reference;

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
      The_Entities : Declaration_Reference := Entities.Table;
   begin
      while The_Entities /= null loop
         if The_Entities.Decl.Line = Line
           and then The_Entities.Decl.Column = Column
           and then The_Entities.Decl.File = File_Ref
         then
            return The_Entities;
         else
            The_Entities := The_Entities.Next;
         end if;
      end loop;

      return Empty_Declaration;
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
      With_Dir : Boolean := False)
      return     String
   is
   begin
      return Get_File (Decl.Decl.File, With_Dir);
   end Get_File;

   function Get_File
     (Ref      : Reference;
      With_Dir : Boolean := False)
      return     String
   is
   begin
      return Get_File (Ref.File, With_Dir);
   end Get_File;

   function Get_File
     (File     : File_Reference;
      With_Dir : in Boolean := False;
      Strip    : Natural := 0)
      return     String
   is
      function Internal_Strip (Full_Name : String) return String;
      --  Internal function to process the Strip parameter

      --------------------
      -- Internal_Strip --
      --------------------

      function Internal_Strip (Full_Name : String) return String is
         Unit_End, Extension_Start : Natural;
         S : Natural := Strip;
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

   begin
      --  If we do not want the full path name

      if not With_Dir then
         return Internal_Strip (File.File);
      end if;

      if File.Dir = null then

         if Ada.Strings.Fixed.Tail (File.File, 3) = "ali" then
            File.Dir := new String'(Find_ALI_File (File.File));
         else
            File.Dir := new String'(Find_Source_File (File.File));
         end if;
      end if;

      return Internal_Strip (File.Dir.all & File.File);
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
     (File : File_Reference; With_Dir : Boolean := False) return String is
   begin
      if File.Gnatchop_File.all = "" then
         return Get_File (File, With_Dir);
      else
         return File.Gnatchop_File.all;
      end if;
   end Get_Gnatchop_File;

   -----------------------
   -- Get_Gnatchop_File --
   -----------------------

   function Get_Gnatchop_File
     (Ref : Reference; With_Dir : Boolean := False) return String is
   begin
      return Get_Gnatchop_File (Ref.File, With_Dir);
   end Get_Gnatchop_File;

   -----------------------
   -- Get_Gnatchop_File --
   -----------------------

   function Get_Gnatchop_File
     (Decl : Declaration_Reference; With_Dir : Boolean := False) return String
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
     return Declaration_Reference is
   begin
      return Decl.Par_Symbol;
   end Get_Parent;

   ---------------------
   -- Get_Source_Line --
   ---------------------

   function Get_Source_Line (Ref : Reference) return String is
   begin
      return To_String (Ref.Source_Line);
   end Get_Source_Line;

   function Get_Source_Line (Decl : Declaration_Reference) return String is
   begin
      return To_String (Decl.Decl.Source_Line);
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

   -----------------------
   -- Grep_Source_Files --
   -----------------------

   procedure Grep_Source_Files is
      Decl : Declaration_Reference := First_Declaration;

      type Simple_Ref;
      type Simple_Ref_Access is access Simple_Ref;
      type Simple_Ref is record
         Ref  : Reference;
         Next : Simple_Ref_Access;
      end record;
      List : Simple_Ref_Access := null;
      --  This structure is used to speed up the parsing of Ada sources:
      --  Every reference found by parsing the .ali files is inserted in this
      --  list, sorted by filename and line numbers. This allows avoiding
      --  parsing a same ada file multiple times

      procedure Free is new Unchecked_Deallocation
        (Simple_Ref, Simple_Ref_Access);
      --  Clear an element of the list

      procedure Grep_List;
      --  For each reference in the list, parse the file and find the
      --  source line

      procedure Insert_In_Order (Ref  : Reference);
      --  Insert a new reference in the list, ordered by line numbers

      procedure Insert_List_Ref (First_Ref : Reference);
      --  Process a list of references

      ---------------
      -- Grep_List --
      ---------------

      procedure Grep_List is
         Line         : String (1 .. 1024);
         Last         : Natural;
         File         : Ada.Text_IO.File_Type;
         Line_Number  : Natural;
         Pos          : Natural;
         Save_List    : Simple_Ref_Access := List;
         Current_File : File_Reference;

      begin
         while List /= null loop

            --  Makes sure we can find and read the file

            Current_File := List.Ref.File;
            Line_Number  := 0;

            begin
               Ada.Text_IO.Open (File,
                                 Ada.Text_IO.In_File,
                                 Get_File (List.Ref, True));

               --  Read the file and find every relevant lines

               while List /= null
                 and then List.Ref.File = Current_File
                 and then not Ada.Text_IO.End_Of_File (File)
               loop
                  Ada.Text_IO.Get_Line (File, Line, Last);
                  Line_Number := Line_Number + 1;

                  while List /= null
                    and then Line_Number = List.Ref.Line
                  loop

                     --  Skip the leading blanks on the line

                     Pos := 1;
                     while Line (Pos) = ' '
                       or else Line (Pos) = ASCII.HT
                     loop
                        Pos := Pos + 1;
                     end loop;

                     List.Ref.Source_Line :=
                       To_Unbounded_String (Line (Pos .. Last));

                     --  Find the next element in the list

                     List := List.Next;
                  end loop;

               end loop;

               Ada.Text_IO.Close (File);

               --  If the Current_File was not found, just skip it

            exception
               when Ada.IO_Exceptions.Name_Error =>
                  null;
            end;

            --  If the line or the file were not found

            while List /= null
              and then List.Ref.File = Current_File
            loop
               List := List.Next;
            end loop;

         end loop;

         --  Clear the list

         while Save_List /= null loop
            List      := Save_List;
            Save_List := Save_List.Next;
            Free (List);
         end loop;
      end Grep_List;

      ---------------------
      -- Insert_In_Order --
      ---------------------

      procedure Insert_In_Order (Ref : Reference) is
         Iter : Simple_Ref_Access := List;
         Prev : Simple_Ref_Access := null;

      begin
         while Iter /= null loop

            --  If we have found the file, sort by lines

            if Iter.Ref.File = Ref.File then

               while Iter /= null
                 and then Iter.Ref.File = Ref.File
               loop
                  if Iter.Ref.Line > Ref.Line then

                     if Iter = List then
                        List := new Simple_Ref'(Ref, List);
                     else
                        Prev.Next := new Simple_Ref'(Ref, Iter);
                     end if;
                     return;
                  end if;

                  Prev := Iter;
                  Iter := Iter.Next;
               end loop;

               if Iter = List then
                  List := new Simple_Ref'(Ref, List);
               else
                  Prev.Next := new Simple_Ref'(Ref, Iter);
               end if;

               return;
            end if;

            Prev := Iter;
            Iter := Iter.Next;
         end loop;

         --  The file was not already in the list, insert it

         List := new Simple_Ref'(Ref, List);
      end Insert_In_Order;

      ---------------------
      -- Insert_List_Ref --
      ---------------------

      procedure Insert_List_Ref (First_Ref : Reference) is
         Ref : Reference := First_Ref;

      begin
         while Ref /= Empty_Reference loop
            Insert_In_Order (Ref);
            Ref := Next (Ref);
         end loop;
      end Insert_List_Ref;

   --  Start of processing for Grep_Source_Files

   begin
      while Decl /= Empty_Declaration loop
         Insert_In_Order (Decl.Decl'Access);
         Insert_List_Ref (First_Body (Decl));
         Insert_List_Ref (First_Reference (Decl));
         Insert_List_Ref (First_Modif (Decl));
         Decl := Next (Decl);
      end loop;

      Grep_List;
   end Grep_Source_Files;

   -----------------------
   -- Longest_File_Name --
   -----------------------

   function Longest_File_Name return Natural is
   begin
      return Files.Longest_Name;
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

   function Next (Decl : Declaration_Reference) return Declaration_Reference is
   begin
      return Decl.Next;
   end Next;

   ----------
   -- Next --
   ----------

   function Next (Ref : Reference) return Reference is
   begin
      return Ref.Next;
   end Next;

   ------------------
   -- Next_Obj_Dir --
   ------------------

   function Next_Obj_Dir return String is
      First : Integer := Directories.Obj_Dir_Index;
      Last  : Integer := Directories.Obj_Dir_Index;

   begin
      if Last > Directories.Obj_Dir_Length then
         return String'(1 .. 0 => ' ');
      end if;

      while Directories.Obj_Dir (Last) /= ' ' loop
         Last := Last + 1;
      end loop;

      Directories.Obj_Dir_Index := Last + 1;
      Directories.Last_Obj_Dir_Start := First;
      return Directories.Obj_Dir (First .. Last - 1);
   end Next_Obj_Dir;

   ------------------
   -- Next_Src_Dir --
   ------------------

   function Next_Src_Dir return String is
      First : Integer := Directories.Src_Dir_Index;
      Last  : Integer := Directories.Src_Dir_Index;

   begin
      if Last > Directories.Src_Dir_Length then
         return String'(1 .. 0 => ' ');
      end if;

      while Directories.Src_Dir (Last) /= ' ' loop
         Last := Last + 1;
      end loop;

      Directories.Src_Dir_Index := Last + 1;
      return Directories.Src_Dir (First .. Last - 1);
   end Next_Src_Dir;

   -------------------------
   -- Next_Unvisited_File --
   -------------------------

   function Next_Unvisited_File return File_Reference is
      The_Files : File_Reference := Files.Table;

   begin
      while The_Files /= null loop
         if not The_Files.Visited then
            The_Files.Visited := True;
            return The_Files;
         end if;

         The_Files := The_Files.Next;
      end loop;

      return Empty_File;
   end Next_Unvisited_File;

   ------------------
   -- Parse_Gnatls --
   ------------------

   procedure Parse_Gnatls
     (Gnatls_Src_Cache : out Ada.Strings.Unbounded.Unbounded_String;
      Gnatls_Obj_Cache : out Ada.Strings.Unbounded.Unbounded_String)
   is
   begin
      Osint.Add_Default_Search_Dirs;

      for J in 1 .. Osint.Nb_Dir_In_Src_Search_Path loop
         if Osint.Dir_In_Src_Search_Path (J)'Length = 0 then
            Ada.Strings.Unbounded.Append (Gnatls_Src_Cache, "./" & ' ');
         else
            Ada.Strings.Unbounded.Append
              (Gnatls_Src_Cache, Osint.Dir_In_Src_Search_Path (J).all & ' ');
         end if;
      end loop;

      for J in 1 .. Osint.Nb_Dir_In_Obj_Search_Path loop
         if Osint.Dir_In_Obj_Search_Path (J)'Length = 0 then
            Ada.Strings.Unbounded.Append (Gnatls_Obj_Cache, "./" & ' ');
         else
            Ada.Strings.Unbounded.Append
              (Gnatls_Obj_Cache, Osint.Dir_In_Obj_Search_Path (J).all & ' ');
         end if;
      end loop;
   end Parse_Gnatls;

   -------------------
   -- Reset_Obj_Dir --
   -------------------

   procedure Reset_Obj_Dir is
   begin
      Directories.Obj_Dir_Index := 1;
   end Reset_Obj_Dir;

   -------------------
   -- Reset_Src_Dir --
   -------------------

   procedure Reset_Src_Dir is
   begin
      Directories.Src_Dir_Index := 1;
   end Reset_Src_Dir;

   -----------------------
   -- Set_Default_Match --
   -----------------------

   procedure Set_Default_Match (Value : Boolean) is
   begin
      Default_Match := Value;
   end Set_Default_Match;

   -------------------
   -- Set_Directory --
   -------------------

   procedure Set_Directory
     (File : in File_Reference;
      Dir  : in String)
   is
   begin
      File.Dir := new String'(Dir);
   end Set_Directory;

   -------------------
   -- Set_Unvisited --
   -------------------

   procedure Set_Unvisited (File_Ref : in File_Reference) is
      The_Files : File_Reference := Files.Table;

   begin
      while The_Files /= null loop
         if The_Files = File_Ref then
            The_Files.Visited := False;
            return;
         end if;

         The_Files := The_Files.Next;
      end loop;
   end Set_Unvisited;

end Xr_Tabls;
