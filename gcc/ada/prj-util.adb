------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P R J . U T I L                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2001-2011, Free Software Foundation, Inc.         --
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

with Ada.Unchecked_Deallocation;

with GNAT.Case_Util; use GNAT.Case_Util;
with GNAT.Regexp;    use GNAT.Regexp;

with Osint;    use Osint;
with Output;   use Output;
with Opt;
with Prj.Com;
with Snames;   use Snames;
with Table;
with Targparm; use Targparm;

with GNAT.HTable;

package body Prj.Util is

   package Source_Info_Table is new Table.Table
     (Table_Component_Type => Source_Info_Iterator,
      Table_Index_Type     => Natural,
      Table_Low_Bound      => 1,
      Table_Initial        => 10,
      Table_Increment      => 100,
      Table_Name           => "Makeutl.Source_Info_Table");

   package Source_Info_Project_HTable is new GNAT.HTable.Simple_HTable
     (Header_Num => Prj.Header_Num,
      Element    => Natural,
      No_Element => 0,
      Key        => Name_Id,
      Hash       => Prj.Hash,
      Equal      => "=");

   procedure Free is new Ada.Unchecked_Deallocation
     (Text_File_Data, Text_File);

   -----------
   -- Close --
   -----------

   procedure Close (File : in out Text_File) is
      Len : Integer;
      Status : Boolean;

   begin
      if File = null then
         Prj.Com.Fail ("Close attempted on an invalid Text_File");
      end if;

      if File.Out_File then
         if File.Buffer_Len > 0 then
            Len := Write (File.FD, File.Buffer'Address, File.Buffer_Len);

            if Len /= File.Buffer_Len then
               Prj.Com.Fail ("Unable to write to an out Text_File");
            end if;
         end if;

         Close (File.FD, Status);

         if not Status then
            Prj.Com.Fail ("Unable to close an out Text_File");
         end if;

      else

         --  Close in file, no need to test status, since this is a file that
         --  we read, and the file was read successfully before we closed it.

         Close (File.FD);
      end if;

      Free (File);
   end Close;

   ------------
   -- Create --
   ------------

   procedure Create (File : out Text_File; Name : String) is
      FD        : File_Descriptor;
      File_Name : String (1 .. Name'Length + 1);

   begin
      File_Name (1 .. Name'Length) := Name;
      File_Name (File_Name'Last) := ASCII.NUL;
      FD := Create_File (Name => File_Name'Address,
                         Fmode => GNAT.OS_Lib.Text);

      if FD = Invalid_FD then
         File := null;

      else
         File := new Text_File_Data;
         File.FD := FD;
         File.Out_File := True;
         File.End_Of_File_Reached := True;
      end if;
   end Create;

   ---------------
   -- Duplicate --
   ---------------

   procedure Duplicate
     (This   : in out Name_List_Index;
      Shared : Shared_Project_Tree_Data_Access)
   is
      Old_Current : Name_List_Index;
      New_Current : Name_List_Index;

   begin
      if This /= No_Name_List then
         Old_Current := This;
         Name_List_Table.Increment_Last (Shared.Name_Lists);
         New_Current := Name_List_Table.Last (Shared.Name_Lists);
         This := New_Current;
         Shared.Name_Lists.Table (New_Current) :=
           (Shared.Name_Lists.Table (Old_Current).Name, No_Name_List);

         loop
            Old_Current := Shared.Name_Lists.Table (Old_Current).Next;
            exit when Old_Current = No_Name_List;
            Shared.Name_Lists.Table (New_Current).Next := New_Current + 1;
            Name_List_Table.Increment_Last (Shared.Name_Lists);
            New_Current := New_Current + 1;
            Shared.Name_Lists.Table (New_Current) :=
              (Shared.Name_Lists.Table (Old_Current).Name, No_Name_List);
         end loop;
      end if;
   end Duplicate;

   -----------------
   -- End_Of_File --
   -----------------

   function End_Of_File (File : Text_File) return Boolean is
   begin
      if File = null then
         Prj.Com.Fail ("End_Of_File attempted on an invalid Text_File");
      end if;

      return File.End_Of_File_Reached;
   end End_Of_File;

   -------------------
   -- Executable_Of --
   -------------------

   function Executable_Of
     (Project  : Project_Id;
      Shared   : Shared_Project_Tree_Data_Access;
      Main     : File_Name_Type;
      Index    : Int;
      Ada_Main : Boolean := True;
      Language : String := "";
      Include_Suffix : Boolean := True) return File_Name_Type
   is
      pragma Assert (Project /= No_Project);

      The_Packages : constant Package_Id := Project.Decl.Packages;

      Builder_Package : constant Prj.Package_Id :=
                          Prj.Util.Value_Of
                            (Name        => Name_Builder,
                             In_Packages => The_Packages,
                             Shared      => Shared);

      Executable : Variable_Value :=
                     Prj.Util.Value_Of
                       (Name                    => Name_Id (Main),
                        Index                   => Index,
                        Attribute_Or_Array_Name => Name_Executable,
                        In_Package              => Builder_Package,
                        Shared                  => Shared);

      Lang   : Language_Ptr;

      Spec_Suffix : Name_Id := No_Name;
      Body_Suffix : Name_Id := No_Name;

      Spec_Suffix_Length : Natural := 0;
      Body_Suffix_Length : Natural := 0;

      procedure Get_Suffixes
        (B_Suffix : File_Name_Type;
         S_Suffix : File_Name_Type);
      --  Get the non empty suffixes in variables Spec_Suffix and Body_Suffix

      function Add_Suffix (File : File_Name_Type) return File_Name_Type;
      --  Return the name of the executable, based on File, and adding the
      --  executable suffix if needed

      ------------------
      -- Get_Suffixes --
      ------------------

      procedure Get_Suffixes
        (B_Suffix : File_Name_Type;
         S_Suffix : File_Name_Type)
      is
      begin
         if B_Suffix /= No_File then
            Body_Suffix := Name_Id (B_Suffix);
            Body_Suffix_Length := Natural (Length_Of_Name (Body_Suffix));
         end if;

         if S_Suffix /= No_File then
            Spec_Suffix := Name_Id (S_Suffix);
            Spec_Suffix_Length := Natural (Length_Of_Name (Spec_Suffix));
         end if;
      end Get_Suffixes;

      ----------------
      -- Add_Suffix --
      ----------------

      function Add_Suffix (File : File_Name_Type) return File_Name_Type is
         Saved_EEOT : constant Name_Id := Executable_Extension_On_Target;
         Result     : File_Name_Type;
         Suffix_From_Project : Variable_Value;
      begin
         if Include_Suffix then
            if Project.Config.Executable_Suffix /= No_Name then
               Executable_Extension_On_Target :=
                 Project.Config.Executable_Suffix;
            end if;

            Result :=  Executable_Name (File);
            Executable_Extension_On_Target := Saved_EEOT;
            return Result;

         elsif Builder_Package /= No_Package then

            --  If the suffix is specified in the project itself, as opposed to
            --  the config file, it needs to be taken into account. However,
            --  when the project was processed, in both cases the suffix was
            --  stored in Project.Config, so get it from the project again.

            Suffix_From_Project :=
              Prj.Util.Value_Of
                (Variable_Name => Name_Executable_Suffix,
                 In_Variables  =>
                   Shared.Packages.Table (Builder_Package).Decl.Attributes,
                 Shared        => Shared);

            if Suffix_From_Project /= Nil_Variable_Value
              and then Suffix_From_Project.Value /= No_Name
            then
               Executable_Extension_On_Target := Suffix_From_Project.Value;
               Result :=  Executable_Name (File);
               Executable_Extension_On_Target := Saved_EEOT;
               return Result;
            end if;
         end if;

         return File;
      end Add_Suffix;

   --  Start of processing for Executable_Of

   begin
      if Ada_Main then
         Lang := Get_Language_From_Name (Project, "ada");
      elsif Language /= "" then
         Lang := Get_Language_From_Name (Project, Language);
      end if;

      if Lang /= null then
         Get_Suffixes
           (B_Suffix => Lang.Config.Naming_Data.Body_Suffix,
            S_Suffix => Lang.Config.Naming_Data.Spec_Suffix);
      end if;

      if Builder_Package /= No_Package then
         if Executable = Nil_Variable_Value and then Ada_Main then
            Get_Name_String (Main);

            --  Try as index the name minus the implementation suffix or minus
            --  the specification suffix.

            declare
               Name : constant String (1 .. Name_Len) :=
                        Name_Buffer (1 .. Name_Len);
               Last : Positive := Name_Len;

               Truncated : Boolean := False;

            begin
               if Body_Suffix /= No_Name
                 and then Last > Natural (Length_Of_Name (Body_Suffix))
                 and then Name (Last - Body_Suffix_Length + 1 .. Last) =
                            Get_Name_String (Body_Suffix)
               then
                  Truncated := True;
                  Last := Last - Body_Suffix_Length;
               end if;

               if Spec_Suffix /= No_Name
                 and then not Truncated
                 and then Last > Spec_Suffix_Length
                 and then Name (Last - Spec_Suffix_Length + 1 .. Last) =
                            Get_Name_String (Spec_Suffix)
               then
                  Truncated := True;
                  Last := Last - Spec_Suffix_Length;
               end if;

               if Truncated then
                  Name_Len := Last;
                  Name_Buffer (1 .. Name_Len) := Name (1 .. Last);
                  Executable :=
                    Prj.Util.Value_Of
                      (Name                    => Name_Find,
                       Index                   => 0,
                       Attribute_Or_Array_Name => Name_Executable,
                       In_Package              => Builder_Package,
                       Shared                  => Shared);
               end if;
            end;
         end if;

         --  If we have found an Executable attribute, return its value,
         --  possibly suffixed by the executable suffix.

         if Executable /= Nil_Variable_Value
           and then Executable.Value /= No_Name
           and then Length_Of_Name (Executable.Value) /= 0
         then
            return Add_Suffix (File_Name_Type (Executable.Value));
         end if;
      end if;

      Get_Name_String (Main);

      --  If there is a body suffix or a spec suffix, remove this suffix,
      --  otherwise remove any suffix ('.' followed by other characters), if
      --  there is one.

      if Body_Suffix /= No_Name
         and then Name_Len > Body_Suffix_Length
         and then Name_Buffer (Name_Len - Body_Suffix_Length + 1 .. Name_Len) =
                    Get_Name_String (Body_Suffix)
      then
         --  Found the body termination, remove it

         Name_Len := Name_Len - Body_Suffix_Length;

      elsif Spec_Suffix /= No_Name
            and then Name_Len > Spec_Suffix_Length
            and then
              Name_Buffer (Name_Len - Spec_Suffix_Length + 1 .. Name_Len) =
                Get_Name_String (Spec_Suffix)
      then
         --  Found the spec termination, remove it

         Name_Len := Name_Len - Spec_Suffix_Length;

      else
         --  Remove any suffix, if there is one

         Get_Name_String (Strip_Suffix (Main));
      end if;

      return Add_Suffix (Name_Find);
   end Executable_Of;

   --------------
   -- Get_Line --
   --------------

   procedure Get_Line
     (File : Text_File;
      Line : out String;
      Last : out Natural)
   is
      C : Character;

      procedure Advance;

      -------------
      -- Advance --
      -------------

      procedure Advance is
      begin
         if File.Cursor = File.Buffer_Len then
            File.Buffer_Len :=
              Read
               (FD => File.FD,
                A  => File.Buffer'Address,
                N  => File.Buffer'Length);

            if File.Buffer_Len = 0 then
               File.End_Of_File_Reached := True;
               return;
            else
               File.Cursor := 1;
            end if;

         else
            File.Cursor := File.Cursor + 1;
         end if;
      end Advance;

   --  Start of processing for Get_Line

   begin
      if File = null then
         Prj.Com.Fail ("Get_Line attempted on an invalid Text_File");

      elsif File.Out_File then
         Prj.Com.Fail ("Get_Line attempted on an out file");
      end if;

      Last := Line'First - 1;

      if not File.End_Of_File_Reached then
         loop
            C := File.Buffer (File.Cursor);
            exit when C = ASCII.CR or else C = ASCII.LF;
            Last := Last + 1;
            Line (Last) := C;
            Advance;

            if File.End_Of_File_Reached then
               return;
            end if;

            exit when Last = Line'Last;
         end loop;

         if C = ASCII.CR or else C = ASCII.LF then
            Advance;

            if File.End_Of_File_Reached then
               return;
            end if;
         end if;

         if C = ASCII.CR
           and then File.Buffer (File.Cursor) = ASCII.LF
         then
            Advance;
         end if;
      end if;
   end Get_Line;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Iter        : out Source_Info_Iterator;
      For_Project : Name_Id)
   is
      Ind : constant Natural := Source_Info_Project_HTable.Get (For_Project);
   begin
      if Ind = 0 then
         Iter := (No_Source_Info, 0);
      else
         Iter := Source_Info_Table.Table (Ind);
      end if;
   end Initialize;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (File : Text_File) return Boolean is
   begin
      return File /= null;
   end Is_Valid;

   ----------
   -- Next --
   ----------

   procedure Next (Iter : in out Source_Info_Iterator) is
   begin
      if Iter.Next = 0 then
         Iter.Info := No_Source_Info;

      else
         Iter := Source_Info_Table.Table (Iter.Next);
      end if;
   end Next;

   ----------
   -- Open --
   ----------

   procedure Open (File : out Text_File; Name : String) is
      FD        : File_Descriptor;
      File_Name : String (1 .. Name'Length + 1);

   begin
      File_Name (1 .. Name'Length) := Name;
      File_Name (File_Name'Last) := ASCII.NUL;
      FD := Open_Read (Name => File_Name'Address,
                       Fmode => GNAT.OS_Lib.Text);

      if FD = Invalid_FD then
         File := null;

      else
         File := new Text_File_Data;
         File.FD := FD;
         File.Buffer_Len :=
           Read (FD => FD,
                 A  => File.Buffer'Address,
                 N  => File.Buffer'Length);

         if File.Buffer_Len = 0 then
            File.End_Of_File_Reached := True;
         else
            File.Cursor := 1;
         end if;
      end if;
   end Open;

   ---------
   -- Put --
   ---------

   procedure Put
     (Into_List  : in out Name_List_Index;
      From_List  : String_List_Id;
      In_Tree    : Project_Tree_Ref;
      Lower_Case : Boolean := False)
   is
      Shared  : constant Shared_Project_Tree_Data_Access := In_Tree.Shared;

      Current_Name : Name_List_Index;
      List         : String_List_Id;
      Element      : String_Element;
      Last         : Name_List_Index :=
                       Name_List_Table.Last (Shared.Name_Lists);
      Value        : Name_Id;

   begin
      Current_Name := Into_List;
      while Current_Name /= No_Name_List
        and then Shared.Name_Lists.Table (Current_Name).Next /= No_Name_List
      loop
         Current_Name := Shared.Name_Lists.Table (Current_Name).Next;
      end loop;

      List := From_List;
      while List /= Nil_String loop
         Element := Shared.String_Elements.Table (List);
         Value := Element.Value;

         if Lower_Case then
            Get_Name_String (Value);
            To_Lower (Name_Buffer (1 .. Name_Len));
            Value := Name_Find;
         end if;

         Name_List_Table.Append
           (Shared.Name_Lists, (Name => Value, Next => No_Name_List));

         Last := Last + 1;

         if Current_Name = No_Name_List then
            Into_List := Last;
         else
            Shared.Name_Lists.Table (Current_Name).Next := Last;
         end if;

         Current_Name := Last;

         List := Element.Next;
      end loop;
   end Put;

   procedure Put (File : Text_File; S : String) is
      Len : Integer;
   begin
      if File = null then
         Prj.Com.Fail ("Attempted to write on an invalid Text_File");

      elsif not File.Out_File then
         Prj.Com.Fail ("Attempted to write an in Text_File");
      end if;

      if File.Buffer_Len + S'Length > File.Buffer'Last then
         --  Write buffer
         Len := Write (File.FD, File.Buffer'Address, File.Buffer_Len);

         if Len /= File.Buffer_Len then
            Prj.Com.Fail ("Failed to write to an out Text_File");
         end if;

         File.Buffer_Len := 0;
      end if;

      File.Buffer (File.Buffer_Len + 1 .. File.Buffer_Len + S'Length) := S;
      File.Buffer_Len := File.Buffer_Len + S'Length;
   end Put;

   --------------
   -- Put_Line --
   --------------

   procedure Put_Line (File : Text_File; Line : String) is
      L : String (1 .. Line'Length + 1);
   begin
      L (1 .. Line'Length) := Line;
      L (L'Last) := ASCII.LF;
      Put (File, L);
   end Put_Line;

   ---------------------------
   -- Read_Source_Info_File --
   ---------------------------

   procedure Read_Source_Info_File (Tree : Project_Tree_Ref) is
      File : Text_File;
      Info : Source_Info_Iterator;
      Proj : Name_Id;

      procedure Report_Error;

      ------------------
      -- Report_Error --
      ------------------

      procedure Report_Error is
      begin
         Write_Line ("errors in source info file """ &
                     Tree.Source_Info_File_Name.all & '"');
         Tree.Source_Info_File_Exists := False;
      end Report_Error;

   begin
      Source_Info_Project_HTable.Reset;
      Source_Info_Table.Init;

      if Tree.Source_Info_File_Name = null then
         Tree.Source_Info_File_Exists := False;
         return;
      end if;

      Open (File, Tree.Source_Info_File_Name.all);

      if not Is_Valid (File) then
         if Opt.Verbose_Mode then
            Write_Line ("source info file " & Tree.Source_Info_File_Name.all &
                        " does not exist");
         end if;

         Tree.Source_Info_File_Exists := False;
         return;
      end if;

      Tree.Source_Info_File_Exists := True;

      if Opt.Verbose_Mode then
         Write_Line ("Reading source info file " &
                     Tree.Source_Info_File_Name.all);
      end if;

      Source_Loop :
      while not End_Of_File (File) loop
         Info := (new Source_Info_Data, 0);
         Source_Info_Table.Increment_Last;

         --  project name
         Get_Line (File, Name_Buffer, Name_Len);
         Proj := Name_Find;
         Info.Info.Project := Proj;
         Info.Next := Source_Info_Project_HTable.Get (Proj);
         Source_Info_Project_HTable.Set (Proj, Source_Info_Table.Last);

         if End_Of_File (File) then
            Report_Error;
            exit Source_Loop;
         end if;

         --  language name
         Get_Line (File, Name_Buffer, Name_Len);
         Info.Info.Language := Name_Find;

         if End_Of_File (File) then
            Report_Error;
            exit Source_Loop;
         end if;

         --  kind
         Get_Line (File, Name_Buffer, Name_Len);
         Info.Info.Kind := Source_Kind'Value (Name_Buffer (1 .. Name_Len));

         if End_Of_File (File) then
            Report_Error;
            exit Source_Loop;
         end if;

         --  display path name
         Get_Line (File, Name_Buffer, Name_Len);
         Info.Info.Display_Path_Name := Name_Find;
         Info.Info.Path_Name := Info.Info.Display_Path_Name;

         if End_Of_File (File) then
            Report_Error;
            exit Source_Loop;
         end if;

         --  optional fields
         Option_Loop :
         loop
            Get_Line (File, Name_Buffer, Name_Len);
            exit Option_Loop when Name_Len = 0;

            if Name_Len <= 2 then
               Report_Error;
               exit Source_Loop;

            else
               if Name_Buffer (1 .. 2) = "P=" then
                  Name_Buffer (1 .. Name_Len - 2) :=
                    Name_Buffer (3 .. Name_Len);
                  Name_Len := Name_Len - 2;
                  Info.Info.Path_Name := Name_Find;

               elsif Name_Buffer (1 .. 2) = "U=" then
                  Name_Buffer (1 .. Name_Len - 2) :=
                    Name_Buffer (3 .. Name_Len);
                  Name_Len := Name_Len - 2;
                  Info.Info.Unit_Name := Name_Find;

               elsif Name_Buffer (1 .. 2) = "I=" then
                  Info.Info.Index := Int'Value (Name_Buffer (3 .. Name_Len));

               elsif Name_Buffer (1 .. Name_Len) = "N=Y" then
                  Info.Info.Naming_Exception := Yes;

               elsif Name_Buffer (1 .. Name_Len) = "N=I" then
                  Info.Info.Naming_Exception := Inherited;

               else
                  Report_Error;
                  exit Source_Loop;
               end if;
            end if;
         end loop Option_Loop;

         Source_Info_Table.Table (Source_Info_Table.Last) := Info;
      end loop Source_Loop;

      Close (File);

   exception
      when others =>
         Close (File);
         Report_Error;
   end Read_Source_Info_File;

   --------------------
   -- Source_Info_Of --
   --------------------

   function Source_Info_Of (Iter : Source_Info_Iterator) return Source_Info is
   begin
      return Iter.Info;
   end Source_Info_Of;

   --------------
   -- Value_Of --
   --------------

   function Value_Of
     (Variable : Variable_Value;
      Default  : String) return String
   is
   begin
      if Variable.Kind /= Single
        or else Variable.Default
        or else Variable.Value = No_Name
      then
         return Default;
      else
         return Get_Name_String (Variable.Value);
      end if;
   end Value_Of;

   function Value_Of
     (Index    : Name_Id;
      In_Array : Array_Element_Id;
      Shared   : Shared_Project_Tree_Data_Access) return Name_Id
   is

      Current    : Array_Element_Id;
      Element    : Array_Element;
      Real_Index : Name_Id := Index;

   begin
      Current := In_Array;

      if Current = No_Array_Element then
         return No_Name;
      end if;

      Element := Shared.Array_Elements.Table (Current);

      if not Element.Index_Case_Sensitive then
         Get_Name_String (Index);
         To_Lower (Name_Buffer (1 .. Name_Len));
         Real_Index := Name_Find;
      end if;

      while Current /= No_Array_Element loop
         Element := Shared.Array_Elements.Table (Current);

         if Real_Index = Element.Index then
            exit when Element.Value.Kind /= Single;
            exit when Element.Value.Value = Empty_String;
            return Element.Value.Value;
         else
            Current := Element.Next;
         end if;
      end loop;

      return No_Name;
   end Value_Of;

   function Value_Of
     (Index                  : Name_Id;
      Src_Index              : Int := 0;
      In_Array               : Array_Element_Id;
      Shared                 : Shared_Project_Tree_Data_Access;
      Force_Lower_Case_Index : Boolean := False;
      Allow_Wildcards        : Boolean := False) return Variable_Value
   is
      Current      : Array_Element_Id;
      Element      : Array_Element;
      Real_Index_1 : Name_Id;
      Real_Index_2 : Name_Id;

   begin
      Current := In_Array;

      if Current = No_Array_Element then
         return Nil_Variable_Value;
      end if;

      Element := Shared.Array_Elements.Table (Current);

      Real_Index_1 := Index;

      if not Element.Index_Case_Sensitive or else Force_Lower_Case_Index then
         if Index /= All_Other_Names then
            Get_Name_String (Index);
            To_Lower (Name_Buffer (1 .. Name_Len));
            Real_Index_1 := Name_Find;
         end if;
      end if;

      while Current /= No_Array_Element loop
         Element := Shared.Array_Elements.Table (Current);
         Real_Index_2 := Element.Index;

         if not Element.Index_Case_Sensitive
           or else Force_Lower_Case_Index
         then
            if Element.Index /= All_Other_Names then
               Get_Name_String (Element.Index);
               To_Lower (Name_Buffer (1 .. Name_Len));
               Real_Index_2 := Name_Find;
            end if;
         end if;

         if Src_Index = Element.Src_Index and then
           (Real_Index_1 = Real_Index_2 or else
              (Real_Index_2 /= All_Other_Names and then
               Allow_Wildcards and then
                 Match (Get_Name_String (Real_Index_1),
                        Compile (Get_Name_String (Real_Index_2),
                                 Glob => True))))
         then
            return Element.Value;
         else
            Current := Element.Next;
         end if;
      end loop;

      return Nil_Variable_Value;
   end Value_Of;

   function Value_Of
     (Name                    : Name_Id;
      Index                   : Int := 0;
      Attribute_Or_Array_Name : Name_Id;
      In_Package              : Package_Id;
      Shared                  : Shared_Project_Tree_Data_Access;
      Force_Lower_Case_Index  : Boolean := False;
      Allow_Wildcards         : Boolean := False) return Variable_Value
   is
      The_Array     : Array_Element_Id;
      The_Attribute : Variable_Value := Nil_Variable_Value;

   begin
      if In_Package /= No_Package then

         --  First, look if there is an array element that fits

         The_Array :=
           Value_Of
             (Name      => Attribute_Or_Array_Name,
              In_Arrays => Shared.Packages.Table (In_Package).Decl.Arrays,
              Shared    => Shared);
         The_Attribute :=
           Value_Of
             (Index                  => Name,
              Src_Index              => Index,
              In_Array               => The_Array,
              Shared                 => Shared,
              Force_Lower_Case_Index => Force_Lower_Case_Index,
              Allow_Wildcards        => Allow_Wildcards);

         --  If there is no array element, look for a variable

         if The_Attribute = Nil_Variable_Value then
            The_Attribute :=
              Value_Of
                (Variable_Name => Attribute_Or_Array_Name,
                 In_Variables  => Shared.Packages.Table
                   (In_Package).Decl.Attributes,
                 Shared        => Shared);
         end if;
      end if;

      return The_Attribute;
   end Value_Of;

   function Value_Of
     (Index     : Name_Id;
      In_Array  : Name_Id;
      In_Arrays : Array_Id;
      Shared    : Shared_Project_Tree_Data_Access) return Name_Id
   is
      Current   : Array_Id;
      The_Array : Array_Data;

   begin
      Current := In_Arrays;
      while Current /= No_Array loop
         The_Array := Shared.Arrays.Table (Current);
         if The_Array.Name = In_Array then
            return Value_Of
              (Index, In_Array => The_Array.Value, Shared => Shared);
         else
            Current := The_Array.Next;
         end if;
      end loop;

      return No_Name;
   end Value_Of;

   function Value_Of
     (Name      : Name_Id;
      In_Arrays : Array_Id;
      Shared    : Shared_Project_Tree_Data_Access) return Array_Element_Id
   is
      Current   : Array_Id;
      The_Array : Array_Data;

   begin
      Current := In_Arrays;
      while Current /= No_Array loop
         The_Array := Shared.Arrays.Table (Current);

         if The_Array.Name = Name then
            return The_Array.Value;
         else
            Current := The_Array.Next;
         end if;
      end loop;

      return No_Array_Element;
   end Value_Of;

   function Value_Of
     (Name        : Name_Id;
      In_Packages : Package_Id;
      Shared      : Shared_Project_Tree_Data_Access) return Package_Id
   is
      Current     : Package_Id;
      The_Package : Package_Element;

   begin
      Current := In_Packages;
      while Current /= No_Package loop
         The_Package := Shared.Packages.Table (Current);
         exit when The_Package.Name /= No_Name
           and then The_Package.Name = Name;
         Current := The_Package.Next;
      end loop;

      return Current;
   end Value_Of;

   function Value_Of
     (Variable_Name : Name_Id;
      In_Variables  : Variable_Id;
      Shared        : Shared_Project_Tree_Data_Access) return Variable_Value
   is
      Current      : Variable_Id;
      The_Variable : Variable;

   begin
      Current := In_Variables;
      while Current /= No_Variable loop
         The_Variable := Shared.Variable_Elements.Table (Current);

         if Variable_Name = The_Variable.Name then
            return The_Variable.Value;
         else
            Current := The_Variable.Next;
         end if;
      end loop;

      return Nil_Variable_Value;
   end Value_Of;

   ----------------------------
   -- Write_Source_Info_File --
   ----------------------------

   procedure Write_Source_Info_File (Tree : Project_Tree_Ref) is
      Iter   : Source_Iterator := For_Each_Source (Tree);
      Source : Prj.Source_Id;
      File   : Text_File;

   begin
      if Opt.Verbose_Mode then
         Write_Line ("Writing new source info file " &
                     Tree.Source_Info_File_Name.all);
      end if;

      Create (File, Tree.Source_Info_File_Name.all);

      if not Is_Valid (File) then
         Write_Line ("warning: unable to create source info file """ &
                     Tree.Source_Info_File_Name.all & '"');
         return;
      end if;

      loop
         Source := Element (Iter);
         exit when Source = No_Source;

         if not Source.Locally_Removed and then
           Source.Replaced_By = No_Source
         then
            --  Project name

            Put_Line (File, Get_Name_String (Source.Project.Name));

            --  Language name

            Put_Line (File, Get_Name_String (Source.Language.Name));

            --  Kind

            Put_Line (File, Source.Kind'Img);

            --  Display path name

            Put_Line (File, Get_Name_String (Source.Path.Display_Name));

            --  Optional lines:

            --  Path name (P=)

            if Source.Path.Name /= Source.Path.Display_Name then
               Put (File, "P=");
               Put_Line (File, Get_Name_String (Source.Path.Name));
            end if;

            --  Unit name (U=)

            if Source.Unit /= No_Unit_Index then
               Put (File, "U=");
               Put_Line (File, Get_Name_String (Source.Unit.Name));
            end if;

            --  Multi-source index (I=)

            if Source.Index /= 0 then
               Put (File, "I=");
               Put_Line (File, Source.Index'Img);
            end if;

            --  Naming exception ("N=T");

            if Source.Naming_Exception = Yes then
               Put_Line (File, "N=Y");

            elsif Source.Naming_Exception = Inherited then
               Put_Line (File, "N=I");
            end if;

            --  Empty line to indicate end of info on this source

            Put_Line (File, "");
         end if;

         Next (Iter);
      end loop;

      Close (File);
   end Write_Source_Info_File;

   ---------------
   -- Write_Str --
   ---------------

   procedure Write_Str
     (S          : String;
      Max_Length : Positive;
      Separator  : Character)
   is
      First : Positive := S'First;
      Last  : Natural  := S'Last;

   begin
      --  Nothing to do for empty strings

      if S'Length > 0 then

         --  Start on a new line if current line is already longer than
         --  Max_Length.

         if Positive (Column) >= Max_Length then
            Write_Eol;
         end if;

         --  If length of remainder is longer than Max_Length, we need to
         --  cut the remainder in several lines.

         while Positive (Column) + S'Last - First > Max_Length loop

            --  Try the maximum length possible

            Last := First + Max_Length - Positive (Column);

            --  Look for last Separator in the line

            while Last >= First and then S (Last) /= Separator loop
               Last := Last - 1;
            end loop;

            --  If we do not find a separator, we output the maximum length
            --  possible.

            if Last < First then
               Last := First + Max_Length - Positive (Column);
            end if;

            Write_Line (S (First .. Last));

            --  Set the beginning of the new remainder

            First := Last + 1;
         end loop;

         --  What is left goes to the buffer, without EOL

         Write_Str (S (First .. S'Last));
      end if;
   end Write_Str;
end Prj.Util;
