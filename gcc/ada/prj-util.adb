------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P R J . U T I L                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--             Copyright (C) 2001-2003 Free Software Foundation, Inc.       --
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

with Ada.Unchecked_Deallocation;

with GNAT.Case_Util; use GNAT.Case_Util;

with Namet;    use Namet;
with Osint;    use Osint;
with Output;   use Output;
with Prj.Com;
with Snames;   use Snames;

package body Prj.Util is

   procedure Free is new Ada.Unchecked_Deallocation
     (Text_File_Data, Text_File);

   -----------
   -- Close --
   -----------

   procedure Close (File : in out Text_File) is
   begin
      if File = null then
         Prj.Com.Fail ("Close attempted on an invalid Text_File");
      end if;

      --  Close file, no need to test status, since this is a file that we
      --  read, and the file was read successfully before we closed it.

      Close (File.FD);
      Free (File);
   end Close;

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
     (Project : Project_Id; Main : Name_Id) return Name_Id
   is
      pragma Assert (Project /= No_Project);

      The_Packages : constant Package_Id :=
                       Projects.Table (Project).Decl.Packages;

      Builder_Package : constant Prj.Package_Id :=
                          Prj.Util.Value_Of
                            (Name        => Name_Builder,
                             In_Packages => The_Packages);

      Executable : Variable_Value :=
                     Prj.Util.Value_Of
                       (Name                    => Main,
                        Attribute_Or_Array_Name => Name_Executable,
                        In_Package              => Builder_Package);

      Executable_Suffix : constant Variable_Value :=
                            Prj.Util.Value_Of
                              (Name                    => Main,
                               Attribute_Or_Array_Name =>
                                 Name_Executable_Suffix,
                               In_Package              => Builder_Package);

      Body_Append : constant String := Get_Name_String
                                          (Projects.Table
                                            (Project).
                                              Naming.Current_Body_Suffix);

      Spec_Append : constant String := Get_Name_String
                                          (Projects.Table
                                            (Project).
                                               Naming.Current_Spec_Suffix);

   begin
      if Builder_Package /= No_Package then
         if Executable = Nil_Variable_Value then
            Get_Name_String (Main);

            --  Try as index the name minus the implementation suffix or minus
            --  the specification suffix.

            declare
               Name : constant String (1 .. Name_Len) :=
                        Name_Buffer (1 .. Name_Len);
               Last : Positive := Name_Len;

               Naming : constant Naming_Data :=
                          Projects.Table (Project).Naming;

               Spec_Suffix : constant String :=
                               Get_Name_String (Naming.Current_Spec_Suffix);
               Body_Suffix : constant String :=
                               Get_Name_String (Naming.Current_Body_Suffix);

               Truncated : Boolean := False;

            begin
               if Last > Body_Suffix'Length
                  and then Name (Last - Body_Suffix'Length + 1 .. Last) =
                                                                  Body_Suffix
               then
                  Truncated := True;
                  Last := Last - Body_Suffix'Length;
               end if;

               if not Truncated
                 and then Last > Spec_Suffix'Length
                 and then Name (Last - Spec_Suffix'Length + 1 .. Last) =
                                                                 Spec_Suffix
               then
                  Truncated := True;
                  Last := Last - Spec_Suffix'Length;
               end if;

               if Truncated then
                  Name_Len := Last;
                  Name_Buffer (1 .. Name_Len) := Name (1 .. Last);
                  Executable :=
                    Prj.Util.Value_Of
                      (Name                    => Name_Find,
                       Attribute_Or_Array_Name => Name_Executable,
                       In_Package              => Builder_Package);
               end if;
            end;
         end if;

         --  If we have found an Executable attribute, return its value,
         --  possibly suffixed by the executable suffix.

         if Executable /= Nil_Variable_Value
           and then Executable.Value /= Empty_Name
         then
            declare
               Exec_Suffix : String_Access := Get_Executable_Suffix;
               Result      : Name_Id := Executable.Value;

            begin
               if Exec_Suffix'Length /= 0 then
                  Get_Name_String (Executable.Value);
                  Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));

                  --  If the Executable does not end with the executable
                  --  suffix, add it.

                  if Name_Len <= Exec_Suffix'Length
                    or else
                      Name_Buffer
                        (Name_Len - Exec_Suffix'Length + 1 .. Name_Len) /=
                                                               Exec_Suffix.all
                  then
                     --  Get the original Executable to keep the correct
                     --  case for systems where file names are case
                     --  insensitive (Windows).

                     Get_Name_String (Executable.Value);
                     Name_Buffer
                       (Name_Len + 1 .. Name_Len + Exec_Suffix'Length) :=
                       Exec_Suffix.all;
                     Name_Len := Name_Len + Exec_Suffix'Length;
                     Result := Name_Find;
                  end if;

                  Free (Exec_Suffix);
               end if;

               return Result;
            end;
         end if;
      end if;

      Get_Name_String (Main);

      --  If there is a body suffix or a spec suffix, remove this suffix,
      --  otherwise remove any suffix ('.' followed by other characters), if
      --  there is one.

      if Name_Len > Body_Append'Length
         and then Name_Buffer (Name_Len - Body_Append'Length + 1 .. Name_Len) =
                    Body_Append
      then
         --  Found the body termination, remove it

         Name_Len := Name_Len - Body_Append'Length;

      elsif Name_Len > Spec_Append'Length
         and then Name_Buffer (Name_Len - Spec_Append'Length + 1 .. Name_Len) =
                    Spec_Append
      then
         --  Found the spec termination, remove it

         Name_Len := Name_Len - Spec_Append'Length;

      else
         --  Remove any suffix, if there is one

         Get_Name_String (Strip_Suffix (Main));
      end if;

      if Executable_Suffix /= Nil_Variable_Value
        and then not Executable_Suffix.Default
      then
         --  If attribute Executable_Suffix is specified, add this suffix

         declare
            Suffix : constant String :=
                       Get_Name_String (Executable_Suffix.Value);
         begin
            Name_Buffer (Name_Len + 1 .. Name_Len + Suffix'Length) := Suffix;
            Name_Len := Name_Len + Suffix'Length;
            return Name_Find;
         end;

      else
         --  Otherwise, add the standard suffix for the platform, if any

         return Executable_Name (Name_Find);
      end if;
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

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (File : Text_File) return Boolean is
   begin
      return File /= null;
   end Is_Valid;

   ----------
   -- Open --
   ----------

   procedure Open (File : out Text_File; Name : in String) is
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

   --------------
   -- Value_Of --
   --------------

   function Value_Of
     (Variable : Variable_Value;
      Default  : String)
      return     String
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
      In_Array : Array_Element_Id)
      return     Name_Id
   is
      Current    : Array_Element_Id := In_Array;
      Element    : Array_Element;
      Real_Index : Name_Id := Index;

   begin
      if Current = No_Array_Element then
         return No_Name;
      end if;

      Element := Array_Elements.Table (Current);

      if not Element.Index_Case_Sensitive then
         Get_Name_String (Index);
         To_Lower (Name_Buffer (1 .. Name_Len));
         Real_Index := Name_Find;
      end if;

      while Current /= No_Array_Element loop
         Element := Array_Elements.Table (Current);

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
     (Index    : Name_Id;
      In_Array : Array_Element_Id)
      return     Variable_Value
   is
      Current : Array_Element_Id := In_Array;
      Element : Array_Element;
      Real_Index : Name_Id := Index;

   begin
      if Current = No_Array_Element then
         return Nil_Variable_Value;
      end if;

      Element := Array_Elements.Table (Current);

      if not Element.Index_Case_Sensitive then
         Get_Name_String (Index);
         To_Lower (Name_Buffer (1 .. Name_Len));
         Real_Index := Name_Find;
      end if;

      while Current /= No_Array_Element loop
         Element := Array_Elements.Table (Current);

         if Real_Index = Element.Index then
            return Element.Value;
         else
            Current := Element.Next;
         end if;
      end loop;

      return Nil_Variable_Value;
   end Value_Of;

   function Value_Of
     (Name                    : Name_Id;
      Attribute_Or_Array_Name : Name_Id;
      In_Package              : Package_Id)
      return                    Variable_Value
   is
      The_Array     : Array_Element_Id;
      The_Attribute : Variable_Value := Nil_Variable_Value;

   begin
      if In_Package /= No_Package then

         --  First, look if there is an array element that fits

         The_Array :=
           Value_Of
             (Name      => Attribute_Or_Array_Name,
              In_Arrays => Packages.Table (In_Package).Decl.Arrays);
         The_Attribute :=
           Value_Of
             (Index    => Name,
              In_Array => The_Array);

         --  If there is no array element, look for a variable

         if The_Attribute = Nil_Variable_Value then
            The_Attribute :=
              Value_Of
                (Variable_Name => Attribute_Or_Array_Name,
                 In_Variables  => Packages.Table (In_Package).Decl.Attributes);
         end if;
      end if;

      return The_Attribute;
   end Value_Of;

   function Value_Of
     (Index     : Name_Id;
      In_Array  : Name_Id;
      In_Arrays : Array_Id)
      return      Name_Id
   is
      Current : Array_Id := In_Arrays;
      The_Array : Array_Data;

   begin
      while Current /= No_Array loop
         The_Array := Arrays.Table (Current);
         if The_Array.Name = In_Array then
            return Value_Of (Index, In_Array => The_Array.Value);
         else
            Current := The_Array.Next;
         end if;
      end loop;

      return No_Name;
   end Value_Of;

   function Value_Of
     (Name      : Name_Id;
      In_Arrays : Array_Id)
      return      Array_Element_Id
   is
      Current    : Array_Id := In_Arrays;
      The_Array  : Array_Data;

   begin
      while Current /= No_Array loop
         The_Array := Arrays.Table (Current);

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
      In_Packages : Package_Id)
      return        Package_Id
   is
      Current : Package_Id := In_Packages;
      The_Package : Package_Element;

   begin
      while Current /= No_Package loop
         The_Package := Packages.Table (Current);
         exit when The_Package.Name /= No_Name
           and then The_Package.Name = Name;
         Current := The_Package.Next;
      end loop;

      return Current;
   end Value_Of;

   function Value_Of
     (Variable_Name : Name_Id;
      In_Variables  : Variable_Id)
      return          Variable_Value
   is
      Current      : Variable_Id := In_Variables;
      The_Variable : Variable;

   begin
      while Current /= No_Variable loop
         The_Variable := Variable_Elements.Table (Current);

         if Variable_Name = The_Variable.Name then
            return The_Variable.Value;
         else
            Current := The_Variable.Next;
         end if;
      end loop;

      return Nil_Variable_Value;
   end Value_Of;

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
