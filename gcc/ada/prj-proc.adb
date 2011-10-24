------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P R J . P R O C                             --
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

with Err_Vars; use Err_Vars;
with Opt;      use Opt;
with Osint;    use Osint;
with Output;   use Output;
with Prj.Attr; use Prj.Attr;
with Prj.Env;
with Prj.Err;  use Prj.Err;
with Prj.Ext;  use Prj.Ext;
with Prj.Nmsc; use Prj.Nmsc;
with Prj.Part;
with Snames;

with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with GNAT.Case_Util; use GNAT.Case_Util;
with GNAT.HTable;

package body Prj.Proc is

   package Processed_Projects is new GNAT.HTable.Simple_HTable
     (Header_Num => Header_Num,
      Element    => Project_Id,
      No_Element => No_Project,
      Key        => Name_Id,
      Hash       => Hash,
      Equal      => "=");
   --  This hash table contains all processed projects

   package Unit_Htable is new GNAT.HTable.Simple_HTable
     (Header_Num => Header_Num,
      Element    => Source_Id,
      No_Element => No_Source,
      Key        => Name_Id,
      Hash       => Hash,
      Equal      => "=");
   --  This hash table contains all processed projects

   procedure Add (To_Exp : in out Name_Id; Str : Name_Id);
   --  Concatenate two strings and returns another string if both
   --  arguments are not null string.

   --  In the following procedures, we are expected to guess the meaning of
   --  the parameters from their names, this is never a good idea, comments
   --  should be added precisely defining every formal ???

   procedure Add_Attributes
     (Project       : Project_Id;
      Project_Name  : Name_Id;
      Project_Dir   : Name_Id;
      Shared        : Shared_Project_Tree_Data_Access;
      Decl          : in out Declarations;
      First         : Attribute_Node_Id;
      Project_Level : Boolean);
   --  Add all attributes, starting with First, with their default values to
   --  the package or project with declarations Decl.

   procedure Check
     (In_Tree   : Project_Tree_Ref;
      Project   : Project_Id;
      Node_Tree : Prj.Tree.Project_Node_Tree_Ref;
      Flags     : Processing_Flags);
   --  Set all projects to not checked, then call Recursive_Check for the
   --  main project Project. Project is set to No_Project if errors occurred.
   --  Current_Dir is for optimization purposes, avoiding extra system calls.
   --  If Allow_Duplicate_Basenames, then files with the same base names are
   --  authorized within a project for source-based languages (never for unit
   --  based languages)

   procedure Copy_Package_Declarations
     (From       : Declarations;
      To         : in out Declarations;
      New_Loc    : Source_Ptr;
      Restricted : Boolean;
      Shared     : Shared_Project_Tree_Data_Access);
   --  Copy a package declaration From to To for a renamed package. Change the
   --  locations of all the attributes to New_Loc. When Restricted is
   --  True, do not copy attributes Body, Spec, Implementation, Specification
   --  and Linker_Options.

   function Expression
     (Project                : Project_Id;
      Shared                 : Shared_Project_Tree_Data_Access;
      From_Project_Node      : Project_Node_Id;
      From_Project_Node_Tree : Project_Node_Tree_Ref;
      Env                    : Prj.Tree.Environment;
      Pkg                    : Package_Id;
      First_Term             : Project_Node_Id;
      Kind                   : Variable_Kind) return Variable_Value;
   --  From N_Expression project node From_Project_Node, compute the value
   --  of an expression and return it as a Variable_Value.

   function Imported_Or_Extended_Project_From
     (Project   : Project_Id;
      With_Name : Name_Id) return Project_Id;
   --  Find an imported or extended project of Project whose name is With_Name

   function Package_From
     (Project   : Project_Id;
      Shared    : Shared_Project_Tree_Data_Access;
      With_Name : Name_Id) return Package_Id;
   --  Find the package of Project whose name is With_Name

   procedure Process_Declarative_Items
     (Project           : Project_Id;
      In_Tree           : Project_Tree_Ref;
      From_Project_Node : Project_Node_Id;
      Node_Tree         : Project_Node_Tree_Ref;
      Env               : Prj.Tree.Environment;
      Pkg               : Package_Id;
      Item              : Project_Node_Id;
      Child_Env         : in out Prj.Tree.Environment);
   --  Process declarative items starting with From_Project_Node, and put them
   --  in declarations Decl. This is a recursive procedure; it calls itself for
   --  a package declaration or a case construction.
   --
   --  Child_Env is the modified environment after seeing declarations like
   --  "for External(...) use" or "for Project_Path use" in aggregate projects.
   --  It should have been initialized first.

   procedure Recursive_Process
     (In_Tree                : Project_Tree_Ref;
      Project                : out Project_Id;
      Packages_To_Check      : String_List_Access;
      From_Project_Node      : Project_Node_Id;
      From_Project_Node_Tree : Project_Node_Tree_Ref;
      Env                    : in out Prj.Tree.Environment;
      Extended_By            : Project_Id);
   --  Process project with node From_Project_Node in the tree. Do nothing if
   --  From_Project_Node is Empty_Node. If project has already been processed,
   --  simply return its project id. Otherwise create a new project id, mark it
   --  as processed, call itself recursively for all imported projects and a
   --  extended project, if any. Then process the declarative items of the
   --  project.
   --
   --  Is_Root_Project should be true only for the project that the user
   --  explicitly loaded. In the context of aggregate projects, only that
   --  project is allowed to modify the environment that will be used to load
   --  projects (Child_Env).

   function Get_Attribute_Index
     (Tree  : Project_Node_Tree_Ref;
      Attr  : Project_Node_Id;
      Index : Name_Id) return Name_Id;
   --  Copy the index of the attribute into Name_Buffer, converting to lower
   --  case if the attribute is case-insensitive.

   ---------
   -- Add --
   ---------

   procedure Add (To_Exp : in out Name_Id; Str : Name_Id) is
   begin
      if To_Exp = No_Name or else To_Exp = Empty_String then

         --  To_Exp is nil or empty. The result is Str

         To_Exp := Str;

      --  If Str is nil, then do not change To_Ext

      elsif Str /= No_Name and then Str /= Empty_String then
         declare
            S : constant String := Get_Name_String (Str);
         begin
            Get_Name_String (To_Exp);
            Add_Str_To_Name_Buffer (S);
            To_Exp := Name_Find;
         end;
      end if;
   end Add;

   --------------------
   -- Add_Attributes --
   --------------------

   procedure Add_Attributes
     (Project       : Project_Id;
      Project_Name  : Name_Id;
      Project_Dir   : Name_Id;
      Shared        : Shared_Project_Tree_Data_Access;
      Decl          : in out Declarations;
      First         : Attribute_Node_Id;
      Project_Level : Boolean)
   is
      The_Attribute  : Attribute_Node_Id := First;

   begin
      while The_Attribute /= Empty_Attribute loop
         if Attribute_Kind_Of (The_Attribute) = Single then
            declare
               New_Attribute : Variable_Value;

            begin
               case Variable_Kind_Of (The_Attribute) is

                  --  Undefined should not happen

                  when Undefined =>
                     pragma Assert
                       (False, "attribute with an undefined kind");
                     raise Program_Error;

                  --  Single attributes have a default value of empty string

                  when Single =>
                     New_Attribute :=
                       (Project  => Project,
                        Kind     => Single,
                        Location => No_Location,
                        Default  => True,
                        Value    => Empty_String,
                        Index    => 0);

                     --  Special cases of <project>'Name and
                     --  <project>'Project_Dir.

                     if Project_Level then
                        if Attribute_Name_Of (The_Attribute) =
                          Snames.Name_Name
                        then
                           New_Attribute.Value := Project_Name;

                        elsif Attribute_Name_Of (The_Attribute) =
                          Snames.Name_Project_Dir
                        then
                           New_Attribute.Value := Project_Dir;
                        end if;
                     end if;

                  --  List attributes have a default value of nil list

                  when List =>
                     New_Attribute :=
                       (Project  => Project,
                        Kind     => List,
                        Location => No_Location,
                        Default  => True,
                        Values   => Nil_String);

               end case;

               Variable_Element_Table.Increment_Last
                 (Shared.Variable_Elements);
               Shared.Variable_Elements.Table
                 (Variable_Element_Table.Last (Shared.Variable_Elements)) :=
                 (Next  => Decl.Attributes,
                  Name  => Attribute_Name_Of (The_Attribute),
                  Value => New_Attribute);
               Decl.Attributes :=
                 Variable_Element_Table.Last
                   (Shared.Variable_Elements);
            end;
         end if;

         The_Attribute := Next_Attribute (After => The_Attribute);
      end loop;
   end Add_Attributes;

   -----------
   -- Check --
   -----------

   procedure Check
     (In_Tree   : Project_Tree_Ref;
      Project   : Project_Id;
      Node_Tree : Prj.Tree.Project_Node_Tree_Ref;
      Flags     : Processing_Flags)
   is
   begin
      Process_Naming_Scheme (In_Tree, Project, Node_Tree, Flags);

      --  Set the Other_Part field for the units

      declare
         Source1 : Source_Id;
         Name    : Name_Id;
         Source2 : Source_Id;
         Iter    : Source_Iterator;

      begin
         Unit_Htable.Reset;

         Iter := For_Each_Source (In_Tree);
         loop
            Source1 := Prj.Element (Iter);
            exit when Source1 = No_Source;

            if Source1.Unit /= No_Unit_Index then
               Name := Source1.Unit.Name;
               Source2 := Unit_Htable.Get (Name);

               if Source2 = No_Source then
                  Unit_Htable.Set (K => Name, E => Source1);
               else
                  Unit_Htable.Remove (Name);
               end if;
            end if;

            Next (Iter);
         end loop;
      end;
   end Check;

   -------------------------------
   -- Copy_Package_Declarations --
   -------------------------------

   procedure Copy_Package_Declarations
     (From       : Declarations;
      To         : in out Declarations;
      New_Loc    : Source_Ptr;
      Restricted : Boolean;
      Shared     : Shared_Project_Tree_Data_Access)
   is
      V1  : Variable_Id;
      V2  : Variable_Id      := No_Variable;
      Var : Variable;
      A1  : Array_Id;
      A2  : Array_Id         := No_Array;
      Arr : Array_Data;
      E1  : Array_Element_Id;
      E2  : Array_Element_Id := No_Array_Element;
      Elm : Array_Element;

   begin
      --  To avoid references in error messages to attribute declarations in
      --  an original package that has been renamed, copy all the attribute
      --  declarations of the package and change all locations to New_Loc,
      --  the location of the renamed package.

      --  First single attributes

      V1 := From.Attributes;
      while V1 /= No_Variable loop

         --  Copy the attribute

         Var := Shared.Variable_Elements.Table (V1);
         V1  := Var.Next;

         --  Do not copy the value of attribute Linker_Options if Restricted

         if Restricted and then Var.Name = Snames.Name_Linker_Options then
            Var.Value.Values := Nil_String;
         end if;

         --  Remove the Next component

         Var.Next := No_Variable;

         --  Change the location to New_Loc

         Var.Value.Location := New_Loc;
         Variable_Element_Table.Increment_Last (Shared.Variable_Elements);

         --  Put in new declaration

         if To.Attributes = No_Variable then
            To.Attributes :=
              Variable_Element_Table.Last (Shared.Variable_Elements);
         else
            Shared.Variable_Elements.Table (V2).Next :=
              Variable_Element_Table.Last (Shared.Variable_Elements);
         end if;

         V2 := Variable_Element_Table.Last (Shared.Variable_Elements);
         Shared.Variable_Elements.Table (V2) := Var;
      end loop;

      --  Then the associated array attributes

      A1 := From.Arrays;
      while A1 /= No_Array loop
         Arr := Shared.Arrays.Table (A1);
         A1  := Arr.Next;

         --  Remove the Next component

         Arr.Next := No_Array;
         Array_Table.Increment_Last (Shared.Arrays);

         --  Create new Array declaration

         if To.Arrays = No_Array then
            To.Arrays := Array_Table.Last (Shared.Arrays);
         else
            Shared.Arrays.Table (A2).Next :=
              Array_Table.Last (Shared.Arrays);
         end if;

         A2 := Array_Table.Last (Shared.Arrays);

         --  Don't store the array as its first element has not been set yet

         --  Copy the array elements of the array

         E1 := Arr.Value;
         Arr.Value := No_Array_Element;
         while E1 /= No_Array_Element loop

            --  Copy the array element

            Elm := Shared.Array_Elements.Table (E1);
            E1 := Elm.Next;

            --  Remove the Next component

            Elm.Next := No_Array_Element;

            Elm.Restricted := Restricted;

            --  Change the location

            Elm.Value.Location := New_Loc;
            Array_Element_Table.Increment_Last (Shared.Array_Elements);

            --  Create new array element

            if Arr.Value = No_Array_Element then
               Arr.Value := Array_Element_Table.Last (Shared.Array_Elements);
            else
               Shared.Array_Elements.Table (E2).Next :=
                 Array_Element_Table.Last (Shared.Array_Elements);
            end if;

            E2 := Array_Element_Table.Last (Shared.Array_Elements);
            Shared.Array_Elements.Table (E2) := Elm;
         end loop;

         --  Finally, store the new array

         Shared.Arrays.Table (A2) := Arr;
      end loop;
   end Copy_Package_Declarations;

   -------------------------
   -- Get_Attribute_Index --
   -------------------------

   function Get_Attribute_Index
     (Tree  : Project_Node_Tree_Ref;
      Attr  : Project_Node_Id;
      Index : Name_Id) return Name_Id
   is
   begin
      if Index = All_Other_Names
        or else not Case_Insensitive (Attr, Tree)
      then
         return Index;
      end if;

      Get_Name_String (Index);
      To_Lower (Name_Buffer (1 .. Name_Len));
      return Name_Find;
   end Get_Attribute_Index;

   ----------------
   -- Expression --
   ----------------

   function Expression
     (Project                : Project_Id;
      Shared                 : Shared_Project_Tree_Data_Access;
      From_Project_Node      : Project_Node_Id;
      From_Project_Node_Tree : Project_Node_Tree_Ref;
      Env                    : Prj.Tree.Environment;
      Pkg                    : Package_Id;
      First_Term             : Project_Node_Id;
      Kind                   : Variable_Kind) return Variable_Value
   is
      The_Term : Project_Node_Id;
      --  The term in the expression list

      The_Current_Term : Project_Node_Id := Empty_Node;
      --  The current term node id

      Result : Variable_Value (Kind => Kind);
      --  The returned result

      Last : String_List_Id := Nil_String;
      --  Reference to the last string elements in Result, when Kind is List

   begin
      Result.Project := Project;
      Result.Location := Location_Of (First_Term, From_Project_Node_Tree);

      --  Process each term of the expression, starting with First_Term

      The_Term := First_Term;
      while Present (The_Term) loop
         The_Current_Term := Current_Term (The_Term, From_Project_Node_Tree);

         case Kind_Of (The_Current_Term, From_Project_Node_Tree) is

            when N_Literal_String =>

               case Kind is

                  when Undefined =>

                     --  Should never happen

                     pragma Assert (False, "Undefined expression kind");
                     raise Program_Error;

                  when Single =>
                     Add (Result.Value,
                          String_Value_Of
                            (The_Current_Term, From_Project_Node_Tree));
                     Result.Index :=
                       Source_Index_Of
                         (The_Current_Term, From_Project_Node_Tree);

                  when List =>

                     String_Element_Table.Increment_Last
                       (Shared.String_Elements);

                     if Last = Nil_String then

                        --  This can happen in an expression like () & "toto"

                        Result.Values := String_Element_Table.Last
                          (Shared.String_Elements);

                     else
                        Shared.String_Elements.Table
                          (Last).Next := String_Element_Table.Last
                                       (Shared.String_Elements);
                     end if;

                     Last := String_Element_Table.Last
                               (Shared.String_Elements);

                     Shared.String_Elements.Table (Last) :=
                       (Value         => String_Value_Of
                                           (The_Current_Term,
                                            From_Project_Node_Tree),
                        Index         => Source_Index_Of
                                           (The_Current_Term,
                                            From_Project_Node_Tree),
                        Display_Value => No_Name,
                        Location      => Location_Of
                                           (The_Current_Term,
                                            From_Project_Node_Tree),
                        Flag          => False,
                        Next          => Nil_String);
               end case;

            when N_Literal_String_List =>

               declare
                  String_Node : Project_Node_Id :=
                                  First_Expression_In_List
                                    (The_Current_Term,
                                     From_Project_Node_Tree);

                  Value : Variable_Value;

               begin
                  if Present (String_Node) then

                     --  If String_Node is nil, it is an empty list, there is
                     --  nothing to do.

                     Value := Expression
                       (Project                => Project,
                        Shared                 => Shared,
                        From_Project_Node      => From_Project_Node,
                        From_Project_Node_Tree => From_Project_Node_Tree,
                        Env                    => Env,
                        Pkg                    => Pkg,
                        First_Term             =>
                          Tree.First_Term
                            (String_Node, From_Project_Node_Tree),
                        Kind                   => Single);
                     String_Element_Table.Increment_Last
                       (Shared.String_Elements);

                     if Result.Values = Nil_String then

                        --  This literal string list is the first term in a
                        --  string list expression

                        Result.Values :=
                          String_Element_Table.Last
                            (Shared.String_Elements);

                     else
                        Shared.String_Elements.Table (Last).Next :=
                          String_Element_Table.Last (Shared.String_Elements);
                     end if;

                     Last :=
                       String_Element_Table.Last (Shared.String_Elements);

                     Shared.String_Elements.Table (Last) :=
                       (Value    => Value.Value,
                        Display_Value => No_Name,
                        Location => Value.Location,
                        Flag     => False,
                        Next     => Nil_String,
                        Index    => Value.Index);

                     loop
                        --  Add the other element of the literal string list
                        --  one after the other.

                        String_Node :=
                          Next_Expression_In_List
                            (String_Node, From_Project_Node_Tree);

                        exit when No (String_Node);

                        Value :=
                          Expression
                            (Project                => Project,
                             Shared                 => Shared,
                             From_Project_Node      => From_Project_Node,
                             From_Project_Node_Tree => From_Project_Node_Tree,
                             Env                    => Env,
                             Pkg                    => Pkg,
                             First_Term             =>
                               Tree.First_Term
                                 (String_Node, From_Project_Node_Tree),
                             Kind                   => Single);

                        String_Element_Table.Increment_Last
                          (Shared.String_Elements);
                        Shared.String_Elements.Table (Last).Next :=
                          String_Element_Table.Last (Shared.String_Elements);
                        Last := String_Element_Table.Last
                          (Shared.String_Elements);
                        Shared.String_Elements.Table (Last) :=
                          (Value    => Value.Value,
                           Display_Value => No_Name,
                           Location => Value.Location,
                           Flag     => False,
                           Next     => Nil_String,
                           Index    => Value.Index);
                     end loop;
                  end if;
               end;

            when N_Variable_Reference | N_Attribute_Reference =>

               declare
                  The_Project     : Project_Id  := Project;
                  The_Package     : Package_Id  := Pkg;
                  The_Name        : Name_Id     := No_Name;
                  The_Variable_Id : Variable_Id := No_Variable;
                  The_Variable    : Variable_Value;
                  Term_Project    : constant Project_Node_Id :=
                                      Project_Node_Of
                                        (The_Current_Term,
                                         From_Project_Node_Tree);
                  Term_Package    : constant Project_Node_Id :=
                                      Package_Node_Of
                                        (The_Current_Term,
                                         From_Project_Node_Tree);
                  Index           : Name_Id := No_Name;

               begin
                  if Present (Term_Project)
                    and then Term_Project /= From_Project_Node
                  then
                     --  This variable or attribute comes from another project

                     The_Name :=
                       Name_Of (Term_Project, From_Project_Node_Tree);
                     The_Project := Imported_Or_Extended_Project_From
                                      (Project   => Project,
                                       With_Name => The_Name);
                  end if;

                  if Present (Term_Package) then

                     --  This is an attribute of a package

                     The_Name :=
                       Name_Of (Term_Package, From_Project_Node_Tree);

                     The_Package := The_Project.Decl.Packages;
                     while The_Package /= No_Package
                       and then Shared.Packages.Table (The_Package).Name /=
                          The_Name
                     loop
                        The_Package :=
                          Shared.Packages.Table (The_Package).Next;
                     end loop;

                     pragma Assert
                       (The_Package /= No_Package, "package not found.");

                  elsif Kind_Of (The_Current_Term, From_Project_Node_Tree) =
                                                        N_Attribute_Reference
                  then
                     The_Package := No_Package;
                  end if;

                  The_Name :=
                    Name_Of (The_Current_Term, From_Project_Node_Tree);

                  if Kind_Of (The_Current_Term, From_Project_Node_Tree) =
                                                        N_Attribute_Reference
                  then
                     Index :=
                       Associative_Array_Index_Of
                         (The_Current_Term, From_Project_Node_Tree);
                  end if;

                  --  If it is not an associative array attribute

                  if Index = No_Name then

                     --  It is not an associative array attribute

                     if The_Package /= No_Package then

                        --  First, if there is a package, look into the package

                        if Kind_Of (The_Current_Term, From_Project_Node_Tree) =
                                                        N_Variable_Reference
                        then
                           The_Variable_Id :=
                             Shared.Packages.Table
                               (The_Package).Decl.Variables;
                        else
                           The_Variable_Id :=
                             Shared.Packages.Table
                               (The_Package).Decl.Attributes;
                        end if;

                        while The_Variable_Id /= No_Variable
                          and then Shared.Variable_Elements.Table
                                     (The_Variable_Id).Name /= The_Name
                        loop
                           The_Variable_Id :=
                             Shared.Variable_Elements.Table
                               (The_Variable_Id).Next;
                        end loop;

                     end if;

                     if The_Variable_Id = No_Variable then

                        --  If we have not found it, look into the project

                        if Kind_Of (The_Current_Term, From_Project_Node_Tree) =
                             N_Variable_Reference
                        then
                           The_Variable_Id := The_Project.Decl.Variables;
                        else
                           The_Variable_Id := The_Project.Decl.Attributes;
                        end if;

                        while The_Variable_Id /= No_Variable
                          and then Shared.Variable_Elements.Table
                            (The_Variable_Id).Name /= The_Name
                        loop
                           The_Variable_Id :=
                             Shared.Variable_Elements.Table
                               (The_Variable_Id).Next;
                        end loop;

                     end if;

                     pragma Assert (The_Variable_Id /= No_Variable,
                                      "variable or attribute not found");

                     The_Variable :=
                       Shared.Variable_Elements.Table (The_Variable_Id).Value;

                  else

                     --  It is an associative array attribute

                     declare
                        The_Array   : Array_Id := No_Array;
                        The_Element : Array_Element_Id := No_Array_Element;
                        Array_Index : Name_Id := No_Name;

                     begin
                        if The_Package /= No_Package then
                           The_Array :=
                             Shared.Packages.Table (The_Package).Decl.Arrays;
                        else
                           The_Array := The_Project.Decl.Arrays;
                        end if;

                        while The_Array /= No_Array
                          and then Shared.Arrays.Table (The_Array).Name /=
                                                                    The_Name
                        loop
                           The_Array := Shared.Arrays.Table (The_Array).Next;
                        end loop;

                        if The_Array /= No_Array then
                           The_Element :=
                             Shared.Arrays.Table (The_Array).Value;
                           Array_Index :=
                             Get_Attribute_Index
                               (From_Project_Node_Tree,
                                The_Current_Term,
                                Index);

                           while The_Element /= No_Array_Element
                             and then Shared.Array_Elements.Table
                                        (The_Element).Index /= Array_Index
                           loop
                              The_Element :=
                                Shared.Array_Elements.Table (The_Element).Next;
                           end loop;

                        end if;

                        if The_Element /= No_Array_Element then
                           The_Variable :=
                             Shared.Array_Elements.Table (The_Element).Value;

                        else
                           if Expression_Kind_Of
                                (The_Current_Term, From_Project_Node_Tree) =
                                                                        List
                           then
                              The_Variable :=
                                (Project  => Project,
                                 Kind     => List,
                                 Location => No_Location,
                                 Default  => True,
                                 Values   => Nil_String);
                           else
                              The_Variable :=
                                (Project  => Project,
                                 Kind     => Single,
                                 Location => No_Location,
                                 Default  => True,
                                 Value    => Empty_String,
                                 Index    => 0);
                           end if;
                        end if;
                     end;
                  end if;

                  case Kind is

                     when Undefined =>

                        --  Should never happen

                        pragma Assert (False, "undefined expression kind");
                        null;

                     when Single =>

                        case The_Variable.Kind is

                           when Undefined =>
                              null;

                           when Single =>
                              Add (Result.Value, The_Variable.Value);

                           when List =>

                              --  Should never happen

                              pragma Assert
                                (False,
                                 "list cannot appear in single " &
                                 "string expression");
                              null;
                        end case;

                     when List =>
                        case The_Variable.Kind is

                           when Undefined =>
                              null;

                           when Single =>
                              String_Element_Table.Increment_Last
                                (Shared.String_Elements);

                              if Last = Nil_String then

                                 --  This can happen in an expression such as
                                 --  () & Var

                                 Result.Values :=
                                   String_Element_Table.Last
                                     (Shared.String_Elements);

                              else
                                 Shared.String_Elements.Table (Last).Next :=
                                     String_Element_Table.Last
                                       (Shared.String_Elements);
                              end if;

                              Last :=
                                String_Element_Table.Last
                                  (Shared.String_Elements);

                              Shared.String_Elements.Table (Last) :=
                                (Value         => The_Variable.Value,
                                 Display_Value => No_Name,
                                 Location      => Location_Of
                                                    (The_Current_Term,
                                                     From_Project_Node_Tree),
                                 Flag          => False,
                                 Next          => Nil_String,
                                 Index         => 0);

                           when List =>

                              declare
                                 The_List : String_List_Id :=
                                              The_Variable.Values;

                              begin
                                 while The_List /= Nil_String loop
                                    String_Element_Table.Increment_Last
                                      (Shared.String_Elements);

                                    if Last = Nil_String then
                                       Result.Values :=
                                         String_Element_Table.Last
                                           (Shared.String_Elements);

                                    else
                                       Shared.
                                         String_Elements.Table (Last).Next :=
                                         String_Element_Table.Last
                                           (Shared.String_Elements);

                                    end if;

                                    Last :=
                                      String_Element_Table.Last
                                        (Shared.String_Elements);

                                    Shared.String_Elements.Table
                                      (Last) :=
                                      (Value         =>
                                         Shared.String_Elements.Table
                                           (The_List).Value,
                                       Display_Value => No_Name,
                                       Location      =>
                                         Location_Of
                                           (The_Current_Term,
                                            From_Project_Node_Tree),
                                       Flag         => False,
                                       Next         => Nil_String,
                                       Index        => 0);

                                    The_List := Shared.String_Elements.Table
                                        (The_List).Next;
                                 end loop;
                              end;
                        end case;
                  end case;
               end;

            when N_External_Value =>
               Get_Name_String
                 (String_Value_Of
                    (External_Reference_Of
                       (The_Current_Term, From_Project_Node_Tree),
                     From_Project_Node_Tree));

               declare
                  Name     : constant Name_Id   := Name_Find;
                  Default  : Name_Id            := No_Name;
                  Value    : Name_Id            := No_Name;
                  Ext_List : Boolean            := False;
                  Str_List : String_List_Access := null;
                  Def_Var  : Variable_Value;

                  Default_Node : constant Project_Node_Id :=
                                   External_Default_Of
                                     (The_Current_Term,
                                      From_Project_Node_Tree);

               begin
                  --  If there is a default value for the external reference,
                  --  get its value.

                  if Present (Default_Node) then
                     Def_Var := Expression
                       (Project                => Project,
                        Shared                 => Shared,
                        From_Project_Node      => From_Project_Node,
                        From_Project_Node_Tree => From_Project_Node_Tree,
                        Env                    => Env,
                        Pkg                    => Pkg,
                        First_Term             =>
                          Tree.First_Term
                            (Default_Node, From_Project_Node_Tree),
                        Kind                   => Single);

                     if Def_Var /= Nil_Variable_Value then
                        Default := Def_Var.Value;
                     end if;
                  end if;

                  Ext_List := Expression_Kind_Of
                                (The_Current_Term,
                                 From_Project_Node_Tree) = List;

                  if Ext_List then
                     Value := Prj.Ext.Value_Of (Env.External, Name, No_Name);

                     if Value /= No_Name then
                        declare
                           Sep   : constant String :=
                                     Get_Name_String (Default);
                           First : Positive := 1;
                           Lst   : Natural;
                           Done  : Boolean := False;
                           Nmb   : Natural;

                        begin
                           Get_Name_String (Value);

                           if Name_Len = 0
                             or else Sep'Length = 0
                             or else Name_Buffer (1 .. Name_Len) = Sep
                           then
                              Done := True;
                           end if;

                           if not Done and then Name_Len < Sep'Length then
                              Str_List :=
                                new String_List'
                                  (1 => new String'
                                       (Name_Buffer (1 .. Name_Len)));
                              Done := True;
                           end if;

                           if not Done then
                              if Name_Buffer (1 .. Sep'Length) = Sep then
                                 First := Sep'Length + 1;
                              end if;

                              if Name_Len - First + 1 >= Sep'Length
                                and then
                                  Name_Buffer (Name_Len - Sep'Length + 1 ..
                                                   Name_Len) = Sep
                              then
                                 Name_Len := Name_Len - Sep'Length;
                              end if;

                              if Name_Len = 0 then
                                 Str_List :=
                                   new String_List'(1 => new String'(""));
                                 Done := True;
                              end if;
                           end if;

                           if not Done then

                              --  Count the number of strings

                              declare
                                 Saved : constant Positive := First;

                              begin
                                 Nmb := 1;
                                 loop
                                    Lst :=
                                      Index
                                        (Source  =>
                                             Name_Buffer (First .. Name_Len),
                                         Pattern => Sep);
                                    exit when Lst = 0;
                                    Nmb := Nmb + 1;
                                    First := Lst + Sep'Length;
                                 end loop;

                                 First := Saved;
                              end;

                              Str_List := new String_List (1 .. Nmb);

                              --  Populate the string list

                              Nmb := 1;
                              loop
                                 Lst :=
                                   Index
                                     (Source  =>
                                          Name_Buffer (First .. Name_Len),
                                      Pattern => Sep);

                                 if Lst = 0 then
                                    Str_List (Nmb) :=
                                      new String'
                                        (Name_Buffer (First .. Name_Len));
                                    exit;

                                 else
                                    Str_List (Nmb) :=
                                      new String'
                                        (Name_Buffer (First .. Lst - 1));
                                    Nmb := Nmb + 1;
                                    First := Lst + Sep'Length;
                                 end if;
                              end loop;
                           end if;
                        end;
                     end if;

                  else
                     --  Get the value

                     Value := Prj.Ext.Value_Of (Env.External, Name, Default);

                     if Value = No_Name then
                        if not Quiet_Output then
                           Error_Msg
                             (Env.Flags, "?undefined external reference",
                              Location_Of
                                (The_Current_Term, From_Project_Node_Tree),
                              Project);
                        end if;

                        Value := Empty_String;
                     end if;
                  end if;

                  case Kind is

                     when Undefined =>
                        null;

                     when Single =>
                        if Ext_List then
                           null; -- error

                        else
                           Add (Result.Value, Value);
                        end if;

                     when List =>
                        if not Ext_List or else Str_List /= null then
                           String_Element_Table.Increment_Last
                             (Shared.String_Elements);

                           if Last = Nil_String then
                              Result.Values :=
                                String_Element_Table.Last
                                  (Shared.String_Elements);

                           else
                              Shared.String_Elements.Table (Last).Next
                                := String_Element_Table.Last
                                  (Shared.String_Elements);
                           end if;

                           Last := String_Element_Table.Last
                             (Shared.String_Elements);

                           if Ext_List then
                              for Ind in Str_List'Range loop
                                 Name_Len := 0;
                                 Add_Str_To_Name_Buffer (Str_List (Ind).all);
                                 Value := Name_Find;
                                 Shared.String_Elements.Table (Last) :=
                                   (Value         => Value,
                                    Display_Value => No_Name,
                                    Location      =>
                                      Location_Of
                                        (The_Current_Term,
                                         From_Project_Node_Tree),
                                    Flag          => False,
                                    Next          => Nil_String,
                                    Index         => 0);

                                 if Ind /= Str_List'Last then
                                    String_Element_Table.Increment_Last
                                      (Shared.String_Elements);
                                    Shared.String_Elements.Table (Last).Next :=
                                        String_Element_Table.Last
                                          (Shared.String_Elements);
                                    Last := String_Element_Table.Last
                                        (Shared.String_Elements);
                                 end if;
                              end loop;

                           else
                              Shared.String_Elements.Table (Last) :=
                                (Value         => Value,
                                 Display_Value => No_Name,
                                 Location      =>
                                   Location_Of
                                     (The_Current_Term,
                                      From_Project_Node_Tree),
                                 Flag          => False,
                                 Next          => Nil_String,
                                 Index         => 0);
                           end if;
                        end if;
                  end case;
               end;

            when others =>

               --  Should never happen

               pragma Assert
                 (False,
                  "illegal node kind in an expression");
               raise Program_Error;

         end case;

         The_Term := Next_Term (The_Term, From_Project_Node_Tree);
      end loop;

      return Result;
   end Expression;

   ---------------------------------------
   -- Imported_Or_Extended_Project_From --
   ---------------------------------------

   function Imported_Or_Extended_Project_From
     (Project   : Project_Id;
      With_Name : Name_Id) return Project_Id
   is
      List        : Project_List;
      Result      : Project_Id;
      Temp_Result : Project_Id;

   begin
      --  First check if it is the name of an extended project

      Result := Project.Extends;
      while Result /= No_Project loop
         if Result.Name = With_Name then
            return Result;
         else
            Result := Result.Extends;
         end if;
      end loop;

      --  Then check the name of each imported project

      Temp_Result := No_Project;
      List := Project.Imported_Projects;
      while List /= null loop
         Result := List.Project;

         --  If the project is directly imported, then returns its ID

         if Result.Name = With_Name then
            return Result;
         end if;

         --  If a project extending the project is imported, then keep this
         --  extending project as a possibility. It will be the returned ID
         --  if the project is not imported directly.

         declare
            Proj : Project_Id;

         begin
            Proj := Result.Extends;
            while Proj /= No_Project loop
               if Proj.Name = With_Name then
                  Temp_Result := Result;
                  exit;
               end if;

               Proj := Proj.Extends;
            end loop;
         end;

         List := List.Next;
      end loop;

      pragma Assert (Temp_Result /= No_Project, "project not found");
      return Temp_Result;
   end Imported_Or_Extended_Project_From;

   ------------------
   -- Package_From --
   ------------------

   function Package_From
     (Project   : Project_Id;
      Shared    : Shared_Project_Tree_Data_Access;
      With_Name : Name_Id) return Package_Id
   is
      Result : Package_Id := Project.Decl.Packages;

   begin
      --  Check the name of each existing package of Project

      while Result /= No_Package
        and then Shared.Packages.Table (Result).Name /= With_Name
      loop
         Result := Shared.Packages.Table (Result).Next;
      end loop;

      if Result = No_Package then

         --  Should never happen

         Write_Line
           ("package """ & Get_Name_String (With_Name) & """ not found");
         raise Program_Error;

      else
         return Result;
      end if;
   end Package_From;

   -------------
   -- Process --
   -------------

   procedure Process
     (In_Tree                : Project_Tree_Ref;
      Project                : out Project_Id;
      Packages_To_Check      : String_List_Access;
      Success                : out Boolean;
      From_Project_Node      : Project_Node_Id;
      From_Project_Node_Tree : Project_Node_Tree_Ref;
      Env                    : in out Prj.Tree.Environment;
      Reset_Tree             : Boolean := True)
   is
   begin
      Process_Project_Tree_Phase_1
        (In_Tree                => In_Tree,
         Project                => Project,
         Success                => Success,
         From_Project_Node      => From_Project_Node,
         From_Project_Node_Tree => From_Project_Node_Tree,
         Env                    => Env,
         Packages_To_Check      => Packages_To_Check,
         Reset_Tree             => Reset_Tree);

      if Project_Qualifier_Of
           (From_Project_Node, From_Project_Node_Tree) /= Configuration
      then
         Process_Project_Tree_Phase_2
           (In_Tree                => In_Tree,
            Project                => Project,
            Success                => Success,
            From_Project_Node      => From_Project_Node,
            From_Project_Node_Tree => From_Project_Node_Tree,
            Env                    => Env);
      end if;
   end Process;

   -------------------------------
   -- Process_Declarative_Items --
   -------------------------------

   procedure Process_Declarative_Items
     (Project           : Project_Id;
      In_Tree           : Project_Tree_Ref;
      From_Project_Node : Project_Node_Id;
      Node_Tree         : Project_Node_Tree_Ref;
      Env               : Prj.Tree.Environment;
      Pkg               : Package_Id;
      Item              : Project_Node_Id;
      Child_Env         : in out Prj.Tree.Environment)
   is
      Shared : constant Shared_Project_Tree_Data_Access := In_Tree.Shared;

      procedure Check_Or_Set_Typed_Variable
        (Value       : in out Variable_Value;
         Declaration : Project_Node_Id);
      --  Check whether Value is valid for this typed variable declaration. If
      --  it is an error, the behavior depends on the flags: either an error is
      --  reported, or a warning, or nothing. In the last two cases, the value
      --  of the variable is set to a valid value, replacing Value.

      procedure Process_Package_Declaration
        (Current_Item : Project_Node_Id);
      procedure Process_Attribute_Declaration
        (Current : Project_Node_Id);
      procedure Process_Case_Construction
        (Current_Item : Project_Node_Id);
      procedure Process_Associative_Array
        (Current_Item : Project_Node_Id);
      procedure Process_Expression
        (Current : Project_Node_Id);
      procedure Process_Expression_For_Associative_Array
        (Current : Project_Node_Id;
         New_Value    : Variable_Value);
      procedure Process_Expression_Variable_Decl
        (Current_Item : Project_Node_Id;
         New_Value    : Variable_Value);
      --  Process the various declarative items

      ---------------------------------
      -- Check_Or_Set_Typed_Variable --
      ---------------------------------

      procedure Check_Or_Set_Typed_Variable
        (Value       : in out Variable_Value;
         Declaration : Project_Node_Id)
      is
         Loc : constant Source_Ptr := Location_Of (Declaration, Node_Tree);

         Reset_Value    : Boolean := False;
         Current_String : Project_Node_Id;

      begin
         --  Report an error for an empty string

         if Value.Value = Empty_String then
            Error_Msg_Name_1 := Name_Of (Declaration, Node_Tree);

            case Env.Flags.Allow_Invalid_External is
               when Error =>
                  Error_Msg
                    (Env.Flags, "no value defined for %%", Loc, Project);
               when Warning =>
                  Reset_Value := True;
                  Error_Msg
                    (Env.Flags, "?no value defined for %%", Loc, Project);
               when Silent =>
                  Reset_Value := True;
            end case;

         else
            --  Loop through all the valid strings for the
            --  string type and compare to the string value.

            Current_String :=
              First_Literal_String
                (String_Type_Of (Declaration, Node_Tree), Node_Tree);

            while Present (Current_String)
              and then
                String_Value_Of (Current_String, Node_Tree) /= Value.Value
            loop
               Current_String :=
                 Next_Literal_String (Current_String, Node_Tree);
            end loop;

            --  Report error if string value is not one for the string type

            if No (Current_String) then
               Error_Msg_Name_1 := Value.Value;
               Error_Msg_Name_2 := Name_Of (Declaration, Node_Tree);

               case Env.Flags.Allow_Invalid_External is
                  when Error =>
                     Error_Msg
                       (Env.Flags, "value %% is illegal for typed string %%",
                        Loc, Project);

                  when Warning =>
                     Error_Msg
                       (Env.Flags, "?value %% is illegal for typed string %%",
                        Loc, Project);
                     Reset_Value := True;

                  when Silent =>
                     Reset_Value := True;
               end case;
            end if;
         end if;

         if Reset_Value then
            Current_String :=
              First_Literal_String
                (String_Type_Of (Declaration, Node_Tree), Node_Tree);
            Value.Value := String_Value_Of (Current_String, Node_Tree);
         end if;
      end Check_Or_Set_Typed_Variable;

      ---------------------------------
      -- Process_Package_Declaration --
      ---------------------------------

      procedure Process_Package_Declaration
        (Current_Item : Project_Node_Id)
      is
      begin
         --  Do not process a package declaration that should be ignored

         if Expression_Kind_Of (Current_Item, Node_Tree) /= Ignored then

            --  Create the new package

            Package_Table.Increment_Last (Shared.Packages);

            declare
               New_Pkg         : constant Package_Id :=
                                  Package_Table.Last (Shared.Packages);
               The_New_Package : Package_Element;

               Project_Of_Renamed_Package : constant Project_Node_Id :=
                                              Project_Of_Renamed_Package_Of
                                                (Current_Item, Node_Tree);

            begin
               --  Set the name of the new package

               The_New_Package.Name := Name_Of (Current_Item, Node_Tree);

               --  Insert the new package in the appropriate list

               if Pkg /= No_Package then
                  The_New_Package.Next :=
                    Shared.Packages.Table (Pkg).Decl.Packages;
                  Shared.Packages.Table (Pkg).Decl.Packages := New_Pkg;

               else
                  The_New_Package.Next  := Project.Decl.Packages;
                  Project.Decl.Packages := New_Pkg;
               end if;

               Shared.Packages.Table (New_Pkg) := The_New_Package;

               if Present (Project_Of_Renamed_Package) then

                  --  Renamed or extending package

                  declare
                     Project_Name : constant Name_Id :=
                                      Name_Of (Project_Of_Renamed_Package,
                                               Node_Tree);

                     Renamed_Project : constant Project_Id :=
                                         Imported_Or_Extended_Project_From
                                           (Project, Project_Name);

                     Renamed_Package : constant Package_Id :=
                                         Package_From
                                           (Renamed_Project, Shared,
                                            Name_Of (Current_Item, Node_Tree));

                  begin
                     --  For a renamed package, copy the declarations of the
                     --  renamed package, but set all the locations to the
                     --  location of the package name in the renaming
                     --  declaration.

                     Copy_Package_Declarations
                       (From       => Shared.Packages.Table
                                        (Renamed_Package).Decl,
                        To         => Shared.Packages.Table (New_Pkg).Decl,
                        New_Loc    => Location_Of (Current_Item, Node_Tree),
                        Restricted => False,
                        Shared     => Shared);
                  end;

               else
                  --  Set the default values of the attributes

                  Add_Attributes
                    (Project,
                     Project.Name,
                     Name_Id (Project.Directory.Name),
                     Shared,
                     Shared.Packages.Table (New_Pkg).Decl,
                     First_Attribute_Of
                       (Package_Id_Of (Current_Item, Node_Tree)),
                     Project_Level => False);
               end if;

               --  Process declarative items (nothing to do when the package is
               --  renaming, as the first declarative item is null).

               Process_Declarative_Items
                 (Project                => Project,
                  In_Tree                => In_Tree,
                  From_Project_Node      => From_Project_Node,
                  Node_Tree              => Node_Tree,
                  Env                    => Env,
                  Pkg                    => New_Pkg,
                  Item                   =>
                    First_Declarative_Item_Of (Current_Item, Node_Tree),
                  Child_Env              => Child_Env);
            end;
         end if;
      end Process_Package_Declaration;

      -------------------------------
      -- Process_Associative_Array --
      -------------------------------

      procedure Process_Associative_Array
        (Current_Item : Project_Node_Id)
      is
         Current_Item_Name : constant Name_Id :=
                               Name_Of (Current_Item, Node_Tree);
         --  The name of the attribute

         Current_Location  : constant Source_Ptr :=
                               Location_Of (Current_Item, Node_Tree);

         New_Array : Array_Id;
         --  The new associative array created

         Orig_Array : Array_Id;
         --  The associative array value

         Orig_Project_Name : Name_Id := No_Name;
         --  The name of the project where the associative array
         --  value is.

         Orig_Project : Project_Id := No_Project;
         --  The id of the project where the associative array
         --  value is.

         Orig_Package_Name : Name_Id := No_Name;
         --  The name of the package, if any, where the associative array value
         --  is located.

         Orig_Package : Package_Id := No_Package;
         --  The id of the package, if any, where the associative array value
         --  is located.

         New_Element : Array_Element_Id := No_Array_Element;
         --  Id of a new array element created

         Prev_Element : Array_Element_Id := No_Array_Element;
         --  Last new element id created

         Orig_Element : Array_Element_Id := No_Array_Element;
         --  Current array element in original associative array

         Next_Element : Array_Element_Id := No_Array_Element;
         --  Id of the array element that follows the new element. This is not
         --  always nil, because values for the associative array attribute may
         --  already have been declared, and the array elements declared are
         --  reused.

         Prj : Project_List;

      begin
         --  First find if the associative array attribute already has elements
         --  declared.

         if Pkg /= No_Package then
            New_Array := Shared.Packages.Table (Pkg).Decl.Arrays;
         else
            New_Array := Project.Decl.Arrays;
         end if;

         while New_Array /= No_Array
           and then Shared.Arrays.Table (New_Array).Name /= Current_Item_Name
         loop
            New_Array := Shared.Arrays.Table (New_Array).Next;
         end loop;

         --  If the attribute has never been declared add new entry in the
         --  arrays of the project/package and link it.

         if New_Array = No_Array then
            Array_Table.Increment_Last (Shared.Arrays);
            New_Array := Array_Table.Last (Shared.Arrays);

            if Pkg /= No_Package then
               Shared.Arrays.Table (New_Array) :=
                 (Name     => Current_Item_Name,
                  Location => Current_Location,
                  Value    => No_Array_Element,
                  Next     => Shared.Packages.Table (Pkg).Decl.Arrays);

               Shared.Packages.Table (Pkg).Decl.Arrays := New_Array;

            else
               Shared.Arrays.Table (New_Array) :=
                 (Name     => Current_Item_Name,
                  Location => Current_Location,
                  Value    => No_Array_Element,
                  Next     => Project.Decl.Arrays);

               Project.Decl.Arrays := New_Array;
            end if;
         end if;

         --  Find the project where the value is declared

         Orig_Project_Name :=
           Name_Of
             (Associative_Project_Of (Current_Item, Node_Tree), Node_Tree);

         Prj := In_Tree.Projects;
         while Prj /= null loop
            if Prj.Project.Name = Orig_Project_Name then
               Orig_Project := Prj.Project;
               exit;
            end if;
            Prj := Prj.Next;
         end loop;

         pragma Assert (Orig_Project /= No_Project,
                        "original project not found");

         if No (Associative_Package_Of (Current_Item, Node_Tree)) then
            Orig_Array := Orig_Project.Decl.Arrays;

         else
            --  If in a package, find the package where the value is declared

            Orig_Package_Name :=
              Name_Of
                (Associative_Package_Of (Current_Item, Node_Tree), Node_Tree);

            Orig_Package := Orig_Project.Decl.Packages;
            pragma Assert (Orig_Package /= No_Package,
                           "original package not found");

            while Shared.Packages.Table
              (Orig_Package).Name /= Orig_Package_Name
            loop
               Orig_Package := Shared.Packages.Table (Orig_Package).Next;
               pragma Assert (Orig_Package /= No_Package,
                              "original package not found");
            end loop;

            Orig_Array := Shared.Packages.Table (Orig_Package).Decl.Arrays;
         end if;

         --  Now look for the array

         while Orig_Array /= No_Array
           and then Shared.Arrays.Table (Orig_Array).Name /= Current_Item_Name
         loop
            Orig_Array := Shared.Arrays.Table (Orig_Array).Next;
         end loop;

         if Orig_Array = No_Array then
            Error_Msg
              (Env.Flags,
               "associative array value not found",
               Location_Of (Current_Item, Node_Tree),
               Project);

         else
            Orig_Element := Shared.Arrays.Table (Orig_Array).Value;

            --  Copy each array element

            while Orig_Element /= No_Array_Element loop

               --  Case of first element

               if Prev_Element = No_Array_Element then

                  --  And there is no array element declared yet, create a new
                  --  first array element.

                  if Shared.Arrays.Table (New_Array).Value =
                    No_Array_Element
                  then
                     Array_Element_Table.Increment_Last
                       (Shared.Array_Elements);
                     New_Element := Array_Element_Table.Last
                       (Shared.Array_Elements);
                     Shared.Arrays.Table (New_Array).Value := New_Element;
                     Next_Element := No_Array_Element;

                     --  Otherwise, the new element is the first

                  else
                     New_Element := Shared.Arrays.Table (New_Array).Value;
                     Next_Element :=
                       Shared.Array_Elements.Table (New_Element).Next;
                  end if;

                  --  Otherwise, reuse an existing element, or create
                  --  one if necessary.

               else
                  Next_Element :=
                    Shared.Array_Elements.Table (Prev_Element).Next;

                  if Next_Element = No_Array_Element then
                     Array_Element_Table.Increment_Last
                       (Shared.Array_Elements);
                     New_Element := Array_Element_Table.Last
                       (Shared.Array_Elements);
                     Shared.Array_Elements.Table (Prev_Element).Next :=
                       New_Element;

                  else
                     New_Element := Next_Element;
                     Next_Element :=
                       Shared.Array_Elements.Table (New_Element).Next;
                  end if;
               end if;

               --  Copy the value of the element

               Shared.Array_Elements.Table (New_Element) :=
                 Shared.Array_Elements.Table (Orig_Element);
               Shared.Array_Elements.Table (New_Element).Value.Project
                 := Project;

               --  Adjust the Next link

               Shared.Array_Elements.Table (New_Element).Next := Next_Element;

               --  Adjust the previous id for the next element

               Prev_Element := New_Element;

               --  Go to the next element in the original array

               Orig_Element := Shared.Array_Elements.Table (Orig_Element).Next;
            end loop;

            --  Make sure that the array ends here, in case there previously a
            --  greater number of elements.

            Shared.Array_Elements.Table (New_Element).Next := No_Array_Element;
         end if;
      end Process_Associative_Array;

      ----------------------------------------------
      -- Process_Expression_For_Associative_Array --
      ----------------------------------------------

      procedure Process_Expression_For_Associative_Array
        (Current   : Project_Node_Id;
         New_Value : Variable_Value)
      is
         Name             : constant Name_Id := Name_Of (Current, Node_Tree);
         Current_Location : constant Source_Ptr :=
                              Location_Of (Current, Node_Tree);

         Index_Name : Name_Id :=
                        Associative_Array_Index_Of (Current, Node_Tree);

         Source_Index : constant Int :=
                          Source_Index_Of (Current, Node_Tree);

         The_Array : Array_Id;
         Elem      : Array_Element_Id := No_Array_Element;

      begin
         if Index_Name /= All_Other_Names then
            Index_Name := Get_Attribute_Index (Node_Tree, Current, Index_Name);
         end if;

         --  Look for the array in the appropriate list

         if Pkg /= No_Package then
            The_Array := Shared.Packages.Table (Pkg).Decl.Arrays;
         else
            The_Array := Project.Decl.Arrays;
         end if;

         while The_Array /= No_Array
           and then Shared.Arrays.Table (The_Array).Name /= Name
         loop
            The_Array := Shared.Arrays.Table (The_Array).Next;
         end loop;

         --  If the array cannot be found, create a new entry in the list.
         --  As The_Array_Element is initialized to No_Array_Element, a new
         --  element will be created automatically later

         if The_Array = No_Array then
            Array_Table.Increment_Last (Shared.Arrays);
            The_Array := Array_Table.Last (Shared.Arrays);

            if Pkg /= No_Package then
               Shared.Arrays.Table (The_Array) :=
                 (Name     => Name,
                  Location => Current_Location,
                  Value    => No_Array_Element,
                  Next     => Shared.Packages.Table (Pkg).Decl.Arrays);

               Shared.Packages.Table (Pkg).Decl.Arrays := The_Array;

            else
               Shared.Arrays.Table (The_Array) :=
                 (Name     => Name,
                  Location => Current_Location,
                  Value    => No_Array_Element,
                  Next     => Project.Decl.Arrays);

               Project.Decl.Arrays := The_Array;
            end if;

         else
            Elem := Shared.Arrays.Table (The_Array).Value;
         end if;

         --  Look in the list, if any, to find an element with the same index
         --  and same source index.

         while Elem /= No_Array_Element
           and then
             (Shared.Array_Elements.Table (Elem).Index /= Index_Name
               or else
                 Shared.Array_Elements.Table (Elem).Src_Index /= Source_Index)
         loop
            Elem := Shared.Array_Elements.Table (Elem).Next;
         end loop;

         --  If no such element were found, create a new one
         --  and insert it in the element list, with the
         --  proper value.

         if Elem = No_Array_Element then
            Array_Element_Table.Increment_Last (Shared.Array_Elements);
            Elem := Array_Element_Table.Last (Shared.Array_Elements);

            Shared.Array_Elements.Table
              (Elem) :=
              (Index                => Index_Name,
               Restricted           => False,
               Src_Index            => Source_Index,
               Index_Case_Sensitive =>
                  not Case_Insensitive (Current, Node_Tree),
               Value                => New_Value,
               Next                 => Shared.Arrays.Table (The_Array).Value);

            Shared.Arrays.Table (The_Array).Value := Elem;

         else
            --  An element with the same index already exists, just replace its
            --  value with the new one.

            Shared.Array_Elements.Table (Elem).Value := New_Value;
         end if;

         if Name = Snames.Name_External then
            if In_Tree.Is_Root_Tree then
               Add (Child_Env.External,
                    External_Name => Get_Name_String (Index_Name),
                    Value         => Get_Name_String (New_Value.Value),
                    Source        => From_External_Attribute);
               Add (Env.External,
                    External_Name => Get_Name_String (Index_Name),
                    Value         => Get_Name_String (New_Value.Value),
                    Source        => From_External_Attribute);
            else
               if Current_Verbosity = High then
                  Debug_Output
                    ("'for External' has no effect except in root aggregate ("
                     & Get_Name_String (Index_Name) & ")", New_Value.Value);
               end if;
            end if;
         end if;
      end Process_Expression_For_Associative_Array;

      --------------------------------------
      -- Process_Expression_Variable_Decl --
      --------------------------------------

      procedure Process_Expression_Variable_Decl
        (Current_Item : Project_Node_Id;
         New_Value    : Variable_Value)
      is
         Name : constant Name_Id := Name_Of (Current_Item, Node_Tree);

         Is_Attribute : constant Boolean :=
                          Kind_Of (Current_Item, Node_Tree) =
                            N_Attribute_Declaration;

         Var  : Variable_Id := No_Variable;

      begin
         --  First, find the list where to find the variable or attribute

         if Is_Attribute then
            if Pkg /= No_Package then
               Var := Shared.Packages.Table (Pkg).Decl.Attributes;
            else
               Var := Project.Decl.Attributes;
            end if;

         else
            if Pkg /= No_Package then
               Var := Shared.Packages.Table (Pkg).Decl.Variables;
            else
               Var := Project.Decl.Variables;
            end if;
         end if;

         --  Loop through the list, to find if it has already been declared

         while Var /= No_Variable
           and then Shared.Variable_Elements.Table (Var).Name /= Name
         loop
            Var := Shared.Variable_Elements.Table (Var).Next;
         end loop;

         --  If it has not been declared, create a new entry in the list

         if Var = No_Variable then

            --  All single string attribute should already have been declared
            --  with a default empty string value.

            pragma Assert
              (not Is_Attribute,
               "illegal attribute declaration for " & Get_Name_String (Name));

            Variable_Element_Table.Increment_Last (Shared.Variable_Elements);
            Var := Variable_Element_Table.Last (Shared.Variable_Elements);

            --  Put the new variable in the appropriate list

            if Pkg /= No_Package then
               Shared.Variable_Elements.Table (Var) :=
                 (Next   => Shared.Packages.Table (Pkg).Decl.Variables,
                  Name   => Name,
                  Value  => New_Value);
               Shared.Packages.Table (Pkg).Decl.Variables := Var;

            else
               Shared.Variable_Elements.Table (Var) :=
                 (Next   => Project.Decl.Variables,
                  Name   => Name,
                  Value  => New_Value);
               Project.Decl.Variables := Var;
            end if;

            --  If the variable/attribute has already been declared, just
            --  change the value.

         else
            Shared.Variable_Elements.Table (Var).Value := New_Value;
         end if;

         if Is_Attribute and then Name = Snames.Name_Project_Path then
            if In_Tree.Is_Root_Tree then
               declare
                  Val : String_List_Id := New_Value.Values;
               begin
                  while Val /= Nil_String loop
                     Prj.Env.Add_Directories
                       (Child_Env.Project_Path,
                        Get_Name_String
                          (Shared.String_Elements.Table (Val).Value));
                     Val := Shared.String_Elements.Table (Val).Next;
                  end loop;
               end;

            else
               if Current_Verbosity = High then
                  Debug_Output
                    ("'for Project_Path' has no effect except in"
                     & " root aggregate");
               end if;
            end if;
         end if;
      end Process_Expression_Variable_Decl;

      ------------------------
      -- Process_Expression --
      ------------------------

      procedure Process_Expression (Current : Project_Node_Id) is
         New_Value : Variable_Value :=
                       Expression
                         (Project                => Project,
                          Shared                 => Shared,
                          From_Project_Node      => From_Project_Node,
                          From_Project_Node_Tree => Node_Tree,
                          Env                    => Env,
                          Pkg                    => Pkg,
                          First_Term             =>
                            Tree.First_Term
                              (Expression_Of (Current, Node_Tree), Node_Tree),
                          Kind                 =>
                            Expression_Kind_Of (Current, Node_Tree));

      begin
         --  Process a typed variable declaration

         if Kind_Of (Current, Node_Tree) = N_Typed_Variable_Declaration then
            Check_Or_Set_Typed_Variable (New_Value, Current);
         end if;

         if Kind_Of (Current, Node_Tree) /= N_Attribute_Declaration
           or else Associative_Array_Index_Of (Current, Node_Tree) = No_Name
         then
            Process_Expression_Variable_Decl (Current, New_Value);
         else
            Process_Expression_For_Associative_Array (Current, New_Value);
         end if;
      end Process_Expression;

      -----------------------------------
      -- Process_Attribute_Declaration --
      -----------------------------------

      procedure Process_Attribute_Declaration (Current : Project_Node_Id) is
      begin
         if Expression_Of (Current, Node_Tree) = Empty_Node then
            Process_Associative_Array (Current);
         else
            Process_Expression (Current);
         end if;
      end Process_Attribute_Declaration;

      -------------------------------
      -- Process_Case_Construction --
      -------------------------------

      procedure Process_Case_Construction
        (Current_Item : Project_Node_Id)
      is
         The_Project : Project_Id := Project;
         --  The id of the project of the case variable

         The_Package : Package_Id := Pkg;
         --  The id of the package, if any, of the case variable

         The_Variable : Variable_Value := Nil_Variable_Value;
         --  The case variable

         Case_Value : Name_Id := No_Name;
         --  The case variable value

         Case_Item     : Project_Node_Id := Empty_Node;
         Choice_String : Project_Node_Id := Empty_Node;
         Decl_Item     : Project_Node_Id := Empty_Node;

      begin
         declare
            Variable_Node : constant Project_Node_Id :=
              Case_Variable_Reference_Of
                (Current_Item,
                 Node_Tree);

            Var_Id : Variable_Id := No_Variable;
            Name   : Name_Id     := No_Name;

         begin
            --  If a project was specified for the case variable, get its id

            if Present (Project_Node_Of (Variable_Node, Node_Tree)) then
               Name :=
                 Name_Of
                   (Project_Node_Of (Variable_Node, Node_Tree), Node_Tree);
               The_Project :=
                 Imported_Or_Extended_Project_From (Project, Name);
            end if;

            --  If a package was specified for the case variable, get its id

            if Present (Package_Node_Of (Variable_Node, Node_Tree)) then
               Name :=
                 Name_Of
                   (Package_Node_Of (Variable_Node, Node_Tree), Node_Tree);
               The_Package := Package_From (The_Project, Shared, Name);
            end if;

            Name := Name_Of (Variable_Node, Node_Tree);

            --  First, look for the case variable into the package, if any

            if The_Package /= No_Package then
               Name := Name_Of (Variable_Node, Node_Tree);

               Var_Id := Shared.Packages.Table (The_Package).Decl.Variables;
               while Var_Id /= No_Variable
                 and then Shared.Variable_Elements.Table (Var_Id).Name /= Name
               loop
                  Var_Id := Shared.Variable_Elements.Table (Var_Id).Next;
               end loop;
            end if;

            --  If not found in the package, or if there is no package, look at
            --  the project level.

            if Var_Id = No_Variable
              and then No (Package_Node_Of (Variable_Node, Node_Tree))
            then
               Var_Id := The_Project.Decl.Variables;
               while Var_Id /= No_Variable
                 and then Shared.Variable_Elements.Table (Var_Id).Name /= Name
               loop
                  Var_Id := Shared.Variable_Elements.Table (Var_Id).Next;
               end loop;
            end if;

            if Var_Id = No_Variable then

               --  Should never happen, because this has already been checked
               --  during parsing.

               Write_Line
                 ("variable """ & Get_Name_String (Name) & """ not found");
               raise Program_Error;
            end if;

            --  Get the case variable

            The_Variable := Shared.Variable_Elements. Table (Var_Id).Value;

            if The_Variable.Kind /= Single then

               --  Should never happen, because this has already been checked
               --  during parsing.

               Write_Line ("variable""" & Get_Name_String (Name) &
                           """ is not a single string variable");
               raise Program_Error;
            end if;

            --  Get the case variable value

            Case_Value := The_Variable.Value;
         end;

         --  Now look into all the case items of the case construction

         Case_Item := First_Case_Item_Of (Current_Item, Node_Tree);

         Case_Item_Loop :
         while Present (Case_Item) loop
            Choice_String := First_Choice_Of (Case_Item, Node_Tree);

            --  When Choice_String is nil, it means that it is the
            --  "when others =>" alternative.

            if No (Choice_String) then
               Decl_Item := First_Declarative_Item_Of (Case_Item, Node_Tree);
               exit Case_Item_Loop;
            end if;

            --  Look into all the alternative of this case item

            Choice_Loop :
            while Present (Choice_String) loop
               if Case_Value = String_Value_Of (Choice_String, Node_Tree) then
                  Decl_Item :=
                    First_Declarative_Item_Of (Case_Item, Node_Tree);
                  exit Case_Item_Loop;
               end if;

               Choice_String := Next_Literal_String (Choice_String, Node_Tree);
            end loop Choice_Loop;

            Case_Item := Next_Case_Item (Case_Item, Node_Tree);
         end loop Case_Item_Loop;

         --  If there is an alternative, then we process it

         if Present (Decl_Item) then
            Process_Declarative_Items
              (Project                => Project,
               In_Tree                => In_Tree,
               From_Project_Node      => From_Project_Node,
               Node_Tree              => Node_Tree,
               Env                    => Env,
               Pkg                    => Pkg,
               Item                   => Decl_Item,
               Child_Env              => Child_Env);
         end if;
      end Process_Case_Construction;

      --  Local variables

      Current, Decl : Project_Node_Id;
      Kind          : Project_Node_Kind;

   --  Start of processing for Process_Declarative_Items

   begin
      Decl := Item;
      while Present (Decl) loop
         Current := Current_Item_Node (Decl, Node_Tree);
         Decl    := Next_Declarative_Item (Decl, Node_Tree);
         Kind    := Kind_Of (Current, Node_Tree);

         case Kind is
            when N_Package_Declaration =>
               Process_Package_Declaration (Current);

            --  Nothing to process for string type declaration

            when N_String_Type_Declaration =>
               null;

            when N_Attribute_Declaration      |
                 N_Typed_Variable_Declaration |
                 N_Variable_Declaration       =>
               Process_Attribute_Declaration (Current);

            when N_Case_Construction =>
               Process_Case_Construction (Current);

            when others =>
               Write_Line ("Illegal declarative item: " & Kind'Img);
               raise Program_Error;
         end case;
      end loop;
   end Process_Declarative_Items;

   ----------------------------------
   -- Process_Project_Tree_Phase_1 --
   ----------------------------------

   procedure Process_Project_Tree_Phase_1
     (In_Tree                : Project_Tree_Ref;
      Project                : out Project_Id;
      Packages_To_Check      : String_List_Access;
      Success                : out Boolean;
      From_Project_Node      : Project_Node_Id;
      From_Project_Node_Tree : Project_Node_Tree_Ref;
      Env                    : in out Prj.Tree.Environment;
      Reset_Tree             : Boolean := True)
   is
   begin
      if Reset_Tree then

         --  Make sure there are no projects in the data structure

         Free_List (In_Tree.Projects, Free_Project => True);
      end if;

      Processed_Projects.Reset;

      --  And process the main project and all of the projects it depends on,
      --  recursively.

      Debug_Increase_Indent ("Process tree, phase 1");

      Recursive_Process
        (Project                => Project,
         In_Tree                => In_Tree,
         Packages_To_Check      => Packages_To_Check,
         From_Project_Node      => From_Project_Node,
         From_Project_Node_Tree => From_Project_Node_Tree,
         Env                    => Env,
         Extended_By            => No_Project);

      Success :=
        Total_Errors_Detected = 0
          and then
          (Warning_Mode /= Treat_As_Error or else Warnings_Detected = 0);

      if Current_Verbosity = High then
         Debug_Decrease_Indent
           ("Done Process tree, phase 1, Success=" & Success'Img);
      end if;
   end Process_Project_Tree_Phase_1;

   ----------------------------------
   -- Process_Project_Tree_Phase_2 --
   ----------------------------------

   procedure Process_Project_Tree_Phase_2
     (In_Tree                : Project_Tree_Ref;
      Project                : Project_Id;
      Success                : out Boolean;
      From_Project_Node      : Project_Node_Id;
      From_Project_Node_Tree : Project_Node_Tree_Ref;
      Env                    : Environment)
   is
      Obj_Dir    : Path_Name_Type;
      Extending  : Project_Id;
      Extending2 : Project_Id;
      Prj        : Project_List;

   --  Start of processing for Process_Project_Tree_Phase_2

   begin
      Success := True;

      Debug_Increase_Indent ("Process tree, phase 2", Project.Name);

      if Project /= No_Project then
         Check (In_Tree, Project, From_Project_Node_Tree, Env.Flags);
      end if;

      --  If main project is an extending all project, set object directory of
      --  all virtual extending projects to object directory of main project.

      if Project /= No_Project
        and then Is_Extending_All (From_Project_Node, From_Project_Node_Tree)
      then
         declare
            Object_Dir : constant Path_Information := Project.Object_Directory;

         begin
            Prj := In_Tree.Projects;
            while Prj /= null loop
               if Prj.Project.Virtual then
                  Prj.Project.Object_Directory := Object_Dir;
               end if;

               Prj := Prj.Next;
            end loop;
         end;
      end if;

      --  Check that no extending project shares its object directory with
      --  the project(s) it extends.

      if Project /= No_Project then
         Prj := In_Tree.Projects;
         while Prj /= null loop
            Extending := Prj.Project.Extended_By;

            if Extending /= No_Project then
               Obj_Dir := Prj.Project.Object_Directory.Name;

               --  Check that a project being extended does not share its
               --  object directory with any project that extends it, directly
               --  or indirectly, including a virtual extending project.

               --  Start with the project directly extending it

               Extending2 := Extending;
               while Extending2 /= No_Project loop
                  if Has_Ada_Sources (Extending2)
                    and then Extending2.Object_Directory.Name = Obj_Dir
                  then
                     if Extending2.Virtual then
                        Error_Msg_Name_1 := Prj.Project.Display_Name;
                        Error_Msg
                          (Env.Flags,
                           "project %% cannot be extended by a virtual" &
                           " project with the same object directory",
                           Prj.Project.Location, Project);

                     else
                        Error_Msg_Name_1 := Extending2.Display_Name;
                        Error_Msg_Name_2 := Prj.Project.Display_Name;
                        Error_Msg
                          (Env.Flags,
                           "project %% cannot extend project %%",
                           Extending2.Location, Project);
                        Error_Msg
                          (Env.Flags,
                           "\they share the same object directory",
                           Extending2.Location, Project);
                     end if;
                  end if;

                  --  Continue with the next extending project, if any

                  Extending2 := Extending2.Extended_By;
               end loop;
            end if;

            Prj := Prj.Next;
         end loop;
      end if;

      Debug_Decrease_Indent ("Done Process tree, phase 2");

      Success := Total_Errors_Detected = 0
        and then
          (Warning_Mode /= Treat_As_Error or else Warnings_Detected = 0);
   end Process_Project_Tree_Phase_2;

   -----------------------
   -- Recursive_Process --
   -----------------------

   procedure Recursive_Process
     (In_Tree                : Project_Tree_Ref;
      Project                : out Project_Id;
      Packages_To_Check      : String_List_Access;
      From_Project_Node      : Project_Node_Id;
      From_Project_Node_Tree : Project_Node_Tree_Ref;
      Env                    : in out Prj.Tree.Environment;
      Extended_By            : Project_Id)
   is
      Shared : constant Shared_Project_Tree_Data_Access := In_Tree.Shared;

      Child_Env              : Prj.Tree.Environment;
      --  Only used for the root aggregate project (if any). This is left
      --  uninitialized otherwise.

      procedure Process_Imported_Projects
        (Imported     : in out Project_List;
         Limited_With : Boolean);
      --  Process imported projects. If Limited_With is True, then only
      --  projects processed through a "limited with" are processed, otherwise
      --  only projects imported through a standard "with" are processed.
      --  Imported is the id of the last imported project.

      procedure Process_Aggregated_Projects;
      --  Process all the projects aggregated in List. This does nothing if the
      --  project is not an aggregate project.

      procedure Process_Extended_Project;
      --  Process the extended project: inherit all packages from the extended
      --  project that are not explicitly defined or renamed. Also inherit the
      --  languages, if attribute Languages is not explicitly defined.

      -------------------------------
      -- Process_Imported_Projects --
      -------------------------------

      procedure Process_Imported_Projects
        (Imported     : in out Project_List;
         Limited_With : Boolean)
      is
         With_Clause : Project_Node_Id;
         New_Project : Project_Id;
         Proj_Node   : Project_Node_Id;

      begin
         With_Clause :=
           First_With_Clause_Of
             (From_Project_Node, From_Project_Node_Tree);

         while Present (With_Clause) loop
            Proj_Node :=
              Non_Limited_Project_Node_Of
                (With_Clause, From_Project_Node_Tree);
            New_Project := No_Project;

            if (Limited_With and then No (Proj_Node))
              or else (not Limited_With and then Present (Proj_Node))
            then
               Recursive_Process
                 (In_Tree                => In_Tree,
                  Project                => New_Project,
                  Packages_To_Check      => Packages_To_Check,
                  From_Project_Node      =>
                    Project_Node_Of (With_Clause, From_Project_Node_Tree),
                  From_Project_Node_Tree => From_Project_Node_Tree,
                  Env                    => Env,
                  Extended_By            => No_Project);

               --  Imported is the id of the last imported project. If
               --  it is nil, then this imported project is our first.

               if Imported = null then
                  Project.Imported_Projects :=
                    new Project_List_Element'
                      (Project => New_Project,
                       Next    => null);
                  Imported := Project.Imported_Projects;
               else
                  Imported.Next := new Project_List_Element'
                    (Project => New_Project,
                     Next    => null);
                  Imported := Imported.Next;
               end if;
            end if;

            With_Clause :=
              Next_With_Clause_Of (With_Clause, From_Project_Node_Tree);
         end loop;
      end Process_Imported_Projects;

      ---------------------------------
      -- Process_Aggregated_Projects --
      ---------------------------------

      procedure Process_Aggregated_Projects is
         List           : Aggregated_Project_List;
         Loaded_Project : Prj.Tree.Project_Node_Id;
         Success        : Boolean := True;
         Tree           : Project_Tree_Ref;

      begin
         if Project.Qualifier not in Aggregate_Project then
            return;
         end if;

         Debug_Increase_Indent ("Process_Aggregated_Projects", Project.Name);

         Prj.Nmsc.Process_Aggregated_Projects
           (Tree      => In_Tree,
            Project   => Project,
            Node_Tree => From_Project_Node_Tree,
            Flags     => Env.Flags);

         List := Project.Aggregated_Projects;
         while Success and then List /= null loop
            Prj.Part.Parse
              (In_Tree           => From_Project_Node_Tree,
               Project           => Loaded_Project,
               Packages_To_Check => Packages_To_Check,
               Project_File_Name => Get_Name_String (List.Path),
               Errout_Handling   => Prj.Part.Never_Finalize,
               Current_Directory => Get_Name_String (Project.Directory.Name),
               Is_Config_File    => False,
               Env               => Child_Env);

            Success := not Prj.Tree.No (Loaded_Project);

            if Success then
               List.Tree := new Project_Tree_Data (Is_Root_Tree => False);
               Prj.Initialize (List.Tree);
               List.Tree.Shared := In_Tree.Shared;

               --  In aggregate library, aggregated projects are parsed using
               --  the aggregate library tree.

               if Project.Qualifier = Aggregate_Library then
                  Tree := In_Tree;
               else
                  Tree := List.Tree;
               end if;

               --  We can only do the phase 1 of the processing, since we do
               --  not have access to the configuration file yet (this is
               --  called when doing phase 1 of the processing for the root
               --  aggregate project).

               if In_Tree.Is_Root_Tree then
                  Process_Project_Tree_Phase_1
                    (In_Tree                => Tree,
                     Project                => List.Project,
                     Packages_To_Check      => Packages_To_Check,
                     Success                => Success,
                     From_Project_Node      => Loaded_Project,
                     From_Project_Node_Tree => From_Project_Node_Tree,
                     Env                    => Child_Env,
                     Reset_Tree             => False);
               else
                  --  use the same environment as the rest of the aggregated
                  --  projects, ie the one that was setup by the root aggregate
                  Process_Project_Tree_Phase_1
                    (In_Tree                => Tree,
                     Project                => List.Project,
                     Packages_To_Check      => Packages_To_Check,
                     Success                => Success,
                     From_Project_Node      => Loaded_Project,
                     From_Project_Node_Tree => From_Project_Node_Tree,
                     Env                    => Env,
                     Reset_Tree             => False);
               end if;

            else
               Debug_Output ("Failed to parse", Name_Id (List.Path));
            end if;

            List := List.Next;
         end loop;

         Debug_Decrease_Indent ("Done Process_Aggregated_Projects");
      end Process_Aggregated_Projects;

      ------------------------------
      -- Process_Extended_Project --
      ------------------------------

      procedure Process_Extended_Project is
         Extended_Pkg : Package_Id;
         Current_Pkg  : Package_Id;
         Element      : Package_Element;
         First        : constant Package_Id := Project.Decl.Packages;
         Attribute1   : Variable_Id;
         Attribute2   : Variable_Id;
         Attr_Value1  : Variable;
         Attr_Value2  : Variable;

      begin
         Extended_Pkg := Project.Extends.Decl.Packages;
         while Extended_Pkg /= No_Package loop
            Element := Shared.Packages.Table (Extended_Pkg);

            Current_Pkg := First;
            while Current_Pkg /= No_Package
              and then
                Shared.Packages.Table (Current_Pkg).Name /= Element.Name
            loop
               Current_Pkg := Shared.Packages.Table (Current_Pkg).Next;
            end loop;

            if Current_Pkg = No_Package then
               Package_Table.Increment_Last (Shared.Packages);
               Current_Pkg := Package_Table.Last (Shared.Packages);
               Shared.Packages.Table (Current_Pkg) :=
                 (Name   => Element.Name,
                  Decl   => No_Declarations,
                  Parent => No_Package,
                  Next   => Project.Decl.Packages);
               Project.Decl.Packages := Current_Pkg;
               Copy_Package_Declarations
                 (From       => Element.Decl,
                  To         => Shared.Packages.Table (Current_Pkg).Decl,
                  New_Loc    => No_Location,
                  Restricted => True,
                  Shared     => Shared);
            end if;

            Extended_Pkg := Element.Next;
         end loop;

         --  Check if attribute Languages is declared in the extending project

         Attribute1 := Project.Decl.Attributes;
         while Attribute1 /= No_Variable loop
            Attr_Value1 := Shared.Variable_Elements. Table (Attribute1);
            exit when Attr_Value1.Name = Snames.Name_Languages;
            Attribute1 := Attr_Value1.Next;
         end loop;

         if Attribute1 = No_Variable or else Attr_Value1.Value.Default then

            --  Attribute Languages is not declared in the extending project.
            --  Check if it is declared in the project being extended.

            Attribute2 := Project.Extends.Decl.Attributes;
            while Attribute2 /= No_Variable loop
               Attr_Value2 := Shared.Variable_Elements.Table (Attribute2);
               exit when Attr_Value2.Name = Snames.Name_Languages;
               Attribute2 := Attr_Value2.Next;
            end loop;

            if Attribute2 /= No_Variable
              and then not Attr_Value2.Value.Default
            then
               --  As attribute Languages is declared in the project being
               --  extended, copy its value for the extending project.

               if Attribute1 = No_Variable then
                  Variable_Element_Table.Increment_Last
                    (Shared.Variable_Elements);
                  Attribute1 := Variable_Element_Table.Last
                    (Shared.Variable_Elements);
                  Attr_Value1.Next := Project.Decl.Attributes;
                  Project.Decl.Attributes := Attribute1;
               end if;

               Attr_Value1.Name := Snames.Name_Languages;
               Attr_Value1.Value := Attr_Value2.Value;
               Shared.Variable_Elements.Table (Attribute1) := Attr_Value1;
            end if;
         end if;
      end Process_Extended_Project;

   --  Start of processing for Recursive_Process

   begin
      if No (From_Project_Node) then
         Project := No_Project;

      else
         declare
            Imported         : Project_List;
            Declaration_Node : Project_Node_Id  := Empty_Node;

            Name : constant Name_Id :=
                     Name_Of (From_Project_Node, From_Project_Node_Tree);

            Name_Node : constant Tree_Private_Part.Project_Name_And_Node :=
                          Tree_Private_Part.Projects_Htable.Get
                            (From_Project_Node_Tree.Projects_HT, Name);

         begin
            Project := Processed_Projects.Get (Name);

            if Project /= No_Project then

               --  Make sure that, when a project is extended, the project id
               --  of the project extending it is recorded in its data, even
               --  when it has already been processed as an imported project.
               --  This is for virtually extended projects.

               if Extended_By /= No_Project then
                  Project.Extended_By := Extended_By;
               end if;

               return;
            end if;

            Project :=
              new Project_Data'
                (Empty_Project
                  (Project_Qualifier_Of
                    (From_Project_Node, From_Project_Node_Tree)));

            In_Tree.Projects :=
              new Project_List_Element'
                    (Project => Project,
                     Next    => In_Tree.Projects);

            Processed_Projects.Set (Name, Project);

            Project.Name := Name;
            Project.Display_Name := Name_Node.Display_Name;
            Get_Name_String (Name);

            --  If name starts with the virtual prefix, flag the project as
            --  being a virtual extending project.

            if Name_Len > Virtual_Prefix'Length
              and then
                Name_Buffer (1 .. Virtual_Prefix'Length) = Virtual_Prefix
            then
               Project.Virtual := True;
            end if;

            Project.Path.Display_Name :=
              Path_Name_Of (From_Project_Node, From_Project_Node_Tree);
            Get_Name_String (Project.Path.Display_Name);
            Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
            Project.Path.Name := Name_Find;

            Project.Location :=
              Location_Of (From_Project_Node, From_Project_Node_Tree);

            Project.Directory.Display_Name :=
              Directory_Of (From_Project_Node, From_Project_Node_Tree);
            Get_Name_String (Project.Directory.Display_Name);
            Canonical_Case_File_Name (Name_Buffer (1 .. Name_Len));
            Project.Directory.Name := Name_Find;

            Project.Extended_By := Extended_By;

            Add_Attributes
              (Project,
               Name,
               Name_Id (Project.Directory.Name),
               In_Tree.Shared,
               Project.Decl,
               Prj.Attr.Attribute_First,
               Project_Level => True);

            Process_Imported_Projects (Imported, Limited_With => False);

            if Project.Qualifier = Aggregate and then In_Tree.Is_Root_Tree then
               Initialize_And_Copy (Child_Env, Copy_From => Env);

            elsif Project.Qualifier = Aggregate_Library then

               --  The child environment is the same as the current one

               Child_Env := Env;

            else
               --  No need to initialize Child_Env, since it will not be
               --  used anyway by Process_Declarative_Items (only the root
               --  aggregate can modify it, and it is never read anyway).

               null;
            end if;

            Declaration_Node :=
              Project_Declaration_Of
                (From_Project_Node, From_Project_Node_Tree);

            Recursive_Process
              (In_Tree                => In_Tree,
               Project                => Project.Extends,
               Packages_To_Check      => Packages_To_Check,
               From_Project_Node      =>
                 Extended_Project_Of
                   (Declaration_Node, From_Project_Node_Tree),
               From_Project_Node_Tree => From_Project_Node_Tree,
               Env                    => Env,
               Extended_By            => Project);

            Process_Declarative_Items
              (Project                => Project,
               In_Tree                => In_Tree,
               From_Project_Node      => From_Project_Node,
               Node_Tree              => From_Project_Node_Tree,
               Env                    => Env,
               Pkg                    => No_Package,
               Item                   => First_Declarative_Item_Of
                 (Declaration_Node, From_Project_Node_Tree),
               Child_Env              => Child_Env);

            if Project.Extends /= No_Project then
               Process_Extended_Project;
            end if;

            Process_Imported_Projects (Imported, Limited_With => True);

            if Err_Vars.Total_Errors_Detected = 0 then
               Process_Aggregated_Projects;

               --  For an aggregate library we add the aggregated projects as
               --  imported ones. This is necessary to give visibility to all
               --  sources from the aggregates from the aggregated library
               --  projects.

               if Project.Qualifier = Aggregate_Library then
                  declare
                     L : Aggregated_Project_List;
                  begin
                     L := Project.Aggregated_Projects;
                     while L /= null loop
                        Project.Imported_Projects :=
                          new Project_List_Element'
                            (Project => L.Project,
                             Next    => Project.Imported_Projects);
                        L := L.Next;
                     end loop;
                  end;
               end if;
            end if;

            if Project.Qualifier = Aggregate and then In_Tree.Is_Root_Tree then
               Free (Child_Env);
            end if;
         end;
      end if;
   end Recursive_Process;

end Prj.Proc;
