------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P R J . P R O C                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2001-2009, Free Software Foundation, Inc.         --
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
with Prj.Err;  use Prj.Err;
with Prj.Ext;  use Prj.Ext;
with Prj.Nmsc; use Prj.Nmsc;
with Snames;

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
      In_Tree       : Project_Tree_Ref;
      Decl          : in out Declarations;
      First         : Attribute_Node_Id;
      Project_Level : Boolean);
   --  Add all attributes, starting with First, with their default values to
   --  the package or project with declarations Decl.

   procedure Check
     (In_Tree : Project_Tree_Ref;
      Project : Project_Id;
      Flags   : Processing_Flags);
   --  Set all projects to not checked, then call Recursive_Check for the
   --  main project Project. Project is set to No_Project if errors occurred.
   --  Current_Dir is for optimization purposes, avoiding extra system calls.
   --  If Allow_Duplicate_Basenames, then files with the same base names are
   --  authorized within a project for source-based languages (never for unit
   --  based languages)

   procedure Copy_Package_Declarations
     (From              : Declarations;
      To                : in out Declarations;
      New_Loc           : Source_Ptr;
      Naming_Restricted : Boolean;
      In_Tree           : Project_Tree_Ref);
   --  Copy a package declaration From to To for a renamed package. Change the
   --  locations of all the attributes to New_Loc. When Naming_Restricted is
   --  True, do not copy attributes Body, Spec, Implementation and
   --  Specification.

   function Expression
     (Project                : Project_Id;
      In_Tree                : Project_Tree_Ref;
      Flags                  : Processing_Flags;
      From_Project_Node      : Project_Node_Id;
      From_Project_Node_Tree : Project_Node_Tree_Ref;
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
      In_Tree   : Project_Tree_Ref;
      With_Name : Name_Id) return Package_Id;
   --  Find the package of Project whose name is With_Name

   procedure Process_Declarative_Items
     (Project                : Project_Id;
      In_Tree                : Project_Tree_Ref;
      Flags                  : Processing_Flags;
      From_Project_Node      : Project_Node_Id;
      From_Project_Node_Tree : Project_Node_Tree_Ref;
      Pkg                    : Package_Id;
      Item                   : Project_Node_Id);
   --  Process declarative items starting with From_Project_Node, and put them
   --  in declarations Decl. This is a recursive procedure; it calls itself for
   --  a package declaration or a case construction.

   procedure Recursive_Process
     (In_Tree                : Project_Tree_Ref;
      Project                : out Project_Id;
      Flags                  : Processing_Flags;
      From_Project_Node      : Project_Node_Id;
      From_Project_Node_Tree : Project_Node_Tree_Ref;
      Extended_By            : Project_Id);
   --  Process project with node From_Project_Node in the tree. Do nothing if
   --  From_Project_Node is Empty_Node. If project has already been processed,
   --  simply return its project id. Otherwise create a new project id, mark it
   --  as processed, call itself recursively for all imported projects and a
   --  extended project, if any. Then process the declarative items of the
   --  project.

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
      In_Tree       : Project_Tree_Ref;
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
                 (In_Tree.Variable_Elements);
               In_Tree.Variable_Elements.Table
                 (Variable_Element_Table.Last
                   (In_Tree.Variable_Elements)) :=
                 (Next  => Decl.Attributes,
                  Name  => Attribute_Name_Of (The_Attribute),
                  Value => New_Attribute);
               Decl.Attributes := Variable_Element_Table.Last
                 (In_Tree.Variable_Elements);
            end;
         end if;

         The_Attribute := Next_Attribute (After => The_Attribute);
      end loop;
   end Add_Attributes;

   -----------
   -- Check --
   -----------

   procedure Check
     (In_Tree : Project_Tree_Ref;
      Project : Project_Id;
      Flags   : Processing_Flags)
   is
   begin
      Process_Naming_Scheme (In_Tree, Project, Flags);

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
     (From              : Declarations;
      To                : in out Declarations;
      New_Loc           : Source_Ptr;
      Naming_Restricted : Boolean;
      In_Tree           : Project_Tree_Ref)
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

         Var := In_Tree.Variable_Elements.Table (V1);
         V1  := Var.Next;

         --  Remove the Next component

         Var.Next := No_Variable;

         --  Change the location to New_Loc

         Var.Value.Location := New_Loc;
         Variable_Element_Table.Increment_Last (In_Tree.Variable_Elements);

         --  Put in new declaration

         if To.Attributes = No_Variable then
            To.Attributes :=
              Variable_Element_Table.Last (In_Tree.Variable_Elements);
         else
            In_Tree.Variable_Elements.Table (V2).Next :=
              Variable_Element_Table.Last (In_Tree.Variable_Elements);
         end if;

         V2 := Variable_Element_Table.Last (In_Tree.Variable_Elements);
         In_Tree.Variable_Elements.Table (V2) := Var;
      end loop;

      --  Then the associated array attributes

      A1 := From.Arrays;
      while A1 /= No_Array loop
         Arr := In_Tree.Arrays.Table (A1);
         A1  := Arr.Next;

         if not Naming_Restricted or else
           (Arr.Name /= Snames.Name_Body
             and then Arr.Name /= Snames.Name_Spec
             and then Arr.Name /= Snames.Name_Implementation
             and then Arr.Name /= Snames.Name_Specification)
         then
            --  Remove the Next component

            Arr.Next := No_Array;

            Array_Table.Increment_Last (In_Tree.Arrays);

            --  Create new Array declaration

            if To.Arrays = No_Array then
               To.Arrays := Array_Table.Last (In_Tree.Arrays);
            else
               In_Tree.Arrays.Table (A2).Next :=
                 Array_Table.Last (In_Tree.Arrays);
            end if;

            A2 := Array_Table.Last (In_Tree.Arrays);

            --  Don't store the array as its first element has not been set yet

            --  Copy the array elements of the array

            E1 := Arr.Value;
            Arr.Value := No_Array_Element;
            while E1 /= No_Array_Element loop

               --  Copy the array element

               Elm := In_Tree.Array_Elements.Table (E1);
               E1 := Elm.Next;

               --  Remove the Next component

               Elm.Next := No_Array_Element;

               --  Change the location

               Elm.Value.Location := New_Loc;
               Array_Element_Table.Increment_Last (In_Tree.Array_Elements);

               --  Create new array element

               if Arr.Value = No_Array_Element then
                  Arr.Value :=
                    Array_Element_Table.Last (In_Tree.Array_Elements);
               else
                  In_Tree.Array_Elements.Table (E2).Next :=
                    Array_Element_Table.Last (In_Tree.Array_Elements);
               end if;

               E2 := Array_Element_Table.Last (In_Tree.Array_Elements);
               In_Tree.Array_Elements.Table (E2) := Elm;
            end loop;

            --  Finally, store the new array

            In_Tree.Arrays.Table (A2) := Arr;
         end if;
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
      Lower : Boolean;

   begin
      Get_Name_String (Index);
      Lower := Case_Insensitive (Attr, Tree);

      --  The index is always case insensitive if it does not include any dot.
      --  ??? Why not use the properties from prj-attr, simply, maybe because
      --  we don't know whether we have a file as an index?

      if not Lower then
         Lower := True;

         for J in 1 .. Name_Len loop
            if Name_Buffer (J) = '.' then
               Lower := False;
               exit;
            end if;
         end loop;
      end if;

      if Lower then
         To_Lower (Name_Buffer (1 .. Name_Len));
         return Name_Find;
      else
         return Index;
      end if;
   end Get_Attribute_Index;

   ----------------
   -- Expression --
   ----------------

   function Expression
     (Project                : Project_Id;
      In_Tree                : Project_Tree_Ref;
      Flags                  : Processing_Flags;
      From_Project_Node      : Project_Node_Id;
      From_Project_Node_Tree : Project_Node_Tree_Ref;
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
                       (In_Tree.String_Elements);

                     if Last = Nil_String then

                        --  This can happen in an expression like () & "toto"

                        Result.Values := String_Element_Table.Last
                          (In_Tree.String_Elements);

                     else
                        In_Tree.String_Elements.Table
                          (Last).Next := String_Element_Table.Last
                                       (In_Tree.String_Elements);
                     end if;

                     Last := String_Element_Table.Last
                               (In_Tree.String_Elements);

                     In_Tree.String_Elements.Table (Last) :=
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
                     --  nothing to do

                     Value := Expression
                       (Project                => Project,
                        In_Tree                => In_Tree,
                        Flags                  => Flags,
                        From_Project_Node      => From_Project_Node,
                        From_Project_Node_Tree => From_Project_Node_Tree,
                        Pkg                    => Pkg,
                        First_Term             =>
                          Tree.First_Term
                            (String_Node, From_Project_Node_Tree),
                        Kind                   => Single);
                     String_Element_Table.Increment_Last
                       (In_Tree.String_Elements);

                     if Result.Values = Nil_String then

                        --  This literal string list is the first term in a
                        --  string list expression

                        Result.Values :=
                          String_Element_Table.Last (In_Tree.String_Elements);

                     else
                        In_Tree.String_Elements.Table
                          (Last).Next :=
                          String_Element_Table.Last (In_Tree.String_Elements);
                     end if;

                     Last :=
                       String_Element_Table.Last (In_Tree.String_Elements);

                     In_Tree.String_Elements.Table (Last) :=
                       (Value    => Value.Value,
                        Display_Value => No_Name,
                        Location => Value.Location,
                        Flag     => False,
                        Next     => Nil_String,
                        Index    => Value.Index);

                     loop
                        --  Add the other element of the literal string list
                        --  one after the other

                        String_Node :=
                          Next_Expression_In_List
                            (String_Node, From_Project_Node_Tree);

                        exit when No (String_Node);

                        Value :=
                          Expression
                            (Project                => Project,
                             In_Tree                => In_Tree,
                             Flags                  => Flags,
                             From_Project_Node      => From_Project_Node,
                             From_Project_Node_Tree => From_Project_Node_Tree,
                             Pkg                    => Pkg,
                             First_Term             =>
                               Tree.First_Term
                                 (String_Node, From_Project_Node_Tree),
                             Kind                   => Single);

                        String_Element_Table.Increment_Last
                          (In_Tree.String_Elements);
                        In_Tree.String_Elements.Table
                          (Last).Next := String_Element_Table.Last
                                        (In_Tree.String_Elements);
                        Last := String_Element_Table.Last
                          (In_Tree.String_Elements);
                        In_Tree.String_Elements.Table (Last) :=
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
                  if Present (Term_Project) and then
                     Term_Project /= From_Project_Node
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
                       and then In_Tree.Packages.Table
                                  (The_Package).Name /= The_Name
                     loop
                        The_Package :=
                          In_Tree.Packages.Table
                            (The_Package).Next;
                     end loop;

                     pragma Assert
                       (The_Package /= No_Package,
                        "package not found.");

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
                             In_Tree.Packages.Table
                               (The_Package).Decl.Variables;
                        else
                           The_Variable_Id :=
                             In_Tree.Packages.Table
                               (The_Package).Decl.Attributes;
                        end if;

                        while The_Variable_Id /= No_Variable
                          and then
                            In_Tree.Variable_Elements.Table
                              (The_Variable_Id).Name /= The_Name
                        loop
                           The_Variable_Id :=
                             In_Tree.Variable_Elements.Table
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
                          and then
                          In_Tree.Variable_Elements.Table
                            (The_Variable_Id).Name /= The_Name
                        loop
                           The_Variable_Id :=
                             In_Tree.Variable_Elements.Table
                               (The_Variable_Id).Next;
                        end loop;

                     end if;

                     pragma Assert (The_Variable_Id /= No_Variable,
                                      "variable or attribute not found");

                     The_Variable :=
                       In_Tree.Variable_Elements.Table
                                                    (The_Variable_Id).Value;

                  else

                     --  It is an associative array attribute

                     declare
                        The_Array   : Array_Id := No_Array;
                        The_Element : Array_Element_Id := No_Array_Element;
                        Array_Index : Name_Id := No_Name;

                     begin
                        if The_Package /= No_Package then
                           The_Array :=
                             In_Tree.Packages.Table
                               (The_Package).Decl.Arrays;
                        else
                           The_Array := The_Project.Decl.Arrays;
                        end if;

                        while The_Array /= No_Array
                          and then In_Tree.Arrays.Table
                                     (The_Array).Name /= The_Name
                        loop
                           The_Array := In_Tree.Arrays.Table
                                          (The_Array).Next;
                        end loop;

                        if The_Array /= No_Array then
                           The_Element := In_Tree.Arrays.Table
                                            (The_Array).Value;
                           Array_Index :=
                             Get_Attribute_Index
                               (From_Project_Node_Tree,
                                The_Current_Term,
                                Index);

                           while The_Element /= No_Array_Element
                             and then
                             In_Tree.Array_Elements.Table
                               (The_Element).Index /= Array_Index
                           loop
                              The_Element :=
                                In_Tree.Array_Elements.Table
                                  (The_Element).Next;
                           end loop;

                        end if;

                        if The_Element /= No_Array_Element then
                           The_Variable :=
                             In_Tree.Array_Elements.Table
                               (The_Element).Value;

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
                                (In_Tree.String_Elements);

                              if Last = Nil_String then

                                 --  This can happen in an expression such as
                                 --  () & Var

                                 Result.Values :=
                                   String_Element_Table.Last
                                     (In_Tree.String_Elements);

                              else
                                 In_Tree.String_Elements.Table
                                   (Last).Next :=
                                     String_Element_Table.Last
                                       (In_Tree.String_Elements);
                              end if;

                              Last :=
                                String_Element_Table.Last
                                  (In_Tree.String_Elements);

                              In_Tree.String_Elements.Table (Last) :=
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
                                      (In_Tree.String_Elements);

                                    if Last = Nil_String then
                                       Result.Values :=
                                         String_Element_Table.Last
                                           (In_Tree.
                                                String_Elements);

                                    else
                                       In_Tree.
                                         String_Elements.Table (Last).Next :=
                                         String_Element_Table.Last
                                           (In_Tree.
                                                String_Elements);

                                    end if;

                                    Last :=
                                      String_Element_Table.Last
                                        (In_Tree.String_Elements);

                                    In_Tree.String_Elements.Table (Last) :=
                                      (Value         =>
                                         In_Tree.String_Elements.Table
                                           (The_List).Value,
                                       Display_Value => No_Name,
                                       Location      =>
                                         Location_Of
                                           (The_Current_Term,
                                            From_Project_Node_Tree),
                                       Flag         => False,
                                       Next         => Nil_String,
                                       Index        => 0);

                                    The_List :=
                                      In_Tree. String_Elements.Table
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
                  Name    : constant Name_Id  := Name_Find;
                  Default : Name_Id           := No_Name;
                  Value   : Name_Id           := No_Name;

                  Def_Var : Variable_Value;

                  Default_Node : constant Project_Node_Id :=
                    External_Default_Of
                      (The_Current_Term, From_Project_Node_Tree);

               begin
                  --  If there is a default value for the external reference,
                  --  get its value.

                  if Present (Default_Node) then
                     Def_Var := Expression
                       (Project                => Project,
                        In_Tree                => In_Tree,
                        Flags                  => Flags,
                        From_Project_Node      => From_Project_Node,
                        From_Project_Node_Tree => From_Project_Node_Tree,
                        Pkg                    => Pkg,
                        First_Term             =>
                          Tree.First_Term
                            (Default_Node, From_Project_Node_Tree),
                        Kind                   => Single);

                     if Def_Var /= Nil_Variable_Value then
                        Default := Def_Var.Value;
                     end if;
                  end if;

                  Value :=
                    Prj.Ext.Value_Of (From_Project_Node_Tree, Name, Default);

                  if Value = No_Name then
                     if not Quiet_Output then
                        Error_Msg
                          (Flags, "?undefined external reference",
                           Location_Of
                             (The_Current_Term, From_Project_Node_Tree),
                           Project);
                     end if;

                     Value := Empty_String;
                  end if;

                  case Kind is

                     when Undefined =>
                        null;

                     when Single =>
                        Add (Result.Value, Value);

                     when List =>
                        String_Element_Table.Increment_Last
                          (In_Tree.String_Elements);

                        if Last = Nil_String then
                           Result.Values := String_Element_Table.Last
                             (In_Tree.String_Elements);

                        else
                           In_Tree.String_Elements.Table
                             (Last).Next := String_Element_Table.Last
                                       (In_Tree.String_Elements);
                        end if;

                        Last := String_Element_Table.Last
                                  (In_Tree.String_Elements);
                        In_Tree.String_Elements.Table (Last) :=
                          (Value    => Value,
                           Display_Value => No_Name,
                           Location      =>
                             Location_Of
                               (The_Current_Term, From_Project_Node_Tree),
                           Flag     => False,
                           Next     => Nil_String,
                           Index    => 0);

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
      In_Tree   : Project_Tree_Ref;
      With_Name : Name_Id) return Package_Id
   is
      Result : Package_Id := Project.Decl.Packages;

   begin
      --  Check the name of each existing package of Project

      while Result /= No_Package
        and then In_Tree.Packages.Table (Result).Name /= With_Name
      loop
         Result := In_Tree.Packages.Table (Result).Next;
      end loop;

      if Result = No_Package then

         --  Should never happen

         Write_Line ("package """ & Get_Name_String (With_Name) &
                     """ not found");
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
      Success                : out Boolean;
      From_Project_Node      : Project_Node_Id;
      From_Project_Node_Tree : Project_Node_Tree_Ref;
      Flags                  : Processing_Flags;
      Reset_Tree             : Boolean       := True)
   is
   begin
      Process_Project_Tree_Phase_1
        (In_Tree                => In_Tree,
         Project                => Project,
         Success                => Success,
         From_Project_Node      => From_Project_Node,
         From_Project_Node_Tree => From_Project_Node_Tree,
         Flags                  => Flags,
         Reset_Tree             => Reset_Tree);

      if Project_Qualifier_Of (From_Project_Node, From_Project_Node_Tree) /=
        Configuration
      then
         Process_Project_Tree_Phase_2
           (In_Tree                => In_Tree,
            Project                => Project,
            Success                => Success,
            From_Project_Node      => From_Project_Node,
            From_Project_Node_Tree => From_Project_Node_Tree,
            Flags                  => Flags);
      end if;
   end Process;

   -------------------------------
   -- Process_Declarative_Items --
   -------------------------------

   procedure Process_Declarative_Items
     (Project                : Project_Id;
      In_Tree                : Project_Tree_Ref;
      Flags                  : Processing_Flags;
      From_Project_Node      : Project_Node_Id;
      From_Project_Node_Tree : Project_Node_Tree_Ref;
      Pkg                    : Package_Id;
      Item                   : Project_Node_Id)
   is
      Current_Declarative_Item : Project_Node_Id;
      Current_Item             : Project_Node_Id;

   begin
      --  Loop through declarative items

      Current_Item := Empty_Node;

      Current_Declarative_Item := Item;
      while Present (Current_Declarative_Item) loop

         --  Get its data

         Current_Item :=
           Current_Item_Node
             (Current_Declarative_Item, From_Project_Node_Tree);

         --  And set Current_Declarative_Item to the next declarative item
         --  ready for the next iteration.

         Current_Declarative_Item :=
           Next_Declarative_Item
             (Current_Declarative_Item, From_Project_Node_Tree);

         case Kind_Of (Current_Item, From_Project_Node_Tree) is

            when N_Package_Declaration =>

               --  Do not process a package declaration that should be ignored

               if Expression_Kind_Of
                    (Current_Item, From_Project_Node_Tree) /= Ignored
               then
                  --  Create the new package

                  Package_Table.Increment_Last (In_Tree.Packages);

                  declare
                     New_Pkg         : constant Package_Id :=
                                         Package_Table.Last (In_Tree.Packages);
                     The_New_Package : Package_Element;

                     Project_Of_Renamed_Package :
                       constant Project_Node_Id :=
                         Project_Of_Renamed_Package_Of
                           (Current_Item, From_Project_Node_Tree);

                  begin
                     --  Set the name of the new package

                     The_New_Package.Name :=
                       Name_Of (Current_Item, From_Project_Node_Tree);

                     --  Insert the new package in the appropriate list

                     if Pkg /= No_Package then
                        The_New_Package.Next :=
                          In_Tree.Packages.Table (Pkg).Decl.Packages;
                        In_Tree.Packages.Table (Pkg).Decl.Packages :=
                          New_Pkg;

                     else
                        The_New_Package.Next  := Project.Decl.Packages;
                        Project.Decl.Packages := New_Pkg;
                     end if;

                     In_Tree.Packages.Table (New_Pkg) :=
                       The_New_Package;

                     if Present (Project_Of_Renamed_Package) then

                        --  Renamed package

                        declare
                           Project_Name : constant Name_Id :=
                                            Name_Of
                                              (Project_Of_Renamed_Package,
                                               From_Project_Node_Tree);

                           Renamed_Project :
                             constant Project_Id :=
                               Imported_Or_Extended_Project_From
                               (Project, Project_Name);

                           Renamed_Package : constant Package_Id :=
                                               Package_From
                                                 (Renamed_Project, In_Tree,
                                                  Name_Of
                                                    (Current_Item,
                                                     From_Project_Node_Tree));

                        begin
                           --  For a renamed package, copy the declarations of
                           --  the renamed package, but set all the locations
                           --  to the location of the package name in the
                           --  renaming declaration.

                           Copy_Package_Declarations
                             (From              =>
                                In_Tree.Packages.Table (Renamed_Package).Decl,
                              To                =>
                                In_Tree.Packages.Table (New_Pkg).Decl,
                              New_Loc           =>
                                Location_Of
                                  (Current_Item, From_Project_Node_Tree),
                              Naming_Restricted => False,
                              In_Tree           => In_Tree);
                        end;

                     --  Standard package declaration, not renaming

                     else
                        --  Set the default values of the attributes

                        Add_Attributes
                          (Project,
                           Project.Name,
                           Name_Id (Project.Directory.Name),
                           In_Tree,
                           In_Tree.Packages.Table (New_Pkg).Decl,
                           First_Attribute_Of
                             (Package_Id_Of
                                (Current_Item, From_Project_Node_Tree)),
                           Project_Level => False);

                        --  And process declarative items of the new package

                        Process_Declarative_Items
                          (Project                => Project,
                           In_Tree                => In_Tree,
                           Flags                  => Flags,
                           From_Project_Node      => From_Project_Node,
                           From_Project_Node_Tree => From_Project_Node_Tree,
                           Pkg                    => New_Pkg,
                           Item                   =>
                             First_Declarative_Item_Of
                               (Current_Item, From_Project_Node_Tree));
                     end if;
                  end;
               end if;

            when N_String_Type_Declaration =>

               --  There is nothing to process

               null;

            when N_Attribute_Declaration      |
                 N_Typed_Variable_Declaration |
                 N_Variable_Declaration       =>

               if Expression_Of (Current_Item, From_Project_Node_Tree) =
                                                                  Empty_Node
               then

                  --  It must be a full associative array attribute declaration

                  declare
                     Current_Item_Name : constant Name_Id :=
                                           Name_Of
                                             (Current_Item,
                                              From_Project_Node_Tree);
                     --  The name of the attribute

                     Current_Location  : constant Source_Ptr :=
                                           Location_Of
                                             (Current_Item,
                                              From_Project_Node_Tree);

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
                     --  The name of the package, if any, where the associative
                     --  array value is.

                     Orig_Package : Package_Id := No_Package;
                     --  The id of the package, if any, where the associative
                     --  array value is.

                     New_Element : Array_Element_Id := No_Array_Element;
                     --  Id of a new array element created

                     Prev_Element : Array_Element_Id := No_Array_Element;
                     --  Last new element id created

                     Orig_Element : Array_Element_Id := No_Array_Element;
                     --  Current array element in original associative array

                     Next_Element : Array_Element_Id := No_Array_Element;
                     --  Id of the array element that follows the new element.
                     --  This is not always nil, because values for the
                     --  associative array attribute may already have been
                     --  declared, and the array elements declared are reused.

                     Prj : Project_List;

                  begin
                     --  First find if the associative array attribute already
                     --  has elements declared.

                     if Pkg /= No_Package then
                        New_Array := In_Tree.Packages.Table
                                       (Pkg).Decl.Arrays;

                     else
                        New_Array := Project.Decl.Arrays;
                     end if;

                     while New_Array /= No_Array
                       and then In_Tree.Arrays.Table (New_Array).Name /=
                                                           Current_Item_Name
                     loop
                        New_Array := In_Tree.Arrays.Table (New_Array).Next;
                     end loop;

                     --  If the attribute has never been declared add new entry
                     --  in the arrays of the project/package and link it.

                     if New_Array = No_Array then
                        Array_Table.Increment_Last (In_Tree.Arrays);
                        New_Array := Array_Table.Last (In_Tree.Arrays);

                        if Pkg /= No_Package then
                           In_Tree.Arrays.Table (New_Array) :=
                             (Name     => Current_Item_Name,
                              Location => Current_Location,
                              Value    => No_Array_Element,
                              Next     => In_Tree.Packages.Table
                                            (Pkg).Decl.Arrays);

                           In_Tree.Packages.Table (Pkg).Decl.Arrays :=
                             New_Array;

                        else
                           In_Tree.Arrays.Table (New_Array) :=
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
                         (Associative_Project_Of
                              (Current_Item, From_Project_Node_Tree),
                          From_Project_Node_Tree);

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

                     if No (Associative_Package_Of
                              (Current_Item, From_Project_Node_Tree))
                     then
                        Orig_Array := Orig_Project.Decl.Arrays;

                     else
                        --  If in a package, find the package where the value
                        --  is declared.

                        Orig_Package_Name :=
                          Name_Of
                            (Associative_Package_Of
                                 (Current_Item, From_Project_Node_Tree),
                             From_Project_Node_Tree);

                        Orig_Package := Orig_Project.Decl.Packages;
                        pragma Assert (Orig_Package /= No_Package,
                                       "original package not found");

                        while In_Tree.Packages.Table
                                (Orig_Package).Name /= Orig_Package_Name
                        loop
                           Orig_Package := In_Tree.Packages.Table
                                             (Orig_Package).Next;
                           pragma Assert (Orig_Package /= No_Package,
                                          "original package not found");
                        end loop;

                        Orig_Array :=
                          In_Tree.Packages.Table (Orig_Package).Decl.Arrays;
                     end if;

                     --  Now look for the array

                     while Orig_Array /= No_Array
                       and then In_Tree.Arrays.Table (Orig_Array).Name /=
                                                         Current_Item_Name
                     loop
                        Orig_Array := In_Tree.Arrays.Table
                                        (Orig_Array).Next;
                     end loop;

                     if Orig_Array = No_Array then
                        Error_Msg
                          (Flags,
                           "associative array value not found",
                           Location_Of (Current_Item, From_Project_Node_Tree),
                           Project);

                     else
                        Orig_Element :=
                          In_Tree.Arrays.Table (Orig_Array).Value;

                        --  Copy each array element

                        while Orig_Element /= No_Array_Element loop

                           --  Case of first element

                           if Prev_Element = No_Array_Element then

                              --  And there is no array element declared yet,
                              --  create a new first array element.

                              if In_Tree.Arrays.Table (New_Array).Value =
                                                              No_Array_Element
                              then
                                 Array_Element_Table.Increment_Last
                                   (In_Tree.Array_Elements);
                                 New_Element := Array_Element_Table.Last
                                   (In_Tree.Array_Elements);
                                 In_Tree.Arrays.Table
                                   (New_Array).Value := New_Element;
                                 Next_Element := No_Array_Element;

                              --  Otherwise, the new element is the first

                              else
                                 New_Element := In_Tree.Arrays.
                                                  Table (New_Array).Value;
                                 Next_Element :=
                                   In_Tree.Array_Elements.Table
                                     (New_Element).Next;
                              end if;

                           --  Otherwise, reuse an existing element, or create
                           --  one if necessary.

                           else
                              Next_Element :=
                                In_Tree.Array_Elements.Table
                                  (Prev_Element).Next;

                              if Next_Element = No_Array_Element then
                                 Array_Element_Table.Increment_Last
                                   (In_Tree.Array_Elements);
                                 New_Element :=
                                   Array_Element_Table.Last
                                    (In_Tree.Array_Elements);
                                 In_Tree.Array_Elements.Table
                                   (Prev_Element).Next := New_Element;

                              else
                                 New_Element := Next_Element;
                                 Next_Element :=
                                   In_Tree.Array_Elements.Table
                                     (New_Element).Next;
                              end if;
                           end if;

                           --  Copy the value of the element

                           In_Tree.Array_Elements.Table
                             (New_Element) :=
                               In_Tree.Array_Elements.Table (Orig_Element);
                           In_Tree.Array_Elements.Table
                             (New_Element).Value.Project := Project;

                           --  Adjust the Next link

                           In_Tree.Array_Elements.Table
                             (New_Element).Next := Next_Element;

                           --  Adjust the previous id for the next element

                           Prev_Element := New_Element;

                           --  Go to the next element in the original array

                           Orig_Element :=
                             In_Tree.Array_Elements.Table
                               (Orig_Element).Next;
                        end loop;

                        --  Make sure that the array ends here, in case there
                        --  previously a greater number of elements.

                        In_Tree.Array_Elements.Table
                          (New_Element).Next := No_Array_Element;
                     end if;
                  end;

               --  Declarations other that full associative arrays

               else
                  declare
                     New_Value : constant Variable_Value :=
                       Expression
                         (Project                => Project,
                          In_Tree                => In_Tree,
                          Flags                  => Flags,
                          From_Project_Node      => From_Project_Node,
                          From_Project_Node_Tree => From_Project_Node_Tree,
                          Pkg                    => Pkg,
                          First_Term             =>
                            Tree.First_Term
                              (Expression_Of
                                   (Current_Item, From_Project_Node_Tree),
                               From_Project_Node_Tree),
                          Kind                   =>
                            Expression_Kind_Of
                              (Current_Item, From_Project_Node_Tree));
                     --  The expression value

                     The_Variable : Variable_Id := No_Variable;

                     Current_Item_Name : constant Name_Id :=
                                           Name_Of
                                             (Current_Item,
                                              From_Project_Node_Tree);

                     Current_Location : constant Source_Ptr :=
                                          Location_Of
                                            (Current_Item,
                                             From_Project_Node_Tree);

                  begin
                     --  Process a typed variable declaration

                     if Kind_Of (Current_Item, From_Project_Node_Tree) =
                          N_Typed_Variable_Declaration
                     then
                        --  Report an error for an empty string

                        if New_Value.Value = Empty_String then
                           Error_Msg_Name_1 :=
                             Name_Of (Current_Item, From_Project_Node_Tree);
                           Error_Msg
                             (Flags,
                              "no value defined for %%",
                              Location_Of
                                (Current_Item, From_Project_Node_Tree),
                              Project);

                        else
                           declare
                              Current_String : Project_Node_Id;

                           begin
                              --  Loop through all the valid strings for the
                              --  string type and compare to the string value.

                              Current_String :=
                                First_Literal_String
                                  (String_Type_Of (Current_Item,
                                                   From_Project_Node_Tree),
                                                   From_Project_Node_Tree);
                              while Present (Current_String)
                                and then
                                  String_Value_Of
                                    (Current_String, From_Project_Node_Tree) /=
                                                               New_Value.Value
                              loop
                                 Current_String :=
                                   Next_Literal_String
                                     (Current_String, From_Project_Node_Tree);
                              end loop;

                              --  Report an error if the string value is not
                              --  one for the string type.

                              if No (Current_String) then
                                 Error_Msg_Name_1 := New_Value.Value;
                                 Error_Msg_Name_2 :=
                                   Name_Of
                                     (Current_Item, From_Project_Node_Tree);
                                 Error_Msg
                                   (Flags,
                                    "value %% is illegal for typed string %%",
                                    Location_Of
                                      (Current_Item, From_Project_Node_Tree),
                                    Project);
                              end if;
                           end;
                        end if;
                     end if;

                     --  Comment here ???

                     if Kind_Of (Current_Item, From_Project_Node_Tree) /=
                          N_Attribute_Declaration
                       or else
                         Associative_Array_Index_Of
                           (Current_Item, From_Project_Node_Tree) = No_Name
                     then
                        --  Case of a variable declaration or of a not
                        --  associative array attribute.

                        --  First, find the list where to find the variable
                        --  or attribute.

                        if Kind_Of (Current_Item, From_Project_Node_Tree) =
                             N_Attribute_Declaration
                        then
                           if Pkg /= No_Package then
                              The_Variable :=
                                In_Tree.Packages.Table
                                  (Pkg).Decl.Attributes;
                           else
                              The_Variable := Project.Decl.Attributes;
                           end if;

                        else
                           if Pkg /= No_Package then
                              The_Variable :=
                                In_Tree.Packages.Table
                                  (Pkg).Decl.Variables;
                           else
                              The_Variable := Project.Decl.Variables;
                           end if;

                        end if;

                        --  Loop through the list, to find if it has already
                        --  been declared.

                        while The_Variable /= No_Variable
                          and then
                            In_Tree.Variable_Elements.Table
                              (The_Variable).Name /= Current_Item_Name
                        loop
                           The_Variable :=
                             In_Tree.Variable_Elements.Table
                               (The_Variable).Next;
                        end loop;

                        --  If it has not been declared, create a new entry
                        --  in the list.

                        if The_Variable = No_Variable then

                           --  All single string attribute should already have
                           --  been declared with a default empty string value.

                           pragma Assert
                             (Kind_Of (Current_Item, From_Project_Node_Tree) /=
                                N_Attribute_Declaration,
                              "illegal attribute declaration for "
                              & Get_Name_String (Current_Item_Name));

                           Variable_Element_Table.Increment_Last
                             (In_Tree.Variable_Elements);
                           The_Variable := Variable_Element_Table.Last
                             (In_Tree.Variable_Elements);

                           --  Put the new variable in the appropriate list

                           if Pkg /= No_Package then
                              In_Tree.Variable_Elements.Table (The_Variable) :=
                                (Next   =>
                                   In_Tree.Packages.Table
                                     (Pkg).Decl.Variables,
                                 Name   => Current_Item_Name,
                                 Value  => New_Value);
                              In_Tree.Packages.Table
                                (Pkg).Decl.Variables := The_Variable;

                           else
                              In_Tree.Variable_Elements.Table (The_Variable) :=
                                (Next   => Project.Decl.Variables,
                                 Name   => Current_Item_Name,
                                 Value  => New_Value);
                              Project.Decl.Variables := The_Variable;
                           end if;

                        --  If the variable/attribute has already been
                        --  declared, just change the value.

                        else
                           In_Tree.Variable_Elements.Table
                             (The_Variable).Value := New_Value;
                        end if;

                     --  Associative array attribute

                     else
                        declare
                           Index_Name : Name_Id :=
                             Associative_Array_Index_Of
                               (Current_Item, From_Project_Node_Tree);
                           The_Array : Array_Id;
                           The_Array_Element : Array_Element_Id :=
                                                 No_Array_Element;

                        begin
                           if Index_Name /= All_Other_Names then
                              Index_Name := Get_Attribute_Index
                                (From_Project_Node_Tree,
                                 Current_Item,
                                 Associative_Array_Index_Of
                                   (Current_Item, From_Project_Node_Tree));
                           end if;

                           --  Look for the array in the appropriate list

                           if Pkg /= No_Package then
                              The_Array :=
                                In_Tree.Packages.Table (Pkg).Decl.Arrays;

                           else
                              The_Array := Project.Decl.Arrays;
                           end if;

                           while
                             The_Array /= No_Array
                               and then
                                 In_Tree.Arrays.Table (The_Array).Name /=
                                                            Current_Item_Name
                           loop
                              The_Array := In_Tree.Arrays.Table
                                             (The_Array).Next;
                           end loop;

                           --  If the array cannot be found, create a new entry
                           --  in the list. As The_Array_Element is initialized
                           --  to No_Array_Element, a new element will be
                           --  created automatically later

                           if The_Array = No_Array then
                              Array_Table.Increment_Last (In_Tree.Arrays);
                              The_Array := Array_Table.Last (In_Tree.Arrays);

                              if Pkg /= No_Package then
                                 In_Tree.Arrays.Table (The_Array) :=
                                   (Name     => Current_Item_Name,
                                    Location => Current_Location,
                                    Value    => No_Array_Element,
                                    Next     => In_Tree.Packages.Table
                                                  (Pkg).Decl.Arrays);

                                 In_Tree.Packages.Table (Pkg).Decl.Arrays :=
                                     The_Array;

                              else
                                 In_Tree.Arrays.Table (The_Array) :=
                                   (Name     => Current_Item_Name,
                                    Location => Current_Location,
                                    Value    => No_Array_Element,
                                    Next     => Project.Decl.Arrays);

                                 Project.Decl.Arrays := The_Array;
                              end if;

                           --  Otherwise initialize The_Array_Element as the
                           --  head of the element list.

                           else
                              The_Array_Element :=
                                In_Tree.Arrays.Table (The_Array).Value;
                           end if;

                           --  Look in the list, if any, to find an element
                           --  with the same index.

                           while The_Array_Element /= No_Array_Element
                             and then
                               In_Tree.Array_Elements.Table
                                 (The_Array_Element).Index /= Index_Name
                           loop
                              The_Array_Element :=
                                In_Tree.Array_Elements.Table
                                  (The_Array_Element).Next;
                           end loop;

                           --  If no such element were found, create a new one
                           --  and insert it in the element list, with the
                           --  proper value.

                           if The_Array_Element = No_Array_Element then
                              Array_Element_Table.Increment_Last
                                (In_Tree.Array_Elements);
                              The_Array_Element := Array_Element_Table.Last
                                (In_Tree.Array_Elements);

                              In_Tree.Array_Elements.Table
                                (The_Array_Element) :=
                                  (Index  => Index_Name,
                                   Src_Index =>
                                     Source_Index_Of
                                       (Current_Item, From_Project_Node_Tree),
                                   Index_Case_Sensitive =>
                                     not Case_Insensitive
                                       (Current_Item, From_Project_Node_Tree),
                                   Value  => New_Value,
                                   Next   => In_Tree.Arrays.Table
                                             (The_Array).Value);
                              In_Tree.Arrays.Table
                                (The_Array).Value := The_Array_Element;

                           --  An element with the same index already exists,
                           --  just replace its value with the new one.

                           else
                              In_Tree.Array_Elements.Table
                                (The_Array_Element).Value := New_Value;
                           end if;
                        end;
                     end if;
                  end;
               end if;

            when N_Case_Construction =>
               declare
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
                                          From_Project_Node_Tree);

                     Var_Id : Variable_Id := No_Variable;
                     Name   : Name_Id     := No_Name;

                  begin
                     --  If a project was specified for the case variable,
                     --  get its id.

                     if Present (Project_Node_Of
                                   (Variable_Node, From_Project_Node_Tree))
                     then
                        Name :=
                          Name_Of
                            (Project_Node_Of
                               (Variable_Node, From_Project_Node_Tree),
                             From_Project_Node_Tree);
                        The_Project :=
                          Imported_Or_Extended_Project_From (Project, Name);
                     end if;

                     --  If a package were specified for the case variable,
                     --  get its id.

                     if Present (Package_Node_Of
                                   (Variable_Node, From_Project_Node_Tree))
                     then
                        Name :=
                          Name_Of
                            (Package_Node_Of
                               (Variable_Node, From_Project_Node_Tree),
                             From_Project_Node_Tree);
                        The_Package :=
                          Package_From (The_Project, In_Tree, Name);
                     end if;

                     Name := Name_Of (Variable_Node, From_Project_Node_Tree);

                     --  First, look for the case variable into the package,
                     --  if any.

                     if The_Package /= No_Package then
                        Var_Id := In_Tree.Packages.Table
                                    (The_Package).Decl.Variables;
                        Name :=
                          Name_Of (Variable_Node, From_Project_Node_Tree);
                        while Var_Id /= No_Variable
                          and then
                            In_Tree.Variable_Elements.Table
                              (Var_Id).Name /= Name
                        loop
                           Var_Id := In_Tree.Variable_Elements.
                                       Table (Var_Id).Next;
                        end loop;
                     end if;

                     --  If not found in the package, or if there is no
                     --  package, look at the project level.

                     if Var_Id = No_Variable
                        and then
                        No (Package_Node_Of
                              (Variable_Node, From_Project_Node_Tree))
                     then
                        Var_Id := The_Project.Decl.Variables;
                        while Var_Id /= No_Variable
                          and then
                            In_Tree.Variable_Elements.Table
                              (Var_Id).Name /= Name
                        loop
                           Var_Id := In_Tree.Variable_Elements.
                                       Table (Var_Id).Next;
                        end loop;
                     end if;

                     if Var_Id = No_Variable then

                        --  Should never happen, because this has already been
                        --  checked during parsing.

                        Write_Line ("variable """ &
                                    Get_Name_String (Name) &
                                    """ not found");
                        raise Program_Error;
                     end if;

                     --  Get the case variable

                     The_Variable := In_Tree.Variable_Elements.
                                       Table (Var_Id).Value;

                     if The_Variable.Kind /= Single then

                        --  Should never happen, because this has already been
                        --  checked during parsing.

                        Write_Line ("variable""" &
                                    Get_Name_String (Name) &
                                    """ is not a single string variable");
                        raise Program_Error;
                     end if;

                     --  Get the case variable value
                     Case_Value := The_Variable.Value;
                  end;

                  --  Now look into all the case items of the case construction

                  Case_Item :=
                    First_Case_Item_Of (Current_Item, From_Project_Node_Tree);
                  Case_Item_Loop :
                     while Present (Case_Item) loop
                        Choice_String :=
                          First_Choice_Of (Case_Item, From_Project_Node_Tree);

                        --  When Choice_String is nil, it means that it is
                        --  the "when others =>" alternative.

                        if No (Choice_String) then
                           Decl_Item :=
                             First_Declarative_Item_Of
                               (Case_Item, From_Project_Node_Tree);
                           exit Case_Item_Loop;
                        end if;

                        --  Look into all the alternative of this case item

                        Choice_Loop :
                           while Present (Choice_String) loop
                              if Case_Value =
                                String_Value_Of
                                  (Choice_String, From_Project_Node_Tree)
                              then
                                 Decl_Item :=
                                   First_Declarative_Item_Of
                                     (Case_Item, From_Project_Node_Tree);
                                 exit Case_Item_Loop;
                              end if;

                              Choice_String :=
                                Next_Literal_String
                                  (Choice_String, From_Project_Node_Tree);
                           end loop Choice_Loop;

                        Case_Item :=
                          Next_Case_Item (Case_Item, From_Project_Node_Tree);
                     end loop Case_Item_Loop;

                  --  If there is an alternative, then we process it

                  if Present (Decl_Item) then
                     Process_Declarative_Items
                       (Project                => Project,
                        In_Tree                => In_Tree,
                        Flags                  => Flags,
                        From_Project_Node      => From_Project_Node,
                        From_Project_Node_Tree => From_Project_Node_Tree,
                        Pkg                    => Pkg,
                        Item                   => Decl_Item);
                  end if;
               end;

            when others =>

               --  Should never happen

               Write_Line ("Illegal declarative item: " &
                           Project_Node_Kind'Image
                             (Kind_Of
                                (Current_Item, From_Project_Node_Tree)));
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
      Success                : out Boolean;
      From_Project_Node      : Project_Node_Id;
      From_Project_Node_Tree : Project_Node_Tree_Ref;
      Flags                  : Processing_Flags;
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

      Recursive_Process
        (Project                => Project,
         In_Tree                => In_Tree,
         Flags                  => Flags,
         From_Project_Node      => From_Project_Node,
         From_Project_Node_Tree => From_Project_Node_Tree,
         Extended_By            => No_Project);

      Success :=
        Total_Errors_Detected = 0
          and then
            (Warning_Mode /= Treat_As_Error or else Warnings_Detected = 0);
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
      Flags                  : Processing_Flags)
   is
      Obj_Dir    : Path_Name_Type;
      Extending  : Project_Id;
      Extending2 : Project_Id;
      Prj        : Project_List;

   --  Start of processing for Process_Project_Tree_Phase_2

   begin
      Success := True;

      if Project /= No_Project then
         Check (In_Tree, Project, Flags);
      end if;

      --  If main project is an extending all project, set object directory of
      --  all virtual extending projects to object directory of main project.

      if Project /= No_Project
        and then
          Is_Extending_All (From_Project_Node, From_Project_Node_Tree)
      then
         declare
            Object_Dir : constant Path_Name_Type :=
                           Project.Object_Directory.Name;
         begin
            Prj := In_Tree.Projects;
            while Prj /= null loop
               if Prj.Project.Virtual then
                  Prj.Project.Object_Directory.Name := Object_Dir;
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
                          (Flags,
                           "project %% cannot be extended by a virtual" &
                           " project with the same object directory",
                           Prj.Project.Location, Project);

                     else
                        Error_Msg_Name_1 := Extending2.Display_Name;
                        Error_Msg_Name_2 := Prj.Project.Display_Name;
                        Error_Msg
                          (Flags,
                           "project %% cannot extend project %%",
                           Extending2.Location, Project);
                        Error_Msg
                          (Flags,
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

      Success :=
        Total_Errors_Detected = 0
          and then
            (Warning_Mode /= Treat_As_Error or else Warnings_Detected = 0);
   end Process_Project_Tree_Phase_2;

   -----------------------
   -- Recursive_Process --
   -----------------------

   procedure Recursive_Process
     (In_Tree                : Project_Tree_Ref;
      Project                : out Project_Id;
      Flags                  : Processing_Flags;
      From_Project_Node      : Project_Node_Id;
      From_Project_Node_Tree : Project_Node_Tree_Ref;
      Extended_By            : Project_Id)
   is
      procedure Process_Imported_Projects
        (Imported     : in out Project_List;
         Limited_With : Boolean);
      --  Process imported projects. If Limited_With is True, then only
      --  projects processed through a "limited with" are processed, otherwise
      --  only projects imported through a standard "with" are processed.
      --  Imported is the id of the last imported project.

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
                  Flags                  => Flags,
                  From_Project_Node      =>
                    Project_Node_Of
                      (With_Clause, From_Project_Node_Tree),
                  From_Project_Node_Tree => From_Project_Node_Tree,
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

            Project := new Project_Data'(Empty_Project);
            In_Tree.Projects := new Project_List_Element'
              (Project => Project,
               Next    => In_Tree.Projects);

            Processed_Projects.Set (Name, Project);

            Project.Name := Name;
            Project.Display_Name := Name_Node.Display_Name;
            Project.Qualifier :=
              Project_Qualifier_Of (From_Project_Node, From_Project_Node_Tree);

            Get_Name_String (Name);

            --  If name starts with the virtual prefix, flag the project as
            --  being a virtual extending project.

            if Name_Len > Virtual_Prefix'Length
              and then Name_Buffer (1 .. Virtual_Prefix'Length) =
                         Virtual_Prefix
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
               In_Tree,
               Project.Decl,
               Prj.Attr.Attribute_First,
               Project_Level => True);

            Process_Imported_Projects (Imported, Limited_With => False);

            Declaration_Node :=
              Project_Declaration_Of
                (From_Project_Node, From_Project_Node_Tree);

            Recursive_Process
              (In_Tree                => In_Tree,
               Project                => Project.Extends,
               Flags                  => Flags,
               From_Project_Node      => Extended_Project_Of
                                          (Declaration_Node,
                                           From_Project_Node_Tree),
               From_Project_Node_Tree => From_Project_Node_Tree,
               Extended_By            => Project);

            Process_Declarative_Items
              (Project                => Project,
               In_Tree                => In_Tree,
               Flags                  => Flags,
               From_Project_Node      => From_Project_Node,
               From_Project_Node_Tree => From_Project_Node_Tree,
               Pkg                    => No_Package,
               Item                   => First_Declarative_Item_Of
                                          (Declaration_Node,
                                           From_Project_Node_Tree));

            --  If it is an extending project, inherit all packages
            --  from the extended project that are not explicitly defined
            --  or renamed. Also inherit the languages, if attribute Languages
            --  is not explicitly defined.

            if Project.Extends /= No_Project then
               declare
                  Extended_Pkg : Package_Id;
                  Current_Pkg  : Package_Id;
                  Element      : Package_Element;
                  First        : constant Package_Id :=
                                   Project.Decl.Packages;
                  Attribute1   : Variable_Id;
                  Attribute2   : Variable_Id;
                  Attr_Value1  : Variable;
                  Attr_Value2  : Variable;

               begin
                  Extended_Pkg := Project.Extends.Decl.Packages;
                  while Extended_Pkg /= No_Package loop
                     Element := In_Tree.Packages.Table (Extended_Pkg);

                     Current_Pkg := First;
                     while Current_Pkg /= No_Package
                       and then In_Tree.Packages.Table (Current_Pkg).Name /=
                                                                 Element.Name
                     loop
                        Current_Pkg :=
                          In_Tree.Packages.Table (Current_Pkg).Next;
                     end loop;

                     if Current_Pkg = No_Package then
                        Package_Table.Increment_Last
                          (In_Tree.Packages);
                        Current_Pkg := Package_Table.Last (In_Tree.Packages);
                        In_Tree.Packages.Table (Current_Pkg) :=
                          (Name   => Element.Name,
                           Decl   => No_Declarations,
                           Parent => No_Package,
                           Next   => Project.Decl.Packages);
                        Project.Decl.Packages := Current_Pkg;
                        Copy_Package_Declarations
                          (From              => Element.Decl,
                           To                =>
                             In_Tree.Packages.Table (Current_Pkg).Decl,
                           New_Loc           => No_Location,
                           Naming_Restricted =>
                             Element.Name = Snames.Name_Naming,
                           In_Tree           => In_Tree);
                     end if;

                     Extended_Pkg := Element.Next;
                  end loop;

                  --  Check if attribute Languages is declared in the
                  --  extending project.

                  Attribute1 := Project.Decl.Attributes;
                  while Attribute1 /= No_Variable loop
                     Attr_Value1 := In_Tree.Variable_Elements.
                                      Table (Attribute1);
                     exit when Attr_Value1.Name = Snames.Name_Languages;
                     Attribute1 := Attr_Value1.Next;
                  end loop;

                  if Attribute1 = No_Variable or else
                     Attr_Value1.Value.Default
                  then
                     --  Attribute Languages is not declared in the extending
                     --  project. Check if it is declared in the project being
                     --  extended.

                     Attribute2 := Project.Extends.Decl.Attributes;
                     while Attribute2 /= No_Variable loop
                        Attr_Value2 := In_Tree.Variable_Elements.
                                         Table (Attribute2);
                        exit when Attr_Value2.Name = Snames.Name_Languages;
                        Attribute2 := Attr_Value2.Next;
                     end loop;

                     if Attribute2 /= No_Variable and then
                        not Attr_Value2.Value.Default
                     then
                        --  As attribute Languages is declared in the project
                        --  being extended, copy its value for the extending
                        --  project.

                        if Attribute1 = No_Variable then
                           Variable_Element_Table.Increment_Last
                             (In_Tree.Variable_Elements);
                           Attribute1 := Variable_Element_Table.Last
                             (In_Tree.Variable_Elements);
                           Attr_Value1.Next := Project.Decl.Attributes;
                           Project.Decl.Attributes := Attribute1;
                        end if;

                        Attr_Value1.Name := Snames.Name_Languages;
                        Attr_Value1.Value := Attr_Value2.Value;
                        In_Tree.Variable_Elements.Table
                          (Attribute1) := Attr_Value1;
                     end if;
                  end if;
               end;
            end if;

            Process_Imported_Projects (Imported, Limited_With => True);
         end;
      end if;
   end Recursive_Process;

end Prj.Proc;
