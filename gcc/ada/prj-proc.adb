------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              P R J . P R O C                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--          Copyright (C) 2001-2002 Free Software Foundation, Inc.          --
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

with Errout;   use Errout;
with Namet;    use Namet;
with Opt;
with Output;   use Output;
with Prj.Attr; use Prj.Attr;
with Prj.Com;  use Prj.Com;
with Prj.Ext;  use Prj.Ext;
with Prj.Nmsc; use Prj.Nmsc;
with Stringt;  use Stringt;

with GNAT.Case_Util; use GNAT.Case_Util;
with GNAT.HTable;

package body Prj.Proc is

   Error_Report : Put_Line_Access := null;

   package Processed_Projects is new GNAT.HTable.Simple_HTable
     (Header_Num => Header_Num,
      Element    => Project_Id,
      No_Element => No_Project,
      Key        => Name_Id,
      Hash       => Hash,
      Equal      => "=");
   --  This hash table contains all processed projects

   procedure Add (To_Exp : in out String_Id; Str : String_Id);
   --  Concatenate two strings and returns another string if both
   --  arguments are not null string.

   procedure Add_Attributes
     (Decl     : in out Declarations;
      First    : Attribute_Node_Id);
   --  Add all attributes, starting with First, with their default
   --  values to the package or project with declarations Decl.

   function Expression
     (Project           : Project_Id;
      From_Project_Node : Project_Node_Id;
      Pkg               : Package_Id;
      First_Term        : Project_Node_Id;
      Kind              : Variable_Kind)
      return              Variable_Value;
   --  From N_Expression project node From_Project_Node, compute the value
   --  of an expression and return it as a Variable_Value.

   function Imported_Or_Modified_Project_From
     (Project   : Project_Id;
      With_Name : Name_Id)
      return      Project_Id;
   --  Find an imported or modified project of Project whose name is With_Name

   function Package_From
     (Project   : Project_Id;
      With_Name : Name_Id)
      return      Package_Id;
   --  Find the package of Project whose name is With_Name

   procedure Process_Declarative_Items
     (Project           : Project_Id;
      From_Project_Node : Project_Node_Id;
      Pkg               : Package_Id;
      Item              : Project_Node_Id);
   --  Process declarative items starting with From_Project_Node, and put them
   --  in declarations Decl. This is a recursive procedure; it calls itself for
   --  a package declaration or a case construction.

   procedure Recursive_Process
     (Project           : out Project_Id;
      From_Project_Node : Project_Node_Id;
      Modified_By       : Project_Id);
   --  Process project with node From_Project_Node in the tree.
   --  Do nothing if From_Project_Node is Empty_Node.
   --  If project has already been processed, simply return its project id.
   --  Otherwise create a new project id, mark it as processed, call itself
   --  recursively for all imported projects and a modified project, if any.
   --  Then process the declarative items of the project.

   procedure Check (Project : in out Project_Id);
   --  Set all projects to not checked, then call Recursive_Check for the
   --  main project Project. Project is set to No_Project if errors occurred.

   procedure Recursive_Check (Project : Project_Id);
   --  If Project is marked as not checked, mark it as checked, call
   --  Check_Naming_Scheme for the project, then call itself for a
   --  possible modified project and all the imported projects of Project.

   ---------
   -- Add --
   ---------

   procedure Add (To_Exp : in out String_Id; Str : String_Id) is
   begin
      if To_Exp = Types.No_String or else String_Length (To_Exp) = 0 then

         --  To_Exp is nil or empty. The result is Str.

         To_Exp := Str;

      --  If Str is nil, then do not change To_Ext

      elsif Str /= No_String then
         Start_String (To_Exp);
         Store_String_Chars (Str);
         To_Exp := End_String;
      end if;
   end Add;

   --------------------
   -- Add_Attributes --
   --------------------

   procedure Add_Attributes
     (Decl           : in out Declarations;
      First          : Attribute_Node_Id) is
      The_Attribute  : Attribute_Node_Id := First;
      Attribute_Data : Attribute_Record;

   begin
      while The_Attribute /= Empty_Attribute loop
         Attribute_Data := Attributes.Table (The_Attribute);

         if Attribute_Data.Kind_2 /= Associative_Array then
            declare
               New_Attribute : Variable_Value;

            begin
               case Attribute_Data.Kind_1 is

                  --  Undefined should not happen

                  when Undefined =>
                     pragma Assert
                       (False, "attribute with an undefined kind");
                     raise Program_Error;

                  --  Single attributes have a default value of empty string

                  when Single =>
                     New_Attribute :=
                       (Kind     => Single,
                        Location => No_Location,
                        Default  => True,
                        Value    => Empty_String);

                  --  List attributes have a default value of nil list

                  when List =>
                     New_Attribute :=
                       (Kind     => List,
                        Location => No_Location,
                        Default  => True,
                        Values   => Nil_String);

               end case;

               Variable_Elements.Increment_Last;
               Variable_Elements.Table (Variable_Elements.Last) :=
                 (Next  => Decl.Attributes,
                  Name  => Attribute_Data.Name,
                  Value => New_Attribute);
               Decl.Attributes := Variable_Elements.Last;
            end;
         end if;

         The_Attribute := Attributes.Table (The_Attribute).Next;
      end loop;

   end Add_Attributes;

   -----------
   -- Check --
   -----------

   procedure Check (Project : in out Project_Id) is
   begin
      --  Make sure that all projects are marked as not checked

      for Index in 1 .. Projects.Last loop
         Projects.Table (Index).Checked := False;
      end loop;

      Recursive_Check (Project);

      if Errout.Total_Errors_Detected > 0 then
         Project := No_Project;
      end if;

   end Check;

   ----------------
   -- Expression --
   ----------------

   function Expression
     (Project           : Project_Id;
      From_Project_Node : Project_Node_Id;
      Pkg               : Package_Id;
      First_Term        : Project_Node_Id;
      Kind              : Variable_Kind)
      return              Variable_Value
   is
      The_Term : Project_Node_Id := First_Term;
      --  The term in the expression list

      The_Current_Term : Project_Node_Id := Empty_Node;
      --  The current term node id

      Term_Kind : Variable_Kind;
      --  The kind of the current term

      Result : Variable_Value (Kind => Kind);
      --  The returned result

      Last : String_List_Id := Nil_String;
      --  Reference to the last string elements in Result, when Kind is List.

   begin
      Result.Location := Location_Of (First_Term);

      --  Process each term of the expression, starting with First_Term

      while The_Term /= Empty_Node loop

         --  We get the term data and kind ...

         Term_Kind := Expression_Kind_Of (The_Term);

         The_Current_Term := Current_Term (The_Term);

         case Kind_Of (The_Current_Term) is

            when N_Literal_String =>

               case Kind is

                  when Undefined =>

                     --  Should never happen

                     pragma Assert (False, "Undefined expression kind");
                     raise Program_Error;

                  when Single =>
                     Add (Result.Value, String_Value_Of (The_Current_Term));

                  when List =>

                     String_Elements.Increment_Last;

                     if Last = Nil_String then

                        --  This can happen in an expression such as
                        --  () & "toto"

                        Result.Values := String_Elements.Last;

                     else
                        String_Elements.Table (Last).Next :=
                          String_Elements.Last;
                     end if;

                     Last := String_Elements.Last;
                     String_Elements.Table (Last) :=
                       (Value    => String_Value_Of (The_Current_Term),
                        Location => Location_Of (The_Current_Term),
                        Next     => Nil_String);

               end case;

            when N_Literal_String_List =>

               declare
                  String_Node : Project_Node_Id :=
                                  First_Expression_In_List (The_Current_Term);

                  Value : Variable_Value;

               begin
                  if String_Node /= Empty_Node then

                     --  If String_Node is nil, it is an empty list,
                     --  there is nothing to do

                     Value := Expression
                       (Project           => Project,
                        From_Project_Node => From_Project_Node,
                        Pkg               => Pkg,
                        First_Term        => Tree.First_Term (String_Node),
                        Kind              => Single);
                     String_Elements.Increment_Last;

                     if Result.Values = Nil_String then

                        --  This literal string list is the first term
                        --  in a string list expression

                        Result.Values := String_Elements.Last;

                     else
                        String_Elements.Table (Last).Next :=
                          String_Elements.Last;
                     end if;

                     Last := String_Elements.Last;
                     String_Elements.Table (Last) :=
                       (Value    => Value.Value,
                        Location => Value.Location,
                        Next     => Nil_String);

                     loop
                        --  Add the other element of the literal string list
                        --  one after the other

                        String_Node :=
                          Next_Expression_In_List (String_Node);

                        exit when String_Node = Empty_Node;

                        Value :=
                          Expression
                          (Project           => Project,
                           From_Project_Node => From_Project_Node,
                           Pkg               => Pkg,
                           First_Term        => Tree.First_Term (String_Node),
                           Kind              => Single);

                        String_Elements.Increment_Last;
                        String_Elements.Table (Last).Next :=
                          String_Elements.Last;
                        Last := String_Elements.Last;
                        String_Elements.Table (Last) :=
                          (Value    => Value.Value,
                           Location => Value.Location,
                           Next     => Nil_String);
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
                                      Project_Node_Of (The_Current_Term);
                  Term_Package    : constant Project_Node_Id :=
                                      Package_Node_Of (The_Current_Term);
                  Index           : String_Id   := No_String;

               begin
                  if Term_Project /= Empty_Node and then
                     Term_Project /= From_Project_Node
                  then
                     --  This variable or attribute comes from another project

                     The_Name := Name_Of (Term_Project);
                     The_Project := Imported_Or_Modified_Project_From
                       (Project => Project, With_Name => The_Name);
                  end if;

                  if Term_Package /= Empty_Node then

                     --  This is an attribute of a package

                     The_Name := Name_Of (Term_Package);
                     The_Package := Projects.Table (The_Project).Decl.Packages;

                     while The_Package /= No_Package
                       and then Packages.Table (The_Package).Name /= The_Name
                     loop
                        The_Package := Packages.Table (The_Package).Next;
                     end loop;

                     pragma Assert
                       (The_Package /= No_Package,
                        "package not found.");

                  elsif Kind_Of (The_Current_Term) = N_Attribute_Reference then
                     The_Package := No_Package;
                  end if;

                  The_Name := Name_Of (The_Current_Term);

                  if Kind_Of (The_Current_Term) = N_Attribute_Reference then
                     Index := Associative_Array_Index_Of (The_Current_Term);
                  end if;

                  --  If it is not an associative array attribute

                  if Index = No_String then

                     --  It is not an associative array attribute

                     if The_Package /= No_Package then

                        --  First, if there is a package, look into the package

                        if
                          Kind_Of (The_Current_Term) = N_Variable_Reference
                        then
                           The_Variable_Id :=
                             Packages.Table (The_Package).Decl.Variables;

                        else
                           The_Variable_Id :=
                             Packages.Table (The_Package).Decl.Attributes;
                        end if;

                        while The_Variable_Id /= No_Variable
                          and then
                          Variable_Elements.Table (The_Variable_Id).Name /=
                          The_Name
                        loop
                           The_Variable_Id :=
                             Variable_Elements.Table (The_Variable_Id).Next;
                        end loop;

                     end if;

                     if The_Variable_Id = No_Variable then

                        --  If we have not found it, look into the project

                        if
                          Kind_Of (The_Current_Term) = N_Variable_Reference
                        then
                           The_Variable_Id :=
                             Projects.Table (The_Project).Decl.Variables;

                        else
                           The_Variable_Id :=
                             Projects.Table (The_Project).Decl.Attributes;
                        end if;

                        while The_Variable_Id /= No_Variable
                          and then
                          Variable_Elements.Table (The_Variable_Id).Name /=
                          The_Name
                        loop
                           The_Variable_Id :=
                             Variable_Elements.Table (The_Variable_Id).Next;
                        end loop;

                     end if;

                     pragma Assert (The_Variable_Id /= No_Variable,
                                      "variable or attribute not found");

                     The_Variable := Variable_Elements.Table
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
                             Packages.Table (The_Package).Decl.Arrays;

                        else
                           The_Array :=
                             Projects.Table (The_Project).Decl.Arrays;
                        end if;

                        while The_Array /= No_Array
                          and then Arrays.Table (The_Array).Name /= The_Name
                        loop
                           The_Array := Arrays.Table (The_Array).Next;
                        end loop;

                        if The_Array /= No_Array then
                           The_Element := Arrays.Table (The_Array).Value;

                           String_To_Name_Buffer (Index);

                           if Case_Insensitive (The_Current_Term) then
                              To_Lower (Name_Buffer (1 .. Name_Len));
                           end if;

                           Array_Index := Name_Find;

                           while The_Element /= No_Array_Element
                             and then Array_Elements.Table (The_Element).Index
                                                         /= Array_Index
                           loop
                              The_Element :=
                                Array_Elements.Table (The_Element).Next;
                           end loop;

                        end if;

                        if The_Element /= No_Array_Element then
                           The_Variable :=
                             Array_Elements.Table (The_Element).Value;

                        else
                           if
                             Expression_Kind_Of (The_Current_Term) = List
                           then
                              The_Variable :=
                                (Kind     => List,
                                 Location => No_Location,
                                 Default  => True,
                                 Values   => Nil_String);

                           else
                              The_Variable :=
                                (Kind     => Single,
                                 Location => No_Location,
                                 Default  => True,
                                 Value    => Empty_String);
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
                              String_Elements.Increment_Last;

                              if Last = Nil_String then

                                 --  This can happen in an expression such as
                                 --  () & Var

                                 Result.Values := String_Elements.Last;

                              else
                                 String_Elements.Table (Last).Next :=
                                   String_Elements.Last;
                              end if;

                              Last := String_Elements.Last;
                              String_Elements.Table (Last) :=
                                (Value    => The_Variable.Value,
                                 Location => Location_Of (The_Current_Term),
                                 Next     => Nil_String);

                           when List =>

                              declare
                                 The_List : String_List_Id :=
                                              The_Variable.Values;

                              begin
                                 while The_List /= Nil_String loop
                                    String_Elements.Increment_Last;

                                    if Last = Nil_String then
                                       Result.Values := String_Elements.Last;

                                    else
                                       String_Elements.Table (Last).Next :=
                                         String_Elements.Last;

                                    end if;

                                    Last := String_Elements.Last;
                                    String_Elements.Table (Last) :=
                                      (Value    =>
                                         String_Elements.Table
                                                          (The_List).Value,
                                       Location => Location_Of
                                                          (The_Current_Term),
                                       Next     => Nil_String);
                                    The_List :=
                                      String_Elements.Table (The_List).Next;

                                 end loop;
                              end;
                        end case;
                  end case;
               end;

            when N_External_Value =>
               String_To_Name_Buffer
                 (String_Value_Of (External_Reference_Of (The_Current_Term)));

               declare
                  Name    : constant Name_Id  := Name_Find;
                  Default : String_Id         := No_String;
                  Value   : String_Id         := No_String;

                  Default_Node : constant Project_Node_Id :=
                                   External_Default_Of (The_Current_Term);

               begin
                  if Default_Node /= Empty_Node then
                     Default := String_Value_Of (Default_Node);
                  end if;

                  Value := Prj.Ext.Value_Of (Name, Default);

                  if Value = No_String then
                     if Error_Report = null then
                        Error_Msg
                          ("undefined external reference",
                           Location_Of (The_Current_Term));

                     else
                        Error_Report
                          ("""" & Get_Name_String (Name) &
                           """ is an undefined external reference",
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
                        String_Elements.Increment_Last;

                        if Last = Nil_String then
                           Result.Values := String_Elements.Last;

                        else
                           String_Elements.Table (Last).Next :=
                             String_Elements.Last;
                        end if;

                        Last := String_Elements.Last;
                        String_Elements.Table (Last) :=
                          (Value    => Value,
                           Location => Location_Of (The_Current_Term),
                           Next     => Nil_String);

                  end case;

               end;

            when others =>

               --  Should never happen

               pragma Assert
                 (False,
                  "illegal node kind in an expression");
               raise Program_Error;

         end case;

         The_Term := Next_Term (The_Term);
      end loop;

      return Result;
   end Expression;

   ---------------------------------------
   -- Imported_Or_Modified_Project_From --
   ---------------------------------------

   function Imported_Or_Modified_Project_From
     (Project   : Project_Id;
      With_Name : Name_Id)
      return      Project_Id
   is
      Data : constant Project_Data := Projects.Table (Project);
      List : Project_List          := Data.Imported_Projects;

   begin
      --  First check if it is the name of a modified project

      if Data.Modifies /= No_Project
        and then Projects.Table (Data.Modifies).Name = With_Name
      then
         return Data.Modifies;

      else
         --  Then check the name of each imported project

         while List /= Empty_Project_List
           and then
             Projects.Table
               (Project_Lists.Table (List).Project).Name /= With_Name

         loop
            List := Project_Lists.Table (List).Next;
         end loop;

         pragma Assert
           (List /= Empty_Project_List,
           "project not found");

         return Project_Lists.Table (List).Project;
      end if;
   end Imported_Or_Modified_Project_From;

   ------------------
   -- Package_From --
   ------------------

   function Package_From
     (Project   : Project_Id;
      With_Name : Name_Id)
      return      Package_Id
   is
      Data   : constant Project_Data := Projects.Table (Project);
      Result : Package_Id := Data.Decl.Packages;

   begin
      --  Check the name of each existing package of Project

      while Result /= No_Package
        and then
        Packages.Table (Result).Name /= With_Name
      loop
         Result := Packages.Table (Result).Next;
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
     (Project           : out Project_Id;
      From_Project_Node : Project_Node_Id;
      Report_Error      : Put_Line_Access)
   is
   begin
      Error_Report := Report_Error;

      --  Make sure there is no projects in the data structure

      Projects.Set_Last (No_Project);
      Processed_Projects.Reset;

      --  And process the main project and all of the projects it depends on,
      --  recursively

      Recursive_Process
        (Project           => Project,
         From_Project_Node => From_Project_Node,
         Modified_By       => No_Project);

      if Errout.Total_Errors_Detected > 0 then
         Project := No_Project;
      end if;

      if Project /= No_Project then
         Check (Project);
      end if;
   end Process;

   -------------------------------
   -- Process_Declarative_Items --
   -------------------------------

   procedure Process_Declarative_Items
     (Project           : Project_Id;
      From_Project_Node : Project_Node_Id;
      Pkg               : Package_Id;
      Item              : Project_Node_Id) is

      Current_Declarative_Item : Project_Node_Id := Item;

      Current_Item : Project_Node_Id := Empty_Node;

   begin
      --  For each declarative item

      while Current_Declarative_Item /= Empty_Node loop

         --  Get its data

         Current_Item := Current_Item_Node (Current_Declarative_Item);

         --  And set Current_Declarative_Item to the next declarative item
         --  ready for the next iteration

         Current_Declarative_Item := Next_Declarative_Item
                                            (Current_Declarative_Item);

         case Kind_Of (Current_Item) is

            when N_Package_Declaration =>
               Packages.Increment_Last;

               declare
                  New_Pkg         : constant Package_Id := Packages.Last;
                  The_New_Package : Package_Element;

                  Project_Of_Renamed_Package : constant Project_Node_Id :=
                                                 Project_Of_Renamed_Package_Of
                                                   (Current_Item);

               begin
                  The_New_Package.Name := Name_Of (Current_Item);

                  if Pkg /= No_Package then
                     The_New_Package.Next :=
                       Packages.Table (Pkg).Decl.Packages;
                     Packages.Table (Pkg).Decl.Packages := New_Pkg;
                  else
                     The_New_Package.Next :=
                       Projects.Table (Project).Decl.Packages;
                     Projects.Table (Project).Decl.Packages := New_Pkg;
                  end if;

                  Packages.Table (New_Pkg) := The_New_Package;

                  if Project_Of_Renamed_Package /= Empty_Node then

                     --  Renamed package

                     declare
                        Project_Name : constant Name_Id :=
                                         Name_Of
                                           (Project_Of_Renamed_Package);

                        Renamed_Project : constant Project_Id :=
                                            Imported_Or_Modified_Project_From
                                              (Project, Project_Name);

                        Renamed_Package : constant Package_Id :=
                                            Package_From
                                              (Renamed_Project,
                                               Name_Of (Current_Item));

                     begin
                        Packages.Table (New_Pkg).Decl :=
                          Packages.Table (Renamed_Package).Decl;
                     end;

                  else
                     --  Set the default values of the attributes

                     Add_Attributes
                       (Packages.Table (New_Pkg).Decl,
                        Package_Attributes.Table
                           (Package_Id_Of (Current_Item)).First_Attribute);

                     Process_Declarative_Items
                       (Project           => Project,
                        From_Project_Node => From_Project_Node,
                        Pkg               => New_Pkg,
                        Item              => First_Declarative_Item_Of
                                                             (Current_Item));
                  end if;

               end;

            when N_String_Type_Declaration =>

               --  There is nothing to process

               null;

            when N_Attribute_Declaration      |
                 N_Typed_Variable_Declaration |
                 N_Variable_Declaration       =>

                  pragma Assert (Expression_Of (Current_Item) /= Empty_Node,
                                 "no expression for an object declaration");

               declare
                  New_Value : constant Variable_Value :=
                                Expression
                                  (Project           => Project,
                                   From_Project_Node => From_Project_Node,
                                   Pkg               => Pkg,
                                   First_Term        =>
                                     Tree.First_Term (Expression_Of
                                                              (Current_Item)),
                                   Kind              =>
                                     Expression_Kind_Of (Current_Item));

                  The_Variable : Variable_Id := No_Variable;

                  Current_Item_Name : constant Name_Id :=
                                        Name_Of (Current_Item);

               begin
                  if Kind_Of (Current_Item) = N_Typed_Variable_Declaration then

                     if String_Equal (New_Value.Value, Empty_String) then
                        Error_Msg_Name_1 := Name_Of (Current_Item);

                        if Error_Report = null then
                           Error_Msg
                             ("no value defined for %",
                              Location_Of (Current_Item));

                        else
                           Error_Report
                             ("no value defined for " &
                              Get_Name_String (Error_Msg_Name_1),
                              Project);
                        end if;

                     else
                        declare
                           Current_String : Project_Node_Id :=
                                              First_Literal_String
                                                (String_Type_Of
                                                  (Current_Item));

                        begin
                           while Current_String /= Empty_Node
                             and then not String_Equal
                                            (String_Value_Of (Current_String),
                                             New_Value.Value)
                           loop
                              Current_String :=
                                Next_Literal_String (Current_String);
                           end loop;

                           if Current_String = Empty_Node then
                              String_To_Name_Buffer (New_Value.Value);
                              Error_Msg_Name_1 := Name_Find;
                              Error_Msg_Name_2 := Name_Of (Current_Item);

                              if Error_Report = null then
                                 Error_Msg
                                   ("value { is illegal for typed string %",
                                    Location_Of (Current_Item));

                              else
                                 Error_Report
                                   ("value """ &
                                    Get_Name_String (Error_Msg_Name_1) &
                                    """ is illegal for typed string """ &
                                    Get_Name_String (Error_Msg_Name_2) &
                                    """",
                                    Project);
                              end if;
                           end if;
                        end;
                     end if;
                  end if;

                  if Kind_Of (Current_Item) /= N_Attribute_Declaration
                    or else
                      Associative_Array_Index_Of (Current_Item) = No_String
                  then
                     --  Usual case

                     --  Code below really needs more comments ???

                     if Kind_Of (Current_Item) = N_Attribute_Declaration then
                        if Pkg /= No_Package then
                           The_Variable :=
                             Packages.Table (Pkg).Decl.Attributes;

                        else
                           The_Variable :=
                             Projects.Table (Project).Decl.Attributes;
                        end if;

                     else
                        if Pkg /= No_Package then
                           The_Variable :=
                             Packages.Table (Pkg).Decl.Variables;

                        else
                           The_Variable :=
                             Projects.Table (Project).Decl.Variables;
                        end if;

                     end if;

                     while
                       The_Variable /= No_Variable
                         and then
                           Variable_Elements.Table (The_Variable).Name /=
                                                          Current_Item_Name
                     loop
                        The_Variable :=
                          Variable_Elements.Table (The_Variable).Next;
                     end loop;

                     if The_Variable = No_Variable then
                        pragma Assert
                          (Kind_Of (Current_Item) /= N_Attribute_Declaration,
                           "illegal attribute declaration");

                        Variable_Elements.Increment_Last;
                        The_Variable := Variable_Elements.Last;

                        if Pkg /= No_Package then
                           Variable_Elements.Table (The_Variable) :=
                             (Next    =>
                                Packages.Table (Pkg).Decl.Variables,
                              Name    => Current_Item_Name,
                              Value   => New_Value);
                           Packages.Table (Pkg).Decl.Variables := The_Variable;

                        else
                           Variable_Elements.Table (The_Variable) :=
                             (Next    =>
                                Projects.Table (Project).Decl.Variables,
                              Name    => Current_Item_Name,
                              Value   => New_Value);
                           Projects.Table (Project).Decl.Variables :=
                             The_Variable;
                        end if;

                     else
                        Variable_Elements.Table (The_Variable).Value :=
                          New_Value;

                     end if;

                  else
                     --  Associative array attribute

                     String_To_Name_Buffer
                       (Associative_Array_Index_Of (Current_Item));

                     if Case_Insensitive (Current_Item) then
                        GNAT.Case_Util.To_Lower (Name_Buffer (1 .. Name_Len));
                     end if;

                     declare
                        The_Array : Array_Id;

                        The_Array_Element : Array_Element_Id :=
                                              No_Array_Element;

                        Index_Name : constant Name_Id := Name_Find;

                     begin

                        if Pkg /= No_Package then
                           The_Array := Packages.Table (Pkg).Decl.Arrays;

                        else
                           The_Array := Projects.Table (Project).Decl.Arrays;
                        end if;

                        while
                          The_Array /= No_Array
                            and then Arrays.Table (The_Array).Name /=
                                                           Current_Item_Name
                        loop
                           The_Array := Arrays.Table (The_Array).Next;
                        end loop;

                        if The_Array = No_Array then
                           Arrays.Increment_Last;
                           The_Array := Arrays.Last;

                           if Pkg /= No_Package then
                              Arrays.Table (The_Array) :=
                                (Name  => Current_Item_Name,
                                 Value => No_Array_Element,
                                 Next  => Packages.Table (Pkg).Decl.Arrays);
                              Packages.Table (Pkg).Decl.Arrays := The_Array;

                           else
                              Arrays.Table (The_Array) :=
                                (Name  => Current_Item_Name,
                                 Value => No_Array_Element,
                                 Next  =>
                                   Projects.Table (Project).Decl.Arrays);
                              Projects.Table (Project).Decl.Arrays :=
                                The_Array;
                           end if;

                        else
                           The_Array_Element := Arrays.Table (The_Array).Value;
                        end if;

                        while The_Array_Element /= No_Array_Element
                          and then
                            Array_Elements.Table (The_Array_Element).Index /=
                                                                  Index_Name
                        loop
                           The_Array_Element :=
                             Array_Elements.Table (The_Array_Element).Next;
                        end loop;

                        if The_Array_Element = No_Array_Element then
                           Array_Elements.Increment_Last;
                           The_Array_Element := Array_Elements.Last;
                           Array_Elements.Table (The_Array_Element) :=
                             (Index  => Index_Name,
                              Value  => New_Value,
                              Next   => Arrays.Table (The_Array).Value);
                           Arrays.Table (The_Array).Value := The_Array_Element;

                        else
                           Array_Elements.Table (The_Array_Element).Value :=
                             New_Value;
                        end if;
                     end;
                  end if;
               end;

            when N_Case_Construction =>
               declare
                  The_Project   : Project_Id      := Project;
                  The_Package   : Package_Id      := Pkg;
                  The_Variable  : Variable_Value  := Nil_Variable_Value;
                  Case_Value    : String_Id       := No_String;
                  Case_Item     : Project_Node_Id := Empty_Node;
                  Choice_String : Project_Node_Id := Empty_Node;
                  Decl_Item     : Project_Node_Id := Empty_Node;

               begin
                  declare
                     Variable_Node : constant Project_Node_Id :=
                                       Case_Variable_Reference_Of
                                         (Current_Item);

                     Var_Id : Variable_Id := No_Variable;
                     Name   : Name_Id     := No_Name;

                  begin
                     if Project_Node_Of (Variable_Node) /= Empty_Node then
                        Name := Name_Of (Project_Node_Of (Variable_Node));
                        The_Project :=
                          Imported_Or_Modified_Project_From (Project, Name);
                     end if;

                     if Package_Node_Of (Variable_Node) /= Empty_Node then
                        Name := Name_Of (Package_Node_Of (Variable_Node));
                        The_Package := Package_From (The_Project, Name);
                     end if;

                     Name := Name_Of (Variable_Node);

                     if The_Package /= No_Package then
                        Var_Id := Packages.Table (The_Package).Decl.Variables;
                        Name := Name_Of (Variable_Node);
                        while Var_Id /= No_Variable
                          and then
                            Variable_Elements.Table (Var_Id).Name /= Name
                        loop
                           Var_Id := Variable_Elements.Table (Var_Id).Next;
                        end loop;
                     end if;

                     if Var_Id = No_Variable
                       and then Package_Node_Of (Variable_Node) = Empty_Node
                     then
                        Var_Id := Projects.Table (The_Project).Decl.Variables;
                        while Var_Id /= No_Variable
                          and then
                            Variable_Elements.Table (Var_Id).Name /= Name
                        loop
                           Var_Id := Variable_Elements.Table (Var_Id).Next;
                        end loop;
                     end if;

                     if Var_Id = No_Variable then

                        --  Should never happen

                        Write_Line ("variable """ &
                                    Get_Name_String (Name) &
                                    """ not found");
                        raise Program_Error;
                     end if;

                     The_Variable := Variable_Elements.Table (Var_Id).Value;

                     if The_Variable.Kind /= Single then

                        --  Should never happen

                        Write_Line ("variable""" &
                                    Get_Name_String (Name) &
                                    """ is not a single string variable");
                        raise Program_Error;
                     end if;

                     Case_Value := The_Variable.Value;
                  end;

                  Case_Item := First_Case_Item_Of (Current_Item);
                  Case_Item_Loop :
                     while Case_Item /= Empty_Node loop
                        Choice_String := First_Choice_Of (Case_Item);

                        if Choice_String = Empty_Node then
                           Decl_Item := First_Declarative_Item_Of (Case_Item);
                           exit Case_Item_Loop;
                        end if;

                        Choice_Loop :
                           while Choice_String /= Empty_Node loop
                              if String_Equal (Case_Value,
                                               String_Value_Of (Choice_String))
                              then
                                 Decl_Item :=
                                   First_Declarative_Item_Of (Case_Item);
                                 exit Case_Item_Loop;
                              end if;

                              Choice_String :=
                                Next_Literal_String (Choice_String);
                           end loop Choice_Loop;
                        Case_Item := Next_Case_Item (Case_Item);
                     end loop Case_Item_Loop;

                  if Decl_Item /= Empty_Node then
                     Process_Declarative_Items
                       (Project           => Project,
                        From_Project_Node => From_Project_Node,
                        Pkg               => Pkg,
                        Item              => Decl_Item);
                  end if;
               end;

            when others =>

               --  Should never happen

               Write_Line ("Illegal declarative item: " &
                           Project_Node_Kind'Image (Kind_Of (Current_Item)));
               raise Program_Error;
         end case;
      end loop;
   end Process_Declarative_Items;

   ---------------------
   -- Recursive_Check --
   ---------------------

   procedure Recursive_Check (Project : Project_Id) is
      Data                  : Project_Data;
      Imported_Project_List : Project_List := Empty_Project_List;

   begin
      --  Do nothing if Project is No_Project, or Project has already
      --  been marked as checked.

      if Project /= No_Project
        and then not Projects.Table (Project).Checked
      then
         Data := Projects.Table (Project);

         --  Call itself for a possible modified project.
         --  (if there is no modified project, then nothing happens).

         Recursive_Check (Data.Modifies);

         --  Call itself for all imported projects

         Imported_Project_List := Data.Imported_Projects;
         while Imported_Project_List /= Empty_Project_List loop
            Recursive_Check
              (Project_Lists.Table (Imported_Project_List).Project);
            Imported_Project_List :=
              Project_Lists.Table (Imported_Project_List).Next;
         end loop;

         --  Mark project as checked

         Projects.Table (Project).Checked := True;

         if Opt.Verbose_Mode then
            Write_Str ("Checking project file """);
            Write_Str (Get_Name_String (Data.Name));
            Write_Line ("""");
         end if;

         Prj.Nmsc.Ada_Check (Project, Error_Report);
      end if;
   end Recursive_Check;

   -----------------------
   -- Recursive_Process --
   -----------------------

   procedure Recursive_Process
     (Project           : out Project_Id;
      From_Project_Node : Project_Node_Id;
      Modified_By       : Project_Id)
   is
      With_Clause : Project_Node_Id;

   begin
      if From_Project_Node = Empty_Node then
         Project := No_Project;

      else
         declare
            Processed_Data   : Project_Data := Empty_Project;
            Imported         : Project_List := Empty_Project_List;
            Declaration_Node : Project_Node_Id := Empty_Node;
            Name             : constant Name_Id :=
                                 Name_Of (From_Project_Node);

         begin
            Project := Processed_Projects.Get (Name);

            if Project /= No_Project then
               return;
            end if;

            Projects.Increment_Last;
            Project := Projects.Last;
            Processed_Projects.Set (Name, Project);

            Processed_Data.Name        := Name;
            Processed_Data.Path_Name   := Path_Name_Of (From_Project_Node);
            Processed_Data.Location    := Location_Of (From_Project_Node);
            Processed_Data.Directory   := Directory_Of (From_Project_Node);
            Processed_Data.Modified_By := Modified_By;
            Processed_Data.Naming      := Standard_Naming_Data;

            Add_Attributes (Processed_Data.Decl, Attribute_First);
            With_Clause := First_With_Clause_Of (From_Project_Node);

            while With_Clause /= Empty_Node loop
               declare
                  New_Project : Project_Id;
                  New_Data    : Project_Data;

               begin
                  Recursive_Process
                    (Project           => New_Project,
                     From_Project_Node => Project_Node_Of (With_Clause),
                     Modified_By       => No_Project);
                  New_Data := Projects.Table (New_Project);

                  --  If we were the first project to import it,
                  --  set First_Referred_By to us.

                  if New_Data.First_Referred_By = No_Project then
                     New_Data.First_Referred_By := Project;
                     Projects.Table (New_Project) := New_Data;
                  end if;

                  --  Add this project to our list of imported projects

                  Project_Lists.Increment_Last;
                  Project_Lists.Table (Project_Lists.Last) :=
                    (Project => New_Project, Next => Empty_Project_List);

                  --  Imported is the id of the last imported project.
                  --  If it is nil, then this imported project is our first.

                  if Imported = Empty_Project_List then
                     Processed_Data.Imported_Projects := Project_Lists.Last;

                  else
                     Project_Lists.Table (Imported).Next := Project_Lists.Last;
                  end if;

                  Imported := Project_Lists.Last;

                  With_Clause := Next_With_Clause_Of (With_Clause);
               end;
            end loop;

            Declaration_Node := Project_Declaration_Of (From_Project_Node);

            Recursive_Process
              (Project           => Processed_Data.Modifies,
               From_Project_Node => Modified_Project_Of (Declaration_Node),
               Modified_By       => Project);

            Projects.Table (Project) := Processed_Data;

            Process_Declarative_Items
              (Project           => Project,
               From_Project_Node => From_Project_Node,
               Pkg               => No_Package,
               Item              => First_Declarative_Item_Of
                                      (Declaration_Node));

         end;
      end if;
   end Recursive_Process;

end Prj.Proc;
