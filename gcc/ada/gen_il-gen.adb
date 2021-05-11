------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                            G E N _ I L . G E N                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2020-2021, Free Software Foundation, Inc.         --
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

with Ada.Containers; use type Ada.Containers.Count_Type;

package body Gen_IL.Gen is

   Enable_Assertions : constant Boolean := True;
   --  True to enable predicates on the _Id types, and preconditions on getters
   --  and setters.

   Overlay_Fields : constant Boolean := True;
   --  False to allocate every field so it doesn't overlay any other fields,
   --  which results in enormous nodes. For experimenting and debugging.
   --  Should be True in normal operation, for efficiency.

   Inline : constant String := "Inline";
   --  For experimenting with Inline_Always

   Is_Syntactic : Fields_Per_Node_Type :=
     (others => (others => False));

   Nodes_And_Entities : constant Type_Vector := Node_Kind & Entity_Kind;
   All_Entities : constant Type_Vector := To_Vector (Entity_Kind, Length => 1);

   procedure Create_Type
     (T : Node_Or_Entity_Type; Parent : Opt_Abstract_Type;
      Fields : Field_Sequence);
   --  Called by the Create_..._Type procedures exported by this package to
   --  create an entry in the Types_Table.

   procedure Create_Union_Type
     (Root : Root_Type; T : Abstract_Type; Children : Type_Array);
   --  Called by Create_Node_Union and Create_Entity_Union to create a union
   --  type.

   function Create_Field
     (Field      : Field_Enum;
      Field_Type : Type_Enum;
      Default_Value : Field_Default_Value;
      Type_Only  : Type_Only_Enum;
      Pre        : String;
      Is_Syntactic : Boolean) return Field_Desc;
   --  Called by the Create_..._Field functions exported by this package to
   --  create an entry in the Field_Table. See Create_Syntactic_Field and
   --  Create_Semantic_Field for additional doc.

   procedure Check_Type (T : Node_Or_Entity_Type);
   --  Check some "legality" rules

   procedure Check_Type (T : Node_Or_Entity_Type) is
      Im : constant String := Node_Or_Entity_Type'Image (T);
   begin
      if Type_Table (T) /= null then
         raise Illegal with "duplicate creation of type " & Image (T);
      end if;

      if T not in Root_Type then
         case T is
            when Node_Type =>
               if Im'Length < 2 or else Im (1 .. 2) /= "N_" then
                  raise Illegal with "Node type names must start with ""N_""";
               end if;

            when Concrete_Entity =>
               if Im'Length < 2 or else Im (1 .. 2) /= "E_" then
                  raise Illegal with
                    "Concrete entity type names must start with ""E_""";
               end if;

            when others => null;
               --  No special prefix for abstract entities
         end case;
      end if;
   end Check_Type;

   procedure Create_Type
     (T : Node_Or_Entity_Type; Parent : Opt_Abstract_Type;
      Fields : Field_Sequence)
   is
   begin
      Check_Type (T);

      if T not in Root_Type then
         if Type_Table (Parent) = null then
            raise Illegal with
              "undefined parent type for " &
              Image (T) & " (parent is " & Image (Parent) & ")";
         end if;

         if Type_Table (Parent).Is_Union then
            raise Illegal with
              "parent type for " &
                Image (T) & " must not be union (" & Image (Parent) & ")";
         end if;
      end if;

      Type_Table (T) :=
        new Type_Info'
          (Is_Union => False, Parent => Parent,
           Children | Concrete_Descendants => Type_Vectors.Empty_Vector,
           First | Last | Fields => <>,
           Allow_Overlap => False);

      if Parent /= No_Type then
         Append (Type_Table (Parent).Children, T);
      end if;

      --  Check that syntactic fields precede semantic fields. Note that this
      --  check is happening before we compute inherited fields.
      --  ????Exempt Chars and Actions from this rule, for now.

      declare
         Semantic_Seen : Boolean := False;
      begin
         for J in Fields'Range loop
            if Fields (J).Is_Syntactic then
               if Semantic_Seen then
                  raise Illegal with
                    "syntactic fields must precede semantic ones " & Image (T);
               end if;

            else
               if Fields (J).F not in Chars | Actions then
                  Semantic_Seen := True;
               end if;
            end if;
         end loop;
      end;

      for J in Fields'Range loop
         declare
            Field : constant Field_Enum := Fields (J).F;
            Is_Syntactic : constant Boolean := Fields (J).Is_Syntactic;

         begin
            Append (Field_Table (Field).Have_This_Field, T);
            Append (Type_Table (T).Fields, Field);

            pragma Assert (not Gen.Is_Syntactic (T) (Field));
            Gen.Is_Syntactic (T) (Field) := Is_Syntactic;
         end;
      end loop;
   end Create_Type;

   --  Other than constraint checks on T at the call site, and the lack of a
   --  parent for root types, the following six all do the same thing.

   procedure Create_Root_Node_Type
     (T : Abstract_Node;
      Fields : Field_Sequence := No_Fields) is
   begin
      Create_Type (T, Parent => No_Type, Fields => Fields);
   end Create_Root_Node_Type;

   procedure Create_Abstract_Node_Type
     (T : Abstract_Node; Parent : Abstract_Type;
      Fields : Field_Sequence := No_Fields)
   is
   begin
      Create_Type (T, Parent, Fields);
   end Create_Abstract_Node_Type;

   procedure Create_Concrete_Node_Type
     (T : Concrete_Node; Parent : Abstract_Type;
      Fields : Field_Sequence := No_Fields)
   is
   begin
      Create_Type (T, Parent, Fields);
   end Create_Concrete_Node_Type;

   procedure Create_Root_Entity_Type
     (T : Abstract_Entity;
      Fields : Field_Sequence := No_Fields) is
   begin
      Create_Type (T, Parent => No_Type, Fields => Fields);
   end Create_Root_Entity_Type;

   procedure Create_Abstract_Entity_Type
     (T : Abstract_Entity; Parent : Abstract_Type;
      Fields : Field_Sequence := No_Fields)
   is
   begin
      Create_Type (T, Parent, Fields);
   end Create_Abstract_Entity_Type;

   procedure Create_Concrete_Entity_Type
     (T : Concrete_Entity; Parent : Abstract_Type;
      Fields : Field_Sequence := No_Fields)
   is
   begin
      Create_Type (T, Parent, Fields);
   end Create_Concrete_Entity_Type;

   function Create_Field
     (Field      : Field_Enum;
      Field_Type : Type_Enum;
      Default_Value : Field_Default_Value;
      Type_Only  : Type_Only_Enum;
      Pre        : String;
      Is_Syntactic : Boolean) return Field_Desc
   is
   begin
      pragma Assert (if Default_Value /= No_Default then Is_Syntactic);
      pragma Assert (if Type_Only /= No_Type_Only then not Is_Syntactic);

      if Field_Table (Field) = null then
         Field_Table (Field) := new Field_Info'
           (Type_Vectors.Empty_Vector, Field_Type, Default_Value, Type_Only,
            Pre => new String'(Pre), Offset => <>);

      else
         if Field_Type /= Field_Table (Field).Field_Type then
            raise Illegal with
              "mismatched field types for " & Image (Field);
         end if;

         --  Check that default values for syntactic fields match. This check
         --  could be stricter; it currently allows a field to have No_Default
         --  in one type, but something else in another type. In that case, we
         --  use the "something else" for all types.

         if Is_Syntactic then
            if Default_Value /= Field_Table (Field).Default_Value then
               if Field_Table (Field).Default_Value = No_Default then
                  Field_Table (Field).Default_Value := Default_Value;
               else
                  raise Illegal with
                    "mismatched default values for " & Image (Field);
               end if;
            end if;
         end if;

         if Type_Only /= Field_Table (Field).Type_Only then
            raise Illegal with "mismatched Type_Only for " & Image (Field);
         end if;

         if Pre /= Field_Table (Field).Pre.all then
            raise Illegal with
              "mismatched extra preconditions for " & Image (Field);
         end if;
      end if;

      return (Field, Is_Syntactic);
   end Create_Field;

   function Create_Syntactic_Field
     (Field      : Node_Field;
      Field_Type : Type_Enum;
      Default_Value : Field_Default_Value := No_Default;
      Pre        : String := "") return Field_Desc
   is
   begin
      return Create_Field
        (Field, Field_Type, Default_Value, No_Type_Only, Pre,
         Is_Syntactic => True);
   end Create_Syntactic_Field;

   function Create_Semantic_Field
     (Field      : Field_Enum;
      Field_Type : Type_Enum;
      Type_Only  : Type_Only_Enum := No_Type_Only;
      Pre        : String := "") return Field_Desc
   is
   begin
      return Create_Field
        (Field, Field_Type, No_Default, Type_Only, Pre, Is_Syntactic => False);
   end Create_Semantic_Field;

   procedure Create_Union_Type
     (Root : Root_Type; T : Abstract_Type; Children : Type_Array)
   is
      Children_Seen : Type_Set := (others => False);

   begin
      Check_Type (T);

      if Children'Length <= 1 then
         raise Illegal with Image (T) & " must have two or more children";
      end if;

      for Child of Children loop
         if Children_Seen (Child) then
            raise Illegal with
              Image (T) & " has duplicate child " & Image (Child);
         end if;

         Children_Seen (Child) := True;

         if Type_Table (Child) = null then
            raise Illegal with
              "undefined child type for " &
              Image (T) & " (child is " & Image (Child) & ")";
         end if;
      end loop;

      Type_Table (T) :=
        new Type_Info'
          (Is_Union => True, Parent => Root,
           Children | Concrete_Descendants => Type_Vectors.Empty_Vector);

      for Child of Children loop
         Append (Type_Table (T).Children, Child);
      end loop;
   end Create_Union_Type;

   procedure Create_Node_Union (T : Abstract_Node; Children : Type_Array) is
   begin
      Create_Union_Type (Node_Kind, T, Children);
   end Create_Node_Union;

   procedure Create_Entity_Union
     (T : Abstract_Entity; Children : Type_Array) is
   begin
      Create_Union_Type (Entity_Kind, T, Children);
   end Create_Entity_Union;

   procedure Compile is
      Fields_Per_Node : Fields_Per_Node_Type := (others => (others => False));

      Type_Bit_Size : array (Concrete_Type) of Bit_Offset := (others => 0);
      Min_Node_Bit_Size : Bit_Offset := Bit_Offset'Last;
      Max_Node_Bit_Size : Bit_Offset := 0;
      Min_Entity_Bit_Size : Bit_Offset := Bit_Offset'Last;
      Max_Entity_Bit_Size : Bit_Offset := 0;
      --  Above are in units of bits; following are in units of slots:
      Min_Node_Size : Field_Offset := Field_Offset'Last;
      Max_Node_Size : Field_Offset := 0;
      Min_Entity_Size : Field_Offset := Field_Offset'Last;
      Max_Entity_Size : Field_Offset := 0;

      Average_Node_Size_In_Slots : Long_Float;

      Node_Field_Types_Used, Entity_Field_Types_Used : Type_Set;

      Setter_Needs_Parent : Field_Set :=
        (Actions | Expression | Else_Actions => True, others => False);
      --  Set of fields where the setter should set the Parent. True for
      --  syntactic fields of type Node_Id and List_Id, but with some
      --  exceptions. Expression and Else_Actions are syntactic AND semantic,
      --  and the Parent is needed.  Default_Expression is also both, but the
      --  Parent is not needed.  Else_Actions is not syntactic, but the Parent
      --  is needed.

      procedure Check_Completeness;
      --  Check that every type and field has been declared

      procedure Compute_Ranges (Root : Root_Type);
      --  Compute the range of Node_Kind/Entity_Kind values for all the types
      --  rooted at Root.

      procedure Compute_Fields_Per_Node;
      --  Compute which fields are in which nodes. Implements inheritance of
      --  fields. Set the Fields component of each Type_Info to include
      --  inherited ones. Set the Is_Syntactic component to the set of fields
      --  that are syntactic in that node kind. Set the Fields_Per_Node table.

      procedure Compute_Field_Offsets;
      --  Compute the offsets of each field.

      procedure Compute_Type_Sizes;
      --  Compute the size of each node and entity type, which is one more than
      --  the maximum bit offset of all fields of the type. Results are
      --  returned in the above Type_Bit_Size and Min_.../Max_... variables.

      procedure Check_For_Syntactic_Mismatch;
      --  Check that fields are either all syntactic or all semantic in all
      --  nodes in which they exist, except for some fields that are
      --  grandfathered in.
      --
      --  Also sets Setter_Needs_Parent.

      function Field_Types_Used (First, Last : Field_Enum) return Type_Set;
      --  Returns the union of the types of all the fields in the range First
      --  .. Last. Only Special_Type; if the declared type of a field is a
      --  descendant of Node_Kind or Entity_Kind, then the low-level getter for
      --  Node_Id can be used.

      procedure Put_Seinfo;
      --  Print out the Seinfo package, which is with'ed by both Sinfo.Nodes
      --  and Einfo.Entities.

      procedure Put_Nodes;
      --  Print out the Sinfo.Nodes package spec and body

      procedure Put_Entities;
      --  Print out the Einfo.Entities package spec and body

      procedure Put_Type_And_Subtypes
        (S : in out Sink'Class; Root : Root_Type);
      --  Called by Put_Nodes and Put_Entities to print out the main type
      --  and subtype declarations in Sinfo.Nodes and Einfo.Entities.

      procedure Put_Subp_Decls (S : in out Sink'Class; Root : Root_Type);
      --  Called by Put_Nodes and Put_Entities to print out the subprogram
      --  declarations in Sinfo.Nodes and Einfo.Entities.

      procedure Put_Subp_Bodies (S : in out Sink'Class; Root : Root_Type);
      --  Called by Put_Nodes and Put_Entities to print out the subprogram
      --  bodies in Sinfo.Nodes and Einfo.Entities.

      function Node_To_Fetch_From (F : Field_Enum) return String;
      --  Node from which a getter should fetch the value.
      --  Normally, we fetch from the node or entity passed in (i.e. formal
      --  parameter N). But if Type_Only was specified, we need to fetch the
      --  corresponding base (etc) type.
      --  ????We should not allocate space in the node for subtypes (etc), but
      --  that's not necessary for it to work.

      procedure Put_Getter_Spec (S : in out Sink'Class; F : Field_Enum);
      procedure Put_Setter_Spec (S : in out Sink'Class; F : Field_Enum);
      procedure Put_Getter_Decl (S : in out Sink'Class; F : Field_Enum);
      procedure Put_Setter_Decl (S : in out Sink'Class; F : Field_Enum);
      procedure Put_Getter_Body (S : in out Sink'Class; F : Field_Enum);
      procedure Put_Setter_Body (S : in out Sink'Class; F : Field_Enum);
      --  Print out the specification, declaration, or body of a getter or
      --  setter for the given field.

      procedure Put_Precondition
        (S : in out Sink'Class; F : Field_Enum);
      --  Print out the precondition, if any, for a getter or setter for the
      --  given field.

      procedure Instantiate_Low_Level_Accessors
        (S : in out Sink'Class; T : Type_Enum);
      --  Print out the low-level getter and setter for a given type

      procedure Put_Traversed_Fields (S : in out Sink'Class);
      --  Called by Put_Nodes to print out the Traversed_Fields table in
      --  Sinfo.Nodes.

      procedure Put_Tables (S : in out Sink'Class; Root : Root_Type);
      --  Called by Put_Nodes and Put_Entities to print out the various tables
      --  in Sinfo.Nodes and Einfo.Entities.

      procedure Put_Nmake;
      --  Print out the Nmake package spec and body, containing
      --  Make_... functions for each concrete node type.

      procedure Put_Make_Decls (S : in out Sink'Class; Root : Root_Type);
      --  Called by Put_Nmake to print out the Make_... function declarations

      procedure Put_Make_Bodies (S : in out Sink'Class; Root : Root_Type);
      --  Called by Put_Nmake to print out the Make_... function bodies

      procedure Put_Make_Spec
        (S : in out Sink'Class; Root : Root_Type; T : Concrete_Type);
      --  Called by Put_Make_Decls and Put_Make_Bodies to print out the spec of
      --  a single Make_... function.

      procedure Put_Seinfo_Tables;
      --  This puts information about both sinfo and einfo.
      --  Not actually needed by the compiler.

      procedure Put_Sinfo_Dot_H;
      --  Print out the sinfo.h file

      procedure Put_Einfo_Dot_H;
      --  Print out the einfo.h file

      procedure Put_C_Type_And_Subtypes
        (S : in out Sink'Class; Root : Root_Type);
      --  Used by Put_Sinfo_Dot_H and Put_Einfo_Dot_H to print out the C code
      --  corresponding to the Ada Node_Kind, Entity_Kind, and subtypes
      --  thereof.

      procedure Put_Low_Level_C_Getter
        (S : in out Sink'Class; T : Type_Enum);
      --  Used by Put_Sinfo_Dot_H and Put_Einfo_Dot_H to print out low-level
      --  getters.

      procedure Put_High_Level_C_Getters
        (S : in out Sink'Class; Root : Root_Type);
      --  Used by Put_Sinfo_Dot_H and Put_Einfo_Dot_H to print out high-level
      --  getters.

      procedure Put_High_Level_C_Getter
        (S : in out Sink'Class; F : Field_Enum);
      --  Used by Put_High_Level_C_Getters to print out one high-level getter.

      procedure Put_Union_Membership
        (S : in out Sink'Class; Root : Root_Type);
      --  Used by Put_Sinfo_Dot_H and Put_Einfo_Dot_H to print out functions to
      --  test membership in a union type.

      procedure Check_Completeness is
      begin
         for T in Node_Or_Entity_Type loop
            if Type_Table (T) = null and then T not in Boundaries then
               raise Illegal with "Missing type declaration for " & Image (T);
            end if;
         end loop;

         for F in Field_Enum loop
            if Field_Table (F) = null
              and then F /= Between_Node_And_Entity_Fields
            then
               raise Illegal with "Missing field declaration for " & Image (F);
            end if;
         end loop;
      end Check_Completeness;

      procedure Compute_Ranges (Root : Root_Type) is

         procedure Do_One_Type (T : Node_Or_Entity_Type);
         --  Compute the range for one type. Passed to Iterate_Types to process
         --  all of them.

         procedure Add_Concrete_Descendant
           (Ancestor : Abstract_Type; Descendant : Concrete_Type);
         --  Add Descendant to the Concrete_Descendants of each of its
         --  ancestors.

         procedure Add_Concrete_Descendant
           (Ancestor : Abstract_Type; Descendant : Concrete_Type) is
         begin
            if Ancestor not in Root_Type then
               Add_Concrete_Descendant
                 (Type_Table (Ancestor).Parent, Descendant);
            end if;

            Append (Type_Table (Ancestor).Concrete_Descendants, Descendant);
         end Add_Concrete_Descendant;

         procedure Do_One_Type (T : Node_Or_Entity_Type) is
         begin
            case T is
               when Concrete_Type =>
                  pragma Annotate (Codepeer, Modified, Type_Table);
                  Type_Table (T).First := T;
                  Type_Table (T).Last  := T;
                  Add_Concrete_Descendant (Type_Table (T).Parent, T);

               when Abstract_Type =>
                  declare
                     Children : Type_Vector renames Type_Table (T).Children;
                  begin
                     --  Ensure that an abstract type is not a leaf in the type
                     --  hierarchy.

                     if Is_Empty (Children) then
                        raise Illegal with Image (T) & " has no children";
                     end if;

                     --  We could support abstract types with only one child,
                     --  but what's the point of having such a type?

                     if Last_Index (Children) = 1 then
                        raise Illegal with Image (T) & " has only one child";
                     end if;

                     Type_Table (T).First := Type_Table (Children (1)).First;
                     Type_Table (T).Last  :=
                       Type_Table (Children (Last_Index (Children))).Last;
                  end;

               when Between_Abstract_Entity_And_Concrete_Node_Types =>
                  raise Program_Error;
            end case;
         end Do_One_Type;
      begin
         Iterate_Types (Root, Post => Do_One_Type'Access);
      end Compute_Ranges;

      procedure Compute_Fields_Per_Node is

         Duplicate_Fields_Found : Boolean := False;

         function Get_Fields (T : Node_Or_Entity_Type) return Field_Vector;
         --  Compute the fields of a given type. This is the fields inherited
         --  from ancestors, plus the fields declared for the type itself.

         function Get_Is_Syntactic (T : Node_Or_Entity_Type) return Field_Set;
         --  Compute the set of fields that are syntactic for a given type.
         --  Note that a field can be syntactic in some node types, but
         --  semantic in others.

         procedure Do_Concrete_Type (CT : Concrete_Type);

         function Get_Fields (T : Node_Or_Entity_Type) return Field_Vector is
            Parent_Fields : constant Field_Vector :=
              (if T in Root_Type then Field_Vectors.Empty_Vector
               else Get_Fields (Type_Table (T).Parent));
         begin
            return Parent_Fields & Type_Table (T).Fields;
         end Get_Fields;

         function Get_Is_Syntactic (T : Node_Or_Entity_Type) return Field_Set
         is
            Parent_Is_Syntactic : constant Field_Set :=
              (if T in Root_Type then (Field_Enum => False)
               else Get_Is_Syntactic (Type_Table (T).Parent));
         begin
            return Parent_Is_Syntactic or Is_Syntactic (T);
         end Get_Is_Syntactic;

         procedure Do_Concrete_Type (CT : Concrete_Type) is
         begin
            Type_Table (CT).Fields := Get_Fields (CT);
            Is_Syntactic (CT) := Get_Is_Syntactic (CT);

            for F of Type_Table (CT).Fields loop
               if Fields_Per_Node (CT) (F) then
                  Put ("duplicate field \1.\2\n", Image (CT), Image (F));
                  Duplicate_Fields_Found := True;
               end if;

               Fields_Per_Node (CT) (F) := True;
            end loop;
         end Do_Concrete_Type;

      begin -- Compute_Fields_Per_Node
         for CT in Concrete_Node loop
            Do_Concrete_Type (CT);
         end loop;

         --  The node fields defined for all three N_Entity kinds should be the
         --  same:

         if Type_Table (N_Defining_Character_Literal).Fields /=
           Type_Table (N_Defining_Identifier).Fields
         then
            raise Illegal with
              "fields for N_Defining_Identifier and " &
              "N_Defining_Character_Literal must match";
         end if;

         if Type_Table (N_Defining_Operator_Symbol).Fields /=
           Type_Table (N_Defining_Identifier).Fields
         then
            raise Illegal with
              "fields for N_Defining_Identifier and " &
              "N_Defining_Operator_Symbol must match";
         end if;

         if Fields_Per_Node (N_Defining_Character_Literal) /=
           Fields_Per_Node (N_Defining_Identifier)
         then
            raise Illegal with
              "Fields of N_Defining_Character_Literal must match " &
              "N_Defining_Identifier";
         end if;

         if Fields_Per_Node (N_Defining_Operator_Symbol) /=
           Fields_Per_Node (N_Defining_Identifier)
         then
            raise Illegal with
              "Fields of N_Defining_Operator_Symbol must match " &
              "N_Defining_Identifier";
         end if;

         --  Copy node fields from N_Entity nodes to entities, so they have
         --  slots allocated (but the getters and setters are only in
         --  Sinfo.Nodes).

         Type_Table (Entity_Kind).Fields :=
           Type_Table (N_Defining_Identifier).Fields &
           Type_Table (Entity_Kind).Fields;

         for CT in Concrete_Entity loop
            Do_Concrete_Type (CT);
         end loop;

         if Duplicate_Fields_Found then
            raise Illegal with "duplicate fields found";
         end if;
      end Compute_Fields_Per_Node;

      function Field_Size (T : Type_Enum) return Bit_Offset is
        (case T is
          when Flag | Float_Rep_Kind => 1,
          when Small_Paren_Count_Type | Component_Alignment_Kind => 2,
          when Nkind_Type | Ekind_Type | Convention_Id => 8,
          when Mechanism_Type | List_Id | Elist_Id | Name_Id | String_Id | Uint
           | Ureal | Source_Ptr | Union_Id | Node_Id
           | Node_Or_Entity_Type => 32,
         when Between_Special_And_Abstract_Node_Types => -- can't happen
           Bit_Offset'Last);
         --  Note that this is not the same as Type_Bit_Size of the field's
         --  type. For one thing, Type_Bit_Size only covers concrete node and
         --  entity types, which does not include most of the above. For
         --  another thing, Type_Bit_Size includes the full size of all the
         --  fields, whereas a field of a node or entity type is just a 32-bit
         --  Node_Id or Entity_Id; i.e. it is indirect.

      function Field_Size (F : Field_Enum) return Bit_Offset is
        (Field_Size (Field_Table (F).Field_Type));

      function To_Bit_Offset (F : Field_Enum; Offset : Field_Offset)
        return Bit_Offset is
          (Bit_Offset (Offset) * Field_Size (F));
      function First_Bit (F : Field_Enum; Offset : Field_Offset)
        return Bit_Offset is
          (To_Bit_Offset (F, Offset));
      function Last_Bit (F : Field_Enum; Offset : Field_Offset)
        return Bit_Offset is
          (To_Bit_Offset (F, Offset + 1) - 1);

      function To_Size_In_Slots (Size_In_Bits : Bit_Offset)
        return Field_Offset is
          ((Field_Offset (Size_In_Bits) + 31) / 32);

      function Type_Size_In_Slots (T : Concrete_Type) return Field_Offset is
        (To_Size_In_Slots (Type_Bit_Size (T))); -- rounded up to slot boundary

      function Type_Bit_Size_Aligned (T : Concrete_Type) return Bit_Offset is
        (Bit_Offset (Type_Size_In_Slots (T)) * 32); -- multiple of slot size

      procedure Compute_Field_Offsets is
         type Offset_Set_Unconstrained is array (Bit_Offset range <>)
           of Boolean with Pack;
         subtype Offset_Set is Offset_Set_Unconstrained (Bit_Offset);
         Offset_Sets : array (Concrete_Type) of Offset_Set :=
           (others => (others => False));

         function All_False
           (F : Field_Enum; Offset : Field_Offset)
           return Offset_Set_Unconstrained is
             (First_Bit (F, Offset) .. Last_Bit (F, Offset) => False);

         function All_True
           (F : Field_Enum; Offset : Field_Offset)
           return Offset_Set_Unconstrained is
             (First_Bit (F, Offset) .. Last_Bit (F, Offset) => True);

         function Offset_OK
           (F : Field_Enum; Offset : Field_Offset) return Boolean;
         --  True if it is OK to choose this offset; that is, if this offset is
         --  not in use for any type that has the field. If Overlay_Fields is
         --  False, then "any type that has the field" --> "any type, whether
         --  or not it has the field".

         procedure Set_Offset_Set
           (F : Field_Enum; Offset : Field_Offset);
         --  Mark the offset as "in use"

         function Choose_Offset
           (F : Field_Enum) return Field_Offset;
         --  Choose an offset for this field

         function Offset_OK
           (F : Field_Enum; Offset : Field_Offset) return Boolean is
         begin
            for T in Concrete_Type loop
               if Fields_Per_Node (T) (F) or else not Overlay_Fields then
                  declare
                     Bits : Offset_Set_Unconstrained renames
                       Offset_Sets (T)
                         (First_Bit (F, Offset) .. Last_Bit (F, Offset));
                  begin
                     if Bits /= All_False (F, Offset) then
                        return False;
                     end if;
                  end;
               end if;
            end loop;

            return True;
         end Offset_OK;

         procedure Set_Offset_Set
           (F : Field_Enum; Offset : Field_Offset) is
         begin
            for T in Concrete_Type loop
               if Fields_Per_Node (T) (F) then
                  declare
                     Bits : Offset_Set_Unconstrained renames
                       Offset_Sets (T)
                         (First_Bit (F, Offset) .. Last_Bit (F, Offset));
                  begin
                     pragma Assert (Bits = All_False (F, Offset));
                     Bits := All_True (F, Offset);
                  end;
               end if;
            end loop;
         end Set_Offset_Set;

         function Choose_Offset
           (F : Field_Enum) return Field_Offset is
         begin
            for Offset in Field_Offset loop
               if Offset_OK (F, Offset) then
                  Set_Offset_Set (F, Offset);

                  return Offset;
               end if;
            end loop;

            raise Illegal with "No available field offset for " & Image (F);
         end Choose_Offset;

         Num_Concrete_Have_Field : array (Field_Enum) of Type_Count :=
           (others => 0);
         --  Number of concrete types that have each field

         function More_Types_Have_Field (F1, F2 : Field_Enum) return Boolean is
           (Num_Concrete_Have_Field (F1) > Num_Concrete_Have_Field (F2));
         --  True if F1 appears in more concrete types than F2

         function Sort_Less (F1, F2 : Field_Enum) return Boolean is
           (if Num_Concrete_Have_Field (F1) = Num_Concrete_Have_Field (F2) then
              F1 < F2
            else More_Types_Have_Field (F1, F2));

         package Sorting is new Field_Vectors.Generic_Sorting
           ("<" => Sort_Less);

         All_Fields : Field_Vector;

      begin

         --  Compute the number of types that have each field

         for T in Concrete_Type loop
            for F in Field_Enum loop
               if Fields_Per_Node (T) (F) then
                  Num_Concrete_Have_Field (F) :=
                    Num_Concrete_Have_Field (F) + 1;
               end if;
            end loop;
         end loop;

         --  Collect all the fields in All_Fields

         for F in Node_Field loop
            Append (All_Fields, F);
         end loop;

         for F in Entity_Field loop
            Append (All_Fields, F);
         end loop;

         --  Sort All_Fields based on how many concrete types have the field.

         Sorting.Sort (All_Fields);

         --  Go through all the fields, and choose the lowest offset that is
         --  free in all types that have the field.

         for F of All_Fields loop
            Field_Table (F).Offset := Choose_Offset (F);
         end loop;

      end Compute_Field_Offsets;

      procedure Compute_Type_Sizes is
         --  Node_Counts is the number of nodes of each kind created during
         --  compilation of a large example.

         Node_Counts : constant array (Concrete_Node) of Natural :=
           (N_Identifier => 429298,
            N_Defining_Identifier => 231636,
            N_Integer_Literal => 90892,
            N_Parameter_Specification => 62811,
            N_Attribute_Reference => 47150,
            N_Expanded_Name => 37375,
            N_Selected_Component => 30699,
            N_Subprogram_Declaration => 20744,
            N_Freeze_Entity => 20314,
            N_Procedure_Specification => 18901,
            N_Object_Declaration => 18023,
            N_Function_Specification => 16570,
            N_Range => 16216,
            N_Explicit_Dereference => 12198,
            N_Component_Association => 11188,
            N_Unchecked_Type_Conversion => 11165,
            N_Subtype_Indication => 10727,
            N_Procedure_Call_Statement => 10056,
            N_Subtype_Declaration => 8141,
            N_Handled_Sequence_Of_Statements => 8078,
            N_Null => 7288,
            N_Aggregate => 7222,
            N_String_Literal => 7152,
            N_Function_Call => 6958,
            N_Simple_Return_Statement => 6911,
            N_And_Then => 6867,
            N_Op_Eq => 6845,
            N_Call_Marker => 6683,
            N_Pragma_Argument_Association => 6525,
            N_Component_Definition => 6487,
            N_Assignment_Statement => 6483,
            N_With_Clause => 6480,
            N_Null_Statement => 5917,
            N_Index_Or_Discriminant_Constraint => 5877,
            N_Generic_Association => 5667,
            N_Full_Type_Declaration => 5573,
            N_If_Statement => 5553,
            N_Subprogram_Body => 5455,
            N_Op_Add => 5443,
            N_Type_Conversion => 5260,
            N_Component_Declaration => 5059,
            N_Raise_Constraint_Error => 4840,
            N_Formal_Concrete_Subprogram_Declaration => 4602,
            N_Expression_With_Actions => 4598,
            N_Op_Ne => 3854,
            N_Indexed_Component => 3834,
            N_Op_Subtract => 3777,
            N_Package_Specification => 3490,
            N_Subprogram_Renaming_Declaration => 3445,
            N_Pragma => 3427,
            N_Case_Statement_Alternative => 3272,
            N_Block_Statement => 3239,
            N_Parameter_Association => 3213,
            N_Op_Lt => 3020,
            N_Op_Not => 2926,
            N_Character_Literal => 2914,
            N_Others_Choice => 2769,
            N_Or_Else => 2576,
            N_Itype_Reference => 2511,
            N_Defining_Operator_Symbol => 2487,
            N_Component_List => 2470,
            N_Formal_Object_Declaration => 2262,
            N_Generic_Subprogram_Declaration => 2227,
            N_Real_Literal => 2156,
            N_Op_Gt => 2156,
            N_Access_To_Object_Definition => 1984,
            N_Op_Le => 1975,
            N_Op_Ge => 1942,
            N_Package_Renaming_Declaration => 1811,
            N_Formal_Type_Declaration => 1756,
            N_Qualified_Expression => 1746,
            N_Package_Declaration => 1729,
            N_Record_Definition => 1651,
            N_Allocator => 1521,
            N_Op_Concat => 1377,
            N_Access_Definition => 1358,
            N_Case_Statement => 1322,
            N_Number_Declaration => 1316,
            N_Generic_Package_Declaration => 1311,
            N_Slice => 1078,
            N_Constrained_Array_Definition => 1068,
            N_Exception_Renaming_Declaration => 1011,
            N_Implicit_Label_Declaration => 978,
            N_Exception_Handler => 966,
            N_Private_Type_Declaration => 898,
            N_Operator_Symbol => 872,
            N_Formal_Private_Type_Definition => 867,
            N_Range_Constraint => 849,
            N_Aspect_Specification => 837,
            N_Variant => 834,
            N_Discriminant_Specification => 746,
            N_Loop_Statement => 744,
            N_Derived_Type_Definition => 731,
            N_Freeze_Generic_Entity => 702,
            N_Iteration_Scheme => 686,
            N_Package_Instantiation => 658,
            N_Loop_Parameter_Specification => 632,
            N_Attribute_Definition_Clause => 608,
            N_Compilation_Unit_Aux => 599,
            N_Compilation_Unit => 599,
            N_Label => 572,
            N_Goto_Statement => 572,
            N_In => 564,
            N_Enumeration_Type_Definition => 523,
            N_Object_Renaming_Declaration => 482,
            N_If_Expression => 476,
            N_Exception_Declaration => 472,
            N_Reference => 455,
            N_Incomplete_Type_Declaration => 438,
            N_Use_Package_Clause => 401,
            N_Unconstrained_Array_Definition => 360,
            N_Variant_Part => 340,
            N_Defining_Program_Unit_Name => 336,
            N_Op_And => 334,
            N_Raise_Program_Error => 329,
            N_Formal_Discrete_Type_Definition => 319,
            N_Contract => 311,
            N_Not_In => 305,
            N_Designator => 285,
            N_Component_Clause => 247,
            N_Formal_Signed_Integer_Type_Definition => 244,
            N_Raise_Statement => 214,
            N_Op_Expon => 205,
            N_Op_Minus => 202,
            N_Op_Multiply => 158,
            N_Exit_Statement => 130,
            N_Function_Instantiation => 129,
            N_Discriminant_Association => 123,
            N_Private_Extension_Declaration => 119,
            N_Extended_Return_Statement => 117,
            N_Op_Divide => 107,
            N_Op_Or => 103,
            N_Signed_Integer_Type_Definition => 101,
            N_Record_Representation_Clause => 76,
            N_Unchecked_Expression => 70,
            N_Op_Abs => 63,
            N_Elsif_Part => 62,
            N_Formal_Floating_Point_Definition => 59,
            N_Formal_Package_Declaration => 58,
            N_Modular_Type_Definition => 55,
            N_Abstract_Subprogram_Declaration => 52,
            N_Validate_Unchecked_Conversion => 49,
            N_Defining_Character_Literal => 36,
            N_Raise_Storage_Error => 33,
            N_Compound_Statement => 29,
            N_Procedure_Instantiation => 28,
            N_Access_Procedure_Definition => 25,
            N_Floating_Point_Definition => 20,
            N_Use_Type_Clause => 19,
            N_Op_Plus => 14,
            N_Package_Body => 13,
            N_Op_Rem => 13,
            N_Enumeration_Representation_Clause => 13,
            N_Access_Function_Definition => 11,
            N_Extension_Aggregate => 11,
            N_Formal_Ordinary_Fixed_Point_Definition => 10,
            N_Op_Mod => 10,
            N_Expression_Function => 9,
            N_Delay_Relative_Statement => 9,
            N_Quantified_Expression => 7,
            N_Formal_Derived_Type_Definition => 7,
            N_Free_Statement => 7,
            N_Iterator_Specification => 5,
            N_Op_Shift_Left => 5,
            N_Formal_Modular_Type_Definition => 4,
            N_Generic_Package_Renaming_Declaration => 1,
            N_Empty => 1,
            N_Real_Range_Specification => 1,
            N_Ordinary_Fixed_Point_Definition => 1,
            N_Op_Shift_Right => 1,
            N_Error => 1,
            N_Mod_Clause => 1,
            others => 0);

         Total_Node_Count : constant Long_Float := 1370676.0;

         type Node_Frequency_Table is array (Concrete_Node) of Long_Float;

         function Init_Node_Frequency return Node_Frequency_Table;
         --  Compute the value of the Node_Frequency table

         function Average_Type_Size_In_Slots return Long_Float;
         --  Compute the average over all concrete node types of the size,
         --  weighted by the frequency of that node type.

         function Init_Node_Frequency return Node_Frequency_Table is
            Result : Node_Frequency_Table := (others => 0.0);

         begin
            for T in Concrete_Node loop
               Result (T) := Long_Float (Node_Counts (T)) / Total_Node_Count;
            end loop;

            return Result;
         end Init_Node_Frequency;

         Node_Frequency : constant Node_Frequency_Table := Init_Node_Frequency;
         --  Table mapping concrete node types to the relative frequency of
         --  that node, in our large example. The sum of these values should
         --  add up to approximately 1.0. For example, if Node_Frequency(K) =
         --  0.02, then that means that approximately 2% of all nodes are K
         --  nodes.

         function Average_Type_Size_In_Slots return Long_Float is
            --  We don't have data on entities, so we leave those out

            Result : Long_Float := 0.0;
         begin
            for T in Concrete_Node loop
               Result := Result +
                 Node_Frequency (T) * Long_Float (Type_Size_In_Slots (T));
            end loop;

            return Result;
         end Average_Type_Size_In_Slots;

      --  Start of processing for Compute_Type_Sizes

      begin
         for T in Concrete_Type loop
            declare
               Max_Offset : Bit_Offset := 0;

            begin
               for F in Field_Enum loop
                  if Fields_Per_Node (T) (F) then
                     Max_Offset :=
                       Bit_Offset'Max
                         (Max_Offset,
                          To_Bit_Offset (F, Field_Table (F).Offset));
                  end if;
               end loop;

               Type_Bit_Size (T) := Max_Offset + 1;
            end;
         end loop;

         for T in Concrete_Node loop
            Min_Node_Bit_Size :=
              Bit_Offset'Min (Min_Node_Bit_Size, Type_Bit_Size (T));
            Max_Node_Bit_Size :=
              Bit_Offset'Max (Max_Node_Bit_Size, Type_Bit_Size (T));
         end loop;

         for T in Concrete_Entity loop
            Min_Entity_Bit_Size :=
              Bit_Offset'Min (Min_Entity_Bit_Size, Type_Bit_Size (T));
            Max_Entity_Bit_Size :=
              Bit_Offset'Max (Max_Entity_Bit_Size, Type_Bit_Size (T));
         end loop;

         Min_Node_Size := To_Size_In_Slots (Min_Node_Bit_Size);
         Max_Node_Size := To_Size_In_Slots (Max_Node_Bit_Size);
         Min_Entity_Size := To_Size_In_Slots (Min_Entity_Bit_Size);
         Max_Entity_Size := To_Size_In_Slots (Max_Entity_Bit_Size);

         Average_Node_Size_In_Slots := Average_Type_Size_In_Slots;
      end Compute_Type_Sizes;

      procedure Check_For_Syntactic_Mismatch is
      begin
         for F in Field_Enum loop
            if F /= Between_Node_And_Entity_Fields then
               declare
                  Syntactic_Seen, Semantic_Seen : Boolean := False;
                  Have_Field : Type_Vector renames
                    Field_Table (F).Have_This_Field;

               begin
                  for J in 1 .. Last_Index (Have_Field) loop
                     if Is_Syntactic (Have_Field (J)) (F) then
                        Syntactic_Seen := True;
                     else
                        Semantic_Seen := True;
                     end if;
                  end loop;

                  --  The following fields violate this rule. We might want to
                  --  simplify by getting rid of these cases, but we allow them
                  --  for now. At least, we don't want to add any new cases of
                  --  syntactic/semantic mismatch.

                  if F in Chars | Actions | Expression | Default_Expression
                  then
                     pragma Assert (Syntactic_Seen and Semantic_Seen);

                  else
                     if Syntactic_Seen and Semantic_Seen then
                        raise Illegal with
                          "syntactic/semantic mismatch for " & Image (F);
                     end if;

                     if Field_Table (F).Field_Type in Traversal_Type
                       and then Syntactic_Seen
                     then
                        Setter_Needs_Parent (F) := True;
                     end if;
                  end if;
               end;
            end if;
         end loop;
      end Check_For_Syntactic_Mismatch;

      function Field_Types_Used (First, Last : Field_Enum) return Type_Set is
         Result : Type_Set := (others => False);
      begin
         for F in First .. Last loop
            if Field_Table (F).Field_Type in Node_Or_Entity_Type then
               Result (Node_Id) := True;
            else
               Result (Field_Table (F).Field_Type) := True;
            end if;
         end loop;

         return Result;
      end Field_Types_Used;

      pragma Style_Checks ("M120");
      --  Lines of the form Put (S, "..."); are more readable if we relax the
      --  line length. We really just want the "..." to be short enough.

      procedure Put_Type_And_Subtypes
        (S : in out Sink'Class; Root : Root_Type)
      is

         procedure Put_Enum_Type;
         --  Print out the enumeration type declaration for a root type
         --  (Node_Kind or Entity_Kind).

         procedure Put_Kind_Subtype (T : Node_Or_Entity_Type);
         --  Print out a subrange (of type Node_Kind or Entity_Kind) for a
         --  given nonroot abstract type.

         procedure Put_Id_Subtype (T : Node_Or_Entity_Type);
         --  Print out a subtype (of type Node_Id or Entity_Id) for a given
         --  nonroot abstract type.

         procedure Put_Enum_Type is
            procedure Put_Enum_Lit (T : Node_Or_Entity_Type);
            --  Print out one enumeration literal in the declaration of
            --  Node_Kind or Entity_Kind.

            First_Time : Boolean := True;

            procedure Put_Enum_Lit (T : Node_Or_Entity_Type) is
            begin
               if T in Concrete_Type then
                  if First_Time then
                     First_Time := False;
                  else
                     Put (S, ",\n");
                  end if;

                  Put (S, "\1", Image (T));
               end if;
            end Put_Enum_Lit;

            type Dummy is array
              (First_Concrete (Root) .. Last_Concrete (Root)) of Boolean;
            Num_Types : constant Root_Int := Dummy'Length;

         begin
            Put (S, "type \1 is -- \2 \1s\n", Image (Root), Image (Num_Types));
            Indent (S, 2);
            Put (S, "(");
            Indent (S, 1);
            Iterate_Types (Root, Pre => Put_Enum_Lit'Access);
            Outdent (S, 1);
            Put (S, "\n) with Size => 8; -- \1\n\n", Image (Root));
            Outdent (S, 2);
         end Put_Enum_Type;

         procedure Put_Kind_Subtype (T : Node_Or_Entity_Type) is
         begin
            if T in Abstract_Type then
               if Type_Table (T).Is_Union then
                  pragma Assert (Type_Table (T).Parent = Root);

                  Put (S, "subtype \1 is\n", Image (T));
                  Indent (S, 2);
                  Put (S, "\1 with Predicate =>\n",
                       Image (Root));
                  Indent (S, 2);
                  Put (S, "\1 in\n", Image (T));
                  Put_Images (S, Type_Table (T).Children);
                  Outdent (S, 2);
                  Put (S, ";\n");
                  Outdent (S, 2);

               elsif Type_Table (T).Parent /= No_Type then
                  Put (S, "subtype \1 is \2 range\n",
                       Image (T),
                       Image (Type_Table (T).Parent));
                  Indent (S, 2);
                  Put (S, "\1 .. \2;\n",
                       Image (Type_Table (T).First),
                       Image (Type_Table (T).Last));
                  Outdent (S, 2);

                  Indent (S, 3);

                  for J in 1 .. Type_Table (T).Concrete_Descendants.Last_Index loop
                     Put (S, "--  \1\n",
                          Image (Type_Table (T).Concrete_Descendants (J)));
                  end loop;

                  Outdent (S, 3);
               end if;
            end if;
         end Put_Kind_Subtype;

         procedure Put_Id_Subtype (T : Node_Or_Entity_Type) is
         begin
            --  ????We have names like Overloadable_Kind_Id.
            --  Perhaps that should be Overloadable_Id.

            if Type_Table (T).Parent /= No_Type then
               Put (S, "subtype \1 is\n", Id_Image (T));
               Indent (S, 2);
               Put (S, "\1", Id_Image (Type_Table (T).Parent));

               if Enable_Assertions then
                  Put (S, " with Predicate =>\n");
                  Indent (S, 2);
                  Put (S, "K (\1) in \2", Id_Image (T), Image (T));
                  Outdent (S, 2);
               end if;

               Put (S, ";\n");
               Outdent (S, 2);
            end if;
         end Put_Id_Subtype;

      begin -- Put_Type_And_Subtypes
         Put_Enum_Type;

         --  Put the getter for Nkind and Ekind here, earlier than the other
         --  getters, because it is needed in predicates of the following
         --  subtypes.

         case Root is
            when Node_Kind =>
               Put_Getter_Decl (S, Nkind);
               Put (S, "function K (N : Node_Id) return Node_Kind renames Nkind;\n");
               Put (S, "--  Shorthand for use in predicates and preconditions below\n");
               Put (S, "--  There is no procedure Set_Nkind.\n");
               Put (S, "--  See Init_Nkind and Mutate_Nkind in Atree.\n\n");

            when Entity_Kind =>
               Put_Getter_Decl (S, Ekind);
               Put (S, "function K (N : Entity_Id) return Entity_Kind renames Ekind;\n");
               Put (S, "--  Shorthand for use in predicates and preconditions below\n");
               Put (S, "--  There is no procedure Set_Ekind here.\n");
               Put (S, "--  See Mutate_Ekind in Atree.\n\n");

            when others => raise Program_Error;
         end case;

         Put (S, "--  Subtypes of \1 for each abstract type:\n\n",
              Image (Root));

         Put (S, "pragma Style_Checks (""M200"");\n");
         Iterate_Types (Root, Pre => Put_Kind_Subtype'Access);

         Put (S, "\n--  Subtypes of \1 with specified \2.\n",
              Id_Image (Root), Image (Root));
         Put (S, "--  These may be used in place of \1 for better documentation,\n",
              Id_Image (Root));
         Put (S, "--  and if assertions are enabled, for run-time checking.\n\n");

         Iterate_Types (Root, Pre => Put_Id_Subtype'Access);
         Put (S, "\n");

         Put (S, "--  Union types (nonhierarchical subtypes of \1)\n\n",
              Id_Image (Root));

         for T in First_Abstract (Root) .. Last_Abstract (Root) loop
            if Type_Table (T) /= null and then Type_Table (T).Is_Union then
               Put_Kind_Subtype (T);
               Put_Id_Subtype (T);
               Put (S, "\n");
            end if;
         end loop;

         Put (S, "subtype Flag is Boolean;\n\n");
      end Put_Type_And_Subtypes;

      function Low_Level_Getter (T : Type_Enum) return String is
        ("Get_" & Image (T));
      function Low_Level_Setter (T : Type_Enum) return String is
        ("Set_" & Image (T));
      function Low_Level_Setter (F : Field_Enum) return String is
        (Low_Level_Setter (Field_Table (F).Field_Type) &
           (if Setter_Needs_Parent (F) then "_With_Parent" else ""));

      procedure Instantiate_Low_Level_Accessors
        (S : in out Sink'Class; T : Type_Enum)
      is
      begin
         --  Special case for types that have defaults; instantiate
         --  Get_32_Bit_Field_With_Default and pass in the Default_Val.

         if T in Elist_Id | Uint then
            pragma Assert (Field_Size (T) = 32);

            declare
               Default_Val : constant String :=
                 (if T = Elist_Id then "No_Elist" else "Uint_0");

            begin
               Put (S, "\nfunction \1 is new Get_32_Bit_Field_With_Default (\2, \3) with \4;\n",
                    Low_Level_Getter (T),
                    Get_Set_Id_Image (T),
                    Default_Val,
                    Inline);
            end;

         --  Otherwise, instantiate the normal getter for the right size in
         --  bits.

         else
            Put (S, "\nfunction \1 is new Get_\2_Bit_Field (\3) with \4;\n",
                 Low_Level_Getter (T),
                 Image (Field_Size (T)),
                 Get_Set_Id_Image (T),
                 Inline);
         end if;

         --  No special case for the setter

         if T in Nkind_Type | Ekind_Type then
            Put (S, "pragma Warnings (Off);\n");
            --  Set_Nkind_Type and Set_Ekind_Type might not be called
         end if;

         Put (S, "procedure \1 is new Set_\2_Bit_Field (\3) with \4;\n",
              Low_Level_Setter (T),
              Image (Field_Size (T)),
              Get_Set_Id_Image (T),
              Inline);

         if T in Nkind_Type | Ekind_Type then
            Put (S, "pragma Warnings (On);\n");
         end if;
      end Instantiate_Low_Level_Accessors;

      procedure Put_Precondition
        (S : in out Sink'Class; F : Field_Enum)
      is
         --  If the field is present in all entities, we want to assert that
         --  N in N_Entity_Id. If the field is present in only some entities,
         --  we don't need that, because we are fetching Ekind in that case,
         --  which will assert N in N_Entity_Id.

         Is_Entity : constant String :=
           (if Field_Table (F).Have_This_Field = All_Entities then
             "N in N_Entity_Id"
            else "");
      begin
         --  If this is an entity field, then we should assert that N is an
         --  entity. We need "N in A | B | ..." unless this is embodied in a
         --  subtype predicate.
         --
         --  We can't put the extra "Pre => ..." specified on the call to
         --  Create_..._Field as part of the precondition, because some of
         --  them call things that are not visible here.

         if Enable_Assertions then
            if Length (Field_Table (F).Have_This_Field) = 1
              or else Field_Table (F).Have_This_Field = Nodes_And_Entities
            then
               if Is_Entity /= "" then
                  Indent (S, 1);
                  Put (S, ", Pre =>\n");
                  Put (S, "\1", Is_Entity);
                  Outdent (S, 1);
               end if;

            else
               Put (S, ", Pre =>\n");
               Indent (S, 1);
               Put (S, "N in ");
               Put_Id_Images (S, Field_Table (F).Have_This_Field);

               pragma Assert (Is_Entity = "");

               Outdent (S, 1);
            end if;
         end if;
      end Put_Precondition;

      function Root_Type_For_Field (F : Field_Enum) return Root_Type is
        (case F is
           when Node_Field           => Node_Kind,
           when Entity_Field         => Entity_Kind,
           when Between_Node_And_Entity_Fields => Node_Kind); -- can't happen

      function N_Type (F : Field_Enum) return String is
        (if Length (Field_Table (F).Have_This_Field) = 1 then
          Id_Image (Field_Table (F).Have_This_Field (1))
         else Id_Image (Root_Type_For_Field (F)));
      --  Name of the parameter type of the N parameter of the getter and
      --  setter for field F. If there's only one Have_This_Field, use that;
      --  the predicate will check for the right Kind. Otherwise, we use
      --  Node_Id or Entity_Id, and the getter and setter will have
      --  preconditions.

      function Node_To_Fetch_From (F : Field_Enum) return String is
      begin
         return
           (case Field_Table (F).Type_Only is
              when No_Type_Only => "N",
              when Base_Type_Only => "Base_Type (N)",
              when Impl_Base_Type_Only => "Implementation_Base_Type (N)",
              when Root_Type_Only => "Root_Type (N)");
      end Node_To_Fetch_From;

      procedure Put_Getter_Spec (S : in out Sink'Class; F : Field_Enum) is
      begin
         Put (S, "function \1\n", Image (F));
         Indent (S, 2);
         Put (S, "(N : \1) return \2",
              N_Type (F), Get_Set_Id_Image (Field_Table (F).Field_Type));
         Outdent (S, 2);
      end Put_Getter_Spec;

      procedure Put_Getter_Decl (S : in out Sink'Class; F : Field_Enum) is
      begin
         Put_Getter_Spec (S, F);
         Put (S, " with \1", Inline);
         Indent (S, 2);
         Put_Precondition (S, F);

         Outdent (S, 2);
         Put (S, ";\n");
      end Put_Getter_Decl;

      procedure Put_Getter_Body (S : in out Sink'Class; F : Field_Enum) is
      begin
         Put_Getter_Spec (S, F);
         Put (S, " is\n");
         Put (S, "begin\n");
         Indent (S, 3);

         if Field_Table (F).Pre.all /= "" then
            Put (S, "pragma Assert (\1);\n", Field_Table (F).Pre.all);
         end if;

         Put (S, "return \1 (\2, \3);\n",
              Low_Level_Getter (Field_Table (F).Field_Type),
              Node_To_Fetch_From (F),
              Image (Field_Table (F).Offset));
         Outdent (S, 3);
         Put (S, "end \1;\n\n", Image (F));
      end Put_Getter_Body;

      procedure Put_Setter_Spec (S : in out Sink'Class; F : Field_Enum) is
         Rec    : Field_Info renames Field_Table (F).all;
         Default : constant String :=
           (if Field_Table (F).Field_Type = Flag then " := True" else "");
      begin
         Put (S, "procedure Set_\1\n", Image (F));
         Indent (S, 2);
         Put (S, "(N : \1; Val : \2\3)",
              N_Type (F), Get_Set_Id_Image (Rec.Field_Type),
              Default);
         Outdent (S, 2);
      end Put_Setter_Spec;

      procedure Put_Setter_Decl (S : in out Sink'Class; F : Field_Enum) is
      begin
         Put_Setter_Spec (S, F);
         Put (S, " with \1", Inline);
         Indent (S, 2);
         Put_Precondition (S, F);
         Outdent (S, 2);
         Put (S, ";\n");
      end Put_Setter_Decl;

      procedure Put_Setter_Body (S : in out Sink'Class; F : Field_Enum) is
         --  If Type_Only was specified in the call to Create_Semantic_Field,
         --  then we assert that the node is a base (etc) type.

         Type_Only_Assertion : constant String :=
           (case Field_Table (F).Type_Only is
              when No_Type_Only => "",
              when Base_Type_Only => "Is_Base_Type (N)",
--  ????It seems like we should call Is_Implementation_Base_Type or
--  Is_Root_Type (which don't currently exist), but the old version always
--  calls Base_Type.
--              when Impl_Base_Type_Only => "Is_Implementation_Base_Type (N)",
--              when Root_Type_Only => "Is_Root_Type (N)");
              when Impl_Base_Type_Only => "Is_Base_Type (N)",
              when Root_Type_Only => "Is_Base_Type (N)");
      begin
         Put_Setter_Spec (S, F);
         Put (S, " is\n");
         Put (S, "begin\n");
         Indent (S, 3);

         if Field_Table (F).Pre.all /= "" then
            Put (S, "pragma Assert (\1);\n", Field_Table (F).Pre.all);
         end if;

         if Type_Only_Assertion /= "" then
            Put (S, "pragma Assert (\1);\n", Type_Only_Assertion);
         end if;

         Put (S, "\1 (N, \2, Val);\n",
              Low_Level_Setter (F),
              Image (Field_Table (F).Offset));
         Outdent (S, 3);
         Put (S, "end Set_\1;\n\n", Image (F));
      end Put_Setter_Body;

      procedure Put_Subp_Decls (S : in out Sink'Class; Root : Root_Type) is
         --  Note that there are several fields that are defined for both nodes
         --  and entities, such as Nkind. These are allocated slots in both,
         --  but here we only put out getters and setters in Sinfo.Nodes, not
         --  Einfo.Entities.

      begin
         Put (S, "--  Getters and setters for fields\n");

         for F in First_Field (Root) .. Last_Field (Root) loop
            --  Nkind/Ekind getter is already done (see Put_Type_And_Subtypes),
            --  and there is no setter for these.

            if F = Nkind then
               Put (S, "\n--  Nkind getter is above\n");

            elsif F = Ekind then
               Put (S, "\n--  Ekind getter is above\n");

            else
               Put_Getter_Decl (S, F);
               Put_Setter_Decl (S, F);
            end if;

            Put (S, "\n");
         end loop;
      end Put_Subp_Decls;

      procedure Put_Subp_Bodies (S : in out Sink'Class; Root : Root_Type) is
      begin
         Put (S, "\n--  Getters and setters for fields\n\n");

         for F in First_Field (Root) .. Last_Field (Root) loop
            Put_Getter_Body (S, F);

            if F not in Nkind | Ekind then
               Put_Setter_Body (S, F);
            end if;
         end loop;
      end Put_Subp_Bodies;

      procedure Put_Traversed_Fields (S : in out Sink'Class) is

         function Is_Traversed_Field
           (T : Concrete_Node; F : Field_Enum) return Boolean;
         --  True if F is a field that should be traversed by Traverse_Func. In
         --  particular, True if F is a syntactic field of T, and is of a
         --  Node_Id or List_Id type.

         function Init_Max_Traversed_Fields return Field_Offset;
         --  Compute the maximum number of syntactic fields that are of type
         --  Node_Id or List_Id over all node types.

         procedure Put_Agg (T : Node_Or_Entity_Type);
         --  Print out the subaggregate for one type

         function Is_Traversed_Field
           (T : Concrete_Node; F : Field_Enum) return Boolean is
         begin
            return Is_Syntactic (T) (F)
              and then Field_Table (F).Field_Type in Traversal_Type;
         end Is_Traversed_Field;

         First_Time : Boolean := True;

         procedure Put_Agg (T : Node_Or_Entity_Type) is
            Left_Opnd_Skipped : Boolean := False;
         begin
            if T in Concrete_Node then
               if First_Time then
                  First_Time := False;
               else
                  Put (S, ",\n");
               end if;

               Put (S, "\1 => (", Image (T));
               Indent (S, 2);

               for FI in 1 .. Last_Index (Type_Table (T).Fields) loop
                  declare
                     F : constant Field_Enum := Type_Table (T).Fields (FI);

                  begin
                     if Is_Traversed_Field (T, F) then
                        if F = Left_Opnd then
                           Left_Opnd_Skipped := True; -- see comment below

                        else
                           Put (S, "\1, ", Image (Field_Table (F).Offset));
                        end if;
                     end if;
                  end;
               end loop;

               --  We always put the Left_Opnd field of N_Op_Concat last. See
               --  comments in Atree.Traverse_Func for the reason. We might as
               --  well do that for all Left_Opnd fields; the old version did
               --  that.

               if Left_Opnd_Skipped then
                  Put (S, "\1, ", Image (Field_Table (Left_Opnd).Offset));
               end if;

               Put (S, "others => No_Field_Offset");

               Outdent (S, 2);
               Put (S, ")");
            end if;
         end Put_Agg;

         function Init_Max_Traversed_Fields return Field_Offset is
            Result : Field_Offset := 0;
         begin
            for T in Concrete_Node loop
               declare
                  Num_Traversed_Fields : Field_Offset := 0; -- in type T

               begin
                  for FI in 1 .. Last_Index (Type_Table (T).Fields) loop
                     declare
                        F : constant Field_Enum := Type_Table (T).Fields (FI);

                     begin
                        if Is_Traversed_Field (T, F) then
                           Num_Traversed_Fields := Num_Traversed_Fields + 1;
                        end if;
                     end;
                  end loop;

                  if Num_Traversed_Fields > Result then
                     Result := Num_Traversed_Fields;
                  end if;
               end;
            end loop;

            return Result;
         end Init_Max_Traversed_Fields;

         Max_Traversed_Fields : constant Field_Offset :=
           Init_Max_Traversed_Fields;

      begin
         Put (S, "--  Table of fields that should be traversed by Traverse subprograms.\n");
         Put (S, "--  Each entry is an array of offsets in slots of fields to be\n");
         Put (S, "--  traversed, terminated by a sentinel equal to No_Field_Offset.\n\n");

         Put (S, "subtype Traversed_Offset_Array is Offset_Array (0 .. \1 + 1);\n",
              Image (Max_Traversed_Fields - 1));
         Put (S, "Traversed_Fields : constant array (Node_Kind) of Traversed_Offset_Array :=\n");
         --  One extra for the sentinel

         Indent (S, 2);
         Put (S, "(");
         Indent (S, 1);
         Iterate_Types (Node_Kind, Pre => Put_Agg'Access);
         Outdent (S, 1);
         Put (S, ");\n\n");
         Outdent (S, 2);
      end Put_Traversed_Fields;

      procedure Put_Tables (S : in out Sink'Class; Root : Root_Type) is

         First_Time : Boolean := True;

         procedure Put_Size (T : Node_Or_Entity_Type);
         procedure Put_Size (T : Node_Or_Entity_Type) is
         begin
            if T in Concrete_Type then
               if First_Time then
                  First_Time := False;
               else
                  Put (S, ",\n");
               end if;

               Put (S, "\1 => \2", Image (T), Image (Type_Size_In_Slots (T)));
            end if;
         end Put_Size;

         procedure Put_Field_Array (T : Concrete_Type);

         procedure Put_Field_Array (T : Concrete_Type) is
            First_Time : Boolean := True;
         begin
            for F in First_Field (Root) .. Last_Field (Root) loop
               if Fields_Per_Node (T) (F) then
                  if First_Time then
                     First_Time := False;
                  else
                     Put (S, ",\n");
                  end if;

                  Put (S, "\1", Image (F));
               end if;
            end loop;
         end Put_Field_Array;

         Field_Enum_Type_Name : constant String :=
           (case Root is
              when Node_Kind => "Node_Field",
              when others => "Entity_Field");  -- Entity_Kind

      begin
         Put (S, "--  Table of sizes in 32-bit slots for given \1, for use by Atree:\n",
              Image (Root));

         case Root is
            when Node_Kind =>
               Put (S, "\nMin_Node_Size : constant Field_Offset := \1;\n",
                    Image (Min_Node_Size));
               Put (S, "Max_Node_Size : constant Field_Offset := \1;\n\n",
                    Image (Max_Node_Size));
               Put (S, "Average_Node_Size_In_Slots : constant := \1;\n\n",
                    Average_Node_Size_In_Slots'Img);
            when Entity_Kind =>
               Put (S, "\nMin_Entity_Size : constant Field_Offset := \1;\n",
                    Image (Min_Entity_Size));
               Put (S, "Max_Entity_Size : constant Field_Offset := \1;\n\n",
                    Image (Max_Entity_Size));
            when others => raise Program_Error;
         end case;

         Put (S, "Size : constant array (\1) of Field_Offset :=\n", Image (Root));
         Indent (S, 2);
         Put (S, "(");
         Indent (S, 1);

         Iterate_Types (Root, Pre => Put_Size'Access);

         Outdent (S, 1);
         Put (S, "); -- Size\n");
         Outdent (S, 2);

         declare
            type Dummy is array
              (First_Field (Root) .. Last_Field (Root)) of Boolean;
            Num_Fields : constant Root_Int := Dummy'Length;
            First_Time : Boolean := True;
         begin
            Put (S, "\n--  Enumeration of all \1 fields:\n\n",
                 Image (Num_Fields));

            Put (S, "type \1 is\n", Field_Enum_Type_Name);
            Indent (S, 2);
            Put (S, "(");
            Indent (S, 1);

            for F in First_Field (Root) .. Last_Field (Root) loop
               if First_Time then
                  First_Time := False;
               else
                  Put (S, ",\n");
               end if;

               Put (S, "\1", Image (F));
            end loop;

            Outdent (S, 1);
            Put (S, "); -- \1\n", Field_Enum_Type_Name);
            Outdent (S, 2);
         end;

         Put (S, "\ntype \1_Index is new Pos;\n", Field_Enum_Type_Name);
         Put (S, "type \1_Array is array (\1_Index range <>) of \1;\n",
              Field_Enum_Type_Name);
         Put (S, "type \1_Array_Ref is access constant \1_Array;\n",
              Field_Enum_Type_Name);
         Put (S, "subtype A is \1_Array;\n", Field_Enum_Type_Name);
         --  Short name to make allocators below more readable

         declare
            First_Time : Boolean := True;

            procedure Do_One_Type (T : Node_Or_Entity_Type);
            procedure Do_One_Type (T : Node_Or_Entity_Type) is
            begin
               if T in Concrete_Type then
                  if First_Time then
                     First_Time := False;
                  else
                     Put (S, ",\n");
                  end if;

                  Put (S, "\1 =>\n", Image (T));
                  Indent (S, 2);
                  Put (S, "new A'(");
                  Indent (S, 6);
                  Indent (S, 1);

                  Put_Field_Array (T);

                  Outdent (S, 1);
                  Put (S, ")");
                  Outdent (S, 6);
                  Outdent (S, 2);
               end if;
            end Do_One_Type;
         begin
            Put (S, "\n--  Table mapping \1s to the sequence of fields that exist in that \1:\n\n",
                 Image (Root));

            Put (S, "\1_Table : constant array (\2) of \1_Array_Ref :=\n",
                 Field_Enum_Type_Name, Image (Root));

            Indent (S, 2);
            Put (S, "(");
            Indent (S, 1);

            Iterate_Types (Root, Pre => Do_One_Type'Access);

            Outdent (S, 1);
            Put (S, "); -- \1_Table\n", Field_Enum_Type_Name);
            Outdent (S, 2);
         end;

         declare
            First_Time : Boolean := True;
         begin
            Put (S, "\n--  Table mapping fields to kind and offset:\n\n");

            Put (S, "\1_Descriptors : constant array (\1) of Field_Descriptor :=\n",
                 Field_Enum_Type_Name);

            Indent (S, 2);
            Put (S, "(");
            Indent (S, 1);

            for F in First_Field (Root) .. Last_Field (Root) loop
               if First_Time then
                  First_Time := False;
               else
                  Put (S, ",\n");
               end if;

               Put (S, "\1 => (\2_Field, \3)", Image (F),
                    Image (Field_Table (F).Field_Type), Image (Field_Table (F).Offset));
            end loop;

            Outdent (S, 1);
            Put (S, "); -- Field_Descriptors\n");
            Outdent (S, 2);
         end;

      end Put_Tables;

      procedure Put_Seinfo is
         S : Sink'Class := Create_File ("seinfo.ads");
      begin
         Put (S, "with Types; use Types;\n");
         Put (S, "\npackage Seinfo is\n\n");
         Indent (S, 3);

         Put (S, "--  This package is automatically generated.\n\n");

         Put (S, "--  Common declarations visible in both Sinfo.Nodes and Einfo.Entities.\n");

         Put (S, "\ntype Field_Kind is\n");
         Indent (S, 2);
         Put (S, "(");
         Indent (S, 1);

         declare
            First_Time : Boolean := True;
         begin
            for T in Special_Type loop
               if First_Time then
                  First_Time := False;
               else
                  Put (S, ",\n");
               end if;

               Put (S, "\1_Field", Image (T));
            end loop;
         end;

         Outdent (S, 1);
         Outdent (S, 2);
         Put (S, ");\n");

         Put (S, "\nField_Size : constant array (Field_Kind) of Field_Size_In_Bits :=\n");
         Indent (S, 2);
         Put (S, "(");
         Indent (S, 1);

         declare
            First_Time : Boolean := True;
         begin
            for T in Special_Type loop
               if First_Time then
                  First_Time := False;
               else
                  Put (S, ",\n");
               end if;

               Put (S, "\1_Field => \2", Image (T), Image (Field_Size (T)));
            end loop;
         end;

         Outdent (S, 1);
         Outdent (S, 2);
         Put (S, ");\n\n");

         Put (S, "type Field_Descriptor is record\n");
         Indent (S, 3);
         Put (S, "Kind : Field_Kind;\n");
         Put (S, "Offset : Field_Offset;\n");
         Outdent (S, 3);
         Put (S, "end record;\n");

         Outdent (S, 3);
         Put (S, "\nend Seinfo;\n");
      end Put_Seinfo;

      procedure Put_Nodes is
         S : Sink'Class := Create_File ("sinfo-nodes.ads");
         B : Sink'Class := Create_File ("sinfo-nodes.adb");

         procedure Put_Setter_With_Parent (Kind : String);
         --  Put the low-level ..._With_Parent setter. Kind is either "Node" or
         --  "List".

         procedure Put_Setter_With_Parent (Kind : String) is
            Error : constant String := (if Kind = "Node" then "" else "_" & Kind);
         begin
            Put (B, "\nprocedure Set_\1_Id_With_Parent\n", Kind);
            Indent (B, 2);
            Put (B, "(N : Node_Id; Offset : Field_Offset; Val : \1_Id);\n\n", Kind);
            Outdent (B, 2);

            Put (B, "procedure Set_\1_Id_With_Parent\n", Kind);
            Indent (B, 2);
            Put (B, "(N : Node_Id; Offset : Field_Offset; Val : \1_Id) is\n", Kind);
            Outdent (B, 2);
            Put (B, "begin\n");
            Indent (B, 3);
            Put (B, "if Present (Val) and then Val /= Error\1 then\n", Error);
            Indent (B, 3);
            Put (B, "pragma Warnings (Off, ""actuals for this call may be in wrong order"");\n");
            Put (B, "Set_Parent (Val, N);\n");
            Put (B, "pragma Warnings (On, ""actuals for this call may be in wrong order"");\n");
            Outdent (B, 3);
            Put (B, "end if;\n\n");

            Put (B, "Set_\1_Id (N, Offset, Val);\n", Kind);
            Outdent (B, 3);
            Put (B, "end Set_\1_Id_With_Parent;\n", Kind);
         end Put_Setter_With_Parent;

      begin
         Put (S, "with Seinfo; use Seinfo;\n");
         Put (S, "pragma Warnings (Off); -- ????\n");
         Put (S, "with Output; use Output;\n");
         Put (S, "pragma Warnings (On); -- ????\n");

         Put (S, "\npackage Sinfo.Nodes is\n\n");
         Indent (S, 3);

         Put (S, "--  This package is automatically generated.\n\n");

         Put_Type_Hierarchy (S, Node_Kind);

         Put_Type_And_Subtypes (S, Node_Kind);

         Put (S, "pragma Assert (Node_Kind'Pos (N_Unused_At_Start) = 0);\n\n");
         Put (S, "pragma Assert (Node_Kind'Last = N_Unused_At_End);\n\n");

         Put_Subp_Decls (S, Node_Kind);

         Put_Traversed_Fields (S);

         Put_Tables (S, Node_Kind);

         Outdent (S, 3);
         Put (S, "\nend Sinfo.Nodes;\n");

         Put (B, "with Atree; use Atree; use Atree.Atree_Private_Part;\n");
         Put (B, "with Nlists; use Nlists;\n");

         Put (B, "\npackage body Sinfo.Nodes is\n\n");
         Indent (B, 3);

         Put (B, "--  This package is automatically generated.\n\n");

         Put (B, "--  Instantiations of low-level getters and setters that take offsets\n");
         Put (B, "--  in units of the size of the field.\n");

         Put (B, "pragma Style_Checks (""M200"");\n");
         for T in Special_Type loop
            if Node_Field_Types_Used (T) then
               Instantiate_Low_Level_Accessors (B, T);
            end if;
         end loop;

         Put_Setter_With_Parent ("Node");
         Put_Setter_With_Parent ("List");

         Put_Subp_Bodies (B, Node_Kind);

         Outdent (B, 3);
         Put (B, "end Sinfo.Nodes;\n");

      end Put_Nodes;

      procedure Put_Entities is
         S : Sink'Class := Create_File ("einfo-entities.ads");
         B : Sink'Class := Create_File ("einfo-entities.adb");
      begin
         Put (S, "with Seinfo; use Seinfo;\n");
         Put (S, "pragma Warnings (Off); -- ????\n");
         Put (S, "with Output; use Output;\n");
         Put (S, "with Sinfo.Nodes; use Sinfo.Nodes;\n");
         Put (S, "pragma Warnings (On); -- ????\n");

         Put (S, "\npackage Einfo.Entities is\n\n");
         Indent (S, 3);

         Put (S, "--  This package is automatically generated.\n\n");

         Put_Type_Hierarchy (S, Entity_Kind);

         Put_Type_And_Subtypes (S, Entity_Kind);

         Put_Subp_Decls (S, Entity_Kind);

         Put_Tables (S, Entity_Kind);

         Outdent (S, 3);
         Put (S, "\nend Einfo.Entities;\n");

         Put (B, "with Atree; use Atree; use Atree.Atree_Private_Part;\n");
         Put (B, "with Einfo.Utils; use Einfo.Utils;\n");
         --  This forms a cycle between packages (via bodies, which is OK)

         Put (B, "\npackage body Einfo.Entities is\n\n");
         Indent (B, 3);

         Put (B, "--  This package is automatically generated.\n\n");

         Put (B, "--  Instantiations of low-level getters and setters that take offsets\n");
         Put (B, "--  in units of the size of the field.\n");

         Put (B, "pragma Style_Checks (""M200"");\n");
         for T in Special_Type loop
            if Entity_Field_Types_Used (T) then
               Instantiate_Low_Level_Accessors (B, T);
            end if;
         end loop;

         Put_Subp_Bodies (B, Entity_Kind);

         Outdent (B, 3);
         Put (B, "end Einfo.Entities;\n");

      end Put_Entities;

      procedure Put_Make_Spec
        (S : in out Sink'Class; Root : Root_Type; T : Concrete_Type)
      is
      begin
         Put (S, "function Make_\1 (Sloc : Source_Ptr", Image_Sans_N (T));
         Indent (S, 3);

         for F of Type_Table (T).Fields loop
            pragma Assert (Fields_Per_Node (T) (F));

            if Is_Syntactic (T) (F) then
               declare
                  Typ : constant String :=
                    (if Field_Table (F).Field_Type = Flag then "Boolean"
                     else Image (Field_Table (F).Field_Type));

                  --  All Flag fields have a default, which is False by
                  --  default.

                  Default : constant String :=
                    (if Field_Table (F).Default_Value = No_Default then
                     (if Field_Table (F).Field_Type = Flag then " := False" else "")
                     else " := " & Value_Image (Field_Table (F).Default_Value));

                  Suppress_Default : constant Boolean := False;
                  --  ????For testing. Strip out the defaults from the old
                  --  nmake.ads. Set this to True, and generate the new
                  --  nmake.ads. Then diff the two. Same for nmake.adb.
                  --  They should be identical, except for minor diffs like
                  --  comments.

               begin
                  Put (S, ";\n");

                  Put (S, "\1", Image (F));
                  Tab_To_Column (S, 36);
                  Put (S, " : \1\2",
                       Typ,
                       (if Suppress_Default then "" else Default));
               end;
            end if;
         end loop;

         Put (S, ")\nreturn \1_Id", Node_Or_Entity (Root));
         Outdent (S, 3);
      end Put_Make_Spec;

      procedure Put_Make_Decls (S : in out Sink'Class; Root : Root_Type) is
      begin
         --  The order of the functions doesn't matter, but we're using
         --  Sinfo_Node_Order here so we can diff the nmake code against the
         --  old version. That means this code won't work for entities.
         --  There was no Emake for entities, but it might be nice to
         --  have someday. If we want that, we should say:
         --
         --    for T in First_Concrete (Root) .. Last_Concrete (Root) loop
         --
         --  We would need to decide which fields to include as parameters,
         --  because there are no syntactic fields of entities.

         for T of Sinfo_Node_Order loop
            Put_Make_Spec (S, Root, T);
            Put (S, ";\npragma \1 (Make_\2);\n\n", Inline, Image_Sans_N (T));
         end loop;
      end Put_Make_Decls;

      procedure Put_Make_Bodies (S : in out Sink'Class; Root : Root_Type) is
      begin
         for T of Sinfo_Node_Order loop
            Put_Make_Spec (S, Root, T);
            Put (S, "\nis\n");

            Indent (S, 3);
            Put (S, "N : constant Node_Id :=\n");

            if T in Entity_Node then
               Put (S, "      New_Entity (\1, Sloc);\n", Image (T));

            else
               Put (S, "      New_Node (\1, Sloc);\n", Image (T));
            end if;

            Outdent (S, 3);

            Put (S, "begin\n");

            Indent (S, 3);
            for F of Type_Table (T).Fields loop
               pragma Assert (Fields_Per_Node (T) (F));

               if Is_Syntactic (T) (F) then
                  declare
                     NWidth : constant := 28;
                     --  This constant comes from the old Xnmake, which wraps
                     --  the Set_... call if the field name is that long or
                     --  longer.

                     F_Name : constant String := Image (F);

                  begin
                     if F_Name'Length < NWidth then
                        Put (S, "Set_\1 (N, \1);\n", F_Name);

                     --  Wrap the line

                     else
                        Put (S, "Set_\1\n", F_Name);
                        Indent (S, 2);
                        Put (S, "(N, \1);\n", F_Name);
                        Outdent (S, 2);
                     end if;
                  end;
               end if;
            end loop;

            if Is_Descendant (N_Op, T) then
               --  Special cases for N_Op nodes: fill in the Chars and Entity
               --  fields even though they were not passed in.

               declare
                  Op : constant String := Image_Sans_N (T);
                  --  This will be something like "Op_And" or "Op_Add"

                  Op_Name_With_Op : constant String :=
                    (if T = N_Op_Plus then "Op_Add"
                     elsif T = N_Op_Minus then "Op_Subtract"
                     else Op);
                  --  Special cases for unary operators that have the same name
                  --  as a binary operator; we use the binary operator name in
                  --  that case.

                  Slid : constant String (1 .. Op_Name_With_Op'Length) :=
                    Op_Name_With_Op;
                  pragma Assert (Slid (1 .. 3) = "Op_");

                  Op_Name : constant String :=
                    (if T in N_Op_Rotate_Left |
                       N_Op_Rotate_Right |
                       N_Op_Shift_Left |
                       N_Op_Shift_Right |
                       N_Op_Shift_Right_Arithmetic
                     then Slid (4 .. Slid'Last)
                     else Slid);
                  --  Special cases for shifts and rotates; the node kind has
                  --  "Op_", but the Name_Id constant does not.

               begin
                  Put (S, "Set_Chars (N, Name_\1);\n", Op_Name);
                  Put (S, "Set_Entity (N, Standard_\1);\n", Op);
               end;
            end if;

            Put (S, "return N;\n");
            Outdent (S, 3);

            Put (S, "end Make_\1;\n\n", Image_Sans_N (T));
         end loop;
      end Put_Make_Bodies;

      --  Documentation for the Nmake package, generated by Put_Nmake below.

      --  The Nmake package contains a set of routines used to construct tree
      --  nodes using a functional style. There is one routine for each node
      --  type defined in Gen_IL.Gen.Gen_Nodes with the general interface:

      --    function Make_xxx (Sloc : Source_Ptr,
      --                       Field_Name_1 : Field_Name_1_Type [:= default]
      --                       Field_Name_2 : Field_Name_2_Type [:= default]
      --                       ...)
      --    return Node_Id

      --  Only syntactic fields are included.

      --  Default values are provided as specified in Gen_Nodes, except that if
      --  no default is specified for a flag field, it has a default of False.

      --  Warning: since calls to Make_xxx routines are normal function calls, the
      --  arguments can be evaluated in any order. This means that at most one such
      --  argument can have side effects (e.g. be a call to a parse routine).

      procedure Put_Nmake is
         S : Sink'Class := Create_File ("nmake.ads");
         B : Sink'Class := Create_File ("nmake.adb");

      begin
         Put (S, "with Namet;  use Namet;\n");
         Put (S, "with Nlists; use Nlists;\n");
         Put (S, "with Types;  use Types;\n");
         Put (S, "with Uintp;  use Uintp;\n");
         Put (S, "with Urealp; use Urealp;\n");

         Put (S, "\npackage Nmake is\n\n");
         Indent (S, 3);

         Put (S, "--  This package is automatically generated.\n\n");
         Put (S, "--  See Put_Nmake in gen_il-gen.adb for documentation.\n\n");
--         Put (S, "pragma Style_Checks (""M200"");\n");
         --  ????Work around bug in a-stouut.adb.

         Put_Make_Decls (S, Node_Kind);

         Outdent (S, 3);
         Put (S, "end Nmake;\n");

         Put (B, "with Atree;  use Atree;\n");
         Put (B, "with Sinfo.Nodes; use Sinfo.Nodes;\n");
         Put (B, "with Sinfo.Utils; use Sinfo.Utils;\n");
         Put (B, "with Snames; use Snames;\n");
         Put (B, "with Stand;  use Stand;\n");

         Put (B, "\npackage body Nmake is\n\n");
         Indent (B, 3);

         Put (B, "--  This package is automatically generated.\n\n");
--         Put (B, "pragma Style_Checks (""M200"");\n");
         --  ????Work around bug in a-stouut.adb.

         Put_Make_Bodies (B, Node_Kind);

         Outdent (B, 3);
         Put (B, "end Nmake;\n");
      end Put_Nmake;

      procedure Put_Seinfo_Tables is
         S : Sink'Class := Create_File ("seinfo_tables.ads");
         B : Sink'Class := Create_File ("seinfo_tables.adb");

         Type_Layout : Type_Layout_Array;

         function Get_Last_Bit
           (T : Concrete_Type; F : Opt_Field_Enum; First_Bit : Bit_Offset)
            return Bit_Offset;
         function First_Bit_Image (First_Bit : Bit_Offset) return String;
         function Last_Bit_Image (Last_Bit : Bit_Offset) return String;

         procedure Put_Field_List (Bit : Bit_Offset);
         --  Print out the list of fields that are allocated (in part, for
         --  fields bigger than one bit) at the given bit offset. This allows
         --  us to see which fields are overlaid with each other, which should
         --  only happen if the sets of types with those fields are disjoint.

         function Get_Last_Bit
           (T : Concrete_Type; F : Opt_Field_Enum; First_Bit : Bit_Offset)
            return Bit_Offset is
         begin
            return Result : Bit_Offset do
               if F = No_Field then
                  --  We don't have a field size for No_Field, so just look at
                  --  the bits up to the next word boundary.

                  Result := First_Bit;

                  while (Result + 1) mod 32 /= 0
                    and then Type_Layout (T) (Result + 1) = No_Field
                  loop
                     Result := Result + 1;
                  end loop;

               else
                  Result := First_Bit + Field_Size (F) - 1;
               end if;
            end return;
         end Get_Last_Bit;

         function First_Bit_Image (First_Bit : Bit_Offset) return String is
            W : constant Bit_Offset := First_Bit / 32;
            B : constant Bit_Offset := First_Bit mod 32;
            pragma Assert (W * 32 + B = First_Bit);
         begin
            return
              Image (W) & "*32" & (if B = 0 then "" else " + " & Image (B));
         end First_Bit_Image;

         function Last_Bit_Image (Last_Bit : Bit_Offset) return String is
            W : constant Bit_Offset := (Last_Bit + 1) / 32;
         begin
            if W * 32 - 1 = Last_Bit then
               return Image (W) & "*32 - 1";
            else
               return First_Bit_Image (Last_Bit);
            end if;
         end Last_Bit_Image;

         function Image_Or_Waste (F : Opt_Field_Enum) return String is
           (if F = No_Field then "Wasted_Bits" else Image (F));

         Num_Wasted_Bits : Bit_Offset'Base := 0;

         Type_Layout_Size : Bit_Offset'Base := Type_Layout'Size;
         --  Total size of Type_Layout, including the Field_Arrays its
         --  components point to.

         procedure Put_Field_List (Bit : Bit_Offset) is
            First_Time : Boolean := True;
         begin
            for F in Field_Enum loop
               if F /= Between_Node_And_Entity_Fields
                 and then Bit in First_Bit (F, Field_Table (F).Offset)
                               .. Last_Bit (F, Field_Table (F).Offset)
               then
                  if First_Time then
                     First_Time := False;
                  else
                     Put (B, ",\n");
                  end if;

                  Put (B, "\1", Image (F));
               end if;
            end loop;
         end Put_Field_List;

      begin -- Put_Seinfo_Tables

         for T in Concrete_Type loop
            Type_Layout (T) := new Field_Array'
              (0 .. Type_Bit_Size_Aligned (T) - 1 => No_Field);
            Type_Layout_Size := Type_Layout_Size + Type_Layout (T).all'Size;

            for F in Field_Enum loop
               if Fields_Per_Node (T) (F) then
                  declare
                     Off : constant Field_Offset := Field_Table (F).Offset;
                     subtype Bit_Range is Bit_Offset
                       range First_Bit (F, Off) .. Last_Bit (F, Off);
                  begin
                     pragma Assert
                       (Type_Layout (T) (Bit_Range) = (Bit_Range => No_Field));
                     Type_Layout (T) (Bit_Range) := (others => F);
                  end;
               end if;
            end loop;
         end loop;

         for T in Concrete_Type loop
            for B in 0 .. Type_Bit_Size_Aligned (T) - 1 loop
               if Type_Layout (T) (B) = No_Field then
                  Num_Wasted_Bits := Num_Wasted_Bits + 1;
               end if;
            end loop;
         end loop;

         Put (S, "\npackage Seinfo_Tables is\n\n");
         Indent (S, 3);

         Put (S, "--  This package is automatically generated.\n\n");

         Put (S, "--  This package is not used by the compiler.\n");
         Put (S, "--  The body contains tables that are intended to be used by humans to\n");
         Put (S, "--  help understand the layout of various data structures.\n\n");

         Put (S, "pragma Elaborate_Body;\n");

         Outdent (S, 3);
         Put (S, "\nend Seinfo_Tables;\n");

         Put (B, "with Gen_IL.Types;  use Gen_IL.Types;\n");
         Put (B, "with Gen_IL.Fields; use Gen_IL.Fields;\n");
         Put (B, "with Gen_IL.Utils;  use Gen_IL.Utils;\n");

         Put (B, "\npackage body Seinfo_Tables is\n\n");
         Indent (B, 3);

         Put (B, "--  This package is automatically generated.\n\n");

         Put (B, "Num_Wasted_Bits : Bit_Offset'Base := \1 with Unreferenced;\n",
              Image (Num_Wasted_Bits));

         Put (B, "\nWasted_Bits : constant Opt_Field_Enum := No_Field;\n");

         Put (B, "\n--  Table showing the layout of each Node_Or_Entity_Type. For each\n");
         Put (B, "--  concrete type, we show the bits used by each field. Each field\n");
         Put (B, "--  uses the same bit range in all types. This table is not used by\n");
         Put (B, "--  the compiler; it is for information only.\n\n");

         Put (B, "--  Wasted_Bits are unused bits between fields, and padding at the end\n");
         Put (B, "--  to round up to a multiple of the slot size.\n");

         Put (B, "\n--  Type_Layout is \1 bytes.\n", Image (Type_Layout_Size / 8));

         Put (B, "\npragma Style_Checks (Off);\n");
         Put (B, "Type_Layout : constant Type_Layout_Array := \n");
         Indent (B, 2);
         Put (B, "--  Concrete node types:\n");
         Put (B, "(");
         Indent (B, 1);

         declare
            First_Time : Boolean := True;
         begin
            for T in Concrete_Type loop
               if First_Time then
                  First_Time := False;
               else
                  Put (B, ",\n\n");
               end if;

               if T = Concrete_Entity'First then
                  Put (B, "--  Concrete entity types:\n\n");
               end if;

               Put (B, "\1 => new Field_Array'\n", Image (T));

               Indent (B, 2);
               Put (B, "(");
               Indent (B, 1);

               declare
                  First_Time : Boolean := True;
                  First_Bit : Bit_Offset := 0;
               begin
                  while First_Bit < Type_Bit_Size_Aligned (T) loop
                     if First_Time then
                        First_Time := False;
                     else
                        Put (B, ",\n");
                     end if;

                     declare
                        F : constant Opt_Field_Enum :=
                          Type_Layout (T) (First_Bit);
                     begin
                        declare
                           Last_Bit : constant Bit_Offset :=
                             Get_Last_Bit (T, F, First_Bit);
                        begin
                           pragma Assert
                             (Type_Layout (T) (First_Bit .. Last_Bit) =
                                              (First_Bit .. Last_Bit => F));

                           if Last_Bit = First_Bit then
                              Put (B, "\1 => \2",
                                   First_Bit_Image (First_Bit),
                                   Image_Or_Waste (F));
                           else
                              pragma Assert
                                (if F /= No_Field then
                                  First_Bit mod Field_Size (F) = 0);
                              Put (B, "\1 .. \2 => \3",
                                   First_Bit_Image (First_Bit),
                                   Last_Bit_Image (Last_Bit),
                                   Image_Or_Waste (F));
                           end if;

                           First_Bit := Last_Bit + 1;
                        end;
                     end;
                  end loop;
               end;

               Outdent (B, 1);
               Put (B, ")");
               Outdent (B, 2);
            end loop;
         end;

         Outdent (B, 1);
         Put (B, ") -- Type_Layout\n");
         Indent (B, 6);
         Put (B, "with Export, Convention => Ada;\n");
         Outdent (B, 6);
         Outdent (B, 2);

         Put (B, "\n--  Table mapping bit offsets to the set of fields at that offset\n\n");
         Put (B, "Bit_Used : constant Offset_To_Fields_Mapping :=\n");

         Indent (B, 2);
         Put (B, "(");
         Indent (B, 1);

         declare
            First_Time : Boolean := True;
         begin
            for Bit in 0 .. Bit_Offset'Max
              (Max_Node_Bit_Size, Max_Entity_Bit_Size)
            loop
               if First_Time then
                  First_Time := False;
               else
                  Put (B, ",\n\n");
               end if;

               Put (B, "\1 => new Field_Array'\n", First_Bit_Image (Bit));

               --  Use [...] notation here, to get around annoying Ada
               --  limitations on empty and singleton aggregates. This code is
               --  not used in the compiler, so there are no bootstrap issues.

               Indent (B, 2);
               Put (B, "[");
               Indent (B, 1);

               Put_Field_List (Bit);

               Outdent (B, 1);
               Put (B, "]");
               Outdent (B, 2);
            end loop;
         end;

         Outdent (B, 1);
         Put (B, "); -- Bit_Used\n");
         Outdent (B, 2);

         Outdent (B, 3);
         Put (B, "\nend Seinfo_Tables;\n");

      end Put_Seinfo_Tables;

      procedure Put_C_Type_And_Subtypes
        (S : in out Sink'Class; Root : Root_Type) is

         procedure Put_Enum_Lit (T : Node_Or_Entity_Type);
         --  Print out the #define corresponding to the Ada enumeration literal
         --  for T in Node_Kind and Entity_Kind (i.e. concrete types).

         procedure Put_Kind_Subtype (T : Node_Or_Entity_Type);
         --  Print out the SUBTYPE macro call corresponding to an abstract
         --  type.

         procedure Put_Enum_Lit (T : Node_Or_Entity_Type) is
         begin
            if T in Concrete_Type then
               Put (S, "#define \1 \2\n", Image (T), Image (Pos (T)));
            end if;
         end Put_Enum_Lit;

         procedure Put_Kind_Subtype (T : Node_Or_Entity_Type) is
         begin
            if T in Abstract_Type and then Type_Table (T).Parent /= No_Type then
               Put (S, "SUBTYPE (\1, \2,\n",
                    Image (T),
                    Image (Type_Table (T).Parent));
               Indent (S, 3);
               Put (S, "\1,\n\2)\n",
                    Image (Type_Table (T).First),
                    Image (Type_Table (T).Last));
               Outdent (S, 3);
            end if;
         end Put_Kind_Subtype;

      begin
         Indent (S, 6);
         Iterate_Types (Root, Pre => Put_Enum_Lit'Access);

         Put (S, "\n#define Number_\1_Kinds \2\n",
              Node_Or_Entity (Root),
              Image (Pos (Last_Concrete (Root)) + 1));

         Outdent (S, 6);

         Indent (S, 3);
         Iterate_Types (Root, Pre => Put_Kind_Subtype'Access);
         Outdent (S, 3);

         Put_Union_Membership (S, Root);
      end Put_C_Type_And_Subtypes;

      procedure Put_Low_Level_C_Getter
        (S : in out Sink'Class; T : Type_Enum)
      is
         T_Image : constant String := Get_Set_Id_Image (T);

      begin
         Put (S, "static \1 Get_\2(Node_Id N, Field_Offset Offset);\n\n",
              T_Image, Image (T));
         Put (S, "INLINE \1\n", T_Image);
         Put (S, "Get_\1(Node_Id N, Field_Offset Offset)\n", Image (T));

         Indent (S, 3);

         --  Same special case as in Instantiate_Low_Level_Accessors

         if T in Elist_Id | Uint then
            pragma Assert (Field_Size (T) = 32);

            declare
               Default_Val : constant String :=
                 (if T = Elist_Id then "No_Elist" else "Uint_0");

            begin
               Put (S, "{ return (\1) Get_32_Bit_Field_With_Default(N, Offset, \2); }\n\n",
                    T_Image, Default_Val);
            end;

         else
            Put (S, "{ return (\1) Get_\2_Bit_Field(N, Offset); }\n\n",
                 T_Image, Image (Field_Size (T)));
         end if;

         Outdent (S, 3);
      end Put_Low_Level_C_Getter;

      procedure Put_High_Level_C_Getter
        (S : in out Sink'Class; F : Field_Enum)
      is
      begin
         Put (S, "INLINE \1 \2\n",
              Get_Set_Id_Image (Field_Table (F).Field_Type), Image (F));
         Put (S, "(Node_Id N)\n");

         Indent (S, 3);
         Put (S, "{ return \1(\2, \3); }\n\n",
              Low_Level_Getter (Field_Table (F).Field_Type),
              Node_To_Fetch_From (F),
              Image (Field_Table (F).Offset));
         Outdent (S, 3);
      end Put_High_Level_C_Getter;

      procedure Put_High_Level_C_Getters
        (S : in out Sink'Class; Root : Root_Type)
      is
      begin
         Put (S, "// Getters for fields\n\n");

         for F in First_Field (Root) .. Last_Field (Root) loop
            Put_High_Level_C_Getter (S, F);
         end loop;
      end Put_High_Level_C_Getters;

      procedure Put_Union_Membership
        (S : in out Sink'Class; Root : Root_Type) is

         procedure Put_Ors (T : Abstract_Type);
         --  Print the "or" (i.e. "||") of tests whether kind is in each child
         --  type.

         procedure Put_Ors (T : Abstract_Type) is
            First_Time : Boolean := True;
         begin
            for Child of Type_Table (T).Children loop
               if First_Time then
                  First_Time := False;
               else
                  Put (S, " ||\n");
               end if;

               --  Unions, other abstract types, and concrete types each have
               --  their own way of testing membership in the C++ code.

               if Child in Abstract_Type then
                  if Type_Table (Child).Is_Union then
                     Put (S, "Is_In_\1 (kind)", Image (Child));

                  else
                     Put (S, "IN (kind, \1)", Image (Child));
                  end if;

               else
                  Put (S, "kind == \1", Image (Child));
               end if;
            end loop;
         end Put_Ors;

      begin
         Put (S, "\n// Membership tests for union types\n\n");

         for T in First_Abstract (Root) .. Last_Abstract (Root) loop
            if Type_Table (T) /= null and then Type_Table (T).Is_Union then
               Put (S, "static Boolean Is_In_\1(\2_Kind kind);\n",
                    Image (T), Node_Or_Entity (Root));
               Put (S, "INLINE Boolean\n");
               Put (S, "Is_In_\1(\2_Kind kind)\n",
                    Image (T), Node_Or_Entity (Root));

               Put (S, "{\n");
               Indent (S, 3);
               Put (S, "return\n");
               Indent (S, 3);
               Put_Ors (T);
               Outdent (S, 3);
               Outdent (S, 3);
               Put (S, ";\n}\n");

               Put (S, "\n");
            end if;
         end loop;
      end Put_Union_Membership;

      procedure Put_Sinfo_Dot_H is
         S : Sink'Class := Create_File ("sinfo.h");

      begin
         Put (S, "#ifdef __cplusplus\n");
         Put (S, "extern ""C"" {\n");
         Put (S, "#endif\n\n");

         Put (S, "typedef Boolean Flag;\n\n");

         Put_C_Type_And_Subtypes (S, Node_Kind);

         Put (S, "\n// Getters corresponding to instantiations of Atree.Get_n_Bit_Field\n");
         Put (S, "// generic functions.\n\n");

         for T in Special_Type loop
            Put_Low_Level_C_Getter (S, T);
         end loop;

         Put_High_Level_C_Getters (S, Node_Kind);

         Put (S, "#ifdef __cplusplus\n");
         Put (S, "}\n");
         Put (S, "#endif\n");
      end Put_Sinfo_Dot_H;

      procedure Put_Einfo_Dot_H is
         S : Sink'Class := Create_File ("einfo.h");

         procedure Put_Membership_Query_Spec (T : Node_Or_Entity_Type);
         procedure Put_Membership_Query_Decl (T : Node_Or_Entity_Type);
         procedure Put_Membership_Query_Defn (T : Node_Or_Entity_Type);
         --  Print out the Is_... function for T that calls the IN macro on the
         --  SUBTYPE.

         procedure Put_Membership_Query_Spec (T : Node_Or_Entity_Type) is
            Im : constant String := Image (T);
            pragma Assert (Im (Im'Last - 4 .. Im'Last) = "_Kind");
            Im2 : constant String := Im (Im'First .. Im'Last - 5);
            Typ : constant String :=
              (if Is_Descendant (Type_Kind, T)
                 and then T /= Type_Kind
               then "_Type"
               else "");
         begin
            pragma Assert (not Type_Table (T).Is_Union);

            Put (S, "INLINE B Is_\1\2 ", Im2, Typ);
            Tab_To_Column (S, 49);
            Put (S, "(E Id)");
         end Put_Membership_Query_Spec;

         procedure Put_Membership_Query_Decl (T : Node_Or_Entity_Type) is
         begin
            if T in Abstract_Type and T not in Root_Type then
               Put_Membership_Query_Spec (T);
               Put (S, ";\n");
            end if;
         end Put_Membership_Query_Decl;

         procedure Put_Membership_Query_Defn (T : Node_Or_Entity_Type) is
         begin
            if T in Abstract_Type and T not in Root_Type then
               Put_Membership_Query_Spec (T);
               Put (S, "\n");
               Indent (S, 3);
               Put (S, "{ return IN (Ekind (Id), \1); }\n", Image (T));
               Outdent (S, 3);
            end if;
         end Put_Membership_Query_Defn;

      begin
         Put (S, "#ifdef __cplusplus\n");
         Put (S, "extern ""C"" {\n");
         Put (S, "#endif\n\n");

         Put (S, "typedef Boolean Flag;\n\n");

         Put_C_Type_And_Subtypes (S, Entity_Kind);

         Put (S, "\n// Getters corresponding to instantiations of Atree.Get_n_Bit_Field\n");
         Put (S, "// generic functions.\n\n");

         --  Note that we do not call Put_Low_Level_C_Getter here. Those are in
         --  sinfo.h, so every file that #includes einfo.h must #include
         --  sinfo.h first.

         Put_High_Level_C_Getters (S, Entity_Kind);

         Put (S, "\n// Abstract type queries\n\n");

         Indent (S, 3);
         Iterate_Types (Entity_Kind, Pre => Put_Membership_Query_Decl'Access);
         Put (S, "\n");
         Iterate_Types (Entity_Kind, Pre => Put_Membership_Query_Defn'Access);
         Outdent (S, 3);

         Put (S, "#ifdef __cplusplus\n");
         Put (S, "}\n");
         Put (S, "#endif\n");
      end Put_Einfo_Dot_H;

   begin -- Compile

      Check_Completeness;

      Compute_Ranges (Node_Kind);
      Compute_Ranges (Entity_Kind);
      Compute_Fields_Per_Node;
      Compute_Field_Offsets;
      Compute_Type_Sizes;
      Check_For_Syntactic_Mismatch;

      Verify_Type_Table;

      Node_Field_Types_Used :=
        Field_Types_Used (Node_Field'First, Node_Field'Last);
      Entity_Field_Types_Used :=
        Field_Types_Used (Entity_Field'First, Entity_Field'Last);

      Put_Seinfo;

      Put_Nodes;

      Put_Entities;

      Put_Nmake;

      Put_Seinfo_Tables;

      Put_Sinfo_Dot_H;
      Put_Einfo_Dot_H;

   end Compile;

   function Sy
     (Field      : Node_Field;
      Field_Type : Type_Enum;
      Default_Value : Field_Default_Value := No_Default;
      Pre        : String := "") return Field_Sequence is
   begin
      return
        (1 => Create_Syntactic_Field (Field, Field_Type, Default_Value, Pre));
   end Sy;

   function Sm
     (Field      : Field_Enum;
      Field_Type : Type_Enum;
      Type_Only  : Type_Only_Enum := No_Type_Only;
      Pre        : String := "") return Field_Sequence is
   begin
      return (1 => Create_Semantic_Field (Field, Field_Type, Type_Only, Pre));
   end Sm;

end Gen_IL.Gen;
