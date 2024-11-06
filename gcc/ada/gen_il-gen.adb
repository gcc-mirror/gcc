------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                            G E N _ I L . G E N                           --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2020-2024, Free Software Foundation, Inc.         --
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
with Ada.Text_IO;

package body Gen_IL.Gen is

   Statistics_Enabled : constant Boolean := False;
   --  Change to True or False to enable/disable statistics printed by
   --  Atree. Should normally be False, for efficiency. Also compile with
   --  -gnatd.A to get the statistics printed.  Enabling these statistics
   --  makes the compiler about 20% slower.

   Num_Header_Slots : constant := 3;
   --  Number of header slots; the first Num_Header_Slots slots are stored in
   --  the header; the rest are dynamically allocated in the Slots table. We
   --  need to subtract this off when accessing dynamic slots. The constant
   --  Seinfo.N_Head will contain this value. Fields that are allocated in the
   --  header slots are quicker to access.
   --
   --  This number can be adjusted for efficiency. We choose 3 because the
   --  minimum node size is 3 slots, and because that causes the size of type
   --  Node_Header to be a power of 2. We can't make it zero, however, because
   --  C doesn't allow zero-length arrays.

   N_Head : constant String := Image (Field_Offset'(Num_Header_Slots));
   --  String form of the above

   Enable_Assertions : constant Boolean := True;
   --  True to enable predicates on the _Id types, and preconditions on getters
   --  and setters.

   Overlay_Fields : constant Boolean := True;
   --  False to allocate every field so it doesn't overlay any other fields,
   --  which results in enormous nodes. For experimenting and debugging.
   --  Should be True in normal operation, for efficiency.

   SS : constant := 32; -- slot size in bits
   SSS : constant String := Image (Bit_Offset'(SS));

   Inline : constant String := "Inline";
   --  For experimenting with Inline_Always

   Syntactic : Fields_Per_Node_Type :=
     (others => (others => False));

   Nodes_And_Entities : constant Type_Vector := Node_Kind & Entity_Kind;
   All_Entities : constant Type_Vector := To_Vector (Entity_Kind, Length => 1);

   procedure Create_Type
     (T            : Node_Or_Entity_Type;
      Parent       : Opt_Abstract_Type;
      Fields       : Field_Sequence;
      Nmake_Assert : String);
   --  Called by the Create_..._Type procedures exported by this package to
   --  create an entry in the Types_Table.

   procedure Create_Union_Type
     (Root : Root_Type; T : Abstract_Type; Children : Type_Array);
   --  Called by Create_Node_Union_Type and Create_Entity_Union_Type to create
   --  a union type.

   function Create_Field
     (Field                 : Field_Enum;
      Field_Type            : Type_Enum;
      Default_Value         : Field_Default_Value;
      Type_Only             : Type_Only_Enum;
      Pre, Pre_Get, Pre_Set : String;
      Is_Syntactic          : Boolean) return Field_Desc;
   --  Called by the Create_..._Field functions exported by this package to
   --  create an entry in the Field_Table. See Create_Syntactic_Field and
   --  Create_Semantic_Field for additional doc.

   procedure Check_Type (T : Node_Or_Entity_Type);
   --  Check some "legality" rules for types in the Gen_IL little language

   ----------------
   -- Check_Type --
   ----------------

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

   -----------------
   -- Create_Type --
   -----------------

   procedure Create_Type
     (T            : Node_Or_Entity_Type;
      Parent       : Opt_Abstract_Type;
      Fields       : Field_Sequence;
      Nmake_Assert : String)
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
           First | Last | Fields => <>, -- filled in later
           Nmake_Assert => new String'(Nmake_Assert));

      if Parent /= No_Type then
         Append (Type_Table (Parent).Children, T);
      end if;

      --  Check that syntactic fields precede semantic fields. Note that this
      --  check is happening before we compute inherited fields.
      --  Exempt Chars and Actions from this rule, for now.

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

      --  Check that node fields are in nodes, and entity fields are in
      --  entities.

      for J in Fields'Range loop
         declare
            Field : constant Field_Enum := Fields (J).F;
            Error_Prefix : constant String :=
              "Field " & Image (T) & "." & Image (Field) & " not in ";
         begin
            case T is
               when Node_Type =>
                  if Field not in Node_Field then
                     raise Illegal with Error_Prefix & "Node_Field";
                  end if;

               when Entity_Type =>
                  if Field not in Entity_Field then
                     raise Illegal with Error_Prefix & "Entity_Field";
                  end if;

               when Type_Boundaries =>
                  raise Program_Error; -- dummy types shouldn't have fields
            end case;
         end;
      end loop;

      --  Compute the Have_This_Field component of fields, the Fields component
      --  of the current type, and Syntactic table.

      for J in Fields'Range loop
         declare
            Field : constant Field_Enum := Fields (J).F;
            Is_Syntactic : constant Boolean := Fields (J).Is_Syntactic;

         begin
            Append (Field_Table (Field).Have_This_Field, T);
            Append (Type_Table (T).Fields, Field);

            pragma Assert (not Syntactic (T) (Field));
            Syntactic (T) (Field) := Is_Syntactic;
         end;
      end loop;
   end Create_Type;

   --  Other than constraint checks on T at the call site, and the lack of a
   --  parent for root types, the following six all do the same thing.

   ---------------------------
   -- Create_Root_Node_Type --
   ---------------------------

   procedure Create_Root_Node_Type
     (T      : Abstract_Node;
      Fields : Field_Sequence := No_Fields) is
   begin
      Create_Type (T, Parent => No_Type, Fields => Fields, Nmake_Assert => "");
   end Create_Root_Node_Type;

   -------------------------------
   -- Create_Abstract_Node_Type --
   -------------------------------

   procedure Create_Abstract_Node_Type
     (T      : Abstract_Node; Parent : Abstract_Type;
      Fields : Field_Sequence := No_Fields)
   is
   begin
      Create_Type (T, Parent, Fields, Nmake_Assert => "");
   end Create_Abstract_Node_Type;

   -------------------------------
   -- Create_Concrete_Node_Type --
   -------------------------------

   procedure Create_Concrete_Node_Type
     (T      : Concrete_Node; Parent : Abstract_Type;
      Fields : Field_Sequence := No_Fields;
      Nmake_Assert : String := "")
   is
   begin
      Create_Type (T, Parent, Fields, Nmake_Assert);
   end Create_Concrete_Node_Type;

   -----------------------------
   -- Create_Root_Entity_Type --
   -----------------------------

   procedure Create_Root_Entity_Type
     (T      : Abstract_Entity;
      Fields : Field_Sequence := No_Fields) is
   begin
      Create_Type (T, Parent => No_Type, Fields => Fields, Nmake_Assert => "");
   end Create_Root_Entity_Type;

   ---------------------------------
   -- Create_Abstract_Entity_Type --
   ---------------------------------

   procedure Create_Abstract_Entity_Type
     (T      : Abstract_Entity; Parent : Abstract_Type;
      Fields : Field_Sequence := No_Fields)
   is
   begin
      Create_Type (T, Parent, Fields, Nmake_Assert => "");
   end Create_Abstract_Entity_Type;

   ---------------------------------
   -- Create_Concrete_Entity_Type --
   ---------------------------------

   procedure Create_Concrete_Entity_Type
     (T      : Concrete_Entity; Parent : Abstract_Type;
      Fields : Field_Sequence := No_Fields)
   is
   begin
      Create_Type (T, Parent, Fields, Nmake_Assert => "");
   end Create_Concrete_Entity_Type;

   ------------------
   -- Create_Field --
   ------------------

   function Create_Field
     (Field                 : Field_Enum;
      Field_Type            : Type_Enum;
      Default_Value         : Field_Default_Value;
      Type_Only             : Type_Only_Enum;
      Pre, Pre_Get, Pre_Set : String;
      Is_Syntactic          : Boolean) return Field_Desc
   is
   begin
      --  Note that this function has the side effect of update the
      --  Field_Table.

      pragma Assert (if Default_Value /= No_Default then Is_Syntactic);
      pragma Assert (if Type_Only /= No_Type_Only then not Is_Syntactic);

      --  First time this field has been seen; create an entry in the
      --  Field_Table.

      if Field_Table (Field) = null then
         Field_Table (Field) := new Field_Info'
           (Type_Vectors.Empty_Vector, Field_Type, Default_Value, Type_Only,
            Pre => new String'(Pre),
            Pre_Get => new String'(Pre_Get),
            Pre_Set => new String'(Pre_Set),
            Offset => Unknown_Offset);

      --  The Field_Table entry has already been created by the 'then' part
      --  above. Now we're seeing the same field being "created" again in a
      --  different type. Here we check consistency of this new Create_Field
      --  call with the old one.

      else
         if Field_Type /= Field_Table (Field).Field_Type then
            raise Illegal with
              "mismatched field types for " & Image (Field);
         end if;

         --  Check that default values for syntactic fields match. This check
         --  could be stricter; it currently allows a field to have No_Default
         --  in one type, but something else in another type. In that case, we
         --  use the "something else" for all types.
         --
         --  Note that the order of calls does not matter; a default value
         --  always overrides a No_Default value.

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

         if Pre_Get /= Field_Table (Field).Pre_Get.all then
            raise Illegal with
              "mismatched extra getter-only preconditions for " &
              Image (Field);
         end if;

         if Pre_Set /= Field_Table (Field).Pre_Set.all then
            raise Illegal with
              "mismatched extra setter-only preconditions for " &
              Image (Field);
         end if;
      end if;

      return (Field, Is_Syntactic);
   end Create_Field;

   ----------------------------
   -- Create_Syntactic_Field --
   ----------------------------

   function Create_Syntactic_Field
     (Field      : Node_Field;
      Field_Type : Type_Enum;
      Default_Value : Field_Default_Value := No_Default;
      Pre, Pre_Get, Pre_Set : String := "") return Field_Desc
   is
   begin
      return Create_Field
        (Field, Field_Type, Default_Value, No_Type_Only,
         Pre, Pre_Get, Pre_Set,
         Is_Syntactic => True);
   end Create_Syntactic_Field;

   ---------------------------
   -- Create_Semantic_Field --
   ---------------------------

   function Create_Semantic_Field
     (Field      : Field_Enum;
      Field_Type : Type_Enum;
      Type_Only  : Type_Only_Enum := No_Type_Only;
      Pre, Pre_Get, Pre_Set : String := "") return Field_Desc
   is
   begin
      return Create_Field
        (Field, Field_Type, No_Default, Type_Only,
         Pre, Pre_Get, Pre_Set,
         Is_Syntactic => False);
   end Create_Semantic_Field;

   -----------------------
   -- Create_Union_Type --
   -----------------------

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

   ----------------------------
   -- Create_Node_Union_Type --
   ----------------------------

   procedure Create_Node_Union_Type
     (T : Abstract_Node; Children : Type_Array) is
   begin
      Create_Union_Type (Node_Kind, T, Children);
   end Create_Node_Union_Type;

   ------------------------------
   -- Create_Entity_Union_Type --
   ------------------------------

   procedure Create_Entity_Union_Type
     (T : Abstract_Entity; Children : Type_Array) is
   begin
      Create_Union_Type (Entity_Kind, T, Children);
   end Create_Entity_Union_Type;

   -------------
   -- Compile --
   -------------

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

      Node_Field_Types_Used, Entity_Field_Types_Used : Type_Set;

      Setter_Needs_Parent : Field_Set :=
        (Actions | Expression | Then_Actions | Else_Actions => True,
         others => False);
      --  Set of fields where the setter should set the Parent. True for
      --  syntactic fields of type Node_Id and List_Id, but with some
      --  exceptions. Expression is syntactic AND semantic, and the Parent
      --  is needed. Default_Expression is also both, but the Parent is not
      --  needed. Then_Actions and Else_Actions are not syntactic, but the
      --  Parent is needed.

      procedure Check_Completeness;
      --  Check that every type and field has been declared

      procedure Compute_Ranges (Root : Root_Type);
      --  Compute the range of Node_Kind/Entity_Kind values for all the types
      --  rooted at Root. The result is stored in the First and Last components
      --  in the Type_Table.

      procedure Compute_Fields_Per_Node;
      --  Compute which fields are in which nodes. Implements inheritance of
      --  fields. Set the Fields component of each Type_Info to include
      --  inherited ones. Set the Is_Syntactic component in the Type_Table to
      --  the set of fields that are syntactic in that node kind. Set the
      --  Fields_Per_Node table.

      procedure Compute_Field_Offsets;
      --  Compute the offsets of each field. The results are stored in the
      --  Offset components in the Field_Table.

      procedure Compute_Type_Sizes;
      --  Compute the size of each node and entity type, which is one more than
      --  the maximum bit offset of all fields of the type. Results are
      --  returned in the above Type_Bit_Size and Min_.../Max_... variables.

      procedure Check_For_Syntactic_Field_Mismatch;
      --  Check that fields are either all syntactic or all semantic in all
      --  nodes in which they exist, except for some fields that already
      --  violate this rule.
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
        (S : in out Sink; Root : Root_Type);
      --  Called by Put_Nodes and Put_Entities to print out the main type
      --  and subtype declarations in Sinfo.Nodes and Einfo.Entities.

      procedure Put_Subp_Decls (S : in out Sink; Root : Root_Type);
      --  Called by Put_Nodes and Put_Entities to print out the subprogram
      --  declarations in Sinfo.Nodes and Einfo.Entities.

      procedure Put_Subp_Bodies (S : in out Sink; Root : Root_Type);
      --  Called by Put_Nodes and Put_Entities to print out the subprogram
      --  bodies in Sinfo.Nodes and Einfo.Entities.

      function Node_To_Fetch_From (F : Field_Enum) return String;
      --  Name of the Node from which a getter should fetch the value.
      --  Normally, we fetch from the node or entity passed in (i.e. formal
      --  parameter N). But if Type_Only was specified, we need to fetch the
      --  corresponding base (etc) type.

      procedure Put_Getter_Spec (S : in out Sink; F : Field_Enum);
      procedure Put_Setter_Spec (S : in out Sink; F : Field_Enum);
      procedure Put_Getter_Decl (S : in out Sink; F : Field_Enum);
      procedure Put_Setter_Decl (S : in out Sink; F : Field_Enum);
      procedure Put_Getter_Setter_Locals
        (S : in out Sink; F : Field_Enum; Get : Boolean);
      procedure Put_Getter_Body (S : in out Sink; F : Field_Enum);
      procedure Put_Setter_Body (S : in out Sink; F : Field_Enum);
      --  Print out the specification, declaration, or body of a getter or
      --  setter for the given field.

      procedure Put_Precondition
        (S : in out Sink; F : Field_Enum);
      --  Print out the precondition, if any, for a getter or setter for the
      --  given field.

      procedure Put_Casts
        (S : in out Sink; T : Type_Enum);
      --  Print out the Cast functions for a given type

      procedure Put_Traversed_Fields (S : in out Sink);
      --  Called by Put_Nodes to print out the Traversed_Fields table in
      --  Sinfo.Nodes.

      procedure Put_Tables (S : in out Sink; Root : Root_Type);
      --  Called by Put_Nodes and Put_Entities to print out the various tables
      --  in Sinfo.Nodes and Einfo.Entities.

      procedure Put_Nmake;
      --  Print out the Nmake package spec and body, containing
      --  Make_... functions for each concrete node type.

      procedure Put_Make_Decls (S : in out Sink; Root : Root_Type);
      --  Called by Put_Nmake to print out the Make_... function declarations

      procedure Put_Make_Bodies (S : in out Sink; Root : Root_Type);
      --  Called by Put_Nmake to print out the Make_... function bodies

      procedure Put_Make_Spec
        (S : in out Sink; Root : Root_Type; T : Concrete_Type);
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
        (S : in out Sink; Root : Root_Type);
      --  Used by Put_Sinfo_Dot_H and Put_Einfo_Dot_H to print out the C code
      --  corresponding to the Ada Node_Kind, Entity_Kind, and subtypes
      --  thereof.

      procedure Put_C_Getters
        (S : in out Sink; Root : Root_Type);
      --  Used by Put_Sinfo_Dot_H and Put_Einfo_Dot_H to print out high-level
      --  getters.

      procedure Put_C_Getter
        (S : in out Sink; F : Field_Enum);
      --  Used by Put_C_Getters to print out one high-level getter.

      procedure Put_Union_Membership
        (S : in out Sink; Root : Root_Type; Only_Prototypes : Boolean);
      --  Used by Put_Sinfo_Dot_H and Put_Einfo_Dot_H to print out functions to
      --  test membership in a union type.

      ------------------------
      -- Check_Completeness --
      ------------------------

      procedure Check_Completeness is
      begin
         for T in Node_Or_Entity_Type loop
            if Type_Table (T) = null and then T not in Type_Boundaries then
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

      --------------------
      -- Compute_Ranges --
      --------------------

      procedure Compute_Ranges (Root : Root_Type) is

         procedure Do_One_Type (T : Node_Or_Entity_Type);
         --  Compute the range for one type. Passed to Iterate_Types to process
         --  all of them.

         procedure Add_Concrete_Descendant_To_Ancestors
           (Ancestor : Abstract_Type; Descendant : Concrete_Type);
         --  Add Descendant to the Concrete_Descendants of each of its
         --  ancestors.

         procedure Add_Concrete_Descendant_To_Ancestors
           (Ancestor : Abstract_Type; Descendant : Concrete_Type) is
         begin
            if Ancestor not in Root_Type then
               Add_Concrete_Descendant_To_Ancestors
                 (Type_Table (Ancestor).Parent, Descendant);
            end if;

            Append (Type_Table (Ancestor).Concrete_Descendants, Descendant);
         end Add_Concrete_Descendant_To_Ancestors;

         procedure Do_One_Type (T : Node_Or_Entity_Type) is
         begin
            case T is
               when Concrete_Type =>
                  pragma Annotate (Codepeer, Modified, Type_Table);
                  Type_Table (T).First := T;
                  Type_Table (T).Last  := T;
                  Add_Concrete_Descendant_To_Ancestors
                    (Type_Table (T).Parent, T);
                  --  Parent cannot be No_Type here, because T is a concrete
                  --  type, and therefore not a root type.

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

                     --  We know that each abstract type has at least two
                     --  children. The concrete types must be ordered so
                     --  that each abstract type is a contiguous subrange.

                     if Type_Table (T).First >= Type_Table (T).Last then
                        raise Illegal with
                          Image (T) & " children out of order";
                     end if;
                  end;

               when Between_Abstract_Entity_And_Concrete_Node_Types =>
                  raise Program_Error;
            end case;
         end Do_One_Type;
      begin
         Iterate_Types (Root, Post => Do_One_Type'Access);
      end Compute_Ranges;

      -----------------------------
      -- Compute_Fields_Per_Node --
      -----------------------------

      procedure Compute_Fields_Per_Node is

         Duplicate_Fields_Found : Boolean := False;

         function Get_Fields (T : Node_Or_Entity_Type) return Field_Vector;
         --  Compute the fields of a given type. This is the fields inherited
         --  from ancestors, plus the fields declared for the type itself.

         function Get_Syntactic_Fields
           (T : Node_Or_Entity_Type) return Field_Set;
         --  Compute the set of fields that are syntactic for a given type.
         --  Note that a field can be syntactic in some node types, but
         --  semantic in others.

         procedure Do_Concrete_Type (CT : Concrete_Type);
         --  Do the Compute_Fields_Per_Node work for a concrete type

         function Get_Fields (T : Node_Or_Entity_Type) return Field_Vector is
            Parent_Fields : constant Field_Vector :=
              (if T in Root_Type then Field_Vectors.Empty_Vector
               else Get_Fields (Type_Table (T).Parent));
         begin
            return Parent_Fields & Type_Table (T).Fields;
         end Get_Fields;

         function Get_Syntactic_Fields
           (T : Node_Or_Entity_Type) return Field_Set
         is
            Parent_Is_Syntactic : constant Field_Set :=
              (if T in Root_Type then (Field_Enum => False)
               else Get_Syntactic_Fields (Type_Table (T).Parent));
         begin
            return Parent_Is_Syntactic or Syntactic (T);
         end Get_Syntactic_Fields;

         procedure Do_Concrete_Type (CT : Concrete_Type) is
         begin
            Type_Table (CT).Fields := Get_Fields (CT);
            Syntactic (CT) := Get_Syntactic_Fields (CT);

            for F of Type_Table (CT).Fields loop
               if Fields_Per_Node (CT) (F) then
                  Ada.Text_IO.Put_Line
                    ("duplicate field" & Image (CT) & Image (F));
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
          when Flag => 1,

          when Small_Paren_Count_Type | Component_Alignment_Kind => 2,

          when Node_Kind_Type | Entity_Kind_Type | Convention_Id => 8,

          when Mechanism_Type
             | List_Id
             | Elist_Id
             | Name_Id
             | String_Id
             | Uint
             | Uint_Subtype
             | Ureal
             | Source_File_Index
             | Source_Ptr
             | Union_Id
             | Node_Id
             | Node_Or_Entity_Type => 32,

         when Between_Special_And_Abstract_Node_Types => -- can't happen
           Bit_Offset'Last);
         --  Size in bits of a a field of type T. It must be a power of 2, and
         --  must match the size of the type in GNAT, which sometimes requires
         --  a Size clause in GNAT.
         --
         --  Note that this is not the same as Type_Bit_Size of the field's
         --  type. For one thing, Type_Bit_Size only covers concrete node and
         --  entity types, which does not include most of the above. For
         --  another thing, Type_Bit_Size includes the full size of all the
         --  fields, whereas a field of a node or entity type is just a 32-bit
         --  Node_Id or Entity_Id; i.e. it is indirect.

      function Field_Size (F : Field_Enum) return Bit_Offset is
        (Field_Size (Field_Table (F).Field_Type));

      function To_Bit_Offset (F : Field_Enum; Offset : Field_Offset'Base)
        return Bit_Offset'Base is
          (Bit_Offset'Base (Offset) * Field_Size (F));
      function First_Bit (F : Field_Enum; Offset : Field_Offset)
        return Bit_Offset is
          (To_Bit_Offset (F, Offset));
      function Last_Bit (F : Field_Enum; Offset : Field_Offset)
        return Bit_Offset is
          (To_Bit_Offset (F, Offset + 1) - 1);

      function To_Size_In_Slots (Size_In_Bits : Bit_Offset)
        return Field_Offset is
          ((Field_Offset (Size_In_Bits) + (SS - 1)) / SS);

      function Type_Size_In_Slots (T : Concrete_Type) return Field_Offset is
        (To_Size_In_Slots (Type_Bit_Size (T))); -- rounded up to slot boundary

      function Type_Bit_Size_Aligned (T : Concrete_Type) return Bit_Offset is
        (Bit_Offset (Type_Size_In_Slots (T)) * SS); -- multiple of slot size

      ---------------------------
      -- Compute_Field_Offsets --
      ---------------------------

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

         procedure Set_Offset_In_Use
           (F : Field_Enum; Offset : Field_Offset);
         --  Mark the offset as "in use"

         procedure Choose_Offset (F : Field_Enum);
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

         procedure Set_Offset_In_Use
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
         end Set_Offset_In_Use;

         procedure Choose_Offset (F : Field_Enum) is
         begin
            for Offset in Field_Offset loop
               if Offset_OK (F, Offset) then
                  Set_Offset_In_Use (F, Offset);

                  Field_Table (F).Offset := Offset;
                  return;
               end if;
            end loop;

            raise Illegal with "No available field offset for " & Image (F) &
              "; need to increase Gen_IL.Internals.Bit_Offset'Last (" &
              Image (Gen_IL.Internals.Bit_Offset'Last) & " is too small)";
         end Choose_Offset;

         Weighted_Node_Frequency : array (Field_Enum) of Type_Count :=
           (others => 0);
         --  Number of concrete types that have each field

         function More_Types_Have_Field (F1, F2 : Field_Enum) return Boolean is
           (Weighted_Node_Frequency (F1) > Weighted_Node_Frequency (F2));
         --  True if F1 appears in more concrete types than F2

         function Sort_Less (F1, F2 : Field_Enum) return Boolean is
           (if Weighted_Node_Frequency (F1) = Weighted_Node_Frequency (F2) then
              F1 < F2
            else More_Types_Have_Field (F1, F2));

         package Sorting is new Field_Vectors.Generic_Sorting
           ("<" => Sort_Less);

         All_Fields : Field_Vector;

      --  Start of processing for Compute_Field_Offsets

      begin
         --  Compute the number of types that have each field, weighted by the
         --  frequency of such nodes.

         for T in Concrete_Type loop
            for F in Field_Enum loop
               if Fields_Per_Node (T) (F) then
                  Weighted_Node_Frequency (F) :=
                    Weighted_Node_Frequency (F) + Type_Frequency (T);
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
         --  This is for efficiency; we want to choose the offsets of the most
         --  common fields first, so they get low numbers.

         Sorting.Sort (All_Fields);

         --  Go through all the fields, and choose the lowest offset that is
         --  free in all types that have the field. This is basically a
         --  graph-coloring algorithm on the interference graph. The
         --  interference graph is an undirected graph with the fields being
         --  nodes (not nodes in the compiler!) in the graph, and an edge
         --  between a pair of fields if they appear in the same node in the
         --  compiler. The "colors" are fields offsets, except that a
         --  complication compared to standard graph coloring is that fields
         --  are different sizes.

         --  First choose offsets for some heavily-used fields, so they will
         --  get low offsets, so they will wind up in the node header for
         --  faster access.

         Choose_Offset (Nkind);
         pragma Assert (Field_Table (Nkind).Offset = 0);
         Choose_Offset (Ekind);
         pragma Assert (Field_Table (Ekind).Offset = 1);
         Choose_Offset (Homonym);
         pragma Assert (Field_Table (Homonym).Offset = 1);
         Choose_Offset (Is_Immediately_Visible);
         pragma Assert (Field_Table (Is_Immediately_Visible).Offset = 16);
         Choose_Offset (From_Limited_With);
         pragma Assert (Field_Table (From_Limited_With).Offset = 17);
         Choose_Offset (Is_Potentially_Use_Visible);
         pragma Assert (Field_Table (Is_Potentially_Use_Visible).Offset = 18);
         Choose_Offset (Is_Generic_Instance);
         pragma Assert (Field_Table (Is_Generic_Instance).Offset = 19);
         Choose_Offset (Scope);
         pragma Assert (Field_Table (Scope).Offset = 2);

         --  Then loop through them all, skipping the ones we did above

         for F of All_Fields loop
            if Field_Table (F).Offset = Unknown_Offset then
               Choose_Offset (F);
            end if;
         end loop;

      end Compute_Field_Offsets;

      ------------------------
      -- Compute_Type_Sizes --
      ------------------------

      procedure Compute_Type_Sizes is
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

               --  No type can be smaller than the header slots

               Type_Bit_Size (T) :=
                 Bit_Offset'Max (Max_Offset + 1, SS * Num_Header_Slots);
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
      end Compute_Type_Sizes;

      ----------------------------------------
      -- Check_For_Syntactic_Field_Mismatch --
      ----------------------------------------

      procedure Check_For_Syntactic_Field_Mismatch is
      begin
         for F in Field_Enum loop
            if F /= Between_Node_And_Entity_Fields then
               declare
                  Syntactic_Seen, Semantic_Seen : Boolean := False;
                  Have_Field : Type_Vector renames
                    Field_Table (F).Have_This_Field;

               begin
                  for J in 1 .. Last_Index (Have_Field) loop
                     if Syntactic (Have_Field (J)) (F) then
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

                     if Field_Table (F).Field_Type in Traversed_Field_Type
                       and then Syntactic_Seen
                     then
                        Setter_Needs_Parent (F) := True;
                     end if;
                  end if;
               end;
            end if;
         end loop;
      end Check_For_Syntactic_Field_Mismatch;

      ----------------------
      -- Field_Types_Used --
      ----------------------

      function Field_Types_Used (First, Last : Field_Enum) return Type_Set is
         Result : Type_Set := (others => False);
      begin
         for F in First .. Last loop
            if Field_Table (F).Field_Type in Node_Or_Entity_Type then
               Result (Node_Id) := True;

            --  Subtypes of Uint all use the same Cast for Uint

            elsif Field_Table (F).Field_Type in Uint_Subtype then
               Result (Uint) := True;

            else
               Result (Field_Table (F).Field_Type) := True;
            end if;
         end loop;

         return Result;
      end Field_Types_Used;

      pragma Style_Checks ("M120");
      --  Lines of the form Put (S, "..."); are more readable if we relax the
      --  line length. We really just want the "..." to be short enough.

      ---------------------------
      -- Put_Type_And_Subtypes --
      ---------------------------

      procedure Put_Type_And_Subtypes
        (S : in out Sink; Root : Root_Type)
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

         procedure Put_Opt_Subtype (T : Node_Or_Entity_Type);
         --  Print out an "optional" subtype; that is, one that allows
         --  Empty. Their names start with "Opt_".

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
                     Put (S, "," & LF);
                  end if;

                  Put (S, Image (T));
               end if;
            end Put_Enum_Lit;

            type Dummy is array
              (First_Concrete (Root) .. Last_Concrete (Root)) of Boolean;
            Num_Types : constant Root_Int := Dummy'Length;

         begin
            Put (S, "type " & Image (Root) & " is -- " &
                    Image (Num_Types) & " " & Image (Root) & "s" & LF);
            Increase_Indent (S, 2);
            Put (S, "(");
            Increase_Indent (S, 1);
            Iterate_Types (Root, Pre => Put_Enum_Lit'Access);
            Decrease_Indent (S, 1);
            Put (S, LF & ") with Size => 8; -- " & Image (Root) & LF & LF);
            Decrease_Indent (S, 2);
         end Put_Enum_Type;

         procedure Put_Kind_Subtype (T : Node_Or_Entity_Type) is
         begin
            if T in Abstract_Type then
               if Type_Table (T).Is_Union then
                  pragma Assert (Type_Table (T).Parent = Root);

                  Put (S, "subtype " & Image (T) & " is" & LF);
                  Increase_Indent (S, 2);
                  Put (S, Image (Root) & " with Predicate =>" & LF);
                  Increase_Indent (S, 2);
                  Put (S, Image (T) & " in" & LF);
                  Put_Types_With_Bars (S, Type_Table (T).Children);
                  Decrease_Indent (S, 2);
                  Put (S, ";" & LF);
                  Decrease_Indent (S, 2);

               elsif Type_Table (T).Parent /= No_Type then
                  Put (S, "subtype " & Image (T) & " is " &
                       Image (Type_Table (T).Parent) & " range" & LF);
                  Increase_Indent (S, 2);
                  Put (S, Image (Type_Table (T).First) & " .. " &
                          Image (Type_Table (T).Last) & ";" & LF);
                  Decrease_Indent (S, 2);

                  Increase_Indent (S, 3);

                  for J in 1 .. Type_Table (T).Concrete_Descendants.Last_Index loop
                     Put (S, "--  " &
                          Image (Type_Table (T).Concrete_Descendants (J)) & LF);
                  end loop;

                  Decrease_Indent (S, 3);
               end if;
            end if;
         end Put_Kind_Subtype;

         procedure Put_Id_Subtype (T : Node_Or_Entity_Type) is
         begin
            if Type_Table (T).Parent /= No_Type then
               Put (S, "subtype " & Id_Image (T) & " is" & LF);
               Increase_Indent (S, 2);
               Put (S, Id_Image (Type_Table (T).Parent));

               if Enable_Assertions then
                  Put (S, " with Predicate =>" & LF);
                  Increase_Indent (S, 2);
                  Put (S, "K (" & Id_Image (T) & ") in " & Image (T));
                  Decrease_Indent (S, 2);
               end if;

               Put (S, ";" & LF);
               Decrease_Indent (S, 2);
            end if;
         end Put_Id_Subtype;

         procedure Put_Opt_Subtype (T : Node_Or_Entity_Type) is
         begin
            if Type_Table (T).Parent /= No_Type then
               Put (S, "subtype Opt_" & Id_Image (T) & " is" & LF);
               Increase_Indent (S, 2);
               Put (S, Id_Image (Root));

               --  Assert that the Opt_XXX subtype is empty or in the XXX
               --  subtype.

               if Enable_Assertions then
                  Put (S, " with Predicate =>" & LF);
                  Increase_Indent (S, 2);
                  Put (S, "Opt_" & Id_Image (T) & " = Empty or else" & LF);
                  Put (S, "Opt_" & Id_Image (T) & " in " & Id_Image (T));
                  Decrease_Indent (S, 2);
               end if;

               Put (S, ";" & LF);
               Decrease_Indent (S, 2);
            end if;
         end Put_Opt_Subtype;

      begin -- Put_Type_And_Subtypes
         Put_Enum_Type;

         --  Put the getter for Nkind and Ekind here, earlier than the other
         --  getters, because it is needed in predicates of the following
         --  subtypes.

         case Root is
            when Node_Kind =>
               Put_Getter_Decl (S, Nkind);
               Put (S, "function K (N : Node_Id) return Node_Kind renames " & Image (Nkind) & ";" & LF);
               Put (S, "--  Shorthand for use in predicates and preconditions below" & LF);
               Put (S, "--  There is no procedure Set_Nkind." & LF);
               Put (S, "--  See Init_Nkind and Mutate_Nkind in Atree." & LF & LF);

            when Entity_Kind =>
               Put_Getter_Decl (S, Ekind);
               Put (S, "function K (N : Entity_Id) return Entity_Kind renames Ekind;" & LF);
               Put (S, "--  Shorthand for use in predicates and preconditions below" & LF);
               Put (S, "--  There is no procedure Set_Ekind here." & LF);
               Put (S, "--  See Mutate_Ekind in Atree." & LF & LF);

            when others => raise Program_Error;
         end case;

         Put (S, "--  Subtypes of " & Image (Root) & " for each abstract type:" & LF & LF);

         Put (S, "pragma Style_Checks (""M200"");" & LF);
         Iterate_Types (Root, Pre => Put_Kind_Subtype'Access);

         Put (S, LF & "--  Subtypes of " & Id_Image (Root) &
              " with specified " & Image (Root) & "." & LF);
         Put (S, "--  These may be used in place of " & Id_Image (Root) &
              " for better documentation," & LF);
         Put (S, "--  and if assertions are enabled, for run-time checking." & LF & LF);

         Iterate_Types (Root, Pre => Put_Id_Subtype'Access);

         Put (S, LF & "--  Union types (nonhierarchical subtypes of " &
              Id_Image (Root) & ")" & LF & LF);

         for T in First_Abstract (Root) .. Last_Abstract (Root) loop
            if Type_Table (T) /= null and then Type_Table (T).Is_Union then
               Put_Kind_Subtype (T);
               Put_Id_Subtype (T);
            end if;
         end loop;

         Put (S, LF & "--  Optional subtypes of " & Id_Image (Root) & "." &
              " These allow Empty." & LF & LF);

         Iterate_Types (Root, Pre => Put_Opt_Subtype'Access);

         Put (S, LF & "--  Optional union types:" & LF & LF);

         for T in First_Abstract (Root) .. Last_Abstract (Root) loop
            if Type_Table (T) /= null and then Type_Table (T).Is_Union then
               Put_Opt_Subtype (T);
            end if;
         end loop;

         Put (S, LF & "subtype Flag is Boolean;" & LF & LF);
      end Put_Type_And_Subtypes;

      -------------------------------------------
      -- Put_Casts --
      -------------------------------------------

      procedure Put_Casts
        (S : in out Sink; T : Type_Enum)
      is
         Pre : constant String :=
           "function Cast is new Ada.Unchecked_Conversion (";
         Lo_Type : constant String := "Field_Size_" & Image (Field_Size (T)) & "_Bit";
         Hi_Type : constant String := Get_Set_Id_Image (T);
      begin
         if T not in Uint_Subtype then
            if T not in Node_Kind_Type | Entity_Kind_Type then
               Put (S, Pre & Hi_Type & ", " & Lo_Type & ");" & LF);
            end if;

            Put (S, Pre & Lo_Type & ", " & Hi_Type & ");" & LF);
         end if;
      end Put_Casts;

      ----------------------
      -- Put_Precondition --
      ----------------------

      procedure Put_Precondition
        (S : in out Sink; F : Field_Enum)
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
                  Increase_Indent (S, 1);
                  Put (S, ", Pre =>" & LF);
                  Put (S, Is_Entity);
                  Decrease_Indent (S, 1);
               end if;

            else
               Put (S, ", Pre =>" & LF);
               Increase_Indent (S, 1);
               Put (S, "N in ");
               Put_Type_Ids_With_Bars (S, Field_Table (F).Have_This_Field);

               pragma Assert (Is_Entity = "");

               Decrease_Indent (S, 1);
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

      procedure Put_Get_Set_Incr
        (S : in out Sink; F : Field_Enum; Get_Or_Set : String)
        with Pre => Get_Or_Set in "Get" | "Set";
      --  If statistics are enabled, put the appropriate increment statement

      ----------------------
      -- Put_Get_Set_Incr --
      ----------------------

      procedure Put_Get_Set_Incr
        (S : in out Sink; F : Field_Enum; Get_Or_Set : String) is
      begin
         if Statistics_Enabled then
            Put (S, "Atree." & Get_Or_Set & "_Count (" & F_Image (F) &
                   ") := Atree." & Get_Or_Set & "_Count (" &
                   F_Image (F) & ") + 1;" & LF);
         end if;
      end Put_Get_Set_Incr;

      ------------------------
      -- Node_To_Fetch_From --
      ------------------------

      function Node_To_Fetch_From (F : Field_Enum) return String is
      begin
         return
           (case Field_Table (F).Type_Only is
              when No_Type_Only => "N",
              when Base_Type_Only => "Base_Type (N)",
              when Impl_Base_Type_Only => "Implementation_Base_Type (N)",
              when Root_Type_Only => "Root_Type (N)");
      end Node_To_Fetch_From;

      ---------------------
      -- Put_Getter_Spec --
      ---------------------

      procedure Put_Getter_Spec (S : in out Sink; F : Field_Enum) is
      begin
         Put (S, "function " & Image (F));
         Put (S, " (N : " & N_Type (F) & ") return " &
              Get_Set_Id_Image (Field_Table (F).Field_Type));
      end Put_Getter_Spec;

      ---------------------
      -- Put_Getter_Decl --
      ---------------------

      procedure Put_Getter_Decl (S : in out Sink; F : Field_Enum) is
      begin
         Put_Getter_Spec (S, F);
         Put (S, " with " & Inline);
         Increase_Indent (S, 2);
         Put_Precondition (S, F);
         Decrease_Indent (S, 2);
         Put (S, ";" & LF);
      end Put_Getter_Decl;

      ------------------------------
      -- Put_Getter_Setter_Locals --
      ------------------------------

      procedure Put_Getter_Setter_Locals
        (S : in out Sink; F : Field_Enum; Get : Boolean)
      is
         Rec : Field_Info renames Field_Table (F).all;

         F_Size : constant Bit_Offset := Field_Size (Rec.Field_Type);
         Off : constant Field_Offset := Rec.Offset;
         F_Per_Slot : constant Field_Offset :=
           SS / Field_Offset (Field_Size (Rec.Field_Type));
         Slot_Off : constant Field_Offset := Off / F_Per_Slot;
         In_NH : constant Boolean := Slot_Off < Num_Header_Slots;

         N : constant String :=
           (if Get then Node_To_Fetch_From (F) else "N");

      begin
         Put (S, " is" & LF);
         Increase_Indent (S, 3);
         Put (S, "--  " & Image (F_Per_Slot) & "  " & Image (F_Size) &
                "-bit fields per " & SSS & "-bit slot." & LF);
         Put (S, "--  Offset " & Image (Off) & " = " &
                Image (Slot_Off) & " slots + " & Image (Off mod F_Per_Slot) &
                " fields in slot." & LF & LF);

         Put (S, "Off : constant := " & Image (Off) & ";" & LF);
         Put (S, "F_Size : constant := " & Image (F_Size) & ";" & LF);

         if Field_Size (Rec.Field_Type) /= SS then
            Put (S, "Mask : constant := 2**F_Size - 1;" & LF);
         end if;

         Put (S, "F_Per_Slot : constant Field_Offset := Slot_Size / F_Size;" & LF);
         Put (S, "Slot_Off : constant Field_Offset := Off / F_Per_Slot;" & LF);

         if In_NH then
            Put (S, "S : Slot renames Node_Offsets.Table (" & N & ").Slots (Slot_Off);" & LF);
         else
            Put (S, "S : Slot renames Slots.Table (Node_Offsets.Table (" & N & ").Offset + Slot_Off);" & LF);
         end if;

         if Field_Size (Rec.Field_Type) /= SS then
            Put (S, "V : constant Natural := Natural ((Off mod F_Per_Slot) * F_Size);" & LF);
            Put (S, LF);
         end if;
      end Put_Getter_Setter_Locals;

      ---------------------
      -- Put_Getter_Body --
      ---------------------

      procedure Put_Getter_Body (S : in out Sink; F : Field_Enum) is
         Rec : Field_Info renames Field_Table (F).all;
         F_Size : constant Bit_Offset := Field_Size (Rec.Field_Type);
         T : constant String := Get_Set_Id_Image (Rec.Field_Type);
      begin
         --  Note that we store the result in a local constant below, so that
         --  the "Pre => ..." can refer to it. The constant is called Val so
         --  that it has the same name as the formal of the setter, so the
         --  "Pre => ..." can refer to it by the same name in both getter
         --  and setter.

         Put_Getter_Spec (S, F);
         Put_Getter_Setter_Locals (S, F, Get => True);

         Put (S, "Raw : constant Field_Size_" & Image (F_Size) & "_Bit :=" & LF);
         Increase_Indent (S, 2);
         Put (S, "Field_Size_" & Image (F_Size) & "_Bit (");

         if Field_Size (Rec.Field_Type) /= SS then
            Put (S, "Shift_Right (S, V) and Mask);" & LF);
         else
            Put (S, "S);" & LF);
         end if;

         Decrease_Indent (S, 2);

         Put (S, "Val : constant " & T & " :=");

         if Field_Has_Special_Default (Rec.Field_Type) then
            pragma Assert (Field_Size (Rec.Field_Type) = 32);
            Put (S, LF);
            Increase_Indent (S, 2);
            Put (S, "(if Raw = 0 then " & Special_Default (Rec.Field_Type) &
                   " else " & "Cast (Raw));");
            Decrease_Indent (S, 2);

         else
            Put (S, " Cast (Raw);");
         end if;

         Put (S, LF);

         Decrease_Indent (S, 3);
         Put (S, "begin" & LF);
         Increase_Indent (S, 3);

         Put (S, "--  pragma Debug (Validate_Node_And_Offset (NN, Slot_Off));" & LF);
         --  Comment out the validation, because it's too slow, and because the
         --  relevant routines in Atree are not visible.

         if Rec.Pre.all /= "" then
            Put (S, "pragma Assert (" & Rec.Pre.all & ");" & LF);
         end if;

         if Rec.Pre_Get.all /= "" then
            Put (S, "pragma Assert (" & Rec.Pre_Get.all & ");" & LF);
         end if;

         Put_Get_Set_Incr (S, F, "Get");
         Put (S, "return Val;" & LF);
         Decrease_Indent (S, 3);
         Put (S, "end " & Image (F) & ";" & LF & LF);
      end Put_Getter_Body;

      ---------------------
      -- Put_Setter_Spec --
      ---------------------

      procedure Put_Setter_Spec (S : in out Sink; F : Field_Enum) is
         Rec    : Field_Info renames Field_Table (F).all;
         Default : constant String :=
           (if Rec.Field_Type = Flag then " := True" else "");
      begin
         Put (S, "procedure Set_" & Image (F));
         Put (S, " (N : " & N_Type (F) & "; Val : " &
              Get_Set_Id_Image (Rec.Field_Type) & Default & ")");
      end Put_Setter_Spec;

      ---------------------
      -- Put_Setter_Decl --
      ---------------------

      procedure Put_Setter_Decl (S : in out Sink; F : Field_Enum) is
      begin
         Put_Setter_Spec (S, F);
         Put (S, " with " & Inline);
         Increase_Indent (S, 2);
         Put_Precondition (S, F);
         Decrease_Indent (S, 2);
         Put (S, ";" & LF);
      end Put_Setter_Decl;

      ---------------------
      -- Put_Setter_Body --
      ---------------------

      procedure Put_Setter_Body (S : in out Sink; F : Field_Enum) is
         Rec : Field_Info renames Field_Table (F).all;
         F_Size : constant Bit_Offset := Field_Size (Rec.Field_Type);

         --  If Type_Only was specified in the call to Create_Semantic_Field,
         --  then we assert that the node is a base type. We cannot assert that
         --  it is an implementation base type or a root type.

         Type_Only_Assertion : constant String :=
           (case Rec.Type_Only is
              when No_Type_Only => "",
              when Base_Type_Only | Impl_Base_Type_Only | Root_Type_Only =>
                "Is_Base_Type (N)");
      begin
         Put_Setter_Spec (S, F);
         Put_Getter_Setter_Locals (S, F, Get => False);

         Put (S, "Raw : constant Field_Size_" & Image (F_Size) & "_Bit := Cast (Val);" & LF);

         Decrease_Indent (S, 3);
         Put (S, "begin" & LF);
         Increase_Indent (S, 3);

         Put (S, "--  pragma Debug (Validate_Node_And_Offset_Write (N, Slot_Off));" & LF);
         --  Comment out the validation, because it's too slow, and because the
         --  relevant routines in Atree are not visible.

         if Rec.Pre.all /= "" then
            Put (S, "pragma Assert (" & Rec.Pre.all & ");" & LF);
         end if;

         if Rec.Pre_Set.all /= "" then
            Put (S, "pragma Assert (" & Rec.Pre_Set.all & ");" & LF);
         end if;

         if Type_Only_Assertion /= "" then
            Put (S, "pragma Assert (" & Type_Only_Assertion & ");" & LF);
         end if;

         if Setter_Needs_Parent (F) then
            declare
               Err : constant String :=
                 (if Rec.Field_Type = List_Id then "Error_List" else "Error");
            begin
               Put (S, "if Present (Val) and then Val /= " & Err & " then" & LF);
               Increase_Indent (S, 3);
               Put (S, "pragma Warnings (Off, ""actuals for this call may be in wrong order"");" & LF);
               Put (S, "Set_Parent (Val, N);" & LF);
               Put (S, "pragma Warnings (On, ""actuals for this call may be in wrong order"");" & LF);
               Decrease_Indent (S, 3);
               Put (S, "end if;" & LF & LF);
            end;
         end if;

         if Field_Size (Rec.Field_Type) /= SS then
            Put (S, "S := (S and not Shift_Left (Mask, V)) or Shift_Left (Slot (Raw), V);" & LF);

         else
            Put (S, "S := Slot (Raw);" & LF);
         end if;

         Put_Get_Set_Incr (S, F, "Set");

         Decrease_Indent (S, 3);
         Put (S, "end Set_" & Image (F) & ";" & LF & LF);
      end Put_Setter_Body;

      --------------------
      -- Put_Subp_Decls --
      --------------------

      procedure Put_Subp_Decls (S : in out Sink; Root : Root_Type) is
         --  Note that there are several fields that are defined for both nodes
         --  and entities, such as Nkind. These are allocated slots in both,
         --  but here we only put out getters and setters in Sinfo.Nodes, not
         --  Einfo.Entities.

      begin
         Put (S, "--  Getters and setters for fields" & LF);

         for F in First_Field (Root) .. Last_Field (Root) loop
            --  Nkind/Ekind getter is already done (see Put_Type_And_Subtypes),
            --  and there is no setter for these.

            if F = Nkind then
               Put (S, LF & "--  Nkind getter is above" & LF);

            elsif F = Ekind then
               Put (S, LF & "--  Ekind getter is above" & LF);

            else
               Put_Getter_Decl (S, F);
               Put_Setter_Decl (S, F);
            end if;

            Put (S, LF);
         end loop;
      end Put_Subp_Decls;

      ---------------------
      -- Put_Subp_Bodies --
      ---------------------

      procedure Put_Subp_Bodies (S : in out Sink; Root : Root_Type) is
      begin
         Put (S, LF & "--  Getters and setters for fields" & LF & LF);

         for F in First_Field (Root) .. Last_Field (Root) loop
            Put_Getter_Body (S, F);

            if F not in Nkind | Ekind then
               Put_Setter_Body (S, F);
            end if;
         end loop;
      end Put_Subp_Bodies;

      --------------------------
      -- Put_Traversed_Fields --
      --------------------------

      procedure Put_Traversed_Fields (S : in out Sink) is

         function Is_Traversed_Field
           (T : Concrete_Node; F : Field_Enum) return Boolean;
         --  True if F is a field that should be traversed by Traverse_Func. In
         --  particular, True if F is a syntactic field of T, and is of a
         --  Node_Id or List_Id type.

         function Init_Max_Traversed_Fields return Field_Offset;
         --  Compute the maximum number of syntactic fields that are of type
         --  Node_Id or List_Id over all node types.

         procedure Put_Aggregate (T : Node_Or_Entity_Type);
         --  Print out the subaggregate for one type

         function Is_Traversed_Field
           (T : Concrete_Node; F : Field_Enum) return Boolean is
         begin
            return Syntactic (T) (F)
              and then Field_Table (F).Field_Type in Traversed_Field_Type;
         end Is_Traversed_Field;

         First_Time : Boolean := True;

         procedure Put_Aggregate (T : Node_Or_Entity_Type) is
            Left_Opnd_Skipped : Boolean := False;
         begin
            if T in Concrete_Node then
               if First_Time then
                  First_Time := False;
               else
                  Put (S, "," & LF);
               end if;

               Put (S, Image (T) & " => (");
               Increase_Indent (S, 2);

               for FI in 1 .. Last_Index (Type_Table (T).Fields) loop
                  declare
                     F : constant Field_Enum := Type_Table (T).Fields (FI);

                  begin
                     if Is_Traversed_Field (T, F) then
                        if F = Left_Opnd then
                           Left_Opnd_Skipped := True; -- see comment below

                        else
                           Put (S, Image (Field_Table (F).Offset) & ", ");
                        end if;
                     end if;
                  end;
               end loop;

               --  We always put the Left_Opnd field of N_Op_Concat last. See
               --  comments in Atree.Traverse_Func for the reason. We might as
               --  well do that for all Left_Opnd fields; the old version did
               --  that.

               if Left_Opnd_Skipped then
                  Put (S, Image (Field_Table (Left_Opnd).Offset) & ", ");
               end if;

               Put (S, "others => No_Field_Offset");

               Decrease_Indent (S, 2);
               Put (S, ")");
            end if;
         end Put_Aggregate;

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
         Put (S, "--  Table of fields that should be traversed by Traverse subprograms." & LF);
         Put (S, "--  Each entry is an array of offsets in slots of fields to be" & LF);
         Put (S, "--  traversed, terminated by a sentinel equal to No_Field_Offset." & LF & LF);

         Put (S, "subtype Traversed_Offset_Array is Offset_Array (0 .. " &
              Image (Max_Traversed_Fields - 1) & " + 1);" & LF);
         Put (S, "Traversed_Fields : constant array (Node_Kind) of Traversed_Offset_Array :=" & LF);
         --  One extra for the sentinel

         Increase_Indent (S, 2);
         Put (S, "(");
         Increase_Indent (S, 1);
         Iterate_Types (Node_Kind, Pre => Put_Aggregate'Access);
         Decrease_Indent (S, 1);
         Put (S, ");" & LF & LF);
         Decrease_Indent (S, 2);
      end Put_Traversed_Fields;

      ----------------
      -- Put_Tables --
      ----------------

      procedure Put_Tables (S : in out Sink; Root : Root_Type) is

         First_Time : Boolean := True;

         procedure Put_Size (T : Node_Or_Entity_Type);
         procedure Put_Size (T : Node_Or_Entity_Type) is
         begin
            if T in Concrete_Type then
               if First_Time then
                  First_Time := False;
               else
                  Put (S, "," & LF);
               end if;

               Put (S, Image (T) & " => " & Image (Type_Size_In_Slots (T)));
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
                     Put (S, "," & LF);
                  end if;

                  Put (S, F_Image (F));
               end if;
            end loop;
         end Put_Field_Array;

         Field_Enum_Type_Name : constant String :=
           (case Root is
              when Node_Kind => "Node_Field",
              when others => "Entity_Field");  -- Entity_Kind

      begin
         Put (S, "--  Table of sizes in " & SSS & "-bit slots for given " &
              Image (Root) & ", for use by Atree:" & LF);

         case Root is
            when Node_Kind =>
               Put (S, LF & "Min_Node_Size : constant Field_Offset := " &
                    Image (Min_Node_Size) & ";" & LF);
               Put (S, "Max_Node_Size : constant Field_Offset := " &
                    Image (Max_Node_Size) & ";" & LF & LF);

            when Entity_Kind =>
               Put (S, LF & "Min_Entity_Size : constant Field_Offset := " &
                    Image (Min_Entity_Size) & ";" & LF);
               Put (S, "Max_Entity_Size : constant Field_Offset := " &
                    Image (Max_Entity_Size) & ";" & LF & LF);
            when others => raise Program_Error;
         end case;

         Put (S, "Size : constant array (" & Image (Root) &
              ") of Field_Offset :=" & LF);
         Increase_Indent (S, 2);
         Put (S, "(");
         Increase_Indent (S, 1);

         Iterate_Types (Root, Pre => Put_Size'Access);

         Decrease_Indent (S, 1);
         Put (S, "); -- Size" & LF);
         Decrease_Indent (S, 2);

         if Root = Node_Kind then
            declare
               type Node_Dummy is array (Node_Field) of Boolean;
               type Entity_Dummy is array (Entity_Field) of Boolean;
               Num_Fields : constant Root_Int :=
                 Node_Dummy'Length + Entity_Dummy'Length;
               First_Time : Boolean := True;
            begin
               Put (S, LF & "--  Enumeration of all " & Image (Num_Fields)
                    & " fields:" & LF & LF);

               Put (S, "type Node_Or_Entity_Field is" & LF);
               Increase_Indent (S, 2);
               Put (S, "(");
               Increase_Indent (S, 1);

               for F in Node_Field loop
                  if First_Time then
                     First_Time := False;
                  else
                     Put (S, "," & LF);
                  end if;

                  Put (S, F_Image (F));
               end loop;

               for F in Entity_Field loop
                  Put (S, "," & LF);
                  Put (S, F_Image (F));
               end loop;

               Decrease_Indent (S, 1);
               Put (S, "); -- Node_Or_Entity_Field" & LF);
               Decrease_Indent (S, 2);
            end;
         end if;

         Put (S, LF & "subtype " & Field_Enum_Type_Name & " is" & LF);
         Increase_Indent (S, 2);
         Put (S, "Node_Or_Entity_Field range " & F_Image (First_Field (Root)) &
                " .. " & F_Image (Last_Field (Root)) & ";" & LF);
         Decrease_Indent (S, 2);

         Put (S, LF & "type " & Field_Enum_Type_Name & "_Index is new Pos;" & LF);
         Put (S, "type " & Field_Enum_Type_Name & "_Array is array (" &
              Field_Enum_Type_Name & "_Index range <>) of " &
              Field_Enum_Type_Name & ";" & LF);
         Put (S, "type " & Field_Enum_Type_Name &
              "_Array_Ref is access constant " & Field_Enum_Type_Name &
              "_Array;" & LF);
         Put (S, "subtype A is " & Field_Enum_Type_Name & "_Array;" & LF);
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
                     Put (S, "," & LF);
                  end if;

                  Put (S, Image (T) & " =>" & LF);
                  Increase_Indent (S, 2);
                  Put (S, "new A'(");
                  Increase_Indent (S, 6);
                  Increase_Indent (S, 1);

                  Put_Field_Array (T);

                  Decrease_Indent (S, 1);
                  Put (S, ")");
                  Decrease_Indent (S, 6);
                  Decrease_Indent (S, 2);
               end if;
            end Do_One_Type;
         begin
            Put (S, LF & "--  Table mapping " & Image (Root) &
                 "s to the sequence of fields that exist in that " &
                 Image (Root) & ":" & LF & LF);

            Put (S, Field_Enum_Type_Name & "_Table : constant array (" &
                 Image (Root) & ") of " & Field_Enum_Type_Name &
                 "_Array_Ref :=" & LF);

            Increase_Indent (S, 2);
            Put (S, "(");
            Increase_Indent (S, 1);

            Iterate_Types (Root, Pre => Do_One_Type'Access);

            Decrease_Indent (S, 1);
            Put (S, "); -- " & Field_Enum_Type_Name & "_Table" & LF);
            Decrease_Indent (S, 2);
         end;

         if Root = Node_Kind then
            declare
               First_Time : Boolean := True;
               FS, FB, LB : Bit_Offset;
               --  Field size in bits, first bit, and last bit for the previous
               --  time around the loop. Used to print a comment after ",".

               procedure One_Comp (F : Field_Enum);

               --------------
               -- One_Comp --
               --------------

               procedure One_Comp (F : Field_Enum) is
                  pragma Annotate (Codepeer, Modified, Field_Table);
                  Offset : constant Field_Offset := Field_Table (F).Offset;
               begin
                  if First_Time then
                     First_Time := False;
                  else
                     Put (S, ",");

                     --  Print comment showing field's bits, except for 1-bit
                     --  fields.

                     if FS /= 1 then
                        Put (S, " -- *" & Image (FS) & " = bits " &
                               Image (FB) & ".." & Image (LB));
                     end if;

                     Put (S, LF);
                  end if;

                  Put (S, F_Image (F) & " => (" &
                       Image (Field_Table (F).Field_Type) & "_Field, " &
                       Image (Offset) & ", " &
                       Image (Field_Table (F).Type_Only) & ")");

                  FS := Field_Size (F);
                  FB := First_Bit (F, Offset);
                  LB := Last_Bit (F, Offset);
               end One_Comp;

            begin
               Put (S, LF & "--  Table mapping fields to kind and offset:" & LF & LF);

               Put (S, "Field_Descriptors : constant array (" &
                    "Node_Or_Entity_Field) of Field_Descriptor :=" & LF);

               Increase_Indent (S, 2);
               Put (S, "(");
               Increase_Indent (S, 1);

               for F in Node_Field loop
                  One_Comp (F);
               end loop;

               for F in Entity_Field loop
                  One_Comp (F);
               end loop;

               Decrease_Indent (S, 1);
               Put (S, "); -- Field_Descriptors" & LF);
               Decrease_Indent (S, 2);
            end;
         end if;

      end Put_Tables;

      ----------------
      -- Put_Seinfo --
      ----------------

      procedure Put_Seinfo is
         S : Sink;
      begin
         Create_File (S, "seinfo.ads");
         Put (S, "with Types; use Types;" & LF);
         Put (S, LF & "package Seinfo is" & LF & LF);
         Increase_Indent (S, 3);

         Put (S, "--  This package is automatically generated." & LF & LF);

         Put (S, "--  Common declarations visible in both Sinfo.Nodes and Einfo.Entities." & LF);

         Put (S, LF & "type Field_Kind is" & LF);
         Increase_Indent (S, 2);
         Put (S, "(");
         Increase_Indent (S, 1);

         declare
            First_Time : Boolean := True;
         begin
            for T in Special_Type loop
               if First_Time then
                  First_Time := False;
               else
                  Put (S, "," & LF);
               end if;

               Put (S, Image (T) & "_Field");
            end loop;
         end;

         Decrease_Indent (S, 1);
         Decrease_Indent (S, 2);
         Put (S, ");" & LF);

         Put (S, LF & "Field_Size : constant array (Field_Kind) of Field_Size_In_Bits :=" & LF);
         Increase_Indent (S, 2);
         Put (S, "(");
         Increase_Indent (S, 1);

         declare
            First_Time : Boolean := True;
         begin
            for T in Special_Type loop
               if First_Time then
                  First_Time := False;
               else
                  Put (S, "," & LF);
               end if;

               Put (S, Image (T) & "_Field => " & Image (Field_Size (T)));
            end loop;
         end;

         Decrease_Indent (S, 1);
         Decrease_Indent (S, 2);
         Put (S, ");" & LF & LF);

         Put (S, "type Type_Only_Enum is" & LF);
         Increase_Indent (S, 2);
         Put (S, "(");

         declare
            First_Time : Boolean := True;
         begin
            for TO in Type_Only_Enum loop
               if First_Time then
                  First_Time := False;
               else
                  Put (S, ", ");
               end if;

               Put (S, Image (TO));
            end loop;
         end;

         Decrease_Indent (S, 2);
         Put (S, ");" & LF & LF);

         Put (S, "type Field_Descriptor is record" & LF);
         Increase_Indent (S, 3);
         Put (S, "Kind : Field_Kind;" & LF);
         Put (S, "Offset : Field_Offset;" & LF);
         Put (S, "Type_Only : Type_Only_Enum;" & LF);
         Decrease_Indent (S, 3);
         Put (S, "end record;" & LF & LF);

         --  Print out the node header types. Note that the Offset field is of
         --  the base type, because we are using zero-origin addressing in
         --  Atree.

         Put (S, "N_Head : constant Field_Offset := " & N_Head & ";" & LF & LF);

         Put (S, "Atree_Statistics_Enabled : constant Boolean := " &
                Capitalize (Boolean'Image (Statistics_Enabled)) & ";" & LF);

         Decrease_Indent (S, 3);
         Put (S, LF & "end Seinfo;" & LF);
      end Put_Seinfo;

      ---------------
      -- Put_Nodes --
      ---------------

      procedure Put_Nodes is
         S : Sink;
         B : Sink;

      begin
         Create_File (S, "sinfo-nodes.ads");
         Create_File (B, "sinfo-nodes.adb");
         Put (S, "with Seinfo; use Seinfo;" & LF);
         Put (S, "pragma Warnings (Off);" & LF);
         --  With's included in case they are needed; so we don't have to keep
         --  switching back and forth.
         Put (S, "with Output; use Output;" & LF);
         Put (S, "pragma Warnings (On);" & LF);

         Put (S, LF & "package Sinfo.Nodes is" & LF & LF);
         Increase_Indent (S, 3);

         Put (S, "--  This package is automatically generated." & LF & LF);

         Put_Type_Hierarchy (S, Node_Kind);

         Put_Type_And_Subtypes (S, Node_Kind);

         Put (S, "pragma Assert (Node_Kind'Pos (N_Unused_At_Start) = 0);" & LF & LF);
         Put (S, "pragma Assert (Node_Kind'Last = N_Unused_At_End);" & LF & LF);

         Put_Subp_Decls (S, Node_Kind);

         Put_Traversed_Fields (S);

         Put_Tables (S, Node_Kind);

         Decrease_Indent (S, 3);
         Put (S, LF & "end Sinfo.Nodes;" & LF);

         Put (B, "with Ada.Unchecked_Conversion;" & LF);
         Put (B, "with Atree; use Atree; use Atree.Atree_Private_Part;" & LF);
         Put (B, "with Nlists; use Nlists;" & LF);
         Put (B, "pragma Warnings (Off);" & LF);
         Put (B, "with Einfo.Utils; use Einfo.Utils;" & LF);
         Put (B, "with Sinfo.Utils; use Sinfo.Utils;" & LF);
         Put (B, "pragma Warnings (On);" & LF);

         Put (B, LF & "package body Sinfo.Nodes is" & LF & LF);
         Increase_Indent (B, 3);

         Put (B, "--  This package is automatically generated." & LF & LF);

         Put (B, "pragma Style_Checks (""M200"");" & LF);

         for T in Special_Type loop
            if Node_Field_Types_Used (T) then
               Put_Casts (B, T);
            end if;
         end loop;

         Put_Subp_Bodies (B, Node_Kind);

         Decrease_Indent (B, 3);
         Put (B, "end Sinfo.Nodes;" & LF);

      end Put_Nodes;

      ------------------
      -- Put_Entities --
      ------------------

      procedure Put_Entities is
         S : Sink;
         B : Sink;
      begin
         Create_File (S, "einfo-entities.ads");
         Create_File (B, "einfo-entities.adb");
         Put (S, "with Sinfo.Nodes; use Sinfo.Nodes;" & LF);

         Put (S, LF & "package Einfo.Entities is" & LF & LF);
         Increase_Indent (S, 3);

         Put (S, "--  This package is automatically generated." & LF & LF);

         Put_Type_Hierarchy (S, Entity_Kind);

         Put_Type_And_Subtypes (S, Entity_Kind);

         Put_Subp_Decls (S, Entity_Kind);

         Put_Tables (S, Entity_Kind);

         Decrease_Indent (S, 3);
         Put (S, LF & "end Einfo.Entities;" & LF);

         Put (B, "with Ada.Unchecked_Conversion;" & LF);
         Put (B, "with Atree; use Atree; use Atree.Atree_Private_Part;" & LF);
         Put (B, "with Einfo.Utils; use Einfo.Utils;" & LF);
         --  This forms a cycle between packages (via bodies, which is OK)

         Put (B, LF & "package body Einfo.Entities is" & LF & LF);
         Increase_Indent (B, 3);

         Put (B, "--  This package is automatically generated." & LF & LF);

         Put (B, "pragma Style_Checks (""M200"");" & LF);

         for T in Special_Type loop
            if Entity_Field_Types_Used (T) then
               Put_Casts (B, T);
            end if;
         end loop;

         Put_Subp_Bodies (B, Entity_Kind);

         Decrease_Indent (B, 3);
         Put (B, "end Einfo.Entities;" & LF);

      end Put_Entities;

      -------------------
      -- Put_Make_Spec --
      -------------------

      procedure Put_Make_Spec
        (S : in out Sink; Root : Root_Type; T : Concrete_Type)
      is
      begin
         Put (S, "function Make_" & Image_Sans_N (T) & "" & LF);
         Increase_Indent (S, 2);
         Put (S, "(Sloc : Source_Ptr");
         Increase_Indent (S, 1);

         for F of Type_Table (T).Fields loop
            pragma Assert (Fields_Per_Node (T) (F));

            if Syntactic (T) (F) then
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

               begin
                  Put (S, ";" & LF);
                  Put (S, Image (F));
                  Put (S, " : " & Typ & Default);
               end;
            end if;
         end loop;

         Put (S, ")" & LF);
         Put (S, "return " & Node_Or_Entity (Root) & "_Id");
         Decrease_Indent (S, 2);
         Decrease_Indent (S, 1);
      end Put_Make_Spec;

      --------------------
      -- Put_Make_Decls --
      --------------------

      procedure Put_Make_Decls (S : in out Sink; Root : Root_Type) is
      begin
         for T in First_Concrete (Root) .. Last_Concrete (Root) loop
            if T not in N_Unused_At_Start | N_Unused_At_End then
               Put_Make_Spec (S, Root, T);
               Put (S, ";" & LF);
               Put (S, "pragma " & Inline & " (Make_" &
                    Image_Sans_N (T) & ");" & LF & LF);
            end if;
         end loop;
      end Put_Make_Decls;

      ---------------------
      -- Put_Make_Bodies --
      ---------------------

      procedure Put_Make_Bodies (S : in out Sink; Root : Root_Type) is
      begin
         for T in First_Concrete (Root) .. Last_Concrete (Root) loop
            if T not in N_Unused_At_Start | N_Unused_At_End then
               Put_Make_Spec (S, Root, T);
               Put (S, LF & "is" & LF);

               Increase_Indent (S, 3);
               Put (S, "N : constant Node_Id :=" & LF);

               if T in Entity_Node then
                  Put (S, "      New_Entity (" & Image (T) & ", Sloc);" & LF);

               else
                  Put (S, "      New_Node (" & Image (T) & ", Sloc);" & LF);
               end if;

               Decrease_Indent (S, 3);

               Put (S, "begin" & LF);

               Increase_Indent (S, 3);
               for F of Type_Table (T).Fields loop
                  pragma Assert (Fields_Per_Node (T) (F));

                  if Syntactic (T) (F) then
                     declare
                        NWidth : constant := 28;
                        --  This constant comes from the old Xnmake, which wraps
                        --  the Set_... call if the field name is that long or
                        --  longer.

                        F_Name : constant String := Image (F);

                     begin
                        if F_Name'Length < NWidth then
                           Put (S, "Set_" & F_Name & " (N, " & F_Name & ");" & LF);

                        --  Wrap the line

                        else
                           Put (S, "Set_" & F_Name & "" & LF);
                           Increase_Indent (S, 2);
                           Put (S, "(N, " & F_Name & ");" & LF);
                           Decrease_Indent (S, 2);
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
                     Put (S, "Set_Chars (N, Name_" & Op_Name & ");" & LF);
                     Put (S, "Set_Entity (N, Standard_" & Op & ");" & LF);
                  end;
               end if;

               if Type_Table (T).Nmake_Assert.all /= "" then
                  Put (S, "pragma Assert (" &
                           Type_Table (T).Nmake_Assert.all & ");" & LF);
               end if;

               Put (S, "return N;" & LF);
               Decrease_Indent (S, 3);

               Put (S, "end Make_" & Image_Sans_N (T) & ";" & LF & LF);
            end if;
         end loop;
      end Put_Make_Bodies;

      ---------------
      -- Put_Nmake --
      ---------------

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
         S : Sink;
         B : Sink;

      begin
         Create_File (S, "nmake.ads");
         Create_File (B, "nmake.adb");
         Put (S, "with Namet;  use Namet;" & LF);
         Put (S, "with Nlists; use Nlists;" & LF);
         Put (S, "with Types;  use Types;" & LF);
         Put (S, "with Uintp;  use Uintp;" & LF);
         Put (S, "with Urealp; use Urealp;" & LF);

         Put (S, LF & "package Nmake is" & LF & LF);
         Increase_Indent (S, 3);

         Put (S, "--  This package is automatically generated." & LF & LF);
         Put (S, "--  See Put_Nmake in gen_il-gen.adb for documentation." & LF & LF);

         Put_Make_Decls (S, Node_Kind);

         Decrease_Indent (S, 3);
         Put (S, "end Nmake;" & LF);

         Put (B, "with Atree;  use Atree;" & LF);
         Put (B, "with Sinfo.Nodes; use Sinfo.Nodes;" & LF);
         Put (B, "with Sinfo.Utils; use Sinfo.Utils;" & LF);
         Put (B, "with Snames; use Snames;" & LF);
         Put (B, "with Stand;  use Stand;" & LF);

         Put (B, LF & "package body Nmake is" & LF & LF);
         Increase_Indent (B, 3);

         Put (B, "--  This package is automatically generated." & LF & LF);
         Put (B, "pragma Style_Checks (""M200"");" & LF);

         Put_Make_Bodies (B, Node_Kind);

         Decrease_Indent (B, 3);
         Put (B, "end Nmake;" & LF);
      end Put_Nmake;

      -----------------------
      -- Put_Seinfo_Tables --
      -----------------------

      procedure Put_Seinfo_Tables is
         S : Sink;
         B : Sink;

         Type_Layout : Concrete_Type_Layout_Array;

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
                  --  the bits up to the next slot boundary.

                  Result := First_Bit;

                  while (Result + 1) mod SS /= 0
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
            W : constant Bit_Offset := First_Bit / SS;
            B : constant Bit_Offset := First_Bit mod SS;
            pragma Assert (W * SS + B = First_Bit);
         begin
            return
              Image (W) & "*" & SSS & (if B = 0 then "" else " + " & Image (B));
         end First_Bit_Image;

         function Last_Bit_Image (Last_Bit : Bit_Offset) return String is
            W : constant Bit_Offset := (Last_Bit + 1) / SS;
         begin
            if W * SS - 1 = Last_Bit then
               return Image (W) & "*" & SSS & " - 1";
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
                     Put (B, "," & LF);
                  end if;

                  Put (B, Image (F));
               end if;
            end loop;
         end Put_Field_List;

      begin -- Put_Seinfo_Tables
         Create_File (S, "seinfo_tables.ads");
         Create_File (B, "seinfo_tables.adb");

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

         Put (S, LF & "package Seinfo_Tables is" & LF & LF);
         Increase_Indent (S, 3);

         Put (S, "--  This package is automatically generated." & LF & LF);

         Put (S, "--  This package is not used by the compiler." & LF);
         Put (S, "--  The body contains tables that are intended to be used by humans to" & LF);
         Put (S, "--  help understand the layout of various data structures." & LF);
         Put (S, "--  Search for ""--"" to find major sections of code." & LF & LF);

         Put (S, "pragma Elaborate_Body;" & LF);

         Decrease_Indent (S, 3);
         Put (S, LF & "end Seinfo_Tables;" & LF);

         Put (B, "with Gen_IL.Types;  use Gen_IL.Types;" & LF);
         Put (B, "with Gen_IL.Fields; use Gen_IL.Fields;" & LF);
         Put (B, "with Gen_IL.Internals;  use Gen_IL.Internals;" & LF);

         Put (B, LF & "package body Seinfo_Tables is" & LF & LF);
         Increase_Indent (B, 3);

         Put (B, "--  This package is automatically generated." & LF & LF);

         Put (B, "Num_Wasted_Bits : Bit_Offset'Base := " & Image (Num_Wasted_Bits) &
              " with Unreferenced;" & LF);

         Put (B, LF & "Wasted_Bits : constant Opt_Field_Enum := No_Field;" & LF);

         Put (B, LF & "--  Table showing the layout of each Node_Or_Entity_Type. For each" & LF);
         Put (B, "--  concrete type, we show the bits used by each field. Each field" & LF);
         Put (B, "--  uses the same bit range in all types. This table is not used by" & LF);
         Put (B, "--  the compiler; it is for information only." & LF & LF);

         Put (B, "--  Wasted_Bits are unused bits between fields, and padding at the end" & LF);
         Put (B, "--  to round up to a multiple of the slot size." & LF);

         Put (B, LF & "--  Type_Layout is " & Image (Type_Layout_Size / 8) & " bytes." & LF);

         Put (B, LF & "pragma Style_Checks (Off);" & LF);
         Put (B, "Type_Layout : constant Concrete_Type_Layout_Array := " & LF);
         Increase_Indent (B, 2);
         Put (B, "--  Concrete node types:" & LF);
         Put (B, "(");
         Increase_Indent (B, 1);

         declare
            First_Time : Boolean := True;

         begin
            for T in Concrete_Type loop
               if First_Time then
                  First_Time := False;
               else
                  Put (B, "," & LF & LF);
               end if;

               if T = Concrete_Entity'First then
                  Put (B, "--  Concrete entity types:" & LF & LF);
               end if;

               Put (B, Image (T) & " => new Field_Array'" & LF);

               Increase_Indent (B, 2);
               Put (B, "(");
               Increase_Indent (B, 1);

               declare
                  First_Time : Boolean := True;
                  First_Bit : Bit_Offset := 0;
                  F : Opt_Field_Enum;

                  function Node_Field_Of_Entity return String is
                     (if T in Entity_Type and then F in Node_Field then
                       " -- N" else "");
                  --  A comment to put out for fields of entities that are
                  --  shared with nodes, such as Chars.

               begin
                  while First_Bit < Type_Bit_Size_Aligned (T) loop
                     if First_Time then
                        First_Time := False;
                     else
                        Put (B, "," & Node_Field_Of_Entity & LF);
                     end if;

                     F := Type_Layout (T) (First_Bit);

                     declare
                        Last_Bit : constant Bit_Offset :=
                          Get_Last_Bit (T, F, First_Bit);
                     begin
                        pragma Assert
                          (Type_Layout (T) (First_Bit .. Last_Bit) =
                                           (First_Bit .. Last_Bit => F));

                        if Last_Bit = First_Bit then
                           Put (B, First_Bit_Image (First_Bit) & " => " &
                                Image_Or_Waste (F));
                        else
                           pragma Assert
                             (if F /= No_Field then
                               First_Bit mod Field_Size (F) = 0);
                           Put (B, First_Bit_Image (First_Bit) & " .. " &
                                Last_Bit_Image (Last_Bit) & " => " &
                                Image_Or_Waste (F));
                        end if;

                        First_Bit := Last_Bit + 1;
                     end;
                  end loop;
               end;

               Decrease_Indent (B, 1);
               Put (B, ")");
               Decrease_Indent (B, 2);
            end loop;
         end;

         Decrease_Indent (B, 1);
         Put (B, ") -- Type_Layout" & LF);
         Increase_Indent (B, 6);
         Put (B, "with Export, Convention => Ada;" & LF);
         Decrease_Indent (B, 6);
         Decrease_Indent (B, 2);

         Put (B, LF & "--  Table mapping bit offsets to the set of fields at that offset" & LF & LF);
         Put (B, "Bit_Used : constant Offset_To_Fields_Mapping :=" & LF);

         Increase_Indent (B, 2);
         Put (B, "(");
         Increase_Indent (B, 1);

         declare
            First_Time : Boolean := True;
         begin
            for Bit in 0 .. Bit_Offset'Max
              (Max_Node_Bit_Size, Max_Entity_Bit_Size)
            loop
               if First_Time then
                  First_Time := False;
               else
                  Put (B, "," & LF & LF);
               end if;

               Put (B, First_Bit_Image (Bit) & " => new Field_Array'" & LF);

               --  Use [...] notation here, to get around annoying Ada
               --  limitations on empty and singleton aggregates. This code is
               --  not used in the compiler, so there are no bootstrap issues.

               Increase_Indent (B, 2);
               Put (B, "[");
               Increase_Indent (B, 1);

               Put_Field_List (Bit);

               Decrease_Indent (B, 1);
               Put (B, "]");
               Decrease_Indent (B, 2);
            end loop;
         end;

         Decrease_Indent (B, 1);
         Put (B, "); -- Bit_Used" & LF);
         Decrease_Indent (B, 2);

         Decrease_Indent (B, 3);
         Put (B, LF & "end Seinfo_Tables;" & LF);

      end Put_Seinfo_Tables;

      -----------------------------
      -- Put_C_Type_And_Subtypes --
      -----------------------------

      procedure Put_C_Type_And_Subtypes
        (S : in out Sink; Root : Root_Type) is

         Cur_Pos : Root_Nat := 0;
         --  Current Node_Kind'Pos or Entity_Kind'Pos to be printed

         procedure Put_Enum_Lit (T : Node_Or_Entity_Type);
         --  Print out the enumerator corresponding to the Ada enumeration literal
         --  for T in Node_Kind and Entity_Kind (i.e. concrete types).
         --  This looks like "Some_Kind = <pos>", where Some_Kind
         --  is the Node_Kind or Entity_Kind enumeration literal, and
         --  <pos> is Node_Kind'Pos or Entity_Kind'Pos of that literal.

         procedure Put_Kind_Subtype (T : Node_Or_Entity_Type);
         --  Print out the SUBTYPE macro call corresponding to an abstract
         --  type.

         procedure Put_Enum_Lit (T : Node_Or_Entity_Type) is
         begin
            if T in Concrete_Type then
               Put (S, "  " & Image (T) & " = " & Image (Cur_Pos) & "," & LF);
               Cur_Pos := Cur_Pos + 1;
            end if;
         end Put_Enum_Lit;

         procedure Put_Kind_Subtype (T : Node_Or_Entity_Type) is
         begin
            if T in Abstract_Type and then Type_Table (T).Parent /= No_Type then
               Put (S, "SUBTYPE (" & Image (T) & ", " &
                    Image (Type_Table (T).Parent) & "," & LF);
               Increase_Indent (S, 3);
               Put (S, Image (Type_Table (T).First) & "," & LF);
               Put (S, Image (Type_Table (T).Last) & ")" & LF);
               Decrease_Indent (S, 3);
            end if;
         end Put_Kind_Subtype;

      begin
         Put_Union_Membership (S, Root, Only_Prototypes => True);

         Put (S, "enum " & Node_Or_Entity (Root) & "_Kind : unsigned int {" & LF);
         Iterate_Types (Root, Pre => Put_Enum_Lit'Access);
         Put (S, "};" & LF);

         Put (S, "#define Number_" & Node_Or_Entity (Root) & "_Kinds " &
              Image (Cur_Pos) & "" & LF & LF);

         Iterate_Types (Root, Pre => Put_Kind_Subtype'Access);

         Put_Union_Membership (S, Root, Only_Prototypes => False);
      end Put_C_Type_And_Subtypes;

      ------------------
      -- Put_C_Getter --
      ------------------

      procedure Put_C_Getter
        (S : in out Sink; F : Field_Enum)
      is
         Rec : Field_Info renames Field_Table (F).all;

         Off : constant Field_Offset := Rec.Offset;
         F_Size : constant Bit_Offset := Field_Size (Rec.Field_Type);
         F_Per_Slot : constant Field_Offset :=
           SS / Field_Offset (Field_Size (Rec.Field_Type));
         Slot_Off : constant Field_Offset := Off / F_Per_Slot;
         In_NH : constant Boolean := Slot_Off < Num_Header_Slots;

         N : constant String := Node_To_Fetch_From (F);
      begin
         Put (S, "INLINE " & Get_Set_Id_Image (Rec.Field_Type) &
              " " & Image (F) & " (Node_Id N)" & LF);

         Put (S, "{" & LF);
         Increase_Indent (S, 3);
         Put (S, "const Field_Offset Off = " & Image (Rec.Offset) & ";" & LF);
         Put (S, "const Field_Offset F_Size = " & Image (F_Size) & ";" & LF);

         if Field_Size (Rec.Field_Type) /= SS then
            Put (S, "const any_slot Mask = (1 << F_Size) - 1;" & LF);
         end if;

         Put (S, "const Field_Offset F_Per_Slot = Slot_Size / F_Size;" & LF);
         Put (S, "const Field_Offset Slot_Off = Off / F_Per_Slot;" & LF);
         Put (S, LF);
         if In_NH then
            Put (S, "any_slot slot = Node_Offsets_Ptr[" & N & "].Slots[Slot_Off];" & LF);
         else
            Put (S, "any_slot slot = *(Slots_Ptr + Node_Offsets_Ptr[" & N &
                   "].Offset + Slot_Off);" & LF);
         end if;

         if Field_Size (Rec.Field_Type) /= SS then
            Put (S, "unsigned int Raw = (slot >> (Off % F_Per_Slot) * F_Size) & Mask;" & LF);
         else
            Put (S, "unsigned int Raw = slot;" & LF);
         end if;

         Put (S, Get_Set_Id_Image (Rec.Field_Type) & " val = (" &
                Get_Set_Id_Image (Rec.Field_Type) & ") ");

         if Field_Has_Special_Default (Rec.Field_Type) then
            Increase_Indent (S, 2);
            Put (S, "(Raw? Raw : " & Special_Default (Rec.Field_Type) & ")");
            Decrease_Indent (S, 2);

         else
            Put (S, "Raw");
         end if;

         Put (S, ";" & LF);

         Put (S, "return val;" & LF);
         Decrease_Indent (S, 3);
         Put (S, "}" & LF & LF);
      end Put_C_Getter;

      -------------------
      -- Put_C_Getters --
      -------------------

      procedure Put_C_Getters
        (S : in out Sink; Root : Root_Type)
      is
      begin
         Put (S, "// Getters for fields" & LF & LF);

         for F in First_Field (Root) .. Last_Field (Root) loop
            Put_C_Getter (S, F);
         end loop;
      end Put_C_Getters;

      --------------------------
      -- Put_Union_Membership --
      --------------------------

      procedure Put_Union_Membership
        (S : in out Sink; Root : Root_Type; Only_Prototypes : Boolean) is

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
                  Put (S, " ||" & LF);
               end if;

               --  Unions, other abstract types, and concrete types each have
               --  their own way of testing membership in the C++ code.

               if Child in Abstract_Type then
                  if Type_Table (Child).Is_Union then
                     Put (S, "Is_In_" & Image (Child) & " (kind)");

                  else
                     Put (S, "IN (kind, " & Image (Child) & ")");
                  end if;

               else
                  Put (S, "kind == " & Image (Child));
               end if;
            end loop;
         end Put_Ors;

      begin
         if not Only_Prototypes then
            Put (S, LF & "// Membership tests for union types" & LF & LF);
         end if;

         for T in First_Abstract (Root) .. Last_Abstract (Root) loop
            if Type_Table (T) /= null and then Type_Table (T).Is_Union then
               Put (S, "INLINE Boolean" & LF);
               Put (S, "Is_In_" & Image (T) & " (" &
                    Node_Or_Entity (Root) & "_Kind kind)" &
                    (if Only_Prototypes then ";" else "") & LF);

               if not Only_Prototypes then
                  Put (S, "{" & LF);
                  Increase_Indent (S, 3);
                  Put (S, "return" & LF);
                  Increase_Indent (S, 3);
                  Put_Ors (T);
                  Decrease_Indent (S, 3);
                  Decrease_Indent (S, 3);
                  Put (S, ";" & LF & "}" & LF);
               end if;

               Put (S, "" & LF);
            end if;
         end loop;
      end Put_Union_Membership;

      ---------------------
      -- Put_Sinfo_Dot_H --
      ---------------------

      procedure Put_Sinfo_Dot_H is
         S : Sink;

      begin
         Create_File (S, "sinfo.h");
         Put (S, "#ifdef __cplusplus" & LF);
         Put (S, "extern ""C"" {" & LF);
         Put (S, "#endif" & LF & LF);

         Put (S, "typedef Boolean Flag;" & LF & LF);

         Put (S, "#define N_Head " & N_Head & LF);
         Put (S, "" & LF);
         Put (S, "typedef struct Node_Header {" & LF);
         Increase_Indent (S, 2);
         Put (S, "any_slot Slots[N_Head];" & LF);
         Put (S, "Field_Offset Offset;" & LF);
         Decrease_Indent (S, 2);
         Put (S, "} Node_Header;" & LF & LF);

         Put (S, "extern Node_Header *Node_Offsets_Ptr;" & LF);
         Put (S, "extern any_slot *Slots_Ptr;" & LF & LF);

         Put_C_Type_And_Subtypes (S, Node_Kind);

         Put (S, "// Getters corresponding to instantiations of Atree.Get_n_Bit_Field"
                 & LF & LF);

         Put_C_Getters (S, Node_Kind);

         Put (S, "#ifdef __cplusplus" & LF);
         Put (S, "}" & LF);
         Put (S, "#endif" & LF);
      end Put_Sinfo_Dot_H;

      ---------------------
      -- Put_Einfo_Dot_H --
      ---------------------

      procedure Put_Einfo_Dot_H is
         S : Sink;

         procedure Put_Membership_Query_Spec (T : Node_Or_Entity_Type);
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

            Put (S, "INLINE B Is_" & Im2 & Typ & " (E Id)");
         end Put_Membership_Query_Spec;

         procedure Put_Membership_Query_Defn (T : Node_Or_Entity_Type) is
         begin
            if T in Abstract_Type and T not in Root_Type then
               Put_Membership_Query_Spec (T);
               Put (S, "" & LF);
               Increase_Indent (S, 3);
               Put (S, "{ return IN (Ekind (Id), " & Image (T) & "); }" & LF);
               Decrease_Indent (S, 3);
            end if;
         end Put_Membership_Query_Defn;

      begin
         Create_File (S, "einfo.h");
         Put (S, "#ifdef __cplusplus" & LF);
         Put (S, "extern ""C"" {" & LF);
         Put (S, "#endif" & LF & LF);

         Put (S, "typedef Boolean Flag;" & LF & LF);

         Put_C_Type_And_Subtypes (S, Entity_Kind);

         Put_C_Getters (S, Entity_Kind);

         Put (S, "// Abstract type queries" & LF & LF);

         Iterate_Types (Entity_Kind, Pre => Put_Membership_Query_Defn'Access);

         Put (S, LF & "#ifdef __cplusplus" & LF);
         Put (S, "}" & LF);
         Put (S, "#endif" & LF);
      end Put_Einfo_Dot_H;

   begin -- Compile

      Check_Completeness;

      Compute_Ranges (Node_Kind);
      Compute_Ranges (Entity_Kind);
      Compute_Fields_Per_Node;
      Compute_Field_Offsets;
      Compute_Type_Sizes;
      Check_For_Syntactic_Field_Mismatch;

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

   --------
   -- Sy --
   --------

   function Sy
     (Field      : Node_Field;
      Field_Type : Type_Enum;
      Default_Value : Field_Default_Value := No_Default;
      Pre, Pre_Get, Pre_Set : String := "") return Field_Sequence is
   begin
      return
        (1 => Create_Syntactic_Field
           (Field, Field_Type, Default_Value, Pre, Pre_Get, Pre_Set));
   end Sy;

   --------
   -- Sm --
   --------

   function Sm
     (Field      : Field_Enum;
      Field_Type : Type_Enum;
      Type_Only  : Type_Only_Enum := No_Type_Only;
      Pre, Pre_Get, Pre_Set : String := "") return Field_Sequence is
   begin
      return (1 => Create_Semantic_Field
                (Field, Field_Type, Type_Only, Pre, Pre_Get, Pre_Set));
   end Sm;

end Gen_IL.Gen;
