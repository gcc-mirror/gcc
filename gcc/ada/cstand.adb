------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               C S T A N D                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--          Copyright (C) 1992-2002 Free Software Foundation, Inc.          --
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

with Atree;    use Atree;
with Csets;    use Csets;
with Debug;    use Debug;
with Einfo;    use Einfo;
with Layout;   use Layout;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Tbuild;   use Tbuild;
with Ttypes;   use Ttypes;
with Ttypef;   use Ttypef;
with Sem_Mech; use Sem_Mech;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Stand;    use Stand;
with Uintp;    use Uintp;
with Urealp;   use Urealp;

package body CStand is

   Stloc  : constant Source_Ptr := Standard_Location;
   Staloc : constant Source_Ptr := Standard_ASCII_Location;
   --  Standard abbreviations used throughout this package

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Build_Float_Type (E : Entity_Id; Siz : Int; Digs : Int);
   --  Procedure to build standard predefined float base type. The first
   --  parameter is the entity for the type, and the second parameter
   --  is the size in bits. The third parameter is the digits value.

   procedure Build_Signed_Integer_Type (E : Entity_Id; Siz : Int);
   --  Procedure to build standard predefined signed integer subtype. The
   --  first parameter is the entity for the subtype. The second parameter
   --  is the size in bits. The corresponding base type is not built by
   --  this routine but instead must be built by the caller where needed.

   procedure Create_Operators;
   --  Make entries for each of the predefined operators in Standard

   procedure Create_Unconstrained_Base_Type
     (E : Entity_Id;
      K : Entity_Kind);
   --  The predefined signed integer types are constrained subtypes which
   --  must have a corresponding unconstrained base type. This type is almost
   --  useless. The only place it has semantics is Subtypes_Statically_Match.
   --  Consequently, we arrange for it to be identical apart from the setting
   --  of the constrained bit. This routine takes an entity E for the Type,
   --  copies it to estabish the base type, then resets the Ekind of the
   --  original entity to K (the Ekind for the subtype). The Etype field of
   --  E is set by the call (to point to the created base type entity), and
   --  also the Is_Constrained flag of E is set.
   --
   --  To understand the exact requirement for this, see RM 3.5.4(11) which
   --  makes it clear that Integer, for example, is constrained, with the
   --  constraint bounds matching the bounds of the (unconstrained) base
   --  type. The point is that Integer and Integer'Base have identical
   --  bounds, but do not statically match, since a subtype with constraints
   --  never matches a subtype with no constraints.

   function Identifier_For (S : Standard_Entity_Type) return Node_Id;
   --  Returns an identifier node with the same name as the defining
   --  identifier corresponding to the given Standard_Entity_Type value

   procedure Make_Component
     (Rec : Entity_Id;
      Typ : Entity_Id;
      Nam : String);
   --  Build a record component with the given type and name, and append to
   --  the list of components of Rec.

   function Make_Formal
     (Typ         : Entity_Id;
      Formal_Name : String)
      return        Entity_Id;
   --  Construct entity for subprogram formal with given name and type

   function Make_Integer (V : Uint) return Node_Id;
   --  Builds integer literal with given value

   procedure Make_Name (Id : Entity_Id; Nam : String);
   --  Make an entry in the names table for Nam, and set as Chars field of Id

   function New_Operator (Op : Name_Id; Typ : Entity_Id) return Entity_Id;
   --  Build entity for standard operator with given name and type.

   function New_Standard_Entity
     (New_Node_Kind : Node_Kind := N_Defining_Identifier)
      return          Entity_Id;
   --  Builds a new entity for Standard

   procedure Set_Integer_Bounds
     (Id  : Entity_Id;
      Typ : Entity_Id;
      Lb  : Uint;
      Hb  : Uint);
   --  Procedure to set bounds for integer type or subtype. Id is the entity
   --  whose bounds and type are to be set. The Typ parameter is the Etype
   --  value for the entity (which will be the same as Id for all predefined
   --  integer base types. The third and fourth parameters are the bounds.

   ----------------------
   -- Build_Float_Type --
   ----------------------

   procedure Build_Float_Type (E : Entity_Id; Siz : Int; Digs : Int) is
   begin
      Set_Type_Definition (Parent (E),
        Make_Floating_Point_Definition (Stloc,
          Digits_Expression => Make_Integer (UI_From_Int (Digs))));
      Set_Ekind                      (E, E_Floating_Point_Type);
      Set_Etype                      (E, E);
      Init_Size                      (E, Siz);
      Set_Prim_Alignment             (E);
      Init_Digits_Value              (E, Digs);
      Set_Float_Bounds               (E);
      Set_Is_Frozen                  (E);
      Set_Is_Public                  (E);
      Set_Size_Known_At_Compile_Time (E);
   end Build_Float_Type;

   -------------------------------
   -- Build_Signed_Integer_Type --
   -------------------------------

   procedure Build_Signed_Integer_Type (E : Entity_Id; Siz : Int) is
      U2Siz1 : constant Uint := 2 ** (Siz - 1);
      Lbound : constant Uint := -U2Siz1;
      Ubound : constant Uint := U2Siz1 - 1;

   begin
      Set_Type_Definition (Parent (E),
        Make_Signed_Integer_Type_Definition (Stloc,
          Low_Bound  => Make_Integer (Lbound),
          High_Bound => Make_Integer (Ubound)));

      Set_Ekind                      (E, E_Signed_Integer_Type);
      Set_Etype                      (E, E);
      Init_Size                      (E, Siz);
      Set_Prim_Alignment             (E);
      Set_Integer_Bounds             (E, E, Lbound, Ubound);
      Set_Is_Frozen                  (E);
      Set_Is_Public                  (E);
      Set_Is_Known_Valid             (E);
      Set_Size_Known_At_Compile_Time (E);
   end Build_Signed_Integer_Type;

   ----------------------
   -- Create_Operators --
   ----------------------

   --  Each operator has an abbreviated signature. The formals have the names
   --  LEFT and RIGHT. Their types are not actually used for resolution.

   procedure Create_Operators is
      Op_Node : Entity_Id;

      --  Following list has two entries for concatenation, to include
      --  explicitly the operation on wide strings.

      Binary_Ops : constant array (S_Binary_Ops) of Name_Id :=
        (Name_Op_Add,      Name_Op_And,   Name_Op_Concat,   Name_Op_Concat,
         Name_Op_Divide,   Name_Op_Eq,    Name_Op_Expon,    Name_Op_Ge,
         Name_Op_Gt,       Name_Op_Le,    Name_Op_Lt,       Name_Op_Mod,
         Name_Op_Multiply, Name_Op_Ne,    Name_Op_Or,       Name_Op_Rem,
         Name_Op_Subtract, Name_Op_Xor);

      Bin_Op_Types : constant array (S_Binary_Ops) of Entity_Id :=
        (Universal_Integer, Standard_Boolean,
         Standard_String,   Standard_Wide_String,
         Universal_Integer, Standard_Boolean,
         Universal_Integer, Standard_Boolean,
         Standard_Boolean,  Standard_Boolean,
         Standard_Boolean,  Universal_Integer,
         Universal_Integer, Standard_Boolean,
         Standard_Boolean,  Universal_Integer,
         Universal_Integer, Standard_Boolean);

      Unary_Ops : constant array (S_Unary_Ops) of Name_Id :=
        (Name_Op_Abs, Name_Op_Subtract, Name_Op_Not, Name_Op_Add);

      Unary_Op_Types : constant array (S_Unary_Ops) of Entity_Id :=
        (Universal_Integer, Universal_Integer,
         Standard_Boolean,  Universal_Integer);

      --  Corresponding to Abs, Minus, Not, and Plus.

   begin
      for J in S_Binary_Ops loop
         Op_Node := New_Operator (Binary_Ops (J), Bin_Op_Types (J));
         SE (J)  := Op_Node;
         Append_Entity (Make_Formal (Any_Type, "LEFT"),  Op_Node);
         Append_Entity (Make_Formal (Any_Type, "RIGHT"), Op_Node);
      end loop;

      for J in S_Unary_Ops loop
         Op_Node := New_Operator (Unary_Ops (J), Unary_Op_Types (J));
         SE (J)  := Op_Node;
         Append_Entity (Make_Formal (Any_Type, "RIGHT"), Op_Node);
      end loop;

      --  For concatenation, we create a separate operator for each
      --  array type. This simplifies the resolution of the component-
      --  component concatenation operation. In Standard, we set the types
      --  of the formals for string and wide string concatenation.

      Set_Etype (First_Entity (Standard_Op_Concat),  Standard_String);
      Set_Etype (Last_Entity  (Standard_Op_Concat),  Standard_String);

      Set_Etype (First_Entity (Standard_Op_Concatw), Standard_Wide_String);
      Set_Etype (Last_Entity  (Standard_Op_Concatw), Standard_Wide_String);

   end Create_Operators;

   ---------------------
   -- Create_Standard --
   ---------------------

   --  The tree for the package Standard is prefixed to all compilations.
   --  Several entities required by semantic analysis are denoted by global
   --  variables that are initialized to point to the corresponding
   --  occurrences in STANDARD. The visible entities of STANDARD are
   --  created here. The private entities defined in STANDARD are created
   --  by Initialize_Standard in the semantics module.

   procedure Create_Standard is
      Decl_S : List_Id;
      --  List of declarations in Standard

      Decl_A : List_Id;
      --  List of declarations in ASCII

      Decl       : Node_Id;
      Pspec      : Node_Id;
      Tdef_Node  : Node_Id;
      Ident_Node : Node_Id;
      Ccode      : Char_Code;
      E_Id       : Entity_Id;
      R_Node     : Node_Id;
      B_Node     : Node_Id;

      procedure Build_Exception (S : Standard_Entity_Type);
      --  Procedure to declare given entity as an exception

      ---------------------
      -- Build_Exception --
      ---------------------

      procedure Build_Exception (S : Standard_Entity_Type) is
      begin
         Set_Ekind          (Standard_Entity (S), E_Exception);
         Set_Etype          (Standard_Entity (S), Standard_Exception_Type);
         Set_Exception_Code (Standard_Entity (S), Uint_0);
         Set_Is_Public      (Standard_Entity (S), True);

         Decl :=
           Make_Exception_Declaration (Stloc,
             Defining_Identifier => Standard_Entity (S));
         Append (Decl, Decl_S);
      end Build_Exception;

   --  Start of processing for Create_Standard

   begin
      Decl_S := New_List;

      --  First step is to create defining identifiers for each entity

      for S in Standard_Entity_Type loop
         declare
            S_Name : constant String := Standard_Entity_Type'Image (S);
            --  Name of entity (note we skip S_ at the start)

            Ident_Node : Node_Id;
            --  Defining identifier node

         begin
            Ident_Node := New_Standard_Entity;
            Make_Name (Ident_Node, S_Name (3 .. S_Name'Length));
            Standard_Entity (S) := Ident_Node;
         end;
      end loop;

      --  Create package declaration node for package Standard

      Standard_Package_Node := New_Node (N_Package_Declaration, Stloc);

      Pspec := New_Node (N_Package_Specification, Stloc);
      Set_Specification (Standard_Package_Node, Pspec);

      Set_Defining_Unit_Name (Pspec, Standard_Standard);
      Set_Visible_Declarations (Pspec, Decl_S);

      Set_Ekind (Standard_Standard, E_Package);
      Set_Is_Pure (Standard_Standard);
      Set_Is_Compilation_Unit (Standard_Standard);

      --  Create type declaration nodes for standard types

      for S in S_Types loop
         Decl := New_Node (N_Full_Type_Declaration, Stloc);
         Set_Defining_Identifier (Decl, Standard_Entity (S));
         Set_Is_Frozen (Standard_Entity (S));
         Set_Is_Public (Standard_Entity (S));
         Append (Decl, Decl_S);
      end loop;

      --  Create type definition node for type Boolean. The Size is set to
      --  1 as required by Ada 95 and current ARG interpretations for Ada/83.

      --  Note: Object_Size of Boolean is 8. This means that we do NOT in
      --  general know that Boolean variables have valid values, so we do
      --  not set the Is_Known_Valid flag.

      Tdef_Node := New_Node (N_Enumeration_Type_Definition, Stloc);
      Set_Literals (Tdef_Node, New_List);
      Append (Standard_False, Literals (Tdef_Node));
      Append (Standard_True, Literals (Tdef_Node));
      Set_Type_Definition (Parent (Standard_Boolean), Tdef_Node);

      Set_Ekind          (Standard_Boolean, E_Enumeration_Type);
      Set_First_Literal  (Standard_Boolean, Standard_False);
      Set_Etype          (Standard_Boolean, Standard_Boolean);
      Init_Esize         (Standard_Boolean, Standard_Character_Size);
      Init_RM_Size       (Standard_Boolean, 1);
      Set_Prim_Alignment (Standard_Boolean);

      Set_Is_Unsigned_Type           (Standard_Boolean);
      Set_Size_Known_At_Compile_Time (Standard_Boolean);

      Set_Ekind           (Standard_True, E_Enumeration_Literal);
      Set_Etype           (Standard_True, Standard_Boolean);
      Set_Enumeration_Pos (Standard_True, Uint_1);
      Set_Enumeration_Rep (Standard_True, Uint_1);
      Set_Is_Known_Valid  (Standard_True, True);

      Set_Ekind           (Standard_False, E_Enumeration_Literal);
      Set_Etype           (Standard_False, Standard_Boolean);
      Set_Enumeration_Pos (Standard_False, Uint_0);
      Set_Enumeration_Rep (Standard_False, Uint_0);
      Set_Is_Known_Valid  (Standard_False, True);

      --  For the bounds of Boolean, we create a range node corresponding to

      --    range False .. True

      --  where the occurrences of the literals must point to the
      --  corresponding  definition.

      R_Node := New_Node (N_Range, Stloc);
      B_Node := New_Node (N_Identifier, Stloc);
      Set_Chars  (B_Node, Chars (Standard_False));
      Set_Entity (B_Node,  Standard_False);
      Set_Etype  (B_Node, Standard_Boolean);
      Set_Is_Static_Expression (B_Node);
      Set_Low_Bound  (R_Node, B_Node);

      B_Node := New_Node (N_Identifier, Stloc);
      Set_Chars  (B_Node, Chars (Standard_True));
      Set_Entity (B_Node,  Standard_True);
      Set_Etype  (B_Node, Standard_Boolean);
      Set_Is_Static_Expression (B_Node);
      Set_High_Bound (R_Node, B_Node);

      Set_Scalar_Range (Standard_Boolean, R_Node);
      Set_Etype (R_Node, Standard_Boolean);
      Set_Parent (R_Node, Standard_Boolean);

      --  Create type definition nodes for predefined integer types

      Build_Signed_Integer_Type
        (Standard_Short_Short_Integer, Standard_Short_Short_Integer_Size);

      Build_Signed_Integer_Type
        (Standard_Short_Integer, Standard_Short_Integer_Size);

      Build_Signed_Integer_Type
        (Standard_Integer, Standard_Integer_Size);

      declare
         LIS : Nat;

      begin
         if Debug_Flag_M then
            LIS := 64;
         else
            LIS := Standard_Long_Integer_Size;
         end if;

         Build_Signed_Integer_Type (Standard_Long_Integer, LIS);
      end;

      Build_Signed_Integer_Type
        (Standard_Long_Long_Integer, Standard_Long_Long_Integer_Size);

      Create_Unconstrained_Base_Type
        (Standard_Short_Short_Integer, E_Signed_Integer_Subtype);

      Create_Unconstrained_Base_Type
        (Standard_Short_Integer, E_Signed_Integer_Subtype);

      Create_Unconstrained_Base_Type
        (Standard_Integer, E_Signed_Integer_Subtype);

      Create_Unconstrained_Base_Type
        (Standard_Long_Integer, E_Signed_Integer_Subtype);

      Create_Unconstrained_Base_Type
        (Standard_Long_Long_Integer, E_Signed_Integer_Subtype);

      --  Create type definition nodes for predefined float types

      Build_Float_Type
        (Standard_Short_Float,
         Standard_Short_Float_Size,
         Standard_Short_Float_Digits);

      Build_Float_Type
        (Standard_Float,
         Standard_Float_Size,
         Standard_Float_Digits);

      Build_Float_Type
        (Standard_Long_Float,
         Standard_Long_Float_Size,
         Standard_Long_Float_Digits);

      Build_Float_Type
        (Standard_Long_Long_Float,
         Standard_Long_Long_Float_Size,
         Standard_Long_Long_Float_Digits);

      --  Create type definition node for type Character. Note that we do not
      --  set the Literals field, since type Character is handled with special
      --  routine that do not need a literal list.

      Tdef_Node := New_Node (N_Enumeration_Type_Definition, Stloc);
      Set_Type_Definition (Parent (Standard_Character), Tdef_Node);

      Set_Ekind          (Standard_Character, E_Enumeration_Type);
      Set_Etype          (Standard_Character, Standard_Character);
      Init_Esize         (Standard_Character, Standard_Character_Size);
      Init_RM_Size       (Standard_Character, 8);
      Set_Prim_Alignment (Standard_Character);

      Set_Is_Unsigned_Type           (Standard_Character);
      Set_Is_Character_Type          (Standard_Character);
      Set_Is_Known_Valid             (Standard_Character);
      Set_Size_Known_At_Compile_Time (Standard_Character);

      --  Create the bounds for type Character.

      R_Node := New_Node (N_Range, Stloc);

      --  Low bound for type Character (Standard.Nul)

      B_Node := New_Node (N_Character_Literal, Stloc);
      Set_Is_Static_Expression (B_Node);
      Set_Chars                (B_Node, No_Name);
      Set_Char_Literal_Value   (B_Node, 16#00#);
      Set_Entity               (B_Node,  Empty);
      Set_Etype                (B_Node, Standard_Character);
      Set_Low_Bound (R_Node, B_Node);

      --  High bound for type Character

      B_Node := New_Node (N_Character_Literal, Stloc);
      Set_Is_Static_Expression (B_Node);
      Set_Chars                (B_Node, No_Name);
      Set_Char_Literal_Value   (B_Node, 16#FF#);
      Set_Entity               (B_Node,  Empty);
      Set_Etype                (B_Node, Standard_Character);
      Set_High_Bound (R_Node, B_Node);

      Set_Scalar_Range (Standard_Character, R_Node);
      Set_Etype (R_Node, Standard_Character);
      Set_Parent (R_Node, Standard_Character);

      --  Create type definition for type Wide_Character. Note that we do not
      --  set the Literals field, since type Wide_Character is handled with
      --  special routines that do not need a literal list.

      Tdef_Node := New_Node (N_Enumeration_Type_Definition, Stloc);
      Set_Type_Definition (Parent (Standard_Wide_Character), Tdef_Node);

      Set_Ekind      (Standard_Wide_Character, E_Enumeration_Type);
      Set_Etype      (Standard_Wide_Character, Standard_Wide_Character);
      Init_Size      (Standard_Wide_Character, Standard_Wide_Character_Size);

      Set_Prim_Alignment             (Standard_Wide_Character);
      Set_Is_Unsigned_Type           (Standard_Wide_Character);
      Set_Is_Character_Type          (Standard_Wide_Character);
      Set_Is_Known_Valid             (Standard_Wide_Character);
      Set_Size_Known_At_Compile_Time (Standard_Wide_Character);

      --  Create the bounds for type Wide_Character.

      R_Node := New_Node (N_Range, Stloc);

      --  Low bound for type Wide_Character

      B_Node := New_Node (N_Character_Literal, Stloc);
      Set_Is_Static_Expression (B_Node);
      Set_Chars                (B_Node, No_Name);    --  ???
      Set_Char_Literal_Value   (B_Node, 16#0000#);
      Set_Entity               (B_Node,  Empty);
      Set_Etype                (B_Node, Standard_Wide_Character);
      Set_Low_Bound (R_Node, B_Node);

      --  High bound for type Wide_Character

      B_Node := New_Node (N_Character_Literal, Stloc);
      Set_Is_Static_Expression (B_Node);
      Set_Chars                (B_Node, No_Name);    --  ???
      Set_Char_Literal_Value   (B_Node, 16#FFFF#);
      Set_Entity               (B_Node,  Empty);
      Set_Etype                (B_Node, Standard_Wide_Character);
      Set_High_Bound           (R_Node, B_Node);

      Set_Scalar_Range (Standard_Wide_Character, R_Node);
      Set_Etype (R_Node, Standard_Wide_Character);
      Set_Parent (R_Node, Standard_Wide_Character);

      --  Create type definition node for type String

      Tdef_Node := New_Node (N_Unconstrained_Array_Definition, Stloc);
      Set_Subtype_Indication (Tdef_Node, Identifier_For (S_Character));
      Set_Subtype_Marks      (Tdef_Node, New_List);
      Append (Identifier_For (S_Positive), Subtype_Marks (Tdef_Node));
      Set_Type_Definition (Parent (Standard_String), Tdef_Node);

      Set_Ekind          (Standard_String, E_String_Type);
      Set_Etype          (Standard_String, Standard_String);
      Set_Component_Type (Standard_String, Standard_Character);
      Set_Component_Size (Standard_String, Uint_8);
      Init_Size_Align    (Standard_String);

      --  Set index type of String

      E_Id := First
        (Subtype_Marks (Type_Definition (Parent (Standard_String))));
      Set_First_Index (Standard_String, E_Id);
      Set_Entity (E_Id, Standard_Positive);
      Set_Etype (E_Id, Standard_Positive);

      --  Create type definition node for type Wide_String

      Tdef_Node := New_Node (N_Unconstrained_Array_Definition, Stloc);
      Set_Subtype_Indication (Tdef_Node, Identifier_For (S_Wide_Character));
      Set_Subtype_Marks (Tdef_Node, New_List);
      Append (Identifier_For (S_Positive), Subtype_Marks (Tdef_Node));
      Set_Type_Definition (Parent (Standard_Wide_String), Tdef_Node);

      Set_Ekind          (Standard_Wide_String, E_String_Type);
      Set_Etype          (Standard_Wide_String, Standard_Wide_String);
      Set_Component_Type (Standard_Wide_String, Standard_Wide_Character);
      Set_Component_Size (Standard_Wide_String, Uint_16);
      Init_Size_Align    (Standard_Wide_String);

      --  Set index type of Wide_String

      E_Id := First
        (Subtype_Marks (Type_Definition (Parent (Standard_Wide_String))));
      Set_First_Index (Standard_Wide_String, E_Id);
      Set_Entity (E_Id, Standard_Positive);
      Set_Etype (E_Id, Standard_Positive);

      --  Create subtype declaration for Natural

      Decl := New_Node (N_Subtype_Declaration, Stloc);
      Set_Defining_Identifier (Decl, Standard_Natural);
      Set_Subtype_Indication (Decl,
        New_Occurrence_Of (Standard_Integer, Stloc));
      Append (Decl, Decl_S);

      Set_Ekind          (Standard_Natural, E_Signed_Integer_Subtype);
      Set_Etype          (Standard_Natural, Base_Type (Standard_Integer));
      Init_Esize         (Standard_Natural, Standard_Integer_Size);
      Init_RM_Size       (Standard_Natural, Standard_Integer_Size - 1);
      Set_Prim_Alignment (Standard_Natural);
      Set_Size_Known_At_Compile_Time
                         (Standard_Natural);
      Set_Integer_Bounds (Standard_Natural,
        Typ => Base_Type (Standard_Integer),
        Lb  => Uint_0,
        Hb  => Intval (High_Bound (Scalar_Range (Standard_Integer))));
      Set_Is_Constrained (Standard_Natural);
      Set_Is_Frozen      (Standard_Natural);
      Set_Is_Public      (Standard_Natural);

      --  Create subtype declaration for Positive

      Decl := New_Node (N_Subtype_Declaration, Stloc);
      Set_Defining_Identifier (Decl, Standard_Positive);
      Set_Subtype_Indication (Decl,
        New_Occurrence_Of (Standard_Integer, Stloc));
      Append (Decl, Decl_S);

      Set_Ekind          (Standard_Positive, E_Signed_Integer_Subtype);
      Set_Etype          (Standard_Positive, Base_Type (Standard_Integer));
      Init_Esize         (Standard_Positive, Standard_Integer_Size);
      Init_RM_Size       (Standard_Positive, Standard_Integer_Size - 1);
      Set_Prim_Alignment (Standard_Positive);

      Set_Size_Known_At_Compile_Time (Standard_Positive);

      Set_Integer_Bounds   (Standard_Positive,
         Typ => Base_Type (Standard_Integer),
         Lb  => Uint_1,
         Hb  => Intval (High_Bound (Scalar_Range (Standard_Integer))));
      Set_Is_Constrained   (Standard_Positive);
      Set_Is_Frozen        (Standard_Positive);
      Set_Is_Public        (Standard_Positive);

      --  Create declaration for package ASCII

      Decl := New_Node (N_Package_Declaration, Stloc);
      Append (Decl, Decl_S);

      Pspec := New_Node (N_Package_Specification, Stloc);
      Set_Specification (Decl, Pspec);

      Set_Defining_Unit_Name (Pspec, Standard_Entity (S_ASCII));
      Set_Ekind (Standard_Entity (S_ASCII), E_Package);
      Decl_A := New_List; -- for ASCII declarations
      Set_Visible_Declarations (Pspec, Decl_A);

      --  Create control character definitions in package ASCII. Note that
      --  the character literal entries created here correspond to literal
      --  values that are impossible in the source, but can be represented
      --  internally with no difficulties.

      Ccode := 16#00#;

      for S in S_ASCII_Names loop
         Decl := New_Node (N_Object_Declaration, Staloc);
         Set_Constant_Present (Decl, True);

         declare
            A_Char    : Entity_Id := Standard_Entity (S);
            Expr_Decl : Node_Id;

         begin
            Set_Sloc                   (A_Char, Staloc);
            Set_Ekind                  (A_Char, E_Constant);
            Set_Not_Source_Assigned    (A_Char, True);
            Set_Is_True_Constant       (A_Char, True);
            Set_Etype                  (A_Char, Standard_Character);
            Set_Scope                  (A_Char, Standard_Entity (S_ASCII));
            Set_Is_Immediately_Visible (A_Char, False);
            Set_Is_Public              (A_Char, True);
            Set_Is_Known_Valid         (A_Char, True);

            Append_Entity (A_Char, Standard_Entity (S_ASCII));
            Set_Defining_Identifier (Decl, A_Char);

            Set_Object_Definition (Decl, Identifier_For (S_Character));
            Expr_Decl := New_Node (N_Character_Literal, Staloc);
            Set_Expression (Decl, Expr_Decl);

            Set_Is_Static_Expression (Expr_Decl);
            Set_Chars                (Expr_Decl, No_Name);
            Set_Etype                (Expr_Decl, Standard_Character);
            Set_Char_Literal_Value   (Expr_Decl, Ccode);
         end;

         Append (Decl, Decl_A);

         --  Increment character code, dealing with non-contiguities

         Ccode := Ccode + 1;

         if Ccode = 16#20# then
            Ccode := 16#21#;
         elsif Ccode = 16#27# then
            Ccode := 16#3A#;
         elsif Ccode = 16#3C# then
            Ccode := 16#3F#;
         elsif Ccode = 16#41# then
            Ccode := 16#5B#;
         end if;
      end loop;

      --  Create semantic phase entities

      Standard_Void_Type := New_Standard_Entity;
      Set_Ekind       (Standard_Void_Type, E_Void);
      Set_Etype       (Standard_Void_Type, Standard_Void_Type);
      Init_Size_Align (Standard_Void_Type);
      Set_Scope       (Standard_Void_Type, Standard_Standard);
      Make_Name       (Standard_Void_Type, "_void_type");

      --  The type field of packages is set to void

      Set_Etype (Standard_Standard, Standard_Void_Type);
      Set_Etype (Standard_ASCII, Standard_Void_Type);

      --  Standard_A_String is actually used in generated code, so it has a
      --  type name that is reasonable, but does not overlap any Ada name.

      Standard_A_String := New_Standard_Entity;
      Set_Ekind      (Standard_A_String, E_Access_Type);
      Set_Scope      (Standard_A_String, Standard_Standard);
      Set_Etype      (Standard_A_String, Standard_A_String);

      if Debug_Flag_6 then
         Init_Size   (Standard_A_String, System_Address_Size);
      else
         Init_Size   (Standard_A_String, System_Address_Size * 2);
      end if;

      Init_Alignment (Standard_A_String);

      Set_Directly_Designated_Type
                     (Standard_A_String, Standard_String);
      Make_Name      (Standard_A_String, "access_string");

      Standard_A_Char := New_Standard_Entity;
      Set_Ekind          (Standard_A_Char, E_Access_Type);
      Set_Scope          (Standard_A_Char, Standard_Standard);
      Set_Etype          (Standard_A_Char, Standard_A_String);
      Init_Size          (Standard_A_Char, System_Address_Size);
      Set_Prim_Alignment (Standard_A_Char);

      Set_Directly_Designated_Type (Standard_A_Char, Standard_Character);
      Make_Name     (Standard_A_Char, "access_character");

      --  Note on type names. The type names for the following special types
      --  are constructed so that they will look reasonable should they ever
      --  appear in error messages etc, although in practice the use of the
      --  special insertion character } for types results in special handling
      --  of these type names in any case. The blanks in these names would
      --  trouble in Gigi, but that's OK here, since none of these types
      --  should ever get through to Gigi! Attributes of these types are
      --  filled out to minimize problems with cascaded errors (for example,
      --  Any_Integer is given reasonable and consistent type and size values)

      Any_Type := New_Standard_Entity;
      Decl := New_Node (N_Full_Type_Declaration, Stloc);
      Set_Defining_Identifier (Decl, Any_Type);
      Set_Scope (Any_Type, Standard_Standard);
      Build_Signed_Integer_Type (Any_Type, Standard_Integer_Size);
      Make_Name (Any_Type, "any type");

      Any_Id := New_Standard_Entity;
      Set_Ekind             (Any_Id, E_Variable);
      Set_Scope             (Any_Id, Standard_Standard);
      Set_Etype             (Any_Id, Any_Type);
      Init_Size_Align       (Any_Id);
      Make_Name             (Any_Id, "any id");

      Any_Access := New_Standard_Entity;
      Set_Ekind             (Any_Access, E_Access_Type);
      Set_Scope             (Any_Access, Standard_Standard);
      Set_Etype             (Any_Access, Any_Access);
      Init_Size             (Any_Access, System_Address_Size);
      Set_Prim_Alignment    (Any_Access);
      Make_Name             (Any_Access, "an access type");

      Any_Array := New_Standard_Entity;
      Set_Ekind             (Any_Array, E_String_Type);
      Set_Scope             (Any_Array, Standard_Standard);
      Set_Etype             (Any_Array, Any_Array);
      Set_Component_Type    (Any_Array, Any_Character);
      Init_Size_Align       (Any_Array);
      Make_Name             (Any_Array, "an array type");

      Any_Boolean := New_Standard_Entity;
      Set_Ekind             (Any_Boolean, E_Enumeration_Type);
      Set_Scope             (Any_Boolean, Standard_Standard);
      Set_Etype             (Any_Boolean, Standard_Boolean);
      Init_Esize            (Any_Boolean, Standard_Character_Size);
      Init_RM_Size          (Any_Boolean, 1);
      Set_Prim_Alignment    (Any_Boolean);
      Set_Is_Unsigned_Type  (Any_Boolean);
      Set_Scalar_Range      (Any_Boolean, Scalar_Range (Standard_Boolean));
      Make_Name             (Any_Boolean, "a boolean type");

      Any_Character := New_Standard_Entity;
      Set_Ekind             (Any_Character, E_Enumeration_Type);
      Set_Scope             (Any_Character, Standard_Standard);
      Set_Etype             (Any_Character, Any_Character);
      Set_Is_Unsigned_Type  (Any_Character);
      Set_Is_Character_Type (Any_Character);
      Init_Esize            (Any_Character, Standard_Character_Size);
      Init_RM_Size          (Any_Character, 8);
      Set_Prim_Alignment    (Any_Character);
      Set_Scalar_Range      (Any_Character, Scalar_Range (Standard_Character));
      Make_Name             (Any_Character, "a character type");

      Any_Composite := New_Standard_Entity;
      Set_Ekind             (Any_Composite, E_Array_Type);
      Set_Scope             (Any_Composite, Standard_Standard);
      Set_Etype             (Any_Composite, Any_Composite);
      Set_Component_Size    (Any_Composite, Uint_0);
      Set_Component_Type    (Any_Composite, Standard_Integer);
      Init_Size_Align       (Any_Composite);
      Make_Name             (Any_Composite, "a composite type");

      Any_Discrete := New_Standard_Entity;
      Set_Ekind             (Any_Discrete, E_Signed_Integer_Type);
      Set_Scope             (Any_Discrete, Standard_Standard);
      Set_Etype             (Any_Discrete, Any_Discrete);
      Init_Size             (Any_Discrete, Standard_Integer_Size);
      Set_Prim_Alignment    (Any_Discrete);
      Make_Name             (Any_Discrete, "a discrete type");

      Any_Fixed := New_Standard_Entity;
      Set_Ekind             (Any_Fixed, E_Ordinary_Fixed_Point_Type);
      Set_Scope             (Any_Fixed, Standard_Standard);
      Set_Etype             (Any_Fixed, Any_Fixed);
      Init_Size             (Any_Fixed, Standard_Integer_Size);
      Set_Prim_Alignment    (Any_Fixed);
      Make_Name             (Any_Fixed, "a fixed-point type");

      Any_Integer := New_Standard_Entity;
      Set_Ekind             (Any_Integer, E_Signed_Integer_Type);
      Set_Scope             (Any_Integer, Standard_Standard);
      Set_Etype             (Any_Integer, Standard_Long_Long_Integer);
      Init_Size             (Any_Integer, Standard_Long_Long_Integer_Size);
      Set_Prim_Alignment    (Any_Integer);

      Set_Integer_Bounds
        (Any_Integer,
         Typ => Base_Type (Standard_Integer),
         Lb  => Uint_0,
         Hb  => Intval (High_Bound (Scalar_Range (Standard_Integer))));
      Make_Name (Any_Integer, "an integer type");

      Any_Modular := New_Standard_Entity;
      Set_Ekind             (Any_Modular, E_Modular_Integer_Type);
      Set_Scope             (Any_Modular, Standard_Standard);
      Set_Etype             (Any_Modular, Standard_Long_Long_Integer);
      Init_Size             (Any_Modular, Standard_Long_Long_Integer_Size);
      Set_Prim_Alignment    (Any_Modular);
      Set_Is_Unsigned_Type  (Any_Modular);
      Make_Name             (Any_Modular, "a modular type");

      Any_Numeric := New_Standard_Entity;
      Set_Ekind             (Any_Numeric, E_Signed_Integer_Type);
      Set_Scope             (Any_Numeric, Standard_Standard);
      Set_Etype             (Any_Numeric, Standard_Long_Long_Integer);
      Init_Size             (Any_Numeric, Standard_Long_Long_Integer_Size);
      Set_Prim_Alignment    (Any_Numeric);
      Make_Name             (Any_Numeric, "a numeric type");

      Any_Real := New_Standard_Entity;
      Set_Ekind             (Any_Real, E_Floating_Point_Type);
      Set_Scope             (Any_Real, Standard_Standard);
      Set_Etype             (Any_Real, Standard_Long_Long_Float);
      Init_Size             (Any_Real, Standard_Long_Long_Float_Size);
      Set_Prim_Alignment    (Any_Real);
      Make_Name             (Any_Real, "a real type");

      Any_Scalar := New_Standard_Entity;
      Set_Ekind             (Any_Scalar, E_Signed_Integer_Type);
      Set_Scope             (Any_Scalar, Standard_Standard);
      Set_Etype             (Any_Scalar, Any_Scalar);
      Init_Size             (Any_Scalar, Standard_Integer_Size);
      Set_Prim_Alignment    (Any_Scalar);
      Make_Name             (Any_Scalar, "a scalar type");

      Any_String := New_Standard_Entity;
      Set_Ekind             (Any_String, E_String_Type);
      Set_Scope             (Any_String, Standard_Standard);
      Set_Etype             (Any_String, Any_String);
      Set_Component_Type    (Any_String, Any_Character);
      Init_Size_Align       (Any_String);
      Make_Name             (Any_String, "a string type");

      declare
         Index   : Node_Id;
         Indexes : List_Id;

      begin
         Index :=
           Make_Range (Stloc,
             Low_Bound  => Make_Integer (Uint_0),
             High_Bound => Make_Integer (Uint_2 ** Standard_Integer_Size));
         Indexes := New_List (Index);
         Set_Etype (Index, Standard_Integer);
         Set_First_Index (Any_String, Index);
      end;

      Standard_Integer_8 := New_Standard_Entity;
      Decl := New_Node (N_Full_Type_Declaration, Stloc);
      Set_Defining_Identifier (Decl, Standard_Integer_8);
      Make_Name (Standard_Integer_8, "integer_8");
      Set_Scope (Standard_Integer_8, Standard_Standard);
      Build_Signed_Integer_Type (Standard_Integer_8, 8);

      Standard_Integer_16 := New_Standard_Entity;
      Decl := New_Node (N_Full_Type_Declaration, Stloc);
      Set_Defining_Identifier (Decl, Standard_Integer_16);
      Make_Name (Standard_Integer_16, "integer_16");
      Set_Scope (Standard_Integer_16, Standard_Standard);
      Build_Signed_Integer_Type (Standard_Integer_16, 16);

      Standard_Integer_32 := New_Standard_Entity;
      Decl := New_Node (N_Full_Type_Declaration, Stloc);
      Set_Defining_Identifier (Decl, Standard_Integer_32);
      Make_Name (Standard_Integer_32, "integer_32");
      Set_Scope (Standard_Integer_32, Standard_Standard);
      Build_Signed_Integer_Type (Standard_Integer_32, 32);

      Standard_Integer_64 := New_Standard_Entity;
      Decl := New_Node (N_Full_Type_Declaration, Stloc);
      Set_Defining_Identifier (Decl, Standard_Integer_64);
      Make_Name (Standard_Integer_64, "integer_64");
      Set_Scope (Standard_Integer_64, Standard_Standard);
      Build_Signed_Integer_Type (Standard_Integer_64, 64);

      Standard_Unsigned := New_Standard_Entity;
      Decl := New_Node (N_Full_Type_Declaration, Stloc);
      Set_Defining_Identifier (Decl, Standard_Unsigned);
      Make_Name (Standard_Unsigned, "unsigned");

      Set_Ekind             (Standard_Unsigned, E_Modular_Integer_Type);
      Set_Scope             (Standard_Unsigned, Standard_Standard);
      Set_Etype             (Standard_Unsigned, Standard_Unsigned);
      Init_Size             (Standard_Unsigned, Standard_Integer_Size);
      Set_Prim_Alignment    (Standard_Unsigned);
      Set_Modulus           (Standard_Unsigned,
                              Uint_2 ** Standard_Integer_Size);

      Set_Is_Unsigned_Type  (Standard_Unsigned);

      R_Node := New_Node (N_Range, Stloc);
      Set_Low_Bound  (R_Node,
        Make_Integer_Literal (Stloc, 0));
      Set_High_Bound (R_Node,
        Make_Integer_Literal (Stloc, Modulus (Standard_Unsigned)));
      Set_Scalar_Range (Standard_Unsigned, R_Node);

      --  Note: universal integer and universal real are constructed as fully
      --  formed signed numeric types, with parameters corresponding to the
      --  longest runtime types (Long_Long_Integer and Long_Long_Float). This
      --  allows Gigi to properly process references to universal types that
      --  are not folded at compile time.

      Universal_Integer := New_Standard_Entity;
      Decl := New_Node (N_Full_Type_Declaration, Stloc);
      Set_Defining_Identifier (Decl, Universal_Integer);
      Make_Name (Universal_Integer, "universal_integer");
      Set_Scope (Universal_Integer, Standard_Standard);
      Build_Signed_Integer_Type
        (Universal_Integer, Standard_Long_Long_Integer_Size);

      Universal_Real := New_Standard_Entity;
      Decl := New_Node (N_Full_Type_Declaration, Stloc);
      Set_Defining_Identifier (Decl, Universal_Real);
      Make_Name (Universal_Real, "universal_real");
      Set_Scope (Universal_Real, Standard_Standard);
      Build_Float_Type
        (Universal_Real,
         Standard_Long_Long_Float_Size,
         Standard_Long_Long_Float_Digits);

      --  Note: universal fixed, unlike universal integer and universal real,
      --  is never used at runtime, so it does not need to have bounds set.

      Universal_Fixed := New_Standard_Entity;
      Decl := New_Node (N_Full_Type_Declaration, Stloc);
      Set_Defining_Identifier (Decl, Universal_Fixed);
      Make_Name            (Universal_Fixed, "universal_fixed");
      Set_Ekind            (Universal_Fixed, E_Ordinary_Fixed_Point_Type);
      Set_Etype            (Universal_Fixed, Universal_Fixed);
      Set_Scope            (Universal_Fixed, Standard_Standard);
      Init_Size            (Universal_Fixed, Standard_Long_Long_Integer_Size);
      Set_Prim_Alignment   (Universal_Fixed);
      Set_Size_Known_At_Compile_Time
                           (Universal_Fixed);

      --  Create type declaration for Duration, using a 64-bit size. The
      --  delta value depends on the mode we are running in:

      --     Normal mode or No_Run_Time mode when word size is 64 bits:
      --       10**(-9) seconds, size is 64 bits

      --     No_Run_Time mode when word size is 32 bits:
      --       10**(-4) seconds, oize is 32 bits

      Build_Duration : declare
         Dlo         : Uint;
         Dhi         : Uint;
         Delta_Val   : Ureal;
         Use_32_Bits : constant Boolean :=
                         No_Run_Time and then System_Word_Size = 32;

      begin
         if Use_32_Bits then
            Dlo := Intval (Type_Low_Bound (Standard_Integer_32));
            Dhi := Intval (Type_High_Bound (Standard_Integer_32));
            Delta_Val := UR_From_Components (Uint_1, Uint_4, 10);

         else
            Dlo := Intval (Type_Low_Bound (Standard_Integer_64));
            Dhi := Intval (Type_High_Bound (Standard_Integer_64));
            Delta_Val := UR_From_Components (Uint_1, Uint_9, 10);
         end if;

         Decl :=
           Make_Full_Type_Declaration (Stloc,
             Defining_Identifier => Standard_Duration,
             Type_Definition =>
               Make_Ordinary_Fixed_Point_Definition (Stloc,
                 Delta_Expression => Make_Real_Literal (Stloc, Delta_Val),
                 Real_Range_Specification =>
                   Make_Real_Range_Specification (Stloc,
                     Low_Bound  => Make_Real_Literal (Stloc,
                       Realval => Dlo * Delta_Val),
                     High_Bound => Make_Real_Literal (Stloc,
                       Realval => Dhi * Delta_Val))));

         Set_Ekind (Standard_Duration, E_Ordinary_Fixed_Point_Type);
         Set_Etype (Standard_Duration, Standard_Duration);

         if Use_32_Bits then
            Init_Size (Standard_Duration, 32);
         else
            Init_Size (Standard_Duration, 64);
         end if;

         Set_Prim_Alignment (Standard_Duration);
         Set_Delta_Value    (Standard_Duration, Delta_Val);
         Set_Small_Value    (Standard_Duration, Delta_Val);
         Set_Scalar_Range   (Standard_Duration,
                              Real_Range_Specification
                                (Type_Definition (Decl)));

         --  Normally it does not matter that nodes in package Standard are
         --  not marked as analyzed. The Scalar_Range of the fixed-point
         --  type Standard_Duration is an exception, because of the special
         --  test made in Freeze.Freeze_Fixed_Point_Type.

         Set_Analyzed (Scalar_Range (Standard_Duration));

         Set_Etype (Type_High_Bound (Standard_Duration), Standard_Duration);
         Set_Etype (Type_Low_Bound  (Standard_Duration), Standard_Duration);

         Set_Is_Static_Expression (Type_High_Bound (Standard_Duration));
         Set_Is_Static_Expression (Type_Low_Bound  (Standard_Duration));

         Set_Corresponding_Integer_Value
           (Type_High_Bound (Standard_Duration), Dhi);

         Set_Corresponding_Integer_Value
           (Type_Low_Bound  (Standard_Duration), Dlo);

         Set_Size_Known_At_Compile_Time (Standard_Duration);
      end Build_Duration;

      --  Build standard exception type. Note that the type name here is
      --  actually used in the generated code, so it must be set correctly

      Standard_Exception_Type := New_Standard_Entity;
      Set_Ekind       (Standard_Exception_Type, E_Record_Type);
      Set_Etype       (Standard_Exception_Type, Standard_Exception_Type);
      Set_Scope       (Standard_Exception_Type, Standard_Standard);
      Set_Girder_Constraint
                      (Standard_Exception_Type, No_Elist);
      Init_Size_Align (Standard_Exception_Type);
      Set_Size_Known_At_Compile_Time
                      (Standard_Exception_Type, True);
      Make_Name       (Standard_Exception_Type, "exception");

      Make_Component  (Standard_Exception_Type, Standard_Boolean,
                                                 "Not_Handled_By_Others");
      Make_Component  (Standard_Exception_Type, Standard_Character, "Lang");
      Make_Component  (Standard_Exception_Type, Standard_Natural,
                                                           "Name_Length");
      Make_Component  (Standard_Exception_Type, Standard_A_Char,
                                                             "Full_Name");
      Make_Component  (Standard_Exception_Type, Standard_A_Char,
                                                            "HTable_Ptr");
      Make_Component  (Standard_Exception_Type, Standard_Integer,
                                                          "Import_Code");

      --  Build tree for record declaration, for use by the back-end.

      declare
         Comp_List : List_Id;
         Comp      : Entity_Id;

      begin
         Comp      := First_Entity (Standard_Exception_Type);
         Comp_List := New_List;

         while Present (Comp) loop
            Append (
              Make_Component_Declaration (Stloc,
                Defining_Identifier => Comp,
                Subtype_Indication => New_Occurrence_Of (Etype (Comp), Stloc)),
              Comp_List);

            Next_Entity (Comp);
         end loop;

         Decl := Make_Full_Type_Declaration (Stloc,
           Defining_Identifier => Standard_Exception_Type,
           Type_Definition =>
             Make_Record_Definition (Stloc,
               End_Label => Empty,
               Component_List =>
                 Make_Component_List (Stloc,
                   Component_Items => Comp_List)));
      end;

      Append (Decl, Decl_S);

      --  Create declarations of standard exceptions

      Build_Exception (S_Constraint_Error);
      Build_Exception (S_Program_Error);
      Build_Exception (S_Storage_Error);
      Build_Exception (S_Tasking_Error);

      --  Numeric_Error is a normal exception in Ada 83, but in Ada 95
      --  it is a renaming of Constraint_Error

      if Ada_83 then
         Build_Exception (S_Numeric_Error);

      else
         Decl := New_Node (N_Exception_Renaming_Declaration, Stloc);
         E_Id := Standard_Entity (S_Numeric_Error);

         Set_Ekind          (E_Id, E_Exception);
         Set_Exception_Code (E_Id, Uint_0);
         Set_Etype          (E_Id, Standard_Exception_Type);
         Set_Is_Public      (E_Id);
         Set_Renamed_Entity (E_Id, Standard_Entity (S_Constraint_Error));

         Set_Defining_Identifier (Decl, E_Id);
         Append (Decl, Decl_S);

         Ident_Node := New_Node (N_Identifier, Stloc);
         Set_Chars  (Ident_Node, Chars (Standard_Entity (S_Constraint_Error)));
         Set_Entity (Ident_Node, Standard_Entity (S_Constraint_Error));
         Set_Name   (Decl, Ident_Node);
      end if;

      --  Abort_Signal is an entity that does not get made visible

      Abort_Signal := New_Standard_Entity;
      Set_Chars          (Abort_Signal, Name_uAbort_Signal);
      Set_Ekind          (Abort_Signal, E_Exception);
      Set_Exception_Code (Abort_Signal, Uint_0);
      Set_Etype          (Abort_Signal, Standard_Exception_Type);
      Set_Scope          (Abort_Signal, Standard_Standard);
      Set_Is_Public      (Abort_Signal, True);
      Decl :=
        Make_Exception_Declaration (Stloc,
          Defining_Identifier => Abort_Signal);

      --  Create defining identifiers for shift operator entities. Note
      --  that these entities are used only for marking shift operators
      --  generated internally, and hence need no structure, just a name
      --  and a unique identity.

      Standard_Op_Rotate_Left := New_Standard_Entity;
      Set_Chars (Standard_Op_Rotate_Left, Name_Rotate_Left);
      Set_Ekind (Standard_Op_Rotate_Left, E_Operator);

      Standard_Op_Rotate_Right := New_Standard_Entity;
      Set_Chars (Standard_Op_Rotate_Right, Name_Rotate_Right);
      Set_Ekind (Standard_Op_Rotate_Right, E_Operator);

      Standard_Op_Shift_Left := New_Standard_Entity;
      Set_Chars (Standard_Op_Shift_Left, Name_Shift_Left);
      Set_Ekind (Standard_Op_Shift_Left, E_Operator);

      Standard_Op_Shift_Right := New_Standard_Entity;
      Set_Chars (Standard_Op_Shift_Right, Name_Shift_Right);
      Set_Ekind (Standard_Op_Shift_Right, E_Operator);

      Standard_Op_Shift_Right_Arithmetic := New_Standard_Entity;
      Set_Chars (Standard_Op_Shift_Right_Arithmetic,
                                          Name_Shift_Right_Arithmetic);
      Set_Ekind (Standard_Op_Shift_Right_Arithmetic,
                                          E_Operator);

      --  Create standard operator declarations

      Create_Operators;

      --  Initialize visibility table with entities in Standard

      for E in Standard_Entity_Type loop
         if Ekind (Standard_Entity (E)) /= E_Operator then
            Set_Name_Entity_Id
              (Chars (Standard_Entity (E)), Standard_Entity (E));
            Set_Homonym (Standard_Entity (E), Empty);
         end if;

         if E not in S_ASCII_Names then
            Set_Scope (Standard_Entity (E), Standard_Standard);
            Set_Is_Immediately_Visible (Standard_Entity (E));
         end if;
      end loop;

      --  The predefined package Standard itself does not have a scope;
      --  it is the only entity in the system not to have one, and this
      --  is what identifies the package to Gigi.

      Set_Scope (Standard_Standard, Empty);

      --  Set global variables indicating last Id values and version

      Last_Standard_Node_Id := Last_Node_Id;
      Last_Standard_List_Id := Last_List_Id;

      --  The Error node has an Etype of Any_Type to help error recovery

      Set_Etype (Error, Any_Type);
   end Create_Standard;

   ------------------------------------
   -- Create_Unconstrained_Base_Type --
   ------------------------------------

   procedure Create_Unconstrained_Base_Type
     (E : Entity_Id;
      K : Entity_Kind)
   is
      New_Ent : constant Entity_Id := New_Copy (E);

   begin
      Set_Ekind          (E, K);
      Set_Is_Constrained (E, True);
      Set_Etype          (E, New_Ent);

      Append_Entity (New_Ent, Standard_Standard);
      Set_Is_Constrained (New_Ent, False);
      Set_Etype          (New_Ent, New_Ent);
      Set_Is_Known_Valid (New_Ent, True);

      if K = E_Signed_Integer_Subtype then
         Set_Etype (Low_Bound  (Scalar_Range (E)), New_Ent);
         Set_Etype (High_Bound (Scalar_Range (E)), New_Ent);
      end if;

   end Create_Unconstrained_Base_Type;

   --------------------
   -- Identifier_For --
   --------------------

   function Identifier_For (S : Standard_Entity_Type) return Node_Id is
      Ident_Node : Node_Id;

   begin
      Ident_Node := New_Node (N_Identifier, Stloc);
      Set_Chars (Ident_Node, Chars (Standard_Entity (S)));
      return Ident_Node;
   end Identifier_For;

   --------------------
   -- Make_Component --
   --------------------

   procedure Make_Component
     (Rec : Entity_Id;
      Typ : Entity_Id;
      Nam : String)
   is
      Id : Entity_Id := New_Standard_Entity;

   begin
      Set_Ekind                 (Id, E_Component);
      Set_Etype                 (Id, Typ);
      Set_Scope                 (Id, Rec);
      Init_Component_Location   (Id);

      Set_Original_Record_Component (Id, Id);
      Make_Name (Id, Nam);
      Append_Entity (Id, Rec);
   end Make_Component;

   -----------------
   -- Make_Formal --
   -----------------

   function Make_Formal
     (Typ         : Entity_Id;
      Formal_Name : String)
      return        Entity_Id
   is
      Formal : Entity_Id;

   begin
      Formal := New_Standard_Entity;

      Set_Ekind     (Formal, E_In_Parameter);
      Set_Mechanism (Formal, Default_Mechanism);
      Set_Scope     (Formal, Standard_Standard);
      Set_Etype     (Formal, Typ);
      Make_Name     (Formal, Formal_Name);

      return Formal;
   end Make_Formal;

   ------------------
   -- Make_Integer --
   ------------------

   function Make_Integer (V : Uint) return Node_Id is
      N : constant Node_Id := Make_Integer_Literal (Stloc, V);

   begin
      Set_Is_Static_Expression (N);
      return N;
   end Make_Integer;

   ---------------
   -- Make_Name --
   ---------------

   procedure Make_Name (Id : Entity_Id; Nam : String) is
   begin
      for J in 1 .. Nam'Length loop
         Name_Buffer (J) := Fold_Lower (Nam (Nam'First + (J - 1)));
      end loop;

      Name_Len := Nam'Length;
      Set_Chars (Id, Name_Find);
   end Make_Name;

   ------------------
   -- New_Operator --
   ------------------

   function New_Operator (Op : Name_Id; Typ : Entity_Id) return Entity_Id is
      Ident_Node : Entity_Id;

   begin
      Ident_Node := Make_Defining_Identifier (Stloc, Op);

      Set_Is_Pure    (Ident_Node, True);
      Set_Ekind      (Ident_Node, E_Operator);
      Set_Etype      (Ident_Node, Typ);
      Set_Scope      (Ident_Node, Standard_Standard);
      Set_Homonym    (Ident_Node, Get_Name_Entity_Id (Op));
      Set_Convention (Ident_Node, Convention_Intrinsic);

      Set_Is_Immediately_Visible   (Ident_Node, True);
      Set_Is_Intrinsic_Subprogram  (Ident_Node, True);

      Set_Name_Entity_Id (Op, Ident_Node);
      Append_Entity (Ident_Node, Standard_Standard);
      return Ident_Node;
   end New_Operator;

   -------------------------
   -- New_Standard_Entity --
   -------------------------

   function New_Standard_Entity
     (New_Node_Kind : Node_Kind := N_Defining_Identifier)
      return          Entity_Id
   is
      E : constant Entity_Id := New_Entity (New_Node_Kind, Stloc);

   begin
      --  All standard entities are Pure and Public

      Set_Is_Pure (E);
      Set_Is_Public (E);

      --  All standard entity names are analyzed manually, and are thus
      --  frozen as soon as they are created.

      Set_Is_Frozen (E);

      --  Set debug information required for all standard types

      Set_Needs_Debug_Info (E);

      --  All standard entities are built with fully qualified names, so
      --  set the flag to prevent an abortive attempt at requalification!

      Set_Has_Qualified_Name (E);

      --  Return newly created entity to be completed by caller

      return E;
   end New_Standard_Entity;

   ----------------------
   -- Set_Float_Bounds --
   ----------------------

   procedure Set_Float_Bounds (Id  : Entity_Id) is
      L  : Node_Id;
      --  Low bound of literal value

      H  : Node_Id;
      --  High bound of literal value

      R  : Node_Id;
      --  Range specification

      Digs  : constant Nat := UI_To_Int (Digits_Value (Id));
      --  Digits value, used to select bounds

   begin
      --  Note: for the call from Cstand to initially create the types in
      --  Standard, Vax_Float will always be False. Circuitry in Sem_Vfpt
      --  will adjust these types appropriately in the Vax_Float case if
      --  a pragma Float_Representation (VAX_Float) is used.

      if Vax_Float (Id) then
         if Digs = VAXFF_Digits then
            L := Real_Convert
                   (VAXFF_First'Universal_Literal_String);
            H := Real_Convert
                   (VAXFF_Last'Universal_Literal_String);

         elsif Digs = VAXDF_Digits then
            L := Real_Convert
                   (VAXDF_First'Universal_Literal_String);
            H := Real_Convert
                   (VAXDF_Last'Universal_Literal_String);

         else
            pragma Assert (Digs = VAXGF_Digits);

            L := Real_Convert
                   (VAXGF_First'Universal_Literal_String);
            H := Real_Convert
                   (VAXGF_Last'Universal_Literal_String);
         end if;

      elsif Is_AAMP_Float (Id) then
         if Digs = AAMPS_Digits then
            L := Real_Convert
                   (AAMPS_First'Universal_Literal_String);
            H := Real_Convert
                   (AAMPS_Last'Universal_Literal_String);

         else
            pragma Assert (Digs = AAMPL_Digits);
            L := Real_Convert
                   (AAMPL_First'Universal_Literal_String);
            H := Real_Convert
                   (AAMPL_Last'Universal_Literal_String);
         end if;

      elsif Digs = IEEES_Digits then
         L := Real_Convert
                (IEEES_First'Universal_Literal_String);
         H := Real_Convert
                (IEEES_Last'Universal_Literal_String);

      elsif Digs = IEEEL_Digits then
         L := Real_Convert
                (IEEEL_First'Universal_Literal_String);
         H := Real_Convert
                (IEEEL_Last'Universal_Literal_String);

      else
         pragma Assert (Digs = IEEEX_Digits);

         L := Real_Convert
                (IEEEX_First'Universal_Literal_String);
         H := Real_Convert
                (IEEEX_Last'Universal_Literal_String);
      end if;

      Set_Etype                (L, Id);
      Set_Is_Static_Expression (L);

      Set_Etype                (H, Id);
      Set_Is_Static_Expression (H);

      R := New_Node (N_Range, Stloc);
      Set_Low_Bound  (R, L);
      Set_High_Bound (R, H);
      Set_Includes_Infinities (R, True);
      Set_Scalar_Range (Id, R);
      Set_Etype (R, Id);
      Set_Parent (R, Id);
   end Set_Float_Bounds;

   ------------------------
   -- Set_Integer_Bounds --
   ------------------------

   procedure Set_Integer_Bounds
     (Id  : Entity_Id;
      Typ : Entity_Id;
      Lb  : Uint;
      Hb  : Uint)
   is
      L : Node_Id;     -- Low bound of literal value
      H : Node_Id;     -- High bound of literal value
      R : Node_Id;     -- Range specification

   begin
      L := Make_Integer (Lb);
      H := Make_Integer (Hb);

      Set_Etype (L, Typ);
      Set_Etype (H, Typ);

      R := New_Node (N_Range, Stloc);
      Set_Low_Bound  (R, L);
      Set_High_Bound (R, H);
      Set_Scalar_Range (Id, R);
      Set_Etype (R, Typ);
      Set_Parent (R, Id);
      Set_Is_Unsigned_Type (Id, Lb >= 0);
   end Set_Integer_Bounds;

end CStand;
