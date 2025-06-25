------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               C S T A N D                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2025, Free Software Foundation, Inc.         --
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

with Atree;          use Atree;
with Csets;          use Csets;
with Debug;          use Debug;
with Einfo;          use Einfo;
with Einfo.Entities; use Einfo.Entities;
with Einfo.Utils;    use Einfo.Utils;
with Elists;         use Elists;
with Layout;         use Layout;
with Namet;          use Namet;
with Nlists;         use Nlists;
with Nmake;          use Nmake;
with Opt;            use Opt;
with Output;         use Output;
with Set_Targ;       use Set_Targ;
with Targparm;       use Targparm;
with Tbuild;         use Tbuild;
with Ttypes;         use Ttypes;
with Sem_Mech;       use Sem_Mech;
with Sem_Util;       use Sem_Util;
with Sinfo;          use Sinfo;
with Sinfo.Nodes;    use Sinfo.Nodes;
with Sinfo.Utils;    use Sinfo.Utils;
with Snames;         use Snames;
with Stand;          use Stand;
with Uintp;          use Uintp;
with Urealp;         use Urealp;

package body CStand is

   Stloc  : Source_Ptr renames Standard_Location;
   Staloc : Source_Ptr renames Standard_ASCII_Location;
   --  Standard abbreviations used throughout this package

   Back_End_Float_Types : Elist_Id := No_Elist;
   --  List used for any floating point supported by the back end. This needs
   --  to be at the library level, because the call back procedures retrieving
   --  this information are at that level.

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Build_Float_Type
     (E     : Entity_Id;
      Digs  : Pos;
      Rep   : Float_Rep_Kind;
      Siz   : Int;
      Align : Nat);
   --  Procedure to build standard predefined float base type. The first
   --  parameter is the entity for the type. The second parameter is the
   --  digits value. The third parameter indicates the representation to
   --  be used for the type. The fourth parameter is the size in bits.
   --  The fifth parameter is the alignment in storage units. Each type
   --  is added to the list of predefined floating point types.
   --
   --  Note that both RM_Size and Esize are set to the specified size, i.e.
   --  we do not set the RM_Size to the precision passed by the back end.
   --  This is consistent with the semantics of 'Size specified in the RM
   --  because we cannot pack components of the type tighter than this size.

   procedure Build_Signed_Integer_Type (E : Entity_Id; Siz : Nat);
   --  Procedure to build standard predefined signed integer subtype. The
   --  first parameter is the entity for the subtype. The second parameter
   --  is the size in bits. The corresponding base type is not built by
   --  this routine but instead must be built by the caller where needed.

   procedure Build_Unsigned_Integer_Type (Uns : Entity_Id; Siz : Nat);
   --  Procedure to build standard predefined unsigned integer subtype. These
   --  subtypes are not user visible, but they are used internally. The first
   --  parameter is the entity for the subtype. The second parameter is the
   --  size in bits.

   procedure Copy_Float_Type (To : Entity_Id; From : Entity_Id);
   --  Build a floating point type, copying representation details from From.
   --  This is used to create predefined floating point types based on
   --  available types in the back end.

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

   function Find_Back_End_Float_Type (Name : String) return Entity_Id;
   --  Return the first float type in Back_End_Float_Types with the given name.
   --  Names of entities in back end types, are either type names of C
   --  predefined types (all lower case), or mode names (upper case).
   --  These are not generally valid identifier names.

   function Identifier_For (S : Standard_Entity_Type) return Node_Id;
   --  Returns an identifier node with the same name as the defining identifier
   --  corresponding to the given Standard_Entity_Type value.

   procedure Make_Aliased_Component
     (Rec : Entity_Id;
      Typ : Entity_Id;
      Nam : String);
   --  Build an aliased record component with the given type and name,
   --  and append to the list of components of Rec.

   function Make_Formal (Typ : Entity_Id; Nam : String) return Entity_Id;
   --  Construct entity for subprogram formal with given name and type

   function Make_Integer (V : Uint) return Node_Id;
   --  Builds integer literal with given value

   function New_Operator (Op : Name_Id; Typ : Entity_Id) return Entity_Id;
   --  Build entity for standard operator with given name and type

   function New_Standard_Entity return Entity_Id;
   --  Builds a new entity for Standard

   function New_Standard_Entity (Nam : String) return Entity_Id;
   --  Builds a new entity for Standard with Nkind = N_Defining_Identifier,
   --  and Chars of this defining identifier set to the given string Nam.

   procedure Print_Standard;
   --  Print representation of package Standard if switch set

   procedure Register_Float_Type
     (Name      : String;
      Digs      : Positive;
      Float_Rep : Float_Rep_Kind;
      Precision : Positive;
      Size      : Positive;
      Alignment : Natural);
   --  Registers a single back end floating-point type (from FPT_Mode_Table in
   --  Set_Targ). This will create a predefined floating-point base type for
   --  one of the floating point types reported by the back end, and add it
   --  to the list of predefined float types. Name is the name of the type
   --  as a normal format (non-null-terminated) string. Digs is the number of
   --  digits, which is always non-zero, since non-floating-point types were
   --  filtered out earlier. Float_Rep indicates the kind of floating-point
   --  type, and Precision, Size and Alignment are the precision, size and
   --  alignment in bits.

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

   procedure Build_Float_Type
     (E     : Entity_Id;
      Digs  : Pos;
      Rep   : Float_Rep_Kind;
      Siz   : Int;
      Align : Nat)
   is
   begin
      Set_Type_Definition (Parent (E),
        Make_Floating_Point_Definition (Stloc,
          Digits_Expression => Make_Integer (UI_From_Int (Digs))));

      Mutate_Ekind                   (E, E_Floating_Point_Type);
      Set_Etype                      (E, E);
      Set_Digits_Value               (E, UI_From_Int (Digs));
      Set_Float_Rep                  (E, Rep);
      Init_Size                      (E, Siz);
      Set_Elem_Alignment             (E, Align);
      Set_Float_Bounds               (E);
      Set_Is_Frozen                  (E);
      Set_Is_Public                  (E);
      Set_Size_Known_At_Compile_Time (E);
   end Build_Float_Type;

   ------------------------------
   -- Find_Back_End_Float_Type --
   ------------------------------

   function Find_Back_End_Float_Type (Name : String) return Entity_Id is
      N : Elmt_Id;

   begin
      N := First_Elmt (Back_End_Float_Types);
      while Present (N) and then Get_Name_String (Chars (Node (N))) /= Name
      loop
         Next_Elmt (N);
      end loop;

      return Node (N);
   end Find_Back_End_Float_Type;

   -------------------------------
   -- Build_Signed_Integer_Type --
   -------------------------------

   procedure Build_Signed_Integer_Type (E : Entity_Id; Siz : Nat) is
      U2Siz1 : constant Uint := 2 ** (Siz - 1);
      Lbound : constant Uint := -U2Siz1;
      Ubound : constant Uint := U2Siz1 - 1;

   begin
      Set_Type_Definition (Parent (E),
        Make_Signed_Integer_Type_Definition (Stloc,
          Low_Bound  => Make_Integer (Lbound),
          High_Bound => Make_Integer (Ubound)));

      Mutate_Ekind                   (E, E_Signed_Integer_Type);
      Set_Etype                      (E, E);
      Init_Size                      (E, Siz);
      Set_Elem_Alignment             (E);
      Set_Integer_Bounds             (E, E, Lbound, Ubound);
      Set_Is_Frozen                  (E);
      Set_Is_Public                  (E);
      Set_Is_Known_Valid             (E);
      Set_Size_Known_At_Compile_Time (E);
   end Build_Signed_Integer_Type;

   ---------------------------------
   -- Build_Unsigned_Integer_Type --
   ---------------------------------

   procedure Build_Unsigned_Integer_Type
     (Uns : Entity_Id;
      Siz : Nat)
   is
      Decl   : constant Node_Id := New_Node (N_Full_Type_Declaration, Stloc);
      R_Node : constant Node_Id := New_Node (N_Range, Stloc);

   begin
      Set_Defining_Identifier (Decl, Uns);

      Mutate_Ekind                   (Uns, E_Modular_Integer_Type);
      Set_Scope                      (Uns, Standard_Standard);
      Set_Etype                      (Uns, Uns);
      Init_Size                      (Uns, Siz);
      Set_Elem_Alignment             (Uns);
      Set_Modulus                    (Uns, Uint_2 ** Siz);
      Set_Is_Unsigned_Type           (Uns);
      Set_Size_Known_At_Compile_Time (Uns);
      Set_Is_Known_Valid             (Uns, True);

      Set_Low_Bound  (R_Node, Make_Integer (Uint_0));
      Set_High_Bound (R_Node, Make_Integer (Modulus (Uns) - 1));
      Set_Etype (Low_Bound  (R_Node), Uns);
      Set_Etype (High_Bound (R_Node), Uns);
      Set_Scalar_Range (Uns, R_Node);
   end Build_Unsigned_Integer_Type;

   ---------------------
   -- Copy_Float_Type --
   ---------------------

   procedure Copy_Float_Type (To : Entity_Id; From : Entity_Id) is
   begin
      Build_Float_Type
        (To, UI_To_Int (Digits_Value (From)), Float_Rep (From),
         UI_To_Int (Esize (From)), UI_To_Int (Alignment (From)));
   end Copy_Float_Type;

   ----------------------
   -- Create_Operators --
   ----------------------

   --  Each operator has an abbreviated signature. The formals have the names
   --  LEFT and RIGHT. Their types are not actually used for resolution.

   procedure Create_Operators is
      Op_Node : Entity_Id;

      --  The following tables define the binary and unary operators and their
      --  corresponding result type.

      Binary_Ops : constant array (S_Binary_Ops) of Name_Id :=

         --  There is one entry here for each binary operator, except for the
         --  case of concatenation, where there are three entries, one for a
         --  String result, one for Wide_String, and one for Wide_Wide_String.

        (Name_Op_Add,
         Name_Op_And,
         Name_Op_Concat,
         Name_Op_Concat,
         Name_Op_Concat,
         Name_Op_Divide,
         Name_Op_Eq,
         Name_Op_Expon,
         Name_Op_Ge,
         Name_Op_Gt,
         Name_Op_Le,
         Name_Op_Lt,
         Name_Op_Mod,
         Name_Op_Multiply,
         Name_Op_Ne,
         Name_Op_Or,
         Name_Op_Rem,
         Name_Op_Subtract,
         Name_Op_Xor);

      Bin_Op_Types : constant array (S_Binary_Ops) of Entity_Id :=

         --  This table has the corresponding result types. The entries are
         --  ordered so they correspond to the Binary_Ops array above.

        (Universal_Integer,         -- Add
         Standard_Boolean,          -- And
         Standard_String,           -- Concat (String)
         Standard_Wide_String,      -- Concat (Wide_String)
         Standard_Wide_Wide_String, -- Concat (Wide_Wide_String)
         Universal_Integer,         -- Divide
         Standard_Boolean,          -- Eq
         Universal_Integer,         -- Expon
         Standard_Boolean,          -- Ge
         Standard_Boolean,          -- Gt
         Standard_Boolean,          -- Le
         Standard_Boolean,          -- Lt
         Universal_Integer,         -- Mod
         Universal_Integer,         -- Multiply
         Standard_Boolean,          -- Ne
         Standard_Boolean,          -- Or
         Universal_Integer,         -- Rem
         Universal_Integer,         -- Subtract
         Standard_Boolean);         -- Xor

      Unary_Ops : constant array (S_Unary_Ops) of Name_Id :=

         --  There is one entry here for each unary operator

        (Name_Op_Abs,
         Name_Op_Subtract,
         Name_Op_Not,
         Name_Op_Add);

      Unary_Op_Types : constant array (S_Unary_Ops) of Entity_Id :=

         --  This table has the corresponding result types. The entries are
         --  ordered so they correspond to the Unary_Ops array above.

        (Universal_Integer,     -- Abs
         Universal_Integer,     -- Subtract
         Standard_Boolean,      -- Not
         Universal_Integer);    -- Add

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
      --  of the formals for string, wide [wide]_string, concatenations.

      Set_Etype (First_Entity (Standard_Op_Concat),  Standard_String);
      Set_Etype (Last_Entity  (Standard_Op_Concat),  Standard_String);

      Set_Etype (First_Entity (Standard_Op_Concatw), Standard_Wide_String);
      Set_Etype (Last_Entity  (Standard_Op_Concatw), Standard_Wide_String);

      Set_Etype (First_Entity (Standard_Op_Concatww),
                 Standard_Wide_Wide_String);

      Set_Etype (Last_Entity (Standard_Op_Concatww),
                 Standard_Wide_Wide_String);
   end Create_Operators;

   ---------------------
   -- Create_Standard --
   ---------------------

   --  The tree for the package Standard is prefixed to all compilations.
   --  Several entities required by semantic analysis are denoted by global
   --  variables that are initialized to point to the corresponding occurrences
   --  in Standard. The visible entities of Standard are created here. Special
   --  entities maybe created here as well or may be created from the semantics
   --  module. By not adding them to the Decls list of Standard they will not
   --  be visible to Ada programs.

   procedure Create_Standard is
      Decl_S : constant List_Id := New_List;
      --  List of declarations in Standard

      Decl_A : constant List_Id := New_List;
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

      procedure Create_Back_End_Float_Types;
      --  Initialize the Back_End_Float_Types list by having the back end
      --  enumerate all available types and building type entities for them.

      procedure Create_Float_Types;
      --  Creates entities for all predefined floating point types, and
      --  adds these to the Predefined_Float_Types list in package Standard.

      procedure Make_Dummy_Index (E : Entity_Id);
      --  Called to provide a dummy index field value for Any_Array/Any_String

      procedure Pack_String_Type (String_Type : Entity_Id);
      --  Generate proper tree for pragma Pack that applies to given type, and
      --  mark type as having the pragma.

      ---------------------
      -- Build_Exception --
      ---------------------

      procedure Build_Exception (S : Standard_Entity_Type) is
      begin
         Mutate_Ekind  (Standard_Entity (S), E_Exception);
         Set_Etype     (Standard_Entity (S), Standard_Exception_Type);
         Set_Is_Public (Standard_Entity (S), True);

         Decl :=
           Make_Exception_Declaration (Stloc,
             Defining_Identifier => Standard_Entity (S));
         Append (Decl, Decl_S);
      end Build_Exception;

      ---------------------------------
      -- Create_Back_End_Float_Types --
      ---------------------------------

      procedure Create_Back_End_Float_Types is
      begin
         for J in 1 .. Num_FPT_Modes loop
            declare
               E : FPT_Mode_Entry renames FPT_Mode_Table (J);
            begin
               Register_Float_Type
                 (E.NAME.all, E.DIGS, E.FLOAT_REP, E.PRECISION, E.SIZE,
                  E.ALIGNMENT);
            end;
         end loop;
      end Create_Back_End_Float_Types;

      ------------------------
      -- Create_Float_Types --
      ------------------------

      procedure Create_Float_Types is
      begin
         --  Create type definition nodes for predefined float types

         Copy_Float_Type
           (Standard_Short_Float,
            Find_Back_End_Float_Type (C_Type_For (S_Short_Float)));
         Set_Is_Implementation_Defined (Standard_Short_Float);

         Copy_Float_Type (Standard_Float, Standard_Short_Float);

         Copy_Float_Type
           (Standard_Long_Float,
            Find_Back_End_Float_Type (C_Type_For (S_Long_Float)));

         Copy_Float_Type
           (Standard_Long_Long_Float,
            Find_Back_End_Float_Type (C_Type_For (S_Long_Long_Float)));
         Set_Is_Implementation_Defined (Standard_Long_Long_Float);

         Predefined_Float_Types := New_Elmt_List;

         Append_Elmt (Standard_Short_Float, Predefined_Float_Types);
         Append_Elmt (Standard_Float, Predefined_Float_Types);
         Append_Elmt (Standard_Long_Float, Predefined_Float_Types);
         Append_Elmt (Standard_Long_Long_Float, Predefined_Float_Types);

         --  Any other back end types are appended at the end of the list of
         --  predefined float types, and will only be selected if the none of
         --  the types in Standard is suitable, or if a specific named type is
         --  requested through a pragma Import.

         while not Is_Empty_Elmt_List (Back_End_Float_Types) loop
            declare
               E : constant Elmt_Id := First_Elmt (Back_End_Float_Types);
            begin
               Append_Elmt (Node (E), To => Predefined_Float_Types);
               Remove_Elmt (Back_End_Float_Types, E);
            end;
         end loop;
      end Create_Float_Types;

      ----------------------
      -- Make_Dummy_Index --
      ----------------------

      procedure Make_Dummy_Index (E : Entity_Id) is
         Index : constant Node_Id :=
           Make_Range (Sloc (E),
             Low_Bound  => Make_Integer (Uint_0),
             High_Bound => Make_Integer (Uint_2 ** Standard_Integer_Size));

         --  Make sure Index is a list as required, so Next_Index is Empty

         Dummy : constant List_Id := New_List (Index);

      begin
         Set_Etype (Index, Standard_Integer);
         Set_First_Index (E, Index);
      end Make_Dummy_Index;

      ----------------------
      -- Pack_String_Type --
      ----------------------

      procedure Pack_String_Type (String_Type : Entity_Id) is
         Prag : constant Node_Id :=
           Make_Pragma (Stloc,
             Chars                        => Name_Pack,
             Pragma_Argument_Associations =>
               New_List (
                 Make_Pragma_Argument_Association (Stloc,
                   Expression => New_Occurrence_Of (String_Type, Stloc))));

      begin
         Append (Prag, Decl_S);
         Record_Rep_Item (String_Type, Prag);
         Set_Has_Pragma_Pack (String_Type, True);
      end Pack_String_Type;

      Char_Size : constant Unat := UI_From_Int (Standard_Character_Size);

   --  Start of processing for Create_Standard

   begin
      --  First step is to create defining identifiers for each entity

      for S in Standard_Entity_Type loop
         declare
            S_Name : constant String := Standard_Entity_Type'Image (S);
            --  Name of entity (note we skip S_ at the start)

            Ident_Node : Node_Id;
            --  Defining identifier node

         begin
            Ident_Node := New_Standard_Entity (S_Name (3 .. S_Name'Length));
            Standard_Entity (S) := Ident_Node;
         end;
      end loop;

      --  Create package declaration node for package Standard

      Standard_Package_Node := New_Node (N_Package_Declaration, Stloc);

      Pspec := New_Node (N_Package_Specification, Stloc);
      Set_Specification (Standard_Package_Node, Pspec);

      Set_Defining_Unit_Name (Pspec, Standard_Standard);
      Set_Visible_Declarations (Pspec, Decl_S);

      Mutate_Ekind (Standard_Standard, E_Package);
      Set_Is_Pure (Standard_Standard);
      Set_Is_Compilation_Unit (Standard_Standard);

      --  Create type declaration nodes for standard types

      for S in S_Types loop
         if S not in S_Natural | S_Positive then
            Decl := New_Node (N_Full_Type_Declaration, Stloc);
            Set_Defining_Identifier (Decl, Standard_Entity (S));
            Append (Decl, Decl_S);
         end if;
      end loop;

      Create_Back_End_Float_Types;

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

      Mutate_Ekind       (Standard_Boolean, E_Enumeration_Type);
      Set_First_Literal  (Standard_Boolean, Standard_False);
      Set_Etype          (Standard_Boolean, Standard_Boolean);
      Set_Esize          (Standard_Boolean, Char_Size);
      Set_RM_Size        (Standard_Boolean, Uint_1);
      Set_Elem_Alignment (Standard_Boolean);

      Set_Is_Unsigned_Type           (Standard_Boolean);
      Set_Size_Known_At_Compile_Time (Standard_Boolean);
      Set_Has_Pragma_Ordered         (Standard_Boolean);

      Mutate_Ekind        (Standard_True, E_Enumeration_Literal);
      Set_Etype           (Standard_True, Standard_Boolean);
      Set_Enumeration_Pos (Standard_True, Uint_1);
      Set_Enumeration_Rep (Standard_True, Uint_1);
      Set_Is_Known_Valid  (Standard_True, True);

      Mutate_Ekind        (Standard_False, E_Enumeration_Literal);
      Set_Etype           (Standard_False, Standard_Boolean);
      Set_Enumeration_Pos (Standard_False, Uint_0);
      Set_Enumeration_Rep (Standard_False, Uint_0);
      Set_Is_Known_Valid  (Standard_False, True);

      --  For the bounds of Boolean, we create a range node corresponding to

      --    range False .. True

      --  where the occurrences of the literals must point to the
      --  corresponding definition.

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

      --  Record entity identifiers for boolean literals in the
      --  Boolean_Literals array, for easy reference during expansion.

      Boolean_Literals := (False => Standard_False, True => Standard_True);

      --  Create type definition nodes for predefined integer types

      Build_Signed_Integer_Type
        (Standard_Short_Short_Integer, Standard_Short_Short_Integer_Size);
      Set_Is_Implementation_Defined (Standard_Short_Short_Integer);

      Build_Signed_Integer_Type
        (Standard_Short_Integer, Standard_Short_Integer_Size);
      Set_Is_Implementation_Defined (Standard_Short_Integer);

      Build_Signed_Integer_Type
        (Standard_Integer, Standard_Integer_Size);

      Build_Signed_Integer_Type
        (Standard_Long_Integer, Standard_Long_Integer_Size);

      Build_Signed_Integer_Type
        (Standard_Long_Long_Integer, Standard_Long_Long_Integer_Size);
      Set_Is_Implementation_Defined (Standard_Long_Long_Integer);

      Build_Signed_Integer_Type
        (Standard_Long_Long_Long_Integer,
         Standard_Long_Long_Long_Integer_Size);
      Set_Is_Implementation_Defined (Standard_Long_Long_Long_Integer);

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

      Create_Unconstrained_Base_Type
        (Standard_Long_Long_Long_Integer, E_Signed_Integer_Subtype);

      Create_Float_Types;

      --  Create type definition node for type Character. Note that we do not
      --  set the Literals field, since type Character is handled with special
      --  routine that do not need a literal list.

      Tdef_Node := New_Node (N_Enumeration_Type_Definition, Stloc);
      Set_Type_Definition (Parent (Standard_Character), Tdef_Node);

      Mutate_Ekind       (Standard_Character, E_Enumeration_Type);
      Set_Etype          (Standard_Character, Standard_Character);
      Set_Esize          (Standard_Character, Char_Size);
      Set_RM_Size        (Standard_Character, Uint_8);
      Set_Elem_Alignment (Standard_Character);

      Set_Has_Pragma_Ordered         (Standard_Character);
      Set_Is_Unsigned_Type           (Standard_Character);
      Set_Is_Character_Type          (Standard_Character);
      Set_Is_Known_Valid             (Standard_Character);
      Set_Size_Known_At_Compile_Time (Standard_Character);

      --  Create the bounds for type Character

      R_Node := New_Node (N_Range, Stloc);

      --  Low bound for type Character (Standard.Nul)

      B_Node := New_Node (N_Character_Literal, Stloc);
      Set_Is_Static_Expression (B_Node);
      Set_Chars                (B_Node, No_Name);
      Set_Char_Literal_Value   (B_Node, Uint_0);
      Set_Entity               (B_Node, Empty);
      Set_Etype                (B_Node, Standard_Character);
      Set_Low_Bound (R_Node, B_Node);

      --  High bound for type Character

      B_Node := New_Node (N_Character_Literal, Stloc);
      Set_Is_Static_Expression (B_Node);
      Set_Chars                (B_Node, No_Name);
      Set_Char_Literal_Value   (B_Node, UI_From_CC (16#FF#));
      Set_Entity               (B_Node, Empty);
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

      Mutate_Ekind   (Standard_Wide_Character, E_Enumeration_Type);
      Set_Etype      (Standard_Wide_Character, Standard_Wide_Character);
      Init_Size      (Standard_Wide_Character, Standard_Wide_Character_Size);

      Set_Elem_Alignment             (Standard_Wide_Character);
      Set_Has_Pragma_Ordered         (Standard_Wide_Character);
      Set_Is_Unsigned_Type           (Standard_Wide_Character);
      Set_Is_Character_Type          (Standard_Wide_Character);
      Set_Is_Known_Valid             (Standard_Wide_Character);
      Set_Size_Known_At_Compile_Time (Standard_Wide_Character);

      --  Create the bounds for type Wide_Character

      R_Node := New_Node (N_Range, Stloc);

      --  Low bound for type Wide_Character

      B_Node := New_Node (N_Character_Literal, Stloc);
      Set_Is_Static_Expression (B_Node);
      Set_Chars                (B_Node, No_Name);
      Set_Char_Literal_Value   (B_Node, Uint_0);
      Set_Entity               (B_Node, Empty);
      Set_Etype                (B_Node, Standard_Wide_Character);
      Set_Low_Bound (R_Node, B_Node);

      --  High bound for type Wide_Character

      B_Node := New_Node (N_Character_Literal, Stloc);
      Set_Is_Static_Expression (B_Node);
      Set_Chars                (B_Node, No_Name);
      Set_Char_Literal_Value   (B_Node, UI_From_CC (16#FFFF#));
      Set_Entity               (B_Node, Empty);
      Set_Etype                (B_Node, Standard_Wide_Character);
      Set_High_Bound           (R_Node, B_Node);

      Set_Scalar_Range (Standard_Wide_Character, R_Node);
      Set_Etype (R_Node, Standard_Wide_Character);
      Set_Parent (R_Node, Standard_Wide_Character);

      --  Create type definition for type Wide_Wide_Character. Note that we
      --  do not set the Literals field, since type Wide_Wide_Character is
      --  handled with special routines that do not need a literal list.

      Tdef_Node := New_Node (N_Enumeration_Type_Definition, Stloc);
      Set_Type_Definition (Parent (Standard_Wide_Wide_Character), Tdef_Node);

      Mutate_Ekind (Standard_Wide_Wide_Character, E_Enumeration_Type);
      Set_Etype (Standard_Wide_Wide_Character,
                 Standard_Wide_Wide_Character);
      Init_Size (Standard_Wide_Wide_Character,
                 Standard_Wide_Wide_Character_Size);

      Set_Elem_Alignment             (Standard_Wide_Wide_Character);
      Set_Has_Pragma_Ordered         (Standard_Wide_Wide_Character);
      Set_Is_Unsigned_Type           (Standard_Wide_Wide_Character);
      Set_Is_Character_Type          (Standard_Wide_Wide_Character);
      Set_Is_Known_Valid             (Standard_Wide_Wide_Character);
      Set_Size_Known_At_Compile_Time (Standard_Wide_Wide_Character);
      Set_Is_Ada_2005_Only           (Standard_Wide_Wide_Character);

      --  Create the bounds for type Wide_Wide_Character

      R_Node := New_Node (N_Range, Stloc);

      --  Low bound for type Wide_Wide_Character

      B_Node := New_Node (N_Character_Literal, Stloc);
      Set_Is_Static_Expression (B_Node);
      Set_Chars                (B_Node, No_Name);
      Set_Char_Literal_Value   (B_Node, Uint_0);
      Set_Entity               (B_Node, Empty);
      Set_Etype                (B_Node, Standard_Wide_Wide_Character);
      Set_Low_Bound (R_Node, B_Node);

      --  High bound for type Wide_Wide_Character

      B_Node := New_Node (N_Character_Literal, Stloc);
      Set_Is_Static_Expression (B_Node);
      Set_Chars                (B_Node, No_Name);
      Set_Char_Literal_Value   (B_Node, UI_From_CC (16#7FFF_FFFF#));
      Set_Entity               (B_Node, Empty);
      Set_Etype                (B_Node, Standard_Wide_Wide_Character);
      Set_High_Bound           (R_Node, B_Node);

      Set_Scalar_Range (Standard_Wide_Wide_Character, R_Node);
      Set_Etype (R_Node, Standard_Wide_Wide_Character);
      Set_Parent (R_Node, Standard_Wide_Wide_Character);

      --  Create type definition node for type String

      Tdef_Node := New_Node (N_Unconstrained_Array_Definition, Stloc);

      declare
         CompDef_Node : Node_Id;
      begin
         CompDef_Node := New_Node (N_Component_Definition, Stloc);
         Set_Aliased_Present      (CompDef_Node, False);
         Set_Access_Definition    (CompDef_Node, Empty);
         Set_Subtype_Indication   (CompDef_Node, Identifier_For (S_Character));
         Set_Component_Definition (Tdef_Node, CompDef_Node);
      end;

      Set_Subtype_Marks      (Tdef_Node, New_List);
      Append (Identifier_For (S_Positive), Subtype_Marks (Tdef_Node));
      Set_Type_Definition (Parent (Standard_String), Tdef_Node);

      Mutate_Ekind        (Standard_String, E_Array_Type);
      Set_Etype           (Standard_String, Standard_String);
      Set_Component_Type  (Standard_String, Standard_Character);
      Set_Component_Size  (Standard_String, Uint_8);
      Reinit_Size_Align   (Standard_String);
      Set_Alignment       (Standard_String, Uint_1);
      Pack_String_Type    (Standard_String);

      --  On targets where a storage unit is larger than a byte, pragma Pack
      --  has a real effect on the representation of type String, and the type
      --  must be marked as having a nonstandard representation.

      if System_Storage_Unit > Uint_8 then
         Set_Has_Non_Standard_Rep (Standard_String);
         Set_Has_Pragma_Pack      (Standard_String);
      end if;

      --  Set index type of String

      E_Id :=
        First (Subtype_Marks (Type_Definition (Parent (Standard_String))));
      Set_First_Index (Standard_String, E_Id);
      Set_Entity (E_Id, Standard_Positive);
      Set_Etype (E_Id, Standard_Positive);

      --  Create type definition node for type Wide_String

      Tdef_Node := New_Node (N_Unconstrained_Array_Definition, Stloc);

      declare
         CompDef_Node : Node_Id;
      begin
         CompDef_Node := New_Node (N_Component_Definition, Stloc);
         Set_Aliased_Present    (CompDef_Node, False);
         Set_Access_Definition  (CompDef_Node, Empty);
         Set_Subtype_Indication (CompDef_Node,
                                 Identifier_For (S_Wide_Character));
         Set_Component_Definition (Tdef_Node, CompDef_Node);
      end;

      Set_Subtype_Marks (Tdef_Node, New_List);
      Append (Identifier_For (S_Positive), Subtype_Marks (Tdef_Node));
      Set_Type_Definition (Parent (Standard_Wide_String), Tdef_Node);

      Mutate_Ekind        (Standard_Wide_String, E_Array_Type);
      Set_Etype           (Standard_Wide_String, Standard_Wide_String);
      Set_Component_Type  (Standard_Wide_String, Standard_Wide_Character);
      Set_Component_Size  (Standard_Wide_String, Uint_16);
      Reinit_Size_Align   (Standard_Wide_String);
      Pack_String_Type    (Standard_Wide_String);

      --  Set index type of Wide_String

      E_Id :=
        First
          (Subtype_Marks (Type_Definition (Parent (Standard_Wide_String))));
      Set_First_Index (Standard_Wide_String, E_Id);
      Set_Entity (E_Id, Standard_Positive);
      Set_Etype (E_Id, Standard_Positive);

      --  Create type definition node for type Wide_Wide_String

      Tdef_Node := New_Node (N_Unconstrained_Array_Definition, Stloc);

      declare
         CompDef_Node : Node_Id;
      begin
         CompDef_Node := New_Node (N_Component_Definition, Stloc);
         Set_Aliased_Present    (CompDef_Node, False);
         Set_Access_Definition  (CompDef_Node, Empty);
         Set_Subtype_Indication (CompDef_Node,
                                 Identifier_For (S_Wide_Wide_Character));
         Set_Component_Definition (Tdef_Node, CompDef_Node);
      end;

      Set_Subtype_Marks (Tdef_Node, New_List);
      Append (Identifier_For (S_Positive), Subtype_Marks (Tdef_Node));
      Set_Type_Definition (Parent (Standard_Wide_Wide_String), Tdef_Node);

      Mutate_Ekind         (Standard_Wide_Wide_String, E_Array_Type);
      Set_Etype            (Standard_Wide_Wide_String,
                            Standard_Wide_Wide_String);
      Set_Component_Type   (Standard_Wide_Wide_String,
                            Standard_Wide_Wide_Character);
      Set_Component_Size   (Standard_Wide_Wide_String, Uint_32);
      Reinit_Size_Align    (Standard_Wide_Wide_String);
      Set_Is_Ada_2005_Only (Standard_Wide_Wide_String);
      Pack_String_Type     (Standard_Wide_Wide_String);

      --  Set index type of Wide_Wide_String

      E_Id :=
        First
         (Subtype_Marks
            (Type_Definition (Parent (Standard_Wide_Wide_String))));
      Set_First_Index (Standard_Wide_Wide_String, E_Id);
      Set_Entity (E_Id, Standard_Positive);
      Set_Etype (E_Id, Standard_Positive);

      --  Setup entity for Natural

      Mutate_Ekind (Standard_Natural, E_Signed_Integer_Subtype);
      Set_Etype (Standard_Natural, Base_Type (Standard_Integer));
      Set_Esize (Standard_Natural, UI_From_Int (Standard_Integer_Size));
      Set_RM_Size  (Standard_Natural, UI_From_Int (Standard_Integer_Size - 1));
      Set_Elem_Alignment (Standard_Natural);
      Set_Size_Known_At_Compile_Time
                         (Standard_Natural);
      Set_Integer_Bounds (Standard_Natural,
        Typ => Base_Type (Standard_Integer),
        Lb  => Uint_0,
        Hb  => Intval (High_Bound (Scalar_Range (Standard_Integer))));
      Set_Is_Constrained (Standard_Natural);

      Append_To
        (Decl_S,
         Make_Subtype_Declaration
           (Stloc,
            Standard_Natural,
            Subtype_Indication =>
              New_Occurrence_Of (Standard_Integer, Stloc)));

      --  Setup entity for Positive

      Mutate_Ekind (Standard_Positive, E_Signed_Integer_Subtype);
      Set_Etype (Standard_Positive, Base_Type (Standard_Integer));
      Set_Esize  (Standard_Positive, UI_From_Int (Standard_Integer_Size));
      Set_RM_Size
        (Standard_Positive, UI_From_Int (Standard_Integer_Size - 1));
      Set_Elem_Alignment (Standard_Positive);

      Set_Size_Known_At_Compile_Time (Standard_Positive);

      Set_Integer_Bounds   (Standard_Positive,
         Typ => Base_Type (Standard_Integer),
         Lb  => Uint_1,
         Hb  => Intval (High_Bound (Scalar_Range (Standard_Integer))));
      Set_Is_Constrained   (Standard_Positive);

      Append_To
        (Decl_S,
         Make_Subtype_Declaration
           (Stloc,
            Standard_Positive,
            Subtype_Indication =>
              New_Occurrence_Of (Standard_Integer, Stloc)));

      --  Create declaration for package ASCII

      Decl := New_Node (N_Package_Declaration, Stloc);
      Append (Decl, Decl_S);

      Pspec := New_Node (N_Package_Specification, Stloc);
      Set_Specification (Decl, Pspec);

      Set_Defining_Unit_Name (Pspec, Standard_Entity (S_ASCII));
      Mutate_Ekind (Standard_Entity (S_ASCII), E_Package);
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
            A_Char    : constant Entity_Id := Standard_Entity (S);
            Expr_Decl : Node_Id;

         begin
            Set_Sloc                   (A_Char, Staloc);
            Mutate_Ekind               (A_Char, E_Constant);
            Set_Never_Set_In_Source    (A_Char, True);
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
            Set_Char_Literal_Value   (Expr_Decl, UI_From_CC (Ccode));
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

      Standard_Void_Type := New_Standard_Entity ("_void_type");
      pragma Assert (Ekind (Standard_Void_Type) = E_Void); -- it's the default
      Set_Etype       (Standard_Void_Type, Standard_Void_Type);
      Set_Scope       (Standard_Void_Type, Standard_Standard);

      --  The type field of packages is set to void

      Set_Etype (Standard_Standard, Standard_Void_Type);
      Set_Etype (Standard_ASCII, Standard_Void_Type);

      --  Standard_A_String is actually used in generated code, so it has a
      --  type name that is reasonable, but does not overlap any Ada name.

      Standard_A_String := New_Standard_Entity ("access_string");
      Mutate_Ekind   (Standard_A_String, E_Access_Type);
      Set_Scope      (Standard_A_String, Standard_Standard);
      Set_Etype      (Standard_A_String, Standard_A_String);

      if Debug_Flag_6 then
         Init_Size   (Standard_A_String, System_Address_Size);
      else
         Init_Size   (Standard_A_String, System_Address_Size * 2);
      end if;

      pragma Assert (not Known_Alignment (Standard_A_String));

      Set_Directly_Designated_Type
                     (Standard_A_String, Standard_String);

      Standard_A_Char := New_Standard_Entity ("access_character");
      Mutate_Ekind       (Standard_A_Char, E_Access_Type);
      Set_Scope          (Standard_A_Char, Standard_Standard);
      Set_Etype          (Standard_A_Char, Standard_A_String);
      Init_Size          (Standard_A_Char, System_Address_Size);
      Set_Elem_Alignment (Standard_A_Char);

      Set_Directly_Designated_Type (Standard_A_Char, Standard_Character);

      --  Standard_Debug_Renaming_Type is used for the special objects created
      --  to encode the names occurring in renaming declarations for use by the
      --  debugger (see exp_dbug.adb). The type is a zero-sized subtype of
      --  Standard.Integer.

      Standard_Debug_Renaming_Type := New_Standard_Entity ("_renaming_type");

      Mutate_Ekind (Standard_Debug_Renaming_Type, E_Signed_Integer_Subtype);
      Set_Scope (Standard_Debug_Renaming_Type, Standard_Standard);
      Set_Etype (Standard_Debug_Renaming_Type, Base_Type (Standard_Integer));
      Set_Esize (Standard_Debug_Renaming_Type, Uint_0);
      Set_RM_Size (Standard_Debug_Renaming_Type, Uint_0);
      Set_Size_Known_At_Compile_Time (Standard_Debug_Renaming_Type);
      Set_Integer_Bounds (Standard_Debug_Renaming_Type,
        Typ => Base_Type (Standard_Debug_Renaming_Type),
        Lb  => Uint_1,
        Hb  => Uint_0);
      Set_Is_Constrained (Standard_Debug_Renaming_Type);
      Set_Has_Size_Clause (Standard_Debug_Renaming_Type);

      --  Note on type names. The type names for the following special types
      --  are constructed so that they will look reasonable should they ever
      --  appear in error messages etc, although in practice the use of the
      --  special insertion character } for types results in special handling
      --  of these type names in any case. The blanks in these names would
      --  trouble in Gigi, but that's OK here, since none of these types
      --  should ever get through to Gigi. Attributes of these types are
      --  filled out to minimize problems with cascaded errors (for example,
      --  Any_Integer is given reasonable and consistent type and size values)

      Any_Type := New_Standard_Entity ("any type");
      Decl     := New_Node (N_Full_Type_Declaration, Stloc);
      Set_Defining_Identifier (Decl, Any_Type);
      Set_Scope (Any_Type, Standard_Standard);
      Build_Signed_Integer_Type (Any_Type, Standard_Integer_Size);

      Any_Id := New_Standard_Entity ("any id");
      Mutate_Ekind          (Any_Id, E_Variable);
      Set_Scope             (Any_Id, Standard_Standard);
      Set_Etype             (Any_Id, Any_Type);
      pragma Assert (not Known_Esize (Any_Id));
      pragma Assert (not Known_Alignment (Any_Id));

      Any_Character := New_Standard_Entity ("a character type");
      Mutate_Ekind          (Any_Character, E_Enumeration_Type);
      Set_Scope             (Any_Character, Standard_Standard);
      Set_Etype             (Any_Character, Any_Character);
      Set_Is_Unsigned_Type  (Any_Character);
      Set_Is_Character_Type (Any_Character);
      Set_Esize             (Any_Character, Char_Size);
      Set_RM_Size           (Any_Character, Uint_8);
      Set_Elem_Alignment    (Any_Character);
      Set_Scalar_Range      (Any_Character, Scalar_Range (Standard_Character));

      Any_Array := New_Standard_Entity ("an array type");
      Mutate_Ekind          (Any_Array, E_Array_Type);
      Set_Scope             (Any_Array, Standard_Standard);
      Set_Etype             (Any_Array, Any_Array);
      Set_Component_Type    (Any_Array, Any_Character);
      Reinit_Size_Align     (Any_Array);
      Make_Dummy_Index      (Any_Array);

      Any_Boolean := New_Standard_Entity ("a boolean type");
      Mutate_Ekind          (Any_Boolean, E_Enumeration_Type);
      Set_Scope             (Any_Boolean, Standard_Standard);
      Set_Etype             (Any_Boolean, Standard_Boolean);
      Set_Esize             (Any_Boolean, Char_Size);
      Set_RM_Size           (Any_Boolean, Uint_1);
      Set_Elem_Alignment    (Any_Boolean);
      Set_Is_Unsigned_Type  (Any_Boolean);
      Set_Scalar_Range      (Any_Boolean, Scalar_Range (Standard_Boolean));

      Any_Composite := New_Standard_Entity ("a composite type");
      Mutate_Ekind          (Any_Composite, E_Array_Type);
      Set_Scope             (Any_Composite, Standard_Standard);
      Set_Etype             (Any_Composite, Any_Composite);
      Set_Component_Type    (Any_Composite, Standard_Integer);
      Reinit_Size_Align     (Any_Composite);

      pragma Assert (not Known_Component_Size (Any_Composite));

      Any_Discrete := New_Standard_Entity ("a discrete type");
      Mutate_Ekind          (Any_Discrete, E_Signed_Integer_Type);
      Set_Scope             (Any_Discrete, Standard_Standard);
      Set_Etype             (Any_Discrete, Any_Discrete);
      Init_Size             (Any_Discrete, Standard_Integer_Size);
      Set_Elem_Alignment    (Any_Discrete);

      Any_Fixed := New_Standard_Entity ("a fixed-point type");
      Mutate_Ekind          (Any_Fixed, E_Ordinary_Fixed_Point_Type);
      Set_Scope             (Any_Fixed, Standard_Standard);
      Set_Etype             (Any_Fixed, Any_Fixed);
      Init_Size             (Any_Fixed, Standard_Integer_Size);
      Set_Elem_Alignment    (Any_Fixed);

      Any_Integer := New_Standard_Entity ("an integer type");
      Mutate_Ekind         (Any_Integer, E_Signed_Integer_Type);
      Set_Scope            (Any_Integer, Standard_Standard);
      Set_Etype            (Any_Integer, Standard_Long_Long_Long_Integer);
      Init_Size            (Any_Integer, Standard_Long_Long_Long_Integer_Size);
      Set_Elem_Alignment   (Any_Integer);

      Set_Integer_Bounds
        (Any_Integer,
         Typ => Base_Type (Standard_Integer),
         Lb  => Uint_0,
         Hb  => Intval (High_Bound (Scalar_Range (Standard_Integer))));

      Any_Modular := New_Standard_Entity ("a modular type");
      Mutate_Ekind         (Any_Modular, E_Modular_Integer_Type);
      Set_Scope            (Any_Modular, Standard_Standard);
      Set_Etype            (Any_Modular, Standard_Long_Long_Long_Integer);
      Init_Size            (Any_Modular, Standard_Long_Long_Long_Integer_Size);
      Set_Elem_Alignment   (Any_Modular);
      Set_Is_Unsigned_Type (Any_Modular);

      Any_Numeric := New_Standard_Entity ("a numeric type");
      Mutate_Ekind         (Any_Numeric, E_Signed_Integer_Type);
      Set_Scope            (Any_Numeric, Standard_Standard);
      Set_Etype            (Any_Numeric, Standard_Long_Long_Long_Integer);
      Init_Size            (Any_Numeric, Standard_Long_Long_Long_Integer_Size);
      Set_Elem_Alignment   (Any_Numeric);

      Any_Real := New_Standard_Entity ("a real type");
      Mutate_Ekind          (Any_Real, E_Floating_Point_Type);
      Set_Scope             (Any_Real, Standard_Standard);
      Set_Etype             (Any_Real, Standard_Long_Long_Float);
      Init_Size             (Any_Real,
        UI_To_Int (Esize (Standard_Long_Long_Float)));
      Set_Elem_Alignment    (Any_Real);

      Any_Scalar := New_Standard_Entity ("a scalar type");
      Mutate_Ekind          (Any_Scalar, E_Signed_Integer_Type);
      Set_Scope             (Any_Scalar, Standard_Standard);
      Set_Etype             (Any_Scalar, Any_Scalar);
      Init_Size             (Any_Scalar, Standard_Integer_Size);
      Set_Elem_Alignment    (Any_Scalar);

      Any_String := New_Standard_Entity ("a string type");
      Mutate_Ekind          (Any_String, E_Array_Type);
      Set_Scope             (Any_String, Standard_Standard);
      Set_Etype             (Any_String, Any_String);
      Set_Component_Type    (Any_String, Any_Character);
      Reinit_Size_Align     (Any_String);
      Make_Dummy_Index      (Any_String);

      Raise_Type := New_Standard_Entity ("raise type");
      Decl := New_Node (N_Full_Type_Declaration, Stloc);
      Set_Defining_Identifier (Decl, Raise_Type);
      Set_Scope (Raise_Type, Standard_Standard);
      Build_Signed_Integer_Type (Raise_Type, Standard_Integer_Size);

      Standard_Integer_8 := New_Standard_Entity ("integer_8");
      Decl := New_Node (N_Full_Type_Declaration, Stloc);
      Set_Defining_Identifier (Decl, Standard_Integer_8);
      Set_Scope (Standard_Integer_8, Standard_Standard);
      Build_Signed_Integer_Type (Standard_Integer_8, 8);

      Standard_Integer_16 := New_Standard_Entity ("integer_16");
      Decl := New_Node (N_Full_Type_Declaration, Stloc);
      Set_Defining_Identifier (Decl, Standard_Integer_16);
      Set_Scope (Standard_Integer_16, Standard_Standard);
      Build_Signed_Integer_Type (Standard_Integer_16, 16);

      Standard_Integer_32 := New_Standard_Entity ("integer_32");
      Decl := New_Node (N_Full_Type_Declaration, Stloc);
      Set_Defining_Identifier (Decl, Standard_Integer_32);
      Set_Scope (Standard_Integer_32, Standard_Standard);
      Build_Signed_Integer_Type (Standard_Integer_32, 32);

      Standard_Integer_64 := New_Standard_Entity ("integer_64");
      Decl := New_Node (N_Full_Type_Declaration, Stloc);
      Set_Defining_Identifier (Decl, Standard_Integer_64);
      Set_Scope (Standard_Integer_64, Standard_Standard);
      Build_Signed_Integer_Type (Standard_Integer_64, 64);

      Standard_Integer_128 := New_Standard_Entity ("integer_128");
      Decl := New_Node (N_Full_Type_Declaration, Stloc);
      Set_Defining_Identifier (Decl, Standard_Integer_128);
      Set_Scope (Standard_Integer_128, Standard_Standard);
      Build_Signed_Integer_Type (Standard_Integer_128, 128);

      --  Standard_*_Unsigned subtypes are not user visible, but they are
      --  used internally. They are unsigned types with the same length as
      --  the correspondingly named signed integer types.

      Standard_Short_Short_Unsigned :=
        New_Standard_Entity ("short_short_unsigned");
      Build_Unsigned_Integer_Type
        (Standard_Short_Short_Unsigned, Standard_Short_Short_Integer_Size);

      Standard_Short_Unsigned := New_Standard_Entity ("short_unsigned");
      Build_Unsigned_Integer_Type
        (Standard_Short_Unsigned, Standard_Short_Integer_Size);

      Standard_Unsigned := New_Standard_Entity ("unsigned");
      Build_Unsigned_Integer_Type
        (Standard_Unsigned, Standard_Integer_Size);

      Standard_Long_Unsigned := New_Standard_Entity ("long_unsigned");
      Build_Unsigned_Integer_Type
        (Standard_Long_Unsigned, Standard_Long_Integer_Size);

      Standard_Long_Long_Unsigned :=
        New_Standard_Entity ("long_long_unsigned");
      Build_Unsigned_Integer_Type
        (Standard_Long_Long_Unsigned, Standard_Long_Long_Integer_Size);

      Standard_Long_Long_Long_Unsigned :=
        New_Standard_Entity ("long_long_long_unsigned");
      Build_Unsigned_Integer_Type
        (Standard_Long_Long_Long_Unsigned,
         Standard_Long_Long_Long_Integer_Size);

      --  Standard_Unsigned_64 is not user visible, but is used internally. It
      --  is an unsigned type mod 2**64 with 64 bits size.

      Standard_Unsigned_64 := New_Standard_Entity ("unsigned_64");
      Build_Unsigned_Integer_Type (Standard_Unsigned_64, 64);

      --  Standard_Address is not user visible, but is used internally. It is
      --  an unsigned type mod 2**System_Address_Size with System.Address size.

      Standard_Address := New_Standard_Entity ("standard_address");
      Build_Unsigned_Integer_Type (Standard_Address, System_Address_Size);

      --  Note: universal integer and universal real are constructed as fully
      --  formed signed numeric types, with parameters corresponding to the
      --  longest runtime types (Long_Long_Long_Integer and Long_Long_Float).
      --  This allows Gigi to properly process references to universal types
      --  that are not folded at compile time.

      Universal_Integer := New_Standard_Entity ("universal_integer");
      Decl := New_Node (N_Full_Type_Declaration, Stloc);
      Set_Defining_Identifier (Decl, Universal_Integer);
      Set_Scope (Universal_Integer, Standard_Standard);
      Build_Signed_Integer_Type
        (Universal_Integer, Standard_Long_Long_Long_Integer_Size);

      Universal_Real := New_Standard_Entity ("universal_real");
      Decl := New_Node (N_Full_Type_Declaration, Stloc);
      Set_Defining_Identifier (Decl, Universal_Real);
      Set_Scope (Universal_Real, Standard_Standard);
      Copy_Float_Type (Universal_Real, Standard_Long_Long_Float);

      --  Note: universal fixed, unlike universal integer and universal real,
      --  is never used at runtime, so it does not need to have bounds set.

      Universal_Fixed := New_Standard_Entity ("universal_fixed");
      Decl := New_Node (N_Full_Type_Declaration, Stloc);
      Set_Defining_Identifier (Decl, Universal_Fixed);
      Mutate_Ekind         (Universal_Fixed, E_Ordinary_Fixed_Point_Type);
      Set_Etype            (Universal_Fixed, Universal_Fixed);
      Set_Scope            (Universal_Fixed, Standard_Standard);
      Init_Size            (Universal_Fixed, Standard_Long_Long_Integer_Size);
      Set_Elem_Alignment   (Universal_Fixed);
      Set_Size_Known_At_Compile_Time
                           (Universal_Fixed);

      Universal_Access := New_Standard_Entity ("universal_access");
      Decl := New_Node (N_Full_Type_Declaration, Stloc);
      Set_Defining_Identifier (Decl, Universal_Access);
      Mutate_Ekind                 (Universal_Access, E_Access_Type);
      Set_Etype                    (Universal_Access, Universal_Access);
      Set_Scope                    (Universal_Access, Standard_Standard);
      Init_Size                    (Universal_Access, System_Address_Size);
      Set_Elem_Alignment           (Universal_Access);
      Set_Directly_Designated_Type (Universal_Access, Any_Type);

      --  Create type declaration for Duration, using a 64-bit size. The
      --  delta and size values depend on the mode set in system.ads.

      Build_Duration : declare
         Dlo       : Uint;
         Dhi       : Uint;
         Delta_Val : Ureal;

      begin
         --  In 32 bit mode, the size is 32 bits, and the delta and
         --  small values are set to 20 milliseconds (20.0*(10.0**(-3)).

         if Duration_32_Bits_On_Target then
            Dlo := Intval (Type_Low_Bound (Standard_Integer_32));
            Dhi := Intval (Type_High_Bound (Standard_Integer_32));
            Delta_Val := UR_From_Components (UI_From_Int (20), Uint_3, 10);

         --  In 64-bit mode, the size is 64-bits and the delta and
         --  small values are set to nanoseconds (1.0*(10.0**(-9)).

         else
            Dlo := Intval (Type_Low_Bound (Standard_Integer_64));
            Dhi := Intval (Type_High_Bound (Standard_Integer_64));
            Delta_Val := UR_From_Components (Uint_1, Uint_9, 10);
         end if;

         Tdef_Node := Make_Ordinary_Fixed_Point_Definition (Stloc,
                 Delta_Expression => Make_Real_Literal (Stloc, Delta_Val),
                 Real_Range_Specification =>
                   Make_Real_Range_Specification (Stloc,
                     Low_Bound  => Make_Real_Literal (Stloc,
                       Realval => Dlo * Delta_Val),
                     High_Bound => Make_Real_Literal (Stloc,
                       Realval => Dhi * Delta_Val)));

         Set_Type_Definition (Parent (Standard_Duration), Tdef_Node);

         Mutate_Ekind (Standard_Duration, E_Ordinary_Fixed_Point_Type);
         Set_Etype (Standard_Duration, Standard_Duration);

         if Duration_32_Bits_On_Target then
            Init_Size (Standard_Duration, 32);
         else
            Init_Size (Standard_Duration, 64);
         end if;

         Set_Elem_Alignment (Standard_Duration);
         Set_Delta_Value    (Standard_Duration, Delta_Val);
         Set_Small_Value    (Standard_Duration, Delta_Val);
         Set_Scalar_Range   (Standard_Duration,
                              Real_Range_Specification
                               (Type_Definition (Parent (Standard_Duration))));

         --  Normally it does not matter that nodes in package Standard are
         --  not marked as analyzed. The Scalar_Range of the fixed-point type
         --  Standard_Duration is an exception, because of the special test
         --  made in Freeze.Freeze_Fixed_Point_Type.

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
      --  actually used in the generated code, so it must be set correctly.
      --  The type Standard_Exception_Type must be consistent with the type
      --  System.Standard_Library.Exception_Data, as the latter is what is
      --  known by the run-time. Components of the record are documented in
      --  the declaration in System.Standard_Library.

      Build_Exception_Type : declare
         Comp_List      : List_Id;
         Comp           : Entity_Id;

      begin
         Standard_Exception_Type := New_Standard_Entity ("exception");
         Mutate_Ekind    (Standard_Exception_Type, E_Record_Type);
         Set_Etype       (Standard_Exception_Type, Standard_Exception_Type);
         Set_Scope       (Standard_Exception_Type, Standard_Standard);
         Set_Stored_Constraint
                         (Standard_Exception_Type, No_Elist);
         Set_Size_Known_At_Compile_Time
                         (Standard_Exception_Type, True);

         pragma Assert (not Known_RM_Size (Standard_Exception_Type));

         Make_Aliased_Component (Standard_Exception_Type, Standard_Boolean,
                         "Not_Handled_By_Others");
         Make_Aliased_Component (Standard_Exception_Type, Standard_Character,
                         "Lang");
         Make_Aliased_Component (Standard_Exception_Type, Standard_Natural,
                         "Name_Length");
         Make_Aliased_Component (Standard_Exception_Type, Standard_Address,
                         "Full_Name");
         Make_Aliased_Component (Standard_Exception_Type, Standard_A_Char,
                         "HTable_Ptr");
         Make_Aliased_Component (Standard_Exception_Type, Standard_Address,
                         "Foreign_Data");
         Make_Aliased_Component (Standard_Exception_Type, Standard_A_Char,
                         "Raise_Hook");

         Layout_Type (Standard_Exception_Type);

         --  Build tree for record declaration, for use by the back-end

         Comp      := First_Entity (Standard_Exception_Type);
         Comp_List := New_List;
         while Present (Comp) loop
            Append (
              Make_Component_Declaration (Stloc,
                Defining_Identifier => Comp,
                Component_Definition =>
                  Make_Component_Definition (Stloc,
                    Aliased_Present    => True,
                    Subtype_Indication =>
                      New_Occurrence_Of (Etype (Comp), Stloc))),
              Comp_List);

            Next_Entity (Comp);
         end loop;

         Decl := Make_Full_Type_Declaration (Stloc,
           Defining_Identifier => Standard_Exception_Type,
           Type_Definition =>
             Make_Record_Definition (Stloc,
               End_Label      => Empty,
               Component_List =>
                 Make_Component_List (Stloc,
                   Component_Items => Comp_List)));

         Append (Decl, Decl_S);
      end Build_Exception_Type;

      --  Create declarations of standard exceptions

      Build_Exception (S_Constraint_Error);
      Build_Exception (S_Program_Error);
      Build_Exception (S_Storage_Error);
      Build_Exception (S_Tasking_Error);

      --  Numeric_Error is a normal exception in Ada 83, but in Ada 95
      --  it is a renaming of Constraint_Error. This test is too early since
      --  it doesn't handle pragma Ada_83. But it's not worth the trouble of
      --  fixing this.

      if Ada_Version = Ada_83 then
         Build_Exception (S_Numeric_Error);

      else
         Decl := New_Node (N_Exception_Renaming_Declaration, Stloc);
         E_Id := Standard_Entity (S_Numeric_Error);

         Mutate_Ekind       (E_Id, E_Exception);
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
      Set_Chars     (Abort_Signal, Name_uAbort_Signal);
      Mutate_Ekind  (Abort_Signal, E_Exception);
      Set_Etype     (Abort_Signal, Standard_Exception_Type);
      Set_Scope     (Abort_Signal, Standard_Standard);
      Set_Is_Public (Abort_Signal, True);
      Decl :=
        Make_Exception_Declaration (Stloc,
          Defining_Identifier => Abort_Signal);

      --  Create defining identifiers for shift operator entities. Note
      --  that these entities are used only for marking shift operators
      --  generated internally, and hence need no structure, just a name
      --  and a unique identity.

      Standard_Op_Rotate_Left := New_Standard_Entity;
      Set_Chars (Standard_Op_Rotate_Left, Name_Rotate_Left);
      Mutate_Ekind (Standard_Op_Rotate_Left, E_Operator);

      Standard_Op_Rotate_Right := New_Standard_Entity;
      Set_Chars (Standard_Op_Rotate_Right, Name_Rotate_Right);
      Mutate_Ekind (Standard_Op_Rotate_Right, E_Operator);

      Standard_Op_Shift_Left := New_Standard_Entity;
      Set_Chars (Standard_Op_Shift_Left, Name_Shift_Left);
      Mutate_Ekind (Standard_Op_Shift_Left, E_Operator);

      Standard_Op_Shift_Right := New_Standard_Entity;
      Set_Chars (Standard_Op_Shift_Right, Name_Shift_Right);
      Mutate_Ekind (Standard_Op_Shift_Right, E_Operator);

      Standard_Op_Shift_Right_Arithmetic := New_Standard_Entity;
      Set_Chars (Standard_Op_Shift_Right_Arithmetic,
                                          Name_Shift_Right_Arithmetic);
      Mutate_Ekind (Standard_Op_Shift_Right_Arithmetic,
                                          E_Operator);

      --  Create standard operator declarations

      Create_Operators;

      --  Initialize visibility table with entities in Standard

      for E in Standard_Entity_Type loop
         if Ekind (Standard_Entity (E)) /= E_Operator then
            Set_Current_Entity (Standard_Entity (E));
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

      --  Print representation of standard if switch set

      if Opt.Print_Standard then
         Print_Standard;
      end if;
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
      Mutate_Ekind         (E, K);
      Set_Is_Constrained   (E, True);
      Set_Is_First_Subtype (E, True);
      Set_Etype            (E, New_Ent);

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
      Ident_Node : constant Node_Id := New_Node (N_Identifier, Stloc);

   begin
      Set_Chars (Ident_Node, Chars (Standard_Entity (S)));
      Set_Entity (Ident_Node, Standard_Entity (S));

      return Ident_Node;
   end Identifier_For;

   ----------------------------
   -- Make_Aliased_Component --
   ----------------------------

   procedure Make_Aliased_Component
     (Rec : Entity_Id;
      Typ : Entity_Id;
      Nam : String)
   is
      Id : constant Entity_Id := New_Standard_Entity (Nam);

   begin
      Mutate_Ekind                  (Id, E_Component);
      Set_Etype                     (Id, Typ);
      Set_Scope                     (Id, Rec);
      Reinit_Component_Location     (Id);
      Set_Original_Record_Component (Id, Id);
      Set_Is_Aliased                (Id);
      Set_Is_Independent            (Id);
      Append_Entity (Id, Rec);
   end Make_Aliased_Component;

   -----------------
   -- Make_Formal --
   -----------------

   function Make_Formal (Typ : Entity_Id; Nam : String) return Entity_Id is
      Formal : constant Entity_Id := New_Standard_Entity (Nam);

   begin
      Mutate_Ekind  (Formal, E_In_Parameter);
      Set_Mechanism (Formal, Default_Mechanism);
      Set_Scope     (Formal, Standard_Standard);
      Set_Etype     (Formal, Typ);

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

   ------------------
   -- New_Operator --
   ------------------

   function New_Operator (Op : Name_Id; Typ : Entity_Id) return Entity_Id is
      Ident_Node : constant Entity_Id := Make_Defining_Identifier (Stloc, Op);

   begin
      Set_Is_Pure    (Ident_Node, True);
      Mutate_Ekind   (Ident_Node, E_Operator);
      Set_Etype      (Ident_Node, Typ);
      Set_Scope      (Ident_Node, Standard_Standard);
      Set_Homonym    (Ident_Node, Get_Name_Entity_Id (Op));
      Set_Convention (Ident_Node, Convention_Intrinsic);

      Set_Is_Immediately_Visible  (Ident_Node, True);
      Set_Is_Intrinsic_Subprogram (Ident_Node, True);
      Set_Is_Not_Self_Hidden (Ident_Node);

      Set_Name_Entity_Id (Op, Ident_Node);
      Append_Entity (Ident_Node, Standard_Standard);

      return Ident_Node;
   end New_Operator;

   -------------------------
   -- New_Standard_Entity --
   -------------------------

   function New_Standard_Entity return Entity_Id
   is
      E : constant Entity_Id := New_Entity (N_Defining_Identifier, Stloc);

   begin
      --  All standard entities are Pure and Public

      Set_Is_Pure (E);
      Set_Is_Public (E);

      --  All standard entity names are analyzed manually, and are thus
      --  frozen and not self-hidden as soon as they are created.

      Set_Is_Frozen (E);
      Set_Is_Not_Self_Hidden (E);

      --  Set debug information required for all standard types

      Set_Needs_Debug_Info (E);

      --  All standard entities are built with fully qualified names, so
      --  set the flag to prevent an abortive attempt at requalification.

      Set_Has_Qualified_Name (E);

      --  Return newly created entity to be completed by caller

      return E;
   end New_Standard_Entity;

   function New_Standard_Entity (Nam : String) return Entity_Id is
      Ent : constant Entity_Id := New_Standard_Entity;

   begin
      for J in 1 .. Nam'Length loop
         Name_Buffer (J) := Fold_Lower (Nam (Nam'First + (J - 1)));
      end loop;

      Name_Len := Nam'Length;
      Set_Chars (Ent, Name_Find);

      return Ent;
   end New_Standard_Entity;

   --------------------
   -- Print_Standard --
   --------------------

   procedure Print_Standard is

      procedure P (Item : String) renames Output.Write_Line;
      --  Short-hand, since we do a lot of line writes here

      procedure P_Int_Range (Size : Pos);
      --  Prints the range of an integer based on its Size

      procedure P_Float_Range (Id : Entity_Id);
      --  Prints the bounds range for the given float type entity

      procedure P_Float_Type (Id : Entity_Id);
      --  Prints the type declaration of the given float type entity

      procedure P_Mixed_Name (Id : Name_Id);
      --  Prints Id in mixed case

      -------------------
      -- P_Float_Range --
      -------------------

      procedure P_Float_Range (Id : Entity_Id) is
      begin
         Write_Str ("     range ");
         UR_Write (Realval (Type_Low_Bound (Id)));
         Write_Str (" .. ");
         UR_Write (Realval (Type_High_Bound (Id)));
         Write_Str (";");
         Write_Eol;
      end P_Float_Range;

      ------------------
      -- P_Float_Type --
      ------------------

      procedure P_Float_Type (Id : Entity_Id) is
      begin
         Write_Str ("   type ");
         P_Mixed_Name (Chars (Id));
         Write_Str (" is digits ");
         Write_Int (UI_To_Int (Digits_Value (Id)));
         Write_Eol;
         P_Float_Range (Id);
         Write_Str ("   for ");
         P_Mixed_Name (Chars (Id));
         Write_Str ("'Size use ");
         Write_Int (UI_To_Int (RM_Size (Id)));
         Write_Line (";");
         Write_Eol;
      end P_Float_Type;

      -----------------
      -- P_Int_Range --
      -----------------

      procedure P_Int_Range (Size : Pos) is
      begin
         Write_Str (" is range -(2 **");
         Write_Int (Size - 1);
         Write_Str (")");
         Write_Str (" .. +(2 **");
         Write_Int (Size - 1);
         Write_Str (" - 1);");
         Write_Eol;
      end P_Int_Range;

      ------------------
      -- P_Mixed_Name --
      ------------------

      procedure P_Mixed_Name (Id : Name_Id) is
      begin
         Get_Name_String (Id);

         for J in 1 .. Name_Len loop
            if J = 1 or else Name_Buffer (J - 1) = '_' then
               Name_Buffer (J) := Fold_Upper (Name_Buffer (J));
            end if;
         end loop;

         Write_Str (Name_Buffer (1 .. Name_Len));
      end P_Mixed_Name;

   --  Start of processing for Print_Standard

   begin
      P ("--  Representation of package Standard");
      Write_Eol;
      P ("--  This is not accurate Ada, since new base types cannot be ");
      P ("--  created, but the listing shows the target dependent");
      P ("--  characteristics of the Standard types for this compiler");
      Write_Eol;

      P ("package Standard is");
      P ("pragma Pure (Standard);");
      Write_Eol;

      P ("   type Boolean is (False, True);");
      P ("   for Boolean'Size use 1;");
      P ("   for Boolean use (False => 0, True => 1);");
      Write_Eol;

      --  Integer types

      Write_Str ("   type Integer");
      P_Int_Range (Standard_Integer_Size);
      Write_Str ("   for Integer'Size use ");
      Write_Int (Standard_Integer_Size);
      P (";");
      Write_Eol;

      P ("   subtype Natural  is Integer range 0 .. Integer'Last;");
      P ("   subtype Positive is Integer range 1 .. Integer'Last;");
      Write_Eol;

      Write_Str ("   type Short_Short_Integer");
      P_Int_Range (Standard_Short_Short_Integer_Size);
      Write_Str ("   for Short_Short_Integer'Size use ");
      Write_Int (Standard_Short_Short_Integer_Size);
      P (";");
      Write_Eol;

      Write_Str ("   type Short_Integer");
      P_Int_Range (Standard_Short_Integer_Size);
      Write_Str ("   for Short_Integer'Size use ");
      Write_Int (Standard_Short_Integer_Size);
      P (";");
      Write_Eol;

      Write_Str ("   type Long_Integer");
      P_Int_Range (Standard_Long_Integer_Size);
      Write_Str ("   for Long_Integer'Size use ");
      Write_Int (Standard_Long_Integer_Size);
      P (";");
      Write_Eol;

      Write_Str ("   type Long_Long_Integer");
      P_Int_Range (Standard_Long_Long_Integer_Size);
      Write_Str ("   for Long_Long_Integer'Size use ");
      Write_Int (Standard_Long_Long_Integer_Size);
      P (";");
      Write_Eol;

      Write_Str ("   type Long_Long_Long_Integer");
      P_Int_Range (Standard_Long_Long_Long_Integer_Size);
      Write_Str ("   for Long_Long_Long_Integer'Size use ");
      Write_Int (Standard_Long_Long_Long_Integer_Size);
      P (";");
      Write_Eol;

      --  Floating point types

      P_Float_Type (Standard_Short_Float);
      P_Float_Type (Standard_Float);
      P_Float_Type (Standard_Long_Float);
      P_Float_Type (Standard_Long_Long_Float);

      P ("   type Character is (...)");
      Write_Str ("   for Character'Size use ");
      Write_Int (Standard_Character_Size);
      P (";");
      P ("   --  See RM A.1(35) for details of this type");
      Write_Eol;

      P ("   type Wide_Character is (...)");
      Write_Str ("   for Wide_Character'Size use ");
      Write_Int (Standard_Wide_Character_Size);
      P (";");
      P ("   --  See RM A.1(36) for details of this type");
      Write_Eol;

      P ("   type Wide_Wide_Character is (...)");
      Write_Str ("   for Wide_Wide_Character'Size use ");
      Write_Int (Standard_Wide_Wide_Character_Size);
      P (";");
      P ("   --  See RM A.1(36) for details of this type");

      P ("   type String is array (Positive range <>) of Character;");
      P ("   pragma Pack (String);");
      Write_Eol;

      P ("   type Wide_String is array (Positive range <>)" &
         " of Wide_Character;");
      P ("   pragma Pack (Wide_String);");
      Write_Eol;

      P ("   type Wide_Wide_String is array (Positive range <>)" &
         "  of Wide_Wide_Character;");
      P ("   pragma Pack (Wide_Wide_String);");
      Write_Eol;

      --  We only have one representation each for 32-bit and 64-bit sizes,
      --  so select the right one based on Duration_32_Bits_On_Target.

      if Duration_32_Bits_On_Target then
         P ("   type Duration is delta 0.020");
         P ("     range -((2 ** 31)     * 0.020) ..");
         P ("           +((2 ** 31 - 1) * 0.020);");
         P ("   for Duration'Small use 0.020;");

      else
         P ("   type Duration is delta 0.000000001");
         P ("     range -((2 ** 63)     * 0.000000001) ..");
         P ("           +((2 ** 63 - 1) * 0.000000001);");
         P ("   for Duration'Small use 0.000000001;");
      end if;

      Write_Eol;

      P ("   Constraint_Error : exception;");
      P ("   Program_Error    : exception;");
      P ("   Storage_Error    : exception;");
      P ("   Tasking_Error    : exception;");
      P ("   Numeric_Error    : exception renames Constraint_Error;");
      Write_Eol;

      P ("end Standard;");
   end Print_Standard;

   -------------------------
   -- Register_Float_Type --
   -------------------------

   procedure Register_Float_Type
     (Name      : String;
      Digs      : Positive;
      Float_Rep : Float_Rep_Kind;
      Precision : Positive;
      Size      : Positive;
      Alignment : Natural)
   is
      pragma Unreferenced (Precision);
      --  See Build_Float_Type for the rationale

      Ent : constant Entity_Id := New_Standard_Entity (Name);

   begin
      Set_Defining_Identifier (New_Node (N_Full_Type_Declaration, Stloc), Ent);
      Set_Scope (Ent, Standard_Standard);
      Build_Float_Type
        (Ent, Pos (Digs), Float_Rep, Int (Size), Nat (Alignment / 8));

      Append_New_Elmt (Ent, Back_End_Float_Types);
   end Register_Float_Type;

   ----------------------
   -- Set_Float_Bounds --
   ----------------------

   procedure Set_Float_Bounds (Id : Entity_Id) is
      L : Node_Id;
      H : Node_Id;
      --  Low and high bounds of literal value

      R : Node_Id;
      --  Range specification

      Radix       : constant Uint := Machine_Radix_Value (Id);
      Mantissa    : constant Uint := Machine_Mantissa_Value (Id);
      Emax        : constant Uint := Machine_Emax_Value (Id);
      Significand : constant Uint := Radix ** Mantissa - 1;
      Exponent    : constant Uint := Emax - Mantissa;

   begin
      H := Make_Float_Literal (Stloc, Radix, Significand, Exponent);
      L := Make_Float_Literal (Stloc, Radix, -Significand, Exponent);

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
      L : Node_Id;
      H : Node_Id;
      --  Low and high bounds of literal value

      R : Node_Id;
      --  Range specification

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
