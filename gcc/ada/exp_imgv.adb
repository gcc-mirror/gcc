------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ I M G V                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2001-2019, Free Software Foundation, Inc.         --
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

with Atree;    use Atree;
with Casing;   use Casing;
with Checks;   use Checks;
with Einfo;    use Einfo;
with Exp_Util; use Exp_Util;
with Lib;      use Lib;
with Namet;    use Namet;
with Nmake;    use Nmake;
with Nlists;   use Nlists;
with Opt;      use Opt;
with Rtsfind;  use Rtsfind;
with Sem_Aux;  use Sem_Aux;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Stand;    use Stand;
with Stringt;  use Stringt;
with Tbuild;   use Tbuild;
with Ttypes;   use Ttypes;
with Uintp;    use Uintp;
with Urealp;   use Urealp;

package body Exp_Imgv is

   function Has_Decimal_Small (E : Entity_Id) return Boolean;
   --  Applies to all entities. True for a Decimal_Fixed_Point_Type, or an
   --  Ordinary_Fixed_Point_Type with a small that is a negative power of ten.
   --  Shouldn't this be in einfo.adb or sem_aux.adb???

   procedure Rewrite_Object_Image
     (N         : Node_Id;
      Pref      : Entity_Id;
      Attr_Name : Name_Id;
      Str_Typ   : Entity_Id);
   --  AI12-00124: Rewrite attribute 'Image when it is applied to an object
   --  reference as an attribute applied to a type. N denotes the node to be
   --  rewritten, Pref denotes the prefix of the 'Image attribute, and Name
   --  and Str_Typ specify which specific string type and 'Image attribute to
   --  apply (e.g. Name_Wide_Image and Standard_Wide_String).

   ------------------------------------
   -- Build_Enumeration_Image_Tables --
   ------------------------------------

   procedure Build_Enumeration_Image_Tables (E : Entity_Id; N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (E);

      Eind : Entity_Id;
      Estr : Entity_Id;
      Ind  : List_Id;
      Ityp : Node_Id;
      Len  : Nat;
      Lit  : Entity_Id;
      Nlit : Nat;
      Str  : String_Id;

      Saved_SSO : constant Character := Opt.Default_SSO;
      --  Used to save the current scalar storage order during the generation
      --  of the literal lookup table.

   begin
      --  Nothing to do for types other than a root enumeration type

      if E /= Root_Type (E) then
         return;

      --  Nothing to do if pragma Discard_Names applies

      elsif Discard_Names (E) then
         return;
      end if;

      --  Otherwise tables need constructing

      Start_String;
      Ind := New_List;
      Lit := First_Literal (E);
      Len := 1;
      Nlit := 0;

      loop
         Append_To (Ind,
           Make_Integer_Literal (Loc, UI_From_Int (Len)));

         exit when No (Lit);
         Nlit := Nlit + 1;

         Get_Unqualified_Decoded_Name_String (Chars (Lit));

         if Name_Buffer (1) /= ''' then
            Set_Casing (All_Upper_Case);
         end if;

         Store_String_Chars (Name_Buffer (1 .. Name_Len));
         Len := Len + Int (Name_Len);
         Next_Literal (Lit);
      end loop;

      if Len < Int (2 ** (8 - 1)) then
         Ityp := Standard_Integer_8;
      elsif Len < Int (2 ** (16 - 1)) then
         Ityp := Standard_Integer_16;
      else
         Ityp := Standard_Integer_32;
      end if;

      Str := End_String;

      Estr :=
        Make_Defining_Identifier (Loc,
          Chars => New_External_Name (Chars (E), 'S'));

      Eind :=
        Make_Defining_Identifier (Loc,
          Chars => New_External_Name (Chars (E), 'N'));

      Set_Lit_Strings (E, Estr);
      Set_Lit_Indexes (E, Eind);

      --  Temporarily set the current scalar storage order to the default
      --  during the generation of the literals table, since both the Image and
      --  Value attributes rely on runtime routines for interpreting table
      --  values.

      Opt.Default_SSO := ' ';

      --  Generate literal table

      Insert_Actions (N,
        New_List (
          Make_Object_Declaration (Loc,
            Defining_Identifier => Estr,
            Constant_Present    => True,
            Object_Definition   =>
              New_Occurrence_Of (Standard_String, Loc),
            Expression          =>
              Make_String_Literal (Loc,
                Strval => Str)),

          Make_Object_Declaration (Loc,
            Defining_Identifier => Eind,
            Constant_Present    => True,

            Object_Definition =>
              Make_Constrained_Array_Definition (Loc,
                Discrete_Subtype_Definitions => New_List (
                  Make_Range (Loc,
                    Low_Bound  => Make_Integer_Literal (Loc, 0),
                    High_Bound => Make_Integer_Literal (Loc, Nlit))),
                Component_Definition =>
                  Make_Component_Definition (Loc,
                    Aliased_Present    => False,
                    Subtype_Indication => New_Occurrence_Of (Ityp, Loc))),

            Expression          =>
              Make_Aggregate (Loc,
                Expressions => Ind))),
        Suppress => All_Checks);

      --  Reset the scalar storage order to the saved value

      Opt.Default_SSO := Saved_SSO;
   end Build_Enumeration_Image_Tables;

   ----------------------------
   -- Expand_Image_Attribute --
   ----------------------------

   --  For all cases other than user-defined enumeration types, the scheme
   --  is as follows. First we insert the following code:

   --    Snn : String (1 .. rt'Width);
   --    Pnn : Natural;
   --    Image_xx (tv, Snn, Pnn [,pm]);
   --
   --  and then Expr is replaced by Snn (1 .. Pnn)

   --  In the above expansion:

   --    rt is the root type of the expression
   --    tv is the expression with the value, usually a type conversion
   --    pm is an extra parameter present in some cases

   --  The following table shows tv, xx, and (if used) pm for the various
   --  possible types of the argument:

   --    For types whose root type is Character
   --      xx = Character
   --      tv = Character (Expr)

   --    For types whose root type is Boolean
   --      xx = Boolean
   --      tv = Boolean (Expr)

   --    For signed integer types with size <= Integer'Size
   --      xx = Integer
   --      tv = Integer (Expr)

   --    For other signed integer types
   --      xx = Long_Long_Integer
   --      tv = Long_Long_Integer (Expr)

   --    For modular types with modulus <= System.Unsigned_Types.Unsigned
   --      xx = Unsigned
   --      tv = System.Unsigned_Types.Unsigned (Expr)

   --    For other modular integer types
   --      xx = Long_Long_Unsigned
   --      tv = System.Unsigned_Types.Long_Long_Unsigned (Expr)

   --    For types whose root type is Wide_Character
   --      xx = Wide_Character
   --      tv = Wide_Character (Expr)
   --      pm = Boolean, true if Ada 2005 mode, False otherwise

   --    For types whose root type is Wide_Wide_Character
   --      xx = Wide_Wide_Character
   --      tv = Wide_Wide_Character (Expr)

   --    For floating-point types
   --      xx = Floating_Point
   --      tv = Long_Long_Float (Expr)
   --      pm = typ'Digits (typ = subtype of expression)

   --    For ordinary fixed-point types
   --      xx = Ordinary_Fixed_Point
   --      tv = Long_Long_Float (Expr)
   --      pm = typ'Aft (typ = subtype of expression)

   --    For decimal fixed-point types with size = Integer'Size
   --      xx = Decimal
   --      tv = Integer (Expr)
   --      pm = typ'Scale (typ = subtype of expression)

   --    For decimal fixed-point types with size > Integer'Size
   --      xx = Long_Long_Decimal
   --      tv = Long_Long_Integer?(Expr) [convert with no scaling]
   --      pm = typ'Scale (typ = subtype of expression)

   --  For enumeration types other than those declared packages Standard
   --  or System, Snn, Pnn, are expanded as above, but the call looks like:

   --    Image_Enumeration_NN (rt'Pos (X), Snn, Pnn, typS, typI'Address)

   --  where rt is the root type of the expression, and typS and typI are
   --  the entities constructed as described in the spec for the procedure
   --  Build_Enumeration_Image_Tables and NN is 32/16/8 depending on the
   --  element type of Lit_Indexes. The rewriting of the expression to
   --  Snn (1 .. Pnn) then occurs as in the other cases. A special case is
   --  when pragma Discard_Names applies, in which case we replace expr by:

   --     (rt'Pos (expr))'Img

   --  So that the result is a space followed by the decimal value for the
   --  position of the enumeration value in the enumeration type.

   procedure Expand_Image_Attribute (N : Node_Id) is
      Loc   : constant Source_Ptr := Sloc (N);
      Exprs : constant List_Id    := Expressions (N);
      Expr  : constant Node_Id    := Relocate_Node (First (Exprs));
      Pref  : constant Node_Id    := Prefix (N);

      procedure Expand_User_Defined_Enumeration_Image;
      --  Expand attribute 'Image in user-defined enumeration types, avoiding
      --  string copy.

      function Is_User_Defined_Enumeration_Type
        (Typ : Entity_Id) return Boolean;
      --  Return True if Typ is a user-defined enumeration type

      -------------------------------------------
      -- Expand_User_Defined_Enumeration_Image --
      -------------------------------------------

      procedure Expand_User_Defined_Enumeration_Image is
         Ins_List : constant List_Id   := New_List;
         P1_Id    : constant Entity_Id := Make_Temporary (Loc, 'P');
         P2_Id    : constant Entity_Id := Make_Temporary (Loc, 'P');
         P3_Id    : constant Entity_Id := Make_Temporary (Loc, 'P');
         P4_Id    : constant Entity_Id := Make_Temporary (Loc, 'P');
         Ptyp     : constant Entity_Id := Entity (Pref);
         Rtyp     : constant Entity_Id := Root_Type (Ptyp);
         S1_Id    : constant Entity_Id := Make_Temporary (Loc, 'S');

      begin
         --  Apply a validity check, since it is a bit drastic to get a
         --  completely junk image value for an invalid value.

         if not Expr_Known_Valid (Expr) then
            Insert_Valid_Check (Expr);
         end if;

         --  Generate:
         --    P1 : constant Natural := Pos;

         Append_To (Ins_List,
           Make_Object_Declaration (Loc,
             Defining_Identifier => P1_Id,
             Object_Definition   =>
               New_Occurrence_Of (Standard_Natural, Loc),
             Constant_Present    => True,
             Expression          =>
               Convert_To (Standard_Natural,
                 Make_Attribute_Reference (Loc,
                   Attribute_Name => Name_Pos,
                   Prefix         => New_Occurrence_Of (Ptyp, Loc),
                   Expressions    => New_List (Expr)))));

         --  Compute the index of the string start, generating:
         --    P2 : constant Natural := call_put_enumN (P1);

         Append_To (Ins_List,
           Make_Object_Declaration (Loc,
             Defining_Identifier => P2_Id,
             Object_Definition   =>
               New_Occurrence_Of (Standard_Natural, Loc),
             Constant_Present    => True,
             Expression          =>
               Convert_To (Standard_Natural,
                 Make_Indexed_Component (Loc,
                   Prefix      =>
                     New_Occurrence_Of (Lit_Indexes (Rtyp), Loc),
                   Expressions =>
                     New_List (New_Occurrence_Of (P1_Id, Loc))))));

         --  Compute the index of the next value, generating:
         --    P3 : constant Natural := call_put_enumN (P1 + 1);

         declare
            Add_Node : constant Node_Id := New_Op_Node (N_Op_Add, Loc);

         begin
            Set_Left_Opnd  (Add_Node, New_Occurrence_Of (P1_Id, Loc));
            Set_Right_Opnd (Add_Node, Make_Integer_Literal (Loc, 1));

            Append_To (Ins_List,
              Make_Object_Declaration (Loc,
                Defining_Identifier => P3_Id,
                Object_Definition   =>
                  New_Occurrence_Of (Standard_Natural, Loc),
                Constant_Present    => True,
                Expression          =>
                  Convert_To (Standard_Natural,
                    Make_Indexed_Component (Loc,
                      Prefix      =>
                        New_Occurrence_Of (Lit_Indexes (Rtyp), Loc),
                      Expressions =>
                        New_List (Add_Node)))));
         end;

         --  Generate:
         --    S4 : String renames call_put_enumS (S2 .. S3 - 1);

         declare
            Sub_Node : constant Node_Id := New_Op_Node (N_Op_Subtract, Loc);

         begin
            Set_Left_Opnd  (Sub_Node, New_Occurrence_Of (P3_Id, Loc));
            Set_Right_Opnd (Sub_Node, Make_Integer_Literal (Loc, 1));

            Append_To (Ins_List,
              Make_Object_Renaming_Declaration (Loc,
                Defining_Identifier => P4_Id,
                Subtype_Mark        =>
                  New_Occurrence_Of (Standard_String, Loc),
                Name                =>
                  Make_Slice (Loc,
                    Prefix         =>
                      New_Occurrence_Of (Lit_Strings (Rtyp), Loc),
                    Discrete_Range =>
                      Make_Range (Loc,
                        Low_Bound  => New_Occurrence_Of (P2_Id, Loc),
                        High_Bound => Sub_Node))));
         end;

         --  Generate:
         --    subtype S1 is string (1 .. P3 - P2);

         declare
            HB : constant Node_Id := New_Op_Node (N_Op_Subtract, Loc);

         begin
            Set_Left_Opnd  (HB, New_Occurrence_Of (P3_Id, Loc));
            Set_Right_Opnd (HB, New_Occurrence_Of (P2_Id, Loc));

            Append_To (Ins_List,
              Make_Subtype_Declaration (Loc,
                Defining_Identifier => S1_Id,
                Subtype_Indication  =>
                  Make_Subtype_Indication (Loc,
                    Subtype_Mark =>
                      New_Occurrence_Of (Standard_String, Loc),
                    Constraint   =>
                      Make_Index_Or_Discriminant_Constraint (Loc,
                        Constraints => New_List (
                          Make_Range (Loc,
                            Low_Bound  => Make_Integer_Literal (Loc, 1),
                            High_Bound => HB))))));
         end;

         --  Insert all the above declarations before N. We suppress checks
         --  because everything is in range at this stage.

         Insert_Actions (N, Ins_List, Suppress => All_Checks);

         Rewrite (N,
           Unchecked_Convert_To (S1_Id, New_Occurrence_Of (P4_Id, Loc)));

         Analyze_And_Resolve (N, Standard_String);
      end Expand_User_Defined_Enumeration_Image;

      --------------------------------------
      -- Is_User_Defined_Enumeration_Type --
      --------------------------------------

      function Is_User_Defined_Enumeration_Type
        (Typ : Entity_Id) return Boolean is
      begin
         return Ekind (Typ) = E_Enumeration_Type
           and then Typ /= Standard_Boolean
           and then Typ /= Standard_Character
           and then Typ /= Standard_Wide_Character
           and then Typ /= Standard_Wide_Wide_Character;
      end Is_User_Defined_Enumeration_Type;

      --  Local variables

      Enum_Case : Boolean;
      Imid      : RE_Id;
      Proc_Ent  : Entity_Id;
      Ptyp      : Entity_Id;
      Rtyp      : Entity_Id;
      Tent      : Entity_Id := Empty;
      Ttyp      : Entity_Id;

      Arg_List : List_Id;
      --  List of arguments for run-time procedure call

      Ins_List : List_Id;
      --  List of actions to be inserted

      Snn : constant Entity_Id := Make_Temporary (Loc, 'S');
      Pnn : constant Entity_Id := Make_Temporary (Loc, 'P');

   --  Start of processing for Expand_Image_Attribute

   begin
      if Is_Object_Image (Pref) then
         Rewrite_Object_Image (N, Pref, Name_Image, Standard_String);
         return;

      --  Enable speed-optimized expansion of user-defined enumeration types
      --  if we are compiling with optimizations enabled and enumeration type
      --  literals are generated. Otherwise the call will be expanded into a
      --  call to the runtime library.

      elsif Optimization_Level > 0
        and then not Global_Discard_Names
        and then Is_User_Defined_Enumeration_Type (Root_Type (Entity (Pref)))
      then
         Expand_User_Defined_Enumeration_Image;
         return;
      end if;

      Ptyp := Entity (Pref);
      Rtyp := Root_Type (Ptyp);

      --  Build declarations of Snn and Pnn to be inserted

      Ins_List := New_List (

         --  Snn : String (1 .. typ'Width);

         Make_Object_Declaration (Loc,
            Defining_Identifier => Snn,
            Object_Definition   =>
              Make_Subtype_Indication (Loc,
                Subtype_Mark => New_Occurrence_Of (Standard_String, Loc),
                Constraint   =>
                  Make_Index_Or_Discriminant_Constraint (Loc,
                    Constraints => New_List (
                      Make_Range (Loc,
                        Low_Bound  => Make_Integer_Literal (Loc, 1),
                        High_Bound =>
                          Make_Attribute_Reference (Loc,
                            Prefix         => New_Occurrence_Of (Rtyp, Loc),
                            Attribute_Name => Name_Width)))))),

         --  Pnn : Natural;

         Make_Object_Declaration (Loc,
           Defining_Identifier => Pnn,
           Object_Definition   => New_Occurrence_Of (Standard_Natural, Loc)));

      --  Set Imid (RE_Id of procedure to call), and Tent, target for the
      --  type conversion of the first argument for all possibilities.

      Enum_Case := False;

      if Rtyp = Standard_Boolean then
         Imid := RE_Image_Boolean;
         Tent := Rtyp;

      --  For standard character, we have to select the version which handles
      --  soft hyphen correctly, based on the version of Ada in use (this is
      --  ugly, but we have no choice).

      elsif Rtyp = Standard_Character then
         if Ada_Version < Ada_2005 then
            Imid := RE_Image_Character;
         else
            Imid := RE_Image_Character_05;
         end if;

         Tent := Rtyp;

      elsif Rtyp = Standard_Wide_Character then
         Imid := RE_Image_Wide_Character;
         Tent := Rtyp;

      elsif Rtyp = Standard_Wide_Wide_Character then
         Imid := RE_Image_Wide_Wide_Character;
         Tent := Rtyp;

      elsif Is_Signed_Integer_Type (Rtyp) then
         if Esize (Rtyp) <= Esize (Standard_Integer) then
            Imid := RE_Image_Integer;
            Tent := Standard_Integer;
         else
            Imid := RE_Image_Long_Long_Integer;
            Tent := Standard_Long_Long_Integer;
         end if;

      elsif Is_Modular_Integer_Type (Rtyp) then
         if Modulus (Rtyp) <= Modulus (RTE (RE_Unsigned)) then
            Imid := RE_Image_Unsigned;
            Tent := RTE (RE_Unsigned);
         else
            Imid := RE_Image_Long_Long_Unsigned;
            Tent := RTE (RE_Long_Long_Unsigned);
         end if;

      elsif Is_Fixed_Point_Type (Rtyp) and then Has_Decimal_Small (Rtyp) then
         if UI_To_Int (Esize (Rtyp)) <= Standard_Integer_Size then
            Imid := RE_Image_Decimal;
            Tent := Standard_Integer;
         else
            Imid := RE_Image_Long_Long_Decimal;
            Tent := Standard_Long_Long_Integer;
         end if;

      elsif Is_Ordinary_Fixed_Point_Type (Rtyp) then
         Imid := RE_Image_Ordinary_Fixed_Point;
         Tent := Standard_Long_Long_Float;

      elsif Is_Floating_Point_Type (Rtyp) then
         Imid := RE_Image_Floating_Point;
         Tent := Standard_Long_Long_Float;

      --  Only other possibility is user-defined enumeration type

      else
         if Discard_Names (First_Subtype (Ptyp))
           or else No (Lit_Strings (Root_Type (Ptyp)))
         then
            --  When pragma Discard_Names applies to the first subtype, build
            --  (Pref'Pos (Expr))'Img.

            Rewrite (N,
              Make_Attribute_Reference (Loc,
                Prefix =>
                   Make_Attribute_Reference (Loc,
                     Prefix         => Pref,
                     Attribute_Name => Name_Pos,
                     Expressions    => New_List (Expr)),
                Attribute_Name =>
                  Name_Img));
            Analyze_And_Resolve (N, Standard_String);
            return;

         else
            --  Here for enumeration type case

            Ttyp := Component_Type (Etype (Lit_Indexes (Rtyp)));

            if Ttyp = Standard_Integer_8 then
               Imid := RE_Image_Enumeration_8;

            elsif Ttyp = Standard_Integer_16 then
               Imid := RE_Image_Enumeration_16;

            else
               Imid := RE_Image_Enumeration_32;
            end if;

            --  Apply a validity check, since it is a bit drastic to get a
            --  completely junk image value for an invalid value.

            if not Expr_Known_Valid (Expr) then
               Insert_Valid_Check (Expr);
            end if;

            Enum_Case := True;
         end if;
      end if;

      --  Build first argument for call

      if Enum_Case then
         Arg_List := New_List (
           Make_Attribute_Reference (Loc,
             Attribute_Name => Name_Pos,
             Prefix         => New_Occurrence_Of (Ptyp, Loc),
             Expressions    => New_List (Expr)));

      else
         Arg_List := New_List (Convert_To (Tent, Expr));
      end if;

      --  Append Snn, Pnn arguments

      Append_To (Arg_List, New_Occurrence_Of (Snn, Loc));
      Append_To (Arg_List, New_Occurrence_Of (Pnn, Loc));

      --  Get entity of procedure to call

      Proc_Ent := RTE (Imid);

      --  If the procedure entity is empty, that means we have a case in
      --  no run time mode where the operation is not allowed, and an
      --  appropriate diagnostic has already been issued.

      if No (Proc_Ent) then
         return;
      end if;

      --  Otherwise complete preparation of arguments for run-time call

      --  Add extra arguments for Enumeration case

      if Enum_Case then
         Append_To (Arg_List, New_Occurrence_Of (Lit_Strings (Rtyp), Loc));
         Append_To (Arg_List,
           Make_Attribute_Reference (Loc,
             Prefix         => New_Occurrence_Of (Lit_Indexes (Rtyp), Loc),
             Attribute_Name => Name_Address));

      --  For floating-point types, append Digits argument

      elsif Is_Floating_Point_Type (Rtyp) then
         Append_To (Arg_List,
           Make_Attribute_Reference (Loc,
             Prefix         => New_Occurrence_Of (Ptyp, Loc),
             Attribute_Name => Name_Digits));

      --  For ordinary fixed-point types, append Aft parameter

      elsif Is_Ordinary_Fixed_Point_Type (Rtyp) then
         Append_To (Arg_List,
           Make_Attribute_Reference (Loc,
             Prefix         => New_Occurrence_Of (Ptyp, Loc),
             Attribute_Name => Name_Aft));

         if Has_Decimal_Small (Rtyp) then
            Set_Conversion_OK (First (Arg_List));
            Set_Etype (First (Arg_List), Tent);
         end if;

      --  For decimal, append Scale and also set to do literal conversion

      elsif Is_Decimal_Fixed_Point_Type (Rtyp) then
         Append_To (Arg_List,
           Make_Attribute_Reference (Loc,
             Prefix         => New_Occurrence_Of (Ptyp, Loc),
             Attribute_Name => Name_Scale));

         Set_Conversion_OK (First (Arg_List));
         Set_Etype (First (Arg_List), Tent);

      --  For Wide_Character, append Ada 2005 indication

      elsif Rtyp = Standard_Wide_Character then
         Append_To (Arg_List,
           New_Occurrence_Of
             (Boolean_Literals (Ada_Version >= Ada_2005), Loc));
      end if;

      --  Now append the procedure call to the insert list

      Append_To (Ins_List,
         Make_Procedure_Call_Statement (Loc,
          Name                   => New_Occurrence_Of (Proc_Ent, Loc),
          Parameter_Associations => Arg_List));

      --  Insert declarations of Snn, Pnn, and the procedure call. We suppress
      --  checks because we are sure that everything is in range at this stage.

      Insert_Actions (N, Ins_List, Suppress => All_Checks);

      --  Final step is to rewrite the expression as a slice and analyze,
      --  again with no checks, since we are sure that everything is OK.

      Rewrite (N,
        Make_Slice (Loc,
          Prefix         => New_Occurrence_Of (Snn, Loc),
          Discrete_Range =>
            Make_Range (Loc,
              Low_Bound  => Make_Integer_Literal (Loc, 1),
              High_Bound => New_Occurrence_Of (Pnn, Loc))));

      Analyze_And_Resolve (N, Standard_String, Suppress => All_Checks);
   end Expand_Image_Attribute;

   ----------------------------
   -- Expand_Value_Attribute --
   ----------------------------

   --  For scalar types derived from Boolean, Character and integer types
   --  in package Standard, typ'Value (X) expands into:

   --    btyp (Value_xx (X))

   --  where btyp is he base type of the prefix

   --    For types whose root type is Character
   --      xx = Character

   --    For types whose root type is Wide_Character
   --      xx = Wide_Character

   --    For types whose root type is Wide_Wide_Character
   --      xx = Wide_Wide_Character

   --    For types whose root type is Boolean
   --      xx = Boolean

   --    For signed integer types with size <= Integer'Size
   --      xx = Integer

   --    For other signed integer types
   --      xx = Long_Long_Integer

   --    For modular types with modulus <= System.Unsigned_Types.Unsigned
   --      xx = Unsigned

   --    For other modular integer types
   --      xx = Long_Long_Unsigned

   --    For floating-point types and ordinary fixed-point types
   --      xx = Real

   --  For Wide_[Wide_]Character types, typ'Value (X) expands into:

   --    btyp (Value_xx (X, EM))

   --  where btyp is the base type of the prefix, and EM is the encoding method

   --  For decimal types with size <= Integer'Size, typ'Value (X)
   --  expands into

   --    btyp?(Value_Decimal (X, typ'Scale));

   --  For all other decimal types, typ'Value (X) expands into

   --    btyp?(Value_Long_Long_Decimal (X, typ'Scale))

   --  For enumeration types other than those derived from types Boolean,
   --  Character, Wide_[Wide_]Character in Standard, typ'Value (X) expands to:

   --    Enum'Val (Value_Enumeration_NN (typS, typI'Address, Num, X))

   --  where typS and typI and the Lit_Strings and Lit_Indexes entities
   --  from T's root type entity, and Num is Enum'Pos (Enum'Last). The
   --  Value_Enumeration_NN function will search the tables looking for
   --  X and return the position number in the table if found which is
   --  used to provide the result of 'Value (using Enum'Val). If the
   --  value is not found Constraint_Error is raised. The suffix _NN
   --  depends on the element type of typI.

   procedure Expand_Value_Attribute (N : Node_Id) is
      Loc   : constant Source_Ptr := Sloc (N);
      Typ   : constant Entity_Id  := Etype (N);
      Btyp  : constant Entity_Id  := Base_Type (Typ);
      Rtyp  : constant Entity_Id  := Root_Type (Typ);
      Exprs : constant List_Id    := Expressions (N);
      Vid   : RE_Id;
      Args  : List_Id;
      Func  : RE_Id;
      Ttyp  : Entity_Id;

   begin
      Args := Exprs;

      if Rtyp = Standard_Character then
         Vid := RE_Value_Character;

      elsif Rtyp = Standard_Boolean then
         Vid := RE_Value_Boolean;

      elsif Rtyp = Standard_Wide_Character then
         Vid := RE_Value_Wide_Character;

         Append_To (Args,
           Make_Integer_Literal (Loc,
             Intval => Int (Wide_Character_Encoding_Method)));

      elsif Rtyp = Standard_Wide_Wide_Character then
         Vid := RE_Value_Wide_Wide_Character;

         Append_To (Args,
           Make_Integer_Literal (Loc,
             Intval => Int (Wide_Character_Encoding_Method)));

      elsif     Rtyp = Base_Type (Standard_Short_Short_Integer)
        or else Rtyp = Base_Type (Standard_Short_Integer)
        or else Rtyp = Base_Type (Standard_Integer)
      then
         Vid := RE_Value_Integer;

      elsif Is_Signed_Integer_Type (Rtyp) then
         Vid := RE_Value_Long_Long_Integer;

      elsif Is_Modular_Integer_Type (Rtyp) then
         if Modulus (Rtyp) <= Modulus (RTE (RE_Unsigned)) then
            Vid := RE_Value_Unsigned;
         else
            Vid := RE_Value_Long_Long_Unsigned;
         end if;

      elsif Is_Decimal_Fixed_Point_Type (Rtyp) then
         if UI_To_Int (Esize (Rtyp)) <= Standard_Integer_Size then
            Vid := RE_Value_Decimal;
         else
            Vid := RE_Value_Long_Long_Decimal;
         end if;

         Append_To (Args,
           Make_Attribute_Reference (Loc,
             Prefix => New_Occurrence_Of (Typ, Loc),
             Attribute_Name => Name_Scale));

         Rewrite (N,
           OK_Convert_To (Btyp,
             Make_Function_Call (Loc,
               Name => New_Occurrence_Of (RTE (Vid), Loc),
               Parameter_Associations => Args)));

         Set_Etype (N, Btyp);
         Analyze_And_Resolve (N, Btyp);
         return;

      elsif Is_Real_Type (Rtyp) then
         Vid := RE_Value_Real;

      --  Only other possibility is user-defined enumeration type

      else
         pragma Assert (Is_Enumeration_Type (Rtyp));

         --  Case of pragma Discard_Names, transform the Value
         --  attribute to Btyp'Val (Long_Long_Integer'Value (Args))

         if Discard_Names (First_Subtype (Typ))
           or else No (Lit_Strings (Rtyp))
         then
            Rewrite (N,
              Make_Attribute_Reference (Loc,
                Prefix => New_Occurrence_Of (Btyp, Loc),
                Attribute_Name => Name_Val,
                Expressions => New_List (
                  Make_Attribute_Reference (Loc,
                    Prefix =>
                      New_Occurrence_Of (Standard_Long_Long_Integer, Loc),
                    Attribute_Name => Name_Value,
                    Expressions => Args))));

            Analyze_And_Resolve (N, Btyp);

         --  Here for normal case where we have enumeration tables, this
         --  is where we build

         --    T'Val (Value_Enumeration_NN (typS, typI'Address, Num, X))

         else
            Ttyp := Component_Type (Etype (Lit_Indexes (Rtyp)));

            if Ttyp = Standard_Integer_8 then
               Func := RE_Value_Enumeration_8;
            elsif Ttyp = Standard_Integer_16 then
               Func := RE_Value_Enumeration_16;
            else
               Func := RE_Value_Enumeration_32;
            end if;

            Prepend_To (Args,
              Make_Attribute_Reference (Loc,
                Prefix => New_Occurrence_Of (Rtyp, Loc),
                Attribute_Name => Name_Pos,
                Expressions => New_List (
                  Make_Attribute_Reference (Loc,
                    Prefix => New_Occurrence_Of (Rtyp, Loc),
                    Attribute_Name => Name_Last))));

            Prepend_To (Args,
              Make_Attribute_Reference (Loc,
                Prefix => New_Occurrence_Of (Lit_Indexes (Rtyp), Loc),
                Attribute_Name => Name_Address));

            Prepend_To (Args,
              New_Occurrence_Of (Lit_Strings (Rtyp), Loc));

            Rewrite (N,
              Make_Attribute_Reference (Loc,
                Prefix => New_Occurrence_Of (Typ, Loc),
                Attribute_Name => Name_Val,
                Expressions => New_List (
                  Make_Function_Call (Loc,
                    Name =>
                      New_Occurrence_Of (RTE (Func), Loc),
                    Parameter_Associations => Args))));

            Analyze_And_Resolve (N, Btyp);
         end if;

         return;
      end if;

      --  Fall through for all cases except user-defined enumeration type
      --  and decimal types, with Vid set to the Id of the entity for the
      --  Value routine and Args set to the list of parameters for the call.

      --  Compiling package Ada.Tags under No_Run_Time_Mode we disable the
      --  expansion of the attribute into the function call statement to avoid
      --  generating spurious errors caused by the use of Integer_Address'Value
      --  in our implementation of Ada.Tags.Internal_Tag

      --  Seems like a bit of a odd approach, there should be a better way ???

      --  There is a better way, test RTE_Available ???

      if No_Run_Time_Mode
        and then Rtyp = RTE (RE_Integer_Address)
        and then RTU_Loaded (Ada_Tags)
        and then Cunit_Entity (Current_Sem_Unit)
                   = Body_Entity (RTU_Entity (Ada_Tags))
      then
         Rewrite (N,
           Unchecked_Convert_To (Rtyp,
             Make_Integer_Literal (Loc, Uint_0)));
      else
         Rewrite (N,
           Convert_To (Btyp,
             Make_Function_Call (Loc,
               Name => New_Occurrence_Of (RTE (Vid), Loc),
               Parameter_Associations => Args)));
      end if;

      Analyze_And_Resolve (N, Btyp);
   end Expand_Value_Attribute;

   ---------------------------------
   -- Expand_Wide_Image_Attribute --
   ---------------------------------

   --  We expand typ'Wide_Image (X) as follows. First we insert this code:

   --    Rnn : Wide_String (1 .. rt'Wide_Width);
   --    Lnn : Natural;
   --    String_To_Wide_String
   --      (typ'Image (Expr), Rnn, Lnn, Wide_Character_Encoding_Method);

   --  where rt is the root type of the prefix type

   --  Now we replace the Wide_Image reference by

   --    Rnn (1 .. Lnn)

   --  This works in all cases because String_To_Wide_String converts any
   --  wide character escape sequences resulting from the Image call to the
   --  proper Wide_Character equivalent

   --  not quite right for typ = Wide_Character ???

   procedure Expand_Wide_Image_Attribute (N : Node_Id) is
      Loc  : constant Source_Ptr := Sloc (N);
      Pref : constant Entity_Id  := Prefix (N);
      Rnn  : constant Entity_Id  := Make_Temporary (Loc, 'S');
      Lnn  : constant Entity_Id  := Make_Temporary (Loc, 'P');
      Rtyp : Entity_Id;

   begin
      if Is_Object_Image (Pref) then
         Rewrite_Object_Image (N, Pref, Name_Wide_Image, Standard_Wide_String);
         return;
      end if;

      Rtyp := Root_Type (Entity (Pref));

      Insert_Actions (N, New_List (

         --  Rnn : Wide_String (1 .. base_typ'Width);

         Make_Object_Declaration (Loc,
            Defining_Identifier => Rnn,
            Object_Definition   =>
              Make_Subtype_Indication (Loc,
                Subtype_Mark =>
                  New_Occurrence_Of (Standard_Wide_String, Loc),
                Constraint   =>
                  Make_Index_Or_Discriminant_Constraint (Loc,
                    Constraints => New_List (
                      Make_Range (Loc,
                        Low_Bound  => Make_Integer_Literal (Loc, 1),
                        High_Bound =>
                          Make_Attribute_Reference (Loc,
                            Prefix         => New_Occurrence_Of (Rtyp, Loc),
                            Attribute_Name => Name_Wide_Width)))))),

         --  Lnn : Natural;

         Make_Object_Declaration (Loc,
           Defining_Identifier => Lnn,
           Object_Definition   => New_Occurrence_Of (Standard_Natural, Loc)),

         --    String_To_Wide_String
         --      (typ'Image (X), Rnn, Lnn, Wide_Character_Encoding_Method);

         Make_Procedure_Call_Statement (Loc,
           Name =>
             New_Occurrence_Of (RTE (RE_String_To_Wide_String), Loc),

           Parameter_Associations => New_List (
             Make_Attribute_Reference (Loc,
               Prefix         => Prefix (N),
               Attribute_Name => Name_Image,
               Expressions    => Expressions (N)),
             New_Occurrence_Of (Rnn, Loc),
             New_Occurrence_Of (Lnn, Loc),
             Make_Integer_Literal (Loc,
               Intval => Int (Wide_Character_Encoding_Method))))),

         --  Suppress checks because we know everything is properly in range

         Suppress => All_Checks);

      --  Final step is to rewrite the expression as a slice and analyze,
      --  again with no checks, since we are sure that everything is OK.

      Rewrite (N,
        Make_Slice (Loc,
          Prefix         => New_Occurrence_Of (Rnn, Loc),
          Discrete_Range =>
            Make_Range (Loc,
              Low_Bound  => Make_Integer_Literal (Loc, 1),
              High_Bound => New_Occurrence_Of (Lnn, Loc))));

      Analyze_And_Resolve (N, Standard_Wide_String, Suppress => All_Checks);
   end Expand_Wide_Image_Attribute;

   --------------------------------------
   -- Expand_Wide_Wide_Image_Attribute --
   --------------------------------------

   --  We expand typ'Wide_Wide_Image (X) as follows. First we insert this code:

   --    Rnn : Wide_Wide_String (1 .. rt'Wide_Wide_Width);
   --    Lnn : Natural;
   --    String_To_Wide_Wide_String
   --      (typ'Image (Expr), Rnn, Lnn, Wide_Character_Encoding_Method);

   --  where rt is the root type of the prefix type

   --  Now we replace the Wide_Wide_Image reference by

   --    Rnn (1 .. Lnn)

   --  This works in all cases because String_To_Wide_Wide_String converts any
   --  wide character escape sequences resulting from the Image call to the
   --  proper Wide_Wide_Character equivalent

   --  not quite right for typ = Wide_Wide_Character ???

   procedure Expand_Wide_Wide_Image_Attribute (N : Node_Id) is
      Loc  : constant Source_Ptr := Sloc (N);
      Pref : constant Entity_Id  := Prefix (N);
      Rnn  : constant Entity_Id  := Make_Temporary (Loc, 'S');
      Lnn  : constant Entity_Id  := Make_Temporary (Loc, 'P');
      Rtyp : Entity_Id;

   begin
      if Is_Object_Image (Pref) then
         Rewrite_Object_Image
           (N, Pref, Name_Wide_Wide_Image, Standard_Wide_Wide_String);
         return;
      end if;

      Rtyp := Root_Type (Entity (Pref));

      Insert_Actions (N, New_List (

         --  Rnn : Wide_Wide_String (1 .. rt'Wide_Wide_Width);

         Make_Object_Declaration (Loc,
            Defining_Identifier => Rnn,
            Object_Definition   =>
              Make_Subtype_Indication (Loc,
                Subtype_Mark =>
                  New_Occurrence_Of (Standard_Wide_Wide_String, Loc),
                Constraint   =>
                  Make_Index_Or_Discriminant_Constraint (Loc,
                    Constraints => New_List (
                      Make_Range (Loc,
                        Low_Bound  => Make_Integer_Literal (Loc, 1),
                        High_Bound =>
                          Make_Attribute_Reference (Loc,
                            Prefix         => New_Occurrence_Of (Rtyp, Loc),
                            Attribute_Name => Name_Wide_Wide_Width)))))),

         --  Lnn : Natural;

         Make_Object_Declaration (Loc,
           Defining_Identifier => Lnn,
           Object_Definition   => New_Occurrence_Of (Standard_Natural, Loc)),

         --    String_To_Wide_Wide_String
         --      (typ'Image (X), Rnn, Lnn, Wide_Character_Encoding_Method);

         Make_Procedure_Call_Statement (Loc,
           Name =>
             New_Occurrence_Of (RTE (RE_String_To_Wide_Wide_String), Loc),

           Parameter_Associations => New_List (
             Make_Attribute_Reference (Loc,
               Prefix         => Prefix (N),
               Attribute_Name => Name_Image,
               Expressions    => Expressions (N)),
             New_Occurrence_Of (Rnn, Loc),
             New_Occurrence_Of (Lnn, Loc),
             Make_Integer_Literal (Loc,
               Intval => Int (Wide_Character_Encoding_Method))))),

         --  Suppress checks because we know everything is properly in range

         Suppress => All_Checks);

      --  Final step is to rewrite the expression as a slice and analyze,
      --  again with no checks, since we are sure that everything is OK.

      Rewrite (N,
        Make_Slice (Loc,
          Prefix         => New_Occurrence_Of (Rnn, Loc),
          Discrete_Range =>
            Make_Range (Loc,
              Low_Bound  => Make_Integer_Literal (Loc, 1),
              High_Bound => New_Occurrence_Of (Lnn, Loc))));

      Analyze_And_Resolve
        (N, Standard_Wide_Wide_String, Suppress => All_Checks);
   end Expand_Wide_Wide_Image_Attribute;

   ----------------------------
   -- Expand_Width_Attribute --
   ----------------------------

   --  The processing here also handles the case of Wide_[Wide_]Width. With the
   --  exceptions noted, the processing is identical

   --  For scalar types derived from Boolean, character and integer types
   --  in package Standard. Note that the Width attribute is computed at
   --  compile time for all cases except those involving non-static sub-
   --  types. For such subtypes, typ'[Wide_[Wide_]]Width expands into:

   --    Result_Type (xx (yy (Ptyp'First), yy (Ptyp'Last)))

   --  where

   --    For types whose root type is Character
   --      xx = Width_Character
   --      yy = Character

   --    For types whose root type is Wide_Character
   --      xx = Wide_Width_Character
   --      yy = Character

   --    For types whose root type is Wide_Wide_Character
   --      xx = Wide_Wide_Width_Character
   --      yy = Character

   --    For types whose root type is Boolean
   --      xx = Width_Boolean
   --      yy = Boolean

   --    For signed integer types
   --      xx = Width_Long_Long_Integer
   --      yy = Long_Long_Integer

   --    For modular integer types
   --      xx = Width_Long_Long_Unsigned
   --      yy = Long_Long_Unsigned

   --  For types derived from Wide_Character, typ'Width expands into

   --    Result_Type (Width_Wide_Character (
   --      Wide_Character (typ'First),
   --      Wide_Character (typ'Last),

   --  and typ'Wide_Width expands into:

   --    Result_Type (Wide_Width_Wide_Character (
   --      Wide_Character (typ'First),
   --      Wide_Character (typ'Last));

   --  and typ'Wide_Wide_Width expands into

   --    Result_Type (Wide_Wide_Width_Wide_Character (
   --      Wide_Character (typ'First),
   --      Wide_Character (typ'Last));

   --  For types derived from Wide_Wide_Character, typ'Width expands into

   --    Result_Type (Width_Wide_Wide_Character (
   --      Wide_Wide_Character (typ'First),
   --      Wide_Wide_Character (typ'Last),

   --  and typ'Wide_Width expands into:

   --    Result_Type (Wide_Width_Wide_Wide_Character (
   --      Wide_Wide_Character (typ'First),
   --      Wide_Wide_Character (typ'Last));

   --  and typ'Wide_Wide_Width expands into

   --    Result_Type (Wide_Wide_Width_Wide_Wide_Char (
   --      Wide_Wide_Character (typ'First),
   --      Wide_Wide_Character (typ'Last));

   --  For real types, typ'Width and typ'Wide_[Wide_]Width expand into

   --    if Ptyp'First > Ptyp'Last then 0 else btyp'Width end if

   --  where btyp is the base type. This looks recursive but it isn't
   --  because the base type is always static, and hence the expression
   --  in the else is reduced to an integer literal.

   --  For user-defined enumeration types, typ'Width expands into

   --    Result_Type (Width_Enumeration_NN
   --                  (typS,
   --                   typI'Address,
   --                   typ'Pos (typ'First),
   --                   typ'Pos (Typ'Last)));

   --  and typ'Wide_Width expands into:

   --    Result_Type (Wide_Width_Enumeration_NN
   --                  (typS,
   --                   typI,
   --                   typ'Pos (typ'First),
   --                   typ'Pos (Typ'Last))
   --                   Wide_Character_Encoding_Method);

   --  and typ'Wide_Wide_Width expands into:

   --    Result_Type (Wide_Wide_Width_Enumeration_NN
   --                  (typS,
   --                   typI,
   --                   typ'Pos (typ'First),
   --                   typ'Pos (Typ'Last))
   --                   Wide_Character_Encoding_Method);

   --  where typS and typI are the enumeration image strings and indexes
   --  table, as described in Build_Enumeration_Image_Tables. NN is 8/16/32
   --  for depending on the element type for typI.

   --  Finally if Discard_Names is in effect for an enumeration type, then
   --  a special if expression is built that yields the space needed for the
   --  decimal representation of the largest pos value in the subtype. See
   --  code below for details.

   procedure Expand_Width_Attribute (N : Node_Id; Attr : Atype := Normal) is
      Loc     : constant Source_Ptr := Sloc (N);
      Typ     : constant Entity_Id  := Etype (N);
      Pref    : constant Node_Id    := Prefix (N);
      Ptyp    : constant Entity_Id  := Etype (Pref);
      Rtyp    : constant Entity_Id  := Root_Type (Ptyp);
      Arglist : List_Id;
      Ttyp    : Entity_Id;
      XX      : RE_Id;
      YY      : Entity_Id;

   begin
      --  Types derived from Standard.Boolean

      if Rtyp = Standard_Boolean then
         XX := RE_Width_Boolean;
         YY := Rtyp;

      --  Types derived from Standard.Character

      elsif Rtyp = Standard_Character then
         case Attr is
            when Normal    => XX := RE_Width_Character;
            when Wide      => XX := RE_Wide_Width_Character;
            when Wide_Wide => XX := RE_Wide_Wide_Width_Character;
         end case;

         YY := Rtyp;

      --  Types derived from Standard.Wide_Character

      elsif Rtyp = Standard_Wide_Character then
         case Attr is
            when Normal    => XX := RE_Width_Wide_Character;
            when Wide      => XX := RE_Wide_Width_Wide_Character;
            when Wide_Wide => XX := RE_Wide_Wide_Width_Wide_Character;
         end case;

         YY := Rtyp;

      --  Types derived from Standard.Wide_Wide_Character

      elsif Rtyp = Standard_Wide_Wide_Character then
         case Attr is
            when Normal    => XX := RE_Width_Wide_Wide_Character;
            when Wide      => XX := RE_Wide_Width_Wide_Wide_Character;
            when Wide_Wide => XX := RE_Wide_Wide_Width_Wide_Wide_Char;
         end case;

         YY := Rtyp;

      --  Signed integer types

      elsif Is_Signed_Integer_Type (Rtyp) then
         XX := RE_Width_Long_Long_Integer;
         YY := Standard_Long_Long_Integer;

      --  Modular integer types

      elsif Is_Modular_Integer_Type (Rtyp) then
         XX := RE_Width_Long_Long_Unsigned;
         YY := RTE (RE_Long_Long_Unsigned);

      --  Real types

      elsif Is_Real_Type (Rtyp) then
         Rewrite (N,
           Make_If_Expression (Loc,
             Expressions => New_List (

               Make_Op_Gt (Loc,
                 Left_Opnd =>
                   Make_Attribute_Reference (Loc,
                     Prefix => New_Occurrence_Of (Ptyp, Loc),
                     Attribute_Name => Name_First),

                 Right_Opnd =>
                   Make_Attribute_Reference (Loc,
                     Prefix => New_Occurrence_Of (Ptyp, Loc),
                     Attribute_Name => Name_Last)),

               Make_Integer_Literal (Loc, 0),

               Make_Attribute_Reference (Loc,
                 Prefix => New_Occurrence_Of (Base_Type (Ptyp), Loc),
                 Attribute_Name => Name_Width))));

         Analyze_And_Resolve (N, Typ);
         return;

      --  User-defined enumeration types

      else
         pragma Assert (Is_Enumeration_Type (Rtyp));

         --  Whenever pragma Discard_Names is in effect, the value we need
         --  is the value needed to accommodate the largest integer pos value
         --  in the range of the subtype + 1 for the space at the start. We
         --  build:

         --     Tnn : constant Integer := Rtyp'Pos (Ptyp'Last)

         --  and replace the expression by

         --     (if Ptyp'Range_Length = 0 then 0
         --      else (if Tnn < 10 then 2
         --            else (if Tnn < 100 then 3
         --                  ...
         --                      else n)))...

         --  where n is equal to Rtyp'Pos (Ptyp'Last) + 1

         --  Note: The above processing is in accordance with the intent of
         --  the RM, which is that Width should be related to the impl-defined
         --  behavior of Image. It is not clear what this means if Image is
         --  not defined (as in the configurable run-time case for GNAT) and
         --  gives an error at compile time.

         --  We choose in this case to just go ahead and implement Width the
         --  same way, returning what Image would have returned if it has been
         --  available in the configurable run-time library.

         if Discard_Names (Rtyp) then
            declare
               Tnn   : constant Entity_Id := Make_Temporary (Loc, 'T');
               Cexpr : Node_Id;
               P     : Int;
               M     : Int;
               K     : Int;

            begin
               Insert_Action (N,
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Tnn,
                   Constant_Present    => True,
                   Object_Definition   =>
                     New_Occurrence_Of (Standard_Integer, Loc),
                   Expression =>
                     Make_Attribute_Reference (Loc,
                       Prefix         => New_Occurrence_Of (Rtyp, Loc),
                       Attribute_Name => Name_Pos,
                       Expressions    => New_List (
                         Convert_To (Rtyp,
                           Make_Attribute_Reference (Loc,
                             Prefix         => New_Occurrence_Of (Ptyp, Loc),
                             Attribute_Name => Name_Last))))));

               --  OK, now we need to build the if expression. First get the
               --  value of M, the largest possible value needed.

               P := UI_To_Int
                      (Enumeration_Pos (Entity (Type_High_Bound (Rtyp))));

               K := 1;
               M := 1;
               while M < P loop
                  M := M * 10;
                  K := K + 1;
               end loop;

               --  Build inner else

               Cexpr := Make_Integer_Literal (Loc, K);

               --  Wrap in inner if's until counted down to 2

               while K > 2 loop
                  M := M / 10;
                  K := K - 1;

                  Cexpr :=
                    Make_If_Expression (Loc,
                      Expressions => New_List (
                        Make_Op_Lt (Loc,
                          Left_Opnd  => New_Occurrence_Of (Tnn, Loc),
                          Right_Opnd => Make_Integer_Literal (Loc, M)),
                        Make_Integer_Literal (Loc, K),
                        Cexpr));
               end loop;

               --  Add initial comparison for null range and we are done, so
               --  rewrite the attribute occurrence with this expression.

               Rewrite (N,
                 Convert_To (Typ,
                   Make_If_Expression (Loc,
                     Expressions => New_List (
                       Make_Op_Eq (Loc,
                         Left_Opnd  =>
                           Make_Attribute_Reference (Loc,
                             Prefix         => New_Occurrence_Of (Ptyp, Loc),
                             Attribute_Name => Name_Range_Length),
                         Right_Opnd => Make_Integer_Literal (Loc, 0)),
                       Make_Integer_Literal (Loc, 0),
                       Cexpr))));

               Analyze_And_Resolve (N, Typ);
               return;
            end;
         end if;

         --  Normal case, not Discard_Names

         Ttyp := Component_Type (Etype (Lit_Indexes (Rtyp)));

         case Attr is
            when Normal =>
               if Ttyp = Standard_Integer_8 then
                  XX := RE_Width_Enumeration_8;
               elsif Ttyp = Standard_Integer_16 then
                  XX := RE_Width_Enumeration_16;
               else
                  XX := RE_Width_Enumeration_32;
               end if;

            when Wide =>
               if Ttyp = Standard_Integer_8 then
                  XX := RE_Wide_Width_Enumeration_8;
               elsif Ttyp = Standard_Integer_16 then
                  XX := RE_Wide_Width_Enumeration_16;
               else
                  XX := RE_Wide_Width_Enumeration_32;
               end if;

            when Wide_Wide =>
               if Ttyp = Standard_Integer_8 then
                  XX := RE_Wide_Wide_Width_Enumeration_8;
               elsif Ttyp = Standard_Integer_16 then
                  XX := RE_Wide_Wide_Width_Enumeration_16;
               else
                  XX := RE_Wide_Wide_Width_Enumeration_32;
               end if;
         end case;

         Arglist :=
           New_List (
             New_Occurrence_Of (Lit_Strings (Rtyp), Loc),

             Make_Attribute_Reference (Loc,
               Prefix => New_Occurrence_Of (Lit_Indexes (Rtyp), Loc),
               Attribute_Name => Name_Address),

             Make_Attribute_Reference (Loc,
               Prefix => New_Occurrence_Of (Ptyp, Loc),
               Attribute_Name => Name_Pos,

               Expressions => New_List (
                 Make_Attribute_Reference (Loc,
                   Prefix => New_Occurrence_Of (Ptyp, Loc),
                   Attribute_Name => Name_First))),

             Make_Attribute_Reference (Loc,
               Prefix => New_Occurrence_Of (Ptyp, Loc),
               Attribute_Name => Name_Pos,

               Expressions => New_List (
                 Make_Attribute_Reference (Loc,
                   Prefix => New_Occurrence_Of (Ptyp, Loc),
                   Attribute_Name => Name_Last))));

         Rewrite (N,
           Convert_To (Typ,
             Make_Function_Call (Loc,
               Name => New_Occurrence_Of (RTE (XX), Loc),
               Parameter_Associations => Arglist)));

         Analyze_And_Resolve (N, Typ);
         return;
      end if;

      --  If we fall through XX and YY are set

      Arglist := New_List (
        Convert_To (YY,
          Make_Attribute_Reference (Loc,
            Prefix => New_Occurrence_Of (Ptyp, Loc),
            Attribute_Name => Name_First)),

        Convert_To (YY,
          Make_Attribute_Reference (Loc,
            Prefix => New_Occurrence_Of (Ptyp, Loc),
            Attribute_Name => Name_Last)));

      Rewrite (N,
        Convert_To (Typ,
          Make_Function_Call (Loc,
            Name => New_Occurrence_Of (RTE (XX), Loc),
            Parameter_Associations => Arglist)));

      Analyze_And_Resolve (N, Typ);
   end Expand_Width_Attribute;

   -----------------------
   -- Has_Decimal_Small --
   -----------------------

   function Has_Decimal_Small (E : Entity_Id) return Boolean is
   begin
      return Is_Decimal_Fixed_Point_Type (E)
        or else
          (Is_Ordinary_Fixed_Point_Type (E)
             and then Ureal_10**Aft_Value (E) * Small_Value (E) = Ureal_1);
   end Has_Decimal_Small;

   --------------------------
   -- Rewrite_Object_Image --
   --------------------------

   procedure Rewrite_Object_Image
     (N         : Node_Id;
      Pref      : Entity_Id;
      Attr_Name : Name_Id;
      Str_Typ   : Entity_Id)
   is
   begin
      Rewrite (N,
        Make_Attribute_Reference (Sloc (N),
          Prefix         => New_Occurrence_Of (Etype (Pref), Sloc (N)),
          Attribute_Name => Attr_Name,
          Expressions    => New_List (Relocate_Node (Pref))));

      Analyze_And_Resolve (N, Str_Typ);
   end Rewrite_Object_Image;
end Exp_Imgv;
