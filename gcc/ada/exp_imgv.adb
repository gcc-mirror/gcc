------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ I M G V                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2001-2010, Free Software Foundation, Inc.         --
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

   ------------------------------------
   -- Build_Enumeration_Image_Tables --
   ------------------------------------

   procedure Build_Enumeration_Image_Tables (E : Entity_Id; N : Node_Id) is
      Loc  : constant Source_Ptr := Sloc (E);
      Str  : String_Id;
      Ind  : List_Id;
      Lit  : Entity_Id;
      Nlit : Nat;
      Len  : Nat;
      Estr : Entity_Id;
      Eind : Entity_Id;
      Ityp : Node_Id;

   begin
      --  Nothing to do for other than a root enumeration type

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
   end Build_Enumeration_Image_Tables;

   ----------------------------
   -- Expand_Image_Attribute --
   ----------------------------

   --  For all cases other than user defined enumeration types, the scheme
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

   --    Missing ???

   procedure Expand_Image_Attribute (N : Node_Id) is
      Loc       : constant Source_Ptr := Sloc (N);
      Exprs     : constant List_Id    := Expressions (N);
      Pref      : constant Node_Id    := Prefix (N);
      Ptyp      : constant Entity_Id  := Entity (Pref);
      Rtyp      : constant Entity_Id  := Root_Type (Ptyp);
      Expr      : constant Node_Id    := Relocate_Node (First (Exprs));
      Imid      : RE_Id;
      Tent      : Entity_Id;
      Ttyp      : Entity_Id;
      Proc_Ent  : Entity_Id;
      Enum_Case : Boolean;

      Arg_List : List_Id;
      --  List of arguments for run-time procedure call

      Ins_List : List_Id;
      --  List of actions to be inserted

      Snn : constant Entity_Id := Make_Temporary (Loc, 'S');
      Pnn : constant Entity_Id := Make_Temporary (Loc, 'P');

   begin
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

      elsif Rtyp = Standard_Character then
         Imid := RE_Image_Character;
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

      --  Only other possibility is user defined enumeration type

      else
         if Discard_Names (First_Subtype (Ptyp))
           or else No (Lit_Strings (Root_Type (Ptyp)))
         then
            --  When pragma Discard_Names applies to the first subtype, build
            --  (Pref'Pos)'Img.

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
             Prefix         => New_Reference_To (Ptyp, Loc),
             Attribute_Name => Name_Digits));

      --  For ordinary fixed-point types, append Aft parameter

      elsif Is_Ordinary_Fixed_Point_Type (Rtyp) then
         Append_To (Arg_List,
           Make_Attribute_Reference (Loc,
             Prefix         => New_Reference_To (Ptyp, Loc),
             Attribute_Name => Name_Aft));

         if Has_Decimal_Small (Rtyp) then
            Set_Conversion_OK (First (Arg_List));
            Set_Etype (First (Arg_List), Tent);
         end if;

      --  For decimal, append Scale and also set to do literal conversion

      elsif Is_Decimal_Fixed_Point_Type (Rtyp) then
         Append_To (Arg_List,
           Make_Attribute_Reference (Loc,
             Prefix         => New_Reference_To (Ptyp, Loc),
             Attribute_Name => Name_Scale));

         Set_Conversion_OK (First (Arg_List));
         Set_Etype (First (Arg_List), Tent);

      --  For Wide_Character, append Ada 2005 indication

      elsif Rtyp = Standard_Wide_Character then
         Append_To (Arg_List,
           New_Reference_To (Boolean_Literals (Ada_Version >= Ada_05), Loc));
      end if;

      --  Now append the procedure call to the insert list

      Append_To (Ins_List,
         Make_Procedure_Call_Statement (Loc,
          Name                   => New_Reference_To (Proc_Ent, Loc),
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
             Prefix => New_Reference_To (Typ, Loc),
             Attribute_Name => Name_Scale));

         Rewrite (N,
           OK_Convert_To (Btyp,
             Make_Function_Call (Loc,
               Name => New_Reference_To (RTE (Vid), Loc),
               Parameter_Associations => Args)));

         Set_Etype (N, Btyp);
         Analyze_And_Resolve (N, Btyp);
         return;

      elsif Is_Real_Type (Rtyp) then
         Vid := RE_Value_Real;

      --  Only other possibility is user defined enumeration type

      else
         pragma Assert (Is_Enumeration_Type (Rtyp));

         --  Case of pragma Discard_Names, transform the Value
         --  attribute to Btyp'Val (Long_Long_Integer'Value (Args))

         if Discard_Names (First_Subtype (Typ))
           or else No (Lit_Strings (Rtyp))
         then
            Rewrite (N,
              Make_Attribute_Reference (Loc,
                Prefix => New_Reference_To (Btyp, Loc),
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
            elsif Ttyp = Standard_Integer_16  then
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
                Prefix => New_Reference_To (Typ, Loc),
                Attribute_Name => Name_Val,
                Expressions => New_List (
                  Make_Function_Call (Loc,
                    Name =>
                      New_Reference_To (RTE (Func), Loc),
                    Parameter_Associations => Args))));

            Analyze_And_Resolve (N, Btyp);
         end if;

         return;
      end if;

      --  Fall through for all cases except user defined enumeration type
      --  and decimal types, with Vid set to the Id of the entity for the
      --  Value routine and Args set to the list of parameters for the call.

      --  Compiling package Ada.Tags under No_Run_Time_Mode we disable the
      --  expansion of the attribute into the function call statement to avoid
      --  generating spurious errors caused by the use of Integer_Address'Value
      --  in our implementation of Ada.Tags.Internal_Tag

      --  Seems like a bit of a kludge, there should be a better way ???

      --  There is a better way, you should also test RTE_Available ???

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
               Name => New_Reference_To (RTE (Vid), Loc),
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
      Rtyp : constant Entity_Id  := Root_Type (Entity (Prefix (N)));
      Rnn  : constant Entity_Id := Make_Temporary (Loc, 'S');
      Lnn  : constant Entity_Id := Make_Temporary (Loc, 'P');

   begin
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
             New_Reference_To (RTE (RE_String_To_Wide_String), Loc),

           Parameter_Associations => New_List (
             Make_Attribute_Reference (Loc,
               Prefix         => Prefix (N),
               Attribute_Name => Name_Image,
               Expressions    => Expressions (N)),
             New_Reference_To (Rnn, Loc),
             New_Reference_To (Lnn, Loc),
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
      Rtyp : constant Entity_Id  := Root_Type (Entity (Prefix (N)));

      Rnn : constant Entity_Id := Make_Temporary (Loc, 'S');
      Lnn : constant Entity_Id := Make_Temporary (Loc, 'P');

   begin
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
             New_Reference_To (RTE (RE_String_To_Wide_Wide_String), Loc),

           Parameter_Associations => New_List (
             Make_Attribute_Reference (Loc,
               Prefix         => Prefix (N),
               Attribute_Name => Name_Image,
               Expressions    => Expressions (N)),
             New_Reference_To (Rnn, Loc),
             New_Reference_To (Lnn, Loc),
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

   --  For user defined enumeration types, typ'Width expands into

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

   --  where typS and typI are the enumeration image strings and
   --  indexes table, as described in Build_Enumeration_Image_Tables.
   --  NN is 8/16/32 for depending on the element type for typI.

   procedure Expand_Width_Attribute (N : Node_Id; Attr : Atype := Normal) is
      Loc     : constant Source_Ptr := Sloc (N);
      Typ     : constant Entity_Id  := Etype (N);
      Pref    : constant Node_Id    := Prefix (N);
      Ptyp    : constant Entity_Id  := Etype (Pref);
      Rtyp    : constant Entity_Id  := Root_Type (Ptyp);
      XX      : RE_Id;
      YY      : Entity_Id;
      Arglist : List_Id;
      Ttyp    : Entity_Id;

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
           Make_Conditional_Expression (Loc,
             Expressions => New_List (

               Make_Op_Gt (Loc,
                 Left_Opnd =>
                   Make_Attribute_Reference (Loc,
                     Prefix => New_Reference_To (Ptyp, Loc),
                     Attribute_Name => Name_First),

                 Right_Opnd =>
                   Make_Attribute_Reference (Loc,
                     Prefix => New_Reference_To (Ptyp, Loc),
                     Attribute_Name => Name_Last)),

               Make_Integer_Literal (Loc, 0),

               Make_Attribute_Reference (Loc,
                 Prefix => New_Reference_To (Base_Type (Ptyp), Loc),
                 Attribute_Name => Name_Width))));

         Analyze_And_Resolve (N, Typ);
         return;

      --  User defined enumeration types

      else
         pragma Assert (Is_Enumeration_Type (Rtyp));

         if Discard_Names (Rtyp) then

            --  This is a configurable run-time, or else a restriction is in
            --  effect. In either case the attribute cannot be supported. Force
            --  a load error from Rtsfind to generate an appropriate message,
            --  as is done with other ZFP violations.

            declare
               Discard : constant Entity_Id := RTE (RE_Null);
               pragma Unreferenced (Discard);
            begin
               return;
            end;
         end if;

         Ttyp := Component_Type (Etype (Lit_Indexes (Rtyp)));

         case Attr is
            when Normal =>
               if Ttyp = Standard_Integer_8 then
                  XX := RE_Width_Enumeration_8;
               elsif Ttyp = Standard_Integer_16  then
                  XX := RE_Width_Enumeration_16;
               else
                  XX := RE_Width_Enumeration_32;
               end if;

            when Wide =>
               if Ttyp = Standard_Integer_8 then
                  XX := RE_Wide_Width_Enumeration_8;
               elsif Ttyp = Standard_Integer_16  then
                  XX := RE_Wide_Width_Enumeration_16;
               else
                  XX := RE_Wide_Width_Enumeration_32;
               end if;

            when Wide_Wide =>
               if Ttyp = Standard_Integer_8 then
                  XX := RE_Wide_Wide_Width_Enumeration_8;
               elsif Ttyp = Standard_Integer_16  then
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
               Prefix => New_Reference_To (Ptyp, Loc),
               Attribute_Name => Name_Pos,

               Expressions => New_List (
                 Make_Attribute_Reference (Loc,
                   Prefix => New_Reference_To (Ptyp, Loc),
                   Attribute_Name => Name_First))),

             Make_Attribute_Reference (Loc,
               Prefix => New_Reference_To (Ptyp, Loc),
               Attribute_Name => Name_Pos,

               Expressions => New_List (
                 Make_Attribute_Reference (Loc,
                   Prefix => New_Reference_To (Ptyp, Loc),
                   Attribute_Name => Name_Last))));

         Rewrite (N,
           Convert_To (Typ,
             Make_Function_Call (Loc,
               Name => New_Reference_To (RTE (XX), Loc),
               Parameter_Associations => Arglist)));

         Analyze_And_Resolve (N, Typ);
         return;
      end if;

      --  If we fall through XX and YY are set

      Arglist := New_List (
        Convert_To (YY,
          Make_Attribute_Reference (Loc,
            Prefix => New_Reference_To (Ptyp, Loc),
            Attribute_Name => Name_First)),

        Convert_To (YY,
          Make_Attribute_Reference (Loc,
            Prefix => New_Reference_To (Ptyp, Loc),
            Attribute_Name => Name_Last)));

      Rewrite (N,
        Convert_To (Typ,
          Make_Function_Call (Loc,
            Name => New_Reference_To (RTE (XX), Loc),
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

end Exp_Imgv;
