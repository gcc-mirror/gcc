------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ D B U G                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1996-2005 Free Software Foundation, Inc.          --
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

with Alloc;    use Alloc;
with Atree;    use Atree;
with Debug;    use Debug;
with Einfo;    use Einfo;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;
with Output;   use Output;
with Sem_Eval; use Sem_Eval;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Stand;    use Stand;
with Stringt;  use Stringt;
with Table;
with Urealp;   use Urealp;

package body Exp_Dbug is

   --  The following table is used to queue up the entities passed as
   --  arguments to Qualify_Entity_Names for later processing when
   --  Qualify_All_Entity_Names is called.

   package Name_Qualify_Units is new Table.Table (
     Table_Component_Type => Node_Id,
     Table_Index_Type     => Nat,
     Table_Low_Bound      => 1,
     Table_Initial        => Alloc.Name_Qualify_Units_Initial,
     Table_Increment      => Alloc.Name_Qualify_Units_Increment,
     Table_Name           => "Name_Qualify_Units");

   --------------------------------
   -- Use of Qualification Flags --
   --------------------------------

   --  There are two flags used to keep track of qualification of entities

   --    Has_Fully_Qualified_Name
   --    Has_Qualified_Name

   --  The difference between these is as follows. Has_Qualified_Name is
   --  set to indicate that the name has been qualified as required by the
   --  spec of this package. As described there, this may involve the full
   --  qualification for the name, but for some entities, notably procedure
   --  local variables, this full qualification is not required.

   --  The flag Has_Fully_Qualified_Name is set if indeed the name has been
   --  fully qualified in the Ada sense. If Has_Fully_Qualified_Name is set,
   --  then Has_Qualified_Name is also set, but the other way round is not
   --  the case.

   --  Consider the following example:

   --     with ...
   --     procedure X is
   --       B : Ddd.Ttt;
   --       procedure Y is ..

   --  Here B is a procedure local variable, so it does not need fully
   --  qualification. The flag Has_Qualified_Name will be set on the
   --  first attempt to qualify B, to indicate that the job is done
   --  and need not be redone.

   --  But Y is qualified as x__y, since procedures are always fully
   --  qualified, so the first time that an attempt is made to qualify
   --  the name y, it will be replaced by x__y, and both flags are set.

   --  Why the two flags? Well there are cases where we derive type names
   --  from object names. As noted in the spec, type names are always
   --  fully qualified. Suppose for example that the backend has to build
   --  a padded type for variable B. then it will construct the PAD name
   --  from B, but it requires full qualification, so the fully qualified
   --  type name will be x__b___PAD. The two flags allow the circuit for
   --  building this name to realize efficiently that b needs further
   --  qualification.

   --------------------
   -- Homonym_Suffix --
   --------------------

   --  The string defined here (and its associated length) is used to
   --  gather the homonym string that will be appended to Name_Buffer
   --  when the name is complete. Strip_Suffixes appends to this string
   --  as does Append_Homonym_Number, and Output_Homonym_Numbers_Suffix
   --  appends the string to the end of Name_Buffer.

   Homonym_Numbers : String (1 .. 256);
   Homonym_Len     : Natural := 0;

   ----------------------
   -- Local Procedures --
   ----------------------

   procedure Add_Uint_To_Buffer (U : Uint);
   --  Add image of universal integer to Name_Buffer, updating Name_Len

   procedure Add_Real_To_Buffer (U : Ureal);
   --  Add nnn_ddd to Name_Buffer, where nnn and ddd are integer values of
   --  the normalized numerator and denominator of the given real value.

   procedure Append_Homonym_Number (E : Entity_Id);
   --  If the entity E has homonyms in the same scope, then make an entry
   --  in the Homonym_Numbers array, bumping Homonym_Count accordingly.

   function Bounds_Match_Size (E : Entity_Id) return  Boolean;
   --  Determine whether the bounds of E match the size of the type. This is
   --  used to determine whether encoding is required for a discrete type.

   procedure Output_Homonym_Numbers_Suffix;
   --  If homonym numbers are stored, then output them into Name_Buffer.

   procedure Prepend_String_To_Buffer (S : String);
   --  Prepend given string to the contents of the string buffer, updating
   --  the value in Name_Len (i.e. string is added at start of buffer).

   procedure Prepend_Uint_To_Buffer (U : Uint);
   --  Prepend image of universal integer to Name_Buffer, updating Name_Len

   procedure Qualify_Entity_Name (Ent : Entity_Id);
   --  If not already done, replaces the Chars field of the given entity
   --  with the appropriate fully qualified name.

   procedure Strip_Suffixes (BNPE_Suffix_Found : in out Boolean);
   --  Given an qualified entity name in Name_Buffer, remove any plain X or
   --  X{nb} qualification suffix. The contents of Name_Buffer is not changed
   --  but Name_Len may be adjusted on return to remove the suffix. If a
   --  BNPE suffix is found and stripped, then BNPE_Suffix_Found is set to
   --  True. If no suffix is found, then BNPE_Suffix_Found is not modified.
   --  This routine also searches for a homonym suffix, and if one is found
   --  it is also stripped, and the entries are added to the global homonym
   --  list (Homonym_Numbers) so that they can later be put back.

   ------------------------
   -- Add_Real_To_Buffer --
   ------------------------

   procedure Add_Real_To_Buffer (U : Ureal) is
   begin
      Add_Uint_To_Buffer (Norm_Num (U));
      Add_Str_To_Name_Buffer ("_");
      Add_Uint_To_Buffer (Norm_Den (U));
   end Add_Real_To_Buffer;

   ------------------------
   -- Add_Uint_To_Buffer --
   ------------------------

   procedure Add_Uint_To_Buffer (U : Uint) is
   begin
      if U < 0 then
         Add_Uint_To_Buffer (-U);
         Add_Char_To_Name_Buffer ('m');
      else
         UI_Image (U, Decimal);
         Add_Str_To_Name_Buffer (UI_Image_Buffer (1 .. UI_Image_Length));
      end if;
   end Add_Uint_To_Buffer;

   ---------------------------
   -- Append_Homonym_Number --
   ---------------------------

   procedure Append_Homonym_Number (E : Entity_Id) is

      procedure Add_Nat_To_H (Nr : Nat);
      --  Little procedure to append Nr to Homonym_Numbers

      ------------------
      -- Add_Nat_To_H --
      ------------------

      procedure Add_Nat_To_H (Nr : Nat) is
      begin
         if Nr >= 10 then
            Add_Nat_To_H (Nr / 10);
         end if;

         Homonym_Len := Homonym_Len + 1;
         Homonym_Numbers (Homonym_Len) :=
           Character'Val (Nr mod 10 + Character'Pos ('0'));
      end Add_Nat_To_H;

   --  Start of processing for Append_Homonym_Number

   begin
      if Has_Homonym (E) then
         declare
            H  : Entity_Id := Homonym (E);
            Nr : Nat := 1;

         begin
            while Present (H) loop
               if Scope (H) = Scope (E) then
                  Nr := Nr + 1;
               end if;

               H := Homonym (H);
            end loop;

            if Homonym_Len > 0 then
               Homonym_Len := Homonym_Len + 1;
               Homonym_Numbers (Homonym_Len) := '_';
            end if;

            Add_Nat_To_H (Nr);
         end;
      end if;
   end Append_Homonym_Number;

   -----------------------
   -- Bounds_Match_Size --
   -----------------------

   function Bounds_Match_Size (E : Entity_Id) return Boolean is
      Siz : Uint;

   begin
      if not Is_OK_Static_Subtype (E) then
         return False;

      elsif Is_Integer_Type (E)
        and then Subtypes_Statically_Match (E, Base_Type (E))
      then
         return True;

      --  Here we check if the static bounds match the natural size, which
      --  is the size passed through with the debugging information. This
      --  is the Esize rounded up to 8, 16, 32 or 64 as appropriate.

      else
         declare
            Umark  : constant Uintp.Save_Mark := Uintp.Mark;
            Result : Boolean;

         begin
            if Esize (E) <= 8 then
               Siz := Uint_8;
            elsif Esize (E) <= 16 then
               Siz := Uint_16;
            elsif Esize (E) <= 32 then
               Siz := Uint_32;
            else
               Siz := Uint_64;
            end if;

            if Is_Modular_Integer_Type (E) or else Is_Enumeration_Type (E) then
               Result :=
                 Expr_Rep_Value (Type_Low_Bound (E)) = 0
                   and then
                 2 ** Siz - Expr_Rep_Value (Type_High_Bound (E)) = 1;

            else
               Result :=
                 Expr_Rep_Value (Type_Low_Bound (E)) + 2 ** (Siz - 1) = 0
                   and then
                 2 ** (Siz - 1) - Expr_Rep_Value (Type_High_Bound (E)) = 1;
            end if;

            Release (Umark);
            return Result;
         end;
      end if;
   end Bounds_Match_Size;

   --------------------------------
   -- Debug_Renaming_Declaration --
   --------------------------------

   function Debug_Renaming_Declaration (N : Node_Id) return Node_Id is
      Loc : constant Source_Ptr := Sloc (N);
      Ent : constant Node_Id    := Defining_Entity (N);
      Nam : constant Node_Id    := Name (N);
      Rnm : Name_Id;
      Ren : Node_Id;
      Lit : Entity_Id;
      Typ : Entity_Id;
      Res : Node_Id;
      Def : Entity_Id;

      function Output_Subscript (N : Node_Id; S : String) return Boolean;
      --  Outputs a single subscript value as ?nnn (subscript is compile
      --  time known value with value nnn) or as ?e (subscript is local
      --  constant with name e), where S supplies the proper string to
      --  use for ?. Returns False if the subscript is not of an appropriate
      --  type to output in one of these two forms. The result is prepended
      --  to the name stored in Name_Buffer.

      ----------------------
      -- Output_Subscript --
      ----------------------

      function Output_Subscript (N : Node_Id; S : String) return Boolean is
      begin
         if Compile_Time_Known_Value (N) then
            Prepend_Uint_To_Buffer (Expr_Value (N));

         elsif Nkind (N) = N_Identifier
           and then Scope (Entity (N)) = Scope (Ent)
           and then Ekind (Entity (N)) = E_Constant
         then
            Prepend_String_To_Buffer (Get_Name_String (Chars (Entity (N))));

         else
            return False;
         end if;

         Prepend_String_To_Buffer (S);
         return True;
      end Output_Subscript;

   --  Start of processing for Debug_Renaming_Declaration

   begin
      if not Comes_From_Source (N)
        and then not Needs_Debug_Info (Ent)
      then
         return Empty;
      end if;

      --  Prepare entity name for type declaration

      Get_Name_String (Chars (Ent));

      case Nkind (N) is
         when N_Object_Renaming_Declaration =>
            Add_Str_To_Name_Buffer ("___XR");

         when N_Exception_Renaming_Declaration =>
            Add_Str_To_Name_Buffer ("___XRE");

         when N_Package_Renaming_Declaration =>
            Add_Str_To_Name_Buffer ("___XRP");

            --  If it is a child unit create a fully qualified name,
            --  to disambiguate multiple child units with the same
            --  name and different parents.

            if Is_Child_Unit (Ent) then
               Prepend_String_To_Buffer ("__");
               Prepend_String_To_Buffer
                 (Get_Name_String (Chars (Scope (Ent))));
            end if;

         when others =>
            return Empty;
      end case;

      Rnm := Name_Find;

      --  Get renamed entity and compute suffix

      Name_Len := 0;
      Ren := Nam;
      loop
         case Nkind (Ren) is

            when N_Identifier =>
               exit;

            when N_Expanded_Name =>

               --  The entity field for an N_Expanded_Name is on the
               --  expanded name node itself, so we are done here too.

               exit;

            when N_Selected_Component =>
               Prepend_String_To_Buffer
                 (Get_Name_String (Chars (Selector_Name (Ren))));
               Prepend_String_To_Buffer ("XR");
               Ren := Prefix (Ren);

            when N_Indexed_Component =>
               declare
                  X : Node_Id := Last (Expressions (Ren));

               begin
                  while Present (X) loop
                     if not Output_Subscript (X, "XS") then
                        Set_Materialize_Entity (Ent);
                        return Empty;
                     end if;

                     Prev (X);
                  end loop;
               end;

               Ren := Prefix (Ren);

            when N_Slice =>

               Typ := Etype (First_Index (Etype (Nam)));

               if not Output_Subscript (Type_High_Bound (Typ), "XS") then
                  Set_Materialize_Entity (Ent);
                  return Empty;
               end if;

               if not Output_Subscript (Type_Low_Bound  (Typ), "XL") then
                  Set_Materialize_Entity (Ent);
                  return Empty;
               end if;

               Ren := Prefix (Ren);

            when N_Explicit_Dereference =>
               Prepend_String_To_Buffer ("XA");
               Ren := Prefix (Ren);

            --  For now, anything else simply results in no translation

            when others =>
               Set_Materialize_Entity (Ent);
               return Empty;
         end case;
      end loop;

      Prepend_String_To_Buffer ("___XE");

      --  For now, the literal name contains only the suffix. The Entity_Id
      --  value for the name is used to create a link from this literal name
      --  to the renamed entity using the Debug_Renaming_Link field. Then the
      --  Qualify_Entity_Name procedure uses this link to create the proper
      --  fully qualified name.

      --  The reason we do things this way is that we really need to copy the
      --  qualification of the renamed entity, and it is really much easier to
      --  do this after the renamed entity has itself been fully qualified.

      Lit := Make_Defining_Identifier (Loc, Chars => Name_Enter);
      Set_Debug_Renaming_Link (Lit, Entity (Ren));

      --  Return the appropriate enumeration type

      Def := Make_Defining_Identifier (Loc, Chars => Rnm);
      Res :=
        Make_Full_Type_Declaration (Loc,
          Defining_Identifier => Def,
          Type_Definition =>
            Make_Enumeration_Type_Definition (Loc,
              Literals => New_List (Lit)));

      Set_Needs_Debug_Info (Def);
      Set_Needs_Debug_Info (Lit);

      Set_Discard_Names (Defining_Identifier (Res));
      return Res;

   --  If we get an exception, just figure it is a case that we cannot
   --  successfully handle using our current approach, since this is
   --  only for debugging, no need to take the compilation with us!

   exception
      when others =>
         return Make_Null_Statement (Loc);
   end Debug_Renaming_Declaration;

   ----------------------
   -- Get_Encoded_Name --
   ----------------------

   --  Note: see spec for details on encodings

   procedure Get_Encoded_Name (E : Entity_Id) is
      Has_Suffix : Boolean;

   begin
      Get_Name_String (Chars (E));

      --  Nothing to do if we do not have a type

      if not Is_Type (E)

      --  Or if this is an enumeration base type

        or else (Is_Enumeration_Type (E)
                   and then E = Base_Type (E))

      --  Or if this is a dummy type for a renaming

        or else (Name_Len >= 3 and then
                   Name_Buffer (Name_Len - 2 .. Name_Len) = "_XR")

        or else (Name_Len >= 4 and then
                   (Name_Buffer (Name_Len - 3 .. Name_Len) = "_XRE"
                      or else
                    Name_Buffer (Name_Len - 3 .. Name_Len) = "_XRP"))

      --  For all these cases, just return the name unchanged

      then
         Name_Buffer (Name_Len + 1) := ASCII.Nul;
         return;
      end if;

      Has_Suffix := True;

      --  Fixed-point case

      if Is_Fixed_Point_Type (E) then
         Get_External_Name_With_Suffix (E, "XF_");
         Add_Real_To_Buffer (Delta_Value (E));

         if Small_Value (E) /= Delta_Value (E) then
            Add_Str_To_Name_Buffer ("_");
            Add_Real_To_Buffer (Small_Value (E));
         end if;

      --  Vax floating-point case

      elsif Vax_Float (E) then

         if Digits_Value (Base_Type (E)) = 6 then
            Get_External_Name_With_Suffix (E, "XFF");

         elsif Digits_Value (Base_Type (E)) = 9 then
            Get_External_Name_With_Suffix (E, "XFF");

         else
            pragma Assert (Digits_Value (Base_Type (E)) = 15);
            Get_External_Name_With_Suffix (E, "XFG");
         end if;

      --  Discrete case where bounds do not match size

      elsif Is_Discrete_Type (E)
        and then not Bounds_Match_Size (E)
      then
         if Has_Biased_Representation (E) then
            Get_External_Name_With_Suffix (E, "XB");
         else
            Get_External_Name_With_Suffix (E, "XD");
         end if;

         declare
            Lo : constant Node_Id := Type_Low_Bound (E);
            Hi : constant Node_Id := Type_High_Bound (E);

            Lo_Con : constant Boolean := Compile_Time_Known_Value (Lo);
            Hi_Con : constant Boolean := Compile_Time_Known_Value (Hi);

            Lo_Discr : constant Boolean :=
                         Nkind (Lo) = N_Identifier
                           and then
                         Ekind (Entity (Lo)) = E_Discriminant;

            Hi_Discr : constant Boolean :=
                         Nkind (Hi) = N_Identifier
                           and then
                         Ekind (Entity (Hi)) = E_Discriminant;

            Lo_Encode : constant Boolean := Lo_Con or Lo_Discr;
            Hi_Encode : constant Boolean := Hi_Con or Hi_Discr;

         begin
            if Lo_Encode or Hi_Encode then
               if Lo_Encode then
                  if Hi_Encode then
                     Add_Str_To_Name_Buffer ("LU_");
                  else
                     Add_Str_To_Name_Buffer ("L_");
                  end if;
               else
                  Add_Str_To_Name_Buffer ("U_");
               end if;

               if Lo_Con then
                  Add_Uint_To_Buffer (Expr_Rep_Value (Lo));
               elsif Lo_Discr then
                  Get_Name_String_And_Append (Chars (Entity (Lo)));
               end if;

               if Lo_Encode and Hi_Encode then
                  Add_Str_To_Name_Buffer ("__");
               end if;

               if Hi_Con then
                  Add_Uint_To_Buffer (Expr_Rep_Value (Hi));
               elsif Hi_Discr then
                  Get_Name_String_And_Append (Chars (Entity (Hi)));
               end if;
            end if;
         end;

      --  For all other cases, the encoded name is the normal type name

      else
         Has_Suffix := False;
         Get_External_Name (E, Has_Suffix);
      end if;

      if Debug_Flag_B and then Has_Suffix then
         Write_Str ("**** type ");
         Write_Name (Chars (E));
         Write_Str (" is encoded as ");
         Write_Str (Name_Buffer (1 .. Name_Len));
         Write_Eol;
      end if;

      Name_Buffer (Name_Len + 1) := ASCII.NUL;
   end Get_Encoded_Name;

   -----------------------
   -- Get_External_Name --
   -----------------------

   procedure Get_External_Name (Entity : Entity_Id; Has_Suffix : Boolean) is
      E    : Entity_Id := Entity;
      Kind : Entity_Kind;

      procedure Get_Qualified_Name_And_Append (Entity : Entity_Id);
      --  Appends fully qualified name of given entity to Name_Buffer

      -----------------------------------
      -- Get_Qualified_Name_And_Append --
      -----------------------------------

      procedure Get_Qualified_Name_And_Append (Entity : Entity_Id) is
      begin
         --  If the entity is a compilation unit, its scope is Standard,
         --  there is no outer scope, and the no further qualification
         --  is required.

         --  If the front end has already computed a fully qualified name,
         --  then it is also the case that no further qualification is
         --  required

         if Present (Scope (Scope (Entity)))
           and then not Has_Fully_Qualified_Name (Entity)
         then
            Get_Qualified_Name_And_Append (Scope (Entity));
            Add_Str_To_Name_Buffer ("__");
            Get_Name_String_And_Append (Chars (Entity));
            Append_Homonym_Number (Entity);

         else
            Get_Name_String_And_Append (Chars (Entity));
         end if;

      end Get_Qualified_Name_And_Append;

   --  Start of processing for Get_External_Name

   begin
      Name_Len := 0;

      --  If this is a child unit, we want the child

      if Nkind (E) = N_Defining_Program_Unit_Name then
         E := Defining_Identifier (Entity);
      end if;

      Kind := Ekind (E);

      --  Case of interface name being used

      if (Kind = E_Procedure or else
          Kind = E_Function  or else
          Kind = E_Constant  or else
          Kind = E_Variable  or else
          Kind = E_Exception)
        and then Present (Interface_Name (E))
        and then No (Address_Clause (E))
        and then not Has_Suffix
      then
         Add_String_To_Name_Buffer (Strval (Interface_Name (E)));

      --  All other cases besides the interface name case

      else
         --  If this is a library level subprogram (i.e. a subprogram that is a
         --  compilation unit other than a subunit), then we prepend _ada_ to
         --  ensure distinctions required as described in the spec.
         --  Check explicitly for child units, because those are not flagged
         --  as Compilation_Units by lib. Should they be ???

         if Is_Subprogram (E)
           and then (Is_Compilation_Unit (E) or Is_Child_Unit (E))
           and then not Has_Suffix
         then
            Add_Str_To_Name_Buffer ("_ada_");
         end if;

         --  If the entity is a subprogram instance that is not a compilation
         --  unit, generate the name of the original Ada entity, which is the
         --  one gdb needs.

         if Is_Generic_Instance (E)
           and then Is_Subprogram (E)
           and then not Is_Compilation_Unit (Scope (E))
           and then (Ekind (Scope (E)) = E_Package
                      or else
                     Ekind (Scope (E)) = E_Package_Body)
           and then Present (Related_Instance (Scope (E)))
         then
            E := Related_Instance (Scope (E));
         end if;

         Get_Qualified_Name_And_Append (E);
      end if;

      Name_Buffer (Name_Len + 1) := ASCII.Nul;
   end Get_External_Name;

   -----------------------------------
   -- Get_External_Name_With_Suffix --
   -----------------------------------

   procedure Get_External_Name_With_Suffix
     (Entity : Entity_Id;
      Suffix : String)
   is
      Has_Suffix : constant Boolean := (Suffix /= "");
      use type Opt.Operating_Mode_Type;

   begin
      if Opt.Operating_Mode /= Opt.Generate_Code then

         --  If we are not in code generation mode, we still may call this
         --  procedure from Back_End (more specifically - from gigi for doing
         --  type representation annotation or some representation-specific
         --  checks). But in this mode there is no need to mess with external
         --  names. Furthermore, the call causes difficulties in this case
         --  because the string representing the homonym number is not
         --  correctly reset as a part of the call to
         --  Output_Homonym_Numbers_Suffix (which is not called in gigi)

         return;
      end if;

      Get_External_Name (Entity, Has_Suffix);

      if Has_Suffix then
         Add_Str_To_Name_Buffer ("___");
         Add_Str_To_Name_Buffer (Suffix);

         Name_Buffer (Name_Len + 1) := ASCII.Nul;
      end if;
   end Get_External_Name_With_Suffix;

   --------------------------
   -- Get_Variant_Encoding --
   --------------------------

   procedure Get_Variant_Encoding (V : Node_Id) is
      Choice : Node_Id;

      procedure Choice_Val (Typ : Character; Choice : Node_Id);
      --  Output encoded value for a single choice value. Typ is the key
      --  character ('S', 'F', or 'T') that precedes the choice value.

      ----------------
      -- Choice_Val --
      ----------------

      procedure Choice_Val (Typ : Character; Choice : Node_Id) is
      begin
         Add_Char_To_Name_Buffer (Typ);

         if Nkind (Choice) = N_Integer_Literal then
            Add_Uint_To_Buffer (Intval (Choice));

         --  Character literal with no entity present (this is the case
         --  Standard.Character or Standard.Wide_Character as root type)

         elsif Nkind (Choice) = N_Character_Literal
           and then No (Entity (Choice))
         then
            Add_Uint_To_Buffer (Char_Literal_Value (Choice));

         else
            declare
               Ent : constant Entity_Id := Entity (Choice);

            begin
               if Ekind (Ent) = E_Enumeration_Literal then
                  Add_Uint_To_Buffer (Enumeration_Rep (Ent));

               else
                  pragma Assert (Ekind (Ent) = E_Constant);
                  Choice_Val (Typ, Constant_Value (Ent));
               end if;
            end;
         end if;
      end Choice_Val;

   --  Start of processing for Get_Variant_Encoding

   begin
      Name_Len := 0;

      Choice := First (Discrete_Choices (V));
      while Present (Choice) loop
         if Nkind (Choice) = N_Others_Choice then
            Add_Char_To_Name_Buffer ('O');

         elsif Nkind (Choice) = N_Range then
            Choice_Val ('R', Low_Bound (Choice));
            Choice_Val ('T', High_Bound (Choice));

         elsif Is_Entity_Name (Choice)
           and then Is_Type (Entity (Choice))
         then
            Choice_Val ('R', Type_Low_Bound (Entity (Choice)));
            Choice_Val ('T', Type_High_Bound (Entity (Choice)));

         elsif Nkind (Choice) = N_Subtype_Indication then
            declare
               Rang : constant Node_Id :=
                        Range_Expression (Constraint (Choice));
            begin
               Choice_Val ('R', Low_Bound (Rang));
               Choice_Val ('T', High_Bound (Rang));
            end;

         else
            Choice_Val ('S', Choice);
         end if;

         Next (Choice);
      end loop;

      Name_Buffer (Name_Len + 1) := ASCII.NUL;

      if Debug_Flag_B then
         declare
            VP : constant Node_Id := Parent (V);    -- Variant_Part
            CL : constant Node_Id := Parent (VP);   -- Component_List
            RD : constant Node_Id := Parent (CL);   -- Record_Definition
            FT : constant Node_Id := Parent (RD);   -- Full_Type_Declaration

         begin
            Write_Str ("**** variant for type ");
            Write_Name (Chars (Defining_Identifier (FT)));
            Write_Str (" is encoded as ");
            Write_Str (Name_Buffer (1 .. Name_Len));
            Write_Eol;
         end;
      end if;
   end Get_Variant_Encoding;

   ---------------------------------
   -- Make_Packed_Array_Type_Name --
   ---------------------------------

   function Make_Packed_Array_Type_Name
     (Typ   : Entity_Id;
      Csize : Uint)
      return  Name_Id
   is
   begin
      Get_Name_String (Chars (Typ));
      Add_Str_To_Name_Buffer ("___XP");
      Add_Uint_To_Buffer (Csize);
      return Name_Find;
   end Make_Packed_Array_Type_Name;

   -----------------------------------
   -- Output_Homonym_Numbers_Suffix --
   -----------------------------------

   procedure Output_Homonym_Numbers_Suffix is
      J : Natural;

   begin
      if Homonym_Len > 0 then

         --  Check for all 1's, in which case we do not output

         J := 1;
         loop
            exit when Homonym_Numbers (J) /= '1';

            --  If we reached end of string we do not output

            if J = Homonym_Len then
               Homonym_Len := 0;
               return;
            end if;

            exit when Homonym_Numbers (J + 1) /= '_';
            J := J + 2;
         end loop;

         --  If we exit the loop then suffix must be output

         Add_Str_To_Name_Buffer ("__");
         Add_Str_To_Name_Buffer (Homonym_Numbers (1 .. Homonym_Len));
         Homonym_Len := 0;
      end if;
   end Output_Homonym_Numbers_Suffix;

   ------------------------------
   -- Prepend_String_To_Buffer --
   ------------------------------

   procedure Prepend_String_To_Buffer (S : String) is
      N : constant Integer := S'Length;
   begin
      Name_Buffer (1 + N .. Name_Len + N) := Name_Buffer (1 .. Name_Len);
      Name_Buffer (1 .. N) := S;
      Name_Len := Name_Len + N;
   end Prepend_String_To_Buffer;

   ----------------------------
   -- Prepend_Uint_To_Buffer --
   ----------------------------

   procedure Prepend_Uint_To_Buffer (U : Uint) is
   begin
      if U < 0 then
         Prepend_String_To_Buffer ("m");
         Prepend_Uint_To_Buffer (-U);
      else
         UI_Image (U, Decimal);
         Prepend_String_To_Buffer (UI_Image_Buffer (1 .. UI_Image_Length));
      end if;
   end Prepend_Uint_To_Buffer;

   ------------------------------
   -- Qualify_All_Entity_Names --
   ------------------------------

   procedure Qualify_All_Entity_Names is
      E   : Entity_Id;
      Ent : Entity_Id;

   begin
      for J in Name_Qualify_Units.First .. Name_Qualify_Units.Last loop
         E := Defining_Entity (Name_Qualify_Units.Table (J));
         Qualify_Entity_Name (E);

         Ent := First_Entity (E);
         while Present (Ent) loop
            Qualify_Entity_Name (Ent);
            Next_Entity (Ent);

            --  There are odd cases where Last_Entity (E) = E. This happens
            --  in the case of renaming of packages. This test avoids getting
            --  stuck in such cases.

            exit when Ent = E;
         end loop;
      end loop;
   end Qualify_All_Entity_Names;

   -------------------------
   -- Qualify_Entity_Name --
   -------------------------

   procedure Qualify_Entity_Name (Ent : Entity_Id) is

      Full_Qualify_Name : String (1 .. Name_Buffer'Length);
      Full_Qualify_Len  : Natural := 0;
      --  Used to accumulate fully qualified name of subprogram

      procedure Fully_Qualify_Name (E : Entity_Id);
      --  Used to qualify a subprogram or type name, where full
      --  qualification up to Standard is always used. Name is set
      --  in Full_Qualify_Name with the length in Full_Qualify_Len.
      --  Note that this routine does not prepend the _ada_ string
      --  required for library subprograms (this is done in the back end).

      function Is_BNPE (S : Entity_Id) return Boolean;
      --  Determines if S is a BNPE, i.e. Body-Nested Package Entity, which
      --  is defined to be a package which is immediately nested within a
      --  package body.

      function Qualify_Needed (S : Entity_Id) return Boolean;
      --  Given a scope, determines if the scope is to be included in the
      --  fully qualified name, True if so, False if not.

      procedure Set_BNPE_Suffix (E : Entity_Id);
      --  Recursive routine to append the BNPE qualification suffix. Works
      --  from right to left with E being the current entity in the list.
      --  The result does NOT have the trailing n's and trailing b stripped.
      --  The caller must do this required stripping.

      procedure Set_Entity_Name (E : Entity_Id);
      --  Internal recursive routine that does most of the work. This routine
      --  leaves the result sitting in Name_Buffer and Name_Len.

      BNPE_Suffix_Needed : Boolean := False;
      --  Set true if a body-nested package entity suffix is required

      Save_Chars : constant Name_Id := Chars (Ent);
      --  Save original name

      ------------------------
      -- Fully_Qualify_Name --
      ------------------------

      procedure Fully_Qualify_Name (E : Entity_Id) is
         Discard : Boolean := False;

      begin
         --  Ignore empty entry (can happen in error cases)

         if No (E) then
            return;

         --  If this we are qualifying entities local to a generic
         --  instance, use the name of the original instantiation,
         --  not that of the anonymous subprogram in the wrapper
         --  package, so that gdb doesn't have to know about these.

         elsif Is_Generic_Instance (E)
           and then Is_Subprogram (E)
           and then not Comes_From_Source (E)
           and then not Is_Compilation_Unit (Scope (E))
         then
            Fully_Qualify_Name (Related_Instance (Scope (E)));
            return;
         end if;

         --  If we reached fully qualified name, then just copy it

         if Has_Fully_Qualified_Name (E) then
            Get_Name_String (Chars (E));
            Strip_Suffixes (Discard);
            Full_Qualify_Name (1 .. Name_Len) := Name_Buffer (1 .. Name_Len);
            Full_Qualify_Len := Name_Len;
            Set_Has_Fully_Qualified_Name (Ent);

         --  Case of non-fully qualified name

         else
            if Scope (E) = Standard_Standard then
               Set_Has_Fully_Qualified_Name (Ent);
            else
               Fully_Qualify_Name (Scope (E));
               Full_Qualify_Name (Full_Qualify_Len + 1) := '_';
               Full_Qualify_Name (Full_Qualify_Len + 2) := '_';
               Full_Qualify_Len := Full_Qualify_Len + 2;
            end if;

            if Has_Qualified_Name (E) then
               Get_Unqualified_Name_String (Chars (E));
            else
               Get_Name_String (Chars (E));
            end if;

            --  A special check here, we never add internal block or loop
            --  names, since they intefere with debugging. We identify these
            --  by the fact that they start with an upper case B or L.
            --  But do add these if what we are qualifying is a __clean
            --  procedure since those need to be made unique.

            if (Name_Buffer (1) = 'B' or else Name_Buffer (1) = 'L')
                and then (not Debug_Flag_VV)
                and then Full_Qualify_Len > 2
                and then Chars (Ent) /= Name_uClean
            then
               Full_Qualify_Len := Full_Qualify_Len - 2;

            else
               Full_Qualify_Name
                 (Full_Qualify_Len + 1 .. Full_Qualify_Len + Name_Len) :=
                   Name_Buffer (1 .. Name_Len);
               Full_Qualify_Len := Full_Qualify_Len + Name_Len;
               Append_Homonym_Number (E);
            end if;
         end if;

         if Is_BNPE (E) then
            BNPE_Suffix_Needed := True;
         end if;
      end Fully_Qualify_Name;

      -------------
      -- Is_BNPE --
      -------------

      function Is_BNPE (S : Entity_Id) return Boolean is
      begin
         return
           Ekind (S) = E_Package
             and then Is_Package_Body_Entity (S);
      end Is_BNPE;

      --------------------
      -- Qualify_Needed --
      --------------------

      function Qualify_Needed (S : Entity_Id) return Boolean is
      begin
         --  If we got all the way to Standard, then we have certainly
         --  fully qualified the name, so set the flag appropriately,
         --  and then return False, since we are most certainly done!

         if S = Standard_Standard then
            Set_Has_Fully_Qualified_Name (Ent, True);
            return False;

         --  Otherwise figure out if further qualification is required

         else
            return
              Is_Subprogram (Ent)
                or else
              Ekind (Ent) = E_Subprogram_Body
                or else
                  (Ekind (S) /= E_Block
                    and then not Is_Dynamic_Scope (S));
         end if;
      end Qualify_Needed;

      ---------------------
      -- Set_BNPE_Suffix --
      ---------------------

      procedure Set_BNPE_Suffix (E : Entity_Id) is
         S : constant Entity_Id := Scope (E);

      begin
         if Qualify_Needed (S) then
            Set_BNPE_Suffix (S);

            if Is_BNPE (E) then
               Add_Char_To_Name_Buffer ('b');
            else
               Add_Char_To_Name_Buffer ('n');
            end if;

         else
            Add_Char_To_Name_Buffer ('X');
         end if;

      end Set_BNPE_Suffix;

      ---------------------
      -- Set_Entity_Name --
      ---------------------

      procedure Set_Entity_Name (E : Entity_Id) is
         S : constant Entity_Id := Scope (E);

      begin
         --  If we reach an already qualified name, just take the encoding
         --  except that we strip the package body suffixes, since these
         --  will be separately put on later.

         if Has_Qualified_Name (E) then
            Get_Name_String_And_Append (Chars (E));
            Strip_Suffixes (BNPE_Suffix_Needed);

            --  If the top level name we are adding is itself fully
            --  qualified, then that means that the name that we are
            --  preparing for the Fully_Qualify_Name call will also
            --  generate a fully qualified name.

            if Has_Fully_Qualified_Name (E) then
               Set_Has_Fully_Qualified_Name (Ent);
            end if;

         --  Case where upper level name is not encoded yet

         else
            --  Recurse if further qualification required

            if Qualify_Needed (S) then
               Set_Entity_Name (S);
               Add_Str_To_Name_Buffer ("__");
            end if;

            --  Otherwise get name and note if it is a NPBE

            Get_Name_String_And_Append (Chars (E));

            if Is_BNPE (E) then
               BNPE_Suffix_Needed := True;
            end if;

            Append_Homonym_Number (E);
         end if;
      end Set_Entity_Name;

   --  Start of processing for Qualify_Entity_Name

   begin
      if Has_Qualified_Name (Ent) then
         return;

      --  Here is where we create the proper link for renaming

      elsif Ekind (Ent) = E_Enumeration_Literal
        and then Present (Debug_Renaming_Link (Ent))
      then
         Name_Len := 0;
         Qualify_Entity_Name (Debug_Renaming_Link (Ent));
         Get_Name_String (Chars (Ent));
         Prepend_String_To_Buffer
           (Get_Name_String (Chars (Debug_Renaming_Link (Ent))));
         Set_Chars (Ent, Name_Enter);
         Set_Has_Qualified_Name (Ent);
         return;

      elsif Is_Subprogram (Ent)
        or else Ekind (Ent) = E_Subprogram_Body
        or else Is_Type (Ent)
      then
         Fully_Qualify_Name (Ent);
         Name_Len := Full_Qualify_Len;
         Name_Buffer (1 .. Name_Len) := Full_Qualify_Name (1 .. Name_Len);

      elsif Qualify_Needed (Scope (Ent)) then
         Name_Len := 0;
         Set_Entity_Name (Ent);

      else
         Set_Has_Qualified_Name (Ent);
         return;
      end if;

      --  Fall through with a fully qualified name in Name_Buffer/Name_Len

      Output_Homonym_Numbers_Suffix;

      --  Add body-nested package suffix if required

      if BNPE_Suffix_Needed
        and then Ekind (Ent) /= E_Enumeration_Literal
      then
         Set_BNPE_Suffix (Ent);

         --  Strip trailing n's and last trailing b as required. note that
         --  we know there is at least one b, or no suffix would be generated.

         while Name_Buffer (Name_Len) = 'n' loop
            Name_Len := Name_Len - 1;
         end loop;

         Name_Len := Name_Len - 1;
      end if;

      Set_Chars (Ent, Name_Enter);
      Set_Has_Qualified_Name (Ent);

      if Debug_Flag_BB then
         Write_Str ("*** ");
         Write_Name (Save_Chars);
         Write_Str (" qualified as ");
         Write_Name (Chars (Ent));
         Write_Eol;
      end if;
   end Qualify_Entity_Name;

   --------------------------
   -- Qualify_Entity_Names --
   --------------------------

   procedure Qualify_Entity_Names (N : Node_Id) is
   begin
      Name_Qualify_Units.Append (N);
   end Qualify_Entity_Names;

   --------------------
   -- Strip_Suffixes --
   --------------------

   procedure Strip_Suffixes (BNPE_Suffix_Found : in out Boolean) is
      SL : Natural;

   begin
      --  Search for and strip BNPE suffix

      for J in reverse 2 .. Name_Len loop
         if Name_Buffer (J) = 'X' then
            Name_Len := J - 1;
            BNPE_Suffix_Found := True;
            exit;
         end if;

         exit when Name_Buffer (J) /= 'b' and then Name_Buffer (J) /= 'n';
      end loop;

      --  Search for and strip homonym numbers suffix

      for J in reverse 2 .. Name_Len - 2 loop
         if Name_Buffer (J) = '_'
           and then Name_Buffer (J + 1) = '_'
         then
            if Name_Buffer (J + 2) in '0' .. '9' then
               if Homonym_Len > 0 then
                  Homonym_Len := Homonym_Len + 1;
                  Homonym_Numbers (Homonym_Len) := '-';
               end if;

               SL := Name_Len - (J + 1);

               Homonym_Numbers (Homonym_Len + 1 .. Homonym_Len + SL) :=
                 Name_Buffer (J + 2 .. Name_Len);
               Name_Len := J - 1;
               Homonym_Len := Homonym_Len + SL;
            end if;

            exit;
         end if;
      end loop;

   end Strip_Suffixes;

end Exp_Dbug;
