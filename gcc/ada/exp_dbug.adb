------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ D B U G                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1996-2022, Free Software Foundation, Inc.         --
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

with Alloc;
with Atree;          use Atree;
with Debug;          use Debug;
with Einfo;          use Einfo;
with Einfo.Entities; use Einfo.Entities;
with Einfo.Utils;    use Einfo.Utils;
with Exp_Util;       use Exp_Util;
with Nlists;         use Nlists;
with Nmake;          use Nmake;
with Opt;            use Opt;
with Output;         use Output;
with Sem_Aux;        use Sem_Aux;
with Sem_Eval;       use Sem_Eval;
with Sem_Util;       use Sem_Util;
with Sinfo;          use Sinfo;
with Sinfo.Nodes;    use Sinfo.Nodes;
with Sinfo.Utils;    use Sinfo.Utils;
with Stand;          use Stand;
with Stringt;        use Stringt;
with Table;
with Tbuild;         use Tbuild;
with Urealp;         use Urealp;

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

   --  The string defined here (and its associated length) is used to gather
   --  the homonym string that will be appended to Name_Buffer when the name
   --  is complete. Strip_Suffixes appends to this string as does
   --  Append_Homonym_Number, and Output_Homonym_Numbers_Suffix appends the
   --  string to the end of Name_Buffer.

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
   --  If homonym numbers are stored, then output them into Name_Buffer

   procedure Prepend_String_To_Buffer (S : String);
   --  Prepend given string to the contents of the string buffer, updating
   --  the value in Name_Len (i.e. string is added at start of buffer).

   procedure Prepend_Uint_To_Buffer (U : Uint);
   --  Prepend image of universal integer to Name_Buffer, updating Name_Len

   procedure Qualify_Entity_Name (Ent : Entity_Id);
   --  If not already done, replaces the Chars field of the given entity
   --  with the appropriate fully qualified name.

   procedure Reset_Buffers;
   --  Reset the contents of Name_Buffer and Homonym_Numbers by setting their
   --  respective lengths to zero.

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
      Add_Char_To_Name_Buffer ('_');
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
         if Homonym_Len > 0 then
            Homonym_Len := Homonym_Len + 1;
            Homonym_Numbers (Homonym_Len) := '_';
         end if;

         Add_Nat_To_H (Homonym_Number (E));
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

      --  Here we check if the static bounds match the natural size, which is
      --  the size passed through with the debugging information. This is the
      --  Esize rounded up to 8, 16, 32, 64 or 128 as appropriate.

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
            elsif Esize (E) <= 64 then
               Siz := Uint_64;
            else
               Siz := Uint_128;
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
      pragma Assert
        (Nkind (N) in N_Object_Renaming_Declaration
                    | N_Package_Renaming_Declaration
                    | N_Exception_Renaming_Declaration);

      Loc : constant Source_Ptr := Sloc (N);
      Ent : constant Node_Id    := Defining_Entity (N);
      Nam : constant Node_Id    := Name (N);
      Ren : Node_Id;
      Typ : Entity_Id;
      Obj : Entity_Id;
      Res : Node_Id;

      Enable : Boolean := Nkind (N) = N_Package_Renaming_Declaration;
      --  By default, we do not generate an encoding for renaming. This is
      --  however done (in which case this is set to True) in a few cases:
      --    - when a package is renamed,
      --    - when the renaming involves a packed array,
      --    - when the renaming involves a packed record.

      Last_Is_Indexed_Comp : Boolean := False;
      --  Whether the last subscript value was an indexed component access (XS)

      procedure Enable_If_Packed_Array (N : Node_Id);
      --  Enable encoding generation if N is a packed array

      function Output_Subscript (N : Node_Id; S : String) return Boolean;
      --  Outputs a single subscript value as ?nnn (subscript is compile time
      --  known value with value nnn) or as ?e (subscript is local constant
      --  with name e), where S supplies the proper string to use for ?.
      --  Returns False if the subscript is not of an appropriate type to
      --  output in one of these two forms. The result is prepended to the
      --  name stored in Name_Buffer.

      function Scope_Contains
        (Outer : Entity_Id;
         Inner : Entity_Id)
         return Boolean;
      --  Return whether Inner belongs to the Outer scope

      ----------------------------
      -- Enable_If_Packed_Array --
      ----------------------------

      procedure Enable_If_Packed_Array (N : Node_Id) is
         T : constant Entity_Id := Underlying_Type (Etype (N));

      begin
         Enable :=
           Enable
             or else
               (Ekind (T) in Array_Kind
                 and then Present (Packed_Array_Impl_Type (T)));
      end Enable_If_Packed_Array;

      ----------------------
      -- Output_Subscript --
      ----------------------

      function Output_Subscript (N : Node_Id; S : String) return Boolean is
      begin
         if Compile_Time_Known_Value (N) then
            Prepend_Uint_To_Buffer (Expr_Value (N));

         elsif Nkind (N) = N_Identifier
           and then Scope_Contains (Scope (Entity (N)), Ent)
           and then Ekind (Entity (N)) in E_Constant | E_In_Parameter
         then
            Prepend_String_To_Buffer (Get_Name_String (Chars (Entity (N))));

         else
            return False;
         end if;

         Prepend_String_To_Buffer (S);
         return True;
      end Output_Subscript;

      --------------------
      -- Scope_Contains --
      --------------------

      function Scope_Contains
        (Outer : Entity_Id;
         Inner : Entity_Id)
         return Boolean
      is
         Cur : Entity_Id := Scope (Inner);

      begin
         while Present (Cur) loop
            if Cur = Outer then
               return True;
            end if;

            Cur := Scope (Cur);
         end loop;

         return False;
      end Scope_Contains;

   --  Start of processing for Debug_Renaming_Declaration

   begin
      if not Comes_From_Source (N) and then not Needs_Debug_Info (Ent) then
         return Empty;
      end if;

      --  Get renamed entity and compute suffix

      Name_Len := 0;
      Ren := Nam;
      loop
         --  The expression that designates the renamed object is sometimes
         --  expanded into bit-wise operations. We want to work instead on
         --  array/record components accesses, so try to analyze the unexpanded
         --  forms.

         Ren := Original_Node (Ren);

         case Nkind (Ren) is
            when N_Expanded_Name
               | N_Identifier
            =>
               if No (Entity (Ren))
                 or else No (Renamed_Entity_Or_Object (Entity (Ren)))
               then
                  exit;
               end if;

               --  This is a renaming of a renaming: traverse until the final
               --  renaming to see if anything is packed along the way.

               Ren := Renamed_Entity_Or_Object (Entity (Ren));

            when N_Selected_Component =>
               declare
                  Sel_Id    : constant Entity_Id :=
                                Entity (Selector_Name (Ren));
                  First_Bit : Uint;

               begin
                  --  If the renaming involves a call to a primitive function,
                  --  we are out of the scope of renaming encodings. We will
                  --  very likely create a variable to hold the renamed value
                  --  anyway, so the renaming entity will be available in
                  --  debuggers.

                  exit when Ekind (Sel_Id) not in E_Component | E_Discriminant;

                  First_Bit := Normalized_First_Bit (Sel_Id);
                  Enable :=
                    Enable
                      or else Is_Packed
                                (Underlying_Type (Etype (Prefix (Ren))))
                      or else (Present (First_Bit)
                                and then First_Bit /= Uint_0);
               end;

               Prepend_String_To_Buffer
                 (Get_Name_String (Chars (Selector_Name (Ren))));
               Prepend_String_To_Buffer ("XR");
               Ren := Prefix (Ren);
               Last_Is_Indexed_Comp := False;

            when N_Indexed_Component =>
               declare
                  X : Node_Id;

               begin
                  Enable_If_Packed_Array (Prefix (Ren));

                  X := Last (Expressions (Ren));
                  while Present (X) loop
                     if not Output_Subscript (X, "XS") then
                        Set_Materialize_Entity (Ent);
                        return Empty;
                     end if;

                     Prev (X);
                     Last_Is_Indexed_Comp := True;
                  end loop;
               end;

               Ren := Prefix (Ren);

            when N_Slice =>

               --  Assuming X is an array:
               --      X (Y1 .. Y2) (Y3)

               --  is equivalent to:
               --      X (Y3)

               --  GDB cannot handle packed array slices, so avoid describing
               --  the slice if we can avoid it.

               if not Last_Is_Indexed_Comp then
                  Enable_If_Packed_Array (Prefix (Ren));
                  Typ := Etype (First_Index (Etype (Ren)));

                  if not Output_Subscript (Type_High_Bound (Typ), "XS") then
                     Set_Materialize_Entity (Ent);
                     return Empty;
                  end if;

                  if not Output_Subscript (Type_Low_Bound  (Typ), "XL") then
                     Set_Materialize_Entity (Ent);
                     return Empty;
                  end if;

                  Last_Is_Indexed_Comp := False;
               end if;

               Ren := Prefix (Ren);

            when N_Explicit_Dereference =>
               Prepend_String_To_Buffer ("XA");
               Ren := Prefix (Ren);
               Last_Is_Indexed_Comp := False;

            --  For now, anything else simply results in no translation

            when others =>
               Set_Materialize_Entity (Ent);
               return Empty;
         end case;
      end loop;

      --  If we found no reason here to emit an encoding, stop now

      if not Enable then
         Set_Materialize_Entity (Ent);
         return Empty;
      end if;

      Prepend_String_To_Buffer ("___XE");

      --  Include the designation of the form of renaming

      case Nkind (N) is
         when N_Object_Renaming_Declaration =>
            Prepend_String_To_Buffer ("___XR");

         when N_Exception_Renaming_Declaration =>
            Prepend_String_To_Buffer ("___XRE");

         when N_Package_Renaming_Declaration =>
            Prepend_String_To_Buffer ("___XRP");

         when others =>
            return Empty;
      end case;

      --  Add the name of the renaming entity to the front

      Prepend_String_To_Buffer (Get_Name_String (Chars (Ent)));

      --  If it is a child unit create a fully qualified name, to disambiguate
      --  multiple child units with the same name and different parents.

      if Nkind (N) = N_Package_Renaming_Declaration
        and then Is_Child_Unit (Ent)
      then
         Prepend_String_To_Buffer ("__");
         Prepend_String_To_Buffer
           (Get_Name_String (Chars (Scope (Ent))));
      end if;

      --  Create the special object whose name is the debug encoding for the
      --  renaming declaration.

      --  For now, the object name contains the suffix encoding for the renamed
      --  object, but not the name of the leading entity. The object is linked
      --  the renamed entity using the Debug_Renaming_Link field. Then the
      --  Qualify_Entity_Name procedure uses this link to create the proper
      --  fully qualified name.

      --  The reason we do things this way is that we really need to copy the
      --  qualification of the renamed entity, and it is really much easier to
      --  do this after the renamed entity has itself been fully qualified.

      Obj := Make_Defining_Identifier (Loc, Chars => Name_Enter);
      Res :=
        Make_Object_Declaration (Loc,
          Defining_Identifier => Obj,
          Object_Definition   => New_Occurrence_Of
                                   (Standard_Debug_Renaming_Type, Loc));

      Set_Debug_Renaming_Link (Obj, Entity (Ren));

      Set_Debug_Info_Needed (Obj);

      --  The renamed entity may be a temporary, e.g. the result of an
      --  implicit dereference in an iterator. Indicate that the temporary
      --  itself requires debug information. If the renamed entity comes
      --  from source this is a no-op.

      Set_Debug_Info_Needed (Entity (Ren));

      --  Mark the object as internal so that it won't be initialized when
      --  pragma Initialize_Scalars or Normalize_Scalars is in use.

      Set_Is_Internal (Obj);

      return Res;

   --  If we get an exception, just figure it is a case that we cannot
   --  successfully handle using our current approach, since this is
   --  only for debugging, no need to take the compilation with us.

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
      --  If not generating code, there is no need to create encoded names, and
      --  problems when the back-end is called to annotate types without full
      --  code generation. See comments in Get_External_Name for additional
      --  details.

      --  However we do create encoded names if the back end is active, even
      --  if Operating_Mode got reset. Otherwise any serious error reported
      --  by the backend calling Error_Msg changes the Compilation_Mode to
      --  Check_Semantics, which disables the functionality of this routine,
      --  causing the generation of spurious additional errors.

      --  Couldn't we just test Original_Operating_Mode here? ???

      if Operating_Mode /= Generate_Code and then not Generating_Code then
         return;
      end if;

      Get_Name_String (Chars (E));

      --  Nothing to do if we do not have a type

      if not Is_Type (E)

      --  Or if this is an enumeration base type

        or else (Is_Enumeration_Type (E) and then Is_Base_Type (E))

      --  Or if this is a dummy type for a renaming

        or else (Name_Len >= 3 and then
                   Name_Buffer (Name_Len - 2 .. Name_Len) = "_XR")

        or else (Name_Len >= 4 and then
                   (Name_Buffer (Name_Len - 3 .. Name_Len) = "_XRE"
                      or else
                    Name_Buffer (Name_Len - 3 .. Name_Len) = "_XRP"))

      --  For all these cases, just return the name unchanged

      then
         Name_Buffer (Name_Len + 1) := ASCII.NUL;
         return;
      end if;

      Has_Suffix := True;

      --  Generate GNAT encodings when asked to for fixed-point case

      if GNAT_Encodings = DWARF_GNAT_Encodings_All
        and then Is_Fixed_Point_Type (E)
      then
         Get_External_Name (E, True, "XF_");
         Add_Real_To_Buffer (Delta_Value (E));

         if Small_Value (E) /= Delta_Value (E) then
            Add_Char_To_Name_Buffer ('_');
            Add_Real_To_Buffer (Small_Value (E));
         end if;

      --  Likewise for discrete case where bounds do not match size

      elsif GNAT_Encodings = DWARF_GNAT_Encodings_All
        and then Is_Discrete_Type (E)
        and then not Bounds_Match_Size (E)
      then
         declare
            Lo : constant Node_Id := Type_Low_Bound (E);
            Hi : constant Node_Id := Type_High_Bound (E);

            Lo_Con : constant Boolean := Compile_Time_Known_Value (Lo);
            Hi_Con : constant Boolean := Compile_Time_Known_Value (Hi);

            Lo_Discr : constant Boolean :=
                         Nkind (Lo) = N_Identifier
                           and then Ekind (Entity (Lo)) = E_Discriminant;

            Hi_Discr : constant Boolean :=
                         Nkind (Hi) = N_Identifier
                           and then Ekind (Entity (Hi)) = E_Discriminant;

            Lo_Encode : constant Boolean := Lo_Con or Lo_Discr;
            Hi_Encode : constant Boolean := Hi_Con or Hi_Discr;

            Biased : constant Boolean := Has_Biased_Representation (E);

         begin
            if Biased then
               Get_External_Name (E, True, "XB");
            else
               Get_External_Name (E, True, "XD");
            end if;

            if Lo_Encode or Hi_Encode then
               if Biased then
                  Add_Char_To_Name_Buffer ('_');
               else
                  if Lo_Encode then
                     if Hi_Encode then
                        Add_Str_To_Name_Buffer ("LU_");
                     else
                        Add_Str_To_Name_Buffer ("L_");
                     end if;
                  else
                     Add_Str_To_Name_Buffer ("U_");
                  end if;
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
         Get_External_Name (E);
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

   procedure Get_External_Name
     (Entity     : Entity_Id;
      Has_Suffix : Boolean := False;
      Suffix     : String  := "")
   is
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
         --  required.

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

      --  Local variables

      E : Entity_Id := Entity;

   --  Start of processing for Get_External_Name

   begin
      --  If we are not in code generation mode, this procedure may still be
      --  called from Back_End (more specifically - from gigi for doing type
      --  representation annotation or some representation-specific checks).
      --  But in this mode there is no need to mess with external names.

      --  Furthermore, the call causes difficulties in this case because the
      --  string representing the homonym number is not correctly reset as a
      --  part of the call to Output_Homonym_Numbers_Suffix (which is not
      --  called in gigi).

      if Operating_Mode /= Generate_Code then
         return;
      end if;

      Reset_Buffers;

      --  If this is a child unit, we want the child

      if Nkind (E) = N_Defining_Program_Unit_Name then
         E := Defining_Identifier (Entity);
      end if;

      --  Case of interface name being used

      if Ekind (E) in E_Constant
                    | E_Exception
                    | E_Function
                    | E_Procedure
                    | E_Variable
        and then Present (Interface_Name (E))
        and then No (Address_Clause (E))
        and then not Has_Suffix
      then
         Append (Global_Name_Buffer, Strval (Interface_Name (E)));

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
           and then Ekind (Scope (E)) in E_Package | E_Package_Body
           and then Present (Related_Instance (Scope (E)))
         then
            E := Related_Instance (Scope (E));
         end if;

         Get_Qualified_Name_And_Append (E);
      end if;

      if Has_Suffix then
         Add_Str_To_Name_Buffer ("___");
         Add_Str_To_Name_Buffer (Suffix);
      end if;

      --  Add a special prefix to distinguish Ghost entities. In Ignored Ghost
      --  mode, these entities should not leak in the "living" space and they
      --  should be removed by the compiler in a post-processing pass. Thus,
      --  the prefix allows anyone to check that the final executable indeed
      --  does not contain such entities, in such a case. Do not insert this
      --  prefix for compilation units, whose name is used as a basis for the
      --  name of the generated elaboration procedure and (when appropriate)
      --  the executable produced. Only insert this prefix once, for Ghost
      --  entities declared inside other Ghost entities. Three leading
      --  underscores are used so that "___ghost_" is a unique substring of
      --  names produced for Ghost entities, while "__ghost_" can appear in
      --  names of entities inside a child/local package called "Ghost".

      --  The compiler-generated finalizer for an enabled Ghost unit is treated
      --  specially, as its name must be known to the binder, which has no
      --  knowledge of Ghost status. In that case, the finalizer is not marked
      --  as Ghost so that no prefix is added. Note that the special ___ghost_
      --  prefix is retained when the Ghost unit is ignored, which still allows
      --  inspecting the final executable for the presence of an ignored Ghost
      --  finalizer procedure.

      if Is_Ghost_Entity (E)
        and then not Is_Compilation_Unit (E)
        and then (Name_Len < 9
                   or else Name_Buffer (1 .. 9) /= "___ghost_")
      then
         Insert_Str_In_Name_Buffer ("___ghost_", 1);
      end if;

      Name_Buffer (Name_Len + 1) := ASCII.NUL;
   end Get_External_Name;

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
         if Nkind (Choice) = N_Integer_Literal then
            Add_Char_To_Name_Buffer (Typ);
            Add_Uint_To_Buffer (Intval (Choice));

         --  Character literal with no entity present (this is the case
         --  Standard.Character or Standard.Wide_Character as root type)

         elsif Nkind (Choice) = N_Character_Literal
           and then No (Entity (Choice))
         then
            Add_Char_To_Name_Buffer (Typ);
            Add_Uint_To_Buffer (Char_Literal_Value (Choice));

         else
            declare
               Ent : constant Entity_Id := Entity (Choice);

            begin
               if Ekind (Ent) = E_Enumeration_Literal then
                  Add_Char_To_Name_Buffer (Typ);
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

   -----------------------------------------
   -- Build_Subprogram_Instance_Renamings --
   -----------------------------------------

   procedure Build_Subprogram_Instance_Renamings
     (N       : Node_Id;
      Wrapper : Entity_Id)
   is
      Loc  : Source_Ptr;
      Decl : Node_Id;
      E    : Entity_Id;

   begin
      E := First_Entity (Wrapper);
      while Present (E) loop
         if Nkind (Parent (E)) = N_Object_Declaration
           and then Present (Corresponding_Generic_Association (Parent (E)))
           and then Is_Elementary_Type (Etype (E))
         then
            Loc := Sloc (Expression (Parent (E)));
            Decl := Make_Object_Renaming_Declaration (Loc,
               Defining_Identifier =>
                 Make_Defining_Identifier (Loc, Chars (E)),
               Subtype_Mark        => New_Occurrence_Of (Etype (E), Loc),
               Name                => New_Occurrence_Of (E, Loc));

            Append (Decl, Declarations (N));
            Set_Debug_Info_Needed (Defining_Identifier (Decl));
         end if;

         Next_Entity (E);
      end loop;
   end Build_Subprogram_Instance_Renamings;

   ------------------------------------
   -- Get_Secondary_DT_External_Name --
   ------------------------------------

   procedure Get_Secondary_DT_External_Name
     (Typ          : Entity_Id;
      Ancestor_Typ : Entity_Id;
      Suffix_Index : Int)
   is
   begin
      Get_External_Name (Typ);

      if Ancestor_Typ /= Typ then
         declare
            Len      : constant Natural := Name_Len;
            Save_Str : constant String (1 .. Name_Len)
                         := Name_Buffer (1 .. Name_Len);
         begin
            Get_External_Name (Ancestor_Typ);

            --  Append the extended name of the ancestor to the
            --  extended name of Typ

            Name_Buffer (Len + 2 .. Len + Name_Len + 1) :=
              Name_Buffer (1 .. Name_Len);
            Name_Buffer (1 .. Len) := Save_Str;
            Name_Buffer (Len + 1) := '_';
            Name_Len := Len + Name_Len + 1;
         end;
      end if;

      Add_Nat_To_Name_Buffer (Suffix_Index);
   end Get_Secondary_DT_External_Name;

   ---------------------------------
   -- Make_Packed_Array_Impl_Type_Name --
   ---------------------------------

   function Make_Packed_Array_Impl_Type_Name
     (Typ   : Entity_Id;
      Csize : Uint)
      return  Name_Id
   is
   begin
      Get_Name_String (Chars (Typ));
      Add_Str_To_Name_Buffer ("___XP");
      Add_Uint_To_Buffer (Csize);
      return Name_Find;
   end Make_Packed_Array_Impl_Type_Name;

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
      Nod : Node_Id;

   begin
      for J in Name_Qualify_Units.First .. Name_Qualify_Units.Last loop
         Nod := Name_Qualify_Units.Table (J);

         --  When a scoping construct is ignored Ghost, it is rewritten as
         --  a null statement. Skip such constructs as they no longer carry
         --  names.

         if Nkind (Nod) = N_Null_Statement then
            goto Continue;
         end if;

         E := Defining_Entity (Nod);
         Reset_Buffers;
         Qualify_Entity_Name (E);

         --  Normally entities in the qualification list are scopes, but in the
         --  case of a library-level package renaming there is an associated
         --  variable that encodes the debugger name and that variable is
         --  entered in the list since it occurs in the Aux_Decls list of the
         --  compilation and doesn't have a normal scope.

         if Ekind (E) /= E_Variable then
            Ent := First_Entity (E);
            while Present (Ent) loop
               Reset_Buffers;
               Qualify_Entity_Name (Ent);
               Next_Entity (Ent);

               --  There are odd cases where Last_Entity (E) = E. This happens
               --  in the case of renaming of packages. This test avoids
               --  getting stuck in such cases.

               exit when Ent = E;
            end loop;
         end if;

         <<Continue>>
         null;
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
      --  fully qualified name, True if so, False if not. Blocks and loops
      --  are excluded from a qualified name.

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

         --  If this we are qualifying entities local to a generic instance,
         --  use the name of the original instantiation, not that of the
         --  anonymous subprogram in the wrapper package, so that gdb doesn't
         --  have to know about these.

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

            --  Here we do one step of the qualification

            Full_Qualify_Name
              (Full_Qualify_Len + 1 .. Full_Qualify_Len + Name_Len) :=
                 Name_Buffer (1 .. Name_Len);
            Full_Qualify_Len := Full_Qualify_Len + Name_Len;
            Append_Homonym_Number (E);
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
         return Ekind (S) = E_Package and then Is_Package_Body_Entity (S);
      end Is_BNPE;

      --------------------
      -- Qualify_Needed --
      --------------------

      function Qualify_Needed (S : Entity_Id) return Boolean is
      begin
         --  If we got all the way to Standard, then we have certainly
         --  fully qualified the name, so set the flag appropriately,
         --  and then return False, since we are most certainly done.

         if S = Standard_Standard then
            Set_Has_Fully_Qualified_Name (Ent, True);
            return False;

         --  Otherwise figure out if further qualification is required

         else
            return Is_Subprogram (Ent)
              or else Ekind (Ent) = E_Subprogram_Body
              or else (Ekind (S) /= E_Block
                        and then Ekind (S) /= E_Loop
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

            --  Otherwise get name and note if it is a BNPE

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

      --  If the entity is a variable encoding the debug name for an object
      --  renaming, then the qualified name of the entity associated with the
      --  renamed object can now be incorporated in the debug name.

      elsif Ekind (Ent) = E_Variable
        and then Present (Debug_Renaming_Link (Ent))
      then
         Name_Len := 0;
         Qualify_Entity_Name (Debug_Renaming_Link (Ent));
         Get_Name_String (Chars (Ent));

         --  Retrieve the now-qualified name of the renamed entity and insert
         --  it in the middle of the name, just preceding the suffix encoding
         --  describing the renamed object.

         declare
            Renamed_Id : constant String :=
                           Get_Name_String (Chars (Debug_Renaming_Link (Ent)));
            Insert_Len : constant Integer := Renamed_Id'Length + 1;
            Index      : Natural := Name_Len - 3;

         begin
            --  Loop backwards through the name to find the start of the "___"
            --  sequence associated with the suffix.

            while Index >= Name_Buffer'First
              and then (Name_Buffer (Index + 1) /= '_'
                         or else Name_Buffer (Index + 2) /= '_'
                         or else Name_Buffer (Index + 3) /= '_')
            loop
               Index := Index - 1;
            end loop;

            pragma Assert (Name_Buffer (Index + 1 .. Index + 3) = "___");

            --  Insert an underscore separator and the entity name just in
            --  front of the suffix.

            Name_Buffer (Index + 1 + Insert_Len .. Name_Len + Insert_Len) :=
              Name_Buffer (Index + 1 .. Name_Len);
            Name_Buffer (Index + 1) := '_';
            Name_Buffer (Index + 2 .. Index + Insert_Len) := Renamed_Id;
            Name_Len := Name_Len + Insert_Len;
         end;

         --  Reset the name of the variable to the new name that includes the
         --  name of the renamed entity.

         Set_Chars (Ent, Name_Enter);

         --  If the entity needs qualification by its scope then develop it
         --  here, add the variable's name, and again reset the entity name.

         if Qualify_Needed (Scope (Ent)) then
            Name_Len := 0;
            Set_Entity_Name (Scope (Ent));
            Add_Str_To_Name_Buffer ("__");

            Get_Name_String_And_Append (Chars (Ent));

            Set_Chars (Ent, Name_Enter);
         end if;

         Set_Has_Qualified_Name (Ent);
         return;

      elsif Is_Subprogram (Ent)
        or else Ekind (Ent) = E_Subprogram_Body
        or else Is_Type (Ent)
        or else Ekind (Ent) = E_Exception
      then
         Fully_Qualify_Name (Ent);
         Name_Len := Full_Qualify_Len;
         Name_Buffer (1 .. Name_Len) := Full_Qualify_Name (1 .. Name_Len);

      --  Qualification needed for enumeration literals when generating C code
      --  (to simplify their management in the backend).

      elsif Modify_Tree_For_C
        and then Ekind (Ent) = E_Enumeration_Literal
        and then Scope (Ultimate_Alias (Ent)) /= Standard_Standard
      then
         Fully_Qualify_Name (Ent);
         Name_Len := Full_Qualify_Len;
         Name_Buffer (1 .. Name_Len) := Full_Qualify_Name (1 .. Name_Len);

      elsif Qualify_Needed (Scope (Ent)) then
         Name_Len := 0;
         Set_Entity_Name (Ent);

      else
         Set_Has_Qualified_Name (Ent);

         --  If a variable is hidden by a subsequent loop variable, qualify
         --  the name of that loop variable to prevent visibility issues when
         --  translating to C. Note that gdb probably never handled properly
         --  this accidental hiding, given that loops are not scopes at
         --  runtime. We also qualify a name if it hides an outer homonym,
         --  and both are declared in blocks.

         if Modify_Tree_For_C and then Ekind (Ent) =  E_Variable then
            if Present (Hiding_Loop_Variable (Ent)) then
               declare
                  Var : constant Entity_Id := Hiding_Loop_Variable (Ent);

               begin
                  Set_Entity_Name (Var);
                  Add_Char_To_Name_Buffer ('L');
                  Set_Chars (Var, Name_Enter);
               end;

            elsif Present (Homonym (Ent))
              and then Ekind (Scope (Ent)) = E_Block
              and then Ekind (Scope (Homonym (Ent))) = E_Block
            then
               Set_Entity_Name (Ent);
               Add_Char_To_Name_Buffer ('B');
               Set_Chars (Ent, Name_Enter);
            end if;
         end if;

         return;
      end if;

      --  Fall through with a fully qualified name in Name_Buffer/Name_Len

      Output_Homonym_Numbers_Suffix;

      --  Add body-nested package suffix if required

      if BNPE_Suffix_Needed
        and then Ekind (Ent) /= E_Enumeration_Literal
      then
         Set_BNPE_Suffix (Ent);

         --  Strip trailing n's and last trailing b as required. Note that
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

   -------------------
   -- Reset_Buffers --
   -------------------

   procedure Reset_Buffers is
   begin
      Name_Len    := 0;
      Homonym_Len := 0;
   end Reset_Buffers;

   --------------------
   -- Strip_Suffixes --
   --------------------

   procedure Strip_Suffixes (BNPE_Suffix_Found : in out Boolean) is
      SL : Natural;

      pragma Warnings (Off, BNPE_Suffix_Found);
      --  Since this procedure only ever sets the flag

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
