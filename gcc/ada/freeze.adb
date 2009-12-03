------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               F R E E Z E                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2009, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- You should have received a copy of the GNU General Public License along  --
-- with this program; see file COPYING3.  If not see                        --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Atree;    use Atree;
with Debug;    use Debug;
with Einfo;    use Einfo;
with Elists;   use Elists;
with Errout;   use Errout;
with Exp_Ch3;  use Exp_Ch3;
with Exp_Ch7;  use Exp_Ch7;
with Exp_Disp; use Exp_Disp;
with Exp_Pakd; use Exp_Pakd;
with Exp_Util; use Exp_Util;
with Exp_Tss;  use Exp_Tss;
with Layout;   use Layout;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Restrict; use Restrict;
with Rident;   use Rident;
with Sem;      use Sem;
with Sem_Aux;  use Sem_Aux;
with Sem_Cat;  use Sem_Cat;
with Sem_Ch6;  use Sem_Ch6;
with Sem_Ch7;  use Sem_Ch7;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Ch13; use Sem_Ch13;
with Sem_Eval; use Sem_Eval;
with Sem_Mech; use Sem_Mech;
with Sem_Prag; use Sem_Prag;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Stand;    use Stand;
with Targparm; use Targparm;
with Tbuild;   use Tbuild;
with Ttypes;   use Ttypes;
with Uintp;    use Uintp;
with Urealp;   use Urealp;

package body Freeze is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Adjust_Esize_For_Alignment (Typ : Entity_Id);
   --  Typ is a type that is being frozen. If no size clause is given,
   --  but a default Esize has been computed, then this default Esize is
   --  adjusted up if necessary to be consistent with a given alignment,
   --  but never to a value greater than Long_Long_Integer'Size. This
   --  is used for all discrete types and for fixed-point types.

   procedure Build_And_Analyze_Renamed_Body
     (Decl  : Node_Id;
      New_S : Entity_Id;
      After : in out Node_Id);
   --  Build body for a renaming declaration, insert in tree and analyze

   procedure Check_Address_Clause (E : Entity_Id);
   --  Apply legality checks to address clauses for object declarations,
   --  at the point the object is frozen.

   procedure Check_Strict_Alignment (E : Entity_Id);
   --  E is a base type. If E is tagged or has a component that is aliased
   --  or tagged or contains something this is aliased or tagged, set
   --  Strict_Alignment.

   procedure Check_Unsigned_Type (E : Entity_Id);
   pragma Inline (Check_Unsigned_Type);
   --  If E is a fixed-point or discrete type, then all the necessary work
   --  to freeze it is completed except for possible setting of the flag
   --  Is_Unsigned_Type, which is done by this procedure. The call has no
   --  effect if the entity E is not a discrete or fixed-point type.

   procedure Freeze_And_Append
     (Ent    : Entity_Id;
      Loc    : Source_Ptr;
      Result : in out List_Id);
   --  Freezes Ent using Freeze_Entity, and appends the resulting list of
   --  nodes to Result, modifying Result from No_List if necessary.

   procedure Freeze_Enumeration_Type (Typ : Entity_Id);
   --  Freeze enumeration type. The Esize field is set as processing
   --  proceeds (i.e. set by default when the type is declared and then
   --  adjusted by rep clauses. What this procedure does is to make sure
   --  that if a foreign convention is specified, and no specific size
   --  is given, then the size must be at least Integer'Size.

   procedure Freeze_Static_Object (E : Entity_Id);
   --  If an object is frozen which has Is_Statically_Allocated set, then
   --  all referenced types must also be marked with this flag. This routine
   --  is in charge of meeting this requirement for the object entity E.

   procedure Freeze_Subprogram (E : Entity_Id);
   --  Perform freezing actions for a subprogram (create extra formals,
   --  and set proper default mechanism values). Note that this routine
   --  is not called for internal subprograms, for which neither of these
   --  actions is needed (or desirable, we do not want for example to have
   --  these extra formals present in initialization procedures, where they
   --  would serve no purpose). In this call E is either a subprogram or
   --  a subprogram type (i.e. an access to a subprogram).

   function Is_Fully_Defined (T : Entity_Id) return Boolean;
   --  True if T is not private and has no private components, or has a full
   --  view. Used to determine whether the designated type of an access type
   --  should be frozen when the access type is frozen. This is done when an
   --  allocator is frozen, or an expression that may involve attributes of
   --  the designated type. Otherwise freezing the access type does not freeze
   --  the designated type.

   procedure Process_Default_Expressions
     (E     : Entity_Id;
      After : in out Node_Id);
   --  This procedure is called for each subprogram to complete processing
   --  of default expressions at the point where all types are known to be
   --  frozen. The expressions must be analyzed in full, to make sure that
   --  all error processing is done (they have only been pre-analyzed). If
   --  the expression is not an entity or literal, its analysis may generate
   --  code which must not be executed. In that case we build a function
   --  body to hold that code. This wrapper function serves no other purpose
   --  (it used to be called to evaluate the default, but now the default is
   --  inlined at each point of call).

   procedure Set_Component_Alignment_If_Not_Set (Typ : Entity_Id);
   --  Typ is a record or array type that is being frozen. This routine
   --  sets the default component alignment from the scope stack values
   --  if the alignment is otherwise not specified.

   procedure Check_Debug_Info_Needed (T : Entity_Id);
   --  As each entity is frozen, this routine is called to deal with the
   --  setting of Debug_Info_Needed for the entity. This flag is set if
   --  the entity comes from source, or if we are in Debug_Generated_Code
   --  mode or if the -gnatdV debug flag is set. However, it never sets
   --  the flag if Debug_Info_Off is set. This procedure also ensures that
   --  subsidiary entities have the flag set as required.

   procedure Undelay_Type (T : Entity_Id);
   --  T is a type of a component that we know to be an Itype.
   --  We don't want this to have a Freeze_Node, so ensure it doesn't.
   --  Do the same for any Full_View or Corresponding_Record_Type.

   procedure Warn_Overlay
     (Expr : Node_Id;
      Typ  : Entity_Id;
      Nam  : Node_Id);
   --  Expr is the expression for an address clause for entity Nam whose type
   --  is Typ. If Typ has a default initialization, and there is no explicit
   --  initialization in the source declaration, check whether the address
   --  clause might cause overlaying of an entity, and emit a warning on the
   --  side effect that the initialization will cause.

   -------------------------------
   -- Adjust_Esize_For_Alignment --
   -------------------------------

   procedure Adjust_Esize_For_Alignment (Typ : Entity_Id) is
      Align : Uint;

   begin
      if Known_Esize (Typ) and then Known_Alignment (Typ) then
         Align := Alignment_In_Bits (Typ);

         if Align > Esize (Typ)
           and then Align <= Standard_Long_Long_Integer_Size
         then
            Set_Esize (Typ, Align);
         end if;
      end if;
   end Adjust_Esize_For_Alignment;

   ------------------------------------
   -- Build_And_Analyze_Renamed_Body --
   ------------------------------------

   procedure Build_And_Analyze_Renamed_Body
     (Decl  : Node_Id;
      New_S : Entity_Id;
      After : in out Node_Id)
   is
      Body_Node : constant Node_Id := Build_Renamed_Body (Decl, New_S);
   begin
      Insert_After (After, Body_Node);
      Mark_Rewrite_Insertion (Body_Node);
      Analyze (Body_Node);
      After := Body_Node;
   end Build_And_Analyze_Renamed_Body;

   ------------------------
   -- Build_Renamed_Body --
   ------------------------

   function Build_Renamed_Body
     (Decl  : Node_Id;
      New_S : Entity_Id) return Node_Id
   is
      Loc : constant Source_Ptr := Sloc (New_S);
      --  We use for the source location of the renamed body, the location
      --  of the spec entity. It might seem more natural to use the location
      --  of the renaming declaration itself, but that would be wrong, since
      --  then the body we create would look as though it was created far
      --  too late, and this could cause problems with elaboration order
      --  analysis, particularly in connection with instantiations.

      N          : constant Node_Id := Unit_Declaration_Node (New_S);
      Nam        : constant Node_Id := Name (N);
      Old_S      : Entity_Id;
      Spec       : constant Node_Id := New_Copy_Tree (Specification (Decl));
      Actuals    : List_Id := No_List;
      Call_Node  : Node_Id;
      Call_Name  : Node_Id;
      Body_Node  : Node_Id;
      Formal     : Entity_Id;
      O_Formal   : Entity_Id;
      Param_Spec : Node_Id;

      Pref : Node_Id := Empty;
      --  If the renamed entity is a primitive operation given in prefix form,
      --  the prefix is the target object and it has to be added as the first
      --  actual in the generated call.

   begin
      --  Determine the entity being renamed, which is the target of the call
      --  statement. If the name is an explicit dereference, this is a renaming
      --  of a subprogram type rather than a subprogram. The name itself is
      --  fully analyzed.

      if Nkind (Nam) = N_Selected_Component then
         Old_S := Entity (Selector_Name (Nam));

      elsif Nkind (Nam) = N_Explicit_Dereference then
         Old_S := Etype (Nam);

      elsif Nkind (Nam) = N_Indexed_Component then
         if Is_Entity_Name (Prefix (Nam)) then
            Old_S := Entity (Prefix (Nam));
         else
            Old_S := Entity (Selector_Name (Prefix (Nam)));
         end if;

      elsif Nkind (Nam) = N_Character_Literal then
         Old_S := Etype (New_S);

      else
         Old_S := Entity (Nam);
      end if;

      if Is_Entity_Name (Nam) then

         --  If the renamed entity is a predefined operator, retain full name
         --  to ensure its visibility.

         if Ekind (Old_S) = E_Operator
           and then Nkind (Nam) = N_Expanded_Name
         then
            Call_Name := New_Copy (Name (N));
         else
            Call_Name := New_Reference_To (Old_S, Loc);
         end if;

      else
         if Nkind (Nam) = N_Selected_Component
           and then Present (First_Formal (Old_S))
           and then
             (Is_Controlling_Formal (First_Formal (Old_S))
                or else Is_Class_Wide_Type (Etype (First_Formal (Old_S))))
         then

            --  Retrieve the target object, to be added as a first actual
            --  in the call.

            Call_Name := New_Occurrence_Of (Old_S, Loc);
            Pref := Prefix (Nam);

         else
            Call_Name := New_Copy (Name (N));
         end if;

         --  The original name may have been overloaded, but
         --  is fully resolved now.

         Set_Is_Overloaded (Call_Name, False);
      end if;

      --  For simple renamings, subsequent calls can be expanded directly as
      --  called to the renamed entity. The body must be generated in any case
      --  for calls they may appear elsewhere.

      if (Ekind (Old_S) = E_Function
           or else Ekind (Old_S) = E_Procedure)
        and then Nkind (Decl) = N_Subprogram_Declaration
      then
         Set_Body_To_Inline (Decl, Old_S);
      end if;

      --  The body generated for this renaming is an internal artifact, and
      --  does not  constitute a freeze point for the called entity.

      Set_Must_Not_Freeze (Call_Name);

      Formal := First_Formal (Defining_Entity (Decl));

      if Present (Pref) then
         declare
            Pref_Type : constant Entity_Id := Etype (Pref);
            Form_Type : constant Entity_Id := Etype (First_Formal (Old_S));

         begin

            --  The controlling formal may be an access parameter, or the
            --  actual may be an access value, so adjust accordingly.

            if Is_Access_Type (Pref_Type)
              and then not Is_Access_Type (Form_Type)
            then
               Actuals := New_List
                 (Make_Explicit_Dereference (Loc, Relocate_Node (Pref)));

            elsif Is_Access_Type (Form_Type)
              and then not Is_Access_Type (Pref)
            then
               Actuals := New_List
                 (Make_Attribute_Reference (Loc,
                   Attribute_Name => Name_Access,
                   Prefix => Relocate_Node (Pref)));
            else
               Actuals := New_List (Pref);
            end if;
         end;

      elsif Present (Formal) then
         Actuals := New_List;

      else
         Actuals := No_List;
      end if;

      if Present (Formal) then
         while Present (Formal) loop
            Append (New_Reference_To (Formal, Loc), Actuals);
            Next_Formal (Formal);
         end loop;
      end if;

      --  If the renamed entity is an entry, inherit its profile. For other
      --  renamings as bodies, both profiles must be subtype conformant, so it
      --  is not necessary to replace the profile given in the declaration.
      --  However, default values that are aggregates are rewritten when
      --  partially analyzed, so we recover the original aggregate to insure
      --  that subsequent conformity checking works. Similarly, if the default
      --  expression was constant-folded, recover the original expression.

      Formal := First_Formal (Defining_Entity (Decl));

      if Present (Formal) then
         O_Formal := First_Formal (Old_S);
         Param_Spec := First (Parameter_Specifications (Spec));

         while Present (Formal) loop
            if Is_Entry (Old_S) then

               if Nkind (Parameter_Type (Param_Spec)) /=
                                                    N_Access_Definition
               then
                  Set_Etype (Formal, Etype (O_Formal));
                  Set_Entity (Parameter_Type (Param_Spec), Etype (O_Formal));
               end if;

            elsif Nkind (Default_Value (O_Formal)) = N_Aggregate
              or else Nkind (Original_Node (Default_Value (O_Formal))) /=
                                           Nkind (Default_Value (O_Formal))
            then
               Set_Expression (Param_Spec,
                 New_Copy_Tree (Original_Node (Default_Value (O_Formal))));
            end if;

            Next_Formal (Formal);
            Next_Formal (O_Formal);
            Next (Param_Spec);
         end loop;
      end if;

      --  If the renamed entity is a function, the generated body contains a
      --  return statement. Otherwise, build a procedure call. If the entity is
      --  an entry, subsequent analysis of the call will transform it into the
      --  proper entry or protected operation call. If the renamed entity is
      --  a character literal, return it directly.

      if Ekind (Old_S) = E_Function
        or else Ekind (Old_S) = E_Operator
        or else (Ekind (Old_S) = E_Subprogram_Type
                  and then Etype (Old_S) /= Standard_Void_Type)
      then
         Call_Node :=
           Make_Simple_Return_Statement (Loc,
              Expression =>
                Make_Function_Call (Loc,
                  Name => Call_Name,
                  Parameter_Associations => Actuals));

      elsif Ekind (Old_S) = E_Enumeration_Literal then
         Call_Node :=
           Make_Simple_Return_Statement (Loc,
              Expression => New_Occurrence_Of (Old_S, Loc));

      elsif Nkind (Nam) = N_Character_Literal then
         Call_Node :=
           Make_Simple_Return_Statement (Loc,
             Expression => Call_Name);

      else
         Call_Node :=
           Make_Procedure_Call_Statement (Loc,
             Name => Call_Name,
             Parameter_Associations => Actuals);
      end if;

      --  Create entities for subprogram body and formals

      Set_Defining_Unit_Name (Spec,
        Make_Defining_Identifier (Loc, Chars => Chars (New_S)));

      Param_Spec := First (Parameter_Specifications (Spec));

      while Present (Param_Spec) loop
         Set_Defining_Identifier (Param_Spec,
           Make_Defining_Identifier (Loc,
             Chars => Chars (Defining_Identifier (Param_Spec))));
         Next (Param_Spec);
      end loop;

      Body_Node :=
        Make_Subprogram_Body (Loc,
          Specification => Spec,
          Declarations => New_List,
          Handled_Statement_Sequence =>
            Make_Handled_Sequence_Of_Statements (Loc,
              Statements => New_List (Call_Node)));

      if Nkind (Decl) /= N_Subprogram_Declaration then
         Rewrite (N,
           Make_Subprogram_Declaration (Loc,
             Specification => Specification (N)));
      end if;

      --  Link the body to the entity whose declaration it completes. If
      --  the body is analyzed when the renamed entity is frozen, it may
      --  be necessary to restore the proper scope (see package Exp_Ch13).

      if Nkind (N) =  N_Subprogram_Renaming_Declaration
        and then Present (Corresponding_Spec (N))
      then
         Set_Corresponding_Spec (Body_Node, Corresponding_Spec (N));
      else
         Set_Corresponding_Spec (Body_Node, New_S);
      end if;

      return Body_Node;
   end Build_Renamed_Body;

   --------------------------
   -- Check_Address_Clause --
   --------------------------

   procedure Check_Address_Clause (E : Entity_Id) is
      Addr : constant Node_Id   := Address_Clause (E);
      Expr : Node_Id;
      Decl : constant Node_Id   := Declaration_Node (E);
      Typ  : constant Entity_Id := Etype (E);

   begin
      if Present (Addr) then
         Expr := Expression (Addr);

         --  If we have no initialization of any kind, then we don't need to
         --  place any restrictions on the address clause, because the object
         --  will be elaborated after the address clause is evaluated. This
         --  happens if the declaration has no initial expression, or the type
         --  has no implicit initialization, or the object is imported.

         --  The same holds for all initialized scalar types and all access
         --  types. Packed bit arrays of size up to 64 are represented using a
         --  modular type with an initialization (to zero) and can be processed
         --  like other initialized scalar types.

         --  If the type is controlled, code to attach the object to a
         --  finalization chain is generated at the point of declaration,
         --  and therefore the elaboration of the object cannot be delayed:
         --  the address expression must be a constant.

         if (No (Expression (Decl))
              and then not Needs_Finalization (Typ)
              and then
                (not Has_Non_Null_Base_Init_Proc (Typ)
                  or else Is_Imported (E)))

           or else
             (Present (Expression (Decl))
               and then Is_Scalar_Type (Typ))

           or else
             Is_Access_Type (Typ)

           or else
             (Is_Bit_Packed_Array (Typ)
               and then
                 Is_Modular_Integer_Type (Packed_Array_Type (Typ)))
         then
            null;

         --  Otherwise, we require the address clause to be constant because
         --  the call to the initialization procedure (or the attach code) has
         --  to happen at the point of the declaration.
         --  Actually the IP call has been moved to the freeze actions
         --  anyway, so maybe we can relax this restriction???

         else
            Check_Constant_Address_Clause (Expr, E);

            --  Has_Delayed_Freeze was set on E when the address clause was
            --  analyzed. Reset the flag now unless freeze actions were
            --  attached to it in the mean time.

            if No (Freeze_Node (E)) then
               Set_Has_Delayed_Freeze (E, False);
            end if;
         end if;

         if not Error_Posted (Expr)
           and then not Needs_Finalization (Typ)
         then
            Warn_Overlay (Expr, Typ, Name (Addr));
         end if;
      end if;
   end Check_Address_Clause;

   -----------------------------
   -- Check_Compile_Time_Size --
   -----------------------------

   procedure Check_Compile_Time_Size (T : Entity_Id) is

      procedure Set_Small_Size (T : Entity_Id; S : Uint);
      --  Sets the compile time known size (32 bits or less) in the Esize
      --  field, of T checking for a size clause that was given which attempts
      --  to give a smaller size.

      function Size_Known (T : Entity_Id) return Boolean;
      --  Recursive function that does all the work

      function Static_Discriminated_Components (T : Entity_Id) return Boolean;
      --  If T is a constrained subtype, its size is not known if any of its
      --  discriminant constraints is not static and it is not a null record.
      --  The test is conservative and doesn't check that the components are
      --  in fact constrained by non-static discriminant values. Could be made
      --  more precise ???

      --------------------
      -- Set_Small_Size --
      --------------------

      procedure Set_Small_Size (T : Entity_Id; S : Uint) is
      begin
         if S > 32 then
            return;

         elsif Has_Size_Clause (T) then
            if RM_Size (T) < S then
               Error_Msg_Uint_1 := S;
               Error_Msg_NE
                 ("size for& too small, minimum allowed is ^",
                  Size_Clause (T), T);

            elsif Unknown_Esize (T) then
               Set_Esize (T, S);
            end if;

         --  Set sizes if not set already

         else
            if Unknown_Esize (T) then
               Set_Esize (T, S);
            end if;

            if Unknown_RM_Size (T) then
               Set_RM_Size (T, S);
            end if;
         end if;
      end Set_Small_Size;

      ----------------
      -- Size_Known --
      ----------------

      function Size_Known (T : Entity_Id) return Boolean is
         Index : Entity_Id;
         Comp  : Entity_Id;
         Ctyp  : Entity_Id;
         Low   : Node_Id;
         High  : Node_Id;

      begin
         if Size_Known_At_Compile_Time (T) then
            return True;

         --  Always True for scalar types. This is true even for generic formal
         --  scalar types. We used to return False in the latter case, but the
         --  size is known at compile time, even in the template, we just do
         --  not know the exact size but that's not the point of this routine.

         elsif Is_Scalar_Type (T)
           or else Is_Task_Type (T)
         then
            return True;

         --  Array types

         elsif Is_Array_Type (T) then

            --  String literals always have known size, and we can set it

            if Ekind (T) = E_String_Literal_Subtype then
               Set_Small_Size (T, Component_Size (T)
                               * String_Literal_Length (T));
               return True;

            --  Unconstrained types never have known at compile time size

            elsif not Is_Constrained (T) then
               return False;

            --  Don't do any recursion on type with error posted, since we may
            --  have a malformed type that leads us into a loop.

            elsif Error_Posted (T) then
               return False;

            --  Otherwise if component size unknown, then array size unknown

            elsif not Size_Known (Component_Type (T)) then
               return False;
            end if;

            --  Check for all indexes static, and also compute possible size
            --  (in case it is less than 32 and may be packable).

            declare
               Esiz : Uint := Component_Size (T);
               Dim  : Uint;

            begin
               Index := First_Index (T);
               while Present (Index) loop
                  if Nkind (Index) = N_Range then
                     Get_Index_Bounds (Index, Low, High);

                  elsif Error_Posted (Scalar_Range (Etype (Index))) then
                     return False;

                  else
                     Low  := Type_Low_Bound (Etype (Index));
                     High := Type_High_Bound (Etype (Index));
                  end if;

                  if not Compile_Time_Known_Value (Low)
                    or else not Compile_Time_Known_Value (High)
                    or else Etype (Index) = Any_Type
                  then
                     return False;

                  else
                     Dim := Expr_Value (High) - Expr_Value (Low) + 1;

                     if Dim >= 0 then
                        Esiz := Esiz * Dim;
                     else
                        Esiz := Uint_0;
                     end if;
                  end if;

                  Next_Index (Index);
               end loop;

               Set_Small_Size (T, Esiz);
               return True;
            end;

         --  Access types always have known at compile time sizes

         elsif Is_Access_Type (T) then
            return True;

         --  For non-generic private types, go to underlying type if present

         elsif Is_Private_Type (T)
           and then not Is_Generic_Type (T)
           and then Present (Underlying_Type (T))
         then
            --  Don't do any recursion on type with error posted, since we may
            --  have a malformed type that leads us into a loop.

            if Error_Posted (T) then
               return False;
            else
               return Size_Known (Underlying_Type (T));
            end if;

         --  Record types

         elsif Is_Record_Type (T) then

            --  A class-wide type is never considered to have a known size

            if Is_Class_Wide_Type (T) then
               return False;

            --  A subtype of a variant record must not have non-static
            --  discriminanted components.

            elsif T /= Base_Type (T)
              and then not Static_Discriminated_Components (T)
            then
               return False;

            --  Don't do any recursion on type with error posted, since we may
            --  have a malformed type that leads us into a loop.

            elsif Error_Posted (T) then
               return False;
            end if;

            --  Now look at the components of the record

            declare
               --  The following two variables are used to keep track of the
               --  size of packed records if we can tell the size of the packed
               --  record in the front end. Packed_Size_Known is True if so far
               --  we can figure out the size. It is initialized to True for a
               --  packed record, unless the record has discriminants. The
               --  reason we eliminate the discriminated case is that we don't
               --  know the way the back end lays out discriminated packed
               --  records. If Packed_Size_Known is True, then Packed_Size is
               --  the size in bits so far.

               Packed_Size_Known : Boolean :=
                                     Is_Packed (T)
                                       and then not Has_Discriminants (T);

               Packed_Size : Uint := Uint_0;

            begin
               --  Test for variant part present

               if Has_Discriminants (T)
                 and then Present (Parent (T))
                 and then Nkind (Parent (T)) = N_Full_Type_Declaration
                 and then Nkind (Type_Definition (Parent (T))) =
                            N_Record_Definition
                 and then not Null_Present (Type_Definition (Parent (T)))
                 and then Present (Variant_Part
                            (Component_List (Type_Definition (Parent (T)))))
               then
                  --  If variant part is present, and type is unconstrained,
                  --  then we must have defaulted discriminants, or a size
                  --  clause must be present for the type, or else the size
                  --  is definitely not known at compile time.

                  if not Is_Constrained (T)
                    and then
                      No (Discriminant_Default_Value
                           (First_Discriminant (T)))
                    and then Unknown_Esize (T)
                  then
                     return False;
                  end if;
               end if;

               --  Loop through components

               Comp := First_Component_Or_Discriminant (T);
               while Present (Comp) loop
                  Ctyp := Etype (Comp);

                  --  We do not know the packed size if there is a component
                  --  clause present (we possibly could, but this would only
                  --  help in the case of a record with partial rep clauses.
                  --  That's because in the case of full rep clauses, the
                  --  size gets figured out anyway by a different circuit).

                  if Present (Component_Clause (Comp)) then
                     Packed_Size_Known := False;
                  end if;

                  --  We need to identify a component that is an array where
                  --  the index type is an enumeration type with non-standard
                  --  representation, and some bound of the type depends on a
                  --  discriminant.

                  --  This is because gigi computes the size by doing a
                  --  substitution of the appropriate discriminant value in
                  --  the size expression for the base type, and gigi is not
                  --  clever enough to evaluate the resulting expression (which
                  --  involves a call to rep_to_pos) at compile time.

                  --  It would be nice if gigi would either recognize that
                  --  this expression can be computed at compile time, or
                  --  alternatively figured out the size from the subtype
                  --  directly, where all the information is at hand ???

                  if Is_Array_Type (Etype (Comp))
                    and then Present (Packed_Array_Type (Etype (Comp)))
                  then
                     declare
                        Ocomp  : constant Entity_Id :=
                                   Original_Record_Component (Comp);
                        OCtyp  : constant Entity_Id := Etype (Ocomp);
                        Ind    : Node_Id;
                        Indtyp : Entity_Id;
                        Lo, Hi : Node_Id;

                     begin
                        Ind := First_Index (OCtyp);
                        while Present (Ind) loop
                           Indtyp := Etype (Ind);

                           if Is_Enumeration_Type (Indtyp)
                             and then Has_Non_Standard_Rep (Indtyp)
                           then
                              Lo := Type_Low_Bound  (Indtyp);
                              Hi := Type_High_Bound (Indtyp);

                              if Is_Entity_Name (Lo)
                                and then Ekind (Entity (Lo)) = E_Discriminant
                              then
                                 return False;

                              elsif Is_Entity_Name (Hi)
                                and then Ekind (Entity (Hi)) = E_Discriminant
                              then
                                 return False;
                              end if;
                           end if;

                           Next_Index (Ind);
                        end loop;
                     end;
                  end if;

                  --  Clearly size of record is not known if the size of one of
                  --  the components is not known.

                  if not Size_Known (Ctyp) then
                     return False;
                  end if;

                  --  Accumulate packed size if possible

                  if Packed_Size_Known then

                     --  We can only deal with elementary types, since for
                     --  non-elementary components, alignment enters into the
                     --  picture, and we don't know enough to handle proper
                     --  alignment in this context. Packed arrays count as
                     --  elementary if the representation is a modular type.

                     if Is_Elementary_Type (Ctyp)
                       or else (Is_Array_Type (Ctyp)
                                and then Present (Packed_Array_Type (Ctyp))
                                and then Is_Modular_Integer_Type
                                           (Packed_Array_Type (Ctyp)))
                     then
                        --  If RM_Size is known and static, then we can
                        --  keep accumulating the packed size.

                        if Known_Static_RM_Size (Ctyp) then

                           --  A little glitch, to be removed sometime ???
                           --  gigi does not understand zero sizes yet.

                           if RM_Size (Ctyp) = Uint_0 then
                              Packed_Size_Known := False;

                           --  Normal case where we can keep accumulating the
                           --  packed array size.

                           else
                              Packed_Size := Packed_Size + RM_Size (Ctyp);
                           end if;

                        --  If we have a field whose RM_Size is not known then
                        --  we can't figure out the packed size here.

                        else
                           Packed_Size_Known := False;
                        end if;

                     --  If we have a non-elementary type we can't figure out
                     --  the packed array size (alignment issues).

                     else
                        Packed_Size_Known := False;
                     end if;
                  end if;

                  Next_Component_Or_Discriminant (Comp);
               end loop;

               if Packed_Size_Known then
                  Set_Small_Size (T, Packed_Size);
               end if;

               return True;
            end;

         --  All other cases, size not known at compile time

         else
            return False;
         end if;
      end Size_Known;

      -------------------------------------
      -- Static_Discriminated_Components --
      -------------------------------------

      function Static_Discriminated_Components
        (T : Entity_Id) return Boolean
      is
         Constraint : Elmt_Id;

      begin
         if Has_Discriminants (T)
           and then Present (Discriminant_Constraint (T))
           and then Present (First_Component (T))
         then
            Constraint := First_Elmt (Discriminant_Constraint (T));
            while Present (Constraint) loop
               if not Compile_Time_Known_Value (Node (Constraint)) then
                  return False;
               end if;

               Next_Elmt (Constraint);
            end loop;
         end if;

         return True;
      end Static_Discriminated_Components;

   --  Start of processing for Check_Compile_Time_Size

   begin
      Set_Size_Known_At_Compile_Time (T, Size_Known (T));
   end Check_Compile_Time_Size;

   -----------------------------
   -- Check_Debug_Info_Needed --
   -----------------------------

   procedure Check_Debug_Info_Needed (T : Entity_Id) is
   begin
      if Debug_Info_Off (T) then
         return;

      elsif Comes_From_Source (T)
        or else Debug_Generated_Code
        or else Debug_Flag_VV
        or else Needs_Debug_Info (T)
      then
         Set_Debug_Info_Needed (T);
      end if;
   end Check_Debug_Info_Needed;

   ----------------------------
   -- Check_Strict_Alignment --
   ----------------------------

   procedure Check_Strict_Alignment (E : Entity_Id) is
      Comp  : Entity_Id;

   begin
      if Is_Tagged_Type (E) or else Is_Concurrent_Type (E) then
         Set_Strict_Alignment (E);

      elsif Is_Array_Type (E) then
         Set_Strict_Alignment (E, Strict_Alignment (Component_Type (E)));

      elsif Is_Record_Type (E) then
         if Is_Limited_Record (E) then
            Set_Strict_Alignment (E);
            return;
         end if;

         Comp := First_Component (E);

         while Present (Comp) loop
            if not Is_Type (Comp)
              and then (Strict_Alignment (Etype (Comp))
                         or else Is_Aliased (Comp))
            then
               Set_Strict_Alignment (E);
               return;
            end if;

            Next_Component (Comp);
         end loop;
      end if;
   end Check_Strict_Alignment;

   -------------------------
   -- Check_Unsigned_Type --
   -------------------------

   procedure Check_Unsigned_Type (E : Entity_Id) is
      Ancestor : Entity_Id;
      Lo_Bound : Node_Id;
      Btyp     : Entity_Id;

   begin
      if not Is_Discrete_Or_Fixed_Point_Type (E) then
         return;
      end if;

      --  Do not attempt to analyze case where range was in error

      if Error_Posted (Scalar_Range (E)) then
         return;
      end if;

      --  The situation that is non trivial is something like

      --     subtype x1 is integer range -10 .. +10;
      --     subtype x2 is x1 range 0 .. V1;
      --     subtype x3 is x2 range V2 .. V3;
      --     subtype x4 is x3 range V4 .. V5;

      --  where Vn are variables. Here the base type is signed, but we still
      --  know that x4 is unsigned because of the lower bound of x2.

      --  The only way to deal with this is to look up the ancestor chain

      Ancestor := E;
      loop
         if Ancestor = Any_Type or else Etype (Ancestor) = Any_Type then
            return;
         end if;

         Lo_Bound := Type_Low_Bound (Ancestor);

         if Compile_Time_Known_Value (Lo_Bound) then

            if Expr_Rep_Value (Lo_Bound) >= 0 then
               Set_Is_Unsigned_Type (E, True);
            end if;

            return;

         else
            Ancestor := Ancestor_Subtype (Ancestor);

            --  If no ancestor had a static lower bound, go to base type

            if No (Ancestor) then

               --  Note: the reason we still check for a compile time known
               --  value for the base type is that at least in the case of
               --  generic formals, we can have bounds that fail this test,
               --  and there may be other cases in error situations.

               Btyp := Base_Type (E);

               if Btyp = Any_Type or else Etype (Btyp) = Any_Type then
                  return;
               end if;

               Lo_Bound := Type_Low_Bound (Base_Type (E));

               if Compile_Time_Known_Value (Lo_Bound)
                 and then Expr_Rep_Value (Lo_Bound) >= 0
               then
                  Set_Is_Unsigned_Type (E, True);
               end if;

               return;
            end if;
         end if;
      end loop;
   end Check_Unsigned_Type;

   -------------------------
   -- Is_Atomic_Aggregate --
   -------------------------

   function  Is_Atomic_Aggregate
     (E   : Entity_Id;
      Typ : Entity_Id) return Boolean
   is
      Loc   : constant Source_Ptr := Sloc (E);
      New_N : Node_Id;
      Par   : Node_Id;
      Temp  : Entity_Id;

   begin
      Par := Parent (E);

      --  Array may be qualified, so find outer context

      if Nkind (Par) = N_Qualified_Expression then
         Par := Parent (Par);
      end if;

      if Nkind_In (Par, N_Object_Declaration, N_Assignment_Statement)
        and then Comes_From_Source (Par)
      then
         Temp :=
           Make_Defining_Identifier (Loc,
             New_Internal_Name ('T'));

         New_N :=
           Make_Object_Declaration (Loc,
             Defining_Identifier => Temp,
             Object_Definition   => New_Occurrence_Of (Typ, Loc),
             Expression          => Relocate_Node (E));
         Insert_Before (Par, New_N);
         Analyze (New_N);

         Set_Expression (Par, New_Occurrence_Of (Temp, Loc));
         return True;

      else
         return False;
      end if;
   end Is_Atomic_Aggregate;

   ----------------
   -- Freeze_All --
   ----------------

   --  Note: the easy coding for this procedure would be to just build a
   --  single list of freeze nodes and then insert them and analyze them
   --  all at once. This won't work, because the analysis of earlier freeze
   --  nodes may recursively freeze types which would otherwise appear later
   --  on in the freeze list. So we must analyze and expand the freeze nodes
   --  as they are generated.

   procedure Freeze_All (From : Entity_Id; After : in out Node_Id) is
      Loc   : constant Source_Ptr := Sloc (After);
      E     : Entity_Id;
      Decl  : Node_Id;

      procedure Freeze_All_Ent (From : Entity_Id; After : in out Node_Id);
      --  This is the internal recursive routine that does freezing of entities
      --  (but NOT the analysis of default expressions, which should not be
      --  recursive, we don't want to analyze those till we are sure that ALL
      --  the types are frozen).

      --------------------
      -- Freeze_All_Ent --
      --------------------

      procedure Freeze_All_Ent
        (From  : Entity_Id;
         After : in out Node_Id)
      is
         E     : Entity_Id;
         Flist : List_Id;
         Lastn : Node_Id;

         procedure Process_Flist;
         --  If freeze nodes are present, insert and analyze, and reset cursor
         --  for next insertion.

         -------------------
         -- Process_Flist --
         -------------------

         procedure Process_Flist is
         begin
            if Is_Non_Empty_List (Flist) then
               Lastn := Next (After);
               Insert_List_After_And_Analyze (After, Flist);

               if Present (Lastn) then
                  After := Prev (Lastn);
               else
                  After := Last (List_Containing (After));
               end if;
            end if;
         end Process_Flist;

      --  Start or processing for Freeze_All_Ent

      begin
         E := From;
         while Present (E) loop

            --  If the entity is an inner package which is not a package
            --  renaming, then its entities must be frozen at this point. Note
            --  that such entities do NOT get frozen at the end of the nested
            --  package itself (only library packages freeze).

            --  Same is true for task declarations, where anonymous records
            --  created for entry parameters must be frozen.

            if Ekind (E) = E_Package
              and then No (Renamed_Object (E))
              and then not Is_Child_Unit (E)
              and then not Is_Frozen (E)
            then
               Push_Scope (E);
               Install_Visible_Declarations (E);
               Install_Private_Declarations (E);

               Freeze_All (First_Entity (E), After);

               End_Package_Scope (E);

            elsif Ekind (E) in Task_Kind
              and then
                (Nkind (Parent (E)) = N_Task_Type_Declaration
                   or else
                 Nkind (Parent (E)) = N_Single_Task_Declaration)
            then
               Push_Scope (E);
               Freeze_All (First_Entity (E), After);
               End_Scope;

            --  For a derived tagged type, we must ensure that all the
            --  primitive operations of the parent have been frozen, so that
            --  their addresses will be in the parent's dispatch table at the
            --  point it is inherited.

            elsif Ekind (E) = E_Record_Type
              and then Is_Tagged_Type (E)
              and then Is_Tagged_Type (Etype (E))
              and then Is_Derived_Type (E)
            then
               declare
                  Prim_List : constant Elist_Id :=
                               Primitive_Operations (Etype (E));

                  Prim : Elmt_Id;
                  Subp : Entity_Id;

               begin
                  Prim  := First_Elmt (Prim_List);

                  while Present (Prim) loop
                     Subp := Node (Prim);

                     if Comes_From_Source (Subp)
                       and then not Is_Frozen (Subp)
                     then
                        Flist := Freeze_Entity (Subp, Loc);
                        Process_Flist;
                     end if;

                     Next_Elmt (Prim);
                  end loop;
               end;
            end if;

            if not Is_Frozen (E) then
               Flist := Freeze_Entity (E, Loc);
               Process_Flist;
            end if;

            --  If an incomplete type is still not frozen, this may be a
            --  premature freezing because of a body declaration that follows.
            --  Indicate where the freezing took place.

            --  If the freezing is caused by the end of the current declarative
            --  part, it is a Taft Amendment type, and there is no error.

            if not Is_Frozen (E)
              and then Ekind (E) = E_Incomplete_Type
            then
               declare
                  Bod : constant Node_Id := Next (After);

               begin
                  if (Nkind (Bod) = N_Subprogram_Body
                        or else Nkind (Bod) = N_Entry_Body
                        or else Nkind (Bod) = N_Package_Body
                        or else Nkind (Bod) = N_Protected_Body
                        or else Nkind (Bod) = N_Task_Body
                        or else Nkind (Bod) in N_Body_Stub)
                     and then
                       List_Containing (After) = List_Containing (Parent (E))
                  then
                     Error_Msg_Sloc := Sloc (Next (After));
                     Error_Msg_NE
                       ("type& is frozen# before its full declaration",
                         Parent (E), E);
                  end if;
               end;
            end if;

            Next_Entity (E);
         end loop;
      end Freeze_All_Ent;

   --  Start of processing for Freeze_All

   begin
      Freeze_All_Ent (From, After);

      --  Now that all types are frozen, we can deal with default expressions
      --  that require us to build a default expression functions. This is the
      --  point at which such functions are constructed (after all types that
      --  might be used in such expressions have been frozen).

      --  We also add finalization chains to access types whose designated
      --  types are controlled. This is normally done when freezing the type,
      --  but this misses recursive type definitions where the later members
      --  of the recursion introduce controlled components.

      --  Loop through entities

      E := From;
      while Present (E) loop
         if Is_Subprogram (E) then

            if not Default_Expressions_Processed (E) then
               Process_Default_Expressions (E, After);
            end if;

            if not Has_Completion (E) then
               Decl := Unit_Declaration_Node (E);

               if Nkind (Decl) = N_Subprogram_Renaming_Declaration then
                  Build_And_Analyze_Renamed_Body (Decl, E, After);

               elsif Nkind (Decl) = N_Subprogram_Declaration
                 and then Present (Corresponding_Body (Decl))
                 and then
                   Nkind (Unit_Declaration_Node (Corresponding_Body (Decl)))
                                          = N_Subprogram_Renaming_Declaration
               then
                  Build_And_Analyze_Renamed_Body
                    (Decl, Corresponding_Body (Decl), After);
               end if;
            end if;

         elsif Ekind (E) in Task_Kind
           and then
             (Nkind (Parent (E)) = N_Task_Type_Declaration
                or else
              Nkind (Parent (E)) = N_Single_Task_Declaration)
         then
            declare
               Ent : Entity_Id;
            begin
               Ent := First_Entity (E);

               while Present (Ent) loop

                  if Is_Entry (Ent)
                    and then not Default_Expressions_Processed (Ent)
                  then
                     Process_Default_Expressions (Ent, After);
                  end if;

                  Next_Entity (Ent);
               end loop;
            end;

         elsif Is_Access_Type (E)
           and then Comes_From_Source (E)
           and then Ekind (Directly_Designated_Type (E)) = E_Incomplete_Type
           and then Needs_Finalization (Designated_Type (E))
           and then No (Associated_Final_Chain (E))
         then
            Build_Final_List (Parent (E), E);
         end if;

         Next_Entity (E);
      end loop;
   end Freeze_All;

   -----------------------
   -- Freeze_And_Append --
   -----------------------

   procedure Freeze_And_Append
     (Ent    : Entity_Id;
      Loc    : Source_Ptr;
      Result : in out List_Id)
   is
      L : constant List_Id := Freeze_Entity (Ent, Loc);
   begin
      if Is_Non_Empty_List (L) then
         if Result = No_List then
            Result := L;
         else
            Append_List (L, Result);
         end if;
      end if;
   end Freeze_And_Append;

   -------------------
   -- Freeze_Before --
   -------------------

   procedure Freeze_Before (N : Node_Id; T : Entity_Id) is
      Freeze_Nodes : constant List_Id := Freeze_Entity (T, Sloc (N));
   begin
      if Is_Non_Empty_List (Freeze_Nodes) then
         Insert_Actions (N, Freeze_Nodes);
      end if;
   end Freeze_Before;

   -------------------
   -- Freeze_Entity --
   -------------------

   function Freeze_Entity (E : Entity_Id; Loc : Source_Ptr) return List_Id is
      Test_E : Entity_Id := E;
      Comp   : Entity_Id;
      F_Node : Node_Id;
      Result : List_Id;
      Indx   : Node_Id;
      Formal : Entity_Id;
      Atype  : Entity_Id;

      Has_Default_Initialization : Boolean := False;
      --  This flag gets set to true for a variable with default initialization

      procedure Check_Current_Instance (Comp_Decl : Node_Id);
      --  Check that an Access or Unchecked_Access attribute with a prefix
      --  which is the current instance type can only be applied when the type
      --  is limited.

      procedure Check_Suspicious_Modulus (Utype : Entity_Id);
      --  Give warning for modulus of 8, 16, 32, or 64 given as an explicit
      --  integer literal without an explicit corresponding size clause. The
      --  caller has checked that Utype is a modular integer type.

      function After_Last_Declaration return Boolean;
      --  If Loc is a freeze_entity that appears after the last declaration
      --  in the scope, inhibit error messages on late completion.

      procedure Freeze_Record_Type (Rec : Entity_Id);
      --  Freeze each component, handle some representation clauses, and freeze
      --  primitive operations if this is a tagged type.

      ----------------------------
      -- After_Last_Declaration --
      ----------------------------

      function After_Last_Declaration return Boolean is
         Spec : constant Node_Id := Parent (Current_Scope);
      begin
         if Nkind (Spec) = N_Package_Specification then
            if Present (Private_Declarations (Spec)) then
               return Loc >= Sloc (Last (Private_Declarations (Spec)));
            elsif Present (Visible_Declarations (Spec)) then
               return Loc >= Sloc (Last (Visible_Declarations (Spec)));
            else
               return False;
            end if;
         else
            return False;
         end if;
      end After_Last_Declaration;

      ----------------------------
      -- Check_Current_Instance --
      ----------------------------

      procedure Check_Current_Instance (Comp_Decl : Node_Id) is

         Rec_Type : constant Entity_Id :=
                      Scope (Defining_Identifier (Comp_Decl));

         Decl : constant Node_Id := Parent (Rec_Type);

         function Process (N : Node_Id) return Traverse_Result;
         --  Process routine to apply check to given node

         -------------
         -- Process --
         -------------

         function Process (N : Node_Id) return Traverse_Result is
         begin
            case Nkind (N) is
               when N_Attribute_Reference =>
                  if (Attribute_Name (N) = Name_Access
                        or else
                      Attribute_Name (N) = Name_Unchecked_Access)
                    and then Is_Entity_Name (Prefix (N))
                    and then Is_Type (Entity (Prefix (N)))
                    and then Entity (Prefix (N)) = E
                  then
                     Error_Msg_N
                       ("current instance must be a limited type", Prefix (N));
                     return Abandon;
                  else
                     return OK;
                  end if;

               when others => return OK;
            end case;
         end Process;

         procedure Traverse is new Traverse_Proc (Process);

      --  Start of processing for Check_Current_Instance

      begin
         --  In Ada95, the (imprecise) rule is that the current instance of a
         --  limited type is aliased. In Ada2005, limitedness must be explicit:
         --  either a tagged type, or a limited record.

         if Is_Limited_Type (Rec_Type)
           and then (Ada_Version < Ada_05 or else Is_Tagged_Type (Rec_Type))
         then
            return;

         elsif Nkind (Decl) = N_Full_Type_Declaration
           and then Limited_Present (Type_Definition (Decl))
         then
            return;

         else
            Traverse (Comp_Decl);
         end if;
      end Check_Current_Instance;

      ------------------------------
      -- Check_Suspicious_Modulus --
      ------------------------------

      procedure Check_Suspicious_Modulus (Utype : Entity_Id) is
         Decl : constant Node_Id := Declaration_Node (Underlying_Type (Utype));

      begin
         if Nkind (Decl) = N_Full_Type_Declaration then
            declare
               Tdef : constant Node_Id := Type_Definition (Decl);
            begin
               if Nkind (Tdef) = N_Modular_Type_Definition then
                  declare
                     Modulus : constant Node_Id :=
                                 Original_Node (Expression (Tdef));
                  begin
                     if Nkind (Modulus) = N_Integer_Literal then
                        declare
                           Modv : constant Uint := Intval (Modulus);
                           Sizv : constant Uint := RM_Size (Utype);

                        begin
                           --  First case, modulus and size are the same. This
                           --  happens if you have something like mod 32, with
                           --  an explicit size of 32, this is for sure a case
                           --  where the warning is given, since it is seems
                           --  very unlikely that someone would want e.g. a
                           --  five bit type stored in 32 bits. It is much
                           --  more likely they wanted a 32-bit type.

                           if Modv = Sizv then
                              null;

                           --  Second case, the modulus is 32 or 64 and no
                           --  size clause is present. This is a less clear
                           --  case for giving the warning, but in the case
                           --  of 32/64 (5-bit or 6-bit types) these seem rare
                           --  enough that it is a likely error (and in any
                           --  case using 2**5 or 2**6 in these cases seems
                           --  clearer. We don't include 8 or 16 here, simply
                           --  because in practice 3-bit and 4-bit types are
                           --  more common and too many false positives if
                           --  we warn in these cases.

                           elsif not Has_Size_Clause (Utype)
                             and then (Modv = Uint_32 or else Modv = Uint_64)
                           then
                              null;

                           --  No warning needed

                           else
                              return;
                           end if;

                           --  If we fall through, give warning

                           Error_Msg_Uint_1 := Modv;
                           Error_Msg_N
                             ("?2 '*'*^' may have been intended here",
                              Modulus);
                        end;
                     end if;
                  end;
               end if;
            end;
         end if;
      end Check_Suspicious_Modulus;

      ------------------------
      -- Freeze_Record_Type --
      ------------------------

      procedure Freeze_Record_Type (Rec : Entity_Id) is
         Comp : Entity_Id;
         IR   : Node_Id;
         ADC  : Node_Id;
         Prev : Entity_Id;

         Junk : Boolean;
         pragma Warnings (Off, Junk);

         Unplaced_Component : Boolean := False;
         --  Set True if we find at least one component with no component
         --  clause (used to warn about useless Pack pragmas).

         Placed_Component : Boolean := False;
         --  Set True if we find at least one component with a component
         --  clause (used to warn about useless Bit_Order pragmas, and also
         --  to detect cases where Implicit_Packing may have an effect).

         All_Scalar_Components : Boolean := True;
         --  Set False if we encounter a component of a non-scalar type

         Scalar_Component_Total_RM_Size : Uint := Uint_0;
         Scalar_Component_Total_Esize   : Uint := Uint_0;
         --  Accumulates total RM_Size values and total Esize values of all
         --  scalar components. Used for processing of Implicit_Packing.

         function Check_Allocator (N : Node_Id) return Node_Id;
         --  If N is an allocator, possibly wrapped in one or more level of
         --  qualified expression(s), return the inner allocator node, else
         --  return Empty.

         procedure Check_Itype (Typ : Entity_Id);
         --  If the component subtype is an access to a constrained subtype of
         --  an already frozen type, make the subtype frozen as well. It might
         --  otherwise be frozen in the wrong scope, and a freeze node on
         --  subtype has no effect. Similarly, if the component subtype is a
         --  regular (not protected) access to subprogram, set the anonymous
         --  subprogram type to frozen as well, to prevent an out-of-scope
         --  freeze node at some eventual point of call. Protected operations
         --  are handled elsewhere.

         ---------------------
         -- Check_Allocator --
         ---------------------

         function Check_Allocator (N : Node_Id) return Node_Id is
            Inner : Node_Id;
         begin
            Inner := N;
            loop
               if Nkind (Inner) = N_Allocator then
                  return Inner;
               elsif Nkind (Inner) = N_Qualified_Expression then
                  Inner := Expression (Inner);
               else
                  return Empty;
               end if;
            end loop;
         end Check_Allocator;

         -----------------
         -- Check_Itype --
         -----------------

         procedure Check_Itype (Typ : Entity_Id) is
            Desig : constant Entity_Id := Designated_Type (Typ);

         begin
            if not Is_Frozen (Desig)
              and then Is_Frozen (Base_Type (Desig))
            then
               Set_Is_Frozen (Desig);

               --  In addition, add an Itype_Reference to ensure that the
               --  access subtype is elaborated early enough. This cannot be
               --  done if the subtype may depend on discriminants.

               if Ekind (Comp) = E_Component
                 and then Is_Itype (Etype (Comp))
                 and then not Has_Discriminants (Rec)
               then
                  IR := Make_Itype_Reference (Sloc (Comp));
                  Set_Itype (IR, Desig);

                  if No (Result) then
                     Result := New_List (IR);
                  else
                     Append (IR, Result);
                  end if;
               end if;

            elsif Ekind (Typ) = E_Anonymous_Access_Subprogram_Type
              and then Convention (Desig) /= Convention_Protected
            then
               Set_Is_Frozen (Desig);
            end if;
         end Check_Itype;

      --  Start of processing for Freeze_Record_Type

      begin
         --  If this is a subtype of a controlled type, declared without a
         --  constraint, the _controller may not appear in the component list
         --  if the parent was not frozen at the point of subtype declaration.
         --  Inherit the _controller component now.

         if Rec /= Base_Type (Rec)
           and then Has_Controlled_Component (Rec)
         then
            if Nkind (Parent (Rec)) = N_Subtype_Declaration
              and then Is_Entity_Name (Subtype_Indication (Parent (Rec)))
            then
               Set_First_Entity (Rec, First_Entity (Base_Type (Rec)));

            --  If this is an internal type without a declaration, as for
            --  record component, the base type may not yet be frozen, and its
            --  controller has not been created. Add an explicit freeze node
            --  for the itype, so it will be frozen after the base type. This
            --  freeze node is used to communicate with the expander, in order
            --  to create the controller for the enclosing record, and it is
            --  deleted afterwards (see exp_ch3). It must not be created when
            --  expansion is off, because it might appear in the wrong context
            --  for the back end.

            elsif Is_Itype (Rec)
              and then Has_Delayed_Freeze (Base_Type (Rec))
              and then
                Nkind (Associated_Node_For_Itype (Rec)) =
                                                     N_Component_Declaration
              and then Expander_Active
            then
               Ensure_Freeze_Node (Rec);
            end if;
         end if;

         --  Freeze components and embedded subtypes

         Comp := First_Entity (Rec);
         Prev := Empty;
         while Present (Comp) loop

            --  First handle the (real) component case

            if Ekind (Comp) = E_Component
              or else Ekind (Comp) = E_Discriminant
            then
               declare
                  CC : constant Node_Id := Component_Clause (Comp);

               begin
                  --  Freezing a record type freezes the type of each of its
                  --  components. However, if the type of the component is
                  --  part of this record, we do not want or need a separate
                  --  Freeze_Node. Note that Is_Itype is wrong because that's
                  --  also set in private type cases. We also can't check for
                  --  the Scope being exactly Rec because of private types and
                  --  record extensions.

                  if Is_Itype (Etype (Comp))
                    and then Is_Record_Type (Underlying_Type
                                             (Scope (Etype (Comp))))
                  then
                     Undelay_Type (Etype (Comp));
                  end if;

                  Freeze_And_Append (Etype (Comp), Loc, Result);

                  --  Check for error of component clause given for variable
                  --  sized type. We have to delay this test till this point,
                  --  since the component type has to be frozen for us to know
                  --  if it is variable length. We omit this test in a generic
                  --  context, it will be applied at instantiation time.

                  if Present (CC) then
                     Placed_Component := True;

                     if Inside_A_Generic then
                        null;

                     elsif not
                       Size_Known_At_Compile_Time
                         (Underlying_Type (Etype (Comp)))
                     then
                        Error_Msg_N
                          ("component clause not allowed for variable " &
                           "length component", CC);
                     end if;

                  else
                     Unplaced_Component := True;
                  end if;

                  --  Case of component requires byte alignment

                  if Must_Be_On_Byte_Boundary (Etype (Comp)) then

                     --  Set the enclosing record to also require byte align

                     Set_Must_Be_On_Byte_Boundary (Rec);

                     --  Check for component clause that is inconsistent with
                     --  the required byte boundary alignment.

                     if Present (CC)
                       and then Normalized_First_Bit (Comp) mod
                                  System_Storage_Unit /= 0
                     then
                        Error_Msg_N
                          ("component & must be byte aligned",
                           Component_Name (Component_Clause (Comp)));
                     end if;
                  end if;

                  --  If component clause is present, then deal with the non-
                  --  default bit order case for Ada 95 mode. The required
                  --  processing for Ada 2005 mode is handled separately after
                  --  processing all components.

                  --  We only do this processing for the base type, and in
                  --  fact that's important, since otherwise if there are
                  --  record subtypes, we could reverse the bits once for
                  --  each subtype, which would be incorrect.

                  if Present (CC)
                    and then Reverse_Bit_Order (Rec)
                    and then Ekind (E) = E_Record_Type
                    and then Ada_Version <= Ada_95
                  then
                     declare
                        CFB : constant Uint    := Component_Bit_Offset (Comp);
                        CSZ : constant Uint    := Esize (Comp);
                        CLC : constant Node_Id := Component_Clause (Comp);
                        Pos : constant Node_Id := Position (CLC);
                        FB  : constant Node_Id := First_Bit (CLC);

                        Storage_Unit_Offset : constant Uint :=
                                                CFB / System_Storage_Unit;

                        Start_Bit : constant Uint :=
                                      CFB mod System_Storage_Unit;

                     begin
                        --  Cases where field goes over storage unit boundary

                        if Start_Bit + CSZ > System_Storage_Unit then

                           --  Allow multi-byte field but generate warning

                           if Start_Bit mod System_Storage_Unit = 0
                             and then CSZ mod System_Storage_Unit = 0
                           then
                              Error_Msg_N
                                ("multi-byte field specified with non-standard"
                                 & " Bit_Order?", CLC);

                              if Bytes_Big_Endian then
                                 Error_Msg_N
                                   ("bytes are not reversed "
                                    & "(component is big-endian)?", CLC);
                              else
                                 Error_Msg_N
                                   ("bytes are not reversed "
                                    & "(component is little-endian)?", CLC);
                              end if;

                           --  Do not allow non-contiguous field

                           else
                              Error_Msg_N
                                ("attempt to specify non-contiguous field "
                                 & "not permitted", CLC);
                              Error_Msg_N
                                ("\caused by non-standard Bit_Order "
                                 & "specified", CLC);
                              Error_Msg_N
                                ("\consider possibility of using "
                                 & "Ada 2005 mode here", CLC);
                           end if;

                        --  Case where field fits in one storage unit

                        else
                           --  Give warning if suspicious component clause

                           if Intval (FB) >= System_Storage_Unit
                             and then Warn_On_Reverse_Bit_Order
                           then
                              Error_Msg_N
                                ("?Bit_Order clause does not affect " &
                                 "byte ordering", Pos);
                              Error_Msg_Uint_1 :=
                                Intval (Pos) + Intval (FB) /
                                  System_Storage_Unit;
                              Error_Msg_N
                                ("?position normalized to ^ before bit " &
                                 "order interpreted", Pos);
                           end if;

                           --  Here is where we fix up the Component_Bit_Offset
                           --  value to account for the reverse bit order.
                           --  Some examples of what needs to be done are:

                           --    First_Bit .. Last_Bit     Component_Bit_Offset
                           --      old          new          old       new

                           --     0 .. 0       7 .. 7         0         7
                           --     0 .. 1       6 .. 7         0         6
                           --     0 .. 2       5 .. 7         0         5
                           --     0 .. 7       0 .. 7         0         4

                           --     1 .. 1       6 .. 6         1         6
                           --     1 .. 4       3 .. 6         1         3
                           --     4 .. 7       0 .. 3         4         0

                           --  The general rule is that the first bit is
                           --  is obtained by subtracting the old ending bit
                           --  from storage_unit - 1.

                           Set_Component_Bit_Offset
                             (Comp,
                              (Storage_Unit_Offset * System_Storage_Unit) +
                                (System_Storage_Unit - 1) -
                                  (Start_Bit + CSZ - 1));

                           Set_Normalized_First_Bit
                             (Comp,
                                Component_Bit_Offset (Comp) mod
                                  System_Storage_Unit);
                        end if;
                     end;
                  end if;
               end;
            end if;

            --  Gather data for possible Implicit_Packing later

            if not Is_Scalar_Type (Etype (Comp)) then
               All_Scalar_Components := False;
            else
               Scalar_Component_Total_RM_Size :=
                 Scalar_Component_Total_RM_Size + RM_Size (Etype (Comp));
               Scalar_Component_Total_Esize :=
                 Scalar_Component_Total_Esize + Esize (Etype (Comp));
            end if;

            --  If the component is an Itype with Delayed_Freeze and is either
            --  a record or array subtype and its base type has not yet been
            --  frozen, we must remove this from the entity list of this
            --  record and put it on the entity list of the scope of its base
            --  type. Note that we know that this is not the type of a
            --  component since we cleared Has_Delayed_Freeze for it in the
            --  previous loop. Thus this must be the Designated_Type of an
            --  access type, which is the type of a component.

            if Is_Itype (Comp)
              and then Is_Type (Scope (Comp))
              and then Is_Composite_Type (Comp)
              and then Base_Type (Comp) /= Comp
              and then Has_Delayed_Freeze (Comp)
              and then not Is_Frozen (Base_Type (Comp))
            then
               declare
                  Will_Be_Frozen : Boolean := False;
                  S              : Entity_Id;

               begin
                  --  We have a pretty bad kludge here. Suppose Rec is subtype
                  --  being defined in a subprogram that's created as part of
                  --  the freezing of Rec'Base. In that case, we know that
                  --  Comp'Base must have already been frozen by the time we
                  --  get to elaborate this because Gigi doesn't elaborate any
                  --  bodies until it has elaborated all of the declarative
                  --  part. But Is_Frozen will not be set at this point because
                  --  we are processing code in lexical order.

                  --  We detect this case by going up the Scope chain of Rec
                  --  and seeing if we have a subprogram scope before reaching
                  --  the top of the scope chain or that of Comp'Base. If we
                  --  do, then mark that Comp'Base will actually be frozen. If
                  --  so, we merely undelay it.

                  S := Scope (Rec);
                  while Present (S) loop
                     if Is_Subprogram (S) then
                        Will_Be_Frozen := True;
                        exit;
                     elsif S = Scope (Base_Type (Comp)) then
                        exit;
                     end if;

                     S := Scope (S);
                  end loop;

                  if Will_Be_Frozen then
                     Undelay_Type (Comp);
                  else
                     if Present (Prev) then
                        Set_Next_Entity (Prev, Next_Entity (Comp));
                     else
                        Set_First_Entity (Rec, Next_Entity (Comp));
                     end if;

                     --  Insert in entity list of scope of base type (which
                     --  must be an enclosing scope, because still unfrozen).

                     Append_Entity (Comp, Scope (Base_Type (Comp)));
                  end if;
               end;

            --  If the component is an access type with an allocator as default
            --  value, the designated type will be frozen by the corresponding
            --  expression in init_proc. In order to place the freeze node for
            --  the designated type before that for the current record type,
            --  freeze it now.

            --  Same process if the component is an array of access types,
            --  initialized with an aggregate. If the designated type is
            --  private, it cannot contain allocators, and it is premature
            --  to freeze the type, so we check for this as well.

            elsif Is_Access_Type (Etype (Comp))
              and then Present (Parent (Comp))
              and then Present (Expression (Parent (Comp)))
            then
               declare
                  Alloc : constant Node_Id :=
                            Check_Allocator (Expression (Parent (Comp)));

               begin
                  if Present (Alloc) then

                     --  If component is pointer to a classwide type, freeze
                     --  the specific type in the expression being allocated.
                     --  The expression may be a subtype indication, in which
                     --  case freeze the subtype mark.

                     if Is_Class_Wide_Type
                          (Designated_Type (Etype (Comp)))
                     then
                        if Is_Entity_Name (Expression (Alloc)) then
                           Freeze_And_Append
                             (Entity (Expression (Alloc)), Loc, Result);
                        elsif
                          Nkind (Expression (Alloc)) = N_Subtype_Indication
                        then
                           Freeze_And_Append
                            (Entity (Subtype_Mark (Expression (Alloc))),
                              Loc, Result);
                        end if;

                     elsif Is_Itype (Designated_Type (Etype (Comp))) then
                        Check_Itype (Etype (Comp));

                     else
                        Freeze_And_Append
                          (Designated_Type (Etype (Comp)), Loc, Result);
                     end if;
                  end if;
               end;

            elsif Is_Access_Type (Etype (Comp))
              and then Is_Itype (Designated_Type (Etype (Comp)))
            then
               Check_Itype (Etype (Comp));

            elsif Is_Array_Type (Etype (Comp))
              and then Is_Access_Type (Component_Type (Etype (Comp)))
              and then Present (Parent (Comp))
              and then Nkind (Parent (Comp)) = N_Component_Declaration
              and then Present (Expression (Parent (Comp)))
              and then Nkind (Expression (Parent (Comp))) = N_Aggregate
              and then Is_Fully_Defined
                 (Designated_Type (Component_Type (Etype (Comp))))
            then
               Freeze_And_Append
                 (Designated_Type
                   (Component_Type (Etype (Comp))), Loc, Result);
            end if;

            Prev := Comp;
            Next_Entity (Comp);
         end loop;

         --  Deal with pragma Bit_Order

         if Reverse_Bit_Order (Rec) and then Base_Type (Rec) = Rec then
            if not Placed_Component then
               ADC :=
                 Get_Attribute_Definition_Clause (Rec, Attribute_Bit_Order);
               Error_Msg_N
                 ("?Bit_Order specification has no effect", ADC);
               Error_Msg_N
                 ("\?since no component clauses were specified", ADC);

            --  Here is where we do Ada 2005 processing for bit order (the Ada
            --  95 case was already taken care of above).

            elsif Ada_Version >= Ada_05 then
               Adjust_Record_For_Reverse_Bit_Order (Rec);
            end if;
         end if;

         --  Set OK_To_Reorder_Components depending on debug flags

         if Rec = Base_Type (Rec)
           and then Convention (Rec) = Convention_Ada
         then
            if (Has_Discriminants (Rec) and then Debug_Flag_Dot_V)
                  or else
               (not Has_Discriminants (Rec) and then Debug_Flag_Dot_R)
            then
               Set_OK_To_Reorder_Components (Rec);
            end if;
         end if;

         --  Check for useless pragma Pack when all components placed. We only
         --  do this check for record types, not subtypes, since a subtype may
         --  have all its components placed, and it still makes perfectly good
         --  sense to pack other subtypes or the parent type. We do not give
         --  this warning if Optimize_Alignment is set to Space, since the
         --  pragma Pack does have an effect in this case (it always resets
         --  the alignment to one).

         if Ekind (Rec) = E_Record_Type
           and then Is_Packed (Rec)
           and then not Unplaced_Component
           and then Optimize_Alignment /= 'S'
         then
            --  Reset packed status. Probably not necessary, but we do it so
            --  that there is no chance of the back end doing something strange
            --  with this redundant indication of packing.

            Set_Is_Packed (Rec, False);

            --  Give warning if redundant constructs warnings on

            if Warn_On_Redundant_Constructs then
               Error_Msg_N
                 ("?pragma Pack has no effect, no unplaced components",
                  Get_Rep_Pragma (Rec, Name_Pack));
            end if;
         end if;

         --  If this is the record corresponding to a remote type, freeze the
         --  remote type here since that is what we are semantically freezing.
         --  This prevents the freeze node for that type in an inner scope.

         --  Also, Check for controlled components and unchecked unions.
         --  Finally, enforce the restriction that access attributes with a
         --  current instance prefix can only apply to limited types.

         if Ekind (Rec) = E_Record_Type then
            if Present (Corresponding_Remote_Type (Rec)) then
               Freeze_And_Append
                 (Corresponding_Remote_Type (Rec), Loc, Result);
            end if;

            Comp := First_Component (Rec);
            while Present (Comp) loop

               --  Do not set Has_Controlled_Component on a class-wide
               --  equivalent type. See Make_CW_Equivalent_Type.

               if not Is_Class_Wide_Equivalent_Type (Rec)
                 and then (Has_Controlled_Component (Etype (Comp))
                            or else (Chars (Comp) /= Name_uParent
                                      and then Is_Controlled (Etype (Comp)))
                            or else (Is_Protected_Type (Etype (Comp))
                                      and then Present
                                        (Corresponding_Record_Type
                                          (Etype (Comp)))
                                      and then Has_Controlled_Component
                                        (Corresponding_Record_Type
                                          (Etype (Comp)))))
               then
                  Set_Has_Controlled_Component (Rec);
                  exit;
               end if;

               if Has_Unchecked_Union (Etype (Comp)) then
                  Set_Has_Unchecked_Union (Rec);
               end if;

               if Has_Per_Object_Constraint (Comp) then

                  --  Scan component declaration for likely misuses of current
                  --  instance, either in a constraint or a default expression.

                  Check_Current_Instance (Parent (Comp));
               end if;

               Next_Component (Comp);
            end loop;
         end if;

         Set_Component_Alignment_If_Not_Set (Rec);

         --  For first subtypes, check if there are any fixed-point fields with
         --  component clauses, where we must check the size. This is not done
         --  till the freeze point, since for fixed-point types, we do not know
         --  the size until the type is frozen. Similar processing applies to
         --  bit packed arrays.

         if Is_First_Subtype (Rec) then
            Comp := First_Component (Rec);

            while Present (Comp) loop
               if Present (Component_Clause (Comp))
                 and then (Is_Fixed_Point_Type (Etype (Comp))
                             or else
                           Is_Bit_Packed_Array (Etype (Comp)))
               then
                  Check_Size
                    (Component_Name (Component_Clause (Comp)),
                     Etype (Comp),
                     Esize (Comp),
                     Junk);
               end if;

               Next_Component (Comp);
            end loop;
         end if;

         --  Generate warning for applying C or C++ convention to a record
         --  with discriminants. This is suppressed for the unchecked union
         --  case, since the whole point in this case is interface C. We also
         --  do not generate this within instantiations, since we will have
         --  generated a message on the template.

         if Has_Discriminants (E)
           and then not Is_Unchecked_Union (E)
           and then (Convention (E) = Convention_C
                       or else
                     Convention (E) = Convention_CPP)
           and then Comes_From_Source (E)
           and then not In_Instance
           and then not Has_Warnings_Off (E)
           and then not Has_Warnings_Off (Base_Type (E))
         then
            declare
               Cprag : constant Node_Id := Get_Rep_Pragma (E, Name_Convention);
               A2    : Node_Id;

            begin
               if Present (Cprag) then
                  A2 := Next (First (Pragma_Argument_Associations (Cprag)));

                  if Convention (E) = Convention_C then
                     Error_Msg_N
                       ("?variant record has no direct equivalent in C", A2);
                  else
                     Error_Msg_N
                       ("?variant record has no direct equivalent in C++", A2);
                  end if;

                  Error_Msg_NE
                    ("\?use of convention for type& is dubious", A2, E);
               end if;
            end;
         end if;

         --  See if Size is too small as is (and implicit packing might help)

         if not Is_Packed (Rec)

           --  No implicit packing if even one component is explicitly placed

           and then not Placed_Component

           --  Must have size clause and all scalar components

           and then Has_Size_Clause (Rec)
           and then All_Scalar_Components

           --  Do not try implicit packing on records with discriminants, too
           --  complicated, especially in the variant record case.

           and then not Has_Discriminants (Rec)

           --  We can implicitly pack if the specified size of the record is
           --  less than the sum of the object sizes (no point in packing if
           --  this is not the case).

           and then Esize (Rec) < Scalar_Component_Total_Esize

           --  And the total RM size cannot be greater than the specified size
           --  since otherwise packing will not get us where we have to be!

           and then Esize (Rec) >= Scalar_Component_Total_RM_Size

           --  Never do implicit packing in CodePeer mode since we don't do
           --  any packing ever in this mode (why not???)

           and then not CodePeer_Mode
         then
            --  If implicit packing enabled, do it

            if Implicit_Packing then
               Set_Is_Packed (Rec);

               --  Otherwise flag the size clause

            else
               declare
                  Sz : constant Node_Id := Size_Clause (Rec);
               begin
                  Error_Msg_NE --  CODEFIX
                    ("size given for& too small", Sz, Rec);
                  Error_Msg_N --  CODEFIX
                    ("\use explicit pragma Pack "
                     & "or use pragma Implicit_Packing", Sz);
               end;
            end if;
         end if;
      end Freeze_Record_Type;

   --  Start of processing for Freeze_Entity

   begin
      --  We are going to test for various reasons why this entity need not be
      --  frozen here, but in the case of an Itype that's defined within a
      --  record, that test actually applies to the record.

      if Is_Itype (E) and then Is_Record_Type (Scope (E)) then
         Test_E := Scope (E);
      elsif Is_Itype (E) and then Present (Underlying_Type (Scope (E)))
        and then Is_Record_Type (Underlying_Type (Scope (E)))
      then
         Test_E := Underlying_Type (Scope (E));
      end if;

      --  Do not freeze if already frozen since we only need one freeze node

      if Is_Frozen (E) then
         return No_List;

      --  It is improper to freeze an external entity within a generic because
      --  its freeze node will appear in a non-valid context. The entity will
      --  be frozen in the proper scope after the current generic is analyzed.

      elsif Inside_A_Generic and then External_Ref_In_Generic (Test_E) then
         return No_List;

      --  Do not freeze a global entity within an inner scope created during
      --  expansion. A call to subprogram E within some internal procedure
      --  (a stream attribute for example) might require freezing E, but the
      --  freeze node must appear in the same declarative part as E itself.
      --  The two-pass elaboration mechanism in gigi guarantees that E will
      --  be frozen before the inner call is elaborated. We exclude constants
      --  from this test, because deferred constants may be frozen early, and
      --  must be diagnosed (e.g. in the case of a deferred constant being used
      --  in a default expression). If the enclosing subprogram comes from
      --  source, or is a generic instance, then the freeze point is the one
      --  mandated by the language, and we freeze the entity. A subprogram that
      --  is a child unit body that acts as a spec does not have a spec that
      --  comes from source, but can only come from source.

      elsif In_Open_Scopes (Scope (Test_E))
        and then Scope (Test_E) /= Current_Scope
        and then Ekind (Test_E) /= E_Constant
      then
         declare
            S : Entity_Id := Current_Scope;

         begin
            while Present (S) loop
               if Is_Overloadable (S) then
                  if Comes_From_Source (S)
                    or else Is_Generic_Instance (S)
                    or else Is_Child_Unit (S)
                  then
                     exit;
                  else
                     return No_List;
                  end if;
               end if;

               S := Scope (S);
            end loop;
         end;

      --  Similarly, an inlined instance body may make reference to global
      --  entities, but these references cannot be the proper freezing point
      --  for them, and in the absence of inlining freezing will take place in
      --  their own scope. Normally instance bodies are analyzed after the
      --  enclosing compilation, and everything has been frozen at the proper
      --  place, but with front-end inlining an instance body is compiled
      --  before the end of the enclosing scope, and as a result out-of-order
      --  freezing must be prevented.

      elsif Front_End_Inlining
        and then In_Instance_Body
        and then Present (Scope (Test_E))
      then
         declare
            S : Entity_Id := Scope (Test_E);

         begin
            while Present (S) loop
               if Is_Generic_Instance (S) then
                  exit;
               else
                  S := Scope (S);
               end if;
            end loop;

            if No (S) then
               return No_List;
            end if;
         end;
      end if;

      --  Here to freeze the entity

      Result := No_List;
      Set_Is_Frozen (E);

      --  Case of entity being frozen is other than a type

      if not Is_Type (E) then

         --  If entity is exported or imported and does not have an external
         --  name, now is the time to provide the appropriate default name.
         --  Skip this if the entity is stubbed, since we don't need a name
         --  for any stubbed routine. For the case on intrinsics, if no
         --  external name is specified, then calls will be handled in
         --  Exp_Intr.Expand_Intrinsic_Call, and no name is needed; if
         --  an external name is provided, then Expand_Intrinsic_Call leaves
         --  calls in place for expansion by GIGI.

         if (Is_Imported (E) or else Is_Exported (E))
           and then No (Interface_Name (E))
           and then Convention (E) /= Convention_Stubbed
           and then Convention (E) /= Convention_Intrinsic
         then
            Set_Encoded_Interface_Name
              (E, Get_Default_External_Name (E));

         --  If entity is an atomic object appearing in a declaration and
         --  the expression is an aggregate, assign it to a temporary to
         --  ensure that the actual assignment is done atomically rather
         --  than component-wise (the assignment to the temp may be done
         --  component-wise, but that is harmless).

         elsif Is_Atomic (E)
           and then Nkind (Parent (E)) = N_Object_Declaration
           and then Present (Expression (Parent (E)))
           and then Nkind (Expression (Parent (E))) = N_Aggregate
           and then
             Is_Atomic_Aggregate (Expression (Parent (E)), Etype (E))
         then
            null;
         end if;

         --  For a subprogram, freeze all parameter types and also the return
         --  type (RM 13.14(14)). However skip this for internal subprograms.
         --  This is also the point where any extra formal parameters are
         --  created since we now know whether the subprogram will use a
         --  foreign convention.

         if Is_Subprogram (E) then
            if not Is_Internal (E) then
               declare
                  F_Type    : Entity_Id;
                  R_Type    : Entity_Id;
                  Warn_Node : Node_Id;

               begin
                  --  Loop through formals

                  Formal := First_Formal (E);
                  while Present (Formal) loop
                     F_Type := Etype (Formal);
                     Freeze_And_Append (F_Type, Loc, Result);

                     if Is_Private_Type (F_Type)
                       and then Is_Private_Type (Base_Type (F_Type))
                       and then No (Full_View (Base_Type (F_Type)))
                       and then not Is_Generic_Type (F_Type)
                       and then not Is_Derived_Type (F_Type)
                     then
                        --  If the type of a formal is incomplete, subprogram
                        --  is being frozen prematurely. Within an instance
                        --  (but not within a wrapper package) this is an
                        --  artifact of our need to regard the end of an
                        --  instantiation as a freeze point. Otherwise it is
                        --  a definite error.

                        if In_Instance then
                           Set_Is_Frozen (E, False);
                           return No_List;

                        elsif not After_Last_Declaration
                          and then not Freezing_Library_Level_Tagged_Type
                        then
                           Error_Msg_Node_1 := F_Type;
                           Error_Msg
                             ("type& must be fully defined before this point",
                               Loc);
                        end if;
                     end if;

                     --  Check suspicious parameter for C function. These tests
                     --  apply only to exported/imported subprograms.

                     if Warn_On_Export_Import
                       and then Comes_From_Source (E)
                       and then (Convention (E) = Convention_C
                                   or else
                                 Convention (E) = Convention_CPP)
                       and then (Is_Imported (E) or else Is_Exported (E))
                       and then Convention (E) /= Convention (Formal)
                       and then not Has_Warnings_Off (E)
                       and then not Has_Warnings_Off (F_Type)
                       and then not Has_Warnings_Off (Formal)
                     then
                        --  Qualify mention of formals with subprogram name

                        Error_Msg_Qual_Level := 1;

                        --  Check suspicious use of fat C pointer

                        if Is_Access_Type (F_Type)
                          and then Esize (F_Type) > Ttypes.System_Address_Size
                        then
                           Error_Msg_N
                             ("?type of & does not correspond to C pointer!",
                              Formal);

                        --  Check suspicious return of boolean

                        elsif Root_Type (F_Type) = Standard_Boolean
                          and then Convention (F_Type) = Convention_Ada
                          and then not Has_Warnings_Off (F_Type)
                          and then not Has_Size_Clause (F_Type)
                          and then VM_Target = No_VM
                        then
                           Error_Msg_N
                             ("& is an 8-bit Ada Boolean?", Formal);
                           Error_Msg_N
                             ("\use appropriate corresponding type in C "
                              & "(e.g. char)?", Formal);

                        --  Check suspicious tagged type

                        elsif (Is_Tagged_Type (F_Type)
                                or else (Is_Access_Type (F_Type)
                                           and then
                                             Is_Tagged_Type
                                               (Designated_Type (F_Type))))
                          and then Convention (E) = Convention_C
                        then
                           Error_Msg_N
                             ("?& involves a tagged type which does not "
                              & "correspond to any C type!", Formal);

                        --  Check wrong convention subprogram pointer

                        elsif Ekind (F_Type) = E_Access_Subprogram_Type
                          and then not Has_Foreign_Convention (F_Type)
                        then
                           Error_Msg_N
                             ("?subprogram pointer & should "
                              & "have foreign convention!", Formal);
                           Error_Msg_Sloc := Sloc (F_Type);
                           Error_Msg_NE
                             ("\?add Convention pragma to declaration of &#",
                              Formal, F_Type);
                        end if;

                        --  Turn off name qualification after message output

                        Error_Msg_Qual_Level := 0;
                     end if;

                     --  Check for unconstrained array in exported foreign
                     --  convention case.

                     if Has_Foreign_Convention (E)
                       and then not Is_Imported (E)
                       and then Is_Array_Type (F_Type)
                       and then not Is_Constrained (F_Type)
                       and then Warn_On_Export_Import

                       --  Exclude VM case, since both .NET and JVM can handle
                       --  unconstrained arrays without a problem.

                       and then VM_Target = No_VM
                     then
                        Error_Msg_Qual_Level := 1;

                        --  If this is an inherited operation, place the
                        --  warning on the derived type declaration, rather
                        --  than on the original subprogram.

                        if Nkind (Original_Node (Parent (E))) =
                          N_Full_Type_Declaration
                        then
                           Warn_Node := Parent (E);

                           if Formal = First_Formal (E) then
                              Error_Msg_NE
                                ("?in inherited operation&", Warn_Node, E);
                           end if;
                        else
                           Warn_Node := Formal;
                        end if;

                        Error_Msg_NE
                          ("?type of argument& is unconstrained array",
                           Warn_Node, Formal);
                        Error_Msg_NE
                          ("?foreign caller must pass bounds explicitly",
                           Warn_Node, Formal);
                        Error_Msg_Qual_Level := 0;
                     end if;

                     if not From_With_Type (F_Type) then
                        if Is_Access_Type (F_Type) then
                           F_Type := Designated_Type (F_Type);
                        end if;

                        --  If the formal is an anonymous_access_to_subprogram
                        --  freeze the  subprogram type as well, to prevent
                        --  scope anomalies in gigi, because there is no other
                        --  clear point at which it could be frozen.

                        if Is_Itype (Etype (Formal))
                          and then Ekind (F_Type) = E_Subprogram_Type
                        then
                           Freeze_And_Append (F_Type, Loc, Result);
                        end if;
                     end if;

                     Next_Formal (Formal);
                  end loop;

                  --  Case of function: similar checks on return type

                  if Ekind (E) = E_Function then

                     --  Freeze return type

                     R_Type := Etype (E);
                     Freeze_And_Append (R_Type, Loc, Result);

                     --  Check suspicious return type for C function

                     if Warn_On_Export_Import
                       and then (Convention (E) = Convention_C
                                   or else
                                 Convention (E) = Convention_CPP)
                       and then (Is_Imported (E) or else Is_Exported (E))
                     then
                        --  Check suspicious return of fat C pointer

                        if Is_Access_Type (R_Type)
                          and then Esize (R_Type) > Ttypes.System_Address_Size
                          and then not Has_Warnings_Off (E)
                          and then not Has_Warnings_Off (R_Type)
                        then
                           Error_Msg_N
                             ("?return type of& does not "
                              & "correspond to C pointer!", E);

                        --  Check suspicious return of boolean

                        elsif Root_Type (R_Type) = Standard_Boolean
                          and then Convention (R_Type) = Convention_Ada
                          and then VM_Target = No_VM
                          and then not Has_Warnings_Off (E)
                          and then not Has_Warnings_Off (R_Type)
                          and then not Has_Size_Clause (R_Type)
                        then
                           declare
                              N : constant Node_Id :=
                                    Result_Definition (Declaration_Node (E));
                           begin
                              Error_Msg_NE
                                ("return type of & is an 8-bit Ada Boolean?",
                                 N, E);
                              Error_Msg_NE
                                ("\use appropriate corresponding type in C "
                                 & "(e.g. char)?", N, E);
                           end;

                        --  Check suspicious return tagged type

                        elsif (Is_Tagged_Type (R_Type)
                                or else (Is_Access_Type (R_Type)
                                           and then
                                             Is_Tagged_Type
                                               (Designated_Type (R_Type))))
                          and then Convention (E) = Convention_C
                          and then not Has_Warnings_Off (E)
                          and then not Has_Warnings_Off (R_Type)
                        then
                           Error_Msg_N
                             ("?return type of & does not "
                              & "correspond to C type!", E);

                        --  Check return of wrong convention subprogram pointer

                        elsif Ekind (R_Type) = E_Access_Subprogram_Type
                          and then not Has_Foreign_Convention (R_Type)
                          and then not Has_Warnings_Off (E)
                          and then not Has_Warnings_Off (R_Type)
                        then
                           Error_Msg_N
                             ("?& should return a foreign "
                              & "convention subprogram pointer", E);
                           Error_Msg_Sloc := Sloc (R_Type);
                           Error_Msg_NE
                             ("\?add Convention pragma to declaration of& #",
                              E, R_Type);
                        end if;
                     end if;

                     --  Give warning for suspicous return of a result of an
                     --  unconstrained array type in a foreign convention
                     --  function.

                     if Has_Foreign_Convention (E)

                       --  We are looking for a return of unconstrained array

                       and then Is_Array_Type (R_Type)
                       and then not Is_Constrained (R_Type)

                       --  Exclude imported routines, the warning does not
                       --  belong on the import, but on the routine definition.

                       and then not Is_Imported (E)

                       --  Exclude VM case, since both .NET and JVM can handle
                       --  return of unconstrained arrays without a problem.

                       and then VM_Target = No_VM

                       --  Check that general warning is enabled, and that it
                       --  is not suppressed for this particular case.

                       and then Warn_On_Export_Import
                       and then not Has_Warnings_Off (E)
                       and then not Has_Warnings_Off (R_Type)
                     then
                        Error_Msg_N
                          ("?foreign convention function& should not " &
                           "return unconstrained array!", E);
                     end if;
                  end if;
               end;
            end if;

            --  Must freeze its parent first if it is a derived subprogram

            if Present (Alias (E)) then
               Freeze_And_Append (Alias (E), Loc, Result);
            end if;

            --  We don't freeze internal subprograms, because we don't normally
            --  want addition of extra formals or mechanism setting to happen
            --  for those. However we do pass through predefined dispatching
            --  cases, since extra formals may be needed in some cases, such as
            --  for the stream 'Input function (build-in-place formals).

            if not Is_Internal (E)
              or else Is_Predefined_Dispatching_Operation (E)
            then
               Freeze_Subprogram (E);
            end if;

         --  Here for other than a subprogram or type

         else
            --  If entity has a type, and it is not a generic unit, then
            --  freeze it first (RM 13.14(10)).

            if Present (Etype (E))
              and then Ekind (E) /= E_Generic_Function
            then
               Freeze_And_Append (Etype (E), Loc, Result);
            end if;

            --  Special processing for objects created by object declaration

            if Nkind (Declaration_Node (E)) = N_Object_Declaration then

               --  Abstract type allowed only for C++ imported variables or
               --  constants.

               --  Note: we inhibit this check for objects that do not come
               --  from source because there is at least one case (the
               --  expansion of x'class'input where x is abstract) where we
               --  legitimately generate an abstract object.

               if Is_Abstract_Type (Etype (E))
                 and then Comes_From_Source (Parent (E))
                 and then not (Is_Imported (E)
                                 and then Is_CPP_Class (Etype (E)))
               then
                  Error_Msg_N ("type of object cannot be abstract",
                               Object_Definition (Parent (E)));

                  if Is_CPP_Class (Etype (E)) then
                     Error_Msg_NE ("\} may need a cpp_constructor",
                       Object_Definition (Parent (E)), Etype (E));
                  end if;
               end if;

               --  For object created by object declaration, perform required
               --  categorization (preelaborate and pure) checks. Defer these
               --  checks to freeze time since pragma Import inhibits default
               --  initialization and thus pragma Import affects these checks.

               Validate_Object_Declaration (Declaration_Node (E));

               --  If there is an address clause, check that it is valid

               Check_Address_Clause (E);

               --  If the object needs any kind of default initialization, an
               --  error must be issued if No_Default_Initialization applies.
               --  The check doesn't apply to imported objects, which are not
               --  ever default initialized, and is why the check is deferred
               --  until freezing, at which point we know if Import applies.
               --  Deferred constants are also exempted from this test because
               --  their completion is explicit, or through an import pragma.

               if Ekind (E) = E_Constant
                 and then Present (Full_View (E))
               then
                  null;

               elsif Comes_From_Source (E)
                 and then not Is_Imported (E)
                 and then not Has_Init_Expression (Declaration_Node (E))
                 and then
                   ((Has_Non_Null_Base_Init_Proc (Etype (E))
                      and then not No_Initialization (Declaration_Node (E))
                      and then not Is_Value_Type (Etype (E))
                      and then not Suppress_Init_Proc (Etype (E)))
                    or else
                      (Needs_Simple_Initialization (Etype (E))
                        and then not Is_Internal (E)))
               then
                  Has_Default_Initialization := True;
                  Check_Restriction
                    (No_Default_Initialization, Declaration_Node (E));
               end if;

               --  Check that a Thread_Local_Storage variable does not have
               --  default initialization, and any explicit initialization must
               --  either be the null constant or a static constant.

               if Has_Pragma_Thread_Local_Storage (E) then
                  declare
                     Decl : constant Node_Id := Declaration_Node (E);
                  begin
                     if Has_Default_Initialization
                       or else
                         (Has_Init_Expression (Decl)
                            and then
                             (No (Expression (Decl))
                                or else not
                                  (Is_Static_Expression (Expression (Decl))
                                     or else
                                   Nkind (Expression (Decl)) = N_Null)))
                     then
                        Error_Msg_NE
                          ("Thread_Local_Storage variable& is "
                           & "improperly initialized", Decl, E);
                        Error_Msg_NE
                          ("\only allowed initialization is explicit "
                           & "NULL or static expression", Decl, E);
                     end if;
                  end;
               end if;

               --  For imported objects, set Is_Public unless there is also an
               --  address clause, which means that there is no external symbol
               --  needed for the Import (Is_Public may still be set for other
               --  unrelated reasons). Note that we delayed this processing
               --  till freeze time so that we can be sure not to set the flag
               --  if there is an address clause. If there is such a clause,
               --  then the only purpose of the Import pragma is to suppress
               --  implicit initialization.

               if Is_Imported (E)
                 and then No (Address_Clause (E))
               then
                  Set_Is_Public (E);
               end if;

               --  For convention C objects of an enumeration type, warn if
               --  the size is not integer size and no explicit size given.
               --  Skip warning for Boolean, and Character, assume programmer
               --  expects 8-bit sizes for these cases.

               if (Convention (E) = Convention_C
                    or else
                   Convention (E) = Convention_CPP)
                 and then Is_Enumeration_Type (Etype (E))
                 and then not Is_Character_Type (Etype (E))
                 and then not Is_Boolean_Type (Etype (E))
                 and then Esize (Etype (E)) < Standard_Integer_Size
                 and then not Has_Size_Clause (E)
               then
                  Error_Msg_Uint_1 := UI_From_Int (Standard_Integer_Size);
                  Error_Msg_N
                    ("?convention C enumeration object has size less than ^",
                     E);
                  Error_Msg_N ("\?use explicit size clause to set size", E);
               end if;
            end if;

            --  Check that a constant which has a pragma Volatile[_Components]
            --  or Atomic[_Components] also has a pragma Import (RM C.6(13)).

            --  Note: Atomic[_Components] also sets Volatile[_Components]

            if Ekind (E) = E_Constant
              and then (Has_Volatile_Components (E) or else Is_Volatile (E))
              and then not Is_Imported (E)
            then
               --  Make sure we actually have a pragma, and have not merely
               --  inherited the indication from elsewhere (e.g. an address
               --  clause, which is not good enough in RM terms!)

               if Has_Rep_Pragma (E, Name_Atomic)
                    or else
                  Has_Rep_Pragma (E, Name_Atomic_Components)
               then
                  Error_Msg_N
                    ("stand alone atomic constant must be " &
                     "imported (RM C.6(13))", E);

               elsif Has_Rep_Pragma (E, Name_Volatile)
                       or else
                     Has_Rep_Pragma (E, Name_Volatile_Components)
               then
                  Error_Msg_N
                    ("stand alone volatile constant must be " &
                     "imported (RM C.6(13))", E);
               end if;
            end if;

            --  Static objects require special handling

            if (Ekind (E) = E_Constant or else Ekind (E) = E_Variable)
              and then Is_Statically_Allocated (E)
            then
               Freeze_Static_Object (E);
            end if;

            --  Remaining step is to layout objects

            if Ekind (E) = E_Variable
                 or else
               Ekind (E) = E_Constant
                 or else
               Ekind (E) = E_Loop_Parameter
                 or else
               Is_Formal (E)
            then
               Layout_Object (E);
            end if;
         end if;

      --  Case of a type or subtype being frozen

      else
         --  We used to check here that a full type must have preelaborable
         --  initialization if it completes a private type specified with
         --  pragma Preelaborable_Intialization, but that missed cases where
         --  the types occur within a generic package, since the freezing
         --  that occurs within a containing scope generally skips traversal
         --  of a generic unit's declarations (those will be frozen within
         --  instances). This check was moved to Analyze_Package_Specification.

         --  The type may be defined in a generic unit. This can occur when
         --  freezing a generic function that returns the type (which is
         --  defined in a parent unit). It is clearly meaningless to freeze
         --  this type. However, if it is a subtype, its size may be determi-
         --  nable and used in subsequent checks, so might as well try to
         --  compute it.

         if Present (Scope (E))
           and then Is_Generic_Unit (Scope (E))
         then
            Check_Compile_Time_Size (E);
            return No_List;
         end if;

         --  Deal with special cases of freezing for subtype

         if E /= Base_Type (E) then

            --  Before we do anything else, a specialized test for the case of
            --  a size given for an array where the array needs to be packed,
            --  but was not so the size cannot be honored. This would of course
            --  be caught by the backend, and indeed we don't catch all cases.
            --  The point is that we can give a better error message in those
            --  cases that we do catch with the circuitry here. Also if pragma
            --  Implicit_Packing is set, this is where the packing occurs.

            --  The reason we do this so early is that the processing in the
            --  automatic packing case affects the layout of the base type, so
            --  it must be done before we freeze the base type.

            if Is_Array_Type (E) then
               declare
                  Lo, Hi : Node_Id;
                  Ctyp   : constant Entity_Id := Component_Type (E);

               begin
                  --  Check enabling conditions. These are straightforward
                  --  except for the test for a limited composite type. This
                  --  eliminates the rare case of a array of limited components
                  --  where there are issues of whether or not we can go ahead
                  --  and pack the array (since we can't freely pack and unpack
                  --  arrays if they are limited).

                  --  Note that we check the root type explicitly because the
                  --  whole point is we are doing this test before we have had
                  --  a chance to freeze the base type (and it is that freeze
                  --  action that causes stuff to be inherited).

                  if Present (Size_Clause (E))
                    and then Known_Static_Esize (E)
                    and then not Is_Packed (E)
                    and then not Has_Pragma_Pack (E)
                    and then Number_Dimensions (E) = 1
                    and then not Has_Component_Size_Clause (E)
                    and then Known_Static_Esize (Ctyp)
                    and then not Is_Limited_Composite (E)
                    and then not Is_Packed (Root_Type (E))
                    and then not Has_Component_Size_Clause (Root_Type (E))
                    and then not CodePeer_Mode
                  then
                     Get_Index_Bounds (First_Index (E), Lo, Hi);

                     if Compile_Time_Known_Value (Lo)
                       and then Compile_Time_Known_Value (Hi)
                       and then Known_Static_RM_Size (Ctyp)
                       and then RM_Size (Ctyp) < 64
                     then
                        declare
                           Lov  : constant Uint      := Expr_Value (Lo);
                           Hiv  : constant Uint      := Expr_Value (Hi);
                           Len  : constant Uint      := UI_Max
                                                         (Uint_0,
                                                          Hiv - Lov + 1);
                           Rsiz : constant Uint      := RM_Size (Ctyp);
                           SZ   : constant Node_Id   := Size_Clause (E);
                           Btyp : constant Entity_Id := Base_Type (E);

                        --  What we are looking for here is the situation where
                        --  the RM_Size given would be exactly right if there
                        --  was a pragma Pack (resulting in the component size
                        --  being the same as the RM_Size). Furthermore, the
                        --  component type size must be an odd size (not a
                        --  multiple of storage unit). If the component RM size
                        --  is an exact number of storage units that is a power
                        --  of two, the array is not packed and has a standard
                        --  representation.

                        begin
                           if RM_Size (E) = Len * Rsiz
                             and then Rsiz mod System_Storage_Unit /= 0
                           then
                              --  For implicit packing mode, just set the
                              --  component size silently.

                              if Implicit_Packing then
                                 Set_Component_Size       (Btyp, Rsiz);
                                 Set_Is_Bit_Packed_Array  (Btyp);
                                 Set_Is_Packed            (Btyp);
                                 Set_Has_Non_Standard_Rep (Btyp);

                                 --  Otherwise give an error message

                              else
                                 Error_Msg_NE
                                   ("size given for& too small", SZ, E);
                                 Error_Msg_N
                                   ("\use explicit pragma Pack "
                                    & "or use pragma Implicit_Packing", SZ);
                              end if;

                           elsif RM_Size (E) = Len * Rsiz
                             and then Implicit_Packing
                             and then
                               (Rsiz / System_Storage_Unit = 1
                                 or else Rsiz / System_Storage_Unit = 2
                                 or else Rsiz / System_Storage_Unit = 4)
                           then

                              --  Not a packed array, but indicate the desired
                              --  component size, for the back-end.

                              Set_Component_Size (Btyp, Rsiz);
                           end if;
                        end;
                     end if;
                  end if;
               end;
            end if;

            --  If ancestor subtype present, freeze that first. Note that this
            --  will also get the base type frozen.

            Atype := Ancestor_Subtype (E);

            if Present (Atype) then
               Freeze_And_Append (Atype, Loc, Result);

            --  Otherwise freeze the base type of the entity before freezing
            --  the entity itself (RM 13.14(15)).

            elsif E /= Base_Type (E) then
               Freeze_And_Append (Base_Type (E), Loc, Result);
            end if;

         --  For a derived type, freeze its parent type first (RM 13.14(15))

         elsif Is_Derived_Type (E) then
            Freeze_And_Append (Etype (E), Loc, Result);
            Freeze_And_Append (First_Subtype (Etype (E)), Loc, Result);
         end if;

         --  For array type, freeze index types and component type first
         --  before freezing the array (RM 13.14(15)).

         if Is_Array_Type (E) then
            declare
               Ctyp : constant Entity_Id := Component_Type (E);

               Non_Standard_Enum : Boolean := False;
               --  Set true if any of the index types is an enumeration type
               --  with a non-standard representation.

            begin
               Freeze_And_Append (Ctyp, Loc, Result);

               Indx := First_Index (E);
               while Present (Indx) loop
                  Freeze_And_Append (Etype (Indx), Loc, Result);

                  if Is_Enumeration_Type (Etype (Indx))
                    and then Has_Non_Standard_Rep (Etype (Indx))
                  then
                     Non_Standard_Enum := True;
                  end if;

                  Next_Index (Indx);
               end loop;

               --  Processing that is done only for base types

               if Ekind (E) = E_Array_Type then

                  --  Propagate flags for component type

                  if Is_Controlled (Component_Type (E))
                    or else Has_Controlled_Component (Ctyp)
                  then
                     Set_Has_Controlled_Component (E);
                  end if;

                  if Has_Unchecked_Union (Component_Type (E)) then
                     Set_Has_Unchecked_Union (E);
                  end if;

                  --  If packing was requested or if the component size was set
                  --  explicitly, then see if bit packing is required. This
                  --  processing is only done for base types, since all the
                  --  representation aspects involved are type-related. This
                  --  is not just an optimization, if we start processing the
                  --  subtypes, they interfere with the settings on the base
                  --  type (this is because Is_Packed has a slightly different
                  --  meaning before and after freezing).

                  declare
                     Csiz : Uint;
                     Esiz : Uint;

                  begin
                     if (Is_Packed (E) or else Has_Pragma_Pack (E))
                       and then not Has_Atomic_Components (E)
                       and then Known_Static_RM_Size (Ctyp)
                     then
                        Csiz := UI_Max (RM_Size (Ctyp), 1);

                     elsif Known_Component_Size (E) then
                        Csiz := Component_Size (E);

                     elsif not Known_Static_Esize (Ctyp) then
                        Csiz := Uint_0;

                     else
                        Esiz := Esize (Ctyp);

                        --  We can set the component size if it is less than
                        --  16, rounding it up to the next storage unit size.

                        if Esiz <= 8 then
                           Csiz := Uint_8;
                        elsif Esiz <= 16 then
                           Csiz := Uint_16;
                        else
                           Csiz := Uint_0;
                        end if;

                        --  Set component size up to match alignment if it
                        --  would otherwise be less than the alignment. This
                        --  deals with cases of types whose alignment exceeds
                        --  their size (padded types).

                        if Csiz /= 0 then
                           declare
                              A : constant Uint := Alignment_In_Bits (Ctyp);
                           begin
                              if Csiz < A then
                                 Csiz := A;
                              end if;
                           end;
                        end if;
                     end if;

                     --  Case of component size that may result in packing

                     if 1 <= Csiz and then Csiz <= 64 then
                        declare
                           Ent         : constant Entity_Id :=
                                           First_Subtype (E);
                           Pack_Pragma : constant Node_Id :=
                                           Get_Rep_Pragma (Ent, Name_Pack);
                           Comp_Size_C : constant Node_Id :=
                                           Get_Attribute_Definition_Clause
                                             (Ent, Attribute_Component_Size);
                        begin
                           --  Warn if we have pack and component size so that
                           --  the pack is ignored.

                           --  Note: here we must check for the presence of a
                           --  component size before checking for a Pack pragma
                           --  to deal with the case where the array type is a
                           --  derived type whose parent is currently private.

                           if Present (Comp_Size_C)
                             and then Has_Pragma_Pack (Ent)
                           then
                              Error_Msg_Sloc := Sloc (Comp_Size_C);
                              Error_Msg_NE
                                ("?pragma Pack for& ignored!",
                                 Pack_Pragma, Ent);
                              Error_Msg_N
                                ("\?explicit component size given#!",
                                 Pack_Pragma);
                           end if;

                           --  Set component size if not already set by a
                           --  component size clause.

                           if not Present (Comp_Size_C) then
                              Set_Component_Size (E, Csiz);
                           end if;

                           --  Check for base type of 8, 16, 32 bits, where an
                           --  unsigned subtype has a length one less than the
                           --  base type (e.g. Natural subtype of Integer).

                           --  In such cases, if a component size was not set
                           --  explicitly, then generate a warning.

                           if Has_Pragma_Pack (E)
                             and then not Present (Comp_Size_C)
                             and then
                               (Csiz = 7 or else Csiz = 15 or else Csiz = 31)
                             and then Esize (Base_Type (Ctyp)) = Csiz + 1
                           then
                              Error_Msg_Uint_1 := Csiz;

                              if Present (Pack_Pragma) then
                                 Error_Msg_N
                                   ("?pragma Pack causes component size "
                                    & "to be ^!", Pack_Pragma);
                                 Error_Msg_N
                                   ("\?use Component_Size to set "
                                    & "desired value!", Pack_Pragma);
                              end if;
                           end if;

                           --  Actual packing is not needed for 8, 16, 32, 64.
                           --  Also not needed for 24 if alignment is 1.

                           if        Csiz = 8
                             or else Csiz = 16
                             or else Csiz = 32
                             or else Csiz = 64
                             or else (Csiz = 24 and then Alignment (Ctyp) = 1)
                           then
                              --  Here the array was requested to be packed,
                              --  but the packing request had no effect, so
                              --  Is_Packed is reset.

                              --  Note: semantically this means that we lose
                              --  track of the fact that a derived type
                              --  inherited a pragma Pack that was non-
                              --  effective, but that seems fine.

                              --  We regard a Pack pragma as a request to set
                              --  a representation characteristic, and this
                              --  request may be ignored.

                              Set_Is_Packed (Base_Type (E), False);

                              --  In all other cases, packing is indeed needed

                           else
                              Set_Has_Non_Standard_Rep (Base_Type (E));
                              Set_Is_Bit_Packed_Array  (Base_Type (E));
                              Set_Is_Packed            (Base_Type (E));
                           end if;
                        end;
                     end if;
                  end;

               --  Processing that is done only for subtypes

               else
                  --  Acquire alignment from base type

                  if Unknown_Alignment (E) then
                     Set_Alignment (E, Alignment (Base_Type (E)));
                     Adjust_Esize_Alignment (E);
                  end if;
               end if;

               --  For bit-packed arrays, check the size

               if Is_Bit_Packed_Array (E) and then Known_RM_Size (E) then
                  declare
                     SizC : constant Node_Id := Size_Clause (E);

                     Discard : Boolean;
                     pragma Warnings (Off, Discard);

                  begin
                     --  It is not clear if it is possible to have no size
                     --  clause at this stage, but it is not worth worrying
                     --  about. Post error on the entity name in the size
                     --  clause if present, else on the type entity itself.

                     if Present (SizC) then
                        Check_Size (Name (SizC), E, RM_Size (E), Discard);
                     else
                        Check_Size (E, E, RM_Size (E), Discard);
                     end if;
                  end;
               end if;

               --  If any of the index types was an enumeration type with
               --  a non-standard rep clause, then we indicate that the
               --  array type is always packed (even if it is not bit packed).

               if Non_Standard_Enum then
                  Set_Has_Non_Standard_Rep (Base_Type (E));
                  Set_Is_Packed            (Base_Type (E));
               end if;

               Set_Component_Alignment_If_Not_Set (E);

               --  If the array is packed, we must create the packed array
               --  type to be used to actually implement the type. This is
               --  only needed for real array types (not for string literal
               --  types, since they are present only for the front end).

               if Is_Packed (E)
                 and then Ekind (E) /= E_String_Literal_Subtype
               then
                  Create_Packed_Array_Type (E);
                  Freeze_And_Append (Packed_Array_Type (E), Loc, Result);

                  --  Size information of packed array type is copied to the
                  --  array type, since this is really the representation. But
                  --  do not override explicit existing size values. If the
                  --  ancestor subtype is constrained the packed_array_type
                  --  will be inherited from it, but the size may have been
                  --  provided already, and must not be overridden either.

                  if not Has_Size_Clause (E)
                    and then
                      (No (Ancestor_Subtype (E))
                        or else not Has_Size_Clause (Ancestor_Subtype (E)))
                  then
                     Set_Esize     (E, Esize     (Packed_Array_Type (E)));
                     Set_RM_Size   (E, RM_Size   (Packed_Array_Type (E)));
                  end if;

                  if not Has_Alignment_Clause (E) then
                     Set_Alignment (E, Alignment (Packed_Array_Type (E)));
                  end if;
               end if;

               --  For non-packed arrays set the alignment of the array to the
               --  alignment of the component type if it is unknown. Skip this
               --  in atomic case (atomic arrays may need larger alignments).

               if not Is_Packed (E)
                 and then Unknown_Alignment (E)
                 and then Known_Alignment (Ctyp)
                 and then Known_Static_Component_Size (E)
                 and then Known_Static_Esize (Ctyp)
                 and then Esize (Ctyp) = Component_Size (E)
                 and then not Is_Atomic (E)
               then
                  Set_Alignment (E, Alignment (Component_Type (E)));
               end if;
            end;

         --  For a class-wide type, the corresponding specific type is
         --  frozen as well (RM 13.14(15))

         elsif Is_Class_Wide_Type (E) then
            Freeze_And_Append (Root_Type (E), Loc, Result);

            --  If the base type of the class-wide type is still incomplete,
            --  the class-wide remains unfrozen as well. This is legal when
            --  E is the formal of a primitive operation of some other type
            --  which is being frozen.

            if not Is_Frozen (Root_Type (E)) then
               Set_Is_Frozen (E, False);
               return Result;
            end if;

            --  If the Class_Wide_Type is an Itype (when type is the anonymous
            --  parent of a derived type) and it is a library-level entity,
            --  generate an itype reference for it. Otherwise, its first
            --  explicit reference may be in an inner scope, which will be
            --  rejected by the back-end.

            if Is_Itype (E)
              and then Is_Compilation_Unit (Scope (E))
            then
               declare
                  Ref : constant Node_Id := Make_Itype_Reference (Loc);

               begin
                  Set_Itype (Ref, E);
                  if No (Result) then
                     Result := New_List (Ref);
                  else
                     Append (Ref, Result);
                  end if;
               end;
            end if;

            --  The equivalent type associated with a class-wide subtype needs
            --  to be frozen to ensure that its layout is done.

            if Ekind (E) = E_Class_Wide_Subtype
              and then Present (Equivalent_Type (E))
            then
               Freeze_And_Append (Equivalent_Type (E), Loc, Result);
            end if;

         --  For a record (sub)type, freeze all the component types (RM
         --  13.14(15). We test for E_Record_(sub)Type here, rather than using
         --  Is_Record_Type, because we don't want to attempt the freeze for
         --  the case of a private type with record extension (we will do that
         --  later when the full type is frozen).

         elsif Ekind (E) = E_Record_Type
           or else Ekind (E) = E_Record_Subtype
         then
            Freeze_Record_Type (E);

         --  For a concurrent type, freeze corresponding record type. This
         --  does not correspond to any specific rule in the RM, but the
         --  record type is essentially part of the concurrent type.
         --  Freeze as well all local entities. This includes record types
         --  created for entry parameter blocks, and whatever local entities
         --  may appear in the private part.

         elsif Is_Concurrent_Type (E) then
            if Present (Corresponding_Record_Type (E)) then
               Freeze_And_Append
                 (Corresponding_Record_Type (E), Loc, Result);
            end if;

            Comp := First_Entity (E);
            while Present (Comp) loop
               if Is_Type (Comp) then
                  Freeze_And_Append (Comp, Loc, Result);

               elsif (Ekind (Comp)) /= E_Function then
                  if Is_Itype (Etype (Comp))
                    and then Underlying_Type (Scope (Etype (Comp))) = E
                  then
                     Undelay_Type (Etype (Comp));
                  end if;

                  Freeze_And_Append (Etype (Comp), Loc, Result);
               end if;

               Next_Entity (Comp);
            end loop;

         --  Private types are required to point to the same freeze node as
         --  their corresponding full views. The freeze node itself has to
         --  point to the partial view of the entity (because from the partial
         --  view, we can retrieve the full view, but not the reverse).
         --  However, in order to freeze correctly, we need to freeze the full
         --  view. If we are freezing at the end of a scope (or within the
         --  scope of the private type), the partial and full views will have
         --  been swapped, the full view appears first in the entity chain and
         --  the swapping mechanism ensures that the pointers are properly set
         --  (on scope exit).

         --  If we encounter the partial view before the full view (e.g. when
         --  freezing from another scope), we freeze the full view, and then
         --  set the pointers appropriately since we cannot rely on swapping to
         --  fix things up (subtypes in an outer scope might not get swapped).

         elsif Is_Incomplete_Or_Private_Type (E)
           and then not Is_Generic_Type (E)
         then
            --  The construction of the dispatch table associated with library
            --  level tagged types forces freezing of all the primitives of the
            --  type, which may cause premature freezing of the partial view.
            --  For example:

            --     package Pkg is
            --        type T is tagged private;
            --        type DT is new T with private;
            --        procedure Prim (X : in out T; Y : in out DT'class);
            --     private
            --        type T is tagged null record;
            --        Obj : T;
            --        type DT is new T with null record;
            --     end;

            --  In this case the type will be frozen later by the usual
            --  mechanism: an object declaration, an instantiation, or the
            --  end of a declarative part.

            if Is_Library_Level_Tagged_Type (E)
              and then not Present (Full_View (E))
            then
               Set_Is_Frozen (E, False);
               return Result;

            --  Case of full view present

            elsif Present (Full_View (E)) then

               --  If full view has already been frozen, then no further
               --  processing is required

               if Is_Frozen (Full_View (E)) then

                  Set_Has_Delayed_Freeze (E, False);
                  Set_Freeze_Node (E, Empty);
                  Check_Debug_Info_Needed (E);

               --  Otherwise freeze full view and patch the pointers so that
               --  the freeze node will elaborate both views in the back-end.

               else
                  declare
                     Full : constant Entity_Id := Full_View (E);

                  begin
                     if Is_Private_Type (Full)
                       and then Present (Underlying_Full_View (Full))
                     then
                        Freeze_And_Append
                          (Underlying_Full_View (Full), Loc, Result);
                     end if;

                     Freeze_And_Append (Full, Loc, Result);

                     if Has_Delayed_Freeze (E) then
                        F_Node := Freeze_Node (Full);

                        if Present (F_Node) then
                           Set_Freeze_Node (E, F_Node);
                           Set_Entity (F_Node, E);

                        else
                           --  {Incomplete,Private}_Subtypes with Full_Views
                           --  constrained by discriminants.

                           Set_Has_Delayed_Freeze (E, False);
                           Set_Freeze_Node (E, Empty);
                        end if;
                     end if;
                  end;

                  Check_Debug_Info_Needed (E);
               end if;

               --  AI-117 requires that the convention of a partial view be the
               --  same as the convention of the full view. Note that this is a
               --  recognized breach of privacy, but it's essential for logical
               --  consistency of representation, and the lack of a rule in
               --  RM95 was an oversight.

               Set_Convention (E, Convention (Full_View (E)));

               Set_Size_Known_At_Compile_Time (E,
                 Size_Known_At_Compile_Time (Full_View (E)));

               --  Size information is copied from the full view to the
               --  incomplete or private view for consistency.

               --  We skip this is the full view is not a type. This is very
               --  strange of course, and can only happen as a result of
               --  certain illegalities, such as a premature attempt to derive
               --  from an incomplete type.

               if Is_Type (Full_View (E)) then
                  Set_Size_Info (E, Full_View (E));
                  Set_RM_Size   (E, RM_Size (Full_View (E)));
               end if;

               return Result;

            --  Case of no full view present. If entity is derived or subtype,
            --  it is safe to freeze, correctness depends on the frozen status
            --  of parent. Otherwise it is either premature usage, or a Taft
            --  amendment type, so diagnosis is at the point of use and the
            --  type might be frozen later.

            elsif E /= Base_Type (E)
              or else Is_Derived_Type (E)
            then
               null;

            else
               Set_Is_Frozen (E, False);
               return No_List;
            end if;

         --  For access subprogram, freeze types of all formals, the return
         --  type was already frozen, since it is the Etype of the function.
         --  Formal types can be tagged Taft amendment types, but otherwise
         --  they cannot be incomplete.

         elsif Ekind (E) = E_Subprogram_Type then
            Formal := First_Formal (E);

            while Present (Formal) loop
               if Ekind (Etype (Formal)) = E_Incomplete_Type
                 and then No (Full_View (Etype (Formal)))
                 and then not Is_Value_Type (Etype (Formal))
               then
                  if Is_Tagged_Type (Etype (Formal)) then
                     null;
                  else
                     Error_Msg_NE
                       ("invalid use of incomplete type&", E, Etype (Formal));
                  end if;
               end if;

               Freeze_And_Append (Etype (Formal), Loc, Result);
               Next_Formal (Formal);
            end loop;

            Freeze_Subprogram (E);

         --  For access to a protected subprogram, freeze the equivalent type
         --  (however this is not set if we are not generating code or if this
         --  is an anonymous type used just for resolution).

         elsif Is_Access_Protected_Subprogram_Type (E) then
            if Present (Equivalent_Type (E)) then
               Freeze_And_Append (Equivalent_Type (E), Loc, Result);
            end if;
         end if;

         --  Generic types are never seen by the back-end, and are also not
         --  processed by the expander (since the expander is turned off for
         --  generic processing), so we never need freeze nodes for them.

         if Is_Generic_Type (E) then
            return Result;
         end if;

         --  Some special processing for non-generic types to complete
         --  representation details not known till the freeze point.

         if Is_Fixed_Point_Type (E) then
            Freeze_Fixed_Point_Type (E);

            --  Some error checks required for ordinary fixed-point type. Defer
            --  these till the freeze-point since we need the small and range
            --  values. We only do these checks for base types

            if Is_Ordinary_Fixed_Point_Type (E)
              and then E = Base_Type (E)
            then
               if Small_Value (E) < Ureal_2_M_80 then
                  Error_Msg_Name_1 := Name_Small;
                  Error_Msg_N
                    ("`&''%` too small, minimum allowed is 2.0'*'*(-80)", E);

               elsif Small_Value (E) > Ureal_2_80 then
                  Error_Msg_Name_1 := Name_Small;
                  Error_Msg_N
                    ("`&''%` too large, maximum allowed is 2.0'*'*80", E);
               end if;

               if Expr_Value_R (Type_Low_Bound (E)) < Ureal_M_10_36 then
                  Error_Msg_Name_1 := Name_First;
                  Error_Msg_N
                    ("`&''%` too small, minimum allowed is -10.0'*'*36", E);
               end if;

               if Expr_Value_R (Type_High_Bound (E)) > Ureal_10_36 then
                  Error_Msg_Name_1 := Name_Last;
                  Error_Msg_N
                    ("`&''%` too large, maximum allowed is 10.0'*'*36", E);
               end if;
            end if;

         elsif Is_Enumeration_Type (E) then
            Freeze_Enumeration_Type (E);

         elsif Is_Integer_Type (E) then
            Adjust_Esize_For_Alignment (E);

            if Is_Modular_Integer_Type (E)
              and then Warn_On_Suspicious_Modulus_Value
            then
               Check_Suspicious_Modulus (E);
            end if;

         elsif Is_Access_Type (E) then

            --  Check restriction for standard storage pool

            if No (Associated_Storage_Pool (E)) then
               Check_Restriction (No_Standard_Storage_Pools, E);
            end if;

            --  Deal with error message for pure access type. This is not an
            --  error in Ada 2005 if there is no pool (see AI-366).

            if Is_Pure_Unit_Access_Type (E)
              and then (Ada_Version < Ada_05
                         or else not No_Pool_Assigned (E))
            then
               Error_Msg_N ("named access type not allowed in pure unit", E);

               if Ada_Version >= Ada_05 then
                  Error_Msg_N
                    ("\would be legal if Storage_Size of 0 given?", E);

               elsif No_Pool_Assigned (E) then
                  Error_Msg_N
                    ("\would be legal in Ada 2005?", E);

               else
                  Error_Msg_N
                    ("\would be legal in Ada 2005 if "
                     & "Storage_Size of 0 given?", E);
               end if;
            end if;
         end if;

         --  Case of composite types

         if Is_Composite_Type (E) then

            --  AI-117 requires that all new primitives of a tagged type must
            --  inherit the convention of the full view of the type. Inherited
            --  and overriding operations are defined to inherit the convention
            --  of their parent or overridden subprogram (also specified in
            --  AI-117), which will have occurred earlier (in Derive_Subprogram
            --  and New_Overloaded_Entity). Here we set the convention of
            --  primitives that are still convention Ada, which will ensure
            --  that any new primitives inherit the type's convention. Class-
            --  wide types can have a foreign convention inherited from their
            --  specific type, but are excluded from this since they don't have
            --  any associated primitives.

            if Is_Tagged_Type (E)
              and then not Is_Class_Wide_Type (E)
              and then Convention (E) /= Convention_Ada
            then
               declare
                  Prim_List : constant Elist_Id := Primitive_Operations (E);
                  Prim      : Elmt_Id;
               begin
                  Prim := First_Elmt (Prim_List);
                  while Present (Prim) loop
                     if Convention (Node (Prim)) = Convention_Ada then
                        Set_Convention (Node (Prim), Convention (E));
                     end if;

                     Next_Elmt (Prim);
                  end loop;
               end;
            end if;
         end if;

         --  Now that all types from which E may depend are frozen, see if the
         --  size is known at compile time, if it must be unsigned, or if
         --  strict alignment is required

         Check_Compile_Time_Size (E);
         Check_Unsigned_Type (E);

         if Base_Type (E) = E then
            Check_Strict_Alignment (E);
         end if;

         --  Do not allow a size clause for a type which does not have a size
         --  that is known at compile time

         if Has_Size_Clause (E)
           and then not Size_Known_At_Compile_Time (E)
         then
            --  Suppress this message if errors posted on E, even if we are
            --  in all errors mode, since this is often a junk message

            if not Error_Posted (E) then
               Error_Msg_N
                 ("size clause not allowed for variable length type",
                  Size_Clause (E));
            end if;
         end if;

         --  Remaining process is to set/verify the representation information,
         --  in particular the size and alignment values. This processing is
         --  not required for generic types, since generic types do not play
         --  any part in code generation, and so the size and alignment values
         --  for such types are irrelevant.

         if Is_Generic_Type (E) then
            return Result;

         --  Otherwise we call the layout procedure

         else
            Layout_Type (E);
         end if;

         --  End of freeze processing for type entities
      end if;

      --  Here is where we logically freeze the current entity. If it has a
      --  freeze node, then this is the point at which the freeze node is
      --  linked into the result list.

      if Has_Delayed_Freeze (E) then

         --  If a freeze node is already allocated, use it, otherwise allocate
         --  a new one. The preallocation happens in the case of anonymous base
         --  types, where we preallocate so that we can set First_Subtype_Link.
         --  Note that we reset the Sloc to the current freeze location.

         if Present (Freeze_Node (E)) then
            F_Node := Freeze_Node (E);
            Set_Sloc (F_Node, Loc);

         else
            F_Node := New_Node (N_Freeze_Entity, Loc);
            Set_Freeze_Node (E, F_Node);
            Set_Access_Types_To_Process (F_Node, No_Elist);
            Set_TSS_Elist (F_Node, No_Elist);
            Set_Actions (F_Node, No_List);
         end if;

         Set_Entity (F_Node, E);

         if Result = No_List then
            Result := New_List (F_Node);
         else
            Append (F_Node, Result);
         end if;

         --  A final pass over record types with discriminants. If the type
         --  has an incomplete declaration, there may be constrained access
         --  subtypes declared elsewhere, which do not depend on the discrimi-
         --  nants of the type, and which are used as component types (i.e.
         --  the full view is a recursive type). The designated types of these
         --  subtypes can only be elaborated after the type itself, and they
         --  need an itype reference.

         if Ekind (E) = E_Record_Type
           and then Has_Discriminants (E)
         then
            declare
               Comp : Entity_Id;
               IR   : Node_Id;
               Typ  : Entity_Id;

            begin
               Comp := First_Component (E);

               while Present (Comp) loop
                  Typ  := Etype (Comp);

                  if Ekind (Comp) = E_Component
                    and then Is_Access_Type (Typ)
                    and then Scope (Typ) /= E
                    and then Base_Type (Designated_Type (Typ)) = E
                    and then Is_Itype (Designated_Type (Typ))
                  then
                     IR := Make_Itype_Reference (Sloc (Comp));
                     Set_Itype (IR, Designated_Type (Typ));
                     Append (IR, Result);
                  end if;

                  Next_Component (Comp);
               end loop;
            end;
         end if;
      end if;

      --  When a type is frozen, the first subtype of the type is frozen as
      --  well (RM 13.14(15)). This has to be done after freezing the type,
      --  since obviously the first subtype depends on its own base type.

      if Is_Type (E) then
         Freeze_And_Append (First_Subtype (E), Loc, Result);

         --  If we just froze a tagged non-class wide record, then freeze the
         --  corresponding class-wide type. This must be done after the tagged
         --  type itself is frozen, because the class-wide type refers to the
         --  tagged type which generates the class.

         if Is_Tagged_Type (E)
           and then not Is_Class_Wide_Type (E)
           and then Present (Class_Wide_Type (E))
         then
            Freeze_And_Append (Class_Wide_Type (E), Loc, Result);
         end if;
      end if;

      Check_Debug_Info_Needed (E);

      --  Special handling for subprograms

      if Is_Subprogram (E) then

         --  If subprogram has address clause then reset Is_Public flag, since
         --  we do not want the backend to generate external references.

         if Present (Address_Clause (E))
           and then not Is_Library_Level_Entity (E)
         then
            Set_Is_Public (E, False);

         --  If no address clause and not intrinsic, then for imported
         --  subprogram in main unit, generate descriptor if we are in
         --  Propagate_Exceptions mode.

         elsif Propagate_Exceptions
           and then Is_Imported (E)
           and then not Is_Intrinsic_Subprogram (E)
           and then Convention (E) /= Convention_Stubbed
         then
            if Result = No_List then
               Result := Empty_List;
            end if;
         end if;
      end if;

      return Result;
   end Freeze_Entity;

   -----------------------------
   -- Freeze_Enumeration_Type --
   -----------------------------

   procedure Freeze_Enumeration_Type (Typ : Entity_Id) is
   begin
      --  By default, if no size clause is present, an enumeration type with
      --  Convention C is assumed to interface to a C enum, and has integer
      --  size. This applies to types. For subtypes, verify that its base
      --  type has no size clause either.

      if Has_Foreign_Convention (Typ)
        and then not Has_Size_Clause (Typ)
        and then not Has_Size_Clause (Base_Type (Typ))
        and then Esize (Typ) < Standard_Integer_Size
      then
         Init_Esize (Typ, Standard_Integer_Size);

      else
         --  If the enumeration type interfaces to C, and it has a size clause
         --  that specifies less than int size, it warrants a warning. The
         --  user may intend the C type to be an enum or a char, so this is
         --  not by itself an error that the Ada compiler can detect, but it
         --  it is a worth a heads-up. For Boolean and Character types we
         --  assume that the programmer has the proper C type in mind.

         if Convention (Typ) = Convention_C
           and then Has_Size_Clause (Typ)
           and then Esize (Typ) /= Esize (Standard_Integer)
           and then not Is_Boolean_Type (Typ)
           and then not Is_Character_Type (Typ)
         then
            Error_Msg_N
              ("C enum types have the size of a C int?", Size_Clause (Typ));
         end if;

         Adjust_Esize_For_Alignment (Typ);
      end if;
   end Freeze_Enumeration_Type;

   -----------------------
   -- Freeze_Expression --
   -----------------------

   procedure Freeze_Expression (N : Node_Id) is
      In_Spec_Exp : constant Boolean := In_Spec_Expression;
      Typ         : Entity_Id;
      Nam         : Entity_Id;
      Desig_Typ   : Entity_Id;
      P           : Node_Id;
      Parent_P    : Node_Id;

      Freeze_Outside : Boolean := False;
      --  This flag is set true if the entity must be frozen outside the
      --  current subprogram. This happens in the case of expander generated
      --  subprograms (_Init_Proc, _Input, _Output, _Read, _Write) which do
      --  not freeze all entities like other bodies, but which nevertheless
      --  may reference entities that have to be frozen before the body and
      --  obviously cannot be frozen inside the body.

      function In_Exp_Body (N : Node_Id) return Boolean;
      --  Given an N_Handled_Sequence_Of_Statements node N, determines whether
      --  it is the handled statement sequence of an expander-generated
      --  subprogram (init proc, stream subprogram, or renaming as body).
      --  If so, this is not a freezing context.

      -----------------
      -- In_Exp_Body --
      -----------------

      function In_Exp_Body (N : Node_Id) return Boolean is
         P  : Node_Id;
         Id : Entity_Id;

      begin
         if Nkind (N) = N_Subprogram_Body then
            P := N;
         else
            P := Parent (N);
         end if;

         if Nkind (P) /= N_Subprogram_Body then
            return False;

         else
            Id := Defining_Unit_Name (Specification (P));

            if Nkind (Id) = N_Defining_Identifier
              and then (Is_Init_Proc (Id)              or else
                        Is_TSS (Id, TSS_Stream_Input)  or else
                        Is_TSS (Id, TSS_Stream_Output) or else
                        Is_TSS (Id, TSS_Stream_Read)   or else
                        Is_TSS (Id, TSS_Stream_Write)  or else
                        Nkind (Original_Node (P)) =
                          N_Subprogram_Renaming_Declaration)
            then
               return True;
            else
               return False;
            end if;
         end if;
      end In_Exp_Body;

   --  Start of processing for Freeze_Expression

   begin
      --  Immediate return if freezing is inhibited. This flag is set by the
      --  analyzer to stop freezing on generated expressions that would cause
      --  freezing if they were in the source program, but which are not
      --  supposed to freeze, since they are created.

      if Must_Not_Freeze (N) then
         return;
      end if;

      --  If expression is non-static, then it does not freeze in a default
      --  expression, see section "Handling of Default Expressions" in the
      --  spec of package Sem for further details. Note that we have to
      --  make sure that we actually have a real expression (if we have
      --  a subtype indication, we can't test Is_Static_Expression!)

      if In_Spec_Exp
        and then Nkind (N) in N_Subexpr
        and then not Is_Static_Expression (N)
      then
         return;
      end if;

      --  Freeze type of expression if not frozen already

      Typ := Empty;

      if Nkind (N) in N_Has_Etype then
         if not Is_Frozen (Etype (N)) then
            Typ := Etype (N);

         --  Base type may be an derived numeric type that is frozen at
         --  the point of declaration, but first_subtype is still unfrozen.

         elsif not Is_Frozen (First_Subtype (Etype (N))) then
            Typ := First_Subtype (Etype (N));
         end if;
      end if;

      --  For entity name, freeze entity if not frozen already. A special
      --  exception occurs for an identifier that did not come from source.
      --  We don't let such identifiers freeze a non-internal entity, i.e.
      --  an entity that did come from source, since such an identifier was
      --  generated by the expander, and cannot have any semantic effect on
      --  the freezing semantics. For example, this stops the parameter of
      --  an initialization procedure from freezing the variable.

      if Is_Entity_Name (N)
        and then not Is_Frozen (Entity (N))
        and then (Nkind (N) /= N_Identifier
                   or else Comes_From_Source (N)
                   or else not Comes_From_Source (Entity (N)))
      then
         Nam := Entity (N);
      else
         Nam := Empty;
      end if;

      --  For an allocator freeze designated type if not frozen already

      --  For an aggregate whose component type is an access type, freeze the
      --  designated type now, so that its freeze does not appear within the
      --  loop that might be created in the expansion of the aggregate. If the
      --  designated type is a private type without full view, the expression
      --  cannot contain an allocator, so the type is not frozen.

      --  For a function, we freeze the entity when the subprogram declaration
      --  is frozen, but a function call may appear in an initialization proc.
      --  before the declaration is frozen. We need to generate the extra
      --  formals, if any, to ensure that the expansion of the call includes
      --  the proper actuals. This only applies to Ada subprograms, not to
      --  imported ones.

      Desig_Typ := Empty;

      case Nkind (N) is
         when N_Allocator =>
            Desig_Typ := Designated_Type (Etype (N));

         when N_Aggregate =>
            if Is_Array_Type (Etype (N))
              and then Is_Access_Type (Component_Type (Etype (N)))
            then
               Desig_Typ := Designated_Type (Component_Type (Etype (N)));
            end if;

         when N_Selected_Component |
            N_Indexed_Component    |
            N_Slice                =>

            if Is_Access_Type (Etype (Prefix (N))) then
               Desig_Typ := Designated_Type (Etype (Prefix (N)));
            end if;

         when N_Identifier =>
            if Present (Nam)
              and then Ekind (Nam) = E_Function
              and then Nkind (Parent (N)) = N_Function_Call
              and then Convention (Nam) = Convention_Ada
            then
               Create_Extra_Formals (Nam);
            end if;

         when others =>
            null;
      end case;

      if Desig_Typ /= Empty
        and then (Is_Frozen (Desig_Typ)
                   or else (not Is_Fully_Defined (Desig_Typ)))
      then
         Desig_Typ := Empty;
      end if;

      --  All done if nothing needs freezing

      if No (Typ)
        and then No (Nam)
        and then No (Desig_Typ)
      then
         return;
      end if;

      --  Loop for looking at the right place to insert the freeze nodes,
      --  exiting from the loop when it is appropriate to insert the freeze
      --  node before the current node P.

      --  Also checks som special exceptions to the freezing rules. These cases
      --  result in a direct return, bypassing the freeze action.

      P := N;
      loop
         Parent_P := Parent (P);

         --  If we don't have a parent, then we are not in a well-formed tree.
         --  This is an unusual case, but there are some legitimate situations
         --  in which this occurs, notably when the expressions in the range of
         --  a type declaration are resolved. We simply ignore the freeze
         --  request in this case. Is this right ???

         if No (Parent_P) then
            return;
         end if;

         --  See if we have got to an appropriate point in the tree

         case Nkind (Parent_P) is

            --  A special test for the exception of (RM 13.14(8)) for the case
            --  of per-object expressions (RM 3.8(18)) occurring in component
            --  definition or a discrete subtype definition. Note that we test
            --  for a component declaration which includes both cases we are
            --  interested in, and furthermore the tree does not have explicit
            --  nodes for either of these two constructs.

            when N_Component_Declaration =>

               --  The case we want to test for here is an identifier that is
               --  a per-object expression, this is either a discriminant that
               --  appears in a context other than the component declaration
               --  or it is a reference to the type of the enclosing construct.

               --  For either of these cases, we skip the freezing

               if not In_Spec_Expression
                 and then Nkind (N) = N_Identifier
                 and then (Present (Entity (N)))
               then
                  --  We recognize the discriminant case by just looking for
                  --  a reference to a discriminant. It can only be one for
                  --  the enclosing construct. Skip freezing in this case.

                  if Ekind (Entity (N)) = E_Discriminant then
                     return;

                  --  For the case of a reference to the enclosing record,
                  --  (or task or protected type), we look for a type that
                  --  matches the current scope.

                  elsif Entity (N) = Current_Scope then
                     return;
                  end if;
               end if;

            --  If we have an enumeration literal that appears as the choice in
            --  the aggregate of an enumeration representation clause, then
            --  freezing does not occur (RM 13.14(10)).

            when N_Enumeration_Representation_Clause =>

               --  The case we are looking for is an enumeration literal

               if (Nkind (N) = N_Identifier or Nkind (N) = N_Character_Literal)
                 and then Is_Enumeration_Type (Etype (N))
               then
                  --  If enumeration literal appears directly as the choice,
                  --  do not freeze (this is the normal non-overloaded case)

                  if Nkind (Parent (N)) = N_Component_Association
                    and then First (Choices (Parent (N))) = N
                  then
                     return;

                  --  If enumeration literal appears as the name of function
                  --  which is the choice, then also do not freeze. This
                  --  happens in the overloaded literal case, where the
                  --  enumeration literal is temporarily changed to a function
                  --  call for overloading analysis purposes.

                  elsif Nkind (Parent (N)) = N_Function_Call
                     and then
                       Nkind (Parent (Parent (N))) = N_Component_Association
                     and then
                       First (Choices (Parent (Parent (N)))) = Parent (N)
                  then
                     return;
                  end if;
               end if;

            --  Normally if the parent is a handled sequence of statements,
            --  then the current node must be a statement, and that is an
            --  appropriate place to insert a freeze node.

            when N_Handled_Sequence_Of_Statements =>

               --  An exception occurs when the sequence of statements is for
               --  an expander generated body that did not do the usual freeze
               --  all operation. In this case we usually want to freeze
               --  outside this body, not inside it, and we skip past the
               --  subprogram body that we are inside.

               if In_Exp_Body (Parent_P) then

                  --  However, we *do* want to freeze at this point if we have
                  --  an entity to freeze, and that entity is declared *inside*
                  --  the body of the expander generated procedure. This case
                  --  is recognized by the scope of the type, which is either
                  --  the spec for some enclosing body, or (in the case of
                  --  init_procs, for which there are no separate specs) the
                  --  current scope.

                  declare
                     Subp : constant Node_Id := Parent (Parent_P);
                     Cspc : Entity_Id;

                  begin
                     if Nkind (Subp) = N_Subprogram_Body then
                        Cspc := Corresponding_Spec (Subp);

                        if (Present (Typ) and then Scope (Typ) = Cspc)
                             or else
                           (Present (Nam) and then Scope (Nam) = Cspc)
                        then
                           exit;

                        elsif Present (Typ)
                          and then Scope (Typ) = Current_Scope
                          and then Current_Scope = Defining_Entity (Subp)
                        then
                           exit;
                        end if;
                     end if;
                  end;

                  --  If not that exception to the exception, then this is
                  --  where we delay the freeze till outside the body.

                  Parent_P := Parent (Parent_P);
                  Freeze_Outside := True;

               --  Here if normal case where we are in handled statement
               --  sequence and want to do the insertion right there.

               else
                  exit;
               end if;

            --  If parent is a body or a spec or a block, then the current node
            --  is a statement or declaration and we can insert the freeze node
            --  before it.

            when N_Package_Specification |
                 N_Package_Body          |
                 N_Subprogram_Body       |
                 N_Task_Body             |
                 N_Protected_Body        |
                 N_Entry_Body            |
                 N_Block_Statement       => exit;

            --  The expander is allowed to define types in any statements list,
            --  so any of the following parent nodes also mark a freezing point
            --  if the actual node is in a list of statements or declarations.

            when N_Exception_Handler          |
                 N_If_Statement               |
                 N_Elsif_Part                 |
                 N_Case_Statement_Alternative |
                 N_Compilation_Unit_Aux       |
                 N_Selective_Accept           |
                 N_Accept_Alternative         |
                 N_Delay_Alternative          |
                 N_Conditional_Entry_Call     |
                 N_Entry_Call_Alternative     |
                 N_Triggering_Alternative     |
                 N_Abortable_Part             |
                 N_Freeze_Entity              =>

               exit when Is_List_Member (P);

            --  Note: The N_Loop_Statement is a special case. A type that
            --  appears in the source can never be frozen in a loop (this
            --  occurs only because of a loop expanded by the expander), so we
            --  keep on going. Otherwise we terminate the search. Same is true
            --  of any entity which comes from source. (if they have predefined
            --  type, that type does not appear to come from source, but the
            --  entity should not be frozen here).

            when N_Loop_Statement =>
               exit when not Comes_From_Source (Etype (N))
                 and then (No (Nam) or else not Comes_From_Source (Nam));

            --  For all other cases, keep looking at parents

            when others =>
               null;
         end case;

         --  We fall through the case if we did not yet find the proper
         --  place in the free for inserting the freeze node, so climb!

         P := Parent_P;
      end loop;

      --  If the expression appears in a record or an initialization procedure,
      --  the freeze nodes are collected and attached to the current scope, to
      --  be inserted and analyzed on exit from the scope, to insure that
      --  generated entities appear in the correct scope. If the expression is
      --  a default for a discriminant specification, the scope is still void.
      --  The expression can also appear in the discriminant part of a private
      --  or concurrent type.

      --  If the expression appears in a constrained subcomponent of an
      --  enclosing record declaration, the freeze nodes must be attached to
      --  the outer record type so they can eventually be placed in the
      --  enclosing declaration list.

      --  The other case requiring this special handling is if we are in a
      --  default expression, since in that case we are about to freeze a
      --  static type, and the freeze scope needs to be the outer scope, not
      --  the scope of the subprogram with the default parameter.

      --  For default expressions and other spec expressions in generic units,
      --  the Move_Freeze_Nodes mechanism (see sem_ch12.adb) takes care of
      --  placing them at the proper place, after the generic unit.

      if (In_Spec_Exp and not Inside_A_Generic)
        or else Freeze_Outside
        or else (Is_Type (Current_Scope)
                  and then (not Is_Concurrent_Type (Current_Scope)
                             or else not Has_Completion (Current_Scope)))
        or else Ekind (Current_Scope) = E_Void
      then
         declare
            Loc          : constant Source_Ptr := Sloc (Current_Scope);
            Freeze_Nodes : List_Id := No_List;
            Pos          : Int := Scope_Stack.Last;

         begin
            if Present (Desig_Typ) then
               Freeze_And_Append (Desig_Typ, Loc, Freeze_Nodes);
            end if;

            if Present (Typ) then
               Freeze_And_Append (Typ, Loc, Freeze_Nodes);
            end if;

            if Present (Nam) then
               Freeze_And_Append (Nam, Loc, Freeze_Nodes);
            end if;

            --  The current scope may be that of a constrained component of
            --  an enclosing record declaration, which is above the current
            --  scope in the scope stack.

            if Is_Record_Type (Scope (Current_Scope)) then
               Pos := Pos - 1;
            end if;

            if Is_Non_Empty_List (Freeze_Nodes) then
               if No (Scope_Stack.Table (Pos).Pending_Freeze_Actions) then
                  Scope_Stack.Table (Pos).Pending_Freeze_Actions :=
                      Freeze_Nodes;
               else
                  Append_List (Freeze_Nodes, Scope_Stack.Table
                                   (Pos).Pending_Freeze_Actions);
               end if;
            end if;
         end;

         return;
      end if;

      --  Now we have the right place to do the freezing. First, a special
      --  adjustment, if we are in spec-expression analysis mode, these freeze
      --  actions must not be thrown away (normally all inserted actions are
      --  thrown away in this mode. However, the freeze actions are from static
      --  expressions and one of the important reasons we are doing this
      --  special analysis is to get these freeze actions. Therefore we turn
      --  off the In_Spec_Expression mode to propagate these freeze actions.
      --  This also means they get properly analyzed and expanded.

      In_Spec_Expression := False;

      --  Freeze the designated type of an allocator (RM 13.14(13))

      if Present (Desig_Typ) then
         Freeze_Before (P, Desig_Typ);
      end if;

      --  Freeze type of expression (RM 13.14(10)). Note that we took care of
      --  the enumeration representation clause exception in the loop above.

      if Present (Typ) then
         Freeze_Before (P, Typ);
      end if;

      --  Freeze name if one is present (RM 13.14(11))

      if Present (Nam) then
         Freeze_Before (P, Nam);
      end if;

      --  Restore In_Spec_Expression flag

      In_Spec_Expression := In_Spec_Exp;
   end Freeze_Expression;

   -----------------------------
   -- Freeze_Fixed_Point_Type --
   -----------------------------

   --  Certain fixed-point types and subtypes, including implicit base types
   --  and declared first subtypes, have not yet set up a range. This is
   --  because the range cannot be set until the Small and Size values are
   --  known, and these are not known till the type is frozen.

   --  To signal this case, Scalar_Range contains an unanalyzed syntactic range
   --  whose bounds are unanalyzed real literals. This routine will recognize
   --  this case, and transform this range node into a properly typed range
   --  with properly analyzed and resolved values.

   procedure Freeze_Fixed_Point_Type (Typ : Entity_Id) is
      Rng   : constant Node_Id    := Scalar_Range (Typ);
      Lo    : constant Node_Id    := Low_Bound (Rng);
      Hi    : constant Node_Id    := High_Bound (Rng);
      Btyp  : constant Entity_Id  := Base_Type (Typ);
      Brng  : constant Node_Id    := Scalar_Range (Btyp);
      BLo   : constant Node_Id    := Low_Bound (Brng);
      BHi   : constant Node_Id    := High_Bound (Brng);
      Small : constant Ureal      := Small_Value (Typ);
      Loval : Ureal;
      Hival : Ureal;
      Atype : Entity_Id;

      Actual_Size : Nat;

      function Fsize (Lov, Hiv : Ureal) return Nat;
      --  Returns size of type with given bounds. Also leaves these
      --  bounds set as the current bounds of the Typ.

      -----------
      -- Fsize --
      -----------

      function Fsize (Lov, Hiv : Ureal) return Nat is
      begin
         Set_Realval (Lo, Lov);
         Set_Realval (Hi, Hiv);
         return Minimum_Size (Typ);
      end Fsize;

   --  Start of processing for Freeze_Fixed_Point_Type

   begin
      --  If Esize of a subtype has not previously been set, set it now

      if Unknown_Esize (Typ) then
         Atype := Ancestor_Subtype (Typ);

         if Present (Atype) then
            Set_Esize (Typ, Esize (Atype));
         else
            Set_Esize (Typ, Esize (Base_Type (Typ)));
         end if;
      end if;

      --  Immediate return if the range is already analyzed. This means that
      --  the range is already set, and does not need to be computed by this
      --  routine.

      if Analyzed (Rng) then
         return;
      end if;

      --  Immediate return if either of the bounds raises Constraint_Error

      if Raises_Constraint_Error (Lo)
        or else Raises_Constraint_Error (Hi)
      then
         return;
      end if;

      Loval := Realval (Lo);
      Hival := Realval (Hi);

      --  Ordinary fixed-point case

      if Is_Ordinary_Fixed_Point_Type (Typ) then

         --  For the ordinary fixed-point case, we are allowed to fudge the
         --  end-points up or down by small. Generally we prefer to fudge up,
         --  i.e. widen the bounds for non-model numbers so that the end points
         --  are included. However there are cases in which this cannot be
         --  done, and indeed cases in which we may need to narrow the bounds.
         --  The following circuit makes the decision.

         --  Note: our terminology here is that Incl_EP means that the bounds
         --  are widened by Small if necessary to include the end points, and
         --  Excl_EP means that the bounds are narrowed by Small to exclude the
         --  end-points if this reduces the size.

         --  Note that in the Incl case, all we care about is including the
         --  end-points. In the Excl case, we want to narrow the bounds as
         --  much as permitted by the RM, to give the smallest possible size.

         Fudge : declare
            Loval_Incl_EP : Ureal;
            Hival_Incl_EP : Ureal;

            Loval_Excl_EP : Ureal;
            Hival_Excl_EP : Ureal;

            Size_Incl_EP  : Nat;
            Size_Excl_EP  : Nat;

            Model_Num     : Ureal;
            First_Subt    : Entity_Id;
            Actual_Lo     : Ureal;
            Actual_Hi     : Ureal;

         begin
            --  First step. Base types are required to be symmetrical. Right
            --  now, the base type range is a copy of the first subtype range.
            --  This will be corrected before we are done, but right away we
            --  need to deal with the case where both bounds are non-negative.
            --  In this case, we set the low bound to the negative of the high
            --  bound, to make sure that the size is computed to include the
            --  required sign. Note that we do not need to worry about the
            --  case of both bounds negative, because the sign will be dealt
            --  with anyway. Furthermore we can't just go making such a bound
            --  symmetrical, since in a twos-complement system, there is an
            --  extra negative value which could not be accommodated on the
            --  positive side.

            if Typ = Btyp
              and then not UR_Is_Negative (Loval)
              and then Hival > Loval
            then
               Loval := -Hival;
               Set_Realval (Lo, Loval);
            end if;

            --  Compute the fudged bounds. If the number is a model number,
            --  then we do nothing to include it, but we are allowed to backoff
            --  to the next adjacent model number when we exclude it. If it is
            --  not a model number then we straddle the two values with the
            --  model numbers on either side.

            Model_Num := UR_Trunc (Loval / Small) * Small;

            if Loval = Model_Num then
               Loval_Incl_EP := Model_Num;
            else
               Loval_Incl_EP := Model_Num - Small;
            end if;

            --  The low value excluding the end point is Small greater, but
            --  we do not do this exclusion if the low value is positive,
            --  since it can't help the size and could actually hurt by
            --  crossing the high bound.

            if UR_Is_Negative (Loval_Incl_EP) then
               Loval_Excl_EP := Loval_Incl_EP + Small;

               --  If the value went from negative to zero, then we have the
               --  case where Loval_Incl_EP is the model number just below
               --  zero, so we want to stick to the negative value for the
               --  base type to maintain the condition that the size will
               --  include signed values.

               if Typ = Btyp
                 and then UR_Is_Zero (Loval_Excl_EP)
               then
                  Loval_Excl_EP := Loval_Incl_EP;
               end if;

            else
               Loval_Excl_EP := Loval_Incl_EP;
            end if;

            --  Similar processing for upper bound and high value

            Model_Num := UR_Trunc (Hival / Small) * Small;

            if Hival = Model_Num then
               Hival_Incl_EP := Model_Num;
            else
               Hival_Incl_EP := Model_Num + Small;
            end if;

            if UR_Is_Positive (Hival_Incl_EP) then
               Hival_Excl_EP := Hival_Incl_EP - Small;
            else
               Hival_Excl_EP := Hival_Incl_EP;
            end if;

            --  One further adjustment is needed. In the case of subtypes, we
            --  cannot go outside the range of the base type, or we get
            --  peculiarities, and the base type range is already set. This
            --  only applies to the Incl values, since clearly the Excl values
            --  are already as restricted as they are allowed to be.

            if Typ /= Btyp then
               Loval_Incl_EP := UR_Max (Loval_Incl_EP, Realval (BLo));
               Hival_Incl_EP := UR_Min (Hival_Incl_EP, Realval (BHi));
            end if;

            --  Get size including and excluding end points

            Size_Incl_EP := Fsize (Loval_Incl_EP, Hival_Incl_EP);
            Size_Excl_EP := Fsize (Loval_Excl_EP, Hival_Excl_EP);

            --  No need to exclude end-points if it does not reduce size

            if Fsize (Loval_Incl_EP, Hival_Excl_EP) = Size_Excl_EP then
               Loval_Excl_EP := Loval_Incl_EP;
            end if;

            if Fsize (Loval_Excl_EP, Hival_Incl_EP) = Size_Excl_EP then
               Hival_Excl_EP := Hival_Incl_EP;
            end if;

            --  Now we set the actual size to be used. We want to use the
            --  bounds fudged up to include the end-points but only if this
            --  can be done without violating a specifically given size
            --  size clause or causing an unacceptable increase in size.

            --  Case of size clause given

            if Has_Size_Clause (Typ) then

               --  Use the inclusive size only if it is consistent with
               --  the explicitly specified size.

               if Size_Incl_EP <= RM_Size (Typ) then
                  Actual_Lo   := Loval_Incl_EP;
                  Actual_Hi   := Hival_Incl_EP;
                  Actual_Size := Size_Incl_EP;

               --  If the inclusive size is too large, we try excluding
               --  the end-points (will be caught later if does not work).

               else
                  Actual_Lo   := Loval_Excl_EP;
                  Actual_Hi   := Hival_Excl_EP;
                  Actual_Size := Size_Excl_EP;
               end if;

            --  Case of size clause not given

            else
               --  If we have a base type whose corresponding first subtype
               --  has an explicit size that is large enough to include our
               --  end-points, then do so. There is no point in working hard
               --  to get a base type whose size is smaller than the specified
               --  size of the first subtype.

               First_Subt := First_Subtype (Typ);

               if Has_Size_Clause (First_Subt)
                 and then Size_Incl_EP <= Esize (First_Subt)
               then
                  Actual_Size := Size_Incl_EP;
                  Actual_Lo   := Loval_Incl_EP;
                  Actual_Hi   := Hival_Incl_EP;

               --  If excluding the end-points makes the size smaller and
               --  results in a size of 8,16,32,64, then we take the smaller
               --  size. For the 64 case, this is compulsory. For the other
               --  cases, it seems reasonable. We like to include end points
               --  if we can, but not at the expense of moving to the next
               --  natural boundary of size.

               elsif Size_Incl_EP /= Size_Excl_EP
                 and then
                    (Size_Excl_EP = 8  or else
                     Size_Excl_EP = 16 or else
                     Size_Excl_EP = 32 or else
                     Size_Excl_EP = 64)
               then
                  Actual_Size := Size_Excl_EP;
                  Actual_Lo   := Loval_Excl_EP;
                  Actual_Hi   := Hival_Excl_EP;

               --  Otherwise we can definitely include the end points

               else
                  Actual_Size := Size_Incl_EP;
                  Actual_Lo   := Loval_Incl_EP;
                  Actual_Hi   := Hival_Incl_EP;
               end if;

               --  One pathological case: normally we never fudge a low bound
               --  down, since it would seem to increase the size (if it has
               --  any effect), but for ranges containing single value, or no
               --  values, the high bound can be small too large. Consider:

               --    type t is delta 2.0**(-14)
               --      range 131072.0 .. 0;

               --  That lower bound is *just* outside the range of 32 bits, and
               --  does need fudging down in this case. Note that the bounds
               --  will always have crossed here, since the high bound will be
               --  fudged down if necessary, as in the case of:

               --    type t is delta 2.0**(-14)
               --      range 131072.0 .. 131072.0;

               --  So we detect the situation by looking for crossed bounds,
               --  and if the bounds are crossed, and the low bound is greater
               --  than zero, we will always back it off by small, since this
               --  is completely harmless.

               if Actual_Lo > Actual_Hi then
                  if UR_Is_Positive (Actual_Lo) then
                     Actual_Lo   := Loval_Incl_EP - Small;
                     Actual_Size := Fsize (Actual_Lo, Actual_Hi);

                  --  And of course, we need to do exactly the same parallel
                  --  fudge for flat ranges in the negative region.

                  elsif UR_Is_Negative (Actual_Hi) then
                     Actual_Hi := Hival_Incl_EP + Small;
                     Actual_Size := Fsize (Actual_Lo, Actual_Hi);
                  end if;
               end if;
            end if;

            Set_Realval (Lo, Actual_Lo);
            Set_Realval (Hi, Actual_Hi);
         end Fudge;

      --  For the decimal case, none of this fudging is required, since there
      --  are no end-point problems in the decimal case (the end-points are
      --  always included).

      else
         Actual_Size := Fsize (Loval, Hival);
      end if;

      --  At this stage, the actual size has been calculated and the proper
      --  required bounds are stored in the low and high bounds.

      if Actual_Size > 64 then
         Error_Msg_Uint_1 := UI_From_Int (Actual_Size);
         Error_Msg_N
           ("size required (^) for type& too large, maximum allowed is 64",
            Typ);
         Actual_Size := 64;
      end if;

      --  Check size against explicit given size

      if Has_Size_Clause (Typ) then
         if Actual_Size > RM_Size (Typ) then
            Error_Msg_Uint_1 := RM_Size (Typ);
            Error_Msg_Uint_2 := UI_From_Int (Actual_Size);
            Error_Msg_NE
              ("size given (^) for type& too small, minimum allowed is ^",
               Size_Clause (Typ), Typ);

         else
            Actual_Size := UI_To_Int (Esize (Typ));
         end if;

      --  Increase size to next natural boundary if no size clause given

      else
         if Actual_Size <= 8 then
            Actual_Size := 8;
         elsif Actual_Size <= 16 then
            Actual_Size := 16;
         elsif Actual_Size <= 32 then
            Actual_Size := 32;
         else
            Actual_Size := 64;
         end if;

         Init_Esize (Typ, Actual_Size);
         Adjust_Esize_For_Alignment (Typ);
      end if;

      --  If we have a base type, then expand the bounds so that they extend to
      --  the full width of the allocated size in bits, to avoid junk range
      --  checks on intermediate computations.

      if Base_Type (Typ) = Typ then
         Set_Realval (Lo, -(Small * (Uint_2 ** (Actual_Size - 1))));
         Set_Realval (Hi,  (Small * (Uint_2 ** (Actual_Size - 1) - 1)));
      end if;

      --  Final step is to reanalyze the bounds using the proper type
      --  and set the Corresponding_Integer_Value fields of the literals.

      Set_Etype (Lo, Empty);
      Set_Analyzed (Lo, False);
      Analyze (Lo);

      --  Resolve with universal fixed if the base type, and the base type if
      --  it is a subtype. Note we can't resolve the base type with itself,
      --  that would be a reference before definition.

      if Typ = Btyp then
         Resolve (Lo, Universal_Fixed);
      else
         Resolve (Lo, Btyp);
      end if;

      --  Set corresponding integer value for bound

      Set_Corresponding_Integer_Value
        (Lo, UR_To_Uint (Realval (Lo) / Small));

      --  Similar processing for high bound

      Set_Etype (Hi, Empty);
      Set_Analyzed (Hi, False);
      Analyze (Hi);

      if Typ = Btyp then
         Resolve (Hi, Universal_Fixed);
      else
         Resolve (Hi, Btyp);
      end if;

      Set_Corresponding_Integer_Value
        (Hi, UR_To_Uint (Realval (Hi) / Small));

      --  Set type of range to correspond to bounds

      Set_Etype (Rng, Etype (Lo));

      --  Set Esize to calculated size if not set already

      if Unknown_Esize (Typ) then
         Init_Esize (Typ, Actual_Size);
      end if;

      --  Set RM_Size if not already set. If already set, check value

      declare
         Minsiz : constant Uint := UI_From_Int (Minimum_Size (Typ));

      begin
         if RM_Size (Typ) /= Uint_0 then
            if RM_Size (Typ) < Minsiz then
               Error_Msg_Uint_1 := RM_Size (Typ);
               Error_Msg_Uint_2 := Minsiz;
               Error_Msg_NE
                 ("size given (^) for type& too small, minimum allowed is ^",
                  Size_Clause (Typ), Typ);
            end if;

         else
            Set_RM_Size (Typ, Minsiz);
         end if;
      end;
   end Freeze_Fixed_Point_Type;

   ------------------
   -- Freeze_Itype --
   ------------------

   procedure Freeze_Itype (T : Entity_Id; N : Node_Id) is
      L : List_Id;

   begin
      Set_Has_Delayed_Freeze (T);
      L := Freeze_Entity (T, Sloc (N));

      if Is_Non_Empty_List (L) then
         Insert_Actions (N, L);
      end if;
   end Freeze_Itype;

   --------------------------
   -- Freeze_Static_Object --
   --------------------------

   procedure Freeze_Static_Object (E : Entity_Id) is

      Cannot_Be_Static : exception;
      --  Exception raised if the type of a static object cannot be made
      --  static. This happens if the type depends on non-global objects.

      procedure Ensure_Expression_Is_SA (N : Node_Id);
      --  Called to ensure that an expression used as part of a type definition
      --  is statically allocatable, which means that the expression type is
      --  statically allocatable, and the expression is either static, or a
      --  reference to a library level constant.

      procedure Ensure_Type_Is_SA (Typ : Entity_Id);
      --  Called to mark a type as static, checking that it is possible
      --  to set the type as static. If it is not possible, then the
      --  exception Cannot_Be_Static is raised.

      -----------------------------
      -- Ensure_Expression_Is_SA --
      -----------------------------

      procedure Ensure_Expression_Is_SA (N : Node_Id) is
         Ent : Entity_Id;

      begin
         Ensure_Type_Is_SA (Etype (N));

         if Is_Static_Expression (N) then
            return;

         elsif Nkind (N) = N_Identifier then
            Ent := Entity (N);

            if Present (Ent)
              and then Ekind (Ent) = E_Constant
              and then Is_Library_Level_Entity (Ent)
            then
               return;
            end if;
         end if;

         raise Cannot_Be_Static;
      end Ensure_Expression_Is_SA;

      -----------------------
      -- Ensure_Type_Is_SA --
      -----------------------

      procedure Ensure_Type_Is_SA (Typ : Entity_Id) is
         N : Node_Id;
         C : Entity_Id;

      begin
         --  If type is library level, we are all set

         if Is_Library_Level_Entity (Typ) then
            return;
         end if;

         --  We are also OK if the type already marked as statically allocated,
         --  which means we processed it before.

         if Is_Statically_Allocated (Typ) then
            return;
         end if;

         --  Mark type as statically allocated

         Set_Is_Statically_Allocated (Typ);

         --  Check that it is safe to statically allocate this type

         if Is_Scalar_Type (Typ) or else Is_Real_Type (Typ) then
            Ensure_Expression_Is_SA (Type_Low_Bound (Typ));
            Ensure_Expression_Is_SA (Type_High_Bound (Typ));

         elsif Is_Array_Type (Typ) then
            N := First_Index (Typ);
            while Present (N) loop
               Ensure_Type_Is_SA (Etype (N));
               Next_Index (N);
            end loop;

            Ensure_Type_Is_SA (Component_Type (Typ));

         elsif Is_Access_Type (Typ) then
            if Ekind (Designated_Type (Typ)) = E_Subprogram_Type then

               declare
                  F : Entity_Id;
                  T : constant Entity_Id := Etype (Designated_Type (Typ));

               begin
                  if T /= Standard_Void_Type then
                     Ensure_Type_Is_SA (T);
                  end if;

                  F := First_Formal (Designated_Type (Typ));

                  while Present (F) loop
                     Ensure_Type_Is_SA (Etype (F));
                     Next_Formal (F);
                  end loop;
               end;

            else
               Ensure_Type_Is_SA (Designated_Type (Typ));
            end if;

         elsif Is_Record_Type (Typ) then
            C := First_Entity (Typ);
            while Present (C) loop
               if Ekind (C) = E_Discriminant
                 or else Ekind (C) = E_Component
               then
                  Ensure_Type_Is_SA (Etype (C));

               elsif Is_Type (C) then
                  Ensure_Type_Is_SA (C);
               end if;

               Next_Entity (C);
            end loop;

         elsif Ekind (Typ) = E_Subprogram_Type then
            Ensure_Type_Is_SA (Etype (Typ));

            C := First_Formal (Typ);
            while Present (C) loop
               Ensure_Type_Is_SA (Etype (C));
               Next_Formal (C);
            end loop;

         else
            raise Cannot_Be_Static;
         end if;
      end Ensure_Type_Is_SA;

   --  Start of processing for Freeze_Static_Object

   begin
      Ensure_Type_Is_SA (Etype (E));

   exception
      when Cannot_Be_Static =>

         --  If the object that cannot be static is imported or exported, then
         --  issue an error message saying that this object cannot be imported
         --  or exported. If it has an address clause it is an overlay in the
         --  current partition and the static requirement is not relevant.

         if Is_Imported (E) and then No (Address_Clause (E)) then
            Error_Msg_N
              ("& cannot be imported (local type is not constant)", E);

         --  Otherwise must be exported, something is wrong if compiler
         --  is marking something as statically allocated which cannot be).

         else pragma Assert (Is_Exported (E));
            Error_Msg_N
              ("& cannot be exported (local type is not constant)", E);
         end if;
   end Freeze_Static_Object;

   -----------------------
   -- Freeze_Subprogram --
   -----------------------

   procedure Freeze_Subprogram (E : Entity_Id) is
      Retype : Entity_Id;
      F      : Entity_Id;

   begin
      --  Subprogram may not have an address clause unless it is imported

      if Present (Address_Clause (E)) then
         if not Is_Imported (E) then
            Error_Msg_N
              ("address clause can only be given " &
               "for imported subprogram",
               Name (Address_Clause (E)));
         end if;
      end if;

      --  Reset the Pure indication on an imported subprogram unless an
      --  explicit Pure_Function pragma was present. We do this because
      --  otherwise it is an insidious error to call a non-pure function from
      --  pure unit and have calls mysteriously optimized away. What happens
      --  here is that the Import can bypass the normal check to ensure that
      --  pure units call only pure subprograms.

      if Is_Imported (E)
        and then Is_Pure (E)
        and then not Has_Pragma_Pure_Function (E)
      then
         Set_Is_Pure (E, False);
      end if;

      --  For non-foreign convention subprograms, this is where we create
      --  the extra formals (for accessibility level and constrained bit
      --  information). We delay this till the freeze point precisely so
      --  that we know the convention!

      if not Has_Foreign_Convention (E) then
         Create_Extra_Formals (E);
         Set_Mechanisms (E);

         --  If this is convention Ada and a Valued_Procedure, that's odd

         if Ekind (E) = E_Procedure
           and then Is_Valued_Procedure (E)
           and then Convention (E) = Convention_Ada
           and then Warn_On_Export_Import
         then
            Error_Msg_N
              ("?Valued_Procedure has no effect for convention Ada", E);
            Set_Is_Valued_Procedure (E, False);
         end if;

      --  Case of foreign convention

      else
         Set_Mechanisms (E);

         --  For foreign conventions, warn about return of an
         --  unconstrained array.

         --  Note: we *do* allow a return by descriptor for the VMS case,
         --  though here there is probably more to be done ???

         if Ekind (E) = E_Function then
            Retype := Underlying_Type (Etype (E));

            --  If no return type, probably some other error, e.g. a
            --  missing full declaration, so ignore.

            if No (Retype) then
               null;

            --  If the return type is generic, we have emitted a warning
            --  earlier on, and there is nothing else to check here. Specific
            --  instantiations may lead to erroneous behavior.

            elsif Is_Generic_Type (Etype (E)) then
               null;

            --  Display warning if returning unconstrained array

            elsif Is_Array_Type (Retype)
              and then not Is_Constrained (Retype)

              --  Exclude cases where descriptor mechanism is set, since the
              --  VMS descriptor mechanisms allow such unconstrained returns.

              and then Mechanism (E) not in Descriptor_Codes

              --  Check appropriate warning is enabled (should we check for
              --  Warnings (Off) on specific entities here, probably so???)

              and then Warn_On_Export_Import

               --  Exclude the VM case, since return of unconstrained arrays
               --  is properly handled in both the JVM and .NET cases.

              and then VM_Target = No_VM
            then
               Error_Msg_N
                ("?foreign convention function& should not return " &
                  "unconstrained array", E);
               return;
            end if;
         end if;

         --  If any of the formals for an exported foreign convention
         --  subprogram have defaults, then emit an appropriate warning since
         --  this is odd (default cannot be used from non-Ada code)

         if Is_Exported (E) then
            F := First_Formal (E);
            while Present (F) loop
               if Warn_On_Export_Import
                 and then Present (Default_Value (F))
               then
                  Error_Msg_N
                    ("?parameter cannot be defaulted in non-Ada call",
                     Default_Value (F));
               end if;

               Next_Formal (F);
            end loop;
         end if;
      end if;

      --  For VMS, descriptor mechanisms for parameters are allowed only for
      --  imported/exported subprograms. Moreover, the NCA descriptor is not
      --  allowed for parameters of exported subprograms.

      if OpenVMS_On_Target then
         if Is_Exported (E) then
            F := First_Formal (E);
            while Present (F) loop
               if Mechanism (F) = By_Descriptor_NCA then
                  Error_Msg_N
                    ("'N'C'A' descriptor for parameter not permitted", F);
                  Error_Msg_N
                    ("\can only be used for imported subprogram", F);
               end if;

               Next_Formal (F);
            end loop;

         elsif not Is_Imported (E) then
            F := First_Formal (E);
            while Present (F) loop
               if Mechanism (F) in Descriptor_Codes then
                  Error_Msg_N
                    ("descriptor mechanism for parameter not permitted", F);
                  Error_Msg_N
                    ("\can only be used for imported/exported subprogram", F);
               end if;

               Next_Formal (F);
            end loop;
         end if;
      end if;

      --  Pragma Inline_Always is disallowed for dispatching subprograms
      --  because the address of such subprograms is saved in the dispatch
      --  table to support dispatching calls, and dispatching calls cannot
      --  be inlined. This is consistent with the restriction against using
      --  'Access or 'Address on an Inline_Always subprogram.

      if Is_Dispatching_Operation (E)
        and then Has_Pragma_Inline_Always (E)
      then
         Error_Msg_N
           ("pragma Inline_Always not allowed for dispatching subprograms", E);
      end if;

      --  Because of the implicit representation of inherited predefined
      --  operators in the front-end, the overriding status of the operation
      --  may be affected when a full view of a type is analyzed, and this is
      --  not captured by the analysis of the corresponding type declaration.
      --  Therefore the correctness of a not-overriding indicator must be
      --  rechecked when the subprogram is frozen.

      if Nkind (E) = N_Defining_Operator_Symbol
        and then not Error_Posted (Parent (E))
      then
         Check_Overriding_Indicator (E, Empty, Is_Primitive (E));
      end if;
   end Freeze_Subprogram;

   ----------------------
   -- Is_Fully_Defined --
   ----------------------

   function Is_Fully_Defined (T : Entity_Id) return Boolean is
   begin
      if Ekind (T) = E_Class_Wide_Type then
         return Is_Fully_Defined (Etype (T));

      elsif Is_Array_Type (T) then
         return Is_Fully_Defined (Component_Type (T));

      elsif Is_Record_Type (T)
        and not Is_Private_Type (T)
      then
         --  Verify that the record type has no components with private types
         --  without completion.

         declare
            Comp : Entity_Id;

         begin
            Comp := First_Component (T);

            while Present (Comp) loop
               if not Is_Fully_Defined (Etype (Comp)) then
                  return False;
               end if;

               Next_Component (Comp);
            end loop;
            return True;
         end;

      else
         return not Is_Private_Type (T)
           or else Present (Full_View (Base_Type (T)));
      end if;
   end Is_Fully_Defined;

   ---------------------------------
   -- Process_Default_Expressions --
   ---------------------------------

   procedure Process_Default_Expressions
     (E     : Entity_Id;
      After : in out Node_Id)
   is
      Loc    : constant Source_Ptr := Sloc (E);
      Dbody  : Node_Id;
      Formal : Node_Id;
      Dcopy  : Node_Id;
      Dnam   : Entity_Id;

   begin
      Set_Default_Expressions_Processed (E);

      --  A subprogram instance and its associated anonymous subprogram share
      --  their signature. The default expression functions are defined in the
      --  wrapper packages for the anonymous subprogram, and should not be
      --  generated again for the instance.

      if Is_Generic_Instance (E)
        and then Present (Alias (E))
        and then Default_Expressions_Processed (Alias (E))
      then
         return;
      end if;

      Formal := First_Formal (E);
      while Present (Formal) loop
         if Present (Default_Value (Formal)) then

            --  We work with a copy of the default expression because we
            --  do not want to disturb the original, since this would mess
            --  up the conformance checking.

            Dcopy := New_Copy_Tree (Default_Value (Formal));

            --  The analysis of the expression may generate insert actions,
            --  which of course must not be executed. We wrap those actions
            --  in a procedure that is not called, and later on eliminated.
            --  The following cases have no side-effects, and are analyzed
            --  directly.

            if Nkind (Dcopy) = N_Identifier
              or else Nkind (Dcopy) = N_Expanded_Name
              or else Nkind (Dcopy) = N_Integer_Literal
              or else (Nkind (Dcopy) = N_Real_Literal
                        and then not Vax_Float (Etype (Dcopy)))
              or else Nkind (Dcopy) = N_Character_Literal
              or else Nkind (Dcopy) = N_String_Literal
              or else Known_Null (Dcopy)
              or else (Nkind (Dcopy) = N_Attribute_Reference
                        and then
                       Attribute_Name (Dcopy) = Name_Null_Parameter)
            then

               --  If there is no default function, we must still do a full
               --  analyze call on the default value, to ensure that all error
               --  checks are performed, e.g. those associated with static
               --  evaluation. Note: this branch will always be taken if the
               --  analyzer is turned off (but we still need the error checks).

               --  Note: the setting of parent here is to meet the requirement
               --  that we can only analyze the expression while attached to
               --  the tree. Really the requirement is that the parent chain
               --  be set, we don't actually need to be in the tree.

               Set_Parent (Dcopy, Declaration_Node (Formal));
               Analyze (Dcopy);

               --  Default expressions are resolved with their own type if the
               --  context is generic, to avoid anomalies with private types.

               if Ekind (Scope (E)) = E_Generic_Package then
                  Resolve (Dcopy);
               else
                  Resolve (Dcopy, Etype (Formal));
               end if;

               --  If that resolved expression will raise constraint error,
               --  then flag the default value as raising constraint error.
               --  This allows a proper error message on the calls.

               if Raises_Constraint_Error (Dcopy) then
                  Set_Raises_Constraint_Error (Default_Value (Formal));
               end if;

            --  If the default is a parameterless call, we use the name of
            --  the called function directly, and there is no body to build.

            elsif Nkind (Dcopy) = N_Function_Call
              and then No (Parameter_Associations (Dcopy))
            then
               null;

            --  Else construct and analyze the body of a wrapper procedure
            --  that contains an object declaration to hold the expression.
            --  Given that this is done only to complete the analysis, it
            --  simpler to build a procedure than a function which might
            --  involve secondary stack expansion.

            else
               Dnam :=
                 Make_Defining_Identifier (Loc, New_Internal_Name ('D'));

               Dbody :=
                 Make_Subprogram_Body (Loc,
                   Specification =>
                     Make_Procedure_Specification (Loc,
                       Defining_Unit_Name => Dnam),

                   Declarations => New_List (
                     Make_Object_Declaration (Loc,
                       Defining_Identifier =>
                         Make_Defining_Identifier (Loc,
                           New_Internal_Name ('T')),
                         Object_Definition =>
                           New_Occurrence_Of (Etype (Formal), Loc),
                         Expression => New_Copy_Tree (Dcopy))),

                   Handled_Statement_Sequence =>
                     Make_Handled_Sequence_Of_Statements (Loc,
                       Statements => New_List));

               Set_Scope (Dnam, Scope (E));
               Set_Assignment_OK (First (Declarations (Dbody)));
               Set_Is_Eliminated (Dnam);
               Insert_After (After, Dbody);
               Analyze (Dbody);
               After := Dbody;
            end if;
         end if;

         Next_Formal (Formal);
      end loop;
   end Process_Default_Expressions;

   ----------------------------------------
   -- Set_Component_Alignment_If_Not_Set --
   ----------------------------------------

   procedure Set_Component_Alignment_If_Not_Set (Typ : Entity_Id) is
   begin
      --  Ignore if not base type, subtypes don't need anything

      if Typ /= Base_Type (Typ) then
         return;
      end if;

      --  Do not override existing representation

      if Is_Packed (Typ) then
         return;

      elsif Has_Specified_Layout (Typ) then
         return;

      elsif Component_Alignment (Typ) /= Calign_Default then
         return;

      else
         Set_Component_Alignment
           (Typ, Scope_Stack.Table
                  (Scope_Stack.Last).Component_Alignment_Default);
      end if;
   end Set_Component_Alignment_If_Not_Set;

   ------------------
   -- Undelay_Type --
   ------------------

   procedure Undelay_Type (T : Entity_Id) is
   begin
      Set_Has_Delayed_Freeze (T, False);
      Set_Freeze_Node (T, Empty);

      --  Since we don't want T to have a Freeze_Node, we don't want its
      --  Full_View or Corresponding_Record_Type to have one either.

      --  ??? Fundamentally, this whole handling is a kludge. What we really
      --  want is to be sure that for an Itype that's part of record R and is a
      --  subtype of type T, that it's frozen after the later of the freeze
      --  points of R and T. We have no way of doing that directly, so what we
      --  do is force most such Itypes to be frozen as part of freezing R via
      --  this procedure and only delay the ones that need to be delayed
      --  (mostly the designated types of access types that are defined as part
      --  of the record).

      if Is_Private_Type (T)
        and then Present (Full_View (T))
        and then Is_Itype (Full_View (T))
        and then Is_Record_Type (Scope (Full_View (T)))
      then
         Undelay_Type (Full_View (T));
      end if;

      if Is_Concurrent_Type (T)
        and then Present (Corresponding_Record_Type (T))
        and then Is_Itype (Corresponding_Record_Type (T))
        and then Is_Record_Type (Scope (Corresponding_Record_Type (T)))
      then
         Undelay_Type (Corresponding_Record_Type (T));
      end if;
   end Undelay_Type;

   ------------------
   -- Warn_Overlay --
   ------------------

   procedure Warn_Overlay
     (Expr : Node_Id;
      Typ  : Entity_Id;
      Nam  : Entity_Id)
   is
      Ent : constant Entity_Id := Entity (Nam);
      --  The object to which the address clause applies

      Init : Node_Id;
      Old  : Entity_Id := Empty;
      Decl : Node_Id;

   begin
      --  No warning if address clause overlay warnings are off

      if not Address_Clause_Overlay_Warnings then
         return;
      end if;

      --  No warning if there is an explicit initialization

      Init := Original_Node (Expression (Declaration_Node (Ent)));

      if Present (Init) and then Comes_From_Source (Init) then
         return;
      end if;

      --  We only give the warning for non-imported entities of a type for
      --  which a non-null base init proc is defined, or for objects of access
      --  types with implicit null initialization, or when Initialize_Scalars
      --  applies and the type is scalar or a string type (the latter being
      --  tested for because predefined String types are initialized by inline
      --  code rather than by an init_proc).

      if Present (Expr)
        and then not Is_Imported (Ent)
        and then (Has_Non_Null_Base_Init_Proc (Typ)
                    or else Is_Access_Type (Typ)
                    or else (Init_Or_Norm_Scalars
                              and then (Is_Scalar_Type (Typ)
                                         or else Is_String_Type (Typ))))
      then
         if Nkind (Expr) = N_Attribute_Reference
           and then Is_Entity_Name (Prefix (Expr))
         then
            Old := Entity (Prefix (Expr));

         elsif Is_Entity_Name (Expr)
           and then Ekind (Entity (Expr)) = E_Constant
         then
            Decl := Declaration_Node (Entity (Expr));

            if Nkind (Decl) = N_Object_Declaration
              and then Present (Expression (Decl))
              and then Nkind (Expression (Decl)) = N_Attribute_Reference
              and then Is_Entity_Name (Prefix (Expression (Decl)))
            then
               Old := Entity (Prefix (Expression (Decl)));

            elsif Nkind (Expr) = N_Function_Call then
               return;
            end if;

         --  A function call (most likely to To_Address) is probably not an
         --  overlay, so skip warning. Ditto if the function call was inlined
         --  and transformed into an entity.

         elsif Nkind (Original_Node (Expr)) = N_Function_Call then
            return;
         end if;

         Decl := Next (Parent (Expr));

         --  If a pragma Import follows, we assume that it is for the current
         --  target of the address clause, and skip the warning.

         if Present (Decl)
           and then Nkind (Decl) = N_Pragma
           and then Pragma_Name (Decl) = Name_Import
         then
            return;
         end if;

         if Present (Old) then
            Error_Msg_Node_2 := Old;
            Error_Msg_N
              ("default initialization of & may modify &?",
               Nam);
         else
            Error_Msg_N
              ("default initialization of & may modify overlaid storage?",
               Nam);
         end if;

         --  Add friendly warning if initialization comes from a packed array
         --  component.

         if Is_Record_Type (Typ)  then
            declare
               Comp : Entity_Id;

            begin
               Comp := First_Component (Typ);

               while Present (Comp) loop
                  if Nkind (Parent (Comp)) = N_Component_Declaration
                    and then Present (Expression (Parent (Comp)))
                  then
                     exit;
                  elsif Is_Array_Type (Etype (Comp))
                     and then Present (Packed_Array_Type (Etype (Comp)))
                  then
                     Error_Msg_NE
                       ("\packed array component& " &
                        "will be initialized to zero?",
                        Nam, Comp);
                     exit;
                  else
                     Next_Component (Comp);
                  end if;
               end loop;
            end;
         end if;

         Error_Msg_N
           ("\use pragma Import for & to " &
            "suppress initialization (RM B.1(24))?",
            Nam);
      end if;
   end Warn_Overlay;

end Freeze;
