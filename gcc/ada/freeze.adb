------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               F R E E Z E                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2016, Free Software Foundation, Inc.         --
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

with Aspects;  use Aspects;
with Atree;    use Atree;
with Checks;   use Checks;
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
with Fname;    use Fname;
with Ghost;    use Ghost;
with Layout;   use Layout;
with Lib;      use Lib;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Restrict; use Restrict;
with Rident;   use Rident;
with Rtsfind;  use Rtsfind;
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
with Warnsw;   use Warnsw;

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
   --  at the point the object is frozen. Also ensure any initialization is
   --  performed only after the object has been frozen.

   procedure Check_Component_Storage_Order
     (Encl_Type        : Entity_Id;
      Comp             : Entity_Id;
      ADC              : Node_Id;
      Comp_ADC_Present : out Boolean);
   --  For an Encl_Type that has a Scalar_Storage_Order attribute definition
   --  clause, verify that the component type has an explicit and compatible
   --  attribute/aspect. For arrays, Comp is Empty; for records, it is the
   --  entity of the component under consideration. For an Encl_Type that
   --  does not have a Scalar_Storage_Order attribute definition clause,
   --  verify that the component also does not have such a clause.
   --  ADC is the attribute definition clause if present (or Empty). On return,
   --  Comp_ADC_Present is set True if the component has a Scalar_Storage_Order
   --  attribute definition clause.

   procedure Check_Expression_Function (N : Node_Id; Nam : Entity_Id);
   --  When an expression function is frozen by a use of it, the expression
   --  itself is frozen. Check that the expression does not include references
   --  to deferred constants without completion. We report this at the freeze
   --  point of the function, to provide a better error message.
   --
   --  In most cases the expression itself is frozen by the time the function
   --  itself is frozen, because the formals will be frozen by then. However,
   --  Attribute references to outer types are freeze points for those types;
   --  this routine generates the required freeze nodes for them.

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
      N      : Node_Id;
      Result : in out List_Id);
   --  Freezes Ent using Freeze_Entity, and appends the resulting list of
   --  nodes to Result, modifying Result from No_List if necessary. N has
   --  the same usage as in Freeze_Entity.

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
   --  This procedure is called for each subprogram to complete processing of
   --  default expressions at the point where all types are known to be frozen.
   --  The expressions must be analyzed in full, to make sure that all error
   --  processing is done (they have only been pre-analyzed). If the expression
   --  is not an entity or literal, its analysis may generate code which must
   --  not be executed. In that case we build a function body to hold that
   --  code. This wrapper function serves no other purpose (it used to be
   --  called to evaluate the default, but now the default is inlined at each
   --  point of call).

   procedure Set_Component_Alignment_If_Not_Set (Typ : Entity_Id);
   --  Typ is a record or array type that is being frozen. This routine sets
   --  the default component alignment from the scope stack values if the
   --  alignment is otherwise not specified.

   procedure Check_Debug_Info_Needed (T : Entity_Id);
   --  As each entity is frozen, this routine is called to deal with the
   --  setting of Debug_Info_Needed for the entity. This flag is set if
   --  the entity comes from source, or if we are in Debug_Generated_Code
   --  mode or if the -gnatdV debug flag is set. However, it never sets
   --  the flag if Debug_Info_Off is set. This procedure also ensures that
   --  subsidiary entities have the flag set as required.

   procedure Set_SSO_From_Default (T : Entity_Id);
   --  T is a record or array type that is being frozen. If it is a base type,
   --  and if SSO_Set_Low/High_By_Default is set, then Reverse_Storage order
   --  will be set appropriately. Note that an explicit occurrence of aspect
   --  Scalar_Storage_Order or an explicit setting of this aspect with an
   --  attribute definition clause occurs, then these two flags are reset in
   --  any case, so call will have no effect.

   procedure Undelay_Type (T : Entity_Id);
   --  T is a type of a component that we know to be an Itype. We don't want
   --  this to have a Freeze_Node, so ensure it doesn't. Do the same for any
   --  Full_View or Corresponding_Record_Type.

   procedure Warn_Overlay (Expr : Node_Id; Typ : Entity_Id; Nam : Node_Id);
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
      Body_Decl    : constant Node_Id := Unit_Declaration_Node (New_S);
      Ent          : constant Entity_Id := Defining_Entity (Decl);
      Body_Node    : Node_Id;
      Renamed_Subp : Entity_Id;

   begin
      --  If the renamed subprogram is intrinsic, there is no need for a
      --  wrapper body: we set the alias that will be called and expanded which
      --  completes the declaration. This transformation is only legal if the
      --  renamed entity has already been elaborated.

      --  Note that it is legal for a renaming_as_body to rename an intrinsic
      --  subprogram, as long as the renaming occurs before the new entity
      --  is frozen (RM 8.5.4 (5)).

      if Nkind (Body_Decl) = N_Subprogram_Renaming_Declaration
        and then Is_Entity_Name (Name (Body_Decl))
      then
         Renamed_Subp := Entity (Name (Body_Decl));
      else
         Renamed_Subp := Empty;
      end if;

      if Present (Renamed_Subp)
        and then Is_Intrinsic_Subprogram (Renamed_Subp)
        and then
          (not In_Same_Source_Unit (Renamed_Subp, Ent)
            or else Sloc (Renamed_Subp) < Sloc (Ent))

        --  We can make the renaming entity intrinsic if the renamed function
        --  has an interface name, or if it is one of the shift/rotate
        --  operations known to the compiler.

        and then
          (Present (Interface_Name (Renamed_Subp))
            or else Nam_In (Chars (Renamed_Subp), Name_Rotate_Left,
                                                  Name_Rotate_Right,
                                                  Name_Shift_Left,
                                                  Name_Shift_Right,
                                                  Name_Shift_Right_Arithmetic))
      then
         Set_Interface_Name (Ent, Interface_Name (Renamed_Subp));

         if Present (Alias (Renamed_Subp)) then
            Set_Alias (Ent, Alias (Renamed_Subp));
         else
            Set_Alias (Ent, Renamed_Subp);
         end if;

         Set_Is_Intrinsic_Subprogram (Ent);
         Set_Has_Completion (Ent);

      else
         Body_Node := Build_Renamed_Body (Decl, New_S);
         Insert_After (After, Body_Node);
         Mark_Rewrite_Insertion (Body_Node);
         Analyze (Body_Node);
         After := Body_Node;
      end if;
   end Build_And_Analyze_Renamed_Body;

   ------------------------
   -- Build_Renamed_Body --
   ------------------------

   function Build_Renamed_Body
     (Decl  : Node_Id;
      New_S : Entity_Id) return Node_Id
   is
      Loc : constant Source_Ptr := Sloc (New_S);
      --  We use for the source location of the renamed body, the location of
      --  the spec entity. It might seem more natural to use the location of
      --  the renaming declaration itself, but that would be wrong, since then
      --  the body we create would look as though it was created far too late,
      --  and this could cause problems with elaboration order analysis,
      --  particularly in connection with instantiations.

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
            Call_Name := New_Occurrence_Of (Old_S, Loc);
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

         --  Original name may have been overloaded, but is fully resolved now

         Set_Is_Overloaded (Call_Name, False);
      end if;

      --  For simple renamings, subsequent calls can be expanded directly as
      --  calls to the renamed entity. The body must be generated in any case
      --  for calls that may appear elsewhere. This is not done in the case
      --  where the subprogram is an instantiation because the actual proper
      --  body has not been built yet.

      if Ekind_In (Old_S, E_Function, E_Procedure)
        and then Nkind (Decl) = N_Subprogram_Declaration
        and then not Is_Generic_Instance (Old_S)
      then
         Set_Body_To_Inline (Decl, Old_S);
      end if;

      --  Check whether the return type is a limited view. If the subprogram
      --  is already frozen the generated body may have a non-limited view
      --  of the type, that must be used, because it is the one in the spec
      --  of the renaming declaration.

      if Ekind (Old_S) = E_Function
        and then Is_Entity_Name (Result_Definition (Spec))
      then
         declare
            Ret_Type : constant Entity_Id := Etype (Result_Definition (Spec));
         begin
            if Has_Non_Limited_View (Ret_Type) then
               Set_Result_Definition
                 (Spec, New_Occurrence_Of (Non_Limited_View (Ret_Type), Loc));
            end if;
         end;
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
               Actuals :=
                 New_List (
                   Make_Attribute_Reference (Loc,
                     Attribute_Name => Name_Access,
                     Prefix         => Relocate_Node (Pref)));
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
            Append (New_Occurrence_Of (Formal, Loc), Actuals);
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
                  Name                   => Call_Name,
                  Parameter_Associations => Actuals));

      elsif Ekind (Old_S) = E_Enumeration_Literal then
         Call_Node :=
           Make_Simple_Return_Statement (Loc,
              Expression => New_Occurrence_Of (Old_S, Loc));

      elsif Nkind (Nam) = N_Character_Literal then
         Call_Node :=
           Make_Simple_Return_Statement (Loc, Expression => Call_Name);

      else
         Call_Node :=
           Make_Procedure_Call_Statement (Loc,
             Name                   => Call_Name,
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

      if Nkind (N) = N_Subprogram_Renaming_Declaration
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
      Addr       : constant Node_Id   := Address_Clause (E);
      Typ        : constant Entity_Id := Etype (E);
      Decl       : Node_Id;
      Expr       : Node_Id;
      Init       : Node_Id;
      Lhs        : Node_Id;
      Tag_Assign : Node_Id;

   begin
      if Present (Addr) then

         --  For a deferred constant, the initialization value is on full view

         if Ekind (E) = E_Constant and then Present (Full_View (E)) then
            Decl := Declaration_Node (Full_View (E));
         else
            Decl := Declaration_Node (E);
         end if;

         Expr := Expression (Addr);

         if Needs_Constant_Address (Decl, Typ) then
            Check_Constant_Address_Clause (Expr, E);

            --  Has_Delayed_Freeze was set on E when the address clause was
            --  analyzed, and must remain set because we want the address
            --  clause to be elaborated only after any entity it references
            --  has been elaborated.
         end if;

         --  If Rep_Clauses are to be ignored, remove address clause from
         --  list attached to entity, because it may be illegal for gigi,
         --  for example by breaking order of elaboration..

         if Ignore_Rep_Clauses then
            declare
               Rep : Node_Id;

            begin
               Rep := First_Rep_Item (E);

               if Rep = Addr then
                  Set_First_Rep_Item (E, Next_Rep_Item (Addr));

               else
                  while Present (Rep)
                    and then Next_Rep_Item (Rep) /= Addr
                  loop
                     Rep := Next_Rep_Item (Rep);
                  end loop;
               end if;

               if Present (Rep) then
                  Set_Next_Rep_Item (Rep, Next_Rep_Item (Addr));
               end if;
            end;

            --  And now remove the address clause

            Kill_Rep_Clause (Addr);

         elsif not Error_Posted (Expr)
           and then not Needs_Finalization (Typ)
         then
            Warn_Overlay (Expr, Typ, Name (Addr));
         end if;

         Init := Expression (Decl);

         --  If a variable, or a non-imported constant, overlays a constant
         --  object and has an initialization value, then the initialization
         --  may end up writing into read-only memory. Detect the cases of
         --  statically identical values and remove the initialization. In
         --  the other cases, give a warning. We will give other warnings
         --  later for the variable if it is assigned.

         if (Ekind (E) = E_Variable
              or else (Ekind (E) = E_Constant
                        and then not Is_Imported (E)))
           and then Overlays_Constant (E)
           and then Present (Init)
         then
            declare
               O_Ent : Entity_Id;
               Off   : Boolean;

            begin
               Find_Overlaid_Entity (Addr, O_Ent, Off);

               if Ekind (O_Ent) = E_Constant
                 and then Etype (O_Ent) = Typ
                 and then Present (Constant_Value (O_Ent))
                 and then Compile_Time_Compare
                            (Init,
                             Constant_Value (O_Ent),
                             Assume_Valid => True) = EQ
               then
                  Set_No_Initialization (Decl);
                  return;

               elsif Comes_From_Source (Init)
                 and then Address_Clause_Overlay_Warnings
               then
                  Error_Msg_Sloc := Sloc (Addr);
                  Error_Msg_NE
                    ("??constant& may be modified via address clause#",
                     Decl, O_Ent);
               end if;
            end;
         end if;

         if Present (Init) then

            --  Capture initialization value at point of declaration,
            --  and make explicit assignment legal, because object may
            --  be a constant.

            Remove_Side_Effects (Init);
            Lhs := New_Occurrence_Of (E, Sloc (Decl));
            Set_Assignment_OK (Lhs);

            --  Move initialization to freeze actions, once the object has
            --  been frozen and the address clause alignment check has been
            --  performed.

            Append_Freeze_Action (E,
              Make_Assignment_Statement (Sloc (Decl),
                Name       => Lhs,
                Expression => Expression (Decl)));

            Set_No_Initialization (Decl);

            --  If the objet is tagged, check whether the tag must be
            --  reassigned explicitly.

            Tag_Assign := Make_Tag_Assignment (Decl);
            if Present (Tag_Assign) then
               Append_Freeze_Action (E, Tag_Assign);
            end if;
         end if;
      end if;
   end Check_Address_Clause;

   -----------------------------
   -- Check_Compile_Time_Size --
   -----------------------------

   procedure Check_Compile_Time_Size (T : Entity_Id) is

      procedure Set_Small_Size (T : Entity_Id; S : Uint);
      --  Sets the compile time known size (64 bits or less) in the RM_Size
      --  field of T, checking for a size clause that was given which attempts
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
         if S > 64 then
            return;

         --  Check for bad size clause given

         elsif Has_Size_Clause (T) then
            if RM_Size (T) < S then
               Error_Msg_Uint_1 := S;
               Error_Msg_NE
                 ("size for& too small, minimum allowed is ^",
                  Size_Clause (T), T);
            end if;

         --  Set size if not set already

         elsif Unknown_RM_Size (T) then
            Set_RM_Size (T, S);
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

         --  Always True for elementary types, even generic formal elementary
         --  types. We used to return False in the latter case, but the size
         --  is known at compile time, even in the template, we just do not
         --  know the exact size but that's not the point of this routine.

         elsif Is_Elementary_Type (T) or else Is_Task_Type (T) then
            return True;

         --  Array types

         elsif Is_Array_Type (T) then

            --  String literals always have known size, and we can set it

            if Ekind (T) = E_String_Literal_Subtype then
               Set_Small_Size
                 (T, Component_Size (T) * String_Literal_Length (T));
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
            --  (in case it is not greater than 64 and may be packable).

            declare
               Size : Uint := Component_Size (T);
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
                        Size := Size * Dim;
                     else
                        Size := Uint_0;
                     end if;
                  end if;

                  Next_Index (Index);
               end loop;

               Set_Small_Size (T, Size);
               return True;
            end;

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
            --  discriminated components.

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
               --  packed record, unless the record has discriminants or atomic
               --  components or independent components.

               --  The reason we eliminate the discriminated case is that
               --  we don't know the way the back end lays out discriminated
               --  packed records. If Packed_Size_Known is True, then
               --  Packed_Size is the size in bits so far.

               Packed_Size_Known : Boolean :=
                 Is_Packed (T)
                   and then not Has_Discriminants (T)
                   and then not Has_Atomic_Components (T)
                   and then not Has_Independent_Components (T);

               Packed_Size : Uint := Uint_0;
               --  Size in bits so far

            begin
               --  Test for variant part present

               if Has_Discriminants (T)
                 and then Present (Parent (T))
                 and then Nkind (Parent (T)) = N_Full_Type_Declaration
                 and then Nkind (Type_Definition (Parent (T))) =
                                               N_Record_Definition
                 and then not Null_Present (Type_Definition (Parent (T)))
                 and then
                   Present (Variant_Part
                              (Component_List (Type_Definition (Parent (T)))))
               then
                  --  If variant part is present, and type is unconstrained,
                  --  then we must have defaulted discriminants, or a size
                  --  clause must be present for the type, or else the size
                  --  is definitely not known at compile time.

                  if not Is_Constrained (T)
                    and then
                      No (Discriminant_Default_Value (First_Discriminant (T)))
                    and then Unknown_RM_Size (T)
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

                  --  We do not know the packed size for an atomic/VFA type
                  --  or component, or an independent type or component, or a
                  --  by-reference type or aliased component (because packing
                  --  does not touch these).

                  if        Is_Atomic_Or_VFA (Ctyp)
                    or else Is_Atomic_Or_VFA (Comp)
                    or else Is_Independent (Ctyp)
                    or else Is_Independent (Comp)
                    or else Is_By_Reference_Type (Ctyp)
                    or else Is_Aliased (Comp)
                  then
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
                    and then Present (Packed_Array_Impl_Type (Etype (Comp)))
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

                     --  We can deal with elementary types, small packed arrays
                     --  if the representation is a modular type and also small
                     --  record types (if the size is not greater than 64, but
                     --  the condition is checked by Set_Small_Size).

                     if Is_Elementary_Type (Ctyp)
                       or else (Is_Array_Type (Ctyp)
                                 and then Present
                                            (Packed_Array_Impl_Type (Ctyp))
                                 and then Is_Modular_Integer_Type
                                            (Packed_Array_Impl_Type (Ctyp)))
                       or else Is_Record_Type (Ctyp)
                     then
                        --  If RM_Size is known and static, then we can keep
                        --  accumulating the packed size.

                        if Known_Static_RM_Size (Ctyp) then

                           Packed_Size := Packed_Size + RM_Size (Ctyp);

                        --  If we have a field whose RM_Size is not known then
                        --  we can't figure out the packed size here.

                        else
                           Packed_Size_Known := False;
                        end if;

                     --  For other types we can't figure out the packed size

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

   -----------------------------------
   -- Check_Component_Storage_Order --
   -----------------------------------

   procedure Check_Component_Storage_Order
     (Encl_Type        : Entity_Id;
      Comp             : Entity_Id;
      ADC              : Node_Id;
      Comp_ADC_Present : out Boolean)
   is
      Encl_Base : Entity_Id;
      Comp_Base : Entity_Id;
      Comp_ADC  : Node_Id;
      Err_Node  : Node_Id;

      Comp_Byte_Aligned : Boolean;
      --  Set for the record case, True if Comp starts on a byte boundary
      --  (in which case it is allowed to have different storage order).

      Comp_SSO_Differs  : Boolean;
      --  Set True when the component is a nested composite, and it does not
      --  have the same scalar storage order as Encl_Type.

      Component_Aliased : Boolean;

   begin
      --  Record case

      if Present (Comp) then
         Err_Node  := Comp;
         Comp_Base := Etype (Comp);

         if Is_Tag (Comp) then
            Comp_Byte_Aligned := True;
            Component_Aliased := False;

         else
            --  If a component clause is present, check if the component starts
            --  on a storage element boundary. Otherwise conservatively assume
            --  it does so only in the case where the record is not packed.

            if Present (Component_Clause (Comp)) then
               Comp_Byte_Aligned :=
                 Normalized_First_Bit (Comp) mod System_Storage_Unit = 0;
            else
               Comp_Byte_Aligned := not Is_Packed (Encl_Type);
            end if;

            Component_Aliased := Is_Aliased (Comp);
         end if;

      --  Array case

      else
         Err_Node  := Encl_Type;
         Comp_Base := Component_Type (Encl_Type);

         Component_Aliased := Has_Aliased_Components (Encl_Type);
      end if;

      --  Note: the Reverse_Storage_Order flag is set on the base type, but
      --  the attribute definition clause is attached to the first subtype.
      --  Also, if the base type is incomplete or private, go to full view
      --  if known

      Encl_Base := Base_Type (Encl_Type);
      if Present (Underlying_Type (Encl_Base)) then
         Encl_Base := Underlying_Type (Encl_Base);
      end if;

      Comp_Base := Base_Type (Comp_Base);
      if Present (Underlying_Type (Comp_Base)) then
         Comp_Base := Underlying_Type (Comp_Base);
      end if;

      Comp_ADC := Get_Attribute_Definition_Clause
                    (First_Subtype (Comp_Base),
                     Attribute_Scalar_Storage_Order);
      Comp_ADC_Present := Present (Comp_ADC);

      --  Case of record or array component: check storage order compatibility.
      --  But, if the record has Complex_Representation, then it is treated as
      --  a scalar in the back end so the storage order is irrelevant.

      if (Is_Record_Type (Comp_Base)
            and then not Has_Complex_Representation (Comp_Base))
        or else Is_Array_Type (Comp_Base)
      then
         Comp_SSO_Differs :=
           Reverse_Storage_Order (Encl_Base)
             /=
           Reverse_Storage_Order (Comp_Base);

         --  Parent and extension must have same storage order

         if Present (Comp) and then Chars (Comp) = Name_uParent then
            if Comp_SSO_Differs then
               Error_Msg_N
                 ("record extension must have same scalar storage order as "
                  & "parent", Err_Node);
            end if;

         --  If component and composite SSO differs, check that component
         --  falls on byte boundaries and isn't bit packed.

         elsif Comp_SSO_Differs then

            --  Component SSO differs from enclosing composite:

            --  Reject if component is a bit-packed array, as it is represented
            --  as a scalar internally.

            if Is_Bit_Packed_Array (Comp_Base) then
               Error_Msg_N
                 ("type of packed component must have same scalar storage "
                  & "order as enclosing composite", Err_Node);

            --  Reject if composite is a bit-packed array, as it is rewritten
            --  into an array of scalars.

            elsif Is_Bit_Packed_Array (Encl_Base) then
               Error_Msg_N
                 ("type of packed array must have same scalar storage order "
                  & "as component", Err_Node);

            --  Reject if not byte aligned

            elsif Is_Record_Type (Encl_Base)
              and then not Comp_Byte_Aligned
            then
               Error_Msg_N
                 ("type of non-byte-aligned component must have same scalar "
                  & "storage order as enclosing composite", Err_Node);

            --  Warn if specified only for the outer composite

            elsif Present (ADC) and then No (Comp_ADC) then
               Error_Msg_NE
                 ("scalar storage order specified for & does not apply to "
                  & "component?", Err_Node, Encl_Base);
            end if;
         end if;

      --  Enclosing type has explicit SSO: non-composite component must not
      --  be aliased.

      elsif Present (ADC) and then Component_Aliased then
         Error_Msg_N
           ("aliased component not permitted for type with explicit "
            & "Scalar_Storage_Order", Err_Node);
      end if;
   end Check_Component_Storage_Order;

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

   -------------------------------
   -- Check_Expression_Function --
   -------------------------------

   procedure Check_Expression_Function (N : Node_Id; Nam : Entity_Id) is
      Decl : Node_Id;

      function Find_Constant (Nod : Node_Id) return Traverse_Result;
      --  Function to search for deferred constant

      -------------------
      -- Find_Constant --
      -------------------

      function Find_Constant (Nod : Node_Id) return Traverse_Result is
      begin
         --  When a constant is initialized with the result of a dispatching
         --  call, the constant declaration is rewritten as a renaming of the
         --  displaced function result. This scenario is not a premature use of
         --  a constant even though the Has_Completion flag is not set.

         if Is_Entity_Name (Nod)
           and then Present (Entity (Nod))
           and then Ekind (Entity (Nod)) = E_Constant
           and then Scope (Entity (Nod)) = Current_Scope
           and then Nkind (Declaration_Node (Entity (Nod))) =
                                                         N_Object_Declaration
           and then not Is_Imported (Entity (Nod))
           and then not Has_Completion (Entity (Nod))
         then
            Error_Msg_NE
              ("premature use of& in call or instance", N, Entity (Nod));

         elsif Nkind (Nod) = N_Attribute_Reference then
            Analyze (Prefix (Nod));

            if Is_Entity_Name (Prefix (Nod))
              and then Is_Type (Entity (Prefix (Nod)))
            then
               Freeze_Before (N, Entity (Prefix (Nod)));
            end if;
         end if;

         return OK;
      end Find_Constant;

      procedure Check_Deferred is new Traverse_Proc (Find_Constant);

   --  Start of processing for Check_Expression_Function

   begin
      Decl := Original_Node (Unit_Declaration_Node (Nam));

      if Scope (Nam) = Current_Scope
        and then Nkind (Decl) = N_Expression_Function
      then
         Check_Deferred (Expression (Decl));
      end if;
   end Check_Expression_Function;

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

      if No (Scalar_Range (E)) or else Error_Posted (Scalar_Range (E)) then
         return;
      end if;

      --  The situation that is nontrivial is something like:

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

   -----------------------------
   -- Is_Atomic_VFA_Aggregate --
   -----------------------------

   function Is_Atomic_VFA_Aggregate (N : Node_Id) return Boolean is
      Loc   : constant Source_Ptr := Sloc (N);
      New_N : Node_Id;
      Par   : Node_Id;
      Temp  : Entity_Id;
      Typ   : Entity_Id;

   begin
      Par := Parent (N);

      --  Array may be qualified, so find outer context

      if Nkind (Par) = N_Qualified_Expression then
         Par := Parent (Par);
      end if;

      if not Comes_From_Source (Par) then
         return False;
      end if;

      case Nkind (Par) is
         when N_Assignment_Statement =>
            Typ := Etype (Name (Par));

            if not Is_Atomic_Or_VFA (Typ)
              and then not (Is_Entity_Name (Name (Par))
                             and then Is_Atomic_Or_VFA (Entity (Name (Par))))
            then
               return False;
            end if;

         when N_Object_Declaration =>
            Typ := Etype (Defining_Identifier (Par));

            if not Is_Atomic_Or_VFA (Typ)
              and then not Is_Atomic_Or_VFA (Defining_Identifier (Par))
            then
               return False;
            end if;

         when others =>
            return False;
      end case;

      Temp := Make_Temporary (Loc, 'T', N);
      New_N :=
        Make_Object_Declaration (Loc,
          Defining_Identifier => Temp,
          Object_Definition   => New_Occurrence_Of (Typ, Loc),
          Expression          => Relocate_Node (N));
      Insert_Before (Par, New_N);
      Analyze (New_N);

      Set_Expression (Par, New_Occurrence_Of (Temp, Loc));
      return True;
   end Is_Atomic_VFA_Aggregate;

   -----------------------------------------------
   -- Explode_Initialization_Compound_Statement --
   -----------------------------------------------

   procedure Explode_Initialization_Compound_Statement (E : Entity_Id) is
      Init_Stmts : constant Node_Id := Initialization_Statements (E);

   begin
      if Present (Init_Stmts)
        and then Nkind (Init_Stmts) = N_Compound_Statement
      then
         Insert_List_Before (Init_Stmts, Actions (Init_Stmts));

         --  Note that we rewrite Init_Stmts into a NULL statement, rather than
         --  just removing it, because Freeze_All may rely on this particular
         --  Node_Id still being present in the enclosing list to know where to
         --  stop freezing.

         Rewrite (Init_Stmts, Make_Null_Statement (Sloc (Init_Stmts)));

         Set_Initialization_Statements (E, Empty);
      end if;
   end Explode_Initialization_Compound_Statement;

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

      procedure Freeze_All_Ent (From : Entity_Id; After : in out Node_Id) is
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

      --  Start of processing for Freeze_All_Ent

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

               if Is_Generic_Instance (E)
                 and then Has_Delayed_Freeze (E)
               then
                  Set_Has_Delayed_Freeze (E, False);
                  Expand_N_Package_Declaration (Unit_Declaration_Node (E));
               end if;

            elsif Ekind (E) in Task_Kind
              and then Nkind_In (Parent (E), N_Task_Type_Declaration,
                                             N_Single_Task_Declaration)
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
                  Prim := First_Elmt (Prim_List);
                  while Present (Prim) loop
                     Subp := Node (Prim);

                     if Comes_From_Source (Subp)
                       and then not Is_Frozen (Subp)
                     then
                        Flist := Freeze_Entity (Subp, After);
                        Process_Flist;
                     end if;

                     Next_Elmt (Prim);
                  end loop;
               end;
            end if;

            if not Is_Frozen (E) then
               Flist := Freeze_Entity (E, After);
               Process_Flist;

            --  If already frozen, and there are delayed aspects, this is where
            --  we do the visibility check for these aspects (see Sem_Ch13 spec
            --  for a description of how we handle aspect visibility).

            elsif Has_Delayed_Aspects (E) then

               --  Retrieve the visibility to the discriminants in order to
               --  analyze properly the aspects.

               Push_Scope_And_Install_Discriminants (E);

               declare
                  Ritem : Node_Id;

               begin
                  Ritem := First_Rep_Item (E);
                  while Present (Ritem) loop
                     if Nkind (Ritem) = N_Aspect_Specification
                       and then Entity (Ritem) = E
                       and then Is_Delayed_Aspect (Ritem)
                     then
                        Check_Aspect_At_End_Of_Declarations (Ritem);
                     end if;

                     Ritem := Next_Rep_Item (Ritem);
                  end loop;
               end;

               Uninstall_Discriminants_And_Pop_Scope (E);
            end if;

            --  If an incomplete type is still not frozen, this may be a
            --  premature freezing because of a body declaration that follows.
            --  Indicate where the freezing took place. Freezing will happen
            --  if the body comes from source, but not if it is internally
            --  generated, for example as the body of a type invariant.

            --  If the freezing is caused by the end of the current declarative
            --  part, it is a Taft Amendment type, and there is no error.

            if not Is_Frozen (E)
              and then Ekind (E) = E_Incomplete_Type
            then
               declare
                  Bod : constant Node_Id := Next (After);

               begin
                  --  The presence of a body freezes all entities previously
                  --  declared in the current list of declarations, but this
                  --  does not apply if the body does not come from source.
                  --  A type invariant is transformed into a subprogram body
                  --  which is placed at the end of the private part of the
                  --  current package, but this body does not freeze incomplete
                  --  types that may be declared in this private part.

                  if (Nkind_In (Bod, N_Subprogram_Body,
                                     N_Entry_Body,
                                     N_Package_Body,
                                     N_Protected_Body,
                                     N_Task_Body)
                        or else Nkind (Bod) in N_Body_Stub)
                    and then
                      List_Containing (After) = List_Containing (Parent (E))
                    and then Comes_From_Source (Bod)
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

      --  For subprograms that are renaming_as_body, we create the wrapper
      --  bodies as needed.

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
                  if Error_Posted (Decl) then
                     Set_Has_Completion (E);
                  else
                     Build_And_Analyze_Renamed_Body (Decl, E, After);
                  end if;

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
           and then Nkind_In (Parent (E), N_Task_Type_Declaration,
                                          N_Single_Task_Declaration)
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
         end if;

         --  Historical note: We used to create a finalization master for an
         --  access type whose designated type is not controlled, but contains
         --  private controlled compoments. This form of postprocessing is no
         --  longer needed because the finalization master is now created when
         --  the access type is frozen (see Exp_Ch3.Freeze_Type).

         Next_Entity (E);
      end loop;
   end Freeze_All;

   -----------------------
   -- Freeze_And_Append --
   -----------------------

   procedure Freeze_And_Append
     (Ent    : Entity_Id;
      N      : Node_Id;
      Result : in out List_Id)
   is
      L : constant List_Id := Freeze_Entity (Ent, N);
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

   procedure Freeze_Before
     (N                 : Node_Id;
      T                 : Entity_Id;
      Do_Freeze_Profile : Boolean := True)
   is
      --  Freeze T, then insert the generated Freeze nodes before the node N.
      --  Flag Freeze_Profile is used when T is an overloadable entity, and
      --  indicates whether its profile should be frozen at the same time.

      Freeze_Nodes : constant List_Id :=
                       Freeze_Entity (T, N, Do_Freeze_Profile);

   begin
      if Ekind (T) = E_Function then
         Check_Expression_Function (N, T);
      end if;

      if Is_Non_Empty_List (Freeze_Nodes) then
         Insert_Actions (N, Freeze_Nodes);
      end if;
   end Freeze_Before;

   -------------------
   -- Freeze_Entity --
   -------------------

   function Freeze_Entity
     (E                 : Entity_Id;
      N                 : Node_Id;
      Do_Freeze_Profile : Boolean := True) return List_Id
   is
      Loc    : constant Source_Ptr := Sloc (N);
      Atype  : Entity_Id;
      Comp   : Entity_Id;
      F_Node : Node_Id;
      Formal : Entity_Id;
      Indx   : Node_Id;

      Has_Default_Initialization : Boolean := False;
      --  This flag gets set to true for a variable with default initialization

      Result : List_Id := No_List;
      --  List of freezing actions, left at No_List if none

      Test_E : Entity_Id := E;
      --  This could use a comment ???

      procedure Add_To_Result (N : Node_Id);
      --  N is a freezing action to be appended to the Result

      function After_Last_Declaration return Boolean;
      --  If Loc is a freeze_entity that appears after the last declaration
      --  in the scope, inhibit error messages on late completion.

      procedure Check_Current_Instance (Comp_Decl : Node_Id);
      --  Check that an Access or Unchecked_Access attribute with a prefix
      --  which is the current instance type can only be applied when the type
      --  is limited.

      procedure Check_Suspicious_Modulus (Utype : Entity_Id);
      --  Give warning for modulus of 8, 16, 32, or 64 given as an explicit
      --  integer literal without an explicit corresponding size clause. The
      --  caller has checked that Utype is a modular integer type.

      procedure Freeze_Array_Type (Arr : Entity_Id);
      --  Freeze array type, including freezing index and component types

      procedure Freeze_Object_Declaration (E : Entity_Id);
      --  Perform checks and generate freeze node if needed for a constant or
      --  variable declared by an object declaration.

      function Freeze_Generic_Entities (Pack : Entity_Id) return List_Id;
      --  Create Freeze_Generic_Entity nodes for types declared in a generic
      --  package. Recurse on inner generic packages.

      function Freeze_Profile (E : Entity_Id) return Boolean;
      --  Freeze formals and return type of subprogram. If some type in the
      --  profile is incomplete and we are in an instance, freezing of the
      --  entity will take place elsewhere, and the function returns False.

      procedure Freeze_Record_Type (Rec : Entity_Id);
      --  Freeze record type, including freezing component types, and freezing
      --  primitive operations if this is a tagged type.

      function Has_Boolean_Aspect_Import (E : Entity_Id) return Boolean;
      --  Determine whether an arbitrary entity is subject to Boolean aspect
      --  Import and its value is specified as True.

      function New_Freeze_Node return Node_Id;
      --  Create a new freeze node for entity E

      procedure Wrap_Imported_Subprogram (E : Entity_Id);
      --  If E is an entity for an imported subprogram with pre/post-conditions
      --  then this procedure will create a wrapper to ensure that proper run-
      --  time checking of the pre/postconditions. See body for details.

      -------------------
      -- Add_To_Result --
      -------------------

      procedure Add_To_Result (N : Node_Id) is
      begin
         if No (Result) then
            Result := New_List (N);
         else
            Append (N, Result);
         end if;
      end Add_To_Result;

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

         function Is_Aliased_View_Of_Type (Typ : Entity_Id) return Boolean;
         --  Determine whether Typ is compatible with the rules for aliased
         --  views of types as defined in RM 3.10 in the various dialects.

         function Process (N : Node_Id) return Traverse_Result;
         --  Process routine to apply check to given node

         -----------------------------
         -- Is_Aliased_View_Of_Type --
         -----------------------------

         function Is_Aliased_View_Of_Type (Typ : Entity_Id) return Boolean is
            Typ_Decl : constant Node_Id := Parent (Typ);

         begin
            --  Common case

            if Nkind (Typ_Decl) = N_Full_Type_Declaration
              and then Limited_Present (Type_Definition (Typ_Decl))
            then
               return True;

            --  The following paragraphs describe what a legal aliased view of
            --  a type is in the various dialects of Ada.

            --  Ada 95

            --  The current instance of a limited type, and a formal parameter
            --  or generic formal object of a tagged type.

            --  Ada 95 limited type
            --    * Type with reserved word "limited"
            --    * A protected or task type
            --    * A composite type with limited component

            elsif Ada_Version <= Ada_95 then
               return Is_Limited_Type (Typ);

            --  Ada 2005

            --  The current instance of a limited tagged type, a protected
            --  type, a task type, or a type that has the reserved word
            --  "limited" in its full definition ... a formal parameter or
            --  generic formal object of a tagged type.

            --  Ada 2005 limited type
            --    * Type with reserved word "limited", "synchronized", "task"
            --      or "protected"
            --    * A composite type with limited component
            --    * A derived type whose parent is a non-interface limited type

            elsif Ada_Version = Ada_2005 then
               return
                 (Is_Limited_Type (Typ) and then Is_Tagged_Type (Typ))
                   or else
                     (Is_Derived_Type (Typ)
                       and then not Is_Interface (Etype (Typ))
                       and then Is_Limited_Type (Etype (Typ)));

            --  Ada 2012 and beyond

            --  The current instance of an immutably limited type ... a formal
            --  parameter or generic formal object of a tagged type.

            --  Ada 2012 limited type
            --    * Type with reserved word "limited", "synchronized", "task"
            --      or "protected"
            --    * A composite type with limited component
            --    * A derived type whose parent is a non-interface limited type
            --    * An incomplete view

            --  Ada 2012 immutably limited type
            --    * Explicitly limited record type
            --    * Record extension with "limited" present
            --    * Non-formal limited private type that is either tagged
            --      or has at least one access discriminant with a default
            --      expression
            --    * Task type, protected type or synchronized interface
            --    * Type derived from immutably limited type

            else
               return
                 Is_Immutably_Limited_Type (Typ)
                   or else Is_Incomplete_Type (Typ);
            end if;
         end Is_Aliased_View_Of_Type;

         -------------
         -- Process --
         -------------

         function Process (N : Node_Id) return Traverse_Result is
         begin
            case Nkind (N) is
               when N_Attribute_Reference =>
                  if Nam_In (Attribute_Name (N), Name_Access,
                                                 Name_Unchecked_Access)
                    and then Is_Entity_Name (Prefix (N))
                    and then Is_Type (Entity (Prefix (N)))
                    and then Entity (Prefix (N)) = E
                  then
                     if Ada_Version < Ada_2012 then
                        Error_Msg_N
                          ("current instance must be a limited type",
                           Prefix (N));
                     else
                        Error_Msg_N
                          ("current instance must be an immutably limited "
                           & "type (RM-2012, 7.5 (8.1/3))", Prefix (N));
                     end if;

                     return Abandon;

                  else
                     return OK;
                  end if;

               when others => return OK;
            end case;
         end Process;

         procedure Traverse is new Traverse_Proc (Process);

         --  Local variables

         Rec_Type : constant Entity_Id :=
                      Scope (Defining_Identifier (Comp_Decl));

      --  Start of processing for Check_Current_Instance

      begin
         if not Is_Aliased_View_Of_Type (Rec_Type) then
            Traverse (Comp_Decl);
         end if;
      end Check_Current_Instance;

      ------------------------------
      -- Check_Suspicious_Modulus --
      ------------------------------

      procedure Check_Suspicious_Modulus (Utype : Entity_Id) is
         Decl : constant Node_Id := Declaration_Node (Underlying_Type (Utype));

      begin
         if not Warn_On_Suspicious_Modulus_Value then
            return;
         end if;

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
                             ("?M?2 '*'*^' may have been intended here",
                              Modulus);
                        end;
                     end if;
                  end;
               end if;
            end;
         end if;
      end Check_Suspicious_Modulus;

      -----------------------
      -- Freeze_Array_Type --
      -----------------------

      procedure Freeze_Array_Type (Arr : Entity_Id) is
         FS     : constant Entity_Id := First_Subtype (Arr);
         Ctyp   : constant Entity_Id := Component_Type (Arr);
         Clause : Entity_Id;

         Non_Standard_Enum : Boolean := False;
         --  Set true if any of the index types is an enumeration type with a
         --  non-standard representation.

      begin
         Freeze_And_Append (Ctyp, N, Result);

         Indx := First_Index (Arr);
         while Present (Indx) loop
            Freeze_And_Append (Etype (Indx), N, Result);

            if Is_Enumeration_Type (Etype (Indx))
              and then Has_Non_Standard_Rep (Etype (Indx))
            then
               Non_Standard_Enum := True;
            end if;

            Next_Index (Indx);
         end loop;

         --  Processing that is done only for base types

         if Ekind (Arr) = E_Array_Type then

            --  Deal with default setting of reverse storage order

            Set_SSO_From_Default (Arr);

            --  Propagate flags for component type

            if Is_Controlled_Active (Component_Type (Arr))
              or else Has_Controlled_Component (Ctyp)
            then
               Set_Has_Controlled_Component (Arr);
            end if;

            if Has_Unchecked_Union (Component_Type (Arr)) then
               Set_Has_Unchecked_Union (Arr);
            end if;

            --  Warn for pragma Pack overriding foreign convention

            if Has_Foreign_Convention (Ctyp)
              and then Has_Pragma_Pack (Arr)
            then
               declare
                  CN : constant Name_Id :=
                         Get_Convention_Name (Convention (Ctyp));
                  PP : constant Node_Id :=
                         Get_Pragma (First_Subtype (Arr), Pragma_Pack);
               begin
                  if Present (PP) then
                     Error_Msg_Name_1 := CN;
                     Error_Msg_Sloc := Sloc (Arr);
                     Error_Msg_N
                       ("pragma Pack affects convention % components #??", PP);
                     Error_Msg_Name_1 := CN;
                     Error_Msg_N
                       ("\array components may not have % compatible "
                        & "representation??", PP);
                  end if;
               end;
            end if;

            --  If packing was requested or if the component size was
            --  set explicitly, then see if bit packing is required. This
            --  processing is only done for base types, since all of the
            --  representation aspects involved are type-related.

            --  This is not just an optimization, if we start processing the
            --  subtypes, they interfere with the settings on the base type
            --  (this is because Is_Packed has a slightly different meaning
            --  before and after freezing).

            declare
               Csiz : Uint;
               Esiz : Uint;

            begin
               if (Is_Packed (Arr) or else Has_Pragma_Pack (Arr))
                 and then Known_Static_RM_Size (Ctyp)
                 and then not Has_Component_Size_Clause (Arr)
               then
                  Csiz := UI_Max (RM_Size (Ctyp), 1);

               elsif Known_Component_Size (Arr) then
                  Csiz := Component_Size (Arr);

               elsif not Known_Static_Esize (Ctyp) then
                  Csiz := Uint_0;

               else
                  Esiz := Esize (Ctyp);

                  --  We can set the component size if it is less than 16,
                  --  rounding it up to the next storage unit size.

                  if Esiz <= 8 then
                     Csiz := Uint_8;
                  elsif Esiz <= 16 then
                     Csiz := Uint_16;
                  else
                     Csiz := Uint_0;
                  end if;

                  --  Set component size up to match alignment if it would
                  --  otherwise be less than the alignment. This deals with
                  --  cases of types whose alignment exceeds their size (the
                  --  padded type cases).

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

               --  Case of component size that may result in bit packing

               if 1 <= Csiz and then Csiz <= 64 then
                  declare
                     Ent         : constant Entity_Id :=
                                     First_Subtype (Arr);
                     Pack_Pragma : constant Node_Id :=
                                     Get_Rep_Pragma (Ent, Name_Pack);
                     Comp_Size_C : constant Node_Id :=
                                     Get_Attribute_Definition_Clause
                                       (Ent, Attribute_Component_Size);

                  begin
                     --  Warn if we have pack and component size so that the
                     --  pack is ignored.

                     --  Note: here we must check for the presence of a
                     --  component size before checking for a Pack pragma to
                     --  deal with the case where the array type is a derived
                     --  type whose parent is currently private.

                     if Present (Comp_Size_C)
                       and then Has_Pragma_Pack (Ent)
                       and then Warn_On_Redundant_Constructs
                     then
                        Error_Msg_Sloc := Sloc (Comp_Size_C);
                        Error_Msg_NE
                          ("?r?pragma Pack for& ignored!", Pack_Pragma, Ent);
                        Error_Msg_N
                          ("\?r?explicit component size given#!", Pack_Pragma);
                        Set_Is_Packed (Base_Type (Ent), False);
                        Set_Is_Bit_Packed_Array (Base_Type (Ent), False);
                     end if;

                     --  Set component size if not already set by a component
                     --  size clause.

                     if not Present (Comp_Size_C) then
                        Set_Component_Size (Arr, Csiz);
                     end if;

                     --  Check for base type of 8, 16, 32 bits, where an
                     --  unsigned subtype has a length one less than the
                     --  base type (e.g. Natural subtype of Integer).

                     --  In such cases, if a component size was not set
                     --  explicitly, then generate a warning.

                     if Has_Pragma_Pack (Arr)
                       and then not Present (Comp_Size_C)
                       and then (Csiz = 7 or else Csiz = 15 or else Csiz = 31)
                       and then Esize (Base_Type (Ctyp)) = Csiz + 1
                     then
                        Error_Msg_Uint_1 := Csiz;

                        if Present (Pack_Pragma) then
                           Error_Msg_N
                             ("??pragma Pack causes component size to be ^!",
                              Pack_Pragma);
                           Error_Msg_N
                             ("\??use Component_Size to set desired value!",
                              Pack_Pragma);
                        end if;
                     end if;

                     --  Bit packing is never needed for 8, 16, 32, 64

                     if Addressable (Csiz) then
                        --  If the Esize of the component is known and equal to
                        --  the component size then even packing is not needed.

                        if Known_Static_Esize (Component_Type (Arr))
                          and then Esize (Component_Type (Arr)) = Csiz
                        then
                           --  Here the array was requested to be packed, but
                           --  the packing request had no effect whatsoever,
                           --  so flag Is_Packed is reset.

                           --  Note: semantically this means that we lose track
                           --  of the fact that a derived type inherited pragma
                           --  Pack that was non-effective, but that is fine.

                           --  We regard a Pack pragma as a request to set a
                           --  representation characteristic, and this request
                           --  may be ignored.

                           Set_Is_Packed            (Base_Type (Arr), False);
                           Set_Has_Non_Standard_Rep (Base_Type (Arr), False);
                        else
                           Set_Is_Packed            (Base_Type (Arr), True);
                           Set_Has_Non_Standard_Rep (Base_Type (Arr), True);
                        end if;

                        Set_Is_Bit_Packed_Array (Base_Type (Arr), False);

                     --  Bit packing is not needed for multiples of the storage
                     --  unit if the type is composite because the back end can
                     --  byte pack composite types.

                     elsif Csiz mod System_Storage_Unit = 0
                       and then Is_Composite_Type (Ctyp)
                     then

                        Set_Is_Packed            (Base_Type (Arr), True);
                        Set_Has_Non_Standard_Rep (Base_Type (Arr), True);
                        Set_Is_Bit_Packed_Array  (Base_Type (Arr), False);

                     --  In all other cases, bit packing is needed

                     else
                        Set_Is_Packed            (Base_Type (Arr), True);
                        Set_Has_Non_Standard_Rep (Base_Type (Arr), True);
                        Set_Is_Bit_Packed_Array  (Base_Type (Arr), True);
                     end if;
                  end;
               end if;
            end;

            --  Check for Aliased or Atomic_Components/Atomic/VFA with
            --  unsuitable packing or explicit component size clause given.

            if (Has_Aliased_Components (Arr)
                 or else Has_Atomic_Components (Arr)
                 or else Is_Atomic_Or_VFA (Ctyp))
              and then
                (Has_Component_Size_Clause (Arr) or else Is_Packed (Arr))
            then
               Alias_Atomic_Check : declare

                  procedure Complain_CS (T : String);
                  --  Outputs error messages for incorrect CS clause or pragma
                  --  Pack for aliased or atomic/VFA components (T is "aliased"
                  --  or "atomic/vfa");

                  -----------------
                  -- Complain_CS --
                  -----------------

                  procedure Complain_CS (T : String) is
                  begin
                     if Has_Component_Size_Clause (Arr) then
                        Clause :=
                          Get_Attribute_Definition_Clause
                            (FS, Attribute_Component_Size);

                        Error_Msg_N
                          ("incorrect component size for "
                           & T & " components", Clause);
                        Error_Msg_Uint_1 := Esize (Ctyp);
                        Error_Msg_N
                          ("\only allowed value is^", Clause);

                     else
                        Error_Msg_N
                          ("cannot pack " & T & " components",
                           Get_Rep_Pragma (FS, Name_Pack));
                     end if;
                  end Complain_CS;

                  --  Start of processing for Alias_Atomic_Check

               begin
                  --  If object size of component type isn't known, we cannot
                  --  be sure so we defer to the back end.

                  if not Known_Static_Esize (Ctyp) then
                     null;

                  --  Case where component size has no effect. First check for
                  --  object size of component type multiple of the storage
                  --  unit size.

                  elsif Esize (Ctyp) mod System_Storage_Unit = 0

                    --  OK in both packing case and component size case if RM
                    --  size is known and static and same as the object size.

                    and then
                      ((Known_Static_RM_Size (Ctyp)
                         and then Esize (Ctyp) = RM_Size (Ctyp))

                        --  Or if we have an explicit component size clause and
                        --  the component size and object size are equal.

                        or else
                          (Has_Component_Size_Clause (Arr)
                            and then Component_Size (Arr) = Esize (Ctyp)))
                  then
                     null;

                  elsif Has_Aliased_Components (Arr) then
                     Complain_CS ("aliased");

                  elsif Has_Atomic_Components (Arr)
                    or else Is_Atomic (Ctyp)
                  then
                     Complain_CS ("atomic");

                  elsif Is_Volatile_Full_Access (Ctyp) then
                     Complain_CS ("volatile full access");
                  end if;
               end Alias_Atomic_Check;
            end if;

            --  Check for Independent_Components/Independent with unsuitable
            --  packing or explicit component size clause given.

            if (Has_Independent_Components (Arr) or else Is_Independent (Ctyp))
                  and then
               (Has_Component_Size_Clause  (Arr) or else Is_Packed (Arr))
            then
               begin
                  --  If object size of component type isn't known, we cannot
                  --  be sure so we defer to the back end.

                  if not Known_Static_Esize (Ctyp) then
                     null;

                  --  Case where component size has no effect. First check for
                  --  object size of component type multiple of the storage
                  --  unit size.

                  elsif Esize (Ctyp) mod System_Storage_Unit = 0

                    --  OK in both packing case and component size case if RM
                    --  size is known and multiple of the storage unit size.

                    and then
                      ((Known_Static_RM_Size (Ctyp)
                         and then RM_Size (Ctyp) mod System_Storage_Unit = 0)

                        --  Or if we have an explicit component size clause and
                        --  the component size is larger than the object size.

                        or else
                          (Has_Component_Size_Clause (Arr)
                            and then Component_Size (Arr) >= Esize (Ctyp)))
                  then
                     null;

                  else
                     if Has_Component_Size_Clause (Arr) then
                        Clause :=
                          Get_Attribute_Definition_Clause
                            (FS, Attribute_Component_Size);

                        Error_Msg_N
                          ("incorrect component size for "
                           & "independent components", Clause);
                        Error_Msg_Uint_1 := Esize (Ctyp);
                        Error_Msg_N
                          ("\minimum allowed is^", Clause);

                     else
                        Error_Msg_N
                          ("cannot pack independent components",
                           Get_Rep_Pragma (FS, Name_Pack));
                     end if;
                  end if;
               end;
            end if;

            --  Warn for case of atomic type

            Clause := Get_Rep_Pragma (FS, Name_Atomic);

            if Present (Clause)
              and then not Addressable (Component_Size (FS))
            then
               Error_Msg_NE
                 ("non-atomic components of type& may not be "
                  & "accessible by separate tasks??", Clause, Arr);

               if Has_Component_Size_Clause (Arr) then
                  Error_Msg_Sloc := Sloc (Get_Attribute_Definition_Clause
                                           (FS, Attribute_Component_Size));
                  Error_Msg_N ("\because of component size clause#??", Clause);

               elsif Has_Pragma_Pack (Arr) then
                  Error_Msg_Sloc := Sloc (Get_Rep_Pragma (FS, Name_Pack));
                  Error_Msg_N ("\because of pragma Pack#??", Clause);
               end if;
            end if;

            --  Check for scalar storage order

            declare
               Dummy : Boolean;
            begin
               Check_Component_Storage_Order
                 (Encl_Type        => Arr,
                  Comp             => Empty,
                  ADC              => Get_Attribute_Definition_Clause
                                        (First_Subtype (Arr),
                                         Attribute_Scalar_Storage_Order),
                  Comp_ADC_Present => Dummy);
            end;

         --  Processing that is done only for subtypes

         else
            --  Acquire alignment from base type

            if Unknown_Alignment (Arr) then
               Set_Alignment (Arr, Alignment (Base_Type (Arr)));
               Adjust_Esize_Alignment (Arr);
            end if;
         end if;

         --  Specific checks for bit-packed arrays

         if Is_Bit_Packed_Array (Arr) then

            --  Check number of elements for bit packed arrays that come from
            --  source and have compile time known ranges. The bit-packed
            --  arrays circuitry does not support arrays with more than
            --  Integer'Last + 1 elements, and when this restriction is
            --  violated, causes incorrect data access.

            --  For the case where this is not compile time known, a run-time
            --  check should be generated???

            if Comes_From_Source (Arr) and then Is_Constrained (Arr) then
               declare
                  Elmts : Uint;
                  Index : Node_Id;
                  Ilen  : Node_Id;
                  Ityp  : Entity_Id;

               begin
                  Elmts := Uint_1;
                  Index := First_Index (Arr);
                  while Present (Index) loop
                     Ityp := Etype (Index);

                     --  Never generate an error if any index is of a generic
                     --  type. We will check this in instances.

                     if Is_Generic_Type (Ityp) then
                        Elmts := Uint_0;
                        exit;
                     end if;

                     Ilen :=
                       Make_Attribute_Reference (Loc,
                         Prefix         => New_Occurrence_Of (Ityp, Loc),
                         Attribute_Name => Name_Range_Length);
                     Analyze_And_Resolve (Ilen);

                     --  No attempt is made to check number of elements if not
                     --  compile time known.

                     if Nkind (Ilen) /= N_Integer_Literal then
                        Elmts := Uint_0;
                        exit;
                     end if;

                     Elmts := Elmts * Intval (Ilen);
                     Next_Index (Index);
                  end loop;

                  if Elmts > Intval (High_Bound
                                       (Scalar_Range (Standard_Integer))) + 1
                  then
                     Error_Msg_N
                       ("bit packed array type may not have "
                        & "more than Integer''Last+1 elements", Arr);
                  end if;
               end;
            end if;

            --  Check size

            if Known_RM_Size (Arr) then
               declare
                  SizC    : constant Node_Id := Size_Clause (Arr);
                  Discard : Boolean;

               begin
                  --  It is not clear if it is possible to have no size clause
                  --  at this stage, but it is not worth worrying about. Post
                  --  error on the entity name in the size clause if present,
                  --  else on the type entity itself.

                  if Present (SizC) then
                     Check_Size (Name (SizC), Arr, RM_Size (Arr), Discard);
                  else
                     Check_Size (Arr, Arr, RM_Size (Arr), Discard);
                  end if;
               end;
            end if;
         end if;

         --  If any of the index types was an enumeration type with a non-
         --  standard rep clause, then we indicate that the array type is
         --  always packed (even if it is not bit packed).

         if Non_Standard_Enum then
            Set_Has_Non_Standard_Rep (Base_Type (Arr));
            Set_Is_Packed            (Base_Type (Arr));
         end if;

         Set_Component_Alignment_If_Not_Set (Arr);

         --  If the array is packed and bit packed or packed to eliminate holes
         --  in the non-contiguous enumeration index types, we must create the
         --  packed array type to be used to actually implement the type. This
         --  is only needed for real array types (not for string literal types,
         --  since they are present only for the front end).

         if Is_Packed (Arr)
           and then (Is_Bit_Packed_Array (Arr) or else Non_Standard_Enum)
           and then Ekind (Arr) /= E_String_Literal_Subtype
         then
            Create_Packed_Array_Impl_Type (Arr);
            Freeze_And_Append (Packed_Array_Impl_Type (Arr), N, Result);

            --  Make sure that we have the necessary routines to implement the
            --  packing, and complain now if not. Note that we only test this
            --  for constrained array types.

            if Is_Constrained (Arr)
              and then Is_Bit_Packed_Array (Arr)
              and then Present (Packed_Array_Impl_Type (Arr))
              and then Is_Array_Type (Packed_Array_Impl_Type (Arr))
            then
               declare
                  CS : constant Uint  := Component_Size (Arr);
                  RE : constant RE_Id := Get_Id (UI_To_Int (CS));

               begin
                  if RE /= RE_Null
                    and then not RTE_Available (RE)
                  then
                     Error_Msg_CRT
                       ("packing of " & UI_Image (CS) & "-bit components",
                        First_Subtype (Etype (Arr)));

                     --  Cancel the packing

                     Set_Is_Packed (Base_Type (Arr), False);
                     Set_Is_Bit_Packed_Array (Base_Type (Arr), False);
                     Set_Packed_Array_Impl_Type (Arr, Empty);
                     goto Skip_Packed;
                  end if;
               end;
            end if;

            --  Size information of packed array type is copied to the array
            --  type, since this is really the representation. But do not
            --  override explicit existing size values. If the ancestor subtype
            --  is constrained the Packed_Array_Impl_Type will be inherited
            --  from it, but the size may have been provided already, and
            --  must not be overridden either.

            if not Has_Size_Clause (Arr)
              and then
                (No (Ancestor_Subtype (Arr))
                  or else not Has_Size_Clause (Ancestor_Subtype (Arr)))
            then
               Set_Esize     (Arr, Esize     (Packed_Array_Impl_Type (Arr)));
               Set_RM_Size   (Arr, RM_Size   (Packed_Array_Impl_Type (Arr)));
            end if;

            if not Has_Alignment_Clause (Arr) then
               Set_Alignment (Arr, Alignment (Packed_Array_Impl_Type (Arr)));
            end if;
         end if;

         <<Skip_Packed>>

         --  For non-packed arrays set the alignment of the array to the
         --  alignment of the component type if it is unknown. Skip this
         --  in atomic/VFA case (atomic/VFA arrays may need larger alignments).

         if not Is_Packed (Arr)
           and then Unknown_Alignment (Arr)
           and then Known_Alignment (Ctyp)
           and then Known_Static_Component_Size (Arr)
           and then Known_Static_Esize (Ctyp)
           and then Esize (Ctyp) = Component_Size (Arr)
           and then not Is_Atomic_Or_VFA (Arr)
         then
            Set_Alignment (Arr, Alignment (Component_Type (Arr)));
         end if;

         --  A Ghost type cannot have a component of protected or task type
         --  (SPARK RM 6.9(19)).

         if Is_Ghost_Entity (Arr) and then Is_Concurrent_Type (Ctyp) then
            Error_Msg_N
              ("ghost array type & cannot have concurrent component type",
               Arr);
         end if;
      end Freeze_Array_Type;

      -------------------------------
      -- Freeze_Object_Declaration --
      -------------------------------

      procedure Freeze_Object_Declaration (E : Entity_Id) is
      begin
         --  Abstract type allowed only for C++ imported variables or constants

         --  Note: we inhibit this check for objects that do not come from
         --  source because there is at least one case (the expansion of
         --  x'Class'Input where x is abstract) where we legitimately
         --  generate an abstract object.

         if Is_Abstract_Type (Etype (E))
           and then Comes_From_Source (Parent (E))
           and then not (Is_Imported (E) and then Is_CPP_Class (Etype (E)))
         then
            Error_Msg_N ("type of object cannot be abstract",
                         Object_Definition (Parent (E)));

            if Is_CPP_Class (Etype (E)) then
               Error_Msg_NE
                 ("\} may need a cpp_constructor",
                  Object_Definition (Parent (E)), Etype (E));

            elsif Present (Expression (Parent (E))) then
               Error_Msg_N --  CODEFIX
                 ("\maybe a class-wide type was meant",
                  Object_Definition (Parent (E)));
            end if;
         end if;

         --  For object created by object declaration, perform required
         --  categorization (preelaborate and pure) checks. Defer these
         --  checks to freeze time since pragma Import inhibits default
         --  initialization and thus pragma Import affects these checks.

         Validate_Object_Declaration (Declaration_Node (E));

         --  If there is an address clause, check that it is valid
         --  and if need be move initialization to the freeze node.

         Check_Address_Clause (E);

         --  Similar processing is needed for aspects that may affect
         --  object layout, like Alignment, if there is an initialization
         --  expression.

         if Has_Delayed_Aspects (E)
           and then Expander_Active
           and then Is_Array_Type (Etype (E))
           and then Present (Expression (Parent (E)))
         then
            declare
               Decl : constant Node_Id := Parent (E);
               Lhs  : constant Node_Id := New_Occurrence_Of (E, Loc);

            begin

               --  Capture initialization value at point of declaration, and
               --  make explicit assignment legal, because object may be a
               --  constant.

               Remove_Side_Effects (Expression (Decl));
               Set_Assignment_OK (Lhs);

               --  Move initialization to freeze actions.

               Append_Freeze_Action (E,
                 Make_Assignment_Statement (Loc,
                   Name       => Lhs,
                   Expression => Expression (Decl)));

               Set_No_Initialization (Decl);
               --  Set_Is_Frozen (E, False);
            end;
         end if;

         --  Reset Is_True_Constant for non-constant aliased object. We
         --  consider that the fact that a non-constant object is aliased may
         --  indicate that some funny business is going on, e.g. an aliased
         --  object is passed by reference to a procedure which captures the
         --  address of the object, which is later used to assign a new value,
         --  even though the compiler thinks that it is not modified. Such
         --  code is highly dubious, but we choose to make it "work" for
         --  non-constant aliased objects.

         --  Note that we used to do this for all aliased objects, whether or
         --  not constant, but this caused anomalies down the line because we
         --  ended up with static objects that were not Is_True_Constant. Not
         --  resetting Is_True_Constant for (aliased) constant objects ensures
         --  that this anomaly never occurs.

         --  However, we don't do that for internal entities. We figure that if
         --  we deliberately set Is_True_Constant for an internal entity, e.g.
         --  a dispatch table entry, then we mean it.

         if Ekind (E) /= E_Constant
           and then (Is_Aliased (E) or else Is_Aliased (Etype (E)))
           and then not Is_Internal_Name (Chars (E))
         then
            Set_Is_True_Constant (E, False);
         end if;

         --  If the object needs any kind of default initialization, an error
         --  must be issued if No_Default_Initialization applies. The check
         --  doesn't apply to imported objects, which are not ever default
         --  initialized, and is why the check is deferred until freezing, at
         --  which point we know if Import applies. Deferred constants are also
         --  exempted from this test because their completion is explicit, or
         --  through an import pragma.

         if Ekind (E) = E_Constant and then Present (Full_View (E)) then
            null;

         elsif Comes_From_Source (E)
           and then not Is_Imported (E)
           and then not Has_Init_Expression (Declaration_Node (E))
           and then
             ((Has_Non_Null_Base_Init_Proc (Etype (E))
                and then not No_Initialization (Declaration_Node (E))
                and then not Initialization_Suppressed (Etype (E)))
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
                          (Is_OK_Static_Expression (Expression (Decl))
                            or else Nkind (Expression (Decl)) = N_Null)))
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

         if Is_Imported (E) and then No (Address_Clause (E)) then
            Set_Is_Public (E);
         end if;

         --  For source objects that are not Imported and are library
         --  level, if no linker section pragma was given inherit the
         --  appropriate linker section from the corresponding type.

         if Comes_From_Source (E)
           and then not Is_Imported (E)
           and then Is_Library_Level_Entity (E)
           and then No (Linker_Section_Pragma (E))
         then
            Set_Linker_Section_Pragma
              (E, Linker_Section_Pragma (Etype (E)));
         end if;

         --  For convention C objects of an enumeration type, warn if the
         --  size is not integer size and no explicit size given. Skip
         --  warning for Boolean, and Character, assume programmer expects
         --  8-bit sizes for these cases.

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
              ("??convention C enumeration object has size less than ^", E);
            Error_Msg_N ("\??use explicit size clause to set size", E);
         end if;
      end Freeze_Object_Declaration;

      -----------------------------
      -- Freeze_Generic_Entities --
      -----------------------------

      function Freeze_Generic_Entities (Pack : Entity_Id) return List_Id is
         E     : Entity_Id;
         F     : Node_Id;
         Flist : List_Id;

      begin
         Flist := New_List;
         E := First_Entity (Pack);
         while Present (E) loop
            if Is_Type (E) and then not Is_Generic_Type (E) then
               F := Make_Freeze_Generic_Entity (Sloc (Pack));
               Set_Entity (F, E);
               Append_To (Flist, F);

            elsif Ekind (E) = E_Generic_Package then
               Append_List_To (Flist, Freeze_Generic_Entities (E));
            end if;

            Next_Entity (E);
         end loop;

         return Flist;
      end Freeze_Generic_Entities;

      --------------------
      -- Freeze_Profile --
      --------------------

      function Freeze_Profile (E : Entity_Id) return Boolean is
         F_Type    : Entity_Id;
         R_Type    : Entity_Id;
         Warn_Node : Node_Id;

      begin
         --  Loop through formals

         Formal := First_Formal (E);
         while Present (Formal) loop
            F_Type := Etype (Formal);

            --  AI05-0151: incomplete types can appear in a profile. By the
            --  time the entity is frozen, the full view must be available,
            --  unless it is a limited view.

            if Is_Incomplete_Type (F_Type)
              and then Present (Full_View (F_Type))
              and then not From_Limited_With (F_Type)
            then
               F_Type := Full_View (F_Type);
               Set_Etype (Formal, F_Type);
            end if;

            if not From_Limited_With (F_Type) then
               Freeze_And_Append (F_Type, N, Result);
            end if;

            if Is_Private_Type (F_Type)
              and then Is_Private_Type (Base_Type (F_Type))
              and then No (Full_View (Base_Type (F_Type)))
              and then not Is_Generic_Type (F_Type)
              and then not Is_Derived_Type (F_Type)
            then
               --  If the type of a formal is incomplete, subprogram is being
               --  frozen prematurely. Within an instance (but not within a
               --  wrapper package) this is an artifact of our need to regard
               --  the end of an instantiation as a freeze point. Otherwise it
               --  is a definite error.

               if In_Instance then
                  Set_Is_Frozen (E, False);
                  Result := No_List;
                  return False;

               elsif not After_Last_Declaration
                 and then not Freezing_Library_Level_Tagged_Type
               then
                  Error_Msg_Node_1 := F_Type;
                  Error_Msg
                    ("type & must be fully defined before this point", Loc);
               end if;
            end if;

            --  Check suspicious parameter for C function. These tests apply
            --  only to exported/imported subprograms.

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
                    ("?x?type of & does not correspond to C pointer!", Formal);

               --  Check suspicious return of boolean

               elsif Root_Type (F_Type) = Standard_Boolean
                 and then Convention (F_Type) = Convention_Ada
                 and then not Has_Warnings_Off (F_Type)
                 and then not Has_Size_Clause (F_Type)
               then
                  Error_Msg_N
                    ("& is an 8-bit Ada Boolean?x?", Formal);
                  Error_Msg_N
                    ("\use appropriate corresponding type in C "
                     & "(e.g. char)?x?", Formal);

               --  Check suspicious tagged type

               elsif (Is_Tagged_Type (F_Type)
                       or else
                        (Is_Access_Type (F_Type)
                          and then Is_Tagged_Type (Designated_Type (F_Type))))
                 and then Convention (E) = Convention_C
               then
                  Error_Msg_N
                    ("?x?& involves a tagged type which does not "
                     & "correspond to any C type!", Formal);

               --  Check wrong convention subprogram pointer

               elsif Ekind (F_Type) = E_Access_Subprogram_Type
                 and then not Has_Foreign_Convention (F_Type)
               then
                  Error_Msg_N
                    ("?x?subprogram pointer & should "
                     & "have foreign convention!", Formal);
                  Error_Msg_Sloc := Sloc (F_Type);
                  Error_Msg_NE
                    ("\?x?add Convention pragma to declaration of &#",
                     Formal, F_Type);
               end if;

               --  Turn off name qualification after message output

               Error_Msg_Qual_Level := 0;
            end if;

            --  Check for unconstrained array in exported foreign convention
            --  case.

            if Has_Foreign_Convention (E)
              and then not Is_Imported (E)
              and then Is_Array_Type (F_Type)
              and then not Is_Constrained (F_Type)
              and then Warn_On_Export_Import
            then
               Error_Msg_Qual_Level := 1;

               --  If this is an inherited operation, place the warning on
               --  the derived type declaration, rather than on the original
               --  subprogram.

               if Nkind (Original_Node (Parent (E))) = N_Full_Type_Declaration
               then
                  Warn_Node := Parent (E);

                  if Formal = First_Formal (E) then
                     Error_Msg_NE ("??in inherited operation&", Warn_Node, E);
                  end if;
               else
                  Warn_Node := Formal;
               end if;

               Error_Msg_NE ("?x?type of argument& is unconstrained array",
                  Warn_Node, Formal);
               Error_Msg_NE ("?x?foreign caller must pass bounds explicitly",
                  Warn_Node, Formal);
               Error_Msg_Qual_Level := 0;
            end if;

            if not From_Limited_With (F_Type) then
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
                  Freeze_And_Append (F_Type, N, Result);
               end if;
            end if;

            Next_Formal (Formal);
         end loop;

         --  Case of function: similar checks on return type

         if Ekind (E) = E_Function then

            --  Freeze return type

            R_Type := Etype (E);

            --  AI05-0151: the return type may have been incomplete
            --  at the point of declaration. Replace it with the full
            --  view, unless the current type is a limited view. In
            --  that case the full view is in a different unit, and
            --  gigi finds the non-limited view after the other unit
            --  is elaborated.

            if Ekind (R_Type) = E_Incomplete_Type
              and then Present (Full_View (R_Type))
              and then not From_Limited_With (R_Type)
            then
               R_Type := Full_View (R_Type);
               Set_Etype (E, R_Type);
            end if;

            Freeze_And_Append (R_Type, N, Result);

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
                  Error_Msg_N ("?x?return type of& does not "
                     & "correspond to C pointer!", E);

               --  Check suspicious return of boolean

               elsif Root_Type (R_Type) = Standard_Boolean
                 and then Convention (R_Type) = Convention_Ada
                 and then not Has_Warnings_Off (E)
                 and then not Has_Warnings_Off (R_Type)
                 and then not Has_Size_Clause (R_Type)
               then
                  declare
                     N : constant Node_Id :=
                           Result_Definition (Declaration_Node (E));
                  begin
                     Error_Msg_NE
                       ("return type of & is an 8-bit Ada Boolean?x?", N, E);
                     Error_Msg_NE
                       ("\use appropriate corresponding type in C "
                        & "(e.g. char)?x?", N, E);
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
                  Error_Msg_N ("?x?return type of & does not "
                     & "correspond to C type!", E);

               --  Check return of wrong convention subprogram pointer

               elsif Ekind (R_Type) = E_Access_Subprogram_Type
                 and then not Has_Foreign_Convention (R_Type)
                 and then not Has_Warnings_Off (E)
                 and then not Has_Warnings_Off (R_Type)
               then
                  Error_Msg_N ("?x?& should return a foreign "
                     & "convention subprogram pointer", E);
                  Error_Msg_Sloc := Sloc (R_Type);
                  Error_Msg_NE
                    ("\?x?add Convention pragma to declaration of& #",
                     E, R_Type);
               end if;
            end if;

            --  Give warning for suspicious return of a result of an
            --  unconstrained array type in a foreign convention function.

            if Has_Foreign_Convention (E)

              --  We are looking for a return of unconstrained array

              and then Is_Array_Type (R_Type)
              and then not Is_Constrained (R_Type)

              --  Exclude imported routines, the warning does not belong on
              --  the import, but rather on the routine definition.

              and then not Is_Imported (E)

              --  Check that general warning is enabled, and that it is not
              --  suppressed for this particular case.

              and then Warn_On_Export_Import
              and then not Has_Warnings_Off (E)
              and then not Has_Warnings_Off (R_Type)
            then
               Error_Msg_N
                 ("?x?foreign convention function& should not return "
                  & "unconstrained array!", E);
            end if;
         end if;

         --  Check suspicious use of Import in pure unit (cases where the RM
         --  allows calls to be omitted).

         if Is_Imported (E)

           --  It might be suspicious if the compilation unit has the Pure
           --  aspect/pragma.

           and then Has_Pragma_Pure (Cunit_Entity (Current_Sem_Unit))

           --  The RM allows omission of calls only in the case of
           --  library-level subprograms (see RM-10.2.1(18)).

           and then Is_Library_Level_Entity (E)

           --  Ignore internally generated entity. This happens in some cases
           --  of subprograms in specs, where we generate an implied body.

           and then Comes_From_Source (Import_Pragma (E))

           --  Assume run-time knows what it is doing

           and then not GNAT_Mode

           --  Assume explicit Pure_Function means import is pure

           and then not Has_Pragma_Pure_Function (E)

           --  Don't need warning in relaxed semantics mode

           and then not Relaxed_RM_Semantics

           --  Assume convention Intrinsic is OK, since this is specialized.
           --  This deals with the DEC unit current_exception.ads

           and then Convention (E) /= Convention_Intrinsic

           --  Assume that ASM interface knows what it is doing. This deals
           --  with e.g. unsigned.ads in the AAMP back end.

           and then Convention (E) /= Convention_Assembler
         then
            Error_Msg_N
              ("pragma Import in Pure unit??", Import_Pragma (E));
            Error_Msg_NE
              ("\calls to & may be omitted (RM 10.2.1(18/3))??",
               Import_Pragma (E), E);
         end if;

         return True;
      end Freeze_Profile;

      ------------------------
      -- Freeze_Record_Type --
      ------------------------

      procedure Freeze_Record_Type (Rec : Entity_Id) is
         ADC  : Node_Id;
         Comp : Entity_Id;
         IR   : Node_Id;
         Prev : Entity_Id;

         Junk : Boolean;
         pragma Warnings (Off, Junk);

         Rec_Pushed : Boolean := False;
         --  Set True if the record type scope Rec has been pushed on the scope
         --  stack. Needed for the analysis of delayed aspects specified to the
         --  components of Rec.

         SSO_ADC : Node_Id;
         --  Scalar_Storage_Order attribute definition clause for the record

         Unplaced_Component : Boolean := False;
         --  Set True if we find at least one component with no component
         --  clause (used to warn about useless Pack pragmas).

         Placed_Component : Boolean := False;
         --  Set True if we find at least one component with a component
         --  clause (used to warn about useless Bit_Order pragmas, and also
         --  to detect cases where Implicit_Packing may have an effect).

         Aliased_Component : Boolean := False;
         --  Set True if we find at least one component which is aliased. This
         --  is used to prevent Implicit_Packing of the record, since packing
         --  cannot modify the size of alignment of an aliased component.

         SSO_ADC_Component : Boolean := False;
         --  Set True if we find at least one component whose type has a
         --  Scalar_Storage_Order attribute definition clause.

         All_Elem_Components : Boolean := True;
         --  Set False if we encounter a component of a composite type

         All_Sized_Components : Boolean := True;
         --  Set False if we encounter a component with unknown RM_Size

         All_Storage_Unit_Components : Boolean := True;
         --  Set False if we encounter a component of a composite type whose
         --  RM_Size is not a multiple of the storage unit.

         Elem_Component_Total_Esize : Uint := Uint_0;
         --  Accumulates total Esize values of all elementary components. Used
         --  for processing of Implicit_Packing.

         Sized_Component_Total_RM_Size : Uint := Uint_0;
         --  Accumulates total RM_Size values of all sized components. Used
         --  for processing of Implicit_Packing.

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

         procedure Freeze_Choices_In_Variant_Part (VP : Node_Id);
         --  Make sure that all types mentioned in Discrete_Choices of the
         --  variants referenceed by the Variant_Part VP are frozen. This is
         --  a recursive routine to deal with nested variants.

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
                  Add_To_Result (IR);
               end if;

            elsif Ekind (Typ) = E_Anonymous_Access_Subprogram_Type
              and then Convention (Desig) /= Convention_Protected
            then
               Set_Is_Frozen (Desig);
            end if;
         end Check_Itype;

         ------------------------------------
         -- Freeze_Choices_In_Variant_Part --
         ------------------------------------

         procedure Freeze_Choices_In_Variant_Part (VP : Node_Id) is
            pragma Assert (Nkind (VP) = N_Variant_Part);

            Variant : Node_Id;
            Choice  : Node_Id;
            CL      : Node_Id;

         begin
            --  Loop through variants

            Variant := First_Non_Pragma (Variants (VP));
            while Present (Variant) loop

               --  Loop through choices, checking that all types are frozen

               Choice := First_Non_Pragma (Discrete_Choices (Variant));
               while Present (Choice) loop
                  if Nkind (Choice) in N_Has_Etype
                    and then Present (Etype (Choice))
                  then
                     Freeze_And_Append (Etype (Choice), N, Result);
                  end if;

                  Next_Non_Pragma (Choice);
               end loop;

               --  Check for nested variant part to process

               CL := Component_List (Variant);

               if not Null_Present (CL) then
                  if Present (Variant_Part (CL)) then
                     Freeze_Choices_In_Variant_Part (Variant_Part (CL));
                  end if;
               end if;

               Next_Non_Pragma (Variant);
            end loop;
         end Freeze_Choices_In_Variant_Part;

      --  Start of processing for Freeze_Record_Type

      begin
         --  Deal with delayed aspect specifications for components. The
         --  analysis of the aspect is required to be delayed to the freeze
         --  point, thus we analyze the pragma or attribute definition
         --  clause in the tree at this point. We also analyze the aspect
         --  specification node at the freeze point when the aspect doesn't
         --  correspond to pragma/attribute definition clause.

         Comp := First_Entity (Rec);
         while Present (Comp) loop
            if Ekind (Comp) = E_Component
              and then Has_Delayed_Aspects (Comp)
            then
               if not Rec_Pushed then
                  Push_Scope (Rec);
                  Rec_Pushed := True;

                  --  The visibility to the discriminants must be restored in
                  --  order to properly analyze the aspects.

                  if Has_Discriminants (Rec) then
                     Install_Discriminants (Rec);
                  end if;
               end if;

               Analyze_Aspects_At_Freeze_Point (Comp);
            end if;

            Next_Entity (Comp);
         end loop;

         --  Pop the scope if Rec scope has been pushed on the scope stack
         --  during the delayed aspect analysis process.

         if Rec_Pushed then
            if Has_Discriminants (Rec) then
               Uninstall_Discriminants (Rec);
            end if;

            Pop_Scope;
         end if;

         --  Freeze components and embedded subtypes

         Comp := First_Entity (Rec);
         Prev := Empty;
         while Present (Comp) loop
            if Is_Aliased (Comp) then
               Aliased_Component := True;
            end if;

            --  Handle the component and discriminant case

            if Ekind_In (Comp, E_Component, E_Discriminant) then
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

                  Freeze_And_Append (Etype (Comp), N, Result);

                  --  Warn for pragma Pack overriding foreign convention

                  if Has_Foreign_Convention (Etype (Comp))
                    and then Has_Pragma_Pack (Rec)

                    --  Don't warn for aliased components, since override
                    --  cannot happen in that case.

                    and then not Is_Aliased (Comp)
                  then
                     declare
                        CN : constant Name_Id :=
                               Get_Convention_Name (Convention (Etype (Comp)));
                        PP : constant Node_Id :=
                               Get_Pragma (Rec, Pragma_Pack);
                     begin
                        if Present (PP) then
                           Error_Msg_Name_1 := CN;
                           Error_Msg_Sloc := Sloc (Comp);
                           Error_Msg_N
                             ("pragma Pack affects convention % component#??",
                              PP);
                           Error_Msg_Name_1 := CN;
                           Error_Msg_NE
                             ("\component & may not have % compatible "
                              & "representation??", PP, Comp);
                        end if;
                     end;
                  end if;

                  --  Check for error of component clause given for variable
                  --  sized type. We have to delay this test till this point,
                  --  since the component type has to be frozen for us to know
                  --  if it is variable length.

                  if Present (CC) then
                     Placed_Component := True;

                     --  We omit this test in a generic context, it will be
                     --  applied at instantiation time.

                     if Inside_A_Generic then
                        null;

                     --  Also omit this test in CodePeer mode, since we do not
                     --  have sufficient info on size and rep clauses.

                     elsif CodePeer_Mode then
                        null;

                     --  Omit check if component has a generic type. This can
                     --  happen in an instantiation within a generic in ASIS
                     --  mode, where we force freeze actions without full
                     --  expansion.

                     elsif Is_Generic_Type (Etype (Comp)) then
                        null;

                     --  Do the check

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
               end;
            end if;

            --  Gather data for possible Implicit_Packing later. Note that at
            --  this stage we might be dealing with a real component, or with
            --  an implicit subtype declaration.

            if Known_Static_RM_Size (Etype (Comp)) then
               Sized_Component_Total_RM_Size :=
                 Sized_Component_Total_RM_Size + RM_Size (Etype (Comp));

               if Is_Elementary_Type (Etype (Comp)) then
                  Elem_Component_Total_Esize :=
                    Elem_Component_Total_Esize + Esize (Etype (Comp));
               else
                  All_Elem_Components := False;

                  if RM_Size (Etype (Comp)) mod System_Storage_Unit /= 0 then
                     All_Storage_Unit_Components := False;
                  end if;
               end if;
            else
               All_Sized_Components := False;
            end if;

            --  If the component is an Itype with Delayed_Freeze and is either
            --  a record or array subtype and its base type has not yet been
            --  frozen, we must remove this from the entity list of this record
            --  and put it on the entity list of the scope of its base type.
            --  Note that we know that this is not the type of a component
            --  since we cleared Has_Delayed_Freeze for it in the previous
            --  loop. Thus this must be the Designated_Type of an access type,
            --  which is the type of a component.

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
                  --  We have a difficult case to handle here. Suppose Rec is
                  --  subtype being defined in a subprogram that's created as
                  --  part of the freezing of Rec'Base. In that case, we know
                  --  that Comp'Base must have already been frozen by the time
                  --  we get to elaborate this because Gigi doesn't elaborate
                  --  any bodies until it has elaborated all of the declarative
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

                     --  If component is pointer to a class-wide type, freeze
                     --  the specific type in the expression being allocated.
                     --  The expression may be a subtype indication, in which
                     --  case freeze the subtype mark.

                     if Is_Class_Wide_Type
                          (Designated_Type (Etype (Comp)))
                     then
                        if Is_Entity_Name (Expression (Alloc)) then
                           Freeze_And_Append
                             (Entity (Expression (Alloc)), N, Result);

                        elsif Nkind (Expression (Alloc)) = N_Subtype_Indication
                        then
                           Freeze_And_Append
                            (Entity (Subtype_Mark (Expression (Alloc))),
                             N, Result);
                        end if;

                     elsif Is_Itype (Designated_Type (Etype (Comp))) then
                        Check_Itype (Etype (Comp));

                     else
                        Freeze_And_Append
                          (Designated_Type (Etype (Comp)), N, Result);
                     end if;
                  end if;
               end;

            elsif Is_Access_Type (Etype (Comp))
              and then Is_Itype (Designated_Type (Etype (Comp)))
            then
               Check_Itype (Etype (Comp));

            --  Freeze the designated type when initializing a component with
            --  an aggregate in case the aggregate contains allocators.

            --     type T is ...;
            --     type T_Ptr is access all T;
            --     type T_Array is array ... of T_Ptr;

            --     type Rec is record
            --        Comp : T_Array := (others => ...);
            --     end record;

            elsif Is_Array_Type (Etype (Comp))
              and then Is_Access_Type (Component_Type (Etype (Comp)))
            then
               declare
                  Comp_Par  : constant Node_Id   := Parent (Comp);
                  Desig_Typ : constant Entity_Id :=
                                Designated_Type
                                  (Component_Type (Etype (Comp)));

               begin
                  --  The only case when this sort of freezing is not done is
                  --  when the designated type is class-wide and the root type
                  --  is the record owning the component. This scenario results
                  --  in a circularity because the class-wide type requires
                  --  primitives that have not been created yet as the root
                  --  type is in the process of being frozen.

                  --     type Rec is tagged;
                  --     type Rec_Ptr is access all Rec'Class;
                  --     type Rec_Array is array ... of Rec_Ptr;

                  --     type Rec is record
                  --        Comp : Rec_Array := (others => ...);
                  --     end record;

                  if Is_Class_Wide_Type (Desig_Typ)
                    and then Root_Type (Desig_Typ) = Rec
                  then
                     null;

                  elsif Is_Fully_Defined (Desig_Typ)
                    and then Present (Comp_Par)
                    and then Nkind (Comp_Par) = N_Component_Declaration
                    and then Present (Expression (Comp_Par))
                    and then Nkind (Expression (Comp_Par)) = N_Aggregate
                  then
                     Freeze_And_Append (Desig_Typ, N, Result);
                  end if;
               end;
            end if;

            Prev := Comp;
            Next_Entity (Comp);
         end loop;

         SSO_ADC :=
           Get_Attribute_Definition_Clause
             (Rec, Attribute_Scalar_Storage_Order);

         --  If the record type has Complex_Representation, then it is treated
         --  as a scalar in the back end so the storage order is irrelevant.

         if Has_Complex_Representation (Rec) then
            if Present (SSO_ADC) then
               Error_Msg_N
                 ("??storage order has no effect with Complex_Representation",
                  SSO_ADC);
            end if;

         else
            --  Deal with default setting of reverse storage order

            Set_SSO_From_Default (Rec);

            --  Check consistent attribute setting on component types

            declare
               Comp_ADC_Present : Boolean;
            begin
               Comp := First_Component (Rec);
               while Present (Comp) loop
                  Check_Component_Storage_Order
                    (Encl_Type        => Rec,
                     Comp             => Comp,
                     ADC              => SSO_ADC,
                     Comp_ADC_Present => Comp_ADC_Present);
                  SSO_ADC_Component := SSO_ADC_Component or Comp_ADC_Present;
                  Next_Component (Comp);
               end loop;
            end;

            --  Now deal with reverse storage order/bit order issues

            if Present (SSO_ADC) then

               --  Check compatibility of Scalar_Storage_Order with Bit_Order,
               --  if the former is specified.

               if Reverse_Bit_Order (Rec) /= Reverse_Storage_Order (Rec) then

                  --  Note: report error on Rec, not on SSO_ADC, as ADC may
                  --  apply to some ancestor type.

                  Error_Msg_Sloc := Sloc (SSO_ADC);
                  Error_Msg_N
                    ("scalar storage order for& specified# inconsistent with "
                     & "bit order", Rec);
               end if;

               --  Warn if there is a Scalar_Storage_Order attribute definition
               --  clause but no component clause, no component that itself has
               --  such an attribute definition, and no pragma Pack.

               if not (Placed_Component
                         or else
                       SSO_ADC_Component
                         or else
                       Is_Packed (Rec))
               then
                  Error_Msg_N
                    ("??scalar storage order specified but no component "
                     & "clause", SSO_ADC);
               end if;
            end if;
         end if;

         --  Deal with Bit_Order aspect

         ADC := Get_Attribute_Definition_Clause (Rec, Attribute_Bit_Order);

         if Present (ADC) and then Base_Type (Rec) = Rec then
            if not (Placed_Component
                     or else Present (SSO_ADC)
                     or else Is_Packed (Rec))
            then
               --  Warn if clause has no effect when no component clause is
               --  present, but suppress warning if the Bit_Order is required
               --  due to the presence of a Scalar_Storage_Order attribute.

               Error_Msg_N
                 ("??bit order specification has no effect", ADC);
               Error_Msg_N
                 ("\??since no component clauses were specified", ADC);

            --  Here is where we do the processing to adjust component clauses
            --  for reversed bit order, when not using reverse SSO.

            elsif Reverse_Bit_Order (Rec)
              and then not Reverse_Storage_Order (Rec)
            then
               Adjust_Record_For_Reverse_Bit_Order (Rec);

            --  Case where we have both an explicit Bit_Order and the same
            --  Scalar_Storage_Order: leave record untouched, the back-end
            --  will take care of required layout conversions.

            else
               null;

            end if;
         end if;

         --  Complete error checking on record representation clause (e.g.
         --  overlap of components). This is called after adjusting the
         --  record for reverse bit order.

         declare
            RRC : constant Node_Id := Get_Record_Representation_Clause (Rec);
         begin
            if Present (RRC) then
               Check_Record_Representation_Clause (RRC);
            end if;
         end;

         --  Set OK_To_Reorder_Components depending on debug flags

         if Is_Base_Type (Rec) and then Convention (Rec) = Convention_Ada then
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
               Error_Msg_N -- CODEFIX
                 ("??pragma Pack has no effect, no unplaced components",
                  Get_Rep_Pragma (Rec, Name_Pack));
            end if;
         end if;

         --  If this is the record corresponding to a remote type, freeze the
         --  remote type here since that is what we are semantically freezing.
         --  This prevents the freeze node for that type in an inner scope.

         if Ekind (Rec) = E_Record_Type then
            if Present (Corresponding_Remote_Type (Rec)) then
               Freeze_And_Append (Corresponding_Remote_Type (Rec), N, Result);
            end if;

            --  Check for controlled components and unchecked unions.

            Comp := First_Component (Rec);
            while Present (Comp) loop

               --  Do not set Has_Controlled_Component on a class-wide
               --  equivalent type. See Make_CW_Equivalent_Type.

               if not Is_Class_Wide_Equivalent_Type (Rec)
                 and then
                   (Has_Controlled_Component (Etype (Comp))
                     or else
                       (Chars (Comp) /= Name_uParent
                         and then Is_Controlled_Active (Etype (Comp)))
                     or else
                       (Is_Protected_Type (Etype (Comp))
                         and then
                           Present (Corresponding_Record_Type (Etype (Comp)))
                         and then
                           Has_Controlled_Component
                             (Corresponding_Record_Type (Etype (Comp)))))
               then
                  Set_Has_Controlled_Component (Rec);
               end if;

               if Has_Unchecked_Union (Etype (Comp)) then
                  Set_Has_Unchecked_Union (Rec);
               end if;

               --  Scan component declaration for likely misuses of current
               --  instance, either in a constraint or a default expression.

               if Has_Per_Object_Constraint (Comp) then
                  Check_Current_Instance (Parent (Comp));
               end if;

               Next_Component (Comp);
            end loop;
         end if;

         --  Enforce the restriction that access attributes with a current
         --  instance prefix can only apply to limited types. This comment
         --  is floating here, but does not seem to belong here???

         --  Set component alignment if not otherwise already set

         Set_Component_Alignment_If_Not_Set (Rec);

         --  For first subtypes, check if there are any fixed-point fields with
         --  component clauses, where we must check the size. This is not done
         --  till the freeze point since for fixed-point types, we do not know
         --  the size until the type is frozen. Similar processing applies to
         --  bit packed arrays.

         if Is_First_Subtype (Rec) then
            Comp := First_Component (Rec);
            while Present (Comp) loop
               if Present (Component_Clause (Comp))
                 and then (Is_Fixed_Point_Type (Etype (Comp))
                            or else Is_Bit_Packed_Array (Etype (Comp)))
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
                       ("?x?variant record has no direct equivalent in C",
                        A2);
                  else
                     Error_Msg_N
                       ("?x?variant record has no direct equivalent in C++",
                        A2);
                  end if;

                  Error_Msg_NE
                    ("\?x?use of convention for type& is dubious", A2, E);
               end if;
            end;
         end if;

         --  See if Size is too small as is (and implicit packing might help)

         if not Is_Packed (Rec)

           --  No implicit packing if even one component is explicitly placed

           and then not Placed_Component

           --  Or even one component is aliased

           and then not Aliased_Component

           --  Must have size clause and all sized components

           and then Has_Size_Clause (Rec)
           and then All_Sized_Components

           --  Do not try implicit packing on records with discriminants, too
           --  complicated, especially in the variant record case.

           and then not Has_Discriminants (Rec)

           --  We want to implicitly pack if the specified size of the record
           --  is less than the sum of the object sizes (no point in packing
           --  if this is not the case) if we can compute it, i.e. if we have
           --  only elementary components. Otherwise, we have at least one
           --  composite component and we want to implicit pack only if bit
           --  packing is required for it, as we are sure in this case that
           --  the back end cannot do the expected layout without packing.

           and then ((All_Elem_Components
                       and then RM_Size (Rec) < Elem_Component_Total_Esize)
                     or else (not All_Elem_Components
                               and then not All_Storage_Unit_Components))

           --  And the total RM size cannot be greater than the specified size
           --  since otherwise packing will not get us where we have to be.

           and then RM_Size (Rec) >= Sized_Component_Total_RM_Size

           --  Never do implicit packing in CodePeer or SPARK modes since
           --  we don't do any packing in these modes, since this generates
           --  over-complex code that confuses static analysis, and in
           --  general, neither CodePeer not GNATprove care about the
           --  internal representation of objects.

           and then not (CodePeer_Mode or GNATprove_Mode)
         then
            --  If implicit packing enabled, do it

            if Implicit_Packing then
               Set_Is_Packed (Rec);

               --  Otherwise flag the size clause

            else
               declare
                  Sz : constant Node_Id := Size_Clause (Rec);
               begin
                  Error_Msg_NE -- CODEFIX
                    ("size given for& too small", Sz, Rec);
                  Error_Msg_N -- CODEFIX
                    ("\use explicit pragma Pack "
                     & "or use pragma Implicit_Packing", Sz);
               end;
            end if;
         end if;

         --  The following checks are relevant only when SPARK_Mode is on as
         --  they are not standard Ada legality rules.

         if SPARK_Mode = On then
            if Is_Effectively_Volatile (Rec) then

               --  A discriminated type cannot be effectively volatile
               --  (SPARK RM C.6(4)).

               if Has_Discriminants (Rec) then
                  Error_Msg_N ("discriminated type & cannot be volatile", Rec);

               --  A tagged type cannot be effectively volatile
               --  (SPARK RM C.6(5)).

               elsif Is_Tagged_Type (Rec) then
                  Error_Msg_N ("tagged type & cannot be volatile", Rec);
               end if;

            --  A non-effectively volatile record type cannot contain
            --  effectively volatile components (SPARK RM C.6(2)).

            else
               Comp := First_Component (Rec);
               while Present (Comp) loop
                  if Comes_From_Source (Comp)
                    and then Is_Effectively_Volatile (Etype (Comp))
                  then
                     Error_Msg_Name_1 := Chars (Rec);
                     Error_Msg_N
                       ("component & of non-volatile type % cannot be "
                        & "volatile", Comp);
                  end if;

                  Next_Component (Comp);
               end loop;
            end if;

            --  A type which does not yield a synchronized object cannot have
            --  a component that yields a synchronized object (SPARK RM 9.5).

            if not Yields_Synchronized_Object (Rec) then
               Comp := First_Component (Rec);
               while Present (Comp) loop
                  if Comes_From_Source (Comp)
                    and then Yields_Synchronized_Object (Etype (Comp))
                  then
                     Error_Msg_Name_1 := Chars (Rec);
                     Error_Msg_N
                       ("component & of non-synchronized type % cannot be "
                        & "synchronized", Comp);
                  end if;

                  Next_Component (Comp);
               end loop;
            end if;

            --  A Ghost type cannot have a component of protected or task type
            --  (SPARK RM 6.9(19)).

            if Is_Ghost_Entity (Rec) then
               Comp := First_Component (Rec);
               while Present (Comp) loop
                  if Comes_From_Source (Comp)
                    and then Is_Concurrent_Type (Etype (Comp))
                  then
                     Error_Msg_Name_1 := Chars (Rec);
                     Error_Msg_N
                       ("component & of ghost type % cannot be concurrent",
                        Comp);
                  end if;

                  Next_Component (Comp);
               end loop;
            end if;
         end if;

         --  Make sure that if we have an iterator aspect, then we have
         --  either Constant_Indexing or Variable_Indexing.

         declare
            Iterator_Aspect : Node_Id;

         begin
            Iterator_Aspect := Find_Aspect (Rec, Aspect_Iterator_Element);

            if No (Iterator_Aspect) then
               Iterator_Aspect := Find_Aspect (Rec, Aspect_Default_Iterator);
            end if;

            if Present (Iterator_Aspect) then
               if Has_Aspect (Rec, Aspect_Constant_Indexing)
                    or else
                  Has_Aspect (Rec, Aspect_Variable_Indexing)
               then
                  null;
               else
                  Error_Msg_N
                    ("Iterator_Element requires indexing aspect",
                     Iterator_Aspect);
               end if;
            end if;
         end;

         --  All done if not a full record definition

         if Ekind (Rec) /= E_Record_Type then
            return;
         end if;

         --  Finally we need to check the variant part to make sure that
         --  all types within choices are properly frozen as part of the
         --  freezing of the record type.

         Check_Variant_Part : declare
            D : constant Node_Id := Declaration_Node (Rec);
            T : Node_Id;
            C : Node_Id;

         begin
            --  Find component list

            C := Empty;

            if Nkind (D) = N_Full_Type_Declaration then
               T := Type_Definition (D);

               if Nkind (T) = N_Record_Definition then
                  C := Component_List (T);

               elsif Nkind (T) = N_Derived_Type_Definition
                 and then Present (Record_Extension_Part (T))
               then
                  C := Component_List (Record_Extension_Part (T));
               end if;
            end if;

            --  Case of variant part present

            if Present (C) and then Present (Variant_Part (C)) then
               Freeze_Choices_In_Variant_Part (Variant_Part (C));
            end if;

            --  Note: we used to call Check_Choices here, but it is too early,
            --  since predicated subtypes are frozen here, but their freezing
            --  actions are in Analyze_Freeze_Entity, which has not been called
            --  yet for entities frozen within this procedure, so we moved that
            --  call to the Analyze_Freeze_Entity for the record type.

         end Check_Variant_Part;

         --  Check that all the primitives of an interface type are abstract
         --  or null procedures.

         if Is_Interface (Rec)
           and then not Error_Posted (Parent (Rec))
         then
            declare
               Elmt : Elmt_Id;
               Subp : Entity_Id;

            begin
               Elmt := First_Elmt (Primitive_Operations (Rec));
               while Present (Elmt) loop
                  Subp := Node (Elmt);

                  if not Is_Abstract_Subprogram (Subp)

                     --  Avoid reporting the error on inherited primitives

                    and then Comes_From_Source (Subp)
                  then
                     Error_Msg_Name_1 := Chars (Subp);

                     if Ekind (Subp) = E_Procedure then
                        if not Null_Present (Parent (Subp)) then
                           Error_Msg_N
                             ("interface procedure % must be abstract or null",
                              Parent (Subp));
                        end if;
                     else
                        Error_Msg_N
                          ("interface function % must be abstract",
                           Parent (Subp));
                     end if;
                  end if;

                  Next_Elmt (Elmt);
               end loop;
            end;
         end if;
      end Freeze_Record_Type;

      -------------------------------
      -- Has_Boolean_Aspect_Import --
      -------------------------------

      function Has_Boolean_Aspect_Import (E : Entity_Id) return Boolean is
         Decl : constant Node_Id := Declaration_Node (E);
         Asp  : Node_Id;
         Expr : Node_Id;

      begin
         if Has_Aspects (Decl) then
            Asp := First (Aspect_Specifications (Decl));
            while Present (Asp) loop
               Expr := Expression (Asp);

               --  The value of aspect Import is True when the expression is
               --  either missing or it is explicitly set to True.

               if Get_Aspect_Id (Asp) = Aspect_Import
                 and then (No (Expr)
                            or else (Compile_Time_Known_Value (Expr)
                                      and then Is_True (Expr_Value (Expr))))
               then
                  return True;
               end if;

               Next (Asp);
            end loop;
         end if;

         return False;
      end Has_Boolean_Aspect_Import;

      ---------------------
      -- New_Freeze_Node --
      ---------------------

      function New_Freeze_Node return Node_Id is
         Save_Ghost_Mode : constant Ghost_Mode_Type := Ghost_Mode;
         Result          : Node_Id;

      begin
         --  Handle the case where an ignored Ghost subprogram freezes the type
         --  of one of its formals. The type can either be non-Ghost or checked
         --  Ghost. Since the freeze node for the type is generated in the
         --  context of the subprogram, the node will be incorrectly flagged as
         --  ignored Ghost and erroneously removed from the tree.

         --    type Typ is ...;
         --    procedure Ignored_Ghost_Proc (Formal : Typ) with Ghost;

         --  Reset the Ghost mode to "none". This preserves the freeze node.

         if Ghost_Mode = Ignore
           and then not Is_Ignored_Ghost_Entity (E)
           and then not Is_Ignored_Ghost_Node (E)
         then
            Ghost_Mode := None;
         end if;

         Result := New_Node (N_Freeze_Entity, Loc);

         Ghost_Mode := Save_Ghost_Mode;
         return Result;
      end New_Freeze_Node;

      ------------------------------
      -- Wrap_Imported_Subprogram --
      ------------------------------

      --  The issue here is that our normal approach of checking preconditions
      --  and postconditions does not work for imported procedures, since we
      --  are not generating code for the body. To get around this we create
      --  a wrapper, as shown by the following example:

      --    procedure K (A : Integer);
      --    pragma Import (C, K);

      --  The spec is rewritten by removing the effects of pragma Import, but
      --  leaving the convention unchanged, as though the source had said:

      --    procedure K (A : Integer);
      --    pragma Convention (C, K);

      --  and we create a body, added to the entity K freeze actions, which
      --  looks like:

      --    procedure K (A : Integer) is
      --       procedure K (A : Integer);
      --       pragma Import (C, K);
      --    begin
      --       K (A);
      --    end K;

      --  Now the contract applies in the normal way to the outer procedure,
      --  and the inner procedure has no contracts, so there is no problem
      --  in just calling it to get the original effect.

      --  In the case of a function, we create an appropriate return statement
      --  for the subprogram body that calls the inner procedure.

      procedure Wrap_Imported_Subprogram (E : Entity_Id) is
         function Copy_Import_Pragma return Node_Id;
         --  Obtain a copy of the Import_Pragma which belongs to subprogram E

         ------------------------
         -- Copy_Import_Pragma --
         ------------------------

         function Copy_Import_Pragma return Node_Id is

            --  The subprogram should have an import pragma, otherwise it does
            --  need a wrapper.

            Prag : constant Node_Id := Import_Pragma (E);
            pragma Assert (Present (Prag));

            --  Save all semantic fields of the pragma

            Save_Asp  : constant Node_Id := Corresponding_Aspect (Prag);
            Save_From : constant Boolean := From_Aspect_Specification (Prag);
            Save_Prag : constant Node_Id := Next_Pragma (Prag);
            Save_Rep  : constant Node_Id := Next_Rep_Item (Prag);

            Result : Node_Id;

         begin
            --  Reset all semantic fields. This avoids a potential infinite
            --  loop when the pragma comes from an aspect as the duplication
            --  will copy the aspect, then copy the corresponding pragma and
            --  so on.

            Set_Corresponding_Aspect      (Prag, Empty);
            Set_From_Aspect_Specification (Prag, False);
            Set_Next_Pragma               (Prag, Empty);
            Set_Next_Rep_Item             (Prag, Empty);

            Result := Copy_Separate_Tree (Prag);

            --  Restore the original semantic fields

            Set_Corresponding_Aspect      (Prag, Save_Asp);
            Set_From_Aspect_Specification (Prag, Save_From);
            Set_Next_Pragma               (Prag, Save_Prag);
            Set_Next_Rep_Item             (Prag, Save_Rep);

            return Result;
         end Copy_Import_Pragma;

         --  Local variables

         Loc   : constant Source_Ptr := Sloc (E);
         CE    : constant Name_Id    := Chars (E);
         Bod   : Node_Id;
         Forml : Entity_Id;
         Parms : List_Id;
         Prag  : Node_Id;
         Spec  : Node_Id;
         Stmt  : Node_Id;

      --  Start of processing for Wrap_Imported_Subprogram

      begin
         --  Nothing to do if not imported

         if not Is_Imported (E) then
            return;

         --  Test enabling conditions for wrapping

         elsif Is_Subprogram (E)
           and then Present (Contract (E))
           and then Present (Pre_Post_Conditions (Contract (E)))
           and then not GNATprove_Mode
         then
            --  Here we do the wrap

            --  Note on calls to Copy_Separate_Tree. The trees we are copying
            --  here are fully analyzed, but we definitely want fully syntactic
            --  unanalyzed trees in the body we construct, so that the analysis
            --  generates the right visibility, and that is exactly what the
            --  calls to Copy_Separate_Tree give us.

            Prag := Copy_Import_Pragma;

            --  Fix up spec to be not imported any more

            Set_Has_Completion (E, False);
            Set_Import_Pragma  (E, Empty);
            Set_Interface_Name (E, Empty);
            Set_Is_Imported    (E, False);

            --  Grab the subprogram declaration and specification

            Spec := Declaration_Node (E);

            --  Build parameter list that we need

            Parms := New_List;
            Forml := First_Formal (E);
            while Present (Forml) loop
               Append_To (Parms, Make_Identifier (Loc, Chars (Forml)));
               Next_Formal (Forml);
            end loop;

            --  Build the call

            if Ekind_In (E, E_Function, E_Generic_Function) then
               Stmt :=
                 Make_Simple_Return_Statement (Loc,
                   Expression =>
                     Make_Function_Call (Loc,
                       Name                   => Make_Identifier (Loc, CE),
                       Parameter_Associations => Parms));

            else
               Stmt :=
                 Make_Procedure_Call_Statement (Loc,
                   Name                   => Make_Identifier (Loc, CE),
                   Parameter_Associations => Parms);
            end if;

            --  Now build the body

            Bod :=
              Make_Subprogram_Body (Loc,
                Specification              =>
                  Copy_Separate_Tree (Spec),
                Declarations               => New_List (
                  Make_Subprogram_Declaration (Loc,
                    Specification => Copy_Separate_Tree (Spec)),
                  Prag),
                Handled_Statement_Sequence =>
                  Make_Handled_Sequence_Of_Statements (Loc,
                    Statements => New_List (Stmt),
                    End_Label  => Make_Identifier (Loc, CE)));

            --  Append the body to freeze result

            Add_To_Result (Bod);
            return;

         --  Case of imported subprogram that does not get wrapped

         else
            --  Set Is_Public. All imported entities need an external symbol
            --  created for them since they are always referenced from another
            --  object file. Note this used to be set when we set Is_Imported
            --  back in Sem_Prag, but now we delay it to this point, since we
            --  don't want to set this flag if we wrap an imported subprogram.

            Set_Is_Public (E);
         end if;
      end Wrap_Imported_Subprogram;

      --  Local variables

      Save_Ghost_Mode : constant Ghost_Mode_Type := Ghost_Mode;

   --  Start of processing for Freeze_Entity

   begin
      --  The entity being frozen may be subject to pragma Ghost. Set the mode
      --  now to ensure that any nodes generated during freezing are properly
      --  flagged as Ghost.

      Set_Ghost_Mode_From_Entity (E);

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
         Ghost_Mode := Save_Ghost_Mode;
         return No_List;

      --  It is improper to freeze an external entity within a generic because
      --  its freeze node will appear in a non-valid context. The entity will
      --  be frozen in the proper scope after the current generic is analyzed.
      --  However, aspects must be analyzed because they may be queried later
      --  within the generic itself, and the corresponding pragma or attribute
      --  definition has not been analyzed yet.

      elsif Inside_A_Generic and then External_Ref_In_Generic (Test_E) then
         if Has_Delayed_Aspects (E) then
            Analyze_Aspects_At_Freeze_Point (E);
         end if;

         Ghost_Mode := Save_Ghost_Mode;
         return No_List;

      --  AI05-0213: A formal incomplete type does not freeze the actual. In
      --  the instance, the same applies to the subtype renaming the actual.

      elsif Is_Private_Type (E)
        and then Is_Generic_Actual_Type (E)
        and then No (Full_View (Base_Type (E)))
        and then Ada_Version >= Ada_2012
      then
         Ghost_Mode := Save_Ghost_Mode;
         return No_List;

      --  Formal subprograms are never frozen

      elsif Is_Formal_Subprogram (E) then
         Ghost_Mode := Save_Ghost_Mode;
         return No_List;

      --  Generic types are never frozen as they lack delayed semantic checks

      elsif Is_Generic_Type (E) then
         Ghost_Mode := Save_Ghost_Mode;
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
            S : Entity_Id;

         begin
            S := Current_Scope;
            while Present (S) loop
               if Is_Overloadable (S) then
                  if Comes_From_Source (S)
                    or else Is_Generic_Instance (S)
                    or else Is_Child_Unit (S)
                  then
                     exit;
                  else
                     Ghost_Mode := Save_Ghost_Mode;
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
            S : Entity_Id;

         begin
            S := Scope (Test_E);
            while Present (S) loop
               if Is_Generic_Instance (S) then
                  exit;
               else
                  S := Scope (S);
               end if;
            end loop;

            if No (S) then
               Ghost_Mode := Save_Ghost_Mode;
               return No_List;
            end if;
         end;

      elsif Ekind (E) = E_Generic_Package then
         Result := Freeze_Generic_Entities (E);

         Ghost_Mode := Save_Ghost_Mode;
         return Result;
      end if;

      --  Add checks to detect proper initialization of scalars that may appear
      --  as subprogram parameters.

      if Is_Subprogram (E) and then Check_Validity_Of_Parameters then
         Apply_Parameter_Validity_Checks (E);
      end if;

      --  Deal with delayed aspect specifications. The analysis of the aspect
      --  is required to be delayed to the freeze point, thus we analyze the
      --  pragma or attribute definition clause in the tree at this point. We
      --  also analyze the aspect specification node at the freeze point when
      --  the aspect doesn't correspond to pragma/attribute definition clause.

      if Has_Delayed_Aspects (E) then
         Analyze_Aspects_At_Freeze_Point (E);
      end if;

      --  Here to freeze the entity

      Set_Is_Frozen (E);

      --  Case of entity being frozen is other than a type

      if not Is_Type (E) then

         --  If entity is exported or imported and does not have an external
         --  name, now is the time to provide the appropriate default name.
         --  Skip this if the entity is stubbed, since we don't need a name
         --  for any stubbed routine. For the case on intrinsics, if no
         --  external name is specified, then calls will be handled in
         --  Exp_Intr.Expand_Intrinsic_Call, and no name is needed. If an
         --  external name is provided, then Expand_Intrinsic_Call leaves
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

         elsif Is_Atomic_Or_VFA (E)
           and then Nkind (Parent (E)) = N_Object_Declaration
           and then Present (Expression (Parent (E)))
           and then Nkind (Expression (Parent (E))) = N_Aggregate
           and then Is_Atomic_VFA_Aggregate (Expression (Parent (E)))
         then
            null;
         end if;

         --  Subprogram case

         if Is_Subprogram (E) then

            --  Check for needing to wrap imported subprogram

            Wrap_Imported_Subprogram (E);

            --  Freeze all parameter types and the return type (RM 13.14(14)).
            --  However skip this for internal subprograms. This is also where
            --  any extra formal parameters are created since we now know
            --  whether the subprogram will use a foreign convention.

            --  In Ada 2012, freezing a subprogram does not always freeze the
            --  corresponding profile (see AI05-019). An attribute reference
            --  is not a freezing point of the profile. Flag Do_Freeze_Profile
            --  indicates whether the profile should be frozen now.
            --  Other constructs that should not freeze ???

            --  This processing doesn't apply to internal entities (see below)

            --  Disable this mechanism for now, to fix regressions in ASIS and
            --  various ACATS tests. Implementation of AI05-019 remains
            --  unsolved ???

            if not Is_Internal (E)
              and then (Do_Freeze_Profile or else True)
            then
               if not Freeze_Profile (E) then
                  Ghost_Mode := Save_Ghost_Mode;
                  return Result;
               end if;
            end if;

            --  Must freeze its parent first if it is a derived subprogram

            if Present (Alias (E)) then
               Freeze_And_Append (Alias (E), N, Result);
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

            --  If warning on suspicious contracts then check for the case of
            --  a postcondition other than False for a No_Return subprogram.

            if No_Return (E)
              and then Warn_On_Suspicious_Contract
              and then Present (Contract (E))
            then
               declare
                  Prag : Node_Id := Pre_Post_Conditions (Contract (E));
                  Exp  : Node_Id;

               begin
                  while Present (Prag) loop
                     if Nam_In (Pragma_Name (Prag), Name_Post,
                                                    Name_Postcondition,
                                                    Name_Refined_Post)
                     then
                        Exp :=
                          Expression
                            (First (Pragma_Argument_Associations (Prag)));

                        if Nkind (Exp) /= N_Identifier
                          or else Chars (Exp) /= Name_False
                        then
                           Error_Msg_NE
                             ("useless postcondition, & is marked "
                              & "No_Return?T?", Exp, E);
                        end if;
                     end if;

                     Prag := Next_Pragma (Prag);
                  end loop;
               end;
            end if;

         --  Here for other than a subprogram or type

         else
            --  If entity has a type, and it is not a generic unit, then
            --  freeze it first (RM 13.14(10)).

            if Present (Etype (E))
              and then Ekind (E) /= E_Generic_Function
            then
               Freeze_And_Append (Etype (E), N, Result);

               --  For an object of an anonymous array type, aspects on the
               --  object declaration apply to the type itself. This is the
               --  case for Atomic_Components, Volatile_Components, and
               --  Independent_Components. In these cases analysis of the
               --  generated pragma will mark the anonymous types accordingly,
               --  and the object itself does not require a freeze node.

               if Ekind (E) = E_Variable
                 and then Is_Itype (Etype (E))
                 and then Is_Array_Type (Etype (E))
                 and then Has_Delayed_Aspects (E)
               then
                  Set_Has_Delayed_Aspects (E, False);
                  Set_Has_Delayed_Freeze (E, False);
                  Set_Freeze_Node (E, Empty);
               end if;
            end if;

            --  Special processing for objects created by object declaration

            if Nkind (Declaration_Node (E)) = N_Object_Declaration then
               Freeze_Object_Declaration (E);
            end if;

            --  Check that a constant which has a pragma Volatile[_Components]
            --  or Atomic[_Components] also has a pragma Import (RM C.6(13)).

            --  Note: Atomic[_Components] also sets Volatile[_Components]

            if Ekind (E) = E_Constant
              and then (Has_Volatile_Components (E) or else Is_Volatile (E))
              and then not Is_Imported (E)
              and then not Has_Boolean_Aspect_Import (E)
            then
               --  Make sure we actually have a pragma, and have not merely
               --  inherited the indication from elsewhere (e.g. an address
               --  clause, which is not good enough in RM terms).

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

            if Ekind_In (E, E_Variable, E_Constant, E_Loop_Parameter)
              or else Is_Formal (E)
            then
               Layout_Object (E);
            end if;

            --  For an object that does not have delayed freezing, and whose
            --  initialization actions have been captured in a compound
            --  statement, move them back now directly within the enclosing
            --  statement sequence.

            if Ekind_In (E, E_Constant, E_Variable)
              and then not Has_Delayed_Freeze (E)
            then
               Explode_Initialization_Compound_Statement (E);
            end if;
         end if;

      --  Case of a type or subtype being frozen

      else
         --  We used to check here that a full type must have preelaborable
         --  initialization if it completes a private type specified with
         --  pragma Preelaborable_Initialization, but that missed cases where
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

         --  In Ada 2012, Freeze_Entities is also used in the front end to
         --  trigger the analysis of aspect expressions, so in this case we
         --  want to continue the freezing process.

         if Present (Scope (E))
           and then Is_Generic_Unit (Scope (E))
           and then
             (not Has_Predicates (E)
               and then not Has_Delayed_Freeze (E))
         then
            Check_Compile_Time_Size (E);
            Ghost_Mode := Save_Ghost_Mode;
            return No_List;
         end if;

         --  Check for error of Type_Invariant'Class applied to an untagged
         --  type (check delayed to freeze time when full type is available).

         declare
            Prag : constant Node_Id := Get_Pragma (E, Pragma_Invariant);
         begin
            if Present (Prag)
              and then Class_Present (Prag)
              and then not Is_Tagged_Type (E)
            then
               Error_Msg_NE
                 ("Type_Invariant''Class cannot be specified for &",
                  Prag, E);
               Error_Msg_N
                 ("\can only be specified for a tagged type", Prag);
            end if;
         end;

         if Is_Ghost_Entity (E) then

            --  A Ghost type cannot be concurrent (SPARK RM 6.9(19)). Verify
            --  this legality rule first to five a finer-grained diagnostic.

            if Is_Concurrent_Type (E) then
               Error_Msg_N ("ghost type & cannot be concurrent", E);

            --  A Ghost type cannot be effectively volatile (SPARK RM 6.9(7))

            elsif Is_Effectively_Volatile (E) then
               Error_Msg_N ("ghost type & cannot be volatile", E);
            end if;
         end if;

         --  Deal with special cases of freezing for subtype

         if E /= Base_Type (E) then

            --  Before we do anything else, a specific test for the case of a
            --  size given for an array where the array would need to be packed
            --  in order for the size to be honored, but is not. This is the
            --  case where implicit packing may apply. The reason we do this so
            --  early is that, if we have implicit packing, the layout of the
            --  base type is affected, so we must do this before we freeze the
            --  base type.

            --  We could do this processing only if implicit packing is enabled
            --  since in all other cases, the error would be caught by the back
            --  end. However, we choose to do the check even if we do not have
            --  implicit packing enabled, since this allows us to give a more
            --  useful error message (advising use of pragma Implicit_Packing
            --  or pragma Pack).

            if Is_Array_Type (E) then
               declare
                  Ctyp : constant Entity_Id := Component_Type (E);
                  Rsiz : constant Uint      := RM_Size (Ctyp);
                  SZ   : constant Node_Id   := Size_Clause (E);
                  Btyp : constant Entity_Id := Base_Type (E);

                  Lo   : Node_Id;
                  Hi   : Node_Id;
                  Indx : Node_Id;

                  Dim       : Uint;
                  Num_Elmts : Uint := Uint_1;
                  --  Number of elements in array

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

                  --  The conditions on the size are identical to those used in
                  --  Freeze_Array_Type to set the Is_Packed flag.

                  if Has_Size_Clause (E)
                    and then Known_Static_RM_Size (E)
                    and then not Is_Packed (E)
                    and then not Has_Pragma_Pack (E)
                    and then not Has_Component_Size_Clause (E)
                    and then Known_Static_RM_Size (Ctyp)
                    and then Rsiz <= 64
                    and then not (Addressable (Rsiz)
                                   and then Known_Static_Esize (Ctyp)
                                   and then Esize (Ctyp) = Rsiz)
                    and then not (Rsiz mod System_Storage_Unit = 0
                                   and then Is_Composite_Type (Ctyp))
                    and then not Is_Limited_Composite (E)
                    and then not Is_Packed (Root_Type (E))
                    and then not Has_Component_Size_Clause (Root_Type (E))
                    and then not (CodePeer_Mode or GNATprove_Mode)
                  then
                     --  Compute number of elements in array

                     Indx := First_Index (E);
                     while Present (Indx) loop
                        Get_Index_Bounds (Indx, Lo, Hi);

                        if not (Compile_Time_Known_Value (Lo)
                                  and then
                                Compile_Time_Known_Value (Hi))
                        then
                           goto No_Implicit_Packing;
                        end if;

                        Dim := Expr_Value (Hi) - Expr_Value (Lo) + 1;

                        if Dim >= 0 then
                           Num_Elmts := Num_Elmts * Dim;
                        else
                           Num_Elmts := Uint_0;
                        end if;

                        Next_Index (Indx);
                     end loop;

                     --  What we are looking for here is the situation where
                     --  the RM_Size given would be exactly right if there was
                     --  a pragma Pack, resulting in the component size being
                     --  the RM_Size of the component type.

                     if RM_Size (E) = Num_Elmts * Rsiz then
                        --  For implicit packing mode, just set the component
                        --  size and Freeze_Array_Type will do the rest.

                        if Implicit_Packing then
                           Set_Component_Size (Btyp, Rsiz);

                           --  Otherwise give an error message

                        else
                           Error_Msg_NE
                             ("size given for& too small", SZ, E);
                           Error_Msg_N -- CODEFIX
                             ("\use explicit pragma Pack "
                              & "or use pragma Implicit_Packing", SZ);
                        end if;
                     end if;
                  end if;
               end;
            end if;

            <<No_Implicit_Packing>>

            --  If ancestor subtype present, freeze that first. Note that this
            --  will also get the base type frozen. Need RM reference ???

            Atype := Ancestor_Subtype (E);

            if Present (Atype) then
               Freeze_And_Append (Atype, N, Result);

            --  No ancestor subtype present

            else
               --  See if we have a nearest ancestor that has a predicate.
               --  That catches the case of derived type with a predicate.
               --  Need RM reference here ???

               Atype := Nearest_Ancestor (E);

               if Present (Atype) and then Has_Predicates (Atype) then
                  Freeze_And_Append (Atype, N, Result);
               end if;

               --  Freeze base type before freezing the entity (RM 13.14(15))

               if E /= Base_Type (E) then
                  Freeze_And_Append (Base_Type (E), N, Result);
               end if;
            end if;

            --  A subtype inherits all the type-related representation aspects
            --  from its parents (RM 13.1(8)).

            Inherit_Aspects_At_Freeze_Point (E);

         --  For a derived type, freeze its parent type first (RM 13.14(15))

         elsif Is_Derived_Type (E) then
            Freeze_And_Append (Etype (E), N, Result);
            Freeze_And_Append (First_Subtype (Etype (E)), N, Result);

            --  A derived type inherits each type-related representation aspect
            --  of its parent type that was directly specified before the
            --  declaration of the derived type (RM 13.1(15)).

            Inherit_Aspects_At_Freeze_Point (E);
         end if;

         --  Check for incompatible size and alignment for record type

         if Warn_On_Size_Alignment
           and then Is_Record_Type (E)
           and then Has_Size_Clause (E) and then Has_Alignment_Clause (E)

           --  If explicit Object_Size clause given assume that the programmer
           --  knows what he is doing, and expects the compiler behavior.

           and then not Has_Object_Size_Clause (E)

           --  Check for size not a multiple of alignment

           and then RM_Size (E) mod (Alignment (E) * System_Storage_Unit) /= 0
         then
            declare
               SC    : constant Node_Id := Size_Clause (E);
               AC    : constant Node_Id := Alignment_Clause (E);
               Loc   : Node_Id;
               Abits : constant Uint := Alignment (E) * System_Storage_Unit;

            begin
               if Present (SC) and then Present (AC) then

                  --  Give a warning

                  if Sloc (SC) > Sloc (AC) then
                     Loc := SC;
                     Error_Msg_NE
                       ("?Z?size is not a multiple of alignment for &",
                        Loc, E);
                     Error_Msg_Sloc := Sloc (AC);
                     Error_Msg_Uint_1 := Alignment (E);
                     Error_Msg_N ("\?Z?alignment of ^ specified #", Loc);

                  else
                     Loc := AC;
                     Error_Msg_NE
                       ("?Z?size is not a multiple of alignment for &",
                        Loc, E);
                     Error_Msg_Sloc := Sloc (SC);
                     Error_Msg_Uint_1 := RM_Size (E);
                     Error_Msg_N ("\?Z?size of ^ specified #", Loc);
                  end if;

                  Error_Msg_Uint_1 := ((RM_Size (E) / Abits) + 1) * Abits;
                  Error_Msg_N ("\?Z?Object_Size will be increased to ^", Loc);
               end if;
            end;
         end if;

         --  Array type

         if Is_Array_Type (E) then
            Freeze_Array_Type (E);

         --  For a class-wide type, the corresponding specific type is
         --  frozen as well (RM 13.14(15))

         elsif Is_Class_Wide_Type (E) then
            Freeze_And_Append (Root_Type (E), N, Result);

            --  If the base type of the class-wide type is still incomplete,
            --  the class-wide remains unfrozen as well. This is legal when
            --  E is the formal of a primitive operation of some other type
            --  which is being frozen.

            if not Is_Frozen (Root_Type (E)) then
               Set_Is_Frozen (E, False);
               Ghost_Mode := Save_Ghost_Mode;
               return Result;
            end if;

            --  The equivalent type associated with a class-wide subtype needs
            --  to be frozen to ensure that its layout is done.

            if Ekind (E) = E_Class_Wide_Subtype
              and then Present (Equivalent_Type (E))
            then
               Freeze_And_Append (Equivalent_Type (E), N, Result);
            end if;

            --  Generate an itype reference for a library-level class-wide type
            --  at the freeze point. Otherwise the first explicit reference to
            --  the type may appear in an inner scope which will be rejected by
            --  the back-end.

            if Is_Itype (E)
              and then Is_Compilation_Unit (Scope (E))
            then
               declare
                  Ref : constant Node_Id := Make_Itype_Reference (Loc);

               begin
                  Set_Itype (Ref, E);

                  --  From a gigi point of view, a class-wide subtype derives
                  --  from its record equivalent type. As a result, the itype
                  --  reference must appear after the freeze node of the
                  --  equivalent type or gigi will reject the reference.

                  if Ekind (E) = E_Class_Wide_Subtype
                    and then Present (Equivalent_Type (E))
                  then
                     Insert_After (Freeze_Node (Equivalent_Type (E)), Ref);
                  else
                     Add_To_Result (Ref);
                  end if;
               end;
            end if;

         --  For a record type or record subtype, freeze all component types
         --  (RM 13.14(15)). We test for E_Record_(sub)Type here, rather than
         --  using Is_Record_Type, because we don't want to attempt the freeze
         --  for the case of a private type with record extension (we will do
         --  that later when the full type is frozen).

         elsif Ekind_In (E, E_Record_Type, E_Record_Subtype)
           and then not (Present (Scope (E))
                          and then Is_Generic_Unit (Scope (E)))
         then
            Freeze_Record_Type (E);

         --  For a concurrent type, freeze corresponding record type. This does
         --  not correspond to any specific rule in the RM, but the record type
         --  is essentially part of the concurrent type. Also freeze all local
         --  entities. This includes record types created for entry parameter
         --  blocks and whatever local entities may appear in the private part.

         elsif Is_Concurrent_Type (E) then
            if Present (Corresponding_Record_Type (E)) then
               Freeze_And_Append (Corresponding_Record_Type (E), N, Result);
            end if;

            Comp := First_Entity (E);
            while Present (Comp) loop
               if Is_Type (Comp) then
                  Freeze_And_Append (Comp, N, Result);

               elsif (Ekind (Comp)) /= E_Function then

                  --  The guard on the presence of the Etype seems to be needed
                  --  for some CodePeer (-gnatcC) cases, but not clear why???

                  if Present (Etype (Comp)) then
                     if Is_Itype (Etype (Comp))
                       and then Underlying_Type (Scope (Etype (Comp))) = E
                     then
                        Undelay_Type (Etype (Comp));
                     end if;

                     Freeze_And_Append (Etype (Comp), N, Result);
                  end if;
               end if;

               Next_Entity (Comp);
            end loop;

         --  Private types are required to point to the same freeze node as
         --  their corresponding full views. The freeze node itself has to
         --  point to the partial view of the entity (because from the partial
         --  view, we can retrieve the full view, but not the reverse).
         --  However, in order to freeze correctly, we need to freeze the full
         --  view. If we are freezing at the end of a scope (or within the
         --  scope) of the private type, the partial and full views will have
         --  been swapped, the full view appears first in the entity chain and
         --  the swapping mechanism ensures that the pointers are properly set
         --  (on scope exit).

         --  If we encounter the partial view before the full view (e.g. when
         --  freezing from another scope), we freeze the full view, and then
         --  set the pointers appropriately since we cannot rely on swapping to
         --  fix things up (subtypes in an outer scope might not get swapped).

         --  If the full view is itself private, the above requirements apply
         --  to the underlying full view instead of the full view. But there is
         --  no swapping mechanism for the underlying full view so we need to
         --  set the pointers appropriately in both cases.

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
            --        procedure Prim (X : in out T; Y : in out DT'Class);
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
               Ghost_Mode := Save_Ghost_Mode;
               return Result;

            --  Case of full view present

            elsif Present (Full_View (E)) then

               --  If full view has already been frozen, then no further
               --  processing is required

               if Is_Frozen (Full_View (E)) then
                  Set_Has_Delayed_Freeze (E, False);
                  Set_Freeze_Node (E, Empty);

               --  Otherwise freeze full view and patch the pointers so that
               --  the freeze node will elaborate both views in the back end.
               --  However, if full view is itself private, freeze underlying
               --  full view instead and patch the pointers so that the freeze
               --  node will elaborate the three views in the back end.

               else
                  declare
                     Full : Entity_Id := Full_View (E);

                  begin
                     if Is_Private_Type (Full)
                       and then Present (Underlying_Full_View (Full))
                     then
                        Full := Underlying_Full_View (Full);
                     end if;

                     Freeze_And_Append (Full, N, Result);

                     if Full /= Full_View (E)
                       and then Has_Delayed_Freeze (Full_View (E))
                     then
                        F_Node := Freeze_Node (Full);

                        if Present (F_Node) then
                           Set_Freeze_Node (Full_View (E), F_Node);
                           Set_Entity (F_Node, Full_View (E));

                        else
                           Set_Has_Delayed_Freeze (Full_View (E), False);
                           Set_Freeze_Node (Full_View (E), Empty);
                        end if;
                     end if;

                     if Has_Delayed_Freeze (E) then
                        F_Node := Freeze_Node (Full_View (E));

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
               end if;

               Check_Debug_Info_Needed (E);

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

               Ghost_Mode := Save_Ghost_Mode;
               return Result;

            --  Case of underlying full view present

            elsif Is_Private_Type (E)
              and then Present (Underlying_Full_View (E))
            then
               if not Is_Frozen (Underlying_Full_View (E)) then
                  Freeze_And_Append (Underlying_Full_View (E), N, Result);
               end if;

               --  Patch the pointers so that the freeze node will elaborate
               --  both views in the back end.

               if Has_Delayed_Freeze (E) then
                  F_Node := Freeze_Node (Underlying_Full_View (E));

                  if Present (F_Node) then
                     Set_Freeze_Node (E, F_Node);
                     Set_Entity (F_Node, E);

                  else
                     Set_Has_Delayed_Freeze (E, False);
                     Set_Freeze_Node (E, Empty);
                  end if;
               end if;

               Check_Debug_Info_Needed (E);

               Ghost_Mode := Save_Ghost_Mode;
               return Result;

            --  Case of no full view present. If entity is derived or subtype,
            --  it is safe to freeze, correctness depends on the frozen status
            --  of parent. Otherwise it is either premature usage, or a Taft
            --  amendment type, so diagnosis is at the point of use and the
            --  type might be frozen later.

            elsif E /= Base_Type (E) or else Is_Derived_Type (E) then
               null;

            else
               Set_Is_Frozen (E, False);
               Ghost_Mode := Save_Ghost_Mode;
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
               then
                  if Is_Tagged_Type (Etype (Formal)) then
                     null;

                  --  AI05-151: Incomplete types are allowed in access to
                  --  subprogram specifications.

                  elsif Ada_Version < Ada_2012 then
                     Error_Msg_NE
                       ("invalid use of incomplete type&", E, Etype (Formal));
                  end if;
               end if;

               Freeze_And_Append (Etype (Formal), N, Result);
               Next_Formal (Formal);
            end loop;

            Freeze_Subprogram (E);

         --  For access to a protected subprogram, freeze the equivalent type
         --  (however this is not set if we are not generating code or if this
         --  is an anonymous type used just for resolution).

         elsif Is_Access_Protected_Subprogram_Type (E) then
            if Present (Equivalent_Type (E)) then
               Freeze_And_Append (Equivalent_Type (E), N, Result);
            end if;
         end if;

         --  Generic types are never seen by the back-end, and are also not
         --  processed by the expander (since the expander is turned off for
         --  generic processing), so we never need freeze nodes for them.

         if Is_Generic_Type (E) then
            Ghost_Mode := Save_Ghost_Mode;
            return Result;
         end if;

         --  Some special processing for non-generic types to complete
         --  representation details not known till the freeze point.

         if Is_Fixed_Point_Type (E) then
            Freeze_Fixed_Point_Type (E);

            --  Some error checks required for ordinary fixed-point type. Defer
            --  these till the freeze-point since we need the small and range
            --  values. We only do these checks for base types

            if Is_Ordinary_Fixed_Point_Type (E) and then Is_Base_Type (E) then
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

         --  The pool applies to named and anonymous access types, but not
         --  to subprogram and to  internal types generated for 'Access
         --  references.

         elsif Is_Access_Type (E)
           and then not Is_Access_Subprogram_Type (E)
           and then Ekind (E) /= E_Access_Attribute_Type
         then
            --  If a pragma Default_Storage_Pool applies, and this type has no
            --  Storage_Pool or Storage_Size clause (which must have occurred
            --  before the freezing point), then use the default. This applies
            --  only to base types.

            --  None of this applies to access to subprograms, for which there
            --  are clearly no pools.

            if Present (Default_Pool)
              and then Is_Base_Type (E)
              and then not Has_Storage_Size_Clause (E)
              and then No (Associated_Storage_Pool (E))
            then
               --  Case of pragma Default_Storage_Pool (null)

               if Nkind (Default_Pool) = N_Null then
                  Set_No_Pool_Assigned (E);

               --  Case of pragma Default_Storage_Pool (storage_pool_NAME)

               else
                  Set_Associated_Storage_Pool (E, Entity (Default_Pool));
               end if;
            end if;

            --  Check restriction for standard storage pool

            if No (Associated_Storage_Pool (E)) then
               Check_Restriction (No_Standard_Storage_Pools, E);
            end if;

            --  Deal with error message for pure access type. This is not an
            --  error in Ada 2005 if there is no pool (see AI-366).

            if Is_Pure_Unit_Access_Type (E)
              and then (Ada_Version < Ada_2005
                         or else not No_Pool_Assigned (E))
              and then not Is_Generic_Unit (Scope (E))
            then
               Error_Msg_N ("named access type not allowed in pure unit", E);

               if Ada_Version >= Ada_2005 then
                  Error_Msg_N
                    ("\would be legal if Storage_Size of 0 given??", E);

               elsif No_Pool_Assigned (E) then
                  Error_Msg_N
                    ("\would be legal in Ada 2005??", E);

               else
                  Error_Msg_N
                    ("\would be legal in Ada 2005 if "
                     & "Storage_Size of 0 given??", E);
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

            --  If the type is a simple storage pool type, then this is where
            --  we attempt to locate and validate its Allocate, Deallocate, and
            --  Storage_Size operations (the first is required, and the latter
            --  two are optional). We also verify that the full type for a
            --  private type is allowed to be a simple storage pool type.

            if Present (Get_Rep_Pragma (E, Name_Simple_Storage_Pool_Type))
              and then (Is_Base_Type (E) or else Has_Private_Declaration (E))
            then
               --  If the type is marked Has_Private_Declaration, then this is
               --  a full type for a private type that was specified with the
               --  pragma Simple_Storage_Pool_Type, and here we ensure that the
               --  pragma is allowed for the full type (for example, it can't
               --  be an array type, or a nonlimited record type).

               if Has_Private_Declaration (E) then
                  if (not Is_Record_Type (E) or else not Is_Limited_View (E))
                    and then not Is_Private_Type (E)
                  then
                     Error_Msg_Name_1 := Name_Simple_Storage_Pool_Type;
                     Error_Msg_N
                       ("pragma% can only apply to full type that is an " &
                        "explicitly limited type", E);
                  end if;
               end if;

               Validate_Simple_Pool_Ops : declare
                  Pool_Type    : Entity_Id renames E;
                  Address_Type : constant Entity_Id := RTE (RE_Address);
                  Stg_Cnt_Type : constant Entity_Id := RTE (RE_Storage_Count);

                  procedure Validate_Simple_Pool_Op_Formal
                    (Pool_Op        : Entity_Id;
                     Pool_Op_Formal : in out Entity_Id;
                     Expected_Mode  : Formal_Kind;
                     Expected_Type  : Entity_Id;
                     Formal_Name    : String;
                     OK_Formal      : in out Boolean);
                  --  Validate one formal Pool_Op_Formal of the candidate pool
                  --  operation Pool_Op. The formal must be of Expected_Type
                  --  and have mode Expected_Mode. OK_Formal will be set to
                  --  False if the formal doesn't match. If OK_Formal is False
                  --  on entry, then the formal will effectively be ignored
                  --  (because validation of the pool op has already failed).
                  --  Upon return, Pool_Op_Formal will be updated to the next
                  --  formal, if any.

                  procedure Validate_Simple_Pool_Operation
                    (Op_Name : Name_Id);
                  --  Search for and validate a simple pool operation with the
                  --  name Op_Name. If the name is Allocate, then there must be
                  --  exactly one such primitive operation for the simple pool
                  --  type. If the name is Deallocate or Storage_Size, then
                  --  there can be at most one such primitive operation. The
                  --  profile of the located primitive must conform to what
                  --  is expected for each operation.

                  ------------------------------------
                  -- Validate_Simple_Pool_Op_Formal --
                  ------------------------------------

                  procedure Validate_Simple_Pool_Op_Formal
                    (Pool_Op        : Entity_Id;
                     Pool_Op_Formal : in out Entity_Id;
                     Expected_Mode  : Formal_Kind;
                     Expected_Type  : Entity_Id;
                     Formal_Name    : String;
                     OK_Formal      : in out Boolean)
                  is
                  begin
                     --  If OK_Formal is False on entry, then simply ignore
                     --  the formal, because an earlier formal has already
                     --  been flagged.

                     if not OK_Formal then
                        return;

                     --  If no formal is passed in, then issue an error for a
                     --  missing formal.

                     elsif not Present (Pool_Op_Formal) then
                        Error_Msg_NE
                          ("simple storage pool op missing formal " &
                           Formal_Name & " of type&", Pool_Op, Expected_Type);
                        OK_Formal := False;

                        return;
                     end if;

                     if Etype (Pool_Op_Formal) /= Expected_Type then

                        --  If the pool type was expected for this formal, then
                        --  this will not be considered a candidate operation
                        --  for the simple pool, so we unset OK_Formal so that
                        --  the op and any later formals will be ignored.

                        if Expected_Type = Pool_Type then
                           OK_Formal := False;

                           return;

                        else
                           Error_Msg_NE
                             ("wrong type for formal " & Formal_Name &
                              " of simple storage pool op; expected type&",
                              Pool_Op_Formal, Expected_Type);
                        end if;
                     end if;

                     --  Issue error if formal's mode is not the expected one

                     if Ekind (Pool_Op_Formal) /= Expected_Mode then
                        Error_Msg_N
                          ("wrong mode for formal of simple storage pool op",
                           Pool_Op_Formal);
                     end if;

                     --  Advance to the next formal

                     Next_Formal (Pool_Op_Formal);
                  end Validate_Simple_Pool_Op_Formal;

                  ------------------------------------
                  -- Validate_Simple_Pool_Operation --
                  ------------------------------------

                  procedure Validate_Simple_Pool_Operation
                    (Op_Name : Name_Id)
                  is
                     Op       : Entity_Id;
                     Found_Op : Entity_Id := Empty;
                     Formal   : Entity_Id;
                     Is_OK    : Boolean;

                  begin
                     pragma Assert
                       (Nam_In (Op_Name, Name_Allocate,
                                         Name_Deallocate,
                                         Name_Storage_Size));

                     Error_Msg_Name_1 := Op_Name;

                     --  For each homonym declared immediately in the scope
                     --  of the simple storage pool type, determine whether
                     --  the homonym is an operation of the pool type, and,
                     --  if so, check that its profile is as expected for
                     --  a simple pool operation of that name.

                     Op := Get_Name_Entity_Id (Op_Name);
                     while Present (Op) loop
                        if Ekind_In (Op, E_Function, E_Procedure)
                          and then Scope (Op) = Current_Scope
                        then
                           Formal := First_Entity (Op);

                           Is_OK := True;

                           --  The first parameter must be of the pool type
                           --  in order for the operation to qualify.

                           if Op_Name = Name_Storage_Size then
                              Validate_Simple_Pool_Op_Formal
                                (Op, Formal, E_In_Parameter, Pool_Type,
                                 "Pool", Is_OK);
                           else
                              Validate_Simple_Pool_Op_Formal
                                (Op, Formal, E_In_Out_Parameter, Pool_Type,
                                 "Pool", Is_OK);
                           end if;

                           --  If another operation with this name has already
                           --  been located for the type, then flag an error,
                           --  since we only allow the type to have a single
                           --  such primitive.

                           if Present (Found_Op) and then Is_OK then
                              Error_Msg_NE
                                ("only one % operation allowed for " &
                                 "simple storage pool type&", Op, Pool_Type);
                           end if;

                           --  In the case of Allocate and Deallocate, a formal
                           --  of type System.Address is required.

                           if Op_Name = Name_Allocate then
                              Validate_Simple_Pool_Op_Formal
                                (Op, Formal, E_Out_Parameter,
                                  Address_Type, "Storage_Address", Is_OK);

                           elsif Op_Name = Name_Deallocate then
                              Validate_Simple_Pool_Op_Formal
                                (Op, Formal, E_In_Parameter,
                                 Address_Type, "Storage_Address", Is_OK);
                           end if;

                           --  In the case of Allocate and Deallocate, formals
                           --  of type Storage_Count are required as the third
                           --  and fourth parameters.

                           if Op_Name /= Name_Storage_Size then
                              Validate_Simple_Pool_Op_Formal
                                (Op, Formal, E_In_Parameter,
                                 Stg_Cnt_Type, "Size_In_Storage_Units", Is_OK);
                              Validate_Simple_Pool_Op_Formal
                                (Op, Formal, E_In_Parameter,
                                 Stg_Cnt_Type, "Alignment", Is_OK);
                           end if;

                           --  If no mismatched formals have been found (Is_OK)
                           --  and no excess formals are present, then this
                           --  operation has been validated, so record it.

                           if not Present (Formal) and then Is_OK then
                              Found_Op := Op;
                           end if;
                        end if;

                        Op := Homonym (Op);
                     end loop;

                     --  There must be a valid Allocate operation for the type,
                     --  so issue an error if none was found.

                     if Op_Name = Name_Allocate
                       and then not Present (Found_Op)
                     then
                        Error_Msg_N ("missing % operation for simple " &
                                     "storage pool type", Pool_Type);

                     elsif Present (Found_Op) then

                        --  Simple pool operations can't be abstract

                        if Is_Abstract_Subprogram (Found_Op) then
                           Error_Msg_N
                             ("simple storage pool operation must not be " &
                              "abstract", Found_Op);
                        end if;

                        --  The Storage_Size operation must be a function with
                        --  Storage_Count as its result type.

                        if Op_Name = Name_Storage_Size then
                           if Ekind (Found_Op) = E_Procedure then
                              Error_Msg_N
                                ("% operation must be a function", Found_Op);

                           elsif Etype (Found_Op) /= Stg_Cnt_Type then
                              Error_Msg_NE
                                ("wrong result type for%, expected type&",
                                 Found_Op, Stg_Cnt_Type);
                           end if;

                        --  Allocate and Deallocate must be procedures

                        elsif Ekind (Found_Op) = E_Function then
                           Error_Msg_N
                             ("% operation must be a procedure", Found_Op);
                        end if;
                     end if;
                  end Validate_Simple_Pool_Operation;

               --  Start of processing for Validate_Simple_Pool_Ops

               begin
                  Validate_Simple_Pool_Operation (Name_Allocate);
                  Validate_Simple_Pool_Operation (Name_Deallocate);
                  Validate_Simple_Pool_Operation (Name_Storage_Size);
               end Validate_Simple_Pool_Ops;
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

         --  Now we set/verify the representation information, in particular
         --  the size and alignment values. This processing is not required for
         --  generic types, since generic types do not play any part in code
         --  generation, and so the size and alignment values for such types
         --  are irrelevant. Ditto for types declared within a generic unit,
         --  which may have components that depend on generic parameters, and
         --  that will be recreated in an instance.

         if Inside_A_Generic then
            null;

         --  Otherwise we call the layout procedure

         else
            Layout_Type (E);
         end if;

         --  If this is an access to subprogram whose designated type is itself
         --  a subprogram type, the return type of this anonymous subprogram
         --  type must be decorated as well.

         if Ekind (E) = E_Anonymous_Access_Subprogram_Type
           and then Ekind (Designated_Type (E)) = E_Subprogram_Type
         then
            Layout_Type (Etype (Designated_Type (E)));
         end if;

         --  If the type has a Defaut_Value/Default_Component_Value aspect,
         --  this is where we analye the expression (after the type is frozen,
         --  since in the case of Default_Value, we are analyzing with the
         --  type itself, and we treat Default_Component_Value similarly for
         --  the sake of uniformity).

         if Is_First_Subtype (E) and then Has_Default_Aspect (E) then
            declare
               Nam : Name_Id;
               Exp : Node_Id;
               Typ : Entity_Id;

            begin
               if Is_Scalar_Type (E) then
                  Nam := Name_Default_Value;
                  Typ := E;
                  Exp := Default_Aspect_Value (Typ);
               else
                  Nam := Name_Default_Component_Value;
                  Typ := Component_Type (E);
                  Exp := Default_Aspect_Component_Value (E);
               end if;

               Analyze_And_Resolve (Exp, Typ);

               if Etype (Exp) /= Any_Type then
                  if not Is_OK_Static_Expression (Exp) then
                     Error_Msg_Name_1 := Nam;
                     Flag_Non_Static_Expr
                       ("aspect% requires static expression", Exp);
                  end if;
               end if;
            end;
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
            F_Node := New_Freeze_Node;
            Set_Freeze_Node (E, F_Node);
            Set_Access_Types_To_Process (F_Node, No_Elist);
            Set_TSS_Elist (F_Node, No_Elist);
            Set_Actions (F_Node, No_List);
         end if;

         Set_Entity (F_Node, E);
         Add_To_Result (F_Node);

         --  A final pass over record types with discriminants. If the type
         --  has an incomplete declaration, there may be constrained access
         --  subtypes declared elsewhere, which do not depend on the discrimi-
         --  nants of the type, and which are used as component types (i.e.
         --  the full view is a recursive type). The designated types of these
         --  subtypes can only be elaborated after the type itself, and they
         --  need an itype reference.

         if Ekind (E) = E_Record_Type and then Has_Discriminants (E) then
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
         Freeze_And_Append (First_Subtype (E), N, Result);

         --  If we just froze a tagged non-class wide record, then freeze the
         --  corresponding class-wide type. This must be done after the tagged
         --  type itself is frozen, because the class-wide type refers to the
         --  tagged type which generates the class.

         if Is_Tagged_Type (E)
           and then not Is_Class_Wide_Type (E)
           and then Present (Class_Wide_Type (E))
         then
            Freeze_And_Append (Class_Wide_Type (E), N, Result);
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
         end if;
      end if;

      Ghost_Mode := Save_Ghost_Mode;
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
      --  type has no size clause either. Treat other foreign conventions
      --  in the same way, and also make sure alignment is set right.

      if Has_Foreign_Convention (Typ)
        and then not Has_Size_Clause (Typ)
        and then not Has_Size_Clause (Base_Type (Typ))
        and then Esize (Typ) < Standard_Integer_Size

        --  Don't do this if Short_Enums on target

        and then not Target_Short_Enums
      then
         Init_Esize (Typ, Standard_Integer_Size);
         Set_Alignment (Typ, Alignment (Standard_Integer));

      --  Normal Ada case or size clause present or not Long_C_Enums on target

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

           --  Don't do this if Short_Enums on target

           and then not Target_Short_Enums
         then
            Error_Msg_N
              ("C enum types have the size of a C int??", Size_Clause (Typ));
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

      function Find_Aggregate_Component_Desig_Type return Entity_Id;
      --  If the expression is an array aggregate, the type of the component
      --  expressions is also frozen. If the component type is an access type
      --  and the expressions include allocators, the designed type is frozen
      --  as well.

      function In_Expanded_Body (N : Node_Id) return Boolean;
      --  Given an N_Handled_Sequence_Of_Statements node N, determines whether
      --  it is the handled statement sequence of an expander-generated
      --  subprogram (init proc, stream subprogram, or renaming as body).
      --  If so, this is not a freezing context.

      -----------------------------------------
      -- Find_Aggregate_Component_Desig_Type --
      -----------------------------------------

      function Find_Aggregate_Component_Desig_Type return Entity_Id is
         Assoc : Node_Id;
         Exp   : Node_Id;

      begin
         if Present (Expressions (N)) then
            Exp := First (Expressions (N));
            while Present (Exp) loop
               if Nkind (Exp) = N_Allocator then
                  return Designated_Type (Component_Type (Etype (N)));
               end if;

               Next (Exp);
            end loop;
         end if;

         if Present (Component_Associations (N)) then
            Assoc := First  (Component_Associations (N));
            while Present (Assoc) loop
               if Nkind (Expression (Assoc)) = N_Allocator then
                  return Designated_Type (Component_Type (Etype (N)));
               end if;

               Next (Assoc);
            end loop;
         end if;

         return Empty;
      end Find_Aggregate_Component_Desig_Type;

      ----------------------
      -- In_Expanded_Body --
      ----------------------

      function In_Expanded_Body (N : Node_Id) return Boolean is
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

            --  The following are expander-created bodies, or bodies that
            --  are not freeze points.

            if Nkind (Id) = N_Defining_Identifier
              and then (Is_Init_Proc (Id)
                         or else Is_TSS (Id, TSS_Stream_Input)
                         or else Is_TSS (Id, TSS_Stream_Output)
                         or else Is_TSS (Id, TSS_Stream_Read)
                         or else Is_TSS (Id, TSS_Stream_Write)
                         or else Nkind_In (Original_Node (P),
                                           N_Subprogram_Renaming_Declaration,
                                           N_Expression_Function))
            then
               return True;
            else
               return False;
            end if;
         end if;
      end In_Expanded_Body;

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
      --  spec of package Sem for further details. Note that we have to make
      --  sure that we actually have a real expression (if we have a subtype
      --  indication, we can't test Is_OK_Static_Expression). However, we
      --  exclude the case of the prefix of an attribute of a static scalar
      --  subtype from this early return, because static subtype attributes
      --  should always cause freezing, even in default expressions, but
      --  the attribute may not have been marked as static yet (because in
      --  Resolve_Attribute, the call to Eval_Attribute follows the call of
      --  Freeze_Expression on the prefix).

      if In_Spec_Exp
        and then Nkind (N) in N_Subexpr
        and then not Is_OK_Static_Expression (N)
        and then (Nkind (Parent (N)) /= N_Attribute_Reference
                   or else not (Is_Entity_Name (N)
                                 and then Is_Type (Entity (N))
                                 and then Is_OK_Static_Subtype (Entity (N))))
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

         if Present (Nam) and then Ekind (Nam) = E_Function then
            Check_Expression_Function (N, Nam);
         end if;

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

               --  Check whether aggregate includes allocators.

               Desig_Typ := Find_Aggregate_Component_Desig_Type;
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

      --  Examine the enclosing context by climbing the parent chain. The
      --  traversal serves two purposes - to detect scenarios where freezeing
      --  is not needed and to find the proper insertion point for the freeze
      --  nodes. Although somewhat similar to Insert_Actions, this traversal
      --  is freezing semantics-sensitive. Inserting freeze nodes blindly in
      --  the tree may result in types being frozen too early.

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

               if In_Expanded_Body (Parent_P) then
                  declare
                     Subp : constant Node_Id := Parent (Parent_P);
                     Spec : Entity_Id;

                  begin
                     --  Freeze the entity only when it is declared inside the
                     --  body of the expander generated procedure. This case
                     --  is recognized by the scope of the entity or its type,
                     --  which is either the spec for some enclosing body, or
                     --  (in the case of init_procs, for which there are no
                     --  separate specs) the current scope.

                     if Nkind (Subp) = N_Subprogram_Body then
                        Spec := Corresponding_Spec (Subp);

                        if (Present (Typ) and then Scope (Typ) = Spec)
                             or else
                           (Present (Nam) and then Scope (Nam) = Spec)
                        then
                           exit;

                        elsif Present (Typ)
                          and then Scope (Typ) = Current_Scope
                          and then Defining_Entity (Subp) = Current_Scope
                        then
                           exit;
                        end if;
                     end if;

                     --  An expression function may act as a completion of
                     --  a function declaration. As such, it can reference
                     --  entities declared between the two views:

                     --     Hidden [];                             -- 1
                     --     function F return ...;
                     --     private
                     --        function Hidden return ...;
                     --        function F return ... is (Hidden);  -- 2

                     --  Refering to the example above, freezing the expression
                     --  of F (2) would place Hidden's freeze node (1) in the
                     --  wrong place. Avoid explicit freezing and let the usual
                     --  scenarios do the job - for example, reaching the end
                     --  of the private declarations, or a call to F.

                     if Nkind (Original_Node (Subp)) =
                                                N_Expression_Function
                     then
                        null;

                     --  Freeze outside the body

                     else
                        Parent_P := Parent (Parent_P);
                        Freeze_Outside := True;
                     end if;
                  end;

               --  Here if normal case where we are in handled statement
               --  sequence and want to do the insertion right there.

               else
                  exit;
               end if;

            --  If parent is a body or a spec or a block, then the current node
            --  is a statement or declaration and we can insert the freeze node
            --  before it.

            when N_Block_Statement       |
                 N_Entry_Body            |
                 N_Package_Body          |
                 N_Package_Specification |
                 N_Protected_Body        |
                 N_Subprogram_Body       |
                 N_Task_Body             => exit;

            --  The expander is allowed to define types in any statements list,
            --  so any of the following parent nodes also mark a freezing point
            --  if the actual node is in a list of statements or declarations.

            when N_Abortable_Part             |
                 N_Accept_Alternative         |
                 N_And_Then                   |
                 N_Case_Statement_Alternative |
                 N_Compilation_Unit_Aux       |
                 N_Conditional_Entry_Call     |
                 N_Delay_Alternative          |
                 N_Elsif_Part                 |
                 N_Entry_Call_Alternative     |
                 N_Exception_Handler          |
                 N_Extended_Return_Statement  |
                 N_Freeze_Entity              |
                 N_If_Statement               |
                 N_Or_Else                    |
                 N_Selective_Accept           |
                 N_Triggering_Alternative     =>

               exit when Is_List_Member (P);

            --  Freeze nodes produced by an expression coming from the Actions
            --  list of a N_Expression_With_Actions node must remain within the
            --  Actions list. Inserting the freeze nodes further up the tree
            --  may lead to use before declaration issues in the case of array
            --  types.

            when N_Expression_With_Actions =>
               if Is_List_Member (P)
                 and then List_Containing (P) = Actions (Parent_P)
               then
                  exit;
               end if;

            --  Note: N_Loop_Statement is a special case. A type that appears
            --  in the source can never be frozen in a loop (this occurs only
            --  because of a loop expanded by the expander), so we keep on
            --  going. Otherwise we terminate the search. Same is true of any
            --  entity which comes from source. (if they have predefined type,
            --  that type does not appear to come from source, but the entity
            --  should not be frozen here).

            when N_Loop_Statement =>
               exit when not Comes_From_Source (Etype (N))
                 and then (No (Nam) or else not Comes_From_Source (Nam));

            --  For all other cases, keep looking at parents

            when others =>
               null;
         end case;

         --  We fall through the case if we did not yet find the proper
         --  place in the free for inserting the freeze node, so climb.

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
            N            : constant Node_Id := Current_Scope;
            Freeze_Nodes : List_Id          := No_List;
            Pos          : Int              := Scope_Stack.Last;

         begin
            if Present (Desig_Typ) then
               Freeze_And_Append (Desig_Typ, N, Freeze_Nodes);
            end if;

            if Present (Typ) then
               Freeze_And_Append (Typ, N, Freeze_Nodes);
            end if;

            if Present (Nam) then
               Freeze_And_Append (Nam, N, Freeze_Nodes);
            end if;

            --  The current scope may be that of a constrained component of
            --  an enclosing record declaration, or of a loop of an enclosing
            --  quantified expression, which is above the current scope in the
            --  scope stack. Indeed in the context of a quantified expression,
            --  a scope is created and pushed above the current scope in order
            --  to emulate the loop-like behavior of the quantified expression.
            --  If the expression is within a top-level pragma, as for a pre-
            --  condition on a library-level subprogram, nothing to do.

            if not Is_Compilation_Unit (Current_Scope)
              and then (Is_Record_Type (Scope (Current_Scope))
                         or else Nkind (Parent (Current_Scope)) =
                                                     N_Quantified_Expression)
            then
               Pos := Pos - 1;
            end if;

            if Is_Non_Empty_List (Freeze_Nodes) then
               if No (Scope_Stack.Table (Pos).Pending_Freeze_Actions) then
                  Scope_Stack.Table (Pos).Pending_Freeze_Actions :=
                    Freeze_Nodes;
               else
                  Append_List (Freeze_Nodes,
                    Scope_Stack.Table (Pos).Pending_Freeze_Actions);
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

      Orig_Lo : Ureal;
      Orig_Hi : Ureal;
      --  Save original bounds (for shaving tests)

      Actual_Size : Nat;
      --  Actual size chosen

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

      Orig_Lo := Loval;
      Orig_Hi := Hival;

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
                 and then Addressable (Size_Excl_EP)
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

      --  Check for shaving

      if Comes_From_Source (Typ) then
         if Orig_Lo < Expr_Value_R (Lo) then
            Error_Msg_N
              ("declared low bound of type & is outside type range??", Typ);
            Error_Msg_N
              ("\low bound adjusted up by delta (RM 3.5.9(13))??", Typ);
         end if;

         if Orig_Hi > Expr_Value_R (Hi) then
            Error_Msg_N
              ("declared high bound of type & is outside type range??", Typ);
            Error_Msg_N
              ("\high bound adjusted down by delta (RM 3.5.9(13))??", Typ);
         end if;
      end if;
   end Freeze_Fixed_Point_Type;

   ------------------
   -- Freeze_Itype --
   ------------------

   procedure Freeze_Itype (T : Entity_Id; N : Node_Id) is
      L : List_Id;

   begin
      Set_Has_Delayed_Freeze (T);
      L := Freeze_Entity (T, N);

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

         if Is_OK_Static_Expression (N) then
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
         --  Do not issue any error message when ignoring rep clauses.

         if Ignore_Rep_Clauses then
            null;

         elsif Is_Imported (E) then
            if No (Address_Clause (E)) then
               Error_Msg_N
                 ("& cannot be imported (local type is not constant)", E);
            end if;

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
      --  explicit Pure_Function pragma was present or the subprogram is an
      --  intrinsic. We do this because otherwise it is an insidious error
      --  to call a non-pure function from pure unit and have calls
      --  mysteriously optimized away. What happens here is that the Import
      --  can bypass the normal check to ensure that pure units call only pure
      --  subprograms.

      --  The reason for the intrinsic exception is that in general, intrinsic
      --  functions (such as shifts) are pure anyway. The only exceptions are
      --  the intrinsics in GNAT.Source_Info, and that unit is not marked Pure
      --  in any case, so no problem arises.

      if Is_Imported (E)
        and then Is_Pure (E)
        and then not Has_Pragma_Pure_Function (E)
        and then not Is_Intrinsic_Subprogram (E)
      then
         Set_Is_Pure (E, False);
      end if;

      --  We also reset the Pure indication on a subprogram with an Address
      --  parameter, because the parameter may be used as a pointer and the
      --  referenced data may change even if the address value does not.

      --  Note that if the programmer gave an explicit Pure_Function pragma,
      --  then we believe the programmer, and leave the subprogram Pure.
      --  We also suppress this check on run-time files.

      if Is_Pure (E)
        and then Is_Subprogram (E)
        and then not Has_Pragma_Pure_Function (E)
        and then not Is_Internal_File_Name (Unit_File_Name (Current_Sem_Unit))
      then
         Check_Function_With_Address_Parameter (E);
      end if;

      --  For non-foreign convention subprograms, this is where we create
      --  the extra formals (for accessibility level and constrained bit
      --  information). We delay this till the freeze point precisely so
      --  that we know the convention.

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
              ("??Valued_Procedure has no effect for convention Ada", E);
            Set_Is_Valued_Procedure (E, False);
         end if;

      --  Case of foreign convention

      else
         Set_Mechanisms (E);

         --  For foreign conventions, warn about return of unconstrained array

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

               --  Check appropriate warning is enabled (should we check for
               --  Warnings (Off) on specific entities here, probably so???)

              and then Warn_On_Export_Import
            then
               Error_Msg_N
                ("?x?foreign convention function& should not return " &
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
                    ("?x?parameter cannot be defaulted in non-Ada call",
                     Default_Value (F));
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

      if Modify_Tree_For_C
        and then Nkind (Parent (E)) = N_Function_Specification
        and then Is_Array_Type (Etype (E))
        and then Is_Constrained (Etype (E))
        and then not Is_Unchecked_Conversion_Instance (E)
        and then not Rewritten_For_C (E)
      then
         Build_Procedure_Form (Unit_Declaration_Node (E));
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

      --  For the designated type of an access to subprogram, all types in
      --  the profile must be fully defined.

      elsif Ekind (T) = E_Subprogram_Type then
         declare
            F : Entity_Id;

         begin
            F := First_Formal (T);
            while Present (F) loop
               if not Is_Fully_Defined (Etype (F)) then
                  return False;
               end if;

               Next_Formal (F);
            end loop;

            return Is_Fully_Defined (Etype (T));
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
              or else Nkind_In (Dcopy, N_Expanded_Name,
                                       N_Integer_Literal,
                                       N_Character_Literal,
                                       N_String_Literal,
                                       N_Real_Literal)
              or else (Nkind (Dcopy) = N_Attribute_Reference
                        and then Attribute_Name (Dcopy) = Name_Null_Parameter)
              or else Known_Null (Dcopy)
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
               Dnam := Make_Temporary (Loc, 'D');

               Dbody :=
                 Make_Subprogram_Body (Loc,
                   Specification =>
                     Make_Procedure_Specification (Loc,
                       Defining_Unit_Name => Dnam),

                   Declarations => New_List (
                     Make_Object_Declaration (Loc,
                       Defining_Identifier => Make_Temporary (Loc, 'T'),
                       Object_Definition   =>
                         New_Occurrence_Of (Etype (Formal), Loc),
                       Expression          => New_Copy_Tree (Dcopy))),

                   Handled_Statement_Sequence =>
                     Make_Handled_Sequence_Of_Statements (Loc,
                       Statements => Empty_List));

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

   --------------------------
   -- Set_SSO_From_Default --
   --------------------------

   procedure Set_SSO_From_Default (T : Entity_Id) is
      Reversed : Boolean;

   begin
      --  Set default SSO for an array or record base type, except in case of
      --  a type extension (which always inherits the SSO of its parent type).

      if Is_Base_Type (T)
        and then (Is_Array_Type (T)
                   or else (Is_Record_Type (T)
                             and then not (Is_Tagged_Type (T)
                                            and then Is_Derived_Type (T))))
      then
         Reversed :=
            (Bytes_Big_Endian     and then SSO_Set_Low_By_Default (T))
              or else
            (not Bytes_Big_Endian and then SSO_Set_High_By_Default (T));

         if (SSO_Set_Low_By_Default (T) or else SSO_Set_High_By_Default (T))

           --  For a record type, if bit order is specified explicitly,
           --  then do not set SSO from default if not consistent. Note that
           --  we do not want to look at a Bit_Order attribute definition
           --  for a parent: if we were to inherit Bit_Order, then both
           --  SSO_Set_*_By_Default flags would have been cleared already
           --  (by Inherit_Aspects_At_Freeze_Point).

           and then not
             (Is_Record_Type (T)
               and then
                 Has_Rep_Item (T, Name_Bit_Order, Check_Parents => False)
               and then Reverse_Bit_Order (T) /= Reversed)
         then
            --  If flags cause reverse storage order, then set the result. Note
            --  that we would have ignored the pragma setting the non default
            --  storage order in any case, hence the assertion at this point.

            pragma Assert
              (not Reversed or else Support_Nondefault_SSO_On_Target);

            Set_Reverse_Storage_Order (T, Reversed);

            --  For a record type, also set reversed bit order. Note: if a bit
            --  order has been specified explicitly, then this is a no-op.

            if Is_Record_Type (T) then
               Set_Reverse_Bit_Order (T, Reversed);
            end if;
         end if;
      end if;
   end Set_SSO_From_Default;

   ------------------
   -- Undelay_Type --
   ------------------

   procedure Undelay_Type (T : Entity_Id) is
   begin
      Set_Has_Delayed_Freeze (T, False);
      Set_Freeze_Node (T, Empty);

      --  Since we don't want T to have a Freeze_Node, we don't want its
      --  Full_View or Corresponding_Record_Type to have one either.

      --  ??? Fundamentally, this whole handling is unpleasant. What we really
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

   procedure Warn_Overlay (Expr : Node_Id; Typ : Entity_Id; Nam : Entity_Id) is
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
      --  types with implicit null initialization, or when Normalize_Scalars
      --  applies and the type is scalar or a string type (the latter being
      --  tested for because predefined String types are initialized by inline
      --  code rather than by an init_proc). Note that we do not give the
      --  warning for Initialize_Scalars, since we suppressed initialization
      --  in this case. Also, do not warn if Suppress_Initialization is set.

      if Present (Expr)
        and then not Is_Imported (Ent)
        and then not Initialization_Suppressed (Typ)
        and then (Has_Non_Null_Base_Init_Proc (Typ)
                   or else Is_Access_Type (Typ)
                   or else (Normalize_Scalars
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

         --  If a pragma Import follows, we assume that it is for the current
         --  target of the address clause, and skip the warning. There may be
         --  a source pragma or an aspect that specifies import and generates
         --  the corresponding pragma. These will indicate that the entity is
         --  imported and that is checked above so that the spurious warning
         --  (generated when the entity is frozen) will be suppressed. The
         --  pragma may be attached to the aspect, so it is not yet a list
         --  member.

         if Is_List_Member (Parent (Expr)) then
            Decl := Next (Parent (Expr));

            if Present (Decl)
              and then Nkind (Decl) = N_Pragma
              and then Pragma_Name (Decl) = Name_Import
            then
               return;
            end if;
         end if;

         --  Otherwise give warning message

         if Present (Old) then
            Error_Msg_Node_2 := Old;
            Error_Msg_N
              ("default initialization of & may modify &??",
               Nam);
         else
            Error_Msg_N
              ("default initialization of & may modify overlaid storage??",
               Nam);
         end if;

         --  Add friendly warning if initialization comes from a packed array
         --  component.

         if Is_Record_Type (Typ) then
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
                     and then Present (Packed_Array_Impl_Type (Etype (Comp)))
                  then
                     Error_Msg_NE
                       ("\packed array component& " &
                        "will be initialized to zero??",
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
            "suppress initialization (RM B.1(24))??",
            Nam);
      end if;
   end Warn_Overlay;

end Freeze;
