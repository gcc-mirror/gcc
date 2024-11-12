------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               F R E E Z E                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2024, Free Software Foundation, Inc.         --
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

with Aspects;        use Aspects;
with Atree;          use Atree;
with Checks;         use Checks;
with Contracts;      use Contracts;
with Debug;          use Debug;
with Einfo;          use Einfo;
with Einfo.Entities; use Einfo.Entities;
with Einfo.Utils;    use Einfo.Utils;
with Elists;         use Elists;
with Errout;         use Errout;
with Exp_Ch7;        use Exp_Ch7;
with Exp_Disp;       use Exp_Disp;
with Exp_Pakd;       use Exp_Pakd;
with Exp_Util;       use Exp_Util;
with Exp_Tss;        use Exp_Tss;
with Ghost;          use Ghost;
with Layout;         use Layout;
with Lib;            use Lib;
with Local_Restrict;
with Namet;          use Namet;
with Nlists;         use Nlists;
with Nmake;          use Nmake;
with Opt;            use Opt;
with Restrict;       use Restrict;
with Rident;         use Rident;
with Rtsfind;        use Rtsfind;
with Sem;            use Sem;
with Sem_Aux;        use Sem_Aux;
with Sem_Cat;        use Sem_Cat;
with Sem_Ch3;        use Sem_Ch3;
with Sem_Ch6;        use Sem_Ch6;
with Sem_Ch7;        use Sem_Ch7;
with Sem_Ch8;        use Sem_Ch8;
with Sem_Ch13;       use Sem_Ch13;
with Sem_Disp;       use Sem_Disp;
with Sem_Eval;       use Sem_Eval;
with Sem_Mech;       use Sem_Mech;
with Sem_Prag;       use Sem_Prag;
with Sem_Res;        use Sem_Res;
with Sem_Util;       use Sem_Util;
with Sinfo;          use Sinfo;
with Sinfo.Nodes;    use Sinfo.Nodes;
with Sinfo.Utils;    use Sinfo.Utils;
with Snames;         use Snames;
with Stand;          use Stand;
with Stringt;        use Stringt;
with Strub;          use Strub;
with Targparm;       use Targparm;
with Tbuild;         use Tbuild;
with Ttypes;         use Ttypes;
with Uintp;          use Uintp;
with Urealp;         use Urealp;
with Warnsw;         use Warnsw;

package body Freeze is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Adjust_Esize_For_Alignment (Typ : Entity_Id);
   --  Typ is a type that is being frozen. If no size clause is given,
   --  but a default Esize has been computed, then this default Esize is
   --  adjusted up if necessary to be consistent with a given alignment,
   --  but never to a value greater than System_Max_Integer_Size. This is
   --  used for all discrete types and for fixed-point types.

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

   procedure Check_Debug_Info_Needed (T : Entity_Id);
   --  As each entity is frozen, this routine is called to deal with the
   --  setting of Debug_Info_Needed for the entity. This flag is set if
   --  the entity comes from source, or if we are in Debug_Generated_Code
   --  mode or if the -gnatdV debug flag is set. However, it never sets
   --  the flag if Debug_Info_Off is set. This procedure also ensures that
   --  subsidiary entities have the flag set as required.

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
   --  adjusted by rep clauses). What this procedure does is to make sure
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
   --  processing is done (they have only been preanalyzed). If the expression
   --  is not an entity or literal, its analysis may generate code which must
   --  not be executed. In that case we build a function body to hold that
   --  code. This wrapper function serves no other purpose (it used to be
   --  called to evaluate the default, but now the default is inlined at each
   --  point of call).

   procedure Set_Component_Alignment_If_Not_Set (Typ : Entity_Id);
   --  Typ is a record or array type that is being frozen. This routine sets
   --  the default component alignment from the scope stack values if the
   --  alignment is otherwise not specified.

   procedure Set_SSO_From_Default (T : Entity_Id);
   --  T is a record or array type that is being frozen. If it is a base type,
   --  and if SSO_Set_Low/High_By_Default is set, then Reverse_Storage order
   --  will be set appropriately. Note that an explicit occurrence of aspect
   --  Scalar_Storage_Order or an explicit setting of this aspect with an
   --  attribute definition clause occurs, then these two flags are reset in
   --  any case, so call will have no effect.

   function Should_Freeze_Type
     (Typ : Entity_Id;
      E   : Entity_Id;
      N   : Node_Id) return Boolean;
   --  True if Typ should be frozen when the profile of E is being frozen at N.

   --  ??? Expression functions that are not completions shouldn't freeze types
   --  but our current expansion and freezing model requires an early freezing
   --  when the tag of Typ is needed or for an aggregate with a subtype of Typ,
   --  so we return True in these cases.

   procedure Undelay_Type (T : Entity_Id);
   --  T is a type of a component that we know to be an Itype. We don't want
   --  this to have a Freeze_Node, so ensure it doesn't. Do the same for any
   --  Full_View or Corresponding_Record_Type.

   procedure Warn_Overlay (Expr : Node_Id; Typ : Entity_Id; Nam : Node_Id);
   --  Expr is the expression for an address clause for the entity denoted by
   --  Nam whose type is Typ. If Typ has a default initialization, and there is
   --  no explicit initialization in the source declaration, check whether the
   --  address clause might cause overlaying of an entity, and emit a warning
   --  on the side effect that the initialization will cause.

   -------------------------------
   -- Adjust_Esize_For_Alignment --
   -------------------------------

   procedure Adjust_Esize_For_Alignment (Typ : Entity_Id) is
      Align : Uint;

   begin
      if Known_Esize (Typ) and then Known_Alignment (Typ) then
         Align := Alignment_In_Bits (Typ);

         if Align > Esize (Typ) then
            if Align > System_Max_Integer_Size then
               pragma Assert (Serious_Errors_Detected > 0);
            else
               Set_Esize (Typ, Align);
            end if;
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
            or else Chars (Renamed_Subp) in Name_Rotate_Left
                                          | Name_Rotate_Right
                                          | Name_Shift_Left
                                          | Name_Shift_Right
                                          | Name_Shift_Right_Arithmetic)
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
      Actuals    : List_Id;
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

      if Nkind (Decl) /= N_Subprogram_Declaration then
         Rewrite (N,
           Make_Subprogram_Declaration (Loc,
             Specification => Specification (N)));
      end if;

      --  For simple renamings, subsequent calls can be expanded directly as
      --  calls to the renamed entity. The body must be generated in any case
      --  for calls that may appear elsewhere. This is not done in the case
      --  where the subprogram is an instantiation because the actual proper
      --  body has not been built yet.

      if Ekind (Old_S) in E_Function | E_Procedure
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

      while Present (Formal) loop
         Append (New_Occurrence_Of (Formal, Loc), Actuals);
         Next_Formal (Formal);
      end loop;

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

      --  Copy SPARK pragma from renaming declaration

      Set_SPARK_Pragma
        (Defining_Unit_Name (Spec), SPARK_Pragma (New_S));
      Set_SPARK_Pragma_Inherited
        (Defining_Unit_Name (Spec), SPARK_Pragma_Inherited (New_S));

      --  In GNATprove, prefer to generate an expression function whenever
      --  possible, to benefit from the more precise analysis in that case
      --  (as if an implicit postcondition had been generated).

      if GNATprove_Mode
        and then Nkind (Call_Node) = N_Simple_Return_Statement
      then
         Body_Node :=
           Make_Expression_Function (Loc,
             Specification => Spec,
             Expression    => Expression (Call_Node));
      else
         Body_Node :=
           Make_Subprogram_Body (Loc,
             Specification              => Spec,
             Declarations               => New_List,
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                 Statements => New_List (Call_Node)));
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

      Assign : Node_Id;
      Decl   : Node_Id;
      Expr   : Node_Id;
      Init   : Node_Id;
      Lhs    : Node_Id;

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
         --  for example by breaking order of elaboration.

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
                     Next_Rep_Item (Rep);
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
                    ("?o?constant& may be modified via address clause#",
                     Decl, O_Ent);
               end if;
            end;
         end if;

         --  Remove side effects from initial expression, except in the case of
         --  limited build-in-place calls and aggregates, which have their own
         --  expansion elsewhere. This exception is necessary to avoid copying
         --  limited objects.

         if Present (Init)
           and then not Is_Inherently_Limited_Type (Typ)
         then
            --  Capture initialization value at point of declaration, and make
            --  explicit assignment legal, because object may be a constant.

            Remove_Side_Effects (Init);
            Lhs := New_Occurrence_Of (E, Sloc (Decl));
            Set_Assignment_OK (Lhs);

            Assign :=
              Make_Assignment_Statement (Sloc (Decl),
                Name       => Lhs,
                Expression => Init);

            Set_No_Initialization (Decl);

            --  If the initialization expression is an aggregate, we do not
            --  adjust after the assignment but, in either case, we do not
            --  finalize before since the object is now uninitialized. Note
            --  that Make_Tag_Ctrl_Assignment will also automatically insert
            --  the tag assignment in the tagged case.

            if Nkind (Unqualify (Init)) = N_Aggregate then
               Set_No_Ctrl_Actions (Assign);
            else
               Set_No_Finalize_Actions (Assign);
            end if;

            --  Move initialization to freeze actions, once the object has
            --  been frozen and the address clause alignment check has been
            --  performed.

            Append_Freeze_Action (E, Assign);
         end if;
      end if;
   end Check_Address_Clause;

   -----------------------------
   -- Check_Compile_Time_Size --
   -----------------------------

   procedure Check_Compile_Time_Size (T : Entity_Id) is

      procedure Set_Small_Size (T : Entity_Id; S : Uint);
      --  Sets the compile time known size in the RM_Size field of T, checking
      --  for a size clause that was given which attempts to give a small size.

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
         if S > System_Max_Integer_Size then
            return;

         --  Check for bad size clause given

         elsif Has_Size_Clause (T) then
            if RM_Size (T) < S then
               Error_Msg_Uint_1 := S;
               Error_Msg_NE (Size_Too_Small_Message, Size_Clause (T), T);
            end if;

         --  Set size if not set already. Do not set it to Uint_0, because in
         --  some cases (notably array-of-record), the Component_Size is
         --  No_Uint, which causes S to be Uint_0. Presumably the RM_Size and
         --  Component_Size will eventually be set correctly by the back end.

         elsif not Known_RM_Size (T) and then S /= Uint_0 then
            Set_RM_Size (T, S);
         end if;
      end Set_Small_Size;

      ----------------
      -- Size_Known --
      ----------------

      function Size_Known (T : Entity_Id) return Boolean is
         Comp  : Entity_Id;
         Ctyp  : Entity_Id;

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
               if Known_Component_Size (T) then
                  Set_Small_Size
                    (T, Component_Size (T) * String_Literal_Length (T));

               else
                  --  The following is wrong, but does what previous versions
                  --  did. The Component_Size is unknown for the string in a
                  --  pragma Warnings.
                  Set_Small_Size (T, Uint_0);
               end if;

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
            --  (in case it is not greater than System_Max_Integer_Size and
            --  thus may be packable).

            declare
               Index : Entity_Id;
               Low   : Node_Id;
               High  : Node_Id;
               Size  : Uint := Component_Size (T);
               Dim   : Uint;

            begin
               --  See comment in Set_Small_Size above

               if No (Size) then
                  Size := Uint_0;
               end if;

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

                     if Dim > Uint_0 then
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

            --  A subtype of a variant record must not have non-static
            --  discriminated components.

            if T /= Base_Type (T)
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
               --  packed record, unless the record has either discriminants or
               --  independent components, or is a strict-alignment type, since
               --  it cannot be fully packed in this case.

               --  The reason we eliminate the discriminated case is that
               --  we don't know the way the back end lays out discriminated
               --  packed records. If Packed_Size_Known is True, then
               --  Packed_Size is the size in bits so far.

               Packed_Size_Known : Boolean :=
                 Is_Packed (T)
                   and then not Has_Discriminants (T)
                   and then not Has_Independent_Components (T)
                   and then not Strict_Alignment (T);

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
                    and then not Known_RM_Size (T)
                    and then not Known_Esize (T)
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

                  --  We do not know the packed size for an independent
                  --  component or if it is of a strict-alignment type,
                  --  since packing does not touch these (RM 13.2(7)).

                  if Is_Independent (Comp)
                    or else Is_Independent (Ctyp)
                    or else Strict_Alignment (Ctyp)
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
                     --  record types as checked by Set_Small_Size.

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
      Comp_Base : Entity_Id;
      Comp_ADC  : Node_Id;
      Encl_Base : Entity_Id;
      Err_Node  : Node_Id;

      Component_Aliased : Boolean;

      Comp_Byte_Aligned : Boolean := False;
      --  Set for the record case, True if Comp is aligned on byte boundaries
      --  (in which case it is allowed to have different storage order).

      Comp_SSO_Differs  : Boolean;
      --  Set True when the component is a nested composite, and it does not
      --  have the same scalar storage order as Encl_Type.

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
            --  and ends on byte boundaries. Otherwise conservatively assume it
            --  does so only in the case where the record is not packed.

            if Present (Component_Clause (Comp)) then
               Comp_Byte_Aligned :=
                 Known_Normalized_First_Bit (Comp)
                   and then
                 Known_Esize (Comp)
                   and then
                 Normalized_First_Bit (Comp) mod System_Storage_Unit = 0
                   and then
                 Esize (Comp) mod System_Storage_Unit = 0;
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

      Comp_ADC :=
        Get_Attribute_Definition_Clause
          (First_Subtype (Comp_Base), Attribute_Scalar_Storage_Order);
      Comp_ADC_Present := Present (Comp_ADC);

      --  Case of record or array component: check storage order compatibility.
      --  But, if the record has Complex_Representation, then it is treated as
      --  a scalar in the back end so the storage order is irrelevant.

      if (Is_Record_Type (Comp_Base)
            and then not Has_Complex_Representation (Comp_Base))
        or else Is_Array_Type (Comp_Base)
      then
         Comp_SSO_Differs :=
           Reverse_Storage_Order (Encl_Base) /=
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

            --  Reject if composite is a bit-packed array, as it is rewritten
            --  into an array of scalars.

            if Is_Bit_Packed_Array (Encl_Base) then
               Error_Msg_N
                 ("type of packed array must have same scalar storage order "
                  & "as component", Err_Node);

            --  Reject if not byte aligned

            elsif Is_Record_Type (Encl_Base)
              and then not Comp_Byte_Aligned
            then
               if Present (Component_Clause (Comp)) then
                  Error_Msg_N
                    ("type of non-byte-aligned component must have same scalar"
                     & " storage order as enclosing record", Err_Node);
               else
                  Error_Msg_N
                    ("type of packed component must have same scalar"
                     & " storage order as enclosing record", Err_Node);
               end if;

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
           and then not (Present (Full_View (Entity (Nod)))
                          and then Has_Completion (Full_View (Entity (Nod))))
         then
            Error_Msg_NE
              ("premature use of& in call or instance", N, Entity (Nod));

         elsif Nkind (Nod) = N_Attribute_Reference then
            Analyze (Prefix (Nod));

            if Is_Entity_Name (Prefix (Nod))
              and then Is_Type (Entity (Prefix (Nod)))
            then
               if Expander_Active then
                  Check_Fully_Declared (Entity (Prefix (Nod)), N);
               end if;

               Freeze_Before (N, Entity (Prefix (Nod)));
            end if;
         end if;

         return OK;
      end Find_Constant;

      procedure Check_Deferred is new Traverse_Proc (Find_Constant);

      --  Local variables

      Decl : Node_Id;

   --  Start of processing for Check_Expression_Function

   begin
      Decl := Original_Node (Unit_Declaration_Node (Nam));

      --  The subprogram body created for the expression function is not
      --  itself a freeze point.

      if Scope (Nam) = Current_Scope
        and then Nkind (Decl) = N_Expression_Function
        and then Nkind (N) /= N_Subprogram_Body
      then
         Check_Deferred (Expression (Decl));
      end if;
   end Check_Expression_Function;

   --------------------------------
   -- Check_Inherited_Conditions --
   --------------------------------

   procedure Check_Inherited_Conditions
     (R               : Entity_Id;
      Late_Overriding : Boolean := False)
   is
      Prim_Ops       : constant Elist_Id := Primitive_Operations (R);
      Decls          : List_Id;
      Op_Node        : Elmt_Id;
      Par_Prim       : Entity_Id;
      Prim           : Entity_Id;

      type Wrapper_Kind is (No_Wrapper, LSP_Wrapper, Condition_Wrapper);

      Wrapper_Needed : Wrapper_Kind;
      --  Kind of wrapper needed by a given inherited primitive of tagged
      --  type R:
      --  * No_Wrapper: No wrapper is needed.
      --  * LSP_Wrapper: Wrapper that handles inherited class-wide pre/post
      --    conditions that call overridden primitives.
      --  * Condition_Wrapper: Wrapper of inherited subprogram that implements
      --    additional interface primitives of the derived type that have
      --    class-wide pre-/postconditions.

      function Build_DTW_Body
        (Loc          : Source_Ptr;
         DTW_Spec     : Node_Id;
         DTW_Decls    : List_Id;
         Par_Prim     : Entity_Id;
         Wrapped_Subp : Entity_Id) return Node_Id;
      --  Build the body of the dispatch table wrapper containing the given
      --  spec and declarations; the call to the wrapped subprogram includes
      --  the proper type conversion.

      function Build_DTW_Spec (Par_Prim : Entity_Id) return Node_Id;
      --  Build the spec of the dispatch table wrapper

      procedure Build_Inherited_Condition_Pragmas
        (Subp               : Entity_Id;
         LSP_Wrapper_Needed : out Boolean);
      --  Build corresponding pragmas for an operation whose ancestor has
      --  class-wide pre/postconditions. If the operation is inherited then
      --  Wrapper_Needed is returned True to force the creation of a wrapper
      --  for the inherited operation. If the ancestor is being overridden,
      --  the pragmas are constructed only to verify their legality, in case
      --  they contain calls to other primitives that may have been overridden.

      procedure Check_Interface_Primitives_Strub_Mode;
      --  Called when R is an interface type to check strub mode compatibility
      --  all its primitives.

      function Needs_Wrapper
        (Class_Cond : Node_Id;
         Subp       : Entity_Id;
         Par_Subp   : Entity_Id) return Boolean;
      --  Checks whether the dispatch-table wrapper (DTW) for Subp must be
      --  built to evaluate the given class-wide condition.

      --------------------
      -- Build_DTW_Body --
      --------------------

      function Build_DTW_Body
        (Loc          : Source_Ptr;
         DTW_Spec     : Node_Id;
         DTW_Decls    : List_Id;
         Par_Prim     : Entity_Id;
         Wrapped_Subp : Entity_Id) return Node_Id
      is
         Actuals    : constant List_Id   := Empty_List;
         Call       : Node_Id;
         Formal     : Entity_Id := First_Formal (Par_Prim);
         New_F_Spec : Entity_Id := First (Parameter_Specifications (DTW_Spec));
         New_Formal : Entity_Id;

      begin
         --  Build parameter association for call to wrapped subprogram

         while Present (Formal) loop
            New_Formal := Defining_Identifier (New_F_Spec);

            --  If the controlling argument is inherited, add conversion to
            --  parent type for the call.

            if Is_Controlling_Formal (Formal) then
               Append_To (Actuals,
                 Make_Type_Conversion (Loc,
                   New_Occurrence_Of (Etype (Formal), Loc),
                   New_Occurrence_Of (New_Formal, Loc)));
            else
               Append_To (Actuals, New_Occurrence_Of (New_Formal, Loc));
            end if;

            Next_Formal (Formal);
            Next (New_F_Spec);
         end loop;

         if Ekind (Wrapped_Subp) = E_Procedure then
            Call :=
              Make_Procedure_Call_Statement (Loc,
                Name => New_Occurrence_Of (Wrapped_Subp, Loc),
                Parameter_Associations => Actuals);
         else
            Call :=
              Make_Simple_Return_Statement (Loc,
                Expression =>
                  Make_Function_Call (Loc,
                    Name => New_Occurrence_Of (Wrapped_Subp, Loc),
                    Parameter_Associations => Actuals));
         end if;

         return
           Make_Subprogram_Body (Loc,
             Specification              => Copy_Subprogram_Spec (DTW_Spec),
             Declarations               => DTW_Decls,
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                 Statements => New_List (Call),
                 End_Label  => Make_Identifier (Loc,
                                 Chars (Defining_Entity (DTW_Spec)))));
      end Build_DTW_Body;

      --------------------
      -- Build_DTW_Spec --
      --------------------

      function Build_DTW_Spec (Par_Prim : Entity_Id) return Node_Id is
         DTW_Id   : Entity_Id;
         DTW_Spec : Node_Id;

      begin
         DTW_Spec := Build_Overriding_Spec (Par_Prim, R);
         DTW_Id   := Defining_Entity (DTW_Spec);

         --  Clear the not-overriding indicator since the DTW wrapper overrides
         --  its wrapped subprogram; required because if present in the parent
         --  primitive, given that Build_Overriding_Spec inherits it, we report
         --  spurious errors.

         Set_Must_Not_Override (DTW_Spec, False);

         --  Add minimal decoration of fields

         Mutate_Ekind (DTW_Id, Ekind (Par_Prim));
         Set_Is_Dispatch_Table_Wrapper (DTW_Id);
         Set_Is_Wrapper (DTW_Id);

         --  The DTW wrapper is never a null procedure

         if Nkind (DTW_Spec) = N_Procedure_Specification then
            Set_Null_Present (DTW_Spec, False);
         end if;

         return DTW_Spec;
      end Build_DTW_Spec;

      ---------------------------------------
      -- Build_Inherited_Condition_Pragmas --
      ---------------------------------------

      procedure Build_Inherited_Condition_Pragmas
        (Subp               : Entity_Id;
         LSP_Wrapper_Needed : out Boolean)
      is
         Class_Pre  : constant Node_Id :=
                        Class_Preconditions (Ultimate_Alias (Subp));
         Class_Post : Node_Id := Class_Postconditions (Par_Prim);
         A_Post     : Node_Id;
         New_Prag   : Node_Id;

      begin
         LSP_Wrapper_Needed := False;

         if No (Class_Pre) and then No (Class_Post) then
            return;
         end if;

         --  For class-wide preconditions we just evaluate whether the wrapper
         --  is needed; there is no need to build the pragma since the check
         --  is performed on the caller side.

         if Present (Class_Pre)
           and then Needs_Wrapper (Class_Pre, Subp, Par_Prim)
         then
            LSP_Wrapper_Needed := True;
         end if;

         --  For class-wide postconditions we evaluate whether the wrapper is
         --  needed and we build the class-wide postcondition pragma to install
         --  it in the wrapper.

         if Present (Class_Post)
           and then Needs_Wrapper (Class_Post, Subp, Par_Prim)
         then
            LSP_Wrapper_Needed := True;

            --  Update the class-wide postcondition

            Class_Post := New_Copy_Tree (Class_Post);
            Build_Class_Wide_Expression
              (Pragma_Or_Expr => Class_Post,
               Subp           => Subp,
               Par_Subp       => Par_Prim,
               Adjust_Sloc    => False);

            --  Install the updated class-wide postcondition in a copy of the
            --  pragma postcondition defined for the nearest ancestor.

            A_Post := Get_Class_Wide_Pragma (Par_Prim,
                        Pragma_Postcondition);

            if No (A_Post) then
               declare
                  Subps : constant Subprogram_List :=
                            Inherited_Subprograms (Subp);
               begin
                  for Index in Subps'Range loop
                     A_Post := Get_Class_Wide_Pragma (Subps (Index),
                                 Pragma_Postcondition);
                     exit when Present (A_Post);
                  end loop;
               end;
            end if;

            --  A_Post can be null here if the postcondition was inlined in the
            --  called subprogram.

            if Present (A_Post) then
               New_Prag := New_Copy_Tree (A_Post);
               Rewrite
                 (Expression (First (Pragma_Argument_Associations (New_Prag))),
                  Class_Post);
               Append (New_Prag, Decls);
            end if;
         end if;
      end Build_Inherited_Condition_Pragmas;

      -------------------------------------------
      -- Check_Interface_Primitives_Strub_Mode --
      -------------------------------------------

      procedure Check_Interface_Primitives_Strub_Mode is
         Elmt        : Elmt_Id;
         Iface_Elmt  : Elmt_Id;
         Iface       : Entity_Id;
         Iface_Prim  : Entity_Id;
         Ifaces_List : Elist_Id;
         Op_Node     : Elmt_Id;
         Prim        : Entity_Id;
         Prim_Iface  : Entity_Id;

      begin
         pragma Assert (Is_Interface (R));

         --  Collect interfaces extended by interface type R

         Collect_Interfaces (R, Ifaces_List);

         Op_Node := First_Elmt (Prim_Ops);
         while Present (Op_Node) loop
            Prim       := Node (Op_Node);
            Prim_Iface := R;
            Par_Prim   := Overridden_Operation (Prim);

            --  We only need to check entities defined in the sources

            --  Check that overrider and overridden primitives have the same
            --  strub mode.

            if Present (Par_Prim) then
               Check_Same_Strub_Mode (Prim, Par_Prim);

            --  No need to check internally added predefined primitives since
            --  they all have the same strub mode.

            elsif Is_Predefined_Dispatching_Operation (Prim)
              and then not Comes_From_Source (Prim)
            then
               null;

            --  Check strub mode of matching primitives of all the interface
            --  types, since several interface types may define primitives with
            --  the same profile that will be implemented by a single primitive
            --  of tagged types implementing R, and therefore must have the
            --  same strub mode.

            else
               --  If this interface primitive has been inherited this is an
               --  internal entity we rely on its renamed entity (which is the
               --  entity defined in the sources).

               if Present (Alias (Prim)) then
                  Prim       := Ultimate_Alias (Prim);
                  Prim_Iface := Find_Dispatching_Type (Prim);
               end if;

               --  Search for primitives conformant with this one in the other
               --  interface types.

               Iface_Elmt := First_Elmt (Ifaces_List);
               while Present (Iface_Elmt) loop
                  Iface := Node (Iface_Elmt);

                  if Iface /= Prim_Iface then
                     Elmt := First_Elmt (Primitive_Operations (Iface));
                     while Present (Elmt) loop
                        Iface_Prim := Node (Elmt);

                        if Chars (Iface_Prim) = Chars (Prim)
                          and then Comes_From_Source (Iface_Prim)
                          and then Is_Interface_Conformant
                                     (Prim_Iface, Iface_Prim, Prim)
                        then
                           --  Check the strub mode passing the original
                           --  primitive (instead of its alias); required
                           --  to report the error at the right location.

                           Check_Same_Strub_Mode (Node (Op_Node), Iface_Prim);
                        end if;

                        Next_Elmt (Elmt);
                     end loop;
                  end if;

                  Next_Elmt (Iface_Elmt);
               end loop;
            end if;

            Next_Elmt (Op_Node);
         end loop;
      end Check_Interface_Primitives_Strub_Mode;

      -------------------
      -- Needs_Wrapper --
      -------------------

      function Needs_Wrapper
        (Class_Cond : Node_Id;
         Subp       : Entity_Id;
         Par_Subp   : Entity_Id) return Boolean
      is
         Result : Boolean := False;

         function Check_Entity (N : Node_Id) return Traverse_Result;
         --  Check calls to overridden primitives

         --------------------
         -- Replace_Entity --
         --------------------

         function Check_Entity (N : Node_Id) return Traverse_Result is
            New_E : Entity_Id;

         begin
            if Nkind (N) = N_Identifier
              and then Present (Entity (N))
              and then
                (Is_Formal (Entity (N)) or else Is_Subprogram (Entity (N)))
              and then
                (Nkind (Parent (N)) /= N_Attribute_Reference
                  or else Attribute_Name (Parent (N)) /= Name_Class)
            then
               --  Determine whether entity has a renaming

               New_E := Get_Mapped_Entity (Entity (N));

               --  If the entity is an overridden primitive and we are not
               --  in GNATprove mode, we must build a wrapper for the current
               --  inherited operation. If the reference is the prefix of an
               --  attribute such as 'Result (or others ???) there is no need
               --  for a wrapper: the condition is just rewritten in terms of
               --  the inherited subprogram.

               if Present (New_E)
                 and then Comes_From_Source (New_E)
                 and then Is_Subprogram (New_E)
                 and then Nkind (Parent (N)) /= N_Attribute_Reference
                 and then not GNATprove_Mode
               then
                  Result := True;
                  return Abandon;
               end if;
            end if;

            return OK;
         end Check_Entity;

         procedure Check_Condition_Entities is
           new Traverse_Proc (Check_Entity);

      --  Start of processing for Needs_Wrapper

      begin
         Update_Primitives_Mapping (Par_Subp, Subp);

         Map_Formals (Par_Subp, Subp);
         Check_Condition_Entities (Class_Cond);

         return Result;
      end Needs_Wrapper;

      Wrappers_List : Elist_Id := No_Elist;
      --  List containing identifiers of built wrappers. Used to defer building
      --  and analyzing their class-wide precondition subprograms.

      Condition_Candidates_List : Elist_Id := No_Elist;
      --  List containing inherited primitives of tagged type R that implement
      --  interface primitives that have pre-/postconditions.

   --  Start of processing for Check_Inherited_Conditions

   begin
      if Late_Overriding then
         Op_Node := First_Elmt (Prim_Ops);
         while Present (Op_Node) loop
            Prim := Node (Op_Node);

            --  Map the overridden primitive to the overriding one

            if Present (Overridden_Operation (Prim))
              and then Comes_From_Source (Prim)
            then
               Par_Prim := Overridden_Operation (Prim);
               Update_Primitives_Mapping (Par_Prim, Prim);

               --  Force discarding previous mappings of its formals

               Map_Formals (Par_Prim, Prim, Force_Update => True);
            end if;

            Next_Elmt (Op_Node);
         end loop;
      end if;

      --  For interface types we only need to check strub mode compatibility
      --  of their primitives (since they don't have wrappers).

      if Is_Interface (R) then
         Check_Interface_Primitives_Strub_Mode;
         return;
      end if;

      --  Perform validity checks on the inherited conditions of overriding
      --  operations, for conformance with LSP, and apply SPARK-specific
      --  restrictions on inherited conditions.

      Op_Node := First_Elmt (Prim_Ops);
      while Present (Op_Node) loop
         Prim     := Node (Op_Node);
         Par_Prim := Overridden_Operation (Prim);

         if Present (Par_Prim)
           and then Comes_From_Source (Prim)
         then
            --  When the primitive is an LSP wrapper we climb to the parent
            --  primitive that has the inherited contract.

            if Is_LSP_Wrapper (Par_Prim) then
               Par_Prim := LSP_Subprogram (Par_Prim);
            end if;

            --  Check that overrider and overridden operations have
            --  the same strub mode.

            Check_Same_Strub_Mode (Prim, Par_Prim);

            --  Analyze the contract items of the overridden operation, before
            --  they are rewritten as pragmas.

            Analyze_Entry_Or_Subprogram_Contract (Par_Prim);

            --  In GNATprove mode this is where we can collect the inherited
            --  conditions, because we do not create the Check pragmas that
            --  normally convey the modified class-wide conditions on
            --  overriding operations.

            if GNATprove_Mode then
               Collect_Inherited_Class_Wide_Conditions (Prim);
            end if;

         --  Check strub mode compatibility of primitives that implement
         --  interface primitives.

         elsif Present (Interface_Alias (Prim)) then
            Check_Same_Strub_Mode (Alias (Prim), Interface_Alias (Prim));
         end if;

         Next_Elmt (Op_Node);
      end loop;

      --  Collect inherited primitives that may need a wrapper to handle
      --  pre-/postconditions of interface primitives; done to improve the
      --  performance when checking if postcondition wrappers are needed.

      Op_Node := First_Elmt (Prim_Ops);
      while Present (Op_Node) loop
         Prim := Node (Op_Node);

         if Present (Interface_Alias (Prim))
           and then not Comes_From_Source (Alias (Prim))
           and then
             (Present (Class_Preconditions (Interface_Alias (Prim)))
                or else
              Present (Class_Postconditions (Interface_Alias (Prim))))
         then
            if No (Condition_Candidates_List) then
               Condition_Candidates_List := New_Elmt_List;
            end if;

            Append_Unique_Elmt (Alias (Prim), Condition_Candidates_List);
         end if;

         Next_Elmt (Op_Node);
      end loop;

      --  Now examine the inherited operations to check whether they require
      --  a wrapper to handle inherited conditions that call other primitives,
      --  so that LSP can be verified/enforced.

      Op_Node := First_Elmt (Prim_Ops);

      while Present (Op_Node) loop
         Decls          := Empty_List;
         Prim           := Node (Op_Node);
         Wrapper_Needed := No_Wrapper;

         --  Skip internal entities built for mapping interface primitives

         if not Comes_From_Source (Prim)
           and then Present (Alias (Prim))
           and then No (Interface_Alias (Prim))
         then
            Par_Prim := Ultimate_Alias (Prim);

            --  When the primitive is an LSP wrapper we climb to the parent
            --  primitive that has the inherited contract.

            if Is_LSP_Wrapper (Par_Prim) then
               Par_Prim := LSP_Subprogram (Par_Prim);
            end if;

            --  Analyze the contract items of the parent operation, and
            --  determine whether this inherited primitive needs a LSP
            --  wrapper. This is determined when the condition is rewritten
            --  in sem_prag, using the mapping between overridden and
            --  overriding operations built in the loop above.

            declare
               LSP_Wrapper_Needed : Boolean;

            begin
               Analyze_Entry_Or_Subprogram_Contract (Par_Prim);
               Build_Inherited_Condition_Pragmas (Prim, LSP_Wrapper_Needed);

               if LSP_Wrapper_Needed then
                  Wrapper_Needed := LSP_Wrapper;
               end if;
            end;

            --  If the LSP wrapper is not needed but the tagged type R
            --  implements additional interface types, and this inherited
            --  primitive covers an interface primitive of these additional
            --  interface types that has class-wide postconditions, then it
            --  requires a pre-/postconditions wrapper.

            if Wrapper_Needed = No_Wrapper
              and then Present (Interfaces (R))
              and then Present (Condition_Candidates_List)
              and then Contains (Condition_Candidates_List, Prim)
            then
               declare
                  Elmt       : Elmt_Id;
                  Ent        : Entity_Id;
                  Iface      : Entity_Id;
                  Iface_Elmt : Elmt_Id;

               begin
                  Elmt := First_Elmt (Prim_Ops);

                  Search : while Present (Elmt) loop
                     Ent := Node (Elmt);

                     --  Perform the search relying on the internal entities
                     --  that link tagged type primitives with interface
                     --  primitives.

                     if Present (Interface_Alias (Ent))
                       and then (Alias (Ent)) = Prim
                       and then
                         (Present (Class_Preconditions (Interface_Alias (Ent)))
                            or else Present (Class_Postconditions
                                               (Interface_Alias (Ent))))
                     then
                        Iface := Find_Dispatching_Type (Interface_Alias (Ent));

                        --  We only need to locate primitives of additional
                        --  interfaces implemented by tagged type R (since
                        --  inherited primitives of parent types that cover
                        --  primitives of inherited interface types don't
                        --  need a wrapper).

                        Iface_Elmt := First_Elmt (Interfaces (R));
                        while Present (Iface_Elmt) loop
                           if Node (Iface_Elmt) = Iface then
                              Wrapper_Needed := Condition_Wrapper;
                              exit Search;
                           end if;

                           Next_Elmt (Iface_Elmt);
                        end loop;
                     end if;

                     Next_Elmt (Elmt);
                  end loop Search;
               end;
            end if;
         end if;

         if Wrapper_Needed /= No_Wrapper
           and then not Is_Abstract_Subprogram (Par_Prim)
           and then Expander_Active
         then
            --  Build the dispatch-table wrapper (DTW). The support for
            --  AI12-0195 relies on two kind of wrappers: one for indirect
            --  calls (also used for AI12-0220), and one for putting in the
            --  dispatch table:
            --
            --    1) "indirect-call wrapper" (ICW) is needed anytime there are
            --       class-wide preconditions. Prim'Access will point directly
            --       at the ICW if any, or at the "pristine" body if Prim has
            --       no class-wide preconditions.
            --
            --    2) "dispatch-table wrapper" (DTW) is needed anytime the class
            --       wide preconditions *or* the class-wide postconditions are
            --       affected by overriding.
            --
            --  The DTW holds a single statement that is a single call where
            --  the controlling actuals are conversions to the corresponding
            --  type in the parent primitive. If the primitive is a function
            --  the statement is a return statement with a call.

            declare
               Alias_Id : constant Entity_Id  := Ultimate_Alias (Prim);
               Loc      : constant Source_Ptr := Sloc (R);
               DTW_Body : Node_Id;
               DTW_Decl : Node_Id;
               DTW_Id   : Entity_Id;
               DTW_Spec : Node_Id;

               Prim_Next_E : constant Entity_Id := Next_Entity (Prim);
               Prim_Prev_E : constant Entity_Id := Prev_Entity (Prim);

            begin
               DTW_Spec := Build_DTW_Spec (Par_Prim);
               DTW_Id   := Defining_Entity (DTW_Spec);
               DTW_Decl := Make_Subprogram_Declaration (Loc,
                             Specification => DTW_Spec);

               --  LSP wrappers reference the parent primitive that has the
               --  the class-wide pre/post condition that calls overridden
               --  primitives. Condition wrappers do not have this attribute
               --  (see predicate Is_LSP_Wrapper).

               if Wrapper_Needed = LSP_Wrapper then
                  Set_LSP_Subprogram (DTW_Id, Par_Prim);
               end if;

               --  The spec of the wrapper has been built using the source
               --  location of its parent primitive; we must update it now
               --  (with the source location of the internal primitive built
               --  by Derive_Subprogram that will override this wrapper) to
               --  avoid inlining conflicts between internally built helpers
               --  for class-wide pre/postconditions of the parent and the
               --  helpers built for this wrapper.

               Set_Sloc (DTW_Id, Sloc (Prim));

               --  For LSP_Wrappers of subprograms that inherit class-wide
               --  preconditions the DTW wrapper reuses the ICW of the parent
               --  (which checks the parent interpretation of the class-wide
               --  preconditions); the interpretation of the class-wide
               --  preconditions for the inherited subprogram is checked
               --  at the caller side.

               --  When the subprogram inherits class-wide postconditions
               --  the DTW also checks the interpretation of the class-wide
               --  postconditions for the inherited subprogram, and the body
               --  of the parent checks its interpretation of the parent for
               --  the class-wide postconditions.

               --      procedure Prim (F1 : T1; ...) is
               --         [ pragma Postcondition (check => Expr); ]
               --      begin
               --         Par_Prim_ICW (Par_Type (F1), ...);
               --      end;

               if Wrapper_Needed = LSP_Wrapper
                 and then Present (Indirect_Call_Wrapper (Par_Prim))
               then
                  DTW_Body :=
                    Build_DTW_Body (Loc,
                      DTW_Spec     => DTW_Spec,
                      DTW_Decls    => Decls,
                      Par_Prim     => Par_Prim,
                      Wrapped_Subp => Indirect_Call_Wrapper (Par_Prim));

               --  For LSP_Wrappers of subprograms that only inherit class-wide
               --  postconditions, and also for Condition_Wrappers (wrappers of
               --  inherited subprograms that implement additional interface
               --  primitives that have class-wide pre-/postconditions), the
               --  DTW wrapper calls the parent primitive (which on its body
               --  checks the interpretation of the class-wide post-conditions
               --  for the parent subprogram), and the DTW checks the
               --  interpretation of the class-wide postconditions for the
               --  inherited subprogram.

               --      procedure Prim (F1 : T1; ...) is
               --         pragma Postcondition (check => Expr);
               --      begin
               --         Par_Prim (Par_Type (F1), ...);
               --      end;

               --  No class-wide preconditions runtime check is generated for
               --  this wrapper call to the parent primitive, since the check
               --  is performed by the caller of the DTW wrapper (see routine
               --  Install_Class_Preconditions_Check).

               else
                  DTW_Body :=
                    Build_DTW_Body (Loc,
                      DTW_Spec     => DTW_Spec,
                      DTW_Decls    => Decls,
                      Par_Prim     => Par_Prim,
                      Wrapped_Subp => Par_Prim);
               end if;

               --  Insert the declaration of the wrapper before the freezing
               --  node of the record type declaration to ensure that it will
               --  override the internal primitive built by Derive_Subprogram.

               if Late_Overriding then
                  Ensure_Freeze_Node (R);
                  Insert_Before_And_Analyze (Freeze_Node (R), DTW_Decl);
               else
                  Append_Freeze_Action (R, DTW_Decl);
                  Analyze (DTW_Decl);
               end if;

               --  The analyis of DTW_Decl has removed Prim from its scope
               --  chain and added DTW_Id at the end of the scope chain. Move
               --  DTW_Id to its correct place in the scope chain: the analysis
               --  of the wrapper declaration has just added DTW_Id at the end
               --  of the list of entities of its scope. However, given that
               --  this wrapper overrides Prim, we must move DTW_Id to the
               --  original place of Prim in its scope chain. This is required
               --  for wrappers of private type primitives to ensure their
               --  correct visibility since wrappers are built when the full
               --  tagged type declaration is frozen (in the private part of
               --  the package) but they may override primitives defined in the
               --  public part of the package.

               declare
                  DTW_Prev_E : constant Entity_Id := Prev_Entity (DTW_Id);

               begin
                  pragma Assert (Last_Entity (Current_Scope) = DTW_Id);
                  pragma Assert
                    (Ekind (Current_Scope) not in E_Package | E_Generic_Package
                       or else No (First_Private_Entity (Current_Scope))
                       or else First_Private_Entity (Current_Scope) /= DTW_Id);

                  --  Remove DTW_Id from the end of the doubly-linked list of
                  --  entities of this scope; no need to handle removing it
                  --  from the beginning of the chain since such case can never
                  --  occur for this entity.

                  Set_Last_Entity (Current_Scope, DTW_Prev_E);
                  Set_Next_Entity (DTW_Prev_E, Empty);

                  --  Place DTW_Id back in the original place of its wrapped
                  --  primitive in the list of entities of this scope.

                  Link_Entities (Prim_Prev_E, DTW_Id);
                  Link_Entities (DTW_Id, Prim_Next_E);
               end;

               --  Insert the body of the wrapper in the freeze actions of
               --  its record type declaration to ensure that it is placed
               --  in the scope of its declaration but not too early to cause
               --  premature freezing of other entities.

               Append_Freeze_Action (R, DTW_Body);
               Analyze (DTW_Body);

               --  Ensure correct decoration

               pragma Assert (Is_Dispatching_Operation (DTW_Id));
               pragma Assert (Present (Overridden_Operation (DTW_Id)));
               pragma Assert (Overridden_Operation (DTW_Id) = Alias_Id);

               --  Inherit dispatch table slot

               Set_DTC_Entity_Value (R, DTW_Id);
               Set_DT_Position (DTW_Id, DT_Position (Alias_Id));

               --  Register the wrapper in the dispatch table

               if Late_Overriding
                 and then not Building_Static_DT (R)
               then
                  Insert_List_After_And_Analyze (Freeze_Node (R),
                    Register_Primitive (Loc, DTW_Id));
               end if;

               --  Defer building helpers and ICW for the DTW. Required to
               --  ensure uniqueness in their names because when building
               --  these wrappers for overlapped subprograms their homonym
               --  number is not definite until all these dispatch table
               --  wrappers of tagged type R have been analyzed.

               if Present (Indirect_Call_Wrapper (Par_Prim)) then
                  Append_New_Elmt (DTW_Id, Wrappers_List);
               end if;
            end;
         end if;

         Next_Elmt (Op_Node);
      end loop;

      --  Build and analyze deferred class-wide precondition subprograms of
      --  built wrappers.

      if Present (Wrappers_List) then
         declare
            Body_N  : Node_Id;
            CW_Subp : Entity_Id;
            Decl_N  : Node_Id;
            DTW_Id  : Entity_Id;
            Elmt    : Elmt_Id;

         begin
            Elmt := First_Elmt (Wrappers_List);

            while Present (Elmt) loop
               DTW_Id := Node (Elmt);
               Next_Elmt (Elmt);

               Merge_Class_Conditions (DTW_Id);
               Make_Class_Precondition_Subps (DTW_Id, Late_Overriding);

               CW_Subp := Static_Call_Helper (DTW_Id);
               Decl_N  := Unit_Declaration_Node (CW_Subp);
               Analyze (Decl_N);

               --  If the DTW was built for a late-overriding primitive
               --  its body must be analyzed now (since the tagged type
               --  is already frozen).

               if Late_Overriding then
                  Body_N :=
                    Unit_Declaration_Node (Corresponding_Body (Decl_N));
                  Analyze (Body_N);
               end if;
            end loop;
         end;
      end if;
   end Check_Inherited_Conditions;

   ----------------------------
   -- Check_Strict_Alignment --
   ----------------------------

   procedure Check_Strict_Alignment (E : Entity_Id) is
      Comp : Entity_Id;

   begin
      --  Bit-packed array types do not require strict alignment, even if they
      --  are by-reference types, because they are accessed in a special way.

      if Is_By_Reference_Type (E) and then not Is_Bit_Packed_Array (E) then
         Set_Strict_Alignment (E);

      elsif Is_Array_Type (E) then
         Set_Strict_Alignment (E, Strict_Alignment (Component_Type (E)));

         --  RM 13.2(7.1/4): Any component of a packed type that contains an
         --  aliased part shall be aligned according to the alignment of its
         --  subtype.

         --  Unfortunately this breaks Florist, which has had the bad habit
         --  of overaligning all the types it declares on 32-bit platforms,
         --  so make an exception if the component size is the storage unit.

         --  Other legacy codebases could also be affected because this was
         --  historically not enforced, so -gnatd_l can be used to disable it.

         if Has_Aliased_Components (E)
           and then not (Known_Component_Size (E)
                          and then Component_Size (E) = System_Storage_Unit)
           and then not Debug_Flag_Underscore_L
         then
            Set_Strict_Alignment (E);
         end if;

      elsif Is_Record_Type (E) then
         Comp := First_Component (E);
         while Present (Comp) loop
            if not Is_Type (Comp)
              and then (Is_Aliased (Comp)
                         or else Strict_Alignment (Etype (Comp)))
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

   -----------------------------------------------
   -- Explode_Initialization_Compound_Statement --
   -----------------------------------------------

   procedure Explode_Initialization_Compound_Statement (E : Entity_Id) is
      Init_Stmts : constant Node_Id := Initialization_Statements (E);

   begin
      if Present (Init_Stmts)
        and then Nkind (Init_Stmts) = N_Compound_Statement
      then
         --  If the entity has its freezing delayed, append the initialization
         --  actions to its freeze actions. Otherwise insert them back at the
         --  point where they have been generated.

         if Has_Delayed_Freeze (E) then
            Append_Freeze_Actions (E, Actions (Init_Stmts));
         else
            Insert_List_Before (Init_Stmts, Actions (Init_Stmts));
         end if;

         --  Note that we rewrite Init_Stmts into a null statement, rather than
         --  just removing it, because Freeze_All may rely on this particular
         --  node still being present in the enclosing list to know where to
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

         procedure Process_Flist;
         --  If freeze nodes are present, insert and analyze, and reset cursor
         --  for next insertion.

         -------------------
         -- Process_Flist --
         -------------------

         procedure Process_Flist is
            Lastn : Node_Id;
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
              and then No (Renamed_Entity (E))
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
              and then Nkind (Parent (E)) in
                         N_Single_Task_Declaration | N_Task_Type_Declaration
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
               Check_Aspects_At_End_Of_Declarations (E);
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

                  if Comes_From_Source (Bod)
                    and then Nkind (Bod) in N_Entry_Body
                                          | N_Package_Body
                                          | N_Protected_Body
                                          | N_Subprogram_Body
                                          | N_Task_Body
                                          | N_Body_Stub
                    and then
                      In_Same_List (After, Parent (E))
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

      --  Local variables

      Decl : Node_Id;
      E    : Entity_Id;
      Item : Entity_Id;

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

            --  Check subprogram renamings for the same strub-mode.
            --  Avoid rechecking dispatching operations, that's taken
            --  care of in Check_Inherited_Conditions, that covers
            --  inherited interface operations.

            Item := Alias (E);
            if Present (Item)
              and then not Is_Dispatching_Operation (E)
            then
               Check_Same_Strub_Mode (E, Item);
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
                   Nkind (Unit_Declaration_Node (Corresponding_Body (Decl))) =
                     N_Subprogram_Renaming_Declaration
               then
                  Build_And_Analyze_Renamed_Body
                    (Decl, Corresponding_Body (Decl), After);
               end if;
            end if;

         --  Freeze the default expressions of entries, entry families, and
         --  protected subprograms.

         elsif Is_Concurrent_Type (E) then
            Item := First_Entity (E);
            while Present (Item) loop
               if Is_Subprogram_Or_Entry (Item)
                 and then not Default_Expressions_Processed (Item)
               then
                  Process_Default_Expressions (Item, After);
               end if;

               Next_Entity (Item);
            end loop;
         end if;

         --  Historical note: We used to create a finalization collection for
         --  access types whose designated type is not controlled, but contains
         --  private controlled compoments. This form of postprocessing is no
         --  longer needed because the finalization collection is now created
         --  when the access type is frozen (see Exp_Ch3.Freeze_Type).

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
      --  Freezing an Expression_Function does not freeze its profile:
      --  the formals will have been frozen otherwise before the E_F
      --  can be called.

      L : constant List_Id :=
        Freeze_Entity
          (Ent, N, Do_Freeze_Profile => not Is_Expression_Function (Ent));
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
      Pack         : constant Entity_Id := Scope (T);

   begin
      if Ekind (T) = E_Function then
         Check_Expression_Function (N, T);
      end if;

      if Is_Non_Empty_List (Freeze_Nodes) then

         --  If the entity is a type declared in an inner package, it may be
         --  frozen by an outer declaration before the package itself is
         --  frozen. Install the package scope to analyze the freeze nodes,
         --  which may include generated subprograms such as predicate
         --  functions, etc.

         if Is_Type (T) and then From_Nested_Package (T) then
            Push_Scope (Pack);
            Install_Visible_Declarations (Pack);
            Install_Private_Declarations (Pack);
            Insert_Actions (N, Freeze_Nodes);
            End_Package_Scope (Pack);

         else
            Insert_Actions (N, Freeze_Nodes);
         end if;
      end if;
   end Freeze_Before;

   -------------------
   -- Freeze_Entity --
   -------------------

   --  WARNING: This routine manages Ghost regions. Return statements must be
   --  replaced by gotos which jump to the end of the routine and restore the
   --  Ghost mode.

   function Freeze_Entity
     (E                 : Entity_Id;
      N                 : Node_Id;
      Do_Freeze_Profile : Boolean := True) return List_Id
   is
      Loc : constant Source_Ptr := Sloc (N);

      Saved_GM  : constant Ghost_Mode_Type := Ghost_Mode;
      Saved_IGR : constant Node_Id         := Ignored_Ghost_Region;
      --  Save the Ghost-related attributes to restore on exit

      Atype  : Entity_Id;
      Comp   : Entity_Id;
      F_Node : Node_Id;
      Formal : Entity_Id;
      Indx   : Node_Id;

      Result : List_Id := No_List;
      --  List of freezing actions, left at No_List if none

      Test_E : Entity_Id := E;
      --  A local temporary used to test if freezing is necessary for E, since
      --  its value can be set to something other than E in certain cases. For
      --  example, E cannot be used directly in cases such as when it is an
      --  Itype defined within a record - since it is the location of record
      --  which matters.

      procedure Add_To_Result (Fnod : Node_Id);
      --  Add freeze action Fnod to list Result

      function After_Last_Declaration return Boolean;
      --  If Loc is a freeze_entity that appears after the last declaration
      --  in the scope, inhibit error messages on late completion.

      procedure Check_Current_Instance (Comp_Decl : Node_Id);
      --  Check that an Access or Unchecked_Access attribute with a prefix
      --  which is the current instance type can only be applied when the type
      --  is limited.

      procedure Check_No_Parts_Violations
        (Typ : Entity_Id; Aspect_No_Parts : Aspect_Id) with
         Pre => Aspect_No_Parts in
                  Aspect_No_Controlled_Parts | Aspect_No_Task_Parts;
      --  Check that Typ does not violate the semantics of the specified
      --  Aspect_No_Parts (No_Controlled_Parts or No_Task_Parts) when it is
      --  specified on Typ or one of its ancestors.

      procedure Check_Suspicious_Convention (Rec_Type : Entity_Id);
      --  Give a warning for pragma Convention with language C or C++ applied
      --  to a discriminated record type. This is suppressed for the unchecked
      --  union case, since the whole point in this case is interface C. We
      --  also do not generate this within instantiations, since we will have
      --  generated a message on the template.

      procedure Check_Suspicious_Modulus (Utype : Entity_Id);
      --  Give warning for modulus of 8, 16, 32, 64 or 128 given as an explicit
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

      procedure Inherit_Freeze_Node
        (Fnod : Node_Id;
         Typ  : Entity_Id);
      --  Set type Typ's freeze node to refer to Fnode. This routine ensures
      --  that any attributes attached to Typ's original node are preserved.

      procedure Wrap_Imported_Subprogram (E : Entity_Id);
      --  If E is an entity for an imported subprogram with pre/post-conditions
      --  then this procedure will create a wrapper to ensure that proper run-
      --  time checking of the pre/postconditions. See body for details.

      -------------------
      -- Add_To_Result --
      -------------------

      procedure Add_To_Result (Fnod : Node_Id) is
      begin
         Append_New_To (Result, Fnod);
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
                  if Attribute_Name (N) in Name_Access | Name_Unchecked_Access
                    and then Is_Entity_Name (Prefix (N))
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

               when others =>
                  return OK;
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

      -------------------------------
      -- Check_No_Parts_Violations --
      -------------------------------

      procedure Check_No_Parts_Violations
        (Typ : Entity_Id; Aspect_No_Parts : Aspect_Id)
      is

         function Find_Aspect_No_Parts
           (Typ : Entity_Id) return Node_Id;
         --  Search for Aspect_No_Parts on a given type. When
         --  the aspect is not explicity specified Empty is returned.

         function Get_Aspect_No_Parts_Value
           (Typ : Entity_Id) return Entity_Id;
         --  Obtain the value for the Aspect_No_Parts on a given
         --  type. When the aspect is not explicitly specified Empty is
         --  returned.

         function Has_Aspect_No_Parts
           (Typ : Entity_Id) return Boolean;
         --  Predicate function which identifies whether No_Parts
         --  is explicitly specified on a given type.

         -------------------------------------
         -- Find_Aspect_No_Parts --
         -------------------------------------

         function Find_Aspect_No_Parts
           (Typ : Entity_Id) return Node_Id
         is
            Partial_View : constant Entity_Id :=
              Incomplete_Or_Partial_View (Typ);

            Aspect_Spec : Entity_Id :=
              Find_Aspect (Typ, Aspect_No_Parts);
            Curr_Aspect_Spec : Entity_Id;
         begin
            --  Examine Typ's associated node, when present, since aspect
            --  specifications do not get transferred when nodes get rewritten.

            --  For example, this can happen in the expansion of array types

            if No (Aspect_Spec)
              and then Present (Associated_Node_For_Itype (Typ))
              and then Nkind (Associated_Node_For_Itype (Typ))
                         = N_Full_Type_Declaration
            then
               Aspect_Spec :=
                 Find_Aspect
                   (Id => Defining_Identifier
                            (Associated_Node_For_Itype (Typ)),
                    A  => Aspect_No_Parts);
            end if;

            --  Examine aspects specifications on private type declarations

            --  Should Find_Aspect be improved to handle this case ???

            if No (Aspect_Spec)
              and then Present (Partial_View)
              and then Present
                         (Aspect_Specifications
                           (Declaration_Node
                             (Partial_View)))
            then
               Curr_Aspect_Spec :=
                 First
                   (Aspect_Specifications
                     (Declaration_Node
                       (Partial_View)));

               --  Search through aspects present on the private type

               while Present (Curr_Aspect_Spec) loop
                  if Get_Aspect_Id (Curr_Aspect_Spec) = Aspect_No_Parts then
                     Aspect_Spec := Curr_Aspect_Spec;
                     exit;
                  end if;

                  Next (Curr_Aspect_Spec);
               end loop;

            end if;

            --  When errors are posted on the aspect return Empty

            if Error_Posted (Aspect_Spec) then
               return Empty;
            end if;

            return Aspect_Spec;
         end Find_Aspect_No_Parts;

         ------------------------------------------
         -- Get_Aspect_No_Parts_Value --
         ------------------------------------------

         function Get_Aspect_No_Parts_Value
           (Typ : Entity_Id) return Entity_Id
         is
            Aspect_Spec : constant Entity_Id :=
              Find_Aspect_No_Parts (Typ);
         begin
            --  Return the value of the aspect when present

            if Present (Aspect_Spec) then

               --  No expression is the same as True

               if No (Expression (Aspect_Spec)) then
                  return Standard_True;
               end if;

               --  Assume its expression has already been constant folded into
               --  a Boolean value and return its value.

               return Entity (Expression (Aspect_Spec));
            end if;

            --  Otherwise, the aspect is not specified - so return Empty

            return Empty;
         end Get_Aspect_No_Parts_Value;

         ------------------------------------
         -- Has_Aspect_No_Parts --
         ------------------------------------

         function Has_Aspect_No_Parts
           (Typ : Entity_Id) return Boolean
         is (Present (Find_Aspect_No_Parts (Typ)));

         --  Generic instances

         -------------------------------------------
         -- Get_Generic_Formal_Types_In_Hierarchy --
         -------------------------------------------

         function Get_Generic_Formal_Types_In_Hierarchy
           is new Collect_Types_In_Hierarchy (Predicate => Is_Generic_Formal);
         --  Return a list of all types within a given type's hierarchy which
         --  are generic formals.

         ----------------------------------------
         -- Get_Types_With_Aspect_In_Hierarchy --
         ----------------------------------------

         function Get_Types_With_Aspect_In_Hierarchy
           is new Collect_Types_In_Hierarchy
                    (Predicate => Has_Aspect_No_Parts);
         --  Returns a list of all types within a given type's hierarchy which
         --  have the Aspect_No_Parts specified.

         --  Local declarations

         Aspect_Value      : Entity_Id;
         Curr_Value        : Entity_Id;
         Curr_Typ_Elmt     : Elmt_Id;
         Curr_Body_Elmt    : Elmt_Id;
         Curr_Formal_Elmt  : Elmt_Id;
         Gen_Bodies        : Elist_Id;
         Gen_Formals       : Elist_Id;
         Scop              : Entity_Id;
         Types_With_Aspect : Elist_Id;

      --  Start of processing for Check_No_Parts_Violations

      begin
         --  Nothing to check if the type is elementary or artificial

         if Is_Elementary_Type (Typ) or else not Comes_From_Source (Typ) then
            return;
         end if;

         Types_With_Aspect := Get_Types_With_Aspect_In_Hierarchy (Typ);

         --  Nothing to check if there are no types with No_Parts specified

         if Is_Empty_Elmt_List (Types_With_Aspect) then
            return;
         end if;

         --  Set name for all errors below

         Error_Msg_Name_1 := Aspect_Names (Aspect_No_Parts);

         --  Obtain the aspect value for No_Parts for comparison

         Aspect_Value :=
           Get_Aspect_No_Parts_Value
             (Node (First_Elmt (Types_With_Aspect)));

         --  When the value is True and there are controlled/task parts or the
         --  type itself is controlled/task, trigger the appropriate error.

         if Aspect_Value = Standard_True then
            if Aspect_No_Parts = Aspect_No_Controlled_Parts then
               if Is_Controlled (Typ) or else Has_Controlled_Component (Typ)
               then
                  Error_Msg_N
                    ("aspect % applied to controlled type &", Typ);
               end if;

            elsif Aspect_No_Parts = Aspect_No_Task_Parts then
               if Has_Task (Typ) then
                  Error_Msg_N
                    ("aspect % applied to task type &", Typ);
                  Error_Msg_N
                    ("\replace task components with access-to-task-type "
                     & "components", Typ);
               end if;

            else
               raise Program_Error;
            end if;
         end if;

         --  Move through Types_With_Aspect - checking that the value specified
         --  for their corresponding Aspect_No_Parts do not override each
         --  other.

         Curr_Typ_Elmt := First_Elmt (Types_With_Aspect);
         while Present (Curr_Typ_Elmt) loop
            Curr_Value :=
              Get_Aspect_No_Parts_Value (Node (Curr_Typ_Elmt));

            --  Compare the aspect value against the current type

            if Curr_Value /= Aspect_Value then
               Error_Msg_NE
                 ("cannot override aspect % of "
                   & "ancestor type &", Typ, Node (Curr_Typ_Elmt));
               return;
            end if;

            Next_Elmt (Curr_Typ_Elmt);
         end loop;

         --  Issue an error if the aspect applies to a type declared inside a
         --  generic body and if said type derives from or has a component
         --  of ageneric formal type - since those are considered to have
         --  controlled/task parts and have Aspect_No_Parts specified as
         --  False by default (RM H.4.1(4/5) is about the language-defined
         --  No_Controlled_Parts aspect, and we are using the same rules for
         --  No_Task_Parts).

         --  We do not check tagged types since deriving from a formal type
         --  within an enclosing generic unit is already illegal
         --  (RM 3.9.1 (4/2)).

         if Aspect_Value = Standard_True
           and then In_Generic_Body (Typ)
           and then not Is_Tagged_Type (Typ)
         then
            Gen_Bodies  := New_Elmt_List;
            Gen_Formals :=
              Get_Generic_Formal_Types_In_Hierarchy
                (Typ                => Typ,
                 Examine_Components => True);

            --  Climb scopes collecting generic bodies

            Scop := Scope (Typ);
            while Present (Scop) and then Scop /= Standard_Standard loop

               --  Generic package body

               if Ekind (Scop) = E_Generic_Package
                 and then In_Package_Body (Scop)
               then
                  Append_Elmt (Scop, Gen_Bodies);

               --  Generic subprogram body

               elsif Is_Generic_Subprogram (Scop) then
                  Append_Elmt (Scop, Gen_Bodies);
               end if;

               Scop := Scope (Scop);
            end loop;

            --  Warn about the improper use of Aspect_No_Parts on a type
            --  declaration deriving from or that has a component of a generic
            --  formal type within the formal type's corresponding generic
            --  body by moving through all formal types in Typ's hierarchy and
            --  checking if they are formals in any of the enclosing generic
            --  bodies.

            --  However, a special exception gets made for formal types which
            --  derive from a type which has Aspect_No_Parts True.

            --  For example:

            --  generic
            --     type Form is private;
            --  package G is
            --     type Type_A is new Form with No_Controlled_Parts; --  OK
            --  end;
            --
            --  package body G is
            --     type Type_B is new Form with No_Controlled_Parts; --  ERROR
            --  end;

            --  generic
            --     type Form is private;
            --  package G is
            --     type Type_A is record C : Form; end record
            --       with No_Controlled_Parts;                       --  OK
            --  end;
            --
            --  package body G is
            --     type Type_B is record C : Form; end record
            --       with No_Controlled_Parts;                       --  ERROR
            --  end;

            --  type Root is tagged null record with No_Controlled_Parts;
            --
            --  generic
            --     type Form is new Root with private;
            --  package G is
            --     type Type_A is record C : Form; end record
            --       with No_Controlled_Parts;                       --  OK
            --  end;
            --
            --  package body G is
            --     type Type_B is record C : Form; end record
            --       with No_Controlled_Parts;                       --  OK
            --  end;

            Curr_Formal_Elmt := First_Elmt (Gen_Formals);
            while Present (Curr_Formal_Elmt) loop

               Curr_Body_Elmt := First_Elmt (Gen_Bodies);
               while Present (Curr_Body_Elmt) loop

                  --  Obtain types in the formal type's hierarchy which have
                  --  the aspect specified.

                  Types_With_Aspect :=
                    Get_Types_With_Aspect_In_Hierarchy
                      (Node (Curr_Formal_Elmt));

                  --  We found a type declaration in a generic body where both
                  --  Aspect_No_Parts is true and one of its ancestors is a
                  --  generic formal type.

                  if Scope (Node (Curr_Formal_Elmt)) =
                       Node (Curr_Body_Elmt)

                    --  Check that no ancestors of the formal type have
                    --  Aspect_No_Parts True before issuing the error.

                    and then (Is_Empty_Elmt_List (Types_With_Aspect)
                               or else
                                 Get_Aspect_No_Parts_Value
                                   (Node (First_Elmt (Types_With_Aspect)))
                                  = Standard_False)
                  then
                     Error_Msg_Node_1 := Typ;
                     Error_Msg_Node_2 := Node (Curr_Formal_Elmt);
                     Error_Msg
                       ("aspect % cannot be applied to "
                         & "type & which has an ancestor or component of "
                         & "formal type & within the formal type's "
                         & "corresponding generic body", Sloc (Typ));
                  end if;

                  Next_Elmt (Curr_Body_Elmt);
               end loop;

               Next_Elmt (Curr_Formal_Elmt);
            end loop;
         end if;
      end Check_No_Parts_Violations;

      ---------------------------------
      -- Check_Suspicious_Convention --
      ---------------------------------

      procedure Check_Suspicious_Convention (Rec_Type : Entity_Id) is
      begin
         if Has_Discriminants (Rec_Type)
           and then Is_Base_Type (Rec_Type)
           and then not Is_Unchecked_Union (Rec_Type)
           and then (Convention (Rec_Type) = Convention_C
                       or else
                     Convention (Rec_Type) = Convention_CPP)
           and then Comes_From_Source (Rec_Type)
           and then not In_Instance
           and then not Has_Warnings_Off (Rec_Type)
         then
            declare
               Cprag : constant Node_Id :=
                         Get_Rep_Pragma (Rec_Type, Name_Convention);
               A2    : Node_Id;

            begin
               if Present (Cprag) then
                  A2 := Next (First (Pragma_Argument_Associations (Cprag)));

                  if Convention (Rec_Type) = Convention_C then
                     Error_Msg_N
                       ("?x?discriminated record has no direct equivalent in "
                        & "C", A2);
                  else
                     Error_Msg_N
                       ("?x?discriminated record has no direct equivalent in "
                        & "C++", A2);
                  end if;

                  Error_Msg_NE
                    ("\?x?use of convention for type& is dubious",
                     A2, Rec_Type);
               end if;
            end;
         end if;
      end Check_Suspicious_Convention;

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
                             ("?.m?2 '*'*^' may have been intended here",
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

         Clause : Node_Id;
         --  Set to Component_Size clause or Atomic pragma, if any

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

            --  Propagate flags from component type

            Propagate_Concurrent_Flags (Arr, Ctyp);
            Propagate_Controlled_Flags (Arr, Ctyp, Comp => True);

            if Has_Unchecked_Union (Ctyp) then
               Set_Has_Unchecked_Union (Arr);
            end if;

            --  The array type requires its own invariant procedure in order to
            --  verify the component invariant over all elements. In GNATprove
            --  mode, the component invariants are checked by other means. They
            --  should not be added to the array type invariant procedure, so
            --  that the procedure can be used to check the array type
            --  invariants if any.

            if Has_Invariants (Ctyp)
              and then not GNATprove_Mode
            then
               Set_Has_Own_Invariants (Arr);
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

            --  Check for Aliased or Atomic or Full Access or Independent
            --  components with an unsuitable component size clause given.
            --  The main purpose is to give an error when bit packing would
            --  be required to honor the component size, because bit packing
            --  is incompatible with these aspects; when bit packing is not
            --  required, the final validation of the component size may be
            --  left to the back end.

            if Has_Component_Size_Clause (Arr) then
               CS_Check : declare
                  procedure Complain_CS (T : String; Min : Boolean := False);
                  --  Output an error message for an unsuitable component size
                  --  clause for independent components (T is either "aliased"
                  --  or "atomic" or "volatile full access" or "independent").

                  -----------------
                  -- Complain_CS --
                  -----------------

                  procedure Complain_CS (T : String; Min : Boolean := False) is
                  begin
                     Clause :=
                       Get_Attribute_Definition_Clause
                         (FS, Attribute_Component_Size);

                     Error_Msg_N
                       ("incorrect component size for " & T & " components",
                        Clause);

                     if Known_Static_Esize (Ctyp) then
                        Error_Msg_Uint_1 := Esize (Ctyp);
                        if Min then
                           Error_Msg_N ("\minimum allowed value is^", Clause);
                        else
                           Error_Msg_N ("\only allowed value is^", Clause);
                        end if;
                     else
                        Error_Msg_N
                          ("\must be multiple of storage unit", Clause);
                     end if;
                  end Complain_CS;

               --  Start of processing for CS_Check

               begin
                  --  OK if the component size and object size are equal, or
                  --  if the component size is a multiple of the storage unit.

                  if (if Known_Static_Esize (Ctyp)
                       then Component_Size (Arr) = Esize (Ctyp)
                       else Component_Size (Arr) mod System_Storage_Unit = 0)
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

                  --  For Independent a larger size is permitted

                  elsif (Has_Independent_Components (Arr)
                          or else Is_Independent (Ctyp))
                    and then (not Known_Static_Esize (Ctyp)
                               or else Component_Size (Arr) < Esize (Ctyp))
                  then
                     Complain_CS ("independent", Min => True);
                  end if;
               end CS_Check;

            --  Check for Aliased or Atomic or Full Access or Independent
            --  components with an unsuitable aspect/pragma Pack given.
            --  The main purpose is to prevent bit packing from occurring,
            --  because bit packing is incompatible with these aspects; when
            --  bit packing cannot occur, the final handling of the packing
            --  may be left to the back end.

            elsif Is_Packed (Arr) and then Known_Static_RM_Size (Ctyp) then
               Pack_Check : declare

                  procedure Complain_Pack (T : String);
                  --  Output a warning message for an unsuitable aspect/pragma
                  --  Pack for independent components (T is either "aliased" or
                  --  "atomic" or "volatile full access" or "independent") and
                  --  reset the Is_Packed flag on the array type.

                  -------------------
                  -- Complain_Pack --
                  -------------------

                  procedure Complain_Pack (T : String) is
                  begin
                     Error_Msg_N
                       ("?cannot pack " & T & " components (RM 13.2(7))",
                        Get_Rep_Pragma (FS, Name_Pack));

                     Set_Is_Packed (Arr, False);
                  end Complain_Pack;

               --  Start of processing for Pack_Check

               begin
                  --  OK if the component size and object size are equal, or
                  --  if the component size is a multiple of the storage unit.

                  if (if Known_Static_Esize (Ctyp)
                       then RM_Size (Ctyp) = Esize (Ctyp)
                       else RM_Size (Ctyp) mod System_Storage_Unit = 0)
                  then
                     null;

                  elsif Has_Aliased_Components (Arr) then
                     Complain_Pack ("aliased");

                  elsif Has_Atomic_Components (Arr)
                    or else Is_Atomic (Ctyp)
                  then
                     Complain_Pack ("atomic");

                  elsif Is_Volatile_Full_Access (Ctyp) then
                     Complain_Pack ("volatile full access");

                  elsif Has_Independent_Components (Arr)
                    or else Is_Independent (Ctyp)
                  then
                     Complain_Pack ("independent");
                  end if;
               end Pack_Check;
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
               if Is_Packed (Arr)
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

                  if Csiz /= 0 and then Known_Alignment (Ctyp) then
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

               if 1 <= Csiz and then Csiz <= System_Max_Integer_Size then
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

                     if No (Comp_Size_C) then
                        Set_Component_Size (Arr, Csiz);
                     end if;

                     --  Check for base type of 8, 16, 32 bits, where an
                     --  unsigned subtype has a length one less than the
                     --  base type (e.g. Natural subtype of Integer).

                     --  In such cases, if a component size was not set
                     --  explicitly, then generate a warning.

                     if Has_Pragma_Pack (Arr)
                       and then No (Comp_Size_C)
                       and then (Csiz = 7 or else Csiz = 15 or else Csiz = 31)
                       and then Known_Esize (Base_Type (Ctyp))
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

                     --  Bit packing is never needed for 8, 16, 32, 64 or 128

                     if Addressable (Csiz) then

                        --  If the Esize of the component is known and equal to
                        --  the component size then even packing is not needed.

                        if Known_Static_Esize (Ctyp)
                          and then Esize (Ctyp) = Csiz
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
                     --  byte pack composite types efficiently. That's not true
                     --  for discrete types because every read would generate a
                     --  lot of instructions, so we keep using the manipulation
                     --  routines of the runtime for them.

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
            --  Acquire alignment from base type. Known_Alignment of the base
            --  type is False for Wide_String, for example.

            if not Known_Alignment (Arr)
              and then Known_Alignment (Base_Type (Arr))
            then
               Set_Alignment (Arr, Alignment (Base_Type (Arr)));
               Adjust_Esize_Alignment (Arr);
            end if;
         end if;

         --  Specific checks for bit-packed arrays

         if Is_Bit_Packed_Array (Arr) then

            --  Check number of elements for bit-packed arrays that come from
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
         --  always packed (even if it is not bit-packed).

         if Non_Standard_Enum then
            Set_Has_Non_Standard_Rep (Base_Type (Arr));
            Set_Is_Packed            (Base_Type (Arr));
         end if;

         Set_Component_Alignment_If_Not_Set (Arr);

         --  If the array is packed and bit-packed or packed to eliminate holes
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
               Copy_Esize (To => Arr, From => Packed_Array_Impl_Type (Arr));
               Copy_RM_Size (To => Arr, From => Packed_Array_Impl_Type (Arr));
            end if;

            if not Has_Alignment_Clause (Arr) then
               Copy_Alignment
                 (To => Arr, From => Packed_Array_Impl_Type (Arr));
            end if;
         end if;

         <<Skip_Packed>>

         --  A Ghost type cannot have a component of protected or task type
         --  (SPARK RM 6.9(21)).

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
         procedure Check_Large_Modular_Array (Typ : Entity_Id);
         --  Check that the size of array type Typ can be computed without
         --  overflow, and generates a Storage_Error otherwise. This is only
         --  relevant for array types whose index is a modular type with
         --  Standard_Long_Long_Integer_Size bits: wrap-around arithmetic
         --  might yield a meaningless value for the length of the array,
         --  or its corresponding attribute.

         procedure Check_Pragma_Thread_Local_Storage (Var_Id : Entity_Id);
         --  Ensure that the initialization state of variable Var_Id subject
         --  to pragma Thread_Local_Storage agrees with the semantics of the
         --  pragma.

         function Has_Default_Initialization
           (Obj_Id : Entity_Id) return Boolean;
         --  Determine whether object Obj_Id default initialized

         -------------------------------
         -- Check_Large_Modular_Array --
         -------------------------------

         procedure Check_Large_Modular_Array (Typ : Entity_Id) is
            Obj_Loc : constant Source_Ptr := Sloc (E);
            Idx_Typ : Entity_Id;

         begin
            --  Nothing to do when expansion is disabled because this routine
            --  generates a runtime check.

            if not Expander_Active then
               return;

            --  Nothing to do for String literal subtypes because their index
            --  cannot be a modular type.

            elsif Ekind (Typ) = E_String_Literal_Subtype then
               return;

            --  Nothing to do for an imported object because the object will
            --  be created on the exporting side.

            elsif Is_Imported (E) then
               return;

            --  Nothing to do for unconstrained array types. This case arises
            --  when the object declaration is illegal.

            elsif not Is_Constrained (Typ) then
               return;
            end if;

            Idx_Typ := Etype (First_Index (Typ));

            --  To prevent arithmetic overflow with large values, we raise
            --  Storage_Error under the following guard:
            --
            --    (Arr'Last / 2 - Arr'First / 2) > (2 ** 30)
            --
            --  This takes care of the boundary case, but it is preferable to
            --  use a smaller limit, because even on 64-bit architectures an
            --  array of more than 2 ** 30 bytes is likely to raise
            --  Storage_Error.

            if Is_Modular_Integer_Type (Idx_Typ)
              and then RM_Size (Idx_Typ) = Standard_Long_Long_Integer_Size
            then
               --  Ensure that the type of the object is elaborated before
               --  the check itself is emitted to avoid elaboration issues
               --  in the code generator at the library level.

               if Is_Itype (Etype (E))
                 and then In_Open_Scopes (Scope (Etype (E)))
               then
                  declare
                     Ref_Node : constant Node_Id :=
                                  Make_Itype_Reference (Obj_Loc);
                  begin
                     Set_Itype (Ref_Node, Etype (E));
                     Insert_Action (Declaration_Node (E), Ref_Node);
                  end;
               end if;

               Insert_Action (Declaration_Node (E),
                 Make_Raise_Storage_Error (Obj_Loc,
                   Condition =>
                     Make_Op_Ge (Obj_Loc,
                       Left_Opnd  =>
                         Make_Op_Subtract (Obj_Loc,
                           Left_Opnd  =>
                             Make_Op_Divide (Obj_Loc,
                               Left_Opnd  =>
                                 Make_Attribute_Reference (Obj_Loc,
                                   Prefix         =>
                                     New_Occurrence_Of (Typ, Obj_Loc),
                                   Attribute_Name => Name_Last),
                               Right_Opnd =>
                                 Make_Integer_Literal (Obj_Loc, Uint_2)),
                           Right_Opnd =>
                             Make_Op_Divide (Obj_Loc,
                               Left_Opnd =>
                                 Make_Attribute_Reference (Obj_Loc,
                                   Prefix         =>
                                     New_Occurrence_Of (Typ, Obj_Loc),
                                   Attribute_Name => Name_First),
                               Right_Opnd =>
                                 Make_Integer_Literal (Obj_Loc, Uint_2))),
                       Right_Opnd =>
                         Make_Integer_Literal (Obj_Loc, (Uint_2 ** 30))),
                   Reason    => SE_Object_Too_Large));
            end if;
         end Check_Large_Modular_Array;

         ---------------------------------------
         -- Check_Pragma_Thread_Local_Storage --
         ---------------------------------------

         procedure Check_Pragma_Thread_Local_Storage (Var_Id : Entity_Id) is
            function Has_Incompatible_Initialization
              (Var_Decl : Node_Id) return Boolean;
            --  Determine whether variable Var_Id with declaration Var_Decl is
            --  initialized with a value that violates the semantics of pragma
            --  Thread_Local_Storage.

            -------------------------------------
            -- Has_Incompatible_Initialization --
            -------------------------------------

            function Has_Incompatible_Initialization
              (Var_Decl : Node_Id) return Boolean
            is
               Init_Expr : constant Node_Id := Expression (Var_Decl);

            begin
               --  The variable is default-initialized. This directly violates
               --  the semantics of the pragma.

               if Has_Default_Initialization (Var_Id) then
                  return True;

               --  The variable has explicit initialization. In this case only
               --  a handful of values satisfy the semantics of the pragma.

               elsif Has_Init_Expression (Var_Decl)
                 and then Present (Init_Expr)
               then
                  --  "null" is a legal form of initialization

                  if Nkind (Init_Expr) = N_Null then
                     return False;

                  --  A static expression is a legal form of initialization

                  elsif Is_Static_Expression (Init_Expr) then
                     return False;

                  --  A static aggregate is a legal form of initialization

                  elsif Nkind (Init_Expr) = N_Aggregate
                    and then Compile_Time_Known_Aggregate (Init_Expr)
                  then
                     return False;

                  --  All other initialization expressions violate the semantic
                  --  of the pragma.

                  else
                     return True;
                  end if;

               --  The variable lacks any kind of initialization, which agrees
               --  with the semantics of the pragma.

               else
                  return False;
               end if;
            end Has_Incompatible_Initialization;

            --  Local declarations

            Var_Decl : constant Node_Id := Declaration_Node (Var_Id);

         --  Start of processing for Check_Pragma_Thread_Local_Storage

         begin
            --  A variable whose initialization is suppressed lacks any kind of
            --  initialization.

            if Suppress_Initialization (Var_Id) then
               null;

            --  The variable has default initialization, or is explicitly
            --  initialized to a value other than null, static expression,
            --  or a static aggregate.

            elsif Has_Incompatible_Initialization (Var_Decl) then
               Error_Msg_NE
                 ("Thread_Local_Storage variable& is improperly initialized",
                  Var_Decl, Var_Id);
               Error_Msg_NE
                 ("\only allowed initialization is explicit NULL, static "
                  & "expression or static aggregate", Var_Decl, Var_Id);
            end if;
         end Check_Pragma_Thread_Local_Storage;

         --------------------------------
         -- Has_Default_Initialization --
         --------------------------------

         function Has_Default_Initialization
           (Obj_Id : Entity_Id) return Boolean
         is
            Obj_Decl : constant Node_Id   := Declaration_Node (Obj_Id);
            Obj_Typ  : constant Entity_Id := Etype (Obj_Id);

         begin
            return
              Comes_From_Source (Obj_Id)
                and then not Is_Imported (Obj_Id)
                and then not Has_Init_Expression (Obj_Decl)
                and then
                  ((Has_Non_Null_Base_Init_Proc (Obj_Typ)
                     and then not No_Initialization (Obj_Decl)
                     and then not Initialization_Suppressed (Obj_Typ))
                   or else
                     (Needs_Simple_Initialization (Obj_Typ)
                       and then not Is_Internal (Obj_Id)));
         end Has_Default_Initialization;

         --  Local variables

         Typ : constant Entity_Id := Etype (E);
         Def : Node_Id;

      --  Start of processing for Freeze_Object_Declaration

      begin
         --  Abstract type allowed only for C++ imported variables or constants

         --  Note: we inhibit this check for objects that do not come from
         --  source because there is at least one case (the expansion of
         --  x'Class'Input where x is abstract) where we legitimately
         --  generate an abstract object.

         if Is_Abstract_Type (Typ)
           and then Comes_From_Source (Parent (E))
           and then not (Is_Imported (E) and then Is_CPP_Class (Typ))
         then
            Def := Object_Definition (Parent (E));

            Error_Msg_N ("type of object cannot be abstract", Def);

            if Is_CPP_Class (Etype (E)) then
               Error_Msg_NE ("\} may need a cpp_constructor", Def, Typ);

            elsif Present (Expression (Parent (E))) then
               Error_Msg_N --  CODEFIX
                 ("\maybe a class-wide type was meant", Def);
            end if;
         end if;

         --  For object created by object declaration, perform required
         --  categorization (preelaborate and pure) checks. Defer these
         --  checks to freeze time since pragma Import inhibits default
         --  initialization and thus pragma Import affects these checks.

         Validate_Object_Declaration (Declaration_Node (E));

         --  If there is an address clause, check that it is valid and if need
         --  be move initialization to the freeze node.

         Check_Address_Clause (E);

         --  Similar processing is needed for aspects that may affect object
         --  layout, like Address, if there is an initialization expression.
         --  We don't do this if there is a pragma Linker_Section, because it
         --  would prevent the back end from statically initializing the
         --  object; we don't want elaboration code in that case.

         if Has_Delayed_Aspects (E)
           and then Expander_Active
           and then Is_Array_Type (Typ)
           and then Present (Expression (Declaration_Node (E)))
           and then No (Linker_Section_Pragma (E))
         then
            declare
               Decl : constant Node_Id := Declaration_Node (E);
               Lhs  : constant Node_Id := New_Occurrence_Of (E, Loc);

            begin
               --  Capture initialization value at point of declaration, and
               --  make explicit assignment legal, because object may be a
               --  constant.

               Remove_Side_Effects (Expression (Decl));
               Set_Assignment_OK (Lhs);

               --  Move initialization to freeze actions

               Append_Freeze_Action (E,
                 Make_Assignment_Statement (Loc,
                   Name       => Lhs,
                   Expression => Expression (Decl)));

               Set_No_Initialization (Decl);
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
           and then (Is_Aliased (E) or else Is_Aliased (Typ))
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

         elsif Has_Default_Initialization (E) then
            Check_Restriction
              (No_Default_Initialization, Declaration_Node (E));
         end if;

         --  Ensure that a variable subject to pragma Thread_Local_Storage
         --
         --    * Lacks default initialization, or
         --
         --    * The initialization expression is either "null", a static
         --      constant, or a compile-time known aggregate.

         if Has_Pragma_Thread_Local_Storage (E) then
            Check_Pragma_Thread_Local_Storage (E);
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

         --  For source objects that are not Imported and are library level, if
         --  no linker section pragma was given inherit the appropriate linker
         --  section from the corresponding type.

         if Comes_From_Source (E)
           and then not Is_Imported (E)
           and then Is_Library_Level_Entity (E)
           and then No (Linker_Section_Pragma (E))
         then
            Set_Linker_Section_Pragma (E, Linker_Section_Pragma (Typ));
         end if;

         --  For convention C objects of an enumeration type, warn if the size
         --  is not integer size and no explicit size given. Skip warning for
         --  Boolean and Character, and assume programmer expects 8-bit sizes
         --  for these cases.

         if (Convention (E) = Convention_C
               or else
             Convention (E) = Convention_CPP)
           and then Is_Enumeration_Type (Typ)
           and then not Is_Character_Type (Typ)
           and then not Is_Boolean_Type (Typ)
           and then Esize (Typ) < Standard_Integer_Size
           and then not Has_Size_Clause (E)
         then
            Error_Msg_Uint_1 := UI_From_Int (Standard_Integer_Size);
            Error_Msg_N
              ("??convention C enumeration object has size less than ^", E);
            Error_Msg_N ("\??use explicit size clause to set size", E);
         end if;

         --  Declaring too big an array in disabled ghost code is OK

         if Is_Array_Type (Typ) and then not Is_Ignored_Ghost_Entity (E) then
            Check_Large_Modular_Array (Typ);
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

            if not From_Limited_With (F_Type)
              and then Should_Freeze_Type (F_Type, E, N)
            then
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

               elsif not After_Last_Declaration then
                  Error_Msg_NE
                    ("type & must be fully defined before this point",
                     N,
                     F_Type);
               end if;
            end if;

            --  Check suspicious parameter for C function. These tests apply
            --  only to exported/imported subprograms.

            if Warn_On_Export_Import
              and then Comes_From_Source (E)
              and then Convention (E) in Convention_C_Family
              and then (Is_Imported (E) or else Is_Exported (E))
              and then Convention (E) /= Convention (Formal)
              and then not Has_Warnings_Off (E)
              and then not Has_Warnings_Off (F_Type)
              and then not Has_Warnings_Off (Formal)
            then
               --  Qualify mention of formals with subprogram name

               Error_Msg_Qual_Level := 1;

               --  Check suspicious use of fat C pointer, but do not emit
               --  a warning on an access to subprogram when unnesting is
               --  active.

               if Is_Access_Type (F_Type)
                 and then Known_Esize (F_Type)
                 and then Esize (F_Type) > Ttypes.System_Address_Size
                 and then (not Unnest_Subprogram_Mode
                            or else not Is_Access_Subprogram_Type (F_Type))
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
               Error_Msg_N ("\?x?foreign caller must pass bounds explicitly",
                  Warn_Node);
               Error_Msg_Qual_Level := 0;
            end if;

            if not From_Limited_With (F_Type) then
               if Is_Access_Type (F_Type) then
                  F_Type := Designated_Type (F_Type);
               end if;
            end if;

            Next_Formal (Formal);
         end loop;

         --  Case of function: similar checks on return type

         if Ekind (E) = E_Function then

            --  Freeze return type

            R_Type := Etype (E);

            --  AI05-0151: the return type may have been incomplete at the
            --  point of declaration. Replace it with the full view, unless the
            --  current type is a limited view. In that case the full view is
            --  in a different unit, and gigi finds the non-limited view after
            --  the other unit is elaborated.

            if Ekind (R_Type) = E_Incomplete_Type
              and then Present (Full_View (R_Type))
              and then not From_Limited_With (R_Type)
            then
               R_Type := Full_View (R_Type);
               Set_Etype (E, R_Type);
            end if;

            if Should_Freeze_Type (R_Type, E, N) then
               Freeze_And_Append (R_Type, N, Result);
            end if;

            --  Check suspicious return type for C function

            if Warn_On_Export_Import
              and then Comes_From_Source (E)
              and then Convention (E) in Convention_C_Family
              and then (Is_Imported (E) or else Is_Exported (E))
            then
               --  Check suspicious return of fat C pointer

               if Is_Access_Type (R_Type)
                 and then Known_Esize (R_Type)
                 and then Esize (R_Type) > Ttypes.System_Address_Size
                 and then not Has_Warnings_Off (E)
                 and then not Has_Warnings_Off (R_Type)
               then
                  Error_Msg_N
                    ("?x?return type of& does not correspond to C pointer!",
                     E);

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

           --  Assume that ASM interface knows what it is doing

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

         Aliased_Component : Boolean := False;
         --  Set True if we find at least one component which is aliased. This
         --  is used to prevent Implicit_Packing of the record, since packing
         --  cannot modify the size of alignment of an aliased component.

         All_Elem_Components : Boolean := True;
         --  True if all components are of a type whose underlying type is
         --  elementary.

         All_Sized_Components : Boolean := True;
         --  True if all components have a known RM_Size

         All_Storage_Unit_Components : Boolean := True;
         --  True if all components have an RM_Size that is a multiple of the
         --  storage unit.

         Elem_Component_Total_Esize : Uint := Uint_0;
         --  Accumulates total Esize values of all elementary components. Used
         --  for processing of Implicit_Packing.

         Final_Storage_Only : Boolean := True;
         --  Used to compute the Finalize_Storage_Only flag

         Placed_Component : Boolean := False;
         --  Set True if we find at least one component with a component
         --  clause (used to warn about useless Bit_Order pragmas, and also
         --  to detect cases where Implicit_Packing may have an effect).

         Relaxed_Finalization : Boolean := True;
         --  Used to compute the Has_Relaxed_Finalization flag

         Sized_Component_Total_RM_Size : Uint := Uint_0;
         --  Accumulates total RM_Size values of all sized components. Used
         --  for processing of Implicit_Packing.

         Sized_Component_Total_Round_RM_Size : Uint := Uint_0;
         --  Accumulates total RM_Size values of all sized components, rounded
         --  individually to a multiple of the storage unit.

         SSO_ADC : Node_Id;
         --  Scalar_Storage_Order attribute definition clause for the record

         SSO_ADC_Component : Boolean := False;
         --  Set True if we find at least one component whose type has a
         --  Scalar_Storage_Order attribute definition clause.

         Unplaced_Component : Boolean := False;
         --  Set True if we find at least one component with no component
         --  clause (used to warn about useless Pack pragmas).

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

         procedure Warn_If_Implicitly_Inherited_Aspects (Tag_Typ : Entity_Id);
         --  Report a warning for Tag_Typ when it implicitly inherits the
         --  First_Controlling_Parameter aspect but does not explicitly
         --  specify it.

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
               Create_Extra_Formals (Desig);
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

         ------------------------------------------
         -- Warn_If_Implicitly_Inherited_Aspects --
         ------------------------------------------

         procedure Warn_If_Implicitly_Inherited_Aspects (Tag_Typ : Entity_Id)
         is
            function Has_First_Ctrl_Param_Aspect return Boolean;
            --  Determines if Tag_Typ explicitly has the aspect/pragma
            --  First_Controlling_Parameter.

            ---------------------------------
            -- Has_First_Ctrl_Param_Aspect --
            ---------------------------------

            function Has_First_Ctrl_Param_Aspect return Boolean is
               Decl_Nod   : constant Node_Id := Parent (Tag_Typ);
               Asp_Nod    : Node_Id;
               Nod        : Node_Id;
               Pragma_Arg : Node_Id;
               Pragma_Ent : Entity_Id;

            begin
               pragma Assert (Nkind (Decl_Nod) = N_Full_Type_Declaration);

               if Present (Aspect_Specifications (Decl_Nod)) then
                  Asp_Nod := First (Aspect_Specifications (Decl_Nod));
                  while Present (Asp_Nod) loop
                     if Chars (Identifier (Asp_Nod))
                       = Name_First_Controlling_Parameter
                     then
                        return True;
                     end if;

                     Next (Asp_Nod);
                  end loop;
               end if;

               --  Search for the occurrence of the pragma

               Nod := Next (Decl_Nod);
               while Present (Nod) loop
                  if Nkind (Nod) = N_Pragma
                    and then Chars (Pragma_Identifier (Nod))
                               = Name_First_Controlling_Parameter
                    and then Present (Pragma_Argument_Associations (Nod))
                  then
                     Pragma_Arg :=
                       Expression (First (Pragma_Argument_Associations (Nod)));

                     if Nkind (Pragma_Arg) = N_Identifier
                       and then Present (Entity (Pragma_Arg))
                     then
                        Pragma_Ent := Entity (Pragma_Arg);

                        if Pragma_Ent = Tag_Typ
                          or else
                            (Is_Concurrent_Type (Pragma_Ent)
                               and then
                                 Corresponding_Record_Type (Pragma_Ent)
                                   = Tag_Typ)
                        then
                           return True;
                        end if;
                     end if;
                  end if;

                  Next (Nod);
               end loop;

               return False;
            end Has_First_Ctrl_Param_Aspect;

            --  Local Variables

            Has_Aspect_First_Ctrl_Param : constant Boolean :=
                                            Has_First_Ctrl_Param_Aspect;

         --  Start of processing for Warn_Implicitly_Inherited_Aspects

         begin
            --  Handle cases where reporting the warning is not needed

            if not Warn_On_Non_Dispatching_Primitives then
               return;

            --  No check needed when this is the full view of a private type
            --  declaration since the pragma/aspect must be placed and checked
            --  in the partial view, and it is implicitly propagated to the
            --  full view.

            elsif Has_Private_Declaration (Tag_Typ)
              and then Is_Tagged_Type (Incomplete_Or_Partial_View (Tag_Typ))
            then
               return;

            --  Similar case but applied to concurrent types

            elsif Is_Concurrent_Record_Type (Tag_Typ)
              and then Has_Private_Declaration
                         (Corresponding_Concurrent_Type (Tag_Typ))
              and then Is_Tagged_Type
                         (Incomplete_Or_Partial_View
                           (Corresponding_Concurrent_Type (Tag_Typ)))
            then
               return;
            end if;

            if Etype (Tag_Typ) /= Tag_Typ
              and then Has_First_Controlling_Parameter_Aspect (Etype (Tag_Typ))
            then
               --  The attribute was implicitly inherited
               pragma Assert
                 (Has_First_Controlling_Parameter_Aspect (Tag_Typ));

               --  No warning needed when the current tagged type is not
               --  an interface type since by definition the aspect is
               --  implicitly propagated from its parent type; the warning
               --  is reported on interface types since it may not be so
               --  clear when some implemented interface types have the
               --  aspect and other interface types don't have it. For
               --  interface types, we don't report the warning when the
               --  interface type is an extension of a single interface
               --  type (for similarity with the behavior with regular
               --  tagged types).

               if not Has_Aspect_First_Ctrl_Param
                 and then Is_Interface (Tag_Typ)
                 and then not Is_Empty_Elmt_List (Interfaces (Tag_Typ))
               then
                  Error_Msg_N
                    ("?_j?implicitly inherits aspect 'First_'Controlling_'"
                     & "Parameter!", Tag_Typ);
                  Error_Msg_NE
                    ("\?_j?from & and must be confirmed explicitly!",
                     Tag_Typ, Etype (Tag_Typ));
               end if;

            elsif Present (Interfaces (Tag_Typ))
              and then not Is_Empty_Elmt_List (Interfaces (Tag_Typ))
            then
               --  To maintain consistency with the behavior when the aspect
               --  is implicitly inherited from its parent type, we do not
               --  report a warning for concurrent record types that implement
               --  a single interface type. By definition, the aspect is
               --  propagated from that interface type as if it were the parent
               --  type. For example:

               --     type Iface is interface with First_Controlling_Parameter;
               --     task type T is new Iface with ...

               if Is_Concurrent_Record_Type (Tag_Typ)
                 and then No (Next_Elmt (First_Elmt (Interfaces (Tag_Typ))))
               then
                  null;

               else
                  declare
                     Elmt  : Elmt_Id := First_Elmt (Interfaces (Tag_Typ));
                     Iface : Entity_Id;

                  begin
                     while Present (Elmt) loop
                        Iface := Node (Elmt);
                        pragma Assert (Present (Iface));

                        if Has_First_Controlling_Parameter_Aspect (Iface)
                          and then not Has_Aspect_First_Ctrl_Param
                        then
                           pragma Assert
                             (Has_First_Controlling_Parameter_Aspect
                               (Tag_Typ));
                           Error_Msg_N
                             ("?_j?implicitly inherits aspect 'First_'"
                              & "Controlling_'Parameter", Tag_Typ);
                           Error_Msg_NE
                             ("\?_j?from & and must be confirmed explicitly!",
                              Tag_Typ, Iface);
                           exit;
                        end if;

                        Next_Elmt (Elmt);
                     end loop;
                  end;
               end if;
            end if;
         end Warn_If_Implicitly_Inherited_Aspects;

      --  Start of processing for Freeze_Record_Type

      begin
         --  Freeze components and embedded subtypes

         Comp := First_Entity (Rec);
         Prev := Empty;
         while Present (Comp) loop
            if Is_Aliased (Comp) then
               Aliased_Component := True;
            end if;

            --  Handle the component and discriminant case

            if Ekind (Comp) in E_Component | E_Discriminant then
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
               declare
                  Comp_Type : constant Entity_Id := Etype (Comp);
                  Comp_Size : constant Uint := RM_Size (Comp_Type);
                  SSU       : constant Int := Ttypes.System_Storage_Unit;

               begin
                  Sized_Component_Total_RM_Size :=
                    Sized_Component_Total_RM_Size + Comp_Size;

                  Sized_Component_Total_Round_RM_Size :=
                    Sized_Component_Total_Round_RM_Size +
                      (Comp_Size + SSU - 1) / SSU * SSU;

                  if Present (Underlying_Type (Comp_Type))
                    and then Is_Elementary_Type (Underlying_Type (Comp_Type))
                  then
                     Elem_Component_Total_Esize :=
                       Elem_Component_Total_Esize + Esize (Comp_Type);
                  else
                     All_Elem_Components := False;

                     if Comp_Size mod SSU /= 0 then
                        All_Storage_Unit_Components := False;
                     end if;
                  end if;
               end;
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
                        Link_Entities (Prev, Next_Entity (Comp));
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
              and then
                Nkind (Parent (Comp))
                  in N_Component_Declaration | N_Discriminant_Specification
              and then Present (Expression (Parent (Comp)))
            then
               declare
                  Alloc : constant Node_Id :=
                            Unqualify (Expression (Parent (Comp)));

               begin
                  if Nkind (Alloc) = N_Allocator then

                     --  If component is pointer to a class-wide type, freeze
                     --  the specific type in the expression being allocated.
                     --  The expression may be a subtype indication, in which
                     --  case freeze the subtype mark.

                     if Is_Class_Wide_Type (Designated_Type (Etype (Comp)))
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
            --  for reversed bit order, when not using reverse SSO. If an error
            --  has been reported on Rec already (such as SSO incompatible with
            --  bit order), don't bother adjusting as this may generate extra
            --  noise.

            elsif Reverse_Bit_Order (Rec)
              and then not Reverse_Storage_Order (Rec)
              and then not Error_Posted (Rec)
            then
               Adjust_Record_For_Reverse_Bit_Order (Rec);

            --  Case where we have both an explicit Bit_Order and the same
            --  Scalar_Storage_Order: leave record untouched, the back-end
            --  will take care of required layout conversions.

            else
               null;

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
                 ("?r?pragma Pack has no effect, no unplaced components",
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

            --  Check for tasks, protected and controlled components, unchecked
            --  unions, and type invariants.

            Comp := First_Component (Rec);
            while Present (Comp) loop
               Propagate_Concurrent_Flags (Rec, Etype (Comp));

               --  Do not set Has_Controlled_Component on a class-wide
               --  equivalent type. See Make_CW_Equivalent_Type.

               if not Is_Class_Wide_Equivalent_Type (Rec)
                 and then
                   (Has_Controlled_Component (Etype (Comp))
                     or else
                       (Chars (Comp) /= Name_uParent
                         and then Is_Controlled (Etype (Comp)))
                     or else
                       (Is_Protected_Type (Etype (Comp))
                         and then
                           Present (Corresponding_Record_Type (Etype (Comp)))
                         and then
                           Has_Controlled_Component
                             (Corresponding_Record_Type (Etype (Comp)))))
               then
                  Set_Has_Controlled_Component (Rec);
                  Final_Storage_Only :=
                    Final_Storage_Only
                      and then Finalize_Storage_Only (Etype (Comp));
                  Relaxed_Finalization :=
                    Relaxed_Finalization
                      and then Has_Relaxed_Finalization (Etype (Comp));
               end if;

               if Has_Unchecked_Union (Etype (Comp)) then
                  Set_Has_Unchecked_Union (Rec);
               end if;

               --  The record type requires its own invariant procedure in
               --  order to verify the invariant of each individual component.
               --  Do not consider internal components such as _parent because
               --  parent class-wide invariants are always inherited.
               --  In GNATprove mode, the component invariants are checked by
               --  other means. They should not be added to the record type
               --  invariant procedure, so that the procedure can be used to
               --  check the recordy type invariants if any.

               if Comes_From_Source (Comp)
                 and then Has_Invariants (Etype (Comp))
                   and then not GNATprove_Mode
               then
                  Set_Has_Own_Invariants (Rec);
               end if;

               --  Scan component declaration for likely misuses of current
               --  instance, either in a constraint or a default expression.

               if Has_Per_Object_Constraint (Comp) then
                  Check_Current_Instance (Parent (Comp));
               end if;

               Next_Component (Comp);
            end loop;

            --  For a type that is not directly controlled but has controlled
            --  components, Finalize_Storage_Only is set if all the controlled
            --  components are Finalize_Storage_Only. The same processing is
            --  appled to Has_Relaxed_Finalization.

            if not Is_Controlled (Rec) and then Has_Controlled_Component (Rec)
            then
               Set_Finalize_Storage_Only    (Rec, Final_Storage_Only);
               Set_Has_Relaxed_Finalization (Rec, Relaxed_Finalization);
            end if;
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
         --  bit-packed arrays.

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
           --  if this is not the case), if we can compute it, i.e. if we have
           --  only elementary components. Otherwise, we have at least one
           --  composite component and we want to implicitly pack only if bit
           --  packing is required for it, as we are sure in this case that
           --  the back end cannot do the expected layout without packing.

           and then
              ((All_Elem_Components
                 and then RM_Size (Rec) < Elem_Component_Total_Esize)
             or else
               (not All_Elem_Components
                 and then not All_Storage_Unit_Components
                 and then RM_Size (Rec) < Sized_Component_Total_Round_RM_Size))

           --  And the total RM size cannot be greater than the specified size
           --  since otherwise packing will not get us where we have to be.

           and then Sized_Component_Total_RM_Size <= RM_Size (Rec)

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

         --  For tagged types, warn on an implicitly inherited aspect/pragma
         --  First_Controlling_Parameter that is not explicitly set.

         if Is_Tagged_Type (Rec) then
            Warn_If_Implicitly_Inherited_Aspects (Rec);
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

      -------------------------
      -- Inherit_Freeze_Node --
      -------------------------

      procedure Inherit_Freeze_Node
        (Fnod : Node_Id;
         Typ  : Entity_Id)
      is
         Typ_Fnod : constant Node_Id := Freeze_Node (Typ);

      begin
         Set_Freeze_Node (Typ, Fnod);
         Set_Entity (Fnod, Typ);

         --  The input type had an existing node. Propagate relevant attributes
         --  from the old freeze node to the inherited freeze node.

         --  ??? if both freeze nodes have attributes, would they differ?

         if Present (Typ_Fnod) then

            --  Attribute Access_Types_To_Process

            if Present (Access_Types_To_Process (Typ_Fnod))
              and then No (Access_Types_To_Process (Fnod))
            then
               Set_Access_Types_To_Process (Fnod,
                 Access_Types_To_Process (Typ_Fnod));
            end if;

            --  Attribute Actions

            if Present (Actions (Typ_Fnod)) and then No (Actions (Fnod)) then
               Set_Actions (Fnod, Actions (Typ_Fnod));
            end if;

            --  Attribute First_Subtype_Link

            if Present (First_Subtype_Link (Typ_Fnod))
              and then No (First_Subtype_Link (Fnod))
            then
               Set_First_Subtype_Link (Fnod, First_Subtype_Link (Typ_Fnod));
            end if;

            --  Attribute TSS_Elist

            if Present (TSS_Elist (Typ_Fnod))
              and then No (TSS_Elist (Fnod))
            then
               Set_TSS_Elist (Fnod, TSS_Elist (Typ_Fnod));
            end if;
         end if;
      end Inherit_Freeze_Node;

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

            Prag := Copy_Import_Pragma;

            --  Fix up spec so it is no longer imported and has convention Ada

            Set_Has_Completion (E, False);
            Set_Import_Pragma  (E, Empty);
            Set_Interface_Name (E, Empty);
            Set_Is_Imported    (E, False);
            Set_Convention     (E, Convention_Ada);

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

            --  An imported function whose result type is anonymous access
            --  creates a new anonymous access type when it is relocated into
            --  the declarations of the body generated below. As a result, the
            --  accessibility level of these two anonymous access types may not
            --  be compatible even though they are essentially the same type.
            --  Use an unchecked type conversion to reconcile this case. Note
            --  that the conversion is safe because in the named access type
            --  case, both the body and imported function utilize the same
            --  type.

            if Ekind (E) in E_Function | E_Generic_Function then
               Stmt :=
                 Make_Simple_Return_Statement (Loc,
                   Expression =>
                     Unchecked_Convert_To (Etype (E),
                       Make_Function_Call (Loc,
                         Name                   => Make_Identifier (Loc, CE),
                         Parameter_Associations => Parms)));

            else
               Stmt :=
                 Make_Procedure_Call_Statement (Loc,
                   Name                   => Make_Identifier (Loc, CE),
                   Parameter_Associations => Parms);
            end if;

            --  Now build the body

            Bod :=
              Make_Subprogram_Body (Loc,
                Specification              => Copy_Subprogram_Spec (Spec),
                Declarations               => New_List (
                  Make_Subprogram_Declaration (Loc,
                    Specification => Copy_Subprogram_Spec (Spec)),
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

   --  Start of processing for Freeze_Entity

   begin
      --  The entity being frozen may be subject to pragma Ghost. Set the mode
      --  now to ensure that any nodes generated during freezing are properly
      --  flagged as Ghost.

      Set_Ghost_Mode (E);

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

         if Is_Itype (E)
           and then not Is_Base_Type (E)
           and then not Is_Frozen (Etype (E))
         then
            --  If a frozen subtype of an unfrozen type seems impossible
            --  then see Analyze_Protected_Definition.Undelay_Itypes.

            Result := Freeze_Entity
                        (Etype (E), N, Do_Freeze_Profile => Do_Freeze_Profile);
         else
            Result := No_List;
         end if;

         goto Leave;

      --  Do not freeze if we are preanalyzing without freezing

      elsif Inside_Preanalysis_Without_Freezing > 0 then
         Result := No_List;
         goto Leave;

      elsif Ekind (E) = E_Generic_Package then
         Result := Freeze_Generic_Entities (E);
         goto Leave;

      --  It is improper to freeze an external entity within a generic because
      --  its freeze node will appear in a non-valid context. The entity will
      --  be frozen in the proper scope after the current generic is analyzed.
      --  However, aspects must be analyzed because they may be queried later
      --  within the generic itself, and the corresponding pragma or attribute
      --  definition has not been analyzed yet. After this, indicate that the
      --  entity has no further delayed aspects, to prevent a later aspect
      --  analysis out of the scope of the generic.

      elsif Inside_A_Generic and then External_Ref_In_Generic (Test_E) then
         if Has_Delayed_Aspects (E) then
            Analyze_Aspects_At_Freeze_Point (E);
            Set_Has_Delayed_Aspects (E, False);
         end if;

         Result := No_List;
         goto Leave;

      --  AI05-0213: A formal incomplete type does not freeze the actual. In
      --  the instance, the same applies to the subtype renaming the actual.

      elsif Is_Private_Type (E)
        and then Is_Generic_Actual_Type (E)
        and then No (Full_View (Base_Type (E)))
        and then Ada_Version >= Ada_2012
      then
         Result := No_List;
         goto Leave;

      --  Formal subprograms are never frozen

      elsif Is_Formal_Subprogram (E) then
         Result := No_List;
         goto Leave;

      --  Generic types are never frozen as they lack delayed semantic checks

      elsif Is_Generic_Type (E) then
         Result := No_List;
         goto Leave;

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
         --  Here we deal with the special case of the expansion of
         --  postconditions. Previously this was handled by the loop below,
         --  since these postcondition checks got isolated to a separate,
         --  internally generated, subprogram. Now, however, the postcondition
         --  checks get contained within their corresponding subprogram
         --  directly.

         if not Comes_From_Source (N)
           and then Nkind (N) = N_Pragma
           and then From_Aspect_Specification (N)
           and then Is_Valid_Assertion_Kind (Original_Aspect_Pragma_Name (N))

           --  Now, verify the placement of the pragma is within an expanded
           --  subprogram which contains postcondition expansion - detected
           --  through the presence of the "Wrapped_Statements" field.

           and then Present (Enclosing_Subprogram (Current_Scope))
           and then Present (Wrapped_Statements
                              (Enclosing_Subprogram (Current_Scope)))
         then
            goto Leave;
         end if;

         --  Otherwise, loop through scopes checking if an enclosing scope
         --  comes from source or is a generic. Note that, for the purpose
         --  of this test, we need to consider that the internally generated
         --  subprogram described above comes from source too if the original
         --  subprogram itself does.

         declare
            S : Entity_Id;

         begin
            S := Current_Scope;
            while Present (S) loop
               if Is_Overloadable (S) then
                  if Comes_From_Source (S)
                    or else (Chars (S) = Name_uWrapped_Statements
                              and then Comes_From_Source (Scope (S)))
                    or else Is_Generic_Instance (S)
                    or else Is_Child_Unit (S)
                  then
                     exit;
                  else
                     Result := No_List;
                     goto Leave;
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
               Result := No_List;
               goto Leave;
            end if;
         end;
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
      --  In addition, a derived type may have inherited aspects that were
      --  delayed in the parent, so these must also be captured now.

      --  For a record type, we deal with the delayed aspect specifications on
      --  components first, which is consistent with the non-delayed case and
      --  makes it possible to have a single processing to detect conflicts.

      if Is_Record_Type (E) then
         declare
            Comp : Entity_Id;

            Rec_Pushed : Boolean := False;
            --  Set True if the record type E has been pushed on the scope
            --  stack. Needed for the analysis of delayed aspects specified
            --  to the components of Rec.

         begin
            Comp := First_Component (E);
            while Present (Comp) loop
               if Has_Delayed_Aspects (Comp) then
                  if not Rec_Pushed then
                     Push_Scope (E);
                     Rec_Pushed := True;

                     --  The visibility to the discriminants must be restored
                     --  in order to properly analyze the aspects.

                     if Has_Discriminants (E) then
                        Install_Discriminants (E);
                     end if;
                  end if;

                  Analyze_Aspects_At_Freeze_Point (Comp);
               end if;

               Next_Component (Comp);
            end loop;

            --  Pop the scope if Rec scope has been pushed on the scope stack
            --  during the delayed aspect analysis process.

            if Rec_Pushed then
               if Has_Discriminants (E) then
                  Uninstall_Discriminants (E);
               end if;

               Pop_Scope;
            end if;
         end;
      end if;

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
         end if;

         --  Subprogram case

         if Is_Subprogram (E) then

            --  Check for needing to wrap imported subprogram

            if not Inside_A_Generic then
               Wrap_Imported_Subprogram (E);
            end if;

            --  Freeze all parameter types and the return type (RM 13.14(14)).
            --  However skip this for internal subprograms. This is also where
            --  any extra formal parameters are created since we now know
            --  whether the subprogram will use a foreign convention.

            --  In Ada 2012, freezing a subprogram does not always freeze the
            --  corresponding profile (see AI05-019). An attribute reference
            --  is not a freezing point of the profile. Similarly, we do not
            --  freeze the profile of primitives of a library-level tagged type
            --  when we are building its dispatch table. Flag Do_Freeze_Profile
            --  indicates whether the profile should be frozen now.

            --  This processing doesn't apply to internal entities (see below)

            if not Is_Internal (E) and then Do_Freeze_Profile then
               if not Freeze_Profile (E) then
                  goto Leave;
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
                     if Pragma_Name_Unmapped (Prag) in Name_Post
                                                     | Name_Postcondition
                                                     | Name_Refined_Post
                     then
                        Exp :=
                          Expression
                            (First (Pragma_Argument_Associations (Prag)));

                        if Nkind (Exp) /= N_Identifier
                          or else Chars (Exp) /= Name_False
                        then
                           Error_Msg_NE
                             ("useless postcondition, & is marked "
                              & "No_Return?.t?", Exp, E);
                        end if;
                     end if;

                     Prag := Next_Pragma (Prag);
                  end loop;
               end;
            end if;

         --  Here for other than a subprogram or type

         else
            --  If entity has a type declared in the current scope, and it is
            --  not a generic unit, then freeze it first.

            if Present (Etype (E))
              and then Ekind (E) /= E_Generic_Function
              and then Within_Scope (Etype (E), Current_Scope)
            then
               Freeze_And_Append (Etype (E), N, Result);
            end if;

            --  Special processing for objects created by object declaration;
            --  we protect the call to Declaration_Node against entities of
            --  expressions replaced by the frontend with an N_Raise_CE node.

            if Ekind (E) in E_Constant | E_Variable
              and then Nkind (Declaration_Node (E)) = N_Object_Declaration
            then
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
                    ("standalone atomic constant must be " &
                     "imported (RM C.6(13))", E);

               elsif Has_Rep_Pragma (E, Name_Volatile)
                       or else
                     Has_Rep_Pragma (E, Name_Volatile_Components)
               then
                  Error_Msg_N
                    ("standalone volatile constant must be " &
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

            if Ekind (E) in E_Variable | E_Constant | E_Loop_Parameter
              or else Is_Formal (E)
            then
               Layout_Object (E);
            end if;

            --  For an object that does not have delayed freezing, and whose
            --  initialization actions have been captured in a compound
            --  statement, move them back now directly within the enclosing
            --  statement sequence.

            if Ekind (E) in E_Constant | E_Variable
              and then not Has_Delayed_Freeze (E)
            then
               Explode_Initialization_Compound_Statement (E);
            end if;

            --  Do not generate a freeze node for a generic unit

            if Is_Generic_Unit (E) then
               Result := No_List;
               goto Leave;
            end if;
         end if;

      --  Case of a type or subtype being frozen

      else
         --  Verify several SPARK legality rules related to Ghost types now
         --  that the type is frozen.

         Check_Ghost_Type (E);

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

         --  Is_Generic_Unit (Scope (E)) is dubious here, do we want instead
         --  In_Generic_Scope (E)???

         if Present (Scope (E))
           and then Is_Generic_Unit (Scope (E))
           and then
             (not Has_Predicates (E)
               and then not Has_Delayed_Freeze (E))
         then
            Check_Compile_Time_Size (E);
            Result := No_List;
            goto Leave;
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
                 ("Type_Invariant''Class cannot be specified for &", Prag, E);
               Error_Msg_N
                 ("\can only be specified for a tagged type", Prag);
            end if;
         end;

         --  If the entity is a declaration of an access-to-subprogram type
         --  with pre/postcondition contracts, build the wrapper (if it hasn't
         --  already been done during aspect processing), and propagate the
         --  pre/postcondition pragmas to the wrapper.

         if Ada_Version >= Ada_2022
           and then Expander_Active
           and then Ekind (E) = E_Access_Subprogram_Type
           and then Nkind (Parent (E)) = N_Full_Type_Declaration
           and then Present (Contract (Designated_Type (E)))
           and then not Is_Derived_Type (E)
         then
            Build_Access_Subprogram_Wrapper (Parent (E));
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
                  Rsiz : constant Uint :=
                    (if Known_RM_Size (Ctyp) then RM_Size (Ctyp) else Uint_0);
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
                    and then Rsiz <= System_Max_Integer_Size
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

                        if Dim > Uint_0 then
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

                        --  Otherwise give an error message, except that if the
                        --  specified Size is zero, there is no need for pragma
                        --  Pack. Note that size zero is not considered
                        --  Addressable.

                        elsif RM_Size (E) /= Uint_0 then
                           Error_Msg_NE
                             ("size given for& too small", SZ, E);
                           Error_Msg_N -- CODEFIX
                             ("\use explicit pragma Pack or use pragma "
                              & "Implicit_Packing", SZ);
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

            if May_Inherit_Delayed_Rep_Aspects (E) then
               Inherit_Delayed_Rep_Aspects (E);
            end if;

            Inherit_Aspects_At_Freeze_Point (E);

         --  For a derived type, freeze its parent type first (RM 13.14(15))

         elsif Is_Derived_Type (E) then
            Freeze_And_Append (Etype (E), N, Result);

            --  A derived type inherits each type-related representation aspect
            --  of its parent type that was directly specified before the
            --  declaration of the derived type (RM 13.1(15)).

            if May_Inherit_Delayed_Rep_Aspects (E) then
               Inherit_Delayed_Rep_Aspects (E);
            end if;

            Inherit_Aspects_At_Freeze_Point (E);
         end if;

         --  Case of array type

         if Is_Array_Type (E) then
            Freeze_Array_Type (E);
         end if;

         --  Check for incompatible size and alignment for array/record type

         if Warn_On_Size_Alignment
           and then (Is_Array_Type (E) or else Is_Record_Type (E))
           and then Has_Size_Clause (E)
           and then Has_Alignment_Clause (E)

           --  If explicit Object_Size clause given assume that the programmer
           --  knows what he is doing, and expects the compiler behavior.

           and then not Has_Object_Size_Clause (E)

           --  It does not really make sense to warn for the minimum alignment
           --  since the programmer could not get rid of the warning.

           and then Alignment (E) > 1

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
                       ("?.z?size is not a multiple of alignment for &",
                        Loc, E);
                     Error_Msg_Sloc := Sloc (AC);
                     Error_Msg_Uint_1 := Alignment (E);
                     Error_Msg_N ("\?.z?alignment of ^ specified #", Loc);

                  else
                     Loc := AC;
                     Error_Msg_NE
                       ("?.z?size is not a multiple of alignment for &",
                        Loc, E);
                     Error_Msg_Sloc := Sloc (SC);
                     Error_Msg_Uint_1 := RM_Size (E);
                     Error_Msg_N ("\?.z?size of ^ specified #", Loc);
                  end if;

                  Error_Msg_Uint_1 := ((RM_Size (E) / Abits) + 1) * Abits;
                  Error_Msg_N ("\?.z?Object_Size will be increased to ^", Loc);
               end if;
            end;
         end if;

         --  For a class-wide type, the corresponding specific type is
         --  frozen as well (RM 13.14(15))

         if Is_Class_Wide_Type (E) then
            Freeze_And_Append (Root_Type (E), N, Result);

            --  If the base type of the class-wide type is still incomplete,
            --  the class-wide remains unfrozen as well. This is legal when
            --  E is the formal of a primitive operation of some other type
            --  which is being frozen.

            if not Is_Frozen (Root_Type (E)) then
               Set_Is_Frozen (E, False);
               goto Leave;
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

         elsif Ekind (E) in E_Record_Type | E_Record_Subtype then
            if not In_Generic_Scope (E) then
               Freeze_Record_Type (E);
            end if;

            --  Report a warning if a discriminated record base type has a
            --  convention with language C or C++ applied to it. This check is
            --  done even within generic scopes (but not in instantiations),
            --  which is why we don't do it as part of Freeze_Record_Type.

            Check_Suspicious_Convention (E);

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

               elsif Ekind (Comp) /= E_Function then

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
              and then No (Full_View (E))
            then
               Set_Is_Frozen (E, False);
               goto Leave;

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
                           Inherit_Freeze_Node
                             (Fnod => F_Node, Typ => Full_View (E));
                        else
                           Set_Has_Delayed_Freeze (Full_View (E), False);
                           Set_Freeze_Node (Full_View (E), Empty);
                        end if;
                     end if;

                     if Has_Delayed_Freeze (E) then
                        F_Node := Freeze_Node (Full_View (E));

                        if Present (F_Node) then
                           Inherit_Freeze_Node (Fnod => F_Node, Typ => E);
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

               --  AI95-117 requires that the convention of a partial view be
               --  the same as the convention of the full view. Note that this
               --  is a recognized breach of privacy, but it's essential for
               --  logical consistency of representation, and the lack of a
               --  rule in RM95 was an oversight.

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
                  Copy_RM_Size (To => E, From => Full_View (E));
               end if;

               goto Leave;

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
                     Inherit_Freeze_Node
                       (Fnod => F_Node,
                        Typ  => E);
                  else
                     Set_Has_Delayed_Freeze (E, False);
                     Set_Freeze_Node (E, Empty);
                  end if;
               end if;

               Check_Debug_Info_Needed (E);

               goto Leave;

            --  Case of no full view present. If entity is subtype or derived,
            --  it is safe to freeze, correctness depends on the frozen status
            --  of parent. Otherwise it is either premature usage, or a Taft
            --  amendment type, so diagnosis is at the point of use and the
            --  type might be frozen later.

            elsif E /= Base_Type (E) then
               declare
                  Btyp : constant Entity_Id := Base_Type (E);

               begin
                  --  However, if the base type is itself private and has no
                  --  (underlying) full view either, wait until the full type
                  --  declaration is seen and all the full views are created.

                  if Is_Private_Type (Btyp)
                    and then No (Full_View (Btyp))
                    and then No (Underlying_Full_View (Btyp))
                    and then Has_Delayed_Freeze (Btyp)
                    and then No (Freeze_Node (Btyp))
                  then
                     Set_Is_Frozen (E, False);
                     Result := No_List;
                     goto Leave;
                  end if;
               end;

            elsif Is_Derived_Type (E) then
               null;

            else
               Set_Is_Frozen (E, False);
               Result := No_List;
               goto Leave;
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
            goto Leave;
         end if;

         --  Some special processing for non-generic types to complete
         --  representation details not known till the freeze point.

         if Is_Fixed_Point_Type (E) then
            Freeze_Fixed_Point_Type (E);

         elsif Is_Enumeration_Type (E) then
            Freeze_Enumeration_Type (E);

         elsif Is_Integer_Type (E) then
            Adjust_Esize_For_Alignment (E);

            if Is_Modular_Integer_Type (E) then
               --  Standard_Address has been built with the assumption that its
               --  modulus was System_Address_Size, but this is not a universal
               --  property and may need to be corrected.

               if Is_RTE (E, RE_Address) then
                  Set_Modulus (Standard_Address, Modulus (E));
                  Set_Intval
                    (High_Bound (Scalar_Range (Standard_Address)),
                     Modulus (E) - 1);

               elsif Warn_On_Suspicious_Modulus_Value then
                  Check_Suspicious_Modulus (E);
               end if;
            end if;

         --  The pool applies to named and anonymous access types, but not
         --  to subprogram and to internal types generated for 'Access
         --  references.

         elsif Is_Access_Object_Type (E)
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

               --  Case of pragma Default_Storage_Pool (Standard)

               elsif Entity (Default_Pool) = Standard_Standard then
                  Set_Associated_Storage_Pool (E, RTE (RE_Global_Pool_Object));

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
                    ("\would be legal if Storage_Size of 0 given", E);

               elsif No_Pool_Assigned (E) then
                  Error_Msg_N
                    ("\would be legal in Ada 2005", E);

               else
                  Error_Msg_N
                    ("\would be legal in Ada 2005 if "
                     & "Storage_Size of 0 given", E);
               end if;
            end if;
         end if;

         --  Case of composite types

         if Is_Composite_Type (E) then

            --  AI95-117 requires that all new primitives of a tagged type
            --  must inherit the convention of the full view of the
            --  type. Inherited and overriding operations are defined to
            --  inherit the convention of their parent or overridden
            --  subprogram (also specified in AI-117), which will have
            --  occurred earlier (in Derive_Subprogram and
            --  New_Overloaded_Entity). Here we set the convention of
            --  primitives that are still convention Ada, which will ensure
            --  that any new primitives inherit the type's convention. We
            --  don't do this for primitives that are internal to avoid
            --  potential problems in the case of nested subprograms and
            --  convention C. In addition, class-wide types can have a
            --  foreign convention inherited from their specific type, but
            --  are excluded from this since they don't have any associated
            --  primitives.

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
                     if Convention (Node (Prim)) = Convention_Ada
                       and then Comes_From_Source (Node (Prim))
                     then
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
                  if (not Is_Record_Type (E)
                       or else not Is_Inherently_Limited_Type (E))
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

                     elsif No (Pool_Op_Formal) then
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
                       (Op_Name in Name_Allocate
                                 | Name_Deallocate
                                 | Name_Storage_Size);

                     Error_Msg_Name_1 := Op_Name;

                     --  For each homonym declared immediately in the scope
                     --  of the simple storage pool type, determine whether
                     --  the homonym is an operation of the pool type, and,
                     --  if so, check that its profile is as expected for
                     --  a simple pool operation of that name.

                     Op := Get_Name_Entity_Id (Op_Name);
                     while Present (Op) loop
                        if Ekind (Op) in E_Function | E_Procedure
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

                           if No (Formal) and then Is_OK then
                              Found_Op := Op;
                           end if;
                        end if;

                        Op := Homonym (Op);
                     end loop;

                     --  There must be a valid Allocate operation for the type,
                     --  so issue an error if none was found.

                     if Op_Name = Name_Allocate
                       and then No (Found_Op)
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

         --  Now that all types from which E may depend are frozen, see if
         --  strict alignment is required, a component clause on a record
         --  is correct, the size is known at compile time and if it must
         --  be unsigned, in that order.

         if Base_Type (E) = E then
            Check_Strict_Alignment (E);
         end if;

         if Ekind (E) in E_Record_Type | E_Record_Subtype then
            declare
               RC : constant Node_Id := Get_Record_Representation_Clause (E);
            begin
               if Present (RC) then
                  Check_Record_Representation_Clause (RC);
               end if;
            end;
         end if;

         Check_Compile_Time_Size (E);

         Check_Unsigned_Type (E);

         --  Do not allow a size clause for a type which does not have a size
         --  that is known at compile time

         if (Has_Size_Clause (E) or else Has_Object_Size_Clause (E))
           and then not Size_Known_At_Compile_Time (E)
           and then not Is_Mutably_Tagged_Type (E)
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
         --  that will be recreated in an instance, except for static subtypes
         --  because they may be referenced in the static expressions of the
         --  generic unit, which need to be evaluated during its processing.

         if Inside_A_Generic and then not Is_Static_Subtype (E) then
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
         --  this is where we analyze the expression (after the type is frozen,
         --  since in the case of Default_Value, we are analyzing with the
         --  type itself, and we treat Default_Component_Value similarly for
         --  the sake of uniformity).

         --  But for an inherited Default_Value aspect specification, the type
         --  of the aspect remains the parent type. RM 3.3.1(11.1), a dynamic
         --  semantics rule, says "The implicit initial value for a scalar
         --  subtype that has the Default_Value aspect specified is the value
         --  of that aspect converted to the nominal subtype". For an inherited
         --  Default_Value aspect specification, no conversion is evaluated at
         --  the point of the derived type declaration.

         if Is_First_Subtype (E)
           and then Has_Default_Aspect (E)
           and then
             (not Is_Scalar_Type (E)
                or else
              not Is_Derived_Type (E)
                or else
              Default_Aspect_Value (E)
                /= Default_Aspect_Value (Etype (Base_Type (E))))
         then
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

         --  Verify at this point that No_Controlled_Parts and No_Task_Parts,
         --  when specified on the current type or one of its ancestors, has
         --  not been overridden and that no violation of the aspect has
         --  occurred.

         --  It is important that we perform the checks here after the type has
         --  been processed because if said type depended on a private type it
         --  will not have been marked controlled or having tasks.

         Check_No_Parts_Violations (E, Aspect_No_Controlled_Parts);
         Check_No_Parts_Violations (E, Aspect_No_Task_Parts);

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
                  Typ := Etype (Comp);

                  if Is_Access_Type (Typ)
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

         --  If we just froze a tagged non-class-wide record, then freeze the
         --  corresponding class-wide type. This must be done after the tagged
         --  type itself is frozen, because the class-wide type refers to the
         --  tagged type which generates the class.

         --  For a tagged type, freeze explicitly those primitive operations
         --  that are expression functions, which otherwise have no clear
         --  freeze point: these have to be frozen before the dispatch table
         --  for the type is built, and before any explicit call to the
         --  primitive, which would otherwise be the freeze point for it.

         if Is_Tagged_Type (E)
           and then not Is_Class_Wide_Type (E)
           and then Present (Class_Wide_Type (E))
         then
            Freeze_And_Append (Class_Wide_Type (E), N, Result);

            declare
               Ops  : constant Elist_Id := Primitive_Operations (E);

               Elmt : Elmt_Id;
               Subp : Entity_Id;

            begin
               if Ops /= No_Elist  then
                  Elmt := First_Elmt (Ops);
                  while Present (Elmt) loop
                     Subp := Node (Elmt);
                     if Is_Expression_Function (Subp) then
                        Freeze_And_Append (Subp, N, Result);
                     end if;

                     Next_Elmt (Elmt);
                  end loop;
               end if;
            end;
         end if;
      end if;

      Check_Debug_Info_Needed (E);

      --  If subprogram has address clause then reset Is_Public flag, since we
      --  do not want the backend to generate external references.

      if Is_Subprogram (E)
        and then Present (Address_Clause (E))
        and then not Is_Library_Level_Entity (E)
      then
         Set_Is_Public (E, False);
      end if;

      --  The Ghost mode of the enclosing context is ignored, while the
      --  entity being frozen is living. Insert the freezing action prior
      --  to the start of the enclosing ignored Ghost region. As a result
      --  the freezeing action will be preserved when the ignored Ghost
      --  context is eliminated. The insertion must take place even when
      --  the context is a spec expression, otherwise "Handling of Default
      --  and Per-Object Expressions" will suppress the insertion, and the
      --  freeze node will be dropped on the floor.

      if Saved_GM = Ignore
        and then Ghost_Mode /= Ignore
        and then Present (Ignored_Ghost_Region)
      then
         Insert_Actions
           (Assoc_Node   => Ignored_Ghost_Region,
            Ins_Actions  => Result,
            Spec_Expr_OK => True);

         Result := No_List;
      end if;

   <<Leave>>
      Restore_Ghost_Region (Saved_GM, Saved_IGR);

      return Result;
   end Freeze_Entity;

   -----------------------------
   -- Freeze_Enumeration_Type --
   -----------------------------

   procedure Freeze_Enumeration_Type (Typ : Entity_Id) is
   begin
      --  By default, if no size clause is present, an enumeration type with
      --  Convention C is assumed to interface to a C enum and has integer
      --  size, except for a boolean type because it is assumed to interface
      --  to _Bool introduced in C99. This applies to types. For subtypes,
      --  verify that its base type has no size clause either. Treat other
      --  foreign conventions in the same way, and also make sure alignment
      --  is set right.

      if Has_Foreign_Convention (Typ)
        and then not Is_Boolean_Type (Typ)
        and then not Has_Size_Clause (Typ)
        and then not Has_Size_Clause (Base_Type (Typ))
        and then Esize (Typ) < Standard_Integer_Size

        --  Don't do this if Short_Enums on target

        and then not Target_Short_Enums
      then
         Set_Esize (Typ, UI_From_Int (Standard_Integer_Size));
         Set_Alignment (Typ, Alignment (Standard_Integer));

      --  Normal Ada case or size clause present or not Long_C_Enums on target

      else
         --  If the enumeration type interfaces to C, and it has a size clause
         --  that is smaller than the size of int, it warrants a warning. The
         --  user may intend the C type to be a boolean or a char, so this is
         --  not by itself an error that the Ada compiler can detect, but it
         --  is worth a heads-up. For Boolean and Character types we
         --  assume that the programmer has the proper C type in mind.
         --  For explicit sizes larger than int, assume the user knows what
         --  he is doing and that the code is intentional.

         if Convention (Typ) = Convention_C
           and then Has_Size_Clause (Typ)
           and then Esize (Typ) < Standard_Integer_Size
           and then not Is_Boolean_Type (Typ)
           and then not Is_Character_Type (Typ)

           --  Don't do this if Short_Enums on target

           and then not Target_Short_Enums
         then
            Error_Msg_N
              ("??the size of enums in C is implementation-defined",
               Size_Clause (Typ));
            Error_Msg_N
              ("\??check that the C counterpart has size of " &
               UI_Image (Esize (Typ)),
               Size_Clause (Typ));
         end if;

         Adjust_Esize_For_Alignment (Typ);
      end if;

      --  Reject a very large size on a type with a non-standard representation
      --  because Expand_Freeze_Enumeration_Type cannot deal with it.

      if Has_Non_Standard_Rep (Typ)
        and then Known_Esize (Typ)
        and then Esize (Typ) > System_Max_Integer_Size
      then
         Error_Msg_N
           ("enumeration type with representation clause too large", Typ);
         Error_Msg_Uint_1 := UI_From_Int (System_Max_Integer_Size);
         Error_Msg_N
           ("\the size of such a type cannot exceed ^ bits", Typ);
      end if;
   end Freeze_Enumeration_Type;

   -----------------------
   -- Freeze_Expression --
   -----------------------

   procedure Freeze_Expression (N : Node_Id) is

      function Declared_In_Expanded_Body
        (N   : Node_Id;
         Typ : Entity_Id;
         Nam : Entity_Id) return Boolean;
      --  Given the N_Handled_Sequence_Of_Statements node of an expander
      --  generated subprogram body, determines if the frozen entity is
      --  declared inside this body. This is recognized locating the
      --  enclosing subprogram of the entity Name or its Type and
      --  checking if it is this subprogram body.

      function Find_Aggregate_Component_Desig_Type return Entity_Id;
      --  If the expression is an array aggregate, the type of the component
      --  expressions is also frozen. If the component type is an access type
      --  and the expressions include allocators, the designed type is frozen
      --  as well.

      function In_Expanded_Body (N : Node_Id) return Boolean;
      --  Given an N_Handled_Sequence_Of_Statements node, determines whether it
      --  is the statement sequence of an expander-generated subprogram body or
      --  of a renaming_as_body. If so, this is not a freezing context and the
      --  entity will be frozen at a later point.

      function Has_Decl_In_List
        (E : Entity_Id;
         N : Node_Id;
         L : List_Id) return Boolean;
      --  Determines whether an entity E referenced in node N is declared in
      --  the list L.

      -------------------------------
      -- Declared_In_Expanded_Body --
      -------------------------------

      function Declared_In_Expanded_Body
        (N   : Node_Id;
         Typ : Entity_Id;
         Nam : Entity_Id) return Boolean
      is
         pragma Assert (In_Expanded_Body (N));

         Subp_Body : constant Node_Id := Parent (N);
         Subp_Id   : Entity_Id;
         Scop      : Entity_Id;

      begin
         if Acts_As_Spec (Subp_Body) then
            Subp_Id := Unique_Defining_Entity (Specification (Subp_Body));
         else
            Subp_Id := Corresponding_Spec (Subp_Body);
         end if;

         if Present (Typ) then
            Scop := Scope (Typ);
         elsif Present (Nam) then
            Scop := Scope (Nam);
         else
            Scop := Standard_Standard;
         end if;

         while Scop /= Standard_Standard
           and then not Is_Subprogram (Scop)
         loop
            Scop := Scope (Scop);
         end loop;

         return Scop = Subp_Id;
      end Declared_In_Expanded_Body;

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
         P  : constant Node_Id := Parent (N);
         Id : Entity_Id;

      begin
         if Nkind (P) /= N_Subprogram_Body then
            return False;

         --  Treat the generated body of an expression function like other
         --  bodies generated during expansion (e.g. stream subprograms) so
         --  that those bodies are not treated as freezing points.

         elsif Was_Expression_Function (P) then
            pragma Assert (not Comes_From_Source (P));
            return True;

         --  This is the body of a generated predicate function

         elsif Present (Corresponding_Spec (P))
           and then Is_Predicate_Function (Corresponding_Spec (P))
         then
            return True;

         --  This is the body of a helper/wrapper built for CW preconditions

         elsif Present (Corresponding_Spec (P))
           and then
             Present (Class_Preconditions_Subprogram (Corresponding_Spec (P)))
         then
            return True;

         else
            Id := Defining_Unit_Name (Specification (P));

            --  This is the body of a Type-Specific Support routine or the one
            --  generated for a renaming_as_body.

            if Nkind (Id) = N_Defining_Identifier
              and then (Is_Init_Proc (Id)
                         or else Is_TSS (Id, TSS_Stream_Input)
                         or else Is_TSS (Id, TSS_Stream_Output)
                         or else Is_TSS (Id, TSS_Stream_Read)
                         or else Is_TSS (Id, TSS_Stream_Write)
                         or else Is_TSS (Id, TSS_Put_Image)
                         or else Nkind (Original_Node (P)) =
                                             N_Subprogram_Renaming_Declaration)
            then
               return True;
            else
               return False;
            end if;
         end if;
      end In_Expanded_Body;

      ----------------------
      -- Has_Decl_In_List --
      ----------------------

      function Has_Decl_In_List
        (E : Entity_Id;
         N : Node_Id;
         L : List_Id) return Boolean
      is
         Decl_Node : Node_Id;

      begin
         --  If E is an itype, pretend that it is declared in N except for a
         --  class-wide subtype with an equivalent type, because this latter
         --  type comes with a bona-fide declaration node.

         if Is_Itype (E) then
            if Ekind (E) = E_Class_Wide_Subtype
              and then Present (Equivalent_Type (E))
            then
               Decl_Node := Declaration_Node (Equivalent_Type (E));
            else
               Decl_Node := N;
            end if;

         else
            Decl_Node := Declaration_Node (E);
         end if;

         return Is_List_Member (Decl_Node)
           and then List_Containing (Decl_Node) = L;
      end Has_Decl_In_List;

      --  Local variables

      In_Spec_Exp : constant Boolean := In_Spec_Expression;

      Desig_Typ : Entity_Id;
      Nam       : Entity_Id;
      P         : Node_Id;
      Parent_P  : Node_Id;
      Typ       : Entity_Id;

      Allocator_Typ : Entity_Id := Empty;

      Freeze_Outside_Subp  : Entity_Id := Empty;
      --  This entity is set if we are inside a subprogram body and the frozen
      --  entity is defined in the enclosing scope of this subprogram. In such
      --  case we must skip the subprogram body when climbing the parents chain
      --  to locate the correct placement for the freezing node.

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

      if Nkind (N) in N_Has_Etype and then Present (Etype (N)) then
         if not Is_Frozen (Etype (N)) then
            Typ := Etype (N);

         --  Base type may be an derived numeric type that is frozen at the
         --  point of declaration, but first_subtype is still unfrozen.

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
        and then Present (Entity (N))
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

            if Nkind (Expression (N)) = N_Qualified_Expression then
               Allocator_Typ := Entity (Subtype_Mark (Expression (N)));
            end if;

         when N_Aggregate =>
            if Is_Array_Type (Etype (N))
              and then Is_Access_Type (Component_Type (Etype (N)))
            then
               --  Check whether aggregate includes allocators

               Desig_Typ := Find_Aggregate_Component_Desig_Type;
            end if;

         when N_Indexed_Component
            | N_Selected_Component
            | N_Slice
         =>
            if Is_Access_Type (Etype (Prefix (N))) then
               Desig_Typ := Designated_Type (Etype (Prefix (N)));
            end if;

         when N_Identifier =>
            if Present (Nam)
              and then Ekind (Nam) = E_Function
              and then Nkind (Parent (N)) = N_Function_Call
              and then not Has_Foreign_Convention (Nam)
            then
               Create_Extra_Formals (Nam);
            end if;

         when others =>
            null;
      end case;

      if Desig_Typ /= Empty
        and then (Is_Frozen (Desig_Typ)
                   or else not Is_Fully_Defined (Desig_Typ))
      then
         Desig_Typ := Empty;
      end if;

      --  All done if nothing needs freezing

      if No (Typ)
        and then No (Nam)
        and then No (Desig_Typ)
        and then No (Allocator_Typ)
      then
         return;
      end if;

      --  Check if we are inside a subprogram body and the frozen entity is
      --  defined in the enclosing scope of this subprogram. In such case we
      --  must skip the subprogram when climbing the parents chain to locate
      --  the correct placement for the freezing node.

      --  This is not needed for default expressions and other spec expressions
      --  in generic units since the Move_Freeze_Nodes mechanism (sem_ch12.adb)
      --  takes care of placing them at the proper place, after the generic
      --  unit.

      if Present (Nam)
        and then Scope (Nam) /= Current_Scope
        and then not (In_Spec_Exp and then Inside_A_Generic)
      then
         declare
            S : Entity_Id := Current_Scope;

         begin
            while Present (S)
              and then In_Same_Source_Unit (Nam, S)
            loop
               if Scope (S) = Scope (Nam) then
                  if Is_Subprogram (S) and then Has_Completion (S) then
                     Freeze_Outside_Subp := S;
                  end if;

                  exit;
               end if;

               S := Scope (S);
            end loop;
         end;
      end if;

      --  Examine the enclosing context by climbing the parent chain

      --  If we identified that we must freeze the entity outside of a given
      --  subprogram then we just climb up to that subprogram checking if some
      --  enclosing node is marked as Must_Not_Freeze (since in such case we
      --  must not freeze yet this entity).

      P := N;

      if Present (Freeze_Outside_Subp) then
         loop
            --  Do not freeze the current expression if another expression in
            --  the chain of parents must not be frozen.

            if Nkind (P) in N_Subexpr and then Must_Not_Freeze (P) then
               return;
            end if;

            Parent_P := Parent (P);

            --  If we don't have a parent, then we are not in a well-formed
            --  tree. This is an unusual case, but there are some legitimate
            --  situations in which this occurs, notably when the expressions
            --  in the range of a type declaration are resolved. We simply
            --  ignore the freeze request in this case.

            if No (Parent_P) then
               return;
            end if;

            --  If the parent is a subprogram body, the candidate insertion
            --  point is just ahead of it.

            if Nkind (Parent_P) = N_Subprogram_Body
              and then Unique_Defining_Entity (Parent_P) =
                         Freeze_Outside_Subp
            then
               P := Parent_P;
               exit;
            end if;

            P := Parent_P;
         end loop;

      --  Otherwise the traversal serves two purposes - to detect scenarios
      --  where freezeing is not needed and to find the proper insertion point
      --  for the freeze nodes. Although somewhat similar to Insert_Actions,
      --  this traversal is freezing semantics-sensitive. Inserting freeze
      --  nodes blindly in the tree may result in types being frozen too early.

      else
         loop
            --  Do not freeze the current expression if another expression in
            --  the chain of parents must not be frozen.

            if Nkind (P) in N_Subexpr and then Must_Not_Freeze (P) then
               return;
            end if;

            Parent_P := Parent (P);

            --  If we don't have a parent, then we are not in a well-formed
            --  tree. This is an unusual case, but there are some legitimate
            --  situations in which this occurs, notably when the expressions
            --  in the range of a type declaration are resolved. We simply
            --  ignore the freeze request in this case.

            if No (Parent_P) then
               return;
            end if;

            --  See if we have got to an appropriate point in the tree

            case Nkind (Parent_P) is

               --  A special test for the exception of (RM 13.14(8)) for the
               --  case of per-object expressions (RM 3.8(18)) occurring in
               --  component definition or a discrete subtype definition. Note
               --  that we test for a component declaration which includes both
               --  cases we are interested in, and furthermore the tree does
               --  not have explicit nodes for either of these two constructs.

               when N_Component_Declaration =>

                  --  The case we want to test for here is an identifier that
                  --  is a per-object expression, this is either a discriminant
                  --  that appears in a context other than the component
                  --  declaration or it is a reference to the type of the
                  --  enclosing construct.

                  --  For either of these cases, we skip the freezing

                  if not In_Spec_Expression
                    and then Nkind (N) = N_Identifier
                    and then Present (Entity (N))
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

               --  If we have an enumeration literal that appears as the choice
               --  in the aggregate of an enumeration representation clause,
               --  then freezing does not occur (RM 13.14(10)).

               when N_Enumeration_Representation_Clause =>

                  --  The case we are looking for is an enumeration literal

                  if Nkind (N) in N_Identifier | N_Character_Literal
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
                     --  enumeration literal is temporarily changed to a
                     --  function call for overloading analysis purposes.

                     elsif Nkind (Parent (N)) = N_Function_Call
                        and then Nkind (Parent (Parent (N))) =
                                   N_Component_Association
                        and then First (Choices (Parent (Parent (N)))) =
                                   Parent (N)
                     then
                        return;
                     end if;
                  end if;

               --  Normally if the parent is a handled sequence of statements,
               --  then the current node must be a statement, and that is an
               --  appropriate place to insert a freeze node.

               when N_Handled_Sequence_Of_Statements =>

                  --  An exception occurs when the sequence of statements is
                  --  for an expander generated body that did not do the usual
                  --  freeze all operation. In this case we usually want to
                  --  freeze outside this body, not inside it, unless the
                  --  entity is declared inside this expander generated body.

                  exit when not In_Expanded_Body (Parent_P)
                    or else Declared_In_Expanded_Body (Parent_P, Typ, Nam);

               --  If parent is a body or a spec or a block, then the current
               --  node is a statement or declaration and we can insert the
               --  freeze node before it.

               when N_Block_Statement
                  | N_Entry_Body
                  | N_Package_Body
                  | N_Package_Specification
                  | N_Protected_Body
                  | N_Subprogram_Body
                  | N_Task_Body
               =>
                  exit;

               --  The expander is allowed to define types in any statements
               --  list, so any of the following parent nodes also mark a
               --  freezing point if the actual node is in a list of
               --  statements or declarations.

               when N_Abortable_Part
                  | N_Accept_Alternative
                  | N_Case_Statement_Alternative
                  | N_Compilation_Unit_Aux
                  | N_Conditional_Entry_Call
                  | N_Delay_Alternative
                  | N_Elsif_Part
                  | N_Entry_Call_Alternative
                  | N_Exception_Handler
                  | N_Extended_Return_Statement
                  | N_Freeze_Entity
                  | N_If_Statement
                  | N_Selective_Accept
                  | N_Triggering_Alternative
               =>
                  if No (Current_Subprogram) then
                     exit when Is_List_Member (P);

                  --  Check exceptional case documented above for an enclosing
                  --  handled sequence of statements.

                  else
                     declare
                        Par : Node_Id := Parent (Parent_P);

                     begin
                        while Present (Par)
                          and then
                            Nkind (Par) /= N_Handled_Sequence_Of_Statements
                          and then Nkind (Parent (Par)) /= N_Subprogram_Body
                        loop
                           Par := Parent (Par);
                        end loop;

                        --  If we don't have a parent, then we are not in a
                        --  well-formed tree and we ignore the freeze request.
                        --  See previous comment in the enclosing loop.

                        if No (Par) then
                           return;
                        end if;

                        exit when not In_Expanded_Body (Par)
                          or else Declared_In_Expanded_Body (Par, Typ, Nam);
                     end;
                  end if;

               --  The freeze nodes produced by an expression coming from the
               --  Actions list of an N_Expression_With_Actions, short-circuit
               --  expression or N_Case_Expression_Alternative node must remain
               --  within the Actions list if they freeze an entity declared in
               --  this list, as inserting the freeze nodes further up the tree
               --  may lead to use before declaration issues for the entity.

               when N_Case_Expression_Alternative
                  | N_Expression_With_Actions
                  | N_Short_Circuit
               =>
                  exit when (Present (Nam)
                              and then
                             Has_Decl_In_List (Nam, P, Actions (Parent_P)))
                    or else (Present (Typ)
                              and then
                             Has_Decl_In_List (Typ, P, Actions (Parent_P)));

               --  Likewise for an N_If_Expression and its two Actions list

               when N_If_Expression =>
                  declare
                     L1 : constant List_Id := Then_Actions (Parent_P);
                     L2 : constant List_Id := Else_Actions (Parent_P);

                  begin
                     exit when (Present (Nam)
                                 and then
                                Has_Decl_In_List (Nam, P, L1))
                       or else (Present (Typ)
                                 and then
                                Has_Decl_In_List (Typ, P, L1))
                       or else (Present (Nam)
                                 and then
                                Has_Decl_In_List (Nam, P, L2))
                       or else (Present (Typ)
                                 and then
                                Has_Decl_In_List (Typ, P, L2));
                  end;

               --  N_Loop_Statement is a special case: a type that appears in
               --  the source can never be frozen in a loop (this occurs only
               --  because of a loop expanded by the expander), so we keep on
               --  going. Otherwise we terminate the search. Same is true of
               --  any entity which comes from source (if it has a predefined
               --  type, this type does not appear to come from source, but the
               --  entity should not be frozen here).

               when N_Loop_Statement =>
                  exit when not Comes_From_Source (Etype (N))
                    and then (No (Nam) or else not Comes_From_Source (Nam));

               --  For all other cases, keep looking at parents

               when others =>
                  null;
            end case;

            --  We fall through the case if we did not yet find the proper
            --  place in the tree for inserting the freeze node, so climb.

            P := Parent_P;
         end loop;
      end if;

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
        or else (Is_Type (Current_Scope)
                  and then (not Is_Concurrent_Type (Current_Scope)
                             or else not Has_Completion (Current_Scope)))
        or else Ekind (Current_Scope) = E_Void
      then
         declare
            Freeze_Nodes : List_Id   := No_List;
            Pos          : Int       := Scope_Stack.Last;
            Scop         : Entity_Id := Current_Scope;

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
            --  an enclosing record declaration, or a block of an enclosing
            --  declare expression in Ada 2022, or of a loop of an enclosing
            --  quantified expression or aggregate with an iterated component
            --  in Ada 2022, which is above the current scope in the scope
            --  stack. Indeed in the context of a quantified expression or
            --  an aggregate with an iterated component, an internal scope is
            --  created and pushed above the current scope in order to emulate
            --  the loop-like behavior of the construct.
            --  If the expression is within a top-level pragma, as for a pre-
            --  condition on a library-level subprogram, nothing to do.

            if not Is_Compilation_Unit (Scop) then
               if Is_Record_Type (Scope (Scop)) then
                  Pos := Pos - 1;

               else
                  while Ekind (Scop) in E_Block | E_Loop
                    and then Is_Internal (Scop)
                  loop
                     Pos  := Pos - 1;
                     Scop := Scope (Scop);
                  end loop;
               end if;
            end if;

            if Is_Non_Empty_List (Freeze_Nodes) then

               --  When the current scope is transient, insert the freeze nodes
               --  prior to the expression that produced them. Transient scopes
               --  may create additional declarations when finalizing objects
               --  or managing the secondary stack. Inserting the freeze nodes
               --  of those constructs prior to the scope would result in a
               --  freeze-before-declaration, therefore the freeze node must
               --  remain interleaved with their constructs.

               if Scope_Is_Transient then
                  Insert_Actions (N, Freeze_Nodes);

               elsif No (Scope_Stack.Table (Pos).Pending_Freeze_Actions) then
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
      --  thrown away in this mode). However, the freeze actions are from
      --  static expressions and one of the important reasons we are doing this
      --  special analysis is to get these freeze actions. Therefore we turn
      --  off the In_Spec_Expression mode to propagate these freeze actions.
      --  This also means they get properly analyzed and expanded.

      In_Spec_Expression := False;

      --  Freeze the subtype mark before a qualified expression on an
      --  allocator as per AARM 13.14(4.a). This is needed in particular to
      --  generate predicate functions.

      if Present (Allocator_Typ) then
         Freeze_Before (P, Allocator_Typ);
      end if;

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

   -----------------------
   -- Freeze_Expr_Types --
   -----------------------

   procedure Freeze_Expr_Types
     (Def_Id : Entity_Id;
      Typ    : Entity_Id;
      Expr   : Node_Id;
      N      : Node_Id)
   is
      function Cloned_Expression return Node_Id;
      --  Build a duplicate of the expression of the return statement that has
      --  no defining entities shared with the original expression.

      function Freeze_Type_Refs (Node : Node_Id) return Traverse_Result;
      --  Freeze all types referenced in the subtree rooted at Node

      -----------------------
      -- Cloned_Expression --
      -----------------------

      function Cloned_Expression return Node_Id is
         function Clone_Id (Node : Node_Id) return Traverse_Result;
         --  Tree traversal routine that clones the defining identifier of
         --  iterator and loop parameter specification nodes.

         --------------
         -- Clone_Id --
         --------------

         function Clone_Id (Node : Node_Id) return Traverse_Result is
         begin
            if Nkind (Node) in
                 N_Iterator_Specification | N_Loop_Parameter_Specification
            then
               Set_Defining_Identifier
                 (Node, New_Copy (Defining_Identifier (Node)));
            end if;

            return OK;
         end Clone_Id;

         procedure Clone_Def_Ids is new Traverse_Proc (Clone_Id);

         --  Local variable

         Dup_Expr : constant Node_Id := New_Copy_Tree (Expr);

      --  Start of processing for Cloned_Expression

      begin
         --  We must duplicate the expression with semantic information to
         --  inherit the decoration of global entities in generic instances.
         --  Set the parent of the new node to be the parent of the original
         --  to get the proper context, which is needed for complete error
         --  reporting and for semantic analysis.

         Set_Parent (Dup_Expr, Parent (Expr));

         --  Replace the defining identifier of iterators and loop param
         --  specifications by a clone to ensure that the cloned expression
         --  and the original expression don't have shared identifiers;
         --  otherwise, as part of the preanalysis of the expression, these
         --  shared identifiers may be left decorated with itypes which
         --  will not be available in the tree passed to the backend.

         Clone_Def_Ids (Dup_Expr);

         return Dup_Expr;
      end Cloned_Expression;

      ----------------------
      -- Freeze_Type_Refs --
      ----------------------

      function Freeze_Type_Refs (Node : Node_Id) return Traverse_Result is
         procedure Check_And_Freeze_Type (Typ : Entity_Id);
         --  Check that Typ is fully declared and freeze it if so

         ---------------------------
         -- Check_And_Freeze_Type --
         ---------------------------

         procedure Check_And_Freeze_Type (Typ : Entity_Id) is
         begin
            --  Skip Itypes created by the preanalysis, and itypes whose
            --  scope is another type (i.e. component subtypes that depend
            --  on a discriminant),

            if Is_Itype (Typ)
              and then (Scope_Within_Or_Same (Scope (Typ), Def_Id)
                         or else Is_Type (Scope (Typ)))
            then
               return;
            end if;

            --  This provides a better error message than generating primitives
            --  whose compilation fails much later. Refine the error message if
            --  possible.

            Check_Fully_Declared (Typ, Node);

            if Error_Posted (Node) then
               if Has_Private_Component (Typ)
                 and then not Is_Private_Type (Typ)
               then
                  Error_Msg_NE ("\type& has private component", Node, Typ);
               end if;

            else
               Freeze_Before (N, Typ);
            end if;
         end Check_And_Freeze_Type;

      --  Start of processing for Freeze_Type_Refs

      begin
         --  Check that a type referenced by an entity can be frozen

         if Is_Entity_Name (Node) and then Present (Entity (Node)) then
            --  The entity itself may be a type, as in a membership test
            --  or an attribute reference. Freezing its own type would be
            --  incomplete if the entity is derived or an extension.

            if Is_Type (Entity (Node)) then
               Check_And_Freeze_Type (Entity (Node));

            else
               Check_And_Freeze_Type (Etype (Entity (Node)));
            end if;

            --  Check that the enclosing record type can be frozen

            if Ekind (Entity (Node)) in E_Component | E_Discriminant then
               Check_And_Freeze_Type (Scope (Entity (Node)));
            end if;

         --  Freezing an access type does not freeze the designated type, but
         --  freezing conversions between access to interfaces requires that
         --  the interface types themselves be frozen, so that dispatch table
         --  entities are properly created.

         --  Unclear whether a more general rule is needed ???

         elsif Nkind (Node) = N_Type_Conversion
           and then Is_Access_Type (Etype (Node))
           and then Is_Interface (Designated_Type (Etype (Node)))
         then
            Check_And_Freeze_Type (Designated_Type (Etype (Node)));
         end if;

         --  An implicit dereference freezes the designated type. In the case
         --  of a dispatching call whose controlling argument is an access
         --  type, the dereference is not made explicit, so we must check for
         --  such a call and freeze the designated type.

         if Nkind (Node) in N_Has_Etype
           and then Present (Etype (Node))
           and then Is_Access_Type (Etype (Node))
         then
            if Nkind (Parent (Node)) = N_Function_Call
              and then Node = Controlling_Argument (Parent (Node))
            then
               Check_And_Freeze_Type (Designated_Type (Etype (Node)));

            --  An explicit dereference freezes the designated type as well,
            --  even though that type is not attached to an entity in the
            --  expression.

            elsif Nkind (Parent (Node)) = N_Explicit_Dereference then
               Check_And_Freeze_Type (Designated_Type (Etype (Node)));
            end if;

         --  An iterator specification freezes the iterator type, even though
         --  that type is not attached to an entity in the construct.

         elsif Nkind (Node) in N_Has_Etype
           and then Present (Etype (Node))
           and then Nkind (Parent (Node)) = N_Iterator_Specification
           and then Node = Name (Parent (Node))
         then
            declare
               Iter : constant Node_Id :=
                 Find_Value_Of_Aspect (Etype (Node), Aspect_Default_Iterator);

            begin
               if Present (Iter) then
                  Check_And_Freeze_Type (Etype (Iter));
               end if;
            end;
         end if;

         --  No point in posting several errors on the same expression

         if Serious_Errors_Detected > 0 then
            return Abandon;
         else
            return OK;
         end if;
      end Freeze_Type_Refs;

      procedure Freeze_References is new Traverse_Proc (Freeze_Type_Refs);

      --  Local variables

      Saved_First_Entity : constant Entity_Id := First_Entity (Def_Id);
      Saved_Last_Entity  : constant Entity_Id := Last_Entity  (Def_Id);
      Dup_Expr           : constant Node_Id   := Cloned_Expression;

   --  Start of processing for Freeze_Expr_Types

   begin
      --  Preanalyze a duplicate of the expression to have available the
      --  minimum decoration needed to locate referenced unfrozen types
      --  without adding any decoration to the function expression.

      --  This routine is also applied to expressions in the contract for
      --  the subprogram. If that happens when expanding the code for
      --  pre/postconditions during expansion of the subprogram body, the
      --  subprogram is already installed.

      if Def_Id /= Current_Scope then
         Push_Scope (Def_Id);
         Install_Formals (Def_Id);

         Preanalyze_Spec_Expression (Dup_Expr, Typ);
         End_Scope;
      else
         Preanalyze_Spec_Expression (Dup_Expr, Typ);
      end if;

      --  Restore certain attributes of Def_Id since the preanalysis may
      --  have introduced itypes to this scope, thus modifying attributes
      --  First_Entity and Last_Entity.

      Set_First_Entity (Def_Id, Saved_First_Entity);
      Set_Last_Entity  (Def_Id, Saved_Last_Entity);

      if Present (Last_Entity (Def_Id)) then
         Set_Next_Entity (Last_Entity (Def_Id), Empty);
      end if;

      --  Freeze all types referenced in the expression

      Freeze_References (Dup_Expr);
   end Freeze_Expr_Types;

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
      Ftyp  : constant Entity_Id  := Underlying_Type (First_Subtype (Typ));

      Small : Ureal;
      Loval : Ureal;
      Hival : Ureal;
      Atype : Entity_Id;

      Orig_Lo : Ureal;
      Orig_Hi : Ureal;
      --  Save original bounds (for shaving tests)

      Actual_Size : Int;
      --  Actual size chosen

      function Fsize (Lov, Hiv : Ureal) return Int;
      --  Returns size of type with given bounds. Also leaves these
      --  bounds set as the current bounds of the Typ.

      function Larger (A, B : Ureal) return Boolean;
      --  Returns true if A > B with a margin of Typ'Small

      function Smaller (A, B : Ureal) return Boolean;
      --  Returns true if A < B with a margin of Typ'Small

      -----------
      -- Fsize --
      -----------

      function Fsize (Lov, Hiv : Ureal) return Int is
      begin
         Set_Realval (Lo, Lov);
         Set_Realval (Hi, Hiv);
         return Minimum_Size (Typ);
      end Fsize;

      ------------
      -- Larger --
      ------------

      function Larger (A, B : Ureal) return Boolean is
      begin
         return A > B and then A - Small_Value (Typ) > B;
      end Larger;

      -------------
      -- Smaller --
      -------------

      function Smaller (A, B : Ureal) return Boolean is
      begin
         return A < B and then A + Small_Value (Typ) < B;
      end Smaller;

   --  Start of processing for Freeze_Fixed_Point_Type

   begin
      --  The type, or its first subtype if we are freezing the anonymous
      --  base, may have a delayed Small aspect. It must be analyzed now,
      --  so that all characteristics of the type (size, bounds) can be
      --  computed and validated in the call to Minimum_Size that follows.

      if Has_Delayed_Aspects (Ftyp) then
         Analyze_Aspects_At_Freeze_Point (Ftyp);
         Set_Has_Delayed_Aspects (Ftyp, False);
      end if;

      if May_Inherit_Delayed_Rep_Aspects (Ftyp) then
         Inherit_Delayed_Rep_Aspects (Ftyp);
         Set_May_Inherit_Delayed_Rep_Aspects (Ftyp, False);
      end if;

      --  Inherit the Small value from the first subtype in any case

      if Typ /= Ftyp then
         Set_Small_Value (Typ, Small_Value (Ftyp));
      end if;

      --  If Esize of a subtype has not previously been set, set it now

      if not Known_Esize (Typ) then
         Atype := Ancestor_Subtype (Typ);

         if Present (Atype) then
            Set_Esize (Typ, Esize (Atype));
         else
            Copy_Esize (To => Typ, From => Btyp);
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

      Small := Small_Value (Typ);
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

            Size_Incl_EP  : Int;
            Size_Excl_EP  : Int;

            Model_Num     : Ureal;
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

            --  Compute the fudged bounds. If the bound is a model number, (or
            --  greater if given low bound, smaller if high bound) then we do
            --  nothing to include it, but we are allowed to backoff to the
            --  next adjacent model number when we exclude it. If it is not a
            --  model number then we straddle the two values with the model
            --  numbers on either side.

            Model_Num := UR_Trunc (Loval / Small) * Small;

            if UR_Ge (Loval, Model_Num) then
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

            if UR_Le (Hival, Model_Num) then
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

               if Has_Size_Clause (Ftyp)
                 and then Size_Incl_EP <= Esize (Ftyp)
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

         --  Enforce some limitations for ordinary fixed-point types. They come
         --  from an exact algorithm used to implement Text_IO.Fixed_IO and the
         --  Fore, Image and Value attributes. The requirement on the Small is
         --  to lie in the range 2**(-(Siz - 1)) .. 2**(Siz - 1) for a type of
         --  Siz bits (Siz=32,64,128) and the requirement on the bounds is to
         --  be smaller in magnitude than 10.0**N * 2**(Siz - 1), where N is
         --  given by the formula N = floor ((Siz - 1) * log 2 / log 10).

         --  If the bounds of a 32-bit type are too large, force 64-bit type

         if Actual_Size <= 32
           and then Small <= Ureal_2_31
           and then (Smaller (Expr_Value_R (Lo), Ureal_M_2_10_18)
                      or else Larger (Expr_Value_R (Hi), Ureal_2_10_18))
         then
            Actual_Size := 33;
         end if;

         --  If the bounds of a 64-bit type are too large, force 128-bit type

         if System_Max_Integer_Size = 128
           and then Actual_Size <= 64
           and then Small <= Ureal_2_63
           and then (Smaller (Expr_Value_R (Lo), Ureal_M_9_10_36)
                      or else Larger (Expr_Value_R (Hi), Ureal_9_10_36))
         then
            Actual_Size := 65;
         end if;

         --  Give error messages for first subtypes and not base types, as the
         --  bounds of base types are always maximum for their size, see below.

         if System_Max_Integer_Size < 128 and then Typ /= Btyp then

            --  See the 128-bit case below for the reason why we cannot test
            --  against the 2**(-63) .. 2**63 range. This quirk should have
            --  been kludged around as in the 128-bit case below, but it was
            --  not and we end up with a ludicrous range as a result???

            if Small < Ureal_2_M_80 then
               Error_Msg_Name_1 := Name_Small;
               Error_Msg_N
                 ("`&''%` too small, minimum allowed is 2.0'*'*(-80)", Typ);

            elsif Small > Ureal_2_80 then
               Error_Msg_Name_1 := Name_Small;
               Error_Msg_N
                 ("`&''%` too large, maximum allowed is 2.0'*'*80", Typ);
            end if;

            if Smaller (Expr_Value_R (Lo), Ureal_M_9_10_36) then
               Error_Msg_Name_1 := Name_First;
               Error_Msg_N
                 ("`&''%` too small, minimum allowed is -9.0E+36", Typ);
            end if;

            if Larger (Expr_Value_R (Hi), Ureal_9_10_36) then
               Error_Msg_Name_1 := Name_Last;
               Error_Msg_N
                 ("`&''%` too large, maximum allowed is 9.0E+36", Typ);
            end if;

         elsif System_Max_Integer_Size = 128 and then Typ /= Btyp then

            --  ACATS c35902d tests a delta equal to 2**(-(Max_Mantissa + 1))
            --  but we cannot really support anything smaller than Fine_Delta
            --  because of the way we implement I/O for fixed point types???

            if Small = Ureal_2_M_128 then
               null;

            elsif Small < Ureal_2_M_127 then
               Error_Msg_Name_1 := Name_Small;
               Error_Msg_N
                 ("`&''%` too small, minimum allowed is 2.0'*'*(-127)", Typ);

            elsif Small > Ureal_2_127 then
               Error_Msg_Name_1 := Name_Small;
               Error_Msg_N
                 ("`&''%` too large, maximum allowed is 2.0'*'*127", Typ);
            end if;

            if Actual_Size > 64
              and then (Norm_Num (Small) > Uint_2 ** 127
                         or else Norm_Den (Small) > Uint_2 ** 127)
              and then Small /= Ureal_2_M_128
            then
               Error_Msg_Name_1 := Name_Small;
               Error_Msg_N
                 ("`&''%` not the ratio of two 128-bit integers", Typ);
            end if;

            if Smaller (Expr_Value_R (Lo), Ureal_M_10_76) then
               Error_Msg_Name_1 := Name_First;
               Error_Msg_N
                 ("`&''%` too small, minimum allowed is -1.0E+76", Typ);
            end if;

            if Larger (Expr_Value_R (Hi), Ureal_10_76) then
               Error_Msg_Name_1 := Name_Last;
               Error_Msg_N
                 ("`&''%` too large, maximum allowed is 1.0E+76", Typ);
            end if;
         end if;

      --  For the decimal case, none of this fudging is required, since there
      --  are no end-point problems in the decimal case (the end-points are
      --  always included).

      else
         Actual_Size := Fsize (Loval, Hival);
      end if;

      --  At this stage, the actual size has been calculated and the proper
      --  required bounds are stored in the low and high bounds.

      if Actual_Size > System_Max_Integer_Size then
         Error_Msg_Uint_1 := UI_From_Int (Actual_Size);
         Error_Msg_Uint_2 := UI_From_Int (System_Max_Integer_Size);
         Error_Msg_N
           ("size required (^) for type& too large, maximum allowed is ^",
            Typ);
         Actual_Size := System_Max_Integer_Size;
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
         elsif Actual_Size <= 64 then
            Actual_Size := 64;
         else
            Actual_Size := 128;
         end if;

         Set_Esize (Typ, UI_From_Int (Actual_Size));
         Adjust_Esize_For_Alignment (Typ);
      end if;

      --  If we have a base type, then expand the bounds so that they extend to
      --  the full width of the allocated size in bits, to avoid junk range
      --  checks on intermediate computations.

      if Typ = Btyp then
         Set_Realval (Lo, -(Small * (Uint_2 ** (Actual_Size - 1))));
         Set_Realval (Hi,  (Small * (Uint_2 ** (Actual_Size - 1) - 1)));
      end if;

      --  Final step is to reanalyze the bounds using the proper type
      --  and set the Corresponding_Integer_Value fields of the literals.

      Set_Etype (Lo, Empty);
      Set_Analyzed (Lo, False);
      Analyze (Lo);

      --  Resolve with universal fixed if the base type, and with the base
      --  type if we are freezing a subtype. Note we can't resolve the base
      --  type with itself, that would be a reference before definition.
      --  The resolution of the bounds of a subtype, if they are given by real
      --  literals,  includes the setting of the Corresponding_Integer_Value,
      --  as for other literals of a fixed-point type.

      if Typ = Btyp then
         Resolve (Lo, Universal_Fixed);
         Set_Corresponding_Integer_Value
           (Lo, UR_To_Uint (Realval (Lo) / Small));
      else
         Resolve (Lo, Btyp);
      end if;

      --  Similar processing for high bound

      Set_Etype (Hi, Empty);
      Set_Analyzed (Hi, False);
      Analyze (Hi);

      if Typ = Btyp then
         Resolve (Hi, Universal_Fixed);
         Set_Corresponding_Integer_Value
           (Hi, UR_To_Uint (Realval (Hi) / Small));
      else
         Resolve (Hi, Btyp);
      end if;

      --  Set type of range to correspond to bounds

      Set_Etype (Rng, Etype (Lo));

      --  Set Esize to calculated size if not set already

      if not Known_Esize (Typ) then
         Set_Esize (Typ, UI_From_Int (Actual_Size));
      end if;

      --  Set RM_Size if not already set. If already set, check value

      declare
         Minsiz : constant Uint := UI_From_Int (Minimum_Size (Typ));

      begin
         if Known_RM_Size (Typ) then
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

         --  In SPARK mode the given bounds must be strictly representable

         if SPARK_Mode = On then
            if Orig_Lo < Expr_Value_R (Lo) then
               Error_Msg_NE
                 ("declared low bound of type & is outside type range",
                  Lo, Typ);
            end if;

            if Orig_Hi > Expr_Value_R (Hi) then
               Error_Msg_NE
                 ("declared high bound of type & is outside type range",
                  Hi, Typ);
            end if;

         else
            if Orig_Lo < Expr_Value_R (Lo) then
               Error_Msg_N
                 ("declared low bound of type & is outside type range??", Typ);
               Error_Msg_N
                 ("\low bound adjusted up by delta (RM 3.5.9(13))??", Typ);
            end if;

            if Orig_Hi > Expr_Value_R (Hi) then
               Error_Msg_N
                 ("declared high bound of type & is outside type range??",
                  Typ);
               Error_Msg_N
                 ("\high bound adjusted down by delta (RM 3.5.9(13))??", Typ);
            end if;
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

      Insert_Actions (N, L);
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

      procedure Set_Profile_Convention (Subp_Id : Entity_Id);
      --  Set the conventions of all anonymous access-to-subprogram formals and
      --  result subtype of subprogram Subp_Id to the convention of Subp_Id.

      ----------------------------
      -- Set_Profile_Convention --
      ----------------------------

      procedure Set_Profile_Convention (Subp_Id : Entity_Id) is
         Conv : constant Convention_Id := Convention (Subp_Id);

         procedure Set_Type_Convention (Typ : Entity_Id);
         --  Set the convention of anonymous access-to-subprogram type Typ and
         --  its designated type to Conv.

         -------------------------
         -- Set_Type_Convention --
         -------------------------

         procedure Set_Type_Convention (Typ : Entity_Id) is
         begin
            --  Set the convention on both the anonymous access-to-subprogram
            --  type and the subprogram type it points to because both types
            --  participate in conformance-related checks.

            if Ekind (Typ) = E_Anonymous_Access_Subprogram_Type then
               Set_Convention (Typ, Conv);
               Set_Convention (Designated_Type (Typ), Conv);
            end if;
         end Set_Type_Convention;

         --  Local variables

         Formal : Entity_Id;

      --  Start of processing for Set_Profile_Convention

      begin
         Formal := First_Formal (Subp_Id);
         while Present (Formal) loop
            Set_Type_Convention (Etype (Formal));
            Next_Formal (Formal);
         end loop;

         if Ekind (Subp_Id) = E_Function then
            Set_Type_Convention (Etype (Subp_Id));
         end if;
      end Set_Profile_Convention;

      --  Local variables

      F      : Entity_Id;
      Retype : Entity_Id;

   --  Start of processing for Freeze_Subprogram

   begin
      --  Subprogram may not have an address clause unless it is imported

      if Present (Address_Clause (E)) then
         if not Is_Imported (E) then
            Error_Msg_N
              ("address clause can only be given for imported subprogram",
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

      --  For C++ constructors check that their external name has been given
      --  (either in pragma CPP_Constructor or in a pragma import).

      if Is_Constructor (E)
        and then Convention (E) = Convention_CPP
        and then
           (No (Interface_Name (E))
              or else String_Equal
                        (L => Strval (Interface_Name (E)),
                         R => Strval (Get_Default_External_Name (E))))
      then
         Error_Msg_N
           ("'C++ constructor must have external name or link name", E);
      end if;

      --  We also reset the Pure indication on a subprogram with an Address
      --  parameter, because the parameter may be used as a pointer and the
      --  referenced data may change even if the address value does not.

      --  Note that if the programmer gave an explicit Pure_Function pragma,
      --  then we believe the programmer, and leave the subprogram Pure. We
      --  also suppress this check on run-time files.

      if Is_Pure (E)
        and then Is_Subprogram (E)
        and then not Has_Pragma_Pure_Function (E)
        and then not Is_Internal_Unit (Current_Sem_Unit)
      then
         Check_Function_With_Address_Parameter (E);
      end if;

      --  Ensure that all anonymous access-to-subprogram types inherit the
      --  convention of their related subprogram (RM 6.3.1(13.1/5)). This is
      --  not done for a defaulted convention Ada because those types also
      --  default to Ada. Convention Protected must not be propagated when
      --  the subprogram is an entry because this would be illegal. The only
      --  way to force convention Protected on these kinds of types is to
      --  include keyword "protected" in the access definition. Conventions
      --  Entry and Intrinsic are also not propagated (specified by AI12-0207).

      if Convention (E) /= Convention_Ada
        and then Convention (E) /= Convention_Protected
        and then Convention (E) /= Convention_Entry
        and then Convention (E) /= Convention_Intrinsic
      then
         Set_Profile_Convention (E);
      end if;

      --  For non-foreign convention subprograms, this is where we create
      --  the extra formals (for accessibility level and constrained bit
      --  information). We delay this till the freeze point precisely so
      --  that we know the convention.

      if not Has_Foreign_Convention (E) then

         --  Extra formals of dispatching operations are added later by
         --  Expand_Freeze_Record_Type, which also adds extra formals to
         --  internal entities built to handle interface types.

         if not Is_Dispatching_Operation (E) then
            Create_Extra_Formals (E);

            pragma Assert
              ((Ekind (E) = E_Subprogram_Type
                  and then Extra_Formals_OK (E))
               or else
                 (Is_Subprogram (E)
                   and then Extra_Formals_OK (E)
                   and then
                     (No (Overridden_Operation (E))
                       or else Extra_Formals_Match_OK (E,
                                 Ultimate_Alias (Overridden_Operation (E))))));
         end if;

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

      if Is_Dispatching_Operation (E)
        and then Present (Overridden_Operation (E))
      then
         Local_Restrict.Check_Overriding
           (Overrider_Op => E, Overridden_Op => Overridden_Operation (E));
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

      --  Check illegal subprograms of tagged types and interface types that
      --  have aspect/pragma First_Controlling_Parameter.

      if Comes_From_Source (E)
        and then Is_Abstract_Subprogram (E)
      then
         if Is_Dispatching_Operation (E) then
            if Ekind (E) = E_Function
              and then Is_Interface (Etype (E))
              and then not Is_Class_Wide_Type (Etype (E))
              and then Has_First_Controlling_Parameter_Aspect
                         (Find_Dispatching_Type (E))
            then
               Error_Msg_NE
                 ("'First_'Controlling_'Parameter disallows returning a "
                  & "non-class-wide interface type",
                  E, Etype (E));
            end if;

         else
            --  The type of the formals cannot be an interface type

            if Present (First_Formal (E)) then
               declare
                  Formal     : Entity_Id := First_Formal (E);
                  Has_Aspect : Boolean := False;

               begin
                  --  Check if some formal has the aspect

                  while Present (Formal) loop
                     if Is_Tagged_Type (Etype (Formal))
                       and then
                         Has_First_Controlling_Parameter_Aspect
                           (Etype (Formal))
                     then
                        Has_Aspect := True;
                     end if;

                     Next_Formal (Formal);
                  end loop;

                  --  If the aspect is present then report the error

                  if Has_Aspect then
                     Formal := First_Formal (E);

                     while Present (Formal) loop
                        if Is_Interface (Etype (Formal))
                          and then not Is_Class_Wide_Type (Etype (Formal))
                        then
                           Error_Msg_NE
                             ("not a dispatching primitive of interface type&",
                              E, Etype (Formal));
                           Error_Msg_N
                             ("\disallowed by 'First_'Controlling_'Parameter "
                              & "aspect", E);
                        end if;

                        Next_Formal (Formal);
                     end loop;
                  end if;
               end;
            end if;

            if Ekind (E) = E_Function
              and then Is_Interface (Etype (E))
              and then not Is_Class_Wide_Type (Etype (E))
              and then Has_First_Controlling_Parameter_Aspect (Etype (E))
            then
               Error_Msg_NE
                 ("not a dispatching primitive of interface type&",
                  E, Etype (E));
               Error_Msg_N
                 ("\disallowed by 'First_'Controlling_'Parameter "
                  & "aspect", E);
            end if;
         end if;
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
            --  The following cases have no side effects, and are analyzed
            --  directly.

            if Nkind (Dcopy) = N_Identifier
              or else Nkind (Dcopy) in N_Expanded_Name
                                     | N_Integer_Literal
                                     | N_Character_Literal
                                     | N_String_Literal
                                     | N_Real_Literal
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
            --  Given that this is done only to complete the analysis, it is
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

   ------------------------
   -- Should_Freeze_Type --
   ------------------------

   function Should_Freeze_Type
     (Typ : Entity_Id;
      E   : Entity_Id;
      N   : Node_Id) return Boolean
   is
      Decl : constant Node_Id := Original_Node (Unit_Declaration_Node (E));

      function Is_Dispatching_Call_Or_Tagged_Result_Or_Aggregate
        (N : Node_Id) return Traverse_Result;
      --  Return Abandon if N is a dispatching call to a subprogram
      --  declared in the same scope as Typ, or a tagged result that
      --  needs specific expansion, or an aggregate whose type is Typ.

      function Check_Freezing is new
        Traverse_Func (Is_Dispatching_Call_Or_Tagged_Result_Or_Aggregate);
      --  Return Abandon if the input expression requires freezing Typ

      function Within_Simple_Return_Statement (N : Node_Id) return Boolean;
      --  Determine whether N is the expression of a simple return statement,
      --  or the dependent expression of a conditional expression which is
      --  the expression of a simple return statement, including recursively.

      -------------------------------------------------------
      -- Is_Dispatching_Call_Or_Tagged_Result_Or_Aggregate --
      -------------------------------------------------------

      function Is_Dispatching_Call_Or_Tagged_Result_Or_Aggregate
        (N : Node_Id) return Traverse_Result
      is
      begin
         if Nkind (N) = N_Function_Call
           and then Present (Controlling_Argument (N))
           and then Scope (Entity (Original_Node (Name (N)))) = Scope (Typ)
         then
            return Abandon;

         --  The expansion done in Expand_Simple_Function_Return will assign
         --  the tag to the result in this case.

         elsif Is_Conversion_Or_Reference_To_Formal (N)
           and then Within_Simple_Return_Statement (N)
           and then Etype (N) = Typ
           and then Is_Tagged_Type (Typ)
           and then not Is_Class_Wide_Type (Typ)
         then
            return Abandon;

         elsif Nkind (N) in N_Aggregate
                          | N_Delta_Aggregate
                          | N_Extension_Aggregate
           and then Base_Type (Etype (N)) = Base_Type (Typ)
         then
            return Abandon;

         else
            return OK;
         end if;
      end Is_Dispatching_Call_Or_Tagged_Result_Or_Aggregate;

      ------------------------------------
      -- Within_Simple_Return_Statement --
      ------------------------------------

      function Within_Simple_Return_Statement (N : Node_Id) return Boolean is
         Par : constant Node_Id := Parent (N);

      begin
         if Nkind (Par) = N_Simple_Return_Statement then
            return True;

         elsif Nkind (Par) = N_Case_Expression_Alternative then
            return Within_Simple_Return_Statement (Parent (Par));

         elsif Nkind (Par) = N_If_Expression
           and then N /= First (Expressions (Par))
         then
            return Within_Simple_Return_Statement (Par);

         else
            return False;
         end if;
      end Within_Simple_Return_Statement;

   --  Start of processing for Should_Freeze_Type

   begin
      return Within_Scope (Typ, Current_Scope)
        or else (Nkind (N) = N_Subprogram_Renaming_Declaration
                  and then Present (Corresponding_Formal_Spec (N)))
        or else (Present (Decl)
                  and then Nkind (Decl) = N_Expression_Function
                  and then Check_Freezing (Expression (Decl)) = Abandon);
   end Should_Freeze_Type;

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

   procedure Warn_Overlay (Expr : Node_Id; Typ : Entity_Id; Nam : Node_Id) is
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

      --  No warning if there is no default initialization

      if No_Initialization (Declaration_Node (Ent)) then
         return;
      end if;

      --  We only give the warning for non-imported entities of a type for
      --  which a non-null base init proc is defined, or for objects of access
      --  types with implicit null initialization, or when Normalize_Scalars
      --  applies and the type is scalar or a string type (the latter being
      --  tested for because predefined String types are initialized by inline
      --  code rather than by an init_proc). Note that we do not give the
      --  warning for Initialize_Scalars, since we suppressed initialization
      --  in this case. Also, do not warn if Suppress_Initialization is set
      --  either on the type, or on the object via pragma or aspect.

      if Present (Expr)
        and then not Is_Imported (Ent)
        and then not Initialization_Suppressed (Typ)
        and then not (Ekind (Ent) = E_Variable
                       and then Initialization_Suppressed (Ent))
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
              ("default initialization of & may modify &?o?",
               Nam);
         else
            Error_Msg_N
              ("default initialization of & may modify overlaid storage?o?",
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
                        "will be initialized to zero?o?",
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
            "suppress initialization (RM B.1(24))?o?",
            Nam);
      end if;
   end Warn_Overlay;

end Freeze;
