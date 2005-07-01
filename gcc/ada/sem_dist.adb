------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ D I S T                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2005, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Atree;    use Atree;
with Casing;   use Casing;
with Einfo;    use Einfo;
with Errout;   use Errout;
with Exp_Dist; use Exp_Dist;
with Exp_Tss;  use Exp_Tss;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Namet;    use Namet;
with Opt;      use Opt;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Stand;    use Stand;
with Stringt;  use Stringt;
with Tbuild;   use Tbuild;

package body Sem_Dist is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure RAS_E_Dereference (Pref : Node_Id);
   --  Handles explicit dereference of Remote Access to Subprograms.

   function Full_Qualified_Name (E : Entity_Id) return String_Id;
   --  returns the full qualified name of the entity in lower case.

   -------------------------
   -- Add_Stub_Constructs --
   -------------------------

   procedure Add_Stub_Constructs (N : Node_Id) is
      U    : constant Node_Id := Unit (N);
      Spec : Entity_Id        := Empty;
      Exp  : Node_Id          := U;         --  Unit that will be expanded

   begin
      pragma Assert (Distribution_Stub_Mode /= No_Stubs);

      if Nkind (U) = N_Package_Declaration then
         Spec := Defining_Entity (Specification (U));

      elsif Nkind (U) = N_Package_Body then
         Spec := Corresponding_Spec (U);

      else pragma Assert (Nkind (U) = N_Package_Instantiation);
         Exp  := Instance_Spec (U);
         Spec := Defining_Entity (Specification (Exp));
      end if;

      pragma Assert (Is_Shared_Passive (Spec)
        or else Is_Remote_Call_Interface (Spec));

      if Distribution_Stub_Mode = Generate_Caller_Stub_Body then

         if Is_Shared_Passive (Spec) then
            null;
         elsif Nkind (U) = N_Package_Body then
            Error_Msg_N
              ("Specification file expected from command line", U);
         else
            Expand_Calling_Stubs_Bodies (Exp);
         end if;

      else

         if Is_Shared_Passive (Spec) then
            Build_Passive_Partition_Stub (Exp);
         else
            Expand_Receiving_Stubs_Bodies (Exp);
         end if;

      end if;
   end Add_Stub_Constructs;

   ---------------------------------------
   -- Build_RAS_Primitive_Specification --
   ---------------------------------------

   function Build_RAS_Primitive_Specification
     (Subp_Spec          : Node_Id;
      Remote_Object_Type : Node_Id) return Node_Id
   is
      Loc : constant Source_Ptr := Sloc (Subp_Spec);

      Primitive_Spec : constant Node_Id :=
                         Copy_Specification (Loc,
                           Spec     => Subp_Spec,
                           New_Name => Name_Call);

      Subtype_Mark_For_Self : Node_Id;

   begin
      if No (Parameter_Specifications (Primitive_Spec)) then
         Set_Parameter_Specifications (Primitive_Spec, New_List);
      end if;

      if Nkind (Remote_Object_Type) in N_Entity then
         Subtype_Mark_For_Self :=
           New_Occurrence_Of (Remote_Object_Type, Loc);
      else
         Subtype_Mark_For_Self := Remote_Object_Type;
      end if;

      Prepend_To (
        Parameter_Specifications (Primitive_Spec),
        Make_Parameter_Specification (Loc,
          Defining_Identifier =>
            Make_Defining_Identifier (Loc, Name_uS),
          Parameter_Type      =>
            Make_Access_Definition (Loc,
              Subtype_Mark =>
                Subtype_Mark_For_Self)));

      --  Trick later semantic analysis into considering this
      --  operation as a primitive (dispatching) operation of
      --  tagged type Obj_Type.

      Set_Comes_From_Source (
        Defining_Unit_Name (Primitive_Spec), True);

      return Primitive_Spec;
   end Build_RAS_Primitive_Specification;

   -------------------------
   -- Full_Qualified_Name --
   -------------------------

   function Full_Qualified_Name (E : Entity_Id) return String_Id is
      Ent         : Entity_Id := E;
      Parent_Name : String_Id := No_String;

   begin
      --  Deals properly with child units

      if Nkind (Ent) = N_Defining_Program_Unit_Name then
         Ent := Defining_Identifier (Ent);
      end if;

      --  Compute recursively the qualification. Only "Standard" has no scope.

      if Present (Scope (Scope (Ent))) then
         Parent_Name := Full_Qualified_Name (Scope (Ent));
      end if;

      --  Every entity should have a name except some expanded blocks
      --  don't bother about those.

      if Chars (Ent) = No_Name then
         return Parent_Name;
      end if;

      --  Add a period between Name and qualification

      if Parent_Name /= No_String then
         Start_String (Parent_Name);
         Store_String_Char (Get_Char_Code ('.'));

      else
         Start_String;
      end if;

      --  Generates the entity name in upper case

      Get_Name_String (Chars (Ent));
      Set_Casing (All_Lower_Case);
      Store_String_Chars (Name_Buffer (1 .. Name_Len));
      return End_String;
   end Full_Qualified_Name;

   ------------------
   -- Get_PCS_Name --
   ------------------

   function Get_PCS_Name return PCS_Names is
      PCS_Name : constant PCS_Names :=
                   Chars (Entity (Expression
                                    (Parent (RTE (RE_DSA_Implementation)))));
   begin
      return PCS_Name;
   end Get_PCS_Name;

   ------------------------
   -- Is_All_Remote_Call --
   ------------------------

   function Is_All_Remote_Call (N : Node_Id) return Boolean is
      Par : Node_Id;

   begin
      if (Nkind (N) = N_Function_Call
              or else Nkind (N) = N_Procedure_Call_Statement)
        and then Nkind (Name (N)) in N_Has_Entity
        and then Is_Remote_Call_Interface (Entity (Name (N)))
        and then Has_All_Calls_Remote (Scope (Entity (Name (N))))
        and then Comes_From_Source (N)
      then
         Par := Parent (Entity (Name (N)));

         while Present (Par)
           and then (Nkind (Par) /= N_Package_Specification
                       or else Is_Wrapper_Package (Defining_Entity (Par)))
         loop
            Par := Parent (Par);
         end loop;

         if Present (Par) then
            return
              not Scope_Within_Or_Same (Current_Scope, Defining_Entity (Par));
         else
            return False;
         end if;
      else
         return False;
      end if;
   end Is_All_Remote_Call;

   ------------------------------------
   -- Package_Specification_Of_Scope --
   ------------------------------------

   function Package_Specification_Of_Scope (E : Entity_Id) return Node_Id is
      N : Node_Id := Parent (E);
   begin
      while Nkind (N) /= N_Package_Specification loop
         N := Parent (N);
      end loop;

      return N;
   end Package_Specification_Of_Scope;

   --------------------------
   -- Process_Partition_ID --
   --------------------------

   procedure Process_Partition_Id (N : Node_Id) is
      Loc            : constant Source_Ptr := Sloc (N);
      Ety            : Entity_Id;
      Get_Pt_Id      : Node_Id;
      Get_Pt_Id_Call : Node_Id;
      Prefix_String  : String_Id;
      Typ            : constant Entity_Id := Etype (N);

   begin
      Ety := Entity (Prefix (N));

      --  In case prefix is not a library unit entity, get the entity
      --  of library unit.

      while (Present (Scope (Ety))
        and then Scope (Ety) /= Standard_Standard)
        and not Is_Child_Unit (Ety)
      loop
         Ety := Scope (Ety);
      end loop;

      --  Retrieve the proper function to call.

      if Is_Remote_Call_Interface (Ety) then
         Get_Pt_Id := New_Occurrence_Of
           (RTE (RE_Get_Active_Partition_Id), Loc);

      elsif Is_Shared_Passive (Ety) then
         Get_Pt_Id := New_Occurrence_Of
           (RTE (RE_Get_Passive_Partition_Id), Loc);

      else
         Get_Pt_Id := New_Occurrence_Of
           (RTE (RE_Get_Local_Partition_Id), Loc);
      end if;

      --  Get and store the String_Id corresponding to the name of the
      --  library unit whose Partition_Id is needed.

      Get_Library_Unit_Name_String (Unit_Declaration_Node (Ety));
      Prefix_String := String_From_Name_Buffer;

      --  Build the function call which will replace the attribute

      if Is_Remote_Call_Interface (Ety)
        or else Is_Shared_Passive (Ety)
      then
         Get_Pt_Id_Call :=
           Make_Function_Call (Loc,
             Name => Get_Pt_Id,
             Parameter_Associations =>
               New_List (Make_String_Literal (Loc, Prefix_String)));

      else
         Get_Pt_Id_Call := Make_Function_Call (Loc, Get_Pt_Id);

      end if;

      --  Replace the attribute node by a conversion of the function call
      --  to the target type.

      Rewrite (N, Convert_To (Typ, Get_Pt_Id_Call));
      Analyze_And_Resolve (N, Typ);
   end Process_Partition_Id;

   ----------------------------------
   -- Process_Remote_AST_Attribute --
   ----------------------------------

   procedure Process_Remote_AST_Attribute
     (N        : Node_Id;
      New_Type : Entity_Id)
   is
      Loc                   : constant Source_Ptr := Sloc (N);
      Remote_Subp           : Entity_Id;
      Tick_Access_Conv_Call : Node_Id;
      Remote_Subp_Decl      : Node_Id;
      RS_Pkg_Specif         : Node_Id;
      RS_Pkg_E              : Entity_Id;
      RAS_Type              : Entity_Id := New_Type;
      Async_E               : Entity_Id;
      All_Calls_Remote_E    : Entity_Id;
      Attribute_Subp        : Entity_Id;

   begin
      --  Check if we have to expand the access attribute

      Remote_Subp := Entity (Prefix (N));

      if not Expander_Active or else Get_PCS_Name = Name_No_DSA then
         return;
      end if;

      if Ekind (RAS_Type) /= E_Record_Type then
         RAS_Type := Equivalent_Type (RAS_Type);
      end if;

      Attribute_Subp := TSS (RAS_Type, TSS_RAS_Access);
      pragma Assert (Present (Attribute_Subp));
      Remote_Subp_Decl := Unit_Declaration_Node (Remote_Subp);

      if Nkind (Remote_Subp_Decl) = N_Subprogram_Body then
         Remote_Subp := Corresponding_Spec (Remote_Subp_Decl);
         Remote_Subp_Decl := Unit_Declaration_Node (Remote_Subp);
      end if;

      RS_Pkg_Specif := Parent (Remote_Subp_Decl);
      RS_Pkg_E := Defining_Entity (RS_Pkg_Specif);

      Async_E :=
        Boolean_Literals (Ekind (Remote_Subp) = E_Procedure
                            and then Is_Asynchronous (Remote_Subp));

      All_Calls_Remote_E :=
        Boolean_Literals (Has_All_Calls_Remote (RS_Pkg_E));

      Tick_Access_Conv_Call :=
        Make_Function_Call (Loc,
          Name => New_Occurrence_Of (Attribute_Subp, Loc),
          Parameter_Associations =>
            New_List (
              Make_String_Literal (Loc, Full_Qualified_Name (RS_Pkg_E)),
              Build_Subprogram_Id (Loc, Remote_Subp),
              New_Occurrence_Of (Async_E, Loc),
              New_Occurrence_Of (All_Calls_Remote_E, Loc)));

      Rewrite (N, Tick_Access_Conv_Call);
      Analyze_And_Resolve (N, RAS_Type);
   end Process_Remote_AST_Attribute;

   ------------------------------------
   -- Process_Remote_AST_Declaration --
   ------------------------------------

   procedure Process_Remote_AST_Declaration (N : Node_Id) is
      Loc            : constant Source_Ptr := Sloc (N);
      User_Type      : constant Node_Id := Defining_Identifier (N);
      Scop           : constant Entity_Id := Scope (User_Type);
      Is_RCI         : constant Boolean :=
        Is_Remote_Call_Interface (Scop);
      Is_RT          : constant Boolean :=
        Is_Remote_Types (Scop);
      Type_Def       : constant Node_Id := Type_Definition (N);

      Parameter      : Node_Id;
      Is_Degenerate  : Boolean;
      --  True iff this RAS has an access formal parameter (see
      --  Exp_Dist.Add_RAS_Dereference_TSS for details).

      Subpkg         : constant Entity_Id :=
                         Make_Defining_Identifier
                           (Loc, New_Internal_Name ('S'));
      Subpkg_Decl    : Node_Id;
      Vis_Decls      : constant List_Id := New_List;
      Priv_Decls     : constant List_Id := New_List;

      Obj_Type       : constant Entity_Id :=
                         Make_Defining_Identifier
                           (Loc, New_External_Name (
                                   Chars (User_Type), 'R'));

      Full_Obj_Type  : constant Entity_Id :=
                         Make_Defining_Identifier
                           (Loc, Chars (Obj_Type));

      RACW_Type      : constant Entity_Id :=
                         Make_Defining_Identifier
                           (Loc, New_External_Name (
                                   Chars (User_Type), 'P'));

      Fat_Type       : constant Entity_Id :=
                        Make_Defining_Identifier
                          (Loc, Chars (User_Type));
      Fat_Type_Decl  : Node_Id;

   begin
      Is_Degenerate := False;
      Parameter := First (Parameter_Specifications (Type_Def));
      while Present (Parameter) loop
         if Nkind (Parameter_Type (Parameter)) = N_Access_Definition then
            Error_Msg_N ("formal parameter& has anonymous access type?",
              Defining_Identifier (Parameter));
            Is_Degenerate := True;
            exit;
         end if;

         Next (Parameter);
      end loop;

      if Is_Degenerate then
         Error_Msg_NE (
           "remote access-to-subprogram type& can only be null?",
           Defining_Identifier (Parameter), User_Type);
         --  The only legal value for a RAS with a formal parameter of an
         --  anonymous access type is null, because it cannot be
         --  subtype-Conformant with any legal remote subprogram declaration.
         --  In this case, we cannot generate a corresponding primitive
         --  operation.
      end if;

      if Get_PCS_Name = Name_No_DSA then
         return;
      end if;

      --  The tagged private type, primitive operation and RACW
      --  type associated with a RAS need to all be declared in
      --  a subpackage of the one that contains the RAS declaration,
      --  because the primitive of the object type, and the associated
      --  primitive of the stub type, need to be dispatching operations
      --  of these types, and the profile of the RAS might contain
      --  tagged types declared in the same scope.

      Append_To (Vis_Decls,
        Make_Private_Type_Declaration (Loc,
          Defining_Identifier => Obj_Type,
          Abstract_Present => True,
          Tagged_Present   => True,
          Limited_Present  => True));

      Append_To (Priv_Decls,
        Make_Full_Type_Declaration (Loc,
          Defining_Identifier =>
            Full_Obj_Type,
          Type_Definition     =>
            Make_Record_Definition (Loc,
              Abstract_Present => True,
              Tagged_Present   => True,
              Limited_Present  => True,
              Null_Present     => True,
              Component_List   => Empty)));

      if not Is_Degenerate then
         Append_To (Vis_Decls,
           Make_Abstract_Subprogram_Declaration (Loc,
             Specification => Build_RAS_Primitive_Specification (
               Subp_Spec          => Type_Def,
               Remote_Object_Type => Obj_Type)));
      end if;

      Append_To (Vis_Decls,
        Make_Full_Type_Declaration (Loc,
          Defining_Identifier => RACW_Type,
          Type_Definition     =>
            Make_Access_To_Object_Definition (Loc,
              All_Present => True,
              Subtype_Indication =>
                Make_Attribute_Reference (Loc,
                  Prefix =>
                    New_Occurrence_Of (Obj_Type, Loc),
                  Attribute_Name =>
                    Name_Class))));
      Set_Is_Remote_Call_Interface (RACW_Type, Is_RCI);
      Set_Is_Remote_Types (RACW_Type, Is_RT);

      Subpkg_Decl :=
        Make_Package_Declaration (Loc,
          Make_Package_Specification (Loc,
            Defining_Unit_Name =>
              Subpkg,
            Visible_Declarations =>
              Vis_Decls,
            Private_Declarations =>
              Priv_Decls,
            End_Label =>
              New_Occurrence_Of (Subpkg, Loc)));
      Set_Is_Remote_Call_Interface (Subpkg, Is_RCI);
      Set_Is_Remote_Types (Subpkg, Is_RT);
      Insert_After_And_Analyze (N, Subpkg_Decl);

      --  Many parts of the analyzer and expander expect
      --  that the fat pointer type used to implement remote
      --  access to subprogram types be a record.
      --  Note: The structure of this type must be kept consistent
      --  with the code generated by Remote_AST_Null_Value for the
      --  corresponding 'null' expression.

      Fat_Type_Decl := Make_Full_Type_Declaration (Loc,
        Defining_Identifier => Fat_Type,
        Type_Definition     =>
          Make_Record_Definition (Loc,
            Component_List =>
              Make_Component_List (Loc,
                Component_Items => New_List (
                  Make_Component_Declaration (Loc,
                    Defining_Identifier =>
                      Make_Defining_Identifier (Loc, Name_Ras),
                    Component_Definition =>
                      Make_Component_Definition (Loc,
                        Aliased_Present     =>
                          False,
                        Subtype_Indication  =>
                          New_Occurrence_Of (RACW_Type, Loc)))))));
      Set_Equivalent_Type (User_Type, Fat_Type);
      Set_Corresponding_Remote_Type (Fat_Type, User_Type);
      Insert_After_And_Analyze (Subpkg_Decl, Fat_Type_Decl);

      --  The reason we suppress the initialization procedure is that we know
      --  that no initialization is required (even if Initialize_Scalars mode
      --  is active), and there are order of elaboration problems if we do try
      --  to generate an init proc for this created record type.

      Set_Suppress_Init_Proc (Fat_Type);

      if Expander_Active then
         Add_RAST_Features (Parent (User_Type));
      end if;
   end Process_Remote_AST_Declaration;

   -----------------------
   -- RAS_E_Dereference --
   -----------------------

   procedure RAS_E_Dereference (Pref : Node_Id) is
      Loc             : constant Source_Ptr := Sloc (Pref);
      Call_Node       : Node_Id;
      New_Type        : constant Entity_Id := Etype (Pref);
      Explicit_Deref  : constant Node_Id   := Parent (Pref);
      Deref_Subp_Call : constant Node_Id   := Parent (Explicit_Deref);
      Deref_Proc      : Entity_Id;
      Params          : List_Id;

   begin
      if Nkind (Deref_Subp_Call) = N_Procedure_Call_Statement then
         Params := Parameter_Associations (Deref_Subp_Call);

         if Present (Params) then
            Prepend (Pref, Params);
         else
            Params := New_List (Pref);
         end if;

      elsif Nkind (Deref_Subp_Call) = N_Indexed_Component then

         Params := Expressions (Deref_Subp_Call);

         if Present (Params) then
            Prepend (Pref, Params);
         else
            Params := New_List (Pref);
         end if;

      else
         --  Context is not a call.

         return;
      end if;

      if not Expander_Active or else Get_PCS_Name = Name_No_DSA then
         return;
      end if;

      Deref_Proc := TSS (New_Type, TSS_RAS_Dereference);
      pragma Assert (Present (Deref_Proc));

      if Ekind (Deref_Proc) = E_Function then
         Call_Node :=
           Make_Function_Call (Loc,
              Name => New_Occurrence_Of (Deref_Proc, Loc),
              Parameter_Associations => Params);

      else
         Call_Node :=
           Make_Procedure_Call_Statement (Loc,
              Name => New_Occurrence_Of (Deref_Proc, Loc),
              Parameter_Associations => Params);
      end if;

      Rewrite (Deref_Subp_Call, Call_Node);
      Analyze (Deref_Subp_Call);
   end RAS_E_Dereference;

   ------------------------------
   -- Remote_AST_E_Dereference --
   ------------------------------

   function Remote_AST_E_Dereference (P : Node_Id) return Boolean is
      ET : constant Entity_Id  := Etype (P);

   begin
      --  Perform the changes only on original dereferences, and only if
      --  we are generating code.

      if Comes_From_Source (P)
        and then Is_Record_Type (ET)
        and then (Is_Remote_Call_Interface (ET)
                   or else Is_Remote_Types (ET))
        and then Present (Corresponding_Remote_Type (ET))
        and then (Nkind (Parent (Parent (P))) = N_Procedure_Call_Statement
                   or else Nkind (Parent (Parent (P))) = N_Indexed_Component)
        and then Expander_Active
      then
         RAS_E_Dereference (P);
         return True;
      else
         return False;
      end if;
   end Remote_AST_E_Dereference;

   ------------------------------
   -- Remote_AST_I_Dereference --
   ------------------------------

   function Remote_AST_I_Dereference (P : Node_Id) return Boolean is
      ET     : constant Entity_Id  := Etype (P);
      Deref  : Node_Id;

   begin
      if Comes_From_Source (P)
        and then (Is_Remote_Call_Interface (ET)
                   or else Is_Remote_Types (ET))
        and then Present (Corresponding_Remote_Type (ET))
        and then Ekind (Entity (P)) /= E_Function
      then
         Deref :=
           Make_Explicit_Dereference (Sloc (P),
             Prefix => Relocate_Node (P));
         Rewrite (P, Deref);
         Set_Etype (P, ET);
         RAS_E_Dereference (Prefix (P));
         return True;
      end if;

      return False;
   end Remote_AST_I_Dereference;

   ---------------------------
   -- Remote_AST_Null_Value --
   ---------------------------

   function Remote_AST_Null_Value
     (N   : Node_Id;
      Typ : Entity_Id) return Boolean
   is
      Loc         : constant Source_Ptr := Sloc (N);
      Target_Type : Entity_Id;

   begin
      if not Expander_Active or else Get_PCS_Name = Name_No_DSA then
         return False;

      elsif Ekind (Typ) = E_Access_Subprogram_Type
        and then (Is_Remote_Call_Interface (Typ)
                    or else Is_Remote_Types (Typ))
        and then Comes_From_Source (N)
        and then Expander_Active
      then
         --  Any null that comes from source and is of the RAS type must
         --  be expanded, except if expansion is not active (nothing
         --  gets expanded into the equivalent record type).

         Target_Type := Equivalent_Type (Typ);

      elsif Ekind (Typ) = E_Record_Type
        and then Present (Corresponding_Remote_Type (Typ))
      then
         --  This is a record type representing a RAS type, this must be
         --  expanded.

         Target_Type := Typ;

      else
         --  We do not have to handle this case

         return False;

      end if;

      Rewrite (N,
        Make_Aggregate (Loc,
          Component_Associations => New_List (
            Make_Component_Association (Loc,
              Choices => New_List (
                Make_Identifier (Loc, Name_Ras)),
              Expression =>
                Make_Null (Loc)))));
      Analyze_And_Resolve (N, Target_Type);
      return True;
   end Remote_AST_Null_Value;

end Sem_Dist;
