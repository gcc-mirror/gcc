------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ D I S T                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                                                                          --
--          Copyright (C) 1992-2001, Free Software Foundation, Inc.         --
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
with Uname;    use Uname;

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

   -----------------------
   -- Get_Subprogram_Id --
   -----------------------

   function Get_Subprogram_Id (E : Entity_Id) return Int is
      Current_Declaration : Node_Id;
      Result              : Int := 0;

   begin
      pragma Assert
        (Is_Remote_Call_Interface (Scope (E))
           and then
             (Nkind (Parent (E)) = N_Procedure_Specification
                or else
              Nkind (Parent (E)) = N_Function_Specification));

      Current_Declaration :=
        First (Visible_Declarations
          (Package_Specification_Of_Scope (Scope (E))));

      while Current_Declaration /= Empty loop
         if Nkind (Current_Declaration) = N_Subprogram_Declaration
           and then Comes_From_Source (Current_Declaration)
         then
            if Defining_Unit_Name
                 (Specification (Current_Declaration)) = E
            then
               return Result;
            end if;

            Result := Result + 1;
         end if;

         Next (Current_Declaration);
      end loop;

      --  Error if we do not find it

      raise Program_Error;
   end Get_Subprogram_Id;

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
      Nd             : Node_Id;
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

      Nd := Enclosing_Lib_Unit_Node (N);

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
      --  library unit whose Partition_Id is needed

      Get_Unit_Name_String (Get_Unit_Name (Unit_Declaration_Node (Ety)));

      --  Remove seven last character ("(spec)" or " (body)").
      --  (this is a bit nasty, should have interface for this ???)

      Name_Len := Name_Len - 7;

      Start_String;
      Store_String_Chars (Name_Buffer (1 .. Name_Len));
      Prefix_String := End_String;

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
      RAS_Decl              : Node_Id;
      RS_Pkg_Specif         : Node_Id;
      RS_Pkg_E              : Entity_Id;
      RAS_Pkg_E             : Entity_Id;
      RAS_Type              : Entity_Id;
      RAS_Name              : Name_Id;
      Async_E               : Entity_Id;
      Subp_Id               : Int;
      Attribute_Subp        : Entity_Id;
      Parameter             : Node_Id;

   begin
      --  Check if we have to expand the access attribute

      Remote_Subp := Entity (Prefix (N));

      if not Expander_Active then
         return;

      elsif Ekind (New_Type) = E_Record_Type then
         RAS_Type := New_Type;

      else
         --  If the remote type has not been constructed yet, create
         --  it and its attributes now.

         Attribute_Subp := TSS (New_Type, Name_uRAS_Access);

         if No (Attribute_Subp) then
            Add_RAST_Features (Parent (New_Type));
         end if;

         RAS_Type := Equivalent_Type (New_Type);
      end if;

      RAS_Name  := Chars (RAS_Type);
      RAS_Decl := Parent (RAS_Type);
      Attribute_Subp := TSS (RAS_Type, Name_uRAS_Access);

      RAS_Pkg_E  := Defining_Entity (Parent (RAS_Decl));
      Remote_Subp_Decl := Unit_Declaration_Node (Remote_Subp);

      if Nkind (Remote_Subp_Decl) = N_Subprogram_Body then
         Remote_Subp := Corresponding_Spec (Remote_Subp_Decl);
         Remote_Subp_Decl := Unit_Declaration_Node (Remote_Subp);
      end if;

      RS_Pkg_Specif := Parent (Remote_Subp_Decl);
      RS_Pkg_E := Defining_Entity (RS_Pkg_Specif);

      Subp_Id := Get_Subprogram_Id (Remote_Subp);

      if Ekind (Remote_Subp) = E_Procedure
        and then Is_Asynchronous (Remote_Subp)
      then
         Async_E := Standard_True;
      else
         Async_E := Standard_False;
      end if;

      --  Right now, we do not call the Name_uAddress_Resolver subprogram,
      --  which means that we end up with a Null_Address value in the ras
      --  field: each dereference of an RAS will go through the PCS, which
      --  is authorized but potentially not very efficient ???

      Parameter := New_Occurrence_Of (RTE (RE_Null_Address), Loc);

      Tick_Access_Conv_Call :=
        Make_Function_Call (Loc,
          Name => New_Occurrence_Of (Attribute_Subp, Loc),
          Parameter_Associations =>
            New_List (
              Parameter,
              Make_String_Literal (Loc, Full_Qualified_Name (RS_Pkg_E)),
              Make_Integer_Literal (Loc, Subp_Id),
              New_Occurrence_Of (Async_E, Loc)));

      Rewrite (N, Tick_Access_Conv_Call);
      Analyze_And_Resolve (N, RAS_Type);

   end Process_Remote_AST_Attribute;

   ------------------------------------
   -- Process_Remote_AST_Declaration --
   ------------------------------------

   procedure Process_Remote_AST_Declaration (N : Node_Id) is
      Loc           : constant Source_Ptr := Sloc (N);
      User_Type     : constant Node_Id := Defining_Identifier (N);
      Fat_Type      : constant Entity_Id :=
                        Make_Defining_Identifier
                          (Loc, Chars (User_Type));
      New_Type_Decl : Node_Id;

   begin
      --  We add a record type declaration for the equivalent fat pointer type

      New_Type_Decl :=
        Make_Full_Type_Declaration (Loc,
          Defining_Identifier => Fat_Type,
          Type_Definition =>
            Make_Record_Definition (Loc,
              Component_List =>
                Make_Component_List (Loc,
                  Component_Items => New_List (

                    Make_Component_Declaration (Loc,
                      Defining_Identifier =>
                        Make_Defining_Identifier (Loc,
                          Chars => Name_Ras),
                      Subtype_Indication =>
                        New_Occurrence_Of
                          (RTE (RE_Unsigned_64), Loc)),

                    Make_Component_Declaration (Loc,
                      Defining_Identifier =>
                        Make_Defining_Identifier (Loc,
                          Chars => Name_Origin),
                      Subtype_Indication =>
                        New_Reference_To
                          (Standard_Integer,
                           Loc)),

                    Make_Component_Declaration (Loc,
                      Defining_Identifier =>
                        Make_Defining_Identifier (Loc,
                          Chars => Name_Receiver),
                      Subtype_Indication =>
                        New_Reference_To
                          (RTE (RE_Unsigned_64), Loc)),

                    Make_Component_Declaration (Loc,
                      Defining_Identifier =>
                        Make_Defining_Identifier (Loc,
                          Chars => Name_Subp_Id),
                      Subtype_Indication =>
                        New_Reference_To
                          (Standard_Natural,
                           Loc)),

                    Make_Component_Declaration (Loc,
                      Defining_Identifier =>
                        Make_Defining_Identifier (Loc,
                          Chars => Name_Async),
                      Subtype_Indication =>
                        New_Reference_To
                          (Standard_Boolean,
                           Loc))))));

      Insert_After (N, New_Type_Decl);
      Set_Equivalent_Type (User_Type, Fat_Type);
      Set_Corresponding_Remote_Type (Fat_Type, User_Type);

      --  The reason we suppress the initialization procedure is that we know
      --  that no initialization is required (even if Initialize_Scalars mode
      --  is active), and there are order of elaboration problems if we do try
      --  to generate an Init_Proc for this created record type.

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
      RAS             : constant Entity_Id :=
                          Corresponding_Remote_Type (New_Type);
      RAS_Decl        : constant Node_Id   := Parent (RAS);
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

      Deref_Proc := TSS (New_Type, Name_uRAS_Dereference);

      if not Expander_Active then
         return;

      elsif No (Deref_Proc) then
         Add_RAST_Features (RAS_Decl);
         Deref_Proc := TSS (New_Type, Name_uRAS_Dereference);
      end if;

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

   function Remote_AST_E_Dereference (P : Node_Id) return Boolean
   is
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

   function Remote_AST_I_Dereference (P : Node_Id) return Boolean
   is
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
     (N    : Node_Id;
      Typ  : Entity_Id)
      return Boolean
   is
      Loc         : constant Source_Ptr := Sloc (N);
      Target_Type : Entity_Id;

   begin
      if not Expander_Active then
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
          Expressions => New_List (
            Make_Integer_Literal (Loc, 0),                  -- Ras
            Make_Integer_Literal (Loc, 0),                  -- Origin
            Make_Integer_Literal (Loc, 0),                  -- Receiver
            Make_Integer_Literal (Loc, 0),                  -- Subp_Id
            New_Occurrence_Of (Standard_False, Loc))));     -- Asyn
      Analyze_And_Resolve (N, Target_Type);
      return True;
   end Remote_AST_Null_Value;

end Sem_Dist;
