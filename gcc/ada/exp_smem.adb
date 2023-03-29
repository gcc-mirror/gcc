------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ S M E M                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1998-2023, Free Software Foundation, Inc.         --
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
with Einfo;          use Einfo;
with Einfo.Entities; use Einfo.Entities;
with Einfo.Utils;    use Einfo.Utils;
with Elists;         use Elists;
with Exp_Ch7;        use Exp_Ch7;
with Exp_Ch9;        use Exp_Ch9;
with Exp_Tss;        use Exp_Tss;
with Exp_Util;       use Exp_Util;
with Nmake;          use Nmake;
with Namet;          use Namet;
with Nlists;         use Nlists;
with Rtsfind;        use Rtsfind;
with Sem;            use Sem;
with Sem_Aux;        use Sem_Aux;
with Sem_Util;       use Sem_Util;
with Sinfo;          use Sinfo;
with Sinfo.Nodes;    use Sinfo.Nodes;
with Sinfo.Utils;    use Sinfo.Utils;
with Snames;         use Snames;
with Stand;          use Stand;
with Stringt;        use Stringt;
with Tbuild;         use Tbuild;

package body Exp_Smem is

   Insert_Node : Node_Id;
   --  Node after which a write call is to be inserted

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Add_Read (N : Node_Id; Call : Node_Id := Empty);
   --  Insert a Shared_Var_ROpen call for variable before node N, unless
   --  Call is a call to an init-proc, in which case the call is inserted
   --  after Call.

   procedure Add_Write_After (N : Node_Id);
   --  Insert a Shared_Var_WOpen call for variable after the node Insert_Node,
   --  as recorded by On_Lhs_Of_Assignment (where it points to the assignment
   --  statement) or Is_Out_Actual (where it points to the subprogram call).
   --  When Insert_Node is a function call, establish a transient scope around
   --  the expression, and insert the write as an after-action of the transient
   --  scope.

   procedure Build_Full_Name (E : Entity_Id; N : out String_Id);
   --  Build the fully qualified string name of a shared variable

   function On_Lhs_Of_Assignment (N : Node_Id) return Boolean;
   --  Determines if N is on the left hand of the assignment. This means that
   --  either it is a simple variable, or it is a record or array variable with
   --  a corresponding selected or indexed component on the left side of an
   --  assignment. If the result is True, then Insert_Node is set to point
   --  to the assignment

   function Is_Out_Actual (N : Node_Id) return Boolean;
   --  In a similar manner, this function determines if N appears as an OUT
   --  or IN OUT parameter to a procedure call. If the result is True, then
   --  Insert_Node is set to point to the call.

   function Build_Shared_Var_Proc_Call
     (Loc : Source_Ptr;
      E   : Entity_Id;
      N   : Name_Id) return Node_Id;
   --  Build a call to support procedure N for shared object E (provided by the
   --  instance of System.Shared_Storage.Shared_Var_Procs associated to E).

   --------------------------------
   -- Build_Shared_Var_Proc_Call --
   --------------------------------

   function Build_Shared_Var_Proc_Call
     (Loc : Source_Ptr;
      E   : Entity_Id;
      N   : Name_Id) return Node_Id
   is
   begin
      return Make_Procedure_Call_Statement (Loc,
        Name => Make_Selected_Component (Loc,
          Prefix        =>
            New_Occurrence_Of (Shared_Var_Procs_Instance (E), Loc),
          Selector_Name => Make_Identifier (Loc, N)));
   end Build_Shared_Var_Proc_Call;

   --------------
   -- Add_Read --
   --------------

   procedure Add_Read (N : Node_Id; Call : Node_Id := Empty) is
      Loc : constant Source_Ptr := Sloc (N);
      Ent : constant Node_Id    := Entity (N);
      SVC : Node_Id;

   begin
      if Present (Shared_Var_Procs_Instance (Ent)) then
         SVC := Build_Shared_Var_Proc_Call (Loc, Ent, Name_Read);

         if Present (Call) and then Is_Init_Proc (Name (Call)) then
            Insert_After_And_Analyze (Call, SVC);
         else
            Insert_Action (N, SVC);
         end if;
      end if;
   end Add_Read;

   -------------------------------
   -- Add_Shared_Var_Lock_Procs --
   -------------------------------

   procedure Add_Shared_Var_Lock_Procs (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);
      Obj : constant Entity_Id  := Entity (Expression (First_Actual (N)));
      Vnm : String_Id;
      Vid : Entity_Id;
      Vde : Node_Id;
      Aft : constant List_Id := New_List;

      In_Transient : constant Boolean := Scope_Is_Transient;

      function Build_Shared_Var_Lock_Call (RE : RE_Id) return Node_Id;
      --  Return a procedure call statement for lock proc RTE

      --------------------------------
      -- Build_Shared_Var_Lock_Call --
      --------------------------------

      function Build_Shared_Var_Lock_Call (RE : RE_Id) return Node_Id is
      begin
         return
           Make_Procedure_Call_Statement (Loc,
             Name                   =>
               New_Occurrence_Of (RTE (RE), Loc),
             Parameter_Associations =>
               New_List (New_Occurrence_Of (Vid, Loc)));
      end Build_Shared_Var_Lock_Call;

   --  Start of processing for Add_Shared_Var_Lock_Procs

   begin
      --  Discussion of transient scopes: we need to have a transient scope
      --  to hold the required lock/unlock actions. Either the current scope
      --  is transient, in which case we reuse it, or we establish a new
      --  transient scope. If this is a function call with unconstrained
      --  return type, we can't introduce a transient scope here (because
      --  Wrap_Transient_Expression would need to declare a temporary with
      --  the unconstrained type outside of the transient block), but in that
      --  case we know that we have already established one at an outer level
      --  for secondary stack management purposes.

      --  If the lock/read/write/unlock actions for this object have already
      --  been emitted in the current scope, no need to perform them anew.

      if In_Transient
        and then Contains (Scope_Stack.Table (Scope_Stack.Last)
                             .Locked_Shared_Objects,
                           Obj)
      then
         return;
      end if;

      Build_Full_Name (Obj, Vnm);

      --  Declare a constant string to hold the name of the shared object.
      --  Note that this must occur outside of the transient scope, as the
      --  scope's finalizer needs to have access to this object. Also, it
      --  appears that GIGI does not support elaborating string literal
      --  subtypes in transient scopes.

      Vid := Make_Temporary (Loc, 'N', Obj);
      Vde :=
        Make_Object_Declaration (Loc,
          Defining_Identifier => Vid,
          Constant_Present    => True,
          Object_Definition   => New_Occurrence_Of (Standard_String, Loc),
          Expression          => Make_String_Literal (Loc, Vnm));

      --  Already in a transient scope. Make sure that we insert Vde outside
      --  that scope.

      if In_Transient then
         Insert_Before_And_Analyze (Node_To_Be_Wrapped, Vde);

      --  Not in a transient scope yet: insert Vde as an action on N prior to
      --  establishing one.

      else
         Insert_Action (N, Vde);
         Establish_Transient_Scope (N, Manage_Sec_Stack => False);
      end if;

      --  Mark object as locked in the current (transient) scope

      Append_New_Elmt
        (Obj,
         To => Scope_Stack.Table (Scope_Stack.Last).Locked_Shared_Objects);

      --  First insert the Lock call before

      Insert_Action (N, Build_Shared_Var_Lock_Call (RE_Shared_Var_Lock));

      --  Now, right after the Lock, insert a call to read the object

      Insert_Action (N, Build_Shared_Var_Proc_Call (Loc, Obj, Name_Read));

      --  For a procedure call only, insert the call to write the object prior
      --  to unlocking.

      if Nkind (N) = N_Procedure_Call_Statement then
         Append_To (Aft, Build_Shared_Var_Proc_Call (Loc, Obj, Name_Write));
      end if;

      --  Finally insert the Unlock call

      Append_To (Aft, Build_Shared_Var_Lock_Call (RE_Shared_Var_Unlock));

      --  Store cleanup actions in transient scope

      Store_Cleanup_Actions_In_Scope (Aft);

      --  If we have established a transient scope here, wrap it now

      if not In_Transient then
         if Nkind (N) = N_Procedure_Call_Statement then
            Wrap_Transient_Statement (N);
         else
            Wrap_Transient_Expression (N);
         end if;
      end if;
   end Add_Shared_Var_Lock_Procs;

   ---------------------
   -- Add_Write_After --
   ---------------------

   procedure Add_Write_After (N : Node_Id) is
      Ent : constant Entity_Id  := Entity (N);
      Loc : constant Source_Ptr := Sloc (N);
      Par : constant Node_Id    := Insert_Node;

   begin
      if Present (Shared_Var_Procs_Instance (Ent)) then
         if Nkind (Insert_Node) = N_Function_Call then
            Establish_Transient_Scope (Insert_Node, Manage_Sec_Stack => False);

            Store_After_Actions_In_Scope (New_List (
              Build_Shared_Var_Proc_Call (Loc, Ent, Name_Write)));
         else
            Insert_After_And_Analyze (Par,
              Build_Shared_Var_Proc_Call (Loc, Ent, Name_Write));
         end if;
      end if;
   end Add_Write_After;

   ---------------------
   -- Build_Full_Name --
   ---------------------

   procedure Build_Full_Name (E : Entity_Id; N : out String_Id) is

      procedure Build_Name (E : Entity_Id);
      --  This is a recursive routine used to construct the fully qualified
      --  string name of the package corresponding to the shared variable.

      ----------------
      -- Build_Name --
      ----------------

      procedure Build_Name (E : Entity_Id) is
      begin
         if Scope (E) /= Standard_Standard then
            Build_Name (Scope (E));
            Store_String_Char ('.');
         end if;

         Get_Decoded_Name_String (Chars (E));
         Store_String_Chars (Name_Buffer (1 .. Name_Len));
      end Build_Name;

   --  Start of processing for Build_Full_Name

   begin
      Start_String;
      Build_Name (E);
      N := End_String;
   end Build_Full_Name;

   ------------------------------------
   -- Expand_Shared_Passive_Variable --
   ------------------------------------

   procedure Expand_Shared_Passive_Variable (N : Node_Id) is
      Typ : constant Entity_Id := Etype (N);

   begin
      --  Nothing to do for protected or limited objects

      if Is_Limited_Type (Typ) or else Is_Concurrent_Type (Typ) then
         return;

      --  If we are on the left hand side of an assignment, then we add the
      --  write call after the assignment.

      elsif On_Lhs_Of_Assignment (N) then
         Add_Write_After (N);

      --  If we are a parameter for an out or in out formal, then in general
      --  we do:

      --    read
      --    call
      --    write

      --  but in the special case of a call to an init proc, we need to first
      --  call the init proc (to set discriminants), then read (to possibly
      --  set other components), then write (to record the updated components
      --  to the backing store):

      --    init-proc-call
      --    read
      --    write

      elsif Is_Out_Actual (N) then

         --  Note: For an init proc call, Add_Read inserts just after the
         --  call node, and we want to have first the read, then the write,
         --  so we need to first Add_Write_After, then Add_Read.

         Add_Write_After (N);
         Add_Read (N, Call => Insert_Node);

      --  All other cases are simple reads

      else
         Add_Read (N);
      end if;
   end Expand_Shared_Passive_Variable;

   -------------------
   -- Is_Out_Actual --
   -------------------

   function Is_Out_Actual (N : Node_Id) return Boolean is
      Formal : Entity_Id;
      Call   : Node_Id;

   begin
      Find_Actual (N, Formal, Call);

      if No (Formal) then
         return False;

      else
         if Ekind (Formal) in E_Out_Parameter | E_In_Out_Parameter then
            Insert_Node := Call;
            return True;
         else
            return False;
         end if;
      end if;
   end Is_Out_Actual;

   ---------------------------
   -- Make_Shared_Var_Procs --
   ---------------------------

   function Make_Shared_Var_Procs (N : Node_Id) return Node_Id is
      Loc     : constant Source_Ptr := Sloc (N);
      Ent     : constant Entity_Id  := Defining_Identifier (N);
      Typ     : constant Entity_Id  := Etype (Ent);
      Vnm     : String_Id;
      Obj     : Node_Id;
      Obj_Typ : Entity_Id;

      After : constant Node_Id := Next (N);
      --  Node located right after N originally (after insertion of the SV
      --  procs this node is right after the last inserted node).

      SVP_Instance : constant Entity_Id := Make_Defining_Identifier (Loc,
                       Chars => New_External_Name (Chars (Ent), 'G'));
      --  Instance of Shared_Storage.Shared_Var_Procs associated with Ent

      Instantiation : Node_Id;
      --  Package instantiation node for SVP_Instance

   --  Start of processing for Make_Shared_Var_Procs

   begin
      Build_Full_Name (Ent, Vnm);

      --  We turn off Shared_Passive during construction and analysis of the
      --  generic package instantiation, to avoid improper attempts to process
      --  the variable references within these instantiation.

      Set_Is_Shared_Passive (Ent, False);

      --  Construct generic package instantiation

      --  package varG is new Shared_Var_Procs (typ, var, "pkg.var");

      Obj     := New_Occurrence_Of (Ent, Loc);
      Obj_Typ := Typ;
      if Is_Concurrent_Type (Typ) then
         Obj     := Convert_Concurrent (N => Obj, Typ => Typ);
         Obj_Typ := Corresponding_Record_Type (Typ);
      end if;

      Instantiation :=
        Make_Package_Instantiation (Loc,
          Defining_Unit_Name   => SVP_Instance,
          Name                 =>
            New_Occurrence_Of (RTE (RE_Shared_Var_Procs), Loc),
          Generic_Associations => New_List (
            Make_Generic_Association (Loc,
              Explicit_Generic_Actual_Parameter =>
                New_Occurrence_Of (Obj_Typ, Loc)),
            Make_Generic_Association (Loc,
              Explicit_Generic_Actual_Parameter => Obj),
            Make_Generic_Association (Loc,
              Explicit_Generic_Actual_Parameter =>
                Make_String_Literal (Loc, Vnm))));

      Insert_After_And_Analyze (N, Instantiation);

      Set_Is_Shared_Passive (Ent, True);
      Set_Shared_Var_Procs_Instance
        (Ent, Defining_Entity (Instance_Spec (Instantiation)));

      --  Return last node before After

      declare
         Nod : Node_Id := Next (N);

      begin
         while Next (Nod) /= After loop
            Next (Nod);
         end loop;

         return Nod;
      end;
   end Make_Shared_Var_Procs;

   --------------------------
   -- On_Lhs_Of_Assignment --
   --------------------------

   function On_Lhs_Of_Assignment (N : Node_Id) return Boolean is
      P : constant Node_Id := Parent (N);

   begin
      if Nkind (P) = N_Assignment_Statement then
         if N = Name (P) then
            Insert_Node := P;
            return True;
         else
            return False;
         end if;

      elsif Nkind (P) in N_Indexed_Component | N_Selected_Component
        and then N = Prefix (P)
      then
         return On_Lhs_Of_Assignment (P);

      else
         return False;
      end if;
   end On_Lhs_Of_Assignment;

end Exp_Smem;
