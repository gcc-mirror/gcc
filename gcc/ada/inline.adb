------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               I N L I N E                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2023, Free Software Foundation, Inc.         --
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
with Aspects;        use Aspects;
with Atree;          use Atree;
with Debug;          use Debug;
with Einfo;          use Einfo;
with Einfo.Entities; use Einfo.Entities;
with Einfo.Utils;    use Einfo.Utils;
with Elists;         use Elists;
with Errout;         use Errout;
with Exp_Ch6;        use Exp_Ch6;
with Exp_Ch7;        use Exp_Ch7;
with Exp_Tss;        use Exp_Tss;
with Exp_Util;       use Exp_Util;
with Fname;          use Fname;
with Fname.UF;       use Fname.UF;
with Lib;            use Lib;
with Namet;          use Namet;
with Nmake;          use Nmake;
with Nlists;         use Nlists;
with Output;         use Output;
with Sem_Aux;        use Sem_Aux;
with Sem_Ch8;        use Sem_Ch8;
with Sem_Ch10;       use Sem_Ch10;
with Sem_Ch12;       use Sem_Ch12;
with Sem_Prag;       use Sem_Prag;
with Sem_Res;        use Sem_Res;
with Sem_Util;       use Sem_Util;
with Sinfo;          use Sinfo;
with Sinfo.Nodes;    use Sinfo.Nodes;
with Sinfo.Utils;    use Sinfo.Utils;
with Sinput;         use Sinput;
with Snames;         use Snames;
with Stand;          use Stand;
with Table;
with Tbuild;         use Tbuild;
with Uintp;          use Uintp;
with Uname;          use Uname;

with GNAT.HTable;

package body Inline is

   Check_Inlining_Restrictions : constant Boolean := True;
   --  In the following cases the frontend rejects inlining because they
   --  are not handled well by the backend. This variable facilitates
   --  disabling these restrictions to evaluate future versions of the
   --  GCC backend in which some of the restrictions may be supported.
   --
   --   - subprograms that have:
   --      - nested subprograms
   --      - instantiations
   --      - package declarations
   --      - task or protected object declarations
   --      - some of the following statements:
   --          - abort
   --          - asynchronous-select
   --          - conditional-entry-call
   --          - delay-relative
   --          - delay-until
   --          - selective-accept
   --          - timed-entry-call

   Inlined_Calls : Elist_Id;
   --  List of frontend inlined calls

   Backend_Calls : Elist_Id;
   --  List of inline calls passed to the backend

   Backend_Instances : Elist_Id;
   --  List of instances inlined for the backend

   Backend_Inlined_Subps : Elist_Id;
   --  List of subprograms inlined by the backend

   Backend_Not_Inlined_Subps : Elist_Id;
   --  List of subprograms that cannot be inlined by the backend

   -----------------------------
   --  Pending_Instantiations --
   -----------------------------

   --  We make entries in this table for the pending instantiations of generic
   --  bodies that are created during semantic analysis. After the analysis is
   --  complete, calling Instantiate_Bodies performs the actual instantiations.

   package Pending_Instantiations is new Table.Table (
     Table_Component_Type => Pending_Body_Info,
     Table_Index_Type     => Int,
     Table_Low_Bound      => 0,
     Table_Initial        => Alloc.Pending_Instantiations_Initial,
     Table_Increment      => Alloc.Pending_Instantiations_Increment,
     Table_Name           => "Pending_Instantiations");

   -------------------------------------
   --  Called_Pending_Instantiations  --
   -------------------------------------

   --  With back-end inlining, the pending instantiations that are not in the
   --  main unit or subunit are performed only after a call to the subprogram
   --  instance, or to a subprogram within the package instance, is inlined.
   --  Since such a call can be within a subsequent pending instantiation,
   --  we make entries in this table that stores the index of these "called"
   --  pending instantiations and perform them when the table is populated.

   package Called_Pending_Instantiations is new Table.Table (
     Table_Component_Type => Int,
     Table_Index_Type     => Int,
     Table_Low_Bound      => 0,
     Table_Initial        => Alloc.Pending_Instantiations_Initial,
     Table_Increment      => Alloc.Pending_Instantiations_Increment,
     Table_Name           => "Called_Pending_Instantiations");

   ---------------------------------
   --  To_Pending_Instantiations  --
   ---------------------------------

   --  With back-end inlining, we also need to have a map from the pending
   --  instantiations to their index in the Pending_Instantiations table.

   Node_Table_Size : constant := 257;
   --  Number of headers in hash table

   subtype Node_Header_Num is Integer range 0 .. Node_Table_Size - 1;
   --  Range of headers in hash table

   function Node_Hash (Id : Node_Id) return Node_Header_Num;
   --  Simple hash function for Node_Ids

   package To_Pending_Instantiations is new GNAT.Htable.Simple_HTable
     (Header_Num => Node_Header_Num,
      Element    => Int,
      No_Element => -1,
      Key        => Node_Id,
      Hash       => Node_Hash,
      Equal      => "=");

   -----------------
   -- Node_Hash --
   -----------------

   function Node_Hash (Id : Node_Id) return Node_Header_Num is
   begin
      return Node_Header_Num (Id mod Node_Table_Size);
   end Node_Hash;

   --------------------
   -- Inlined Bodies --
   --------------------

   --  Inlined functions are actually placed in line by the backend if the
   --  corresponding bodies are available (i.e. compiled). Whenever we find
   --  a call to an inlined subprogram, we add the name of the enclosing
   --  compilation unit to a worklist. After all compilation, and after
   --  expansion of generic bodies, we traverse the list of pending bodies
   --  and compile them as well.

   package Inlined_Bodies is new Table.Table (
     Table_Component_Type => Entity_Id,
     Table_Index_Type     => Int,
     Table_Low_Bound      => 0,
     Table_Initial        => Alloc.Inlined_Bodies_Initial,
     Table_Increment      => Alloc.Inlined_Bodies_Increment,
     Table_Name           => "Inlined_Bodies");

   -----------------------
   -- Inline Processing --
   -----------------------

   --  For each call to an inlined subprogram, we make entries in a table
   --  that stores caller and callee, and indicates the call direction from
   --  one to the other. We also record the compilation unit that contains
   --  the callee. After analyzing the bodies of all such compilation units,
   --  we compute the transitive closure of inlined subprograms called from
   --  the main compilation unit and make it available to the code generator
   --  in no particular order, thus allowing cycles in the call graph.

   Last_Inlined : Entity_Id := Empty;

   --  For each entry in the table we keep a list of successors in topological
   --  order, i.e. callers of the current subprogram.

   type Subp_Index is new Nat;
   No_Subp : constant Subp_Index := 0;

   --  The subprogram entities are hashed into the Inlined table

   Num_Hash_Headers : constant := 512;

   Hash_Headers : array (Subp_Index range 0 .. Num_Hash_Headers - 1)
                                                          of Subp_Index;

   type Succ_Index is new Nat;
   No_Succ : constant Succ_Index := 0;

   type Succ_Info is record
      Subp : Subp_Index;
      Next : Succ_Index;
   end record;

   --  The following table stores list elements for the successor lists. These
   --  lists cannot be chained directly through entries in the Inlined table,
   --  because a given subprogram can appear in several such lists.

   package Successors is new Table.Table (
      Table_Component_Type => Succ_Info,
      Table_Index_Type     => Succ_Index,
      Table_Low_Bound      => 1,
      Table_Initial        => Alloc.Successors_Initial,
      Table_Increment      => Alloc.Successors_Increment,
      Table_Name           => "Successors");

   type Subp_Info is record
      Name        : Entity_Id  := Empty;
      Next        : Subp_Index := No_Subp;
      First_Succ  : Succ_Index := No_Succ;
      Main_Call   : Boolean    := False;
      Processed   : Boolean    := False;
   end record;

   package Inlined is new Table.Table (
      Table_Component_Type => Subp_Info,
      Table_Index_Type     => Subp_Index,
      Table_Low_Bound      => 1,
      Table_Initial        => Alloc.Inlined_Initial,
      Table_Increment      => Alloc.Inlined_Increment,
      Table_Name           => "Inlined");

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Add_Call (Called : Entity_Id; Caller : Entity_Id := Empty);
   --  Make two entries in Inlined table, for an inlined subprogram being
   --  called, and for the inlined subprogram that contains the call. If
   --  the call is in the main compilation unit, Caller is Empty.

   procedure Add_Inlined_Instance (E : Entity_Id);
   --  Add instance E to the list of inlined instances for the unit

   procedure Add_Inlined_Subprogram (E : Entity_Id);
   --  Add subprogram E to the list of inlined subprograms for the unit

   function Add_Subp (E : Entity_Id) return Subp_Index;
   --  Make entry in Inlined table for subprogram E, or return table index
   --  that already holds E.

   procedure Establish_Actual_Mapping_For_Inlined_Call
     (N                     : Node_Id;
      Subp                  : Entity_Id;
      Decls                 : List_Id;
      Body_Or_Expr_To_Check : Node_Id);
   --  Establish a mapping from formals to actuals in the call N for the target
   --  subprogram Subp, and create temporaries or renamings when needed for the
   --  actuals that are expressions (except for actuals given by simple entity
   --  names or literals) or that are scalars that require copying to preserve
   --  semantics. Any temporary objects that are created are inserted in Decls.
   --  Body_Or_Expr_To_Check indicates the target body (or possibly expression
   --  of an expression function), which may be traversed to count formal uses.

   function Get_Code_Unit_Entity (E : Entity_Id) return Entity_Id;
   pragma Inline (Get_Code_Unit_Entity);
   --  Return the entity node for the unit containing E. Always return the spec
   --  for a package.

   function Has_Initialized_Type (E : Entity_Id) return Boolean;
   --  If a candidate for inlining contains type declarations for types with
   --  nontrivial initialization procedures, they are not worth inlining.

   function Has_Single_Return (N : Node_Id) return Boolean;
   --  In general we cannot inline functions that return unconstrained type.
   --  However, we can handle such functions if all return statements return
   --  a local variable that is the first declaration in the body of the
   --  function. In that case the call can be replaced by that local
   --  variable as is done for other inlined calls.

   function In_Main_Unit_Or_Subunit (E : Entity_Id) return Boolean;
   --  Return True if E is in the main unit or its spec or in a subunit

   function Is_Nested (E : Entity_Id) return Boolean;
   --  If the function is nested inside some other function, it will always
   --  be compiled if that function is, so don't add it to the inline list.
   --  We cannot compile a nested function outside the scope of the containing
   --  function anyway. This is also the case if the function is defined in a
   --  task body or within an entry (for example, an initialization procedure).

   procedure Remove_Aspects_And_Pragmas (Body_Decl : Node_Id);
   --  Remove all aspects and/or pragmas that have no meaning in inlined body
   --  Body_Decl. The analysis of these items is performed on the non-inlined
   --  body. The items currently removed are:
   --    Always_Terminates
   --    Contract_Cases
   --    Global
   --    Depends
   --    Exceptional_Cases
   --    Postcondition
   --    Precondition
   --    Refined_Global
   --    Refined_Depends
   --    Refined_Post
   --    Subprogram_Variant
   --    Test_Case
   --    Unmodified
   --    Unreferenced

   procedure Reset_Actual_Mapping_For_Inlined_Call (Subp : Entity_Id);
   --  Reset the Renamed_Object field to Empty on all formals of Subp, which
   --  can be set by a call to Establish_Actual_Mapping_For_Inlined_Call.

   ------------------------------
   -- Deferred Cleanup Actions --
   ------------------------------

   --  The cleanup actions for scopes that contain package instantiations with
   --  a body are delayed until after the package body is instantiated. because
   --  the body may contain finalizable objects or other constructs that affect
   --  the cleanup code. A scope that contains such instantiations only needs
   --  to be finalized once, even though it may contain more than one instance.
   --  We keep a list of scopes that must still be finalized and Cleanup_Scopes
   --  will be invoked after all the body instantiations have been completed.

   To_Clean : Elist_Id;

   procedure Add_Scope_To_Clean (Scop : Entity_Id);
   --  Build set of scopes on which cleanup actions must be performed

   procedure Cleanup_Scopes;
   --  Complete cleanup actions on scopes that need it

   --------------
   -- Add_Call --
   --------------

   procedure Add_Call (Called : Entity_Id; Caller : Entity_Id := Empty) is
      P1 : constant Subp_Index := Add_Subp (Called);
      P2 : Subp_Index;
      J  : Succ_Index;

   begin
      if Present (Caller) then
         P2 := Add_Subp (Caller);

         --  Add P1 to the list of successors of P2, if not already there.
         --  Note that P2 may contain more than one call to P1, and only
         --  one needs to be recorded.

         J := Inlined.Table (P2).First_Succ;
         while J /= No_Succ loop
            if Successors.Table (J).Subp = P1 then
               return;
            end if;

            J := Successors.Table (J).Next;
         end loop;

         --  On exit, make a successor entry for P1

         Successors.Increment_Last;
         Successors.Table (Successors.Last).Subp := P1;
         Successors.Table (Successors.Last).Next :=
                             Inlined.Table (P2).First_Succ;
         Inlined.Table (P2).First_Succ := Successors.Last;
      else
         Inlined.Table (P1).Main_Call := True;
      end if;
   end Add_Call;

   ----------------------
   -- Add_Inlined_Body --
   ----------------------

   procedure Add_Inlined_Body (E : Entity_Id; N : Node_Id) is

      type Inline_Level_Type is (Dont_Inline, Inline_Call, Inline_Package);
      --  Level of inlining for the call: Dont_Inline means no inlining,
      --  Inline_Call means that only the call is considered for inlining,
      --  Inline_Package means that the call is considered for inlining and
      --  its package compiled and scanned for more inlining opportunities.

      function Is_Non_Loading_Expression_Function
        (Id : Entity_Id) return Boolean;
      --  Determine whether arbitrary entity Id denotes a subprogram which is
      --  either
      --
      --    * An expression function
      --
      --    * A function completed by an expression function where both the
      --      spec and body are in the same context.

      function Must_Inline return Inline_Level_Type;
      --  Inlining is only done if the call statement N is in the main unit,
      --  or within the body of another inlined subprogram.

      ----------------------------------------
      -- Is_Non_Loading_Expression_Function --
      ----------------------------------------

      function Is_Non_Loading_Expression_Function
        (Id : Entity_Id) return Boolean
      is
         Body_Decl : Node_Id;
         Body_Id   : Entity_Id;
         Spec_Decl : Node_Id;

      begin
         --  A stand-alone expression function is transformed into a spec-body
         --  pair in-place. Since both the spec and body are in the same list,
         --  the inlining of such an expression function does not need to load
         --  anything extra.

         if Is_Expression_Function (Id) then
            return True;

         --  A function may be completed by an expression function

         elsif Ekind (Id) = E_Function then
            Spec_Decl := Unit_Declaration_Node (Id);

            if Nkind (Spec_Decl) = N_Subprogram_Declaration then
               Body_Id := Corresponding_Body (Spec_Decl);

               if Present (Body_Id) then
                  Body_Decl := Unit_Declaration_Node (Body_Id);

                  --  The inlining of a completing expression function does
                  --  not need to load anything extra when both the spec and
                  --  body are in the same context.

                  return
                    Was_Expression_Function (Body_Decl)
                      and then Parent (Spec_Decl) = Parent (Body_Decl);
               end if;
            end if;
         end if;

         return False;
      end Is_Non_Loading_Expression_Function;

      -----------------
      -- Must_Inline --
      -----------------

      function Must_Inline return Inline_Level_Type is
         Scop : Entity_Id;
         Comp : Node_Id;

      begin
         --  Check if call is in main unit

         Scop := Current_Scope;

         --  Do not try to inline if scope is standard. This could happen, for
         --  example, for a call to Add_Global_Declaration, and it causes
         --  trouble to try to inline at this level.

         if Scop = Standard_Standard then
            return Dont_Inline;
         end if;

         --  Otherwise lookup scope stack to outer scope

         while Scope (Scop) /= Standard_Standard
           and then not Is_Child_Unit (Scop)
         loop
            Scop := Scope (Scop);
         end loop;

         Comp := Parent (Scop);
         while Nkind (Comp) /= N_Compilation_Unit loop
            Comp := Parent (Comp);
         end loop;

         --  If the call is in the main unit, inline the call and compile the
         --  package of the subprogram to find more calls to be inlined.

         if Comp = Cunit (Main_Unit)
           or else Comp = Library_Unit (Cunit (Main_Unit))
         then
            Add_Call (E);
            return Inline_Package;
         end if;

         --  The call is not in the main unit. See if it is in some subprogram
         --  that can be inlined outside its unit. If so, inline the call and,
         --  if the inlining level is set to 1, stop there; otherwise also
         --  compile the package as above.

         Scop := Current_Scope;
         while Scope (Scop) /= Standard_Standard
           and then not Is_Child_Unit (Scop)
         loop
            if Is_Overloadable (Scop)
              and then Is_Inlined (Scop)
              and then not Is_Nested (Scop)
            then
               Add_Call (E, Scop);

               if Inline_Level = 1 then
                  return Inline_Call;
               else
                  return Inline_Package;
               end if;
            end if;

            Scop := Scope (Scop);
         end loop;

         return Dont_Inline;
      end Must_Inline;

      Inst      : Entity_Id;
      Inst_Decl : Node_Id;
      Level     : Inline_Level_Type;

   --  Start of processing for Add_Inlined_Body

   begin
      Append_New_Elmt (N, To => Backend_Calls);

      --  Skip subprograms that cannot or need not be inlined outside their
      --  unit or parent subprogram.

      if Is_Abstract_Subprogram (E)
        or else Convention (E) = Convention_Protected
        or else In_Main_Unit_Or_Subunit (E)
        or else Is_Nested (E)
      then
         return;
      end if;

      --  Find out whether the call must be inlined. Unless the result is
      --  Dont_Inline, Must_Inline also creates an edge for the call in the
      --  callgraph; however, it will not be activated until after Is_Called
      --  is set on the subprogram.

      Level := Must_Inline;

      if Level = Dont_Inline then
         return;
      end if;

      --  If a previous call to the subprogram has been inlined, nothing to do

      if Is_Called (E) then
         return;
      end if;

      --  If the subprogram is an instance, then inline the instance

      if Is_Generic_Instance (E) then
         Add_Inlined_Instance (E);
      end if;

      --  Mark the subprogram as called

      Set_Is_Called (E);

      --  If the call was generated by the compiler and is to a subprogram in
      --  a run-time unit, we need to suppress debugging information for it,
      --  so that the code that is eventually inlined will not affect the
      --  debugging of the program. We do not do it if the call comes from
      --  source because, even if the call is inlined, the user may expect it
      --  to be present in the debugging information.

      if not Comes_From_Source (N)
        and then In_Extended_Main_Source_Unit (N)
        and then Is_Predefined_Unit (Get_Source_Unit (E))
      then
         Set_Needs_Debug_Info (E, False);
      end if;

      --  If the subprogram is an expression function, or is completed by one
      --  where both the spec and body are in the same context, then there is
      --  no need to load any package body since the body of the function is
      --  in the spec.

      if Is_Non_Loading_Expression_Function (E) then
         return;
      end if;

      --  Find unit containing E, and add to list of inlined bodies if needed.
      --  Library-level functions must be handled specially, because there is
      --  no enclosing package to retrieve. In this case, it is the body of
      --  the function that will have to be loaded.

      declare
         Pack : constant Entity_Id := Get_Code_Unit_Entity (E);

      begin
         if Pack = E then
            Inlined_Bodies.Increment_Last;
            Inlined_Bodies.Table (Inlined_Bodies.Last) := E;

         else
            pragma Assert (Ekind (Pack) = E_Package);

            --  If the subprogram is within an instance, inline the instance

            if Comes_From_Source (E) then
               Inst := Scope (E);

               while Present (Inst) and then Inst /= Standard_Standard loop
                  exit when Is_Generic_Instance (Inst);
                  Inst := Scope (Inst);
               end loop;

               if Present (Inst)
                 and then Is_Generic_Instance (Inst)
                 and then not Is_Called (Inst)
               then
                  Inst_Decl := Unit_Declaration_Node (Inst);

                  --  Do not inline the instance if the body already exists,
                  --  or the instance node is simply missing.

                  if Present (Corresponding_Body (Inst_Decl))
                    or else (Nkind (Parent (Inst_Decl)) /= N_Compilation_Unit
                              and then No (Next (Inst_Decl)))
                  then
                     Set_Is_Called (Inst);
                  else
                     Add_Inlined_Instance (Inst);
                  end if;
               end if;
            end if;

            --  If the unit containing E is an instance, nothing more to do

            if Is_Generic_Instance (Pack) then
               null;

            --  Do not inline the package if the subprogram is an init proc
            --  or other internally generated subprogram, because in that
            --  case the subprogram body appears in the same unit that
            --  declares the type, and that body is visible to the back end.
            --  Do not inline it either if it is in the main unit.
            --  Extend the -gnatn2 processing to -gnatn1 for Inline_Always
            --  calls if the back end takes care of inlining the call.
            --  Note that Level is in Inline_Call | Inline_Package here.

            elsif ((Level = Inline_Call
                      and then Has_Pragma_Inline_Always (E)
                      and then Back_End_Inlining)
                    or else Level = Inline_Package)
              and then not Is_Inlined (Pack)
              and then not Is_Internal (E)
              and then not In_Main_Unit_Or_Subunit (Pack)
            then
               Set_Is_Inlined (Pack);
               Inlined_Bodies.Increment_Last;
               Inlined_Bodies.Table (Inlined_Bodies.Last) := Pack;
            end if;
         end if;

         --  Ensure that Analyze_Inlined_Bodies will be invoked after
         --  completing the analysis of the current unit.

         Inline_Processing_Required := True;
      end;
   end Add_Inlined_Body;

   --------------------------
   -- Add_Inlined_Instance --
   --------------------------

   procedure Add_Inlined_Instance (E : Entity_Id) is
      Decl_Node : constant Node_Id := Unit_Declaration_Node (E);
      Index     : Int;

   begin
      --  This machinery is only used with back-end inlining

      if not Back_End_Inlining then
         return;
      end if;

      --  Register the instance in the list

      Append_New_Elmt (Decl_Node, To => Backend_Instances);

      --  Retrieve the index of its corresponding pending instantiation
      --  and mark this corresponding pending instantiation as needed.

      Index := To_Pending_Instantiations.Get (Decl_Node);
      if Index >= 0 then
         Called_Pending_Instantiations.Append (Index);
      else
         pragma Assert (False);
         null;
      end if;

      Set_Is_Called (E);
   end Add_Inlined_Instance;

   ----------------------------
   -- Add_Inlined_Subprogram --
   ----------------------------

   procedure Add_Inlined_Subprogram (E : Entity_Id) is
      Decl : constant Node_Id   := Parent (Declaration_Node (E));
      Pack : constant Entity_Id := Get_Code_Unit_Entity (E);

      procedure Register_Backend_Inlined_Subprogram (Subp : Entity_Id);
      --  Append Subp to the list of subprograms inlined by the backend

      procedure Register_Backend_Not_Inlined_Subprogram (Subp : Entity_Id);
      --  Append Subp to the list of subprograms that cannot be inlined by
      --  the backend.

      -----------------------------------------
      -- Register_Backend_Inlined_Subprogram --
      -----------------------------------------

      procedure Register_Backend_Inlined_Subprogram (Subp : Entity_Id) is
      begin
         Append_New_Elmt (Subp, To => Backend_Inlined_Subps);
      end Register_Backend_Inlined_Subprogram;

      ---------------------------------------------
      -- Register_Backend_Not_Inlined_Subprogram --
      ---------------------------------------------

      procedure Register_Backend_Not_Inlined_Subprogram (Subp : Entity_Id) is
      begin
         Append_New_Elmt (Subp, To => Backend_Not_Inlined_Subps);
      end Register_Backend_Not_Inlined_Subprogram;

   --  Start of processing for Add_Inlined_Subprogram

   begin
      --  We can inline the subprogram if its unit is known to be inlined or is
      --  an instance whose body will be analyzed anyway or the subprogram was
      --  generated as a body by the compiler (for example an initialization
      --  procedure) or its declaration was provided along with the body (for
      --  example an expression function) and it does not declare types with
      --  nontrivial initialization procedures.

      if (Is_Inlined (Pack)
           or else Is_Generic_Instance (Pack)
           or else Nkind (Decl) = N_Subprogram_Body
           or else Present (Corresponding_Body (Decl)))
        and then not Has_Initialized_Type (E)
      then
         Register_Backend_Inlined_Subprogram (E);

         if No (Last_Inlined) then
            Set_First_Inlined_Subprogram (Cunit (Main_Unit), E);
         else
            Set_Next_Inlined_Subprogram (Last_Inlined, E);
         end if;

         Last_Inlined := E;

      else
         Register_Backend_Not_Inlined_Subprogram (E);
      end if;
   end Add_Inlined_Subprogram;

   --------------------------------
   --  Add_Pending_Instantiation --
   --------------------------------

   procedure Add_Pending_Instantiation
     (Inst     : Node_Id;
      Act_Decl : Node_Id;
      Fin_Scop : Node_Id := Empty)
   is
      Act_Decl_Id : Entity_Id;
      Index       : Int;

   begin
      --  Here is a defense against a ludicrous number of instantiations
      --  caused by a circular set of instantiation attempts.

      if Pending_Instantiations.Last + 1 >= Maximum_Instantiations then
         Error_Msg_Uint_1 := UI_From_Int (Maximum_Instantiations);
         Error_Msg_N ("too many instantiations, exceeds max of^", Inst);
         Error_Msg_N ("\limit can be changed using -gnateinn switch", Inst);
         raise Unrecoverable_Error;
      end if;

      --  Capture the body of the generic instantiation along with its context
      --  for later processing by Instantiate_Bodies.

      Pending_Instantiations.Append
        ((Inst_Node                => Inst,
          Act_Decl                 => Act_Decl,
          Fin_Scop                 => Fin_Scop,
          Config_Switches          => Save_Config_Switches,
          Current_Sem_Unit         => Current_Sem_Unit,
          Expander_Status          => Expander_Active,
          Local_Suppress_Stack_Top => Local_Suppress_Stack_Top,
          Scope_Suppress           => Scope_Suppress,
          Warnings                 => Save_Warnings));

      --  With back-end inlining, also associate the index to the instantiation

      if Back_End_Inlining then
         Act_Decl_Id := Defining_Entity (Act_Decl);
         Index := Pending_Instantiations.Last;

         To_Pending_Instantiations.Set (Act_Decl, Index);

         --  If an instantiation is in the main unit or subunit, or is a nested
         --  subprogram, then its body is needed as per the analysis done in
         --  Analyze_Package_Instantiation & Analyze_Subprogram_Instantiation.

         if In_Main_Unit_Or_Subunit (Act_Decl_Id)
           or else (Is_Subprogram (Act_Decl_Id)
                     and then Is_Nested (Act_Decl_Id))
         then
            Called_Pending_Instantiations.Append (Index);

            Set_Is_Called (Act_Decl_Id);
         end if;
      end if;
   end Add_Pending_Instantiation;

   ------------------------
   -- Add_Scope_To_Clean --
   ------------------------

   procedure Add_Scope_To_Clean (Scop : Entity_Id) is
      Elmt : Elmt_Id;

   begin
      Elmt := First_Elmt (To_Clean);
      while Present (Elmt) loop
         if Node (Elmt) = Scop then
            return;
         end if;

         Next_Elmt (Elmt);
      end loop;

      Append_Elmt (Scop, To_Clean);
   end Add_Scope_To_Clean;

   --------------
   -- Add_Subp --
   --------------

   function Add_Subp (E : Entity_Id) return Subp_Index is
      Index : Subp_Index := Subp_Index (E) mod Num_Hash_Headers;
      J     : Subp_Index;

      procedure New_Entry;
      --  Initialize entry in Inlined table

      procedure New_Entry is
      begin
         Inlined.Increment_Last;
         Inlined.Table (Inlined.Last).Name        := E;
         Inlined.Table (Inlined.Last).Next        := No_Subp;
         Inlined.Table (Inlined.Last).First_Succ  := No_Succ;
         Inlined.Table (Inlined.Last).Main_Call   := False;
         Inlined.Table (Inlined.Last).Processed   := False;
      end New_Entry;

   --  Start of processing for Add_Subp

   begin
      if Hash_Headers (Index) = No_Subp then
         New_Entry;
         Hash_Headers (Index) := Inlined.Last;
         return Inlined.Last;

      else
         J := Hash_Headers (Index);
         while J /= No_Subp loop
            if Inlined.Table (J).Name = E then
               return J;
            else
               Index := J;
               J := Inlined.Table (J).Next;
            end if;
         end loop;

         --  On exit, subprogram was not found. Enter in table. Index is
         --  the current last entry on the hash chain.

         New_Entry;
         Inlined.Table (Index).Next := Inlined.Last;
         return Inlined.Last;
      end if;
   end Add_Subp;

   ----------------------------
   -- Analyze_Inlined_Bodies --
   ----------------------------

   procedure Analyze_Inlined_Bodies is
      Comp_Unit : Node_Id;
      J         : Int;
      Pack      : Entity_Id;
      Subp      : Subp_Index;
      S         : Succ_Index;

      type Pending_Index is new Nat;

      package Pending_Inlined is new Table.Table (
         Table_Component_Type => Subp_Index,
         Table_Index_Type     => Pending_Index,
         Table_Low_Bound      => 1,
         Table_Initial        => Alloc.Inlined_Initial,
         Table_Increment      => Alloc.Inlined_Increment,
         Table_Name           => "Pending_Inlined");
      --  The workpile used to compute the transitive closure

   --  Start of processing for Analyze_Inlined_Bodies

   begin
      if Serious_Errors_Detected = 0 then
         Push_Scope (Standard_Standard);

         J := 0;
         while J <= Inlined_Bodies.Last
           and then Serious_Errors_Detected = 0
         loop
            Pack := Inlined_Bodies.Table (J);
            while Present (Pack)
              and then Scope (Pack) /= Standard_Standard
              and then not Is_Child_Unit (Pack)
            loop
               Pack := Scope (Pack);
            end loop;

            Comp_Unit := Parent (Pack);
            while Present (Comp_Unit)
              and then Nkind (Comp_Unit) /= N_Compilation_Unit
            loop
               Comp_Unit := Parent (Comp_Unit);
            end loop;

            --  Load the body if it exists and contains inlineable entities,
            --  unless it is the main unit, or is an instance whose body has
            --  already been analyzed.

            if Present (Comp_Unit)
              and then Comp_Unit /= Cunit (Main_Unit)
              and then Body_Required (Comp_Unit)
              and then
                (Nkind (Unit (Comp_Unit)) /= N_Package_Declaration
                  or else
                    (No (Corresponding_Body (Unit (Comp_Unit)))
                      and then Body_Needed_For_Inlining
                                 (Defining_Entity (Unit (Comp_Unit)))))
            then
               declare
                  Bname : constant Unit_Name_Type :=
                            Get_Body_Name (Get_Unit_Name (Unit (Comp_Unit)));

                  OK : Boolean;

               begin
                  if not Is_Loaded (Bname) then
                     Style_Check := False;
                     Load_Needed_Body (Comp_Unit, OK);

                     if not OK then

                        --  Warn that a body was not available for inlining
                        --  by the back-end.

                        Error_Msg_Unit_1 := Bname;
                        Error_Msg_N
                          ("one or more inlined subprograms accessed in $!??",
                           Comp_Unit);
                        Error_Msg_File_1 :=
                          Get_File_Name (Bname, Subunit => False);
                        Error_Msg_N ("\but file{ was not found!??", Comp_Unit);
                     end if;
                  end if;
               end;
            end if;

            J := J + 1;

            if J > Inlined_Bodies.Last then

               --  The analysis of required bodies may have produced additional
               --  generic instantiations. To obtain further inlining, we need
               --  to perform another round of generic body instantiations.

               Instantiate_Bodies;

               --  Symmetrically, the instantiation of required generic bodies
               --  may have caused additional bodies to be inlined. To obtain
               --  further inlining, we keep looping over the inlined bodies.
            end if;
         end loop;

         --  The list of inlined subprograms is an overestimate, because it
         --  includes inlined functions called from functions that are compiled
         --  as part of an inlined package, but are not themselves called. An
         --  accurate computation of just those subprograms that are needed
         --  requires that we perform a transitive closure over the call graph,
         --  starting from calls in the main compilation unit.

         for Index in Inlined.First .. Inlined.Last loop
            if not Is_Called (Inlined.Table (Index).Name) then

               --  This means that Add_Inlined_Body added the subprogram to the
               --  table but wasn't able to handle its code unit. Do nothing.

               Inlined.Table (Index).Processed := True;

            elsif Inlined.Table (Index).Main_Call then
               Pending_Inlined.Increment_Last;
               Pending_Inlined.Table (Pending_Inlined.Last) := Index;
               Inlined.Table (Index).Processed := True;

            else
               Set_Is_Called (Inlined.Table (Index).Name, False);
            end if;
         end loop;

         --  Iterate over the workpile until it is emptied, propagating the
         --  Is_Called flag to the successors of the processed subprogram.

         while Pending_Inlined.Last >= Pending_Inlined.First loop
            Subp := Pending_Inlined.Table (Pending_Inlined.Last);
            Pending_Inlined.Decrement_Last;

            S := Inlined.Table (Subp).First_Succ;

            while S /= No_Succ loop
               Subp := Successors.Table (S).Subp;

               if not Inlined.Table (Subp).Processed then
                  Set_Is_Called (Inlined.Table (Subp).Name);
                  Pending_Inlined.Increment_Last;
                  Pending_Inlined.Table (Pending_Inlined.Last) := Subp;
                  Inlined.Table (Subp).Processed := True;
               end if;

               S := Successors.Table (S).Next;
            end loop;
         end loop;

         --  Finally add the called subprograms to the list of inlined
         --  subprograms for the unit.

         for Index in Inlined.First .. Inlined.Last loop
            declare
               E : constant Subprogram_Kind_Id := Inlined.Table (Index).Name;

            begin
               if Is_Called (E) and then not Is_Ignored_Ghost_Entity (E) then
                  Add_Inlined_Subprogram (E);
               end if;
            end;
         end loop;

         Pop_Scope;
      end if;
   end Analyze_Inlined_Bodies;

   --------------------------
   -- Build_Body_To_Inline --
   --------------------------

   procedure Build_Body_To_Inline (N : Node_Id; Spec_Id : Entity_Id) is
      Decl            : constant Node_Id := Unit_Declaration_Node (Spec_Id);
      Original_Body   : Node_Id;
      Body_To_Analyze : Node_Id;
      Max_Size        : constant := 10;

      function Has_Extended_Return return Boolean;
      --  This function returns True if the subprogram has an extended return
      --  statement.

      function Has_Pending_Instantiation return Boolean;
      --  If some enclosing body contains instantiations that appear before
      --  the corresponding generic body, the enclosing body has a freeze node
      --  so that it can be elaborated after the generic itself. This might
      --  conflict with subsequent inlinings, so that it is unsafe to try to
      --  inline in such a case.

      function Has_Single_Return_In_GNATprove_Mode return Boolean;
      --  This function is called only in GNATprove mode, and it returns
      --  True if the subprogram has no return statement or a single return
      --  statement as last statement. It returns False for subprogram with
      --  a single return as last statement inside one or more blocks, as
      --  inlining would generate gotos in that case as well (although the
      --  goto is useless in that case).

      function Uses_Secondary_Stack (Bod : Node_Id) return Boolean;
      --  If the body of the subprogram includes a call that returns an
      --  unconstrained type, the secondary stack is involved, and it is
      --  not worth inlining.

      -------------------------
      -- Has_Extended_Return --
      -------------------------

      function Has_Extended_Return return Boolean is
         Body_To_Inline : constant Node_Id := N;

         function Check_Return (N : Node_Id) return Traverse_Result;
         --  Returns OK on node N if this is not an extended return statement

         ------------------
         -- Check_Return --
         ------------------

         function Check_Return (N : Node_Id) return Traverse_Result is
         begin
            case Nkind (N) is
               when N_Extended_Return_Statement =>
                  return Abandon;

               --  Skip locally declared subprogram bodies inside the body to
               --  inline, as the return statements inside those do not count.

               when N_Subprogram_Body =>
                  if N = Body_To_Inline then
                     return OK;
                  else
                     return Skip;
                  end if;

               when others =>
                  return OK;
            end case;
         end Check_Return;

         function Check_All_Returns is new Traverse_Func (Check_Return);

      --  Start of processing for Has_Extended_Return

      begin
         return Check_All_Returns (N) /= OK;
      end Has_Extended_Return;

      -------------------------------
      -- Has_Pending_Instantiation --
      -------------------------------

      function Has_Pending_Instantiation return Boolean is
         S : Entity_Id;

      begin
         S := Current_Scope;
         while Present (S) loop
            if Is_Compilation_Unit (S)
              or else Is_Child_Unit (S)
            then
               return False;

            elsif Ekind (S) = E_Package
              and then Has_Forward_Instantiation (S)
            then
               return True;
            end if;

            S := Scope (S);
         end loop;

         return False;
      end Has_Pending_Instantiation;

      -----------------------------------------
      -- Has_Single_Return_In_GNATprove_Mode --
      -----------------------------------------

      function Has_Single_Return_In_GNATprove_Mode return Boolean is
         Body_To_Inline : constant Node_Id := N;
         Last_Statement : Node_Id := Empty;

         function Check_Return (N : Node_Id) return Traverse_Result;
         --  Returns OK on node N if this is not a return statement different
         --  from the last statement in the subprogram.

         ------------------
         -- Check_Return --
         ------------------

         function Check_Return (N : Node_Id) return Traverse_Result is
         begin
            case Nkind (N) is
               when N_Extended_Return_Statement
                  | N_Simple_Return_Statement
               =>
                  if N = Last_Statement then
                     return OK;
                  else
                     return Abandon;
                  end if;

               --  Skip locally declared subprogram bodies inside the body to
               --  inline, as the return statements inside those do not count.

               when N_Subprogram_Body =>
                  if N = Body_To_Inline then
                     return OK;
                  else
                     return Skip;
                  end if;

               when others =>
                  return OK;
            end case;
         end Check_Return;

         function Check_All_Returns is new Traverse_Func (Check_Return);

      --  Start of processing for Has_Single_Return_In_GNATprove_Mode

      begin
         --  Retrieve the last statement

         Last_Statement := Last (Statements (Handled_Statement_Sequence (N)));

         --  Check that the last statement is the only possible return
         --  statement in the subprogram.

         return Check_All_Returns (N) = OK;
      end Has_Single_Return_In_GNATprove_Mode;

      --------------------------
      -- Uses_Secondary_Stack --
      --------------------------

      function Uses_Secondary_Stack (Bod : Node_Id) return Boolean is
         function Check_Call (N : Node_Id) return Traverse_Result;
         --  Look for function calls that return an unconstrained type

         ----------------
         -- Check_Call --
         ----------------

         function Check_Call (N : Node_Id) return Traverse_Result is
         begin
            if Nkind (N) = N_Function_Call
              and then Is_Entity_Name (Name (N))
              and then Is_Composite_Type (Etype (Entity (Name (N))))
              and then not Is_Constrained (Etype (Entity (Name (N))))
            then
               Cannot_Inline
                 ("cannot inline & (call returns unconstrained type)?",
                  N, Spec_Id);
               return Abandon;
            else
               return OK;
            end if;
         end Check_Call;

         function Check_Calls is new Traverse_Func (Check_Call);

      begin
         return Check_Calls (Bod) = Abandon;
      end Uses_Secondary_Stack;

   --  Start of processing for Build_Body_To_Inline

   begin
      --  Return immediately if done already

      if Nkind (Decl) = N_Subprogram_Declaration
        and then Present (Body_To_Inline (Decl))
      then
         return;

      --  Subprograms that have return statements in the middle of the body are
      --  inlined with gotos. GNATprove does not currently support gotos, so
      --  we prevent such inlining.

      elsif GNATprove_Mode
        and then not Has_Single_Return_In_GNATprove_Mode
      then
         Cannot_Inline ("cannot inline & (multiple returns)?", N, Spec_Id);
         return;

      --  Functions that return controlled types cannot currently be inlined
      --  because they require secondary stack handling; controlled actions
      --  may also interfere in complex ways with inlining.

      elsif Ekind (Spec_Id) = E_Function
        and then Needs_Finalization (Etype (Spec_Id))
      then
         Cannot_Inline
           ("cannot inline & (controlled return type)?", N, Spec_Id);
         return;
      end if;

      if Has_Excluded_Declaration (Spec_Id, Declarations (N)) then
         return;
      end if;

      if Present (Handled_Statement_Sequence (N)) then
         if Present (Exception_Handlers (Handled_Statement_Sequence (N))) then
            Cannot_Inline
              ("cannot inline& (exception handler)?",
               First (Exception_Handlers (Handled_Statement_Sequence (N))),
               Spec_Id);
            return;

         elsif Has_Excluded_Statement
                 (Spec_Id, Statements (Handled_Statement_Sequence (N)))
         then
            return;
         end if;
      end if;

      --  We do not inline a subprogram that is too large, unless it is marked
      --  Inline_Always or we are in GNATprove mode. This pragma does not
      --  suppress the other checks on inlining (forbidden declarations,
      --  handlers, etc).

      if not (Has_Pragma_Inline_Always (Spec_Id) or else GNATprove_Mode)
        and then List_Length
                   (Statements (Handled_Statement_Sequence (N))) > Max_Size
      then
         Cannot_Inline ("cannot inline& (body too large)?", N, Spec_Id);
         return;
      end if;

      if Has_Pending_Instantiation then
         Cannot_Inline
           ("cannot inline& (forward instance within enclosing body)?",
             N, Spec_Id);
         return;
      end if;

      --  Within an instance, the body to inline must be treated as a nested
      --  generic, so that the proper global references are preserved.

      --  Note that we do not do this at the library level, because it is not
      --  needed, and furthermore this causes trouble if front-end inlining
      --  is activated (-gnatN).

      if In_Instance and then Scope (Current_Scope) /= Standard_Standard then
         Save_Env (Scope (Current_Scope), Scope (Current_Scope));
         Original_Body := Copy_Generic_Node (N, Empty, Instantiating => True);
      else
         Original_Body := Copy_Separate_Tree (N);
      end if;

      --  We need to capture references to the formals in order to substitute
      --  the actuals at the point of inlining, i.e. instantiation. To treat
      --  the formals as globals to the body to inline, we nest it within a
      --  dummy parameterless subprogram, declared within the real one. To
      --  avoid generating an internal name (which is never public, and which
      --  affects serial numbers of other generated names), we use an internal
      --  symbol that cannot conflict with user declarations.

      Set_Parameter_Specifications (Specification (Original_Body), No_List);
      Set_Defining_Unit_Name
        (Specification (Original_Body),
         Make_Defining_Identifier (Sloc (N), Name_uParent));
      Set_Corresponding_Spec (Original_Body, Empty);

      --  Remove all aspects/pragmas that have no meaning in an inlined body

      Remove_Aspects_And_Pragmas (Original_Body);

      Body_To_Analyze :=
        Copy_Generic_Node (Original_Body, Empty, Instantiating => False);

      --  Set return type of function, which is also global and does not need
      --  to be resolved.

      if Ekind (Spec_Id) = E_Function then
         Set_Result_Definition
           (Specification (Body_To_Analyze),
            New_Occurrence_Of (Etype (Spec_Id), Sloc (N)));
      end if;

      if No (Declarations (N)) then
         Set_Declarations (N, New_List (Body_To_Analyze));
      else
         Append (Body_To_Analyze, Declarations (N));
      end if;

      Start_Generic;

      Analyze (Body_To_Analyze);
      Push_Scope (Defining_Entity (Body_To_Analyze));
      Save_Global_References (Original_Body);
      End_Scope;
      Remove (Body_To_Analyze);

      End_Generic;

      --  Restore environment if previously saved

      if In_Instance and then Scope (Current_Scope) /= Standard_Standard then
         Restore_Env;
      end if;

      --  Functions that return unconstrained composite types require
      --  secondary stack handling, and cannot currently be inlined, unless
      --  all return statements return a local variable that is the first
      --  local declaration in the body. We had to delay this check until
      --  the body of the function is analyzed since Has_Single_Return()
      --  requires a minimum decoration.

      if Ekind (Spec_Id) = E_Function
        and then not Is_Scalar_Type (Etype (Spec_Id))
        and then not Is_Access_Type (Etype (Spec_Id))
        and then not Is_Constrained (Etype (Spec_Id))
      then
         if not Has_Single_Return (Body_To_Analyze)

           --  Skip inlining if the function returns an unconstrained type
           --  using an extended return statement, since this part of the
           --  new inlining model is not yet supported by the current
           --  implementation.

           or else (Returns_Unconstrained_Type (Spec_Id)
                     and then Has_Extended_Return)
         then
            Cannot_Inline
              ("cannot inline & (unconstrained return type)?", N, Spec_Id);
            return;
         end if;

      --  If secondary stack is used, there is no point in inlining. We have
      --  already issued the warning in this case, so nothing to do.

      elsif Uses_Secondary_Stack (Body_To_Analyze) then
         return;
      end if;

      Set_Body_To_Inline (Decl, Original_Body);
      Mutate_Ekind (Defining_Entity (Original_Body), Ekind (Spec_Id));
      Set_Is_Inlined (Spec_Id);
   end Build_Body_To_Inline;

   -------------------------------------------
   -- Call_Can_Be_Inlined_In_GNATprove_Mode --
   -------------------------------------------

   function Call_Can_Be_Inlined_In_GNATprove_Mode
    (N    : Node_Id;
     Subp : Entity_Id) return Boolean
   is
      F : Entity_Id;
      A : Node_Id;

   begin
      F := First_Formal (Subp);
      A := First_Actual (N);
      while Present (F) loop
         if Ekind (F) /= E_Out_Parameter
           and then not Same_Type (Etype (F), Etype (A))
           and then
             (Is_By_Reference_Type (Etype (A))
               or else Is_Limited_Type (Etype (A)))
         then
            return False;
         end if;

         Next_Formal (F);
         Next_Actual (A);
      end loop;

      return True;
   end Call_Can_Be_Inlined_In_GNATprove_Mode;

   --------------------------------------
   -- Can_Be_Inlined_In_GNATprove_Mode --
   --------------------------------------

   function Can_Be_Inlined_In_GNATprove_Mode
     (Spec_Id : Entity_Id;
      Body_Id : Entity_Id) return Boolean
   is
      function Has_Formal_Or_Result_Of_Deep_Type
        (Id : Entity_Id) return Boolean;
      --  Returns true if the subprogram has at least one formal parameter or
      --  a return type of a deep type: either an access type or a composite
      --  type containing an access type.

      function Has_Formal_With_Discriminant_Dependent_Fields
        (Id : Entity_Id) return Boolean;
      --  Returns true if the subprogram has at least one formal parameter of
      --  an unconstrained record type with per-object constraints on component
      --  types.

      function Has_Skip_Proof_Annotation (Id : Entity_Id) return Boolean;
      --  Returns True if subprogram Id has an annotation Skip_Proof or
      --  Skip_Flow_And_Proof.

      function Has_Some_Contract (Id : Entity_Id) return Boolean;
      --  Return True if subprogram Id has any contract. The presence of
      --  Extensions_Visible or Volatile_Function is also considered as a
      --  contract here.

      function Is_Unit_Subprogram (Id : Entity_Id) return Boolean;
      --  Return True if subprogram Id defines a compilation unit

      function In_Package_Spec (Id : Entity_Id) return Boolean;
      --  Return True if subprogram Id is defined in the package specification,
      --  either its visible or private part.

      function Maybe_Traversal_Function (Id : Entity_Id) return Boolean;
      --  Return True if subprogram Id could be a traversal function, as
      --  defined in SPARK RM 3.10. This is only a safe approximation, as the
      --  knowledge of the SPARK boundary is needed to determine exactly
      --  traversal functions.

      ---------------------------------------
      -- Has_Formal_Or_Result_Of_Deep_Type --
      ---------------------------------------

      function Has_Formal_Or_Result_Of_Deep_Type
        (Id : Entity_Id) return Boolean
      is
         function Is_Deep (Typ : Entity_Id) return Boolean;
         --  Return True if Typ is deep: either an access type or a composite
         --  type containing an access type.

         -------------
         -- Is_Deep --
         -------------

         function Is_Deep (Typ : Entity_Id) return Boolean is
         begin
            case Type_Kind'(Ekind (Typ)) is
               when Access_Kind =>
                  return True;

               when E_Array_Type
                  | E_Array_Subtype
               =>
                  return Is_Deep (Component_Type (Typ));

               when Record_Kind =>
                  declare
                     Comp : Entity_Id := First_Component_Or_Discriminant (Typ);
                  begin
                     while Present (Comp) loop
                        if Is_Deep (Etype (Comp)) then
                           return True;
                        end if;
                        Next_Component_Or_Discriminant (Comp);
                     end loop;
                  end;
                  return False;

               when Scalar_Kind
                  | E_String_Literal_Subtype
                  | Concurrent_Kind
                  | Incomplete_Kind
                  | E_Exception_Type
                  | E_Subprogram_Type
               =>
                  return False;

               when E_Private_Type
                  | E_Private_Subtype
                  | E_Limited_Private_Type
                  | E_Limited_Private_Subtype
               =>
                  --  Conservatively consider that the type might be deep if
                  --  its completion has not been seen yet.

                  if No (Underlying_Type (Typ)) then
                     return True;

                  --  Do not peek under a private type if its completion has
                  --  SPARK_Mode Off. In such a case, a deep type is considered
                  --  by GNATprove to be not deep.

                  elsif Present (Full_View (Typ))
                    and then Present (SPARK_Pragma (Full_View (Typ)))
                    and then Get_SPARK_Mode_From_Annotation
                      (SPARK_Pragma (Full_View (Typ))) = Off
                  then
                     return False;

                  --  Otherwise peek under the private type.

                  else
                     return Is_Deep (Underlying_Type (Typ));
                  end if;
            end case;
         end Is_Deep;

         --  Local variables

         Subp_Id    : constant Entity_Id := Ultimate_Alias (Id);
         Formal     : Entity_Id;
         Formal_Typ : Entity_Id;

      --  Start of processing for Has_Formal_Or_Result_Of_Deep_Type

      begin
         --  Inspect all parameters of the subprogram looking for a formal
         --  of a deep type.

         Formal := First_Formal (Subp_Id);
         while Present (Formal) loop
            Formal_Typ := Etype (Formal);

            if Is_Deep (Formal_Typ) then
               return True;
            end if;

            Next_Formal (Formal);
         end loop;

         --  Check whether this is a function whose return type is deep

         if Ekind (Subp_Id) = E_Function
           and then Is_Deep (Etype (Subp_Id))
         then
            return True;
         end if;

         return False;
      end Has_Formal_Or_Result_Of_Deep_Type;

      ---------------------------------------------------
      -- Has_Formal_With_Discriminant_Dependent_Fields --
      ---------------------------------------------------

      function Has_Formal_With_Discriminant_Dependent_Fields
        (Id : Entity_Id) return Boolean
      is
         function Has_Discriminant_Dependent_Component
           (Typ : Entity_Id) return Boolean;
         --  Determine whether unconstrained record type Typ has at least one
         --  component that depends on a discriminant.

         ------------------------------------------
         -- Has_Discriminant_Dependent_Component --
         ------------------------------------------

         function Has_Discriminant_Dependent_Component
           (Typ : Entity_Id) return Boolean
         is
            Comp : Entity_Id;

         begin
            --  Inspect all components of the record type looking for one that
            --  depends on a discriminant.

            Comp := First_Component (Typ);
            while Present (Comp) loop
               if Has_Discriminant_Dependent_Constraint (Comp) then
                  return True;
               end if;

               Next_Component (Comp);
            end loop;

            return False;
         end Has_Discriminant_Dependent_Component;

         --  Local variables

         Subp_Id    : constant Entity_Id := Ultimate_Alias (Id);
         Formal     : Entity_Id;
         Formal_Typ : Entity_Id;

      --  Start of processing for
      --  Has_Formal_With_Discriminant_Dependent_Fields

      begin
         --  Inspect all parameters of the subprogram looking for a formal
         --  of an unconstrained record type with at least one discriminant
         --  dependent component.

         Formal := First_Formal (Subp_Id);
         while Present (Formal) loop
            Formal_Typ := Etype (Formal);

            if Is_Record_Type (Formal_Typ)
              and then not Is_Constrained (Formal_Typ)
              and then Has_Discriminant_Dependent_Component (Formal_Typ)
            then
               return True;
            end if;

            Next_Formal (Formal);
         end loop;

         return False;
      end Has_Formal_With_Discriminant_Dependent_Fields;

      -------------------------------
      -- Has_Skip_Proof_Annotation --
      -------------------------------

      function Has_Skip_Proof_Annotation (Id : Entity_Id) return Boolean is
         Decl : Node_Id := Unit_Declaration_Node (Id);

      begin
         Next (Decl);

         while Present (Decl)
           and then Nkind (Decl) = N_Pragma
         loop
            if Get_Pragma_Id (Decl) = Pragma_Annotate
              and then List_Length (Pragma_Argument_Associations (Decl)) = 3
            then
               declare
                  Arg1      : constant Node_Id :=
                    First (Pragma_Argument_Associations (Decl));
                  Arg2      : constant Node_Id := Next (Arg1);
                  Arg1_Name : constant String :=
                    Get_Name_String (Chars (Get_Pragma_Arg (Arg1)));
                  Arg2_Name : constant String :=
                    Get_Name_String (Chars (Get_Pragma_Arg (Arg2)));
               begin
                  if Arg1_Name = "gnatprove"
                    and then Arg2_Name in "skip_proof" | "skip_flow_and_proof"
                  then
                     return True;
                  end if;
               end;
            end if;

            Next (Decl);
         end loop;

         return False;
      end Has_Skip_Proof_Annotation;

      -----------------------
      -- Has_Some_Contract --
      -----------------------

      function Has_Some_Contract (Id : Entity_Id) return Boolean is
         Items : Node_Id;

      begin
         --  A call to an expression function may precede the actual body which
         --  is inserted at the end of the enclosing declarations. Ensure that
         --  the related entity is decorated before inspecting the contract.

         if Is_Subprogram_Or_Generic_Subprogram (Id) then
            Items := Contract (Id);

            --  Note that Classifications is not Empty when Extensions_Visible
            --  or Volatile_Function is present, which causes such subprograms
            --  to be considered to have a contract here. This is fine as we
            --  want to avoid inlining these too.

            return Present (Items)
              and then (Present (Pre_Post_Conditions (Items)) or else
                        Present (Contract_Test_Cases (Items)) or else
                        Present (Classifications     (Items)));
         end if;

         return False;
      end Has_Some_Contract;

      ---------------------
      -- In_Package_Spec --
      ---------------------

      function In_Package_Spec (Id : Entity_Id) return Boolean is
         P : constant Node_Id := Parent (Subprogram_Spec (Id));
         --  Parent of the subprogram's declaration

      begin
         return Nkind (Enclosing_Declaration (P)) = N_Package_Declaration;
      end In_Package_Spec;

      ------------------------
      -- Is_Unit_Subprogram --
      ------------------------

      function Is_Unit_Subprogram (Id : Entity_Id) return Boolean is
         Decl : Node_Id := Parent (Parent (Id));
      begin
         if Nkind (Parent (Id)) = N_Defining_Program_Unit_Name then
            Decl := Parent (Decl);
         end if;

         return Nkind (Parent (Decl)) = N_Compilation_Unit;
      end Is_Unit_Subprogram;

      ------------------------------
      -- Maybe_Traversal_Function --
      ------------------------------

      function Maybe_Traversal_Function (Id : Entity_Id) return Boolean is
      begin
         return Ekind (Id) = E_Function

           --  Only traversal functions return an anonymous access-to-object
           --  type in SPARK.

           and then Is_Anonymous_Access_Type (Etype (Id));
      end Maybe_Traversal_Function;

      --  Local declarations

      Id : Entity_Id;
      --  Procedure or function entity for the subprogram

   --  Start of processing for Can_Be_Inlined_In_GNATprove_Mode

   begin
      pragma Assert (Present (Spec_Id) or else Present (Body_Id));

      if Present (Spec_Id) then
         Id := Spec_Id;
      else
         Id := Body_Id;
      end if;

      --  Only local subprograms without contracts are inlined in GNATprove
      --  mode, as these are the subprograms which a user is not interested in
      --  analyzing in isolation, but rather in the context of their call. This
      --  is a convenient convention, that could be changed for an explicit
      --  pragma/aspect one day.

      --  In a number of special cases, inlining is not desirable or not
      --  possible, see below.

      --  Do not inline unit-level subprograms

      if Is_Unit_Subprogram (Id) then
         return False;

      --  Do not inline subprograms declared in package specs, because they are
      --  not local, i.e. can be called either from anywhere (if declared in
      --  visible part) or from the child units (if declared in private part).

      elsif In_Package_Spec (Id) then
         return False;

      --  Do not inline subprograms declared in other units. This is important
      --  in particular for subprograms defined in the private part of a
      --  package spec, when analyzing one of its child packages, as otherwise
      --  we issue spurious messages about the impossibility to inline such
      --  calls.

      elsif not In_Extended_Main_Code_Unit (Id) then
         return False;

      --  Do not inline dispatching operations, as only their static calls
      --  can be analyzed in context, and not their dispatching calls.

      elsif Is_Dispatching_Operation (Id) then
         return False;

      --  Do not inline subprograms marked No_Return, possibly used for
      --  signaling errors, which GNATprove handles specially.

      elsif No_Return (Id) then
         return False;

      --  Do not inline subprograms that have a contract on the spec or the
      --  body. Use the contract(s) instead in GNATprove. This also prevents
      --  inlining of subprograms with Extensions_Visible or Volatile_Function.

      elsif (Present (Spec_Id) and then Has_Some_Contract (Spec_Id))
               or else
            (Present (Body_Id) and then Has_Some_Contract (Body_Id))
      then
         return False;

      --  Do not inline expression functions, which are directly inlined at the
      --  prover level.

      elsif (Present (Spec_Id) and then Is_Expression_Function (Spec_Id))
              or else
            (Present (Body_Id) and then Is_Expression_Function (Body_Id))
      then
         return False;

      --  Do not inline generic subprogram instances. The visibility rules of
      --  generic instances plays badly with inlining.

      elsif Is_Generic_Instance (Spec_Id) then
         return False;

      --  Only inline subprograms whose spec is marked SPARK_Mode On. For
      --  the subprogram body, a similar check is performed after the body
      --  is analyzed, as this is where a pragma SPARK_Mode might be inserted.

      elsif Present (Spec_Id)
        and then
          (No (SPARK_Pragma (Spec_Id))
            or else
           Get_SPARK_Mode_From_Annotation (SPARK_Pragma (Spec_Id)) /= On)
      then
         return False;

      --  Do not inline subprograms and entries defined inside protected types,
      --  which typically are not helper subprograms, which also avoids getting
      --  spurious messages on calls that cannot be inlined.

      elsif Within_Protected_Type (Id) then
         return False;

      --  Do not inline predicate functions (treated specially by GNATprove)

      elsif Is_Predicate_Function (Id) then
         return False;

      --  Do not inline subprograms with a parameter of an unconstrained
      --  record type if it has discrimiant dependent fields. Indeed, with
      --  such parameters, the frontend cannot always ensure type compliance
      --  in record component accesses (in particular with records containing
      --  packed arrays).

      elsif Has_Formal_With_Discriminant_Dependent_Fields (Id) then
         return False;

      --  Do not inline subprograms with a formal parameter or return type of
      --  a deep type, as in that case inlining might generate code that
      --  violates borrow-checking rules of SPARK 3.10 even if the original
      --  code did not.

      elsif Has_Formal_Or_Result_Of_Deep_Type (Id) then
         return False;

      --  Do not inline subprograms which may be traversal functions. Such
      --  inlining introduces temporary variables of named access type for
      --  which assignments are move instead of borrow/observe, possibly
      --  leading to spurious errors when checking SPARK rules related to
      --  pointer usage.

      elsif Maybe_Traversal_Function (Id) then
         return False;

      --  Do not inline subprograms with the Skip_Proof or Skip_Flow_And_Proof
      --  annotation, which should be handled separately.

      elsif Has_Skip_Proof_Annotation (Id) then
         return False;

      --  Otherwise, this is a subprogram declared inside the private part of a
      --  package, or inside a package body, or locally in a subprogram, and it
      --  does not have any contract. Inline it.

      else
         return True;
      end if;
   end Can_Be_Inlined_In_GNATprove_Mode;

   -------------------
   -- Cannot_Inline --
   -------------------

   procedure Cannot_Inline
     (Msg           : String;
      N             : Node_Id;
      Subp          : Entity_Id;
      Is_Serious    : Boolean := False;
      Suppress_Info : Boolean := False)
   is
   begin
      --  In GNATprove mode, inlining is the technical means by which the
      --  higher-level goal of contextual analysis is reached, so issue
      --  messages about failure to apply contextual analysis to a
      --  subprogram, rather than failure to inline it.

      if GNATprove_Mode
        and then Msg (Msg'First .. Msg'First + 12) = "cannot inline"
      then
         declare
            Len1 : constant Positive :=
              String (String'("cannot inline"))'Length;
            Len2 : constant Positive :=
              String (String'("info: no contextual analysis of"))'Length;

            New_Msg : String (1 .. Msg'Length + Len2 - Len1);

         begin
            New_Msg (1 .. Len2) := "info: no contextual analysis of";
            New_Msg (Len2 + 1 .. Msg'Length + Len2 - Len1) :=
              Msg (Msg'First + Len1 .. Msg'Last);
            Cannot_Inline (New_Msg, N, Subp, Is_Serious, Suppress_Info);
            return;
         end;
      end if;

      pragma Assert (Msg (Msg'Last) = '?');

      --  Legacy front-end inlining model

      if not Back_End_Inlining then

         --  Do not emit warning if this is a predefined unit which is not
         --  the main unit. With validity checks enabled, some predefined
         --  subprograms may contain nested subprograms and become ineligible
         --  for inlining.

         if Is_Predefined_Unit (Get_Source_Unit (Subp))
           and then not In_Extended_Main_Source_Unit (Subp)
         then
            null;

         --  In GNATprove mode, issue an info message when -gnatd_f is set and
         --  Suppress_Info is False, and indicate that the subprogram is not
         --  always inlined by setting flag Is_Inlined_Always to False.

         elsif GNATprove_Mode then
            Set_Is_Inlined_Always (Subp, False);

            if Debug_Flag_Underscore_F and not Suppress_Info then
               Error_Msg_NE (Msg, N, Subp);
            end if;

         elsif Has_Pragma_Inline_Always (Subp) then

            --  Remove last character (question mark) to make this into an
            --  error, because the Inline_Always pragma cannot be obeyed.

            Error_Msg_NE (Msg (Msg'First .. Msg'Last - 1), N, Subp);

         elsif Ineffective_Inline_Warnings then
            Error_Msg_NE (Msg & "p?", N, Subp);
         end if;

      --  New semantics relying on back-end inlining

      elsif Is_Serious then

         --  Remove last character (question mark) to make this into an error.

         Error_Msg_NE (Msg (Msg'First .. Msg'Last - 1), N, Subp);

      --  In GNATprove mode, issue an info message when -gnatd_f is set and
      --  Suppress_Info is False, and indicate that the subprogram is not
      --  always inlined by setting flag Is_Inlined_Always to False.

      elsif GNATprove_Mode then
         Set_Is_Inlined_Always (Subp, False);

         if Debug_Flag_Underscore_F and not Suppress_Info then
            Error_Msg_NE (Msg, N, Subp);
         end if;

      else

         --  Do not emit warning if this is a predefined unit which is not
         --  the main unit. This behavior is currently provided for backward
         --  compatibility but it will be removed when we enforce the
         --  strictness of the new rules.

         if Is_Predefined_Unit (Get_Source_Unit (Subp))
           and then not In_Extended_Main_Source_Unit (Subp)
         then
            null;

         elsif Has_Pragma_Inline_Always (Subp) then

            --  Emit a warning if this is a call to a runtime subprogram
            --  which is located inside a generic. Previously this call
            --  was silently skipped.

            if Is_Generic_Instance (Subp) then
               declare
                  Gen_P : constant Entity_Id := Generic_Parent (Parent (Subp));
               begin
                  if Is_Predefined_Unit (Get_Source_Unit (Gen_P)) then
                     Set_Is_Inlined (Subp, False);
                     Error_Msg_NE (Msg & "p?", N, Subp);
                     return;
                  end if;
               end;
            end if;

            --  Remove last character (question mark) to make this into an
            --  error, because the Inline_Always pragma cannot be obeyed.

            Error_Msg_NE (Msg (Msg'First .. Msg'Last - 1), N, Subp);

         else
            Set_Is_Inlined (Subp, False);

            if Ineffective_Inline_Warnings then
               Error_Msg_NE (Msg & "p?", N, Subp);
            end if;
         end if;
      end if;
   end Cannot_Inline;

   --------------------------------------------
   -- Check_And_Split_Unconstrained_Function --
   --------------------------------------------

   procedure Check_And_Split_Unconstrained_Function
     (N       : Node_Id;
      Spec_Id : Entity_Id;
      Body_Id : Entity_Id)
   is
      procedure Build_Body_To_Inline (N : Node_Id; Spec_Id : Entity_Id);
      --  Use generic machinery to build an unexpanded body for the subprogram.
      --  This body is subsequently used for inline expansions at call sites.

      procedure Build_Return_Object_Formal
        (Loc      : Source_Ptr;
         Obj_Decl : Node_Id;
         Formals  : List_Id);
      --  Create a formal parameter for return object declaration Obj_Decl of
      --  an extended return statement and add it to list Formals.

      function Can_Split_Unconstrained_Function (N : Node_Id) return Boolean;
      --  Return true if we generate code for the function body N, the function
      --  body N has no local declarations and its unique statement is a single
      --  extended return statement with a handled statements sequence.

      procedure Copy_Formals
        (Loc     : Source_Ptr;
         Subp_Id : Entity_Id;
         Formals : List_Id);
      --  Create new formal parameters from the formal parameters of subprogram
      --  Subp_Id and add them to list Formals.

      function Copy_Return_Object (Obj_Decl : Node_Id) return Node_Id;
      --  Create a copy of return object declaration Obj_Decl of an extended
      --  return statement.

      procedure Split_Unconstrained_Function
        (N       : Node_Id;
         Spec_Id : Entity_Id);
      --  N is an inlined function body that returns an unconstrained type and
      --  has a single extended return statement. Split N in two subprograms:
      --  a procedure P' and a function F'. The formals of P' duplicate the
      --  formals of N plus an extra formal which is used to return a value;
      --  its body is composed by the declarations and list of statements
      --  of the extended return statement of N.

      --------------------------
      -- Build_Body_To_Inline --
      --------------------------

      procedure Build_Body_To_Inline (N : Node_Id; Spec_Id : Entity_Id) is
         procedure Generate_Subprogram_Body
           (N              : Node_Id;
            Body_To_Inline : out Node_Id);
         --  Generate a parameterless duplicate of subprogram body N. Note that
         --  occurrences of pragmas referencing the formals are removed since
         --  they have no meaning when the body is inlined and the formals are
         --  rewritten (the analysis of the non-inlined body will handle these
         --  pragmas). A new internal name is associated with Body_To_Inline.

         ------------------------------
         -- Generate_Subprogram_Body --
         ------------------------------

         procedure Generate_Subprogram_Body
           (N              : Node_Id;
            Body_To_Inline : out Node_Id)
         is
         begin
            --  Within an instance, the body to inline must be treated as a
            --  nested generic so that proper global references are preserved.

            --  Note that we do not do this at the library level, because it
            --  is not needed, and furthermore this causes trouble if front
            --  end inlining is activated (-gnatN).

            if In_Instance
              and then Scope (Current_Scope) /= Standard_Standard
            then
               Body_To_Inline :=
                 Copy_Generic_Node (N, Empty, Instantiating => True);
            else
               Body_To_Inline := New_Copy_Tree (N);
            end if;

            --  Remove aspects/pragmas that have no meaning in an inlined body

            Remove_Aspects_And_Pragmas (Body_To_Inline);

            --  We need to capture references to the formals in order
            --  to substitute the actuals at the point of inlining, i.e.
            --  instantiation. To treat the formals as globals to the body to
            --  inline, we nest it within a dummy parameterless subprogram,
            --  declared within the real one.

            Set_Parameter_Specifications
              (Specification (Body_To_Inline), No_List);

            --  A new internal name is associated with Body_To_Inline to avoid
            --  conflicts when the non-inlined body N is analyzed.

            Set_Defining_Unit_Name (Specification (Body_To_Inline),
               Make_Temporary (Sloc (N), 'P'));
            Set_Corresponding_Spec (Body_To_Inline, Empty);
         end Generate_Subprogram_Body;

         --  Local variables

         Decl            : constant Node_Id := Unit_Declaration_Node (Spec_Id);
         Original_Body   : Node_Id;
         Body_To_Analyze : Node_Id;

      --  Start of processing for Build_Body_To_Inline

      begin
         pragma Assert (Current_Scope = Spec_Id);

         --  Within an instance, the body to inline must be treated as a nested
         --  generic, so that the proper global references are preserved. We
         --  do not do this at the library level, because it is not needed, and
         --  furthermore this causes trouble if front-end inlining is activated
         --  (-gnatN).

         if In_Instance
           and then Scope (Current_Scope) /= Standard_Standard
         then
            Save_Env (Scope (Current_Scope), Scope (Current_Scope));
         end if;

         --  Capture references to formals in order to substitute the actuals
         --  at the point of inlining or instantiation. To treat the formals
         --  as globals to the body to inline, nest the body within a dummy
         --  parameterless subprogram, declared within the real one.

         Generate_Subprogram_Body (N, Original_Body);
         Body_To_Analyze :=
           Copy_Generic_Node (Original_Body, Empty, Instantiating => False);

         --  Set return type of function, which is also global and does not
         --  need to be resolved.

         if Ekind (Spec_Id) = E_Function then
            Set_Result_Definition (Specification (Body_To_Analyze),
              New_Occurrence_Of (Etype (Spec_Id), Sloc (N)));
         end if;

         if No (Declarations (N)) then
            Set_Declarations (N, New_List (Body_To_Analyze));
         else
            Append_To (Declarations (N), Body_To_Analyze);
         end if;

         Preanalyze (Body_To_Analyze);

         Push_Scope (Defining_Entity (Body_To_Analyze));
         Save_Global_References (Original_Body);
         End_Scope;
         Remove (Body_To_Analyze);

         --  Restore environment if previously saved

         if In_Instance
           and then Scope (Current_Scope) /= Standard_Standard
         then
            Restore_Env;
         end if;

         pragma Assert (No (Body_To_Inline (Decl)));
         Set_Body_To_Inline (Decl, Original_Body);
         Mutate_Ekind (Defining_Entity (Original_Body), Ekind (Spec_Id));
      end Build_Body_To_Inline;

      --------------------------------
      -- Build_Return_Object_Formal --
      --------------------------------

      procedure Build_Return_Object_Formal
        (Loc      : Source_Ptr;
         Obj_Decl : Node_Id;
         Formals  : List_Id)
      is
         Obj_Def : constant Node_Id   := Object_Definition (Obj_Decl);
         Obj_Id  : constant Entity_Id := Defining_Entity   (Obj_Decl);
         Typ_Def : Node_Id;

      begin
         --  Build the type definition of the formal parameter. The use of
         --  New_Copy_Tree ensures that global references preserved in the
         --  case of generics.

         if Is_Entity_Name (Obj_Def) then
            Typ_Def := New_Copy_Tree (Obj_Def);
         else
            Typ_Def := New_Copy_Tree (Subtype_Mark (Obj_Def));
         end if;

         --  Generate:
         --
         --    Obj_Id : [out] Typ_Def

         --  Mode OUT should not be used when the return object is declared as
         --  a constant. Check the definition of the object declaration because
         --  the object has not been analyzed yet.

         Append_To (Formals,
           Make_Parameter_Specification (Loc,
             Defining_Identifier    =>
               Make_Defining_Identifier (Loc, Chars (Obj_Id)),
             In_Present             => False,
             Out_Present            => not Constant_Present (Obj_Decl),
             Null_Exclusion_Present => False,
             Parameter_Type         => Typ_Def));
      end Build_Return_Object_Formal;

      --------------------------------------
      -- Can_Split_Unconstrained_Function --
      --------------------------------------

      function Can_Split_Unconstrained_Function (N : Node_Id) return Boolean is
         Stmt : constant Node_Id :=
                  First (Statements (Handled_Statement_Sequence (N)));
         Decl : Node_Id;

      begin
         --  No user defined declarations allowed in the function except inside
         --  the unique return statement; implicit labels are the only allowed
         --  declarations.

         Decl := First (Declarations (N));
         while Present (Decl) loop
            if Nkind (Decl) /= N_Implicit_Label_Declaration then
               return False;
            end if;

            Next (Decl);
         end loop;

         --  We only split the inlined function when we are generating the code
         --  of its body; otherwise we leave duplicated split subprograms in
         --  the tree which (if referenced) generate wrong references at link
         --  time.

         return In_Extended_Main_Code_Unit (N)
           and then Present (Stmt)
           and then Nkind (Stmt) = N_Extended_Return_Statement
           and then No (Next (Stmt))
           and then Present (Handled_Statement_Sequence (Stmt));
      end Can_Split_Unconstrained_Function;

      ------------------
      -- Copy_Formals --
      ------------------

      procedure Copy_Formals
        (Loc     : Source_Ptr;
         Subp_Id : Entity_Id;
         Formals : List_Id)
      is
         Formal : Entity_Id;
         Spec   : Node_Id;

      begin
         Formal := First_Formal (Subp_Id);
         while Present (Formal) loop
            Spec := Parent (Formal);

            --  Create an exact copy of the formal parameter. The use of
            --  New_Copy_Tree ensures that global references are preserved
            --  in case of generics.

            Append_To (Formals,
              Make_Parameter_Specification (Loc,
                Defining_Identifier    =>
                  Make_Defining_Identifier (Sloc (Formal), Chars (Formal)),
                In_Present             => In_Present  (Spec),
                Out_Present            => Out_Present (Spec),
                Null_Exclusion_Present => Null_Exclusion_Present (Spec),
                Parameter_Type         =>
                  New_Copy_Tree (Parameter_Type (Spec)),
                Expression             => New_Copy_Tree (Expression (Spec))));

            Next_Formal (Formal);
         end loop;
      end Copy_Formals;

      ------------------------
      -- Copy_Return_Object --
      ------------------------

      function Copy_Return_Object (Obj_Decl : Node_Id) return Node_Id is
         Obj_Id : constant Entity_Id := Defining_Entity (Obj_Decl);

      begin
         --  The use of New_Copy_Tree ensures that global references are
         --  preserved in case of generics.

         return
           Make_Object_Declaration (Sloc (Obj_Decl),
             Defining_Identifier    =>
               Make_Defining_Identifier (Sloc (Obj_Id), Chars (Obj_Id)),
             Aliased_Present        => Aliased_Present  (Obj_Decl),
             Constant_Present       => Constant_Present (Obj_Decl),
             Null_Exclusion_Present => Null_Exclusion_Present (Obj_Decl),
             Object_Definition      =>
               New_Copy_Tree (Object_Definition (Obj_Decl)),
             Expression             => New_Copy_Tree (Expression (Obj_Decl)));
      end Copy_Return_Object;

      ----------------------------------
      -- Split_Unconstrained_Function --
      ----------------------------------

      procedure Split_Unconstrained_Function
        (N        : Node_Id;
         Spec_Id  : Entity_Id)
      is
         Loc      : constant Source_Ptr := Sloc (N);
         Ret_Stmt : constant Node_Id :=
                      First (Statements (Handled_Statement_Sequence (N)));
         Ret_Obj  : constant Node_Id :=
                      First (Return_Object_Declarations (Ret_Stmt));

         procedure Build_Procedure
           (Proc_Id   : out Entity_Id;
            Decl_List : out List_Id);
         --  Build a procedure containing the statements found in the extended
         --  return statement of the unconstrained function body N.

         ---------------------
         -- Build_Procedure --
         ---------------------

         procedure Build_Procedure
           (Proc_Id   : out Entity_Id;
            Decl_List : out List_Id)
         is
            Formals   : constant List_Id   := New_List;
            Subp_Name : constant Name_Id   := New_Internal_Name ('F');

            Body_Decls : List_Id := No_List;
            Decl       : Node_Id;
            Proc_Body  : Node_Id;
            Proc_Spec  : Node_Id;

         begin
            --  Create formal parameters for the return object and all formals
            --  of the unconstrained function in order to pass their values to
            --  the procedure.

            Build_Return_Object_Formal
              (Loc      => Loc,
               Obj_Decl => Ret_Obj,
               Formals  => Formals);

            Copy_Formals
              (Loc     => Loc,
               Subp_Id => Spec_Id,
               Formals => Formals);

            Proc_Id := Make_Defining_Identifier (Loc, Chars => Subp_Name);

            Proc_Spec :=
              Make_Procedure_Specification (Loc,
                Defining_Unit_Name       => Proc_Id,
                Parameter_Specifications => Formals);

            Decl_List := New_List;

            Append_To (Decl_List,
              Make_Subprogram_Declaration (Loc, Proc_Spec));

            --  Can_Convert_Unconstrained_Function checked that the function
            --  has no local declarations except implicit label declarations.
            --  Copy these declarations to the built procedure.

            if Present (Declarations (N)) then
               Body_Decls := New_List;

               Decl := First (Declarations (N));
               while Present (Decl) loop
                  pragma Assert (Nkind (Decl) = N_Implicit_Label_Declaration);

                  Append_To (Body_Decls,
                    Make_Implicit_Label_Declaration (Loc,
                      Make_Defining_Identifier (Loc,
                        Chars => Chars (Defining_Identifier (Decl))),
                      Label_Construct => Empty));

                  Next (Decl);
               end loop;
            end if;

            pragma Assert (Present (Handled_Statement_Sequence (Ret_Stmt)));

            Proc_Body :=
              Make_Subprogram_Body (Loc,
                Specification              => Copy_Subprogram_Spec (Proc_Spec),
                Declarations               => Body_Decls,
                Handled_Statement_Sequence =>
                  New_Copy_Tree (Handled_Statement_Sequence (Ret_Stmt)));

            Set_Defining_Unit_Name (Specification (Proc_Body),
               Make_Defining_Identifier (Loc, Subp_Name));

            Append_To (Decl_List, Proc_Body);
         end Build_Procedure;

         --  Local variables

         New_Obj   : constant Node_Id := Copy_Return_Object (Ret_Obj);
         Blk_Stmt  : Node_Id;
         Proc_Call : Node_Id;
         Proc_Id   : Entity_Id;

      --  Start of processing for Split_Unconstrained_Function

      begin
         --  Build the associated procedure, analyze it and insert it before
         --  the function body N.

         declare
            Scope     : constant Entity_Id := Current_Scope;
            Decl_List : List_Id;
         begin
            Pop_Scope;
            Build_Procedure (Proc_Id, Decl_List);
            Insert_Actions (N, Decl_List);
            Set_Is_Inlined (Proc_Id);
            Push_Scope (Scope);
         end;

         --  Build the call to the generated procedure

         declare
            Actual_List : constant List_Id := New_List;
            Formal      : Entity_Id;

         begin
            Append_To (Actual_List,
              New_Occurrence_Of (Defining_Identifier (New_Obj), Loc));

            Formal := First_Formal (Spec_Id);
            while Present (Formal) loop
               Append_To (Actual_List, New_Occurrence_Of (Formal, Loc));

               --  Avoid spurious warning on unreferenced formals

               Set_Referenced (Formal);
               Next_Formal (Formal);
            end loop;

            Proc_Call :=
              Make_Procedure_Call_Statement (Loc,
                Name                   => New_Occurrence_Of (Proc_Id, Loc),
                Parameter_Associations => Actual_List);
         end;

         --  Generate:

         --    declare
         --       New_Obj : ...
         --    begin
         --       Proc (New_Obj, ...);
         --       return New_Obj;
         --    end;

         Blk_Stmt :=
           Make_Block_Statement (Loc,
             Declarations               => New_List (New_Obj),
             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                 Statements => New_List (

                   Proc_Call,

                   Make_Simple_Return_Statement (Loc,
                     Expression =>
                       New_Occurrence_Of
                         (Defining_Identifier (New_Obj), Loc)))));

         Rewrite (Ret_Stmt, Blk_Stmt);
      end Split_Unconstrained_Function;

      --  Local variables

      Decl : constant Node_Id := Unit_Declaration_Node (Spec_Id);

   --  Start of processing for Check_And_Split_Unconstrained_Function

   begin
      pragma Assert (Back_End_Inlining
        and then Ekind (Spec_Id) = E_Function
        and then Returns_Unconstrained_Type (Spec_Id)
        and then Comes_From_Source (Body_Id)
        and then (Has_Pragma_Inline_Always (Spec_Id)
                    or else Optimization_Level > 0));

      --  This routine must not be used in GNATprove mode since GNATprove
      --  relies on frontend inlining

      pragma Assert (not GNATprove_Mode);

      --  No need to split the function if we cannot generate the code

      if Serious_Errors_Detected /= 0 then
         return;
      end if;

      --  No action needed in stubs since the attribute Body_To_Inline
      --  is not available

      if Nkind (Decl) = N_Subprogram_Body_Stub then
         return;

      --  Cannot build the body to inline if the attribute is already set.
      --  This attribute may have been set if this is a subprogram renaming
      --  declarations (see Freeze.Build_Renamed_Body).

      elsif Present (Body_To_Inline (Decl)) then
         return;

      --  Do not generate a body to inline for protected functions, because the
      --  transformation generates a call to a protected procedure, causing
      --  spurious errors. We don't inline protected operations anyway, so
      --  this is no loss. We might as well ignore intrinsics and foreign
      --  conventions as well -- just allow Ada conventions.

      elsif not (Convention (Spec_Id) = Convention_Ada
        or else Convention (Spec_Id) = Convention_Ada_Pass_By_Copy
        or else Convention (Spec_Id) = Convention_Ada_Pass_By_Reference)
      then
         return;

      --  Check excluded declarations

      elsif Has_Excluded_Declaration (Spec_Id, Declarations (N)) then
         return;

      --  Check excluded statements. There is no need to protect us against
      --  exception handlers since they are supported by the GCC backend.

      elsif Present (Handled_Statement_Sequence (N))
        and then Has_Excluded_Statement
                   (Spec_Id, Statements (Handled_Statement_Sequence (N)))
      then
         return;
      end if;

      --  Build the body to inline only if really needed

      if Can_Split_Unconstrained_Function (N) then
         Split_Unconstrained_Function (N, Spec_Id);
         Build_Body_To_Inline (N, Spec_Id);
         Set_Is_Inlined (Spec_Id);
      end if;
   end Check_And_Split_Unconstrained_Function;

   ---------------------------------------------
   -- Check_Object_Renaming_In_GNATprove_Mode --
   ---------------------------------------------

   procedure Check_Object_Renaming_In_GNATprove_Mode (Spec_Id : Entity_Id) is
      Decl      : constant Node_Id := Unit_Declaration_Node (Spec_Id);
      Body_Decl : constant Node_Id :=
        Unit_Declaration_Node (Corresponding_Body (Decl));

      function Check_Object_Renaming (N : Node_Id) return Traverse_Result;
      --  Returns Abandon on node N if this is a reference to an object
      --  renaming, which will be expanded into the renamed object in
      --  GNATprove mode.

      ---------------------------
      -- Check_Object_Renaming --
      ---------------------------

      function Check_Object_Renaming (N : Node_Id) return Traverse_Result is
      begin
         case Nkind (Original_Node (N)) is
            when N_Expanded_Name
               | N_Identifier
            =>
               declare
                  Obj_Id : constant Entity_Id := Entity (Original_Node (N));
               begin
                  --  Recognize the case when SPARK expansion rewrites a
                  --  reference to an object renaming.

                  if Present (Obj_Id)
                    and then Is_Object (Obj_Id)
                    and then Present (Renamed_Object (Obj_Id))
                    and then Nkind (Renamed_Object (Obj_Id)) not in N_Entity

                    --  Copy_Generic_Node called for inlining expects the
                    --  references to global entities to have the same kind
                    --  in the "generic" code and its "instantiation".

                    and then Nkind (Original_Node (N)) /=
                      Nkind (Renamed_Object (Obj_Id))
                  then
                     return Abandon;
                  else
                     return OK;
                  end if;
               end;

            when others =>
               return OK;
         end case;
      end Check_Object_Renaming;

      function Check_All_Object_Renamings is new
        Traverse_Func (Check_Object_Renaming);

   --  Start of processing for Check_Object_Renaming_In_GNATprove_Mode

   begin
      --  Subprograms with object renamings replaced by the special SPARK
      --  expansion cannot be inlined.

      if Check_All_Object_Renamings (Body_Decl) /= OK then
         Cannot_Inline ("cannot inline & (object renaming)?",
                        Body_Decl, Spec_Id);
         Set_Body_To_Inline (Decl, Empty);
      end if;
   end Check_Object_Renaming_In_GNATprove_Mode;

   -------------------------------------
   -- Check_Package_Body_For_Inlining --
   -------------------------------------

   procedure Check_Package_Body_For_Inlining (N : Node_Id; P : Entity_Id) is
      Bname : Unit_Name_Type;
      E     : Entity_Id;
      OK    : Boolean;

   begin
      --  Legacy implementation (relying on frontend inlining)

      if not Back_End_Inlining
        and then Is_Compilation_Unit (P)
        and then not Is_Generic_Instance (P)
      then
         Bname := Get_Body_Name (Get_Unit_Name (Unit (N)));

         E := First_Entity (P);
         while Present (E) loop
            if Has_Pragma_Inline_Always (E)
              or else (Has_Pragma_Inline (E) and Front_End_Inlining)
            then
               if not Is_Loaded (Bname) then
                  Load_Needed_Body (N, OK);

                  if OK then

                     --  Check we are not trying to inline a parent whose body
                     --  depends on a child, when we are compiling the body of
                     --  the child. Otherwise we have a potential elaboration
                     --  circularity with inlined subprograms and with
                     --  Taft-Amendment types.

                     declare
                        Comp        : Node_Id;      --  Body just compiled
                        Child_Spec  : Entity_Id;    --  Spec of main unit
                        Ent         : Entity_Id;    --  For iteration
                        With_Clause : Node_Id;      --  Context of body.

                     begin
                        if Nkind (Unit (Cunit (Main_Unit))) = N_Package_Body
                          and then Present (Body_Entity (P))
                        then
                           Child_Spec :=
                             Defining_Entity
                               ((Unit (Library_Unit (Cunit (Main_Unit)))));

                           Comp :=
                             Parent (Unit_Declaration_Node (Body_Entity (P)));

                           --  Check whether the context of the body just
                           --  compiled includes a child of itself, and that
                           --  child is the spec of the main compilation.

                           With_Clause := First (Context_Items (Comp));
                           while Present (With_Clause) loop
                              if Nkind (With_Clause) = N_With_Clause
                                and then
                                  Scope (Entity (Name (With_Clause))) = P
                                and then
                                  Entity (Name (With_Clause)) = Child_Spec
                              then
                                 Error_Msg_Node_2 := Child_Spec;
                                 Error_Msg_NE
                                   ("body of & depends on child unit&??",
                                    With_Clause, P);
                                 Error_Msg_N
                                   ("\subprograms in body cannot be inlined??",
                                    With_Clause);

                                 --  Disable further inlining from this unit,
                                 --  and keep Taft-amendment types incomplete.

                                 Ent := First_Entity (P);
                                 while Present (Ent) loop
                                    if Is_Type (Ent)
                                      and then Has_Completion_In_Body (Ent)
                                    then
                                       Set_Full_View (Ent, Empty);

                                    elsif Is_Subprogram (Ent) then
                                       Set_Is_Inlined (Ent, False);
                                    end if;

                                    Next_Entity (Ent);
                                 end loop;

                                 return;
                              end if;

                              Next (With_Clause);
                           end loop;
                        end if;
                     end;

                  elsif Ineffective_Inline_Warnings then
                     Error_Msg_Unit_1 := Bname;
                     Error_Msg_N
                       ("unable to inline subprograms defined in $?p?", P);
                     Error_Msg_N ("\body not found?p?", P);
                     return;
                  end if;
               end if;

               return;
            end if;

            Next_Entity (E);
         end loop;
      end if;
   end Check_Package_Body_For_Inlining;

   --------------------
   -- Cleanup_Scopes --
   --------------------

   procedure Cleanup_Scopes is
      Decl : Node_Id;
      Elmt : Elmt_Id;
      Fin  : Entity_Id;
      Kind : Entity_Kind;
      Scop : Entity_Id;

   begin
      Elmt := First_Elmt (To_Clean);
      while Present (Elmt) loop
         Scop := Node (Elmt);
         Kind := Ekind (Scop);

         if Kind = E_Block then
            Decl := Parent (Block_Node (Scop));

         else
            Decl := Unit_Declaration_Node (Scop);

            if Nkind (Decl) in N_Subprogram_Declaration
                             | N_Task_Type_Declaration
                             | N_Subprogram_Body_Stub
            then
               Decl := Unit_Declaration_Node (Corresponding_Body (Decl));
            end if;
         end if;

         --  Finalizers are built only for package specs and bodies that are
         --  compilation units, so check that we do not have anything else.
         --  Moreover, they must be built at most once for each entity during
         --  the compilation of the main unit. However, if other units are
         --  later compiled for inlining purposes, they may also contain body
         --  instances and, therefore, appear again here, so we need to make
         --  sure that we do not build two finalizers for them (note that the
         --  contents of the finalizer for these units is irrelevant since it
         --  is not output in the generated code).

         if Kind in E_Package | E_Package_Body then
            declare
               Unit_Entity : constant Entity_Id :=
                 (if Kind = E_Package then Scop else Spec_Entity (Scop));

            begin
               pragma Assert (Is_Compilation_Unit (Unit_Entity)
                 and then (No (Finalizer (Scop))
                            or else Unit_Entity /= Main_Unit_Entity));

               if No (Finalizer (Scop)) then
                  Build_Finalizer
                    (N           => Decl,
                     Clean_Stmts => No_List,
                     Mark_Id     => Empty,
                     Top_Decls   => No_List,
                     Defer_Abort => False,
                     Fin_Id      => Fin);

                  if Present (Fin) then
                     Set_Finalizer (Scop, Fin);
                  end if;
               end if;
            end;

         else
            Push_Scope (Scop);
            Expand_Cleanup_Actions (Decl);
            End_Scope;
         end if;

         Next_Elmt (Elmt);
      end loop;
   end Cleanup_Scopes;

   -----------------------------------------------
   -- Establish_Actual_Mapping_For_Inlined_Call --
   -----------------------------------------------

   procedure Establish_Actual_Mapping_For_Inlined_Call
     (N                     : Node_Id;
      Subp                  : Entity_Id;
      Decls                 : List_Id;
      Body_Or_Expr_To_Check : Node_Id)
   is

      function Formal_Is_Used_Once (Formal : Entity_Id) return Boolean;
      --  Determine whether a formal parameter is used only once in
      --  Body_Or_Expr_To_Check.

      -------------------------
      -- Formal_Is_Used_Once --
      -------------------------

      function Formal_Is_Used_Once (Formal : Entity_Id) return Boolean is
         Use_Counter : Nat := 0;

         function Count_Uses (N : Node_Id) return Traverse_Result;
         --  Traverse the tree and count the uses of the formal parameter.
         --  In this case, for optimization purposes, we do not need to
         --  continue the traversal once more than one use is encountered.

         ----------------
         -- Count_Uses --
         ----------------

         function Count_Uses (N : Node_Id) return Traverse_Result is
         begin
            --  The original node is an identifier

            if Nkind (N) = N_Identifier
              and then Present (Entity (N))

               --  Original node's entity points to the one in the copied body

              and then Nkind (Entity (N)) = N_Identifier
              and then Present (Entity (Entity (N)))

               --  The entity of the copied node is the formal parameter

              and then Entity (Entity (N)) = Formal
            then
               Use_Counter := Use_Counter + 1;

               --  If this is a second use then abandon the traversal

               if Use_Counter > 1 then
                  return Abandon;
               end if;
            end if;

            return OK;
         end Count_Uses;

         procedure Count_Formal_Uses is new Traverse_Proc (Count_Uses);

      --  Start of processing for Formal_Is_Used_Once

      begin
         Count_Formal_Uses (Body_Or_Expr_To_Check);
         return Use_Counter = 1;
      end Formal_Is_Used_Once;

      -- Local Data --

      F        : Entity_Id;
      A        : Node_Id;
      Decl     : Node_Id;
      Loc      : constant Source_Ptr := Sloc (N);
      New_A    : Node_Id;
      Temp     : Entity_Id;
      Temp_Typ : Entity_Id;

   --  Start of processing for Establish_Actual_Mapping_For_Inlined_Call

   begin
      F := First_Formal (Subp);
      A := First_Actual (N);
      while Present (F) loop
         if Present (Renamed_Object (F)) then

            --  If expander is active, it is an error to try to inline a
            --  recursive subprogram. In GNATprove mode, just indicate that the
            --  inlining will not happen, and mark the subprogram as not always
            --  inlined.

            if GNATprove_Mode then
               Cannot_Inline
                 ("cannot inline call to recursive subprogram?", N, Subp);
               Set_Is_Inlined_Always (Subp, False);
            else
               Error_Msg_N
                 ("cannot inline call to recursive subprogram", N);
            end if;

            return;
         end if;

         --  Reset Last_Assignment for any parameters of mode out or in out, to
         --  prevent spurious warnings about overwriting for assignments to the
         --  formal in the inlined code.

         if Is_Entity_Name (A) and then Ekind (F) /= E_In_Parameter then

            --  In GNATprove mode a protected component acting as an actual
            --  subprogram parameter will appear as inlined-for-proof. However,
            --  its E_Component entity is not an assignable object, so the
            --  assertion in Set_Last_Assignment will fail. We just omit the
            --  call to Set_Last_Assignment, because GNATprove flags useless
            --  assignments with its own flow analysis.
            --
            --  In GNAT mode such a problem does not occur, because protected
            --  components are inlined via object renamings whose entity kind
            --  E_Variable is assignable.

            if Is_Assignable (Entity (A)) then
               Set_Last_Assignment (Entity (A), Empty);
            else
               pragma Assert
                 (GNATprove_Mode and then Is_Protected_Component (Entity (A)));
            end if;
         end if;

         --  If the argument may be a controlling argument in a call within
         --  the inlined body, we must preserve its class-wide nature to ensure
         --  that dynamic dispatching will take place subsequently. If the
         --  formal has a constraint, then it must be preserved to retain the
         --  semantics of the body.

         if Is_Class_Wide_Type (Etype (F))
           or else (Is_Access_Type (Etype (F))
                     and then Is_Class_Wide_Type (Designated_Type (Etype (F))))
         then
            Temp_Typ := Etype (F);

         elsif Base_Type (Etype (F)) = Base_Type (Etype (A))
           and then Etype (F) /= Base_Type (Etype (F))
           and then Is_Constrained (Etype (F))
         then
            Temp_Typ := Etype (F);

         else
            Temp_Typ := Etype (A);
         end if;

         --  If the actual is a simple name or a literal, no need to create a
         --  temporary, object can be used directly. Skip this optimization in
         --  GNATprove mode, to make sure any check on a type conversion will
         --  be issued.

         if (Is_Entity_Name (A)
              and then
                (not Is_Scalar_Type (Etype (A))
                  or else Ekind (Entity (A)) = E_Enumeration_Literal)
              and then not GNATprove_Mode)

         --  When the actual is an identifier and the corresponding formal is
         --  used only once in the original body, the formal can be substituted
         --  directly with the actual parameter. Skip this optimization in
         --  GNATprove mode, to make sure any check on a type conversion
         --  will be issued.

           or else
             (Nkind (A) = N_Identifier
               and then Formal_Is_Used_Once (F)
               and then not GNATprove_Mode)

         --  If the actual is a literal and the formal has its address taken,
         --  we cannot pass the literal itself as an argument, so its value
         --  must be captured in a temporary.

           or else
             (Nkind (A) in
                N_Real_Literal | N_Integer_Literal | N_Character_Literal
               and then not Address_Taken (F))
         then
            if Etype (F) /= Etype (A) then
               Set_Renamed_Object
                 (F, Unchecked_Convert_To (Etype (F), Relocate_Node (A)));
            else
               Set_Renamed_Object (F, A);
            end if;

         else
            Temp := Make_Temporary (Loc, 'C');

            --  If the actual for an in/in-out parameter is a view conversion,
            --  make it into an unchecked conversion, given that an untagged
            --  type conversion is not a proper object for a renaming.

            --  In-out conversions that involve real conversions have already
            --  been transformed in Expand_Actuals.

            if Nkind (A) = N_Type_Conversion
              and then Ekind (F) /= E_In_Parameter
            then
               New_A := Unchecked_Convert_To (Etype (F), Expression (A));

            --  In GNATprove mode, keep the most precise type of the actual for
            --  the temporary variable, when the formal type is unconstrained.
            --  Otherwise, the AST may contain unexpected assignment statements
            --  to a temporary variable of unconstrained type renaming a local
            --  variable of constrained type, which is not expected by
            --  GNATprove.

            elsif Etype (F) /= Etype (A)
              and then (not GNATprove_Mode or else Is_Constrained (Etype (F)))
            then
               New_A    := Unchecked_Convert_To (Etype (F), Relocate_Node (A));
               Temp_Typ := Etype (F);

            else
               New_A := Relocate_Node (A);
            end if;

            Set_Sloc (New_A, Sloc (N));

            --  If the actual has a by-reference type, it cannot be copied,
            --  so its value is captured in a renaming declaration. Otherwise
            --  declare a local constant initialized with the actual.

            --  We also use a renaming declaration for expressions of an array
            --  type that is not bit-packed, both for efficiency reasons and to
            --  respect the semantics of the call: in most cases the original
            --  call will pass the parameter by reference, and thus the inlined
            --  code will have the same semantics.

            --  Finally, we need a renaming declaration in the case of limited
            --  types for which initialization cannot be by copy either.

            if Ekind (F) = E_In_Parameter
              and then not Is_By_Reference_Type (Etype (A))
              and then not Is_Limited_Type (Etype (A))
              and then
                (not Is_Array_Type (Etype (A))
                  or else not Is_Object_Reference (A)
                  or else Is_Bit_Packed_Array (Etype (A)))
            then
               Decl :=
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Temp,
                   Constant_Present    => True,
                   Object_Definition   => New_Occurrence_Of (Temp_Typ, Loc),
                   Expression          => New_A);

            else
               --  In GNATprove mode, make an explicit copy of input
               --  parameters when formal and actual types differ, to make
               --  sure any check on the type conversion will be issued.
               --  The legality of the copy is ensured by calling first
               --  Call_Can_Be_Inlined_In_GNATprove_Mode.

               if GNATprove_Mode
                 and then Ekind (F) /= E_Out_Parameter
                 and then not Same_Type (Etype (F), Etype (A))
               then
                  pragma Assert (not Is_By_Reference_Type (Etype (A)));
                  pragma Assert (not Is_Limited_Type (Etype (A)));

                  Append_To (Decls,
                    Make_Object_Declaration (Loc,
                      Defining_Identifier => Make_Temporary (Loc, 'C'),
                      Constant_Present    => True,
                      Object_Definition   => New_Occurrence_Of (Temp_Typ, Loc),
                      Expression          => New_Copy_Tree (New_A)));
               end if;

               Decl :=
                 Make_Object_Renaming_Declaration (Loc,
                   Defining_Identifier => Temp,
                   Subtype_Mark        => New_Occurrence_Of (Temp_Typ, Loc),
                   Name                => New_A);
            end if;

            Append (Decl, Decls);
            Set_Renamed_Object (F, Temp);
         end if;

         Next_Formal (F);
         Next_Actual (A);
      end loop;
   end Establish_Actual_Mapping_For_Inlined_Call;

   -------------------------
   -- Expand_Inlined_Call --
   -------------------------

   procedure Expand_Inlined_Call
    (N         : Node_Id;
     Subp      : Entity_Id;
     Orig_Subp : Entity_Id)
   is
      Decls     : constant List_Id    := New_List;
      Is_Predef : constant Boolean    :=
                    Is_Predefined_Unit (Get_Source_Unit (Subp));
      Loc       : constant Source_Ptr := Sloc (N);
      Orig_Bod  : constant Node_Id    :=
                    Body_To_Inline (Unit_Declaration_Node (Subp));

      Uses_Back_End : constant Boolean :=
                        Back_End_Inlining and then Optimization_Level > 0;
      --  The back-end expansion is used if the target supports back-end
      --  inlining and some level of optimixation is required; otherwise
      --  the inlining takes place fully as a tree expansion.

      Blk      : Node_Id;
      Decl     : Node_Id;
      Exit_Lab : Entity_Id := Empty;
      Lab_Decl : Node_Id   := Empty;
      Lab_Id   : Node_Id;
      Num_Ret  : Nat       := 0;
      Ret_Type : Entity_Id;
      Temp     : Entity_Id;

      Is_Unc      : Boolean;
      Is_Unc_Decl : Boolean;
      --  If the type returned by the function is unconstrained and the call
      --  can be inlined, special processing is required.

      Return_Object : Entity_Id := Empty;
      --  Entity in declaration in an extended_return_statement

      Targ : Node_Id := Empty;
      --  The target of the call. If context is an assignment statement then
      --  this is the left-hand side of the assignment, else it is a temporary
      --  to which the return value is assigned prior to rewriting the call.

      Targ1 : Node_Id := Empty;
      --  A separate target used when the return type is unconstrained

      procedure Declare_Postconditions_Result;
      --  When generating C code, declare _Result, which may be used in the
      --  inlined _Postconditions procedure to verify the return value.

      procedure Make_Exit_Label;
      --  Build declaration for exit label to be used in Return statements,
      --  sets Exit_Lab (the label node) and Lab_Decl (corresponding implicit
      --  declaration). Does nothing if Exit_Lab already set.

      procedure Make_Loop_Labels_Unique (HSS : Node_Id);
      --  When compiling for CCG and performing front-end inlining, replace
      --  loop names and references to them so that they do not conflict with
      --  homographs in the current subprogram.

      function Process_Formals (N : Node_Id) return Traverse_Result;
      --  Replace occurrence of a formal with the corresponding actual, or the
      --  thunk generated for it. Replace a return statement with an assignment
      --  to the target of the call, with appropriate conversions if needed.

      function Process_Formals_In_Aspects (N : Node_Id) return Traverse_Result;
      --  Because aspects are linked indirectly to the rest of the tree,
      --  replacement of formals appearing in aspect specifications must
      --  be performed in a separate pass, using an instantiation of the
      --  previous subprogram over aspect specifications reachable from N.

      function Process_Sloc (Nod : Node_Id) return Traverse_Result;
      --  If the call being expanded is that of an internal subprogram, set the
      --  sloc of the generated block to that of the call itself, so that the
      --  expansion is skipped by the "next" command in gdb. Same processing
      --  for a subprogram in a predefined file, e.g. Ada.Tags. If
      --  Debug_Generated_Code is true, suppress this change to simplify our
      --  own development. Same in GNATprove mode, to ensure that warnings and
      --  diagnostics point to the proper location.

      procedure Reset_Dispatching_Calls (N : Node_Id);
      --  In subtree N search for occurrences of dispatching calls that use the
      --  Ada 2005 Object.Operation notation and the object is a formal of the
      --  inlined subprogram. Reset the entity associated with Operation in all
      --  the found occurrences.

      procedure Rewrite_Function_Call (N : Node_Id; Blk : Node_Id);
      --  If the function body is a single expression, replace call with
      --  expression, else insert block appropriately.

      procedure Rewrite_Procedure_Call (N : Node_Id; Blk : Node_Id);
      --  If procedure body has no local variables, inline body without
      --  creating block, otherwise rewrite call with block.

      -----------------------------------
      -- Declare_Postconditions_Result --
      -----------------------------------

      procedure Declare_Postconditions_Result is
         Enclosing_Subp : constant Entity_Id := Scope (Subp);

      begin
         pragma Assert
           (Modify_Tree_For_C
             and then Is_Subprogram (Enclosing_Subp)
             and then Present (Wrapped_Statements (Enclosing_Subp)));

         if Ekind (Enclosing_Subp) = E_Function then
            if Nkind (First (Parameter_Associations (N))) in
                 N_Numeric_Or_String_Literal
            then
               Append_To (Declarations (Blk),
                 Make_Object_Declaration (Loc,
                   Defining_Identifier =>
                     Make_Defining_Identifier (Loc, Name_uResult),
                   Constant_Present    => True,
                   Object_Definition   =>
                     New_Occurrence_Of (Etype (Enclosing_Subp), Loc),
                   Expression          =>
                     New_Copy_Tree (First (Parameter_Associations (N)))));
            else
               Append_To (Declarations (Blk),
                 Make_Object_Renaming_Declaration (Loc,
                   Defining_Identifier =>
                     Make_Defining_Identifier (Loc, Name_uResult),
                   Subtype_Mark        =>
                     New_Occurrence_Of (Etype (Enclosing_Subp), Loc),
                   Name                =>
                     New_Copy_Tree (First (Parameter_Associations (N)))));
            end if;
         end if;
      end Declare_Postconditions_Result;

      ---------------------
      -- Make_Exit_Label --
      ---------------------

      procedure Make_Exit_Label is
         Lab_Ent : Entity_Id;
      begin
         if No (Exit_Lab) then
            Lab_Ent := Make_Temporary (Loc, 'L');
            Lab_Id  := New_Occurrence_Of (Lab_Ent, Loc);
            Exit_Lab := Make_Label (Loc, Lab_Id);
            Lab_Decl :=
              Make_Implicit_Label_Declaration (Loc,
                Defining_Identifier => Lab_Ent,
                Label_Construct     => Exit_Lab);
         end if;
      end Make_Exit_Label;

      -----------------------------
      -- Make_Loop_Labels_Unique --
      -----------------------------

      procedure Make_Loop_Labels_Unique (HSS : Node_Id) is
         function Process_Loop (N : Node_Id) return Traverse_Result;

         ------------------
         -- Process_Loop --
         ------------------

         function Process_Loop (N : Node_Id) return Traverse_Result is
            Id : Entity_Id;

         begin
            if Nkind (N) = N_Loop_Statement
              and then Present (Identifier (N))
            then
               --  Create new external name for loop and update the
               --  corresponding entity.

               Id := Entity (Identifier (N));
               Set_Chars (Id, New_External_Name (Chars (Id), 'L', -1));
               Set_Chars (Identifier (N), Chars (Id));

            elsif Nkind (N) = N_Exit_Statement
              and then Present (Name (N))
            then
               --  The exit statement must name an enclosing loop, whose name
               --  has already been updated.

               Set_Chars (Name (N), Chars (Entity (Name (N))));
            end if;

            return OK;
         end Process_Loop;

         procedure Update_Loop_Names is new Traverse_Proc (Process_Loop);

         --  Local variables

         Stmt : Node_Id;

      --  Start of processing for Make_Loop_Labels_Unique

      begin
         if Modify_Tree_For_C then
            Stmt := First (Statements (HSS));
            while Present (Stmt) loop
               Update_Loop_Names (Stmt);
               Next (Stmt);
            end loop;
         end if;
      end Make_Loop_Labels_Unique;

      ---------------------
      -- Process_Formals --
      ---------------------

      function Process_Formals (N : Node_Id) return Traverse_Result is
         A   : Entity_Id;
         E   : Entity_Id;
         Ret : Node_Id;

         Had_Private_View : Boolean;

      begin
         if Is_Entity_Name (N) and then Present (Entity (N)) then
            E := Entity (N);

            if Is_Formal (E) and then Scope (E) = Subp then
               A := Renamed_Object (E);

               --  Rewrite the occurrence of the formal into an occurrence of
               --  the actual. Also establish visibility on the proper view of
               --  the actual's subtype for the body's context (if the actual's
               --  subtype is private at the call point but its full view is
               --  visible to the body, then the inlined tree here must be
               --  analyzed with the full view).
               --
               --  The Has_Private_View flag is cleared by rewriting, so it
               --  must be explicitly saved and restored, just like when
               --  instantiating the body to inline.

               if Is_Entity_Name (A) then
                  Had_Private_View := Has_Private_View (N);
                  Rewrite (N, New_Occurrence_Of (Entity (A), Sloc (N)));
                  Set_Has_Private_View (N, Had_Private_View);
                  Check_Private_View (N);

               elsif Nkind (A) = N_Defining_Identifier then
                  Had_Private_View := Has_Private_View (N);
                  Rewrite (N, New_Occurrence_Of (A, Sloc (N)));
                  Set_Has_Private_View (N, Had_Private_View);
                  Check_Private_View (N);

               --  Numeric literal

               else
                  Rewrite (N, New_Copy (A));
               end if;
            end if;

            return Skip;

         elsif Is_Entity_Name (N)
           and then Present (Return_Object)
           and then Chars (N) = Chars (Return_Object)
         then
            --  Occurrence within an extended return statement. The return
            --  object is local to the body been inlined, and thus the generic
            --  copy is not analyzed yet, so we match by name, and replace it
            --  with target of call.

            if Nkind (Targ) = N_Defining_Identifier then
               Rewrite (N, New_Occurrence_Of (Targ, Loc));
            else
               Rewrite (N, New_Copy_Tree (Targ));
            end if;

            return Skip;

         elsif Nkind (N) = N_Simple_Return_Statement then
            if No (Expression (N)) then
               Num_Ret := Num_Ret + 1;
               Make_Exit_Label;
               Rewrite (N,
                 Make_Goto_Statement (Loc, Name => New_Copy (Lab_Id)));

            else
               if Nkind (Parent (N)) = N_Handled_Sequence_Of_Statements
                 and then Nkind (Parent (Parent (N))) = N_Subprogram_Body
               then
                  --  Function body is a single expression. No need for
                  --  exit label.

                  null;

               else
                  Num_Ret := Num_Ret + 1;
                  Make_Exit_Label;
               end if;

               --  Because of the presence of private types, the views of the
               --  expression and the context may be different, so place
               --  a type conversion to the context type to avoid spurious
               --  errors, e.g. when the expression is a numeric literal and
               --  the context is private. If the expression is an aggregate,
               --  use a qualified expression, because an aggregate is not a
               --  legal argument of a conversion. Ditto for numeric, character
               --  and string literals, and attributes that yield a universal
               --  type, because those must be resolved to a specific type.

               if Nkind (Expression (N)) in N_Aggregate
                                          | N_Character_Literal
                                          | N_Null
                                          | N_String_Literal
                 or else Yields_Universal_Type (Expression (N))
               then
                  Ret :=
                    Make_Qualified_Expression (Sloc (N),
                      Subtype_Mark => New_Occurrence_Of (Ret_Type, Sloc (N)),
                      Expression   => Relocate_Node (Expression (N)));

               --  Use an unchecked type conversion between access types, for
               --  which a type conversion would not always be valid, as no
               --  check may result from the conversion.

               elsif Is_Access_Type (Ret_Type) then
                  Ret :=
                    Unchecked_Convert_To
                      (Ret_Type, Relocate_Node (Expression (N)));

               --  Otherwise use a type conversion, which may trigger a check

               else
                  Ret :=
                    Make_Type_Conversion (Sloc (N),
                      Subtype_Mark => New_Occurrence_Of (Ret_Type, Sloc (N)),
                      Expression   => Relocate_Node (Expression (N)));
               end if;

               if Nkind (Targ) = N_Defining_Identifier then
                  Rewrite (N,
                    Make_Assignment_Statement (Loc,
                      Name       => New_Occurrence_Of (Targ, Loc),
                      Expression => Ret));
               else
                  Rewrite (N,
                    Make_Assignment_Statement (Loc,
                      Name       => New_Copy (Targ),
                      Expression => Ret));
               end if;

               Set_Assignment_OK (Name (N));

               if Present (Exit_Lab) then
                  Insert_After (N,
                    Make_Goto_Statement (Loc, Name => New_Copy (Lab_Id)));
               end if;
            end if;

            return OK;

         --  An extended return becomes a block whose first statement is the
         --  assignment of the initial expression of the return object to the
         --  target of the call itself.

         elsif Nkind (N) = N_Extended_Return_Statement then
            declare
               Return_Decl : constant Entity_Id :=
                               First (Return_Object_Declarations (N));
               Assign      : Node_Id;

            begin
               Return_Object := Defining_Identifier (Return_Decl);

               if Present (Expression (Return_Decl)) then
                  if Nkind (Targ) = N_Defining_Identifier then
                     Assign :=
                       Make_Assignment_Statement (Loc,
                         Name       => New_Occurrence_Of (Targ, Loc),
                         Expression => Expression (Return_Decl));
                  else
                     Assign :=
                       Make_Assignment_Statement (Loc,
                         Name       => New_Copy (Targ),
                         Expression => Expression (Return_Decl));
                  end if;

                  Set_Assignment_OK (Name (Assign));

                  if No (Handled_Statement_Sequence (N)) then
                     Set_Handled_Statement_Sequence (N,
                       Make_Handled_Sequence_Of_Statements (Loc,
                         Statements => New_List));
                  end if;

                  Prepend (Assign,
                    Statements (Handled_Statement_Sequence (N)));
               end if;

               Rewrite (N,
                 Make_Block_Statement (Loc,
                    Handled_Statement_Sequence =>
                      Handled_Statement_Sequence (N)));

               return OK;
            end;

         --  Remove pragma Unreferenced since it may refer to formals that
         --  are not visible in the inlined body, and in any case we will
         --  not be posting warnings on the inlined body so it is unneeded.

         elsif Nkind (N) = N_Pragma
           and then Pragma_Name (N) = Name_Unreferenced
         then
            Rewrite (N, Make_Null_Statement (Sloc (N)));
            return OK;

         else
            return OK;
         end if;
      end Process_Formals;

      procedure Replace_Formals is new Traverse_Proc (Process_Formals);

      --------------------------------
      -- Process_Formals_In_Aspects --
      --------------------------------

      function Process_Formals_In_Aspects
        (N : Node_Id) return Traverse_Result
      is
         A : Node_Id;

      begin
         if Has_Aspects (N) then
            A := First (Aspect_Specifications (N));
            while Present (A) loop
               Replace_Formals (Expression (A));

               Next (A);
            end loop;
         end if;
         return OK;
      end Process_Formals_In_Aspects;

      procedure Replace_Formals_In_Aspects is
        new Traverse_Proc (Process_Formals_In_Aspects);

      ------------------
      -- Process_Sloc --
      ------------------

      function Process_Sloc (Nod : Node_Id) return Traverse_Result is
      begin
         if not Debug_Generated_Code then
            Set_Sloc (Nod, Sloc (N));
            Set_Comes_From_Source (Nod, False);
         end if;

         return OK;
      end Process_Sloc;

      procedure Reset_Slocs is new Traverse_Proc (Process_Sloc);

      ------------------------------
      --  Reset_Dispatching_Calls --
      ------------------------------

      procedure Reset_Dispatching_Calls (N : Node_Id) is

         function Do_Reset (N : Node_Id) return Traverse_Result;

         --------------
         -- Do_Reset --
         --------------

         function Do_Reset (N : Node_Id) return Traverse_Result is
         begin
            if Nkind (N) = N_Procedure_Call_Statement
              and then Nkind (Name (N)) = N_Selected_Component
              and then Nkind (Prefix (Name (N))) = N_Identifier
              and then Is_Formal (Entity (Prefix (Name (N))))
              and then Is_Dispatching_Operation
                         (Entity (Selector_Name (Name (N))))
            then
               Set_Entity (Selector_Name (Name (N)), Empty);
            end if;

            return OK;
         end Do_Reset;

         procedure Do_Reset_Calls is new Traverse_Proc (Do_Reset);

      begin
         Do_Reset_Calls (N);
      end Reset_Dispatching_Calls;

      ---------------------------
      -- Rewrite_Function_Call --
      ---------------------------

      procedure Rewrite_Function_Call (N : Node_Id; Blk : Node_Id) is
         HSS : constant Node_Id := Handled_Statement_Sequence (Blk);
         Fst : constant Node_Id := First (Statements (HSS));

      begin
         Make_Loop_Labels_Unique (HSS);

         --  Optimize simple case: function body is a single return statement,
         --  which has been expanded into an assignment.

         if Is_Empty_List (Declarations (Blk))
           and then Nkind (Fst) = N_Assignment_Statement
           and then No (Next (Fst))
         then
            --  The function call may have been rewritten as the temporary
            --  that holds the result of the call, in which case remove the
            --  now useless declaration.

            if Nkind (N) = N_Identifier
              and then Nkind (Parent (Entity (N))) = N_Object_Declaration
            then
               Rewrite (Parent (Entity (N)), Make_Null_Statement (Loc));
            end if;

            Rewrite (N, Expression (Fst));

         elsif Nkind (N) = N_Identifier
           and then Nkind (Parent (Entity (N))) = N_Object_Declaration
         then
            --  The block assigns the result of the call to the temporary

            Insert_After (Parent (Entity (N)), Blk);

         --  If the context is an assignment, and the left-hand side is free of
         --  side-effects, the replacement is also safe.

         elsif Nkind (Parent (N)) = N_Assignment_Statement
           and then
            (Is_Entity_Name (Name (Parent (N)))
              or else
                (Nkind (Name (Parent (N))) = N_Explicit_Dereference
                  and then Is_Entity_Name (Prefix (Name (Parent (N)))))

              or else
                (Nkind (Name (Parent (N))) = N_Selected_Component
                  and then Is_Entity_Name (Prefix (Name (Parent (N))))))
         then
            --  Replace assignment with the block

            declare
               Original_Assignment : constant Node_Id := Parent (N);

            begin
               --  Preserve the original assignment node to keep the complete
               --  assignment subtree consistent enough for Analyze_Assignment
               --  to proceed (specifically, the original Lhs node must still
               --  have an assignment statement as its parent).

               --  We cannot rely on Original_Node to go back from the block
               --  node to the assignment node, because the assignment might
               --  already be a rewrite substitution.

               Discard_Node (Relocate_Node (Original_Assignment));
               Rewrite (Original_Assignment, Blk);
            end;

         elsif Nkind (Parent (N)) = N_Object_Declaration then

            --  A call to a function which returns an unconstrained type
            --  found in the expression initializing an object-declaration is
            --  expanded into a procedure call which must be added after the
            --  object declaration.

            if Is_Unc_Decl and Back_End_Inlining then
               Insert_Action_After (Parent (N), Blk);
            else
               Set_Expression (Parent (N), Empty);
               Insert_After (Parent (N), Blk);
            end if;

         elsif Is_Unc and then not Back_End_Inlining then
            Insert_Before (Parent (N), Blk);
         end if;
      end Rewrite_Function_Call;

      ----------------------------
      -- Rewrite_Procedure_Call --
      ----------------------------

      procedure Rewrite_Procedure_Call (N : Node_Id; Blk : Node_Id) is
         HSS : constant Node_Id := Handled_Statement_Sequence (Blk);

      begin
         Make_Loop_Labels_Unique (HSS);

         --  If there is a transient scope for N, this will be the scope of the
         --  actions for N, and the statements in Blk need to be within this
         --  scope. For example, they need to have visibility on the constant
         --  declarations created for the formals.

         --  If N needs no transient scope, and if there are no declarations in
         --  the inlined body, we can do a little optimization and insert the
         --  statements for the body directly after N, and rewrite N to a
         --  null statement, instead of rewriting N into a full-blown block
         --  statement.

         if not Scope_Is_Transient
           and then Is_Empty_List (Declarations (Blk))
         then
            Insert_List_After (N, Statements (HSS));
            Rewrite (N, Make_Null_Statement (Loc));
         else
            Rewrite (N, Blk);
         end if;
      end Rewrite_Procedure_Call;

   --  Start of processing for Expand_Inlined_Call

   begin
      --  Initializations for old/new semantics

      if not Uses_Back_End then
         Is_Unc      := Is_Array_Type (Etype (Subp))
                          and then not Is_Constrained (Etype (Subp));
         Is_Unc_Decl := False;
      else
         Is_Unc      := Returns_Unconstrained_Type (Subp)
                          and then Optimization_Level > 0;
         Is_Unc_Decl := Nkind (Parent (N)) = N_Object_Declaration
                          and then Is_Unc;
      end if;

      --  Check for an illegal attempt to inline a recursive procedure. If the
      --  subprogram has parameters this is detected when trying to supply a
      --  binding for parameters that already have one. For parameterless
      --  subprograms this must be done explicitly.

      if In_Open_Scopes (Subp) then
         Cannot_Inline
           ("cannot inline call to recursive subprogram?", N, Subp);
         Set_Is_Inlined (Subp, False);
         return;

      --  Skip inlining if this is not a true inlining since the attribute
      --  Body_To_Inline is also set for renamings (see sinfo.ads). For a
      --  true inlining, Orig_Bod has code rather than being an entity.

      elsif Nkind (Orig_Bod) in N_Entity then
         return;
      end if;

      if Nkind (Orig_Bod) in N_Defining_Identifier
                           | N_Defining_Operator_Symbol
      then
         --  Subprogram is renaming_as_body. Calls occurring after the renaming
         --  can be replaced with calls to the renamed entity directly, because
         --  the subprograms are subtype conformant. If the renamed subprogram
         --  is an inherited operation, we must redo the expansion because
         --  implicit conversions may be needed. Similarly, if the renamed
         --  entity is inlined, expand the call for further optimizations.

         Set_Name (N, New_Occurrence_Of (Orig_Bod, Loc));

         if Present (Alias (Orig_Bod)) or else Is_Inlined (Orig_Bod) then
            Expand_Call (N);
         end if;

         return;
      end if;

      --  Register the call in the list of inlined calls

      Append_New_Elmt (N, To => Inlined_Calls);

      --  Use generic machinery to copy body of inlined subprogram, as if it
      --  were an instantiation, resetting source locations appropriately, so
      --  that nested inlined calls appear in the main unit.

      Save_Env (Subp, Empty);
      Set_Copied_Sloc_For_Inlined_Body (N, Defining_Entity (Orig_Bod));

      --  Old semantics

      if not Uses_Back_End then
         declare
            Bod : Node_Id;

         begin
            Bod := Copy_Generic_Node (Orig_Bod, Empty, Instantiating => True);
            Blk :=
              Make_Block_Statement (Loc,
                Declarations               => Declarations (Bod),
                Handled_Statement_Sequence =>
                  Handled_Statement_Sequence (Bod));

            if No (Declarations (Bod)) then
               Set_Declarations (Blk, New_List);
            end if;

            --  When generating C code, declare _Result, which may be used to
            --  verify the return value.

            if Modify_Tree_For_C
              and then Nkind (N) = N_Procedure_Call_Statement
              and then Chars (Name (N)) = Name_uWrapped_Statements
            then
               Declare_Postconditions_Result;
            end if;

            --  For the unconstrained case, capture the name of the local
            --  variable that holds the result. This must be the first
            --  declaration in the block, because its bounds cannot depend
            --  on local variables. Otherwise there is no way to declare the
            --  result outside of the block. Needless to say, in general the
            --  bounds will depend on the actuals in the call.

            --  If the context is an assignment statement, as is the case
            --  for the expansion of an extended return, the left-hand side
            --  provides bounds even if the return type is unconstrained.

            if Is_Unc then
               declare
                  First_Decl : Node_Id;

               begin
                  First_Decl := First (Declarations (Blk));

                  --  If the body is a single extended return statement,the
                  --  resulting block is a nested block.

                  if No (First_Decl) then
                     First_Decl :=
                       First (Statements (Handled_Statement_Sequence (Blk)));

                     if Nkind (First_Decl) = N_Block_Statement then
                        First_Decl := First (Declarations (First_Decl));
                     end if;
                  end if;

                  --  No front-end inlining possible

                  if Nkind (First_Decl) /= N_Object_Declaration then
                     return;
                  end if;

                  if Nkind (Parent (N)) /= N_Assignment_Statement then
                     Targ1 := Defining_Identifier (First_Decl);
                  else
                     Targ1 := Name (Parent (N));
                  end if;
               end;
            end if;
         end;

      --  New semantics

      else
         declare
            Bod : Node_Id;

         begin
            --  General case

            if not Is_Unc then
               Bod :=
                 Copy_Generic_Node (Orig_Bod, Empty, Instantiating => True);
               Blk :=
                 Make_Block_Statement (Loc,
                   Declarations               => Declarations (Bod),
                   Handled_Statement_Sequence =>
                     Handled_Statement_Sequence (Bod));

            --  Inline a call to a function that returns an unconstrained type.
            --  The semantic analyzer checked that frontend-inlined functions
            --  returning unconstrained types have no declarations and have
            --  a single extended return statement. As part of its processing
            --  the function was split into two subprograms: a procedure P' and
            --  a function F' that has a block with a call to procedure P' (see
            --  Split_Unconstrained_Function).

            else
               pragma Assert
                 (Nkind
                   (First
                     (Statements (Handled_Statement_Sequence (Orig_Bod)))) =
                                                         N_Block_Statement);

               declare
                  Blk_Stmt    : constant Node_Id :=
                    First (Statements (Handled_Statement_Sequence (Orig_Bod)));
                  First_Stmt  : constant Node_Id :=
                    First (Statements (Handled_Statement_Sequence (Blk_Stmt)));
                  Second_Stmt : constant Node_Id := Next (First_Stmt);

               begin
                  pragma Assert
                    (Nkind (First_Stmt) = N_Procedure_Call_Statement
                      and then Nkind (Second_Stmt) = N_Simple_Return_Statement
                      and then No (Next (Second_Stmt)));

                  Bod :=
                    Copy_Generic_Node
                      (First
                        (Statements (Handled_Statement_Sequence (Orig_Bod))),
                       Empty, Instantiating => True);
                  Blk := Bod;

                  --  Capture the name of the local variable that holds the
                  --  result. This must be the first declaration in the block,
                  --  because its bounds cannot depend on local variables.
                  --  Otherwise there is no way to declare the result outside
                  --  of the block. Needless to say, in general the bounds will
                  --  depend on the actuals in the call.

                  if Nkind (Parent (N)) /= N_Assignment_Statement then
                     Targ1 := Defining_Identifier (First (Declarations (Blk)));

                  --  If the context is an assignment statement, as is the case
                  --  for the expansion of an extended return, the left-hand
                  --  side provides bounds even if the return type is
                  --  unconstrained.

                  else
                     Targ1 := Name (Parent (N));
                  end if;
               end;
            end if;

            if No (Declarations (Bod)) then
               Set_Declarations (Blk, New_List);
            end if;
         end;
      end if;

      --  If this is a derived function, establish the proper return type

      if Present (Orig_Subp) and then Orig_Subp /= Subp then
         Ret_Type := Etype (Orig_Subp);
      else
         Ret_Type := Etype (Subp);
      end if;

      --  Create temporaries for the actuals that are expressions, or that are
      --  scalars and require copying to preserve semantics.

      Establish_Actual_Mapping_For_Inlined_Call (N, Subp, Decls, Orig_Bod);

      --  Establish target of function call. If context is not assignment or
      --  declaration, create a temporary as a target. The declaration for the
      --  temporary may be subsequently optimized away if the body is a single
      --  expression, or if the left-hand side of the assignment is simple
      --  enough, i.e. an entity or an explicit dereference of one.

      if Ekind (Subp) = E_Function then
         if Nkind (Parent (N)) = N_Assignment_Statement
           and then Is_Entity_Name (Name (Parent (N)))
         then
            Targ := Name (Parent (N));

         elsif Nkind (Parent (N)) = N_Assignment_Statement
           and then Nkind (Name (Parent (N))) = N_Explicit_Dereference
           and then Is_Entity_Name (Prefix (Name (Parent (N))))
         then
            Targ := Name (Parent (N));

         elsif Nkind (Parent (N)) = N_Assignment_Statement
           and then Nkind (Name (Parent (N))) = N_Selected_Component
           and then Is_Entity_Name (Prefix (Name (Parent (N))))
         then
            Targ := New_Copy_Tree (Name (Parent (N)));

         elsif Nkind (Parent (N)) = N_Object_Declaration
           and then Is_Limited_Type (Etype (Subp))
         then
            Targ := Defining_Identifier (Parent (N));

         --  New semantics: In an object declaration avoid an extra copy
         --  of the result of a call to an inlined function that returns
         --  an unconstrained type

         elsif Uses_Back_End
           and then Nkind (Parent (N)) = N_Object_Declaration
           and then Is_Unc
         then
            Targ := Defining_Identifier (Parent (N));

         else
            --  Replace call with temporary and create its declaration

            Temp := Make_Temporary (Loc, 'C');
            Set_Is_Internal (Temp);

            --  For the unconstrained case, the generated temporary has the
            --  same constrained declaration as the result variable. It may
            --  eventually be possible to remove that temporary and use the
            --  result variable directly.

            if Is_Unc and then Nkind (Parent (N)) /= N_Assignment_Statement
            then
               Decl :=
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Temp,
                   Object_Definition   =>
                     New_Copy_Tree (Object_Definition (Parent (Targ1))));

               Replace_Formals (Decl);

            else
               Decl :=
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Temp,
                   Object_Definition   => New_Occurrence_Of (Ret_Type, Loc));

               Set_Etype (Temp, Ret_Type);
            end if;

            Set_No_Initialization (Decl);
            Append (Decl, Decls);
            Rewrite (N, New_Occurrence_Of (Temp, Loc));
            Targ := Temp;
         end if;
      end if;

      Insert_Actions (N, Decls);

      if Is_Unc_Decl then

         --  Special management for inlining a call to a function that returns
         --  an unconstrained type and initializes an object declaration: we
         --  avoid generating undesired extra calls and goto statements.

         --     Given:
         --                 function Func (...) return String is
         --                 begin
         --                    declare
         --                       Result : String (1 .. 4);
         --                    begin
         --                       Proc (Result, ...);
         --                       return Result;
         --                    end;
         --                 end Func;

         --                 Result : String := Func (...);

         --     Replace this object declaration by:

         --                 Result : String (1 .. 4);
         --                 Proc (Result, ...);

         Remove_Homonym (Targ);

         Decl :=
           Make_Object_Declaration
             (Loc,
              Defining_Identifier => Targ,
              Object_Definition   =>
                New_Copy_Tree (Object_Definition (Parent (Targ1))));
         Replace_Formals (Decl);
         Rewrite (Parent (N), Decl);
         Analyze (Parent (N));

         --  Avoid spurious warnings since we know that this declaration is
         --  referenced by the procedure call.

         Set_Never_Set_In_Source (Targ, False);

         --  Remove the local declaration of the extended return stmt from the
         --  inlined code

         Remove (Parent (Targ1));

         --  Update the reference to the result (since we have rewriten the
         --  object declaration)

         declare
            Blk_Call_Stmt : Node_Id;

         begin
            --  Capture the call to the procedure

            Blk_Call_Stmt :=
              First (Statements (Handled_Statement_Sequence (Blk)));
            pragma Assert
              (Nkind (Blk_Call_Stmt) = N_Procedure_Call_Statement);

            Remove (First (Parameter_Associations (Blk_Call_Stmt)));
            Prepend_To (Parameter_Associations (Blk_Call_Stmt),
              New_Occurrence_Of (Targ, Loc));
         end;

         --  Remove the return statement

         pragma Assert
           (Nkind (Last (Statements (Handled_Statement_Sequence (Blk)))) =
                                                   N_Simple_Return_Statement);

         Remove (Last (Statements (Handled_Statement_Sequence (Blk))));
      end if;

      --  Traverse the tree and replace formals with actuals or their thunks.
      --  Attach block to tree before analysis and rewriting.

      Replace_Formals (Blk);
      Replace_Formals_In_Aspects (Blk);
      Set_Parent (Blk, N);

      if GNATprove_Mode then
         null;

      elsif not Comes_From_Source (Subp) or else Is_Predef then
         Reset_Slocs (Blk);
      end if;

      if Is_Unc_Decl then

         --  No action needed since return statement has been already removed

         null;

      elsif Present (Exit_Lab) then

         --  If there's a single return statement at the end of the subprogram,
         --  the corresponding goto statement and the corresponding label are
         --  useless.

         if Num_Ret = 1
           and then
             Nkind (Last (Statements (Handled_Statement_Sequence (Blk)))) =
                                                            N_Goto_Statement
         then
            Remove (Last (Statements (Handled_Statement_Sequence (Blk))));
         else
            Append (Lab_Decl, (Declarations (Blk)));
            Append (Exit_Lab, Statements (Handled_Statement_Sequence (Blk)));
         end if;
      end if;

      --  Analyze Blk with In_Inlined_Body set, to avoid spurious errors
      --  on conflicting private views that Gigi would ignore. If this is a
      --  predefined unit, analyze with checks off, as is done in the non-
      --  inlined run-time units.

      declare
         I_Flag : constant Boolean := In_Inlined_Body;

      begin
         In_Inlined_Body := True;

         if Is_Predef then
            declare
               Style : constant Boolean := Style_Check;

            begin
               Style_Check := False;

               --  Search for dispatching calls that use the Object.Operation
               --  notation using an Object that is a parameter of the inlined
               --  function. We reset the decoration of Operation to force
               --  the reanalysis of the inlined dispatching call because
               --  the actual object has been inlined.

               Reset_Dispatching_Calls (Blk);

               --  In GNATprove mode, always consider checks on, even for
               --  predefined units.

               if GNATprove_Mode then
                  Analyze (Blk);
               else
                  Analyze (Blk, Suppress => All_Checks);
               end if;

               Style_Check := Style;
            end;

         else
            Analyze (Blk);
         end if;

         In_Inlined_Body := I_Flag;
      end;

      if Ekind (Subp) = E_Procedure then
         Rewrite_Procedure_Call (N, Blk);

      else
         Rewrite_Function_Call (N, Blk);

         if Is_Unc_Decl then
            null;

         --  For the unconstrained case, the replacement of the call has been
         --  made prior to the complete analysis of the generated declarations.
         --  Propagate the proper type now.

         elsif Is_Unc then
            if Nkind (N) = N_Identifier then
               Set_Etype (N, Etype (Entity (N)));
            else
               Set_Etype (N, Etype (Targ1));
            end if;
         end if;
      end if;

      Restore_Env;

      --  Cleanup mapping between formals and actuals for other expansions

      Reset_Actual_Mapping_For_Inlined_Call (Subp);
   end Expand_Inlined_Call;

   --------------------------
   -- Get_Code_Unit_Entity --
   --------------------------

   function Get_Code_Unit_Entity (E : Entity_Id) return Entity_Id is
      Unit : Entity_Id := Cunit_Entity (Get_Code_Unit (E));

   begin
      if Ekind (Unit) = E_Package_Body then
         Unit := Spec_Entity (Unit);
      end if;

      return Unit;
   end Get_Code_Unit_Entity;

   ------------------------------
   -- Has_Excluded_Declaration --
   ------------------------------

   function Has_Excluded_Declaration
     (Subp  : Entity_Id;
      Decls : List_Id) return Boolean
   is
      function Is_Unchecked_Conversion (D : Node_Id) return Boolean;
      --  Nested subprograms make a given body ineligible for inlining, but
      --  we make an exception for instantiations of unchecked conversion.
      --  The body has not been analyzed yet, so check the name, and verify
      --  that the visible entity with that name is the predefined unit.

      -----------------------------
      -- Is_Unchecked_Conversion --
      -----------------------------

      function Is_Unchecked_Conversion (D : Node_Id) return Boolean is
         Id   : constant Node_Id := Name (D);
         Conv : Entity_Id;

      begin
         if Nkind (Id) = N_Identifier
           and then Chars (Id) = Name_Unchecked_Conversion
         then
            Conv := Current_Entity (Id);

         elsif Nkind (Id) in N_Selected_Component | N_Expanded_Name
           and then Chars (Selector_Name (Id)) = Name_Unchecked_Conversion
         then
            Conv := Current_Entity (Selector_Name (Id));
         else
            return False;
         end if;

         return Present (Conv)
           and then Is_Predefined_Unit (Get_Source_Unit (Conv))
           and then Is_Intrinsic_Subprogram (Conv);
      end Is_Unchecked_Conversion;

      --  Local variables

      Decl : Node_Id;

   --  Start of processing for Has_Excluded_Declaration

   begin
      --  No action needed if the check is not needed

      if not Check_Inlining_Restrictions then
         return False;
      end if;

      Decl := First (Decls);
      while Present (Decl) loop

         --  First declarations universally excluded

         if Nkind (Decl) = N_Package_Declaration then
            Cannot_Inline
              ("cannot inline & (nested package declaration)?", Decl, Subp);
            return True;

         elsif Nkind (Decl) = N_Package_Instantiation then
            Cannot_Inline
              ("cannot inline & (nested package instantiation)?", Decl, Subp);
            return True;
         end if;

         --  Then declarations excluded only for front-end inlining

         if Back_End_Inlining then
            null;

         elsif Nkind (Decl) = N_Task_Type_Declaration
           or else Nkind (Decl) = N_Single_Task_Declaration
         then
            Cannot_Inline
              ("cannot inline & (nested task type declaration)?", Decl, Subp);
            return True;

         elsif Nkind (Decl) in N_Protected_Type_Declaration
                             | N_Single_Protected_Declaration
         then
            Cannot_Inline
              ("cannot inline & (nested protected type declaration)?",
               Decl, Subp);
            return True;

         elsif Nkind (Decl) = N_Subprogram_Body then
            Cannot_Inline
              ("cannot inline & (nested subprogram)?", Decl, Subp);
            return True;

         elsif Nkind (Decl) = N_Function_Instantiation
           and then not Is_Unchecked_Conversion (Decl)
         then
            Cannot_Inline
              ("cannot inline & (nested function instantiation)?", Decl, Subp);
            return True;

         elsif Nkind (Decl) = N_Procedure_Instantiation then
            Cannot_Inline
              ("cannot inline & (nested procedure instantiation)?",
               Decl, Subp);
            return True;

         --  Subtype declarations with predicates will generate predicate
         --  functions, i.e. nested subprogram bodies, so inlining is not
         --  possible.

         elsif Nkind (Decl) = N_Subtype_Declaration then
            declare
               A    : Node_Id;
               A_Id : Aspect_Id;

            begin
               A := First (Aspect_Specifications (Decl));
               while Present (A) loop
                  A_Id := Get_Aspect_Id (Chars (Identifier (A)));

                  if A_Id = Aspect_Predicate
                    or else A_Id = Aspect_Static_Predicate
                    or else A_Id = Aspect_Dynamic_Predicate
                  then
                     Cannot_Inline
                       ("cannot inline & (subtype declaration with "
                        & "predicate)?", Decl, Subp);
                     return True;
                  end if;

                  Next (A);
               end loop;
            end;
         end if;

         Next (Decl);
      end loop;

      return False;
   end Has_Excluded_Declaration;

   ----------------------------
   -- Has_Excluded_Statement --
   ----------------------------

   function Has_Excluded_Statement
     (Subp  : Entity_Id;
      Stats : List_Id) return Boolean
   is
      S : Node_Id;
      E : Node_Id;

   begin
      --  No action needed if the check is not needed

      if not Check_Inlining_Restrictions then
         return False;
      end if;

      S := First (Stats);
      while Present (S) loop
         if Nkind (S) in N_Abort_Statement
                       | N_Asynchronous_Select
                       | N_Conditional_Entry_Call
                       | N_Delay_Relative_Statement
                       | N_Delay_Until_Statement
                       | N_Selective_Accept
                       | N_Timed_Entry_Call
         then
            Cannot_Inline
              ("cannot inline & (non-allowed statement)?", S, Subp);
            return True;

         elsif Nkind (S) = N_Block_Statement then
            if Has_Excluded_Declaration (Subp, Declarations (S)) then
               return True;

            elsif Present (Handled_Statement_Sequence (S)) then
               if not Back_End_Inlining
                 and then
                   Present
                     (Exception_Handlers (Handled_Statement_Sequence (S)))
               then
                  Cannot_Inline
                    ("cannot inline& (exception handler)?",
                     First (Exception_Handlers
                              (Handled_Statement_Sequence (S))),
                     Subp);
                  return True;

               elsif Has_Excluded_Statement
                       (Subp, Statements (Handled_Statement_Sequence (S)))
               then
                  return True;
               end if;
            end if;

         elsif Nkind (S) = N_Case_Statement then
            E := First (Alternatives (S));
            while Present (E) loop
               if Has_Excluded_Statement (Subp, Statements (E)) then
                  return True;
               end if;

               Next (E);
            end loop;

         elsif Nkind (S) = N_If_Statement then
            if Has_Excluded_Statement (Subp, Then_Statements (S)) then
               return True;
            end if;

            if Present (Elsif_Parts (S)) then
               E := First (Elsif_Parts (S));
               while Present (E) loop
                  if Has_Excluded_Statement (Subp, Then_Statements (E)) then
                     return True;
                  end if;

                  Next (E);
               end loop;
            end if;

            if Present (Else_Statements (S))
              and then Has_Excluded_Statement (Subp, Else_Statements (S))
            then
               return True;
            end if;

         elsif Nkind (S) = N_Loop_Statement
           and then Has_Excluded_Statement (Subp, Statements (S))
         then
            return True;

         elsif Nkind (S) = N_Extended_Return_Statement then
            if Present (Handled_Statement_Sequence (S))
              and then
                Has_Excluded_Statement
                  (Subp, Statements (Handled_Statement_Sequence (S)))
            then
               return True;

            elsif not Back_End_Inlining
              and then Present (Handled_Statement_Sequence (S))
              and then
                Present (Exception_Handlers
                          (Handled_Statement_Sequence (S)))
            then
               Cannot_Inline
                 ("cannot inline& (exception handler)?",
                  First (Exception_Handlers (Handled_Statement_Sequence (S))),
                  Subp);
               return True;
            end if;
         end if;

         Next (S);
      end loop;

      return False;
   end Has_Excluded_Statement;

   --------------------------
   -- Has_Initialized_Type --
   --------------------------

   function Has_Initialized_Type (E : Entity_Id) return Boolean is
      E_Body : constant Node_Id := Subprogram_Body (E);
      Decl   : Node_Id;

   begin
      if No (E_Body) then -- imported subprogram
         return False;

      else
         Decl := First (Declarations (E_Body));
         while Present (Decl) loop
            if Nkind (Decl) = N_Full_Type_Declaration
              and then Comes_From_Source (Decl)
              and then Present (Init_Proc (Defining_Identifier (Decl)))
            then
               return True;
            end if;

            Next (Decl);
         end loop;
      end if;

      return False;
   end Has_Initialized_Type;

   -----------------------
   -- Has_Single_Return --
   -----------------------

   function Has_Single_Return (N : Node_Id) return Boolean is
      Return_Statement : Node_Id := Empty;

      function Check_Return (N : Node_Id) return Traverse_Result;

      ------------------
      -- Check_Return --
      ------------------

      function Check_Return (N : Node_Id) return Traverse_Result is
      begin
         if Nkind (N) = N_Simple_Return_Statement then
            if Present (Expression (N))
              and then Is_Entity_Name (Expression (N))
            then
               pragma Assert (Present (Entity (Expression (N))));

               if No (Return_Statement) then
                  Return_Statement := N;
                  return OK;

               else
                  pragma Assert
                    (Present (Entity (Expression (Return_Statement))));

                  if Entity (Expression (N)) =
                       Entity (Expression (Return_Statement))
                  then
                     return OK;
                  else
                     return Abandon;
                  end if;
               end if;

            --  A return statement within an extended return is a noop after
            --  inlining.

            elsif No (Expression (N))
              and then Nkind (Parent (Parent (N))) =
                         N_Extended_Return_Statement
            then
               return OK;

            else
               --  Expression has wrong form

               return Abandon;
            end if;

         --  We can only inline a build-in-place function if it has a single
         --  extended return.

         elsif Nkind (N) = N_Extended_Return_Statement then
            if No (Return_Statement) then
               Return_Statement := N;
               return OK;

            else
               return Abandon;
            end if;

         else
            return OK;
         end if;
      end Check_Return;

      function Check_All_Returns is new Traverse_Func (Check_Return);

   --  Start of processing for Has_Single_Return

   begin
      if Check_All_Returns (N) /= OK then
         return False;

      elsif Nkind (Return_Statement) = N_Extended_Return_Statement then
         return True;

      else
         return
           Present (Declarations (N))
             and then Present (First (Declarations (N)))
             and then Nkind (First (Declarations (N))) = N_Object_Declaration
             and then Entity (Expression (Return_Statement)) =
                        Defining_Identifier (First (Declarations (N)));
      end if;
   end Has_Single_Return;

   -----------------------------
   -- In_Main_Unit_Or_Subunit --
   -----------------------------

   function In_Main_Unit_Or_Subunit (E : Entity_Id) return Boolean is
      Comp : Node_Id := Cunit (Get_Code_Unit (E));

   begin
      --  Check whether the subprogram or package to inline is within the main
      --  unit or its spec or within a subunit. In either case there are no
      --  additional bodies to process. If the subprogram appears in a parent
      --  of the current unit, the check on whether inlining is possible is
      --  done in Analyze_Inlined_Bodies.

      while Nkind (Unit (Comp)) = N_Subunit loop
         Comp := Library_Unit (Comp);
      end loop;

      return Comp = Cunit (Main_Unit)
        or else Comp = Library_Unit (Cunit (Main_Unit));
   end In_Main_Unit_Or_Subunit;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Pending_Instantiations.Init;
      Called_Pending_Instantiations.Init;
      Inlined_Bodies.Init;
      Successors.Init;
      Inlined.Init;

      for J in Hash_Headers'Range loop
         Hash_Headers (J) := No_Subp;
      end loop;

      Inlined_Calls := No_Elist;
      Backend_Calls := No_Elist;
      Backend_Instances := No_Elist;
      Backend_Inlined_Subps := No_Elist;
      Backend_Not_Inlined_Subps := No_Elist;
   end Initialize;

   ---------------------------------
   -- Inline_Static_Function_Call --
   ---------------------------------

   procedure Inline_Static_Function_Call (N : Node_Id; Subp : Entity_Id) is

      function Replace_Formal (N : Node_Id) return Traverse_Result;
      --  Replace each occurrence of a formal with the
      --  corresponding actual, using the mapping created
      --  by Establish_Actual_Mapping_For_Inlined_Call.

      function Reset_Sloc (Nod : Node_Id) return Traverse_Result;
      --  Reset the Sloc of a node to that of the call itself, so that errors
      --  will be flagged on the call to the static expression function itself
      --  rather than on the expression of the function's declaration.

      --------------------
      -- Replace_Formal --
      --------------------

      function Replace_Formal (N : Node_Id) return Traverse_Result is
         A : Entity_Id;
         E : Entity_Id;

      begin
         if Is_Entity_Name (N) and then Present (Entity (N)) then
            E := Entity (N);

            if Is_Formal (E) and then Scope (E) = Subp then
               A := Renamed_Object (E);

               if Nkind (A) = N_Defining_Identifier then
                  Rewrite (N, New_Occurrence_Of (A, Sloc (N)));

               --  Literal cases

               else
                  Rewrite (N, New_Copy (A));
               end if;
            end if;

            return Skip;

         else
            return OK;
         end if;
      end Replace_Formal;

      procedure Replace_Formals is new Traverse_Proc (Replace_Formal);

      ------------------
      -- Process_Sloc --
      ------------------

      function Reset_Sloc (Nod : Node_Id) return Traverse_Result is
      begin
         Set_Sloc (Nod, Sloc (N));
         Set_Comes_From_Source (Nod, False);

         return OK;
      end Reset_Sloc;

      procedure Reset_Slocs is new Traverse_Proc (Reset_Sloc);

   --  Start of processing for Inline_Static_Function_Call

   begin
      pragma Assert (Is_Static_Function_Call (N));

      declare
         Decls     : constant List_Id := New_List;
         Func_Expr : constant Node_Id :=
                       Expression_Of_Expression_Function (Subp);
         Expr_Copy : constant Node_Id := New_Copy_Tree (Func_Expr);

      begin
         --  Create a mapping from formals to actuals, also creating temps in
         --  Decls, when needed, to hold the actuals.

         Establish_Actual_Mapping_For_Inlined_Call (N, Subp, Decls, Func_Expr);

         --  Ensure that the copy has the same parent as the call (this seems
         --  to matter when GNATprove_Mode is set and there are nested static
         --  calls; prevents blowups in Insert_Actions, though it's not clear
         --  exactly why this is needed???).

         Set_Parent (Expr_Copy, Parent (N));

         Insert_Actions (N, Decls);

         --  Now substitute actuals for their corresponding formal references
         --  within the expression.

         Replace_Formals (Expr_Copy);

         Reset_Slocs (Expr_Copy);

         --  Apply a qualified expression with the function's result subtype,
         --  to ensure that we check the expression against any constraint
         --  or predicate, which will cause the call to be illegal if the
         --  folded expression doesn't satisfy them. (The predicate case
         --  might not get checked if the subtype hasn't been frozen yet,
         --  which can happen if this static expression happens to be what
         --  causes the freezing, because Has_Static_Predicate doesn't get
         --  set on the subtype until it's frozen and Build_Predicates is
         --  called. It's not clear how to address this case. ???)

         Rewrite (Expr_Copy,
           Make_Qualified_Expression (Sloc (Expr_Copy),
             Subtype_Mark =>
               New_Occurrence_Of (Etype (N), Sloc (Expr_Copy)),
             Expression =>
               Relocate_Node (Expr_Copy)));

         Set_Etype (Expr_Copy, Etype (N));

         Analyze_And_Resolve (Expr_Copy, Etype (N));

         --  Finally rewrite the function call as the folded static result

         Rewrite (N, Expr_Copy);

         --  Cleanup mapping between formals and actuals for other expansions

         Reset_Actual_Mapping_For_Inlined_Call (Subp);
      end;
   end Inline_Static_Function_Call;

   ------------------------
   -- Instantiate_Bodies --
   ------------------------

   --  Generic bodies contain all the non-local references, so an
   --  instantiation does not need any more context than Standard
   --  itself, even if the instantiation appears in an inner scope.
   --  Generic associations have verified that the contract model is
   --  satisfied, so that any error that may occur in the analysis of
   --  the body is an internal error.

   procedure Instantiate_Bodies is

      procedure Instantiate_Body (Info : Pending_Body_Info);
      --  Instantiate a pending body

      ------------------------
      --  Instantiate_Body  --
      ------------------------

      procedure Instantiate_Body (Info : Pending_Body_Info) is
         Scop : Entity_Id;

      begin
         --  If the instantiation node is absent, it has been removed as part
         --  of unreachable code.

         if No (Info.Inst_Node) then
            null;

         --  If the instantiation node is a package body, this means that the
         --  instance is a compilation unit and the instantiation has already
         --  been performed by Build_Instance_Compilation_Unit_Nodes.

         elsif Nkind (Info.Inst_Node) = N_Package_Body then
            null;

         --  For other package instances, instantiate the body and register the
         --  finalization scope, if any, for subsequent generation of cleanups.

         elsif Nkind (Info.Inst_Node) = N_Package_Instantiation then

            --  If the enclosing finalization scope is a package body, set the
            --  In_Package_Body flag on its spec. This is required, in the case
            --  where the body contains other package instantiations that have
            --  a body, for Analyze_Package_Instantiation to compute a correct
            --  finalization scope.

            if Present (Info.Fin_Scop)
              and then Ekind (Info.Fin_Scop) = E_Package_Body
            then
               Set_In_Package_Body (Spec_Entity (Info.Fin_Scop), True);
            end if;

            Instantiate_Package_Body (Info);

            if Present (Info.Fin_Scop) then
               Scop := Info.Fin_Scop;

               --  If the enclosing finalization scope is dynamic, the instance
               --  may have been relocated, for example if it was declared in a
               --  protected entry, protected subprogram, or task body.

               if Is_Dynamic_Scope (Scop) then
                  Scop :=
                    Enclosing_Dynamic_Scope (Defining_Entity (Info.Act_Decl));
               end if;

               Add_Scope_To_Clean (Scop);

               --  Reset the In_Package_Body flag if it was set above

               if Ekind (Info.Fin_Scop) = E_Package_Body then
                  Set_In_Package_Body (Spec_Entity (Info.Fin_Scop), False);
               end if;
            end if;

         --  For subprogram instances, always instantiate the body

         else
            Instantiate_Subprogram_Body (Info);
         end if;
      end Instantiate_Body;

      J, K : Nat;
      Info : Pending_Body_Info;

   --  Start of processing for Instantiate_Bodies

   begin
      if Serious_Errors_Detected = 0 then
         Expander_Active := (Operating_Mode = Opt.Generate_Code);
         Push_Scope (Standard_Standard);
         To_Clean := New_Elmt_List;

         if Is_Generic_Unit (Cunit_Entity (Main_Unit)) then
            Start_Generic;
         end if;

         --  A body instantiation may generate additional instantiations, so
         --  the following loop must scan to the end of a possibly expanding
         --  set (that's why we cannot simply use a FOR loop here). We must
         --  also capture the element lest the set be entirely reallocated.

         J := 0;
         if Back_End_Inlining then
            while J <= Called_Pending_Instantiations.Last
              and then Serious_Errors_Detected = 0
            loop
               K := Called_Pending_Instantiations.Table (J);
               Info := Pending_Instantiations.Table (K);
               Instantiate_Body (Info);

               J := J + 1;
            end loop;

         else
            while J <= Pending_Instantiations.Last
              and then Serious_Errors_Detected = 0
            loop
               Info := Pending_Instantiations.Table (J);
               Instantiate_Body (Info);

               J := J + 1;
            end loop;
         end if;

         --  Reset the table of instantiations. Additional instantiations
         --  may be added through inlining, when additional bodies are
         --  analyzed.

         if Back_End_Inlining then
            Called_Pending_Instantiations.Init;
         else
            Pending_Instantiations.Init;
         end if;

         --  We can now complete the cleanup actions of scopes that contain
         --  pending instantiations (skipped for generic units, since we
         --  never need any cleanups in generic units).

         if Expander_Active
           and then not Is_Generic_Unit (Main_Unit_Entity)
         then
            Cleanup_Scopes;
         elsif Is_Generic_Unit (Cunit_Entity (Main_Unit)) then
            End_Generic;
         end if;

         Pop_Scope;
      end if;
   end Instantiate_Bodies;

   ---------------
   -- Is_Nested --
   ---------------

   function Is_Nested (E : Entity_Id) return Boolean is
      Scop : Entity_Id;

   begin
      Scop := Scope (E);
      while Scop /= Standard_Standard loop
         if Is_Subprogram (Scop) then
            return True;

         elsif Ekind (Scop) = E_Task_Type
           or else Ekind (Scop) = E_Entry
           or else Ekind (Scop) = E_Entry_Family
         then
            return True;
         end if;

         Scop := Scope (Scop);
      end loop;

      return False;
   end Is_Nested;

   ------------------------
   -- List_Inlining_Info --
   ------------------------

   procedure List_Inlining_Info is
      Elmt  : Elmt_Id;
      Nod   : Node_Id;
      Count : Nat;

   begin
      if not Debug_Flag_Dot_J then
         return;
      end if;

      --  Generate listing of calls inlined by the frontend

      if Present (Inlined_Calls) then
         Count := 0;
         Elmt  := First_Elmt (Inlined_Calls);
         while Present (Elmt) loop
            Nod := Node (Elmt);

            if not In_Internal_Unit (Nod) then
               Count := Count + 1;

               if Count = 1 then
                  Write_Str ("List of calls inlined by the frontend");
                  Write_Eol;
               end if;

               Write_Str ("  ");
               Write_Int (Count);
               Write_Str (":");
               Write_Location (Sloc (Nod));
               Write_Str (":");
               Output.Write_Eol;
            end if;

            Next_Elmt (Elmt);
         end loop;
      end if;

      --  Generate listing of calls passed to the backend

      if Present (Backend_Calls) then
         Count := 0;

         Elmt := First_Elmt (Backend_Calls);
         while Present (Elmt) loop
            Nod := Node (Elmt);

            if not In_Internal_Unit (Nod) then
               Count := Count + 1;

               if Count = 1 then
                  Write_Str ("List of inlined calls passed to the backend");
                  Write_Eol;
               end if;

               Write_Str ("  ");
               Write_Int (Count);
               Write_Str (":");
               Write_Location (Sloc (Nod));
               Output.Write_Eol;
            end if;

            Next_Elmt (Elmt);
         end loop;
      end if;

      --  Generate listing of instances inlined for the backend

      if Present (Backend_Instances) then
         Count := 0;

         Elmt := First_Elmt (Backend_Instances);
         while Present (Elmt) loop
            Nod := Node (Elmt);

            if not In_Internal_Unit (Nod) then
               Count := Count + 1;

               if Count = 1 then
                  Write_Str ("List of instances inlined for the backend");
                  Write_Eol;
               end if;

               Write_Str ("  ");
               Write_Int (Count);
               Write_Str (":");
               Write_Location (Sloc (Nod));
               Output.Write_Eol;
            end if;

            Next_Elmt (Elmt);
         end loop;
      end if;

      --  Generate listing of subprograms passed to the backend

      if Present (Backend_Inlined_Subps) and then Back_End_Inlining then
         Count := 0;

         Elmt := First_Elmt (Backend_Inlined_Subps);
         while Present (Elmt) loop
            Nod := Node (Elmt);

            if not In_Internal_Unit (Nod) then
               Count := Count + 1;

               if Count = 1 then
                  Write_Str
                    ("List of inlined subprograms passed to the backend");
                  Write_Eol;
               end if;

               Write_Str ("  ");
               Write_Int (Count);
               Write_Str (":");
               Write_Name (Chars (Nod));
               Write_Str (" (");
               Write_Location (Sloc (Nod));
               Write_Str (")");
               Output.Write_Eol;
            end if;

            Next_Elmt (Elmt);
         end loop;
      end if;

      --  Generate listing of subprograms that cannot be inlined by the backend

      if Present (Backend_Not_Inlined_Subps) and then Back_End_Inlining then
         Count := 0;

         Elmt := First_Elmt (Backend_Not_Inlined_Subps);
         while Present (Elmt) loop
            Nod := Node (Elmt);

            if not In_Internal_Unit (Nod) then
               Count := Count + 1;

               if Count = 1 then
                  Write_Str
                    ("List of subprograms that cannot be inlined by backend");
                  Write_Eol;
               end if;

               Write_Str ("  ");
               Write_Int (Count);
               Write_Str (":");
               Write_Name (Chars (Nod));
               Write_Str (" (");
               Write_Location (Sloc (Nod));
               Write_Str (")");
               Output.Write_Eol;
            end if;

            Next_Elmt (Elmt);
         end loop;
      end if;
   end List_Inlining_Info;

   ----------
   -- Lock --
   ----------

   procedure Lock is
   begin
      Pending_Instantiations.Release;
      Pending_Instantiations.Locked := True;
      Called_Pending_Instantiations.Release;
      Called_Pending_Instantiations.Locked := True;
      Inlined_Bodies.Release;
      Inlined_Bodies.Locked := True;
      Successors.Release;
      Successors.Locked := True;
      Inlined.Release;
      Inlined.Locked := True;
   end Lock;

   --------------------------------
   -- Remove_Aspects_And_Pragmas --
   --------------------------------

   procedure Remove_Aspects_And_Pragmas (Body_Decl : Node_Id) is
      procedure Remove_Items (List : List_Id);
      --  Remove all useless aspects/pragmas from a particular list

      ------------------
      -- Remove_Items --
      ------------------

      procedure Remove_Items (List : List_Id) is
         Item      : Node_Id;
         Item_Id   : Node_Id;
         Next_Item : Node_Id;

      begin
         --  Traverse the list looking for an aspect specification or a pragma

         Item := First (List);
         while Present (Item) loop
            Next_Item := Next (Item);

            if Nkind (Item) = N_Aspect_Specification then
               Item_Id := Identifier (Item);
            elsif Nkind (Item) = N_Pragma then
               Item_Id := Pragma_Identifier (Item);
            else
               Item_Id := Empty;
            end if;

            if Present (Item_Id)
              and then Chars (Item_Id) in Name_Always_Terminates
                                        | Name_Contract_Cases
                                        | Name_Global
                                        | Name_Depends
                                        | Name_Exceptional_Cases
                                        | Name_Postcondition
                                        | Name_Precondition
                                        | Name_Refined_Global
                                        | Name_Refined_Depends
                                        | Name_Refined_Post
                                        | Name_Subprogram_Variant
                                        | Name_Test_Case
                                        | Name_Unmodified
                                        | Name_Unreferenced
                                        | Name_Unused
            then
               Remove (Item);
            end if;

            Item := Next_Item;
         end loop;
      end Remove_Items;

   --  Start of processing for Remove_Aspects_And_Pragmas

   begin
      Remove_Items (Aspect_Specifications (Body_Decl));
      Remove_Items (Declarations          (Body_Decl));

      --  Pragmas Unmodified, Unreferenced, and Unused may additionally appear
      --  in the body of the subprogram.

      Remove_Items (Statements (Handled_Statement_Sequence (Body_Decl)));
   end Remove_Aspects_And_Pragmas;

   --------------------------
   -- Remove_Dead_Instance --
   --------------------------

   procedure Remove_Dead_Instance (N : Node_Id) is
   begin
      for J in 0 .. Pending_Instantiations.Last loop
         if Pending_Instantiations.Table (J).Inst_Node = N then
            Pending_Instantiations.Table (J).Inst_Node := Empty;
            return;
         end if;
      end loop;
   end Remove_Dead_Instance;

   -------------------------------------------
   -- Reset_Actual_Mapping_For_Inlined_Call --
   -------------------------------------------

   procedure Reset_Actual_Mapping_For_Inlined_Call (Subp : Entity_Id) is
      F : Entity_Id := First_Formal (Subp);

   begin
      while Present (F) loop
         Set_Renamed_Object (F, Empty);
         Next_Formal (F);
      end loop;
   end Reset_Actual_Mapping_For_Inlined_Call;

end Inline;
