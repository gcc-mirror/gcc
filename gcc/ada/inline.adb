------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               I N L I N E                                --
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
with Einfo;    use Einfo;
with Elists;   use Elists;
with Errout;   use Errout;
with Exp_Ch7;  use Exp_Ch7;
with Exp_Tss;  use Exp_Tss;
with Fname;    use Fname;
with Fname.UF; use Fname.UF;
with Lib;      use Lib;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Opt;      use Opt;
with Sem_Aux;  use Sem_Aux;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Ch10; use Sem_Ch10;
with Sem_Ch12; use Sem_Ch12;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Stand;    use Stand;
with Uname;    use Uname;

package body Inline is

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
   --  that stores caller and callee, and indicates a prerequisite from
   --  one to the other. We also record the compilation unit that contains
   --  the callee. After analyzing the bodies of all such compilation units,
   --  we produce a list of subprograms in  topological order, for use by the
   --  back-end. If P2 is a prerequisite of P1, then P1 calls P2, and for
   --  proper inlining the back-end must analyze the body of P2 before that of
   --  P1. The code below guarantees that the transitive closure of inlined
   --  subprograms called from the main compilation unit is made available to
   --  the code generator.

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

   --  The following table stores list elements for the successor lists.
   --  These lists cannot be chained directly through entries in the Inlined
   --  table, because a given subprogram can appear in several such lists.

   package Successors is new Table.Table (
      Table_Component_Type => Succ_Info,
      Table_Index_Type     => Succ_Index,
      Table_Low_Bound      => 1,
      Table_Initial        => Alloc.Successors_Initial,
      Table_Increment      => Alloc.Successors_Increment,
      Table_Name           => "Successors");

   type Subp_Info is record
      Name        : Entity_Id  := Empty;
      First_Succ  : Succ_Index := No_Succ;
      Count       : Integer    := 0;
      Listed      : Boolean    := False;
      Main_Call   : Boolean    := False;
      Next        : Subp_Index := No_Subp;
      Next_Nopred : Subp_Index := No_Subp;
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

   function Scope_In_Main_Unit (Scop : Entity_Id) return Boolean;
   --  Return True if Scop is in the main unit or its spec, or in a
   --  parent of the main unit if it is a child unit.

   procedure Add_Call (Called : Entity_Id; Caller : Entity_Id := Empty);
   --  Make two entries in Inlined table, for an inlined subprogram being
   --  called, and for the inlined subprogram that contains the call. If
   --  the call is in the main compilation unit, Caller is Empty.

   function Add_Subp (E : Entity_Id) return Subp_Index;
   --  Make entry in Inlined table for subprogram E, or return table index
   --  that already holds E.

   function Has_Initialized_Type (E : Entity_Id) return Boolean;
   --  If a candidate for inlining contains type declarations for types with
   --  non-trivial initialization procedures, they are not worth inlining.

   function Is_Nested (E : Entity_Id) return Boolean;
   --  If the function is nested inside some other function, it will
   --  always be compiled if that function is, so don't add it to the
   --  inline list. We cannot compile a nested function outside the
   --  scope of the containing function anyway. This is also the case if
   --  the function is defined in a task body or within an entry (for
   --  example, an initialization procedure).

   procedure Add_Inlined_Subprogram (Index : Subp_Index);
   --  Add subprogram to Inlined List once all of its predecessors have been
   --  placed on the list. Decrement the count of all its successors, and
   --  add them to list (recursively) if count drops to zero.

   ------------------------------
   -- Deferred Cleanup Actions --
   ------------------------------

   --  The cleanup actions for scopes that contain instantiations is delayed
   --  until after expansion of those instantiations, because they may
   --  contain finalizable objects or tasks that affect the cleanup code.
   --  A scope that contains instantiations only needs to be finalized once,
   --  even if it contains more than one instance. We keep a list of scopes
   --  that must still be finalized, and call cleanup_actions after all the
   --  instantiations have been completed.

   To_Clean : Elist_Id;

   procedure Add_Scope_To_Clean (Inst : Entity_Id);
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

         --  Add P2 to the list of successors of P1, if not already there.
         --  Note that P2 may contain more than one call to P1, and only
         --  one needs to be recorded.

         J := Inlined.Table (P1).First_Succ;
         while J /= No_Succ loop
            if Successors.Table (J).Subp = P2 then
               return;
            end if;

            J := Successors.Table (J).Next;
         end loop;

         --  On exit, make a successor entry for P2

         Successors.Increment_Last;
         Successors.Table (Successors.Last).Subp := P2;
         Successors.Table (Successors.Last).Next :=
                             Inlined.Table (P1).First_Succ;
         Inlined.Table (P1).First_Succ := Successors.Last;

         Inlined.Table (P2).Count := Inlined.Table (P2).Count + 1;

      else
         Inlined.Table (P1).Main_Call := True;
      end if;
   end Add_Call;

   ----------------------
   -- Add_Inlined_Body --
   ----------------------

   procedure Add_Inlined_Body (E : Entity_Id) is
      Pack : Entity_Id;

      function Must_Inline return Boolean;
      --  Inlining is only done if the call statement N is in the main unit,
      --  or within the body of another inlined subprogram.

      -----------------
      -- Must_Inline --
      -----------------

      function Must_Inline return Boolean is
         Scop : Entity_Id;
         Comp : Node_Id;

      begin
         --  Check if call is in main unit

         Scop := Current_Scope;

         --  Do not try to inline if scope is standard. This could happen, for
         --  example, for a call to Add_Global_Declaration, and it causes
         --  trouble to try to inline at this level.

         if Scop = Standard_Standard then
            return False;
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

         if Comp = Cunit (Main_Unit)
           or else Comp = Library_Unit (Cunit (Main_Unit))
         then
            Add_Call (E);
            return True;
         end if;

         --  Call is not in main unit. See if it's in some inlined subprogram

         Scop := Current_Scope;
         while Scope (Scop) /= Standard_Standard
           and then not Is_Child_Unit (Scop)
         loop
            if Is_Overloadable (Scop)
              and then Is_Inlined (Scop)
            then
               Add_Call (E, Scop);
               return True;
            end if;

            Scop := Scope (Scop);
         end loop;

         return False;
      end Must_Inline;

   --  Start of processing for Add_Inlined_Body

   begin
      --  Find unit containing E, and add to list of inlined bodies if needed.
      --  If the body is already present, no need to load any other unit. This
      --  is the case for an initialization procedure, which appears in the
      --  package declaration that contains the type. It is also the case if
      --  the body has already been analyzed. Finally, if the unit enclosing
      --  E is an instance, the instance body will be analyzed in any case,
      --  and there is no need to add the enclosing unit (whose body might not
      --  be available).

      --  Library-level functions must be handled specially, because there is
      --  no enclosing package to retrieve. In this case, it is the body of
      --  the function that will have to be loaded.

      if not Is_Abstract_Subprogram (E) and then not Is_Nested (E)
        and then Convention (E) /= Convention_Protected
      then
         Pack := Scope (E);

         if Must_Inline
           and then Ekind (Pack) = E_Package
         then
            Set_Is_Called (E);

            if Pack = Standard_Standard then

               --  Library-level inlined function. Add function itself to
               --  list of needed units.

               Inlined_Bodies.Increment_Last;
               Inlined_Bodies.Table (Inlined_Bodies.Last) := E;

            elsif Is_Generic_Instance (Pack) then
               null;

            elsif not Is_Inlined (Pack)
              and then not Has_Completion (E)
              and then not Scope_In_Main_Unit (Pack)
            then
               Set_Is_Inlined (Pack);
               Inlined_Bodies.Increment_Last;
               Inlined_Bodies.Table (Inlined_Bodies.Last) := Pack;
            end if;
         end if;
      end if;
   end Add_Inlined_Body;

   ----------------------------
   -- Add_Inlined_Subprogram --
   ----------------------------

   procedure Add_Inlined_Subprogram (Index : Subp_Index) is
      E    : constant Entity_Id := Inlined.Table (Index).Name;
      Succ : Succ_Index;
      Subp : Subp_Index;

      function Back_End_Cannot_Inline (Subp : Entity_Id) return Boolean;
      --  There are various conditions under which back-end inlining cannot
      --  be done reliably:
      --
      --    a) If a body has handlers, it must not be inlined, because this
      --    may violate program semantics, and because in zero-cost exception
      --    mode it will lead to undefined symbols at link time.
      --
      --    b) If a body contains inlined function instances, it cannot be
      --    inlined under ZCX because the numeric suffix generated by gigi
      --    will be different in the body and the place of the inlined call.
      --
      --  If the body to be inlined contains calls to subprograms declared
      --  in the same body that have no previous spec, the back-end cannot
      --  inline either because the bodies to be inlined are processed before
      --  the rest of the enclosing package body, and gigi will then find
      --  references to entities that have not been elaborated yet.
      --
      --  This procedure must be carefully coordinated with the back end.

      ----------------------------
      -- Back_End_Cannot_Inline --
      ----------------------------

      function Back_End_Cannot_Inline (Subp : Entity_Id) return Boolean is
         Decl     : constant Node_Id := Unit_Declaration_Node (Subp);
         Body_Ent : Entity_Id;
         Ent      : Entity_Id;
         Bad_Call : Node_Id;

         function Process (N : Node_Id) return Traverse_Result;
         --  Look for calls to subprograms with no previous spec, declared
         --  in the same enclosiong package body.

         -------------
         -- Process --
         -------------

         function Process (N : Node_Id) return Traverse_Result is
         begin
            if Nkind (N) = N_Procedure_Call_Statement
              or else Nkind (N) = N_Function_Call
            then
               if Is_Entity_Name (Name (N))
                 and then Comes_From_Source (Entity (Name (N)))
                 and then
                    Nkind (Unit_Declaration_Node (Entity (Name (N))))
                      = N_Subprogram_Body
                 and then In_Same_Extended_Unit (Subp, Entity (Name (N)))
               then
                  Bad_Call := N;
                  return Abandon;
               else
                  return OK;
               end if;
            else
               return OK;
            end if;
         end Process;

         function Has_Exposed_Call is new Traverse_Func (Process);

      --  Start of processing for Back_End_Cannot_Inline

      begin
         if Nkind (Decl) = N_Subprogram_Declaration
           and then Present (Corresponding_Body (Decl))
         then
            Body_Ent := Corresponding_Body (Decl);
         else
            return False;
         end if;

         --  If subprogram is marked Inline_Always, inlining is mandatory

         if Has_Pragma_Inline_Always (Subp) then
            return False;
         end if;

         if Present
          (Exception_Handlers
            (Handled_Statement_Sequence
              (Unit_Declaration_Node (Corresponding_Body (Decl)))))
         then
            return True;
         end if;

         Ent := First_Entity (Body_Ent);
         while Present (Ent) loop
            if Is_Subprogram (Ent)
              and then Is_Generic_Instance (Ent)
            then
               return True;
            end if;

            Next_Entity (Ent);
         end loop;

         if Has_Exposed_Call
              (Unit_Declaration_Node (Corresponding_Body (Decl))) = Abandon
         then
            if Ineffective_Inline_Warnings then
               Error_Msg_N
                 ("?call to subprogram with no separate spec"
                  & " prevents inlining!!", Bad_Call);
            end if;

            return True;
         else
            return False;
         end if;
      end Back_End_Cannot_Inline;

   --  Start of processing for Add_Inlined_Subprogram

   begin
      --  Insert the current subprogram in the list of inlined subprograms,
      --  if it can actually be inlined by the back-end.

      if not Scope_In_Main_Unit (E)
        and then Is_Inlined (E)
        and then not Is_Nested (E)
        and then not Has_Initialized_Type (E)
      then
         if Back_End_Cannot_Inline (E) then
            Set_Is_Inlined (E, False);

         else
            if No (Last_Inlined) then
               Set_First_Inlined_Subprogram (Cunit (Main_Unit), E);
            else
               Set_Next_Inlined_Subprogram (Last_Inlined, E);
            end if;

            Last_Inlined := E;
         end if;
      end if;

      Inlined.Table (Index).Listed := True;

      --  Now add to the list those callers of the current subprogram that
      --  are themselves called. They may appear on the graph as callers
      --  of the current one, even if they are themselves not called, and
      --  there is no point in including them in the list for the backend.
      --  Furthermore, they might not even be public, in which case the
      --  back-end cannot handle them at all.

      Succ := Inlined.Table (Index).First_Succ;
      while Succ /= No_Succ loop
         Subp := Successors.Table (Succ).Subp;
         Inlined.Table (Subp).Count := Inlined.Table (Subp).Count - 1;

         if Inlined.Table (Subp).Count = 0
           and then Is_Called (Inlined.Table (Subp).Name)
         then
            Add_Inlined_Subprogram (Subp);
         end if;

         Succ := Successors.Table (Succ).Next;
      end loop;
   end Add_Inlined_Subprogram;

   ------------------------
   -- Add_Scope_To_Clean --
   ------------------------

   procedure Add_Scope_To_Clean (Inst : Entity_Id) is
      Scop : constant Entity_Id := Enclosing_Dynamic_Scope (Inst);
      Elmt : Elmt_Id;

   begin
      --  If the instance appears in a library-level package declaration,
      --  all finalization is global, and nothing needs doing here.

      if Scop = Standard_Standard then
         return;
      end if;

      --  If the instance appears within a generic subprogram there is nothing
      --  to finalize either.

      declare
         S : Entity_Id;

      begin
         S := Scope (Inst);
         while Present (S) and then S /= Standard_Standard loop
            if Is_Generic_Subprogram (S) then
               return;
            end if;

            S := Scope (S);
         end loop;
      end;

      Elmt := First_Elmt (To_Clean);
      while Present (Elmt) loop
         if Node (Elmt) = Scop then
            return;
         end if;

         Elmt := Next_Elmt (Elmt);
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
         Inlined.Table (Inlined.Last).First_Succ  := No_Succ;
         Inlined.Table (Inlined.Last).Count       := 0;
         Inlined.Table (Inlined.Last).Listed      := False;
         Inlined.Table (Inlined.Last).Main_Call   := False;
         Inlined.Table (Inlined.Last).Next        := No_Subp;
         Inlined.Table (Inlined.Last).Next_Nopred := No_Subp;
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
      S         : Succ_Index;

   begin
      Analyzing_Inlined_Bodies := False;

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

            --  Load the body, unless it the main unit, or is an instance
            --  whose body has already been analyzed.

            if Present (Comp_Unit)
              and then Comp_Unit /= Cunit (Main_Unit)
              and then Body_Required (Comp_Unit)
              and then (Nkind (Unit (Comp_Unit)) /= N_Package_Declaration
                         or else No (Corresponding_Body (Unit (Comp_Unit))))
            then
               declare
                  Bname : constant Unit_Name_Type :=
                            Get_Body_Name (Get_Unit_Name (Unit (Comp_Unit)));

                  OK : Boolean;

               begin
                  if not Is_Loaded (Bname) then
                     Load_Needed_Body (Comp_Unit, OK);

                     if not OK then

                        --  Warn that a body was not available for inlining
                        --  by the back-end.

                        Error_Msg_Unit_1 := Bname;
                        Error_Msg_N
                          ("one or more inlined subprograms accessed in $!?",
                           Comp_Unit);
                        Error_Msg_File_1 :=
                          Get_File_Name (Bname, Subunit => False);
                        Error_Msg_N ("\but file{ was not found!?", Comp_Unit);
                     end if;
                  end if;
               end;
            end if;

            J := J + 1;
         end loop;

         --  The analysis of required bodies may have produced additional
         --  generic instantiations. To obtain further inlining, we perform
         --  another round of generic body instantiations. Establishing a
         --  fully recursive loop between inlining and generic instantiations
         --  is unlikely to yield more than this one additional pass.

         Instantiate_Bodies;

         --  The list of inlined subprograms is an overestimate, because
         --  it includes inlined functions called from functions that are
         --  compiled as part of an inlined package, but are not themselves
         --  called. An accurate computation of just those subprograms that
         --  are needed requires that we perform a transitive closure over
         --  the call graph, starting from calls in the main program. Here
         --  we do one step of the inverse transitive closure, and reset
         --  the Is_Called flag on subprograms all of whose callers are not.

         for Index in Inlined.First .. Inlined.Last loop
            S := Inlined.Table (Index).First_Succ;

            if S /= No_Succ
              and then not Inlined.Table (Index).Main_Call
            then
               Set_Is_Called (Inlined.Table (Index).Name, False);

               while S /= No_Succ loop
                  if Is_Called
                    (Inlined.Table (Successors.Table (S).Subp).Name)
                   or else Inlined.Table (Successors.Table (S).Subp).Main_Call
                  then
                     Set_Is_Called (Inlined.Table (Index).Name);
                     exit;
                  end if;

                  S := Successors.Table (S).Next;
               end loop;
            end if;
         end loop;

         --  Now that the units are compiled, chain the subprograms within
         --  that are called and inlined. Produce list of inlined subprograms
         --  sorted in  topological order. Start with all subprograms that
         --  have no prerequisites, i.e. inlined subprograms that do not call
         --  other inlined subprograms.

         for Index in Inlined.First .. Inlined.Last loop

            if Is_Called (Inlined.Table (Index).Name)
              and then Inlined.Table (Index).Count = 0
              and then not Inlined.Table (Index).Listed
            then
               Add_Inlined_Subprogram (Index);
            end if;
         end loop;

         --  Because Add_Inlined_Subprogram treats recursively nodes that have
         --  no prerequisites left, at the end of the loop all subprograms
         --  must have been listed. If there are any unlisted subprograms
         --  left, there must be some recursive chains that cannot be inlined.

         for Index in Inlined.First .. Inlined.Last loop
            if Is_Called (Inlined.Table (Index).Name)
              and then Inlined.Table (Index).Count /= 0
              and then not Is_Predefined_File_Name
                (Unit_File_Name
                  (Get_Source_Unit (Inlined.Table (Index).Name)))
            then
               Error_Msg_N
                 ("& cannot be inlined?", Inlined.Table (Index).Name);

               --  A warning on the first one might be sufficient ???
            end if;
         end loop;

         Pop_Scope;
      end if;
   end Analyze_Inlined_Bodies;

   -----------------------------
   -- Check_Body_For_Inlining --
   -----------------------------

   procedure Check_Body_For_Inlining (N : Node_Id; P : Entity_Id) is
      Bname : Unit_Name_Type;
      E     : Entity_Id;
      OK    : Boolean;

   begin
      if Is_Compilation_Unit (P)
        and then not Is_Generic_Instance (P)
      then
         Bname := Get_Body_Name (Get_Unit_Name (Unit (N)));

         E := First_Entity (P);
         while Present (E) loop
            if Has_Pragma_Inline_Always (E)
              or else (Front_End_Inlining and then Has_Pragma_Inline (E))
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
                                   ("body of & depends on child unit&?",
                                      With_Clause, P);
                                 Error_Msg_N
                                   ("\subprograms in body cannot be inlined?",
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
                       ("unable to inline subprograms defined in $?", P);
                     Error_Msg_N ("\body not found?", P);
                     return;
                  end if;
               end if;

               return;
            end if;

            Next_Entity (E);
         end loop;
      end if;
   end Check_Body_For_Inlining;

   --------------------
   -- Cleanup_Scopes --
   --------------------

   procedure Cleanup_Scopes is
      Elmt : Elmt_Id;
      Decl : Node_Id;
      Scop : Entity_Id;

   begin
      Elmt := First_Elmt (To_Clean);
      while Present (Elmt) loop
         Scop := Node (Elmt);

         if Ekind (Scop) = E_Entry then
            Scop := Protected_Body_Subprogram (Scop);

         elsif Is_Subprogram (Scop)
           and then Is_Protected_Type (Scope (Scop))
           and then Present (Protected_Body_Subprogram (Scop))
         then
            --  If a protected operation contains an instance, its
            --  cleanup operations have been delayed, and the subprogram
            --  has been rewritten in the expansion of the enclosing
            --  protected body. It is the corresponding subprogram that
            --  may require the cleanup operations, so propagate the
            --  information that triggers cleanup activity.

            Set_Uses_Sec_Stack
              (Protected_Body_Subprogram (Scop),
                Uses_Sec_Stack (Scop));
            Set_Finalization_Chain_Entity
              (Protected_Body_Subprogram (Scop),
                Finalization_Chain_Entity (Scop));
            Scop := Protected_Body_Subprogram (Scop);
         end if;

         if Ekind (Scop) = E_Block then
            Decl := Parent (Block_Node (Scop));

         else
            Decl := Unit_Declaration_Node (Scop);

            if Nkind (Decl) = N_Subprogram_Declaration
              or else Nkind (Decl) = N_Task_Type_Declaration
              or else Nkind (Decl) = N_Subprogram_Body_Stub
            then
               Decl := Unit_Declaration_Node (Corresponding_Body (Decl));
            end if;
         end if;

         Push_Scope (Scop);
         Expand_Cleanup_Actions (Decl);
         End_Scope;

         Elmt := Next_Elmt (Elmt);
      end loop;
   end Cleanup_Scopes;

   --------------------------
   -- Has_Initialized_Type --
   --------------------------

   function Has_Initialized_Type (E : Entity_Id) return Boolean is
      E_Body : constant Node_Id := Get_Subprogram_Body (E);
      Decl   : Node_Id;

   begin
      if No (E_Body) then        --  imported subprogram
         return False;

      else
         Decl := First (Declarations (E_Body));
         while Present (Decl) loop

            if Nkind (Decl) = N_Full_Type_Declaration
              and then Present (Init_Proc (Defining_Identifier (Decl)))
            then
               return True;
            end if;

            Next (Decl);
         end loop;
      end if;

      return False;
   end Has_Initialized_Type;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Analyzing_Inlined_Bodies := False;
      Pending_Descriptor.Init;
      Pending_Instantiations.Init;
      Inlined_Bodies.Init;
      Successors.Init;
      Inlined.Init;

      for J in Hash_Headers'Range loop
         Hash_Headers (J) := No_Subp;
      end loop;
   end Initialize;

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
      J    : Int;
      Info : Pending_Body_Info;

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
         --  set (that's why we can't simply use a FOR loop here).

         J := 0;
         while J <= Pending_Instantiations.Last
           and then Serious_Errors_Detected = 0
         loop
            Info := Pending_Instantiations.Table (J);

            --  If the instantiation node is absent, it has been removed
            --  as part of unreachable code.

            if No (Info.Inst_Node) then
               null;

            elsif Nkind (Info.Act_Decl) = N_Package_Declaration then
               Instantiate_Package_Body (Info);
               Add_Scope_To_Clean (Defining_Entity (Info.Act_Decl));

            else
               Instantiate_Subprogram_Body (Info);
            end if;

            J := J + 1;
         end loop;

         --  Reset the table of instantiations. Additional instantiations
         --  may be added through inlining, when additional bodies are
         --  analyzed.

         Pending_Instantiations.Init;

         --  We can now complete the cleanup actions of scopes that contain
         --  pending instantiations (skipped for generic units, since we
         --  never need any cleanups in generic units).
         --  pending instantiations.

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
         if Ekind (Scop) in Subprogram_Kind then
            return True;

         elsif Ekind (Scop) = E_Task_Type
           or else Ekind (Scop) = E_Entry
           or else Ekind (Scop) = E_Entry_Family then
            return True;
         end if;

         Scop := Scope (Scop);
      end loop;

      return False;
   end Is_Nested;

   ----------
   -- Lock --
   ----------

   procedure Lock is
   begin
      Pending_Instantiations.Locked := True;
      Inlined_Bodies.Locked := True;
      Successors.Locked := True;
      Inlined.Locked := True;
      Pending_Instantiations.Release;
      Inlined_Bodies.Release;
      Successors.Release;
      Inlined.Release;
   end Lock;

   --------------------------
   -- Remove_Dead_Instance --
   --------------------------

   procedure Remove_Dead_Instance (N : Node_Id) is
      J : Int;

   begin
      J := 0;
      while J <= Pending_Instantiations.Last loop
         if Pending_Instantiations.Table (J).Inst_Node = N then
            Pending_Instantiations.Table (J).Inst_Node := Empty;
            return;
         end if;

         J := J + 1;
      end loop;
   end Remove_Dead_Instance;

   ------------------------
   -- Scope_In_Main_Unit --
   ------------------------

   function Scope_In_Main_Unit (Scop : Entity_Id) return Boolean is
      Comp : Node_Id;
      S    : Entity_Id;
      Ent  : Entity_Id := Cunit_Entity (Main_Unit);

   begin
      --  The scope may be within the main unit, or it may be an ancestor
      --  of the main unit, if the main unit is a child unit. In both cases
      --  it makes no sense to process the body before the main unit. In
      --  the second case, this may lead to circularities if a parent body
      --  depends on a child spec, and we are analyzing the child.

      S := Scop;
      while Scope (S) /= Standard_Standard
        and then not Is_Child_Unit (S)
      loop
         S := Scope (S);
      end loop;

      Comp := Parent (S);
      while Present (Comp)
        and then Nkind (Comp) /= N_Compilation_Unit
      loop
         Comp := Parent (Comp);
      end loop;

      if Is_Child_Unit (Ent) then
         while Present (Ent)
           and then Is_Child_Unit (Ent)
         loop
            if Scope (Ent) = S then
               return True;
            end if;

            Ent := Scope (Ent);
         end loop;
      end if;

      return
        Comp = Cunit (Main_Unit)
          or else Comp = Library_Unit (Cunit (Main_Unit));
   end Scope_In_Main_Unit;

end Inline;
