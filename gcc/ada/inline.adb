------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               I N L I N E                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2012, Free Software Foundation, Inc.         --
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
      Next        : Subp_Index := No_Subp;
      First_Succ  : Succ_Index := No_Succ;
      Listed      : Boolean    := False;
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

   function Get_Code_Unit_Entity (E : Entity_Id) return Entity_Id;
   pragma Inline (Get_Code_Unit_Entity);
   --  Return the entity node for the unit containing E. Always return
   --  the spec for a package.

   function In_Main_Unit_Or_Subunit (E : Entity_Id) return Boolean;
   --  Return True if E is in the main unit or its spec or in a subunit

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
   --  Add the subprogram to the list of inlined subprogram for the unit

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

   procedure Add_Inlined_Body (E : Entity_Id) is

      type Inline_Level_Type is (Dont_Inline, Inline_Call, Inline_Package);
      --  Level of inlining for the call: Dont_Inline means no inlining,
      --  Inline_Call means that only the call is considered for inlining,
      --  Inline_Package means that the call is considered for inlining and
      --  its package compiled and scanned for more inlining opportunities.

      function Must_Inline return Inline_Level_Type;
      --  Inlining is only done if the call statement N is in the main unit,
      --  or within the body of another inlined subprogram.

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

         --  The call is not in the main unit. See if it is in some inlined
         --  subprogram. If so, inline the call and, if the inlining level is
         --  set to 1, stop there; otherwise also compile the package as above.

         Scop := Current_Scope;
         while Scope (Scop) /= Standard_Standard
           and then not Is_Child_Unit (Scop)
         loop
            if Is_Overloadable (Scop)
              and then Is_Inlined (Scop)
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

      Level : Inline_Level_Type;

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

      if Is_Abstract_Subprogram (E)
        or else Is_Nested (E)
        or else Convention (E) = Convention_Protected
      then
         return;
      end if;

      Level := Must_Inline;
      if Level /= Dont_Inline then
         declare
            Pack : constant Entity_Id := Get_Code_Unit_Entity (E);

         begin
            if Pack = E then

               --  Library-level inlined function. Add function itself to
               --  list of needed units.

               Set_Is_Called (E);
               Inlined_Bodies.Increment_Last;
               Inlined_Bodies.Table (Inlined_Bodies.Last) := E;

            elsif Ekind (Pack) = E_Package then
               Set_Is_Called (E);

               if Is_Generic_Instance (Pack) then
                  null;

               --  Do not inline the package if the subprogram is an init proc
               --  or other internally generated subprogram, because in that
               --  case the subprogram body appears in the same unit that
               --  declares the type, and that body is visible to the back end.
               --  Do not inline it either if it is in the main unit.

               elsif Level = Inline_Package
                 and then not Is_Inlined (Pack)
                 and then Comes_From_Source (E)
                 and then not In_Main_Unit_Or_Subunit (Pack)
               then
                  Set_Is_Inlined (Pack);
                  Inlined_Bodies.Increment_Last;
                  Inlined_Bodies.Table (Inlined_Bodies.Last) := Pack;
               end if;
            end if;
         end;
      end if;
   end Add_Inlined_Body;

   ----------------------------
   -- Add_Inlined_Subprogram --
   ----------------------------

   procedure Add_Inlined_Subprogram (Index : Subp_Index) is
      E    : constant Entity_Id := Inlined.Table (Index).Name;
      Pack : constant Entity_Id := Get_Code_Unit_Entity (E);

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
      --  This procedure must be carefully coordinated with the back end.

      ----------------------------
      -- Back_End_Cannot_Inline --
      ----------------------------

      function Back_End_Cannot_Inline (Subp : Entity_Id) return Boolean is
         Decl     : constant Node_Id := Unit_Declaration_Node (Subp);
         Body_Ent : Entity_Id;
         Ent      : Entity_Id;

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

         return False;
      end Back_End_Cannot_Inline;

   --  Start of processing for Add_Inlined_Subprogram

   begin
      --  If the subprogram is to be inlined, and if its unit is known to be
      --  inlined or is an instance whose body will be analyzed anyway or the
      --  subprogram has been generated by the compiler, and if it is declared
      --  at the library level not in the main unit, and if it can be inlined
      --  by the back-end, then insert it in the list of inlined subprograms.

      if Is_Inlined (E)
        and then (Is_Inlined (Pack)
                    or else Is_Generic_Instance (Pack)
                    or else Is_Internal (E))
        and then not In_Main_Unit_Or_Subunit (E)
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

      --  If the instance is within a generic unit, no finalization code
      --  can be generated. Note that at this point all bodies have been
      --  analyzed, and the scope stack itself is not present, and the flag
      --  Inside_A_Generic is not set.

      declare
         S : Entity_Id;

      begin
         S := Scope (Inst);
         while Present (S) and then S /= Standard_Standard loop
            if Is_Generic_Unit (S) then
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
         Inlined.Table (Inlined.Last).Next        := No_Subp;
         Inlined.Table (Inlined.Last).First_Succ  := No_Succ;
         Inlined.Table (Inlined.Last).Listed      := False;
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

      function Is_Ancestor_Of_Main
        (U_Name : Entity_Id;
         Nam    : Node_Id) return Boolean;
      --  Determine whether the unit whose body is loaded is an ancestor of
      --  the main unit, and has a with_clause on it. The body is not
      --  analyzed yet, so the check is purely lexical: the name of the with
      --  clause is a selected component, and names of ancestors must match.

      -------------------------
      -- Is_Ancestor_Of_Main --
      -------------------------

      function Is_Ancestor_Of_Main
        (U_Name : Entity_Id;
         Nam    : Node_Id) return Boolean
      is
         Pref : Node_Id;

      begin
         if Nkind (Nam) /= N_Selected_Component then
            return False;

         else
            if Chars (Selector_Name (Nam)) /=
               Chars (Cunit_Entity (Main_Unit))
            then
               return False;
            end if;

            Pref := Prefix (Nam);
            if Nkind (Pref) = N_Identifier then

               --  Par is an ancestor of Par.Child.

               return Chars (Pref) = Chars (U_Name);

            elsif Nkind (Pref) = N_Selected_Component
              and then Chars (Selector_Name (Pref)) = Chars (U_Name)
            then
               --  Par.Child is an ancestor of Par.Child.Grand.

               return True;   --  should check that ancestor match

            else
               --  A is an ancestor of A.B.C if it is an ancestor of A.B

               return Is_Ancestor_Of_Main (U_Name, Pref);
            end if;
         end if;
      end Is_Ancestor_Of_Main;

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

            --  Load the body, unless it is the main unit, or is an instance
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
                     Style_Check := False;
                     Load_Needed_Body (Comp_Unit, OK, Do_Analyze => False);

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

                     else
                        --  If the package to be inlined is an ancestor unit of
                        --  the main unit, and it has a semantic dependence on
                        --  it, the inlining cannot take place to prevent an
                        --  elaboration circularity. The desired body is not
                        --  analyzed yet, to prevent the completion of Taft
                        --  amendment types that would lead to elaboration
                        --  circularities in gigi.

                        declare
                           U_Id      : constant Entity_Id :=
                                         Defining_Entity (Unit (Comp_Unit));
                           Body_Unit : constant Node_Id :=
                                         Library_Unit (Comp_Unit);
                           Item      : Node_Id;

                        begin
                           Item := First (Context_Items (Body_Unit));
                           while Present (Item) loop
                              if Nkind (Item) = N_With_Clause
                                and then
                                  Is_Ancestor_Of_Main (U_Id, Name (Item))
                              then
                                 Set_Is_Inlined (U_Id, False);
                                 exit;
                              end if;

                              Next (Item);
                           end loop;

                           --  If no suspicious with_clauses, analyze the body.

                           if Is_Inlined (U_Id) then
                              Semantics (Body_Unit);
                           end if;
                        end;
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

         --  The list of inlined subprograms is an overestimate, because it
         --  includes inlined functions called from functions that are compiled
         --  as part of an inlined package, but are not themselves called. An
         --  accurate computation of just those subprograms that are needed
         --  requires that we perform a transitive closure over the call graph,
         --  starting from calls in the main program.

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
            if Is_Called (Inlined.Table (Index).Name)
              and then not Inlined.Table (Index).Listed
            then
               Add_Inlined_Subprogram (Index);
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

end Inline;
