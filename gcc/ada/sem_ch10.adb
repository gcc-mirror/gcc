------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ C H 1 0                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2007, Free Software Foundation, Inc.         --
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
with Debug;    use Debug;
with Einfo;    use Einfo;
with Errout;   use Errout;
with Exp_Util; use Exp_Util;
with Fname;    use Fname;
with Fname.UF; use Fname.UF;
with Freeze;   use Freeze;
with Impunit;  use Impunit;
with Inline;   use Inline;
with Lib;      use Lib;
with Lib.Load; use Lib.Load;
with Lib.Xref; use Lib.Xref;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Output;   use Output;
with Restrict; use Restrict;
with Rident;   use Rident;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Ch3;  use Sem_Ch3;
with Sem_Ch6;  use Sem_Ch6;
with Sem_Ch7;  use Sem_Ch7;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Dist; use Sem_Dist;
with Sem_Prag; use Sem_Prag;
with Sem_Util; use Sem_Util;
with Sem_Warn; use Sem_Warn;
with Stand;    use Stand;
with Sinfo;    use Sinfo;
with Sinfo.CN; use Sinfo.CN;
with Sinput;   use Sinput;
with Snames;   use Snames;
with Style;    use Style;
with Stylesw;  use Stylesw;
with Tbuild;   use Tbuild;
with Uname;    use Uname;

package body Sem_Ch10 is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Analyze_Context (N : Node_Id);
   --  Analyzes items in the context clause of compilation unit

   procedure Build_Limited_Views (N : Node_Id);
   --  Build and decorate the list of shadow entities for a package mentioned
   --  in a limited_with clause. If the package was not previously analyzed
   --  then it also performs a basic decoration of the real entities; this
   --  is required to do not pass non-decorated entities to the back-end.
   --  Implements Ada 2005 (AI-50217).

   procedure Check_Body_Needed_For_SAL (Unit_Name : Entity_Id);
   --  Check whether the source for the body of a compilation unit must
   --  be included in a standalone library.

   procedure Check_Private_Child_Unit (N : Node_Id);
   --  If a with_clause mentions a private child unit, the compilation
   --  unit must be a member of the same family, as described in 10.1.2.

   procedure Check_Stub_Level (N : Node_Id);
   --  Verify that a stub is declared immediately within a compilation unit,
   --  and not in an inner frame.

   procedure Expand_With_Clause (Item : Node_Id; Nam : Node_Id; N : Node_Id);
   --  When a child unit appears in a context clause, the implicit withs on
   --  parents are made explicit, and with clauses are inserted in the context
   --  clause before the one for the child. If a parent in the with_clause
   --  is a renaming, the implicit with_clause is on the renaming whose name
   --  is mentioned in the with_clause, and not on the package it renames.
   --  N is the compilation unit whose list of context items receives the
   --  implicit with_clauses.

   function Get_Parent_Entity (Unit : Node_Id) return Entity_Id;
   --  Get defining entity of parent unit of a child unit. In most cases this
   --  is the defining entity of the unit, but for a child instance whose
   --  parent needs a body for inlining, the instantiation node of the parent
   --  has not yet been rewritten as a package declaration, and the entity has
   --  to be retrieved from the Instance_Spec of the unit.

   procedure Implicit_With_On_Parent (Child_Unit : Node_Id; N : Node_Id);
   --  If the main unit is a child unit, implicit withs are also added for
   --  all its ancestors.

   function In_Chain (E : Entity_Id) return Boolean;
   --  Check that the shadow entity is not already in the homonym chain, for
   --  example through a limited_with clause in a parent unit.

   procedure Install_Context_Clauses (N : Node_Id);
   --  Subsidiary to Install_Context and Install_Parents. Process only with_
   --  and use_clauses for current unit and its library unit if any.

   procedure Install_Limited_Context_Clauses (N : Node_Id);
   --  Subsidiary to Install_Context. Process only limited with_clauses
   --  for current unit. Implements Ada 2005 (AI-50217).

   procedure Install_Limited_Withed_Unit (N : Node_Id);
   --  Place shadow entities for a limited_with package in the visibility
   --  structures for the current compilation. Implements Ada 2005 (AI-50217).

   procedure Install_Withed_Unit
     (With_Clause     : Node_Id;
      Private_With_OK : Boolean := False);
   --  If the unit is not a child unit, make unit immediately visible.
   --  The caller ensures that the unit is not already currently installed.
   --  The flag Private_With_OK is set true in Install_Private_With_Clauses,
   --  which is called when compiling the private part of a package, or
   --  installing the private declarations of a parent unit.

   procedure Install_Parents (Lib_Unit : Node_Id; Is_Private : Boolean);
   --  This procedure establishes the context for the compilation of a child
   --  unit. If Lib_Unit is a child library spec then the context of the parent
   --  is installed, and the parent itself made immediately visible, so that
   --  the child unit is processed in the declarative region of the parent.
   --  Install_Parents makes a recursive call to itself to ensure that all
   --  parents are loaded in the nested case. If Lib_Unit is a library body,
   --  the only effect of Install_Parents is to install the private decls of
   --  the parents, because the visible parent declarations will have been
   --  installed as part of the context of the corresponding spec.

   procedure Install_Siblings (U_Name : Entity_Id; N : Node_Id);
   --  In the compilation of a child unit, a child of any of the  ancestor
   --  units is directly visible if it is visible, because the parent is in
   --  an enclosing scope. Iterate over context to find child units of U_Name
   --  or of some ancestor of it.

   function Is_Child_Spec (Lib_Unit : Node_Id) return Boolean;
   --  Lib_Unit is a library unit which may be a spec or a body. Is_Child_Spec
   --  returns True if Lib_Unit is a library spec which is a child spec, i.e.
   --  a library spec that has a parent. If the call to Is_Child_Spec returns
   --  True, then Parent_Spec (Lib_Unit) is non-Empty and points to the
   --  compilation unit for the parent spec.
   --
   --  Lib_Unit can also be a subprogram body that acts as its own spec. If
   --  the Parent_Spec is  non-empty, this is also a child unit.

   procedure Remove_Context_Clauses (N : Node_Id);
   --  Subsidiary of previous one. Remove use_ and with_clauses

   procedure Remove_Limited_With_Clause (N : Node_Id);
   --  Remove from visibility the shadow entities introduced for a package
   --  mentioned in a limited_with clause. Implements Ada 2005 (AI-50217).

   procedure Remove_Parents (Lib_Unit : Node_Id);
   --  Remove_Parents checks if Lib_Unit is a child spec. If so then the parent
   --  contexts established by the corresponding call to Install_Parents are
   --  removed. Remove_Parents contains a recursive call to itself to ensure
   --  that all parents are removed in the nested case.

   procedure Remove_Unit_From_Visibility (Unit_Name : Entity_Id);
   --  Reset all visibility flags on unit after compiling it, either as a
   --  main unit or as a unit in the context.

   procedure Unchain (E : Entity_Id);
   --  Remove single entity from visibility list

   procedure Analyze_Proper_Body (N : Node_Id; Nam : Entity_Id);
   --  Common processing for all stubs (subprograms, tasks, packages, and
   --  protected cases). N is the stub to be analyzed. Once the subunit
   --  name is established, load and analyze. Nam is the non-overloadable
   --  entity for which the proper body provides a completion. Subprogram
   --  stubs are handled differently because they can be declarations.

   procedure sm;
   --  A dummy procedure, for debugging use, called just before analyzing the
   --  main unit (after dealing with any context clauses).

   --------------------------
   -- Limited_With_Clauses --
   --------------------------

   --  Limited_With clauses are the mechanism chosen for Ada05 to support
   --  mutually recursive types declared in different units. A limited_with
   --  clause that names package P in the context of unit U makes the types
   --  declared in the visible part of P available within U, but with the
   --  restriction that these types can only be used as incomplete types.
   --  The limited_with clause does not impose a semantic dependence on P,
   --  and it is possible for two packages to have limited_with_clauses on
   --  each other without creating an elaboration circularity.

   --  To support this feature, the analysis of a limited_with clause must
   --  create an abbreviated view of the package, without performing any
   --  semantic analysis on it. This "package abstract" contains shadow
   --  types that are in one-one correspondence with the real types in the
   --  package, and that have the properties of incomplete types.

   --  The implementation creates two element lists: one to chain the shadow
   --  entities, and one to chain the corresponding type entities in the tree
   --  of the package. Links between corresponding entities in both chains
   --  allow the compiler to select the proper view of a given type, depending
   --  on the context. Note that in contrast with the handling of private
   --  types, the limited view and the non-limited view of a type are treated
   --  as separate entities, and no entity exchange needs to take place, which
   --  makes the implementation must simpler than could be feared.

   ------------------------------
   -- Analyze_Compilation_Unit --
   ------------------------------

   procedure Analyze_Compilation_Unit (N : Node_Id) is
      Unit_Node     : constant Node_Id := Unit (N);
      Lib_Unit      : Node_Id          := Library_Unit (N);
      Spec_Id       : Entity_Id;
      Main_Cunit    : constant Node_Id := Cunit (Main_Unit);
      Par_Spec_Name : Unit_Name_Type;
      Unum          : Unit_Number_Type;

      procedure Check_Redundant_Withs
        (Context_Items      : List_Id;
         Spec_Context_Items : List_Id := No_List);
      --  Determine whether the context list of a compilation unit contains
      --  redundant with clauses. When checking body clauses against spec
      --  clauses, set Context_Items to the context list of the body and
      --  Spec_Context_Items to that of the spec. Parent packages are not
      --  examined for documentation purposes.

      procedure Generate_Parent_References (N : Node_Id; P_Id : Entity_Id);
      --  Generate cross-reference information for the parents of child units.
      --  N is a defining_program_unit_name, and P_Id is the immediate parent.

      ---------------------------
      -- Check_Redundant_Withs --
      ---------------------------

      procedure Check_Redundant_Withs
        (Context_Items      : List_Id;
         Spec_Context_Items : List_Id := No_List)
      is
         Clause : Node_Id;

         procedure Process_Body_Clauses
          (Context_List      : List_Id;
           Clause            : Node_Id;
           Used              : in out Boolean;
           Used_Type_Or_Elab : in out Boolean);
         --  Examine the context clauses of a package body, trying to match
         --  the name entity of Clause with any list element. If the match
         --  occurs on a use package clause, set Used to True, for a use
         --  type clause, pragma Elaborate or pragma Elaborate_All, set
         --  Used_Type_Or_Elab to True.

         procedure Process_Spec_Clauses
          (Context_List : List_Id;
           Clause       : Node_Id;
           Used         : in out Boolean;
           Withed       : in out Boolean;
           Exit_On_Self : Boolean := False);
         --  Examine the context clauses of a package spec, trying to match
         --  the name entity of Clause with any list element. If the match
         --  occurs on a use package clause, set Used to True, for a with
         --  package clause other than Clause, set Withed to True. Limited
         --  with clauses, implicitly generated with clauses and withs
         --  having pragmas Elaborate or Elaborate_All applied to them are
         --  skipped. Exit_On_Self is used to control the search loop and
         --  force an exit whenever Clause sees itself in the search.

         --------------------------
         -- Process_Body_Clauses --
         --------------------------

         procedure Process_Body_Clauses
          (Context_List      : List_Id;
           Clause            : Node_Id;
           Used              : in out Boolean;
           Used_Type_Or_Elab : in out Boolean)
         is
            Nam_Ent   : constant Entity_Id := Entity (Name (Clause));
            Cont_Item : Node_Id;
            Prag_Unit : Node_Id;
            Subt_Mark : Node_Id;
            Use_Item  : Node_Id;

            function Same_Unit (N : Node_Id; P : Entity_Id) return Boolean;
            --  In an expanded name in a use clause, if the prefix is a
            --  renamed package, the entity is set to the original package
            --  as a result, when checking whether the package appears in a
            --  previous with_clause, the renaming has to be taken into
            --  account, to prevent spurious or incorrect warnings. The
            --  common case is the use of Text_IO.

            ---------------
            -- Same_Unit --
            ---------------

            function Same_Unit (N : Node_Id; P : Entity_Id) return Boolean is
            begin
               return Entity (N) = P
                 or else
                   (Present (Renamed_Object (P))
                     and then Entity (N) = Renamed_Object (P));
            end Same_Unit;

         --  Start of processing for Process_Body_Clauses

         begin
            Used := False;
            Used_Type_Or_Elab := False;

            Cont_Item := First (Context_List);
            while Present (Cont_Item) loop

               --  Package use clause

               if Nkind (Cont_Item) = N_Use_Package_Clause
                 and then not Used
               then
                  --  Search through use clauses

                  Use_Item := First (Names (Cont_Item));
                  while Present (Use_Item) and then not Used loop

                     --  Case of a direct use of the one we are looking for

                     if Entity (Use_Item) = Nam_Ent then
                        Used := True;

                     --  Handle nested case, as in "with P; use P.Q.R"

                     else
                        declare
                           UE : Node_Id;

                        begin
                           --  Loop through prefixes looking for match

                           UE := Use_Item;
                           while Nkind (UE) = N_Expanded_Name loop
                              if Same_Unit (Prefix (UE), Nam_Ent) then
                                 Used := True;
                                 exit;
                              end if;

                              UE := Prefix (UE);
                           end loop;
                        end;
                     end if;

                     Next (Use_Item);
                  end loop;

               --  USE TYPE clause

               elsif Nkind (Cont_Item) = N_Use_Type_Clause
                 and then not Used_Type_Or_Elab
               then
                  Subt_Mark := First (Subtype_Marks (Cont_Item));
                  while Present (Subt_Mark)
                    and then not Used_Type_Or_Elab
                  loop
                     if Same_Unit (Prefix (Subt_Mark), Nam_Ent) then
                        Used_Type_Or_Elab := True;
                     end if;

                     Next (Subt_Mark);
                  end loop;

               --  Pragma Elaborate or Elaborate_All

               elsif Nkind (Cont_Item) = N_Pragma
                 and then
                   (Chars (Cont_Item) = Name_Elaborate
                      or else
                    Chars (Cont_Item) = Name_Elaborate_All)
                 and then not Used_Type_Or_Elab
               then
                  Prag_Unit :=
                    First (Pragma_Argument_Associations (Cont_Item));
                  while Present (Prag_Unit)
                    and then not Used_Type_Or_Elab
                  loop
                     if Entity (Expression (Prag_Unit)) = Nam_Ent then
                        Used_Type_Or_Elab := True;
                     end if;

                     Next (Prag_Unit);
                  end loop;
               end if;

               Next (Cont_Item);
            end loop;
         end Process_Body_Clauses;

         --------------------------
         -- Process_Spec_Clauses --
         --------------------------

         procedure Process_Spec_Clauses
          (Context_List : List_Id;
           Clause       : Node_Id;
           Used         : in out Boolean;
           Withed       : in out Boolean;
           Exit_On_Self : Boolean := False)
         is
            Nam_Ent   : constant Entity_Id := Entity (Name (Clause));
            Cont_Item : Node_Id;
            Use_Item  : Node_Id;

         begin
            Used := False;
            Withed := False;

            Cont_Item := First (Context_List);
            while Present (Cont_Item) loop

               --  Stop the search since the context items after Cont_Item
               --  have already been examined in a previous iteration of
               --  the reverse loop in Check_Redundant_Withs.

               if Exit_On_Self
                 and Cont_Item = Clause
               then
                  exit;
               end if;

               --  Package use clause

               if Nkind (Cont_Item) = N_Use_Package_Clause
                 and then not Used
               then
                  Use_Item := First (Names (Cont_Item));
                  while Present (Use_Item) and then not Used loop
                     if Entity (Use_Item) = Nam_Ent then
                        Used := True;
                     end if;

                     Next (Use_Item);
                  end loop;

               --  Package with clause. Avoid processing self, implicitly
               --  generated with clauses or limited with clauses. Note
               --  that we examine with clauses having pragmas Elaborate
               --  or Elaborate_All applied to them due to cases such as:
               --
               --     with Pack;
               --     with Pack;
               --     pragma Elaborate (Pack);
               --
               --  In this case, the second with clause is redundant since
               --  the pragma applies only to the first "with Pack;".

               elsif Nkind (Cont_Item) = N_With_Clause
                 and then not Implicit_With (Cont_Item)
                 and then not Limited_Present (Cont_Item)
                 and then Cont_Item /= Clause
                 and then Entity (Name (Cont_Item)) = Nam_Ent
               then
                  Withed := True;
               end if;

               Next (Cont_Item);
            end loop;
         end Process_Spec_Clauses;

      --  Start of processing for Check_Redundant_Withs

      begin
         Clause := Last (Context_Items);
         while Present (Clause) loop

            --  Avoid checking implicitly generated with clauses, limited
            --  with clauses or withs that have pragma Elaborate or
            --  Elaborate_All apllied.

            if Nkind (Clause) = N_With_Clause
              and then not Implicit_With (Clause)
              and then not Limited_Present (Clause)
              and then not Elaborate_Present (Clause)
            then
               --  Package body-to-spec check

               if Present (Spec_Context_Items) then
                  declare
                     Used_In_Body      : Boolean := False;
                     Used_In_Spec      : Boolean := False;
                     Used_Type_Or_Elab : Boolean := False;
                     Withed_In_Spec    : Boolean := False;

                  begin
                     Process_Spec_Clauses
                      (Context_List => Spec_Context_Items,
                       Clause       => Clause,
                       Used         => Used_In_Spec,
                       Withed       => Withed_In_Spec);

                     Process_Body_Clauses
                      (Context_List      => Context_Items,
                       Clause            => Clause,
                       Used              => Used_In_Body,
                       Used_Type_Or_Elab => Used_Type_Or_Elab);

                     --  "Type Elab" refers to the presence of either a use
                     --  type clause, pragmas Elaborate or Elaborate_All.

                     --  +---------------+---------------------------+------+
                     --  | Spec          | Body                      | Warn |
                     --  +--------+------+--------+------+-----------+------+
                     --  | Withed | Used | Withed | Used | Type Elab |      |
                     --  |   X    |      |   X    |      |           |  X   |
                     --  |   X    |      |   X    |  X   |           |      |
                     --  |   X    |      |   X    |      |     X     |      |
                     --  |   X    |      |   X    |  X   |     X     |      |
                     --  |   X    |  X   |   X    |      |           |  X   |
                     --  |   X    |  X   |   X    |      |     X     |      |
                     --  |   X    |  X   |   X    |  X   |           |  X   |
                     --  |   X    |  X   |   X    |  X   |     X     |      |
                     --  +--------+------+--------+------+-----------+------+

                     if (Withed_In_Spec
                           and then not Used_Type_Or_Elab)
                             and then
                               ((not Used_In_Spec
                                   and then not Used_In_Body)
                                     or else
                                       Used_In_Spec)
                     then
                        Error_Msg_N ("?redundant with clause in body", Clause);
                     end if;

                     Used_In_Body := False;
                     Used_In_Spec := False;
                     Used_Type_Or_Elab := False;
                     Withed_In_Spec := False;
                  end;

               --  Standalone package spec or body check

               else
                  declare
                     Dont_Care : Boolean := False;
                     Withed    : Boolean := False;

                  begin
                     --  The mechanism for examining the context clauses of a
                     --  package spec can be applied to package body clauses.

                     Process_Spec_Clauses
                      (Context_List => Context_Items,
                       Clause       => Clause,
                       Used         => Dont_Care,
                       Withed       => Withed,
                       Exit_On_Self => True);

                     if Withed then
                        Error_Msg_N ("?redundant with clause", Clause);
                     end if;
                  end;
               end if;
            end if;

            Prev (Clause);
         end loop;
      end Check_Redundant_Withs;

      --------------------------------
      -- Generate_Parent_References --
      --------------------------------

      procedure Generate_Parent_References (N : Node_Id; P_Id : Entity_Id) is
         Pref   : Node_Id;
         P_Name : Entity_Id := P_Id;

      begin
         Pref := Name (Parent (Defining_Entity (N)));

         if Nkind (Pref) = N_Expanded_Name then

            --  Done already, if the unit has been compiled indirectly as
            --  part of the closure of its context because of inlining.

            return;
         end if;

         while Nkind (Pref) = N_Selected_Component loop
            Change_Selected_Component_To_Expanded_Name (Pref);
            Set_Entity (Pref, P_Name);
            Set_Etype (Pref, Etype (P_Name));
            Generate_Reference (P_Name, Pref, 'r');
            Pref   := Prefix (Pref);
            P_Name := Scope (P_Name);
         end loop;

         --  The guard here on P_Name is to handle the error condition where
         --  the parent unit is missing because the file was not found.

         if Present (P_Name) then
            Set_Entity (Pref, P_Name);
            Set_Etype (Pref, Etype (P_Name));
            Generate_Reference (P_Name, Pref, 'r');
            Style.Check_Identifier (Pref, P_Name);
         end if;
      end Generate_Parent_References;

   --  Start of processing for Analyze_Compilation_Unit

   begin
      Process_Compilation_Unit_Pragmas (N);

      --  If the unit is a subunit whose parent has not been analyzed (which
      --  indicates that the main unit is a subunit, either the current one or
      --  one of its descendents) then the subunit is compiled as part of the
      --  analysis of the parent, which we proceed to do. Basically this gets
      --  handled from the top down and we don't want to do anything at this
      --  level (i.e. this subunit will be handled on the way down from the
      --  parent), so at this level we immediately return. If the subunit
      --  ends up not analyzed, it means that the parent did not contain a
      --  stub for it, or that there errors were dectected in some ancestor.

      if Nkind (Unit_Node) = N_Subunit
        and then not Analyzed (Lib_Unit)
      then
         Semantics (Lib_Unit);

         if not Analyzed (Proper_Body (Unit_Node)) then
            if Serious_Errors_Detected > 0 then
               Error_Msg_N ("subunit not analyzed (errors in parent unit)", N);
            else
               Error_Msg_N ("missing stub for subunit", N);
            end if;
         end if;

         return;
      end if;

      --  Analyze context (this will call Sem recursively for with'ed units)

      Analyze_Context (N);

      --  If the unit is a package body, the spec is already loaded and must
      --  be analyzed first, before we analyze the body.

      if Nkind (Unit_Node) = N_Package_Body then

         --  If no Lib_Unit, then there was a serious previous error, so
         --  just ignore the entire analysis effort

         if No (Lib_Unit) then
            return;

         else
            Semantics (Lib_Unit);
            Check_Unused_Withs (Get_Cunit_Unit_Number (Lib_Unit));

            --  Verify that the library unit is a package declaration

            if not Nkind_In (Unit (Lib_Unit), N_Package_Declaration,
                                              N_Generic_Package_Declaration)
            then
               Error_Msg_N
                 ("no legal package declaration for package body", N);
               return;

            --  Otherwise, the entity in the declaration is visible. Update
            --  the version to reflect dependence of this body on the spec.

            else
               Spec_Id := Defining_Entity (Unit (Lib_Unit));
               Set_Is_Immediately_Visible (Spec_Id, True);
               Version_Update (N, Lib_Unit);

               if Nkind (Defining_Unit_Name (Unit_Node)) =
                                             N_Defining_Program_Unit_Name
               then
                  Generate_Parent_References (Unit_Node, Scope (Spec_Id));
               end if;
            end if;
         end if;

      --  If the unit is a subprogram body, then we similarly need to analyze
      --  its spec. However, things are a little simpler in this case, because
      --  here, this analysis is done only for error checking and consistency
      --  purposes, so there's nothing else to be done.

      elsif Nkind (Unit_Node) = N_Subprogram_Body then
         if Acts_As_Spec (N) then

            --  If the subprogram body is a child unit, we must create a
            --  declaration for it, in order to properly load the parent(s).
            --  After this, the original unit does not acts as a spec, because
            --  there is an explicit one. If this unit appears in a context
            --  clause, then an implicit with on the parent will be added when
            --  installing the context. If this is the main unit, there is no
            --  Unit_Table entry for the declaration (it has the unit number
            --  of the main unit) and code generation is unaffected.

            Unum := Get_Cunit_Unit_Number (N);
            Par_Spec_Name := Get_Parent_Spec_Name (Unit_Name (Unum));

            if Par_Spec_Name /= No_Unit_Name then
               Unum :=
                 Load_Unit
                   (Load_Name  => Par_Spec_Name,
                    Required   => True,
                    Subunit    => False,
                    Error_Node => N);

               if Unum /= No_Unit then

                  --  Build subprogram declaration and attach parent unit to it
                  --  This subprogram declaration does not come from source,
                  --  Nevertheless the backend must generate debugging info for
                  --  it, and this must be indicated explicitly. We also mark
                  --  the body entity as a child unit now, to prevent a
                  --  cascaded error if the spec entity cannot be entered
                  --  in its scope.

                  declare
                     Loc : constant Source_Ptr := Sloc (N);
                     SCS : constant Boolean :=
                             Get_Comes_From_Source_Default;

                  begin
                     Set_Comes_From_Source_Default (False);
                     Lib_Unit :=
                       Make_Compilation_Unit (Loc,
                         Context_Items => New_Copy_List (Context_Items (N)),
                         Unit =>
                           Make_Subprogram_Declaration (Sloc (N),
                             Specification =>
                               Copy_Separate_Tree
                                 (Specification (Unit_Node))),
                         Aux_Decls_Node =>
                           Make_Compilation_Unit_Aux (Loc));

                     Set_Library_Unit (N, Lib_Unit);
                     Set_Parent_Spec (Unit (Lib_Unit), Cunit (Unum));
                     Semantics (Lib_Unit);

                     --  Now that a separate declaration exists, the body
                     --  of the child unit does not act as spec any longer.

                     Set_Acts_As_Spec (N, False);
                     Set_Is_Child_Unit (Defining_Entity (Unit_Node));
                     Set_Needs_Debug_Info (Defining_Entity (Unit (Lib_Unit)));
                     Set_Comes_From_Source_Default (SCS);
                  end;
               end if;
            end if;

         --  Here for subprogram with separate declaration

         else
            Semantics (Lib_Unit);
            Check_Unused_Withs (Get_Cunit_Unit_Number (Lib_Unit));
            Version_Update (N, Lib_Unit);
         end if;

         if Nkind (Defining_Unit_Name (Specification (Unit_Node))) =
                                             N_Defining_Program_Unit_Name
         then
            Generate_Parent_References (
              Specification (Unit_Node),
                Scope (Defining_Entity (Unit (Lib_Unit))));
         end if;
      end if;

      --  If it is a child unit, the parent must be elaborated first
      --  and we update version, since we are dependent on our parent.

      if Is_Child_Spec (Unit_Node) then

         --  The analysis of the parent is done with style checks off

         declare
            Save_Style_Check : constant Boolean := Style_Check;
            Save_C_Restrict  : constant Save_Cunit_Boolean_Restrictions :=
                                 Cunit_Boolean_Restrictions_Save;

         begin
            if not GNAT_Mode then
               Style_Check := False;
            end if;

            Semantics (Parent_Spec (Unit_Node));
            Version_Update (N, Parent_Spec (Unit_Node));
            Style_Check := Save_Style_Check;
            Cunit_Boolean_Restrictions_Restore (Save_C_Restrict);
         end;
      end if;

      --  With the analysis done, install the context. Note that we can't
      --  install the context from the with clauses as we analyze them, because
      --  each with clause must be analyzed in a clean visibility context, so
      --  we have to wait and install them all at once.

      Install_Context (N);

      if Is_Child_Spec (Unit_Node) then

         --  Set the entities of all parents in the program_unit_name

         Generate_Parent_References (
           Unit_Node, Get_Parent_Entity (Unit (Parent_Spec (Unit_Node))));
      end if;

      --  All components of the context: with-clauses, library unit, ancestors
      --  if any, (and their context)  are analyzed and installed.

      --  Call special debug routine sm if this is the main unit

      if Current_Sem_Unit = Main_Unit then
         sm;
      end if;

      --  Now analyze the unit (package, subprogram spec, body) itself

      Analyze (Unit_Node);

      if Warn_On_Redundant_Constructs then
         Check_Redundant_Withs (Context_Items (N));

         if Nkind (Unit_Node) = N_Package_Body then
            Check_Redundant_Withs
              (Context_Items      => Context_Items (N),
               Spec_Context_Items => Context_Items (Lib_Unit));
         end if;
      end if;

      --  The above call might have made Unit_Node an N_Subprogram_Body from
      --  something else, so propagate any Acts_As_Spec flag.

      if Nkind (Unit_Node) = N_Subprogram_Body
        and then Acts_As_Spec (Unit_Node)
      then
         Set_Acts_As_Spec (N);
      end if;

      --  Register predefined units in Rtsfind

      declare
         Unum : constant Unit_Number_Type := Get_Source_Unit (Sloc (N));
      begin
         if Is_Predefined_File_Name (Unit_File_Name (Unum)) then
            Set_RTU_Loaded (Unit_Node);
         end if;
      end;

      --  Treat compilation unit pragmas that appear after the library unit

      if Present (Pragmas_After (Aux_Decls_Node (N))) then
         declare
            Prag_Node : Node_Id := First (Pragmas_After (Aux_Decls_Node (N)));
         begin
            while Present (Prag_Node) loop
               Analyze (Prag_Node);
               Next (Prag_Node);
            end loop;
         end;
      end if;

      --  Generate distribution stubs if requested and no error

      if N = Main_Cunit
        and then (Distribution_Stub_Mode = Generate_Receiver_Stub_Body
                    or else
                  Distribution_Stub_Mode = Generate_Caller_Stub_Body)
        and then not Fatal_Error (Main_Unit)
      then
         if Is_RCI_Pkg_Spec_Or_Body (N) then

            --  Regular RCI package

            Add_Stub_Constructs (N);

         elsif (Nkind (Unit_Node) = N_Package_Declaration
                 and then Is_Shared_Passive (Defining_Entity
                                              (Specification (Unit_Node))))
           or else (Nkind (Unit_Node) = N_Package_Body
                     and then
                       Is_Shared_Passive (Corresponding_Spec (Unit_Node)))
         then
            --  Shared passive package

            Add_Stub_Constructs (N);

         elsif Nkind (Unit_Node) = N_Package_Instantiation
           and then
             Is_Remote_Call_Interface
               (Defining_Entity (Specification (Instance_Spec (Unit_Node))))
         then
            --  Instantiation of a RCI generic package

            Add_Stub_Constructs (N);
         end if;

      end if;

      --  Remove unit from visibility, so that environment is clean for
      --  the next compilation, which is either the main unit or some
      --  other unit in the context.

      if Nkind_In (Unit_Node, N_Package_Declaration,
                              N_Package_Renaming_Declaration,
                              N_Subprogram_Declaration)
        or else Nkind (Unit_Node) in N_Generic_Declaration
        or else
          (Nkind (Unit_Node) = N_Subprogram_Body
            and then Acts_As_Spec (Unit_Node))
      then
         Remove_Unit_From_Visibility (Defining_Entity (Unit_Node));

      --  If the unit is an instantiation whose body will be elaborated for
      --  inlining purposes, use the the proper entity of the instance. The
      --  entity may be missing if the instantiation was illegal.

      elsif Nkind (Unit_Node) = N_Package_Instantiation
        and then not Error_Posted (Unit_Node)
        and then Present (Instance_Spec (Unit_Node))
      then
         Remove_Unit_From_Visibility
           (Defining_Entity (Instance_Spec (Unit_Node)));

      elsif Nkind (Unit_Node) = N_Package_Body
        or else (Nkind (Unit_Node) = N_Subprogram_Body
                  and then not Acts_As_Spec (Unit_Node))
      then
         --  Bodies that are not the main unit are compiled if they are generic
         --  or contain generic or inlined units. Their analysis brings in the
         --  context of the corresponding spec (unit declaration) which must be
         --  removed as well, to return the compilation environment to its
         --  proper state.

         Remove_Context (Lib_Unit);
         Set_Is_Immediately_Visible (Defining_Entity (Unit (Lib_Unit)), False);
      end if;

      --  Last step is to deinstall the context we just installed as well as
      --  the unit just compiled.

      Remove_Context (N);

      --  If this is the main unit and we are generating code, we must check
      --  that all generic units in the context have a body if they need it,
      --  even if they have not been instantiated. In the absence of .ali files
      --  for generic units, we must force the load of the body, just to
      --  produce the proper error if the body is absent. We skip this
      --  verification if the main unit itself is generic.

      if Get_Cunit_Unit_Number (N) = Main_Unit
        and then Operating_Mode = Generate_Code
        and then Expander_Active
      then
         --  Check whether the source for the body of the unit must be included
         --  in a standalone library.

         Check_Body_Needed_For_SAL (Cunit_Entity (Main_Unit));

         --  Indicate that the main unit is now analyzed, to catch possible
         --  circularities between it and generic bodies. Remove main unit from
         --  visibility. This might seem superfluous, but the main unit must
         --  not be visible in the generic body expansions that follow.

         Set_Analyzed (N, True);
         Set_Is_Immediately_Visible (Cunit_Entity (Main_Unit), False);

         declare
            Item  : Node_Id;
            Nam   : Entity_Id;
            Un    : Unit_Number_Type;

            Save_Style_Check : constant Boolean := Style_Check;
            Save_C_Restrict  : constant Save_Cunit_Boolean_Restrictions :=
                                 Cunit_Boolean_Restrictions_Save;

         begin
            Item := First (Context_Items (N));
            while Present (Item) loop

               --  Check for explicit with clause

               if Nkind (Item) = N_With_Clause
                 and then not Implicit_With (Item)

                  --  Ada 2005 (AI-50217): Ignore limited-withed units

                 and then not Limited_Present (Item)
               then
                  Nam := Entity (Name (Item));

                  if (Is_Generic_Subprogram (Nam)
                       and then not Is_Intrinsic_Subprogram (Nam))
                    or else (Ekind (Nam) = E_Generic_Package
                              and then Unit_Requires_Body (Nam))
                  then
                     Style_Check := False;

                     if Present (Renamed_Object (Nam)) then
                        Un :=
                           Load_Unit
                             (Load_Name  => Get_Body_Name
                                              (Get_Unit_Name
                                                (Unit_Declaration_Node
                                                  (Renamed_Object (Nam)))),
                              Required   => False,
                              Subunit    => False,
                              Error_Node => N,
                              Renamings  => True);
                     else
                        Un :=
                          Load_Unit
                            (Load_Name  => Get_Body_Name
                                             (Get_Unit_Name (Item)),
                             Required   => False,
                             Subunit    => False,
                             Error_Node => N,
                             Renamings  => True);
                     end if;

                     if Un = No_Unit then
                        Error_Msg_NE
                          ("body of generic unit& not found", Item, Nam);
                        exit;

                     elsif not Analyzed (Cunit (Un))
                       and then Un /= Main_Unit
                       and then not Fatal_Error (Un)
                     then
                        Style_Check := False;
                        Semantics (Cunit (Un));
                     end if;
                  end if;
               end if;

               Next (Item);
            end loop;

            Style_Check := Save_Style_Check;
            Cunit_Boolean_Restrictions_Restore (Save_C_Restrict);
         end;
      end if;

      --  Deal with creating elaboration Boolean if needed. We create an
      --  elaboration boolean only for units that come from source since
      --  units manufactured by the compiler never need elab checks.

      if Comes_From_Source (N)
        and then Nkind_In (Unit_Node, N_Package_Declaration,
                                      N_Generic_Package_Declaration,
                                      N_Subprogram_Declaration,
                                      N_Generic_Subprogram_Declaration)
      then
         declare
            Loc  : constant Source_Ptr       := Sloc (N);
            Unum : constant Unit_Number_Type := Get_Source_Unit (Loc);

         begin
            Spec_Id := Defining_Entity (Unit_Node);
            Generate_Definition (Spec_Id);

            --  See if an elaboration entity is required for possible access
            --  before elaboration checking. Note that we must allow for this
            --  even if -gnatE is not set, since a client may be compiled in
            --  -gnatE mode and reference the entity.

            --  These entities are also used by the binder to prevent multiple
            --  attempts to execute the elaboration code for the library case
            --  where the elaboration routine might otherwise be called more
            --  than once.

            --  Case of units which do not require elaboration checks

            if
               --  Pure units do not need checks

                 Is_Pure (Spec_Id)

               --  Preelaborated units do not need checks

                 or else Is_Preelaborated (Spec_Id)

               --  No checks needed if pagma Elaborate_Body present

                 or else Has_Pragma_Elaborate_Body (Spec_Id)

               --  No checks needed if unit does not require a body

                 or else not Unit_Requires_Body (Spec_Id)

               --  No checks needed for predefined files

                 or else Is_Predefined_File_Name (Unit_File_Name (Unum))

               --  No checks required if no separate spec

                 or else Acts_As_Spec (N)
            then
               --  This is a case where we only need the entity for
               --  checking to prevent multiple elaboration checks.

               Set_Elaboration_Entity_Required (Spec_Id, False);

            --  Case of elaboration entity is required for access before
            --  elaboration checking (so certainly we must build it!)

            else
               Set_Elaboration_Entity_Required (Spec_Id, True);
            end if;

            Build_Elaboration_Entity (N, Spec_Id);
         end;
      end if;

      --  Freeze the compilation unit entity. This for sure is needed because
      --  of some warnings that can be output (see Freeze_Subprogram), but may
      --  in general be required. If freezing actions result, place them in the
      --  compilation unit actions list, and analyze them.

      declare
         Loc : constant Source_Ptr := Sloc (N);
         L   : constant List_Id :=
                 Freeze_Entity (Cunit_Entity (Current_Sem_Unit), Loc);
      begin
         while Is_Non_Empty_List (L) loop
            Insert_Library_Level_Action (Remove_Head (L));
         end loop;
      end;

      Set_Analyzed (N);

      if Nkind (Unit_Node) = N_Package_Declaration
        and then Get_Cunit_Unit_Number (N) /= Main_Unit
        and then Expander_Active
      then
         declare
            Save_Style_Check : constant Boolean := Style_Check;
            Save_Warning     : constant Warning_Mode_Type := Warning_Mode;
            Options          : Style_Check_Options;

         begin
            Save_Style_Check_Options (Options);
            Reset_Style_Check_Options;
            Opt.Warning_Mode := Suppress;
            Check_Body_For_Inlining (N, Defining_Entity (Unit_Node));

            Reset_Style_Check_Options;
            Set_Style_Check_Options (Options);
            Style_Check := Save_Style_Check;
            Warning_Mode := Save_Warning;
         end;
      end if;

      --  If we are generating obsolescent warnings, then here is where we
      --  generate them for the with'ed items. The reason for this special
      --  processing is that the normal mechanism of generating the warnings
      --  for referenced entities does not work for context clause references.
      --  That's because when we first analyze the context, it is too early to
      --  know if the with'ing unit is itself obsolescent (which suppresses
      --  the warnings).

      if not GNAT_Mode and then Warn_On_Obsolescent_Feature then

         --  Push current compilation unit as scope, so that the test for
         --  being within an obsolescent unit will work correctly.

         Push_Scope (Defining_Entity (Unit_Node));

         --  Loop through context items to deal with with clauses

         declare
            Item : Node_Id;
            Nam  : Node_Id;
            Ent  : Entity_Id;

         begin
            Item := First (Context_Items (N));
            while Present (Item) loop
               if Nkind (Item) = N_With_Clause

                  --  Suppress this check in limited-withed units. Further work
                  --  needed here if we decide to incorporate this check on
                  --  limited-withed units.

                 and then not Limited_Present (Item)
               then
                  Nam := Name (Item);
                  Ent := Entity (Nam);

                  if Is_Obsolescent (Ent) then
                     Output_Obsolescent_Entity_Warnings (Nam, Ent);
                  end if;
               end if;

               Next (Item);
            end loop;
         end;

         --  Remove temporary install of current unit as scope

         Pop_Scope;
      end if;
   end Analyze_Compilation_Unit;

   ---------------------
   -- Analyze_Context --
   ---------------------

   procedure Analyze_Context (N : Node_Id) is
      Ukind : constant Node_Kind := Nkind (Unit (N));
      Item  : Node_Id;

   begin
      --  First process all configuration pragmas at the start of the context
      --  items. Strictly these are not part of the context clause, but that
      --  is where the parser puts them. In any case for sure we must analyze
      --  these before analyzing the actual context items, since they can have
      --  an effect on that analysis (e.g. pragma Ada_2005 may allow a unit to
      --  be with'ed as a result of changing categorizations in Ada 2005).

      Item := First (Context_Items (N));
      while Present (Item)
        and then Nkind (Item) = N_Pragma
        and then Chars (Item) in Configuration_Pragma_Names
      loop
         Analyze (Item);
         Next (Item);
      end loop;

      --  Loop through actual context items. This is done in two passes:

      --  a) The first pass analyzes non-limited with-clauses and also any
      --     configuration pragmas (we need to get the latter analyzed right
      --     away, since they can affect processing of subsequent items.

      --  b) The second pass analyzes limited_with clauses (Ada 2005: AI-50217)

      while Present (Item) loop

         --  For with clause, analyze the with clause, and then update
         --  the version, since we are dependent on a unit that we with.

         if Nkind (Item) = N_With_Clause
           and then not Limited_Present (Item)
         then
            --  Skip analyzing with clause if no unit, nothing to do (this
            --  happens for a with that references a non-existant unit)

            if Present (Library_Unit (Item)) then
               Analyze (Item);
            end if;

            if not Implicit_With (Item) then
               Version_Update (N, Library_Unit (Item));
            end if;

         --  Skip pragmas. Configuration pragmas at the start were handled in
         --  the loop above, and remaining pragmas are not processed until we
         --  actually install the context (see Install_Context). We delay the
         --  analysis of these pragmas to make sure that we have installed all
         --  the implicit with's on parent units.

         --  Skip use clauses at this stage, since we don't want to do any
         --  installing of potentially use visible entities until we we
         --  actually install the complete context (in Install_Context).
         --  Otherwise things can get installed in the wrong context.

         else
            null;
         end if;

         Next (Item);
      end loop;

      --  Second pass: examine all limited_with clauses. All other context
      --  items are ignored in this pass.

      Item := First (Context_Items (N));
      while Present (Item) loop
         if Nkind (Item) = N_With_Clause
           and then Limited_Present (Item)
         then
            --  No need to check errors on implicitly generated limited-with
            --  clauses.

            if not Implicit_With (Item) then

               --  Check compilation unit containing the limited-with clause

               if not Nkind_In (Ukind, N_Package_Declaration,
                                       N_Subprogram_Declaration,
                                       N_Package_Renaming_Declaration,
                                       N_Subprogram_Renaming_Declaration)
                 and then Ukind not in N_Generic_Declaration
                 and then Ukind not in N_Generic_Renaming_Declaration
                 and then Ukind not in N_Generic_Instantiation
               then
                  Error_Msg_N ("limited with_clause not allowed here", Item);

               --  Check wrong use of a limited with clause applied to the
               --  compilation unit containing the limited-with clause.

               --      limited with P.Q;
               --      package P.Q is ...

               elsif Unit (Library_Unit (Item)) = Unit (N) then
                  Error_Msg_N ("wrong use of limited-with clause", Item);

               --  Check wrong use of limited-with clause applied to some
               --  immediate ancestor.

               elsif Is_Child_Spec (Unit (N)) then
                  declare
                     Lib_U : constant Entity_Id := Unit (Library_Unit (Item));
                     P     : Node_Id;

                  begin
                     P := Parent_Spec (Unit (N));
                     loop
                        if Unit (P) = Lib_U then
                           Error_Msg_N ("limited with_clause of immediate "
                                        & "ancestor not allowed", Item);
                           exit;
                        end if;

                        exit when not Is_Child_Spec (Unit (P));
                        P := Parent_Spec (Unit (P));
                     end loop;
                  end;
               end if;

               --  Check if the limited-withed unit is already visible through
               --  some context clause of the current compilation unit or some
               --  ancestor of the current compilation unit.

               declare
                  Lim_Unit_Name : constant Node_Id := Name (Item);
                  Comp_Unit     : Node_Id;
                  It            : Node_Id;
                  Unit_Name     : Node_Id;

               begin
                  Comp_Unit := N;
                  loop
                     It := First (Context_Items (Comp_Unit));
                     while Present (It) loop
                        if Item /= It
                          and then Nkind (It) = N_With_Clause
                          and then not Limited_Present (It)
                          and then
                            Nkind_In (Unit (Library_Unit (It)),
                                       N_Package_Declaration,
                                       N_Package_Renaming_Declaration)
                        then
                           if Nkind (Unit (Library_Unit (It))) =
                                                      N_Package_Declaration
                           then
                              Unit_Name := Name (It);
                           else
                              Unit_Name := Name (Unit (Library_Unit (It)));
                           end if;

                           --  Check if the named package (or some ancestor)
                           --  leaves visible the full-view of the unit given
                           --  in the limited-with clause

                           loop
                              if Designate_Same_Unit (Lim_Unit_Name,
                                                      Unit_Name)
                              then
                                 Error_Msg_Sloc := Sloc (It);
                                 Error_Msg_N
                                   ("simultaneous visibility of limited "
                                    & "and unlimited views not allowed",
                                    Item);
                                 Error_Msg_NE
                                   ("\unlimited view visible through "
                                    & "context clause #",
                                    Item, It);
                                 exit;

                              elsif Nkind (Unit_Name) = N_Identifier then
                                 exit;
                              end if;

                              Unit_Name := Prefix (Unit_Name);
                           end loop;
                        end if;

                        Next (It);
                     end loop;

                     exit when not Is_Child_Spec (Unit (Comp_Unit));

                     Comp_Unit := Parent_Spec (Unit (Comp_Unit));
                  end loop;
               end;
            end if;

            --  Skip analyzing with clause if no unit, see above

            if Present (Library_Unit (Item)) then
               Analyze (Item);
            end if;

            --  A limited_with does not impose an elaboration order, but
            --  there is a semantic dependency for recompilation purposes.

            if not Implicit_With (Item) then
               Version_Update (N, Library_Unit (Item));
            end if;

            --  Pragmas and use clauses and with clauses other than limited
            --  with's are ignored in this pass through the context items.

         else
            null;
         end if;

         Next (Item);
      end loop;
   end Analyze_Context;

   -------------------------------
   -- Analyze_Package_Body_Stub --
   -------------------------------

   procedure Analyze_Package_Body_Stub (N : Node_Id) is
      Id   : constant Entity_Id := Defining_Identifier (N);
      Nam  : Entity_Id;

   begin
      --  The package declaration must be in the current declarative part

      Check_Stub_Level (N);
      Nam := Current_Entity_In_Scope (Id);

      if No (Nam) or else not Is_Package_Or_Generic_Package (Nam) then
         Error_Msg_N ("missing specification for package stub", N);

      elsif Has_Completion (Nam)
        and then Present (Corresponding_Body (Unit_Declaration_Node (Nam)))
      then
         Error_Msg_N ("duplicate or redundant stub for package", N);

      else
         --  Indicate that the body of the package exists. If we are doing
         --  only semantic analysis, the stub stands for the body. If we are
         --  generating code, the existence of the body will be confirmed
         --  when we load the proper body.

         Set_Has_Completion (Nam);
         Set_Scope (Defining_Entity (N), Current_Scope);
         Generate_Reference (Nam, Id, 'b');
         Analyze_Proper_Body (N, Nam);
      end if;
   end Analyze_Package_Body_Stub;

   -------------------------
   -- Analyze_Proper_Body --
   -------------------------

   procedure Analyze_Proper_Body (N : Node_Id; Nam : Entity_Id) is
      Subunit_Name      : constant Unit_Name_Type := Get_Unit_Name (N);
      Unum              : Unit_Number_Type;

      procedure Optional_Subunit;
      --  This procedure is called when the main unit is a stub, or when we
      --  are not generating code. In such a case, we analyze the subunit if
      --  present, which is user-friendly and in fact required for ASIS, but
      --  we don't complain if the subunit is missing.

      ----------------------
      -- Optional_Subunit --
      ----------------------

      procedure Optional_Subunit is
         Comp_Unit : Node_Id;

      begin
         --  Try to load subunit, but ignore any errors that occur during
         --  the loading of the subunit, by using the special feature in
         --  Errout to ignore all errors. Note that Fatal_Error will still
         --  be set, so we will be able to check for this case below.

         if not ASIS_Mode then
            Ignore_Errors_Enable := Ignore_Errors_Enable + 1;
         end if;

         Unum :=
           Load_Unit
             (Load_Name  => Subunit_Name,
              Required   => False,
              Subunit    => True,
              Error_Node => N);

         if not ASIS_Mode then
            Ignore_Errors_Enable := Ignore_Errors_Enable - 1;
         end if;

         --  All done if we successfully loaded the subunit

         if Unum /= No_Unit
           and then (not Fatal_Error (Unum) or else Try_Semantics)
         then
            Comp_Unit := Cunit (Unum);

            --  If the file was empty or seriously mangled, the unit
            --  itself may be missing.

            if No (Unit (Comp_Unit)) then
               Error_Msg_N
                 ("subunit does not contain expected proper body", N);

            elsif Nkind (Unit (Comp_Unit)) /= N_Subunit then
               Error_Msg_N
                 ("expected SEPARATE subunit, found child unit",
                  Cunit_Entity (Unum));
            else
               Set_Corresponding_Stub (Unit (Comp_Unit), N);
               Analyze_Subunit (Comp_Unit);
               Set_Library_Unit (N, Comp_Unit);
            end if;

         elsif Unum = No_Unit
           and then Present (Nam)
         then
            if Is_Protected_Type (Nam) then
               Set_Corresponding_Body (Parent (Nam), Defining_Identifier (N));
            else
               Set_Corresponding_Body (
                 Unit_Declaration_Node (Nam), Defining_Identifier (N));
            end if;
         end if;
      end Optional_Subunit;

   --  Start of processing for Analyze_Proper_Body

   begin
      --  If the subunit is already loaded, it means that the main unit
      --  is a subunit, and that the current unit is one of its parents
      --  which was being analyzed to provide the needed context for the
      --  analysis of the subunit. In this case we analyze the subunit and
      --  continue with the parent, without looking a subsequent subunits.

      if Is_Loaded (Subunit_Name) then

         --  If the proper body is already linked to the stub node,
         --  the stub is in a generic unit and just needs analyzing.

         if Present (Library_Unit (N)) then
            Set_Corresponding_Stub (Unit (Library_Unit (N)), N);
            Analyze_Subunit (Library_Unit (N));

         --  Otherwise we must load the subunit and link to it

         else
            --  Load the subunit, this must work, since we originally
            --  loaded the subunit earlier on. So this will not really
            --  load it, just give access to it.

            Unum :=
              Load_Unit
                (Load_Name  => Subunit_Name,
                 Required   => True,
                 Subunit    => False,
                 Error_Node => N);

            --  And analyze the subunit in the parent context (note that we
            --  do not call Semantics, since that would remove the parent
            --  context). Because of this, we have to manually reset the
            --  compiler state to Analyzing since it got destroyed by Load.

            if Unum /= No_Unit then
               Compiler_State := Analyzing;

               --  Check that the proper body is a subunit and not a child
               --  unit. If the unit was previously loaded, the error will
               --  have been emitted when copying the generic node, so we
               --  just return to avoid cascaded errors.

               if Nkind (Unit (Cunit (Unum))) /= N_Subunit then
                  return;
               end if;

               Set_Corresponding_Stub (Unit (Cunit (Unum)), N);
               Analyze_Subunit (Cunit (Unum));
               Set_Library_Unit (N, Cunit (Unum));
            end if;
         end if;

      --  If the main unit is a subunit, then we are just performing semantic
      --  analysis on that subunit, and any other subunits of any parent unit
      --  should be ignored, except that if we are building trees for ASIS
      --  usage we want to annotate the stub properly.

      elsif Nkind (Unit (Cunit (Main_Unit))) = N_Subunit
        and then Subunit_Name /= Unit_Name (Main_Unit)
      then
         if ASIS_Mode then
            Optional_Subunit;
         end if;

         --  But before we return, set the flag for unloaded subunits. This
         --  will suppress junk warnings of variables in the same declarative
         --  part (or a higher level one) that are in danger of looking unused
         --  when in fact there might be a declaration in the subunit that we
         --  do not intend to load.

         Unloaded_Subunits := True;
         return;

      --  If the subunit is not already loaded, and we are generating code,
      --  then this is the case where compilation started from the parent,
      --  and we are generating code for an entire subunit tree. In that
      --  case we definitely need to load the subunit.

      --  In order to continue the analysis with the rest of the parent,
      --  and other subunits, we load the unit without requiring its
      --  presence, and emit a warning if not found, rather than terminating
      --  the compilation abruptly, as for other missing file problems.

      elsif Original_Operating_Mode = Generate_Code then

         --  If the proper body is already linked to the stub node,
         --  the stub is in a generic unit and just needs analyzing.

         --  We update the version. Although we are not technically
         --  semantically dependent on the subunit, given our approach
         --  of macro substitution of subunits, it makes sense to
         --  include it in the version identification.

         if Present (Library_Unit (N)) then
            Set_Corresponding_Stub (Unit (Library_Unit (N)), N);
            Analyze_Subunit (Library_Unit (N));
            Version_Update (Cunit (Main_Unit), Library_Unit (N));

         --  Otherwise we must load the subunit and link to it

         else
            Unum :=
              Load_Unit
                (Load_Name  => Subunit_Name,
                 Required   => False,
                 Subunit    => True,
                 Error_Node => N);

            if Original_Operating_Mode = Generate_Code
              and then Unum = No_Unit
            then
               Error_Msg_Unit_1 := Subunit_Name;
               Error_Msg_File_1 :=
                 Get_File_Name (Subunit_Name, Subunit => True);
               Error_Msg_N
                 ("subunit$$ in file{ not found?", N);
               Subunits_Missing := True;
            end if;

            --  Load_Unit may reset Compiler_State, since it may have been
            --  necessary to parse an additional units, so we make sure
            --  that we reset it to the Analyzing state.

            Compiler_State := Analyzing;

            if Unum /= No_Unit then
               if Debug_Flag_L then
                  Write_Str ("*** Loaded subunit from stub. Analyze");
                  Write_Eol;
               end if;

               declare
                  Comp_Unit : constant Node_Id := Cunit (Unum);

               begin
                  --  Check for child unit instead of subunit

                  if Nkind (Unit (Comp_Unit)) /= N_Subunit then
                     Error_Msg_N
                       ("expected SEPARATE subunit, found child unit",
                        Cunit_Entity (Unum));

                  --  OK, we have a subunit

                  else
                     --  Set corresponding stub (even if errors)

                     Set_Corresponding_Stub (Unit (Comp_Unit), N);

                     --  Analyze the unit if semantics active

                     if not Fatal_Error (Unum) or else Try_Semantics then
                        Analyze_Subunit (Comp_Unit);
                     end if;

                     --  Set the library unit pointer in any case

                     Set_Library_Unit (N, Comp_Unit);

                     --  We update the version. Although we are not technically
                     --  semantically dependent on the subunit, given our
                     --  approach of macro substitution of subunits, it makes
                     --  sense to include it in the version identification.

                     Version_Update (Cunit (Main_Unit), Comp_Unit);
                  end if;
               end;
            end if;
         end if;

         --  The remaining case is when the subunit is not already loaded and
         --  we are not generating code. In this case we are just performing
         --  semantic analysis on the parent, and we are not interested in
         --  the subunit. For subprograms, analyze the stub as a body. For
         --  other entities the stub has already been marked as completed.

      else
         Optional_Subunit;
      end if;

   end Analyze_Proper_Body;

   ----------------------------------
   -- Analyze_Protected_Body_Stub --
   ----------------------------------

   procedure Analyze_Protected_Body_Stub (N : Node_Id) is
      Nam : Entity_Id := Current_Entity_In_Scope (Defining_Identifier (N));

   begin
      Check_Stub_Level (N);

      --  First occurence of name may have been as an incomplete type

      if Present (Nam) and then Ekind (Nam) = E_Incomplete_Type then
         Nam := Full_View (Nam);
      end if;

      if No (Nam)
        or else not Is_Protected_Type (Etype (Nam))
      then
         Error_Msg_N ("missing specification for Protected body", N);
      else
         Set_Scope (Defining_Entity (N), Current_Scope);
         Set_Has_Completion (Etype (Nam));
         Generate_Reference (Nam, Defining_Identifier (N), 'b');
         Analyze_Proper_Body (N, Etype (Nam));
      end if;
   end Analyze_Protected_Body_Stub;

   ----------------------------------
   -- Analyze_Subprogram_Body_Stub --
   ----------------------------------

   --  A subprogram body stub can appear with or without a previous
   --  specification. If there is one, the analysis of the body will
   --  find it and verify conformance.  The formals appearing in the
   --  specification of the stub play no role, except for requiring an
   --  additional conformance check. If there is no previous subprogram
   --  declaration, the stub acts as a spec, and provides the defining
   --  entity for the subprogram.

   procedure Analyze_Subprogram_Body_Stub (N : Node_Id) is
      Decl : Node_Id;

   begin
      Check_Stub_Level (N);

      --  Verify that the identifier for the stub is unique within this
      --  declarative part.

      if Nkind_In (Parent (N), N_Block_Statement,
                               N_Package_Body,
                               N_Subprogram_Body)
      then
         Decl := First (Declarations (Parent (N)));
         while Present (Decl)
           and then Decl /= N
         loop
            if Nkind (Decl) = N_Subprogram_Body_Stub
              and then (Chars (Defining_Unit_Name (Specification (Decl))) =
                        Chars (Defining_Unit_Name (Specification (N))))
            then
               Error_Msg_N ("identifier for stub is not unique", N);
            end if;

            Next (Decl);
         end loop;
      end if;

      --  Treat stub as a body, which checks conformance if there is a previous
      --  declaration, or else introduces entity and its signature.

      Analyze_Subprogram_Body (N);
      Analyze_Proper_Body (N, Empty);
   end Analyze_Subprogram_Body_Stub;

   ---------------------
   -- Analyze_Subunit --
   ---------------------

   --  A subunit is compiled either by itself (for semantic checking)
   --  or as part of compiling the parent (for code generation). In
   --  either case, by the time we actually process the subunit, the
   --  parent has already been installed and analyzed. The node N is
   --  a compilation unit, whose context needs to be treated here,
   --  because we come directly here from the parent without calling
   --  Analyze_Compilation_Unit.

   --  The compilation context includes the explicit context of the
   --  subunit, and the context of the parent, together with the parent
   --  itself. In order to compile the current context, we remove the
   --  one inherited from the parent, in order to have a clean visibility
   --  table. We restore the parent context before analyzing the proper
   --  body itself. On exit, we remove only the explicit context of the
   --  subunit.

   procedure Analyze_Subunit (N : Node_Id) is
      Lib_Unit : constant Node_Id   := Library_Unit (N);
      Par_Unit : constant Entity_Id := Current_Scope;

      Lib_Spec        : Node_Id := Library_Unit (Lib_Unit);
      Num_Scopes      : Int := 0;
      Use_Clauses     : array (1 .. Scope_Stack.Last) of Node_Id;
      Enclosing_Child : Entity_Id := Empty;
      Svg             : constant Suppress_Array := Scope_Suppress;

      procedure Analyze_Subunit_Context;
      --  Capture names in use clauses of the subunit. This must be done
      --  before re-installing parent declarations, because items in the
      --  context must not be hidden by declarations local to the parent.

      procedure Re_Install_Parents (L : Node_Id; Scop : Entity_Id);
      --  Recursive procedure to restore scope of all ancestors of subunit,
      --  from outermost in. If parent is not a subunit, the call to install
      --  context installs context of spec and (if parent is a child unit)
      --  the context of its parents as well. It is confusing that parents
      --  should be treated differently in both cases, but the semantics are
      --  just not identical.

      procedure Re_Install_Use_Clauses;
      --  As part of the removal of the parent scope, the use clauses are
      --  removed, to be reinstalled when the context of the subunit has
      --  been analyzed. Use clauses may also have been affected by the
      --  analysis of the context of the subunit, so they have to be applied
      --  again, to insure that the compilation environment of the rest of
      --  the parent unit is identical.

      procedure Remove_Scope;
      --  Remove current scope from scope stack, and preserve the list
      --  of use clauses in it, to be reinstalled after context is analyzed.

      -----------------------------
      -- Analyze_Subunit_Context --
      -----------------------------

      procedure Analyze_Subunit_Context is
         Item      :  Node_Id;
         Nam       :  Node_Id;
         Unit_Name : Entity_Id;

      begin
         Analyze_Context (N);

         --  Make withed units immediately visible. If child unit, make the
         --  ultimate parent immediately visible.

         Item := First (Context_Items (N));
         while Present (Item) loop
            if Nkind (Item) = N_With_Clause then

               --  Protect frontend against previous errors in context clauses

               if Nkind (Name (Item)) /= N_Selected_Component then
                  if Error_Posted (Item) then
                     null;

                  else
                     Unit_Name := Entity (Name (Item));
                     while Is_Child_Unit (Unit_Name) loop
                        Set_Is_Visible_Child_Unit (Unit_Name);
                        Unit_Name := Scope (Unit_Name);
                     end loop;

                     if not Is_Immediately_Visible (Unit_Name) then
                        Set_Is_Immediately_Visible (Unit_Name);
                        Set_Context_Installed (Item);
                     end if;
                  end if;
               end if;

            elsif Nkind (Item) = N_Use_Package_Clause then
               Nam := First (Names (Item));
               while Present (Nam) loop
                  Analyze (Nam);
                  Next (Nam);
               end loop;

            elsif Nkind (Item) = N_Use_Type_Clause then
               Nam := First (Subtype_Marks (Item));
               while Present (Nam) loop
                  Analyze (Nam);
                  Next (Nam);
               end loop;
            end if;

            Next (Item);
         end loop;

         --  Reset visibility of withed units. They will be made visible
         --  again when we install the subunit context.

         Item := First (Context_Items (N));
         while Present (Item) loop
            if Nkind (Item) = N_With_Clause

               --  Protect frontend against previous errors in context clauses

              and then Nkind (Name (Item)) /= N_Selected_Component
              and then not Error_Posted (Item)
            then
               Unit_Name := Entity (Name (Item));
               while Is_Child_Unit (Unit_Name) loop
                  Set_Is_Visible_Child_Unit (Unit_Name, False);
                  Unit_Name := Scope (Unit_Name);
               end loop;

               if Context_Installed (Item) then
                  Set_Is_Immediately_Visible (Unit_Name, False);
                  Set_Context_Installed (Item, False);
               end if;
            end if;

            Next (Item);
         end loop;
      end Analyze_Subunit_Context;

      ------------------------
      -- Re_Install_Parents --
      ------------------------

      procedure Re_Install_Parents (L : Node_Id; Scop : Entity_Id) is
         E : Entity_Id;

      begin
         if Nkind (Unit (L)) = N_Subunit then
            Re_Install_Parents (Library_Unit (L), Scope (Scop));
         end if;

         Install_Context (L);

         --  If the subunit occurs within a child unit, we must restore the
         --  immediate visibility of any siblings that may occur in context.

         if Present (Enclosing_Child) then
            Install_Siblings (Enclosing_Child, L);
         end if;

         Push_Scope (Scop);

         if Scop /= Par_Unit then
            Set_Is_Immediately_Visible (Scop);
         end if;

         --  Make entities in scope visible again. For child units, restore
         --  visibility only if they are actually in context.

         E := First_Entity (Current_Scope);
         while Present (E) loop
            if not Is_Child_Unit (E)
              or else Is_Visible_Child_Unit (E)
            then
               Set_Is_Immediately_Visible (E);
            end if;

            Next_Entity (E);
         end loop;

         --  A subunit appears within a body, and for a nested subunits
         --  all the parents are bodies. Restore full visibility of their
         --  private entities.

         if Ekind (Scop) = E_Package
           or else Ekind (Scop) = E_Generic_Package
         then
            Set_In_Package_Body (Scop);
            Install_Private_Declarations (Scop);
         end if;
      end Re_Install_Parents;

      ----------------------------
      -- Re_Install_Use_Clauses --
      ----------------------------

      procedure Re_Install_Use_Clauses is
         U  : Node_Id;
      begin
         for J in reverse 1 .. Num_Scopes loop
            U := Use_Clauses (J);
            Scope_Stack.Table (Scope_Stack.Last - J + 1).First_Use_Clause := U;
            Install_Use_Clauses (U, Force_Installation => True);
         end loop;
      end Re_Install_Use_Clauses;

      ------------------
      -- Remove_Scope --
      ------------------

      procedure Remove_Scope is
         E : Entity_Id;

      begin
         Num_Scopes := Num_Scopes + 1;
         Use_Clauses (Num_Scopes) :=
           Scope_Stack.Table (Scope_Stack.Last).First_Use_Clause;

         E := First_Entity (Current_Scope);
         while Present (E) loop
            Set_Is_Immediately_Visible (E, False);
            Next_Entity (E);
         end loop;

         if Is_Child_Unit (Current_Scope) then
            Enclosing_Child := Current_Scope;
         end if;

         Pop_Scope;
      end Remove_Scope;

   --  Start of processing for Analyze_Subunit

   begin
      if not Is_Empty_List (Context_Items (N)) then

         --  Save current use clauses

         Remove_Scope;
         Remove_Context (Lib_Unit);

         --  Now remove parents and their context, including enclosing
         --  subunits and the outer parent body which is not a subunit.

         if Present (Lib_Spec) then
            Remove_Context (Lib_Spec);

            while Nkind (Unit (Lib_Spec)) = N_Subunit loop
               Lib_Spec := Library_Unit (Lib_Spec);
               Remove_Scope;
               Remove_Context (Lib_Spec);
            end loop;

            if Nkind (Unit (Lib_Unit)) = N_Subunit then
               Remove_Scope;
            end if;

            if Nkind (Unit (Lib_Spec)) = N_Package_Body then
               Remove_Context (Library_Unit (Lib_Spec));
            end if;
         end if;

         Set_Is_Immediately_Visible (Par_Unit, False);

         Analyze_Subunit_Context;

         Re_Install_Parents (Lib_Unit, Par_Unit);
         Set_Is_Immediately_Visible (Par_Unit);

         --  If the context includes a child unit of the parent of the
         --  subunit, the parent will have been removed from visibility,
         --  after compiling that cousin in the context. The visibility
         --  of the parent must be restored now. This also applies if the
         --  context includes another subunit of the same parent which in
         --  turn includes a child unit in its context.

         if Ekind (Par_Unit) = E_Package
           or else Ekind (Par_Unit) = E_Generic_Package
         then
            if not Is_Immediately_Visible (Par_Unit)
              or else (Present (First_Entity (Par_Unit))
                        and then not Is_Immediately_Visible
                                      (First_Entity (Par_Unit)))
            then
               Set_Is_Immediately_Visible   (Par_Unit);
               Install_Visible_Declarations (Par_Unit);
               Install_Private_Declarations (Par_Unit);
            end if;
         end if;

         Re_Install_Use_Clauses;
         Install_Context (N);

         --  Restore state of suppress flags for current body

         Scope_Suppress := Svg;

         --  If the subunit is within a child unit, then siblings of any
         --  parent unit that appear in the context clause of the subunit
         --  must also be made immediately visible.

         if Present (Enclosing_Child) then
            Install_Siblings (Enclosing_Child, N);
         end if;

      end if;

      Analyze (Proper_Body (Unit (N)));
      Remove_Context (N);

      --  The subunit may contain a with_clause on a sibling of some
      --  ancestor. Removing the context will remove from visibility those
      --  ancestor child units, which must be restored to the visibility
      --  they have in the enclosing body.

      if Present (Enclosing_Child) then
         declare
            C : Entity_Id;
         begin
            C := Current_Scope;
            while Present (C)
              and then Is_Child_Unit (C)
            loop
               Set_Is_Immediately_Visible (C);
               Set_Is_Visible_Child_Unit (C);
               C := Scope (C);
            end loop;
         end;
      end if;
   end Analyze_Subunit;

   ----------------------------
   -- Analyze_Task_Body_Stub --
   ----------------------------

   procedure Analyze_Task_Body_Stub (N : Node_Id) is
      Nam : Entity_Id := Current_Entity_In_Scope (Defining_Identifier (N));
      Loc : constant Source_Ptr := Sloc (N);

   begin
      Check_Stub_Level (N);

      --  First occurence of name may have been as an incomplete type

      if Present (Nam) and then Ekind (Nam) = E_Incomplete_Type then
         Nam := Full_View (Nam);
      end if;

      if No (Nam)
        or else not Is_Task_Type (Etype (Nam))
      then
         Error_Msg_N ("missing specification for task body", N);
      else
         Set_Scope (Defining_Entity (N), Current_Scope);
         Generate_Reference (Nam, Defining_Identifier (N), 'b');
         Set_Has_Completion (Etype (Nam));
         Analyze_Proper_Body (N, Etype (Nam));

         --  Set elaboration flag to indicate that entity is callable.
         --  This cannot be done in the expansion of the body  itself,
         --  because the proper body is not in a declarative part. This
         --  is only done if expansion is active, because the context
         --  may be generic and the flag not defined yet.

         if Expander_Active then
            Insert_After (N,
              Make_Assignment_Statement (Loc,
                Name =>
                  Make_Identifier (Loc,
                    New_External_Name (Chars (Etype (Nam)), 'E')),
                 Expression => New_Reference_To (Standard_True, Loc)));
         end if;

      end if;
   end Analyze_Task_Body_Stub;

   -------------------------
   -- Analyze_With_Clause --
   -------------------------

   --  Analyze the declaration of a unit in a with clause. At end,
   --  label the with clause with the defining entity for the unit.

   procedure Analyze_With_Clause (N : Node_Id) is

      --  Retrieve the original kind of the unit node, before analysis.
      --  If it is a subprogram instantiation, its analysis below will
      --  rewrite as the declaration of the wrapper package. If the same
      --  instantiation appears indirectly elsewhere in the context, it
      --  will have been analyzed already.

      Unit_Kind : constant Node_Kind :=
                    Nkind (Original_Node (Unit (Library_Unit (N))));
      Nam       : constant Node_Id := Name (N);
      E_Name    : Entity_Id;
      Par_Name  : Entity_Id;
      Pref      : Node_Id;
      U         : Node_Id;

      Intunit : Boolean;
      --  Set True if the unit currently being compiled is an internal unit

      Save_Style_Check : constant Boolean := Opt.Style_Check;
      Save_C_Restrict  : constant Save_Cunit_Boolean_Restrictions :=
                           Cunit_Boolean_Restrictions_Save;

   begin
      if Limited_Present (N) then

         --  Ada 2005 (AI-50217): Build visibility structures but do not
         --  analyze the unit.

         Build_Limited_Views (N);
         return;
      end if;

      --  We reset ordinary style checking during the analysis of a with'ed
      --  unit, but we do NOT reset GNAT special analysis mode (the latter
      --  definitely *does* apply to with'ed units).

      if not GNAT_Mode then
         Style_Check := False;
      end if;

      --  If the library unit is a predefined unit, and we are in high
      --  integrity mode, then temporarily reset Configurable_Run_Time_Mode
      --  for the analysis of the with'ed unit. This mode does not prevent
      --  explicit with'ing of run-time units.

      if Configurable_Run_Time_Mode
        and then
          Is_Predefined_File_Name
            (Unit_File_Name (Get_Source_Unit (Unit (Library_Unit (N)))))
      then
         Configurable_Run_Time_Mode := False;
         Semantics (Library_Unit (N));
         Configurable_Run_Time_Mode := True;

      else
         Semantics (Library_Unit (N));
      end if;

      U := Unit (Library_Unit (N));
      Intunit := Is_Internal_File_Name (Unit_File_Name (Current_Sem_Unit));

      --  Following checks are skipped for dummy packages (those supplied for
      --  with's where no matching file could be found). Such packages are
      --  identified by the Sloc value being set to No_Location

      if Sloc (U) /= No_Location then

         --  Check restrictions, except that we skip the check if this is an
         --  internal unit unless we are compiling the internal unit as the
         --  main unit. We also skip this for dummy packages.

         Check_Restriction_No_Dependence (Nam, N);

         if not Intunit or else Current_Sem_Unit = Main_Unit then
            Check_Restricted_Unit (Unit_Name (Get_Source_Unit (U)), N);
         end if;

         --  Deal with special case of GNAT.Current_Exceptions which interacts
         --  with the optimization of local raise statements into gotos.

         if Nkind (Nam) = N_Selected_Component
           and then Nkind (Prefix (Nam)) = N_Identifier
           and then Chars (Prefix (Nam)) = Name_Gnat
           and then (Chars (Selector_Name (Nam)) = Name_Most_Recent_Exception
                       or else
                     Chars (Selector_Name (Nam)) = Name_Exception_Traces)
         then
            Check_Restriction (No_Exception_Propagation, N);
            Special_Exception_Package_Used := True;
         end if;

         --  Check for inappropriate with of internal implementation unit if we
         --  are currently compiling the main unit and the main unit is itself
         --  not an internal unit. We do not issue this message for implicit
         --  with's generated by the compiler itself.

         if Implementation_Unit_Warnings
           and then Current_Sem_Unit = Main_Unit
           and then not Intunit
           and then not Implicit_With (N)
           and then not GNAT_Mode
         then
            declare
               U_Kind : constant Kind_Of_Unit :=
                          Get_Kind_Of_Unit (Get_Source_Unit (U));

            begin
               if U_Kind = Implementation_Unit then
                  Error_Msg_F ("& is an internal 'G'N'A'T unit?", Name (N));
                  Error_Msg_F
                    ("\use of this unit is non-portable " &
                     "and version-dependent?",
                     Name (N));

               elsif U_Kind = Ada_05_Unit
                 and then Ada_Version < Ada_05
                 and then Warn_On_Ada_2005_Compatibility
               then
                  Error_Msg_N ("& is an Ada 2005 unit?", Name (N));
               end if;
            end;
         end if;
      end if;

      --  Semantic analysis of a generic unit is performed on a copy of
      --  the original tree. Retrieve the entity on  which semantic info
      --  actually appears.

      if Unit_Kind in N_Generic_Declaration then
         E_Name := Defining_Entity (U);

      --  Note: in the following test, Unit_Kind is the original Nkind, but in
      --  the case of an instantiation, semantic analysis above will have
      --  replaced the unit by its instantiated version. If the instance body
      --  has been generated, the instance now denotes the body entity. For
      --  visibility purposes we need the entity of its spec.

      elsif (Unit_Kind = N_Package_Instantiation
              or else Nkind (Original_Node (Unit (Library_Unit (N)))) =
                                                  N_Package_Instantiation)
        and then Nkind (U) = N_Package_Body
      then
         E_Name := Corresponding_Spec (U);

      elsif Unit_Kind = N_Package_Instantiation
        and then Nkind (U) = N_Package_Instantiation
      then
         --  If the instance has not been rewritten as a package declaration,
         --  then it appeared already in a previous with clause. Retrieve
         --  the entity from the previous instance.

         E_Name := Defining_Entity (Specification (Instance_Spec (U)));

      elsif Unit_Kind in N_Subprogram_Instantiation then

         --  Instantiation node is replaced with a wrapper package. Retrieve
         --  the visible subprogram created by the instance from corresponding
         --  attribute of the wrapper.

         E_Name := Related_Instance (Defining_Entity (U));

      elsif Unit_Kind = N_Package_Renaming_Declaration
        or else Unit_Kind in N_Generic_Renaming_Declaration
      then
         E_Name := Defining_Entity (U);

      elsif Unit_Kind = N_Subprogram_Body
        and then Nkind (Name (N)) = N_Selected_Component
        and then not Acts_As_Spec (Library_Unit (N))
      then
         --  For a child unit that has no spec, one has been created and
         --  analyzed. The entity required is that of the spec.

         E_Name := Corresponding_Spec (U);

      else
         E_Name := Defining_Entity (U);
      end if;

      if Nkind (Name (N)) = N_Selected_Component then

         --  Child unit in a with clause

         Change_Selected_Component_To_Expanded_Name (Name (N));
      end if;

      --  Restore style checks and restrictions

      Style_Check := Save_Style_Check;
      Cunit_Boolean_Restrictions_Restore (Save_C_Restrict);

      --  Record the reference, but do NOT set the unit as referenced, we want
      --  to consider the unit as unreferenced if this is the only reference
      --  that occurs.

      Set_Entity_With_Style_Check (Name (N), E_Name);
      Generate_Reference (E_Name, Name (N), 'w', Set_Ref => False);

      if Is_Child_Unit (E_Name) then
         Pref     := Prefix (Name (N));
         Par_Name := Scope (E_Name);
         while Nkind (Pref) = N_Selected_Component loop
            Change_Selected_Component_To_Expanded_Name (Pref);
            Set_Entity_With_Style_Check (Pref, Par_Name);

            Generate_Reference (Par_Name, Pref);
            Pref := Prefix (Pref);

            --  If E_Name is the dummy entity for a nonexistent unit, its scope
            --  is set to Standard_Standard, and no attempt should be made to
            --  further unwind scopes.

            if Par_Name /= Standard_Standard then
               Par_Name := Scope (Par_Name);
            end if;
         end loop;

         if Present (Entity (Pref))
           and then not Analyzed (Parent (Parent (Entity (Pref))))
         then
            --  If the entity is set without its unit being compiled, the
            --  original parent is a renaming, and Par_Name is the renamed
            --  entity. For visibility purposes, we need the original entity,
            --  which must be analyzed now because Load_Unit directly retrieves
            --  the renamed unit, and the renaming declaration itself has not
            --  been analyzed.

            Analyze (Parent (Parent (Entity (Pref))));
            pragma Assert (Renamed_Object (Entity (Pref)) = Par_Name);
            Par_Name := Entity (Pref);
         end if;

         Set_Entity_With_Style_Check (Pref, Par_Name);
         Generate_Reference (Par_Name, Pref);
      end if;

      --  If the withed unit is System, and a system extension pragma is
      --  present, compile the extension now, rather than waiting for a
      --  visibility check on a specific entity.

      if Chars (E_Name) = Name_System
        and then Scope (E_Name) = Standard_Standard
        and then Present (System_Extend_Unit)
        and then Present_System_Aux (N)
      then
         --  If the extension is not present, an error will have been emitted

         null;
      end if;

      --  Ada 2005 (AI-262): Remove from visibility the entity corresponding
      --  to private_with units; they will be made visible later (just before
      --  the private part is analyzed)

      if Private_Present (N) then
         Set_Is_Immediately_Visible (E_Name, False);
      end if;
   end Analyze_With_Clause;

   ------------------------------
   -- Check_Private_Child_Unit --
   ------------------------------

   procedure Check_Private_Child_Unit (N : Node_Id) is
      Lib_Unit   : constant Node_Id := Unit (N);
      Item       : Node_Id;
      Curr_Unit  : Entity_Id;
      Sub_Parent : Node_Id;
      Priv_Child : Entity_Id;
      Par_Lib    : Entity_Id;
      Par_Spec   : Node_Id;

      function Is_Private_Library_Unit (Unit : Entity_Id) return Boolean;
      --  Returns true if and only if the library unit is declared with
      --  an explicit designation of private.

      function Is_Private_Library_Unit (Unit : Entity_Id) return Boolean is
         Comp_Unit : constant Node_Id := Parent (Unit_Declaration_Node (Unit));

      begin
         return Private_Present (Comp_Unit);
      end Is_Private_Library_Unit;

   --  Start of processing for Check_Private_Child_Unit

   begin
      if Nkind_In (Lib_Unit, N_Package_Body, N_Subprogram_Body) then
         Curr_Unit := Defining_Entity (Unit (Library_Unit (N)));
         Par_Lib   := Curr_Unit;

      elsif Nkind (Lib_Unit) = N_Subunit then

         --  The parent is itself a body. The parent entity is to be found in
         --  the corresponding spec.

         Sub_Parent := Library_Unit (N);
         Curr_Unit  := Defining_Entity (Unit (Library_Unit (Sub_Parent)));

         --  If the parent itself is a subunit, Curr_Unit is the entity
         --  of the enclosing body, retrieve the spec entity which is
         --  the proper ancestor we need for the following tests.

         if Ekind (Curr_Unit) = E_Package_Body then
            Curr_Unit := Spec_Entity (Curr_Unit);
         end if;

         Par_Lib    := Curr_Unit;

      else
         Curr_Unit := Defining_Entity (Lib_Unit);

         Par_Lib := Curr_Unit;
         Par_Spec  := Parent_Spec (Lib_Unit);

         if No (Par_Spec) then
            Par_Lib := Empty;
         else
            Par_Lib := Defining_Entity (Unit (Par_Spec));
         end if;
      end if;

      --  Loop through context items

      Item := First (Context_Items (N));
      while Present (Item) loop

         --  Ada 2005 (AI-262): Allow private_with of a private child package
         --  in public siblings

         if Nkind (Item) = N_With_Clause
            and then not Implicit_With (Item)
            and then Is_Private_Descendant (Entity (Name (Item)))
         then
            Priv_Child := Entity (Name (Item));

            declare
               Curr_Parent  : Entity_Id := Par_Lib;
               Child_Parent : Entity_Id := Scope (Priv_Child);
               Prv_Ancestor : Entity_Id := Child_Parent;
               Curr_Private : Boolean   := Is_Private_Library_Unit (Curr_Unit);

            begin
               --  If the child unit is a public child then locate the nearest
               --  private ancestor. Child_Parent will then be set to the
               --  parent of that ancestor.

               if not Is_Private_Library_Unit (Priv_Child) then
                  while Present (Prv_Ancestor)
                    and then not Is_Private_Library_Unit (Prv_Ancestor)
                  loop
                     Prv_Ancestor := Scope (Prv_Ancestor);
                  end loop;

                  if Present (Prv_Ancestor) then
                     Child_Parent := Scope (Prv_Ancestor);
                  end if;
               end if;

               while Present (Curr_Parent)
                 and then Curr_Parent /= Standard_Standard
                 and then Curr_Parent /= Child_Parent
               loop
                  Curr_Private :=
                    Curr_Private or else Is_Private_Library_Unit (Curr_Parent);
                  Curr_Parent := Scope (Curr_Parent);
               end loop;

               if No (Curr_Parent) then
                  Curr_Parent := Standard_Standard;
               end if;

               if Curr_Parent /= Child_Parent then
                  if Ekind (Priv_Child) = E_Generic_Package
                    and then Chars (Priv_Child) in Text_IO_Package_Name
                    and then Chars (Scope (Scope (Priv_Child))) = Name_Ada
                  then
                     Error_Msg_NE
                       ("& is a nested package, not a compilation unit",
                       Name (Item), Priv_Child);

                  else
                     Error_Msg_N
                       ("unit in with clause is private child unit!", Item);
                     Error_Msg_NE
                       ("\current unit must also have parent&!",
                        Item, Child_Parent);
                  end if;

               elsif Curr_Private
                 or else Private_Present (Item)
                 or else Nkind_In (Lib_Unit, N_Package_Body, N_Subunit)
                 or else (Nkind (Lib_Unit) = N_Subprogram_Body
                            and then not Acts_As_Spec (Parent (Lib_Unit)))
               then
                  null;

               else
                  Error_Msg_NE
                    ("current unit must also be private descendant of&",
                     Item, Child_Parent);
               end if;
            end;
         end if;

         Next (Item);
      end loop;

   end Check_Private_Child_Unit;

   ----------------------
   -- Check_Stub_Level --
   ----------------------

   procedure Check_Stub_Level (N : Node_Id) is
      Par  : constant Node_Id   := Parent (N);
      Kind : constant Node_Kind := Nkind (Par);

   begin
      if Nkind_In (Kind, N_Package_Body,
                         N_Subprogram_Body,
                         N_Task_Body,
                         N_Protected_Body)
        and then Nkind_In (Parent (Par), N_Compilation_Unit, N_Subunit)
      then
         null;

      --  In an instance, a missing stub appears at any level. A warning
      --  message will have been emitted already for the missing file.

      elsif not In_Instance then
         Error_Msg_N ("stub cannot appear in an inner scope", N);

      elsif Expander_Active then
         Error_Msg_N ("missing proper body", N);
      end if;
   end Check_Stub_Level;

   ------------------------
   -- Expand_With_Clause --
   ------------------------

   procedure Expand_With_Clause (Item : Node_Id; Nam : Node_Id; N : Node_Id) is
      Loc   : constant Source_Ptr := Sloc (Nam);
      Ent   : constant Entity_Id := Entity (Nam);
      Withn : Node_Id;
      P     : Node_Id;

      function Build_Unit_Name (Nam : Node_Id) return Node_Id;
      --  Comment requireed here ???

      ---------------------
      -- Build_Unit_Name --
      ---------------------

      function Build_Unit_Name (Nam : Node_Id) return Node_Id is
         Renaming : Entity_Id;
         Result   : Node_Id;

      begin
         if Nkind (Nam) = N_Identifier then

            --  If the parent unit P in the name of the with_clause for P.Q
            --  is a renaming of package R, then the entity of the parent is
            --  set to R, but the identifier retains Chars (P) to be consistent
            --  with the source (see details in lib-load). However, the
            --  implicit_with_clause for the parent must make the entity for
            --  P visible, because P.Q may be used as a prefix within the
            --  current unit. The entity for P is the current_entity with that
            --  name, because the package renaming declaration for it has just
            --  been analyzed. Note that this case can only happen if P.Q has
            --  already appeared in a previous with_clause in a related unit,
            --  such as the library body of the current unit.

            if Chars (Nam) /= Chars (Entity (Nam)) then
               Renaming := Current_Entity (Nam);
               pragma Assert (Renamed_Entity (Renaming) = Entity (Nam));
               return New_Occurrence_Of (Renaming, Loc);

            else
               return New_Occurrence_Of (Entity (Nam), Loc);
            end if;

         else
            Result :=
              Make_Expanded_Name (Loc,
                Chars  => Chars (Entity (Nam)),
                Prefix => Build_Unit_Name (Prefix (Nam)),
                Selector_Name => New_Occurrence_Of (Entity (Nam), Loc));
            Set_Entity (Result, Entity (Nam));
            return Result;
         end if;
      end Build_Unit_Name;

   --  Start of processing for Expand_With_Clause

   begin
      New_Nodes_OK := New_Nodes_OK + 1;
      Withn :=
        Make_With_Clause (Loc, Name => Build_Unit_Name (Nam));

      P := Parent (Unit_Declaration_Node (Ent));
      Set_Library_Unit          (Withn, P);
      Set_Corresponding_Spec    (Withn, Ent);
      Set_First_Name            (Withn, True);
      Set_Implicit_With         (Withn, True);

      --  If the unit is a package declaration, a private_with_clause on a
      --  child unit implies that the implicit with on the parent is also
      --  private.

      if Nkind (Unit (N)) = N_Package_Declaration then
         Set_Private_Present    (Withn, Private_Present (Item));
      end if;

      Prepend (Withn, Context_Items (N));
      Mark_Rewrite_Insertion (Withn);
      Install_Withed_Unit (Withn);

      if Nkind (Nam) = N_Expanded_Name then
         Expand_With_Clause (Item, Prefix (Nam), N);
      end if;

      New_Nodes_OK := New_Nodes_OK - 1;
   end Expand_With_Clause;

   -----------------------
   -- Get_Parent_Entity --
   -----------------------

   function Get_Parent_Entity (Unit : Node_Id) return Entity_Id is
   begin
      if Nkind (Unit) = N_Package_Body
        and then Nkind (Original_Node (Unit)) = N_Package_Instantiation
      then
         return
           Defining_Entity
             (Specification (Instance_Spec (Original_Node (Unit))));

      elsif Nkind (Unit) = N_Package_Instantiation then
         return Defining_Entity (Specification (Instance_Spec (Unit)));

      else
         return Defining_Entity (Unit);
      end if;
   end Get_Parent_Entity;

   -----------------------------
   -- Implicit_With_On_Parent --
   -----------------------------

   procedure Implicit_With_On_Parent
     (Child_Unit : Node_Id;
      N          : Node_Id)
   is
      Loc    : constant Source_Ptr := Sloc (N);
      P      : constant Node_Id    := Parent_Spec (Child_Unit);
      P_Unit : Node_Id             := Unit (P);
      P_Name : constant Entity_Id  := Get_Parent_Entity (P_Unit);
      Withn  : Node_Id;

      function Build_Ancestor_Name (P : Node_Id) return Node_Id;
      --  Build prefix of child unit name. Recurse if needed

      function Build_Unit_Name return Node_Id;
      --  If the unit is a child unit, build qualified name with all ancestors

      -------------------------
      -- Build_Ancestor_Name --
      -------------------------

      function Build_Ancestor_Name (P : Node_Id) return Node_Id is
         P_Ref  : constant Node_Id :=
                   New_Reference_To (Defining_Entity (P), Loc);
         P_Spec : Node_Id := P;

      begin
         --  Ancestor may have been rewritten as a package body. Retrieve
         --  the original spec to trace earlier ancestors.

         if Nkind (P) = N_Package_Body
           and then Nkind (Original_Node (P)) = N_Package_Instantiation
         then
            P_Spec := Original_Node (P);
         end if;

         if No (Parent_Spec (P_Spec)) then
            return P_Ref;
         else
            return
              Make_Selected_Component (Loc,
                Prefix => Build_Ancestor_Name (Unit (Parent_Spec (P_Spec))),
                Selector_Name => P_Ref);
         end if;
      end Build_Ancestor_Name;

      ---------------------
      -- Build_Unit_Name --
      ---------------------

      function Build_Unit_Name return Node_Id is
         Result : Node_Id;
      begin
         if No (Parent_Spec (P_Unit)) then
            return New_Reference_To (P_Name, Loc);
         else
            Result :=
              Make_Expanded_Name (Loc,
                Chars  => Chars (P_Name),
                Prefix => Build_Ancestor_Name (Unit (Parent_Spec (P_Unit))),
                Selector_Name => New_Reference_To (P_Name, Loc));
            Set_Entity (Result, P_Name);
            return Result;
         end if;
      end Build_Unit_Name;

   --  Start of processing for Implicit_With_On_Parent

   begin
      --  The unit of the current compilation may be a package body that
      --  replaces an instance node. In this case we need the original instance
      --  node to construct the proper parent name.

      if Nkind (P_Unit) = N_Package_Body
        and then Nkind (Original_Node (P_Unit)) = N_Package_Instantiation
      then
         P_Unit := Original_Node (P_Unit);
      end if;

      --  We add the implicit with if the child unit is the current unit being
      --  compiled. If the current unit is a body, we do not want to add an
      --  implicit_with a second time to the corresponding spec.

      if Nkind (Child_Unit) = N_Package_Declaration
        and then Child_Unit /= Unit (Cunit (Current_Sem_Unit))
      then
         return;
      end if;

      New_Nodes_OK := New_Nodes_OK + 1;
      Withn := Make_With_Clause (Loc, Name => Build_Unit_Name);

      Set_Library_Unit          (Withn, P);
      Set_Corresponding_Spec    (Withn, P_Name);
      Set_First_Name            (Withn, True);
      Set_Implicit_With         (Withn, True);

      --  Node is placed at the beginning of the context items, so that
      --  subsequent use clauses on the parent can be validated.

      Prepend (Withn, Context_Items (N));
      Mark_Rewrite_Insertion (Withn);
      Install_Withed_Unit (Withn);

      if Is_Child_Spec (P_Unit) then
         Implicit_With_On_Parent (P_Unit, N);
      end if;

      New_Nodes_OK := New_Nodes_OK - 1;
   end Implicit_With_On_Parent;

   --------------
   -- In_Chain --
   --------------

   function In_Chain (E : Entity_Id) return Boolean is
      H : Entity_Id;

   begin
      H := Current_Entity (E);
      while Present (H) loop
         if H = E then
            return True;
         else
            H := Homonym (H);
         end if;
      end loop;

      return False;
   end In_Chain;

   ---------------------
   -- Install_Context --
   ---------------------

   procedure Install_Context (N : Node_Id) is
      Lib_Unit : constant Node_Id := Unit (N);

   begin
      Install_Context_Clauses (N);

      if Is_Child_Spec (Lib_Unit) then
         Install_Parents (Lib_Unit, Private_Present (Parent (Lib_Unit)));
      end if;

      Install_Limited_Context_Clauses (N);

   end Install_Context;

   -----------------------------
   -- Install_Context_Clauses --
   -----------------------------

   procedure Install_Context_Clauses (N : Node_Id) is
      Lib_Unit      : constant Node_Id := Unit (N);
      Item          : Node_Id;
      Uname_Node    : Entity_Id;
      Check_Private : Boolean := False;
      Decl_Node     : Node_Id;
      Lib_Parent    : Entity_Id;

   begin
      --  First skip configuration pragmas at the start of the context. They
      --  are not technically part of the context clause, but that's where the
      --  parser puts them. Note they were analyzed in Analyze_Context.

      Item := First (Context_Items (N));
      while Present (Item)
        and then Nkind (Item) = N_Pragma
        and then Chars (Item) in Configuration_Pragma_Names
      loop
         Next (Item);
      end loop;

      --  Loop through the actual context clause items. We process everything
      --  except Limited_With clauses in this routine. Limited_With clauses
      --  are separately installed (see Install_Limited_Context_Clauses).

      while Present (Item) loop

         --  Case of explicit WITH clause

         if Nkind (Item) = N_With_Clause
           and then not Implicit_With (Item)
         then
            if Limited_Present (Item) then

               --  Limited withed units will be installed later

               goto Continue;

            --  If Name (Item) is not an entity name, something is wrong, and
            --  this will be detected in due course, for now ignore the item

            elsif not Is_Entity_Name (Name (Item)) then
               goto Continue;

            elsif No (Entity (Name (Item))) then
               Set_Entity (Name (Item), Any_Id);
               goto Continue;
            end if;

            Uname_Node := Entity (Name (Item));

            if Is_Private_Descendant (Uname_Node) then
               Check_Private := True;
            end if;

            Install_Withed_Unit (Item);

            Decl_Node := Unit_Declaration_Node (Uname_Node);

            --  If the unit is a subprogram instance, it appears nested within
            --  a package that carries the parent information.

            if Is_Generic_Instance (Uname_Node)
              and then Ekind (Uname_Node) /= E_Package
            then
               Decl_Node := Parent (Parent (Decl_Node));
            end if;

            if Is_Child_Spec (Decl_Node) then
               if Nkind (Name (Item)) = N_Expanded_Name then
                  Expand_With_Clause (Item, Prefix (Name (Item)), N);
               else
                  --  If not an expanded name, the child unit must be a
                  --  renaming, nothing to do.

                  null;
               end if;

            elsif Nkind (Decl_Node) = N_Subprogram_Body
              and then not Acts_As_Spec (Parent (Decl_Node))
              and then Is_Child_Spec (Unit (Library_Unit (Parent (Decl_Node))))
            then
               Implicit_With_On_Parent
                 (Unit (Library_Unit (Parent (Decl_Node))), N);
            end if;

            --  Check license conditions unless this is a dummy unit

            if Sloc (Library_Unit (Item)) /= No_Location then
               License_Check : declare

                  Withu : constant Unit_Number_Type :=
                            Get_Source_Unit (Library_Unit (Item));

                  Withl : constant License_Type :=
                            License (Source_Index (Withu));

                  Unitl : constant License_Type :=
                           License (Source_Index (Current_Sem_Unit));

                  procedure License_Error;
                  --  Signal error of bad license

                  -------------------
                  -- License_Error --
                  -------------------

                  procedure License_Error is
                  begin
                     Error_Msg_N
                       ("?license of with'ed unit & may be inconsistent",
                        Name (Item));
                  end License_Error;

               --  Start of processing for License_Check

               begin
                  --  Exclude license check if withed unit is an internal unit.
                  --  This situation arises e.g. with the GPL version of GNAT.

                  if Is_Internal_File_Name (Unit_File_Name (Withu)) then
                     null;

                     --  Otherwise check various cases
                  else
                     case Unitl is
                        when Unknown =>
                           null;

                        when Restricted =>
                           if Withl = GPL then
                              License_Error;
                           end if;

                        when GPL =>
                           if Withl = Restricted then
                              License_Error;
                           end if;

                        when Modified_GPL =>
                           if Withl = Restricted or else Withl = GPL then
                              License_Error;
                           end if;

                        when Unrestricted =>
                           null;
                     end case;
                  end if;
               end License_Check;
            end if;

         --  Case of USE PACKAGE clause

         elsif Nkind (Item) = N_Use_Package_Clause then
            Analyze_Use_Package (Item);

         --  Case of USE TYPE clause

         elsif Nkind (Item) = N_Use_Type_Clause then
            Analyze_Use_Type (Item);

         --  case of PRAGMA

         elsif Nkind (Item) = N_Pragma then
            Analyze (Item);
         end if;

      <<Continue>>
         Next (Item);
      end loop;

      if Is_Child_Spec (Lib_Unit) then

         --  The unit also has implicit withs on its own parents

         if No (Context_Items (N)) then
            Set_Context_Items (N, New_List);
         end if;

         Implicit_With_On_Parent (Lib_Unit, N);
      end if;

      --  If the unit is a body, the context of the specification must also
      --  be installed.

      if Nkind (Lib_Unit) = N_Package_Body
        or else (Nkind (Lib_Unit) = N_Subprogram_Body
                   and then not Acts_As_Spec (N))
      then
         Install_Context (Library_Unit (N));

         if Is_Child_Spec (Unit (Library_Unit (N))) then

            --  If the unit is the body of a public child unit, the private
            --  declarations of the parent must be made visible. If the child
            --  unit is private, the private declarations have been installed
            --  already in the call to Install_Parents for the spec. Installing
            --  private declarations must be done for all ancestors of public
            --  child units. In addition, sibling units mentioned in the
            --  context clause of the body are directly visible.

            declare
               Lib_Spec : Node_Id;
               P        : Node_Id;
               P_Name   : Entity_Id;

            begin
               Lib_Spec := Unit (Library_Unit (N));
               while Is_Child_Spec (Lib_Spec) loop
                  P      := Unit (Parent_Spec (Lib_Spec));
                  P_Name := Defining_Entity (P);

                  if not (Private_Present (Parent (Lib_Spec)))
                    and then not In_Private_Part (P_Name)
                  then
                     Install_Private_Declarations (P_Name);
                     Install_Private_With_Clauses (P_Name);
                     Set_Use (Private_Declarations (Specification (P)));
                  end if;

                  Lib_Spec := P;
               end loop;
            end;
         end if;

         --  For a package body, children in context are immediately visible

         Install_Siblings (Defining_Entity (Unit (Library_Unit (N))), N);
      end if;

      if Nkind_In (Lib_Unit, N_Generic_Package_Declaration,
                             N_Generic_Subprogram_Declaration,
                             N_Package_Declaration,
                             N_Subprogram_Declaration)
      then
         if Is_Child_Spec (Lib_Unit) then
            Lib_Parent := Defining_Entity (Unit (Parent_Spec (Lib_Unit)));
            Set_Is_Private_Descendant
              (Defining_Entity (Lib_Unit),
               Is_Private_Descendant (Lib_Parent)
                 or else Private_Present (Parent (Lib_Unit)));

         else
            Set_Is_Private_Descendant
              (Defining_Entity (Lib_Unit),
               Private_Present (Parent (Lib_Unit)));
         end if;
      end if;

      if Check_Private then
         Check_Private_Child_Unit (N);
      end if;
   end Install_Context_Clauses;

   -------------------------------------
   -- Install_Limited_Context_Clauses --
   -------------------------------------

   procedure Install_Limited_Context_Clauses (N : Node_Id) is
      Item : Node_Id;

      procedure Check_Renamings (P : Node_Id; W : Node_Id);
      --  Check that the unlimited view of a given compilation_unit is not
      --  already visible through "use + renamings".

      procedure Check_Private_Limited_Withed_Unit (Item : Node_Id);
      --  Check that if a limited_with clause of a given compilation_unit
      --  mentions a descendant of a private child of some library unit,
      --  then the given compilation_unit shall be the declaration of a
      --  private descendant of that library unit.

      procedure Expand_Limited_With_Clause
        (Comp_Unit : Node_Id;
         Nam       : Node_Id;
         N         : Node_Id);
      --  If a child unit appears in a limited_with clause, there are implicit
      --  limited_with clauses on all parents that are not already visible
      --  through a regular with clause. This procedure creates the implicit
      --  limited with_clauses for the parents and loads the corresponding
      --  units. The shadow entities are created when the inserted clause is
      --  analyzed. Implements Ada 2005 (AI-50217).

      ---------------------
      -- Check_Renamings --
      ---------------------

      procedure Check_Renamings (P : Node_Id; W : Node_Id) is
         Item   : Node_Id;
         Spec   : Node_Id;
         WEnt   : Entity_Id;
         Nam    : Node_Id;
         E      : Entity_Id;
         E2     : Entity_Id;

      begin
         pragma Assert (Nkind (W) = N_With_Clause);

         --  Protect the frontend against previous critical errors

         case Nkind (Unit (Library_Unit (W))) is
            when N_Subprogram_Declaration         |
                 N_Package_Declaration            |
                 N_Generic_Subprogram_Declaration |
                 N_Generic_Package_Declaration    =>
               null;

            when others =>
               return;
         end case;

         --  Check "use + renamings"

         WEnt := Defining_Unit_Name (Specification (Unit (Library_Unit (W))));
         Spec := Specification (Unit (P));

         Item := First (Visible_Declarations (Spec));
         while Present (Item) loop

            --  Look only at use package clauses

            if Nkind (Item) = N_Use_Package_Clause then

               --  Traverse the list of packages

               Nam := First (Names (Item));
               while Present (Nam) loop
                  E := Entity (Nam);

                  pragma Assert (Present (Parent (E)));

                  if Nkind (Parent (E)) = N_Package_Renaming_Declaration
                    and then Renamed_Entity (E) = WEnt
                  then
                     --  The unlimited view is visible through use clause and
                     --  renamings. There is not need to generate the error
                     --  message here because Is_Visible_Through_Renamings
                     --  takes care of generating the precise error message.

                     return;

                  elsif Nkind (Parent (E)) = N_Package_Specification then

                     --  The use clause may refer to a local package.
                     --  Check all the enclosing scopes.

                     E2 := E;
                     while E2 /= Standard_Standard
                       and then E2 /= WEnt
                     loop
                        E2 := Scope (E2);
                     end loop;

                     if E2 = WEnt then
                        Error_Msg_N
                          ("unlimited view visible through use clause ", W);
                        return;
                     end if;
                  end if;

                  Next (Nam);
               end loop;
            end if;

            Next (Item);
         end loop;

         --  Recursive call to check all the ancestors

         if Is_Child_Spec (Unit (P)) then
            Check_Renamings (P => Parent_Spec (Unit (P)), W => W);
         end if;
      end Check_Renamings;

      ---------------------------------------
      -- Check_Private_Limited_Withed_Unit --
      ---------------------------------------

      procedure Check_Private_Limited_Withed_Unit (Item : Node_Id) is
         Curr_Parent  : Node_Id;
         Child_Parent : Node_Id;

      begin
         --  Compilation unit of the parent of the withed library unit

         Child_Parent := Parent_Spec (Unit (Library_Unit (Item)));

         --  If the child unit is a public child, then locate its nearest
         --  private ancestor, if any; Child_Parent will then be set to
         --  the parent of that ancestor.

         if not Private_Present (Library_Unit (Item)) then
            while Present (Child_Parent)
              and then not Private_Present (Child_Parent)
            loop
               Child_Parent := Parent_Spec (Unit (Child_Parent));
            end loop;

            if No (Child_Parent) then
               return;
            end if;

            Child_Parent := Parent_Spec (Unit (Child_Parent));
         end if;

         --  Traverse all the ancestors of the current compilation
         --  unit to check if it is a descendant of named library unit.

         Curr_Parent := Parent (Item);
         while Present (Parent_Spec (Unit (Curr_Parent)))
           and then Curr_Parent /= Child_Parent
         loop
            Curr_Parent := Parent_Spec (Unit (Curr_Parent));
         end loop;

         if Curr_Parent /= Child_Parent then
            Error_Msg_N
              ("unit in with clause is private child unit!", Item);
            Error_Msg_NE
              ("\current unit must also have parent&!",
               Item, Defining_Unit_Name (Specification (Unit (Child_Parent))));

         elsif not Private_Present (Parent (Item))
           and then not Private_Present (Item)
           and then not Nkind_In (Unit (Parent (Item)), N_Package_Body,
                                                        N_Subprogram_Body,
                                                        N_Subunit)
         then
            Error_Msg_NE
              ("current unit must also be private descendant of&",
               Item, Defining_Unit_Name (Specification (Unit (Child_Parent))));
         end if;
      end Check_Private_Limited_Withed_Unit;

      --------------------------------
      -- Expand_Limited_With_Clause --
      --------------------------------

      procedure Expand_Limited_With_Clause
        (Comp_Unit : Node_Id;
         Nam       : Node_Id;
         N         : Node_Id)
      is
         Loc   : constant Source_Ptr := Sloc (Nam);
         Unum  : Unit_Number_Type;
         Withn : Node_Id;

         function Previous_Withed_Unit (W : Node_Id) return Boolean;
         --  Returns true if the context already includes a with_clause for
         --  this unit. If the with_clause is non-limited, the unit is fully
         --  visible and an implicit limited_with should not be created. If
         --  there is already a limited_with clause for W, a second one is
         --  simply redundant.

         --------------------------
         -- Previous_Withed_Unit --
         --------------------------

         function Previous_Withed_Unit (W : Node_Id) return Boolean is
            Item : Node_Id;

         begin
            --  A limited with_clause cannot appear in the same context_clause
            --  as a nonlimited with_clause which mentions the same library.

            Item := First (Context_Items (Comp_Unit));
            while Present (Item) loop
               if Nkind (Item) = N_With_Clause
                 and then Library_Unit (Item) = Library_Unit (W)
               then
                  return True;
               end if;

               Next (Item);
            end loop;

            return False;
         end Previous_Withed_Unit;

      --  Start of processing for Expand_Limited_With_Clause

      begin
         New_Nodes_OK := New_Nodes_OK + 1;

         if Nkind (Nam) = N_Identifier then

            --  Create node for name of withed unit

            Withn :=
              Make_With_Clause (Loc,
                Name => New_Copy (Nam));

         else pragma Assert (Nkind (Nam) = N_Selected_Component);
            Withn :=
              Make_With_Clause (Loc,
                Name => Make_Selected_Component (Loc,
                  Prefix        => New_Copy_Tree (Prefix (Nam)),
                  Selector_Name => New_Copy (Selector_Name (Nam))));
            Set_Parent (Withn, Parent (N));
         end if;

         Set_Limited_Present (Withn);
         Set_First_Name      (Withn);
         Set_Implicit_With   (Withn);

         Unum :=
           Load_Unit
             (Load_Name  => Get_Spec_Name (Get_Unit_Name (Nam)),
              Required   => True,
              Subunit    => False,
              Error_Node => Nam);

         --  Do not generate a limited_with_clause on the current unit.
         --  This path is taken when a unit has a limited_with clause on
         --  one of its child units.

         if Unum = Current_Sem_Unit then
            return;
         end if;

         Set_Library_Unit (Withn, Cunit (Unum));
         Set_Corresponding_Spec
           (Withn, Specification (Unit (Cunit (Unum))));

         if not Previous_Withed_Unit (Withn) then
            Prepend (Withn, Context_Items (Parent (N)));
            Mark_Rewrite_Insertion (Withn);

            --  Add implicit limited_with_clauses for parents of child units
            --  mentioned in limited_with clauses.

            if Nkind (Nam) = N_Selected_Component then
               Expand_Limited_With_Clause (Comp_Unit, Prefix (Nam), N);
            end if;

            Analyze (Withn);

            if not Limited_View_Installed (Withn) then
               Install_Limited_Withed_Unit (Withn);
            end if;
         end if;

         New_Nodes_OK := New_Nodes_OK - 1;
      end Expand_Limited_With_Clause;

   --  Start of processing for Install_Limited_Context_Clauses

   begin
      Item := First (Context_Items (N));
      while Present (Item) loop
         if Nkind (Item) = N_With_Clause
           and then Limited_Present (Item)
         then
            if Nkind (Name (Item)) = N_Selected_Component then
               Expand_Limited_With_Clause
                 (Comp_Unit => N, Nam => Prefix (Name (Item)), N => Item);
            end if;

            Check_Private_Limited_Withed_Unit (Item);

            if not Implicit_With (Item)
              and then Is_Child_Spec (Unit (N))
            then
               Check_Renamings (Parent_Spec (Unit (N)), Item);
            end if;

            --  A unit may have a limited with on itself if it has a limited
            --  with_clause on one of its child units. In that case it is
            --  already being compiled and it makes no sense to install its
            --  limited view.

            --  If the item is a limited_private_with_clause, install it if the
            --  current unit is a body or if it is a private child. Otherwise
            --  the private clause is installed before analyzing the private
            --  part of the current unit.

            if Library_Unit (Item) /= Cunit (Current_Sem_Unit)
              and then not Limited_View_Installed (Item)
            then
               if not Private_Present (Item)
                 or else Private_Present (N)
                 or else Nkind_In (Unit (N), N_Package_Body,
                                             N_Subprogram_Body,
                                             N_Subunit)
               then
                  Install_Limited_Withed_Unit (Item);
               end if;
            end if;

         --  All items other than Limited_With clauses are ignored (they were
         --  installed separately early on by Install_Context_Clause).

         else
            null;
         end if;

         Next (Item);
      end loop;

      --  Ada 2005 (AI-412): Examine the visible declarations of a package
      --  spec, looking for incomplete subtype declarations of incomplete
      --  types visible through a limited with clause.

      if Ada_Version >= Ada_05
        and then Analyzed (N)
        and then Nkind (Unit (N)) = N_Package_Declaration
      then
         declare
            Decl         : Node_Id;
            Def_Id       : Entity_Id;
            Non_Lim_View : Entity_Id;

         begin
            Decl := First (Visible_Declarations (Specification (Unit (N))));
            while Present (Decl) loop
               if Nkind (Decl) = N_Subtype_Declaration
                 and then
                   Ekind (Defining_Identifier (Decl)) = E_Incomplete_Subtype
                 and then
                   From_With_Type (Defining_Identifier (Decl))
               then
                  Def_Id := Defining_Identifier (Decl);
                  Non_Lim_View := Non_Limited_View (Def_Id);

                  if not Is_Incomplete_Type (Non_Lim_View) then

                     --  Convert an incomplete subtype declaration into a
                     --  corresponding non-limited view subtype declaration.
                     --  This is usually the case when analyzing a body that
                     --  has regular with-clauses, when the spec has limited
                     --  ones.

                     --  If the non-limited view is still incomplete, it is
                     --  the dummy entry already created, and the declaration
                     --  cannot be reanalyzed. This is the case when installing
                     --  a parent unit that has limited with-clauses.

                     Set_Subtype_Indication (Decl,
                       New_Reference_To (Non_Lim_View, Sloc (Def_Id)));
                     Set_Etype (Def_Id, Non_Lim_View);
                     Set_Ekind (Def_Id, Subtype_Kind (Ekind (Non_Lim_View)));
                     Set_Analyzed (Decl, False);

                     --  Reanalyze the declaration, suppressing the call to
                     --  Enter_Name to avoid duplicate names.

                     Analyze_Subtype_Declaration
                      (N    => Decl,
                       Skip => True);
                  end if;
               end if;

               Next (Decl);
            end loop;
         end;
      end if;
   end Install_Limited_Context_Clauses;

   ---------------------
   -- Install_Parents --
   ---------------------

   procedure Install_Parents (Lib_Unit : Node_Id; Is_Private : Boolean) is
      P      : Node_Id;
      E_Name : Entity_Id;
      P_Name : Entity_Id;
      P_Spec : Node_Id;

   begin
      P := Unit (Parent_Spec (Lib_Unit));
      P_Name := Get_Parent_Entity (P);

      if Etype (P_Name) = Any_Type then
         return;
      end if;

      if Ekind (P_Name) = E_Generic_Package
        and then not Nkind_In (Lib_Unit, N_Generic_Subprogram_Declaration,
                                         N_Generic_Package_Declaration)
        and then Nkind (Lib_Unit) not in N_Generic_Renaming_Declaration
      then
         Error_Msg_N
           ("child of a generic package must be a generic unit", Lib_Unit);

      elsif not Is_Package_Or_Generic_Package (P_Name) then
         Error_Msg_N
           ("parent unit must be package or generic package", Lib_Unit);
         raise Unrecoverable_Error;

      elsif Present (Renamed_Object (P_Name)) then
         Error_Msg_N ("parent unit cannot be a renaming", Lib_Unit);
         raise Unrecoverable_Error;

      --  Verify that a child of an instance is itself an instance, or the
      --  renaming of one. Given that an instance that is a unit is replaced
      --  with a package declaration, check against the original node. The
      --  parent may be currently being instantiated, in which case it appears
      --  as a declaration, but the generic_parent is already established
      --  indicating that we deal with an instance.

      elsif Nkind (Original_Node (P)) = N_Package_Instantiation then
         if Nkind (Lib_Unit) in N_Renaming_Declaration
           or else Nkind (Original_Node (Lib_Unit)) in N_Generic_Instantiation
           or else
             (Nkind (Lib_Unit) = N_Package_Declaration
                and then Present (Generic_Parent (Specification (Lib_Unit))))
         then
            null;
         else
            Error_Msg_N
              ("child of an instance must be an instance or renaming",
                Lib_Unit);
         end if;
      end if;

      --  This is the recursive call that ensures all parents are loaded

      if Is_Child_Spec (P) then
         Install_Parents (P,
           Is_Private or else Private_Present (Parent (Lib_Unit)));
      end if;

      --  Now we can install the context for this parent

      Install_Context_Clauses (Parent_Spec (Lib_Unit));
      Install_Limited_Context_Clauses (Parent_Spec (Lib_Unit));
      Install_Siblings (P_Name, Parent (Lib_Unit));

      --  The child unit is in the declarative region of the parent. The parent
      --  must therefore appear in the scope stack and be visible, as when
      --  compiling the corresponding body. If the child unit is private or it
      --  is a package body, private declarations must be accessible as well.
      --  Use declarations in the parent must also be installed. Finally, other
      --  child units of the same parent that are in the context are
      --  immediately visible.

      --  Find entity for compilation unit, and set its private descendant
      --  status as needed.

      E_Name := Defining_Entity (Lib_Unit);

      Set_Is_Child_Unit (E_Name);

      Set_Is_Private_Descendant (E_Name,
         Is_Private_Descendant (P_Name)
           or else Private_Present (Parent (Lib_Unit)));

      P_Spec := Specification (Unit_Declaration_Node (P_Name));
      Push_Scope (P_Name);

      --  Save current visibility of unit

      Scope_Stack.Table (Scope_Stack.Last).Previous_Visibility :=
        Is_Immediately_Visible (P_Name);
      Set_Is_Immediately_Visible (P_Name);
      Install_Visible_Declarations (P_Name);
      Set_Use (Visible_Declarations (P_Spec));

      --  If the parent is a generic unit, its formal part may contain formal
      --  packages and use clauses for them.

      if Ekind (P_Name) = E_Generic_Package then
         Set_Use (Generic_Formal_Declarations (Parent (P_Spec)));
      end if;

      if Is_Private
        or else Private_Present (Parent (Lib_Unit))
      then
         Install_Private_Declarations (P_Name);
         Install_Private_With_Clauses (P_Name);
         Set_Use (Private_Declarations (P_Spec));
      end if;
   end Install_Parents;

   ----------------------------------
   -- Install_Private_With_Clauses --
   ----------------------------------

   procedure Install_Private_With_Clauses (P : Entity_Id) is
      Decl   : constant Node_Id := Unit_Declaration_Node (P);
      Item   : Node_Id;

   begin
      if Debug_Flag_I then
         Write_Str ("install private with clauses of ");
         Write_Name (Chars (P));
         Write_Eol;
      end if;

      if Nkind (Parent (Decl)) = N_Compilation_Unit then
         Item := First (Context_Items (Parent (Decl)));
         while Present (Item) loop
            if Nkind (Item) = N_With_Clause
              and then Private_Present (Item)
            then
               if Limited_Present (Item) then
                  if not Limited_View_Installed (Item) then
                     Install_Limited_Withed_Unit (Item);
                  end if;
               else
                  Install_Withed_Unit (Item, Private_With_OK => True);
               end if;
            end if;

            Next (Item);
         end loop;
      end if;
   end Install_Private_With_Clauses;

   ----------------------
   -- Install_Siblings --
   ----------------------

   procedure Install_Siblings (U_Name : Entity_Id; N : Node_Id) is
      Item : Node_Id;
      Id   : Entity_Id;
      Prev : Entity_Id;
   begin
      --  Iterate over explicit with clauses, and check whether the scope of
      --  each entity is an ancestor of the current unit, in which case it is
      --  immediately visible.

      Item := First (Context_Items (N));
      while Present (Item) loop

         --  Do not install private_with_clauses if the unit is a package
         --  declaration, unless it is itself a private child unit.

         if Nkind (Item) = N_With_Clause
           and then not Implicit_With (Item)
           and then not Limited_Present (Item)
           and then
              (not Private_Present (Item)
                 or else Nkind (Unit (N)) /= N_Package_Declaration
                 or else Private_Present (N))
         then
            Id := Entity (Name (Item));

            if Is_Child_Unit (Id)
              and then Is_Ancestor_Package (Scope (Id), U_Name)
            then
               Set_Is_Immediately_Visible (Id);

               --  Check for the presence of another unit in the context,
               --  that may be inadvertently hidden by the child.

               Prev := Current_Entity (Id);

               if Present (Prev)
                 and then Is_Immediately_Visible (Prev)
                 and then not Is_Child_Unit (Prev)
               then
                  declare
                     Clause : Node_Id;

                  begin
                     Clause := First (Context_Items (N));
                     while Present (Clause) loop
                        if Nkind (Clause) = N_With_Clause
                          and then Entity (Name (Clause)) = Prev
                        then
                           Error_Msg_NE
                              ("child unit& hides compilation unit " &
                               "with the same name?",
                                 Name (Item), Id);
                           exit;
                        end if;

                        Next (Clause);
                     end loop;
                  end;
               end if;

            --  The With_Clause may be on a grand-child or one of its further
            --  descendants, which makes a child immediately visible. Examine
            --  ancestry to determine whether such a child exists. For example,
            --  if current unit is A.C, and with_clause is on A.X.Y.Z, then X
            --  is immediately visible.

            elsif Is_Child_Unit (Id) then
               declare
                  Par : Entity_Id;

               begin
                  Par := Scope (Id);
                  while Is_Child_Unit (Par) loop
                     if Is_Ancestor_Package (Scope (Par), U_Name) then
                        Set_Is_Immediately_Visible (Par);
                        exit;
                     end if;

                     Par := Scope (Par);
                  end loop;
               end;
            end if;
         end if;

         Next (Item);
      end loop;
   end Install_Siblings;

   -------------------------------
   -- Install_Limited_With_Unit --
   -------------------------------

   procedure Install_Limited_Withed_Unit (N : Node_Id) is
      P_Unit           : constant Entity_Id := Unit (Library_Unit (N));
      E                : Entity_Id;
      P                : Entity_Id;
      Is_Child_Package : Boolean := False;
      Lim_Header       : Entity_Id;
      Lim_Typ          : Entity_Id;

      function Has_Limited_With_Clause
        (C_Unit : Entity_Id;
         Pack   : Entity_Id) return Boolean;
      --  Determine whether any package in the ancestor chain starting with
      --  C_Unit has a limited with clause for package Pack.

      function Has_With_Clause
        (C_Unit     : Node_Id;
         Pack       : Entity_Id;
         Is_Limited : Boolean := False) return Boolean;
      --  Determine whether compilation unit C_Unit contains a with clause
      --  for package Pack. Use flag Is_Limited to designate desired clause
      --  kind. This is a subsidiary routine to Has_Limited_With_Clause.

      function Is_Visible_Through_Renamings (P : Entity_Id) return Boolean;
      --  Check if some package installed though normal with-clauses has a
      --  renaming declaration of package P. AARM 10.1.2(21/2).

      -----------------------------
      -- Has_Limited_With_Clause --
      -----------------------------

      function Has_Limited_With_Clause
        (C_Unit : Entity_Id;
         Pack   : Entity_Id) return Boolean
      is
         Par      : Entity_Id;
         Par_Unit : Node_Id;

      begin
         Par := C_Unit;
         while Present (Par) loop
            if Ekind (Par) /= E_Package then
               exit;
            end if;

            --  Retrieve the Compilation_Unit node for Par and determine if
            --  its context clauses contain a limited with for Pack.

            Par_Unit := Parent (Parent (Parent (Par)));

            if Nkind (Par_Unit) = N_Package_Declaration then
               Par_Unit := Parent (Par_Unit);
            end if;

            if Has_With_Clause (Par_Unit, Pack, True) then
               return True;
            end if;

            --  If there are more ancestors, climb up the tree, otherwise
            --  we are done.

            if Is_Child_Unit (Par) then
               Par := Scope (Par);
            else
               exit;
            end if;
         end loop;

         return False;
      end Has_Limited_With_Clause;

      ---------------------
      -- Has_With_Clause --
      ---------------------

      function Has_With_Clause
        (C_Unit     : Node_Id;
         Pack       : Entity_Id;
         Is_Limited : Boolean := False) return Boolean
      is
         Item : Node_Id;
         Nam  : Entity_Id;

      begin
         if Present (Context_Items (C_Unit)) then
            Item := First (Context_Items (C_Unit));
            while Present (Item) loop
               if Nkind (Item) = N_With_Clause then

                  --  Retrieve the entity of the imported compilation unit

                  if Nkind (Name (Item)) = N_Selected_Component then
                     Nam := Entity (Selector_Name (Name (Item)));
                  else
                     Nam := Entity (Name (Item));
                  end if;

                  if Nam = Pack
                    and then
                      ((Is_Limited and then Limited_Present (Item))
                          or else
                       (not Is_Limited and then not Limited_Present (Item)))
                  then
                     return True;
                  end if;
               end if;

               Next (Item);
            end loop;
         end if;

         return False;
      end Has_With_Clause;

      ----------------------------------
      -- Is_Visible_Through_Renamings --
      ----------------------------------

      function Is_Visible_Through_Renamings (P : Entity_Id) return Boolean is
         Kind     : constant Node_Kind :=
                      Nkind (Unit (Cunit (Current_Sem_Unit)));
         Aux_Unit : Node_Id;
         Item     : Node_Id;
         Decl     : Entity_Id;

      begin
         --  Example of the error detected by this subprogram:

         --  package P is
         --    type T is ...
         --  end P;

         --  with P;
         --  package Q is
         --     package Ren_P renames P;
         --  end Q;

         --  with Q;
         --  package R is ...

         --  limited with P; -- ERROR
         --  package R.C is ...

         Aux_Unit := Cunit (Current_Sem_Unit);

         loop
            Item := First (Context_Items (Aux_Unit));
            while Present (Item) loop
               if Nkind (Item) = N_With_Clause
                 and then not Limited_Present (Item)
                 and then Nkind (Unit (Library_Unit (Item)))
                            = N_Package_Declaration
               then
                  Decl :=
                    First (Visible_Declarations
                            (Specification (Unit (Library_Unit (Item)))));
                  while Present (Decl) loop
                     if Nkind (Decl) = N_Package_Renaming_Declaration
                       and then Entity (Name (Decl)) = P
                     then
                        --  Generate the error message only if the current unit
                        --  is a package declaration; in case of subprogram
                        --  bodies and package bodies we just return true to
                        --  indicate that the limited view must not be
                        --  installed.

                        if Kind = N_Package_Declaration then
                           Error_Msg_N
                             ("simultaneous visibility of the limited and " &
                              "unlimited views not allowed", N);
                           Error_Msg_Sloc := Sloc (Item);
                           Error_Msg_NE
                             ("\\  unlimited view of & visible through the " &
                              "context clause #", N, P);
                           Error_Msg_Sloc := Sloc (Decl);
                           Error_Msg_NE ("\\  and the renaming #", N, P);
                        end if;

                        return True;
                     end if;

                     Next (Decl);
                  end loop;
               end if;

               Next (Item);
            end loop;

            if Present (Library_Unit (Aux_Unit)) then
               if Aux_Unit = Library_Unit (Aux_Unit) then

                  --  Aux_Unit is a body that acts as a spec. Clause has
                  --  already been flagged as illegal.

                  return False;

               else
                  Aux_Unit := Library_Unit (Aux_Unit);
               end if;
            else
               Aux_Unit := Parent_Spec (Unit (Aux_Unit));
            end if;

            exit when No (Aux_Unit);
         end loop;

         return False;
      end Is_Visible_Through_Renamings;

   --  Start of processing for Install_Limited_Withed_Unit

   begin
      pragma Assert (not Limited_View_Installed (N));

      --  In case of limited with_clause on subprograms, generics, instances,
      --  or renamings, the corresponding error was previously posted and we
      --  have nothing to do here.

      if Nkind (P_Unit) /= N_Package_Declaration then
         return;
      end if;

      P := Defining_Unit_Name (Specification (P_Unit));

      --  Handle child packages

      if Nkind (P) = N_Defining_Program_Unit_Name then
         Is_Child_Package := True;
         P := Defining_Identifier (P);
      end if;

      --  Do not install the limited-view if the full-view is already visible
      --  through renaming declarations.

      if Is_Visible_Through_Renamings (P) then
         return;
      end if;

      --  Do not install the limited view if this is the unit being analyzed.
      --  This unusual case will happen when a unit has a limited_with clause
      --  on one of its children. The compilation of the child forces the
      --  load of the parent which tries to install the limited view of the
      --  child again. Installing the limited view must also be disabled
      --  when compiling the body of the child unit.

      if P = Cunit_Entity (Current_Sem_Unit)
        or else
         (Nkind (Unit (Cunit (Current_Sem_Unit))) = N_Package_Body
            and then P = Main_Unit_Entity)
      then
         return;
      end if;

      --  This scenario is similar to the one above, the difference is that
      --  the compilation of sibling Par.Sib forces the load of parent Par
      --  which tries to install the limited view of Lim_Pack [1]. However
      --  Par.Sib has a with clause for Lim_Pack [2] in its body, and thus
      --  needs the non-limited views of all entities from Lim_Pack.

      --     limited with Lim_Pack;   --  [1]
      --     package Par is ...           package Lim_Pack is ...

      --                                  with Lim_Pack;  --  [2]
      --     package Par.Sib is ...       package body Par.Sib is ...

      --  In this case Main_Unit_Entity is the spec of Par.Sib and Current_
      --  Sem_Unit is the body of Par.Sib.

      if Ekind (P) = E_Package
        and then Ekind (Main_Unit_Entity) = E_Package
        and then Is_Child_Unit (Main_Unit_Entity)

         --  The body has a regular with clause

        and then Nkind (Unit (Cunit (Current_Sem_Unit))) = N_Package_Body
        and then Has_With_Clause (Cunit (Current_Sem_Unit), P)

         --  One of the ancestors has a limited with clause

        and then Nkind (Parent (Parent (Main_Unit_Entity))) =
                   N_Package_Specification
        and then Has_Limited_With_Clause (Scope (Main_Unit_Entity), P)
      then
         return;
      end if;

      --  A common use of the limited-with is to have a limited-with
      --  in the package spec, and a normal with in its package body.
      --  For example:

      --       limited with X;  -- [1]
      --       package A is ...

      --       with X;          -- [2]
      --       package body A is ...

      --  The compilation of A's body installs the context clauses found at [2]
      --  and then the context clauses of its specification (found at [1]). As
      --  a consequence, at [1] the specification of X has been analyzed and it
      --  is immediately visible. According to the semantics of limited-with
      --  context clauses we don't install the limited view because the full
      --  view of X supersedes its limited view.

      if Analyzed (P_Unit)
        and then (Is_Immediately_Visible (P)
                    or else (Is_Child_Package
                               and then Is_Visible_Child_Unit (P)))
      then
         --  Ada 2005 (AI-262): Install the private declarations of P

         if Private_Present (N)
           and then not In_Private_Part (P)
         then
            declare
               Id : Entity_Id;

            begin
               Id := First_Private_Entity (P);
               while Present (Id) loop
                  if not Is_Internal (Id)
                    and then not Is_Child_Unit (Id)
                  then
                     if not In_Chain (Id) then
                        Set_Homonym (Id, Current_Entity (Id));
                        Set_Current_Entity (Id);
                     end if;

                     Set_Is_Immediately_Visible (Id);
                  end if;

                  Next_Entity (Id);
               end loop;

               Set_In_Private_Part (P);
            end;
         end if;

         return;
      end if;

      if Debug_Flag_I then
         Write_Str ("install limited view of ");
         Write_Name (Chars (P));
         Write_Eol;
      end if;

      --  If the unit has not been analyzed and the limited view has not been
      --  already installed then we install it.

      if not Analyzed (P_Unit) then
         if not In_Chain (P) then

            --  Minimum decoration

            Set_Ekind (P, E_Package);
            Set_Etype (P, Standard_Void_Type);
            Set_Scope (P, Standard_Standard);

            if Is_Child_Package then
               Set_Is_Child_Unit (P);
               Set_Is_Visible_Child_Unit (P);
               Set_Scope (P, Defining_Entity (Unit (Parent_Spec (P_Unit))));
            end if;

            --  Place entity on visibility structure

            Set_Homonym (P, Current_Entity (P));
            Set_Current_Entity (P);

            if Debug_Flag_I then
               Write_Str ("   (homonym) chain ");
               Write_Name (Chars (P));
               Write_Eol;
            end if;

            --  Install the incomplete view. The first element of the limited
            --  view is a header (an E_Package entity) used to reference the
            --  first shadow entity in the private part of the package.

            Lim_Header := Limited_View (P);
            Lim_Typ    := First_Entity (Lim_Header);

            while Present (Lim_Typ)
              and then Lim_Typ /= First_Private_Entity (Lim_Header)
            loop
               Set_Homonym (Lim_Typ, Current_Entity (Lim_Typ));
               Set_Current_Entity (Lim_Typ);

               if Debug_Flag_I then
                  Write_Str ("   (homonym) chain ");
                  Write_Name (Chars (Lim_Typ));
                  Write_Eol;
               end if;

               Next_Entity (Lim_Typ);
            end loop;
         end if;

      --  If the unit appears in a previous regular with_clause, the regular
      --  entities of the public part of the withed package must be replaced
      --  by the shadow ones.

      --  This code must be kept synchronized with the code that replaces the
      --  shadow entities by the real entities (see body of Remove_Limited
      --  With_Clause); otherwise the contents of the homonym chains are not
      --  consistent.

      else
         --  Hide all the type entities of the public part of the package to
         --  avoid its usage. This is needed to cover all the subtype decla-
         --  rations because we do not remove them from the homonym chain.

         E := First_Entity (P);
         while Present (E) and then E /= First_Private_Entity (P) loop
            if Is_Type (E) then
               Set_Was_Hidden (E, Is_Hidden (E));
               Set_Is_Hidden (E);
            end if;

            Next_Entity (E);
         end loop;

         --  Replace the real entities by the shadow entities of the limited
         --  view. The first element of the limited view is a header that is
         --  used to reference the first shadow entity in the private part
         --  of the package. Successive elements are the limited views of the
         --  type (including regular incomplete types) declared in the package.

         Lim_Header := Limited_View (P);

         Lim_Typ := First_Entity (Lim_Header);
         while Present (Lim_Typ)
           and then Lim_Typ /= First_Private_Entity (Lim_Header)
         loop
            pragma Assert (not In_Chain (Lim_Typ));

            --  Do not unchain nested packages and child units

            if Ekind (Lim_Typ) /= E_Package
              and then not Is_Child_Unit (Lim_Typ)
            then
               declare
                  Prev : Entity_Id;

               begin
                  Prev := Current_Entity (Lim_Typ);
                  E := Prev;

                  --  Replace E in the homonyms list, so that the limited
                  --  view becomes available.

                  if E = Non_Limited_View (Lim_Typ) then
                     Set_Homonym (Lim_Typ, Homonym (Prev));
                     Set_Current_Entity (Lim_Typ);

                  else
                     loop
                        E := Homonym (Prev);

                        --  E may have been removed when installing a
                        --  previous limited_with_clause.

                        exit when No (E);

                        exit when E = Non_Limited_View (Lim_Typ);

                        Prev := Homonym (Prev);
                     end loop;

                     if Present (E) then
                        Set_Homonym (Lim_Typ, Homonym (Homonym (Prev)));
                        Set_Homonym (Prev, Lim_Typ);
                     end if;
                  end if;
               end;

               if Debug_Flag_I then
                  Write_Str ("   (homonym) chain ");
                  Write_Name (Chars (Lim_Typ));
                  Write_Eol;
               end if;
            end if;

            Next_Entity (Lim_Typ);
         end loop;
      end if;

      --  The package must be visible while the limited-with clause is active
      --  because references to the type P.T must resolve in the usual way.
      --  In addition, we remember that the limited-view has been installed to
      --  uninstall it at the point of context removal.

      Set_Is_Immediately_Visible (P);
      Set_Limited_View_Installed (N);

      --  If the package in the limited_with clause is a child unit, the
      --  clause is unanalyzed and appears as a selected component. Recast
      --  it as an expanded name so that the entity can be properly set. Use
      --  entity of parent, if available, for higher ancestors in the name.

      if Nkind (Name (N)) = N_Selected_Component then
         declare
            Nam : Node_Id;
            Ent : Entity_Id;

         begin
            Nam := Name (N);
            Ent := P;
            while Nkind (Nam) = N_Selected_Component
              and then Present (Ent)
            loop
               Change_Selected_Component_To_Expanded_Name (Nam);

               --  Set entity of parent identifiers if the unit is a child
               --  unit. This ensures that the tree is properly formed from
               --  semantic point of view (e.g. for ASIS queries).

               Set_Entity (Nam, Ent);

               Nam := Prefix (Nam);
               Ent := Scope (Ent);

               --  Set entity of last ancestor

               if Nkind (Nam) = N_Identifier then
                  Set_Entity (Nam, Ent);
               end if;
            end loop;
         end;
      end if;

      Set_Entity (Name (N), P);
      Set_From_With_Type (P);
   end Install_Limited_Withed_Unit;

   -------------------------
   -- Install_Withed_Unit --
   -------------------------

   procedure Install_Withed_Unit
     (With_Clause     : Node_Id;
      Private_With_OK : Boolean := False)
   is
      Uname : constant Entity_Id := Entity (Name (With_Clause));
      P     : constant Entity_Id := Scope (Uname);

   begin
      --  Ada 2005 (AI-262): Do not install the private withed unit if we are
      --  compiling a package declaration and the Private_With_OK flag was not
      --  set by the caller. These declarations will be installed later (before
      --  analyzing the private part of the package).

      if Private_Present (With_Clause)
        and then Nkind (Unit (Parent (With_Clause))) = N_Package_Declaration
        and then not (Private_With_OK)
      then
         return;
      end if;

      if Debug_Flag_I then
         if Private_Present (With_Clause) then
            Write_Str ("install private withed unit ");
         else
            Write_Str ("install withed unit ");
         end if;

         Write_Name (Chars (Uname));
         Write_Eol;
      end if;

      --  We do not apply the restrictions to an internal unit unless
      --  we are compiling the internal unit as a main unit. This check
      --  is also skipped for dummy units (for missing packages).

      if Sloc (Uname) /= No_Location
        and then (not Is_Internal_File_Name (Unit_File_Name (Current_Sem_Unit))
                    or else Current_Sem_Unit = Main_Unit)
      then
         Check_Restricted_Unit
           (Unit_Name (Get_Source_Unit (Uname)), With_Clause);
      end if;

      if P /= Standard_Standard then

         --  If the unit is not analyzed after analysis of the with clause and
         --  it is an instantiation then it awaits a body and is the main unit.
         --  Its appearance in the context of some other unit indicates a
         --  circular dependency (DEC suite perversity).

         if not Analyzed (Uname)
           and then Nkind (Parent (Uname)) = N_Package_Instantiation
         then
            Error_Msg_N
              ("instantiation depends on itself", Name (With_Clause));

         elsif not Is_Visible_Child_Unit (Uname) then
            Set_Is_Visible_Child_Unit (Uname);

            --  If the child unit appears in the context of its parent, it is
            --  immediately visible.

            if In_Open_Scopes (Scope (Uname)) then
               Set_Is_Immediately_Visible (Uname);
            end if;

            if Is_Generic_Instance (Uname)
              and then Ekind (Uname) in Subprogram_Kind
            then
               --  Set flag as well on the visible entity that denotes the
               --  instance, which renames the current one.

               Set_Is_Visible_Child_Unit
                 (Related_Instance
                   (Defining_Entity (Unit (Library_Unit (With_Clause)))));
            end if;

            --  The parent unit may have been installed already, and may have
            --  appeared in a use clause.

            if In_Use (Scope (Uname)) then
               Set_Is_Potentially_Use_Visible (Uname);
            end if;

            Set_Context_Installed (With_Clause);
         end if;

      elsif not Is_Immediately_Visible (Uname) then
         if not Private_Present (With_Clause)
           or else Private_With_OK
         then
            Set_Is_Immediately_Visible (Uname);
         end if;

         Set_Context_Installed (With_Clause);
      end if;

      --   A with-clause overrides a with-type clause: there are no restric-
      --   tions on the use of package entities.

      if Ekind (Uname) = E_Package then
         Set_From_With_Type (Uname, False);
      end if;

      --  Ada 2005 (AI-377): it is illegal for a with_clause to name a child
      --  unit if there is a visible homograph for it declared in the same
      --  declarative region. This pathological case can only arise when an
      --  instance I1 of a generic unit G1 has an explicit child unit I1.G2,
      --  G1 has a generic child also named G2, and the context includes with_
      --  clauses for both I1.G2 and for G1.G2, making an implicit declaration
      --  of I1.G2 visible as well. If the child unit is named Standard, do
      --  not apply the check to the Standard package itself.

      if Is_Child_Unit (Uname)
        and then Is_Visible_Child_Unit (Uname)
        and then Ada_Version >= Ada_05
      then
         declare
            Decl1 : constant Node_Id  := Unit_Declaration_Node (P);
            Decl2 : Node_Id;
            P2    : Entity_Id;
            U2    : Entity_Id;

         begin
            U2 := Homonym (Uname);
            while Present (U2)
              and then U2 /= Standard_Standard
           loop
               P2 := Scope (U2);
               Decl2  := Unit_Declaration_Node (P2);

               if Is_Child_Unit (U2)
                 and then Is_Visible_Child_Unit (U2)
               then
                  if Is_Generic_Instance (P)
                    and then Nkind (Decl1) = N_Package_Declaration
                    and then Generic_Parent (Specification (Decl1)) = P2
                  then
                     Error_Msg_N ("illegal with_clause", With_Clause);
                     Error_Msg_N
                       ("\child unit has visible homograph" &
                           " (RM 8.3(26), 10.1.1(19))",
                         With_Clause);
                     exit;

                  elsif Is_Generic_Instance (P2)
                    and then Nkind (Decl2) = N_Package_Declaration
                    and then Generic_Parent (Specification (Decl2)) = P
                  then
                     --  With_clause for child unit of instance appears before
                     --  in the context. We want to place the error message on
                     --  it, not on the generic child unit itself.

                     declare
                        Prev_Clause : Node_Id;

                     begin
                        Prev_Clause := First (List_Containing (With_Clause));
                        while Entity (Name (Prev_Clause)) /= U2 loop
                           Next (Prev_Clause);
                        end loop;

                        pragma Assert (Present (Prev_Clause));
                        Error_Msg_N ("illegal with_clause", Prev_Clause);
                        Error_Msg_N
                          ("\child unit has visible homograph" &
                              " (RM 8.3(26), 10.1.1(19))",
                            Prev_Clause);
                        exit;
                     end;
                  end if;
               end if;

               U2 := Homonym (U2);
            end loop;
         end;
      end if;
   end Install_Withed_Unit;

   -------------------
   -- Is_Child_Spec --
   -------------------

   function Is_Child_Spec (Lib_Unit : Node_Id) return Boolean is
      K : constant Node_Kind := Nkind (Lib_Unit);

   begin
      return (K in N_Generic_Declaration              or else
              K in N_Generic_Instantiation            or else
              K in N_Generic_Renaming_Declaration     or else
              K =  N_Package_Declaration              or else
              K =  N_Package_Renaming_Declaration     or else
              K =  N_Subprogram_Declaration           or else
              K =  N_Subprogram_Renaming_Declaration)
        and then Present (Parent_Spec (Lib_Unit));
   end Is_Child_Spec;

   -----------------------
   -- Load_Needed_Body --
   -----------------------

   --  N is a generic unit named in a with clause, or else it is a unit that
   --  contains a generic unit or an inlined function. In order to perform an
   --  instantiation, the body of the unit must be present. If the unit itself
   --  is generic, we assume that an instantiation follows, and load & analyze
   --  the body unconditionally. This forces analysis of the spec as well.

   --  If the unit is not generic, but contains a generic unit, it is loaded on
   --  demand, at the point of instantiation (see ch12).

   procedure Load_Needed_Body (N : Node_Id; OK : out Boolean) is
      Body_Name : Unit_Name_Type;
      Unum      : Unit_Number_Type;

      Save_Style_Check : constant Boolean := Opt.Style_Check;
      --  The loading and analysis is done with style checks off

   begin
      if not GNAT_Mode then
         Style_Check := False;
      end if;

      Body_Name := Get_Body_Name (Get_Unit_Name (Unit (N)));
      Unum :=
        Load_Unit
          (Load_Name  => Body_Name,
           Required   => False,
           Subunit    => False,
           Error_Node => N,
           Renamings  => True);

      if Unum = No_Unit then
         OK := False;

      else
         Compiler_State := Analyzing; -- reset after load

         if not Fatal_Error (Unum) or else Try_Semantics then
            if Debug_Flag_L then
               Write_Str ("*** Loaded generic body");
               Write_Eol;
            end if;

            Semantics (Cunit (Unum));
         end if;

         OK := True;
      end if;

      Style_Check := Save_Style_Check;
   end Load_Needed_Body;

   -------------------------
   -- Build_Limited_Views --
   -------------------------

   procedure Build_Limited_Views (N : Node_Id) is
      Unum : constant Unit_Number_Type := Get_Source_Unit (Library_Unit (N));
      P    : constant Entity_Id        := Cunit_Entity (Unum);

      Spec        : Node_Id;            --  To denote a package specification
      Lim_Typ     : Entity_Id;          --  To denote shadow entities
      Comp_Typ    : Entity_Id;          --  To denote real entities

      Lim_Header  : Entity_Id;          --  Package entity
      Last_Lim_E  : Entity_Id := Empty; --  Last limited entity built
      Last_Pub_Lim_E : Entity_Id;       --  To set the first private entity

      procedure Decorate_Incomplete_Type
        (E    : Entity_Id;
         Scop : Entity_Id);
      --  Add attributes of an incomplete type to a shadow entity. The same
      --  attributes are placed on the real entity, so that gigi receives
      --  a consistent view.

      procedure Decorate_Package_Specification (P : Entity_Id);
      --  Add attributes of a package entity to the entity in a package
      --  declaration

      procedure Decorate_Tagged_Type
        (Loc  : Source_Ptr;
         T    : Entity_Id;
         Scop : Entity_Id);
      --  Set basic attributes of tagged type T, including its class_wide type.
      --  The parameters Loc, Scope are used to decorate the class_wide type.

      procedure Build_Chain
        (Scope      : Entity_Id;
         First_Decl : Node_Id);
      --  Construct list of shadow entities and attach it to entity of
      --  package that is mentioned in a limited_with clause.

      function New_Internal_Shadow_Entity
        (Kind       : Entity_Kind;
         Sloc_Value : Source_Ptr;
         Id_Char    : Character) return Entity_Id;
      --  Build a new internal entity and append it to the list of shadow
      --  entities available through the limited-header

      ------------------------------
      -- Decorate_Incomplete_Type --
      ------------------------------

      procedure Decorate_Incomplete_Type
        (E    : Entity_Id;
         Scop : Entity_Id)
      is
      begin
         Set_Ekind             (E, E_Incomplete_Type);
         Set_Scope             (E, Scop);
         Set_Etype             (E, E);
         Set_Is_First_Subtype  (E, True);
         Set_Stored_Constraint (E, No_Elist);
         Set_Full_View         (E, Empty);
         Init_Size_Align       (E);
      end Decorate_Incomplete_Type;

      --------------------------
      -- Decorate_Tagged_Type --
      --------------------------

      procedure Decorate_Tagged_Type
        (Loc  : Source_Ptr;
         T    : Entity_Id;
         Scop : Entity_Id)
      is
         CW : Entity_Id;

      begin
         Decorate_Incomplete_Type (T, Scop);
         Set_Is_Tagged_Type (T);

         --  Build corresponding class_wide type, if not previously done

         --  Warning: The class-wide entity is shared by the limited-view
         --  and the full-view.

         if No (Class_Wide_Type (T)) then
            CW := Make_Defining_Identifier (Loc,  New_Internal_Name ('S'));

            Set_Ekind                     (CW, E_Class_Wide_Type);
            Set_Etype                     (CW, T);
            Set_Scope                     (CW, Scop);
            Set_Is_Tagged_Type            (CW);
            Set_Is_First_Subtype          (CW, True);
            Init_Size_Align               (CW);
            Set_Has_Unknown_Discriminants (CW, True);
            Set_Class_Wide_Type           (CW, CW);
            Set_Equivalent_Type           (CW, Empty);
            Set_From_With_Type            (CW, From_With_Type (T));

            Set_Class_Wide_Type           (T, CW);
         end if;
      end Decorate_Tagged_Type;

      ------------------------------------
      -- Decorate_Package_Specification --
      ------------------------------------

      procedure Decorate_Package_Specification (P : Entity_Id) is
      begin
         --  Place only the most basic attributes

         Set_Ekind (P, E_Package);
         Set_Etype (P, Standard_Void_Type);
      end Decorate_Package_Specification;

      --------------------------------
      -- New_Internal_Shadow_Entity --
      --------------------------------

      function New_Internal_Shadow_Entity
        (Kind       : Entity_Kind;
         Sloc_Value : Source_Ptr;
         Id_Char    : Character) return Entity_Id
      is
         E : constant Entity_Id :=
               Make_Defining_Identifier (Sloc_Value,
                 Chars => New_Internal_Name (Id_Char));

      begin
         Set_Ekind       (E, Kind);
         Set_Is_Internal (E, True);

         if Kind in Type_Kind then
            Init_Size_Align (E);
         end if;

         Append_Entity (E, Lim_Header);
         Last_Lim_E := E;
         return E;
      end New_Internal_Shadow_Entity;

      -----------------
      -- Build_Chain --
      -----------------

      procedure Build_Chain
        (Scope         : Entity_Id;
         First_Decl    : Node_Id)
      is
         Analyzed_Unit : constant Boolean := Analyzed (Cunit (Unum));
         Is_Tagged     : Boolean;
         Decl          : Node_Id;

      begin
         Decl := First_Decl;
         while Present (Decl) loop

            --  For each library_package_declaration in the environment, there
            --  is an implicit declaration of a *limited view* of that library
            --  package. The limited view of a package contains:

            --   * For each nested package_declaration, a declaration of the
            --     limited view of that package, with the same defining-
            --     program-unit name.

            --   * For each type_declaration in the visible part, an incomplete
            --     type-declaration with the same defining_identifier, whose
            --     completion is the type_declaration. If the type_declaration
            --     is tagged, then the incomplete_type_declaration is tagged
            --     incomplete.

            --     The partial view is tagged if the declaration has the
            --     explicit keyword, or else if it is a type extension, both
            --     of which can be ascertained syntactically.

            if Nkind (Decl) = N_Full_Type_Declaration then
               Is_Tagged :=
                  (Nkind (Type_Definition (Decl)) = N_Record_Definition
                    and then Tagged_Present (Type_Definition (Decl)))
                 or else
                   (Nkind (Type_Definition (Decl)) = N_Derived_Type_Definition
                     and then
                       Present
                         (Record_Extension_Part (Type_Definition (Decl))));

               Comp_Typ := Defining_Identifier (Decl);

               if not Analyzed_Unit then
                  if Is_Tagged then
                     Decorate_Tagged_Type (Sloc (Decl), Comp_Typ, Scope);
                  else
                     Decorate_Incomplete_Type (Comp_Typ, Scope);
                  end if;
               end if;

               --  Create shadow entity for type

               Lim_Typ := New_Internal_Shadow_Entity
                 (Kind       => Ekind (Comp_Typ),
                  Sloc_Value => Sloc (Comp_Typ),
                  Id_Char    => 'Z');

               Set_Chars  (Lim_Typ, Chars (Comp_Typ));
               Set_Parent (Lim_Typ, Parent (Comp_Typ));
               Set_From_With_Type (Lim_Typ);

               if Is_Tagged then
                  Decorate_Tagged_Type (Sloc (Decl), Lim_Typ, Scope);
               else
                  Decorate_Incomplete_Type (Lim_Typ, Scope);
               end if;

               Set_Non_Limited_View (Lim_Typ, Comp_Typ);

            elsif Nkind (Decl) = N_Private_Type_Declaration
              or else Nkind (Decl) = N_Incomplete_Type_Declaration
            then
               Comp_Typ := Defining_Identifier (Decl);

               if not Analyzed_Unit then
                  if Tagged_Present (Decl) then
                     Decorate_Tagged_Type (Sloc (Decl), Comp_Typ, Scope);
                  else
                     Decorate_Incomplete_Type (Comp_Typ, Scope);
                  end if;
               end if;

               Lim_Typ  := New_Internal_Shadow_Entity
                 (Kind       => Ekind (Comp_Typ),
                  Sloc_Value => Sloc (Comp_Typ),
                  Id_Char    => 'Z');

               Set_Chars  (Lim_Typ, Chars (Comp_Typ));
               Set_Parent (Lim_Typ, Parent (Comp_Typ));
               Set_From_With_Type (Lim_Typ);

               if Tagged_Present (Decl) then
                  Decorate_Tagged_Type (Sloc (Decl), Lim_Typ, Scope);
               else
                  Decorate_Incomplete_Type (Lim_Typ, Scope);
               end if;

               Set_Non_Limited_View (Lim_Typ, Comp_Typ);

            elsif Nkind (Decl) = N_Private_Extension_Declaration then
               Comp_Typ := Defining_Identifier (Decl);

               if not Analyzed_Unit then
                  Decorate_Tagged_Type (Sloc (Decl), Comp_Typ, Scope);
               end if;

               --  Create shadow entity for type

               Lim_Typ := New_Internal_Shadow_Entity
                 (Kind       => Ekind (Comp_Typ),
                  Sloc_Value => Sloc (Comp_Typ),
                  Id_Char    => 'Z');

               Set_Chars  (Lim_Typ, Chars (Comp_Typ));
               Set_Parent (Lim_Typ, Parent (Comp_Typ));
               Set_From_With_Type (Lim_Typ);

               Decorate_Tagged_Type (Sloc (Decl), Lim_Typ, Scope);
               Set_Non_Limited_View (Lim_Typ, Comp_Typ);

            elsif Nkind (Decl) = N_Package_Declaration then

               --  Local package

               declare
                  Spec : constant Node_Id := Specification (Decl);

               begin
                  Comp_Typ := Defining_Unit_Name (Spec);

                  if not Analyzed (Cunit (Unum)) then
                     Decorate_Package_Specification (Comp_Typ);
                     Set_Scope (Comp_Typ, Scope);
                  end if;

                  Lim_Typ  := New_Internal_Shadow_Entity
                    (Kind       => Ekind (Comp_Typ),
                     Sloc_Value => Sloc (Comp_Typ),
                     Id_Char    => 'Z');

                  Decorate_Package_Specification (Lim_Typ);
                  Set_Scope (Lim_Typ, Scope);

                  Set_Chars (Lim_Typ, Chars (Comp_Typ));
                  Set_Parent (Lim_Typ, Parent (Comp_Typ));
                  Set_From_With_Type (Lim_Typ);

                  --  Note: The non_limited_view attribute is not used
                  --  for local packages.

                  Build_Chain
                    (Scope      => Lim_Typ,
                     First_Decl => First (Visible_Declarations (Spec)));
               end;
            end if;

            Next (Decl);
         end loop;
      end Build_Chain;

   --  Start of processing for Build_Limited_Views

   begin
      pragma Assert (Limited_Present (N));

      --  A library_item mentioned in a limited_with_clause shall
      --  be a package_declaration, not a subprogram_declaration,
      --  generic_declaration, generic_instantiation, or
      --  package_renaming_declaration

      case Nkind (Unit (Library_Unit (N))) is

         when N_Package_Declaration =>
            null;

         when N_Subprogram_Declaration =>
            Error_Msg_N ("subprograms not allowed in "
                         & "limited with_clauses", N);
            return;

         when N_Generic_Package_Declaration |
              N_Generic_Subprogram_Declaration =>
            Error_Msg_N ("generics not allowed in "
                         & "limited with_clauses", N);
            return;

         when N_Generic_Instantiation =>
            Error_Msg_N ("generic instantiations not allowed in "
                         & "limited with_clauses", N);
            return;

         when N_Generic_Renaming_Declaration =>
            Error_Msg_N ("generic renamings not allowed in "
                         & "limited with_clauses", N);
            return;

         when N_Subprogram_Renaming_Declaration =>
            Error_Msg_N ("renamed subprograms not allowed in "
                         & "limited with_clauses", N);
            return;

         when N_Package_Renaming_Declaration =>
            Error_Msg_N ("renamed packages not allowed in "
                         & "limited with_clauses", N);
            return;

         when others =>
            raise Program_Error;
      end case;

      --  Check if the chain is already built

      Spec := Specification (Unit (Library_Unit (N)));

      if Limited_View_Installed (Spec) then
         return;
      end if;

      Set_Ekind (P, E_Package);

      --  Build the header of the limited_view

      Lim_Header := Make_Defining_Identifier (Sloc (N),
                      Chars => New_Internal_Name (Id_Char => 'Z'));
      Set_Ekind (Lim_Header, E_Package);
      Set_Is_Internal (Lim_Header);
      Set_Limited_View (P, Lim_Header);

      --  Create the auxiliary chain. All the shadow entities are appended to
      --  the list of entities of the limited-view header

      Build_Chain
        (Scope      => P,
         First_Decl => First (Visible_Declarations (Spec)));

      --  Save the last built shadow entity. It is needed later to set the
      --  reference to the first shadow entity in the private part

      Last_Pub_Lim_E := Last_Lim_E;

      --  Ada 2005 (AI-262): Add the limited view of the private declarations
      --  Required to give support to limited-private-with clauses

      Build_Chain (Scope      => P,
                   First_Decl => First (Private_Declarations (Spec)));

      if Last_Pub_Lim_E /= Empty then
         Set_First_Private_Entity (Lim_Header,
                                   Next_Entity (Last_Pub_Lim_E));
      else
         Set_First_Private_Entity (Lim_Header,
                                   First_Entity (P));
      end if;

      Set_Limited_View_Installed (Spec);
   end Build_Limited_Views;

   -------------------------------
   -- Check_Body_Needed_For_SAL --
   -------------------------------

   procedure Check_Body_Needed_For_SAL (Unit_Name : Entity_Id) is

      function Entity_Needs_Body (E : Entity_Id) return Boolean;
      --  Determine whether use of entity E might require the presence of its
      --  body. For a package this requires a recursive traversal of all nested
      --  declarations.

      ---------------------------
      -- Entity_Needed_For_SAL --
      ---------------------------

      function Entity_Needs_Body (E : Entity_Id) return Boolean is
         Ent : Entity_Id;

      begin
         if Is_Subprogram (E)
           and then Has_Pragma_Inline (E)
         then
            return True;

         elsif Ekind (E) = E_Generic_Function
           or else Ekind (E) = E_Generic_Procedure
         then
            return True;

         elsif Ekind (E) = E_Generic_Package
           and then
             Nkind (Unit_Declaration_Node (E)) = N_Generic_Package_Declaration
           and then Present (Corresponding_Body (Unit_Declaration_Node (E)))
         then
            return True;

         elsif Ekind (E) = E_Package
           and then
             Nkind (Unit_Declaration_Node (E)) = N_Package_Declaration
           and then Present (Corresponding_Body (Unit_Declaration_Node (E)))
         then
            Ent := First_Entity (E);
            while Present (Ent) loop
               if Entity_Needs_Body (Ent) then
                  return True;
               end if;

               Next_Entity (Ent);
            end loop;

            return False;

         else
            return False;
         end if;
      end Entity_Needs_Body;

   --  Start of processing for Check_Body_Needed_For_SAL

   begin
      if Ekind (Unit_Name) = E_Generic_Package
        and then
          Nkind (Unit_Declaration_Node (Unit_Name)) =
                                            N_Generic_Package_Declaration
        and then
          Present (Corresponding_Body (Unit_Declaration_Node (Unit_Name)))
      then
         Set_Body_Needed_For_SAL (Unit_Name);

      elsif Ekind (Unit_Name) = E_Generic_Procedure
        or else Ekind (Unit_Name) = E_Generic_Function
      then
         Set_Body_Needed_For_SAL (Unit_Name);

      elsif Is_Subprogram (Unit_Name)
        and then Nkind (Unit_Declaration_Node (Unit_Name)) =
                                            N_Subprogram_Declaration
        and then Has_Pragma_Inline (Unit_Name)
      then
         Set_Body_Needed_For_SAL (Unit_Name);

      elsif Ekind (Unit_Name) = E_Subprogram_Body then
         Check_Body_Needed_For_SAL
           (Corresponding_Spec (Unit_Declaration_Node (Unit_Name)));

      elsif Ekind (Unit_Name) = E_Package
        and then Entity_Needs_Body (Unit_Name)
      then
         Set_Body_Needed_For_SAL (Unit_Name);

      elsif Ekind (Unit_Name) = E_Package_Body
        and then Nkind (Unit_Declaration_Node (Unit_Name)) = N_Package_Body
      then
         Check_Body_Needed_For_SAL
           (Corresponding_Spec (Unit_Declaration_Node (Unit_Name)));
      end if;
   end Check_Body_Needed_For_SAL;

   --------------------
   -- Remove_Context --
   --------------------

   procedure Remove_Context (N : Node_Id) is
      Lib_Unit : constant Node_Id := Unit (N);

   begin
      --  If this is a child unit, first remove the parent units

      if Is_Child_Spec (Lib_Unit) then
         Remove_Parents (Lib_Unit);
      end if;

      Remove_Context_Clauses (N);
   end Remove_Context;

   ----------------------------
   -- Remove_Context_Clauses --
   ----------------------------

   procedure Remove_Context_Clauses (N : Node_Id) is
      Item      : Node_Id;
      Unit_Name : Entity_Id;

   begin
      --  Ada 2005 (AI-50217): We remove the context clauses in two phases:
      --  limited-views first and regular-views later (to maintain the
      --  stack model).

      --  First Phase: Remove limited_with context clauses

      Item := First (Context_Items (N));
      while Present (Item) loop

         --  We are interested only in with clauses which got installed
         --  on entry.

         if Nkind (Item) = N_With_Clause
           and then Limited_Present (Item)
           and then Limited_View_Installed (Item)
         then
            Remove_Limited_With_Clause (Item);
         end if;

         Next (Item);
      end loop;

      --  Second Phase: Loop through context items and undo regular
      --  with_clauses and use_clauses.

      Item := First (Context_Items (N));
      while Present (Item) loop

         --  We are interested only in with clauses which got installed on
         --  entry, as indicated by their Context_Installed flag set

         if Nkind (Item) = N_With_Clause
           and then Limited_Present (Item)
           and then Limited_View_Installed (Item)
         then
            null;

         elsif Nkind (Item) = N_With_Clause
            and then Context_Installed (Item)
         then
            --  Remove items from one with'ed unit

            Unit_Name := Entity (Name (Item));
            Remove_Unit_From_Visibility (Unit_Name);
            Set_Context_Installed (Item, False);

         elsif Nkind (Item) = N_Use_Package_Clause then
            End_Use_Package (Item);

         elsif Nkind (Item) = N_Use_Type_Clause then
            End_Use_Type (Item);
         end if;

         Next (Item);
      end loop;
   end Remove_Context_Clauses;

   --------------------------------
   -- Remove_Limited_With_Clause --
   --------------------------------

   procedure Remove_Limited_With_Clause (N : Node_Id) is
      P_Unit     : constant Entity_Id := Unit (Library_Unit (N));
      E          : Entity_Id;
      P          : Entity_Id;
      Lim_Header : Entity_Id;
      Lim_Typ    : Entity_Id;
      Prev       : Entity_Id;

   begin
      pragma Assert (Limited_View_Installed (N));

      --  In case of limited with_clause on subprograms, generics, instances,
      --  or renamings, the corresponding error was previously posted and we
      --  have nothing to do here.

      if Nkind (P_Unit) /= N_Package_Declaration then
         return;
      end if;

      P := Defining_Unit_Name (Specification (P_Unit));

      --  Handle child packages

      if Nkind (P) = N_Defining_Program_Unit_Name then
         P := Defining_Identifier (P);
      end if;

      if Debug_Flag_I then
         Write_Str ("remove limited view of ");
         Write_Name (Chars (P));
         Write_Str (" from visibility");
         Write_Eol;
      end if;

      --  Prepare the removal of the shadow entities from visibility. The
      --  first element of the limited view is a header (an E_Package
      --  entity) that is used to reference the first shadow entity in the
      --  private part of the package

      Lim_Header := Limited_View (P);
      Lim_Typ    := First_Entity (Lim_Header);

      --  Remove package and shadow entities from visibility if it has not
      --  been analyzed

      if not Analyzed (P_Unit) then
         Unchain (P);
         Set_Is_Immediately_Visible (P, False);

         while Present (Lim_Typ) loop
            Unchain (Lim_Typ);
            Next_Entity (Lim_Typ);
         end loop;

      --  Otherwise this package has already appeared in the closure and its
      --  shadow entities must be replaced by its real entities. This code
      --  must be kept synchronized with the complementary code in Install
      --  Limited_Withed_Unit.

      else
         --  Real entities that are type or subtype declarations were hidden
         --  from visibility at the point of installation of the limited-view.
         --  Now we recover the previous value of the hidden attribute.

         E := First_Entity (P);
         while Present (E) and then E /= First_Private_Entity (P) loop
            if Is_Type (E) then
               Set_Is_Hidden (E, Was_Hidden (E));
            end if;

            Next_Entity (E);
         end loop;

         while Present (Lim_Typ)
           and then Lim_Typ /= First_Private_Entity (Lim_Header)
         loop
            --  Nested packages and child units were not unchained

            if Ekind (Lim_Typ) /= E_Package
              and then not Is_Child_Unit (Non_Limited_View (Lim_Typ))
            then
               --  Handle incomplete types of the real view. For this purpose
               --  we traverse the list of visible entities to look for an
               --  incomplete type in the real-view associated with Lim_Typ.

               E := First_Entity (P);
               while Present (E) and then E /= First_Private_Entity (P) loop
                  exit when Ekind (E) = E_Incomplete_Type
                    and then Present (Full_View (E))
                    and then Full_View (E) = Lim_Typ;

                  Next_Entity (E);
               end loop;

               --  If the previous search was not sucessful then the entity
               --  to be restored in the homonym list is the non-limited view

               if E = First_Private_Entity (P) then
                  E := Non_Limited_View (Lim_Typ);
               end if;

               pragma Assert (not In_Chain (E));

               Prev := Current_Entity (Lim_Typ);

               if Prev = Lim_Typ then
                  Set_Current_Entity (E);

               else
                  while Present (Prev)
                    and then Homonym (Prev) /= Lim_Typ
                  loop
                     Prev := Homonym (Prev);
                  end loop;

                  if Present (Prev) then
                     Set_Homonym (Prev, E);
                  end if;
               end if;

               --  We must also set the next homonym entity of the real entity
               --  to handle the case in which the next homonym was a shadow
               --  entity.

               Set_Homonym (E, Homonym (Lim_Typ));
            end if;

            Next_Entity (Lim_Typ);
         end loop;
      end if;

      --  Indicate that the limited view of the package is not installed

      Set_From_With_Type         (P, False);
      Set_Limited_View_Installed (N, False);
   end Remove_Limited_With_Clause;

   --------------------
   -- Remove_Parents --
   --------------------

   procedure Remove_Parents (Lib_Unit : Node_Id) is
      P      : Node_Id;
      P_Name : Entity_Id;
      P_Spec : Node_Id := Empty;
      E      : Entity_Id;
      Vis    : constant Boolean :=
                 Scope_Stack.Table (Scope_Stack.Last).Previous_Visibility;

   begin
      if Is_Child_Spec (Lib_Unit) then
         P_Spec := Parent_Spec (Lib_Unit);

      elsif Nkind (Lib_Unit) = N_Package_Body
        and then Nkind (Original_Node (Lib_Unit)) = N_Package_Instantiation
      then
         P_Spec := Parent_Spec (Original_Node (Lib_Unit));
      end if;

      if Present (P_Spec) then

         P := Unit (P_Spec);
         P_Name := Get_Parent_Entity (P);
         Remove_Context_Clauses (P_Spec);
         End_Package_Scope (P_Name);
         Set_Is_Immediately_Visible (P_Name, Vis);

         --  Remove from visibility the siblings as well, which are directly
         --  visible while the parent is in scope.

         E := First_Entity (P_Name);
         while Present (E) loop
            if Is_Child_Unit (E) then
               Set_Is_Immediately_Visible (E, False);
            end if;

            Next_Entity (E);
         end loop;

         Set_In_Package_Body (P_Name, False);

         --  This is the recursive call to remove the context of any
         --  higher level parent. This recursion ensures that all parents
         --  are removed in the reverse order of their installation.

         Remove_Parents (P);
      end if;
   end Remove_Parents;

   ---------------------------------
   -- Remove_Private_With_Clauses --
   ---------------------------------

   procedure Remove_Private_With_Clauses (Comp_Unit : Node_Id) is
      Item : Node_Id;

      function In_Regular_With_Clause (E : Entity_Id) return Boolean;
      --  Check whether a given unit appears in a regular with_clause.
      --  Used to determine whether a private_with_clause, implicit or
      --  explicit, should be ignored.

      ----------------------------
      -- In_Regular_With_Clause --
      ----------------------------

      function In_Regular_With_Clause (E : Entity_Id) return Boolean
      is
         Item : Node_Id;

      begin
         Item := First (Context_Items (Comp_Unit));
         while Present (Item) loop
            if Nkind (Item) = N_With_Clause
              and then Entity (Name (Item)) = E
              and then not Private_Present (Item)
            then
               return True;
            end if;
            Next (Item);
         end loop;

         return False;
      end In_Regular_With_Clause;

   --  Start of processing for Remove_Private_With_Clauses

   begin
      Item := First (Context_Items (Comp_Unit));
      while Present (Item) loop
         if Nkind (Item) = N_With_Clause
           and then Private_Present (Item)
         then

            --  If private_with_clause is redundant, remove it from
            --  context, as a small optimization to subsequent handling
            --  of private_with clauses in other nested packages..

            if In_Regular_With_Clause (Entity (Name (Item))) then
               declare
                  Nxt : constant Node_Id := Next (Item);

               begin
                  Remove (Item);
                  Item := Nxt;
               end;

            elsif Limited_Present (Item) then
               if not Limited_View_Installed (Item) then
                  Remove_Limited_With_Clause (Item);
               end if;

               Next (Item);

            else
               Remove_Unit_From_Visibility (Entity (Name (Item)));
               Set_Context_Installed (Item, False);
               Next (Item);
            end if;

         else
            Next (Item);
         end if;
      end loop;
   end Remove_Private_With_Clauses;

   ---------------------------------
   -- Remove_Unit_From_Visibility --
   ---------------------------------

   procedure Remove_Unit_From_Visibility (Unit_Name : Entity_Id) is
      P : constant Entity_Id := Scope (Unit_Name);

   begin

      if Debug_Flag_I then
         Write_Str ("remove unit ");
         Write_Name (Chars (Unit_Name));
         Write_Str (" from visibility");
         Write_Eol;
      end if;

      if P /= Standard_Standard then
         Set_Is_Visible_Child_Unit (Unit_Name, False);
      end if;

      Set_Is_Potentially_Use_Visible (Unit_Name, False);
      Set_Is_Immediately_Visible     (Unit_Name, False);
   end Remove_Unit_From_Visibility;

   --------
   -- sm --
   --------

   procedure sm is
   begin
      null;
   end sm;

   -------------
   -- Unchain --
   -------------

   procedure Unchain (E : Entity_Id) is
      Prev : Entity_Id;

   begin
      Prev := Current_Entity (E);

      if No (Prev) then
         return;

      elsif Prev = E then
         Set_Name_Entity_Id (Chars (E), Homonym (E));

      else
         while Present (Prev)
           and then Homonym (Prev) /= E
         loop
            Prev := Homonym (Prev);
         end loop;

         if Present (Prev) then
            Set_Homonym (Prev, Homonym (E));
         end if;
      end if;

      if Debug_Flag_I then
         Write_Str ("   (homonym) unchain ");
         Write_Name (Chars (E));
         Write_Eol;
      end if;
   end Unchain;

end Sem_Ch10;
