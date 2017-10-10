------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ C H 1 0                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2017, Free Software Foundation, Inc.         --
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

with Aspects;   use Aspects;
with Atree;     use Atree;
with Contracts; use Contracts;
with Debug;     use Debug;
with Einfo;     use Einfo;
with Errout;    use Errout;
with Exp_Util;  use Exp_Util;
with Elists;    use Elists;
with Fname;     use Fname;
with Fname.UF;  use Fname.UF;
with Freeze;    use Freeze;
with Ghost;     use Ghost;
with Impunit;   use Impunit;
with Inline;    use Inline;
with Lib;       use Lib;
with Lib.Load;  use Lib.Load;
with Lib.Xref;  use Lib.Xref;
with Namet;     use Namet;
with Nlists;    use Nlists;
with Nmake;     use Nmake;
with Opt;       use Opt;
with Output;    use Output;
with Par_SCO;   use Par_SCO;
with Restrict;  use Restrict;
with Rident;    use Rident;
with Rtsfind;   use Rtsfind;
with Sem;       use Sem;
with Sem_Aux;   use Sem_Aux;
with Sem_Ch3;   use Sem_Ch3;
with Sem_Ch6;   use Sem_Ch6;
with Sem_Ch7;   use Sem_Ch7;
with Sem_Ch8;   use Sem_Ch8;
with Sem_Dist;  use Sem_Dist;
with Sem_Prag;  use Sem_Prag;
with Sem_Util;  use Sem_Util;
with Sem_Warn;  use Sem_Warn;
with Stand;     use Stand;
with Sinfo;     use Sinfo;
with Sinfo.CN;  use Sinfo.CN;
with Sinput;    use Sinput;
with Snames;    use Snames;
with Style;     use Style;
with Stylesw;   use Stylesw;
with Tbuild;    use Tbuild;
with Uname;     use Uname;

package body Sem_Ch10 is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Analyze_Context (N : Node_Id);
   --  Analyzes items in the context clause of compilation unit

   procedure Build_Limited_Views (N : Node_Id);
   --  Build and decorate the list of shadow entities for a package mentioned
   --  in a limited_with clause. If the package was not previously analyzed
   --  then it also performs a basic decoration of the real entities. This is
   --  required in order to avoid passing non-decorated entities to the
   --  back-end. Implements Ada 2005 (AI-50217).

   procedure Analyze_Proper_Body (N : Node_Id; Nam : Entity_Id);
   --  Common processing for all stubs (subprograms, tasks, packages, and
   --  protected cases). N is the stub to be analyzed. Once the subunit name
   --  is established, load and analyze. Nam is the non-overloadable entity
   --  for which the proper body provides a completion. Subprogram stubs are
   --  handled differently because they can be declarations.

   procedure Check_Body_Needed_For_SAL (Unit_Name : Entity_Id);
   --  Check whether the source for the body of a compilation unit must be
   --  included in a standalone library.

   procedure Check_No_Elab_Code_All (N : Node_Id);
   --  Carries out possible tests for violation of No_Elab_Code all for withed
   --  units in the Context_Items of unit N.

   procedure Check_Private_Child_Unit (N : Node_Id);
   --  If a with_clause mentions a private child unit, the compilation unit
   --  must be a member of the same family, as described in 10.1.2.

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

   procedure Generate_Parent_References (N : Node_Id; P_Id : Entity_Id);
   --  Generate cross-reference information for the parents of child units
   --  and of subunits. N is a defining_program_unit_name, and P_Id is the
   --  immediate parent scope.

   function Has_With_Clause
     (C_Unit     : Node_Id;
      Pack       : Entity_Id;
      Is_Limited : Boolean := False) return Boolean;
   --  Determine whether compilation unit C_Unit contains a [limited] with
   --  clause for package Pack. Use the flag Is_Limited to designate desired
   --  clause kind.

   procedure Implicit_With_On_Parent (Child_Unit : Node_Id; N : Node_Id);
   --  If the main unit is a child unit, implicit withs are also added for
   --  all its ancestors.

   function In_Chain (E : Entity_Id) return Boolean;
   --  Check that the shadow entity is not already in the homonym chain, for
   --  example through a limited_with clause in a parent unit.

   procedure Install_Context_Clauses (N : Node_Id; Chain : Boolean := True);
   --  Subsidiary to Install_Context and Install_Parents. Process all with
   --  and use clauses for current unit and its library unit if any. The flag
   --  Chain is used to control the "chaining" or linking together of use-type
   --  and use-package clauses to avoid circularities with reinstalling
   --  clauses.

   procedure Install_Limited_Context_Clauses (N : Node_Id);
   --  Subsidiary to Install_Context. Process only limited with_clauses for
   --  current unit. Implements Ada 2005 (AI-50217).

   procedure Install_Limited_Withed_Unit (N : Node_Id);
   --  Place shadow entities for a limited_with package in the visibility
   --  structures for the current compilation. Implements Ada 2005 (AI-50217).

   procedure Install_Withed_Unit
     (With_Clause     : Node_Id;
      Private_With_OK : Boolean := False);
   --  If the unit is not a child unit, make unit immediately visible. The
   --  caller ensures that the unit is not already currently installed. The
   --  flag Private_With_OK is set true in Install_Private_With_Clauses, which
   --  is called when compiling the private part of a package, or installing
   --  the private declarations of a parent unit.

   procedure Install_Parents
     (Lib_Unit   : Node_Id;
      Is_Private : Boolean;
      Chain      : Boolean := True);
   --  This procedure establishes the context for the compilation of a child
   --  unit. If Lib_Unit is a child library spec then the context of the parent
   --  is installed, and the parent itself made immediately visible, so that
   --  the child unit is processed in the declarative region of the parent.
   --  Install_Parents makes a recursive call to itself to ensure that all
   --  parents are loaded in the nested case. If Lib_Unit is a library body,
   --  the only effect of Install_Parents is to install the private decls of
   --  the parents, because the visible parent declarations will have been
   --  installed as part of the context of the corresponding spec. The flag
   --  Chain is used to control the "chaining" or linking of use-type and
   --  use-package clauses to avoid circularities when installing context.

   procedure Install_Siblings (U_Name : Entity_Id; N : Node_Id);
   --  In the compilation of a child unit, a child of any of the  ancestor
   --  units is directly visible if it is visible, because the parent is in
   --  an enclosing scope. Iterate over context to find child units of U_Name
   --  or of some ancestor of it.

   function Is_Ancestor_Unit (U1 : Node_Id; U2 : Node_Id) return Boolean;
   --  When compiling a unit Q descended from some parent unit P, a limited
   --  with_clause in the context of P that names some other ancestor of Q
   --  must not be installed because the ancestor is immediately visible.

   function Is_Child_Spec (Lib_Unit : Node_Id) return Boolean;
   --  Lib_Unit is a library unit which may be a spec or a body. Is_Child_Spec
   --  returns True if Lib_Unit is a library spec which is a child spec, i.e.
   --  a library spec that has a parent. If the call to Is_Child_Spec returns
   --  True, then Parent_Spec (Lib_Unit) is non-Empty and points to the
   --  compilation unit for the parent spec.
   --
   --  Lib_Unit can also be a subprogram body that acts as its own spec. If the
   --  Parent_Spec is non-empty, this is also a child unit.

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
   --  Reset all visibility flags on unit after compiling it, either as a main
   --  unit or as a unit in the context.

   procedure Unchain (E : Entity_Id);
   --  Remove single entity from visibility list

   procedure sm;
   --  A dummy procedure, for debugging use, called just before analyzing the
   --  main unit (after dealing with any context clauses).

   --------------------------
   -- Limited_With_Clauses --
   --------------------------

   --  Limited_With clauses are the mechanism chosen for Ada 2005 to support
   --  mutually recursive types declared in different units. A limited_with
   --  clause that names package P in the context of unit U makes the types
   --  declared in the visible part of P available within U, but with the
   --  restriction that these types can only be used as incomplete types.
   --  The limited_with clause does not impose a semantic dependence on P,
   --  and it is possible for two packages to have limited_with_clauses on
   --  each other without creating an elaboration circularity.

   --  To support this feature, the analysis of a limited_with clause must
   --  create an abbreviated view of the package, without performing any
   --  semantic analysis on it. This "package abstract" contains shadow types
   --  that are in one-one correspondence with the real types in the package,
   --  and that have the properties of incomplete types.

   --  The implementation creates two element lists: one to chain the shadow
   --  entities, and one to chain the corresponding type entities in the tree
   --  of the package. Links between corresponding entities in both chains
   --  allow the compiler to select the proper view of a given type, depending
   --  on the context. Note that in contrast with the handling of private
   --  types, the limited view and the non-limited view of a type are treated
   --  as separate entities, and no entity exchange needs to take place, which
   --  makes the implementation much simpler than could be feared.

   ------------------------------
   -- Analyze_Compilation_Unit --
   ------------------------------

   procedure Analyze_Compilation_Unit (N : Node_Id) is
      procedure Check_Redundant_Withs
        (Context_Items      : List_Id;
         Spec_Context_Items : List_Id := No_List);
      --  Determine whether the context list of a compilation unit contains
      --  redundant with clauses. When checking body clauses against spec
      --  clauses, set Context_Items to the context list of the body and
      --  Spec_Context_Items to that of the spec. Parent packages are not
      --  examined for documentation purposes.

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
           Used              : out Boolean;
           Used_Type_Or_Elab : out Boolean);
         --  Examine the context clauses of a package body, trying to match the
         --  name entity of Clause with any list element. If the match occurs
         --  on a use package clause set Used to True, for a use type clause or
         --  pragma Elaborate[_All], set Used_Type_Or_Elab to True.

         procedure Process_Spec_Clauses
          (Context_List : List_Id;
           Clause       : Node_Id;
           Used         : out Boolean;
           Withed       : out Boolean;
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
           Used              : out Boolean;
           Used_Type_Or_Elab : out Boolean)
         is
            Nam_Ent   : constant Entity_Id := Entity (Name (Clause));
            Cont_Item : Node_Id;
            Prag_Unit : Node_Id;
            Subt_Mark : Node_Id;
            Use_Item  : Node_Id;

            function Same_Unit (N : Node_Id; P : Entity_Id) return Boolean;
            --  In an expanded name in a use clause, if the prefix is a renamed
            --  package, the entity is set to the original package as a result,
            --  when checking whether the package appears in a previous with
            --  clause, the renaming has to be taken into account, to prevent
            --  spurious/incorrect warnings. A common case is use of Text_IO.

            ---------------
            -- Same_Unit --
            ---------------

            function Same_Unit (N : Node_Id; P : Entity_Id) return Boolean is
            begin
               return Entity (N) = P
                 or else (Present (Renamed_Object (P))
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

                  Use_Item := Name (Cont_Item);

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

               --  USE TYPE clause

               elsif Nkind (Cont_Item) = N_Use_Type_Clause
                 and then not Used_Type_Or_Elab
               then
                  Subt_Mark := Subtype_Mark (Cont_Item);
                  if not Used_Type_Or_Elab
                    and then Same_Unit (Prefix (Subt_Mark), Nam_Ent)
                  then
                     Used_Type_Or_Elab := True;
                  end if;

               --  Pragma Elaborate or Elaborate_All

               elsif Nkind (Cont_Item) = N_Pragma
                 and then
                   Nam_In (Pragma_Name_Unmapped (Cont_Item),
                           Name_Elaborate, Name_Elaborate_All)
                 and then not Used_Type_Or_Elab
               then
                  Prag_Unit :=
                    First (Pragma_Argument_Associations (Cont_Item));
                  while Present (Prag_Unit) and then not Used_Type_Or_Elab loop
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
           Used         : out Boolean;
           Withed       : out Boolean;
           Exit_On_Self : Boolean := False)
         is
            Nam_Ent   : constant Entity_Id := Entity (Name (Clause));
            Cont_Item : Node_Id;

         begin
            Used := False;
            Withed := False;

            Cont_Item := First (Context_List);
            while Present (Cont_Item) loop

               --  Stop the search since the context items after Cont_Item have
               --  already been examined in a previous iteration of the reverse
               --  loop in Check_Redundant_Withs.

               if Exit_On_Self
                 and Cont_Item = Clause
               then
                  exit;
               end if;

               --  Package use clause

               if Nkind (Cont_Item) = N_Use_Package_Clause
                 and then not Used
               then
                  if Entity (Name (Cont_Item)) = Nam_Ent then
                     Used := True;
                  end if;

               --  Package with clause. Avoid processing self, implicitly
               --  generated with clauses or limited with clauses. Note that
               --  we examine with clauses having pragmas Elaborate or
               --  Elaborate_All applied to them due to cases such as:

               --     with Pack;
               --     with Pack;
               --     pragma Elaborate (Pack);

               --  In this case, the second with clause is redundant since
               --  the pragma applies only to the first "with Pack;".

               --  Note that we only consider with_clauses that comes from
               --  source. In the case of renamings used as prefixes of names
               --  in with_clauses, we generate a with_clause for the prefix,
               --  which we do not treat as implicit because it is needed for
               --  visibility analysis, but is also not redundant.

               elsif Nkind (Cont_Item) = N_With_Clause
                 and then not Implicit_With (Cont_Item)
                 and then Comes_From_Source (Cont_Item)
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

            --  Avoid checking implicitly generated with clauses, limited with
            --  clauses or withs that have pragma Elaborate or Elaborate_All.

            if Nkind (Clause) = N_With_Clause
              and then not Implicit_With (Clause)
              and then not Limited_Present (Clause)
              and then not Elaborate_Present (Clause)

              --  With_clauses introduced for renamings of parent clauses
              --  are not marked implicit because they need to be properly
              --  installed, but they do not come from source and do not
              --  require warnings.

              and then Comes_From_Source (Clause)
            then
               --  Package body-to-spec check

               if Present (Spec_Context_Items) then
                  declare
                     Used_In_Body      : Boolean;
                     Used_In_Spec      : Boolean;
                     Used_Type_Or_Elab : Boolean;
                     Withed_In_Spec    : Boolean;

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
                               ((not Used_In_Spec and then not Used_In_Body)
                                  or else Used_In_Spec)
                     then
                        Error_Msg_N -- CODEFIX
                          ("redundant with clause in body?r?", Clause);
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
                        Error_Msg_N -- CODEFIX
                          ("redundant with clause?r?", Clause);
                     end if;
                  end;
               end if;
            end if;

            Prev (Clause);
         end loop;
      end Check_Redundant_Withs;

      --  Local variables

      Main_Cunit    : constant Node_Id := Cunit (Main_Unit);
      Unit_Node     : constant Node_Id := Unit (N);
      Lib_Unit      : Node_Id          := Library_Unit (N);
      Par_Spec_Name : Unit_Name_Type;
      Spec_Id       : Entity_Id;
      Unum          : Unit_Number_Type;

   --  Start of processing for Analyze_Compilation_Unit

   begin
      Process_Compilation_Unit_Pragmas (N);

      --  If the unit is a subunit whose parent has not been analyzed (which
      --  indicates that the main unit is a subunit, either the current one or
      --  one of its descendants) then the subunit is compiled as part of the
      --  analysis of the parent, which we proceed to do. Basically this gets
      --  handled from the top down and we don't want to do anything at this
      --  level (i.e. this subunit will be handled on the way down from the
      --  parent), so at this level we immediately return. If the subunit ends
      --  up not analyzed, it means that the parent did not contain a stub for
      --  it, or that there errors were detected in some ancestor.

      if Nkind (Unit_Node) = N_Subunit and then not Analyzed (Lib_Unit) then
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

      --  Analyze context (this will call Sem recursively for with'ed units) To
      --  detect circularities among with-clauses that are not caught during
      --  loading, we set the Context_Pending flag on the current unit. If the
      --  flag is already set there is a potential circularity. We exclude
      --  predefined units from this check because they are known to be safe.
      --  We also exclude package bodies that are present because circularities
      --  between bodies are harmless (and necessary).

      if Context_Pending (N) then
         declare
            Circularity : Boolean := True;

         begin
            if In_Predefined_Unit (N) then
               Circularity := False;

            else
               for U in Main_Unit + 1 .. Last_Unit loop
                  if Nkind (Unit (Cunit (U))) = N_Package_Body
                    and then not Analyzed (Cunit (U))
                  then
                     Circularity := False;
                     exit;
                  end if;
               end loop;
            end if;

            if Circularity then
               Error_Msg_N ("circular dependency caused by with_clauses", N);
               Error_Msg_N
                 ("\possibly missing limited_with clause"
                  & " in one of the following", N);

               for U in Main_Unit .. Last_Unit loop
                  if Context_Pending (Cunit (U)) then
                     Error_Msg_Unit_1 := Get_Unit_Name (Unit (Cunit (U)));
                     Error_Msg_N ("\unit$", N);
                  end if;
               end loop;

               raise Unrecoverable_Error;
            end if;
         end;
      else
         Set_Context_Pending (N);
      end if;

      Analyze_Context (N);

      Set_Context_Pending (N, False);

      --  If the unit is a package body, the spec is already loaded and must be
      --  analyzed first, before we analyze the body.

      if Nkind (Unit_Node) = N_Package_Body then

         --  If no Lib_Unit, then there was a serious previous error, so just
         --  ignore the entire analysis effort.

         if No (Lib_Unit) then
            Check_Error_Detected;
            return;

         else
            --  Analyze the package spec

            Semantics (Lib_Unit);

            --  Check for unused with's

            Check_Unused_Withs (Get_Cunit_Unit_Number (Lib_Unit));

            --  Verify that the library unit is a package declaration

            if not Nkind_In (Unit (Lib_Unit), N_Package_Declaration,
                                              N_Generic_Package_Declaration)
            then
               Error_Msg_N
                 ("no legal package declaration for package body", N);
               return;

            --  Otherwise, the entity in the declaration is visible. Update the
            --  version to reflect dependence of this body on the spec.

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
      --  here, this analysis is done mostly for error checking and consistency
      --  purposes (but not only, e.g. there could be a contract on the spec),
      --  so there's nothing else to be done.

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
                  --  in its scope. Finally we create a Units table entry for
                  --  the subprogram declaration, to maintain a one-to-one
                  --  correspondence with compilation unit nodes. This is
                  --  critical for the tree traversals performed by CodePeer.

                  declare
                     Loc : constant Source_Ptr := Sloc (N);
                     SCS : constant Boolean :=
                             Get_Comes_From_Source_Default;

                  begin
                     Set_Comes_From_Source_Default (False);

                     --  Note: We copy the Context_Items from the explicit body
                     --  to the implicit spec, setting the former to Empty_List
                     --  to preserve the treeish nature of the tree, during
                     --  analysis of the spec. Then we put it back the way it
                     --  was -- copy the Context_Items from the spec to the
                     --  body, and set the spec Context_Items to Empty_List.
                     --  It is necessary to preserve the treeish nature,
                     --  because otherwise we will call End_Use_* twice on the
                     --  same thing.

                     Lib_Unit :=
                       Make_Compilation_Unit (Loc,
                         Context_Items => Context_Items (N),
                         Unit =>
                           Make_Subprogram_Declaration (Sloc (N),
                             Specification =>
                               Copy_Separate_Tree
                                 (Specification (Unit_Node))),
                         Aux_Decls_Node =>
                           Make_Compilation_Unit_Aux (Loc));

                     Set_Context_Items (N, Empty_List);
                     Set_Library_Unit (N, Lib_Unit);
                     Set_Parent_Spec (Unit (Lib_Unit), Cunit (Unum));
                     Make_Child_Decl_Unit (N);
                     Semantics (Lib_Unit);

                     --  Now that a separate declaration exists, the body
                     --  of the child unit does not act as spec any longer.

                     Set_Acts_As_Spec (N, False);
                     Set_Is_Child_Unit (Defining_Entity (Unit_Node));
                     Set_Debug_Info_Needed (Defining_Entity (Unit (Lib_Unit)));
                     Set_Comes_From_Source_Default (SCS);

                     --  Restore Context_Items to the body

                     Set_Context_Items (N, Context_Items (Lib_Unit));
                     Set_Context_Items (Lib_Unit, Empty_List);
                  end;
               end if;
            end if;

         --  Here for subprogram with separate declaration

         else
            Semantics (Lib_Unit);
            Check_Unused_Withs (Get_Cunit_Unit_Number (Lib_Unit));
            Version_Update (N, Lib_Unit);
         end if;

         --  If this is a child unit, generate references to the parents

         if Nkind (Defining_Unit_Name (Specification (Unit_Node))) =
                                             N_Defining_Program_Unit_Name
         then
            Generate_Parent_References
              (Specification (Unit_Node),
               Scope (Defining_Entity (Unit (Lib_Unit))));
         end if;
      end if;

      --  If it is a child unit, the parent must be elaborated first and we
      --  update version, since we are dependent on our parent.

      if Is_Child_Spec (Unit_Node) then

         --  The analysis of the parent is done with style checks off

         declare
            Save_Style_Check : constant Boolean := Style_Check;

         begin
            if not GNAT_Mode then
               Style_Check := False;
            end if;

            Semantics (Parent_Spec (Unit_Node));
            Version_Update (N, Parent_Spec (Unit_Node));

            --  Restore style check settings

            Style_Check := Save_Style_Check;
         end;
      end if;

      --  With the analysis done, install the context. Note that we can't
      --  install the context from the with clauses as we analyze them, because
      --  each with clause must be analyzed in a clean visibility context, so
      --  we have to wait and install them all at once.

      Install_Context (N);

      if Is_Child_Spec (Unit_Node) then

         --  Set the entities of all parents in the program_unit_name

         Generate_Parent_References
           (Unit_Node, Get_Parent_Entity (Unit (Parent_Spec (Unit_Node))));
      end if;

      --  All components of the context: with-clauses, library unit, ancestors
      --  if any, (and their context) are analyzed and installed.

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

      if In_Predefined_Unit (N) then
         Set_RTU_Loaded (Unit_Node);
      end if;

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

      --  Analyze the contract of a [generic] subprogram that acts as a
      --  compilation unit after all compilation pragmas have been analyzed.

      if Nkind_In (Unit_Node, N_Generic_Subprogram_Declaration,
                              N_Subprogram_Declaration)
      then
         Analyze_Entry_Or_Subprogram_Contract (Defining_Entity (Unit_Node));
      end if;

      --  Generate distribution stubs if requested and no error

      if N = Main_Cunit
        and then (Distribution_Stub_Mode = Generate_Receiver_Stub_Body
                    or else
                  Distribution_Stub_Mode = Generate_Caller_Stub_Body)
        and then Fatal_Error (Main_Unit) /= Error_Detected
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

      --  Remove unit from visibility, so that environment is clean for the
      --  next compilation, which is either the main unit or some other unit
      --  in the context.

      if Nkind_In (Unit_Node, N_Package_Declaration,
                              N_Package_Renaming_Declaration,
                              N_Subprogram_Declaration)
        or else Nkind (Unit_Node) in N_Generic_Declaration
        or else (Nkind (Unit_Node) = N_Subprogram_Body
                  and then Acts_As_Spec (Unit_Node))
      then
         Remove_Unit_From_Visibility (Defining_Entity (Unit_Node));

      --  If the unit is an instantiation whose body will be elaborated for
      --  inlining purposes, use the proper entity of the instance. The entity
      --  may be missing if the instantiation was illegal.

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

      --  When generating code for a non-generic main unit, check that withed
      --  generic units have a body if they need it, even if the units have not
      --  been instantiated. Force the load of the bodies to produce the proper
      --  error if the body is absent. The same applies to GNATprove mode, with
      --  the added benefit of capturing global references within the generic.
      --  This in turn allows for proper inlining of subprogram bodies without
      --  a previous declaration.

      if Get_Cunit_Unit_Number (N) = Main_Unit
        and then ((Operating_Mode = Generate_Code and then Expander_Active)
                     or else
                  (Operating_Mode = Check_Semantics and then GNATprove_Mode))
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

                  --  Compile the generic subprogram, unless it is intrinsic or
                  --  imported so no body is required, or generic package body
                  --  if the package spec requires a body.

                  if (Is_Generic_Subprogram (Nam)
                       and then not Is_Intrinsic_Subprogram (Nam)
                       and then not Is_Imported (Nam))
                    or else (Ekind (Nam) = E_Generic_Package
                              and then Unit_Requires_Body (Nam))
                  then
                     Style_Check := False;

                     if Present (Renamed_Object (Nam)) then
                        Un :=
                          Load_Unit
                            (Load_Name  =>
                               Get_Body_Name
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
                            (Load_Name  =>
                               Get_Body_Name (Get_Unit_Name (Item)),
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
                       and then Fatal_Error (Un) /= Error_Detected
                     then
                        Style_Check := False;
                        Semantics (Cunit (Un));
                     end if;
                  end if;
               end if;

               Next (Item);
            end loop;

            --  Restore style checks settings

            Style_Check := Save_Style_Check;
         end;

         --  In GNATprove mode, force the loading of an Interrupt_Priority when
         --  processing compilation units with potentially "main" subprograms.
         --  This is required for the ceiling priority protocol checks, which
         --  are triggered by these subprograms.

         if GNATprove_Mode
           and then Nkind_In (Unit_Node, N_Function_Instantiation,
                                         N_Procedure_Instantiation,
                                         N_Subprogram_Body)
         then
            declare
               Spec : Node_Id;

            begin
               case Nkind (Unit_Node) is
                  when N_Subprogram_Body =>
                     Spec := Specification (Unit_Node);

                  when N_Subprogram_Instantiation =>
                     Spec :=
                       Subprogram_Specification (Entity (Name (Unit_Node)));

                  when others =>
                     raise Program_Error;
               end case;

               pragma Assert (Nkind (Spec) in N_Subprogram_Specification);

               --  Main subprogram must have no parameters, and if it is a
               --  function, it must return an integer.

               if No (Parameter_Specifications (Spec))
                 and then (Nkind (Spec) = N_Procedure_Specification
                             or else
                           Is_Integer_Type (Etype (Result_Definition (Spec))))
               then
                  SPARK_Implicit_Load (RE_Interrupt_Priority);
               end if;
            end;
         end if;
      end if;

      --  Deal with creating elaboration counter if needed. We create an
      --  elaboration counter only for units that come from source since
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

            --  They are also needed to ensure explicit visibility from the
            --  binder generated code of all the units involved in a partition
            --  when control-flow preservation is requested.

            --  Case of units which do not require an elaboration entity

            if not Opt.Suppress_Control_Flow_Optimizations
              and then
              ( --  Pure units do not need checks

                Is_Pure (Spec_Id)

                --  Preelaborated units do not need checks

                or else Is_Preelaborated (Spec_Id)

                --  No checks needed if pragma Elaborate_Body present

                or else Has_Pragma_Elaborate_Body (Spec_Id)

                --  No checks needed if unit does not require a body

                or else not Unit_Requires_Body (Spec_Id)

                --  No checks needed for predefined files

                or else Is_Predefined_Unit (Unum)

                --  No checks required if no separate spec

                or else Acts_As_Spec (N)
              )
            then
               --  This is a case where we only need the entity for
               --  checking to prevent multiple elaboration checks.

               Set_Elaboration_Entity_Required (Spec_Id, False);

            --  Case of elaboration entity is required for access before
            --  elaboration checking (so certainly we must build it).

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
         L : constant List_Id :=
               Freeze_Entity (Cunit_Entity (Current_Sem_Unit), N);
      begin
         while Is_Non_Empty_List (L) loop
            Insert_Library_Level_Action (Remove_Head (L));
         end loop;
      end;

      Set_Analyzed (N);

      --  Call Check_Package_Body so that a body containing subprograms with
      --  Inline_Always can be made available for front end inlining.

      if Nkind (Unit_Node) = N_Package_Declaration
        and then Get_Cunit_Unit_Number (N) /= Main_Unit

        --  We don't need to do this if the Expander is not active, since there
        --  is no code to inline.

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

            Check_Package_Body_For_Inlining (N, Defining_Entity (Unit_Node));

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

      if not GNAT_Mode
        and then Warn_On_Obsolescent_Feature
        and then Nkind (Unit_Node) not in N_Generic_Instantiation
      then
         --  Push current compilation unit as scope, so that the test for
         --  being within an obsolescent unit will work correctly. The check
         --  is not performed within an instantiation, because the warning
         --  will have been emitted in the corresponding generic unit.

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

      --  If No_Elaboration_Code_All was encountered, this is where we do the
      --  transitive test of with'ed units to make sure they have the aspect.
      --  This is delayed till the end of analyzing the compilation unit to
      --  ensure that the pragma/aspect, if present, has been analyzed.

      Check_No_Elab_Code_All (N);
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
        and then Pragma_Name (Item) in Configuration_Pragma_Names
      loop
         Analyze (Item);
         Next (Item);
      end loop;

      --  This is the point at which we capture the configuration settings
      --  for the unit. At the moment only the Optimize_Alignment setting
      --  needs to be captured. Probably more later ???

      if Optimize_Alignment_Local then
         Set_OA_Setting (Current_Sem_Unit, 'L');
      else
         Set_OA_Setting (Current_Sem_Unit, Optimize_Alignment);
      end if;

      --  Loop through actual context items. This is done in two passes:

      --  a) The first pass analyzes non-limited with-clauses and also any
      --     configuration pragmas (we need to get the latter analyzed right
      --     away, since they can affect processing of subsequent items).

      --  b) The second pass analyzes limited_with clauses (Ada 2005: AI-50217)

      while Present (Item) loop

         --  For with clause, analyze the with clause, and then update the
         --  version, since we are dependent on a unit that we with.

         if Nkind (Item) = N_With_Clause
           and then not Limited_Present (Item)
         then
            --  Skip analyzing with clause if no unit, nothing to do (this
            --  happens for a with that references a non-existent unit).

            if Present (Library_Unit (Item)) then

               --  Skip analyzing with clause if this is a with_clause for
               --  the main unit, which happens if a subunit has a useless
               --  with_clause on its parent.

               if Library_Unit (Item) /= Cunit (Current_Sem_Unit) then
                  Analyze (Item);

               --  Here for the case of a useless with for the main unit

               else
                  Set_Entity (Name (Item), Cunit_Entity (Current_Sem_Unit));
               end if;
            end if;

            --  Do version update (skipped for implicit with)

            if not Implicit_With (Item) then
               Version_Update (N, Library_Unit (Item));
            end if;

         --  Skip pragmas. Configuration pragmas at the start were handled in
         --  the loop above, and remaining pragmas are not processed until we
         --  actually install the context (see Install_Context). We delay the
         --  analysis of these pragmas to make sure that we have installed all
         --  the implicit with's on parent units.

         --  Skip use clauses at this stage, since we don't want to do any
         --  installing of potentially use-visible entities until we
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

               --  Verify that the illegal contexts given in 10.1.2 (18/2) are
               --  properly rejected, including renaming declarations.

               if not Nkind_In (Ukind, N_Package_Declaration,
                                       N_Subprogram_Declaration)
                 and then Ukind not in N_Generic_Declaration
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
                           Error_Msg_N ("limited with_clause cannot "
                                        & "name ancestor", Item);
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
                           --  in the limited-with clause.

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
      Opts : Config_Switches_Type;

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
         --  Retain and restore the configuration options of the enclosing
         --  context as the proper body may introduce a set of its own.

         Save_Opt_Config_Switches (Opts);

         --  Indicate that the body of the package exists. If we are doing
         --  only semantic analysis, the stub stands for the body. If we are
         --  generating code, the existence of the body will be confirmed
         --  when we load the proper body.

         Set_Has_Completion (Nam);
         Set_Scope (Defining_Entity (N), Current_Scope);
         Set_Ekind (Defining_Entity (N), E_Package_Body);
         Set_Corresponding_Spec_Of_Stub (N, Nam);
         Generate_Reference (Nam, Id, 'b');
         Analyze_Proper_Body (N, Nam);

         Restore_Opt_Config_Switches (Opts);
      end if;
   end Analyze_Package_Body_Stub;

   -------------------------
   -- Analyze_Proper_Body --
   -------------------------

   procedure Analyze_Proper_Body (N : Node_Id; Nam : Entity_Id) is
      Subunit_Name : constant Unit_Name_Type := Get_Unit_Name (N);

      procedure Optional_Subunit;
      --  This procedure is called when the main unit is a stub, or when we
      --  are not generating code. In such a case, we analyze the subunit if
      --  present, which is user-friendly and in fact required for ASIS, but we
      --  don't complain if the subunit is missing. In GNATprove_Mode, we issue
      --  an error to avoid formal verification of a partial unit.

      ----------------------
      -- Optional_Subunit --
      ----------------------

      procedure Optional_Subunit is
         Comp_Unit : Node_Id;
         Unum      : Unit_Number_Type;

      begin
         --  Try to load subunit, but ignore any errors that occur during the
         --  loading of the subunit, by using the special feature in Errout to
         --  ignore all errors. Note that Fatal_Error will still be set, so we
         --  will be able to check for this case below.

         if not (ASIS_Mode or GNATprove_Mode) then
            Ignore_Errors_Enable := Ignore_Errors_Enable + 1;
         end if;

         Unum :=
           Load_Unit
             (Load_Name  => Subunit_Name,
              Required   => GNATprove_Mode,
              Subunit    => True,
              Error_Node => N);

         if not (ASIS_Mode or GNATprove_Mode) then
            Ignore_Errors_Enable := Ignore_Errors_Enable - 1;
         end if;

         --  All done if we successfully loaded the subunit

         if Unum /= No_Unit
           and then (Fatal_Error (Unum) /= Error_Detected
                      or else Try_Semantics)
         then
            Comp_Unit := Cunit (Unum);

            --  If the file was empty or seriously mangled, the unit itself may
            --  be missing.

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
               Set_Corresponding_Body (N, Defining_Entity (Unit (Comp_Unit)));
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

      --  Local variables

      Comp_Unit : Node_Id;
      Unum      : Unit_Number_Type;

   --  Start of processing for Analyze_Proper_Body

   begin
      --  If the subunit is already loaded, it means that the main unit is a
      --  subunit, and that the current unit is one of its parents which was
      --  being analyzed to provide the needed context for the analysis of the
      --  subunit. In this case we analyze the subunit and continue with the
      --  parent, without looking at subsequent subunits.

      if Is_Loaded (Subunit_Name) then

         --  If the proper body is already linked to the stub node, the stub is
         --  in a generic unit and just needs analyzing.

         if Present (Library_Unit (N)) then
            Set_Corresponding_Stub (Unit (Library_Unit (N)), N);

            --  If the subunit has severe errors, the spec of the enclosing
            --  body may not be available, in which case do not try analysis.

            if Serious_Errors_Detected > 0
              and then No (Library_Unit (Library_Unit (N)))
            then
               return;
            end if;

            --  Collect SCO information for loaded subunit if we are in the
            --  extended main unit.

            if Generate_SCO
              and then In_Extended_Main_Source_Unit
                         (Cunit_Entity (Current_Sem_Unit))
            then
               SCO_Record_Raw (Get_Cunit_Unit_Number (Library_Unit (N)));
            end if;

            Analyze_Subunit (Library_Unit (N));

         --  Otherwise we must load the subunit and link to it

         else
            --  Load the subunit, this must work, since we originally loaded
            --  the subunit earlier on. So this will not really load it, just
            --  give access to it.

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
      --  usage we want to annotate the stub properly. If the main unit is
      --  itself a subunit, another subunit is irrelevant unless it is a
      --  subunit of the current one, that is to say appears in the current
      --  source tree.

      elsif Nkind (Unit (Cunit (Main_Unit))) = N_Subunit
        and then Subunit_Name /= Unit_Name (Main_Unit)
      then
         if ASIS_Mode then
            declare
               PB : constant Node_Id := Proper_Body (Unit (Cunit (Main_Unit)));
            begin
               if Nkind_In (PB, N_Package_Body, N_Subprogram_Body)
                 and then List_Containing (N) = Declarations (PB)
               then
                  Optional_Subunit;
               end if;
            end;
         end if;

         --  But before we return, set the flag for unloaded subunits. This
         --  will suppress junk warnings of variables in the same declarative
         --  part (or a higher level one) that are in danger of looking unused
         --  when in fact there might be a declaration in the subunit that we
         --  do not intend to load.

         Unloaded_Subunits := True;
         return;

      --  If the subunit is not already loaded, and we are generating code,
      --  then this is the case where compilation started from the parent, and
      --  we are generating code for an entire subunit tree. In that case we
      --  definitely need to load the subunit.

      --  In order to continue the analysis with the rest of the parent,
      --  and other subunits, we load the unit without requiring its
      --  presence, and emit a warning if not found, rather than terminating
      --  the compilation abruptly, as for other missing file problems.

      elsif Original_Operating_Mode = Generate_Code then

         --  If the proper body is already linked to the stub node, the stub is
         --  in a generic unit and just needs analyzing.

         --  We update the version. Although we are not strictly technically
         --  semantically dependent on the subunit, given our approach of macro
         --  substitution of subunits, it makes sense to include it in the
         --  version identification.

         if Present (Library_Unit (N)) then
            Set_Corresponding_Stub (Unit (Library_Unit (N)), N);
            Analyze_Subunit (Library_Unit (N));
            Version_Update (Cunit (Main_Unit), Library_Unit (N));

         --  Otherwise we must load the subunit and link to it

         else
            --  Make sure that, if the subunit is preprocessed and -gnateG is
            --  specified, the preprocessed file will be written.

            Lib.Analysing_Subunit_Of_Main := True;
            Unum :=
              Load_Unit
                (Load_Name  => Subunit_Name,
                 Required   => False,
                 Subunit    => True,
                 Error_Node => N);
            Lib.Analysing_Subunit_Of_Main := False;

            --  Give message if we did not get the unit Emit warning even if
            --  missing subunit is not within main unit, to simplify debugging.

            pragma Assert (Original_Operating_Mode = Generate_Code);
            if Unum = No_Unit then
               Error_Msg_Unit_1 := Subunit_Name;
               Error_Msg_File_1 :=
                 Get_File_Name (Subunit_Name, Subunit => True);
               Error_Msg_N
                 ("subunit$$ in file{ not found??!!", N);
               Subunits_Missing := True;
            end if;

            --  Load_Unit may reset Compiler_State, since it may have been
            --  necessary to parse an additional units, so we make sure that
            --  we reset it to the Analyzing state.

            Compiler_State := Analyzing;

            if Unum /= No_Unit then
               if Debug_Flag_L then
                  Write_Str ("*** Loaded subunit from stub. Analyze");
                  Write_Eol;
               end if;

               Comp_Unit := Cunit (Unum);

               --  Check for child unit instead of subunit

               if Nkind (Unit (Comp_Unit)) /= N_Subunit then
                  Error_Msg_N
                    ("expected SEPARATE subunit, found child unit",
                     Cunit_Entity (Unum));

               --  OK, we have a subunit

               else
                  Set_Corresponding_Stub (Unit (Comp_Unit), N);
                  Set_Library_Unit (N, Comp_Unit);

                  --  We update the version. Although we are not technically
                  --  semantically dependent on the subunit, given our approach
                  --  of macro substitution of subunits, it makes sense to
                  --  include it in the version identification.

                  Version_Update (Cunit (Main_Unit), Comp_Unit);

                  --  Collect SCO information for loaded subunit if we are in
                  --  the extended main unit.

                  if Generate_SCO
                    and then In_Extended_Main_Source_Unit
                               (Cunit_Entity (Current_Sem_Unit))
                  then
                     SCO_Record_Raw (Unum);
                  end if;

                  --  Analyze the unit if semantics active

                  if Fatal_Error (Unum) /= Error_Detected
                    or else Try_Semantics
                  then
                     Analyze_Subunit (Comp_Unit);
                  end if;
               end if;
            end if;
         end if;

      --  The remaining case is when the subunit is not already loaded and we
      --  are not generating code. In this case we are just performing semantic
      --  analysis on the parent, and we are not interested in the subunit. For
      --  subprograms, analyze the stub as a body. For other entities the stub
      --  has already been marked as completed.

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

      --  First occurrence of name may have been as an incomplete type

      if Present (Nam) and then Ekind (Nam) = E_Incomplete_Type then
         Nam := Full_View (Nam);
      end if;

      if No (Nam) or else not Is_Protected_Type (Etype (Nam)) then
         Error_Msg_N ("missing specification for Protected body", N);

      else
         Set_Scope (Defining_Entity (N), Current_Scope);
         Set_Ekind (Defining_Entity (N), E_Protected_Body);
         Set_Has_Completion (Etype (Nam));
         Set_Corresponding_Spec_Of_Stub (N, Nam);
         Generate_Reference (Nam, Defining_Identifier (N), 'b');
         Analyze_Proper_Body (N, Etype (Nam));
      end if;
   end Analyze_Protected_Body_Stub;

   ----------------------------------
   -- Analyze_Subprogram_Body_Stub --
   ----------------------------------

   --  A subprogram body stub can appear with or without a previous spec. If
   --  there is one, then the analysis of the body will find it and verify
   --  conformance. The formals appearing in the specification of the stub play
   --  no role, except for requiring an additional conformance check. If there
   --  is no previous subprogram declaration, the stub acts as a spec, and
   --  provides the defining entity for the subprogram.

   procedure Analyze_Subprogram_Body_Stub (N : Node_Id) is
      Decl : Node_Id;
      Opts : Config_Switches_Type;

   begin
      Check_Stub_Level (N);

      --  Verify that the identifier for the stub is unique within this
      --  declarative part.

      if Nkind_In (Parent (N), N_Block_Statement,
                               N_Package_Body,
                               N_Subprogram_Body)
      then
         Decl := First (Declarations (Parent (N)));
         while Present (Decl) and then Decl /= N loop
            if Nkind (Decl) = N_Subprogram_Body_Stub
              and then (Chars (Defining_Unit_Name (Specification (Decl))) =
                        Chars (Defining_Unit_Name (Specification (N))))
            then
               Error_Msg_N ("identifier for stub is not unique", N);
            end if;

            Next (Decl);
         end loop;
      end if;

      --  Retain and restore the configuration options of the enclosing context
      --  as the proper body may introduce a set of its own.

      Save_Opt_Config_Switches (Opts);

      --  Treat stub as a body, which checks conformance if there is a previous
      --  declaration, or else introduces entity and its signature.

      Analyze_Subprogram_Body (N);
      Analyze_Proper_Body (N, Empty);

      Restore_Opt_Config_Switches (Opts);
   end Analyze_Subprogram_Body_Stub;

   ---------------------
   -- Analyze_Subunit --
   ---------------------

   --  A subunit is compiled either by itself (for semantic checking) or as
   --  part of compiling the parent (for code generation). In either case, by
   --  the time we actually process the subunit, the parent has already been
   --  installed and analyzed. The node N is a compilation unit, whose context
   --  needs to be treated here, because we come directly here from the parent
   --  without calling Analyze_Compilation_Unit.

   --  The compilation context includes the explicit context of the subunit,
   --  and the context of the parent, together with the parent itself. In order
   --  to compile the current context, we remove the one inherited from the
   --  parent, in order to have a clean visibility table. We restore the parent
   --  context before analyzing the proper body itself. On exit, we remove only
   --  the explicit context of the subunit.

   --  WARNING: This routine manages SPARK regions. Return statements must be
   --  replaced by gotos which jump to the end of the routine and restore the
   --  SPARK mode.

   procedure Analyze_Subunit (N : Node_Id) is
      Lib_Unit : constant Node_Id   := Library_Unit (N);
      Par_Unit : constant Entity_Id := Current_Scope;

      Lib_Spec        : Node_Id := Library_Unit (Lib_Unit);
      Num_Scopes      : Nat := 0;
      Use_Clauses     : array (1 .. Scope_Stack.Last) of Node_Id;
      Enclosing_Child : Entity_Id := Empty;
      Svg             : constant Suppress_Record := Scope_Suppress;

      Save_Cunit_Restrictions : constant Save_Cunit_Boolean_Restrictions :=
                                  Cunit_Boolean_Restrictions_Save;
      --  Save non-partition wide restrictions before processing the subunit.
      --  All subunits are analyzed with config restrictions reset and we need
      --  to restore these saved values at the end.

      procedure Analyze_Subunit_Context;
      --  Capture names in use clauses of the subunit. This must be done before
      --  re-installing parent declarations, because items in the context must
      --  not be hidden by declarations local to the parent.

      procedure Re_Install_Parents (L : Node_Id; Scop : Entity_Id);
      --  Recursive procedure to restore scope of all ancestors of subunit,
      --  from outermost in. If parent is not a subunit, the call to install
      --  context installs context of spec and (if parent is a child unit) the
      --  context of its parents as well. It is confusing that parents should
      --  be treated differently in both cases, but the semantics are just not
      --  identical.

      procedure Re_Install_Use_Clauses;
      --  As part of the removal of the parent scope, the use clauses are
      --  removed, to be reinstalled when the context of the subunit has been
      --  analyzed. Use clauses may also have been affected by the analysis of
      --  the context of the subunit, so they have to be applied again, to
      --  insure that the compilation environment of the rest of the parent
      --  unit is identical.

      procedure Remove_Scope;
      --  Remove current scope from scope stack, and preserve the list of use
      --  clauses in it, to be reinstalled after context is analyzed.

      -----------------------------
      -- Analyze_Subunit_Context --
      -----------------------------

      procedure Analyze_Subunit_Context is
         Item      :  Node_Id;
         Unit_Name : Entity_Id;

      begin
         Analyze_Context (N);
         Check_No_Elab_Code_All (N);

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
                     --  If a subunits has serious syntax errors, the context
                     --  may not have been loaded. Add a harmless unit name to
                     --  attempt processing.

                     if Serious_Errors_Detected > 0
                       and then No (Entity (Name (Item)))
                     then
                        Set_Entity (Name (Item), Standard_Standard);
                     end if;

                     Unit_Name := Entity (Name (Item));
                     loop
                        Set_Is_Visible_Lib_Unit (Unit_Name);
                        exit when Scope (Unit_Name) = Standard_Standard;
                        Unit_Name := Scope (Unit_Name);

                        if No (Unit_Name) then
                           Check_Error_Detected;
                           return;
                        end if;
                     end loop;

                     if not Is_Immediately_Visible (Unit_Name) then
                        Set_Is_Immediately_Visible (Unit_Name);
                        Set_Context_Installed (Item);
                     end if;
                  end if;
               end if;

            elsif Nkind (Item) = N_Use_Package_Clause then
               Analyze (Name (Item));

            elsif Nkind (Item) = N_Use_Type_Clause then
               Analyze (Subtype_Mark (Item));
            end if;

            Next (Item);
         end loop;

         --  Reset visibility of withed units. They will be made visible again
         --  when we install the subunit context.

         Item := First (Context_Items (N));
         while Present (Item) loop
            if Nkind (Item) = N_With_Clause

               --  Protect frontend against previous errors in context clauses

              and then Nkind (Name (Item)) /= N_Selected_Component
              and then not Error_Posted (Item)
            then
               Unit_Name := Entity (Name (Item));
               loop
                  Set_Is_Visible_Lib_Unit (Unit_Name, False);
                  exit when Scope (Unit_Name) = Standard_Standard;
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

         Install_Context (L, False);

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
            if not Is_Child_Unit (E) or else Is_Visible_Lib_Unit (E) then
               Set_Is_Immediately_Visible (E);
            end if;

            Next_Entity (E);
         end loop;

         --  A subunit appears within a body, and for a nested subunits all the
         --  parents are bodies. Restore full visibility of their private
         --  entities.

         if Is_Package_Or_Generic_Package (Scop) then
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
            Install_Use_Clauses (U);
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

      Saved_SM  : SPARK_Mode_Type := SPARK_Mode;
      Saved_SMP : Node_Id         := SPARK_Mode_Pragma;
      --  Save the SPARK mode-related data to restore on exit. Removing
      --  enclosing scopes and contexts to provide a clean environment for the
      --  context of the subunit will eliminate any previously set SPARK_Mode.

   --  Start of processing for Analyze_Subunit

   begin
      --  For subunit in main extended unit, we reset the configuration values
      --  for the non-partition-wide restrictions. For other units reset them.

      if In_Extended_Main_Source_Unit (N) then
         Restore_Config_Cunit_Boolean_Restrictions;
      else
         Reset_Cunit_Boolean_Restrictions;
      end if;

      if Style_Check then
         declare
            Nam : Node_Id := Name (Unit (N));

         begin
            if Nkind (Nam) = N_Selected_Component then
               Nam := Selector_Name (Nam);
            end if;

            Check_Identifier (Nam, Par_Unit);
         end;
      end if;

      if not Is_Empty_List (Context_Items (N)) then

         --  Save current use clauses

         Remove_Scope;
         Remove_Context (Lib_Unit);

         --  Now remove parents and their context, including enclosing subunits
         --  and the outer parent body which is not a subunit.

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

         --  Take into account the effect of any SPARK_Mode configuration
         --  pragma, which takes precedence over a different value of
         --  SPARK_Mode inherited from the context of the stub.

         if SPARK_Mode /= None then
            Saved_SM  := SPARK_Mode;
            Saved_SMP := SPARK_Mode_Pragma;
         end if;

         Re_Install_Parents (Lib_Unit, Par_Unit);
         Set_Is_Immediately_Visible (Par_Unit);

         --  If the context includes a child unit of the parent of the subunit,
         --  the parent will have been removed from visibility, after compiling
         --  that cousin in the context. The visibility of the parent must be
         --  restored now. This also applies if the context includes another
         --  subunit of the same parent which in turn includes a child unit in
         --  its context.

         if Is_Package_Or_Generic_Package (Par_Unit) then
            if not Is_Immediately_Visible (Par_Unit)
              or else (Present (First_Entity (Par_Unit))
                        and then not
                          Is_Immediately_Visible (First_Entity (Par_Unit)))
            then
               Set_Is_Immediately_Visible   (Par_Unit);
               Install_Visible_Declarations (Par_Unit);
               Install_Private_Declarations (Par_Unit);
            end if;
         end if;

         Re_Install_Use_Clauses;
         Install_Context (N, Chain => False);

         --  Restore state of suppress flags for current body

         Scope_Suppress := Svg;

         --  If the subunit is within a child unit, then siblings of any parent
         --  unit that appear in the context clause of the subunit must also be
         --  made immediately visible.

         if Present (Enclosing_Child) then
            Install_Siblings (Enclosing_Child, N);
         end if;
      end if;

      Generate_Parent_References (Unit (N), Par_Unit);

      --  Reinstall the SPARK_Mode which was in effect prior to any scope and
      --  context manipulations, taking into account a possible SPARK_Mode
      --  configuration pragma if present.

      Install_SPARK_Mode (Saved_SM, Saved_SMP);

      Analyze (Proper_Body (Unit (N)));
      Remove_Context (N);

      --  The subunit may contain a with_clause on a sibling of some ancestor.
      --  Removing the context will remove from visibility those ancestor child
      --  units, which must be restored to the visibility they have in the
      --  enclosing body.

      if Present (Enclosing_Child) then
         declare
            C : Entity_Id;
         begin
            C := Current_Scope;
            while Present (C) and then C /= Standard_Standard loop
               Set_Is_Immediately_Visible (C);
               Set_Is_Visible_Lib_Unit (C);
               C := Scope (C);
            end loop;
         end;
      end if;

      --  Deal with restore of restrictions

      Cunit_Boolean_Restrictions_Restore (Save_Cunit_Restrictions);
   end Analyze_Subunit;

   ----------------------------
   -- Analyze_Task_Body_Stub --
   ----------------------------

   procedure Analyze_Task_Body_Stub (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);
      Nam : Entity_Id := Current_Entity_In_Scope (Defining_Identifier (N));

   begin
      Check_Stub_Level (N);

      --  First occurrence of name may have been as an incomplete type

      if Present (Nam) and then Ekind (Nam) = E_Incomplete_Type then
         Nam := Full_View (Nam);
      end if;

      if No (Nam) or else not Is_Task_Type (Etype (Nam)) then
         Error_Msg_N ("missing specification for task body", N);

      else
         Set_Scope (Defining_Entity (N), Current_Scope);
         Set_Ekind (Defining_Entity (N), E_Task_Body);
         Generate_Reference (Nam, Defining_Identifier (N), 'b');
         Set_Corresponding_Spec_Of_Stub (N, Nam);

         --  Check for duplicate stub, if so give message and terminate

         if Has_Completion (Etype (Nam)) then
            Error_Msg_N ("duplicate stub for task", N);
            return;
         else
            Set_Has_Completion (Etype (Nam));
         end if;

         Analyze_Proper_Body (N, Etype (Nam));

         --  Set elaboration flag to indicate that entity is callable. This
         --  cannot be done in the expansion of the body itself, because the
         --  proper body is not in a declarative part. This is only done if
         --  expansion is active, because the context may be generic and the
         --  flag not defined yet.

         if Expander_Active then
            Insert_After (N,
              Make_Assignment_Statement (Loc,
                Name        =>
                  Make_Identifier (Loc,
                    Chars => New_External_Name (Chars (Etype (Nam)), 'E')),
                 Expression => New_Occurrence_Of (Standard_True, Loc)));
         end if;
      end if;
   end Analyze_Task_Body_Stub;

   -------------------------
   -- Analyze_With_Clause --
   -------------------------

   --  Analyze the declaration of a unit in a with clause. At end, label the
   --  with clause with the defining entity for the unit.

   procedure Analyze_With_Clause (N : Node_Id) is

      --  Retrieve the original kind of the unit node, before analysis. If it
      --  is a subprogram instantiation, its analysis below will rewrite the
      --  node as the declaration of the wrapper package. If the same
      --  instantiation appears indirectly elsewhere in the context, it will
      --  have been analyzed already.

      Unit_Kind : constant Node_Kind :=
                    Nkind (Original_Node (Unit (Library_Unit (N))));
      Nam       : constant Node_Id := Name (N);
      E_Name    : Entity_Id;
      Par_Name  : Entity_Id;
      Pref      : Node_Id;
      U         : Node_Id;

      Intunit : Boolean;
      --  Set True if the unit currently being compiled is an internal unit

      Restriction_Violation : Boolean := False;
      --  Set True if a with violates a restriction, no point in giving any
      --  warnings if we have this definite error.

      Save_Style_Check : constant Boolean := Opt.Style_Check;

   begin
      U := Unit (Library_Unit (N));

      --  If this is an internal unit which is a renaming, then this is a
      --  violation of No_Obsolescent_Features.

      --  Note: this is not quite right if the user defines one of these units
      --  himself, but that's a marginal case, and fixing it is hard ???

      if Restriction_Check_Required (No_Obsolescent_Features) then
         if In_Predefined_Renaming (U) then
            Check_Restriction (No_Obsolescent_Features, N);
            Restriction_Violation := True;
         end if;
      end if;

      --  Check No_Implementation_Units violation

      if Restriction_Check_Required (No_Implementation_Units) then
         if Not_Impl_Defined_Unit (Get_Source_Unit (U)) then
            null;
         else
            Check_Restriction (No_Implementation_Units, Nam);
            Restriction_Violation := True;
         end if;
      end if;

      --  Several actions are skipped for dummy packages (those supplied for
      --  with's where no matching file could be found). Such packages are
      --  identified by the Sloc value being set to No_Location.

      if Limited_Present (N) then

         --  Ada 2005 (AI-50217): Build visibility structures but do not
         --  analyze the unit.

         --  If the designated unit is a predefined unit, which might be used
         --  implicitly through the rtsfind machinery, a limited with clause
         --  on such a unit is usually pointless, because run-time units are
         --  unlikely to appear in mutually dependent units, and because this
         --  disables the rtsfind mechanism. We transform such limited with
         --  clauses into regular with clauses.

         if Sloc (U) /= No_Location then
            if In_Predefined_Unit (U)

              --  In ASIS mode the rtsfind mechanism plays no role, and
              --  we need to maintain the original tree structure, so
              --  this transformation is not performed in this case.

              and then not ASIS_Mode
            then
               Set_Limited_Present (N, False);
               Analyze_With_Clause (N);
            else
               Build_Limited_Views (N);
            end if;
         end if;

         return;
      end if;

      --  If we are compiling under "don't quit" mode (-gnatq) and we have
      --  already detected serious errors then we mark the with-clause nodes as
      --  analyzed before the corresponding compilation unit is analyzed. This
      --  is done here to protect the frontend against never ending recursion
      --  caused by circularities in the sources (because the previous errors
      --  may break the regular machine of the compiler implemented in
      --  Load_Unit to detect circularities).

      if Serious_Errors_Detected > 0 and then Try_Semantics then
         Set_Analyzed (N);
      end if;

      Semantics (Library_Unit (N));

      Intunit := Is_Internal_Unit (Current_Sem_Unit);

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
           and then Nam_In (Chars (Selector_Name (Nam)),
                            Name_Most_Recent_Exception,
                            Name_Exception_Traces)
         then
            Check_Restriction (No_Exception_Propagation, N);
            Special_Exception_Package_Used := True;
         end if;

         --  Check for inappropriate with of internal implementation unit if we
         --  are not compiling an internal unit and also check for withing unit
         --  in wrong version of Ada. Do not issue these messages for implicit
         --  with's generated by the compiler itself.

         if Implementation_Unit_Warnings
           and then not Intunit
           and then not Implicit_With (N)
           and then not Restriction_Violation
         then
            declare
               U_Kind : constant Kind_Of_Unit :=
                          Get_Kind_Of_Unit (Get_Source_Unit (U));

            begin
               if U_Kind = Implementation_Unit then
                  Error_Msg_F ("& is an internal 'G'N'A'T unit?i?", Name (N));

                  --  Add alternative name if available, otherwise issue a
                  --  general warning message.

                  if Error_Msg_Strlen /= 0 then
                     Error_Msg_F ("\use ""~"" instead?i?", Name (N));
                  else
                     Error_Msg_F
                       ("\use of this unit is non-portable " &
                        "and version-dependent?i?", Name (N));
                  end if;

               elsif U_Kind = Ada_2005_Unit
                 and then Ada_Version < Ada_2005
                 and then Warn_On_Ada_2005_Compatibility
               then
                  Error_Msg_N ("& is an Ada 2005 unit?i?", Name (N));

               elsif U_Kind = Ada_2012_Unit
                 and then Ada_Version < Ada_2012
                 and then Warn_On_Ada_2012_Compatibility
               then
                  Error_Msg_N ("& is an Ada 2012 unit?i?", Name (N));
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
        and then Present (Instance_Spec (U))
      then
         --  If the instance has not been rewritten as a package declaration,
         --  then it appeared already in a previous with clause. Retrieve
         --  the entity from the previous instance.

         E_Name := Defining_Entity (Specification (Instance_Spec (U)));

      elsif Unit_Kind in N_Subprogram_Instantiation then

         --  The visible subprogram is created during instantiation, and is
         --  an attribute of the wrapper package. We retrieve the wrapper
         --  package directly from the instantiation node. If the instance
         --  is inlined the unit is still an instantiation. Otherwise it has
         --  been rewritten as the declaration of the wrapper itself.

         if Nkind (U) in N_Subprogram_Instantiation then
            E_Name :=
              Related_Instance
                (Defining_Entity (Specification (Instance_Spec (U))));
         else
            E_Name := Related_Instance (Defining_Entity (U));
         end if;

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

         --  If this is a child unit without a spec, and it has been analyzed
         --  already, a declaration has been created for it. The with_clause
         --  must reflect the actual body, and not the generated declaration,
         --  to prevent spurious binding errors involving an out-of-date spec.
         --  Note that this can only happen if the unit includes more than one
         --  with_clause for the child unit (e.g. in separate subunits).

         if Unit_Kind = N_Subprogram_Declaration
           and then Analyzed (Library_Unit (N))
           and then not Comes_From_Source (Library_Unit (N))
         then
            Set_Library_Unit (N,
               Cunit (Get_Source_Unit (Corresponding_Body (U))));
         end if;
      end if;

      --  Restore style checks

      Style_Check := Save_Style_Check;

      --  Record the reference, but do NOT set the unit as referenced, we want
      --  to consider the unit as unreferenced if this is the only reference
      --  that occurs.

      Set_Entity_With_Checks (Name (N), E_Name);
      Generate_Reference (E_Name, Name (N), 'w', Set_Ref => False);

      --  Generate references and check No_Dependence restriction for parents

      if Is_Child_Unit (E_Name) then
         Pref     := Prefix (Name (N));
         Par_Name := Scope (E_Name);
         while Nkind (Pref) = N_Selected_Component loop
            Change_Selected_Component_To_Expanded_Name (Pref);

            if Present (Entity (Selector_Name (Pref)))
              and then
                Present (Renamed_Entity (Entity (Selector_Name (Pref))))
              and then Entity (Selector_Name (Pref)) /= Par_Name
            then
            --  The prefix is a child unit that denotes a renaming declaration.
            --  Replace the prefix directly with the renamed unit, because the
            --  rest of the prefix is irrelevant to the visibility of the real
            --  unit.

               Rewrite (Pref, New_Occurrence_Of (Par_Name, Sloc (Pref)));
               exit;
            end if;

            Set_Entity_With_Checks (Pref, Par_Name);

            Generate_Reference (Par_Name, Pref);
            Check_Restriction_No_Dependence (Pref, N);
            Pref := Prefix (Pref);

            --  If E_Name is the dummy entity for a nonexistent unit, its scope
            --  is set to Standard_Standard, and no attempt should be made to
            --  further unwind scopes.

            if Par_Name /= Standard_Standard then
               Par_Name := Scope (Par_Name);
            end if;

            --  Abandon processing in case of previous errors

            if No (Par_Name) then
               Check_Error_Detected;
               return;
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

         --  Guard against missing or misspelled child units

         if Present (Par_Name) then
            Set_Entity_With_Checks (Pref, Par_Name);
            Generate_Reference (Par_Name, Pref);

         else
            pragma Assert (Serious_Errors_Detected /= 0);

            --  Mark the node to indicate that a related error has been posted.
            --  This defends further compilation passes against improper use of
            --  the invalid WITH clause node.

            Set_Error_Posted (N);
            Set_Name (N, Error);
            return;
         end if;
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

      --  Propagate Fatal_Error setting from with'ed unit to current unit

      case Fatal_Error (Get_Source_Unit (Library_Unit (N))) is

         --  Nothing to do if with'ed unit had no error

         when None =>
            null;

         --  If with'ed unit had a detected fatal error, propagate it

         when Error_Detected =>
            Set_Fatal_Error (Current_Sem_Unit, Error_Detected);

         --  If with'ed unit had an ignored error, then propagate it but do not
         --  overide an existring setting.

         when Error_Ignored =>
            if Fatal_Error (Current_Sem_Unit) = None then
               Set_Fatal_Error (Current_Sem_Unit, Error_Ignored);
            end if;
      end case;

      Mark_Ghost_Clause (N);
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

      -----------------------------
      -- Is_Private_Library_Unit --
      -----------------------------

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

         --  If the parent itself is a subunit, Curr_Unit is the entity of the
         --  enclosing body, retrieve the spec entity which is the proper
         --  ancestor we need for the following tests.

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
            and then not Limited_Present (Item)
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
      --  Build name to be used in implicit with_clause. In most cases this
      --  is the source name, but if renamings are present we must make the
      --  original unit visible, not the one it renames. The entity in the
      --  with clause is the renamed unit, but the identifier is the one from
      --  the source, which allows us to recover the unit renaming.

      ---------------------
      -- Build_Unit_Name --
      ---------------------

      function Build_Unit_Name (Nam : Node_Id) return Node_Id is
         Ent      : Entity_Id;
         Result   : Node_Id;

      begin
         if Nkind (Nam) = N_Identifier then
            return New_Occurrence_Of (Entity (Nam), Loc);

         else
            Ent := Entity (Nam);

            if Present (Entity (Selector_Name (Nam)))
              and then Chars (Entity (Selector_Name (Nam))) /= Chars (Ent)
              and then
                Nkind (Unit_Declaration_Node (Entity (Selector_Name (Nam))))
                  = N_Package_Renaming_Declaration
            then
               --  The name in the with_clause is of the form A.B.C, and B is
               --  given by a renaming declaration. In that case we may not
               --  have analyzed the unit for B, but replaced it directly in
               --  lib-load with the unit it renames. We have to make A.B
               --  visible, so analyze the declaration for B now, in case it
               --  has not been done yet.

               Ent := Entity (Selector_Name (Nam));
               Analyze
                 (Parent
                   (Unit_Declaration_Node (Entity (Selector_Name (Nam)))));
            end if;

            Result :=
              Make_Expanded_Name (Loc,
                Chars  => Chars (Entity (Nam)),
                Prefix => Build_Unit_Name (Prefix (Nam)),
                Selector_Name => New_Occurrence_Of (Ent, Loc));
            Set_Entity (Result, Ent);
            return Result;
         end if;
      end Build_Unit_Name;

   --  Start of processing for Expand_With_Clause

   begin
      Withn :=
        Make_With_Clause (Loc,
          Name => Build_Unit_Name (Nam));

      P := Parent (Unit_Declaration_Node (Ent));
      Set_Library_Unit       (Withn, P);
      Set_Corresponding_Spec (Withn, Ent);
      Set_First_Name         (Withn, True);
      Set_Implicit_With      (Withn, True);

      --  If the unit is a package or generic package declaration, a private_
      --  with_clause on a child unit implies that the implicit with on the
      --  parent is also private.

      if Nkind_In (Unit (N), N_Package_Declaration,
                             N_Generic_Package_Declaration)
      then
         Set_Private_Present (Withn, Private_Present (Item));
      end if;

      Prepend (Withn, Context_Items (N));
      Mark_Rewrite_Insertion (Withn);
      Install_Withed_Unit (Withn);

      --  If we have "with X.Y;", we want to recurse on "X", except in the
      --  unusual case where X.Y is a renaming of X. In that case, the scope
      --  of X will be null.

      if Nkind (Nam) = N_Expanded_Name
        and then Present (Scope (Entity (Prefix (Nam))))
      then
         Expand_With_Clause (Item, Prefix (Nam), N);
      end if;
   end Expand_With_Clause;

   --------------------------------
   -- Generate_Parent_References --
   --------------------------------

   procedure Generate_Parent_References (N : Node_Id; P_Id : Entity_Id) is
      Pref   : Node_Id;
      P_Name : Entity_Id := P_Id;

   begin
      if Nkind (N) = N_Subunit then
         Pref := Name (N);
      else
         Pref := Name (Parent (Defining_Entity (N)));
      end if;

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

   ---------------------
   -- Has_With_Clause --
   ---------------------

   function Has_With_Clause
     (C_Unit     : Node_Id;
      Pack       : Entity_Id;
      Is_Limited : Boolean := False) return Boolean
   is
      Item : Node_Id;

      function Named_Unit (Clause : Node_Id) return Entity_Id;
      --  Return the entity for the unit named in a [limited] with clause

      ----------------
      -- Named_Unit --
      ----------------

      function Named_Unit (Clause : Node_Id) return Entity_Id is
      begin
         if Nkind (Name (Clause)) = N_Selected_Component then
            return Entity (Selector_Name (Name (Clause)));
         else
            return Entity (Name (Clause));
         end if;
      end Named_Unit;

   --  Start of processing for Has_With_Clause

   begin
      if Present (Context_Items (C_Unit)) then
         Item := First (Context_Items (C_Unit));
         while Present (Item) loop
            if Nkind (Item) = N_With_Clause
              and then Limited_Present (Item) = Is_Limited
              and then Named_Unit (Item) = Pack
            then
               return True;
            end if;

            Next (Item);
         end loop;
      end if;

      return False;
   end Has_With_Clause;

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
                   New_Occurrence_Of (Defining_Entity (P), Loc);
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
            return New_Occurrence_Of (P_Name, Loc);

         else
            Result :=
              Make_Expanded_Name (Loc,
                Chars  => Chars (P_Name),
                Prefix => Build_Ancestor_Name (Unit (Parent_Spec (P_Unit))),
                Selector_Name => New_Occurrence_Of (P_Name, Loc));
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

   procedure Install_Context (N : Node_Id; Chain : Boolean := True) is
      Lib_Unit : constant Node_Id := Unit (N);

   begin
      Install_Context_Clauses (N, Chain);

      if Is_Child_Spec (Lib_Unit) then
         Install_Parents
           (Lib_Unit   => Lib_Unit,
            Is_Private => Private_Present (Parent (Lib_Unit)),
            Chain      => Chain);
      end if;

      Install_Limited_Context_Clauses (N);
   end Install_Context;

   -----------------------------
   -- Install_Context_Clauses --
   -----------------------------

   procedure Install_Context_Clauses (N : Node_Id; Chain : Boolean := True) is
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
        and then Pragma_Name (Item) in Configuration_Pragma_Names
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
                       ("license of withed unit & may be inconsistent??",
                        Name (Item));
                  end License_Error;

               --  Start of processing for License_Check

               begin
                  --  Exclude license check if withed unit is an internal unit.
                  --  This situation arises e.g. with the GPL version of GNAT.

                  if Is_Internal_Unit (Withu) then
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
            Analyze_Use_Package (Item, Chain);

         --  Case of USE TYPE clause

         elsif Nkind (Item) = N_Use_Type_Clause then
            Analyze_Use_Type (Item, Chain);

         --  case of PRAGMA

         elsif Nkind (Item) = N_Pragma then
            Analyze (Item);
         end if;

      <<Continue>>
         Next (Item);
      end loop;

      if Is_Child_Spec (Lib_Unit) then

         --  The unit also has implicit with_clauses on its own parents

         if No (Context_Items (N)) then
            Set_Context_Items (N, New_List);
         end if;

         Implicit_With_On_Parent (Lib_Unit, N);
      end if;

      --  If the unit is a body, the context of the specification must also
      --  be installed. That includes private with_clauses in that context.

      if Nkind (Lib_Unit) = N_Package_Body
        or else (Nkind (Lib_Unit) = N_Subprogram_Body
                  and then not Acts_As_Spec (N))
      then
         Install_Context (Library_Unit (N), Chain);

         --  Only install private with-clauses of a spec that comes from
         --  source, excluding specs created for a subprogram body that is
         --  a child unit.

         if Comes_From_Source (Library_Unit (N)) then
            Install_Private_With_Clauses
              (Defining_Entity (Unit (Library_Unit (N))));
         end if;

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
      --  mentions a descendant of a private child of some library unit, then
      --  the given compilation_unit must be the declaration of a private
      --  descendant of that library unit, or a public descendant of such. The
      --  code is analogous to that of Check_Private_Child_Unit but we cannot
      --  use entities on the limited with_clauses because their units have not
      --  been analyzed, so we have to climb the tree of ancestors looking for
      --  private keywords.

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
         E      : Entity_Id;
         E2     : Entity_Id;

      begin
         pragma Assert (Nkind (W) = N_With_Clause);

         --  Protect the frontend against previous critical errors

         case Nkind (Unit (Library_Unit (W))) is
            when N_Generic_Package_Declaration
               | N_Generic_Subprogram_Declaration
               | N_Package_Declaration
               | N_Subprogram_Declaration
            =>
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

               E := Entity (Name (Item));

               pragma Assert (Present (Parent (E)));

               if Nkind (Parent (E)) = N_Package_Renaming_Declaration
                 and then Renamed_Entity (E) = WEnt
               then
                  --  The unlimited view is visible through use clause and
                  --  renamings. There is no need to generate the error
                  --  message here because Is_Visible_Through_Renamings
                  --  takes care of generating the precise error message.

                  return;

               elsif Nkind (Parent (E)) = N_Package_Specification then

                  --  The use clause may refer to a local package.
                  --  Check all the enclosing scopes.

                  E2 := E;
                  while E2 /= Standard_Standard and then E2 /= WEnt loop
                     E2 := Scope (E2);
                  end loop;

                  if E2 = WEnt then
                     Error_Msg_N
                       ("unlimited view visible through use clause ", W);
                     return;
                  end if;
               end if;
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
         Curr_Private : Boolean;

      begin
         --  Compilation unit of the parent of the withed library unit

         Child_Parent := Library_Unit (Item);

         --  If the child unit is a public child, then locate its nearest
         --  private ancestor, if any, then Child_Parent will then be set to
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
         end if;

         Child_Parent := Parent_Spec (Unit (Child_Parent));

         --  Traverse all the ancestors of the current compilation unit to
         --  check if it is a descendant of named library unit.

         Curr_Parent := Parent (Item);
         Curr_Private := Private_Present (Curr_Parent);

         while Present (Parent_Spec (Unit (Curr_Parent)))
           and then Curr_Parent /= Child_Parent
         loop
            Curr_Parent := Parent_Spec (Unit (Curr_Parent));
            Curr_Private := Curr_Private or else Private_Present (Curr_Parent);
         end loop;

         if Curr_Parent /= Child_Parent then
            Error_Msg_N
              ("unit in with clause is private child unit!", Item);
            Error_Msg_NE
              ("\current unit must also have parent&!",
               Item, Defining_Unit_Name (Specification (Unit (Child_Parent))));

         elsif Private_Present (Parent (Item))
            or else Curr_Private
            or else Private_Present (Item)
            or else Nkind_In (Unit (Parent (Item)), N_Package_Body,
                                                    N_Subprogram_Body,
                                                    N_Subunit)
         then
            --  Current unit is private, of descendant of a private unit

            null;

         else
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

         --  Do not generate a limited_with_clause on the current unit. This
         --  path is taken when a unit has a limited_with clause on one of its
         --  child units.

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
      end Expand_Limited_With_Clause;

   --  Start of processing for Install_Limited_Context_Clauses

   begin
      Item := First (Context_Items (N));
      while Present (Item) loop
         if Nkind (Item) = N_With_Clause
           and then Limited_Present (Item)
           and then not Error_Posted (Item)
         then
            if Nkind (Name (Item)) = N_Selected_Component then
               Expand_Limited_With_Clause
                 (Comp_Unit => N, Nam => Prefix (Name (Item)), N => Item);
            end if;

            Check_Private_Limited_Withed_Unit (Item);

            if not Implicit_With (Item) and then Is_Child_Spec (Unit (N)) then
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
              and then
                not Is_Ancestor_Unit
                      (Library_Unit (Item), Cunit (Current_Sem_Unit))
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
         end if;

         Next (Item);
      end loop;

      --  Ada 2005 (AI-412): Examine visible declarations of a package spec,
      --  looking for incomplete subtype declarations of incomplete types
      --  visible through a limited with clause.

      if Ada_Version >= Ada_2005
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
                   From_Limited_With (Defining_Identifier (Decl))
               then
                  Def_Id := Defining_Identifier (Decl);
                  Non_Lim_View := Non_Limited_View (Def_Id);

                  if not Is_Incomplete_Type (Non_Lim_View) then

                     --  Convert an incomplete subtype declaration into a
                     --  corresponding non-limited view subtype declaration.
                     --  This is usually the case when analyzing a body that
                     --  has regular with clauses,  when the spec has limited
                     --  ones.

                     --  If the non-limited view is still incomplete, it is
                     --  the dummy entry already created, and the declaration
                     --  cannot be reanalyzed. This is the case when installing
                     --  a parent unit that has limited with-clauses.

                     Set_Subtype_Indication (Decl,
                       New_Occurrence_Of (Non_Lim_View, Sloc (Def_Id)));
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

   procedure Install_Parents
     (Lib_Unit   : Node_Id;
      Is_Private : Boolean;
      Chain      : Boolean := True)
   is
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
         Install_Parents
           (Lib_Unit   => P,
            Is_Private =>
              Is_Private or else Private_Present (Parent (Lib_Unit)),
            Chain      => Chain);
      end if;

      --  Now we can install the context for this parent

      Install_Context_Clauses (Parent_Spec (Lib_Unit), Chain);
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
      --  status as needed. Indicate that it is a compilation unit, which is
      --  redundant in general, but needed if this is a generated child spec
      --  for a child body without previous spec.

      E_Name := Defining_Entity (Lib_Unit);

      Set_Is_Child_Unit (E_Name);
      Set_Is_Compilation_Unit (E_Name);

      Set_Is_Private_Descendant (E_Name,
         Is_Private_Descendant (P_Name)
           or else Private_Present (Parent (Lib_Unit)));

      P_Spec := Package_Specification (P_Name);
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

      if Is_Private or else Private_Present (Parent (Lib_Unit)) then
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
               --  If the unit is an ancestor of the current one, it is the
               --  case of a private limited with clause on a child unit, and
               --  the compilation of one of its descendants, In that case the
               --  limited view is errelevant.

               if Limited_Present (Item) then
                  if not Limited_View_Installed (Item)
                    and then
                      not Is_Ancestor_Unit (Library_Unit (Item),
                                            Cunit (Current_Sem_Unit))
                  then
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

         --  Do not install private_with_clauses declaration, unless unit
         --  is itself a private child unit, or is a body. Note that for a
         --  subprogram body the private_with_clause does not take effect
         --  until after the specification.

         if Nkind (Item) /= N_With_Clause
           or else Implicit_With (Item)
           or else Limited_Present (Item)
           or else Error_Posted (Item)

            --  Skip processing malformed trees

           or else (Try_Semantics
                     and then Nkind (Name (Item)) not in N_Has_Entity)
         then
            null;

         elsif not Private_Present (Item)
           or else Private_Present (N)
           or else Nkind (Unit (N)) = N_Package_Body
         then
            Id := Entity (Name (Item));

            if Is_Child_Unit (Id)
              and then Is_Ancestor_Package (Scope (Id), U_Name)
            then
               Set_Is_Immediately_Visible (Id);

               --  Check for the presence of another unit in the context that
               --  may be inadvertently hidden by the child.

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
                               "with the same name??",
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

         --  If the item is a private with-clause on a child unit, the parent
         --  may have been installed already, but the child unit must remain
         --  invisible until installed in a private part or body, unless there
         --  is already a regular with_clause for it in the current unit.

         elsif Private_Present (Item) then
            Id := Entity (Name (Item));

            if Is_Child_Unit (Id) then
               declare
                  Clause : Node_Id;

                  function In_Context return Boolean;
                  --  Scan context of current unit, to check whether there is
                  --  a with_clause on the same unit as a private with-clause
                  --  on a parent, in which case child unit is visible. If the
                  --  unit is a grand-child, the same applies to its parent.

                  ----------------
                  -- In_Context --
                  ----------------

                  function In_Context return Boolean is
                  begin
                     Clause :=
                       First (Context_Items (Cunit (Current_Sem_Unit)));
                     while Present (Clause) loop
                        if Nkind (Clause) = N_With_Clause
                          and then Comes_From_Source (Clause)
                          and then Is_Entity_Name (Name (Clause))
                          and then not Private_Present (Clause)
                        then
                           if Entity (Name (Clause)) = Id
                             or else
                               (Nkind (Name (Clause)) = N_Expanded_Name
                                 and then Entity (Prefix (Name (Clause))) = Id)
                           then
                              return True;
                           end if;
                        end if;

                        Next (Clause);
                     end loop;

                     return False;
                  end In_Context;

               begin
                  Set_Is_Visible_Lib_Unit (Id, In_Context);
               end;
            end if;
         end if;

         Next (Item);
      end loop;
   end Install_Siblings;

   ---------------------------------
   -- Install_Limited_Withed_Unit --
   ---------------------------------

   procedure Install_Limited_Withed_Unit (N : Node_Id) is
      P_Unit           : constant Entity_Id := Unit (Library_Unit (N));
      E                : Entity_Id;
      P                : Entity_Id;
      Is_Child_Package : Boolean := False;
      Lim_Header       : Entity_Id;
      Lim_Typ          : Entity_Id;

      procedure Check_Body_Required;
      --  A unit mentioned in a limited with_clause may not be mentioned in
      --  a regular with_clause, but must still be included in the current
      --  partition. We need to determine whether the unit needs a body, so
      --  that the binder can determine the name of the file to be compiled.
      --  Checking whether a unit needs a body can be done without semantic
      --  analysis, by examining the nature of the declarations in the package.

      function Has_Limited_With_Clause
        (C_Unit : Entity_Id;
         Pack   : Entity_Id) return Boolean;
      --  Determine whether any package in the ancestor chain starting with
      --  C_Unit has a limited with clause for package Pack.

      function Is_Visible_Through_Renamings (P : Entity_Id) return Boolean;
      --  Check if some package installed though normal with-clauses has a
      --  renaming declaration of package P. AARM 10.1.2(21/2).

      -------------------------
      -- Check_Body_Required --
      -------------------------

      procedure Check_Body_Required is
         PA : constant List_Id :=
                Pragmas_After (Aux_Decls_Node (Parent (P_Unit)));

         procedure Check_Declarations (Spec : Node_Id);
         --  Recursive procedure that does the work and checks nested packages

         ------------------------
         -- Check_Declarations --
         ------------------------

         procedure Check_Declarations (Spec : Node_Id) is
            Decl             : Node_Id;
            Incomplete_Decls : constant Elist_Id := New_Elmt_List;

            Subp_List        : constant Elist_Id := New_Elmt_List;

            procedure Check_Pragma_Import (P : Node_Id);
            --  If a pragma import applies to a previous subprogram, the
            --  enclosing unit may not need a body. The processing is syntactic
            --  and does not require a declaration to be analyzed. The code
            --  below also handles pragma Import when applied to a subprogram
            --  that renames another. In this case the pragma applies to the
            --  renamed entity.
            --
            --  Chains of multiple renames are not handled by the code below.
            --  It is probably impossible to handle all cases without proper
            --  name resolution. In such cases the algorithm is conservative
            --  and will indicate that a body is needed???

            -------------------------
            -- Check_Pragma_Import --
            -------------------------

            procedure Check_Pragma_Import (P : Node_Id) is
               Arg      : Node_Id;
               Prev_Id  : Elmt_Id;
               Subp_Id  : Elmt_Id;
               Imported : Node_Id;

               procedure Remove_Homonyms (E : Node_Id);
               --  Make one pass over list of subprograms. Called again if
               --  subprogram is a renaming. E is known to be an identifier.

               ---------------------
               -- Remove_Homonyms --
               ---------------------

               procedure Remove_Homonyms (E : Node_Id) is
                  R : Entity_Id := Empty;
                  --  Name of renamed entity, if any

               begin
                  Subp_Id := First_Elmt (Subp_List);
                  while Present (Subp_Id) loop
                     if Chars (Node (Subp_Id)) = Chars (E) then
                        if Nkind (Parent (Parent (Node (Subp_Id))))
                          /= N_Subprogram_Renaming_Declaration
                        then
                           Prev_Id := Subp_Id;
                           Next_Elmt (Subp_Id);
                           Remove_Elmt (Subp_List, Prev_Id);
                        else
                           R := Name (Parent (Parent (Node (Subp_Id))));
                           exit;
                        end if;
                     else
                        Next_Elmt (Subp_Id);
                     end if;
                  end loop;

                  if Present (R) then
                     if Nkind (R) = N_Identifier then
                        Remove_Homonyms (R);

                     elsif Nkind (R) = N_Selected_Component then
                        Remove_Homonyms (Selector_Name (R));

                     --  Renaming of attribute

                     else
                        null;
                     end if;
                  end if;
               end Remove_Homonyms;

            --  Start of processing for Check_Pragma_Import

            begin
               --  Find name of entity in Import pragma. We have not analyzed
               --  the construct, so we must guard against syntax errors.

               Arg := Next (First (Pragma_Argument_Associations (P)));

               if No (Arg)
                 or else Nkind (Expression (Arg)) /= N_Identifier
               then
                  return;
               else
                  Imported := Expression (Arg);
               end if;

               Remove_Homonyms (Imported);
            end Check_Pragma_Import;

         --  Start of processing for Check_Declarations

         begin
            --  Search for Elaborate Body pragma

            Decl := First (Visible_Declarations (Spec));
            while Present (Decl)
              and then Nkind (Decl) = N_Pragma
            loop
               if Get_Pragma_Id (Decl) = Pragma_Elaborate_Body then
                  Set_Body_Required (Library_Unit (N));
                  return;
               end if;

               Next (Decl);
            end loop;

            --  Look for declarations that require the presence of a body. We
            --  have already skipped pragmas at the start of the list.

            while Present (Decl) loop

               --  Subprogram that comes from source means body may be needed.
               --  Save for subsequent examination of import pragmas.

               if Comes_From_Source (Decl)
                 and then (Nkind_In (Decl, N_Subprogram_Declaration,
                                           N_Subprogram_Renaming_Declaration,
                                           N_Generic_Subprogram_Declaration))
               then
                  Append_Elmt (Defining_Entity (Decl), Subp_List);

               --  Package declaration of generic package declaration. We need
               --  to recursively examine nested declarations.

               elsif Nkind_In (Decl, N_Package_Declaration,
                                     N_Generic_Package_Declaration)
               then
                  Check_Declarations (Specification (Decl));

               elsif Nkind (Decl) = N_Pragma
                 and then Pragma_Name (Decl) = Name_Import
               then
                  Check_Pragma_Import (Decl);
               end if;

               Next (Decl);
            end loop;

            --  Same set of tests for private part. In addition to subprograms
            --  detect the presence of Taft Amendment types (incomplete types
            --  completed in the body).

            Decl := First (Private_Declarations (Spec));
            while Present (Decl) loop
               if Comes_From_Source (Decl)
                 and then (Nkind_In (Decl, N_Subprogram_Declaration,
                                           N_Subprogram_Renaming_Declaration,
                                           N_Generic_Subprogram_Declaration))
               then
                  Append_Elmt (Defining_Entity (Decl), Subp_List);

               elsif Nkind_In (Decl, N_Package_Declaration,
                                     N_Generic_Package_Declaration)
               then
                  Check_Declarations (Specification (Decl));

               --  Collect incomplete type declarations for separate pass

               elsif Nkind (Decl) = N_Incomplete_Type_Declaration then
                  Append_Elmt (Decl, Incomplete_Decls);

               elsif Nkind (Decl) = N_Pragma
                 and then Pragma_Name (Decl) = Name_Import
               then
                  Check_Pragma_Import (Decl);
               end if;

               Next (Decl);
            end loop;

            --  Now check incomplete declarations to locate Taft amendment
            --  types. This can be done by examining the defining identifiers
            --  of  type declarations without real semantic analysis.

            declare
               Inc : Elmt_Id;

            begin
               Inc := First_Elmt (Incomplete_Decls);
               while Present (Inc) loop
                  Decl := Next (Node (Inc));
                  while Present (Decl) loop
                     if Nkind (Decl) = N_Full_Type_Declaration
                       and then Chars (Defining_Identifier (Decl)) =
                                Chars (Defining_Identifier (Node (Inc)))
                     then
                        exit;
                     end if;

                     Next (Decl);
                  end loop;

                  --  If no completion, this is a TAT, and a body is needed

                  if No (Decl) then
                     Set_Body_Required (Library_Unit (N));
                     return;
                  end if;

                  Next_Elmt (Inc);
               end loop;
            end;

            --  Finally, check whether there are subprograms that still require
            --  a body, i.e. are not renamings or null.

            if not Is_Empty_Elmt_List (Subp_List) then
               declare
                  Subp_Id : Elmt_Id;
                  Spec    : Node_Id;

               begin
                  Subp_Id := First_Elmt (Subp_List);
                  Spec    := Parent (Node (Subp_Id));

                  while Present (Subp_Id) loop
                     if Nkind (Parent (Spec))
                        = N_Subprogram_Renaming_Declaration
                     then
                        null;

                     elsif Nkind (Spec) = N_Procedure_Specification
                       and then Null_Present (Spec)
                     then
                        null;

                     else
                        Set_Body_Required (Library_Unit (N));
                        return;
                     end if;

                     Next_Elmt (Subp_Id);
                  end loop;
               end;
            end if;
         end Check_Declarations;

      --  Start of processing for Check_Body_Required

      begin
         --  If this is an imported package (Java and CIL usage) no body is
         --  needed. Scan list of pragmas that may follow a compilation unit
         --  to look for a relevant pragma Import.

         if Present (PA) then
            declare
               Prag : Node_Id;

            begin
               Prag := First (PA);
               while Present (Prag) loop
                  if Nkind (Prag) = N_Pragma
                    and then Get_Pragma_Id (Prag) = Pragma_Import
                  then
                     return;
                  end if;

                  Next (Prag);
               end loop;
            end;
         end if;

         Check_Declarations (Specification (P_Unit));
      end Check_Body_Required;

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

            --  If there are more ancestors, climb up the tree, otherwise we
            --  are done.

            if Is_Child_Unit (Par) then
               Par := Scope (Par);
            else
               exit;
            end if;
         end loop;

         return False;
      end Has_Limited_With_Clause;

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
                 and then Nkind (Unit (Library_Unit (Item))) =
                                                  N_Package_Declaration
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
                        --  bodies and package bodies we just return True to
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

            --  If it is a body not acting as spec, follow pointer to the
            --  corresponding spec, otherwise follow pointer to parent spec.

            if Present (Library_Unit (Aux_Unit))
              and then Nkind_In (Unit (Aux_Unit),
                                 N_Package_Body, N_Subprogram_Body)
            then
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
      --  have nothing to do here. If the file is missing altogether, it has
      --  no source location.

      if Nkind (P_Unit) /= N_Package_Declaration
        or else Sloc (P_Unit) = No_Location
      then
         return;
      end if;

      P := Defining_Unit_Name (Specification (P_Unit));

      --  Handle child packages

      if Nkind (P) = N_Defining_Program_Unit_Name then
         Is_Child_Package := True;
         P := Defining_Identifier (P);
      end if;

      --  Do not install the limited-view if the context of the unit is already
      --  available through a regular with clause.

      if Nkind (Unit (Cunit (Current_Sem_Unit))) = N_Package_Body
        and then Has_With_Clause (Cunit (Current_Sem_Unit), P)
      then
         return;
      end if;

      --  Do not install the limited-view if the full-view is already visible
      --  through renaming declarations.

      if Is_Visible_Through_Renamings (P) then
         return;
      end if;

      --  Do not install the limited view if this is the unit being analyzed.
      --  This unusual case will happen when a unit has a limited_with clause
      --  on one of its children. The compilation of the child forces the load
      --  of the parent which tries to install the limited view of the child
      --  again. Installing the limited view must also be disabled when
      --  compiling the body of the child unit.

      if P = Cunit_Entity (Current_Sem_Unit)
        or else (Nkind (Unit (Cunit (Current_Sem_Unit))) = N_Package_Body
                  and then P = Main_Unit_Entity
                  and then Is_Ancestor_Unit
                             (Cunit (Main_Unit), Cunit (Current_Sem_Unit)))
      then
         return;
      end if;

      --  This scenario is similar to the one above, the difference is that the
      --  compilation of sibling Par.Sib forces the load of parent Par which
      --  tries to install the limited view of Lim_Pack [1]. However Par.Sib
      --  has a with clause for Lim_Pack [2] in its body, and thus needs the
      --  non-limited views of all entities from Lim_Pack.

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

      --  A common use of the limited-with is to have a limited-with in the
      --  package spec, and a normal with in its package body. For example:

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
        and then
          (Is_Immediately_Visible (P)
            or else (Is_Child_Package and then Is_Visible_Lib_Unit (P)))
      then

         --  The presence of both the limited and the analyzed nonlimited view
         --  may also be an error, such as an illegal context for a limited
         --  with_clause. In that case, do not process the context item at all.

         if Error_Posted (N) then
            return;
         end if;

         if Nkind (Unit (Cunit (Current_Sem_Unit))) = N_Package_Body then
            declare
               Item : Node_Id;
            begin
               Item := First (Context_Items (Cunit (Current_Sem_Unit)));
               while Present (Item) loop
                  if Nkind (Item) = N_With_Clause
                    and then Comes_From_Source (Item)
                    and then Entity (Name (Item)) = P
                  then
                     return;
                  end if;

                  Next (Item);
               end loop;
            end;

            --  If this is a child body, assume that the nonlimited with_clause
            --  appears in an ancestor. Could be refined ???

            if Is_Child_Unit
              (Defining_Entity
                 (Unit (Library_Unit (Cunit (Current_Sem_Unit)))))
            then
               return;
            end if;

         else

            --  If in package declaration, nonlimited view brought in from
            --  parent unit or some error condition.

            return;
         end if;
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
            Set_Is_Visible_Lib_Unit (P);

            if Is_Child_Package then
               Set_Is_Child_Unit (P);
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

                  --  Replace E in the homonyms list, so that the limited view
                  --  becomes available.

                  --  If the non-limited view is a record with an anonymous
                  --  self-referential component, the analysis of the record
                  --  declaration creates an incomplete type with the same name
                  --  in order to define an internal access type. The visible
                  --  entity is now the incomplete type, and that is the one to
                  --  replace in the visibility structure.

                  if E = Non_Limited_View (Lim_Typ)
                    or else
                      (Ekind (E) = E_Incomplete_Type
                        and then Full_View (E) = Non_Limited_View (Lim_Typ))
                  then
                     Set_Homonym (Lim_Typ, Homonym (Prev));
                     Set_Current_Entity (Lim_Typ);

                  else
                     loop
                        E := Homonym (Prev);

                        --  E may have been removed when installing a previous
                        --  limited_with_clause.

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

      --  If unit has not been analyzed in some previous context, check
      --  (imperfectly ???) whether it might need a body.

      if not Analyzed (P_Unit) then
         Check_Body_Required;
      end if;

      --  If the package in the limited_with clause is a child unit, the clause
      --  is unanalyzed and appears as a selected component. Recast it as an
      --  expanded name so that the entity can be properly set. Use entity of
      --  parent, if available, for higher ancestors in the name.

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
               --  semantic point of view (e.g. for ASIS queries). The unit
               --  entities are not fully analyzed, so we need to follow unit
               --  links in the tree.

               Set_Entity (Nam, Ent);

               Nam := Prefix (Nam);
               Ent :=
                 Defining_Entity
                   (Unit (Parent_Spec (Unit_Declaration_Node (Ent))));

               --  Set entity of last ancestor

               if Nkind (Nam) = N_Identifier then
                  Set_Entity (Nam, Ent);
               end if;
            end loop;
         end;
      end if;

      Set_Entity (Name (N), P);
      Set_From_Limited_With (P);
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

      --  We do not apply the restrictions to an internal unit unless we are
      --  compiling the internal unit as a main unit. This check is also
      --  skipped for dummy units (for missing packages).

      if Sloc (Uname) /= No_Location
        and then (not Is_Internal_Unit (Current_Sem_Unit)
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

         elsif not Is_Visible_Lib_Unit (Uname) then

            --  Abandon processing in case of previous errors

            if No (Scope (Uname)) then
               Check_Error_Detected;
               return;
            end if;

            Set_Is_Visible_Lib_Unit (Uname);

            --  If the unit is a wrapper package for a compilation unit that is
            --  a subprogrm instance, indicate that the instance itself is a
            --  visible unit. This is necessary if the instance is inlined.

            if Is_Wrapper_Package (Uname) then
               Set_Is_Visible_Lib_Unit (Related_Instance (Uname));
            end if;

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

               Set_Is_Visible_Lib_Unit
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
         Set_Is_Visible_Lib_Unit (Uname);

         if not Private_Present (With_Clause) or else Private_With_OK then
            Set_Is_Immediately_Visible (Uname);
         end if;

         Set_Context_Installed (With_Clause);
      end if;

      --   A with-clause overrides a with-type clause: there are no restric-
      --   tions on the use of package entities.

      if Ekind (Uname) = E_Package then
         Set_From_Limited_With (Uname, False);
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
        and then Is_Visible_Lib_Unit (Uname)
        and then Ada_Version >= Ada_2005
      then
         declare
            Decl1 : constant Node_Id := Unit_Declaration_Node (P);
            Decl2 : Node_Id;
            P2    : Entity_Id;
            U2    : Entity_Id;

         begin
            U2 := Homonym (Uname);
            while Present (U2) and then U2 /= Standard_Standard loop
               P2 := Scope (U2);
               Decl2  := Unit_Declaration_Node (P2);

               if Is_Child_Unit (U2) and then Is_Visible_Lib_Unit (U2) then
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

   ------------------------------------
   -- Is_Legal_Shadow_Entity_In_Body --
   ------------------------------------

   function Is_Legal_Shadow_Entity_In_Body (T : Entity_Id) return Boolean is
      C_Unit : constant Node_Id := Cunit (Current_Sem_Unit);
   begin
      return Nkind (Unit (C_Unit)) = N_Package_Body
        and then
          Has_With_Clause
            (C_Unit, Cunit_Entity (Get_Source_Unit (Non_Limited_View (T))));
   end Is_Legal_Shadow_Entity_In_Body;

   ----------------------
   -- Is_Ancestor_Unit --
   ----------------------

   function Is_Ancestor_Unit (U1 : Node_Id; U2 : Node_Id) return Boolean is
      E1 : constant Entity_Id := Defining_Entity (Unit (U1));
      E2 : Entity_Id;
   begin
      if Nkind_In (Unit (U2), N_Package_Body, N_Subprogram_Body) then
         E2 := Defining_Entity (Unit (Library_Unit (U2)));
         return Is_Ancestor_Package (E1, E2);
      else
         return False;
      end if;
   end Is_Ancestor_Unit;

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

   procedure Load_Needed_Body
     (N          : Node_Id;
      OK         : out Boolean;
      Do_Analyze : Boolean := True)
   is
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

         if Fatal_Error (Unum) /= Error_Detected or else Try_Semantics then
            if Debug_Flag_L then
               Write_Str ("*** Loaded generic body");
               Write_Eol;
            end if;

            if Do_Analyze then
               Semantics (Cunit (Unum));
            end if;
         end if;

         OK := True;
      end if;

      Style_Check := Save_Style_Check;
   end Load_Needed_Body;

   -------------------------
   -- Build_Limited_Views --
   -------------------------

   procedure Build_Limited_Views (N : Node_Id) is
      Unum        : constant Unit_Number_Type :=
                      Get_Source_Unit (Library_Unit (N));
      Is_Analyzed : constant Boolean := Analyzed (Cunit (Unum));

      Shadow_Pack : Entity_Id;
      --  The corresponding shadow entity of the withed package. This entity
      --  offers incomplete views of packages and types as well as abstract
      --  views of states and variables declared within.

      Last_Shadow : Entity_Id := Empty;
      --  The last shadow entity created by routine Build_Shadow_Entity

      procedure Build_Shadow_Entity
        (Ent       : Entity_Id;
         Scop      : Entity_Id;
         Shadow    : out Entity_Id;
         Is_Tagged : Boolean := False);
      --  Create a shadow entity that hides Ent and offers an abstract or
      --  incomplete view of Ent. Scop is the proper scope. Flag Is_Tagged
      --  should be set when Ent is a tagged type. The generated entity is
      --  added to Lim_Header. This routine updates the value of Last_Shadow.

      procedure Decorate_Package (Ent : Entity_Id; Scop : Entity_Id);
      --  Perform minimal decoration of a package or its corresponding shadow
      --  entity denoted by Ent. Scop is the proper scope.

      procedure Decorate_State (Ent : Entity_Id; Scop : Entity_Id);
      --  Perform full decoration of an abstract state or its corresponding
      --  shadow entity denoted by Ent. Scop is the proper scope.

      procedure Decorate_Type
        (Ent         : Entity_Id;
         Scop        : Entity_Id;
         Is_Tagged   : Boolean := False;
         Materialize : Boolean := False);
      --  Perform minimal decoration of a type or its corresponding shadow
      --  entity denoted by Ent. Scop is the proper scope. Flag Is_Tagged
      --  should be set when Ent is a tagged type. Flag Materialize should be
      --  set when Ent is a tagged type and its class-wide type needs to appear
      --  in the tree.

      procedure Decorate_Variable (Ent : Entity_Id; Scop : Entity_Id);
      --  Perform minimal decoration of a variable denoted by Ent. Scop is the
      --  proper scope.

      procedure Process_Declarations_And_States
        (Pack  : Entity_Id;
         Decls : List_Id;
         Scop  : Entity_Id;
         Create_Abstract_Views : Boolean);
      --  Inspect the states of package Pack and declarative list Decls. Create
      --  shadow entities for all nested packages, states, types and variables
      --  encountered. Scop is the proper scope. Create_Abstract_Views should
      --  be set when the abstract states and variables need to be processed.

      -------------------------
      -- Build_Shadow_Entity --
      -------------------------

      procedure Build_Shadow_Entity
        (Ent       : Entity_Id;
         Scop      : Entity_Id;
         Shadow    : out Entity_Id;
         Is_Tagged : Boolean := False)
      is
      begin
         Shadow := Make_Temporary (Sloc (Ent), 'Z');

         --  The shadow entity must share the same name and parent as the
         --  entity it hides.

         Set_Chars  (Shadow, Chars (Ent));
         Set_Parent (Shadow, Parent (Ent));

         --  The abstract view of a variable is a state, not another variable

         if Ekind (Ent) = E_Variable then
            Set_Ekind (Shadow, E_Abstract_State);
         else
            Set_Ekind (Shadow, Ekind (Ent));
         end if;

         Set_Is_Internal       (Shadow);
         Set_From_Limited_With (Shadow);

         --  Add the new shadow entity to the limited view of the package

         Last_Shadow := Shadow;
         Append_Entity (Shadow, Shadow_Pack);

         --  Perform context-specific decoration of the shadow entity

         if Ekind (Ent) = E_Abstract_State then
            Decorate_State       (Shadow, Scop);
            Set_Non_Limited_View (Shadow, Ent);

         elsif Ekind (Ent) = E_Package then
            Decorate_Package (Shadow, Scop);

         elsif Is_Type (Ent) then
            Decorate_Type        (Shadow, Scop, Is_Tagged);
            Set_Non_Limited_View (Shadow, Ent);

            if Is_Tagged then
               Set_Non_Limited_View
                 (Class_Wide_Type (Shadow), Class_Wide_Type (Ent));
            end if;

            if Is_Incomplete_Or_Private_Type (Ent) then
               Set_Private_Dependents (Shadow, New_Elmt_List);
            end if;

         elsif Ekind (Ent) = E_Variable then
            Decorate_State       (Shadow, Scop);
            Set_Non_Limited_View (Shadow, Ent);
         end if;
      end Build_Shadow_Entity;

      ----------------------
      -- Decorate_Package --
      ----------------------

      procedure Decorate_Package (Ent : Entity_Id; Scop : Entity_Id) is
      begin
         Set_Ekind (Ent, E_Package);
         Set_Etype (Ent, Standard_Void_Type);
         Set_Scope (Ent, Scop);
      end Decorate_Package;

      --------------------
      -- Decorate_State --
      --------------------

      procedure Decorate_State (Ent : Entity_Id; Scop : Entity_Id) is
      begin
         Set_Ekind               (Ent, E_Abstract_State);
         Set_Etype               (Ent, Standard_Void_Type);
         Set_Scope               (Ent, Scop);
         Set_Encapsulating_State (Ent, Empty);
      end Decorate_State;

      -------------------
      -- Decorate_Type --
      -------------------

      procedure Decorate_Type
        (Ent         : Entity_Id;
         Scop        : Entity_Id;
         Is_Tagged   : Boolean := False;
         Materialize : Boolean := False)
      is
         CW_Typ : Entity_Id;

      begin
         --  An unanalyzed type or a shadow entity of a type is treated as an
         --  incomplete type, and carries the corresponding attributes.

         Set_Ekind              (Ent, E_Incomplete_Type);
         Set_Etype              (Ent, Ent);
         Set_Full_View          (Ent, Empty);
         Set_Is_First_Subtype   (Ent);
         Set_Scope              (Ent, Scop);
         Set_Stored_Constraint  (Ent, No_Elist);
         Init_Size_Align        (Ent);

         if From_Limited_With (Ent) then
            Set_Private_Dependents (Ent, New_Elmt_List);
         end if;

         --  A tagged type and its corresponding shadow entity share one common
         --  class-wide type. The list of primitive operations for the shadow
         --  entity is empty.

         if Is_Tagged then
            Set_Is_Tagged_Type (Ent);
            Set_Direct_Primitive_Operations (Ent, New_Elmt_List);

            CW_Typ :=
              New_External_Entity
                (E_Void, Scope (Ent), Sloc (Ent), Ent, 'C', 0, 'T');

            Set_Class_Wide_Type (Ent, CW_Typ);

            --  Set parent to be the same as the parent of the tagged type.
            --  We need a parent field set, and it is supposed to point to
            --  the declaration of the type. The tagged type declaration
            --  essentially declares two separate types, the tagged type
            --  itself and the corresponding class-wide type, so it is
            --  reasonable for the parent fields to point to the declaration
            --  in both cases.

            Set_Parent (CW_Typ, Parent (Ent));

            Set_Ekind                     (CW_Typ, E_Class_Wide_Type);
            Set_Class_Wide_Type           (CW_Typ, CW_Typ);
            Set_Etype                     (CW_Typ, Ent);
            Set_Equivalent_Type           (CW_Typ, Empty);
            Set_From_Limited_With         (CW_Typ, From_Limited_With (Ent));
            Set_Has_Unknown_Discriminants (CW_Typ);
            Set_Is_First_Subtype          (CW_Typ);
            Set_Is_Tagged_Type            (CW_Typ);
            Set_Materialize_Entity        (CW_Typ, Materialize);
            Set_Scope                     (CW_Typ, Scop);
            Init_Size_Align               (CW_Typ);
         end if;
      end Decorate_Type;

      -----------------------
      -- Decorate_Variable --
      -----------------------

      procedure Decorate_Variable (Ent : Entity_Id; Scop : Entity_Id) is
      begin
         Set_Ekind (Ent, E_Variable);
         Set_Etype (Ent, Standard_Void_Type);
         Set_Scope (Ent, Scop);
      end Decorate_Variable;

      -------------------------------------
      -- Process_Declarations_And_States --
      -------------------------------------

      procedure Process_Declarations_And_States
        (Pack  : Entity_Id;
         Decls : List_Id;
         Scop  : Entity_Id;
         Create_Abstract_Views : Boolean)
      is
         procedure Find_And_Process_States;
         --  Determine whether package Pack defines abstract state either by
         --  using an aspect or a pragma. If this is the case, build shadow
         --  entities for all abstract states of Pack.

         procedure Process_States (States : Elist_Id);
         --  Generate shadow entities for all abstract states in list States

         -----------------------------
         -- Find_And_Process_States --
         -----------------------------

         procedure Find_And_Process_States is
            procedure Process_State (State : Node_Id);
            --  Generate shadow entities for a single abstract state or
            --  multiple states expressed as an aggregate.

            -------------------
            -- Process_State --
            -------------------

            procedure Process_State (State : Node_Id) is
               Loc   : constant Source_Ptr := Sloc (State);
               Decl  : Node_Id;
               Dummy : Entity_Id;
               Elmt  : Node_Id;
               Id    : Entity_Id;

            begin
               --  Multiple abstract states appear as an aggregate

               if Nkind (State) = N_Aggregate then
                  Elmt := First (Expressions (State));
                  while Present (Elmt) loop
                     Process_State (Elmt);
                     Next (Elmt);
                  end loop;

                  return;

               --  A null state has no abstract view

               elsif Nkind (State) = N_Null then
                  return;

               --  State declaration with various options appears as an
               --  extension aggregate.

               elsif Nkind (State) = N_Extension_Aggregate then
                  Decl := Ancestor_Part (State);

               --  Simple state declaration

               elsif Nkind (State) = N_Identifier then
                  Decl := State;

               --  Possibly an illegal state declaration

               else
                  return;
               end if;

               --  Abstract states are elaborated when the related pragma is
               --  elaborated. Since the withed package is not analyzed yet,
               --  the entities of the abstract states are not available. To
               --  overcome this complication, create the entities now and
               --  store them in their respective declarations. The entities
               --  are later used by routine Create_Abstract_State to declare
               --  and enter the states into visibility.

               if No (Entity (Decl)) then
                  Id := Make_Defining_Identifier (Loc, Chars (Decl));

                  Set_Entity     (Decl, Id);
                  Set_Parent     (Id, State);
                  Decorate_State (Id, Scop);

               --  Otherwise the package was previously withed

               else
                  Id := Entity (Decl);
               end if;

               Build_Shadow_Entity (Id, Scop, Dummy);
            end Process_State;

            --  Local variables

            Pack_Decl : constant Node_Id := Unit_Declaration_Node (Pack);
            Asp       : Node_Id;
            Decl      : Node_Id;

         --  Start of processing for Find_And_Process_States

         begin
            --  Find aspect Abstract_State

            Asp := First (Aspect_Specifications (Pack_Decl));
            while Present (Asp) loop
               if Chars (Identifier (Asp)) = Name_Abstract_State then
                  Process_State (Expression (Asp));

                  return;
               end if;

               Next (Asp);
            end loop;

            --  Find pragma Abstract_State by inspecting the declarations

            Decl := First (Decls);
            while Present (Decl) and then Nkind (Decl) = N_Pragma loop
               if Pragma_Name (Decl) = Name_Abstract_State then
                  Process_State
                    (Get_Pragma_Arg
                       (First (Pragma_Argument_Associations (Decl))));

                  return;
               end if;

               Next (Decl);
            end loop;
         end Find_And_Process_States;

         --------------------
         -- Process_States --
         --------------------

         procedure Process_States (States : Elist_Id) is
            Dummy : Entity_Id;
            Elmt  : Elmt_Id;

         begin
            Elmt := First_Elmt (States);
            while Present (Elmt) loop
               Build_Shadow_Entity (Node (Elmt), Scop, Dummy);

               Next_Elmt (Elmt);
            end loop;
         end Process_States;

         --  Local variables

         Is_Tagged : Boolean;
         Decl      : Node_Id;
         Def       : Node_Id;
         Def_Id    : Entity_Id;
         Shadow    : Entity_Id;

      --  Start of processing for Process_Declarations_And_States

      begin
         --  Build abstract views for all states defined in the package

         if Create_Abstract_Views then

            --  When a package has been analyzed, all states are stored in list
            --  Abstract_States. Generate the shadow entities directly.

            if Is_Analyzed then
               if Present (Abstract_States (Pack)) then
                  Process_States (Abstract_States (Pack));
               end if;

            --  The package may declare abstract states by using an aspect or a
            --  pragma. Attempt to locate one of these construct and if found,
            --  build the shadow entities.

            else
               Find_And_Process_States;
            end if;
         end if;

         --  Inspect the declarative list, looking for nested packages, types
         --  and variable declarations.

         Decl := First (Decls);
         while Present (Decl) loop

            --  Packages

            if Nkind (Decl) = N_Package_Declaration then
               Def_Id := Defining_Entity (Decl);

               --  Perform minor decoration when the withed package has not
               --  been analyzed.

               if not Is_Analyzed then
                  Decorate_Package (Def_Id, Scop);
               end if;

               --  Create a shadow entity that offers a limited view of all
               --  visible types declared within.

               Build_Shadow_Entity (Def_Id, Scop, Shadow);

               Process_Declarations_And_States
                 (Pack  => Def_Id,
                  Decls => Visible_Declarations (Specification (Decl)),
                  Scop  => Shadow,
                  Create_Abstract_Views => Create_Abstract_Views);

            --  Types

            elsif Nkind_In (Decl, N_Full_Type_Declaration,
                                  N_Incomplete_Type_Declaration,
                                  N_Private_Extension_Declaration,
                                  N_Private_Type_Declaration,
                                  N_Protected_Type_Declaration,
                                  N_Task_Type_Declaration)
            then
               Def_Id := Defining_Entity (Decl);

               --  Determine whether the type is tagged. Note that packages
               --  included via a limited with clause are not always analyzed,
               --  hence the tree lookup rather than the use of attribute
               --  Is_Tagged_Type.

               if Nkind (Decl) = N_Full_Type_Declaration then
                  Def := Type_Definition (Decl);

                  Is_Tagged :=
                     (Nkind (Def) = N_Record_Definition
                        and then Tagged_Present (Def))
                    or else
                     (Nkind (Def) = N_Derived_Type_Definition
                        and then Present (Record_Extension_Part (Def)));

               elsif Nkind_In (Decl, N_Incomplete_Type_Declaration,
                                     N_Private_Type_Declaration)
               then
                  Is_Tagged := Tagged_Present (Decl);

               elsif Nkind (Decl) = N_Private_Extension_Declaration then
                  Is_Tagged := True;

               else
                  Is_Tagged := False;
               end if;

               --  Perform minor decoration when the withed package has not
               --  been analyzed.

               if not Is_Analyzed then
                  Decorate_Type (Def_Id, Scop, Is_Tagged, True);
               end if;

               --  Create a shadow entity that hides the type and offers an
               --  incomplete view of the said type.

               Build_Shadow_Entity (Def_Id, Scop, Shadow, Is_Tagged);

            --  Variables

            elsif Create_Abstract_Views
              and then Nkind (Decl) = N_Object_Declaration
              and then not Constant_Present (Decl)
            then
               Def_Id := Defining_Entity (Decl);

               --  Perform minor decoration when the withed package has not
               --  been analyzed.

               if not Is_Analyzed then
                  Decorate_Variable (Def_Id, Scop);
               end if;

               --  Create a shadow entity that hides the variable and offers an
               --  abstract view of the said variable.

               Build_Shadow_Entity (Def_Id, Scop, Shadow);
            end if;

            Next (Decl);
         end loop;
      end Process_Declarations_And_States;

      --  Local variables

      Nam  : constant Node_Id   := Name (N);
      Pack : constant Entity_Id := Cunit_Entity (Unum);

      Last_Public_Shadow : Entity_Id := Empty;
      Private_Shadow     : Entity_Id;
      Spec               : Node_Id;

   --  Start of processing for Build_Limited_Views

   begin
      pragma Assert (Limited_Present (N));

      --  A library_item mentioned in a limited_with_clause is a package
      --  declaration, not a subprogram declaration, generic declaration,
      --  generic instantiation, or package renaming declaration.

      case Nkind (Unit (Library_Unit (N))) is
         when N_Package_Declaration =>
            null;

         when N_Subprogram_Declaration =>
            Error_Msg_N ("subprograms not allowed in limited with_clauses", N);
            return;

         when N_Generic_Package_Declaration
            | N_Generic_Subprogram_Declaration
         =>
            Error_Msg_N ("generics not allowed in limited with_clauses", N);
            return;

         when N_Generic_Instantiation =>
            Error_Msg_N
              ("generic instantiations not allowed in limited with_clauses",
               N);
            return;

         when N_Generic_Renaming_Declaration =>
            Error_Msg_N
              ("generic renamings not allowed in limited with_clauses", N);
            return;

         when N_Subprogram_Renaming_Declaration =>
            Error_Msg_N
              ("renamed subprograms not allowed in limited with_clauses", N);
            return;

         when N_Package_Renaming_Declaration =>
            Error_Msg_N
              ("renamed packages not allowed in limited with_clauses", N);
            return;

         when others =>
            raise Program_Error;
      end case;

      --  The withed unit may not be analyzed, but the with calause itself
      --  must be minimally decorated. This ensures that the checks on unused
      --  with clauses also process limieted withs.

      Set_Ekind (Pack, E_Package);
      Set_Etype (Pack, Standard_Void_Type);

      if Is_Entity_Name (Nam) then
         Set_Entity (Nam, Pack);

      elsif Nkind (Nam) = N_Selected_Component then
         Set_Entity (Selector_Name (Nam), Pack);
      end if;

      --  Check if the chain is already built

      Spec := Specification (Unit (Library_Unit (N)));

      if Limited_View_Installed (Spec) then
         return;
      end if;

      --  Create the shadow package wich hides the withed unit and provides
      --  incomplete view of all types and packages declared within.

      Shadow_Pack := Make_Temporary (Sloc (N), 'Z');
      Set_Ekind        (Shadow_Pack, E_Package);
      Set_Is_Internal  (Shadow_Pack);
      Set_Limited_View (Pack, Shadow_Pack);

      --  Inspect the abstract states and visible declarations of the withed
      --  unit and create shadow entities that hide existing packages, states,
      --  variables and types.

      Process_Declarations_And_States
        (Pack  => Pack,
         Decls => Visible_Declarations (Spec),
         Scop  => Pack,
         Create_Abstract_Views => True);

      Last_Public_Shadow := Last_Shadow;

      --  Ada 2005 (AI-262): Build the limited view of the private declarations
      --  to accommodate limited private with clauses.

      Process_Declarations_And_States
        (Pack  => Pack,
         Decls => Private_Declarations (Spec),
         Scop  => Pack,
         Create_Abstract_Views => False);

      if Present (Last_Public_Shadow) then
         Private_Shadow := Next_Entity (Last_Public_Shadow);
      else
         Private_Shadow := First_Entity (Shadow_Pack);
      end if;

      Set_First_Private_Entity (Shadow_Pack, Private_Shadow);
      Set_Limited_View_Installed (Spec);
   end Build_Limited_Views;

   ----------------------------
   -- Check_No_Elab_Code_All --
   ----------------------------

   procedure Check_No_Elab_Code_All (N : Node_Id) is
   begin
      if Present (No_Elab_Code_All_Pragma)
        and then In_Extended_Main_Source_Unit (N)
        and then Present (Context_Items (N))
      then
         declare
            CL : constant List_Id := Context_Items (N);
            CI : Node_Id;

         begin
            CI := First (CL);
            while Present (CI) loop
               if Nkind (CI) = N_With_Clause
                 and then not
                   No_Elab_Code_All (Get_Source_Unit (Library_Unit (CI)))

                 --  In GNATprove mode, some runtime units are implicitly
                 --  loaded to make their entities available for analysis. In
                 --  this case, ignore violations of No_Elaboration_Code_All
                 --  for this special analysis mode.

                 and then not
                   (GNATprove_Mode and then Implicit_With (CI))
               then
                  Error_Msg_Sloc := Sloc (No_Elab_Code_All_Pragma);
                  Error_Msg_N
                    ("violation of No_Elaboration_Code_All#", CI);
                  Error_Msg_NE
                    ("\unit& does not have No_Elaboration_Code_All",
                     CI, Entity (Name (CI)));
               end if;

               Next (CI);
            end loop;
         end;
      end if;
   end Check_No_Elab_Code_All;

   -------------------------------
   -- Check_Body_Needed_For_SAL --
   -------------------------------

   procedure Check_Body_Needed_For_SAL (Unit_Name : Entity_Id) is
      function Entity_Needs_Body (E : Entity_Id) return Boolean;
      --  Determine whether use of entity E might require the presence of its
      --  body. For a package this requires a recursive traversal of all nested
      --  declarations.

      -----------------------
      -- Entity_Needs_Body --
      -----------------------

      function Entity_Needs_Body (E : Entity_Id) return Boolean is
         Ent : Entity_Id;

      begin
         if Is_Subprogram (E) and then Has_Pragma_Inline (E) then
            return True;

         elsif Ekind_In (E, E_Generic_Function, E_Generic_Procedure) then

            --  A generic subprogram always requires the presence of its
            --  body because an instantiation needs both templates. The only
            --  exceptions is a generic subprogram renaming. In this case the
            --  body is needed only when the template is declared outside the
            --  compilation unit being checked.

            if Present (Renamed_Entity (E)) then
               return not Within_Scope (E, Unit_Name);
            else
               return True;
            end if;

         elsif Ekind (E) = E_Generic_Package
           and then
             Nkind (Unit_Declaration_Node (E)) = N_Generic_Package_Declaration
           and then Present (Corresponding_Body (Unit_Declaration_Node (E)))
         then
            return True;

         elsif Ekind (E) = E_Package
           and then Nkind (Unit_Declaration_Node (E)) = N_Package_Declaration
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
        and then Nkind (Unit_Declaration_Node (Unit_Name)) =
                                            N_Generic_Package_Declaration
        and then
          Present (Corresponding_Body (Unit_Declaration_Node (Unit_Name)))
      then
         Set_Body_Needed_For_SAL (Unit_Name);

      elsif Ekind_In (Unit_Name, E_Generic_Procedure, E_Generic_Function) then
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

      --  Prepare the removal of the shadow entities from visibility. The first
      --  element of the limited view is a header (an E_Package entity) that is
      --  used to reference the first shadow entity in the private part of the
      --  package

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
         --  If the limited_with_clause is in some other unit in the context
         --  then it is not visible in the main unit.

         if not In_Extended_Main_Source_Unit (N) then
            Set_Is_Immediately_Visible (P, False);
         end if;

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
               --  If the package has incomplete types, the limited view of the
               --  incomplete type is in fact never visible (AI05-129) but we
               --  have created a shadow entity E1 for it, that points to E2,
               --  a non-limited incomplete type. This in turn has a full view
               --  E3 that is the full declaration. There is a corresponding
               --  shadow entity E4. When reinstalling the non-limited view,
               --  E2 must become the current entity and E3 must be ignored.

               E := Non_Limited_View (Lim_Typ);

               if Present (Current_Entity (E))
                 and then Ekind (Current_Entity (E)) = E_Incomplete_Type
                 and then Full_View (Current_Entity (E)) = E
               then

                  --  Lim_Typ is the limited view of a full type declaration
                  --  that has a previous incomplete declaration, i.e. E3 from
                  --  the previous description. Nothing to insert.

                  null;

               else
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

                  --  Preserve structure of homonym chain

                  Set_Homonym (E, Homonym (Lim_Typ));
               end if;
            end if;

            Next_Entity (Lim_Typ);
         end loop;
      end if;

      --  Indicate that the limited view of the package is not installed

      Set_From_Limited_With      (P, False);
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

         --  This is the recursive call to remove the context of any higher
         --  level parent. This recursion ensures that all parents are removed
         --  in the reverse order of their installation.

         Remove_Parents (P);
      end if;
   end Remove_Parents;

   ---------------------------------
   -- Remove_Private_With_Clauses --
   ---------------------------------

   procedure Remove_Private_With_Clauses (Comp_Unit : Node_Id) is
      Item : Node_Id;

      function In_Regular_With_Clause (E : Entity_Id) return Boolean;
      --  Check whether a given unit appears in a regular with_clause. Used to
      --  determine whether a private_with_clause, implicit or explicit, should
      --  be ignored.

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

              --  The following guard is needed to ensure that the name has
              --  been properly analyzed before we go fetching its entity.

              and then Is_Entity_Name (Name (Item))
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
         if Nkind (Item) = N_With_Clause and then Private_Present (Item) then

            --  If private_with_clause is redundant, remove it from context,
            --  as a small optimization to subsequent handling of private_with
            --  clauses in other nested packages. We replace the clause with
            --  a null statement, which is otherwise ignored by the rest of
            --  the compiler, so that ASIS tools can reconstruct the source.

            if In_Regular_With_Clause (Entity (Name (Item))) then
               declare
                  Nxt : constant Node_Id := Next (Item);
               begin
                  Rewrite (Item, Make_Null_Statement (Sloc (Item)));
                  Analyze (Item);
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
   begin
      if Debug_Flag_I then
         Write_Str ("remove unit ");
         Write_Name (Chars (Unit_Name));
         Write_Str (" from visibility");
         Write_Eol;
      end if;

      Set_Is_Visible_Lib_Unit        (Unit_Name, False);
      Set_Is_Potentially_Use_Visible (Unit_Name, False);
      Set_Is_Immediately_Visible     (Unit_Name, False);

      --  If the unit is a wrapper package, the subprogram instance is
      --  what must be removed from visibility.
      --  Should we use Related_Instance instead???

      if Is_Wrapper_Package (Unit_Name) then
         Set_Is_Immediately_Visible (Current_Entity (Unit_Name), False);
      end if;
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
         while Present (Prev) and then Homonym (Prev) /= E loop
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
