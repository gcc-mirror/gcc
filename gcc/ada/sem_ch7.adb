------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S E M . C H 7                               --
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
-- to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, --
-- MA 02111-1307, USA.                                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package contains the routines to process package specifications and
--  bodies. The most important semantic aspects of package processing are the
--  handling of private and full declarations, and the construction of
--  dispatch tables for tagged types.

with Atree;    use Atree;
with Debug;    use Debug;
with Einfo;    use Einfo;
with Elists;   use Elists;
with Errout;   use Errout;
with Exp_Disp; use Exp_Disp;
with Exp_Dbug; use Exp_Dbug;
with Lib;      use Lib;
with Lib.Xref; use Lib.Xref;
with Namet;    use Namet;
with Nmake;    use Nmake;
with Nlists;   use Nlists;
with Opt;      use Opt;
with Output;   use Output;
with Sem;      use Sem;
with Sem_Cat;  use Sem_Cat;
with Sem_Ch3;  use Sem_Ch3;
with Sem_Ch6;  use Sem_Ch6;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Ch10; use Sem_Ch10;
with Sem_Ch12; use Sem_Ch12;
with Sem_Util; use Sem_Util;
with Sem_Warn; use Sem_Warn;
with Snames;   use Snames;
with Stand;    use Stand;
with Sinfo;    use Sinfo;
with Sinput;   use Sinput;
with Style;

package body Sem_Ch7 is

   -----------------------------------
   -- Handling private declarations --
   -----------------------------------

   --  The principle that each entity has a single defining occurrence clashes
   --  with the presence of two separate definitions for private types: the
   --  first is the private type declaration, and the second is the full type
   --  declaration. It is important that all references to the type point to
   --  the same defining occurrence, namely the first one. To enforce the two
   --  separate views of the entity, the corresponding information is swapped
   --  between the two declarations. Outside of the package, the defining
   --  occurrence only contains the private declaration information, while in
   --  the private part and the body of the package the defining occurrence
   --  contains the full declaration. To simplify the swap, the defining
   --  occurrence that currently holds the private declaration points to the
   --  full declaration. During semantic processing the defining occurrence
   --  also points to a list of private dependents, that is to say access types
   --  or composite types whose designated types or component types are
   --  subtypes or derived types of the private type in question. After the
   --  full declaration has been seen, the private dependents are updated to
   --  indicate that they have full definitions.

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Install_Package_Entity (Id : Entity_Id);
   --  Basic procedure for the previous two. Places one entity on its
   --  visibility chain, and recurses on the visible part if the entity
   --  is an inner package.

   function Is_Private_Base_Type (E : Entity_Id) return Boolean;
   --  True for a private type that is not a subtype

   function Is_Visible_Dependent (Dep : Entity_Id) return Boolean;
   --  If the private dependent is a private type whose full view is
   --  derived from the parent type, its full properties are revealed
   --  only if we are in the immediate scope of the private dependent.
   --  Should this predicate be tightened further???

   procedure Declare_Inherited_Private_Subprograms (Id : Entity_Id);
   --  Called upon entering the private part of a public child package
   --  and the body of a nested package, to potentially declare certain
   --  inherited subprograms that were inherited by types in the visible
   --  part, but whose declaration was deferred because the parent
   --  operation was private and not visible at that point. These
   --  subprograms are located by traversing the visible part declarations
   --  looking for non-private type extensions and then examining each of
   --  the primitive operations of such types to find those that were
   --  inherited but declared with a special internal name. Each such
   --  operation is now declared as an operation with a normal name (using
   --  the name of the parent operation) and replaces the previous implicit
   --  operation in the primitive operations list of the type. If the
   --  inherited private operation has been overridden, then it's
   --  replaced by the overriding operation.

   --------------------------
   -- Analyze_Package_Body --
   --------------------------

   procedure Analyze_Package_Body (N : Node_Id) is
      Loc              : constant Source_Ptr := Sloc (N);
      HSS              : Node_Id;
      Body_Id          : Entity_Id;
      Spec_Id          : Entity_Id;
      Last_Spec_Entity : Entity_Id;
      New_N            : Node_Id;
      Pack_Decl        : Node_Id;

      procedure Install_Composite_Operations (P : Entity_Id);
      --  Composite types declared in the current scope may depend on
      --  types that were private at the point of declaration, and whose
      --  full view is now in  scope. Indicate that the corresponding
      --  operations on the composite type are available.

      ----------------------------------
      -- Install_Composite_Operations --
      ----------------------------------

      procedure Install_Composite_Operations (P : Entity_Id) is
         Id : Entity_Id;

      begin
         Id := First_Entity (P);

         while Present (Id) loop

            if Is_Type (Id)
              and then (Is_Limited_Composite (Id)
                         or else Is_Private_Composite (Id))
              and then No (Private_Component (Id))
            then
               Set_Is_Limited_Composite (Id, False);
               Set_Is_Private_Composite (Id, False);
            end if;

            Next_Entity (Id);
         end loop;
      end Install_Composite_Operations;

   --  Start of processing for Analyze_Package_Body

   begin
      --  Find corresponding package specification, and establish the
      --  current scope. The visible defining entity for the package is the
      --  defining occurrence in the spec. On exit from the package body, all
      --  body declarations are attached to the defining entity for the body,
      --  but the later is never used for name resolution. In this fashion
      --  there is only one visible entity that denotes the package.

      if Debug_Flag_C then
         Write_Str ("====  Compiling package body ");
         Write_Name (Chars (Defining_Entity (N)));
         Write_Str (" from ");
         Write_Location (Loc);
         Write_Eol;
      end if;

      --  Set Body_Id. Note that this Will be reset to point to the
      --  generic copy later on in the generic case.

      Body_Id := Defining_Entity (N);

      if Present (Corresponding_Spec (N)) then

         --  Body is body of package instantiation. Corresponding spec
         --  has already been set.

         Spec_Id := Corresponding_Spec (N);
         Pack_Decl := Unit_Declaration_Node (Spec_Id);

      else
         Spec_Id := Current_Entity_In_Scope (Defining_Entity (N));

         if Present (Spec_Id)
           and then Is_Package (Spec_Id)
         then
            Pack_Decl := Unit_Declaration_Node (Spec_Id);

            if Nkind (Pack_Decl) = N_Package_Renaming_Declaration then
               Error_Msg_N ("cannot supply body for package renaming", N);
               return;

            elsif Present (Corresponding_Body (Pack_Decl)) then
               Error_Msg_N ("redefinition of package body", N);
               return;
            end if;

         else
            Error_Msg_N ("missing specification for package body", N);
            return;
         end if;

         if Is_Package (Spec_Id)
           and then
             (Scope (Spec_Id) = Standard_Standard
               or else Is_Child_Unit (Spec_Id))
           and then not Unit_Requires_Body (Spec_Id)
         then
            if Ada_Version = Ada_83 then
               Error_Msg_N
                 ("optional package body (not allowed in Ada 95)?", N);
            else
               Error_Msg_N
                 ("spec of this package does not allow a body", N);
            end if;
         end if;
      end if;

      Set_Is_Compilation_Unit (Body_Id, Is_Compilation_Unit (Spec_Id));
      Style.Check_Identifier (Body_Id, Spec_Id);

      if Is_Child_Unit (Spec_Id) then
         if Nkind (Parent (N)) /= N_Compilation_Unit then
            Error_Msg_NE
              ("body of child unit& cannot be an inner package", N, Spec_Id);
         end if;

         Set_Is_Child_Unit (Body_Id);
      end if;

      --  Generic package case

      if Ekind (Spec_Id) = E_Generic_Package then

         --  Disable expansion and perform semantic analysis on copy.
         --  The unannotated body will be used in all instantiations.

         Body_Id := Defining_Entity (N);
         Set_Ekind (Body_Id, E_Package_Body);
         Set_Scope (Body_Id, Scope (Spec_Id));
         Set_Body_Entity (Spec_Id, Body_Id);
         Set_Spec_Entity (Body_Id, Spec_Id);

         New_N := Copy_Generic_Node (N, Empty, Instantiating => False);
         Rewrite (N, New_N);

         --  Update Body_Id to point to the copied node for the remainder
         --  of the processing.

         Body_Id := Defining_Entity (N);
         Start_Generic;
      end if;

      --  The Body_Id is that of the copied node in the generic case, the
      --  current node otherwise. Note that N was rewritten above, so we
      --  must be sure to get the latest Body_Id value.

      Set_Ekind (Body_Id, E_Package_Body);
      Set_Body_Entity (Spec_Id, Body_Id);
      Set_Spec_Entity (Body_Id, Spec_Id);

      --  Defining name for the package body is not a visible entity: Only
      --  the defining name for the declaration is visible.

      Set_Etype (Body_Id, Standard_Void_Type);
      Set_Scope (Body_Id, Scope (Spec_Id));
      Set_Corresponding_Spec (N, Spec_Id);
      Set_Corresponding_Body (Pack_Decl, Body_Id);

      --  The body entity is not used for semantics or code generation, but
      --  it is attached to the entity list of the enclosing scope to simplify
      --  the listing of back-annotations for the types it main contain.

      if Scope (Spec_Id) /= Standard_Standard then
         Append_Entity (Body_Id, Scope (Spec_Id));
      end if;

      --  Indicate that we are currently compiling the body of the package

      Set_In_Package_Body (Spec_Id);
      Set_Has_Completion (Spec_Id);
      Last_Spec_Entity := Last_Entity (Spec_Id);

      New_Scope (Spec_Id);

      Set_Categorization_From_Pragmas (N);

      Install_Visible_Declarations (Spec_Id);
      Install_Private_Declarations (Spec_Id);
      Install_Private_With_Clauses (Spec_Id);
      Install_Composite_Operations (Spec_Id);

      if Ekind (Spec_Id) = E_Generic_Package then
         Set_Use (Generic_Formal_Declarations (Pack_Decl));
      end if;

      Set_Use (Visible_Declarations (Specification (Pack_Decl)));
      Set_Use (Private_Declarations (Specification (Pack_Decl)));

      --  This is a nested package, so it may be necessary to declare
      --  certain inherited subprograms that are not yet visible because
      --  the parent type's subprograms are now visible.

      if Ekind (Scope (Spec_Id)) = E_Package
        and then Scope (Spec_Id) /= Standard_Standard
      then
         Declare_Inherited_Private_Subprograms (Spec_Id);
      end if;

      if Present (Declarations (N)) then
         Analyze_Declarations (Declarations (N));
      end if;

      HSS := Handled_Statement_Sequence (N);

      if Present (HSS) then
         Process_End_Label (HSS, 't', Spec_Id);
         Analyze (HSS);

         --  Check that elaboration code in a preelaborable package body is
         --  empty other than null statements and labels (RM 10.2.1(6)).

         Validate_Null_Statement_Sequence (N);
      end if;

      Validate_Categorization_Dependency (N, Spec_Id);
      Check_Completion (Body_Id);

      --  Generate start of body reference. Note that we do this fairly late,
      --  because the call will use In_Extended_Main_Source_Unit as a check,
      --  and we want to make sure that Corresponding_Stub links are set

      Generate_Reference (Spec_Id, Body_Id, 'b', Set_Ref => False);

      --  For a generic package, collect global references and mark
      --  them on the original body so that they are not resolved
      --  again at the point of instantiation.

      if Ekind (Spec_Id) /= E_Package then
         Save_Global_References (Original_Node (N));
         End_Generic;
      end if;

      --  The entities of the package body have so far been chained onto
      --  the declaration chain for the spec. That's been fine while we
      --  were in the body, since we wanted them to be visible, but now
      --  that we are leaving the package body, they are no longer visible,
      --  so we remove them from the entity chain of the package spec entity,
      --  and copy them to the entity chain of the package body entity, where
      --  they will never again be visible.

      if Present (Last_Spec_Entity) then
         Set_First_Entity (Body_Id, Next_Entity (Last_Spec_Entity));
         Set_Next_Entity (Last_Spec_Entity, Empty);
         Set_Last_Entity (Body_Id, Last_Entity (Spec_Id));
         Set_Last_Entity (Spec_Id, Last_Spec_Entity);

      else
         Set_First_Entity (Body_Id, First_Entity (Spec_Id));
         Set_Last_Entity  (Body_Id, Last_Entity  (Spec_Id));
         Set_First_Entity (Spec_Id, Empty);
         Set_Last_Entity  (Spec_Id, Empty);
      end if;

      End_Package_Scope (Spec_Id);

      --  All entities declared in body are not visible

      declare
         E : Entity_Id;

      begin
         E := First_Entity (Body_Id);

         while Present (E) loop
            Set_Is_Immediately_Visible (E, False);
            Set_Is_Potentially_Use_Visible (E, False);
            Set_Is_Hidden (E);

            --  Child units may appear on the entity list (for example if
            --  they appear in the context of a subunit) but they are not
            --  body entities.

            if not Is_Child_Unit (E) then
               Set_Is_Package_Body_Entity (E);
            end if;

            Next_Entity (E);
         end loop;
      end;

      Check_References (Body_Id);

      --  For a generic unit, check that the formal parameters are referenced,
      --  and that local variables are used, as for regular packages.

      if Ekind (Spec_Id) = E_Generic_Package then
         Check_References (Spec_Id);
      end if;

      --  The processing so far has made all entities of the package body
      --  public (i.e. externally visible to the linker). This is in general
      --  necessary, since inlined or generic bodies, for which code is
      --  generated in other units, may need to see these entities. The
      --  following loop runs backwards from the end of the entities of the
      --  package body making these entities invisible until we reach a
      --  referencer, i.e. a declaration that could reference a previous
      --  declaration, a generic body or an inlined body, or a stub (which
      --  may contain either of these). This is of course an approximation,
      --  but it is conservative and definitely correct.

      --  We only do this at the outer (library) level non-generic packages.
      --  The reason is simply to cut down on the number of external symbols
      --  generated, so this is simply an optimization of the efficiency
      --  of the compilation process. It has no other effect.

      if (Scope (Spec_Id) = Standard_Standard or else Is_Child_Unit (Spec_Id))
        and then not Is_Generic_Unit (Spec_Id)
        and then Present (Declarations (N))
      then
         Make_Non_Public_Where_Possible : declare

            function Has_Referencer
              (L     : List_Id;
               Outer : Boolean)
               return  Boolean;
            --  Traverse the given list of declarations in reverse order.
            --  Return True as soon as a referencer is reached. Return
            --  False if none is found. The Outer parameter is True for
            --  the outer level call, and False for inner level calls for
            --  nested packages. If Outer is True, then any entities up
            --  to the point of hitting a referencer get their Is_Public
            --  flag cleared, so that the entities will be treated as
            --  static entities in the C sense, and need not have fully
            --  qualified names. For inner levels, we need all names to
            --  be fully qualified to deal with the same name appearing
            --  in parallel packages (right now this is tied to their
            --  being external).

            --------------------
            -- Has_Referencer --
            --------------------

            function Has_Referencer
              (L     : List_Id;
               Outer : Boolean)
               return  Boolean
            is
               D : Node_Id;
               E : Entity_Id;
               K : Node_Kind;
               S : Entity_Id;

            begin
               if No (L) then
                  return False;
               end if;

               D := Last (L);

               while Present (D) loop
                  K := Nkind (D);

                  if K in N_Body_Stub then
                     return True;

                  elsif K = N_Subprogram_Body then
                     if Acts_As_Spec (D) then
                        E := Defining_Entity (D);

                        --  An inlined body acts as a referencer. Note also
                        --  that we never reset Is_Public for an inlined
                        --  subprogram. Gigi requires Is_Public to be set.

                        --  Note that we test Has_Pragma_Inline here rather
                        --  than Is_Inlined. We are compiling this for a
                        --  client, and it is the client who will decide
                        --  if actual inlining should occur, so we need to
                        --  assume that the procedure could be inlined for
                        --  the purpose of accessing global entities.

                        if Has_Pragma_Inline (E) then
                           return True;
                        else
                           Set_Is_Public (E, False);
                        end if;

                     else
                        E := Corresponding_Spec (D);

                        if Present (E)
                          and then (Is_Generic_Unit (E)
                                     or else Has_Pragma_Inline (E)
                                     or else Is_Inlined (E))
                        then
                           return True;
                        end if;
                     end if;

                  --  Processing for package bodies

                  elsif K = N_Package_Body
                    and then Present (Corresponding_Spec (D))
                  then
                     E := Corresponding_Spec (D);

                     --  Generic package body is a referencer. It would
                     --  seem that we only have to consider generics that
                     --  can be exported, i.e. where the corresponding spec
                     --  is the spec of the current package, but because of
                     --  nested instantiations, a fully private generic
                     --  body may export other private body entities.

                     if Is_Generic_Unit (E) then
                        return True;

                     --  For non-generic package body, recurse into body
                     --  unless this is an instance, we ignore instances
                     --  since they cannot have references that affect
                     --  outer entities.

                     elsif not Is_Generic_Instance (E) then
                        if Has_Referencer
                             (Declarations (D), Outer => False)
                        then
                           return True;
                        end if;
                     end if;

                  --  Processing for package specs, recurse into declarations.
                  --  Again we skip this for the case of generic instances.

                  elsif K = N_Package_Declaration then
                     S := Specification (D);

                     if not Is_Generic_Unit (Defining_Entity (S)) then
                        if Has_Referencer
                             (Private_Declarations (S), Outer => False)
                        then
                           return True;
                        elsif Has_Referencer
                               (Visible_Declarations (S), Outer => False)
                        then
                           return True;
                        end if;
                     end if;

                  --  Objects and exceptions need not be public if we have
                  --  not encountered a referencer so far. We only reset
                  --  the flag for outer level entities that are not
                  --  imported/exported, and which have no interface name.

                  elsif K = N_Object_Declaration
                    or else K = N_Exception_Declaration
                    or else K = N_Subprogram_Declaration
                  then
                     E := Defining_Entity (D);

                     if Outer
                       and then not Is_Imported (E)
                       and then not Is_Exported (E)
                       and then No (Interface_Name (E))
                     then
                        Set_Is_Public (E, False);
                     end if;
                  end if;

                  Prev (D);
               end loop;

               return False;
            end Has_Referencer;

         --  Start of processing for Make_Non_Public_Where_Possible

         begin
            declare
               Discard : Boolean;
               pragma Warnings (Off, Discard);

            begin
               Discard := Has_Referencer (Declarations (N), Outer => True);
            end;
         end Make_Non_Public_Where_Possible;
      end if;

      --  If expander is not active, then here is where we turn off the
      --  In_Package_Body flag, otherwise it is turned off at the end of
      --  the corresponding expansion routine. If this is an instance body,
      --  we need to qualify names of local entities, because the body may
      --  have been compiled as a preliminary to another instantiation.

      if not Expander_Active then
         Set_In_Package_Body (Spec_Id, False);

         if Is_Generic_Instance (Spec_Id)
           and then Operating_Mode = Generate_Code
         then
            Qualify_Entity_Names (N);
         end if;
      end if;
   end Analyze_Package_Body;

   ---------------------------------
   -- Analyze_Package_Declaration --
   ---------------------------------

   procedure Analyze_Package_Declaration (N : Node_Id) is
      Id : constant Node_Id := Defining_Entity (N);
      PF : Boolean;

   begin
      Generate_Definition (Id);
      Enter_Name (Id);
      Set_Ekind (Id, E_Package);
      Set_Etype (Id, Standard_Void_Type);

      New_Scope (Id);

      PF := Is_Pure (Enclosing_Lib_Unit_Entity);
      Set_Is_Pure (Id, PF);

      Set_Categorization_From_Pragmas (N);

      if Debug_Flag_C then
         Write_Str ("====  Compiling package spec ");
         Write_Name (Chars (Id));
         Write_Str (" from ");
         Write_Location (Sloc (N));
         Write_Eol;
      end if;

      Analyze (Specification (N));
      Validate_Categorization_Dependency (N, Id);
      End_Package_Scope (Id);

      --  For a compilation unit, indicate whether it needs a body, and
      --  whether elaboration warnings may be meaningful on it.

      if Nkind (Parent (N)) = N_Compilation_Unit then
         Set_Body_Required (Parent (N), Unit_Requires_Body (Id));

         if not Body_Required (Parent (N)) then
            Set_Suppress_Elaboration_Warnings (Id);
         end if;

         Validate_RT_RAT_Component (N);
      end if;
   end Analyze_Package_Declaration;

   -----------------------------------
   -- Analyze_Package_Specification --
   -----------------------------------

   --  Note that this code is shared for the analysis of generic package
   --  specs (see Sem_Ch12.Analyze_Generic_Package_Declaration for details).

   procedure Analyze_Package_Specification (N : Node_Id) is
      Id           : constant Entity_Id  := Defining_Entity (N);
      Orig_Decl    : constant Node_Id    := Original_Node (Parent (N));
      Vis_Decls    : constant List_Id    := Visible_Declarations (N);
      Priv_Decls   : constant List_Id    := Private_Declarations (N);
      E            : Entity_Id;
      L            : Entity_Id;
      Public_Child : Boolean;

      procedure Clear_Constants (Id : Entity_Id; FE : Entity_Id);
      --  Clears constant indications (Never_Set_In_Source, Constant_Value,
      --  and Is_True_Constant) on all variables that are entities of Id,
      --  and on the chain whose first element is FE. A recursive call is
      --  made for all packages and generic packages.

      procedure Generate_Parent_References;
      --  For a child unit, generate references to parent units, for
      --  GPS navigation purposes.

      function Is_Public_Child (Child, Unit : Entity_Id) return Boolean;
      --  Child and Unit are entities of compilation units. True if Child
      --  is a public child of Parent as defined in 10.1.1

      procedure Inspect_Deferred_Constant_Completion;
      --  Examines the deferred constants in the private part of the package
      --  specification. Emits the error message "constant declaration requires
      --  initialization expression " if not completed by an Import pragma.

      procedure Inspect_Unchecked_Union_Completion (Decls : List_Id);
      --  Detects all incomplete or private type declarations having a known
      --  discriminant part that are completed by an Unchecked_Union. Emits
      --  the error message "Unchecked_Union may not complete discriminated
      --  partial view".

      ---------------------
      -- Clear_Constants --
      ---------------------

      procedure Clear_Constants (Id : Entity_Id; FE : Entity_Id) is
         E : Entity_Id;

      begin
         --  Ignore package renamings, not interesting and they can
         --  cause self referential loops in the code below.

         if Nkind (Parent (Id)) = N_Package_Renaming_Declaration then
            return;
         end if;

         --  Note: in the loop below, the check for Next_Entity pointing
         --  back to the package entity seems very odd, but it is needed,
         --  because this kind of unexpected circularity does occur ???

         E := FE;
         while Present (E) and then E /= Id loop
            if Ekind (E) = E_Variable then
               Set_Never_Set_In_Source (E, False);
               Set_Is_True_Constant    (E, False);
               Set_Current_Value       (E, Empty);
               Set_Is_Known_Non_Null   (E, False);

            elsif Ekind (E) = E_Package
                    or else
                  Ekind (E) = E_Generic_Package
            then
               Clear_Constants (E, First_Entity (E));
               Clear_Constants (E, First_Private_Entity (E));
            end if;

            Next_Entity (E);
         end loop;
      end Clear_Constants;

      --------------------------------
      -- Generate_Parent_References --
      --------------------------------

      procedure Generate_Parent_References is
         Decl : constant Node_Id := Parent (N);

      begin
         if Id = Cunit_Entity (Main_Unit)
           or else Parent (Decl) = Library_Unit (Cunit (Main_Unit))
         then
            Generate_Reference (Id, Scope (Id), 'k', False);

         elsif Nkind (Unit (Cunit (Main_Unit))) /= N_Subprogram_Body
           and then Nkind (Unit (Cunit (Main_Unit))) /= N_Subunit
         then
            --  If current unit is an ancestor of main unit, generate
            --  a reference to its own parent.

            declare
               U         : Node_Id;
               Main_Spec : Node_Id := Unit (Cunit (Main_Unit));

            begin
               if Nkind (Main_Spec) = N_Package_Body then
                  Main_Spec := Unit (Library_Unit (Cunit (Main_Unit)));
               end if;

               U := Parent_Spec (Main_Spec);
               while Present (U) loop
                  if U = Parent (Decl) then
                     Generate_Reference (Id, Scope (Id), 'k',  False);
                     exit;

                  elsif Nkind (Unit (U)) = N_Package_Body then
                     exit;

                  else
                     U := Parent_Spec (Unit (U));
                  end if;
               end loop;
            end;
         end if;
      end Generate_Parent_References;

      ---------------------
      -- Is_Public_Child --
      ---------------------

      function Is_Public_Child (Child, Unit : Entity_Id) return Boolean is
      begin
         if not Is_Private_Descendant (Child) then
            return True;
         else
            if Child = Unit then
               return not Private_Present (
                 Parent (Unit_Declaration_Node (Child)));
            else
               return Is_Public_Child (Scope (Child), Unit);
            end if;
         end if;
      end Is_Public_Child;

      ------------------------------------------
      -- Inspect_Deferred_Constant_Completion --
      ------------------------------------------

      procedure Inspect_Deferred_Constant_Completion is
         Decl   : Node_Id;

      begin
         Decl := First (Priv_Decls);
         while Present (Decl) loop

            --  Deferred constant signature

            if Nkind (Decl) = N_Object_Declaration
              and then Constant_Present (Decl)
              and then No (Expression (Decl))

               --  No need to check internally generated constants

              and then Comes_From_Source (Decl)

               --  The constant is not completed. A full object declaration
               --  or a pragma Import complete a deferred constant.

              and then not Has_Completion (Defining_Identifier (Decl))
            then
               Error_Msg_N
                 ("constant declaration requires initialization expression",
                 Defining_Identifier (Decl));
            end if;

            Decl := Next (Decl);
         end loop;
      end Inspect_Deferred_Constant_Completion;

      ----------------------------------------
      -- Inspect_Unchecked_Union_Completion --
      ----------------------------------------

      procedure Inspect_Unchecked_Union_Completion (Decls : List_Id) is
         Decl : Node_Id := First (Decls);

      begin
         while Present (Decl) loop

            --  We are looking at an incomplete or private type declaration
            --  with a known_discriminant_part whose full view is an
            --  Unchecked_Union.

            if (Nkind (Decl) = N_Incomplete_Type_Declaration
                  or else
                Nkind (Decl) = N_Private_Type_Declaration)
              and then Has_Discriminants (Defining_Identifier (Decl))
              and then Present (Full_View (Defining_Identifier (Decl)))
              and then Is_Unchecked_Union
                (Full_View (Defining_Identifier (Decl)))
            then
               Error_Msg_N ("completion of discriminated partial view" &
                 " cannot be an Unchecked_Union",
                 Full_View (Defining_Identifier (Decl)));
            end if;

            Next (Decl);
         end loop;
      end Inspect_Unchecked_Union_Completion;

   --  Start of processing for Analyze_Package_Specification

   begin
      if Present (Vis_Decls) then
         Analyze_Declarations (Vis_Decls);
      end if;

      --  Verify that incomplete types have received full declarations

      E := First_Entity (Id);
      while Present (E) loop
         if Ekind (E) = E_Incomplete_Type
           and then No (Full_View (E))
         then
            Error_Msg_N ("no declaration in visible part for incomplete}", E);
         end if;

         Next_Entity (E);
      end loop;

      if Is_Remote_Call_Interface (Id)
         and then Nkind (Parent (Parent (N))) = N_Compilation_Unit
      then
         Validate_RCI_Declarations (Id);
      end if;

      --  Save global references in the visible declarations, before
      --  installing private declarations of parent unit if there is one,
      --  because the privacy status of types defined in the parent will
      --  change. This is only relevant for generic child units, but is
      --  done in all cases for uniformity.

      if Ekind (Id) = E_Generic_Package
        and then Nkind (Orig_Decl) = N_Generic_Package_Declaration
      then
         declare
            Orig_Spec : constant Node_Id    := Specification (Orig_Decl);
            Save_Priv : constant List_Id := Private_Declarations (Orig_Spec);

         begin
            Set_Private_Declarations (Orig_Spec, Empty_List);
            Save_Global_References   (Orig_Decl);
            Set_Private_Declarations (Orig_Spec, Save_Priv);
         end;
      end if;

      --  If package is a public child unit, then make the private
      --  declarations of the parent visible.

      Public_Child := False;

      declare
         Par       : Entity_Id;
         Pack_Decl : Node_Id;
         Par_Spec  : Node_Id;

      begin
         Par := Id;
         Par_Spec := Parent_Spec (Parent (N));

         --  If the package is formal package of an enclosing generic, is is
         --  transformed into a local generic declaration, and compiled to make
         --  its spec available. We need to retrieve the original generic to
         --  determine whether it is a child unit, and install its parents.

         if No (Par_Spec)
           and then
             Nkind (Original_Node (Parent (N))) = N_Formal_Package_Declaration
         then
            Par := Entity (Name (Original_Node (Parent (N))));
            Par_Spec := Parent_Spec (Unit_Declaration_Node (Par));
         end if;

         if Present (Par_Spec) then
            Generate_Parent_References;

            while Scope (Par) /= Standard_Standard
              and then Is_Public_Child (Id, Par)
            loop
               Public_Child := True;
               Par := Scope (Par);
               Install_Private_Declarations (Par);
               Install_Private_With_Clauses (Par);
               Pack_Decl := Unit_Declaration_Node (Par);
               Set_Use (Private_Declarations (Specification (Pack_Decl)));
            end loop;
         end if;
      end;

      if Is_Compilation_Unit (Id) then
         Install_Private_With_Clauses (Id);
      end if;

      --  Analyze private part if present. The flag In_Private_Part is
      --  reset in End_Package_Scope.

      L := Last_Entity (Id);

      if Present (Priv_Decls) then
         Set_In_Private_Part (Id);

         --  Upon entering a public child's private part, it may be
         --  necessary to declare subprograms that were derived in
         --  the package visible part but not yet made visible.

         if Public_Child then
            Declare_Inherited_Private_Subprograms (Id);
         end if;

         Analyze_Declarations (Priv_Decls);

         --  Check the private declarations for incomplete deferred constants

         Inspect_Deferred_Constant_Completion;

         --  The first private entity is the immediate follower of the last
         --  visible entity, if there was one.

         if Present (L) then
            Set_First_Private_Entity (Id, Next_Entity (L));
         else
            Set_First_Private_Entity (Id, First_Entity (Id));
         end if;

      --  There may be inherited private subprograms that need to be
      --  declared, even in the absence of an explicit private part.
      --  If there are any public declarations in the package and
      --  the package is a public child unit, then an implicit private
      --  part is assumed.

      elsif Present (L) and then Public_Child then
         Set_In_Private_Part (Id);
         Declare_Inherited_Private_Subprograms (Id);
         Set_First_Private_Entity (Id, Next_Entity (L));
      end if;

      --  Check rule of 3.6(11), which in general requires
      --  waiting till all full types have been seen.

      E := First_Entity (Id);
      while Present (E) loop
         if Ekind (E) = E_Record_Type or else Ekind (E) = E_Array_Type then
            Check_Aliased_Component_Types (E);
         end if;

         Next_Entity (E);
      end loop;

      --  Ada 2005 (AI-216): The completion of an incomplete or private type
      --  declaration having a known_discriminant_part shall not be an
      --  Unchecked_Union type.

      if Present (Vis_Decls) then
         Inspect_Unchecked_Union_Completion (Vis_Decls);
      end if;

      if Present (Priv_Decls) then
         Inspect_Unchecked_Union_Completion (Priv_Decls);
      end if;

      if Ekind (Id) = E_Generic_Package
        and then Nkind (Orig_Decl) = N_Generic_Package_Declaration
        and then Present (Priv_Decls)
      then
         --  Save global references in private declarations, ignoring the
         --  visible declarations that were processed earlier.

         declare
            Orig_Spec : constant Node_Id := Specification (Orig_Decl);
            Save_Vis  : constant List_Id := Visible_Declarations (Orig_Spec);
            Save_Form : constant List_Id :=
                          Generic_Formal_Declarations (Orig_Decl);

         begin
            Set_Visible_Declarations        (Orig_Spec, Empty_List);
            Set_Generic_Formal_Declarations (Orig_Decl, Empty_List);
            Save_Global_References          (Orig_Decl);
            Set_Generic_Formal_Declarations (Orig_Decl, Save_Form);
            Set_Visible_Declarations        (Orig_Spec, Save_Vis);
         end;
      end if;

      Process_End_Label (N, 'e', Id);

      --  For the case of a library level package, we must go through all
      --  the entities clearing the indications that the value may be
      --  constant and not modified. Why? Because any client of this
      --  package may modify these values freely from anywhere. This
      --  also applies to any nested packages or generic packages.

      --  For now we unconditionally clear constants for packages that
      --  are instances of generic packages. The reason is that we do not
      --  have the body yet, and we otherwise think things are unreferenced
      --  when they are not. This should be fixed sometime (the effect is
      --  not terrible, we just lose some warnings, and also some cases
      --  of value propagation) ???

      if Is_Library_Level_Entity (Id)
        or else Is_Generic_Instance (Id)
      then
         Clear_Constants (Id, First_Entity (Id));
         Clear_Constants (Id, First_Private_Entity (Id));
      end if;
   end Analyze_Package_Specification;

   --------------------------------------
   -- Analyze_Private_Type_Declaration --
   --------------------------------------

   procedure Analyze_Private_Type_Declaration (N : Node_Id) is
      PF : constant Boolean   := Is_Pure (Enclosing_Lib_Unit_Entity);
      Id : constant Entity_Id := Defining_Identifier (N);

   begin
      Generate_Definition (Id);
      Set_Is_Pure         (Id, PF);
      Init_Size_Align     (Id);

      if (Ekind (Current_Scope) /= E_Package
          and then Ekind (Current_Scope) /= E_Generic_Package)
        or else In_Private_Part (Current_Scope)
      then
         Error_Msg_N ("invalid context for private declaration", N);
      end if;

      New_Private_Type (N, Id, N);
      Set_Depends_On_Private (Id);
   end Analyze_Private_Type_Declaration;

   -------------------------------------------
   -- Declare_Inherited_Private_Subprograms --
   -------------------------------------------

   procedure Declare_Inherited_Private_Subprograms (Id : Entity_Id) is
      E              : Entity_Id;
      Op_List        : Elist_Id;
      Op_Elmt        : Elmt_Id;
      Op_Elmt_2      : Elmt_Id;
      Prim_Op        : Entity_Id;
      New_Op         : Entity_Id := Empty;
      Parent_Subp    : Entity_Id;
      Found_Explicit : Boolean;
      Decl_Privates  : Boolean;

      function Has_Overriding_Pragma (Subp : Entity_Id) return Boolean;
      --  Check whether a pragma Overriding has been provided for a primitive
      --  operation that is found to be overriding in the private part.

      function Is_Primitive_Of (T : Entity_Id; S : Entity_Id) return Boolean;
      --  Check whether an inherited subprogram is an operation of an
      --  untagged derived type.

      ---------------------------
      -- Has_Overriding_Pragma --
      ---------------------------

      function Has_Overriding_Pragma (Subp : Entity_Id) return Boolean is
         Decl : constant Node_Id := Unit_Declaration_Node (Subp);
         Prag : Node_Id;

      begin
         if No (Decl)
           or else Nkind (Decl) /= N_Subprogram_Declaration
           or else No (Next (Decl))
         then
            return False;

         else
            Prag := Next (Decl);

            while Present (Prag)
              and then Nkind (Prag) = N_Pragma
            loop
               if Chars (Prag) = Name_Overriding
                 or else Chars (Prag) = Name_Optional_Overriding
               then
                  return True;
               else
                  Next (Prag);
               end if;
            end loop;
         end if;

         return False;
      end Has_Overriding_Pragma;

      ---------------------
      -- Is_Primitive_Of --
      ---------------------

      function Is_Primitive_Of (T : Entity_Id; S : Entity_Id) return Boolean is
         Formal : Entity_Id;

      begin
         if Etype (S) = T then
            return True;

         else
            Formal := First_Formal (S);

            while Present (Formal) loop
               if Etype (Formal) = T then
                  return True;
               end if;

               Next_Formal (Formal);
            end loop;

            return False;
         end if;
      end Is_Primitive_Of;

   --  Start of processing for Declare_Inherited_Private_Subprograms

   begin
      E := First_Entity (Id);
      while Present (E) loop

         --  If the entity is a nonprivate type extension whose parent
         --  type is declared in an open scope, then the type may have
         --  inherited operations that now need to be made visible.
         --  Ditto if the entity is a formal derived type in a child unit.

         if ((Is_Derived_Type (E) and then not Is_Private_Type (E))
               or else
                 (Nkind (Parent (E)) = N_Private_Extension_Declaration
                   and then Is_Generic_Type (E)))
           and then In_Open_Scopes (Scope (Etype (E)))
           and then E = Base_Type (E)
         then
            if Is_Tagged_Type (E) then
               Op_List       := Primitive_Operations (E);
               New_Op        := Empty;
               Decl_Privates := False;

               Op_Elmt := First_Elmt (Op_List);
               while Present (Op_Elmt) loop
                  Prim_Op := Node (Op_Elmt);

                  --  If the primitive operation is an implicit operation
                  --  with an internal name whose parent operation has
                  --  a normal name, then we now need to either declare the
                  --  operation (i.e., make it visible), or replace it
                  --  by an overriding operation if one exists.

                  if Present (Alias (Prim_Op))
                    and then not Comes_From_Source (Prim_Op)
                    and then Is_Internal_Name (Chars (Prim_Op))
                    and then not Is_Internal_Name (Chars (Alias (Prim_Op)))
                  then
                     Parent_Subp := Alias (Prim_Op);

                     Found_Explicit := False;
                     Op_Elmt_2 := Next_Elmt (Op_Elmt);
                     while Present (Op_Elmt_2) loop
                        if Chars (Node (Op_Elmt_2)) = Chars (Parent_Subp)
                          and then Type_Conformant (Prim_Op, Node (Op_Elmt_2))
                        then
                           --  The private inherited operation has been
                           --  overridden by an explicit subprogram, so
                           --  change the private op's list element to
                           --  designate the explicit so the explicit
                           --  one will get the right dispatching slot.

                           New_Op := Node (Op_Elmt_2);
                           Replace_Elmt (Op_Elmt, New_Op);
                           Remove_Elmt (Op_List, Op_Elmt_2);
                           Found_Explicit := True;
                           Decl_Privates  := True;

                           --  If explicit_overriding is in effect, check that
                           --  the overriding operation is properly labelled.

                           if Explicit_Overriding
                             and then Comes_From_Source (New_Op)
                              and then not Has_Overriding_Pragma (New_Op)
                           then
                              Error_Msg_NE
                                ("Missing overriding pragma for&",
                                  New_Op, New_Op);
                           end if;

                           exit;
                        end if;

                        Next_Elmt (Op_Elmt_2);
                     end loop;

                     if not Found_Explicit then
                        Derive_Subprogram
                          (New_Op, Alias (Prim_Op), E, Etype (E));

                        pragma Assert
                          (Is_Dispatching_Operation (New_Op)
                            and then Node (Last_Elmt (Op_List)) = New_Op);

                        --  Substitute the new operation for the old one
                        --  in the type's primitive operations list. Since
                        --  the new operation was also just added to the end
                        --  of list, the last element must be removed.

                        --  (Question: is there a simpler way of declaring
                        --  the operation, say by just replacing the name
                        --  of the earlier operation, reentering it in the
                        --  in the symbol table (how?), and marking it as
                        --  private???)

                        Replace_Elmt (Op_Elmt, New_Op);
                        Remove_Last_Elmt (Op_List);
                        Decl_Privates := True;
                     end if;
                  end if;

                  Next_Elmt (Op_Elmt);
               end loop;

               --  The type's DT attributes need to be recalculated
               --  in the case where private dispatching operations
               --  have been added or overridden. Normally this action
               --  occurs during type freezing, but we force it here
               --  since the type may already have been frozen (e.g.,
               --  if the type's package has an empty private part).
               --  This can only be done if expansion is active, otherwise
               --  Tag may not be present.

               if Decl_Privates
                 and then Expander_Active
               then
                  Set_All_DT_Position (E);
               end if;

            else
               --   Non-tagged type, scan forward to locate
               --   inherited hidden operations.

               Prim_Op := Next_Entity (E);

               while Present (Prim_Op) loop
                  if Is_Subprogram (Prim_Op)
                    and then Present (Alias (Prim_Op))
                    and then not Comes_From_Source (Prim_Op)
                    and then Is_Internal_Name (Chars (Prim_Op))
                    and then not Is_Internal_Name (Chars (Alias (Prim_Op)))
                    and then Is_Primitive_Of (E, Prim_Op)
                  then
                     Derive_Subprogram (New_Op, Alias (Prim_Op), E, Etype (E));
                  end if;

                  Next_Entity (Prim_Op);
               end loop;
            end if;
         end if;

         Next_Entity (E);
      end loop;
   end Declare_Inherited_Private_Subprograms;

   -----------------------
   -- End_Package_Scope --
   -----------------------

   procedure End_Package_Scope (P : Entity_Id) is
   begin
      Uninstall_Declarations (P);
      Pop_Scope;
   end End_Package_Scope;

   ---------------------------
   -- Exchange_Declarations --
   ---------------------------

   procedure Exchange_Declarations (Id : Entity_Id) is
      Full_Id : constant Entity_Id := Full_View (Id);
      H1      : constant Entity_Id := Homonym (Id);
      Next1   : constant Entity_Id := Next_Entity (Id);
      H2      : Entity_Id;
      Next2   : Entity_Id;

   begin
      --  If missing full declaration for type, nothing to exchange

      if No (Full_Id) then
         return;
      end if;

      --  Otherwise complete the exchange, and preserve semantic links

      Next2 := Next_Entity (Full_Id);
      H2    := Homonym (Full_Id);

      --  Reset full declaration pointer to reflect the switched entities
      --  and readjust the next entity chains.

      Exchange_Entities (Id, Full_Id);

      Set_Next_Entity (Id, Next1);
      Set_Homonym     (Id, H1);

      Set_Full_View   (Full_Id, Id);
      Set_Next_Entity (Full_Id, Next2);
      Set_Homonym     (Full_Id, H2);
   end Exchange_Declarations;

   ----------------------------
   -- Install_Package_Entity --
   ----------------------------

   procedure Install_Package_Entity (Id : Entity_Id) is
   begin
      if not Is_Internal (Id) then
         if Debug_Flag_E then
            Write_Str ("Install: ");
            Write_Name (Chars (Id));
            Write_Eol;
         end if;

         if not Is_Child_Unit (Id) then
            Set_Is_Immediately_Visible (Id);
         end if;

      end if;
   end Install_Package_Entity;

   ----------------------------------
   -- Install_Private_Declarations --
   ----------------------------------

   procedure Install_Private_Declarations (P : Entity_Id) is
      Id        : Entity_Id;
      Priv_Elmt : Elmt_Id;
      Priv      : Entity_Id;
      Full      : Entity_Id;

   begin
      --  First exchange declarations for private types, so that the
      --  full declaration is visible. For each private type, we check
      --  its Private_Dependents list and also exchange any subtypes of
      --  or derived types from it. Finally, if this is a Taft amendment
      --  type, the incomplete declaration is irrelevant, and we want to
      --  link the eventual full declaration with the original private
      --  one so we also skip the exchange.

      Id := First_Entity (P);
      while Present (Id) and then Id /= First_Private_Entity (P) loop

         if Is_Private_Base_Type (Id)
           and then Comes_From_Source (Full_View (Id))
           and then Present (Full_View (Id))
           and then Scope (Full_View (Id)) = Scope (Id)
           and then Ekind (Full_View (Id)) /= E_Incomplete_Type
         then
            --  If there is a use-type clause on the private type, set the
            --  full view accordingly.

            Set_In_Use (Full_View (Id), In_Use (Id));
            Full := Full_View (Id);

            if Is_Private_Base_Type (Full)
              and then Has_Private_Declaration (Full)
              and then Nkind (Parent (Full)) = N_Full_Type_Declaration
              and then In_Open_Scopes (Scope (Etype (Full)))
              and then In_Package_Body (Current_Scope)
              and then not Is_Private_Type (Etype (Full))
            then
               --  This is the completion of a private type by a derivation
               --  from another private type which is not private anymore. This
               --  can only happen in a package nested within a child package,
               --  when the parent type is defined in the parent unit. At this
               --  point the current type is not private either, and we have to
               --  install the underlying full view, which is now visible.

               if No (Full_View (Full))
                 and then Present (Underlying_Full_View (Full))
               then
                  Set_Full_View (Id, Underlying_Full_View (Full));
                  Set_Underlying_Full_View (Full, Empty);
                  Set_Is_Frozen (Full_View (Id));
               end if;
            end if;

            Priv_Elmt := First_Elmt (Private_Dependents (Id));

            Exchange_Declarations (Id);
            Set_Is_Immediately_Visible (Id);

            while Present (Priv_Elmt) loop
               Priv := Node (Priv_Elmt);

               --  Before the exchange, verify that the presence of the
               --  Full_View field. It will be empty if the entity
               --  has already been installed due to a previous call.

               if Present (Full_View (Priv))
                 and then Is_Visible_Dependent (Priv)
               then

                  --  For each subtype that is swapped, we also swap the
                  --  reference to it in Private_Dependents, to allow access
                  --  to it when we swap them out in End_Package_Scope.

                  Replace_Elmt (Priv_Elmt, Full_View (Priv));
                  Exchange_Declarations (Priv);
                  Set_Is_Immediately_Visible
                    (Priv, In_Open_Scopes (Scope (Priv)));
                  Set_Is_Potentially_Use_Visible
                    (Priv, Is_Potentially_Use_Visible (Node (Priv_Elmt)));
               end if;

               Next_Elmt (Priv_Elmt);
            end loop;
         end if;

         Next_Entity (Id);
      end loop;

      --  Next make other declarations in the private part visible as well

      Id := First_Private_Entity (P);

      while Present (Id) loop
         Install_Package_Entity (Id);
         Set_Is_Hidden (Id, False);
         Next_Entity (Id);
      end loop;

      --  Indicate that the private part is currently visible, so it can be
      --  properly reset on exit.

      Set_In_Private_Part (P);
   end Install_Private_Declarations;

   ----------------------------------
   -- Install_Visible_Declarations --
   ----------------------------------

   procedure Install_Visible_Declarations (P : Entity_Id) is
      Id : Entity_Id;

   begin
      Id := First_Entity (P);

      while Present (Id) and then Id /= First_Private_Entity (P) loop
         Install_Package_Entity (Id);
         Next_Entity (Id);
      end loop;
   end Install_Visible_Declarations;

   --------------------------
   -- Is_Private_Base_Type --
   --------------------------

   function Is_Private_Base_Type (E : Entity_Id) return Boolean is
   begin
      return Ekind (E) = E_Private_Type
        or else Ekind (E) = E_Limited_Private_Type
        or else Ekind (E) = E_Record_Type_With_Private;
   end Is_Private_Base_Type;

   --------------------------
   -- Is_Visible_Dependent --
   --------------------------

   function Is_Visible_Dependent (Dep : Entity_Id) return Boolean
   is
      S : constant Entity_Id := Scope (Dep);

   begin
      --  Renamings created for actual types have the visibility of the
      --  actual.

      if Ekind (S) = E_Package
        and then Is_Generic_Instance (S)
        and then (Is_Generic_Actual_Type (Dep)
                   or else Is_Generic_Actual_Type (Full_View (Dep)))
      then
         return True;

      elsif not (Is_Derived_Type (Dep))
        and then Is_Derived_Type (Full_View (Dep))
      then
         --  When instantiating a package body, the scope stack is empty,
         --  so check instead whether the dependent type is defined in
         --  the same scope as the instance itself.

         return In_Open_Scopes (S)
           or else (Is_Generic_Instance (Current_Scope)
              and then Scope (Dep) = Scope (Current_Scope));
      else
         return True;
      end if;
   end Is_Visible_Dependent;

   ----------------------------
   -- May_Need_Implicit_Body --
   ----------------------------

   procedure May_Need_Implicit_Body (E : Entity_Id) is
      P     : constant Node_Id := Unit_Declaration_Node (E);
      S     : constant Node_Id := Parent (P);
      B     : Node_Id;
      Decls : List_Id;

   begin
      if not Has_Completion (E)
        and then Nkind (P) = N_Package_Declaration
        and then Present (Activation_Chain_Entity (P))
      then
         B :=
           Make_Package_Body (Sloc (E),
             Defining_Unit_Name => Make_Defining_Identifier (Sloc (E),
               Chars => Chars (E)),
             Declarations  => New_List);

         if Nkind (S) = N_Package_Specification then
            if Present (Private_Declarations (S)) then
               Decls := Private_Declarations (S);
            else
               Decls := Visible_Declarations (S);
            end if;
         else
            Decls := Declarations (S);
         end if;

         Append (B, Decls);
         Analyze (B);
      end if;
   end May_Need_Implicit_Body;

   ----------------------
   -- New_Private_Type --
   ----------------------

   procedure New_Private_Type (N : Node_Id; Id : Entity_Id; Def : Node_Id) is
   begin
      Enter_Name (Id);

      if Limited_Present (Def) then
         Set_Ekind (Id, E_Limited_Private_Type);
      else
         Set_Ekind (Id, E_Private_Type);
      end if;

      Set_Etype              (Id, Id);
      Set_Has_Delayed_Freeze (Id);
      Set_Is_First_Subtype   (Id);
      Init_Size_Align        (Id);

      Set_Is_Constrained (Id,
        No (Discriminant_Specifications (N))
          and then not Unknown_Discriminants_Present (N));

      --  Set tagged flag before processing discriminants, to catch
      --  illegal usage.

      Set_Is_Tagged_Type (Id, Tagged_Present (Def));

      Set_Discriminant_Constraint (Id, No_Elist);
      Set_Stored_Constraint (Id, No_Elist);

      if Present (Discriminant_Specifications (N)) then
         New_Scope (Id);
         Process_Discriminants (N);
         End_Scope;

      elsif Unknown_Discriminants_Present (N) then
         Set_Has_Unknown_Discriminants (Id);
      end if;

      Set_Private_Dependents (Id, New_Elmt_List);

      if Tagged_Present (Def) then
         Set_Ekind                (Id, E_Record_Type_With_Private);
         Make_Class_Wide_Type     (Id);
         Set_Primitive_Operations (Id, New_Elmt_List);
         Set_Is_Abstract          (Id, Abstract_Present (Def));
         Set_Is_Limited_Record    (Id, Limited_Present (Def));
         Set_Has_Delayed_Freeze   (Id, True);

      elsif Abstract_Present (Def) then
         Error_Msg_N ("only a tagged type can be abstract", N);
      end if;
   end New_Private_Type;

   ----------------------------
   -- Uninstall_Declarations --
   ----------------------------

   procedure Uninstall_Declarations (P : Entity_Id) is
      Decl      : constant Node_Id := Unit_Declaration_Node (P);
      Id        : Entity_Id;
      Full      : Entity_Id;
      Priv_Elmt : Elmt_Id;
      Priv_Sub  : Entity_Id;

      procedure Preserve_Full_Attributes (Priv, Full : Entity_Id);
      --  Copy to the private declaration the attributes of the full view
      --  that need to be available for the partial view also.

      function Type_In_Use (T : Entity_Id) return Boolean;
      --  Check whether type or base type appear in an active use_type clause

      ------------------------------
      -- Preserve_Full_Attributes --
      ------------------------------

      procedure Preserve_Full_Attributes (Priv, Full : Entity_Id) is
         Priv_Is_Base_Type : constant Boolean := Priv = Base_Type (Priv);

      begin
         Set_Size_Info (Priv, (Full));
         Set_RM_Size (Priv, RM_Size (Full));
         Set_Size_Known_At_Compile_Time (Priv, Size_Known_At_Compile_Time
                                                                      (Full));
         Set_Is_Volatile       (Priv, Is_Volatile       (Full));
         Set_Treat_As_Volatile (Priv, Treat_As_Volatile (Full));

         if Referenced (Full) then
            Set_Referenced (Priv);
         end if;

         if Priv_Is_Base_Type then
            Set_Is_Controlled (Priv, Is_Controlled (Base_Type (Full)));
            Set_Finalize_Storage_Only (Priv, Finalize_Storage_Only
                                                           (Base_Type (Full)));
            Set_Has_Task (Priv, Has_Task (Base_Type (Full)));
            Set_Has_Controlled_Component (Priv, Has_Controlled_Component
                                                           (Base_Type (Full)));
         end if;

         Set_Freeze_Node (Priv, Freeze_Node (Full));

         if Is_Tagged_Type (Priv)
           and then Is_Tagged_Type (Full)
           and then not Error_Posted (Full)
         then
            if Priv_Is_Base_Type then
               Set_Access_Disp_Table (Priv, Access_Disp_Table
                                                           (Base_Type (Full)));
            end if;

            Set_First_Entity (Priv, First_Entity (Full));
            Set_Last_Entity  (Priv, Last_Entity (Full));
            Set_Has_Discriminants (Priv, Has_Discriminants (Full));
         end if;
      end Preserve_Full_Attributes;

      -----------------
      -- Type_In_Use --
      -----------------

      function Type_In_Use (T : Entity_Id) return Boolean is
      begin
         return Scope (Base_Type (T)) = P
           and then  (In_Use (T) or else In_Use (Base_Type (T)));
      end Type_In_Use;

   --  Start of processing for Uninstall_Declarations

   begin
      Id := First_Entity (P);

      while Present (Id) and then Id /= First_Private_Entity (P) loop
         if Debug_Flag_E then
            Write_Str ("unlinking visible entity ");
            Write_Int (Int (Id));
            Write_Eol;
         end if;

         --  On  exit from the package scope, we must preserve the visibility
         --  established by use clauses in the current scope. Two cases:

         --  a) If the entity is an operator, it may be a primitive operator of
         --  a type for which there is a visible use-type clause.

         --  b) for other entities, their use-visibility is determined by a
         --  visible use clause for the package itself. For a generic instance,
         --  the instantiation of the formals appears in the visible part,
         --  but the formals are private and remain so.

         if Ekind (Id) = E_Function
           and then  Is_Operator_Symbol_Name (Chars (Id))
           and then not Is_Hidden (Id)
           and then not Error_Posted (Id)
         then
            Set_Is_Potentially_Use_Visible (Id,
              In_Use (P)
              or else Type_In_Use (Etype (Id))
              or else Type_In_Use (Etype (First_Formal (Id)))
              or else (Present (Next_Formal (First_Formal (Id)))
                         and then
                           Type_In_Use
                             (Etype (Next_Formal (First_Formal (Id))))));
         else
            Set_Is_Potentially_Use_Visible (Id,
              In_Use (P) and not Is_Hidden (Id));
         end if;

         --  Local entities are not immediately visible outside of the package

         Set_Is_Immediately_Visible (Id, False);

         --  If this is a private type with a full view (for example a local
         --  subtype of a private type declared elsewhere), ensure that the
         --  full view is also removed from visibility: it may be exposed when
         --  swapping views in an instantiation.

         if Is_Type (Id)
           and then Present (Full_View (Id))
         then
            Set_Is_Immediately_Visible (Full_View (Id), False);
         end if;

         if Is_Tagged_Type (Id) and then Ekind (Id) = E_Record_Type then
            Check_Abstract_Overriding (Id);
         end if;

         if (Ekind (Id) = E_Private_Type
               or else Ekind (Id) = E_Limited_Private_Type)
           and then No (Full_View (Id))
           and then not Is_Generic_Type (Id)
           and then not Is_Derived_Type (Id)
         then
            Error_Msg_N ("missing full declaration for private type&", Id);

         elsif Ekind (Id) = E_Record_Type_With_Private
           and then not Is_Generic_Type (Id)
           and then No (Full_View (Id))
         then
            if Nkind (Parent (Id)) = N_Private_Type_Declaration then
               Error_Msg_N ("missing full declaration for private type&", Id);
            else
               Error_Msg_N
                 ("missing full declaration for private extension", Id);
            end if;

         elsif Ekind (Id) = E_Constant
           and then No (Constant_Value (Id))
           and then No (Full_View (Id))
           and then not Is_Imported (Id)
           and then (Nkind (Parent (Id)) /= N_Object_Declaration
                      or else not No_Initialization (Parent (Id)))
         then
            if not Has_Private_Declaration (Etype (Id)) then

               --  We assume that the user did not not intend a deferred
               --  constant declaration, and the expression is just missing.

               Error_Msg_N
                 ("constant declaration requires initialization expression",
                   Parent (Id));

               if Is_Limited_Type (Etype (Id)) then
                  Error_Msg_N
                    ("\else remove keyword CONSTANT from declaration",
                    Parent (Id));
               end if;

            else
               Error_Msg_N
                  ("missing full declaration for deferred constant ('R'M 7.4)",
                     Id);

               if Is_Limited_Type (Etype (Id)) then
                  Error_Msg_N
                    ("\else remove keyword CONSTANT from declaration",
                    Parent (Id));
               end if;
            end if;
         end if;

         Next_Entity (Id);
      end loop;

      --  If the specification was installed as the parent of a public child
      --  unit, the private declarations were not installed, and there is
      --  nothing to do.

      if not In_Private_Part (P) then
         return;
      else
         Set_In_Private_Part (P, False);
      end if;

      --  Make private entities invisible and exchange full and private
      --  declarations for private types.

      while Present (Id) loop
         if Debug_Flag_E then
            Write_Str ("unlinking private entity ");
            Write_Int (Int (Id));
            Write_Eol;
         end if;

         if Is_Tagged_Type (Id) and then Ekind (Id) = E_Record_Type then
            Check_Abstract_Overriding (Id);
         end if;

         Set_Is_Immediately_Visible (Id, False);

         if Is_Private_Base_Type (Id)
           and then Present (Full_View (Id))
         then
            Full := Full_View (Id);

            --  If the partial view is not declared in the visible part
            --  of the package (as is the case when it is a type derived
            --  from some other private type in the private part of the
            --  current package), no exchange takes place.

            if No (Parent (Id))
              or else List_Containing (Parent (Id))
                /= Visible_Declarations (Specification (Decl))
            then
               goto Next_Id;
            end if;

            --  The entry in the private part points to the full declaration,
            --  which is currently visible. Exchange them so only the private
            --  type declaration remains accessible, and link private and
            --  full declaration in the opposite direction. Before the actual
            --  exchange, we copy back attributes of the full view that
            --  must be available to the partial view too.

            Preserve_Full_Attributes (Id, Full);

            Set_Is_Potentially_Use_Visible (Id, In_Use (P));

            if  Is_Indefinite_Subtype (Full)
              and then not Is_Indefinite_Subtype (Id)
            then
               Error_Msg_N
                 ("full view of type must be definite subtype", Full);
            end if;

            Priv_Elmt := First_Elmt (Private_Dependents (Id));

            --  Swap out the subtypes and derived types of Id that were
            --  compiled in this scope, or installed previously by
            --  Install_Private_Declarations.
            --  Before we do the swap, we verify the presence of the
            --  Full_View field which may be empty due to a swap by
            --  a previous call to End_Package_Scope (e.g. from the
            --  freezing mechanism).

            while Present (Priv_Elmt) loop
               Priv_Sub := Node (Priv_Elmt);

               if Present (Full_View (Priv_Sub)) then

                  if Scope (Priv_Sub) = P
                     or else not In_Open_Scopes (Scope (Priv_Sub))
                  then
                     Set_Is_Immediately_Visible (Priv_Sub, False);
                  end if;

                  if Is_Visible_Dependent (Priv_Sub) then
                     Preserve_Full_Attributes
                       (Priv_Sub, Full_View (Priv_Sub));
                     Replace_Elmt (Priv_Elmt, Full_View (Priv_Sub));
                     Exchange_Declarations (Priv_Sub);
                  end if;
               end if;

               Next_Elmt (Priv_Elmt);
            end loop;

            --  Now restore the type itself to its private view.

            Exchange_Declarations (Id);

         elsif Ekind (Id) = E_Incomplete_Type
           and then No (Full_View (Id))
         then
            --  Mark Taft amendment types

            Set_Has_Completion_In_Body (Id);

         elsif not Is_Child_Unit (Id)
           and then (not Is_Private_Type (Id)
                      or else No (Full_View (Id)))
         then
            Set_Is_Hidden (Id);
            Set_Is_Potentially_Use_Visible (Id, False);
         end if;

         <<Next_Id>>
            Next_Entity (Id);
      end loop;
   end Uninstall_Declarations;

   ------------------------
   -- Unit_Requires_Body --
   ------------------------

   function Unit_Requires_Body (P : Entity_Id) return Boolean is
      E : Entity_Id;

   begin
      --  Imported entity never requires body. Right now, only
      --  subprograms can be imported, but perhaps in the future
      --  we will allow import of packages.

      if Is_Imported (P) then
         return False;

      --  Body required if library package with pragma Elaborate_Body

      elsif Has_Pragma_Elaborate_Body (P) then
         return True;

      --  Body required if subprogram

      elsif Is_Subprogram (P) or else Is_Generic_Subprogram (P) then
         return True;

      --  Treat a block as requiring a body

      elsif Ekind (P) = E_Block then
         return True;

      elsif Ekind (P) = E_Package
        and then Nkind (Parent (P)) = N_Package_Specification
        and then Present (Generic_Parent (Parent (P)))
      then
         declare
            G_P : constant Entity_Id := Generic_Parent (Parent (P));

         begin
            if Has_Pragma_Elaborate_Body (G_P) then
               return True;
            end if;
         end;
      end if;

      --  Otherwise search entity chain for entity requiring completion

      E := First_Entity (P);
      while Present (E) loop

         --  Always ignore child units. Child units get added to the entity
         --  list of a parent unit, but are not original entities of the
         --  parent, and so do not affect whether the parent needs a body.

         if Is_Child_Unit (E) then
            null;

         --  Ignore formal packages and their renamings

         elsif Ekind (E) = E_Package
           and then Nkind (Original_Node (Unit_Declaration_Node (E))) =
                                                N_Formal_Package_Declaration
         then
            null;

         --  Otherwise test to see if entity requires a completion

         elsif (Is_Overloadable (E)
               and then Ekind (E) /= E_Enumeration_Literal
               and then Ekind (E) /= E_Operator
               and then not Is_Abstract (E)
               and then not Has_Completion (E))

           or else
             (Ekind (E) = E_Package
               and then E /= P
               and then not Has_Completion (E)
               and then Unit_Requires_Body (E))

           or else
             (Ekind (E) = E_Incomplete_Type and then No (Full_View (E)))

           or else
            ((Ekind (E) = E_Task_Type or else
              Ekind (E) = E_Protected_Type)
               and then not Has_Completion (E))

           or else
             (Ekind (E) = E_Generic_Package and then E /= P
               and then not Has_Completion (E)
               and then Unit_Requires_Body (E))

           or else
             (Is_Generic_Subprogram (E)
               and then not Has_Completion (E))

         then
            return True;

         --  Entity that does not require completion

         else
            null;
         end if;

         Next_Entity (E);
      end loop;

      return False;
   end Unit_Requires_Body;

end Sem_Ch7;
