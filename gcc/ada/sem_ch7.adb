------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S E M _ C H 7                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2016, Free Software Foundation, Inc.         --
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

--  This package contains the routines to process package specifications and
--  bodies. The most important semantic aspects of package processing are the
--  handling of private and full declarations, and the construction of dispatch
--  tables for tagged types.

with Aspects;   use Aspects;
with Atree;     use Atree;
with Contracts; use Contracts;
with Debug;     use Debug;
with Einfo;     use Einfo;
with Elists;    use Elists;
with Errout;    use Errout;
with Exp_Disp;  use Exp_Disp;
with Exp_Dist;  use Exp_Dist;
with Exp_Dbug;  use Exp_Dbug;
with Ghost;     use Ghost;
with Lib;       use Lib;
with Lib.Xref;  use Lib.Xref;
with Namet;     use Namet;
with Nmake;     use Nmake;
with Nlists;    use Nlists;
with Opt;       use Opt;
with Output;    use Output;
with Restrict;  use Restrict;
with Sem;       use Sem;
with Sem_Aux;   use Sem_Aux;
with Sem_Cat;   use Sem_Cat;
with Sem_Ch3;   use Sem_Ch3;
with Sem_Ch6;   use Sem_Ch6;
with Sem_Ch8;   use Sem_Ch8;
with Sem_Ch10;  use Sem_Ch10;
with Sem_Ch12;  use Sem_Ch12;
with Sem_Ch13;  use Sem_Ch13;
with Sem_Disp;  use Sem_Disp;
with Sem_Eval;  use Sem_Eval;
with Sem_Prag;  use Sem_Prag;
with Sem_Util;  use Sem_Util;
with Sem_Warn;  use Sem_Warn;
with Snames;    use Snames;
with Stand;     use Stand;
with Sinfo;     use Sinfo;
with Sinput;    use Sinput;
with Style;
with Uintp;     use Uintp;

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

   procedure Analyze_Package_Body_Helper (N : Node_Id);
   --  Does all the real work of Analyze_Package_Body

   procedure Check_Anonymous_Access_Types
     (Spec_Id : Entity_Id;
      P_Body  : Node_Id);
   --  If the spec of a package has a limited_with_clause, it may declare
   --  anonymous access types whose designated type is a limited view, such an
   --  anonymous access return type for a function. This access type cannot be
   --  elaborated in the spec itself, but it may need an itype reference if it
   --  is used within a nested scope. In that case the itype reference is
   --  created at the beginning of the corresponding package body and inserted
   --  before other body declarations.

   procedure Declare_Inherited_Private_Subprograms (Id : Entity_Id);
   --  Called upon entering the private part of a public child package and the
   --  body of a nested package, to potentially declare certain inherited
   --  subprograms that were inherited by types in the visible part, but whose
   --  declaration was deferred because the parent operation was private and
   --  not visible at that point. These subprograms are located by traversing
   --  the visible part declarations looking for non-private type extensions
   --  and then examining each of the primitive operations of such types to
   --  find those that were inherited but declared with a special internal
   --  name. Each such operation is now declared as an operation with a normal
   --  name (using the name of the parent operation) and replaces the previous
   --  implicit operation in the primitive operations list of the type. If the
   --  inherited private operation has been overridden, then it's replaced by
   --  the overriding operation.

   procedure Install_Package_Entity (Id : Entity_Id);
   --  Supporting procedure for Install_{Visible,Private}_Declarations. Places
   --  one entity on its visibility chain, and recurses on the visible part if
   --  the entity is an inner package.

   function Is_Private_Base_Type (E : Entity_Id) return Boolean;
   --  True for a private type that is not a subtype

   function Is_Visible_Dependent (Dep : Entity_Id) return Boolean;
   --  If the private dependent is a private type whose full view is derived
   --  from the parent type, its full properties are revealed only if we are in
   --  the immediate scope of the private dependent. Should this predicate be
   --  tightened further???

   function Requires_Completion_In_Body
     (Id      : Entity_Id;
      Pack_Id : Entity_Id) return Boolean;
   --  Subsidiary to routines Unit_Requires_Body and Unit_Requires_Body_Info.
   --  Determine whether entity Id declared in package spec Pack_Id requires
   --  completion in a package body.

   procedure Unit_Requires_Body_Info (Pack_Id : Entity_Id);
   --  Outputs info messages showing why package Pack_Id requires a body. The
   --  caller has checked that the switch requesting this information is set,
   --  and that the package does indeed require a body.

   --------------------------
   -- Analyze_Package_Body --
   --------------------------

   procedure Analyze_Package_Body (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);

   begin
      if Debug_Flag_C then
         Write_Str ("==> package body ");
         Write_Name (Chars (Defining_Entity (N)));
         Write_Str (" from ");
         Write_Location (Loc);
         Write_Eol;
         Indent;
      end if;

      --  The real work is split out into the helper, so it can do "return;"
      --  without skipping the debug output.

      Analyze_Package_Body_Helper (N);

      if Debug_Flag_C then
         Outdent;
         Write_Str ("<== package body ");
         Write_Name (Chars (Defining_Entity (N)));
         Write_Str (" from ");
         Write_Location (Loc);
         Write_Eol;
      end if;
   end Analyze_Package_Body;

   ---------------------------------
   -- Analyze_Package_Body_Helper --
   ---------------------------------

   procedure Analyze_Package_Body_Helper (N : Node_Id) is
      procedure Hide_Public_Entities (Decls : List_Id);
      --  Attempt to hide all public entities found in declarative list Decls
      --  by resetting their Is_Public flag to False depending on whether the
      --  entities are not referenced by inlined or generic bodies. This kind
      --  of processing is a conservative approximation and may still leave
      --  certain entities externally visible.

      procedure Install_Composite_Operations (P : Entity_Id);
      --  Composite types declared in the current scope may depend on types
      --  that were private at the point of declaration, and whose full view
      --  is now in scope. Indicate that the corresponding operations on the
      --  composite type are available.

      --------------------------
      -- Hide_Public_Entities --
      --------------------------

      procedure Hide_Public_Entities (Decls : List_Id) is
         function Contains_Subp_Or_Const_Refs (N : Node_Id) return Boolean;
         --  Subsidiary to routine Has_Referencer. Determine whether a node
         --  contains a reference to a subprogram or a non-static constant.
         --  WARNING: this is a very expensive routine as it performs a full
         --  tree traversal.

         function Has_Referencer
           (Decls     : List_Id;
            Top_Level : Boolean := False) return Boolean;
         --  A "referencer" is a construct which may reference a previous
         --  declaration. Examine all declarations in list Decls in reverse
         --  and determine whether once such referencer exists. All entities
         --  in the range Last (Decls) .. Referencer are hidden from external
         --  visibility.

         ---------------------------------
         -- Contains_Subp_Or_Const_Refs --
         ---------------------------------

         function Contains_Subp_Or_Const_Refs (N : Node_Id) return Boolean is
            Reference_Seen : Boolean := False;

            function Is_Subp_Or_Const_Ref
              (N : Node_Id) return Traverse_Result;
            --  Determine whether a node denotes a reference to a subprogram or
            --  a non-static constant.

            --------------------------
            -- Is_Subp_Or_Const_Ref --
            --------------------------

            function Is_Subp_Or_Const_Ref
              (N : Node_Id) return Traverse_Result
            is
               Val : Node_Id;

            begin
               --  Detect a reference of the form
               --    Subp_Call

               if Nkind (N) in N_Subprogram_Call
                 and then Is_Entity_Name (Name (N))
               then
                  Reference_Seen := True;
                  return Abandon;

               --  Detect a reference of the form
               --    Subp'Some_Attribute

               elsif Nkind (N) = N_Attribute_Reference
                 and then Is_Entity_Name (Prefix (N))
                 and then Present (Entity (Prefix (N)))
                 and then Is_Subprogram (Entity (Prefix (N)))
               then
                  Reference_Seen := True;
                  return Abandon;

               --  Detect the use of a non-static constant

               elsif Is_Entity_Name (N)
                 and then Present (Entity (N))
                 and then Ekind (Entity (N)) = E_Constant
               then
                  Val := Constant_Value (Entity (N));

                  if Present (Val)
                    and then not Compile_Time_Known_Value (Val)
                  then
                     Reference_Seen := True;
                     return Abandon;
                  end if;
               end if;

               return OK;
            end Is_Subp_Or_Const_Ref;

            procedure Find_Subp_Or_Const_Ref is
              new Traverse_Proc (Is_Subp_Or_Const_Ref);

         --  Start of processing for Contains_Subp_Or_Const_Refs

         begin
            Find_Subp_Or_Const_Ref (N);

            return Reference_Seen;
         end Contains_Subp_Or_Const_Refs;

         --------------------
         -- Has_Referencer --
         --------------------

         function Has_Referencer
           (Decls     : List_Id;
            Top_Level : Boolean := False) return Boolean
         is
            Decl    : Node_Id;
            Decl_Id : Entity_Id;
            Spec    : Node_Id;

            Has_Non_Subp_Const_Referencer : Boolean := False;
            --  Flag set for inlined subprogram bodies that do not contain
            --  references to other subprograms or non-static constants.

         begin
            if No (Decls) then
               return False;
            end if;

            --  Examine all declarations in reverse order, hiding all entities
            --  from external visibility until a referencer has been found. The
            --  algorithm recurses into nested packages.

            Decl := Last (Decls);
            while Present (Decl) loop

               --  A stub is always considered a referencer

               if Nkind (Decl) in N_Body_Stub then
                  return True;

               --  Package declaration

               elsif Nkind (Decl) = N_Package_Declaration
                 and then not Has_Non_Subp_Const_Referencer
               then
                  Spec := Specification (Decl);

                  --  Inspect the declarations of a non-generic package to try
                  --  and hide more entities from external visibility.

                  if not Is_Generic_Unit (Defining_Entity (Spec)) then
                     if Has_Referencer (Private_Declarations (Spec))
                       or else Has_Referencer (Visible_Declarations (Spec))
                     then
                        return True;
                     end if;
                  end if;

               --  Package body

               elsif Nkind (Decl) = N_Package_Body
                 and then Present (Corresponding_Spec (Decl))
               then
                  Decl_Id := Corresponding_Spec (Decl);

                  --  A generic package body is a referencer. It would seem
                  --  that we only have to consider generics that can be
                  --  exported, i.e. where the corresponding spec is the
                  --  spec of the current package, but because of nested
                  --  instantiations, a fully private generic body may export
                  --  other private body entities. Furthermore, regardless of
                  --  whether there was a previous inlined subprogram, (an
                  --  instantiation of) the generic package may reference any
                  --  entity declared before it.

                  if Is_Generic_Unit (Decl_Id) then
                     return True;

                  --  Inspect the declarations of a non-generic package body to
                  --  try and hide more entities from external visibility.

                  elsif not Has_Non_Subp_Const_Referencer
                    and then Has_Referencer (Declarations (Decl))
                  then
                     return True;
                  end if;

               --  Subprogram body

               elsif Nkind (Decl) = N_Subprogram_Body then
                  if Present (Corresponding_Spec (Decl)) then
                     Decl_Id := Corresponding_Spec (Decl);

                     --  A generic subprogram body acts as a referencer

                     if Is_Generic_Unit (Decl_Id) then
                        return True;
                     end if;

                     --  An inlined subprogram body acts as a referencer

                     if Is_Inlined (Decl_Id)
                       or else Has_Pragma_Inline (Decl_Id)
                     then
                        --  Inspect the statements of the subprogram body
                        --  to determine whether the body references other
                        --  subprograms and/or non-static constants.

                        if Top_Level
                          and then not Contains_Subp_Or_Const_Refs (Decl)
                        then
                           Has_Non_Subp_Const_Referencer := True;
                        else
                           return True;
                        end if;
                     end if;

                  --  Otherwise this is a stand alone subprogram body

                  else
                     Decl_Id := Defining_Entity (Decl);

                     --  An inlined body acts as a referencer. Note that an
                     --  inlined subprogram remains Is_Public as gigi requires
                     --  the flag to be set.

                     --  Note that we test Has_Pragma_Inline here rather than
                     --  Is_Inlined. We are compiling this for a client, and
                     --  it is the client who will decide if actual inlining
                     --  should occur, so we need to assume that the procedure
                     --  could be inlined for the purpose of accessing global
                     --  entities.

                     if Has_Pragma_Inline (Decl_Id) then
                        if Top_Level
                          and then not Contains_Subp_Or_Const_Refs (Decl)
                        then
                           Has_Non_Subp_Const_Referencer := True;
                        else
                           return True;
                        end if;
                     else
                        Set_Is_Public (Decl_Id, False);
                     end if;
                  end if;

               --  Exceptions, objects and renamings do not need to be public
               --  if they are not followed by a construct which can reference
               --  and export them. The Is_Public flag is reset on top level
               --  entities only as anything nested is local to its context.

               elsif Nkind_In (Decl, N_Exception_Declaration,
                                     N_Object_Declaration,
                                     N_Object_Renaming_Declaration,
                                     N_Subprogram_Declaration,
                                     N_Subprogram_Renaming_Declaration)
               then
                  Decl_Id := Defining_Entity (Decl);

                  if Top_Level
                    and then not Is_Imported (Decl_Id)
                    and then not Is_Exported (Decl_Id)
                    and then No (Interface_Name (Decl_Id))
                    and then
                      (not Has_Non_Subp_Const_Referencer
                        or else Nkind (Decl) = N_Subprogram_Declaration)
                  then
                     Set_Is_Public (Decl_Id, False);
                  end if;
               end if;

               Prev (Decl);
            end loop;

            return Has_Non_Subp_Const_Referencer;
         end Has_Referencer;

         --  Local variables

         Discard : Boolean := True;
         pragma Unreferenced (Discard);

      --  Start of processing for Hide_Public_Entities

      begin
         --  The algorithm examines the top level declarations of a package
         --  body in reverse looking for a construct that may export entities
         --  declared prior to it. If such a scenario is encountered, then all
         --  entities in the range Last (Decls) .. construct are hidden from
         --  external visibility. Consider:

         --    package Pack is
         --       generic
         --       package Gen is
         --       end Gen;
         --    end Pack;

         --    package body Pack is
         --       External_Obj : ...;      --  (1)

         --       package body Gen is      --  (2)
         --          ... External_Obj ...  --  (3)
         --       end Gen;

         --       Local_Obj : ...;         --  (4)
         --    end Pack;

         --  In this example Local_Obj (4) must not be externally visible as
         --  it cannot be exported by anything in Pack. The body of generic
         --  package Gen (2) on the other hand acts as a "referencer" and may
         --  export anything declared before it. Since the compiler does not
         --  perform flow analysis, it is not possible to determine precisely
         --  which entities will be exported when Gen is instantiated. In the
         --  example above External_Obj (1) is exported at (3), but this may
         --  not always be the case. The algorithm takes a conservative stance
         --  and leaves entity External_Obj public.

         Discard := Has_Referencer (Decls, Top_Level => True);
      end Hide_Public_Entities;

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

      --  Local variables

      Save_Ghost_Mode  : constant Ghost_Mode_Type := Ghost_Mode;
      Body_Id          : Entity_Id;
      HSS              : Node_Id;
      Last_Spec_Entity : Entity_Id;
      New_N            : Node_Id;
      Pack_Decl        : Node_Id;
      Spec_Id          : Entity_Id;

   --  Start of processing for Analyze_Package_Body_Helper

   begin
      --  A [generic] package body "freezes" the contract of the nearest
      --  enclosing package body and all other contracts encountered in the
      --  same declarative part up to and excluding the package body:

      --    package body Nearest_Enclosing_Package
      --      with Refined_State => (State => Constit)
      --    is
      --       Constit : ...;

      --       package body Freezes_Enclosing_Package_Body
      --         with Refined_State => (State_2 => Constit_2)
      --       is
      --          Constit_2 : ...;

      --          procedure Proc
      --            with Refined_Depends => (Input => (Constit, Constit_2)) ...

      --  This ensures that any annotations referenced by the contract of a
      --  [generic] subprogram body declared within the current package body
      --  are available. This form of "freezing" is decoupled from the usual
      --  Freeze_xxx mechanism because it must also work in the context of
      --  generics where normal freezing is disabled.

      --  Only bodies coming from source should cause this type of "freezing"

      if Comes_From_Source (N) then
         Analyze_Previous_Contracts (N);
      end if;

      --  Find corresponding package specification, and establish the current
      --  scope. The visible defining entity for the package is the defining
      --  occurrence in the spec. On exit from the package body, all body
      --  declarations are attached to the defining entity for the body, but
      --  the later is never used for name resolution. In this fashion there
      --  is only one visible entity that denotes the package.

      --  Set Body_Id. Note that this will be reset to point to the generic
      --  copy later on in the generic case.

      Body_Id := Defining_Entity (N);

      --  Body is body of package instantiation. Corresponding spec has already
      --  been set.

      if Present (Corresponding_Spec (N)) then
         Spec_Id   := Corresponding_Spec (N);
         Pack_Decl := Unit_Declaration_Node (Spec_Id);

      else
         Spec_Id := Current_Entity_In_Scope (Defining_Entity (N));

         if Present (Spec_Id)
           and then Is_Package_Or_Generic_Package (Spec_Id)
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

         if Is_Package_Or_Generic_Package (Spec_Id)
           and then (Scope (Spec_Id) = Standard_Standard
                      or else Is_Child_Unit (Spec_Id))
           and then not Unit_Requires_Body (Spec_Id)
         then
            if Ada_Version = Ada_83 then
               Error_Msg_N
                 ("optional package body (not allowed in Ada 95)??", N);
            else
               Error_Msg_N ("spec of this package does not allow a body", N);
            end if;
         end if;
      end if;

      --  A package body is Ghost when the corresponding spec is Ghost. Set
      --  the mode now to ensure that any nodes generated during analysis and
      --  expansion are properly flagged as ignored Ghost.

      Set_Ghost_Mode (N, Spec_Id);

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

         --  Disable expansion and perform semantic analysis on copy. The
         --  unannotated body will be used in all instantiations.

         Body_Id := Defining_Entity (N);
         Set_Ekind (Body_Id, E_Package_Body);
         Set_Scope (Body_Id, Scope (Spec_Id));
         Set_Is_Obsolescent (Body_Id, Is_Obsolescent (Spec_Id));
         Set_Body_Entity (Spec_Id, Body_Id);
         Set_Spec_Entity (Body_Id, Spec_Id);

         New_N := Copy_Generic_Node (N, Empty, Instantiating => False);
         Rewrite (N, New_N);

         --  Once the contents of the generic copy and the template are
         --  swapped, do the same for their respective aspect specifications.

         Exchange_Aspects (N, New_N);

         --  Collect all contract-related source pragmas found within the
         --  template and attach them to the contract of the package body.
         --  This contract is used in the capture of global references within
         --  annotations.

         Create_Generic_Contract (N);

         --  Update Body_Id to point to the copied node for the remainder of
         --  the processing.

         Body_Id := Defining_Entity (N);
         Start_Generic;
      end if;

      --  The Body_Id is that of the copied node in the generic case, the
      --  current node otherwise. Note that N was rewritten above, so we must
      --  be sure to get the latest Body_Id value.

      Set_Ekind (Body_Id, E_Package_Body);
      Set_Body_Entity (Spec_Id, Body_Id);
      Set_Spec_Entity (Body_Id, Spec_Id);

      --  Defining name for the package body is not a visible entity: Only the
      --  defining name for the declaration is visible.

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

      if Has_Aspects (N) then
         Analyze_Aspect_Specifications (N, Body_Id);
      end if;

      Push_Scope (Spec_Id);

      --  Set SPARK_Mode only for non-generic package

      if Ekind (Spec_Id) = E_Package then
         Set_SPARK_Pragma               (Body_Id, SPARK_Mode_Pragma);
         Set_SPARK_Aux_Pragma           (Body_Id, SPARK_Mode_Pragma);
         Set_SPARK_Pragma_Inherited     (Body_Id);
         Set_SPARK_Aux_Pragma_Inherited (Body_Id);
      end if;

      --  Inherit the "ghostness" of the package spec. Note that this property
      --  is not directly inherited as the body may be subject to a different
      --  Ghost assertion policy.

      if Ghost_Mode > None or else Is_Ghost_Entity (Spec_Id) then
         Set_Is_Ghost_Entity (Body_Id);

         --  The Ghost policy in effect at the point of declaration and at the
         --  point of completion must match (SPARK RM 6.9(14)).

         Check_Ghost_Completion (Spec_Id, Body_Id);
      end if;

      Set_Categorization_From_Pragmas (N);

      Install_Visible_Declarations (Spec_Id);
      Install_Private_Declarations (Spec_Id);
      Install_Private_With_Clauses (Spec_Id);
      Install_Composite_Operations (Spec_Id);

      Check_Anonymous_Access_Types (Spec_Id, N);

      if Ekind (Spec_Id) = E_Generic_Package then
         Set_Use (Generic_Formal_Declarations (Pack_Decl));
      end if;

      Set_Use (Visible_Declarations (Specification (Pack_Decl)));
      Set_Use (Private_Declarations (Specification (Pack_Decl)));

      --  This is a nested package, so it may be necessary to declare certain
      --  inherited subprograms that are not yet visible because the parent
      --  type's subprograms are now visible.

      if Ekind (Scope (Spec_Id)) = E_Package
        and then Scope (Spec_Id) /= Standard_Standard
      then
         Declare_Inherited_Private_Subprograms (Spec_Id);
      end if;

      --  A package body "freezes" the contract of its initial declaration.
      --  This analysis depends on attribute Corresponding_Spec being set. Only
      --  bodies coming from source shuld cause this type of "freezing".

      if Present (Declarations (N)) then
         Analyze_Declarations (Declarations (N));
         Inspect_Deferred_Constant_Completion (Declarations (N));
      end if;

      --  Verify that the SPARK_Mode of the body agrees with that of its spec

      if Present (SPARK_Pragma (Body_Id)) then
         if Present (SPARK_Aux_Pragma (Spec_Id)) then
            if Get_SPARK_Mode_From_Annotation (SPARK_Aux_Pragma (Spec_Id)) =
                 Off
              and then
                Get_SPARK_Mode_From_Annotation (SPARK_Pragma (Body_Id)) = On
            then
               Error_Msg_Sloc := Sloc (SPARK_Pragma (Body_Id));
               Error_Msg_N ("incorrect application of SPARK_Mode#", N);
               Error_Msg_Sloc := Sloc (SPARK_Aux_Pragma (Spec_Id));
               Error_Msg_NE
                 ("\value Off was set for SPARK_Mode on & #", N, Spec_Id);
            end if;

         else
            Error_Msg_Sloc := Sloc (SPARK_Pragma (Body_Id));
            Error_Msg_N ("incorrect application of SPARK_Mode#", N);
            Error_Msg_Sloc := Sloc (Spec_Id);
            Error_Msg_NE
              ("\no value was set for SPARK_Mode on & #", N, Spec_Id);
         end if;
      end if;

      --  Analyze_Declarations has caused freezing of all types. Now generate
      --  bodies for RACW primitives and stream attributes, if any.

      if Ekind (Spec_Id) = E_Package and then Has_RACW (Spec_Id) then

         --  Attach subprogram bodies to support RACWs declared in spec

         Append_RACW_Bodies (Declarations (N), Spec_Id);
         Analyze_List (Declarations (N));
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

      --  For a generic package, collect global references and mark them on
      --  the original body so that they are not resolved again at the point
      --  of instantiation.

      if Ekind (Spec_Id) /= E_Package then
         Save_Global_References (Original_Node (N));
         End_Generic;
      end if;

      --  The entities of the package body have so far been chained onto the
      --  declaration chain for the spec. That's been fine while we were in the
      --  body, since we wanted them to be visible, but now that we are leaving
      --  the package body, they are no longer visible, so we remove them from
      --  the entity chain of the package spec entity, and copy them to the
      --  entity chain of the package body entity, where they will never again
      --  be visible.

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

            --  Child units may appear on the entity list (e.g. if they appear
            --  in the context of a subunit) but they are not body entities.

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

      --  At this point all entities of the package body are externally visible
      --  to the linker as their Is_Public flag is set to True. This proactive
      --  approach is necessary because an inlined or a generic body for which
      --  code is generated in other units may need to see these entities. Cut
      --  down the number of global symbols that do not neet public visibility
      --  as this has two beneficial effects:
      --    (1) It makes the compilation process more efficient.
      --    (2) It gives the code generatormore freedom to optimize within each
      --        unit, especially subprograms.

      --  This is done only for top level library packages or child units as
      --  the algorithm does a top down traversal of the package body.

      if (Scope (Spec_Id) = Standard_Standard or else Is_Child_Unit (Spec_Id))
        and then not Is_Generic_Unit (Spec_Id)
      then
         Hide_Public_Entities (Declarations (N));
      end if;

      --  If expander is not active, then here is where we turn off the
      --  In_Package_Body flag, otherwise it is turned off at the end of the
      --  corresponding expansion routine. If this is an instance body, we need
      --  to qualify names of local entities, because the body may have been
      --  compiled as a preliminary to another instantiation.

      if not Expander_Active then
         Set_In_Package_Body (Spec_Id, False);

         if Is_Generic_Instance (Spec_Id)
           and then Operating_Mode = Generate_Code
         then
            Qualify_Entity_Names (N);
         end if;
      end if;

      Ghost_Mode := Save_Ghost_Mode;
   end Analyze_Package_Body_Helper;

   ---------------------------------
   -- Analyze_Package_Declaration --
   ---------------------------------

   procedure Analyze_Package_Declaration (N : Node_Id) is
      Id  : constant Node_Id := Defining_Entity (N);
      Par : constant Node_Id := Parent_Spec (N);

      Body_Required : Boolean;
      --  True when this package declaration requires a corresponding body

      Comp_Unit : Boolean;
      --  True when this package declaration is not a nested declaration

      PF : Boolean;
      --  True when in the context of a declared pure library unit

   begin
      if Debug_Flag_C then
         Write_Str ("==> package spec ");
         Write_Name (Chars (Id));
         Write_Str (" from ");
         Write_Location (Sloc (N));
         Write_Eol;
         Indent;
      end if;

      Generate_Definition (Id);
      Enter_Name (Id);
      Set_Ekind  (Id, E_Package);
      Set_Etype  (Id, Standard_Void_Type);

      --  Set SPARK_Mode from context only for non-generic package

      if Ekind (Id) = E_Package then
         Set_SPARK_Pragma               (Id, SPARK_Mode_Pragma);
         Set_SPARK_Aux_Pragma           (Id, SPARK_Mode_Pragma);
         Set_SPARK_Pragma_Inherited     (Id);
         Set_SPARK_Aux_Pragma_Inherited (Id);
      end if;

      --  A package declared within a Ghost refion is automatically Ghost. A
      --  child package is Ghost when its parent is Ghost (SPARK RM 6.9(2)).

      if Ghost_Mode > None
        or else (Present (Par)
                  and then Is_Ghost_Entity (Defining_Entity (Unit (Par))))
      then
         Set_Is_Ghost_Entity (Id);
      end if;

      --  Analyze aspect specifications immediately, since we need to recognize
      --  things like Pure early enough to diagnose violations during analysis.

      if Has_Aspects (N) then
         Analyze_Aspect_Specifications (N, Id);
      end if;

      --  Ada 2005 (AI-217): Check if the package has been illegally named
      --  in a limited-with clause of its own context. In this case the error
      --  has been previously notified by Analyze_Context.

      --     limited with Pkg; -- ERROR
      --     package Pkg is ...

      if From_Limited_With (Id) then
         return;
      end if;

      Push_Scope (Id);

      PF := Is_Pure (Enclosing_Lib_Unit_Entity);
      Set_Is_Pure (Id, PF);

      Set_Categorization_From_Pragmas (N);

      Analyze (Specification (N));
      Validate_Categorization_Dependency (N, Id);

      Body_Required := Unit_Requires_Body (Id);

      --  When this spec does not require an explicit body, we know that there
      --  are no entities requiring completion in the language sense; we call
      --  Check_Completion here only to ensure that any nested package
      --  declaration that requires an implicit body gets one. (In the case
      --  where a body is required, Check_Completion is called at the end of
      --  the body's declarative part.)

      if not Body_Required then
         Check_Completion;
      end if;

      Comp_Unit := Nkind (Parent (N)) = N_Compilation_Unit;

      if Comp_Unit then

         --  Set Body_Required indication on the compilation unit node, and
         --  determine whether elaboration warnings may be meaningful on it.

         Set_Body_Required (Parent (N), Body_Required);

         if not Body_Required then
            Set_Suppress_Elaboration_Warnings (Id);
         end if;
      end if;

      End_Package_Scope (Id);

      --  For the declaration of a library unit that is a remote types package,
      --  check legality rules regarding availability of stream attributes for
      --  types that contain non-remote access values. This subprogram performs
      --  visibility tests that rely on the fact that we have exited the scope
      --  of Id.

      if Comp_Unit then
         Validate_RT_RAT_Component (N);
      end if;

      if Debug_Flag_C then
         Outdent;
         Write_Str ("<== package spec ");
         Write_Name (Chars (Id));
         Write_Str (" from ");
         Write_Location (Sloc (N));
         Write_Eol;
      end if;
   end Analyze_Package_Declaration;

   -----------------------------------
   -- Analyze_Package_Specification --
   -----------------------------------

   --  Note that this code is shared for the analysis of generic package specs
   --  (see Sem_Ch12.Analyze_Generic_Package_Declaration for details).

   procedure Analyze_Package_Specification (N : Node_Id) is
      Id           : constant Entity_Id  := Defining_Entity (N);
      Orig_Decl    : constant Node_Id    := Original_Node (Parent (N));
      Vis_Decls    : constant List_Id    := Visible_Declarations (N);
      Priv_Decls   : constant List_Id    := Private_Declarations (N);
      E            : Entity_Id;
      L            : Entity_Id;
      Public_Child : Boolean;

      Private_With_Clauses_Installed : Boolean := False;
      --  In Ada 2005, private with_clauses are visible in the private part
      --  of a nested package, even if it appears in the public part of the
      --  enclosing package. This requires a separate step to install these
      --  private_with_clauses, and remove them at the end of the nested
      --  package.

      procedure Check_One_Tagged_Type_Or_Extension_At_Most;
      --  Issue an error in SPARK mode if a package specification contains
      --  more than one tagged type or type extension.

      procedure Clear_Constants (Id : Entity_Id; FE : Entity_Id);
      --  Clears constant indications (Never_Set_In_Source, Constant_Value, and
      --  Is_True_Constant) on all variables that are entities of Id, and on
      --  the chain whose first element is FE. A recursive call is made for all
      --  packages and generic packages.

      procedure Generate_Parent_References;
      --  For a child unit, generate references to parent units, for
      --  GPS navigation purposes.

      function Is_Public_Child (Child, Unit : Entity_Id) return Boolean;
      --  Child and Unit are entities of compilation units. True if Child
      --  is a public child of Parent as defined in 10.1.1

      procedure Inspect_Unchecked_Union_Completion (Decls : List_Id);
      --  Reject completion of an incomplete or private type declarations
      --  having a known discriminant part by an unchecked union.

      procedure Install_Parent_Private_Declarations (Inst_Id : Entity_Id);
      --  Given the package entity of a generic package instantiation or
      --  formal package whose corresponding generic is a child unit, installs
      --  the private declarations of each of the child unit's parents.
      --  This has to be done at the point of entering the instance package's
      --  private part rather than being done in Sem_Ch12.Install_Parent
      --  (which is where the parents' visible declarations are installed).

      ------------------------------------------------
      -- Check_One_Tagged_Type_Or_Extension_At_Most --
      ------------------------------------------------

      procedure Check_One_Tagged_Type_Or_Extension_At_Most is
         Previous : Node_Id;

         procedure Check_Decls (Decls : List_Id);
         --  Check that either Previous is Empty and Decls does not contain
         --  more than one tagged type or type extension, or Previous is
         --  already set and Decls contains no tagged type or type extension.

         -----------------
         -- Check_Decls --
         -----------------

         procedure Check_Decls (Decls : List_Id) is
            Decl : Node_Id;

         begin
            Decl := First (Decls);
            while Present (Decl) loop
               if Nkind (Decl) = N_Full_Type_Declaration
                 and then Is_Tagged_Type (Defining_Identifier (Decl))
               then
                  if No (Previous) then
                     Previous := Decl;

                  else
                     Error_Msg_Sloc := Sloc (Previous);
                     Check_SPARK_05_Restriction
                       ("at most one tagged type or type extension allowed",
                        "\\ previous declaration#",
                        Decl);
                  end if;
               end if;

               Next (Decl);
            end loop;
         end Check_Decls;

      --  Start of processing for Check_One_Tagged_Type_Or_Extension_At_Most

      begin
         Previous := Empty;
         Check_Decls (Vis_Decls);

         if Present (Priv_Decls) then
            Check_Decls (Priv_Decls);
         end if;
      end Check_One_Tagged_Type_Or_Extension_At_Most;

      ---------------------
      -- Clear_Constants --
      ---------------------

      procedure Clear_Constants (Id : Entity_Id; FE : Entity_Id) is
         E : Entity_Id;

      begin
         --  Ignore package renamings, not interesting and they can cause self
         --  referential loops in the code below.

         if Nkind (Parent (Id)) = N_Package_Renaming_Declaration then
            return;
         end if;

         --  Note: in the loop below, the check for Next_Entity pointing back
         --  to the package entity may seem odd, but it is needed, because a
         --  package can contain a renaming declaration to itself, and such
         --  renamings are generated automatically within package instances.

         E := FE;
         while Present (E) and then E /= Id loop
            if Is_Assignable (E) then
               Set_Never_Set_In_Source (E, False);
               Set_Is_True_Constant    (E, False);
               Set_Current_Value       (E, Empty);
               Set_Is_Known_Null       (E, False);
               Set_Last_Assignment     (E, Empty);

               if not Can_Never_Be_Null (E) then
                  Set_Is_Known_Non_Null (E, False);
               end if;

            elsif Is_Package_Or_Generic_Package (E) then
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

         elsif not Nkind_In (Unit (Cunit (Main_Unit)), N_Subprogram_Body,
                                                       N_Subunit)
         then
            --  If current unit is an ancestor of main unit, generate a
            --  reference to its own parent.

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

      ----------------------------------------
      -- Inspect_Unchecked_Union_Completion --
      ----------------------------------------

      procedure Inspect_Unchecked_Union_Completion (Decls : List_Id) is
         Decl : Node_Id;

      begin
         Decl := First (Decls);
         while Present (Decl) loop

            --  We are looking at an incomplete or private type declaration
            --  with a known_discriminant_part whose full view is an
            --  Unchecked_Union.

            if Nkind_In (Decl, N_Incomplete_Type_Declaration,
                               N_Private_Type_Declaration)
              and then Has_Discriminants (Defining_Identifier (Decl))
              and then Present (Full_View (Defining_Identifier (Decl)))
              and then
                Is_Unchecked_Union (Full_View (Defining_Identifier (Decl)))
            then
               Error_Msg_N
                 ("completion of discriminated partial view "
                  & "cannot be an unchecked union",
                 Full_View (Defining_Identifier (Decl)));
            end if;

            Next (Decl);
         end loop;
      end Inspect_Unchecked_Union_Completion;

      -----------------------------------------
      -- Install_Parent_Private_Declarations --
      -----------------------------------------

      procedure Install_Parent_Private_Declarations (Inst_Id : Entity_Id) is
         Inst_Par  : Entity_Id;
         Gen_Par   : Entity_Id;
         Inst_Node : Node_Id;

      begin
         Inst_Par := Inst_Id;

         Gen_Par :=
           Generic_Parent (Specification (Unit_Declaration_Node (Inst_Par)));
         while Present (Gen_Par) and then Is_Child_Unit (Gen_Par) loop
            Inst_Node := Get_Package_Instantiation_Node (Inst_Par);

            if Nkind_In (Inst_Node, N_Package_Instantiation,
                                    N_Formal_Package_Declaration)
              and then Nkind (Name (Inst_Node)) = N_Expanded_Name
            then
               Inst_Par := Entity (Prefix (Name (Inst_Node)));

               if Present (Renamed_Entity (Inst_Par)) then
                  Inst_Par := Renamed_Entity (Inst_Par);
               end if;

               Gen_Par :=
                 Generic_Parent
                   (Specification (Unit_Declaration_Node (Inst_Par)));

               --  Install the private declarations and private use clauses
               --  of a parent instance of the child instance, unless the
               --  parent instance private declarations have already been
               --  installed earlier in Analyze_Package_Specification, which
               --  happens when a generic child is instantiated, and the
               --  instance is a child of the parent instance.

               --  Installing the use clauses of the parent instance twice
               --  is both unnecessary and wrong, because it would cause the
               --  clauses to be chained to themselves in the use clauses
               --  list of the scope stack entry. That in turn would cause
               --  an endless loop from End_Use_Clauses upon scope exit.

               --  The parent is now fully visible. It may be a hidden open
               --  scope if we are currently compiling some child instance
               --  declared within it, but while the current instance is being
               --  compiled the parent is immediately visible. In particular
               --  its entities must remain visible if a stack save/restore
               --  takes place through a call to Rtsfind.

               if Present (Gen_Par) then
                  if not In_Private_Part (Inst_Par) then
                     Install_Private_Declarations (Inst_Par);
                     Set_Use (Private_Declarations
                                (Specification
                                   (Unit_Declaration_Node (Inst_Par))));
                     Set_Is_Hidden_Open_Scope (Inst_Par, False);
                  end if;

               --  If we've reached the end of the generic instance parents,
               --  then finish off by looping through the nongeneric parents
               --  and installing their private declarations.

               --  If one of the non-generic parents is itself on the scope
               --  stack, do not install its private declarations: they are
               --  installed in due time when the private part of that parent
               --  is analyzed. This is delicate ???

               else
                  while Present (Inst_Par)
                    and then Inst_Par /= Standard_Standard
                    and then (not In_Open_Scopes (Inst_Par)
                               or else not In_Private_Part (Inst_Par))
                  loop
                     Install_Private_Declarations (Inst_Par);
                     Set_Use (Private_Declarations
                                (Specification
                                   (Unit_Declaration_Node (Inst_Par))));
                     Inst_Par := Scope (Inst_Par);
                  end loop;

                  exit;
               end if;

            else
               exit;
            end if;
         end loop;
      end Install_Parent_Private_Declarations;

   --  Start of processing for Analyze_Package_Specification

   begin
      if Present (Vis_Decls) then
         Analyze_Declarations (Vis_Decls);
      end if;

      --  Inspect the entities defined in the package and ensure that all
      --  incomplete types have received full declarations. Build default
      --  initial condition and invariant procedures for all qualifying types.

      E := First_Entity (Id);
      while Present (E) loop

         --  Check on incomplete types

         --  AI05-0213: A formal incomplete type has no completion

         if Ekind (E) = E_Incomplete_Type
           and then No (Full_View (E))
           and then not Is_Generic_Type (E)
         then
            Error_Msg_N ("no declaration in visible part for incomplete}", E);
         end if;

         if Is_Type (E) then

            --  Each private type subject to pragma Default_Initial_Condition
            --  declares a specialized procedure which verifies the assumption
            --  of the pragma. The declaration appears in the visible part of
            --  the package to allow for being called from the outside.

            if Has_Default_Init_Cond (E) then
               Build_Default_Init_Cond_Procedure_Declaration (E);

            --  A private extension inherits the default initial condition
            --  procedure from its parent type.

            elsif Has_Inherited_Default_Init_Cond (E) then
               Inherit_Default_Init_Cond_Procedure (E);
            end if;

            --  If invariants are present, build the invariant procedure for a
            --  private type, but not any of its subtypes or interface types.

            if Has_Invariants (E) then
               if Ekind (E) = E_Private_Subtype then
                  null;
               else
                  Build_Invariant_Procedure (E, N);
               end if;
            end if;
         end if;

         Next_Entity (E);
      end loop;

      if Is_Remote_Call_Interface (Id)
         and then Nkind (Parent (Parent (N))) = N_Compilation_Unit
      then
         Validate_RCI_Declarations (Id);
      end if;

      --  Save global references in the visible declarations, before installing
      --  private declarations of parent unit if there is one, because the
      --  privacy status of types defined in the parent will change. This is
      --  only relevant for generic child units, but is done in all cases for
      --  uniformity.

      if Ekind (Id) = E_Generic_Package
        and then Nkind (Orig_Decl) = N_Generic_Package_Declaration
      then
         declare
            Orig_Spec : constant Node_Id := Specification (Orig_Decl);
            Save_Priv : constant List_Id := Private_Declarations (Orig_Spec);
         begin
            Set_Private_Declarations (Orig_Spec, Empty_List);
            Save_Global_References   (Orig_Decl);
            Set_Private_Declarations (Orig_Spec, Save_Priv);
         end;
      end if;

      --  If package is a public child unit, then make the private declarations
      --  of the parent visible.

      Public_Child := False;

      declare
         Par       : Entity_Id;
         Pack_Decl : Node_Id;
         Par_Spec  : Node_Id;

      begin
         Par := Id;
         Par_Spec := Parent_Spec (Parent (N));

         --  If the package is formal package of an enclosing generic, it is
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
              and then In_Open_Scopes (Par)
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
      else

         --  The current compilation unit may include private with_clauses,
         --  which are visible in the private part of the current nested
         --  package, and have to be installed now. This is not done for
         --  nested instantiations, where the private with_clauses of the
         --  enclosing unit have no effect once the instantiation info is
         --  established and we start analyzing the package declaration.

         declare
            Comp_Unit : constant Entity_Id := Cunit_Entity (Current_Sem_Unit);
         begin
            if Is_Package_Or_Generic_Package (Comp_Unit)
              and then not In_Private_Part (Comp_Unit)
              and then not In_Instance
            then
               Install_Private_With_Clauses (Comp_Unit);
               Private_With_Clauses_Installed := True;
            end if;
         end;
      end if;

      --  If this is a package associated with a generic instance or formal
      --  package, then the private declarations of each of the generic's
      --  parents must be installed at this point.

      if Is_Generic_Instance (Id) then
         Install_Parent_Private_Declarations (Id);
      end if;

      --  Analyze private part if present. The flag In_Private_Part is reset
      --  in End_Package_Scope.

      L := Last_Entity (Id);

      if Present (Priv_Decls) then
         Set_In_Private_Part (Id);

         --  Upon entering a public child's private part, it may be necessary
         --  to declare subprograms that were derived in the package's visible
         --  part but not yet made visible.

         if Public_Child then
            Declare_Inherited_Private_Subprograms (Id);
         end if;

         Analyze_Declarations (Priv_Decls);

         --  Check the private declarations for incomplete deferred constants

         Inspect_Deferred_Constant_Completion (Priv_Decls);

         --  The first private entity is the immediate follower of the last
         --  visible entity, if there was one.

         if Present (L) then
            Set_First_Private_Entity (Id, Next_Entity (L));
         else
            Set_First_Private_Entity (Id, First_Entity (Id));
         end if;

      --  There may be inherited private subprograms that need to be declared,
      --  even in the absence of an explicit private part.  If there are any
      --  public declarations in the package and the package is a public child
      --  unit, then an implicit private part is assumed.

      elsif Present (L) and then Public_Child then
         Set_In_Private_Part (Id);
         Declare_Inherited_Private_Subprograms (Id);
         Set_First_Private_Entity (Id, Next_Entity (L));
      end if;

      E := First_Entity (Id);
      while Present (E) loop

         --  Check rule of 3.6(11), which in general requires waiting till all
         --  full types have been seen.

         if Ekind (E) = E_Record_Type or else Ekind (E) = E_Array_Type then
            Check_Aliased_Component_Types (E);
         end if;

         --  Check preelaborable initialization for full type completing a
         --  private type for which pragma Preelaborable_Initialization given.

         if Is_Type (E)
           and then Must_Have_Preelab_Init (E)
           and then not Has_Preelaborable_Initialization (E)
         then
            Error_Msg_N
              ("full view of & does not have preelaborable initialization", E);
         end if;

         --  An invariant may appear on a full view of a type

         if Is_Type (E)
           and then Has_Private_Declaration (E)
           and then Nkind (Parent (E)) = N_Full_Type_Declaration
         then
            declare
               IP_Built : Boolean := False;

            begin
               if Has_Aspects (Parent (E)) then
                  declare
                     ASN : Node_Id;

                  begin
                     ASN := First (Aspect_Specifications (Parent (E)));
                     while Present (ASN) loop
                        if Nam_In (Chars (Identifier (ASN)),
                             Name_Invariant,
                             Name_Type_Invariant)
                        then
                           Build_Invariant_Procedure (E, N);
                           IP_Built := True;
                           exit;
                        end if;

                        Next (ASN);
                     end loop;
                  end;
               end if;

               --  Invariants may have been inherited from progenitors

               if not IP_Built
                 and then Has_Interfaces (E)
                 and then Has_Inheritable_Invariants (E)
                 and then not Is_Interface (E)
                 and then not Is_Class_Wide_Type (E)
               then
                  Build_Invariant_Procedure (E, N);
               end if;
            end;
         end if;

         Next_Entity (E);
      end loop;

      --  Ada 2005 (AI-216): The completion of an incomplete or private type
      --  declaration having a known_discriminant_part shall not be an
      --  unchecked union type.

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

      --  Remove private_with_clauses of enclosing compilation unit, if they
      --  were installed.

      if Private_With_Clauses_Installed then
         Remove_Private_With_Clauses (Cunit (Current_Sem_Unit));
      end if;

      --  For the case of a library level package, we must go through all the
      --  entities clearing the indications that the value may be constant and
      --  not modified. Why? Because any client of this package may modify
      --  these values freely from anywhere. This also applies to any nested
      --  packages or generic packages.

      --  For now we unconditionally clear constants for packages that are
      --  instances of generic packages. The reason is that we do not have the
      --  body yet, and we otherwise think things are unreferenced when they
      --  are not. This should be fixed sometime (the effect is not terrible,
      --  we just lose some warnings, and also some cases of value propagation)
      --  ???

      if Is_Library_Level_Entity (Id)
        or else Is_Generic_Instance (Id)
      then
         Clear_Constants (Id, First_Entity (Id));
         Clear_Constants (Id, First_Private_Entity (Id));
      end if;

      --  Issue an error in SPARK mode if a package specification contains
      --  more than one tagged type or type extension.

      Check_One_Tagged_Type_Or_Extension_At_Most;

      --  If switch set, output information on why body required

      if List_Body_Required_Info
        and then In_Extended_Main_Source_Unit (Id)
        and then Unit_Requires_Body (Id)
      then
         Unit_Requires_Body_Info (Id);
      end if;
   end Analyze_Package_Specification;

   --------------------------------------
   -- Analyze_Private_Type_Declaration --
   --------------------------------------

   procedure Analyze_Private_Type_Declaration (N : Node_Id) is
      Id : constant Entity_Id := Defining_Identifier (N);
      PF : constant Boolean   := Is_Pure (Enclosing_Lib_Unit_Entity);

   begin
      Generate_Definition (Id);
      Set_Is_Pure         (Id, PF);
      Init_Size_Align     (Id);

      if not Is_Package_Or_Generic_Package (Current_Scope)
        or else In_Private_Part (Current_Scope)
      then
         Error_Msg_N ("invalid context for private declaration", N);
      end if;

      New_Private_Type (N, Id, N);
      Set_Depends_On_Private (Id);

      --  A type declared within a Ghost region is automatically Ghost
      --  (SPARK RM 6.9(2)).

      if Ghost_Mode > None then
         Set_Is_Ghost_Entity (Id);
      end if;

      if Has_Aspects (N) then
         Analyze_Aspect_Specifications (N, Id);
      end if;
   end Analyze_Private_Type_Declaration;

   ----------------------------------
   -- Check_Anonymous_Access_Types --
   ----------------------------------

   procedure Check_Anonymous_Access_Types
     (Spec_Id : Entity_Id;
      P_Body  : Node_Id)
   is
      E  : Entity_Id;
      IR : Node_Id;

   begin
      --  Itype references are only needed by gigi, to force elaboration of
      --  itypes. In the absence of code generation, they are not needed.

      if not Expander_Active then
         return;
      end if;

      E := First_Entity (Spec_Id);
      while Present (E) loop
         if Ekind (E) = E_Anonymous_Access_Type
           and then From_Limited_With (E)
         then
            IR := Make_Itype_Reference (Sloc (P_Body));
            Set_Itype (IR, E);

            if No (Declarations (P_Body)) then
               Set_Declarations (P_Body, New_List (IR));
            else
               Prepend (IR, Declarations (P_Body));
            end if;
         end if;

         Next_Entity (E);
      end loop;
   end Check_Anonymous_Access_Types;

   -------------------------------------------
   -- Declare_Inherited_Private_Subprograms --
   -------------------------------------------

   procedure Declare_Inherited_Private_Subprograms (Id : Entity_Id) is

      function Is_Primitive_Of (T : Entity_Id; S : Entity_Id) return Boolean;
      --  Check whether an inherited subprogram S is an operation of an
      --  untagged derived type T.

      ---------------------
      -- Is_Primitive_Of --
      ---------------------

      function Is_Primitive_Of (T : Entity_Id; S : Entity_Id) return Boolean is
         Formal : Entity_Id;

      begin
         --  If the full view is a scalar type, the type is the anonymous base
         --  type, but the operation mentions the first subtype, so check the
         --  signature against the base type.

         if Base_Type (Etype (S)) = Base_Type (T) then
            return True;

         else
            Formal := First_Formal (S);
            while Present (Formal) loop
               if Base_Type (Etype (Formal)) = Base_Type (T) then
                  return True;
               end if;

               Next_Formal (Formal);
            end loop;

            return False;
         end if;
      end Is_Primitive_Of;

      --  Local variables

      E           : Entity_Id;
      Op_List     : Elist_Id;
      Op_Elmt     : Elmt_Id;
      Op_Elmt_2   : Elmt_Id;
      Prim_Op     : Entity_Id;
      New_Op      : Entity_Id := Empty;
      Parent_Subp : Entity_Id;
      Tag         : Entity_Id;

   --  Start of processing for Declare_Inherited_Private_Subprograms

   begin
      E := First_Entity (Id);
      while Present (E) loop

         --  If the entity is a nonprivate type extension whose parent type
         --  is declared in an open scope, then the type may have inherited
         --  operations that now need to be made visible. Ditto if the entity
         --  is a formal derived type in a child unit.

         if ((Is_Derived_Type (E) and then not Is_Private_Type (E))
               or else
                 (Nkind (Parent (E)) = N_Private_Extension_Declaration
                   and then Is_Generic_Type (E)))
           and then In_Open_Scopes (Scope (Etype (E)))
           and then Is_Base_Type (E)
         then
            if Is_Tagged_Type (E) then
               Op_List := Primitive_Operations (E);
               New_Op  := Empty;
               Tag     := First_Tag_Component (E);

               Op_Elmt := First_Elmt (Op_List);
               while Present (Op_Elmt) loop
                  Prim_Op := Node (Op_Elmt);

                  --  Search primitives that are implicit operations with an
                  --  internal name whose parent operation has a normal name.

                  if Present (Alias (Prim_Op))
                    and then Find_Dispatching_Type (Alias (Prim_Op)) /= E
                    and then not Comes_From_Source (Prim_Op)
                    and then Is_Internal_Name (Chars (Prim_Op))
                    and then not Is_Internal_Name (Chars (Alias (Prim_Op)))
                  then
                     Parent_Subp := Alias (Prim_Op);

                     --  Case 1: Check if the type has also an explicit
                     --  overriding for this primitive.

                     Op_Elmt_2 := Next_Elmt (Op_Elmt);
                     while Present (Op_Elmt_2) loop

                        --  Skip entities with attribute Interface_Alias since
                        --  they are not overriding primitives (these entities
                        --  link an interface primitive with their covering
                        --  primitive)

                        if Chars (Node (Op_Elmt_2)) = Chars (Parent_Subp)
                          and then Type_Conformant (Prim_Op, Node (Op_Elmt_2))
                          and then No (Interface_Alias (Node (Op_Elmt_2)))
                        then
                           --  The private inherited operation has been
                           --  overridden by an explicit subprogram:
                           --  replace the former by the latter.

                           New_Op := Node (Op_Elmt_2);
                           Replace_Elmt (Op_Elmt, New_Op);
                           Remove_Elmt  (Op_List, Op_Elmt_2);
                           Set_Overridden_Operation (New_Op, Parent_Subp);

                           --  We don't need to inherit its dispatching slot.
                           --  Set_All_DT_Position has previously ensured that
                           --  the same slot was assigned to the two primitives

                           if Present (Tag)
                             and then Present (DTC_Entity (New_Op))
                             and then Present (DTC_Entity (Prim_Op))
                           then
                              pragma Assert
                                (DT_Position (New_Op) = DT_Position (Prim_Op));
                              null;
                           end if;

                           goto Next_Primitive;
                        end if;

                        Next_Elmt (Op_Elmt_2);
                     end loop;

                     --  Case 2: We have not found any explicit overriding and
                     --  hence we need to declare the operation (i.e., make it
                     --  visible).

                     Derive_Subprogram (New_Op, Alias (Prim_Op), E, Etype (E));

                     --  Inherit the dispatching slot if E is already frozen

                     if Is_Frozen (E)
                       and then Present (DTC_Entity (Alias (Prim_Op)))
                     then
                        Set_DTC_Entity_Value (E, New_Op);
                        Set_DT_Position_Value (New_Op,
                          DT_Position (Alias (Prim_Op)));
                     end if;

                     pragma Assert
                       (Is_Dispatching_Operation (New_Op)
                         and then Node (Last_Elmt (Op_List)) = New_Op);

                     --  Substitute the new operation for the old one in the
                     --  type's primitive operations list. Since the new
                     --  operation was also just added to the end of list,
                     --  the last element must be removed.

                     --  (Question: is there a simpler way of declaring the
                     --  operation, say by just replacing the name of the
                     --  earlier operation, reentering it in the in the symbol
                     --  table (how?), and marking it as private???)

                     Replace_Elmt (Op_Elmt, New_Op);
                     Remove_Last_Elmt (Op_List);
                  end if;

                  <<Next_Primitive>>
                  Next_Elmt (Op_Elmt);
               end loop;

               --  Generate listing showing the contents of the dispatch table

               if Debug_Flag_ZZ then
                  Write_DT (E);
               end if;

            else
               --  For untagged type, scan forward to locate inherited hidden
               --  operations.

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

                  --  Derived operations appear immediately after the type
                  --  declaration (or the following subtype indication for
                  --  a derived scalar type). Further declarations cannot
                  --  include inherited operations of the type.

                  if Present (Prim_Op) then
                     exit when Ekind (Prim_Op) not in Overloadable_Kind;
                  end if;
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

      --  Reset full declaration pointer to reflect the switched entities and
      --  readjust the next entity chains.

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

         if Is_Child_Unit (Id) then
            null;

         --  Do not enter implicitly inherited non-overridden subprograms of
         --  a tagged type back into visibility if they have non-conformant
         --  homographs (Ada RM 8.3 12.3/2).

         elsif Is_Hidden_Non_Overridden_Subpgm (Id) then
            null;

         else
            Set_Is_Immediately_Visible (Id);
         end if;
      end if;
   end Install_Package_Entity;

   ----------------------------------
   -- Install_Private_Declarations --
   ----------------------------------

   procedure Install_Private_Declarations (P : Entity_Id) is
      Id        : Entity_Id;
      Full      : Entity_Id;
      Priv_Deps : Elist_Id;

      procedure Swap_Private_Dependents (Priv_Deps : Elist_Id);
      --  When the full view of a private type is made available, we do the
      --  same for its private dependents under proper visibility conditions.
      --  When compiling a grand-chid unit this needs to be done recursively.

      -----------------------------
      -- Swap_Private_Dependents --
      -----------------------------

      procedure Swap_Private_Dependents (Priv_Deps : Elist_Id) is
         Deps      : Elist_Id;
         Priv      : Entity_Id;
         Priv_Elmt : Elmt_Id;
         Is_Priv   : Boolean;

      begin
         Priv_Elmt := First_Elmt (Priv_Deps);
         while Present (Priv_Elmt) loop
            Priv := Node (Priv_Elmt);

            --  Before the exchange, verify that the presence of the Full_View
            --  field. This field will be empty if the entity has already been
            --  installed due to a previous call.

            if Present (Full_View (Priv)) and then Is_Visible_Dependent (Priv)
            then
               if Is_Private_Type (Priv) then
                  Deps := Private_Dependents (Priv);
                  Is_Priv := True;
               else
                  Is_Priv := False;
               end if;

               --  For each subtype that is swapped, we also swap the reference
               --  to it in Private_Dependents, to allow access to it when we
               --  swap them out in End_Package_Scope.

               Replace_Elmt (Priv_Elmt, Full_View (Priv));

               --  Ensure that both views of the dependent private subtype are
               --  immediately visible if within some open scope. Check full
               --  view before exchanging views.

               if In_Open_Scopes (Scope (Full_View (Priv))) then
                  Set_Is_Immediately_Visible (Priv);
               end if;

               Exchange_Declarations (Priv);
               Set_Is_Immediately_Visible
                 (Priv, In_Open_Scopes (Scope (Priv)));

               Set_Is_Potentially_Use_Visible
                 (Priv, Is_Potentially_Use_Visible (Node (Priv_Elmt)));

               --  Within a child unit, recurse, except in generic child unit,
               --  which (unfortunately) handle private_dependents separately.

               if Is_Priv
                 and then Is_Child_Unit (Cunit_Entity (Current_Sem_Unit))
                 and then not Is_Empty_Elmt_List (Deps)
                 and then not Inside_A_Generic
               then
                  Swap_Private_Dependents (Deps);
               end if;
            end if;

            Next_Elmt (Priv_Elmt);
         end loop;
      end Swap_Private_Dependents;

   --  Start of processing for Install_Private_Declarations

   begin
      --  First exchange declarations for private types, so that the full
      --  declaration is visible. For each private type, we check its
      --  Private_Dependents list and also exchange any subtypes of or derived
      --  types from it. Finally, if this is a Taft amendment type, the
      --  incomplete declaration is irrelevant, and we want to link the
      --  eventual full declaration with the original private one so we
      --  also skip the exchange.

      Id := First_Entity (P);
      while Present (Id) and then Id /= First_Private_Entity (P) loop
         if Is_Private_Base_Type (Id)
           and then Present (Full_View (Id))
           and then Comes_From_Source (Full_View (Id))
           and then Scope (Full_View (Id)) = Scope (Id)
           and then Ekind (Full_View (Id)) /= E_Incomplete_Type
         then
            --  If there is a use-type clause on the private type, set the full
            --  view accordingly.

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
               --  point the current type is not private either, and we have
               --  to install the underlying full view, which is now visible.
               --  Save the current full view as well, so that all views can be
               --  restored on exit. It may seem that after compiling the child
               --  body there are not environments to restore, but the back-end
               --  expects those links to be valid, and freeze nodes depend on
               --  them.

               if No (Full_View (Full))
                 and then Present (Underlying_Full_View (Full))
               then
                  Set_Full_View (Id, Underlying_Full_View (Full));
                  Set_Underlying_Full_View (Id, Full);

                  Set_Underlying_Full_View (Full, Empty);
                  Set_Is_Frozen (Full_View (Id));
               end if;
            end if;

            Priv_Deps := Private_Dependents (Id);
            Exchange_Declarations (Id);
            Set_Is_Immediately_Visible (Id);
            Swap_Private_Dependents (Priv_Deps);
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
      Id          : Entity_Id;
      Last_Entity : Entity_Id;

   begin
      pragma Assert
        (Is_Package_Or_Generic_Package (P) or else Is_Record_Type (P));

      if Is_Package_Or_Generic_Package (P) then
         Last_Entity := First_Private_Entity (P);
      else
         Last_Entity := Empty;
      end if;

      Id := First_Entity (P);
      while Present (Id) and then Id /= Last_Entity loop
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
      --  Renamings created for actual types have the visibility of the actual

      if Ekind (S) = E_Package
        and then Is_Generic_Instance (S)
        and then (Is_Generic_Actual_Type (Dep)
                   or else Is_Generic_Actual_Type (Full_View (Dep)))
      then
         return True;

      elsif not (Is_Derived_Type (Dep))
        and then Is_Derived_Type (Full_View (Dep))
      then
         --  When instantiating a package body, the scope stack is empty, so
         --  check instead whether the dependent type is defined in the same
         --  scope as the instance itself.

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
        and then (Present (Activation_Chain_Entity (P)) or else Has_RACW (E))
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
      --  For other than Ada 2012, enter the name in the current scope

      if Ada_Version < Ada_2012 then
         Enter_Name (Id);

      --  Ada 2012 (AI05-0162): Enter the name in the current scope. Note that
      --  there may be an incomplete previous view.

      else
         declare
            Prev : Entity_Id;
         begin
            Prev := Find_Type_Name (N);
            pragma Assert (Prev = Id
              or else (Ekind (Prev) = E_Incomplete_Type
                        and then Present (Full_View (Prev))
                        and then Full_View (Prev) = Id));
         end;
      end if;

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

      --  Set tagged flag before processing discriminants, to catch illegal
      --  usage.

      Set_Is_Tagged_Type (Id, Tagged_Present (Def));

      Set_Discriminant_Constraint (Id, No_Elist);
      Set_Stored_Constraint (Id, No_Elist);

      if Present (Discriminant_Specifications (N)) then
         Push_Scope (Id);
         Process_Discriminants (N);
         End_Scope;

      elsif Unknown_Discriminants_Present (N) then
         Set_Has_Unknown_Discriminants (Id);
      end if;

      Set_Private_Dependents (Id, New_Elmt_List);

      if Tagged_Present (Def) then
         Set_Ekind                       (Id, E_Record_Type_With_Private);
         Set_Direct_Primitive_Operations (Id, New_Elmt_List);
         Set_Is_Abstract_Type            (Id, Abstract_Present (Def));
         Set_Is_Limited_Record           (Id, Limited_Present (Def));
         Set_Has_Delayed_Freeze          (Id, True);

         --  Create a class-wide type with the same attributes

         Make_Class_Wide_Type (Id);

      elsif Abstract_Present (Def) then
         Error_Msg_N ("only a tagged type can be abstract", N);
      end if;
   end New_Private_Type;

   ---------------------------------
   -- Requires_Completion_In_Body --
   ---------------------------------

   function Requires_Completion_In_Body
     (Id      : Entity_Id;
      Pack_Id : Entity_Id) return Boolean
   is
   begin
      --  Always ignore child units. Child units get added to the entity list
      --  of a parent unit, but are not original entities of the parent, and
      --  so do not affect whether the parent needs a body.

      if Is_Child_Unit (Id) then
         return False;

      --  Ignore formal packages and their renamings

      elsif Ekind (Id) = E_Package
        and then Nkind (Original_Node (Unit_Declaration_Node (Id))) =
                   N_Formal_Package_Declaration
      then
         return False;

      --  Otherwise test to see if entity requires a completion. Note that
      --  subprogram entities whose declaration does not come from source are
      --  ignored here on the basis that we assume the expander will provide an
      --  implicit completion at some point.

      elsif (Is_Overloadable (Id)
              and then not Ekind_In (Id, E_Enumeration_Literal, E_Operator)
              and then not Is_Abstract_Subprogram (Id)
              and then not Has_Completion (Id)
              and then Comes_From_Source (Parent (Id)))

        or else
          (Ekind (Id) = E_Package
            and then Id /= Pack_Id
            and then not Has_Completion (Id)
            and then Unit_Requires_Body (Id))

        or else
          (Ekind (Id) = E_Incomplete_Type
            and then No (Full_View (Id))
            and then not Is_Generic_Type (Id))

        or else
          (Ekind_In (Id, E_Task_Type, E_Protected_Type)
            and then not Has_Completion (Id))

        or else
          (Ekind (Id) = E_Generic_Package
            and then Id /= Pack_Id
            and then not Has_Completion (Id)
            and then Unit_Requires_Body (Id))

        or else
          (Is_Generic_Subprogram (Id)
            and then not Has_Completion (Id))
      then
         return True;

      --  Otherwise the entity does not require completion in a package body

      else
         return False;
      end if;
   end Requires_Completion_In_Body;

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
      --  Copy to the private declaration the attributes of the full view that
      --  need to be available for the partial view also.

      function Type_In_Use (T : Entity_Id) return Boolean;
      --  Check whether type or base type appear in an active use_type clause

      ------------------------------
      -- Preserve_Full_Attributes --
      ------------------------------

      procedure Preserve_Full_Attributes (Priv, Full : Entity_Id) is
         Priv_Is_Base_Type : constant Boolean := Is_Base_Type (Priv);

      begin
         Set_Size_Info (Priv, (Full));
         Set_RM_Size                 (Priv, RM_Size (Full));
         Set_Size_Known_At_Compile_Time
                                     (Priv, Size_Known_At_Compile_Time (Full));
         Set_Is_Volatile             (Priv, Is_Volatile                (Full));
         Set_Treat_As_Volatile       (Priv, Treat_As_Volatile          (Full));
         Set_Is_Ada_2005_Only        (Priv, Is_Ada_2005_Only           (Full));
         Set_Is_Ada_2012_Only        (Priv, Is_Ada_2012_Only           (Full));
         Set_Has_Pragma_Unmodified   (Priv, Has_Pragma_Unmodified      (Full));
         Set_Has_Pragma_Unreferenced (Priv, Has_Pragma_Unreferenced    (Full));
         Set_Has_Pragma_Unreferenced_Objects
                                     (Priv, Has_Pragma_Unreferenced_Objects
                                                                       (Full));
         if Is_Unchecked_Union (Full) then
            Set_Is_Unchecked_Union (Base_Type (Priv));
         end if;
         --  Why is atomic not copied here ???

         if Referenced (Full) then
            Set_Referenced (Priv);
         end if;

         if Priv_Is_Base_Type then
            Set_Is_Controlled (Priv, Is_Controlled (Base_Type (Full)));
            Set_Finalize_Storage_Only
                              (Priv, Finalize_Storage_Only
                                                   (Base_Type (Full)));
            Set_Has_Task      (Priv, Has_Task      (Base_Type (Full)));
            Set_Has_Protected (Priv, Has_Protected (Base_Type (Full)));
            Set_Has_Controlled_Component
                              (Priv, Has_Controlled_Component
                                                   (Base_Type (Full)));
         end if;

         Set_Freeze_Node (Priv, Freeze_Node (Full));

         --  Propagate information of type invariants, which may be specified
         --  for the full view.

         if Has_Invariants (Full) and not Has_Invariants (Priv) then
            Set_Has_Invariants (Priv);
            Set_Subprograms_For_Type (Priv, Subprograms_For_Type (Full));
         end if;

         if Is_Tagged_Type (Priv)
           and then Is_Tagged_Type (Full)
           and then not Error_Posted (Full)
         then
            if Is_Tagged_Type (Priv) then

               --  If the type is tagged, the tag itself must be available on
               --  the partial view, for expansion purposes.

               Set_First_Entity (Priv, First_Entity (Full));

               --  If there are discriminants in the partial view, these remain
               --  visible. Otherwise only the tag itself is visible, and there
               --  are no nameable components in the partial view.

               if No (Last_Entity (Priv)) then
                  Set_Last_Entity (Priv, First_Entity (Priv));
               end if;
            end if;

            Set_Has_Discriminants (Priv, Has_Discriminants (Full));

            if Has_Discriminants (Full) then
               Set_Discriminant_Constraint (Priv,
                 Discriminant_Constraint (Full));
            end if;
         end if;
      end Preserve_Full_Attributes;

      -----------------
      -- Type_In_Use --
      -----------------

      function Type_In_Use (T : Entity_Id) return Boolean is
      begin
         return Scope (Base_Type (T)) = P
           and then (In_Use (T) or else In_Use (Base_Type (T)));
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

         --  On exit from the package scope, we must preserve the visibility
         --  established by use clauses in the current scope. Two cases:

         --  a) If the entity is an operator, it may be a primitive operator of
         --  a type for which there is a visible use-type clause.

         --  b) for other entities, their use-visibility is determined by a
         --  visible use clause for the package itself. For a generic instance,
         --  the instantiation of the formals appears in the visible part,
         --  but the formals are private and remain so.

         if Ekind (Id) = E_Function
           and then Is_Operator_Symbol_Name (Chars (Id))
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
            if In_Use (P) and then not Is_Hidden (Id) then

               --  A child unit of a use-visible package remains use-visible
               --  only if it is itself a visible child unit. Otherwise it
               --  would remain visible in other contexts where P is use-
               --  visible, because once compiled it stays in the entity list
               --  of its parent unit.

               if Is_Child_Unit (Id) then
                  Set_Is_Potentially_Use_Visible
                    (Id, Is_Visible_Lib_Unit (Id));
               else
                  Set_Is_Potentially_Use_Visible (Id);
               end if;

            else
               Set_Is_Potentially_Use_Visible (Id, False);
            end if;
         end if;

         --  Local entities are not immediately visible outside of the package

         Set_Is_Immediately_Visible (Id, False);

         --  If this is a private type with a full view (for example a local
         --  subtype of a private type declared elsewhere), ensure that the
         --  full view is also removed from visibility: it may be exposed when
         --  swapping views in an instantiation. Similarly, ensure that the
         --  use-visibility is properly set on both views.

         if Is_Type (Id) and then Present (Full_View (Id)) then
            Set_Is_Immediately_Visible     (Full_View (Id), False);
            Set_Is_Potentially_Use_Visible (Full_View (Id),
              Is_Potentially_Use_Visible (Id));
         end if;

         if Is_Tagged_Type (Id) and then Ekind (Id) = E_Record_Type then
            Check_Abstract_Overriding (Id);
            Check_Conventions (Id);
         end if;

         if Ekind_In (Id, E_Private_Type, E_Limited_Private_Type)
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

         --  Case of constant, check for deferred constant declaration with
         --  no full view. Likely just a matter of a missing expression, or
         --  accidental use of the keyword constant.

         elsif Ekind (Id) = E_Constant

           --  OK if constant value present

           and then No (Constant_Value (Id))

           --  OK if full view present

           and then No (Full_View (Id))

           --  OK if imported, since that provides the completion

           and then not Is_Imported (Id)

           --  OK if object declaration replaced by renaming declaration as
           --  a result of OK_To_Rename processing (e.g. for concatenation)

           and then Nkind (Parent (Id)) /= N_Object_Renaming_Declaration

           --  OK if object declaration with the No_Initialization flag set

           and then not (Nkind (Parent (Id)) = N_Object_Declaration
                          and then No_Initialization (Parent (Id)))
         then
            --  If no private declaration is present, we assume the user did
            --  not intend a deferred constant declaration and the problem
            --  is simply that the initializing expression is missing.

            if not Has_Private_Declaration (Etype (Id)) then

               --  We assume that the user did not intend a deferred constant
               --  declaration, and the expression is just missing.

               Error_Msg_N
                 ("constant declaration requires initialization expression",
                   Parent (Id));

               if Is_Limited_Type (Etype (Id)) then
                  Error_Msg_N
                    ("\if variable intended, remove CONSTANT from declaration",
                    Parent (Id));
               end if;

            --  Otherwise if a private declaration is present, then we are
            --  missing the full declaration for the deferred constant.

            else
               Error_Msg_N
                 ("missing full declaration for deferred constant (RM 7.4)",
                  Id);

               if Is_Limited_Type (Etype (Id)) then
                  Error_Msg_N
                    ("\if variable intended, remove CONSTANT from declaration",
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
      --  declarations for private types. Id is now the first private entity
      --  in the package.

      while Present (Id) loop
         if Debug_Flag_E then
            Write_Str ("unlinking private entity ");
            Write_Int (Int (Id));
            Write_Eol;
         end if;

         if Is_Tagged_Type (Id) and then Ekind (Id) = E_Record_Type then
            Check_Abstract_Overriding (Id);
            Check_Conventions (Id);
         end if;

         Set_Is_Immediately_Visible (Id, False);

         if Is_Private_Base_Type (Id) and then Present (Full_View (Id)) then
            Full := Full_View (Id);

            --  If the partial view is not declared in the visible part of the
            --  package (as is the case when it is a type derived from some
            --  other private type in the private part of the current package),
            --  no exchange takes place.

            if No (Parent (Id))
              or else List_Containing (Parent (Id)) /=
                               Visible_Declarations (Specification (Decl))
            then
               goto Next_Id;
            end if;

            --  The entry in the private part points to the full declaration,
            --  which is currently visible. Exchange them so only the private
            --  type declaration remains accessible, and link private and full
            --  declaration in the opposite direction. Before the actual
            --  exchange, we copy back attributes of the full view that must
            --  be available to the partial view too.

            Preserve_Full_Attributes (Id, Full);

            Set_Is_Potentially_Use_Visible (Id, In_Use (P));

            --  The following test may be redundant, as this is already
            --  diagnosed in sem_ch3. ???

            if not Is_Definite_Subtype (Full)
              and then Is_Definite_Subtype (Id)
            then
               Error_Msg_Sloc := Sloc (Parent (Id));
               Error_Msg_NE
                 ("full view of& not compatible with declaration#", Full, Id);
            end if;

            --  Swap out the subtypes and derived types of Id that
            --  were compiled in this scope, or installed previously
            --  by Install_Private_Declarations.

            --  Before we do the swap, we verify the presence of the Full_View
            --  field which may be empty due to a swap by a previous call to
            --  End_Package_Scope (e.g. from the freezing mechanism).

            Priv_Elmt := First_Elmt (Private_Dependents (Id));
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

            --  Now restore the type itself to its private view

            Exchange_Declarations (Id);

            --  If we have installed an underlying full view for a type derived
            --  from a private type in a child unit, restore the proper views
            --  of private and full view. See corresponding code in
            --  Install_Private_Declarations.

            --  After the exchange, Full denotes the private type in the
            --  visible part of the package.

            if Is_Private_Base_Type (Full)
              and then Present (Full_View (Full))
              and then Present (Underlying_Full_View (Full))
              and then In_Package_Body (Current_Scope)
            then
               Set_Full_View (Full, Underlying_Full_View (Full));
               Set_Underlying_Full_View (Full, Empty);
            end if;

         elsif Ekind (Id) = E_Incomplete_Type
           and then Comes_From_Source (Id)
           and then No (Full_View (Id))
         then
            --  Mark Taft amendment types. Verify that there are no primitive
            --  operations declared for the type (3.10.1(9)).

            Set_Has_Completion_In_Body (Id);

            declare
               Elmt : Elmt_Id;
               Subp : Entity_Id;

            begin
               Elmt := First_Elmt (Private_Dependents (Id));
               while Present (Elmt) loop
                  Subp := Node (Elmt);

                  --  Is_Primitive is tested because there can be cases where
                  --  nonprimitive subprograms (in nested packages) are added
                  --  to the Private_Dependents list.

                  if Is_Overloadable (Subp) and then Is_Primitive (Subp) then
                     Error_Msg_NE
                       ("type& must be completed in the private part",
                         Parent (Subp), Id);

                  --  The result type of an access-to-function type cannot be a
                  --  Taft-amendment type, unless the version is Ada 2012 or
                  --  later (see AI05-151).

                  elsif Ada_Version < Ada_2012
                    and then Ekind (Subp) = E_Subprogram_Type
                  then
                     if Etype (Subp) = Id
                       or else
                         (Is_Class_Wide_Type (Etype (Subp))
                           and then Etype (Etype (Subp)) = Id)
                     then
                        Error_Msg_NE
                          ("type& must be completed in the private part",
                             Associated_Node_For_Itype (Subp), Id);
                     end if;
                  end if;

                  Next_Elmt (Elmt);
               end loop;
            end;

         elsif not Is_Child_Unit (Id)
           and then (not Is_Private_Type (Id) or else No (Full_View (Id)))
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

   function Unit_Requires_Body
     (Pack_Id               : Entity_Id;
      Ignore_Abstract_State : Boolean := False) return Boolean
   is
      E : Entity_Id;

      Requires_Body : Boolean := False;
      --  Flag set when the unit has at least one construct that requries
      --  completion in a body.

   begin
      --  Imported entity never requires body. Right now, only subprograms can
      --  be imported, but perhaps in the future we will allow import of
      --  packages.

      if Is_Imported (Pack_Id) then
         return False;

      --  Body required if library package with pragma Elaborate_Body

      elsif Has_Pragma_Elaborate_Body (Pack_Id) then
         return True;

      --  Body required if subprogram

      elsif Is_Subprogram_Or_Generic_Subprogram (Pack_Id) then
         return True;

      --  Treat a block as requiring a body

      elsif Ekind (Pack_Id) = E_Block then
         return True;

      elsif Ekind (Pack_Id) = E_Package
        and then Nkind (Parent (Pack_Id)) = N_Package_Specification
        and then Present (Generic_Parent (Parent (Pack_Id)))
      then
         declare
            G_P : constant Entity_Id := Generic_Parent (Parent (Pack_Id));
         begin
            if Has_Pragma_Elaborate_Body (G_P) then
               return True;
            end if;
         end;
      end if;

      --  Traverse the entity chain of the package and look for constructs that
      --  require a completion in a body.

      E := First_Entity (Pack_Id);
      while Present (E) loop

         --  Skip abstract states because their completion depends on several
         --  criteria (see below).

         if Ekind (E) = E_Abstract_State then
            null;

         elsif Requires_Completion_In_Body (E, Pack_Id) then
            Requires_Body := True;
            exit;
         end if;

         Next_Entity (E);
      end loop;

      --  A [generic] package that defines at least one non-null abstract state
      --  requires a completion only when at least one other construct requires
      --  a completion in a body (SPARK RM 7.1.4(4) and (6)). This check is not
      --  performed if the caller requests this behavior.

      if not Ignore_Abstract_State
        and then Ekind_In (Pack_Id, E_Generic_Package, E_Package)
        and then Has_Non_Null_Abstract_State (Pack_Id)
        and then Requires_Body
      then
         return True;
      end if;

      return Requires_Body;
   end Unit_Requires_Body;

   -----------------------------
   -- Unit_Requires_Body_Info --
   -----------------------------

   procedure Unit_Requires_Body_Info (Pack_Id : Entity_Id) is
      E : Entity_Id;

   begin
      --  An imported entity never requires body. Right now, only subprograms
      --  can be imported, but perhaps in the future we will allow import of
      --  packages.

      if Is_Imported (Pack_Id) then
         return;

      --  Body required if library package with pragma Elaborate_Body

      elsif Has_Pragma_Elaborate_Body (Pack_Id) then
         Error_Msg_N ("info: & requires body (Elaborate_Body)?Y?", Pack_Id);

      --  Body required if subprogram

      elsif Is_Subprogram_Or_Generic_Subprogram (Pack_Id) then
         Error_Msg_N ("info: & requires body (subprogram case)?Y?", Pack_Id);

      --  Body required if generic parent has Elaborate_Body

      elsif Ekind (Pack_Id) = E_Package
        and then Nkind (Parent (Pack_Id)) = N_Package_Specification
        and then Present (Generic_Parent (Parent (Pack_Id)))
      then
         declare
            G_P : constant Entity_Id := Generic_Parent (Parent (Pack_Id));
         begin
            if Has_Pragma_Elaborate_Body (G_P) then
               Error_Msg_N
                 ("info: & requires body (generic parent Elaborate_Body)?Y?",
                  Pack_Id);
            end if;
         end;

      --  A [generic] package that introduces at least one non-null abstract
      --  state requires completion. However, there is a separate rule that
      --  requires that such a package have a reason other than this for a
      --  body being required (if necessary a pragma Elaborate_Body must be
      --  provided). If Ignore_Abstract_State is True, we don't do this check
      --  (so we can use Unit_Requires_Body to check for some other reason).

      elsif Ekind_In (Pack_Id, E_Generic_Package, E_Package)
        and then Present (Abstract_States (Pack_Id))
        and then not Is_Null_State
                       (Node (First_Elmt (Abstract_States (Pack_Id))))
      then
         Error_Msg_N
           ("info: & requires body (non-null abstract state aspect)?Y?",
            Pack_Id);
      end if;

      --  Otherwise search entity chain for entity requiring completion

      E := First_Entity (Pack_Id);
      while Present (E) loop
         if Requires_Completion_In_Body (E, Pack_Id) then
            Error_Msg_Node_2 := E;
            Error_Msg_NE
              ("info: & requires body (& requires completion)?Y?", E, Pack_Id);
         end if;

         Next_Entity (E);
      end loop;
   end Unit_Requires_Body_Info;
end Sem_Ch7;
