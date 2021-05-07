------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E X P _ C H 8                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2021, Free Software Foundation, Inc.         --
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
with Exp_Ch3;        use Exp_Ch3;
with Exp_Ch4;        use Exp_Ch4;
with Exp_Ch6;        use Exp_Ch6;
with Exp_Dbug;       use Exp_Dbug;
with Exp_Util;       use Exp_Util;
with Freeze;         use Freeze;
with Namet;          use Namet;
with Nmake;          use Nmake;
with Nlists;         use Nlists;
with Opt;            use Opt;
with Sem;            use Sem;
with Sem_Aux;        use Sem_Aux;
with Sem_Ch8;        use Sem_Ch8;
with Sem_Util;       use Sem_Util;
with Sinfo;          use Sinfo;
with Sinfo.Nodes;    use Sinfo.Nodes;
with Sinfo.Utils;    use Sinfo.Utils;
with Snames;         use Snames;
with Stand;          use Stand;
with Tbuild;         use Tbuild;

package body Exp_Ch8 is

   ---------------------------------------------
   -- Expand_N_Exception_Renaming_Declaration --
   ---------------------------------------------

   procedure Expand_N_Exception_Renaming_Declaration (N : Node_Id) is
      Decl : Node_Id;

   begin
      Decl := Debug_Renaming_Declaration (N);

      if Present (Decl) then
         Insert_Action (N, Decl);
      end if;
   end Expand_N_Exception_Renaming_Declaration;

   ------------------------------------------
   -- Expand_N_Object_Renaming_Declaration --
   ------------------------------------------

   --  Most object renaming cases can be done by just capturing the address
   --  of the renamed object. The cases in which this is not true are when
   --  this address is not computable, since it involves extraction of a
   --  packed array element, or of a record component to which a component
   --  clause applies (that can specify an arbitrary bit boundary), or where
   --  the enclosing record itself has a non-standard representation.

   --  In Ada 2020, a third case arises when the renamed object is a nonatomic
   --  subcomponent of an atomic object, because reads of or writes to it must
   --  access the enclosing atomic object. That's also the case for an object
   --  subject to the Volatile_Full_Access GNAT aspect/pragma in any language
   --  version. For the sake of simplicity, we treat any subcomponent of an
   --  atomic or Volatile_Full_Access object in any language version this way.

   --  In these three cases, we pre-evaluate the renaming expression, by
   --  extracting and freezing the values of any subscripts, and then we
   --  set the flag Is_Renaming_Of_Object which means that any reference
   --  to the object will be handled by macro substitution in the front
   --  end, and the back end will know to ignore the renaming declaration.

   --  An additional odd case that requires processing by expansion is
   --  the renaming of a discriminant of a mutable record type. The object
   --  is a constant because it renames something that cannot be assigned to,
   --  but in fact the underlying value can change and must be reevaluated
   --  at each reference. Gigi does have a notion of a "constant view" of
   --  an object, and therefore the front-end must perform the expansion.
   --  For simplicity, and to bypass some obscure code-generation problem,
   --  we use macro substitution for all renamed discriminants, whether the
   --  enclosing type is constrained or not.

   --  The other special processing required is for the case of renaming
   --  of an object of a class wide type, where it is necessary to build
   --  the appropriate subtype for the renamed object.
   --  More comments needed for this para ???

   procedure Expand_N_Object_Renaming_Declaration (N : Node_Id) is
      function Evaluation_Required (Nam : Node_Id) return Boolean;
      --  Determines whether it is necessary to do static name evaluation for
      --  renaming of Nam. It is considered necessary if evaluating the name
      --  involves indexing a packed array, or extracting a component of a
      --  record to which a component clause applies, or a subcomponent of an
      --  atomic object. Note that we are only interested in these operations
      --  if they occur as part of the name itself, subscripts are just values
      --  that are computed as part of the evaluation, so they are unimportant.
      --  In addition, always return True for Modify_Tree_For_C since the
      --  code generator doesn't know how to handle renamings.

      -------------------------
      -- Evaluation_Required --
      -------------------------

      function Evaluation_Required (Nam : Node_Id) return Boolean is
      begin
         if Modify_Tree_For_C then
            return True;

         elsif Nkind (Nam) in N_Indexed_Component | N_Slice then
            if Is_Packed (Etype (Prefix (Nam))) then
               return True;

            elsif Is_Full_Access_Object (Prefix (Nam)) then
               return True;

            else
               return Evaluation_Required (Prefix (Nam));
            end if;

         elsif Nkind (Nam) = N_Selected_Component then
            declare
               Rec_Type : constant Entity_Id := Etype (Prefix (Nam));

            begin
               if Present (Component_Clause (Entity (Selector_Name (Nam))))
                 or else Has_Non_Standard_Rep (Rec_Type)
               then
                  return True;

               elsif Ekind (Entity (Selector_Name (Nam))) = E_Discriminant
                 and then Is_Record_Type (Rec_Type)
                 and then not Is_Concurrent_Record_Type (Rec_Type)
               then
                  return True;

               elsif Is_Full_Access_Object (Prefix (Nam)) then
                  return True;

               else
                  return Evaluation_Required (Prefix (Nam));
               end if;
            end;

         else
            return False;
         end if;
      end Evaluation_Required;

      --  Local variables

      Decl : Node_Id;
      Nam  : constant Node_Id   := Name (N);
      T    : constant Entity_Id := Etype (Defining_Identifier (N));

   --  Start of processing for Expand_N_Object_Renaming_Declaration

   begin
      --  Perform name evaluation if required

      if Evaluation_Required (Nam) then
         Evaluate_Name (Nam);
         Set_Is_Renaming_Of_Object (Defining_Identifier (N));
      end if;

      --  Deal with construction of subtype in class-wide case

      if Is_Class_Wide_Type (T) then
         Expand_Subtype_From_Expr (N, T, Subtype_Mark (N), Name (N));
         Find_Type (Subtype_Mark (N));
         Set_Etype (Defining_Identifier (N), Entity (Subtype_Mark (N)));

         --  Freeze the class-wide subtype here to ensure that the subtype
         --  and equivalent type are frozen before the renaming.

         Freeze_Before (N, Entity (Subtype_Mark (N)));
      end if;

      --  Ada 2005 (AI-318-02): If the renamed object is a call to a build-in-
      --  place function, then a temporary return object needs to be created
      --  and access to it must be passed to the function.

      if Is_Build_In_Place_Function_Call (Nam) then
         Make_Build_In_Place_Call_In_Anonymous_Context (Nam);

      --  Ada 2005 (AI-318-02): Specialization of previous case for renaming
      --  containing build-in-place function calls whose returned object covers
      --  interface types.

      elsif Present (Unqual_BIP_Iface_Function_Call (Nam)) then
         Make_Build_In_Place_Iface_Call_In_Anonymous_Context (Nam);
      end if;

      --  Create renaming entry for debug information. Mark the entity as
      --  needing debug info if it comes from sources because the current
      --  setting in Freeze_Entity occurs too late. ???

      Set_Debug_Info_Defining_Id (N);
      Decl := Debug_Renaming_Declaration (N);

      if Present (Decl) then
         Insert_Action (N, Decl);
      end if;
   end Expand_N_Object_Renaming_Declaration;

   -------------------------------------------
   -- Expand_N_Package_Renaming_Declaration --
   -------------------------------------------

   procedure Expand_N_Package_Renaming_Declaration (N : Node_Id) is
      Decl : Node_Id;

   begin
      Decl := Debug_Renaming_Declaration (N);

      if Present (Decl) then

         --  If we are in a compilation unit, then this is an outer
         --  level declaration, and must have a scope of Standard

         if Nkind (Parent (N)) = N_Compilation_Unit then
            declare
               Aux : constant Node_Id := Aux_Decls_Node (Parent (N));

            begin
               Push_Scope (Standard_Standard);

               if No (Actions (Aux)) then
                  Set_Actions (Aux, New_List (Decl));
               else
                  Append (Decl, Actions (Aux));
               end if;

               Analyze (Decl);

               --  Enter the debug variable in the qualification list, which
               --  must be done at this point because auxiliary declarations
               --  occur at the library level and aren't associated with a
               --  normal scope.

               Qualify_Entity_Names (Decl);

               Pop_Scope;
            end;

         --  Otherwise, just insert after the package declaration

         else
            Insert_Action (N, Decl);
         end if;
      end if;
   end Expand_N_Package_Renaming_Declaration;

   ----------------------------------------------
   -- Expand_N_Subprogram_Renaming_Declaration --
   ----------------------------------------------

   procedure Expand_N_Subprogram_Renaming_Declaration (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);
      Id  : constant Entity_Id  := Defining_Entity (N);

      function Build_Body_For_Renaming (Typ : Entity_Id) return Node_Id;
      --  Build and return the body for the renaming declaration of an equality
      --  or inequality operator of type Typ.

      -----------------------------
      -- Build_Body_For_Renaming --
      -----------------------------

      function Build_Body_For_Renaming (Typ : Entity_Id) return Node_Id is
         Left    : constant Entity_Id := First_Formal (Id);
         Right   : constant Entity_Id := Next_Formal (Left);
         Body_Id : Entity_Id;
         Decl    : Node_Id;

      begin
         Set_Alias (Id, Empty);
         Set_Has_Completion (Id, False);
         Rewrite (N,
           Make_Subprogram_Declaration (Sloc (N),
             Specification => Specification (N)));
         Set_Has_Delayed_Freeze (Id);

         Body_Id := Make_Defining_Identifier (Sloc (N), Chars (Id));
         Set_Debug_Info_Needed (Body_Id);

         if Has_Variant_Part (Typ) then
            Decl :=
              Build_Variant_Record_Equality
                (Typ         => Typ,
                 Body_Id     => Body_Id,
                 Param_Specs => Copy_Parameter_List (Id));

         --  Build body for renamed equality, to capture its current meaning.
         --  It may be redefined later, but the renaming is elaborated where
         --  it occurs. This is technically known as Squirreling semantics.
         --  Renaming is rewritten as a subprogram declaration, and the
         --  generated body is inserted into the freeze actions for the
         --  subprogram.

         else
            Decl :=
              Make_Subprogram_Body (Loc,
                Specification              =>
                  Make_Function_Specification (Loc,
                    Defining_Unit_Name       => Body_Id,
                    Parameter_Specifications => Copy_Parameter_List (Id),
                    Result_Definition        =>
                      New_Occurrence_Of (Standard_Boolean, Loc)),
                Declarations               => Empty_List,
                Handled_Statement_Sequence => Empty);

            Set_Handled_Statement_Sequence (Decl,
              Make_Handled_Sequence_Of_Statements (Loc,
                Statements => New_List (
                  Make_Simple_Return_Statement (Loc,
                    Expression =>
                      Expand_Record_Equality
                        (Id,
                         Typ    => Typ,
                         Lhs    => Make_Identifier (Loc, Chars (Left)),
                         Rhs    => Make_Identifier (Loc, Chars (Right)),
                         Bodies => Declarations (Decl))))));
         end if;

         return Decl;
      end Build_Body_For_Renaming;

      --  Local variables

      Nam : constant Node_Id := Name (N);

   --  Start of processing for Expand_N_Subprogram_Renaming_Declaration

   begin
      --  When the prefix of the name is a function call, we must force the
      --  call to be made by removing side effects from the call, since we
      --  must only call the function once.

      if Nkind (Nam) = N_Selected_Component
        and then Nkind (Prefix (Nam)) = N_Function_Call
      then
         Remove_Side_Effects (Prefix (Nam));

      --  For an explicit dereference, the prefix must be captured to prevent
      --  reevaluation on calls through the renaming, which could result in
      --  calling the wrong subprogram if the access value were to be changed.

      elsif Nkind (Nam) = N_Explicit_Dereference then
         Force_Evaluation (Prefix (Nam));
      end if;

      --  Handle cases where we build a body for a renamed equality

      if Is_Entity_Name (Nam)
        and then Chars (Entity (Nam)) = Name_Op_Eq
        and then Scope (Entity (Nam)) = Standard_Standard
      then
         declare
            Typ  : constant Entity_Id := Etype (First_Formal (Id));

         begin
            --  Check whether this is a renaming of a predefined equality on an
            --  untagged record type (AI05-0123).

            if Ada_Version >= Ada_2012
              and then Is_Record_Type (Typ)
              and then not Is_Tagged_Type (Typ)
              and then not Is_Frozen (Typ)
            then
               Append_Freeze_Action (Id, Build_Body_For_Renaming (Typ));
            end if;
         end;
      end if;
   end Expand_N_Subprogram_Renaming_Declaration;

end Exp_Ch8;
