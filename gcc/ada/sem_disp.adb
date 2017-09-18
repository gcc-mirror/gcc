------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ D I S P                              --
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

with Atree;    use Atree;
with Debug;    use Debug;
with Elists;   use Elists;
with Einfo;    use Einfo;
with Exp_Disp; use Exp_Disp;
with Exp_Util; use Exp_Util;
with Exp_Ch7;  use Exp_Ch7;
with Exp_Tss;  use Exp_Tss;
with Errout;   use Errout;
with Lib.Xref; use Lib.Xref;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Output;   use Output;
with Restrict; use Restrict;
with Rident;   use Rident;
with Sem;      use Sem;
with Sem_Aux;  use Sem_Aux;
with Sem_Ch3;  use Sem_Ch3;
with Sem_Ch6;  use Sem_Ch6;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Eval; use Sem_Eval;
with Sem_Type; use Sem_Type;
with Sem_Util; use Sem_Util;
with Snames;   use Snames;
with Sinfo;    use Sinfo;
with Tbuild;   use Tbuild;
with Uintp;    use Uintp;
with Warnsw;   use Warnsw;

package body Sem_Disp is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Add_Dispatching_Operation
     (Tagged_Type : Entity_Id;
      New_Op      : Entity_Id);
   --  Add New_Op in the list of primitive operations of Tagged_Type

   function Check_Controlling_Type
     (T    : Entity_Id;
      Subp : Entity_Id) return Entity_Id;
   --  T is the tagged type of a formal parameter or the result of Subp.
   --  If the subprogram has a controlling parameter or result that matches
   --  the type, then returns the tagged type of that parameter or result
   --  (returning the designated tagged type in the case of an access
   --  parameter); otherwise returns empty.

   function Find_Hidden_Overridden_Primitive (S : Entity_Id) return Entity_Id;
   --  [Ada 2012:AI-0125] Find an inherited hidden primitive of the dispatching
   --  type of S that has the same name of S, a type-conformant profile, an
   --  original corresponding operation O that is a primitive of a visible
   --  ancestor of the dispatching type of S and O is visible at the point of
   --  of declaration of S. If the entity is found the Alias of S is set to the
   --  original corresponding operation S and its Overridden_Operation is set
   --  to the found entity; otherwise return Empty.
   --
   --  This routine does not search for non-hidden primitives since they are
   --  covered by the normal Ada 2005 rules.

   function Is_Inherited_Public_Operation (Op : Entity_Id) return Boolean;
   --  Check whether a primitive operation is inherited from an operation
   --  declared in the visible part of its package.

   -------------------------------
   -- Add_Dispatching_Operation --
   -------------------------------

   procedure Add_Dispatching_Operation
     (Tagged_Type : Entity_Id;
      New_Op      : Entity_Id)
   is
      List : constant Elist_Id := Primitive_Operations (Tagged_Type);

   begin
      --  The dispatching operation may already be on the list, if it is the
      --  wrapper for an inherited function of a null extension (see Exp_Ch3
      --  for the construction of function wrappers). The list of primitive
      --  operations must not contain duplicates.

      Append_Unique_Elmt (New_Op, List);
   end Add_Dispatching_Operation;

   --------------------------
   -- Covered_Interface_Op --
   --------------------------

   function Covered_Interface_Op (Prim : Entity_Id) return Entity_Id is
      Tagged_Type : constant Entity_Id := Find_Dispatching_Type (Prim);
      Elmt        : Elmt_Id;
      E           : Entity_Id;

   begin
      pragma Assert (Is_Dispatching_Operation (Prim));

      --  Although this is a dispatching primitive we must check if its
      --  dispatching type is available because it may be the primitive
      --  of a private type not defined as tagged in its partial view.

      if Present (Tagged_Type) and then Has_Interfaces (Tagged_Type) then

         --  If the tagged type is frozen then the internal entities associated
         --  with interfaces are available in the list of primitives of the
         --  tagged type and can be used to speed up this search.

         if Is_Frozen (Tagged_Type) then
            Elmt := First_Elmt (Primitive_Operations (Tagged_Type));
            while Present (Elmt) loop
               E := Node (Elmt);

               if Present (Interface_Alias (E))
                 and then Alias (E) = Prim
               then
                  return Interface_Alias (E);
               end if;

               Next_Elmt (Elmt);
            end loop;

         --  Otherwise we must collect all the interface primitives and check
         --  if the Prim overrides (implements) some interface primitive.

         else
            declare
               Ifaces_List : Elist_Id;
               Iface_Elmt  : Elmt_Id;
               Iface       : Entity_Id;
               Iface_Prim  : Entity_Id;

            begin
               Collect_Interfaces (Tagged_Type, Ifaces_List);
               Iface_Elmt := First_Elmt (Ifaces_List);
               while Present (Iface_Elmt) loop
                  Iface := Node (Iface_Elmt);

                  Elmt := First_Elmt (Primitive_Operations (Iface));
                  while Present (Elmt) loop
                     Iface_Prim := Node (Elmt);

                     if Chars (Iface_Prim) = Chars (Prim)
                       and then Is_Interface_Conformant
                                  (Tagged_Type, Iface_Prim, Prim)
                     then
                        return Iface_Prim;
                     end if;

                     Next_Elmt (Elmt);
                  end loop;

                  Next_Elmt (Iface_Elmt);
               end loop;
            end;
         end if;
      end if;

      return Empty;
   end Covered_Interface_Op;

   -------------------------------
   -- Check_Controlling_Formals --
   -------------------------------

   procedure Check_Controlling_Formals
     (Typ  : Entity_Id;
      Subp : Entity_Id)
   is
      Formal    : Entity_Id;
      Ctrl_Type : Entity_Id;

   begin
      Formal := First_Formal (Subp);
      while Present (Formal) loop
         Ctrl_Type := Check_Controlling_Type (Etype (Formal), Subp);

         if Present (Ctrl_Type) then

            --  When controlling type is concurrent and declared within a
            --  generic or inside an instance use corresponding record type.

            if Is_Concurrent_Type (Ctrl_Type)
              and then Present (Corresponding_Record_Type (Ctrl_Type))
            then
               Ctrl_Type := Corresponding_Record_Type (Ctrl_Type);
            end if;

            if Ctrl_Type = Typ then
               Set_Is_Controlling_Formal (Formal);

               --  Ada 2005 (AI-231): Anonymous access types that are used in
               --  controlling parameters exclude null because it is necessary
               --  to read the tag to dispatch, and null has no tag.

               if Ekind (Etype (Formal)) = E_Anonymous_Access_Type then
                  Set_Can_Never_Be_Null (Etype (Formal));
                  Set_Is_Known_Non_Null (Etype (Formal));
               end if;

               --  Check that the parameter's nominal subtype statically
               --  matches the first subtype.

               if Ekind (Etype (Formal)) = E_Anonymous_Access_Type then
                  if not Subtypes_Statically_Match
                           (Typ, Designated_Type (Etype (Formal)))
                  then
                     Error_Msg_N
                       ("parameter subtype does not match controlling type",
                        Formal);
                  end if;

               --  Within a predicate function, the formal may be a subtype
               --  of a tagged type, given that the predicate is expressed
               --  in terms of the subtype.

               elsif not Subtypes_Statically_Match (Typ, Etype (Formal))
                 and then not Is_Predicate_Function (Subp)
               then
                  Error_Msg_N
                    ("parameter subtype does not match controlling type",
                     Formal);
               end if;

               if Present (Default_Value (Formal)) then

                  --  In Ada 2005, access parameters can have defaults

                  if Ekind (Etype (Formal)) = E_Anonymous_Access_Type
                    and then Ada_Version < Ada_2005
                  then
                     Error_Msg_N
                       ("default not allowed for controlling access parameter",
                        Default_Value (Formal));

                  elsif not Is_Tag_Indeterminate (Default_Value (Formal)) then
                     Error_Msg_N
                       ("default expression must be a tag indeterminate" &
                        " function call", Default_Value (Formal));
                  end if;
               end if;

            elsif Comes_From_Source (Subp) then
               Error_Msg_N
                 ("operation can be dispatching in only one type", Subp);
            end if;
         end if;

         Next_Formal (Formal);
      end loop;

      if Ekind_In (Subp, E_Function, E_Generic_Function) then
         Ctrl_Type := Check_Controlling_Type (Etype (Subp), Subp);

         if Present (Ctrl_Type) then
            if Ctrl_Type = Typ then
               Set_Has_Controlling_Result (Subp);

               --  Check that result subtype statically matches first subtype
               --  (Ada 2005): Subp may have a controlling access result.

               if Subtypes_Statically_Match (Typ, Etype (Subp))
                 or else (Ekind (Etype (Subp)) = E_Anonymous_Access_Type
                            and then
                              Subtypes_Statically_Match
                                (Typ, Designated_Type (Etype (Subp))))
               then
                  null;

               else
                  Error_Msg_N
                    ("result subtype does not match controlling type", Subp);
               end if;

            elsif Comes_From_Source (Subp) then
               Error_Msg_N
                 ("operation can be dispatching in only one type", Subp);
            end if;
         end if;
      end if;
   end Check_Controlling_Formals;

   ----------------------------
   -- Check_Controlling_Type --
   ----------------------------

   function Check_Controlling_Type
     (T    : Entity_Id;
      Subp : Entity_Id) return Entity_Id
   is
      Tagged_Type : Entity_Id := Empty;

   begin
      if Is_Tagged_Type (T) then
         if Is_First_Subtype (T) then
            Tagged_Type := T;
         else
            Tagged_Type := Base_Type (T);
         end if;

      --  If the type is incomplete, it may have been declared without a
      --  Tagged indication, but the full view may be tagged, in which case
      --  that is the controlling type of the subprogram. This is one of the
      --  approx. 579 places in the language where a lookahead would help.

      elsif Ekind (T) = E_Incomplete_Type
        and then Present (Full_View (T))
        and then Is_Tagged_Type (Full_View (T))
      then
         Set_Is_Tagged_Type (T);
         Tagged_Type := Full_View (T);

      elsif Ekind (T) = E_Anonymous_Access_Type
        and then Is_Tagged_Type (Designated_Type (T))
      then
         if Ekind (Designated_Type (T)) /= E_Incomplete_Type then
            if Is_First_Subtype (Designated_Type (T)) then
               Tagged_Type := Designated_Type (T);
            else
               Tagged_Type := Base_Type (Designated_Type (T));
            end if;

         --  Ada 2005: an incomplete type can be tagged. An operation with an
         --  access parameter of the type is dispatching.

         elsif Scope (Designated_Type (T)) = Current_Scope then
            Tagged_Type := Designated_Type (T);

         --  Ada 2005 (AI-50217)

         elsif From_Limited_With (Designated_Type (T))
           and then Has_Non_Limited_View (Designated_Type (T))
           and then Scope (Designated_Type (T)) = Scope (Subp)
         then
            if Is_First_Subtype (Non_Limited_View (Designated_Type (T))) then
               Tagged_Type := Non_Limited_View (Designated_Type (T));
            else
               Tagged_Type := Base_Type (Non_Limited_View
                                         (Designated_Type (T)));
            end if;
         end if;
      end if;

      if No (Tagged_Type) or else Is_Class_Wide_Type (Tagged_Type) then
         return Empty;

      --  The dispatching type and the primitive operation must be defined in
      --  the same scope, except in the case of internal operations and formal
      --  abstract subprograms.

      elsif ((Scope (Subp) = Scope (Tagged_Type) or else Is_Internal (Subp))
               and then (not Is_Generic_Type (Tagged_Type)
                          or else not Comes_From_Source (Subp)))
        or else
          (Is_Formal_Subprogram (Subp) and then Is_Abstract_Subprogram (Subp))
        or else
          (Nkind (Parent (Parent (Subp))) = N_Subprogram_Renaming_Declaration
            and then
              Present (Corresponding_Formal_Spec (Parent (Parent (Subp))))
            and then
              Is_Abstract_Subprogram (Subp))
      then
         return Tagged_Type;

      else
         return Empty;
      end if;
   end Check_Controlling_Type;

   ----------------------------
   -- Check_Dispatching_Call --
   ----------------------------

   procedure Check_Dispatching_Call (N : Node_Id) is
      Loc                    : constant Source_Ptr := Sloc (N);
      Actual                 : Node_Id;
      Formal                 : Entity_Id;
      Control                : Node_Id := Empty;
      Func                   : Entity_Id;
      Subp_Entity            : Entity_Id;
      Indeterm_Ancestor_Call : Boolean := False;
      Indeterm_Ctrl_Type     : Entity_Id;

      Static_Tag : Node_Id := Empty;
      --  If a controlling formal has a statically tagged actual, the tag of
      --  this actual is to be used for any tag-indeterminate actual.

      procedure Check_Direct_Call;
      --  In the case when the controlling actual is a class-wide type whose
      --  root type's completion is a task or protected type, the call is in
      --  fact direct. This routine detects the above case and modifies the
      --  call accordingly.

      procedure Check_Dispatching_Context (Call : Node_Id);
      --  If the call is tag-indeterminate and the entity being called is
      --  abstract, verify that the context is a call that will eventually
      --  provide a tag for dispatching, or has provided one already.

      -----------------------
      -- Check_Direct_Call --
      -----------------------

      procedure Check_Direct_Call is
         Typ : Entity_Id := Etype (Control);
      begin
         --  Predefined primitives do not receive wrappers since they are built
         --  from scratch for the corresponding record of synchronized types.
         --  Equality is in general predefined, but is excluded from the check
         --  when it is user-defined.

         if Is_Predefined_Dispatching_Operation (Subp_Entity)
           and then not Is_User_Defined_Equality (Subp_Entity)
         then
            return;
         end if;

         if Is_Class_Wide_Type (Typ) then
            Typ := Root_Type (Typ);
         end if;

         if Is_Private_Type (Typ) and then Present (Full_View (Typ)) then
            Typ := Full_View (Typ);
         end if;

         if Is_Concurrent_Type (Typ)
              and then
            Present (Corresponding_Record_Type (Typ))
         then
            Typ := Corresponding_Record_Type (Typ);

            --  The concurrent record's list of primitives should contain a
            --  wrapper for the entity of the call, retrieve it.

            declare
               Prim          : Entity_Id;
               Prim_Elmt     : Elmt_Id;
               Wrapper_Found : Boolean := False;

            begin
               Prim_Elmt := First_Elmt (Primitive_Operations (Typ));
               while Present (Prim_Elmt) loop
                  Prim := Node (Prim_Elmt);

                  if Is_Primitive_Wrapper (Prim)
                    and then Wrapped_Entity (Prim) = Subp_Entity
                  then
                     Wrapper_Found := True;
                     exit;
                  end if;

                  Next_Elmt (Prim_Elmt);
               end loop;

               --  A primitive declared between two views should have a
               --  corresponding wrapper.

               pragma Assert (Wrapper_Found);

               --  Modify the call by setting the proper entity

               Set_Entity (Name (N), Prim);
            end;
         end if;
      end Check_Direct_Call;

      -------------------------------
      -- Check_Dispatching_Context --
      -------------------------------

      procedure Check_Dispatching_Context (Call : Node_Id) is
         Subp : constant Entity_Id := Entity (Name (Call));

         procedure Abstract_Context_Error;
         --  Error for abstract call dispatching on result is not dispatching

         ----------------------------
         -- Abstract_Context_Error --
         ----------------------------

         procedure Abstract_Context_Error is
         begin
            if Ekind (Subp) = E_Function then
               Error_Msg_N
                 ("call to abstract function must be dispatching", N);

            --  This error can occur for a procedure in the case of a call to
            --  an abstract formal procedure with a statically tagged operand.

            else
               Error_Msg_N
                 ("call to abstract procedure must be dispatching", N);
            end if;
         end Abstract_Context_Error;

         --  Local variables

         Scop : constant Entity_Id := Current_Scope_No_Loops;
         Typ  : constant Entity_Id := Etype (Subp);
         Par  : Node_Id;

      --  Start of processing for Check_Dispatching_Context

      begin
         --  If the called subprogram is a private overriding, replace it
         --  with its alias, which has the correct body. Verify that the
         --  two subprograms have the same controlling type (this is not the
         --  case for an inherited subprogram that has become abstract).

         if Is_Abstract_Subprogram (Subp)
           and then No (Controlling_Argument (Call))
         then
            if Present (Alias (Subp))
              and then not Is_Abstract_Subprogram (Alias (Subp))
              and then No (DTC_Entity (Subp))
              and then Find_Dispatching_Type (Subp) =
                         Find_Dispatching_Type (Alias (Subp))
            then
               --  Private overriding of inherited abstract operation, call is
               --  legal.

               Set_Entity (Name (N), Alias (Subp));
               return;

            --  An obscure special case: a null procedure may have a class-
            --  wide pre/postcondition that includes a call to an abstract
            --  subp. Calls within the expression may not have been rewritten
            --  as dispatching calls yet, because the null body appears in
            --  the current declarative part. The expression will be properly
            --  rewritten/reanalyzed when the postcondition procedure is built.

            --  Similarly, if this is a pre/postcondition for an abstract
            --  subprogram, it may call another abstract function which is
            --  a primitive of an abstract type. The call is non-dispatching
            --  but will be legal in overridings of the operation.

            elsif (Is_Subprogram (Scop)
                    or else Chars (Scop) = Name_Postcondition)
              and then
                (Is_Abstract_Subprogram (Scop)
                  or else
                    (Nkind (Parent (Scop)) = N_Procedure_Specification
                      and then Null_Present (Parent (Scop))))
            then
               null;

            elsif Ekind (Current_Scope) = E_Function
              and then Nkind (Unit_Declaration_Node (Scop)) =
                         N_Generic_Subprogram_Declaration
            then
               null;

            else
               --  We need to determine whether the context of the call
               --  provides a tag to make the call dispatching. This requires
               --  the call to be the actual in an enclosing call, and that
               --  actual must be controlling.  If the call is an operand of
               --  equality, the other operand must not ve abstract.

               if not Is_Tagged_Type (Typ)
                 and then not
                   (Ekind (Typ) = E_Anonymous_Access_Type
                     and then Is_Tagged_Type (Designated_Type (Typ)))
               then
                  Abstract_Context_Error;
                  return;
               end if;

               Par := Parent (Call);

               if Nkind (Par) = N_Parameter_Association then
                  Par := Parent (Par);
               end if;

               if Nkind (Par) = N_Qualified_Expression
                 or else Nkind (Par) = N_Unchecked_Type_Conversion
               then
                  Par := Parent (Par);
               end if;

               if Nkind_In (Par, N_Function_Call, N_Procedure_Call_Statement)
                 and then Is_Entity_Name (Name (Par))
               then
                  declare
                     Enc_Subp : constant Entity_Id := Entity (Name (Par));
                     A        : Node_Id;
                     F        : Entity_Id;
                     Control  : Entity_Id;
                     Ret_Type : Entity_Id;

                  begin
                     --  Find controlling formal that can provide tag for the
                     --  tag-indeterminate actual. The corresponding actual
                     --  must be the corresponding class-wide type.

                     F := First_Formal (Enc_Subp);
                     A := First_Actual (Par);

                     --  Find controlling type of call. Dereference if function
                     --  returns an access type.

                     Ret_Type := Etype (Call);
                     if Is_Access_Type (Etype (Call)) then
                        Ret_Type := Designated_Type (Ret_Type);
                     end if;

                     while Present (F) loop
                        Control := Etype (A);

                        if Is_Access_Type (Control) then
                           Control := Designated_Type (Control);
                        end if;

                        if Is_Controlling_Formal (F)
                          and then not (Call = A or else Parent (Call) = A)
                          and then Control = Class_Wide_Type (Ret_Type)
                        then
                           return;
                        end if;

                        Next_Formal (F);
                        Next_Actual (A);
                     end loop;

                     if Nkind (Par) = N_Function_Call
                       and then Is_Tag_Indeterminate (Par)
                     then
                        --  The parent may be an actual of an enclosing call

                        Check_Dispatching_Context (Par);
                        return;

                     else
                        Error_Msg_N
                          ("call to abstract function must be dispatching",
                           Call);
                        return;
                     end if;
                  end;

               --  For equality operators, one of the operands must be
               --  statically or dynamically tagged.

               elsif Nkind_In (Par, N_Op_Eq, N_Op_Ne) then
                  if N = Right_Opnd (Par)
                    and then Is_Tag_Indeterminate (Left_Opnd (Par))
                  then
                     Abstract_Context_Error;

                  elsif N = Left_Opnd (Par)
                    and then Is_Tag_Indeterminate (Right_Opnd (Par))
                  then
                     Abstract_Context_Error;
                  end if;

                  return;

               --  The left-hand side of an assignment provides the tag

               elsif Nkind (Par) = N_Assignment_Statement then
                  return;

               else
                  Abstract_Context_Error;
               end if;
            end if;
         end if;
      end Check_Dispatching_Context;

   --  Start of processing for Check_Dispatching_Call

   begin
      --  Find a controlling argument, if any

      if Present (Parameter_Associations (N)) then
         Subp_Entity := Entity (Name (N));

         Actual := First_Actual (N);
         Formal := First_Formal (Subp_Entity);
         while Present (Actual) loop
            Control := Find_Controlling_Arg (Actual);
            exit when Present (Control);

            --  Check for the case where the actual is a tag-indeterminate call
            --  whose result type is different than the tagged type associated
            --  with the containing call, but is an ancestor of the type.

            if Is_Controlling_Formal (Formal)
              and then Is_Tag_Indeterminate (Actual)
              and then Base_Type (Etype (Actual)) /= Base_Type (Etype (Formal))
              and then Is_Ancestor (Etype (Actual), Etype (Formal))
            then
               Indeterm_Ancestor_Call := True;
               Indeterm_Ctrl_Type     := Etype (Formal);

            --  If the formal is controlling but the actual is not, the type
            --  of the actual is statically known, and may be used as the
            --  controlling tag for some other tag-indeterminate actual.

            elsif Is_Controlling_Formal (Formal)
              and then Is_Entity_Name (Actual)
              and then Is_Tagged_Type (Etype (Actual))
            then
               Static_Tag := Actual;
            end if;

            Next_Actual (Actual);
            Next_Formal (Formal);
         end loop;

         --  If the call doesn't have a controlling actual but does have an
         --  indeterminate actual that requires dispatching treatment, then an
         --  object is needed that will serve as the controlling argument for
         --  a dispatching call on the indeterminate actual. This can occur
         --  in the unusual situation of a default actual given by a tag-
         --  indeterminate call and where the type of the call is an ancestor
         --  of the type associated with a containing call to an inherited
         --  operation (see AI-239).

         --  Rather than create an object of the tagged type, which would
         --  be problematic for various reasons (default initialization,
         --  discriminants), the tag of the containing call's associated
         --  tagged type is directly used to control the dispatching.

         if No (Control)
           and then Indeterm_Ancestor_Call
           and then No (Static_Tag)
         then
            Control :=
              Make_Attribute_Reference (Loc,
                Prefix         => New_Occurrence_Of (Indeterm_Ctrl_Type, Loc),
                Attribute_Name => Name_Tag);

            Analyze (Control);
         end if;

         if Present (Control) then

            --  Verify that no controlling arguments are statically tagged

            if Debug_Flag_E then
               Write_Str ("Found Dispatching call");
               Write_Int (Int (N));
               Write_Eol;
            end if;

            Actual := First_Actual (N);
            while Present (Actual) loop
               if Actual /= Control then

                  if not Is_Controlling_Actual (Actual) then
                     null; -- Can be anything

                  elsif Is_Dynamically_Tagged (Actual) then
                     null; -- Valid parameter

                  elsif Is_Tag_Indeterminate (Actual) then

                     --  The tag is inherited from the enclosing call (the node
                     --  we are currently analyzing). Explicitly expand the
                     --  actual, since the previous call to Expand (from
                     --  Resolve_Call) had no way of knowing about the
                     --  required dispatching.

                     Propagate_Tag (Control, Actual);

                  else
                     Error_Msg_N
                       ("controlling argument is not dynamically tagged",
                        Actual);
                     return;
                  end if;
               end if;

               Next_Actual (Actual);
            end loop;

            --  Mark call as a dispatching call

            Set_Controlling_Argument (N, Control);
            Check_Restriction (No_Dispatching_Calls, N);

            --  The dispatching call may need to be converted into a direct
            --  call in certain cases.

            Check_Direct_Call;

         --  If there is a statically tagged actual and a tag-indeterminate
         --  call to a function of the ancestor (such as that provided by a
         --  default), then treat this as a dispatching call and propagate
         --  the tag to the tag-indeterminate call(s).

         elsif Present (Static_Tag) and then Indeterm_Ancestor_Call then
            Control :=
              Make_Attribute_Reference (Loc,
                Prefix         =>
                  New_Occurrence_Of (Etype (Static_Tag), Loc),
                Attribute_Name => Name_Tag);

            Analyze (Control);

            Actual := First_Actual (N);
            Formal := First_Formal (Subp_Entity);
            while Present (Actual) loop
               if Is_Tag_Indeterminate (Actual)
                 and then Is_Controlling_Formal (Formal)
               then
                  Propagate_Tag (Control, Actual);
               end if;

               Next_Actual (Actual);
               Next_Formal (Formal);
            end loop;

            Check_Dispatching_Context (N);

         elsif Nkind (N) /= N_Function_Call then

            --  The call is not dispatching, so check that there aren't any
            --  tag-indeterminate abstract calls left among its actuals.

            Actual := First_Actual (N);
            while Present (Actual) loop
               if Is_Tag_Indeterminate (Actual) then

                  --  Function call case

                  if Nkind (Original_Node (Actual)) = N_Function_Call then
                     Func := Entity (Name (Original_Node (Actual)));

                  --  If the actual is an attribute then it can't be abstract
                  --  (the only current case of a tag-indeterminate attribute
                  --  is the stream Input attribute).

                  elsif Nkind (Original_Node (Actual)) = N_Attribute_Reference
                  then
                     Func := Empty;

                  --  Ditto if it is an explicit dereference

                  elsif Nkind (Original_Node (Actual)) = N_Explicit_Dereference
                  then
                     Func := Empty;

                  --  Only other possibility is a qualified expression whose
                  --  constituent expression is itself a call.

                  else
                     Func :=
                       Entity (Name (Original_Node
                         (Expression (Original_Node (Actual)))));
                  end if;

                  if Present (Func) and then Is_Abstract_Subprogram (Func) then
                     Error_Msg_N
                       ("call to abstract function must be dispatching",
                        Actual);
                  end if;
               end if;

               Next_Actual (Actual);
            end loop;

            Check_Dispatching_Context (N);
            return;

         elsif Nkind (Parent (N)) in N_Subexpr then
            Check_Dispatching_Context (N);

         elsif Nkind (Parent (N)) = N_Assignment_Statement
           and then Is_Class_Wide_Type (Etype (Name (Parent (N))))
         then
            return;

         elsif Is_Abstract_Subprogram (Subp_Entity) then
            Check_Dispatching_Context (N);
            return;
         end if;

      else
         --  If dispatching on result, the enclosing call, if any, will
         --  determine the controlling argument. Otherwise this is the
         --  primitive operation of the root type.

         Check_Dispatching_Context (N);
      end if;
   end Check_Dispatching_Call;

   ---------------------------------
   -- Check_Dispatching_Operation --
   ---------------------------------

   procedure Check_Dispatching_Operation (Subp, Old_Subp : Entity_Id) is
      procedure Warn_On_Late_Primitive_After_Private_Extension
        (Typ  : Entity_Id;
         Prim : Entity_Id);
      --  Prim is a dispatching primitive of the tagged type Typ. Warn on Prim
      --  if it is a public primitive defined after some private extension of
      --  the tagged type.

      ----------------------------------------------------
      -- Warn_On_Late_Primitive_After_Private_Extension --
      ----------------------------------------------------

      procedure Warn_On_Late_Primitive_After_Private_Extension
        (Typ  : Entity_Id;
         Prim : Entity_Id)
      is
         E : Entity_Id;

      begin
         if Warn_On_Late_Primitives
           and then Comes_From_Source (Prim)
           and then Has_Private_Extension (Typ)
           and then Is_Package_Or_Generic_Package (Current_Scope)
           and then not In_Private_Part (Current_Scope)
         then
            E := Next_Entity (Typ);

            while E /= Prim loop
               if Ekind (E) = E_Record_Type_With_Private
                 and then Etype (E) = Typ
               then
                  Error_Msg_Name_1 := Chars (Typ);
                  Error_Msg_Name_2 := Chars (E);
                  Error_Msg_Sloc := Sloc (E);
                  Error_Msg_N
                    ("?j?primitive of type % defined after private extension "
                     & "% #?", Prim);
                  Error_Msg_Name_1 := Chars (Prim);
                  Error_Msg_Name_2 := Chars (E);
                  Error_Msg_N
                    ("\spec of % should appear before declaration of type %!",
                     Prim);
                  exit;
               end if;

               Next_Entity (E);
            end loop;
         end if;
      end Warn_On_Late_Primitive_After_Private_Extension;

      --  Local variables

      Body_Is_Last_Primitive : Boolean   := False;
      Has_Dispatching_Parent : Boolean   := False;
      Ovr_Subp               : Entity_Id := Empty;
      Tagged_Type            : Entity_Id;

   --  Start of processing for Check_Dispatching_Operation

   begin
      if not Ekind_In (Subp, E_Function, E_Procedure) then
         return;

      --  The Default_Initial_Condition procedure is not a primitive subprogram
      --  even if it relates to a tagged type. This routine is not meant to be
      --  inherited or overridden.

      elsif Is_DIC_Procedure (Subp) then
         return;

      --  The "partial" and "full" type invariant procedures are not primitive
      --  subprograms even if they relate to a tagged type. These routines are
      --  not meant to be inherited or overridden.

      elsif Is_Invariant_Procedure (Subp)
        or else Is_Partial_Invariant_Procedure (Subp)
      then
         return;
      end if;

      Set_Is_Dispatching_Operation (Subp, False);
      Tagged_Type := Find_Dispatching_Type (Subp);

      --  Ada 2005 (AI-345): Use the corresponding record (if available).
      --  Required because primitives of concurrent types are attached
      --  to the corresponding record (not to the concurrent type).

      if Ada_Version >= Ada_2005
        and then Present (Tagged_Type)
        and then Is_Concurrent_Type (Tagged_Type)
        and then Present (Corresponding_Record_Type (Tagged_Type))
      then
         Tagged_Type := Corresponding_Record_Type (Tagged_Type);
      end if;

      --  (AI-345): The task body procedure is not a primitive of the tagged
      --  type

      if Present (Tagged_Type)
        and then Is_Concurrent_Record_Type (Tagged_Type)
        and then Present (Corresponding_Concurrent_Type (Tagged_Type))
        and then Is_Task_Type (Corresponding_Concurrent_Type (Tagged_Type))
        and then Subp = Get_Task_Body_Procedure
                          (Corresponding_Concurrent_Type (Tagged_Type))
      then
         return;
      end if;

      --  If Subp is derived from a dispatching operation then it should
      --  always be treated as dispatching. In this case various checks
      --  below will be bypassed. Makes sure that late declarations for
      --  inherited private subprograms are treated as dispatching, even
      --  if the associated tagged type is already frozen.

      Has_Dispatching_Parent :=
        Present (Alias (Subp))
          and then Is_Dispatching_Operation (Alias (Subp));

      if No (Tagged_Type) then

         --  Ada 2005 (AI-251): Check that Subp is not a primitive associated
         --  with an abstract interface type unless the interface acts as a
         --  parent type in a derivation. If the interface type is a formal
         --  type then the operation is not primitive and therefore legal.

         declare
            E   : Entity_Id;
            Typ : Entity_Id;

         begin
            E := First_Entity (Subp);
            while Present (E) loop

               --  For an access parameter, check designated type

               if Ekind (Etype (E)) = E_Anonymous_Access_Type then
                  Typ := Designated_Type (Etype (E));
               else
                  Typ := Etype (E);
               end if;

               if Comes_From_Source (Subp)
                 and then Is_Interface (Typ)
                 and then not Is_Class_Wide_Type (Typ)
                 and then not Is_Derived_Type (Typ)
                 and then not Is_Generic_Type (Typ)
                 and then not In_Instance
               then
                  Error_Msg_N ("??declaration of& is too late!", Subp);
                  Error_Msg_NE -- CODEFIX??
                    ("\??spec should appear immediately after declaration of "
                     & "& !", Subp, Typ);
                  exit;
               end if;

               Next_Entity (E);
            end loop;

            --  In case of functions check also the result type

            if Ekind (Subp) = E_Function then
               if Is_Access_Type (Etype (Subp)) then
                  Typ := Designated_Type (Etype (Subp));
               else
                  Typ := Etype (Subp);
               end if;

               --  The following should be better commented, especially since
               --  we just added several new conditions here ???

               if Comes_From_Source (Subp)
                 and then Is_Interface (Typ)
                 and then not Is_Class_Wide_Type (Typ)
                 and then not Is_Derived_Type (Typ)
                 and then not Is_Generic_Type (Typ)
                 and then not In_Instance
               then
                  Error_Msg_N ("??declaration of& is too late!", Subp);
                  Error_Msg_NE
                    ("\??spec should appear immediately after declaration of "
                     & "& !", Subp, Typ);
               end if;
            end if;
         end;

         return;

      --  The subprograms build internally after the freezing point (such as
      --  init procs, interface thunks, type support subprograms, and Offset
      --  to top functions for accessing interface components in variable
      --  size tagged types) are not primitives.

      elsif Is_Frozen (Tagged_Type)
        and then not Comes_From_Source (Subp)
        and then not Has_Dispatching_Parent
      then
         --  Complete decoration of internally built subprograms that override
         --  a dispatching primitive. These entities correspond with the
         --  following cases:

         --  1. Ada 2005 (AI-391): Wrapper functions built by the expander
         --     to override functions of nonabstract null extensions. These
         --     primitives were added to the list of primitives of the tagged
         --     type by Make_Controlling_Function_Wrappers. However, attribute
         --     Is_Dispatching_Operation must be set to true.

         --  2. Ada 2005 (AI-251): Wrapper procedures of null interface
         --     primitives.

         --  3. Subprograms associated with stream attributes (built by
         --     New_Stream_Subprogram)

         --  4. Wrapper built for inherited operations with inherited class-
         --     wide conditions, where the conditions include calls to other
         --     overridden primitives. The wrappers include checks on these
         --     modified conditions. (AI12-113).

         if Present (Old_Subp)
           and then Present (Overridden_Operation (Subp))
           and then Is_Dispatching_Operation (Old_Subp)
         then
            pragma Assert
              ((Ekind (Subp) = E_Function
                 and then Is_Dispatching_Operation (Old_Subp)
                 and then Is_Null_Extension (Base_Type (Etype (Subp))))

              or else
               (Ekind (Subp) = E_Procedure
                 and then Is_Dispatching_Operation (Old_Subp)
                 and then Present (Alias (Old_Subp))
                 and then Is_Null_Interface_Primitive
                             (Ultimate_Alias (Old_Subp)))

              or else Get_TSS_Name (Subp) = TSS_Stream_Read
              or else Get_TSS_Name (Subp) = TSS_Stream_Write

              or else Present (Contract (Overridden_Operation (Subp))));

            Check_Controlling_Formals (Tagged_Type, Subp);
            Override_Dispatching_Operation (Tagged_Type, Old_Subp, Subp);
            Set_Is_Dispatching_Operation (Subp);
         end if;

         return;

      --  The operation may be a child unit, whose scope is the defining
      --  package, but which is not a primitive operation of the type.

      elsif Is_Child_Unit (Subp) then
         return;

      --  If the subprogram is not defined in a package spec, the only case
      --  where it can be a dispatching op is when it overrides an operation
      --  before the freezing point of the type.

      elsif ((not Is_Package_Or_Generic_Package (Scope (Subp)))
               or else In_Package_Body (Scope (Subp)))
        and then not Has_Dispatching_Parent
      then
         if not Comes_From_Source (Subp)
           or else (Present (Old_Subp) and then not Is_Frozen (Tagged_Type))
         then
            null;

         --  If the type is already frozen, the overriding is not allowed
         --  except when Old_Subp is not a dispatching operation (which can
         --  occur when Old_Subp was inherited by an untagged type). However,
         --  a body with no previous spec freezes the type *after* its
         --  declaration, and therefore is a legal overriding (unless the type
         --  has already been frozen). Only the first such body is legal.

         elsif Present (Old_Subp)
           and then Is_Dispatching_Operation (Old_Subp)
         then
            if Comes_From_Source (Subp)
              and then
                (Nkind (Unit_Declaration_Node (Subp)) = N_Subprogram_Body
                  or else Nkind (Unit_Declaration_Node (Subp)) in N_Body_Stub)
            then
               declare
                  Subp_Body : constant Node_Id := Unit_Declaration_Node (Subp);
                  Decl_Item : Node_Id;

               begin
                  --  ??? The checks here for whether the type has been frozen
                  --  prior to the new body are not complete. It's not simple
                  --  to check frozenness at this point since the body has
                  --  already caused the type to be prematurely frozen in
                  --  Analyze_Declarations, but we're forced to recheck this
                  --  here because of the odd rule interpretation that allows
                  --  the overriding if the type wasn't frozen prior to the
                  --  body. The freezing action should probably be delayed
                  --  until after the spec is seen, but that's a tricky
                  --  change to the delicate freezing code.

                  --  Look at each declaration following the type up until the
                  --  new subprogram body. If any of the declarations is a body
                  --  then the type has been frozen already so the overriding
                  --  primitive is illegal.

                  Decl_Item := Next (Parent (Tagged_Type));
                  while Present (Decl_Item)
                    and then (Decl_Item /= Subp_Body)
                  loop
                     if Comes_From_Source (Decl_Item)
                       and then (Nkind (Decl_Item) in N_Proper_Body
                                  or else Nkind (Decl_Item) in N_Body_Stub)
                     then
                        Error_Msg_N ("overriding of& is too late!", Subp);
                        Error_Msg_N
                          ("\spec should appear immediately after the type!",
                           Subp);
                        exit;
                     end if;

                     Next (Decl_Item);
                  end loop;

                  --  If the subprogram doesn't follow in the list of
                  --  declarations including the type then the type has
                  --  definitely been frozen already and the body is illegal.

                  if No (Decl_Item) then
                     Error_Msg_N ("overriding of& is too late!", Subp);
                     Error_Msg_N
                       ("\spec should appear immediately after the type!",
                        Subp);

                  elsif Is_Frozen (Subp) then

                     --  The subprogram body declares a primitive operation.
                     --  If the subprogram is already frozen, we must update
                     --  its dispatching information explicitly here. The
                     --  information is taken from the overridden subprogram.
                     --  We must also generate a cross-reference entry because
                     --  references to other primitives were already created
                     --  when type was frozen.

                     Body_Is_Last_Primitive := True;

                     if Present (DTC_Entity (Old_Subp)) then
                        Set_DTC_Entity (Subp, DTC_Entity (Old_Subp));
                        Set_DT_Position_Value (Subp, DT_Position (Old_Subp));

                        if not Restriction_Active (No_Dispatching_Calls) then
                           if Building_Static_DT (Tagged_Type) then

                              --  If the static dispatch table has not been
                              --  built then there is nothing else to do now;
                              --  otherwise we notify that we cannot build the
                              --  static dispatch table.

                              if Has_Dispatch_Table (Tagged_Type) then
                                 Error_Msg_N
                                   ("overriding of& is too late for building "
                                    & " static dispatch tables!", Subp);
                                 Error_Msg_N
                                   ("\spec should appear immediately after "
                                    & "the type!", Subp);
                              end if;

                           --  No code required to register primitives in VM
                           --  targets

                           elsif not Tagged_Type_Expansion then
                              null;

                           else
                              Insert_Actions_After (Subp_Body,
                                Register_Primitive (Sloc (Subp_Body),
                                Prim    => Subp));
                           end if;

                           --  Indicate that this is an overriding operation,
                           --  and replace the overridden entry in the list of
                           --  primitive operations, which is used for xref
                           --  generation subsequently.

                           Generate_Reference (Tagged_Type, Subp, 'P', False);
                           Override_Dispatching_Operation
                             (Tagged_Type, Old_Subp, Subp);
                        end if;
                     end if;
                  end if;
               end;

            else
               Error_Msg_N ("overriding of& is too late!", Subp);
               Error_Msg_N
                 ("\subprogram spec should appear immediately after the type!",
                  Subp);
            end if;

         --  If the type is not frozen yet and we are not in the overriding
         --  case it looks suspiciously like an attempt to define a primitive
         --  operation, which requires the declaration to be in a package spec
         --  (3.2.3(6)). Only report cases where the type and subprogram are
         --  in the same declaration list (by checking the enclosing parent
         --  declarations), to avoid spurious warnings on subprograms in
         --  instance bodies when the type is declared in the instance spec
         --  but hasn't been frozen by the instance body.

         elsif not Is_Frozen (Tagged_Type)
           and then In_Same_List (Parent (Tagged_Type), Parent (Parent (Subp)))
         then
            Error_Msg_N
              ("??not dispatching (must be defined in a package spec)", Subp);
            return;

         --  When the type is frozen, it is legitimate to define a new
         --  non-primitive operation.

         else
            return;
         end if;

      --  Now, we are sure that the scope is a package spec. If the subprogram
      --  is declared after the freezing point of the type that's an error

      elsif Is_Frozen (Tagged_Type) and then not Has_Dispatching_Parent then
         Error_Msg_N ("this primitive operation is declared too late", Subp);
         Error_Msg_NE
           ("??no primitive operations for& after this line",
            Freeze_Node (Tagged_Type),
            Tagged_Type);
         return;
      end if;

      Check_Controlling_Formals (Tagged_Type, Subp);

      Ovr_Subp := Old_Subp;

      --  [Ada 2012:AI-0125]: Search for inherited hidden primitive that may be
      --  overridden by Subp. This only applies to source subprograms, and
      --  their declaration must carry an explicit overriding indicator.

      if No (Ovr_Subp)
        and then Ada_Version >= Ada_2012
        and then Comes_From_Source (Subp)
        and then
          Nkind (Unit_Declaration_Node (Subp)) = N_Subprogram_Declaration
      then
         Ovr_Subp := Find_Hidden_Overridden_Primitive (Subp);

         --  Verify that the proper overriding indicator has been supplied.

         if Present (Ovr_Subp)
           and then
             not Must_Override (Specification (Unit_Declaration_Node (Subp)))
         then
            Error_Msg_NE ("missing overriding indicator for&", Subp, Subp);
         end if;
      end if;

      --  Now it should be a correct primitive operation, put it in the list

      if Present (Ovr_Subp) then

         --  If the type has interfaces we complete this check after we set
         --  attribute Is_Dispatching_Operation.

         Check_Subtype_Conformant (Subp, Ovr_Subp);

         --  A primitive operation with the name of a primitive controlled
         --  operation does not override a non-visible overriding controlled
         --  operation, i.e. one declared in a private part when the full
         --  view of a type is controlled. Conversely, it will override a
         --  visible operation that may be declared in a partial view when
         --  the full view is controlled.

         if Nam_In (Chars (Subp), Name_Initialize, Name_Adjust, Name_Finalize)
           and then Is_Controlled (Tagged_Type)
           and then not Is_Visibly_Controlled (Tagged_Type)
           and then not Is_Inherited_Public_Operation (Ovr_Subp)
         then
            Set_Overridden_Operation (Subp, Empty);

            --  If the subprogram specification carries an overriding
            --  indicator, no need for the warning: it is either redundant,
            --  or else an error will be reported.

            if Nkind (Parent (Subp)) = N_Procedure_Specification
              and then
                (Must_Override (Parent (Subp))
                  or else Must_Not_Override (Parent (Subp)))
            then
               null;

            --  Here we need the warning

            else
               Error_Msg_NE
                 ("operation does not override inherited&??", Subp, Subp);
            end if;

         else
            Override_Dispatching_Operation (Tagged_Type, Ovr_Subp, Subp);

            --  Ada 2005 (AI-251): In case of late overriding of a primitive
            --  that covers abstract interface subprograms we must register it
            --  in all the secondary dispatch tables associated with abstract
            --  interfaces. We do this now only if not building static tables,
            --  nor when the expander is inactive (we avoid trying to register
            --  primitives in semantics-only mode, since the type may not have
            --  an associated dispatch table). Otherwise the patch code is
            --  emitted after those tables are built, to prevent access before
            --  elaboration in gigi.

            if Body_Is_Last_Primitive and then Expander_Active then
               declare
                  Subp_Body : constant Node_Id := Unit_Declaration_Node (Subp);
                  Elmt      : Elmt_Id;
                  Prim      : Node_Id;

               begin
                  Elmt := First_Elmt (Primitive_Operations (Tagged_Type));
                  while Present (Elmt) loop
                     Prim := Node (Elmt);

                     --  No code required to register primitives in VM targets

                     if Present (Alias (Prim))
                       and then Present (Interface_Alias (Prim))
                       and then Alias (Prim) = Subp
                       and then not Building_Static_DT (Tagged_Type)
                       and then Tagged_Type_Expansion
                     then
                        Insert_Actions_After (Subp_Body,
                          Register_Primitive (Sloc (Subp_Body), Prim => Prim));
                     end if;

                     Next_Elmt (Elmt);
                  end loop;

                  --  Redisplay the contents of the updated dispatch table

                  if Debug_Flag_ZZ then
                     Write_Str ("Late overriding: ");
                     Write_DT (Tagged_Type);
                  end if;
               end;
            end if;
         end if;

      --  If the tagged type is a concurrent type then we must be compiling
      --  with no code generation (we are either compiling a generic unit or
      --  compiling under -gnatc mode) because we have previously tested that
      --  no serious errors has been reported. In this case we do not add the
      --  primitive to the list of primitives of Tagged_Type but we leave the
      --  primitive decorated as a dispatching operation to be able to analyze
      --  and report errors associated with the Object.Operation notation.

      elsif Is_Concurrent_Type (Tagged_Type) then
         pragma Assert (not Expander_Active);

         --  Attach operation to list of primitives of the synchronized type
         --  itself, for ASIS use.

         Append_Elmt (Subp, Direct_Primitive_Operations (Tagged_Type));

      --  If no old subprogram, then we add this as a dispatching operation,
      --  but we avoid doing this if an error was posted, to prevent annoying
      --  cascaded errors.

      elsif not Error_Posted (Subp) then
         Add_Dispatching_Operation (Tagged_Type, Subp);
      end if;

      Set_Is_Dispatching_Operation (Subp, True);

      --  Ada 2005 (AI-251): If the type implements interfaces we must check
      --  subtype conformance against all the interfaces covered by this
      --  primitive.

      if Present (Ovr_Subp)
        and then Has_Interfaces (Tagged_Type)
      then
         declare
            Ifaces_List     : Elist_Id;
            Iface_Elmt      : Elmt_Id;
            Iface_Prim_Elmt : Elmt_Id;
            Iface_Prim      : Entity_Id;
            Ret_Typ         : Entity_Id;

         begin
            Collect_Interfaces (Tagged_Type, Ifaces_List);

            Iface_Elmt := First_Elmt (Ifaces_List);
            while Present (Iface_Elmt) loop
               if not Is_Ancestor (Node (Iface_Elmt), Tagged_Type) then
                  Iface_Prim_Elmt :=
                    First_Elmt (Primitive_Operations (Node (Iface_Elmt)));
                  while Present (Iface_Prim_Elmt) loop
                     Iface_Prim := Node (Iface_Prim_Elmt);

                     if Is_Interface_Conformant
                          (Tagged_Type, Iface_Prim, Subp)
                     then
                        --  Handle procedures, functions whose return type
                        --  matches, or functions not returning interfaces

                        if Ekind (Subp) = E_Procedure
                          or else Etype (Iface_Prim) = Etype (Subp)
                          or else not Is_Interface (Etype (Iface_Prim))
                        then
                           Check_Subtype_Conformant
                             (New_Id  => Subp,
                              Old_Id  => Iface_Prim,
                              Err_Loc => Subp,
                              Skip_Controlling_Formals => True);

                        --  Handle functions returning interfaces

                        elsif Implements_Interface
                                (Etype (Subp), Etype (Iface_Prim))
                        then
                           --  Temporarily force both entities to return the
                           --  same type. Required because Subtype_Conformant
                           --  does not handle this case.

                           Ret_Typ := Etype (Iface_Prim);
                           Set_Etype (Iface_Prim, Etype (Subp));

                           Check_Subtype_Conformant
                             (New_Id  => Subp,
                              Old_Id  => Iface_Prim,
                              Err_Loc => Subp,
                              Skip_Controlling_Formals => True);

                           Set_Etype (Iface_Prim, Ret_Typ);
                        end if;
                     end if;

                     Next_Elmt (Iface_Prim_Elmt);
                  end loop;
               end if;

               Next_Elmt (Iface_Elmt);
            end loop;
         end;
      end if;

      if not Body_Is_Last_Primitive then
         Set_DT_Position_Value (Subp, No_Uint);

      elsif Has_Controlled_Component (Tagged_Type)
        and then Nam_In (Chars (Subp), Name_Initialize,
                                       Name_Adjust,
                                       Name_Finalize,
                                       Name_Finalize_Address)
      then
         declare
            F_Node   : constant Node_Id := Freeze_Node (Tagged_Type);
            Decl     : Node_Id;
            Old_P    : Entity_Id;
            Old_Bod  : Node_Id;
            Old_Spec : Entity_Id;

            C_Names : constant array (1 .. 4) of Name_Id :=
                        (Name_Initialize,
                         Name_Adjust,
                         Name_Finalize,
                         Name_Finalize_Address);

            D_Names : constant array (1 .. 4) of TSS_Name_Type :=
                        (TSS_Deep_Initialize,
                         TSS_Deep_Adjust,
                         TSS_Deep_Finalize,
                         TSS_Finalize_Address);

         begin
            --  Remove previous controlled function which was constructed and
            --  analyzed when the type was frozen. This requires removing the
            --  body of the redefined primitive, as well as its specification
            --  if needed (there is no spec created for Deep_Initialize, see
            --  exp_ch3.adb). We must also dismantle the exception information
            --  that may have been generated for it when front end zero-cost
            --  tables are enabled.

            for J in D_Names'Range loop
               Old_P := TSS (Tagged_Type, D_Names (J));

               if Present (Old_P)
                and then Chars (Subp) = C_Names (J)
               then
                  Old_Bod := Unit_Declaration_Node (Old_P);
                  Remove (Old_Bod);
                  Set_Is_Eliminated (Old_P);
                  Set_Scope (Old_P,  Scope (Current_Scope));

                  if Nkind (Old_Bod) = N_Subprogram_Body
                    and then Present (Corresponding_Spec (Old_Bod))
                  then
                     Old_Spec := Corresponding_Spec (Old_Bod);
                     Set_Has_Completion             (Old_Spec, False);
                  end if;
               end if;
            end loop;

            Build_Late_Proc (Tagged_Type, Chars (Subp));

            --  The new operation is added to the actions of the freeze node
            --  for the type, but this node has already been analyzed, so we
            --  must retrieve and analyze explicitly the new body.

            if Present (F_Node)
              and then Present (Actions (F_Node))
            then
               Decl := Last (Actions (F_Node));
               Analyze (Decl);
            end if;
         end;
      end if;

      --  For similarity with record extensions, in Ada 9X the language should
      --  have disallowed adding visible operations to a tagged type after
      --  deriving a private extension from it. Report a warning if this
      --  primitive is defined after a private extension of Tagged_Type.

      Warn_On_Late_Primitive_After_Private_Extension (Tagged_Type, Subp);
   end Check_Dispatching_Operation;

   ------------------------------------------
   -- Check_Operation_From_Incomplete_Type --
   ------------------------------------------

   procedure Check_Operation_From_Incomplete_Type
     (Subp : Entity_Id;
      Typ  : Entity_Id)
   is
      Full       : constant Entity_Id := Full_View (Typ);
      Parent_Typ : constant Entity_Id := Etype (Full);
      Old_Prim   : constant Elist_Id  := Primitive_Operations (Parent_Typ);
      New_Prim   : constant Elist_Id  := Primitive_Operations (Full);
      Op1, Op2   : Elmt_Id;
      Prev       : Elmt_Id := No_Elmt;

      function Derives_From (Parent_Subp : Entity_Id) return Boolean;
      --  Check that Subp has profile of an operation derived from Parent_Subp.
      --  Subp must have a parameter or result type that is Typ or an access
      --  parameter or access result type that designates Typ.

      ------------------
      -- Derives_From --
      ------------------

      function Derives_From (Parent_Subp : Entity_Id) return Boolean is
         F1, F2 : Entity_Id;

      begin
         if Chars (Parent_Subp) /= Chars (Subp) then
            return False;
         end if;

         --  Check that the type of controlling formals is derived from the
         --  parent subprogram's controlling formal type (or designated type
         --  if the formal type is an anonymous access type).

         F1 := First_Formal (Parent_Subp);
         F2 := First_Formal (Subp);
         while Present (F1) and then Present (F2) loop
            if Ekind (Etype (F1)) = E_Anonymous_Access_Type then
               if Ekind (Etype (F2)) /= E_Anonymous_Access_Type then
                  return False;
               elsif Designated_Type (Etype (F1)) = Parent_Typ
                 and then Designated_Type (Etype (F2)) /= Full
               then
                  return False;
               end if;

            elsif Ekind (Etype (F2)) = E_Anonymous_Access_Type then
               return False;

            elsif Etype (F1) = Parent_Typ and then Etype (F2) /= Full then
               return False;
            end if;

            Next_Formal (F1);
            Next_Formal (F2);
         end loop;

         --  Check that a controlling result type is derived from the parent
         --  subprogram's result type (or designated type if the result type
         --  is an anonymous access type).

         if Ekind (Parent_Subp) = E_Function then
            if Ekind (Subp) /= E_Function then
               return False;

            elsif Ekind (Etype (Parent_Subp)) = E_Anonymous_Access_Type then
               if Ekind (Etype (Subp)) /= E_Anonymous_Access_Type then
                  return False;

               elsif Designated_Type (Etype (Parent_Subp)) = Parent_Typ
                 and then Designated_Type (Etype (Subp)) /= Full
               then
                  return False;
               end if;

            elsif Ekind (Etype (Subp)) = E_Anonymous_Access_Type then
               return False;

            elsif Etype (Parent_Subp) = Parent_Typ
              and then Etype (Subp) /= Full
            then
               return False;
            end if;

         elsif Ekind (Subp) = E_Function then
            return False;
         end if;

         return No (F1) and then No (F2);
      end Derives_From;

   --  Start of processing for Check_Operation_From_Incomplete_Type

   begin
      --  The operation may override an inherited one, or may be a new one
      --  altogether. The inherited operation will have been hidden by the
      --  current one at the point of the type derivation, so it does not
      --  appear in the list of primitive operations of the type. We have to
      --  find the proper place of insertion in the list of primitive opera-
      --  tions by iterating over the list for the parent type.

      Op1 := First_Elmt (Old_Prim);
      Op2 := First_Elmt (New_Prim);
      while Present (Op1) and then Present (Op2) loop
         if Derives_From (Node (Op1)) then
            if No (Prev) then

               --  Avoid adding it to the list of primitives if already there

               if Node (Op2) /= Subp then
                  Prepend_Elmt (Subp, New_Prim);
               end if;

            else
               Insert_Elmt_After (Subp, Prev);
            end if;

            return;
         end if;

         Prev := Op2;
         Next_Elmt (Op1);
         Next_Elmt (Op2);
      end loop;

      --  Operation is a new primitive

      Append_Elmt (Subp, New_Prim);
   end Check_Operation_From_Incomplete_Type;

   ---------------------------------------
   -- Check_Operation_From_Private_View --
   ---------------------------------------

   procedure Check_Operation_From_Private_View (Subp, Old_Subp : Entity_Id) is
      Tagged_Type : Entity_Id;

   begin
      if Is_Dispatching_Operation (Alias (Subp)) then
         Set_Scope (Subp, Current_Scope);
         Tagged_Type := Find_Dispatching_Type (Subp);

         --  Add Old_Subp to primitive operations if not already present

         if Present (Tagged_Type) and then Is_Tagged_Type (Tagged_Type) then
            Append_Unique_Elmt (Old_Subp, Primitive_Operations (Tagged_Type));

            --  If Old_Subp isn't already marked as dispatching then this is
            --  the case of an operation of an untagged private type fulfilled
            --  by a tagged type that overrides an inherited dispatching
            --  operation, so we set the necessary dispatching attributes here.

            if not Is_Dispatching_Operation (Old_Subp) then

               --  If the untagged type has no discriminants, and the full
               --  view is constrained, there will be a spurious mismatch of
               --  subtypes on the controlling arguments, because the tagged
               --  type is the internal base type introduced in the derivation.
               --  Use the original type to verify conformance, rather than the
               --  base type.

               if not Comes_From_Source (Tagged_Type)
                 and then Has_Discriminants (Tagged_Type)
               then
                  declare
                     Formal : Entity_Id;

                  begin
                     Formal := First_Formal (Old_Subp);
                     while Present (Formal) loop
                        if Tagged_Type = Base_Type (Etype (Formal)) then
                           Tagged_Type := Etype (Formal);
                        end if;

                        Next_Formal (Formal);
                     end loop;
                  end;

                  if Tagged_Type = Base_Type (Etype (Old_Subp)) then
                     Tagged_Type := Etype (Old_Subp);
                  end if;
               end if;

               Check_Controlling_Formals (Tagged_Type, Old_Subp);
               Set_Is_Dispatching_Operation (Old_Subp, True);
               Set_DT_Position_Value (Old_Subp, No_Uint);
            end if;

            --  If the old subprogram is an explicit renaming of some other
            --  entity, it is not overridden by the inherited subprogram.
            --  Otherwise, update its alias and other attributes.

            if Present (Alias (Old_Subp))
              and then Nkind (Unit_Declaration_Node (Old_Subp)) /=
                                        N_Subprogram_Renaming_Declaration
            then
               Set_Alias (Old_Subp, Alias (Subp));

               --  The derived subprogram should inherit the abstractness of
               --  the parent subprogram (except in the case of a function
               --  returning the type). This sets the abstractness properly
               --  for cases where a private extension may have inherited an
               --  abstract operation, but the full type is derived from a
               --  descendant type and inherits a nonabstract version.

               if Etype (Subp) /= Tagged_Type then
                  Set_Is_Abstract_Subprogram
                    (Old_Subp, Is_Abstract_Subprogram (Alias (Subp)));
               end if;
            end if;
         end if;
      end if;
   end Check_Operation_From_Private_View;

   --------------------------
   -- Find_Controlling_Arg --
   --------------------------

   function Find_Controlling_Arg (N : Node_Id) return Node_Id is
      Orig_Node : constant Node_Id := Original_Node (N);
      Typ       : Entity_Id;

   begin
      if Nkind (Orig_Node) = N_Qualified_Expression then
         return Find_Controlling_Arg (Expression (Orig_Node));
      end if;

      --  Dispatching on result case. If expansion is disabled, the node still
      --  has the structure of a function call. However, if the function name
      --  is an operator and the call was given in infix form, the original
      --  node has no controlling result and we must examine the current node.

      if Nkind (N) = N_Function_Call
        and then Present (Controlling_Argument (N))
        and then Has_Controlling_Result (Entity (Name (N)))
      then
         return Controlling_Argument (N);

      --  If expansion is enabled, the call may have been transformed into
      --  an indirect call, and we need to recover the original node.

      elsif Nkind (Orig_Node) = N_Function_Call
        and then Present (Controlling_Argument (Orig_Node))
        and then Has_Controlling_Result (Entity (Name (Orig_Node)))
      then
         return Controlling_Argument (Orig_Node);

      --  Type conversions are dynamically tagged if the target type, or its
      --  designated type, are classwide. An interface conversion expands into
      --  a dereference, so test must be performed on the original node.

      elsif Nkind (Orig_Node) = N_Type_Conversion
        and then Nkind (N) = N_Explicit_Dereference
        and then Is_Controlling_Actual (N)
      then
         declare
            Target_Type : constant Entity_Id :=
                             Entity (Subtype_Mark (Orig_Node));

         begin
            if Is_Class_Wide_Type (Target_Type) then
               return N;

            elsif Is_Access_Type (Target_Type)
              and then Is_Class_Wide_Type (Designated_Type (Target_Type))
            then
               return N;

            else
               return Empty;
            end if;
         end;

      --  Normal case

      elsif Is_Controlling_Actual (N)
        or else
         (Nkind (Parent (N)) = N_Qualified_Expression
           and then Is_Controlling_Actual (Parent (N)))
      then
         Typ := Etype (N);

         if Is_Access_Type (Typ) then

            --  In the case of an Access attribute, use the type of the prefix,
            --  since in the case of an actual for an access parameter, the
            --  attribute's type may be of a specific designated type, even
            --  though the prefix type is class-wide.

            if Nkind (N) = N_Attribute_Reference then
               Typ := Etype (Prefix (N));

            --  An allocator is dispatching if the type of qualified expression
            --  is class_wide, in which case this is the controlling type.

            elsif Nkind (Orig_Node) = N_Allocator
               and then Nkind (Expression (Orig_Node)) = N_Qualified_Expression
            then
               Typ := Etype (Expression (Orig_Node));
            else
               Typ := Designated_Type (Typ);
            end if;
         end if;

         if Is_Class_Wide_Type (Typ)
           or else
             (Nkind (Parent (N)) = N_Qualified_Expression
               and then Is_Access_Type (Etype (N))
               and then Is_Class_Wide_Type (Designated_Type (Etype (N))))
         then
            return N;
         end if;
      end if;

      return Empty;
   end Find_Controlling_Arg;

   ---------------------------
   -- Find_Dispatching_Type --
   ---------------------------

   function Find_Dispatching_Type (Subp : Entity_Id) return Entity_Id is
      A_Formal  : Entity_Id;
      Formal    : Entity_Id;
      Ctrl_Type : Entity_Id;

   begin
      if Ekind_In (Subp, E_Function, E_Procedure)
        and then Present (DTC_Entity (Subp))
      then
         return Scope (DTC_Entity (Subp));

      --  For subprograms internally generated by derivations of tagged types
      --  use the alias subprogram as a reference to locate the dispatching
      --  type of Subp.

      elsif not Comes_From_Source (Subp)
        and then Present (Alias (Subp))
        and then Is_Dispatching_Operation (Alias (Subp))
      then
         if Ekind (Alias (Subp)) = E_Function
           and then Has_Controlling_Result (Alias (Subp))
         then
            return Check_Controlling_Type (Etype (Subp), Subp);

         else
            Formal   := First_Formal (Subp);
            A_Formal := First_Formal (Alias (Subp));
            while Present (A_Formal) loop
               if Is_Controlling_Formal (A_Formal) then
                  return Check_Controlling_Type (Etype (Formal), Subp);
               end if;

               Next_Formal (Formal);
               Next_Formal (A_Formal);
            end loop;

            pragma Assert (False);
            return Empty;
         end if;

      --  General case

      else
         Formal := First_Formal (Subp);
         while Present (Formal) loop
            Ctrl_Type := Check_Controlling_Type (Etype (Formal), Subp);

            if Present (Ctrl_Type) then
               return Ctrl_Type;
            end if;

            Next_Formal (Formal);
         end loop;

         --  The subprogram may also be dispatching on result

         if Present (Etype (Subp)) then
            return Check_Controlling_Type (Etype (Subp), Subp);
         end if;
      end if;

      pragma Assert (not Is_Dispatching_Operation (Subp));
      return Empty;
   end Find_Dispatching_Type;

   --------------------------------------
   -- Find_Hidden_Overridden_Primitive --
   --------------------------------------

   function Find_Hidden_Overridden_Primitive (S : Entity_Id) return Entity_Id
   is
      Tag_Typ   : constant Entity_Id := Find_Dispatching_Type (S);
      Elmt      : Elmt_Id;
      Orig_Prim : Entity_Id;
      Prim      : Entity_Id;
      Vis_List  : Elist_Id;

   begin
      --  This Ada 2012 rule applies only for type extensions or private
      --  extensions, where the parent type is not in a parent unit, and
      --  where an operation is never declared but still inherited.

      if No (Tag_Typ)
        or else not Is_Record_Type (Tag_Typ)
        or else Etype (Tag_Typ) = Tag_Typ
        or else In_Open_Scopes (Scope (Etype (Tag_Typ)))
      then
         return Empty;
      end if;

      --  Collect the list of visible ancestor of the tagged type

      Vis_List := Visible_Ancestors (Tag_Typ);

      Elmt := First_Elmt (Primitive_Operations (Tag_Typ));
      while Present (Elmt) loop
         Prim := Node (Elmt);

         --  Find an inherited hidden dispatching primitive with the name of S
         --  and a type-conformant profile.

         if Present (Alias (Prim))
           and then Is_Hidden (Alias (Prim))
           and then Find_Dispatching_Type (Alias (Prim)) /= Tag_Typ
           and then Primitive_Names_Match (S, Prim)
           and then Type_Conformant (S, Prim)
         then
            declare
               Vis_Ancestor : Elmt_Id;
               Elmt         : Elmt_Id;

            begin
               --  The original corresponding operation of Prim must be an
               --  operation of a visible ancestor of the dispatching type S,
               --  and the original corresponding operation of S2 must be
               --  visible.

               Orig_Prim := Original_Corresponding_Operation (Prim);

               if Orig_Prim /= Prim
                 and then Is_Immediately_Visible (Orig_Prim)
               then
                  Vis_Ancestor := First_Elmt (Vis_List);
                  while Present (Vis_Ancestor) loop
                     Elmt :=
                       First_Elmt (Primitive_Operations (Node (Vis_Ancestor)));
                     while Present (Elmt) loop
                        if Node (Elmt) = Orig_Prim then
                           Set_Overridden_Operation (S, Prim);
                           Set_Alias (Prim, Orig_Prim);
                           return Prim;
                        end if;

                        Next_Elmt (Elmt);
                     end loop;

                     Next_Elmt (Vis_Ancestor);
                  end loop;
               end if;
            end;
         end if;

         Next_Elmt (Elmt);
      end loop;

      return Empty;
   end Find_Hidden_Overridden_Primitive;

   ---------------------------------------
   -- Find_Primitive_Covering_Interface --
   ---------------------------------------

   function Find_Primitive_Covering_Interface
     (Tagged_Type : Entity_Id;
      Iface_Prim  : Entity_Id) return Entity_Id
   is
      E  : Entity_Id;
      El : Elmt_Id;

   begin
      pragma Assert (Is_Interface (Find_Dispatching_Type (Iface_Prim))
        or else (Present (Alias (Iface_Prim))
                  and then
                    Is_Interface
                      (Find_Dispatching_Type (Ultimate_Alias (Iface_Prim)))));

      --  Search in the homonym chain. Done to speed up locating visible
      --  entities and required to catch primitives associated with the partial
      --  view of private types when processing the corresponding full view.

      E := Current_Entity (Iface_Prim);
      while Present (E) loop
         if Is_Subprogram (E)
           and then Is_Dispatching_Operation (E)
           and then Is_Interface_Conformant (Tagged_Type, Iface_Prim, E)
         then
            return E;
         end if;

         E := Homonym (E);
      end loop;

      --  Search in the list of primitives of the type. Required to locate
      --  the covering primitive if the covering primitive is not visible
      --  (for example, non-visible inherited primitive of private type).

      El := First_Elmt (Primitive_Operations (Tagged_Type));
      while Present (El) loop
         E := Node (El);

         --  Keep separate the management of internal entities that link
         --  primitives with interface primitives from tagged type primitives.

         if No (Interface_Alias (E)) then
            if Present (Alias (E)) then

               --  This interface primitive has not been covered yet

               if Alias (E) = Iface_Prim then
                  return E;

               --  The covering primitive was inherited

               elsif Overridden_Operation (Ultimate_Alias (E))
                       = Iface_Prim
               then
                  return E;
               end if;
            end if;

            --  Check if E covers the interface primitive (includes case in
            --  which E is an inherited private primitive).

            if Is_Interface_Conformant (Tagged_Type, Iface_Prim, E) then
               return E;
            end if;

         --  Use the internal entity that links the interface primitive with
         --  the covering primitive to locate the entity.

         elsif Interface_Alias (E) = Iface_Prim then
            return Alias (E);
         end if;

         Next_Elmt (El);
      end loop;

      --  Not found

      return Empty;
   end Find_Primitive_Covering_Interface;

   ---------------------------
   -- Inherited_Subprograms --
   ---------------------------

   function Inherited_Subprograms
     (S               : Entity_Id;
      No_Interfaces   : Boolean := False;
      Interfaces_Only : Boolean := False;
      One_Only        : Boolean := False) return Subprogram_List
   is
      Result : Subprogram_List (1 .. 6000);
      --  6000 here is intended to be infinity. We could use an expandable
      --  table, but it would be awfully heavy, and there is no way that we
      --  could reasonably exceed this value.

      N : Nat := 0;
      --  Number of entries in Result

      Parent_Op : Entity_Id;
      --  Traverses the Overridden_Operation chain

      procedure Store_IS (E : Entity_Id);
      --  Stores E in Result if not already stored

      --------------
      -- Store_IS --
      --------------

      procedure Store_IS (E : Entity_Id) is
      begin
         for J in 1 .. N loop
            if E = Result (J) then
               return;
            end if;
         end loop;

         N := N + 1;
         Result (N) := E;
      end Store_IS;

   --  Start of processing for Inherited_Subprograms

   begin
      pragma Assert (not (No_Interfaces and Interfaces_Only));

      if Present (S) and then Is_Dispatching_Operation (S) then

         --  Deal with direct inheritance

         if not Interfaces_Only then
            Parent_Op := S;
            loop
               Parent_Op := Overridden_Operation (Parent_Op);
               exit when No (Parent_Op)
                 or else
                   (No_Interfaces
                     and then
                       Is_Interface (Find_Dispatching_Type (Parent_Op)));

               if Is_Subprogram_Or_Generic_Subprogram (Parent_Op) then
                  Store_IS (Parent_Op);

                  if One_Only then
                     goto Done;
                  end if;
               end if;
            end loop;
         end if;

         --  Now deal with interfaces

         if not No_Interfaces then
            declare
               Tag_Typ : Entity_Id;
               Prim    : Entity_Id;
               Elmt    : Elmt_Id;

            begin
               Tag_Typ := Find_Dispatching_Type (S);

               --  In the presence of limited views there may be no visible
               --  dispatching type. Primitives will be inherited when non-
               --  limited view is frozen.

               if No (Tag_Typ) then
                  return Result (1 .. 0);
               end if;

               if Is_Concurrent_Type (Tag_Typ) then
                  Tag_Typ := Corresponding_Record_Type (Tag_Typ);
               end if;

               --  Search primitive operations of dispatching type

               if Present (Tag_Typ)
                 and then Present (Primitive_Operations (Tag_Typ))
               then
                  Elmt := First_Elmt (Primitive_Operations (Tag_Typ));
                  while Present (Elmt) loop
                     Prim := Node (Elmt);

                     --  The following test eliminates some odd cases in which
                     --  Ekind (Prim) is Void, to be investigated further ???

                     if not Is_Subprogram_Or_Generic_Subprogram (Prim) then
                        null;

                     --  For [generic] subprogram, look at interface alias

                     elsif Present (Interface_Alias (Prim))
                       and then Alias (Prim) = S
                     then
                        --  We have found a primitive covered by S

                        Store_IS (Interface_Alias (Prim));

                        if One_Only then
                           goto Done;
                        end if;
                     end if;

                     Next_Elmt (Elmt);
                  end loop;
               end if;
            end;
         end if;
      end if;

      <<Done>>

      return Result (1 .. N);
   end Inherited_Subprograms;

   ---------------------------
   -- Is_Dynamically_Tagged --
   ---------------------------

   function Is_Dynamically_Tagged (N : Node_Id) return Boolean is
   begin
      if Nkind (N) = N_Error then
         return False;

      elsif Present (Find_Controlling_Arg (N)) then
         return True;

      --  Special cases: entities, and calls that dispatch on result

      elsif Is_Entity_Name (N) then
         return Is_Class_Wide_Type (Etype (N));

      elsif Nkind (N) = N_Function_Call
         and then Is_Class_Wide_Type (Etype (N))
      then
         return True;

      --  Otherwise check whether call has controlling argument

      else
         return False;
      end if;
   end Is_Dynamically_Tagged;

   ---------------------------------
   -- Is_Null_Interface_Primitive --
   ---------------------------------

   function Is_Null_Interface_Primitive (E : Entity_Id) return Boolean is
   begin
      return Comes_From_Source (E)
        and then Is_Dispatching_Operation (E)
        and then Ekind (E) = E_Procedure
        and then Null_Present (Parent (E))
        and then Is_Interface (Find_Dispatching_Type (E));
   end Is_Null_Interface_Primitive;

   -----------------------------------
   -- Is_Inherited_Public_Operation --
   -----------------------------------

   function Is_Inherited_Public_Operation (Op : Entity_Id) return Boolean is
      Prim      : constant Entity_Id := Alias (Op);
      Scop      : constant Entity_Id := Scope (Prim);
      Pack_Decl : Node_Id;

   begin
      if Comes_From_Source (Prim) and then Ekind (Scop) = E_Package then
         Pack_Decl := Unit_Declaration_Node (Scop);
         return Nkind (Pack_Decl) = N_Package_Declaration
           and then List_Containing (Unit_Declaration_Node (Prim)) =
                            Visible_Declarations (Specification (Pack_Decl));

      else
         return False;
      end if;
   end Is_Inherited_Public_Operation;

   ------------------------------
   -- Is_Overriding_Subprogram --
   ------------------------------

   function Is_Overriding_Subprogram (E : Entity_Id) return Boolean is
      Inherited : constant Subprogram_List :=
                    Inherited_Subprograms (E, One_Only => True);
   begin
      return Inherited'Length > 0;
   end Is_Overriding_Subprogram;

   --------------------------
   -- Is_Tag_Indeterminate --
   --------------------------

   function Is_Tag_Indeterminate (N : Node_Id) return Boolean is
      Nam       : Entity_Id;
      Actual    : Node_Id;
      Orig_Node : constant Node_Id := Original_Node (N);

   begin
      if Nkind (Orig_Node) = N_Function_Call
        and then Is_Entity_Name (Name (Orig_Node))
      then
         Nam := Entity (Name (Orig_Node));

         if not Has_Controlling_Result (Nam) then
            return False;

         --  The function may have a controlling result, but if the return type
         --  is not visibly tagged, then this is not tag-indeterminate.

         elsif Is_Access_Type (Etype (Nam))
           and then not Is_Tagged_Type (Designated_Type (Etype (Nam)))
         then
            return False;

         --  An explicit dereference means that the call has already been
         --  expanded and there is no tag to propagate.

         elsif Nkind (N) = N_Explicit_Dereference then
            return False;

         --  If there are no actuals, the call is tag-indeterminate

         elsif No (Parameter_Associations (Orig_Node)) then
            return True;

         else
            Actual := First_Actual (Orig_Node);
            while Present (Actual) loop
               if Is_Controlling_Actual (Actual)
                 and then not Is_Tag_Indeterminate (Actual)
               then
                  --  One operand is dispatching

                  return False;
               end if;

               Next_Actual (Actual);
            end loop;

            return True;
         end if;

      elsif Nkind (Orig_Node) = N_Qualified_Expression then
         return Is_Tag_Indeterminate (Expression (Orig_Node));

      --  Case of a call to the Input attribute (possibly rewritten), which is
      --  always tag-indeterminate except when its prefix is a Class attribute.

      elsif Nkind (Orig_Node) = N_Attribute_Reference
        and then
          Get_Attribute_Id (Attribute_Name (Orig_Node)) = Attribute_Input
        and then Nkind (Prefix (Orig_Node)) /= N_Attribute_Reference
      then
         return True;

      --  In Ada 2005, a function that returns an anonymous access type can be
      --  dispatching, and the dereference of a call to such a function can
      --  also be tag-indeterminate if the call itself is.

      elsif Nkind (Orig_Node) = N_Explicit_Dereference
        and then Ada_Version >= Ada_2005
      then
         return Is_Tag_Indeterminate (Prefix (Orig_Node));

      else
         return False;
      end if;
   end Is_Tag_Indeterminate;

   ------------------------------------
   -- Override_Dispatching_Operation --
   ------------------------------------

   procedure Override_Dispatching_Operation
     (Tagged_Type : Entity_Id;
      Prev_Op     : Entity_Id;
      New_Op      : Entity_Id;
      Is_Wrapper  : Boolean := False)
   is
      Elmt : Elmt_Id;
      Prim : Node_Id;

   begin
      --  Diagnose failure to match No_Return in parent (Ada-2005, AI-414, but
      --  we do it unconditionally in Ada 95 now, since this is our pragma).

      if No_Return (Prev_Op) and then not No_Return (New_Op) then
         Error_Msg_N ("procedure & must have No_Return pragma", New_Op);
         Error_Msg_N ("\since overridden procedure has No_Return", New_Op);
      end if;

      --  If there is no previous operation to override, the type declaration
      --  was malformed, and an error must have been emitted already.

      Elmt := First_Elmt (Primitive_Operations (Tagged_Type));
      while Present (Elmt) and then Node (Elmt) /= Prev_Op loop
         Next_Elmt (Elmt);
      end loop;

      if No (Elmt) then
         return;
      end if;

      --  The location of entities that come from source in the list of
      --  primitives of the tagged type must follow their order of occurrence
      --  in the sources to fulfill the C++ ABI. If the overridden entity is a
      --  primitive of an interface that is not implemented by the parents of
      --  this tagged type (that is, it is an alias of an interface primitive
      --  generated by Derive_Interface_Progenitors), then we must append the
      --  new entity at the end of the list of primitives.

      if Present (Alias (Prev_Op))
        and then Etype (Tagged_Type) /= Tagged_Type
        and then Is_Interface (Find_Dispatching_Type (Alias (Prev_Op)))
        and then not Is_Ancestor (Find_Dispatching_Type (Alias (Prev_Op)),
                                  Tagged_Type, Use_Full_View => True)
        and then not Implements_Interface
                       (Etype (Tagged_Type),
                        Find_Dispatching_Type (Alias (Prev_Op)))
      then
         Remove_Elmt (Primitive_Operations (Tagged_Type), Elmt);
         Append_Elmt (New_Op, Primitive_Operations (Tagged_Type));

      --  The new primitive replaces the overridden entity. Required to ensure
      --  that overriding primitive is assigned the same dispatch table slot.

      else
         Replace_Elmt (Elmt, New_Op);
      end if;

      if Ada_Version >= Ada_2005 and then Has_Interfaces (Tagged_Type) then

         --  Ada 2005 (AI-251): Update the attribute alias of all the aliased
         --  entities of the overridden primitive to reference New_Op, and
         --  also propagate the proper value of Is_Abstract_Subprogram. Verify
         --  that the new operation is subtype conformant with the interface
         --  operations that it implements (for operations inherited from the
         --  parent itself, this check is made when building the derived type).

         --  Note: This code is executed with internally generated wrappers of
         --  functions with controlling result and late overridings.

         Elmt := First_Elmt (Primitive_Operations (Tagged_Type));
         while Present (Elmt) loop
            Prim := Node (Elmt);

            if Prim = New_Op then
               null;

            --  Note: The check on Is_Subprogram protects the frontend against
            --  reading attributes in entities that are not yet fully decorated

            elsif Is_Subprogram (Prim)
              and then Present (Interface_Alias (Prim))
              and then Alias (Prim) = Prev_Op
            then
               Set_Alias (Prim, New_Op);

               --  No further decoration needed yet for internally generated
               --  wrappers of controlling functions since (at this stage)
               --  they are not yet decorated.

               if not Is_Wrapper then
                  Check_Subtype_Conformant (New_Op, Prim);

                  Set_Is_Abstract_Subprogram (Prim,
                    Is_Abstract_Subprogram (New_Op));

                  --  Ensure that this entity will be expanded to fill the
                  --  corresponding entry in its dispatch table.

                  if not Is_Abstract_Subprogram (Prim) then
                     Set_Has_Delayed_Freeze (Prim);
                  end if;
               end if;
            end if;

            Next_Elmt (Elmt);
         end loop;
      end if;

      if (not Is_Package_Or_Generic_Package (Current_Scope))
        or else not In_Private_Part (Current_Scope)
      then
         --  Not a private primitive

         null;

      else pragma Assert (Is_Inherited_Operation (Prev_Op));

         --  Make the overriding operation into an alias of the implicit one.
         --  In this fashion a call from outside ends up calling the new body
         --  even if non-dispatching, and a call from inside calls the over-
         --  riding operation because it hides the implicit one. To indicate
         --  that the body of Prev_Op is never called, set its dispatch table
         --  entity to Empty. If the overridden operation has a dispatching
         --  result, so does the overriding one.

         Set_Alias (Prev_Op, New_Op);
         Set_DTC_Entity (Prev_Op, Empty);
         Set_Has_Controlling_Result (New_Op, Has_Controlling_Result (Prev_Op));
         return;
      end if;
   end Override_Dispatching_Operation;

   -------------------
   -- Propagate_Tag --
   -------------------

   procedure Propagate_Tag (Control : Node_Id; Actual : Node_Id) is
      Call_Node : Node_Id;
      Arg       : Node_Id;

   begin
      if Nkind (Actual) = N_Function_Call then
         Call_Node := Actual;

      elsif Nkind (Actual) = N_Identifier
        and then Nkind (Original_Node (Actual)) = N_Function_Call
      then
         --  Call rewritten as object declaration when stack-checking is
         --  enabled. Propagate tag to expression in declaration, which is
         --  original call.

         Call_Node := Expression (Parent (Entity (Actual)));

      --  Ada 2005: If this is a dereference of a call to a function with a
      --  dispatching access-result, the tag is propagated when the dereference
      --  itself is expanded (see exp_ch6.adb) and there is nothing else to do.

      elsif Nkind (Actual) = N_Explicit_Dereference
        and then Nkind (Original_Node (Prefix (Actual))) = N_Function_Call
      then
         return;

      --  When expansion is suppressed, an unexpanded call to 'Input can occur,
      --  and in that case we can simply return.

      elsif Nkind (Actual) = N_Attribute_Reference then
         pragma Assert (Attribute_Name (Actual) = Name_Input);

         return;

      --  Only other possibilities are parenthesized or qualified expression,
      --  or an expander-generated unchecked conversion of a function call to
      --  a stream Input attribute.

      else
         Call_Node := Expression (Actual);
      end if;

      --  No action needed if the call has been already expanded

      if Is_Expanded_Dispatching_Call (Call_Node) then
         return;
      end if;

      --  Do not set the Controlling_Argument if already set. This happens in
      --  the special case of _Input (see Exp_Attr, case Input).

      if No (Controlling_Argument (Call_Node)) then
         Set_Controlling_Argument (Call_Node, Control);
      end if;

      Arg := First_Actual (Call_Node);
      while Present (Arg) loop
         if Is_Tag_Indeterminate (Arg) then
            Propagate_Tag (Control,  Arg);
         end if;

         Next_Actual (Arg);
      end loop;

      --  Expansion of dispatching calls is suppressed on VM targets, because
      --  the VM back-ends directly handle the generation of dispatching calls
      --  and would have to undo any expansion to an indirect call.

      if Tagged_Type_Expansion then
         declare
            Call_Typ : constant Entity_Id := Etype (Call_Node);

         begin
            Expand_Dispatching_Call (Call_Node);

            --  If the controlling argument is an interface type and the type
            --  of Call_Node differs then we must add an implicit conversion to
            --  force displacement of the pointer to the object to reference
            --  the secondary dispatch table of the interface.

            if Is_Interface (Etype (Control))
              and then Etype (Control) /= Call_Typ
            then
               --  Cannot use Convert_To because the previous call to
               --  Expand_Dispatching_Call leaves decorated the Call_Node
               --  with the type of Control.

               Rewrite (Call_Node,
                 Make_Type_Conversion (Sloc (Call_Node),
                   Subtype_Mark =>
                     New_Occurrence_Of (Etype (Control), Sloc (Call_Node)),
                   Expression => Relocate_Node (Call_Node)));
               Set_Etype (Call_Node, Etype (Control));
               Set_Analyzed (Call_Node);

               Expand_Interface_Conversion (Call_Node);
            end if;
         end;

      --  Expansion of a dispatching call results in an indirect call, which in
      --  turn causes current values to be killed (see Resolve_Call), so on VM
      --  targets we do the call here to ensure consistent warnings between VM
      --  and non-VM targets.

      else
         Kill_Current_Values;
      end if;
   end Propagate_Tag;

end Sem_Disp;
