------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ A T T R                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2005 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

with Atree;    use Atree;
with Checks;   use Checks;
with Einfo;    use Einfo;
with Elists;   use Elists;
with Exp_Ch2;  use Exp_Ch2;
with Exp_Ch9;  use Exp_Ch9;
with Exp_Imgv; use Exp_Imgv;
with Exp_Pakd; use Exp_Pakd;
with Exp_Strm; use Exp_Strm;
with Exp_Tss;  use Exp_Tss;
with Exp_Util; use Exp_Util;
with Gnatvsn;  use Gnatvsn;
with Hostparm; use Hostparm;
with Lib;      use Lib;
with Namet;    use Namet;
with Nmake;    use Nmake;
with Nlists;   use Nlists;
with Opt;      use Opt;
with Restrict; use Restrict;
with Rident;   use Rident;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Ch7;  use Sem_Ch7;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Eval; use Sem_Eval;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Stand;    use Stand;
with Stringt;  use Stringt;
with Tbuild;   use Tbuild;
with Ttypes;   use Ttypes;
with Uintp;    use Uintp;
with Uname;    use Uname;
with Validsw;  use Validsw;

package body Exp_Attr is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Compile_Stream_Body_In_Scope
     (N     : Node_Id;
      Decl  : Node_Id;
      Arr   : Entity_Id;
      Check : Boolean);
   --  The body for a stream subprogram may be generated outside of the scope
   --  of the type. If the type is fully private, it may depend on the full
   --  view of other types (e.g. indices) that are currently private as well.
   --  We install the declarations of the package in which the type is declared
   --  before compiling the body in what is its proper environment. The Check
   --  parameter indicates if checks are to be suppressed for the stream body.
   --  We suppress checks for array/record reads, since the rule is that these
   --  are like assignments, out of range values due to uninitialized storage,
   --  or other invalid values do NOT cause a Constraint_Error to be raised.

   procedure Expand_Fpt_Attribute
     (N    : Node_Id;
      Rtp  : Entity_Id;
      Nam  : Name_Id;
      Args : List_Id);
   --  This procedure expands a call to a floating-point attribute function.
   --  N is the attribute reference node, and Args is a list of arguments to
   --  be passed to the function call. Rtp is the root type of the floating
   --  point type involved (used to select the proper generic instantiation
   --  of the package containing the attribute routines). The Nam argument
   --  is the attribute processing routine to be called. This is normally
   --  the same as the attribute name, except in the Unaligned_Valid case.

   procedure Expand_Fpt_Attribute_R (N : Node_Id);
   --  This procedure expands a call to a floating-point attribute function
   --  that takes a single floating-point argument. The function to be called
   --  is always the same as the attribute name.

   procedure Expand_Fpt_Attribute_RI (N : Node_Id);
   --  This procedure expands a call to a floating-point attribute function
   --  that takes one floating-point argument and one integer argument. The
   --  function to be called is always the same as the attribute name.

   procedure Expand_Fpt_Attribute_RR (N : Node_Id);
   --  This procedure expands a call to a floating-point attribute function
   --  that takes two floating-point arguments. The function to be called
   --  is always the same as the attribute name.

   procedure Expand_Pred_Succ (N : Node_Id);
   --  Handles expansion of Pred or Succ attributes for case of non-real
   --  operand with overflow checking required.

   function Get_Index_Subtype (N : Node_Id) return Entity_Id;
   --  Used for Last, Last, and Length, when the prefix is an array type,
   --  Obtains the corresponding index subtype.

   procedure Expand_Access_To_Type (N : Node_Id);
   --  A reference to a type within its own scope is resolved to a reference
   --  to the current instance of the type in its initialization procedure.

   function Find_Stream_Subprogram
     (Typ : Entity_Id;
      Nam : TSS_Name_Type) return Entity_Id;
   --  Returns the stream-oriented subprogram attribute for Typ. For tagged
   --  types, the corresponding primitive operation is looked up, else the
   --  appropriate TSS from the type itself, or from its closest ancestor
   --  defining it, is returned. In both cases, inheritance of representation
   --  aspects is thus taken into account.

   function Get_Stream_Convert_Pragma (T : Entity_Id) return Node_Id;
   --  Given a type, find a corresponding stream convert pragma that applies to
   --  the implementation base type of this type (Typ). If found, return the
   --  pragma node, otherwise return Empty if no pragma is found.

   function Is_Constrained_Packed_Array (Typ : Entity_Id) return Boolean;
   --  Utility for array attributes, returns true on packed constrained
   --  arrays, and on access to same.

   ----------------------------------
   -- Compile_Stream_Body_In_Scope --
   ----------------------------------

   procedure Compile_Stream_Body_In_Scope
     (N     : Node_Id;
      Decl  : Node_Id;
      Arr   : Entity_Id;
      Check : Boolean)
   is
      Installed : Boolean := False;
      Scop      : constant Entity_Id := Scope (Arr);
      Curr      : constant Entity_Id := Current_Scope;

   begin
      if Is_Hidden (Arr)
        and then not In_Open_Scopes (Scop)
        and then Ekind (Scop) = E_Package
      then
         New_Scope (Scop);
         Install_Visible_Declarations (Scop);
         Install_Private_Declarations (Scop);
         Installed := True;

         --  The entities in the package are now visible, but the generated
         --  stream entity must appear in the current scope (usually an
         --  enclosing stream function) so that itypes all have their proper
         --  scopes.

         New_Scope (Curr);
      end if;

      if Check then
         Insert_Action (N, Decl);
      else
         Insert_Action (N, Decl, All_Checks);
      end if;

      if Installed then

         --  Remove extra copy of current scope, and package itself

         Pop_Scope;
         End_Package_Scope (Scop);
      end if;
   end Compile_Stream_Body_In_Scope;

   ---------------------------
   -- Expand_Access_To_Type --
   ---------------------------

   procedure Expand_Access_To_Type (N : Node_Id) is
      Loc    : constant Source_Ptr   := Sloc (N);
      Typ    : constant Entity_Id    := Etype (N);
      Pref   : constant Node_Id      := Prefix (N);
      Par    : Node_Id;
      Formal : Entity_Id;

   begin
      if Is_Entity_Name (Pref)
        and then Is_Type (Entity (Pref))
      then
         --  If the current instance name denotes a task type,
         --  then the access attribute is rewritten to be the
         --  name of the "_task" parameter associated with the
         --  task type's task body procedure. An unchecked
         --  conversion is applied to ensure a type match in
         --  cases of expander-generated calls (e.g., init procs).

         if Is_Task_Type (Entity (Pref)) then
            Formal :=
              First_Entity (Get_Task_Body_Procedure (Entity (Pref)));

            while Present (Formal) loop
               exit when Chars (Formal) = Name_uTask;
               Next_Entity (Formal);
            end loop;

            pragma Assert (Present (Formal));

            Rewrite (N,
              Unchecked_Convert_To (Typ, New_Occurrence_Of (Formal, Loc)));
            Set_Etype (N, Typ);

         --  The expression must appear in a default expression,
         --  (which in the initialization procedure is the rhs of
         --  an assignment), and not in a discriminant constraint.

         else
            Par := Parent (N);

            while Present (Par) loop
               exit when Nkind (Par) = N_Assignment_Statement;

               if Nkind (Par) = N_Component_Declaration then
                  return;
               end if;

               Par := Parent (Par);
            end loop;

            if Present (Par) then
               Rewrite (N,
                 Make_Attribute_Reference (Loc,
                   Prefix => Make_Identifier (Loc, Name_uInit),
                   Attribute_Name  => Attribute_Name (N)));

               Analyze_And_Resolve (N, Typ);
            end if;
         end if;
      end if;
   end Expand_Access_To_Type;

   --------------------------
   -- Expand_Fpt_Attribute --
   --------------------------

   procedure Expand_Fpt_Attribute
     (N    : Node_Id;
      Rtp  : Entity_Id;
      Nam  : Name_Id;
      Args : List_Id)
   is
      Loc : constant Source_Ptr := Sloc (N);
      Typ : constant Entity_Id  := Etype (N);
      Pkg : RE_Id;
      Fnm : Node_Id;

   begin
      --  The function name is the selected component Fat_xxx.yyy where xxx
      --  is the floating-point root type, and yyy is the argument Nam.

      --  Note: it would be more usual to have separate RE entries for each
      --  of the entities in the Fat packages, but first they have identical
      --  names (so we would have to have lots of renaming declarations to
      --  meet the normal RE rule of separate names for all runtime entities),
      --  and second there would be an awful lot of them!

      if Rtp = Standard_Short_Float then
         Pkg := RE_Fat_Short_Float;
      elsif Rtp = Standard_Float then
         Pkg := RE_Fat_Float;
      elsif Rtp = Standard_Long_Float then
         Pkg := RE_Fat_Long_Float;
      else
         Pkg := RE_Fat_Long_Long_Float;
      end if;

      Fnm :=
        Make_Selected_Component (Loc,
          Prefix        => New_Reference_To (RTE (Pkg), Loc),
          Selector_Name => Make_Identifier (Loc, Nam));

      --  The generated call is given the provided set of parameters, and then
      --  wrapped in a conversion which converts the result to the target type
      --  We use the base type as the target because a range check may be
      --  required.

      Rewrite (N,
        Unchecked_Convert_To (Base_Type (Etype (N)),
          Make_Function_Call (Loc,
            Name => Fnm,
            Parameter_Associations => Args)));

      Analyze_And_Resolve (N, Typ);
   end Expand_Fpt_Attribute;

   ----------------------------
   -- Expand_Fpt_Attribute_R --
   ----------------------------

   --  The single argument is converted to its root type to call the
   --  appropriate runtime function, with the actual call being built
   --  by Expand_Fpt_Attribute

   procedure Expand_Fpt_Attribute_R (N : Node_Id) is
      E1  : constant Node_Id    := First (Expressions (N));
      Rtp : constant Entity_Id  := Root_Type (Etype (E1));

   begin
      Expand_Fpt_Attribute
        (N, Rtp, Attribute_Name (N),
         New_List (Unchecked_Convert_To (Rtp, Relocate_Node (E1))));
   end Expand_Fpt_Attribute_R;

   -----------------------------
   -- Expand_Fpt_Attribute_RI --
   -----------------------------

   --  The first argument is converted to its root type and the second
   --  argument is converted to standard long long integer to call the
   --  appropriate runtime function, with the actual call being built
   --  by Expand_Fpt_Attribute

   procedure Expand_Fpt_Attribute_RI (N : Node_Id) is
      E1  : constant Node_Id   := First (Expressions (N));
      Rtp : constant Entity_Id := Root_Type (Etype (E1));
      E2  : constant Node_Id   := Next (E1);

   begin
      Expand_Fpt_Attribute
        (N, Rtp, Attribute_Name (N),
         New_List (
           Unchecked_Convert_To (Rtp, Relocate_Node (E1)),
           Unchecked_Convert_To (Standard_Integer, Relocate_Node (E2))));
   end Expand_Fpt_Attribute_RI;

   -----------------------------
   -- Expand_Fpt_Attribute_RR --
   -----------------------------

   --  The two arguments is converted to their root types to call the
   --  appropriate runtime function, with the actual call being built
   --  by Expand_Fpt_Attribute

   procedure Expand_Fpt_Attribute_RR (N : Node_Id) is
      E1  : constant Node_Id   := First (Expressions (N));
      Rtp : constant Entity_Id := Root_Type (Etype (E1));
      E2  : constant Node_Id   := Next (E1);

   begin
      Expand_Fpt_Attribute
        (N, Rtp, Attribute_Name (N),
         New_List (
           Unchecked_Convert_To (Rtp, Relocate_Node (E1)),
           Unchecked_Convert_To (Rtp, Relocate_Node (E2))));
   end Expand_Fpt_Attribute_RR;

   ----------------------------------
   -- Expand_N_Attribute_Reference --
   ----------------------------------

   procedure Expand_N_Attribute_Reference (N : Node_Id) is
      Loc   : constant Source_Ptr   := Sloc (N);
      Typ   : constant Entity_Id    := Etype (N);
      Btyp  : constant Entity_Id    := Base_Type (Typ);
      Pref  : constant Node_Id      := Prefix (N);
      Exprs : constant List_Id      := Expressions (N);
      Id    : constant Attribute_Id := Get_Attribute_Id (Attribute_Name (N));

      procedure Rewrite_Stream_Proc_Call (Pname : Entity_Id);
      --  Rewrites a stream attribute for Read, Write or Output with the
      --  procedure call. Pname is the entity for the procedure to call.

      ------------------------------
      -- Rewrite_Stream_Proc_Call --
      ------------------------------

      procedure Rewrite_Stream_Proc_Call (Pname : Entity_Id) is
         Item       : constant Node_Id   := Next (First (Exprs));
         Formal     : constant Entity_Id := Next_Formal (First_Formal (Pname));
         Formal_Typ : constant Entity_Id := Etype (Formal);
         Is_Written : constant Boolean   := (Ekind (Formal) /= E_In_Parameter);

      begin
         --  The expansion depends on Item, the second actual, which is
         --  the object being streamed in or out.

         --  If the item is a component of a packed array type, and
         --  a conversion is needed on exit, we introduce a temporary to
         --  hold the value, because otherwise the packed reference will
         --  not be properly expanded.

         if Nkind (Item) = N_Indexed_Component
           and then Is_Packed (Base_Type (Etype (Prefix (Item))))
           and then Base_Type (Etype (Item)) /= Base_Type (Formal_Typ)
           and then Is_Written
         then
            declare
               Temp : constant Entity_Id :=
                        Make_Defining_Identifier
                          (Loc, New_Internal_Name ('V'));
               Decl : Node_Id;
               Assn : Node_Id;

            begin
               Decl :=
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Temp,
                   Object_Definition    =>
                     New_Occurrence_Of (Formal_Typ, Loc));
               Set_Etype (Temp, Formal_Typ);

               Assn :=
                 Make_Assignment_Statement (Loc,
                   Name => New_Copy_Tree (Item),
                   Expression =>
                     Unchecked_Convert_To
                       (Etype (Item), New_Occurrence_Of (Temp, Loc)));

               Rewrite (Item, New_Occurrence_Of (Temp, Loc));
               Insert_Actions (N,
                 New_List (
                   Decl,
                   Make_Procedure_Call_Statement (Loc,
                     Name => New_Occurrence_Of (Pname, Loc),
                     Parameter_Associations => Exprs),
                   Assn));

               Rewrite (N, Make_Null_Statement (Loc));
               return;
            end;
         end if;

         --  For the class-wide dispatching cases, and for cases in which
         --  the base type of the second argument matches the base type of
         --  the corresponding formal parameter (that is to say the stream
         --  operation is not inherited), we are all set, and can use the
         --  argument unchanged.

         --  For all other cases we do an unchecked conversion of the second
         --  parameter to the type of the formal of the procedure we are
         --  calling. This deals with the private type cases, and with going
         --  to the root type as required in elementary type case.

         if not Is_Class_Wide_Type (Entity (Pref))
           and then not Is_Class_Wide_Type (Etype (Item))
           and then Base_Type (Etype (Item)) /= Base_Type (Formal_Typ)
         then
            Rewrite (Item,
              Unchecked_Convert_To (Formal_Typ, Relocate_Node (Item)));

            --  For untagged derived types set Assignment_OK, to prevent
            --  copies from being created when the unchecked conversion
            --  is expanded (which would happen in Remove_Side_Effects
            --  if Expand_N_Unchecked_Conversion were allowed to call
            --  Force_Evaluation). The copy could violate Ada semantics
            --  in cases such as an actual that is an out parameter.
            --  Note that this approach is also used in exp_ch7 for calls
            --  to controlled type operations to prevent problems with
            --  actuals wrapped in unchecked conversions.

            if Is_Untagged_Derivation (Etype (Expression (Item))) then
               Set_Assignment_OK (Item);
            end if;
         end if;

         --  And now rewrite the call

         Rewrite (N,
           Make_Procedure_Call_Statement (Loc,
             Name => New_Occurrence_Of (Pname, Loc),
             Parameter_Associations => Exprs));

         Analyze (N);
      end Rewrite_Stream_Proc_Call;

   --  Start of processing for Expand_N_Attribute_Reference

   begin
      --  Do required validity checking, if enabled. Do not apply check to
      --  output parameters of an Asm instruction, since the value of this
      --  is not set till after the attribute has been elaborated.

      if Validity_Checks_On and then Validity_Check_Operands
        and then Id /= Attribute_Asm_Output
      then
         declare
            Expr : Node_Id;
         begin
            Expr := First (Expressions (N));
            while Present (Expr) loop
               Ensure_Valid (Expr);
               Next (Expr);
            end loop;
         end;
      end if;

      --  Remaining processing depends on specific attribute

      case Id is

      ------------
      -- Access --
      ------------

      when Attribute_Access =>

         if Ekind (Btyp) = E_Access_Protected_Subprogram_Type then

            --  The value of the attribute_reference is a record containing
            --  two fields: an access to the protected object, and an access
            --  to the subprogram itself. The prefix is a selected component.

            declare
               Agg     : Node_Id;
               Sub     : Entity_Id;
               E_T     : constant Entity_Id := Equivalent_Type (Btyp);
               Acc     : constant Entity_Id :=
                           Etype (Next_Component (First_Component (E_T)));
               Obj_Ref : Node_Id;
               Curr    : Entity_Id;

            begin
               --  Within the body of the protected type, the prefix
               --  designates a local operation, and the object is the first
               --  parameter of the corresponding protected body of the
               --  current enclosing operation.

               if Is_Entity_Name (Pref) then
                  pragma Assert (In_Open_Scopes (Scope (Entity (Pref))));
                  Sub :=
                    New_Occurrence_Of
                      (Protected_Body_Subprogram (Entity (Pref)), Loc);
                  Curr := Current_Scope;

                  while Scope (Curr) /= Scope (Entity (Pref)) loop
                     Curr := Scope (Curr);
                  end loop;

                  Obj_Ref :=
                    Make_Attribute_Reference (Loc,
                      Prefix =>
                         New_Occurrence_Of
                           (First_Formal
                              (Protected_Body_Subprogram (Curr)), Loc),
                      Attribute_Name => Name_Address);

               --  Case where the prefix is not an entity name. Find the
               --  version of the protected operation to be called from
               --  outside the protected object.

               else
                  Sub :=
                    New_Occurrence_Of
                      (External_Subprogram
                        (Entity (Selector_Name (Pref))), Loc);

                  Obj_Ref :=
                    Make_Attribute_Reference (Loc,
                      Prefix => Relocate_Node (Prefix (Pref)),
                        Attribute_Name => Name_Address);
               end if;

               Agg :=
                 Make_Aggregate (Loc,
                   Expressions =>
                     New_List (
                       Obj_Ref,
                       Unchecked_Convert_To (Acc,
                         Make_Attribute_Reference (Loc,
                           Prefix => Sub,
                           Attribute_Name => Name_Address))));

               Rewrite (N, Agg);

               Analyze_And_Resolve (N, E_T);

               --  For subsequent analysis,  the node must retain its type.
               --  The backend will replace it with the equivalent type where
               --  needed.

               Set_Etype (N, Typ);
            end;

         elsif Ekind (Btyp) = E_General_Access_Type then
            declare
               Ref_Object : constant Node_Id := Get_Referenced_Object (Pref);
               Parm_Ent   : Entity_Id;
               Conversion : Node_Id;

            begin
               --  If the prefix of an Access attribute is a dereference of an
               --  access parameter (or a renaming of such a dereference) and
               --  the context is a general access type (but not an anonymous
               --  access type), then rewrite the attribute as a conversion of
               --  the access parameter to the context access type.  This will
               --  result in an accessibility check being performed, if needed.

               --    (X.all'Access => Acc_Type (X))

               if Nkind (Ref_Object) = N_Explicit_Dereference
                 and then Is_Entity_Name (Prefix (Ref_Object))
               then
                  Parm_Ent := Entity (Prefix (Ref_Object));

                  if Ekind (Parm_Ent) in Formal_Kind
                    and then Ekind (Etype (Parm_Ent)) = E_Anonymous_Access_Type
                    and then Present (Extra_Accessibility (Parm_Ent))
                  then
                     Conversion :=
                        Convert_To (Typ, New_Copy_Tree (Prefix (Ref_Object)));

                     Rewrite (N, Conversion);
                     Analyze_And_Resolve (N, Typ);
                  end if;

               --  Ada 2005 (AI-251): If the designated type is an interface,
               --  then rewrite the referenced object as a conversion to force
               --  the displacement of the pointer to the secondary dispatch
               --  table.

               elsif Is_Interface (Directly_Designated_Type (Btyp)) then
                  Conversion := Convert_To (Typ, New_Copy_Tree (Ref_Object));
                  Rewrite (N, Conversion);
                  Analyze_And_Resolve (N, Typ);
               end if;
            end;

         --  If the prefix is a type name, this is a reference to the current
         --  instance of the type, within its initialization procedure.

         else
            Expand_Access_To_Type (N);
         end if;

      --------------
      -- Adjacent --
      --------------

      --  Transforms 'Adjacent into a call to the floating-point attribute
      --  function Adjacent in Fat_xxx (where xxx is the root type)

      when Attribute_Adjacent =>
         Expand_Fpt_Attribute_RR (N);

      -------------
      -- Address --
      -------------

      when Attribute_Address => Address : declare
         Task_Proc : Entity_Id;

      begin
         --  If the prefix is a task or a task type, the useful address
         --  is that of the procedure for the task body, i.e. the actual
         --  program unit. We replace the original entity with that of
         --  the procedure.

         if Is_Entity_Name (Pref)
           and then Is_Task_Type (Entity (Pref))
         then
            Task_Proc := Next_Entity (Root_Type (Etype (Pref)));

            while Present (Task_Proc) loop
               exit when Ekind (Task_Proc) = E_Procedure
                 and then Etype (First_Formal (Task_Proc)) =
                                  Corresponding_Record_Type (Etype (Pref));
               Next_Entity (Task_Proc);
            end loop;

            if Present (Task_Proc) then
               Set_Entity (Pref, Task_Proc);
               Set_Etype  (Pref, Etype (Task_Proc));
            end if;

         --  Similarly, the address of a protected operation is the address
         --  of the corresponding protected body, regardless of the protected
         --  object from which it is selected.

         elsif Nkind (Pref) = N_Selected_Component
           and then Is_Subprogram (Entity (Selector_Name (Pref)))
           and then Is_Protected_Type (Scope (Entity (Selector_Name (Pref))))
         then
            Rewrite (Pref,
              New_Occurrence_Of (
                External_Subprogram (Entity (Selector_Name (Pref))), Loc));

         elsif Nkind (Pref) = N_Explicit_Dereference
           and then Ekind (Etype (Pref)) = E_Subprogram_Type
           and then Convention (Etype (Pref)) = Convention_Protected
         then
            --  The prefix is be a dereference of an access_to_protected_
            --  subprogram. The desired address is the second component of
            --  the record that represents the access.

            declare
               Addr : constant Entity_Id := Etype (N);
               Ptr  : constant Node_Id   := Prefix (Pref);
               T    : constant Entity_Id :=
                        Equivalent_Type (Base_Type (Etype (Ptr)));

            begin
               Rewrite (N,
                 Unchecked_Convert_To (Addr,
                   Make_Selected_Component (Loc,
                     Prefix => Unchecked_Convert_To (T, Ptr),
                     Selector_Name => New_Occurrence_Of (
                       Next_Entity (First_Entity (T)), Loc))));

               Analyze_And_Resolve (N, Addr);
            end;
         end if;

         --  Deal with packed array reference, other cases are handled by gigi

         if Involves_Packed_Array_Reference (Pref) then
            Expand_Packed_Address_Reference (N);
         end if;
      end Address;

      ---------------
      -- Alignment --
      ---------------

      when Attribute_Alignment => Alignment : declare
         Ptyp     : constant Entity_Id := Etype (Pref);
         New_Node : Node_Id;

      begin
         --  For class-wide types, X'Class'Alignment is transformed into a
         --  direct reference to the Alignment of the class type, so that the
         --  back end does not have to deal with the X'Class'Alignment
         --  reference.

         if Is_Entity_Name (Pref)
           and then Is_Class_Wide_Type (Entity (Pref))
         then
            Rewrite (Prefix (N), New_Occurrence_Of (Entity (Pref), Loc));
            return;

         --  For x'Alignment applied to an object of a class wide type,
         --  transform X'Alignment into a call to the predefined primitive
         --  operation _Alignment applied to X.

         elsif Is_Class_Wide_Type (Ptyp) then
            New_Node :=
              Make_Function_Call (Loc,
                Name => New_Reference_To
                  (Find_Prim_Op (Ptyp, Name_uAlignment), Loc),
                Parameter_Associations => New_List (Pref));

            if Typ /= Standard_Integer then

               --  The context is a specific integer type with which the
               --  original attribute was compatible. The function has a
               --  specific type as well, so to preserve the compatibility
               --  we must convert explicitly.

               New_Node := Convert_To (Typ, New_Node);
            end if;

            Rewrite (N, New_Node);
            Analyze_And_Resolve (N, Typ);
            return;

         --  For all other cases, we just have to deal with the case of
         --  the fact that the result can be universal.

         else
            Apply_Universal_Integer_Attribute_Checks (N);
         end if;
      end Alignment;

      ---------------
      -- AST_Entry --
      ---------------

      when Attribute_AST_Entry => AST_Entry : declare
         Ttyp : Entity_Id;
         T_Id : Node_Id;
         Eent : Entity_Id;

         Entry_Ref : Node_Id;
         --  The reference to the entry or entry family

         Index : Node_Id;
         --  The index expression for an entry family reference, or
         --  the Empty if Entry_Ref references a simple entry.

      begin
         if Nkind (Pref) = N_Indexed_Component then
            Entry_Ref := Prefix (Pref);
            Index := First (Expressions (Pref));
         else
            Entry_Ref := Pref;
            Index := Empty;
         end if;

         --  Get expression for Task_Id and the entry entity

         if Nkind (Entry_Ref) = N_Selected_Component then
            T_Id :=
              Make_Attribute_Reference (Loc,
                Attribute_Name => Name_Identity,
                Prefix         => Prefix (Entry_Ref));

            Ttyp := Etype (Prefix (Entry_Ref));
            Eent := Entity (Selector_Name (Entry_Ref));

         else
            T_Id :=
              Make_Function_Call (Loc,
                Name => New_Occurrence_Of (RTE (RE_Current_Task), Loc));

            Eent  := Entity (Entry_Ref);

            --  We have to find the enclosing task to get the task type
            --  There must be one, since we already validated this earlier

            Ttyp := Current_Scope;
            while not Is_Task_Type (Ttyp) loop
               Ttyp := Scope (Ttyp);
            end loop;
         end if;

         --  Now rewrite the attribute with a call to Create_AST_Handler

         Rewrite (N,
           Make_Function_Call (Loc,
             Name => New_Occurrence_Of (RTE (RE_Create_AST_Handler), Loc),
             Parameter_Associations => New_List (
               T_Id,
               Entry_Index_Expression (Loc, Eent, Index, Ttyp))));

         Analyze_And_Resolve (N, RTE (RE_AST_Handler));
      end AST_Entry;

      ------------------
      -- Bit_Position --
      ------------------

      --  We compute this if a component clause was present, otherwise
      --  we leave the computation up to Gigi, since we don't know what
      --  layout will be chosen.

      --  Note that the attribute can apply to a naked record component
      --  in generated code (i.e. the prefix is an identifier that
      --  references the component or discriminant entity).

      when Attribute_Bit_Position => Bit_Position :
      declare
         CE : Entity_Id;

      begin
         if Nkind (Pref) = N_Identifier then
            CE := Entity (Pref);
         else
            CE := Entity (Selector_Name (Pref));
         end if;

         if Known_Static_Component_Bit_Offset (CE) then
            Rewrite (N,
              Make_Integer_Literal (Loc,
                Intval => Component_Bit_Offset (CE)));
            Analyze_And_Resolve (N, Typ);

         else
            Apply_Universal_Integer_Attribute_Checks (N);
         end if;
      end Bit_Position;

      ------------------
      -- Body_Version --
      ------------------

      --  A reference to P'Body_Version or P'Version is expanded to

      --     Vnn : Unsigned;
      --     pragma Import (C, Vnn, "uuuuT";
      --     ...
      --     Get_Version_String (Vnn)

      --  where uuuu is the unit name (dots replaced by double underscore)
      --  and T is B for the cases of Body_Version, or Version applied to a
      --  subprogram acting as its own spec, and S for Version applied to a
      --  subprogram spec or package. This sequence of code references the
      --  the unsigned constant created in the main program by the binder.

      --  A special exception occurs for Standard, where the string
      --  returned is a copy of the library  string in gnatvsn.ads.

      when Attribute_Body_Version | Attribute_Version => Version : declare
         E    : constant Entity_Id :=
                  Make_Defining_Identifier (Loc, New_Internal_Name ('V'));
         Pent : Entity_Id := Entity (Pref);
         S    : String_Id;

      begin
         --  If not library unit, get to containing library unit

         while Pent /= Standard_Standard
           and then Scope (Pent) /= Standard_Standard
         loop
            Pent := Scope (Pent);
         end loop;

         --  Special case Standard

         if Pent = Standard_Standard
           or else Pent = Standard_ASCII
         then
            Rewrite (N,
              Make_String_Literal (Loc,
                Strval => Verbose_Library_Version));

         --  All other cases

         else
            --  Build required string constant

            Get_Name_String (Get_Unit_Name (Pent));

            Start_String;
            for J in 1 .. Name_Len - 2 loop
               if Name_Buffer (J) = '.' then
                  Store_String_Chars ("__");
               else
                  Store_String_Char (Get_Char_Code (Name_Buffer (J)));
               end if;
            end loop;

            --  Case of subprogram acting as its own spec, always use body

            if Nkind (Declaration_Node (Pent)) in N_Subprogram_Specification
              and then Nkind (Parent (Declaration_Node (Pent))) =
                                                          N_Subprogram_Body
              and then Acts_As_Spec (Parent (Declaration_Node (Pent)))
            then
               Store_String_Chars ("B");

            --  Case of no body present, always use spec

            elsif not Unit_Requires_Body (Pent) then
               Store_String_Chars ("S");

            --  Otherwise use B for Body_Version, S for spec

            elsif Id = Attribute_Body_Version then
               Store_String_Chars ("B");
            else
               Store_String_Chars ("S");
            end if;

            S := End_String;
            Lib.Version_Referenced (S);

            --  Insert the object declaration

            Insert_Actions (N, New_List (
              Make_Object_Declaration (Loc,
                Defining_Identifier => E,
                Object_Definition   =>
                  New_Occurrence_Of (RTE (RE_Unsigned), Loc))));

            --  Set entity as imported with correct external name

            Set_Is_Imported (E);
            Set_Interface_Name (E, Make_String_Literal (Loc, S));

            --  And now rewrite original reference

            Rewrite (N,
              Make_Function_Call (Loc,
                Name => New_Reference_To (RTE (RE_Get_Version_String), Loc),
                Parameter_Associations => New_List (
                  New_Occurrence_Of (E, Loc))));
         end if;

         Analyze_And_Resolve (N, RTE (RE_Version_String));
      end Version;

      -------------
      -- Ceiling --
      -------------

      --  Transforms 'Ceiling into a call to the floating-point attribute
      --  function Ceiling in Fat_xxx (where xxx is the root type)

      when Attribute_Ceiling =>
         Expand_Fpt_Attribute_R (N);

      --------------
      -- Callable --
      --------------

      --  Transforms 'Callable attribute into a call to the Callable function

      when Attribute_Callable => Callable :
      begin
         Rewrite (N,
           Build_Call_With_Task (Pref, RTE (RE_Callable)));
         Analyze_And_Resolve (N, Standard_Boolean);
      end Callable;

      ------------
      -- Caller --
      ------------

      --  Transforms 'Caller attribute into a call to either the
      --  Task_Entry_Caller or the Protected_Entry_Caller function.

      when Attribute_Caller => Caller : declare
         Id_Kind    : constant Entity_Id := RTE (RO_AT_Task_Id);
         Ent        : constant Entity_Id := Entity (Pref);
         Conctype   : constant Entity_Id := Scope (Ent);
         Nest_Depth : Integer := 0;
         Name       : Node_Id;
         S          : Entity_Id;

      begin
         --  Protected case

         if Is_Protected_Type (Conctype) then
            if Abort_Allowed
              or else Restriction_Active (No_Entry_Queue) = False
              or else Number_Entries (Conctype) > 1
            then
               Name :=
                 New_Reference_To
                   (RTE (RE_Protected_Entry_Caller), Loc);
            else
               Name :=
                 New_Reference_To
                   (RTE (RE_Protected_Single_Entry_Caller), Loc);
            end if;

            Rewrite (N,
              Unchecked_Convert_To (Id_Kind,
                Make_Function_Call (Loc,
                  Name => Name,
                  Parameter_Associations => New_List
                    (New_Reference_To (
                      Object_Ref
                        (Corresponding_Body (Parent (Conctype))), Loc)))));

         --  Task case

         else
            --  Determine the nesting depth of the E'Caller attribute, that
            --  is, how many accept statements are nested within the accept
            --  statement for E at the point of E'Caller. The runtime uses
            --  this depth to find the specified entry call.

            for J in reverse 0 .. Scope_Stack.Last loop
               S := Scope_Stack.Table (J).Entity;

               --  We should not reach the scope of the entry, as it should
               --  already have been checked in Sem_Attr that this attribute
               --  reference is within a matching accept statement.

               pragma Assert (S /= Conctype);

               if S = Ent then
                  exit;

               elsif Is_Entry (S) then
                  Nest_Depth := Nest_Depth + 1;
               end if;
            end loop;

            Rewrite (N,
              Unchecked_Convert_To (Id_Kind,
                Make_Function_Call (Loc,
                  Name => New_Reference_To (
                    RTE (RE_Task_Entry_Caller), Loc),
                  Parameter_Associations => New_List (
                    Make_Integer_Literal (Loc,
                      Intval => Int (Nest_Depth))))));
         end if;

         Analyze_And_Resolve (N, Id_Kind);
      end Caller;

      -------------
      -- Compose --
      -------------

      --  Transforms 'Compose into a call to the floating-point attribute
      --  function Compose in Fat_xxx (where xxx is the root type)

      --  Note: we strictly should have special code here to deal with the
      --  case of absurdly negative arguments (less than Integer'First)
      --  which will return a (signed) zero value, but it hardly seems
      --  worth the effort. Absurdly large positive arguments will raise
      --  constraint error which is fine.

      when Attribute_Compose =>
         Expand_Fpt_Attribute_RI (N);

      -----------------
      -- Constrained --
      -----------------

      when Attribute_Constrained => Constrained : declare
         Formal_Ent : constant Entity_Id := Param_Entity (Pref);
         Typ        : constant Entity_Id := Etype (Pref);

      begin
         --  Reference to a parameter where the value is passed as an extra
         --  actual, corresponding to the extra formal referenced by the
         --  Extra_Constrained field of the corresponding formal. If this
         --  is an entry in-parameter, it is replaced by a constant renaming
         --  for which Extra_Constrained is never created.

         if Present (Formal_Ent)
           and then Ekind (Formal_Ent) /= E_Constant
           and then Present (Extra_Constrained (Formal_Ent))
         then
            Rewrite (N,
              New_Occurrence_Of
                (Extra_Constrained (Formal_Ent), Sloc (N)));

         --  For variables with a Extra_Constrained field, we use the
         --  corresponding entity.

         elsif Nkind (Pref) = N_Identifier
           and then Ekind (Entity (Pref)) = E_Variable
           and then Present (Extra_Constrained (Entity (Pref)))
         then
            Rewrite (N,
              New_Occurrence_Of
                (Extra_Constrained (Entity (Pref)), Sloc (N)));

         --  For all other entity names, we can tell at compile time

         elsif Is_Entity_Name (Pref) then
            declare
               Ent : constant Entity_Id   := Entity (Pref);
               Res : Boolean;

            begin
               --  (RM J.4) obsolescent cases

               if Is_Type (Ent) then

                  --  Private type

                  if Is_Private_Type (Ent) then
                     Res := not Has_Discriminants (Ent)
                              or else Is_Constrained (Ent);

                  --  It not a private type, must be a generic actual type
                  --  that corresponded to a private type. We know that this
                  --  correspondence holds, since otherwise the reference
                  --  within the generic template would have been illegal.

                  else
                     if Is_Composite_Type (Underlying_Type (Ent)) then
                        Res := Is_Constrained (Ent);
                     else
                        Res := True;
                     end if;
                  end if;

               --  If the prefix is not a variable or is aliased, then
               --  definitely true; if it's a formal parameter without
               --  an associated extra formal, then treat it as constrained.

               elsif not Is_Variable (Pref)
                 or else Present (Formal_Ent)
                 or else Is_Aliased_View (Pref)
               then
                  Res := True;

               --  Variable case, just look at type to see if it is
               --  constrained. Note that the one case where this is
               --  not accurate (the procedure formal case), has been
               --  handled above.

               else
                  Res := Is_Constrained (Etype (Ent));
               end if;

               Rewrite (N,
                 New_Reference_To (Boolean_Literals (Res), Loc));
            end;

         --  Prefix is not an entity name. These are also cases where
         --  we can always tell at compile time by looking at the form
         --  and type of the prefix. If an explicit dereference of an
         --  object with constrained partial view, this is unconstrained
         --  (Ada 2005 AI-363).

         else
            Rewrite (N,
              New_Reference_To (
                Boolean_Literals (
                  not Is_Variable (Pref)
                    or else
                     (Nkind (Pref) = N_Explicit_Dereference
                        and then
                          not Has_Constrained_Partial_View (Base_Type (Typ)))
                    or else Is_Constrained (Typ)),
                Loc));
         end if;

         Analyze_And_Resolve (N, Standard_Boolean);
      end Constrained;

      ---------------
      -- Copy_Sign --
      ---------------

      --  Transforms 'Copy_Sign into a call to the floating-point attribute
      --  function Copy_Sign in Fat_xxx (where xxx is the root type)

      when Attribute_Copy_Sign =>
         Expand_Fpt_Attribute_RR (N);

      -----------
      -- Count --
      -----------

      --  Transforms 'Count attribute into a call to the Count function

      when Attribute_Count => Count :
      declare
         Entnam  : Node_Id;
         Index   : Node_Id;
         Name    : Node_Id;
         Call    : Node_Id;
         Conctyp : Entity_Id;

      begin
         --  If the prefix is a member of an entry family, retrieve both
         --  entry name and index. For a simple entry there is no index.

         if Nkind (Pref) = N_Indexed_Component then
            Entnam := Prefix (Pref);
            Index := First (Expressions (Pref));
         else
            Entnam := Pref;
            Index := Empty;
         end if;

         --  Find the concurrent type in which this attribute is referenced
         --  (there had better be one).

         Conctyp := Current_Scope;
         while not Is_Concurrent_Type (Conctyp) loop
            Conctyp := Scope (Conctyp);
         end loop;

         --  Protected case

         if Is_Protected_Type (Conctyp) then

            if Abort_Allowed
              or else Restriction_Active (No_Entry_Queue) = False
              or else Number_Entries (Conctyp) > 1
            then
               Name := New_Reference_To (RTE (RE_Protected_Count), Loc);

               Call :=
                 Make_Function_Call (Loc,
                   Name => Name,
                   Parameter_Associations => New_List (
                     New_Reference_To (
                       Object_Ref (
                         Corresponding_Body (Parent (Conctyp))), Loc),
                     Entry_Index_Expression (
                       Loc, Entity (Entnam), Index, Scope (Entity (Entnam)))));
            else
               Name := New_Reference_To (RTE (RE_Protected_Count_Entry), Loc);

               Call := Make_Function_Call (Loc,
                   Name => Name,
                   Parameter_Associations => New_List (
                     New_Reference_To (
                       Object_Ref (
                         Corresponding_Body (Parent (Conctyp))), Loc)));
            end if;

         --  Task case

         else
            Call :=
              Make_Function_Call (Loc,
                Name => New_Reference_To (RTE (RE_Task_Count), Loc),
                Parameter_Associations => New_List (
                  Entry_Index_Expression
                    (Loc, Entity (Entnam), Index, Scope (Entity (Entnam)))));
         end if;

         --  The call returns type Natural but the context is universal integer
         --  so any integer type is allowed. The attribute was already resolved
         --  so its Etype is the required result type. If the base type of the
         --  context type is other than Standard.Integer we put in a conversion
         --  to the required type. This can be a normal typed conversion since
         --  both input and output types of the conversion are integer types

         if Base_Type (Typ) /= Base_Type (Standard_Integer) then
            Rewrite (N, Convert_To (Typ, Call));
         else
            Rewrite (N, Call);
         end if;

         Analyze_And_Resolve (N, Typ);
      end Count;

      ---------------
      -- Elab_Body --
      ---------------

      --  This processing is shared by Elab_Spec

      --  What we do is to insert the following declarations

      --     procedure tnn;
      --     pragma Import (C, enn, "name___elabb/s");

      --  and then the Elab_Body/Spec attribute is replaced by a reference
      --  to this defining identifier.

      when Attribute_Elab_Body |
           Attribute_Elab_Spec =>

         Elab_Body : declare
            Ent  : constant Entity_Id :=
                     Make_Defining_Identifier (Loc,
                       New_Internal_Name ('E'));
            Str  : String_Id;
            Lang : Node_Id;

            procedure Make_Elab_String (Nod : Node_Id);
            --  Given Nod, an identifier, or a selected component, put the
            --  image into the current string literal, with double underline
            --  between components.

            procedure Make_Elab_String (Nod : Node_Id) is
            begin
               if Nkind (Nod) = N_Selected_Component then
                  Make_Elab_String (Prefix (Nod));
                  if Java_VM then
                     Store_String_Char ('$');
                  else
                     Store_String_Char ('_');
                     Store_String_Char ('_');
                  end if;

                  Get_Name_String (Chars (Selector_Name (Nod)));

               else
                  pragma Assert (Nkind (Nod) = N_Identifier);
                  Get_Name_String (Chars (Nod));
               end if;

               Store_String_Chars (Name_Buffer (1 .. Name_Len));
            end Make_Elab_String;

         --  Start of processing for Elab_Body/Elab_Spec

         begin
            --  First we need to prepare the string literal for the name of
            --  the elaboration routine to be referenced.

            Start_String;
            Make_Elab_String (Pref);

            if Java_VM then
               Store_String_Chars ("._elab");
               Lang := Make_Identifier (Loc, Name_Ada);
            else
               Store_String_Chars ("___elab");
               Lang := Make_Identifier (Loc, Name_C);
            end if;

            if Id = Attribute_Elab_Body then
               Store_String_Char ('b');
            else
               Store_String_Char ('s');
            end if;

            Str := End_String;

            Insert_Actions (N, New_List (
              Make_Subprogram_Declaration (Loc,
                Specification =>
                  Make_Procedure_Specification (Loc,
                    Defining_Unit_Name => Ent)),

              Make_Pragma (Loc,
                Chars => Name_Import,
                Pragma_Argument_Associations => New_List (
                  Make_Pragma_Argument_Association (Loc,
                    Expression => Lang),

                  Make_Pragma_Argument_Association (Loc,
                    Expression =>
                      Make_Identifier (Loc, Chars (Ent))),

                  Make_Pragma_Argument_Association (Loc,
                    Expression =>
                      Make_String_Literal (Loc, Str))))));

            Set_Entity (N, Ent);
            Rewrite (N, New_Occurrence_Of (Ent, Loc));
         end Elab_Body;

      ----------------
      -- Elaborated --
      ----------------

      --  Elaborated is always True for preelaborated units, predefined
      --  units, pure units and units which have Elaborate_Body pragmas.
      --  These units have no elaboration entity.

      --  Note: The Elaborated attribute is never passed through to Gigi

      when Attribute_Elaborated => Elaborated : declare
         Ent : constant Entity_Id := Entity (Pref);

      begin
         if Present (Elaboration_Entity (Ent)) then
            Rewrite (N,
              New_Occurrence_Of (Elaboration_Entity (Ent), Loc));
         else
            Rewrite (N, New_Occurrence_Of (Standard_True, Loc));
         end if;
      end Elaborated;

      --------------
      -- Enum_Rep --
      --------------

      when Attribute_Enum_Rep => Enum_Rep :
      begin
         --  X'Enum_Rep (Y) expands to

         --    target-type (Y)

         --  This is simply a direct conversion from the enumeration type
         --  to the target integer type, which is treated by Gigi as a normal
         --  integer conversion, treating the enumeration type as an integer,
         --  which is exactly what we want! We set Conversion_OK to make sure
         --  that the analyzer does not complain about what otherwise might
         --  be an illegal conversion.

         if Is_Non_Empty_List (Exprs) then
            Rewrite (N,
              OK_Convert_To (Typ, Relocate_Node (First (Exprs))));

         --  X'Enum_Rep where X is an enumeration literal is replaced by
         --  the literal value.

         elsif Ekind (Entity (Pref)) = E_Enumeration_Literal then
            Rewrite (N,
              Make_Integer_Literal (Loc, Enumeration_Rep (Entity (Pref))));

         --  If this is a renaming of a literal, recover the representation
         --  of the original.

         elsif Ekind (Entity (Pref)) = E_Constant
           and then Present (Renamed_Object (Entity (Pref)))
           and then
             Ekind (Entity (Renamed_Object (Entity (Pref))))
               = E_Enumeration_Literal
         then
            Rewrite (N,
              Make_Integer_Literal (Loc,
                Enumeration_Rep (Entity (Renamed_Object (Entity (Pref))))));

         --  X'Enum_Rep where X is an object does a direct unchecked conversion
         --  of the object value, as described for the type case above.

         else
            Rewrite (N,
              OK_Convert_To (Typ, Relocate_Node (Pref)));
         end if;

         Set_Etype (N, Typ);
         Analyze_And_Resolve (N, Typ);

      end Enum_Rep;

      --------------
      -- Exponent --
      --------------

      --  Transforms 'Exponent into a call to the floating-point attribute
      --  function Exponent in Fat_xxx (where xxx is the root type)

      when Attribute_Exponent =>
         Expand_Fpt_Attribute_R (N);

      ------------------
      -- External_Tag --
      ------------------

      --  transforme X'External_Tag into Ada.Tags.External_Tag (X'tag)

      when Attribute_External_Tag => External_Tag :
      begin
         Rewrite (N,
           Make_Function_Call (Loc,
             Name => New_Reference_To (RTE (RE_External_Tag), Loc),
             Parameter_Associations => New_List (
               Make_Attribute_Reference (Loc,
                 Attribute_Name => Name_Tag,
                 Prefix => Prefix (N)))));

         Analyze_And_Resolve (N, Standard_String);
      end External_Tag;

      -----------
      -- First --
      -----------

      when Attribute_First => declare
         Ptyp : constant Entity_Id := Etype (Pref);

      begin
         --  If the prefix type is a constrained packed array type which
         --  already has a Packed_Array_Type representation defined, then
         --  replace this attribute with a direct reference to 'First of the
         --  appropriate index subtype (since otherwise Gigi will try to give
         --  us the value of 'First for this implementation type).

         if Is_Constrained_Packed_Array (Ptyp) then
            Rewrite (N,
              Make_Attribute_Reference (Loc,
                Attribute_Name => Name_First,
                Prefix => New_Reference_To (Get_Index_Subtype (N), Loc)));
            Analyze_And_Resolve (N, Typ);

         elsif Is_Access_Type (Ptyp) then
            Apply_Access_Check (N);
         end if;
      end;

      ---------------
      -- First_Bit --
      ---------------

      --  We compute this if a component clause was present, otherwise
      --  we leave the computation up to Gigi, since we don't know what
      --  layout will be chosen.

      when Attribute_First_Bit => First_Bit :
      declare
         CE : constant Entity_Id := Entity (Selector_Name (Pref));

      begin
         if Known_Static_Component_Bit_Offset (CE) then
            Rewrite (N,
              Make_Integer_Literal (Loc,
                Component_Bit_Offset (CE) mod System_Storage_Unit));

            Analyze_And_Resolve (N, Typ);

         else
            Apply_Universal_Integer_Attribute_Checks (N);
         end if;
      end First_Bit;

      -----------------
      -- Fixed_Value --
      -----------------

      --  We transform:

      --     fixtype'Fixed_Value (integer-value)

      --  into

      --     fixtype(integer-value)

      --  we do all the required analysis of the conversion here, because
      --  we do not want this to go through the fixed-point conversion
      --  circuits. Note that gigi always treats fixed-point as equivalent
      --  to the corresponding integer type anyway.

      when Attribute_Fixed_Value => Fixed_Value :
      begin
         Rewrite (N,
           Make_Type_Conversion (Loc,
             Subtype_Mark => New_Occurrence_Of (Entity (Pref), Loc),
             Expression   => Relocate_Node (First (Exprs))));
         Set_Etype (N, Entity (Pref));
         Set_Analyzed (N);

      --  Note: it might appear that a properly analyzed unchecked conversion
      --  would be just fine here, but that's not the case, since the full
      --  range checks performed by the following call are critical!

         Apply_Type_Conversion_Checks (N);
      end Fixed_Value;

      -----------
      -- Floor --
      -----------

      --  Transforms 'Floor into a call to the floating-point attribute
      --  function Floor in Fat_xxx (where xxx is the root type)

      when Attribute_Floor =>
         Expand_Fpt_Attribute_R (N);

      ----------
      -- Fore --
      ----------

      --  For the fixed-point type Typ:

      --    Typ'Fore

      --  expands into

      --    Result_Type (System.Fore (Long_Long_Float (Type'First)),
      --                              Long_Long_Float (Type'Last))

      --  Note that we know that the type is a non-static subtype, or Fore
      --  would have itself been computed dynamically in Eval_Attribute.

      when Attribute_Fore => Fore :
      declare
         Ptyp : constant Entity_Id := Etype (Pref);

      begin
         Rewrite (N,
           Convert_To (Typ,
             Make_Function_Call (Loc,
               Name => New_Reference_To (RTE (RE_Fore), Loc),

               Parameter_Associations => New_List (
                 Convert_To (Standard_Long_Long_Float,
                   Make_Attribute_Reference (Loc,
                     Prefix => New_Reference_To (Ptyp, Loc),
                     Attribute_Name => Name_First)),

                 Convert_To (Standard_Long_Long_Float,
                   Make_Attribute_Reference (Loc,
                     Prefix => New_Reference_To (Ptyp, Loc),
                     Attribute_Name => Name_Last))))));

         Analyze_And_Resolve (N, Typ);
      end Fore;

      --------------
      -- Fraction --
      --------------

      --  Transforms 'Fraction into a call to the floating-point attribute
      --  function Fraction in Fat_xxx (where xxx is the root type)

      when Attribute_Fraction =>
         Expand_Fpt_Attribute_R (N);

      --------------
      -- Identity --
      --------------

      --  For an exception returns a reference to the exception data:
      --      Exception_Id!(Prefix'Reference)

      --  For a task it returns a reference to the _task_id component of
      --  corresponding record:

      --    taskV!(Prefix)._Task_Id, converted to the type Task_Id defined

      --  in Ada.Task_Identification

      when Attribute_Identity => Identity : declare
         Id_Kind : Entity_Id;

      begin
         if Etype (Pref) = Standard_Exception_Type then
            Id_Kind := RTE (RE_Exception_Id);

            if Present (Renamed_Object (Entity (Pref))) then
               Set_Entity (Pref, Renamed_Object (Entity (Pref)));
            end if;

            Rewrite (N,
              Unchecked_Convert_To (Id_Kind, Make_Reference (Loc, Pref)));
         else
            Id_Kind := RTE (RO_AT_Task_Id);

            Rewrite (N,
              Unchecked_Convert_To (Id_Kind, Concurrent_Ref (Pref)));
         end if;

         Analyze_And_Resolve (N, Id_Kind);
      end Identity;

      -----------
      -- Image --
      -----------

      --  Image attribute is handled in separate unit Exp_Imgv

      when Attribute_Image =>
         Exp_Imgv.Expand_Image_Attribute (N);

      ---------
      -- Img --
      ---------

      --  X'Img is expanded to typ'Image (X), where typ is the type of X

      when Attribute_Img => Img :
      begin
         Rewrite (N,
           Make_Attribute_Reference (Loc,
             Prefix => New_Reference_To (Etype (Pref), Loc),
             Attribute_Name => Name_Image,
             Expressions => New_List (Relocate_Node (Pref))));

         Analyze_And_Resolve (N, Standard_String);
      end Img;

      -----------
      -- Input --
      -----------

      when Attribute_Input => Input : declare
         P_Type : constant Entity_Id := Entity (Pref);
         B_Type : constant Entity_Id := Base_Type (P_Type);
         U_Type : constant Entity_Id := Underlying_Type (P_Type);
         Strm   : constant Node_Id   := First (Exprs);
         Fname  : Entity_Id;
         Decl   : Node_Id;
         Call   : Node_Id;
         Prag   : Node_Id;
         Arg2   : Node_Id;
         Rfunc  : Node_Id;

         Cntrl  : Node_Id := Empty;
         --  Value for controlling argument in call. Always Empty except in
         --  the dispatching (class-wide type) case, where it is a reference
         --  to the dummy object initialized to the right internal tag.

         procedure Freeze_Stream_Subprogram (F : Entity_Id);
         --  The expansion of the attribute reference may generate a call to
         --  a user-defined stream subprogram that is frozen by the call. This
         --  can lead to access-before-elaboration problem if the reference
         --  appears in an object declaration and the subprogram body has not
         --  been seen. The freezing of the subprogram requires special code
         --  because it appears in an expanded context where expressions do
         --  not freeze their constituents.

         ------------------------------
         -- Freeze_Stream_Subprogram --
         ------------------------------

         procedure Freeze_Stream_Subprogram (F : Entity_Id) is
            Decl : constant Node_Id := Unit_Declaration_Node (F);
            Bod  : Node_Id;

         begin
            --  If this is user-defined subprogram, the corresponding
            --  stream function appears as a renaming-as-body, and the
            --  user subprogram must be retrieved by tree traversal.

            if Present (Decl)
              and then Nkind (Decl) = N_Subprogram_Declaration
              and then Present (Corresponding_Body (Decl))
            then
               Bod := Corresponding_Body (Decl);

               if Nkind (Unit_Declaration_Node (Bod)) =
                 N_Subprogram_Renaming_Declaration
               then
                  Set_Is_Frozen (Entity (Name (Unit_Declaration_Node (Bod))));
               end if;
            end if;
         end Freeze_Stream_Subprogram;

      --  Start of processing for Input

      begin
         --  If no underlying type, we have an error that will be diagnosed
         --  elsewhere, so here we just completely ignore the expansion.

         if No (U_Type) then
            return;
         end if;

         --  If there is a TSS for Input, just call it

         Fname := Find_Stream_Subprogram (P_Type, TSS_Stream_Input);

         if Present (Fname) then
            null;

         else
            --  If there is a Stream_Convert pragma, use it, we rewrite

            --     sourcetyp'Input (stream)

            --  as

            --     sourcetyp (streamread (strmtyp'Input (stream)));

            --  where stmrearead is the given Read function that converts
            --  an argument of type strmtyp to type sourcetyp or a type
            --  from which it is derived. The extra conversion is required
            --  for the derived case.

            Prag := Get_Stream_Convert_Pragma (P_Type);

            if Present (Prag) then
               Arg2  := Next (First (Pragma_Argument_Associations (Prag)));
               Rfunc := Entity (Expression (Arg2));

               Rewrite (N,
                 Convert_To (B_Type,
                   Make_Function_Call (Loc,
                     Name => New_Occurrence_Of (Rfunc, Loc),
                     Parameter_Associations => New_List (
                       Make_Attribute_Reference (Loc,
                         Prefix =>
                           New_Occurrence_Of
                             (Etype (First_Formal (Rfunc)), Loc),
                         Attribute_Name => Name_Input,
                         Expressions => Exprs)))));

               Analyze_And_Resolve (N, B_Type);
               return;

            --  Elementary types

            elsif Is_Elementary_Type (U_Type) then

               --  A special case arises if we have a defined _Read routine,
               --  since in this case we are required to call this routine.

               if Present (TSS (Base_Type (U_Type), TSS_Stream_Read)) then
                  Build_Record_Or_Elementary_Input_Function
                    (Loc, U_Type, Decl, Fname);
                  Insert_Action (N, Decl);

               --  For normal cases, we call the I_xxx routine directly

               else
                  Rewrite (N, Build_Elementary_Input_Call (N));
                  Analyze_And_Resolve (N, P_Type);
                  return;
               end if;

            --  Array type case

            elsif Is_Array_Type (U_Type) then
               Build_Array_Input_Function (Loc, U_Type, Decl, Fname);
               Compile_Stream_Body_In_Scope (N, Decl, U_Type, Check => False);

            --  Dispatching case with class-wide type

            elsif Is_Class_Wide_Type (P_Type) then

               declare
                  Rtyp : constant Entity_Id := Root_Type (P_Type);
                  Dnn  : Entity_Id;
                  Decl : Node_Id;

               begin
                  --  Read the internal tag (RM 13.13.2(34)) and use it to
                  --  initialize a dummy tag object:

                  --    Dnn : Ada.Tags.Tag
                  --           := Descendant_Tag (String'Input (Strm), P_Type);

                  --  This dummy object is used only to provide a controlling
                  --  argument for the eventual _Input call. Descendant_Tag is
                  --  called rather than Internal_Tag to ensure that we have a
                  --  tag for a type that is descended from the prefix type and
                  --  declared at the same accessibility level (the exception
                  --  Tag_Error will be raised otherwise). The level check is
                  --  required for Ada 2005 because tagged types can be
                  --  extended in nested scopes (AI-344).

                  Dnn :=
                    Make_Defining_Identifier (Loc,
                      Chars => New_Internal_Name ('D'));

                  Decl :=
                    Make_Object_Declaration (Loc,
                      Defining_Identifier => Dnn,
                      Object_Definition =>
                        New_Occurrence_Of (RTE (RE_Tag), Loc),
                      Expression =>
                        Make_Function_Call (Loc,
                          Name =>
                            New_Occurrence_Of (RTE (RE_Descendant_Tag), Loc),
                          Parameter_Associations => New_List (
                            Make_Attribute_Reference (Loc,
                              Prefix =>
                                New_Occurrence_Of (Standard_String, Loc),
                              Attribute_Name => Name_Input,
                              Expressions => New_List (
                                Relocate_Node
                                  (Duplicate_Subexpr (Strm)))),
                            Make_Attribute_Reference (Loc,
                              Prefix => New_Reference_To (P_Type, Loc),
                              Attribute_Name => Name_Tag))));

                  Insert_Action (N, Decl);

                  --  Now we need to get the entity for the call, and construct
                  --  a function call node, where we preset a reference to Dnn
                  --  as the controlling argument (doing an unchecked convert
                  --  to the class-wide tagged type to make it look like a real
                  --  tagged object).

                  Fname := Find_Prim_Op (Rtyp, TSS_Stream_Input);
                  Cntrl := Unchecked_Convert_To (P_Type,
                             New_Occurrence_Of (Dnn, Loc));
                  Set_Etype (Cntrl, P_Type);
                  Set_Parent (Cntrl, N);
               end;

            --  For tagged types, use the primitive Input function

            elsif Is_Tagged_Type (U_Type) then
               Fname := Find_Prim_Op (U_Type, TSS_Stream_Input);

            --  All other record type cases, including protected records. The
            --  latter only arise for expander generated code for handling
            --  shared passive partition access.

            else
               pragma Assert
                 (Is_Record_Type (U_Type) or else Is_Protected_Type (U_Type));

               --  Ada 2005 (AI-216): Program_Error is raised when executing
               --  the default implementation of the Input attribute of an
               --  unchecked union type if the type lacks default discriminant
               --  values.

               if Is_Unchecked_Union (Base_Type (U_Type))
                 and then not Present (Discriminant_Constraint (U_Type))
               then
                  Insert_Action (N,
                    Make_Raise_Program_Error (Loc,
                      Reason => PE_Unchecked_Union_Restriction));

                  return;
               end if;

               Build_Record_Or_Elementary_Input_Function
                 (Loc, Base_Type (U_Type), Decl, Fname);
               Insert_Action (N, Decl);

               if Nkind (Parent (N)) = N_Object_Declaration
                 and then Is_Record_Type (U_Type)
               then
                  --  The stream function may contain calls to user-defined
                  --  Read procedures for individual components.

                  declare
                     Comp : Entity_Id;
                     Func : Entity_Id;

                  begin
                     Comp := First_Component (U_Type);
                     while Present (Comp) loop
                        Func :=
                          Find_Stream_Subprogram
                            (Etype (Comp), TSS_Stream_Read);

                        if Present (Func) then
                           Freeze_Stream_Subprogram (Func);
                        end if;

                        Next_Component (Comp);
                     end loop;
                  end;
               end if;
            end if;
         end if;

         --  If we fall through, Fname is the function to be called. The result
         --  is obtained by calling the appropriate function, then converting
         --  the result. The conversion does a subtype check.

         Call :=
           Make_Function_Call (Loc,
             Name => New_Occurrence_Of (Fname, Loc),
             Parameter_Associations => New_List (
                Relocate_Node (Strm)));

         Set_Controlling_Argument (Call, Cntrl);
         Rewrite (N, Unchecked_Convert_To (P_Type, Call));
         Analyze_And_Resolve (N, P_Type);

         if Nkind (Parent (N)) = N_Object_Declaration then
            Freeze_Stream_Subprogram (Fname);
         end if;
      end Input;

      -------------------
      -- Integer_Value --
      -------------------

      --  We transform

      --    inttype'Fixed_Value (fixed-value)

      --  into

      --    inttype(integer-value))

      --  we do all the required analysis of the conversion here, because
      --  we do not want this to go through the fixed-point conversion
      --  circuits. Note that gigi always treats fixed-point as equivalent
      --  to the corresponding integer type anyway.

      when Attribute_Integer_Value => Integer_Value :
      begin
         Rewrite (N,
           Make_Type_Conversion (Loc,
             Subtype_Mark => New_Occurrence_Of (Entity (Pref), Loc),
             Expression   => Relocate_Node (First (Exprs))));
         Set_Etype (N, Entity (Pref));
         Set_Analyzed (N);

      --  Note: it might appear that a properly analyzed unchecked conversion
      --  would be just fine here, but that's not the case, since the full
      --  range checks performed by the following call are critical!

         Apply_Type_Conversion_Checks (N);
      end Integer_Value;

      ----------
      -- Last --
      ----------

      when Attribute_Last => declare
         Ptyp : constant Entity_Id := Etype (Pref);

      begin
         --  If the prefix type is a constrained packed array type which
         --  already has a Packed_Array_Type representation defined, then
         --  replace this attribute with a direct reference to 'Last of the
         --  appropriate index subtype (since otherwise Gigi will try to give
         --  us the value of 'Last for this implementation type).

         if Is_Constrained_Packed_Array (Ptyp) then
            Rewrite (N,
              Make_Attribute_Reference (Loc,
                Attribute_Name => Name_Last,
                Prefix => New_Reference_To (Get_Index_Subtype (N), Loc)));
            Analyze_And_Resolve (N, Typ);

         elsif Is_Access_Type (Ptyp) then
            Apply_Access_Check (N);
         end if;
      end;

      --------------
      -- Last_Bit --
      --------------

      --  We compute this if a component clause was present, otherwise
      --  we leave the computation up to Gigi, since we don't know what
      --  layout will be chosen.

      when Attribute_Last_Bit => Last_Bit :
      declare
         CE : constant Entity_Id := Entity (Selector_Name (Pref));

      begin
         if Known_Static_Component_Bit_Offset (CE)
           and then Known_Static_Esize (CE)
         then
            Rewrite (N,
              Make_Integer_Literal (Loc,
               Intval => (Component_Bit_Offset (CE) mod System_Storage_Unit)
                                + Esize (CE) - 1));

            Analyze_And_Resolve (N, Typ);

         else
            Apply_Universal_Integer_Attribute_Checks (N);
         end if;
      end Last_Bit;

      ------------------
      -- Leading_Part --
      ------------------

      --  Transforms 'Leading_Part into a call to the floating-point attribute
      --  function Leading_Part in Fat_xxx (where xxx is the root type)

      --  Note: strictly, we should have special case code to deal with
      --  absurdly large positive arguments (greater than Integer'Last), which
      --  result in returning the first argument unchanged, but it hardly seems
      --  worth the effort. We raise constraint error for absurdly negative
      --  arguments which is fine.

      when Attribute_Leading_Part =>
         Expand_Fpt_Attribute_RI (N);

      ------------
      -- Length --
      ------------

      when Attribute_Length => declare
         Ptyp : constant Entity_Id := Etype (Pref);
         Ityp : Entity_Id;
         Xnum : Uint;

      begin
         --  Processing for packed array types

         if Is_Array_Type (Ptyp) and then Is_Packed (Ptyp) then
            Ityp := Get_Index_Subtype (N);

            --  If the index type, Ityp, is an enumeration type with
            --  holes, then we calculate X'Length explicitly using

            --     Typ'Max
            --       (0, Ityp'Pos (X'Last  (N)) -
            --           Ityp'Pos (X'First (N)) + 1);

            --  Since the bounds in the template are the representation
            --  values and gigi would get the wrong value.

            if Is_Enumeration_Type (Ityp)
              and then Present (Enum_Pos_To_Rep (Base_Type (Ityp)))
            then
               if No (Exprs) then
                  Xnum := Uint_1;
               else
                  Xnum := Expr_Value (First (Expressions (N)));
               end if;

               Rewrite (N,
                 Make_Attribute_Reference (Loc,
                   Prefix         => New_Occurrence_Of (Typ, Loc),
                   Attribute_Name => Name_Max,
                   Expressions    => New_List
                     (Make_Integer_Literal (Loc, 0),

                      Make_Op_Add (Loc,
                        Left_Opnd =>
                          Make_Op_Subtract (Loc,
                            Left_Opnd =>
                              Make_Attribute_Reference (Loc,
                                Prefix => New_Occurrence_Of (Ityp, Loc),
                                Attribute_Name => Name_Pos,

                                Expressions => New_List (
                                  Make_Attribute_Reference (Loc,
                                    Prefix => Duplicate_Subexpr (Pref),
                                   Attribute_Name => Name_Last,
                                    Expressions => New_List (
                                      Make_Integer_Literal (Loc, Xnum))))),

                            Right_Opnd =>
                              Make_Attribute_Reference (Loc,
                                Prefix => New_Occurrence_Of (Ityp, Loc),
                                Attribute_Name => Name_Pos,

                                Expressions => New_List (
                                  Make_Attribute_Reference (Loc,
                                    Prefix =>
                                      Duplicate_Subexpr_No_Checks (Pref),
                                   Attribute_Name => Name_First,
                                    Expressions => New_List (
                                      Make_Integer_Literal (Loc, Xnum)))))),

                        Right_Opnd => Make_Integer_Literal (Loc, 1)))));

               Analyze_And_Resolve (N, Typ, Suppress => All_Checks);
               return;

            --  If the prefix type is a constrained packed array type which
            --  already has a Packed_Array_Type representation defined, then
            --  replace this attribute with a direct reference to 'Range_Length
            --  of the appropriate index subtype (since otherwise Gigi will try
            --  to give us the value of 'Length for this implementation type).

            elsif Is_Constrained (Ptyp) then
               Rewrite (N,
                 Make_Attribute_Reference (Loc,
                   Attribute_Name => Name_Range_Length,
                   Prefix => New_Reference_To (Ityp, Loc)));
               Analyze_And_Resolve (N, Typ);
            end if;

         --  If we have a packed array that is not bit packed, which was

         --  Access type case

         elsif Is_Access_Type (Ptyp) then
            Apply_Access_Check (N);

            --  If the designated type is a packed array type, then we
            --  convert the reference to:

            --    typ'Max (0, 1 +
            --                xtyp'Pos (Pref'Last (Expr)) -
            --                xtyp'Pos (Pref'First (Expr)));

            --  This is a bit complex, but it is the easiest thing to do
            --  that works in all cases including enum types with holes
            --  xtyp here is the appropriate index type.

            declare
               Dtyp : constant Entity_Id := Designated_Type (Ptyp);
               Xtyp : Entity_Id;

            begin
               if Is_Array_Type (Dtyp) and then Is_Packed (Dtyp) then
                  Xtyp := Get_Index_Subtype (N);

                  Rewrite (N,
                    Make_Attribute_Reference (Loc,
                      Prefix         => New_Occurrence_Of (Typ, Loc),
                      Attribute_Name => Name_Max,
                      Expressions    => New_List (
                        Make_Integer_Literal (Loc, 0),

                        Make_Op_Add (Loc,
                          Make_Integer_Literal (Loc, 1),
                          Make_Op_Subtract (Loc,
                            Left_Opnd =>
                              Make_Attribute_Reference (Loc,
                                Prefix => New_Occurrence_Of (Xtyp, Loc),
                                Attribute_Name => Name_Pos,
                                Expressions    => New_List (
                                  Make_Attribute_Reference (Loc,
                                    Prefix => Duplicate_Subexpr (Pref),
                                    Attribute_Name => Name_Last,
                                    Expressions =>
                                      New_Copy_List (Exprs)))),

                            Right_Opnd =>
                              Make_Attribute_Reference (Loc,
                                Prefix => New_Occurrence_Of (Xtyp, Loc),
                                Attribute_Name => Name_Pos,
                                Expressions    => New_List (
                                  Make_Attribute_Reference (Loc,
                                    Prefix =>
                                      Duplicate_Subexpr_No_Checks (Pref),
                                    Attribute_Name => Name_First,
                                    Expressions =>
                                      New_Copy_List (Exprs)))))))));

                  Analyze_And_Resolve (N, Typ);
               end if;
            end;

         --  Otherwise leave it to gigi

         else
            Apply_Universal_Integer_Attribute_Checks (N);
         end if;
      end;

      -------------
      -- Machine --
      -------------

      --  Transforms 'Machine into a call to the floating-point attribute
      --  function Machine in Fat_xxx (where xxx is the root type)

      when Attribute_Machine =>
         Expand_Fpt_Attribute_R (N);

      ------------------
      -- Machine_Size --
      ------------------

      --  Machine_Size is equivalent to Object_Size, so transform it into
      --  Object_Size and that way Gigi never sees Machine_Size.

      when Attribute_Machine_Size =>
         Rewrite (N,
           Make_Attribute_Reference (Loc,
             Prefix => Prefix (N),
             Attribute_Name => Name_Object_Size));

         Analyze_And_Resolve (N, Typ);

      --------------
      -- Mantissa --
      --------------

      --  The only case that can get this far is the dynamic case of the old
      --  Ada 83 Mantissa attribute for the fixed-point case. For this case, we
      --  expand:

      --    typ'Mantissa

      --  into

      --    ityp (System.Mantissa.Mantissa_Value
      --           (Integer'Integer_Value (typ'First),
      --            Integer'Integer_Value (typ'Last)));

      when Attribute_Mantissa => Mantissa : declare
         Ptyp : constant Entity_Id := Etype (Pref);

      begin
         Rewrite (N,
           Convert_To (Typ,
             Make_Function_Call (Loc,
               Name => New_Occurrence_Of (RTE (RE_Mantissa_Value), Loc),

               Parameter_Associations => New_List (

                 Make_Attribute_Reference (Loc,
                   Prefix => New_Occurrence_Of (Standard_Integer, Loc),
                   Attribute_Name => Name_Integer_Value,
                   Expressions => New_List (

                     Make_Attribute_Reference (Loc,
                       Prefix => New_Occurrence_Of (Ptyp, Loc),
                       Attribute_Name => Name_First))),

                 Make_Attribute_Reference (Loc,
                   Prefix => New_Occurrence_Of (Standard_Integer, Loc),
                   Attribute_Name => Name_Integer_Value,
                   Expressions => New_List (

                     Make_Attribute_Reference (Loc,
                       Prefix => New_Occurrence_Of (Ptyp, Loc),
                       Attribute_Name => Name_Last)))))));

         Analyze_And_Resolve (N, Typ);
      end Mantissa;

      ---------
      -- Mod --
      ---------

      when Attribute_Mod => Mod_Case : declare
         Arg  : constant Node_Id := Relocate_Node (First (Exprs));
         Hi   : constant Node_Id := Type_High_Bound (Etype (Arg));
         Modv : constant Uint    := Modulus (Btyp);

      begin

         --  This is not so simple. The issue is what type to use for the
         --  computation of the modular value.

         --  The easy case is when the modulus value is within the bounds
         --  of the signed integer type of the argument. In this case we can
         --  just do the computation in that signed integer type, and then
         --  do an ordinary conversion to the target type.

         if Modv <= Expr_Value (Hi) then
            Rewrite (N,
              Convert_To (Btyp,
                Make_Op_Mod (Loc,
                  Left_Opnd  => Arg,
                  Right_Opnd => Make_Integer_Literal (Loc, Modv))));

         --  Here we know that the modulus is larger than type'Last of the
         --  integer type. There are two cases to consider:

         --    a) The integer value is non-negative. In this case, it is
         --    returned as the result (since it is less than the modulus).

         --    b) The integer value is negative. In this case, we know that the
         --    result is modulus + value, where the value might be as small as
         --    -modulus. The trouble is what type do we use to do the subtract.
         --    No type will do, since modulus can be as big as 2**64, and no
         --    integer type accomodates this value. Let's do bit of algebra

         --         modulus + value
         --      =  modulus - (-value)
         --      =  (modulus - 1) - (-value - 1)

         --    Now modulus - 1 is certainly in range of the modular type.
         --    -value is in the range 1 .. modulus, so -value -1 is in the
         --    range 0 .. modulus-1 which is in range of the modular type.
         --    Furthermore, (-value - 1) can be expressed as -(value + 1)
         --    which we can compute using the integer base type.

         --  Once this is done we analyze the conditional expression without
         --  range checks, because we know everything is in range, and we
         --  want to prevent spurious warnings on either branch.

         else
            Rewrite (N,
              Make_Conditional_Expression (Loc,
                Expressions => New_List (
                  Make_Op_Ge (Loc,
                    Left_Opnd  => Duplicate_Subexpr (Arg),
                    Right_Opnd => Make_Integer_Literal (Loc, 0)),

                  Convert_To (Btyp,
                    Duplicate_Subexpr_No_Checks (Arg)),

                  Make_Op_Subtract (Loc,
                    Left_Opnd =>
                      Make_Integer_Literal (Loc,
                        Intval => Modv - 1),
                    Right_Opnd =>
                      Convert_To (Btyp,
                        Make_Op_Minus (Loc,
                          Right_Opnd =>
                            Make_Op_Add (Loc,
                              Left_Opnd  => Duplicate_Subexpr_No_Checks (Arg),
                              Right_Opnd =>
                                Make_Integer_Literal (Loc,
                                  Intval => 1))))))));

         end if;

         Analyze_And_Resolve (N, Btyp, All_Checks);
      end Mod_Case;

      -----------
      -- Model --
      -----------

      --  Transforms 'Model into a call to the floating-point attribute
      --  function Model in Fat_xxx (where xxx is the root type)

      when Attribute_Model =>
         Expand_Fpt_Attribute_R (N);

      -----------------
      -- Object_Size --
      -----------------

      --  The processing for Object_Size shares the processing for Size

      ------------
      -- Output --
      ------------

      when Attribute_Output => Output : declare
         P_Type : constant Entity_Id := Entity (Pref);
         U_Type : constant Entity_Id := Underlying_Type (P_Type);
         Pname  : Entity_Id;
         Decl   : Node_Id;
         Prag   : Node_Id;
         Arg3   : Node_Id;
         Wfunc  : Node_Id;

      begin
         --  If no underlying type, we have an error that will be diagnosed
         --  elsewhere, so here we just completely ignore the expansion.

         if No (U_Type) then
            return;
         end if;

         --  If TSS for Output is present, just call it

         Pname := Find_Stream_Subprogram (P_Type, TSS_Stream_Output);

         if Present (Pname) then
            null;

         else
            --  If there is a Stream_Convert pragma, use it, we rewrite

            --     sourcetyp'Output (stream, Item)

            --  as

            --     strmtyp'Output (Stream, strmwrite (acttyp (Item)));

            --  where strmwrite is the given Write function that converts an
            --  argument of type sourcetyp or a type acctyp, from which it is
            --  derived to type strmtyp. The conversion to acttyp is required
            --  for the derived case.

            Prag := Get_Stream_Convert_Pragma (P_Type);

            if Present (Prag) then
               Arg3 :=
                 Next (Next (First (Pragma_Argument_Associations (Prag))));
               Wfunc := Entity (Expression (Arg3));

               Rewrite (N,
                 Make_Attribute_Reference (Loc,
                   Prefix => New_Occurrence_Of (Etype (Wfunc), Loc),
                   Attribute_Name => Name_Output,
                   Expressions => New_List (
                   Relocate_Node (First (Exprs)),
                     Make_Function_Call (Loc,
                       Name => New_Occurrence_Of (Wfunc, Loc),
                       Parameter_Associations => New_List (
                         Convert_To (Etype (First_Formal (Wfunc)),
                           Relocate_Node (Next (First (Exprs)))))))));

               Analyze (N);
               return;

            --  For elementary types, we call the W_xxx routine directly.
            --  Note that the effect of Write and Output is identical for
            --  the case of an elementary type, since there are no
            --  discriminants or bounds.

            elsif Is_Elementary_Type (U_Type) then

               --  A special case arises if we have a defined _Write routine,
               --  since in this case we are required to call this routine.

               if Present (TSS (Base_Type (U_Type), TSS_Stream_Write)) then
                  Build_Record_Or_Elementary_Output_Procedure
                    (Loc, U_Type, Decl, Pname);
                  Insert_Action (N, Decl);

               --  For normal cases, we call the W_xxx routine directly

               else
                  Rewrite (N, Build_Elementary_Write_Call (N));
                  Analyze (N);
                  return;
               end if;

            --  Array type case

            elsif Is_Array_Type (U_Type) then
               Build_Array_Output_Procedure (Loc, U_Type, Decl, Pname);
               Compile_Stream_Body_In_Scope (N, Decl, U_Type, Check => False);

            --  Class-wide case, first output external tag, then dispatch
            --  to the appropriate primitive Output function (RM 13.13.2(31)).

            elsif Is_Class_Wide_Type (P_Type) then
               Tag_Write : declare
                  Strm : constant Node_Id := First (Exprs);
                  Item : constant Node_Id := Next (Strm);

               begin
                  --  The code is:
                  --  if Get_Access_Level (Item'Tag)
                  --       /= Get_Access_Level (P_Type'Tag)
                  --  then
                  --     raise Tag_Error;
                  --  end if;
                  --  String'Output (Strm, External_Tag (Item'Tag));

                  --  Ada 2005 (AI-344): Check that the accessibility level
                  --  of the type of the output object is not deeper than
                  --  that of the attribute's prefix type.

                  if Ada_Version >= Ada_05 then
                     Insert_Action (N,
                       Make_Implicit_If_Statement (N,
                         Condition =>
                           Make_Op_Ne (Loc,
                             Left_Opnd  =>
                               Make_Function_Call (Loc,
                                 Name =>
                                   New_Reference_To
                                     (RTE (RE_Get_Access_Level), Loc),
                                 Parameter_Associations =>
                                   New_List (Make_Attribute_Reference (Loc,
                                               Prefix         =>
                                                 Relocate_Node (
                                                   Duplicate_Subexpr (Item,
                                                     Name_Req => True)),
                                               Attribute_Name =>
                                                  Name_Tag))),
                             Right_Opnd =>
                               Make_Integer_Literal
                                 (Loc, Type_Access_Level (P_Type))),
                         Then_Statements =>
                           New_List (Make_Raise_Statement (Loc,
                                       New_Occurrence_Of (
                                         RTE (RE_Tag_Error), Loc)))));
                  end if;

                  Insert_Action (N,
                    Make_Attribute_Reference (Loc,
                      Prefix => New_Occurrence_Of (Standard_String, Loc),
                      Attribute_Name => Name_Output,
                      Expressions => New_List (
                        Relocate_Node (Duplicate_Subexpr (Strm)),
                        Make_Function_Call (Loc,
                          Name =>
                            New_Occurrence_Of (RTE (RE_External_Tag), Loc),
                          Parameter_Associations => New_List (
                           Make_Attribute_Reference (Loc,
                             Prefix =>
                               Relocate_Node
                                 (Duplicate_Subexpr (Item, Name_Req => True)),
                             Attribute_Name => Name_Tag))))));
               end Tag_Write;

               Pname := Find_Prim_Op (U_Type, TSS_Stream_Output);

            --  Tagged type case, use the primitive Output function

            elsif Is_Tagged_Type (U_Type) then
               Pname := Find_Prim_Op (U_Type, TSS_Stream_Output);

--              --  All other record type cases, including protected records.
--              --  The latter only arise for expander generated code for
--              --  handling shared passive partition access.

            else
               pragma Assert
                 (Is_Record_Type (U_Type) or else Is_Protected_Type (U_Type));

               --  Ada 2005 (AI-216): Program_Error is raised when executing
               --  the default implementation of the Output attribute of an
               --  unchecked union type if the type lacks default discriminant
               --  values.

               if Is_Unchecked_Union (Base_Type (U_Type))
                 and then not Present (Discriminant_Constraint (U_Type))
               then
                  Insert_Action (N,
                    Make_Raise_Program_Error (Loc,
                      Reason => PE_Unchecked_Union_Restriction));

                  return;
               end if;

               Build_Record_Or_Elementary_Output_Procedure
                 (Loc, Base_Type (U_Type), Decl, Pname);
               Insert_Action (N, Decl);
            end if;
         end if;

         --  If we fall through, Pname is the name of the procedure to call

         Rewrite_Stream_Proc_Call (Pname);
      end Output;

      ---------
      -- Pos --
      ---------

      --  For enumeration types with a standard representation, Pos is
      --  handled by Gigi.

      --  For enumeration types, with a non-standard representation we
      --  generate a call to the _Rep_To_Pos function created when the
      --  type was frozen. The call has the form

      --    _rep_to_pos (expr, flag)

      --  The parameter flag is True if range checks are enabled, causing
      --  Program_Error to be raised if the expression has an invalid
      --  representation, and False if range checks are suppressed.

      --  For integer types, Pos is equivalent to a simple integer
      --  conversion and we rewrite it as such

      when Attribute_Pos => Pos :
      declare
         Etyp : Entity_Id := Base_Type (Entity (Pref));

      begin
         --  Deal with zero/non-zero boolean values

         if Is_Boolean_Type (Etyp) then
            Adjust_Condition (First (Exprs));
            Etyp := Standard_Boolean;
            Set_Prefix (N, New_Occurrence_Of (Standard_Boolean, Loc));
         end if;

         --  Case of enumeration type

         if Is_Enumeration_Type (Etyp) then

            --  Non-standard enumeration type (generate call)

            if Present (Enum_Pos_To_Rep (Etyp)) then
               Append_To (Exprs, Rep_To_Pos_Flag (Etyp, Loc));
               Rewrite (N,
                 Convert_To (Typ,
                   Make_Function_Call (Loc,
                     Name =>
                       New_Reference_To (TSS (Etyp, TSS_Rep_To_Pos), Loc),
                     Parameter_Associations => Exprs)));

               Analyze_And_Resolve (N, Typ);

            --  Standard enumeration type (do universal integer check)

            else
               Apply_Universal_Integer_Attribute_Checks (N);
            end if;

         --  Deal with integer types (replace by conversion)

         elsif Is_Integer_Type (Etyp) then
            Rewrite (N, Convert_To (Typ, First (Exprs)));
            Analyze_And_Resolve (N, Typ);
         end if;

      end Pos;

      --------------
      -- Position --
      --------------

      --  We compute this if a component clause was present, otherwise
      --  we leave the computation up to Gigi, since we don't know what
      --  layout will be chosen.

      when Attribute_Position => Position :
      declare
         CE : constant Entity_Id := Entity (Selector_Name (Pref));

      begin
         if Present (Component_Clause (CE)) then
            Rewrite (N,
              Make_Integer_Literal (Loc,
                Intval => Component_Bit_Offset (CE) / System_Storage_Unit));
            Analyze_And_Resolve (N, Typ);

         else
            Apply_Universal_Integer_Attribute_Checks (N);
         end if;
      end Position;

      ----------
      -- Pred --
      ----------

      --  1. Deal with enumeration types with holes
      --  2. For floating-point, generate call to attribute function
      --  3. For other cases, deal with constraint checking

      when Attribute_Pred => Pred :
      declare
         Ptyp : constant Entity_Id := Base_Type (Etype (Pref));

      begin
         --  For enumeration types with non-standard representations, we
         --  expand typ'Pred (x) into

         --    Pos_To_Rep (Rep_To_Pos (x) - 1)

         --    If the representation is contiguous, we compute instead
         --    Lit1 + Rep_to_Pos (x -1), to catch invalid representations.

         if Is_Enumeration_Type (Ptyp)
           and then Present (Enum_Pos_To_Rep (Ptyp))
         then
            if Has_Contiguous_Rep (Ptyp) then
               Rewrite (N,
                  Unchecked_Convert_To (Ptyp,
                     Make_Op_Add (Loc,
                        Left_Opnd  =>
                         Make_Integer_Literal (Loc,
                           Enumeration_Rep (First_Literal (Ptyp))),
                        Right_Opnd =>
                          Make_Function_Call (Loc,
                            Name =>
                              New_Reference_To
                               (TSS (Ptyp, TSS_Rep_To_Pos), Loc),

                            Parameter_Associations =>
                              New_List (
                                Unchecked_Convert_To (Ptyp,
                                  Make_Op_Subtract (Loc,
                                    Left_Opnd =>
                                     Unchecked_Convert_To (Standard_Integer,
                                       Relocate_Node (First (Exprs))),
                                    Right_Opnd =>
                                      Make_Integer_Literal (Loc, 1))),
                                Rep_To_Pos_Flag (Ptyp, Loc))))));

            else
               --  Add Boolean parameter True, to request program errror if
               --  we have a bad representation on our hands. If checks are
               --  suppressed, then add False instead

               Append_To (Exprs, Rep_To_Pos_Flag (Ptyp, Loc));
               Rewrite (N,
                 Make_Indexed_Component (Loc,
                   Prefix => New_Reference_To (Enum_Pos_To_Rep (Ptyp), Loc),
                   Expressions => New_List (
                     Make_Op_Subtract (Loc,
                    Left_Opnd =>
                      Make_Function_Call (Loc,
                        Name =>
                          New_Reference_To (TSS (Ptyp, TSS_Rep_To_Pos), Loc),
                          Parameter_Associations => Exprs),
                    Right_Opnd => Make_Integer_Literal (Loc, 1)))));
            end if;

            Analyze_And_Resolve (N, Typ);

         --  For floating-point, we transform 'Pred into a call to the Pred
         --  floating-point attribute function in Fat_xxx (xxx is root type)

         elsif Is_Floating_Point_Type (Ptyp) then
            Expand_Fpt_Attribute_R (N);
            Analyze_And_Resolve (N, Typ);

         --  For modular types, nothing to do (no overflow, since wraps)

         elsif Is_Modular_Integer_Type (Ptyp) then
            null;

         --  For other types, if range checking is enabled, we must generate
         --  a check if overflow checking is enabled.

         elsif not Overflow_Checks_Suppressed (Ptyp) then
            Expand_Pred_Succ (N);
         end if;

      end Pred;

      ------------------
      -- Range_Length --
      ------------------

      when Attribute_Range_Length => Range_Length : declare
         P_Type : constant Entity_Id := Etype (Pref);

      begin
         --  The only special processing required is for the case where
         --  Range_Length is applied to an enumeration type with holes.
         --  In this case we transform

         --     X'Range_Length

         --  to

         --     X'Pos (X'Last) - X'Pos (X'First) + 1

         --  So that the result reflects the proper Pos values instead
         --  of the underlying representations.

         if Is_Enumeration_Type (P_Type)
           and then Has_Non_Standard_Rep (P_Type)
         then
            Rewrite (N,
              Make_Op_Add (Loc,
                Left_Opnd =>
                  Make_Op_Subtract (Loc,
                    Left_Opnd =>
                      Make_Attribute_Reference (Loc,
                        Attribute_Name => Name_Pos,
                        Prefix => New_Occurrence_Of (P_Type, Loc),
                        Expressions => New_List (
                          Make_Attribute_Reference (Loc,
                            Attribute_Name => Name_Last,
                            Prefix => New_Occurrence_Of (P_Type, Loc)))),

                    Right_Opnd =>
                      Make_Attribute_Reference (Loc,
                        Attribute_Name => Name_Pos,
                        Prefix => New_Occurrence_Of (P_Type, Loc),
                        Expressions => New_List (
                          Make_Attribute_Reference (Loc,
                            Attribute_Name => Name_First,
                            Prefix => New_Occurrence_Of (P_Type, Loc))))),

                Right_Opnd =>
                  Make_Integer_Literal (Loc, 1)));

            Analyze_And_Resolve (N, Typ);

         --  For all other cases, attribute is handled by Gigi, but we need
         --  to deal with the case of the range check on a universal integer.

         else
            Apply_Universal_Integer_Attribute_Checks (N);
         end if;

      end Range_Length;

      ----------
      -- Read --
      ----------

      when Attribute_Read => Read : declare
         P_Type : constant Entity_Id := Entity (Pref);
         B_Type : constant Entity_Id := Base_Type (P_Type);
         U_Type : constant Entity_Id := Underlying_Type (P_Type);
         Pname  : Entity_Id;
         Decl   : Node_Id;
         Prag   : Node_Id;
         Arg2   : Node_Id;
         Rfunc  : Node_Id;
         Lhs    : Node_Id;
         Rhs    : Node_Id;

      begin
         --  If no underlying type, we have an error that will be diagnosed
         --  elsewhere, so here we just completely ignore the expansion.

         if No (U_Type) then
            return;
         end if;

         --  The simple case, if there is a TSS for Read, just call it

         Pname := Find_Stream_Subprogram (P_Type, TSS_Stream_Read);

         if Present (Pname) then
            null;

         else
            --  If there is a Stream_Convert pragma, use it, we rewrite

            --     sourcetyp'Read (stream, Item)

            --  as

            --     Item := sourcetyp (strmread (strmtyp'Input (Stream)));

            --  where strmread is the given Read function that converts an
            --  argument of type strmtyp to type sourcetyp or a type from which
            --  it is derived. The conversion to sourcetyp is required in the
            --  latter case.

            --  A special case arises if Item is a type conversion in which
            --  case, we have to expand to:

            --     Itemx := typex (strmread (strmtyp'Input (Stream)));

            --  where Itemx is the expression of the type conversion (i.e.
            --  the actual object), and typex is the type of Itemx.

            Prag := Get_Stream_Convert_Pragma (P_Type);

            if Present (Prag) then
               Arg2  := Next (First (Pragma_Argument_Associations (Prag)));
               Rfunc := Entity (Expression (Arg2));
               Lhs := Relocate_Node (Next (First (Exprs)));
               Rhs :=
                 Convert_To (B_Type,
                   Make_Function_Call (Loc,
                     Name => New_Occurrence_Of (Rfunc, Loc),
                     Parameter_Associations => New_List (
                       Make_Attribute_Reference (Loc,
                         Prefix =>
                           New_Occurrence_Of
                             (Etype (First_Formal (Rfunc)), Loc),
                         Attribute_Name => Name_Input,
                         Expressions => New_List (
                           Relocate_Node (First (Exprs)))))));

               if Nkind (Lhs) = N_Type_Conversion then
                  Lhs := Expression (Lhs);
                  Rhs := Convert_To (Etype (Lhs), Rhs);
               end if;

               Rewrite (N,
                 Make_Assignment_Statement (Loc,
                   Name       => Lhs,
                   Expression => Rhs));
               Set_Assignment_OK (Lhs);
               Analyze (N);
               return;

            --  For elementary types, we call the I_xxx routine using the first
            --  parameter and then assign the result into the second parameter.
            --  We set Assignment_OK to deal with the conversion case.

            elsif Is_Elementary_Type (U_Type) then
               declare
                  Lhs : Node_Id;
                  Rhs : Node_Id;

               begin
                  Lhs := Relocate_Node (Next (First (Exprs)));
                  Rhs := Build_Elementary_Input_Call (N);

                  if Nkind (Lhs) = N_Type_Conversion then
                     Lhs := Expression (Lhs);
                     Rhs := Convert_To (Etype (Lhs), Rhs);
                  end if;

                  Set_Assignment_OK (Lhs);

                  Rewrite (N,
                    Make_Assignment_Statement (Loc,
                      Name => Lhs,
                      Expression => Rhs));

                  Analyze (N);
                  return;
               end;

            --  Array type case

            elsif Is_Array_Type (U_Type) then
               Build_Array_Read_Procedure (N, U_Type, Decl, Pname);
               Compile_Stream_Body_In_Scope (N, Decl, U_Type, Check => False);

            --  Tagged type case, use the primitive Read function. Note that
            --  this will dispatch in the class-wide case which is what we want

            elsif Is_Tagged_Type (U_Type) then
               Pname := Find_Prim_Op (U_Type, TSS_Stream_Read);

            --  All other record type cases, including protected records. The
            --  latter only arise for expander generated code for handling
            --  shared passive partition access.

            else
               pragma Assert
                 (Is_Record_Type (U_Type) or else Is_Protected_Type (U_Type));

               --  Ada 2005 (AI-216): Program_Error is raised when executing
               --  the default implementation of the Read attribute of an
               --  Unchecked_Union type.

               if Is_Unchecked_Union (Base_Type (U_Type)) then
                  Insert_Action (N,
                    Make_Raise_Program_Error (Loc,
                      Reason => PE_Unchecked_Union_Restriction));
               end if;

               if Has_Discriminants (U_Type)
                 and then Present
                   (Discriminant_Default_Value (First_Discriminant (U_Type)))
               then
                  Build_Mutable_Record_Read_Procedure
                    (Loc, Base_Type (U_Type), Decl, Pname);
               else
                  Build_Record_Read_Procedure
                    (Loc, Base_Type (U_Type), Decl, Pname);
               end if;

               --  Suppress checks, uninitialized or otherwise invalid
               --  data does not cause constraint errors to be raised for
               --  a complete record read.

               Insert_Action (N, Decl, All_Checks);
            end if;
         end if;

         Rewrite_Stream_Proc_Call (Pname);
      end Read;

      ---------------
      -- Remainder --
      ---------------

      --  Transforms 'Remainder into a call to the floating-point attribute
      --  function Remainder in Fat_xxx (where xxx is the root type)

      when Attribute_Remainder =>
         Expand_Fpt_Attribute_RR (N);

      -----------
      -- Round --
      -----------

      --  The handling of the Round attribute is quite delicate. The processing
      --  in Sem_Attr introduced a conversion to universal real, reflecting the
      --  semantics of Round, but we do not want anything to do with universal
      --  real at runtime, since this corresponds to using floating-point
      --  arithmetic.

      --  What we have now is that the Etype of the Round attribute correctly
      --  indicates the final result type. The operand of the Round is the
      --  conversion to universal real, described above, and the operand of
      --  this conversion is the actual operand of Round, which may be the
      --  special case of a fixed point multiplication or division (Etype =
      --  universal fixed)

      --  The exapander will expand first the operand of the conversion, then
      --  the conversion, and finally the round attribute itself, since we
      --  always work inside out. But we cannot simply process naively in this
      --  order. In the semantic world where universal fixed and real really
      --  exist and have infinite precision, there is no problem, but in the
      --  implementation world, where universal real is a floating-point type,
      --  we would get the wrong result.

      --  So the approach is as follows. First, when expanding a multiply or
      --  divide whose type is universal fixed, we do nothing at all, instead
      --  deferring the operation till later.

      --  The actual processing is done in Expand_N_Type_Conversion which
      --  handles the special case of Round by looking at its parent to see if
      --  it is a Round attribute, and if it is, handling the conversion (or
      --  its fixed multiply/divide child) in an appropriate manner.

      --  This means that by the time we get to expanding the Round attribute
      --  itself, the Round is nothing more than a type conversion (and will
      --  often be a null type conversion), so we just replace it with the
      --  appropriate conversion operation.

      when Attribute_Round =>
         Rewrite (N,
           Convert_To (Etype (N), Relocate_Node (First (Exprs))));
         Analyze_And_Resolve (N);

      --------------
      -- Rounding --
      --------------

      --  Transforms 'Rounding into a call to the floating-point attribute
      --  function Rounding in Fat_xxx (where xxx is the root type)

      when Attribute_Rounding =>
         Expand_Fpt_Attribute_R (N);

      -------------
      -- Scaling --
      -------------

      --  Transforms 'Scaling into a call to the floating-point attribute
      --  function Scaling in Fat_xxx (where xxx is the root type)

      when Attribute_Scaling =>
         Expand_Fpt_Attribute_RI (N);

      ----------
      -- Size --
      ----------

      when Attribute_Size        |
           Attribute_Object_Size |
           Attribute_Value_Size  |
           Attribute_VADS_Size   => Size :

      declare
         Ptyp     : constant Entity_Id := Etype (Pref);
         Siz      : Uint;
         New_Node : Node_Id;

      begin
         --  Processing for VADS_Size case. Note that this processing removes
         --  all traces of VADS_Size from the tree, and completes all required
         --  processing for VADS_Size by translating the attribute reference
         --  to an appropriate Size or Object_Size reference.

         if Id = Attribute_VADS_Size
           or else (Use_VADS_Size and then Id = Attribute_Size)
         then
            --  If the size is specified, then we simply use the specified
            --  size. This applies to both types and objects. The size of an
            --  object can be specified in the following ways:

            --    An explicit size object is given for an object
            --    A component size is specified for an indexed component
            --    A component clause is specified for a selected component
            --    The object is a component of a packed composite object

            --  If the size is specified, then VADS_Size of an object

            if (Is_Entity_Name (Pref)
                 and then Present (Size_Clause (Entity (Pref))))
              or else
                (Nkind (Pref) = N_Component_Clause
                  and then (Present (Component_Clause
                                     (Entity (Selector_Name (Pref))))
                             or else Is_Packed (Etype (Prefix (Pref)))))
              or else
                (Nkind (Pref) = N_Indexed_Component
                  and then (Component_Size (Etype (Prefix (Pref))) /= 0
                             or else Is_Packed (Etype (Prefix (Pref)))))
            then
               Set_Attribute_Name (N, Name_Size);

            --  Otherwise if we have an object rather than a type, then the
            --  VADS_Size attribute applies to the type of the object, rather
            --  than the object itself. This is one of the respects in which
            --  VADS_Size differs from Size.

            else
               if (not Is_Entity_Name (Pref)
                    or else not Is_Type (Entity (Pref)))
                 and then (Is_Scalar_Type (Etype (Pref))
                            or else Is_Constrained (Etype (Pref)))
               then
                  Rewrite (Pref, New_Occurrence_Of (Etype (Pref), Loc));
               end if;

               --  For a scalar type for which no size was explicitly given,
               --  VADS_Size means Object_Size. This is the other respect in
               --  which VADS_Size differs from Size.

               if Is_Scalar_Type (Etype (Pref))
                 and then No (Size_Clause (Etype (Pref)))
               then
                  Set_Attribute_Name (N, Name_Object_Size);

               --  In all other cases, Size and VADS_Size are the sane

               else
                  Set_Attribute_Name (N, Name_Size);
               end if;
            end if;
         end if;

         --  For class-wide types,  X'Class'Size is transformed into a
         --  direct reference to the Size of the class type, so that gigi
         --  does not have to deal with the X'Class'Size reference.

         if Is_Entity_Name (Pref)
           and then Is_Class_Wide_Type (Entity (Pref))
         then
            Rewrite (Prefix (N), New_Occurrence_Of (Entity (Pref), Loc));
            return;

         --  For x'Size applied to an object of a class-wide type, transform
         --  X'Size into a call to the primitive operation _Size applied to X.

         elsif Is_Class_Wide_Type (Ptyp) then
            New_Node :=
              Make_Function_Call (Loc,
                Name => New_Reference_To
                  (Find_Prim_Op (Ptyp, Name_uSize), Loc),
                Parameter_Associations => New_List (Pref));

            if Typ /= Standard_Long_Long_Integer then

               --  The context is a specific integer type with which the
               --  original attribute was compatible. The function has a
               --  specific type as well, so to preserve the compatibility
               --  we must convert explicitly.

               New_Node := Convert_To (Typ, New_Node);
            end if;

            Rewrite (N, New_Node);
            Analyze_And_Resolve (N, Typ);
            return;

         --  For an array component, we can do Size in the front end
         --  if the component_size of the array is set.

         elsif Nkind (Pref) = N_Indexed_Component then
            Siz := Component_Size (Etype (Prefix (Pref)));

         --  For a record component, we can do Size in the front end if there
         --  is a component clause, or if the record is packed and the
         --  component's size is known at compile time.

         elsif Nkind (Pref) = N_Selected_Component then
            declare
               Rec  : constant Entity_Id := Etype (Prefix (Pref));
               Comp : constant Entity_Id := Entity (Selector_Name (Pref));

            begin
               if Present (Component_Clause (Comp)) then
                  Siz := Esize (Comp);

               elsif Is_Packed (Rec) then
                  Siz := RM_Size (Ptyp);

               else
                  Apply_Universal_Integer_Attribute_Checks (N);
                  return;
               end if;
            end;

         --  All other cases are handled by Gigi

         else
            Apply_Universal_Integer_Attribute_Checks (N);

            --  If we have Size applied to a formal parameter, that is a
            --  packed array subtype, then apply size to the actual subtype.

            if Is_Entity_Name (Pref)
              and then Is_Formal (Entity (Pref))
              and then Is_Array_Type (Etype (Pref))
              and then Is_Packed (Etype (Pref))
            then
               Rewrite (N,
                 Make_Attribute_Reference (Loc,
                   Prefix =>
                     New_Occurrence_Of (Get_Actual_Subtype (Pref), Loc),
                   Attribute_Name => Name_Size));
               Analyze_And_Resolve (N, Typ);
            end if;

            return;
         end if;

         --  Common processing for record and array component case

         if Siz /= 0 then
            Rewrite (N, Make_Integer_Literal (Loc, Siz));

            Analyze_And_Resolve (N, Typ);

            --  The result is not a static expression

            Set_Is_Static_Expression (N, False);
         end if;
      end Size;

      ------------------
      -- Storage_Pool --
      ------------------

      when Attribute_Storage_Pool =>
         Rewrite (N,
           Make_Type_Conversion (Loc,
             Subtype_Mark => New_Reference_To (Etype (N), Loc),
             Expression   => New_Reference_To (Entity (N), Loc)));
         Analyze_And_Resolve (N, Typ);

      ------------------
      -- Storage_Size --
      ------------------

      when Attribute_Storage_Size => Storage_Size :
      declare
         Ptyp : constant Entity_Id := Etype (Pref);

      begin
         --  Access type case, always go to the root type

         --  The case of access types results in a value of zero for the case
         --  where no storage size attribute clause has been given. If a
         --  storage size has been given, then the attribute is converted
         --  to a reference to the variable used to hold this value.

         if Is_Access_Type (Ptyp) then
            if Present (Storage_Size_Variable (Root_Type (Ptyp))) then
               Rewrite (N,
                 Make_Attribute_Reference (Loc,
                   Prefix => New_Reference_To (Typ, Loc),
                   Attribute_Name => Name_Max,
                   Expressions => New_List (
                     Make_Integer_Literal (Loc, 0),
                     Convert_To (Typ,
                       New_Reference_To
                         (Storage_Size_Variable (Root_Type (Ptyp)), Loc)))));

            elsif Present (Associated_Storage_Pool (Root_Type (Ptyp))) then
               Rewrite (N,
                 OK_Convert_To (Typ,
                   Make_Function_Call (Loc,
                     Name =>
                       New_Reference_To
                        (Find_Prim_Op
                          (Etype (Associated_Storage_Pool (Root_Type (Ptyp))),
                           Attribute_Name (N)),
                         Loc),

                     Parameter_Associations => New_List (New_Reference_To (
                       Associated_Storage_Pool (Root_Type (Ptyp)), Loc)))));
            else
               Rewrite (N, Make_Integer_Literal (Loc, 0));
            end if;

            Analyze_And_Resolve (N, Typ);

         --  The case of a task type (an obsolescent feature) is handled the
         --  same way, seems as reasonable as anything, and it is what the
         --  ACVC tests (e.g. CD1009K) seem to expect.

         --  If there is no Storage_Size variable, then we return the default
         --  task stack size, otherwise, expand a Storage_Size attribute as
         --  follows:

         --  Typ (Adjust_Storage_Size (taskZ))

         --  except for the case of a task object which has a Storage_Size
         --  pragma:

         --  Typ (Adjust_Storage_Size (taskV!(name)._Size))

         else
            if not Present (Storage_Size_Variable (Ptyp)) then
               Rewrite (N,
                 Convert_To (Typ,
                   Make_Function_Call (Loc,
                     Name =>
                       New_Occurrence_Of (RTE (RE_Default_Stack_Size), Loc))));

            else
               if not (Is_Entity_Name (Pref) and then
                 Is_Task_Type (Entity (Pref))) and then
                   Chars (Last_Entity (Corresponding_Record_Type (Ptyp))) =
                     Name_uSize
               then
                  Rewrite (N,
                    Convert_To (Typ,
                      Make_Function_Call (Loc,
                        Name => New_Occurrence_Of (
                          RTE (RE_Adjust_Storage_Size), Loc),
                        Parameter_Associations =>
                          New_List (
                            Make_Selected_Component (Loc,
                              Prefix =>
                                Unchecked_Convert_To (
                                  Corresponding_Record_Type (Ptyp),
                                  New_Copy_Tree (Pref)),
                              Selector_Name =>
                                Make_Identifier (Loc, Name_uSize))))));

               --  Task not having Storage_Size pragma

               else
                  Rewrite (N,
                    Convert_To (Typ,
                      Make_Function_Call (Loc,
                        Name => New_Occurrence_Of (
                          RTE (RE_Adjust_Storage_Size), Loc),
                        Parameter_Associations =>
                          New_List (
                            New_Reference_To (
                              Storage_Size_Variable (Ptyp), Loc)))));
               end if;

               Analyze_And_Resolve (N, Typ);
            end if;
         end if;
      end Storage_Size;

      -----------------
      -- Stream_Size --
      -----------------

      when Attribute_Stream_Size => Stream_Size : declare
         Ptyp : constant Entity_Id := Etype (Pref);
         Size : Int;

      begin
         --  If we have a Stream_Size clause for this type use it, otherwise
         --  the Stream_Size if the size of the type.

         if Has_Stream_Size_Clause (Ptyp) then
            Size := UI_To_Int
              (Static_Integer (Expression (Stream_Size_Clause (Ptyp))));
         else
            Size := UI_To_Int (Esize (Ptyp));
         end if;

         Rewrite (N, Make_Integer_Literal (Loc, Intval => Size));
         Analyze_And_Resolve (N, Typ);
      end Stream_Size;

      ----------
      -- Succ --
      ----------

      --  1. Deal with enumeration types with holes
      --  2. For floating-point, generate call to attribute function
      --  3. For other cases, deal with constraint checking

      when Attribute_Succ => Succ :
      declare
         Ptyp : constant Entity_Id := Base_Type (Etype (Pref));

      begin
         --  For enumeration types with non-standard representations, we
         --  expand typ'Succ (x) into

         --    Pos_To_Rep (Rep_To_Pos (x) + 1)

         --    If the representation is contiguous, we compute instead
         --    Lit1 + Rep_to_Pos (x+1), to catch invalid representations.

         if Is_Enumeration_Type (Ptyp)
           and then Present (Enum_Pos_To_Rep (Ptyp))
         then
            if Has_Contiguous_Rep (Ptyp) then
               Rewrite (N,
                  Unchecked_Convert_To (Ptyp,
                     Make_Op_Add (Loc,
                        Left_Opnd  =>
                         Make_Integer_Literal (Loc,
                           Enumeration_Rep (First_Literal (Ptyp))),
                        Right_Opnd =>
                          Make_Function_Call (Loc,
                            Name =>
                              New_Reference_To
                               (TSS (Ptyp, TSS_Rep_To_Pos), Loc),

                            Parameter_Associations =>
                              New_List (
                                Unchecked_Convert_To (Ptyp,
                                  Make_Op_Add (Loc,
                                  Left_Opnd =>
                                    Unchecked_Convert_To (Standard_Integer,
                                      Relocate_Node (First (Exprs))),
                                  Right_Opnd =>
                                    Make_Integer_Literal (Loc, 1))),
                                Rep_To_Pos_Flag (Ptyp, Loc))))));
            else
               --  Add Boolean parameter True, to request program errror if
               --  we have a bad representation on our hands. Add False if
               --  checks are suppressed.

               Append_To (Exprs, Rep_To_Pos_Flag (Ptyp, Loc));
               Rewrite (N,
                 Make_Indexed_Component (Loc,
                   Prefix => New_Reference_To (Enum_Pos_To_Rep (Ptyp), Loc),
                   Expressions => New_List (
                     Make_Op_Add (Loc,
                       Left_Opnd =>
                         Make_Function_Call (Loc,
                           Name =>
                             New_Reference_To
                               (TSS (Ptyp, TSS_Rep_To_Pos), Loc),
                           Parameter_Associations => Exprs),
                       Right_Opnd => Make_Integer_Literal (Loc, 1)))));
            end if;

            Analyze_And_Resolve (N, Typ);

         --  For floating-point, we transform 'Succ into a call to the Succ
         --  floating-point attribute function in Fat_xxx (xxx is root type)

         elsif Is_Floating_Point_Type (Ptyp) then
            Expand_Fpt_Attribute_R (N);
            Analyze_And_Resolve (N, Typ);

         --  For modular types, nothing to do (no overflow, since wraps)

         elsif Is_Modular_Integer_Type (Ptyp) then
            null;

         --  For other types, if range checking is enabled, we must generate
         --  a check if overflow checking is enabled.

         elsif not Overflow_Checks_Suppressed (Ptyp) then
            Expand_Pred_Succ (N);
         end if;
      end Succ;

      ---------
      -- Tag --
      ---------

      --  Transforms X'Tag into a direct reference to the tag of X

      when Attribute_Tag => Tag :
      declare
         Ttyp           : Entity_Id;
         Prefix_Is_Type : Boolean;

      begin
         if Is_Entity_Name (Pref) and then Is_Type (Entity (Pref)) then
            Ttyp := Entity (Pref);
            Prefix_Is_Type := True;
         else
            Ttyp := Etype (Pref);
            Prefix_Is_Type := False;
         end if;

         if Is_Class_Wide_Type (Ttyp) then
            Ttyp := Root_Type (Ttyp);
         end if;

         Ttyp := Underlying_Type (Ttyp);

         if Prefix_Is_Type then

            --  For JGNAT we leave the type attribute unexpanded because
            --  there's not a dispatching table to reference.

            if not Java_VM then
               Rewrite (N,
                 Unchecked_Convert_To (RTE (RE_Tag),
                   New_Reference_To
                     (Node (First_Elmt (Access_Disp_Table (Ttyp))), Loc)));
               Analyze_And_Resolve (N, RTE (RE_Tag));
            end if;

         else
            Rewrite (N,
              Make_Selected_Component (Loc,
                Prefix => Relocate_Node (Pref),
                Selector_Name =>
                  New_Reference_To (First_Tag_Component (Ttyp), Loc)));
            Analyze_And_Resolve (N, RTE (RE_Tag));
         end if;
      end Tag;

      ----------------
      -- Terminated --
      ----------------

      --  Transforms 'Terminated attribute into a call to Terminated function

      when Attribute_Terminated => Terminated :
      begin
         if Restricted_Profile then
            Rewrite (N,
              Build_Call_With_Task (Pref, RTE (RE_Restricted_Terminated)));

         else
            Rewrite (N,
              Build_Call_With_Task (Pref, RTE (RE_Terminated)));
         end if;

         Analyze_And_Resolve (N, Standard_Boolean);
      end Terminated;

      ----------------
      -- To_Address --
      ----------------

      --  Transforms System'To_Address (X) into unchecked conversion
      --  from (integral) type of X to type address.

      when Attribute_To_Address =>
         Rewrite (N,
           Unchecked_Convert_To (RTE (RE_Address),
             Relocate_Node (First (Exprs))));
         Analyze_And_Resolve (N, RTE (RE_Address));

      ----------------
      -- Truncation --
      ----------------

      --  Transforms 'Truncation into a call to the floating-point attribute
      --  function Truncation in Fat_xxx (where xxx is the root type)

      when Attribute_Truncation =>
         Expand_Fpt_Attribute_R (N);

      -----------------------
      -- Unbiased_Rounding --
      -----------------------

      --  Transforms 'Unbiased_Rounding into a call to the floating-point
      --  attribute function Unbiased_Rounding in Fat_xxx (where xxx is the
      --  root type)

      when Attribute_Unbiased_Rounding =>
         Expand_Fpt_Attribute_R (N);

      ----------------------
      -- Unchecked_Access --
      ----------------------

      when Attribute_Unchecked_Access =>
         Expand_Access_To_Type (N);

      -----------------
      -- UET_Address --
      -----------------

      when Attribute_UET_Address => UET_Address : declare
         Ent : constant Entity_Id :=
                 Make_Defining_Identifier (Loc, New_Internal_Name ('T'));

      begin
         Insert_Action (N,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Ent,
             Aliased_Present     => True,
             Object_Definition   =>
               New_Occurrence_Of (RTE (RE_Address), Loc)));

         --  Construct name __gnat_xxx__SDP, where xxx is the unit name
         --  in normal external form.

         Get_External_Unit_Name_String (Get_Unit_Name (Pref));
         Name_Buffer (1 + 7 .. Name_Len + 7) := Name_Buffer (1 .. Name_Len);
         Name_Len := Name_Len + 7;
         Name_Buffer (1 .. 7) := "__gnat_";
         Name_Buffer (Name_Len + 1 .. Name_Len + 5) := "__SDP";
         Name_Len := Name_Len + 5;

         Set_Is_Imported (Ent);
         Set_Interface_Name (Ent,
           Make_String_Literal (Loc,
             Strval => String_From_Name_Buffer));

         Rewrite (N,
           Make_Attribute_Reference (Loc,
             Prefix => New_Occurrence_Of (Ent, Loc),
             Attribute_Name => Name_Address));

         Analyze_And_Resolve (N, Typ);
      end UET_Address;

      -------------------------
      -- Unrestricted_Access --
      -------------------------

      when Attribute_Unrestricted_Access =>
         Expand_Access_To_Type (N);

      ---------------
      -- VADS_Size --
      ---------------

      --  The processing for VADS_Size is shared with Size

      ---------
      -- Val --
      ---------

      --  For enumeration types with a standard representation, and for all
      --  other types, Val is handled by Gigi. For enumeration types with
      --  a non-standard representation we use the _Pos_To_Rep array that
      --  was created when the type was frozen.

      when Attribute_Val => Val :
      declare
         Etyp : constant Entity_Id := Base_Type (Entity (Pref));

      begin
         if Is_Enumeration_Type (Etyp)
           and then Present (Enum_Pos_To_Rep (Etyp))
         then
            if Has_Contiguous_Rep (Etyp) then
               declare
                  Rep_Node : constant Node_Id :=
                    Unchecked_Convert_To (Etyp,
                       Make_Op_Add (Loc,
                         Left_Opnd =>
                            Make_Integer_Literal (Loc,
                              Enumeration_Rep (First_Literal (Etyp))),
                         Right_Opnd =>
                          (Convert_To (Standard_Integer,
                             Relocate_Node (First (Exprs))))));

               begin
                  Rewrite (N,
                     Unchecked_Convert_To (Etyp,
                         Make_Op_Add (Loc,
                           Left_Opnd =>
                             Make_Integer_Literal (Loc,
                               Enumeration_Rep (First_Literal (Etyp))),
                           Right_Opnd =>
                             Make_Function_Call (Loc,
                               Name =>
                                 New_Reference_To
                                   (TSS (Etyp, TSS_Rep_To_Pos), Loc),
                               Parameter_Associations => New_List (
                                 Rep_Node,
                                 Rep_To_Pos_Flag (Etyp, Loc))))));
               end;

            else
               Rewrite (N,
                 Make_Indexed_Component (Loc,
                   Prefix => New_Reference_To (Enum_Pos_To_Rep (Etyp), Loc),
                   Expressions => New_List (
                     Convert_To (Standard_Integer,
                       Relocate_Node (First (Exprs))))));
            end if;

            Analyze_And_Resolve (N, Typ);
         end if;
      end Val;

      -----------
      -- Valid --
      -----------

      --  The code for valid is dependent on the particular types involved.
      --  See separate sections below for the generated code in each case.

      when Attribute_Valid => Valid :
      declare
         Ptyp : constant Entity_Id  := Etype (Pref);
         Btyp : Entity_Id           := Base_Type (Ptyp);
         Tst  : Node_Id;

         Save_Validity_Checks_On : constant Boolean := Validity_Checks_On;
         --  Save the validity checking mode. We always turn off validity
         --  checking during process of 'Valid since this is one place
         --  where we do not want the implicit validity checks to intefere
         --  with the explicit validity check that the programmer is doing.

         function Make_Range_Test return Node_Id;
         --  Build the code for a range test of the form
         --    Btyp!(Pref) >= Btyp!(Ptyp'First)
         --      and then
         --    Btyp!(Pref) <= Btyp!(Ptyp'Last)

         ---------------------
         -- Make_Range_Test --
         ---------------------

         function Make_Range_Test return Node_Id is
         begin
            return
              Make_And_Then (Loc,
                Left_Opnd =>
                  Make_Op_Ge (Loc,
                    Left_Opnd =>
                      Unchecked_Convert_To (Btyp, Duplicate_Subexpr (Pref)),

                    Right_Opnd =>
                      Unchecked_Convert_To (Btyp,
                        Make_Attribute_Reference (Loc,
                          Prefix => New_Occurrence_Of (Ptyp, Loc),
                          Attribute_Name => Name_First))),

                Right_Opnd =>
                  Make_Op_Le (Loc,
                    Left_Opnd =>
                      Unchecked_Convert_To (Btyp,
                        Duplicate_Subexpr_No_Checks (Pref)),

                    Right_Opnd =>
                      Unchecked_Convert_To (Btyp,
                        Make_Attribute_Reference (Loc,
                          Prefix => New_Occurrence_Of (Ptyp, Loc),
                          Attribute_Name => Name_Last))));
         end Make_Range_Test;

      --  Start of processing for Attribute_Valid

      begin
         --  Turn off validity checks. We do not want any implicit validity
         --  checks to intefere with the explicit check from the attribute

         Validity_Checks_On := False;

         --  Floating-point case. This case is handled by the Valid attribute
         --  code in the floating-point attribute run-time library.

         if Is_Floating_Point_Type (Ptyp) then
            declare
               Rtp : constant Entity_Id := Root_Type (Etype (Pref));

            begin
               --  If the floating-point object might be unaligned, we need
               --  to call the special routine Unaligned_Valid, which makes
               --  the needed copy, being careful not to load the value into
               --  any floating-point register. The argument in this case is
               --  obj'Address (see Unchecked_Valid routine in s-fatgen.ads).

               if Is_Possibly_Unaligned_Object (Pref) then
                  Set_Attribute_Name (N, Name_Unaligned_Valid);
                  Expand_Fpt_Attribute
                    (N, Rtp, Name_Unaligned_Valid,
                     New_List (
                       Make_Attribute_Reference (Loc,
                         Prefix         => Relocate_Node (Pref),
                         Attribute_Name => Name_Address)));

               --  In the normal case where we are sure the object is aligned,
               --  we generate a caqll to Valid, and the argument in this case
               --  is obj'Unrestricted_Access (after converting obj to the
               --  right floating-point type).

               else
                  Expand_Fpt_Attribute
                    (N, Rtp, Name_Valid,
                     New_List (
                       Make_Attribute_Reference (Loc,
                         Prefix         => Unchecked_Convert_To (Rtp, Pref),
                         Attribute_Name => Name_Unrestricted_Access)));
               end if;

               --  One more task, we still need a range check. Required
               --  only if we have a constraint, since the Valid routine
               --  catches infinities properly (infinities are never valid).

               --  The way we do the range check is simply to create the
               --  expression: Valid (N) and then Base_Type(Pref) in Typ.

               if not Subtypes_Statically_Match (Ptyp, Btyp) then
                  Rewrite (N,
                    Make_And_Then (Loc,
                      Left_Opnd  => Relocate_Node (N),
                      Right_Opnd =>
                        Make_In (Loc,
                          Left_Opnd => Convert_To (Btyp, Pref),
                          Right_Opnd => New_Occurrence_Of (Ptyp, Loc))));
               end if;
            end;

         --  Enumeration type with holes

         --  For enumeration types with holes, the Pos value constructed by
         --  the Enum_Rep_To_Pos function built in Exp_Ch3 called with a
         --  second argument of False returns minus one for an invalid value,
         --  and the non-negative pos value for a valid value, so the
         --  expansion of X'Valid is simply:

         --     type(X)'Pos (X) >= 0

         --  We can't quite generate it that way because of the requirement
         --  for the non-standard second argument of False in the resulting
         --  rep_to_pos call, so we have to explicitly create:

         --     _rep_to_pos (X, False) >= 0

         --  If we have an enumeration subtype, we also check that the
         --  value is in range:

         --    _rep_to_pos (X, False) >= 0
         --      and then
         --       (X >= type(X)'First and then type(X)'Last <= X)

         elsif Is_Enumeration_Type (Ptyp)
           and then Present (Enum_Pos_To_Rep (Base_Type (Ptyp)))
         then
            Tst :=
              Make_Op_Ge (Loc,
                Left_Opnd =>
                  Make_Function_Call (Loc,
                    Name =>
                      New_Reference_To
                        (TSS (Base_Type (Ptyp), TSS_Rep_To_Pos), Loc),
                    Parameter_Associations => New_List (
                      Pref,
                      New_Occurrence_Of (Standard_False, Loc))),
                Right_Opnd => Make_Integer_Literal (Loc, 0));

            if Ptyp /= Btyp
              and then
                (Type_Low_Bound (Ptyp) /= Type_Low_Bound (Btyp)
                  or else
                 Type_High_Bound (Ptyp) /= Type_High_Bound (Btyp))
            then
               --  The call to Make_Range_Test will create declarations
               --  that need a proper insertion point, but Pref is now
               --  attached to a node with no ancestor. Attach to tree
               --  even if it is to be rewritten below.

               Set_Parent (Tst, Parent (N));

               Tst :=
                 Make_And_Then (Loc,
                   Left_Opnd  => Make_Range_Test,
                   Right_Opnd => Tst);
            end if;

            Rewrite (N, Tst);

         --  Fortran convention booleans

         --  For the very special case of Fortran convention booleans, the
         --  value is always valid, since it is an integer with the semantics
         --  that non-zero is true, and any value is permissible.

         elsif Is_Boolean_Type (Ptyp)
           and then Convention (Ptyp) = Convention_Fortran
         then
            Rewrite (N, New_Occurrence_Of (Standard_True, Loc));

         --  For biased representations, we will be doing an unchecked
         --  conversion without unbiasing the result. That means that the range
         --  test has to take this into account, and the proper form of the
         --  test is:

         --    Btyp!(Pref) < Btyp!(Ptyp'Range_Length)

         elsif Has_Biased_Representation (Ptyp) then
            Btyp := RTE (RE_Unsigned_32);
            Rewrite (N,
              Make_Op_Lt (Loc,
                Left_Opnd =>
                  Unchecked_Convert_To (Btyp, Duplicate_Subexpr (Pref)),
                Right_Opnd =>
                  Unchecked_Convert_To (Btyp,
                    Make_Attribute_Reference (Loc,
                      Prefix => New_Occurrence_Of (Ptyp, Loc),
                      Attribute_Name => Name_Range_Length))));

         --  For all other scalar types, what we want logically is a
         --  range test:

         --     X in type(X)'First .. type(X)'Last

         --  But that's precisely what won't work because of possible
         --  unwanted optimization (and indeed the basic motivation for
         --  the Valid attribute is exactly that this test does not work!)
         --  What will work is:

         --     Btyp!(X) >= Btyp!(type(X)'First)
         --       and then
         --     Btyp!(X) <= Btyp!(type(X)'Last)

         --  where Btyp is an integer type large enough to cover the full
         --  range of possible stored values (i.e. it is chosen on the basis
         --  of the size of the type, not the range of the values). We write
         --  this as two tests, rather than a range check, so that static
         --  evaluation will easily remove either or both of the checks if
         --  they can be -statically determined to be true (this happens
         --  when the type of X is static and the range extends to the full
         --  range of stored values).

         --  Unsigned types. Note: it is safe to consider only whether the
         --  subtype is unsigned, since we will in that case be doing all
         --  unsigned comparisons based on the subtype range. Since we use the
         --  actual subtype object size, this is appropriate.

         --  For example, if we have

         --    subtype x is integer range 1 .. 200;
         --    for x'Object_Size use 8;

         --  Now the base type is signed, but objects of this type are bits
         --  unsigned, and doing an unsigned test of the range 1 to 200 is
         --  correct, even though a value greater than 127 looks signed to a
         --  signed comparison.

         elsif Is_Unsigned_Type (Ptyp) then
            if Esize (Ptyp) <= 32 then
               Btyp := RTE (RE_Unsigned_32);
            else
               Btyp := RTE (RE_Unsigned_64);
            end if;

            Rewrite (N, Make_Range_Test);

         --  Signed types

         else
            if Esize (Ptyp) <= Esize (Standard_Integer) then
               Btyp := Standard_Integer;
            else
               Btyp := Universal_Integer;
            end if;

            Rewrite (N, Make_Range_Test);
         end if;

         Analyze_And_Resolve (N, Standard_Boolean);
         Validity_Checks_On := Save_Validity_Checks_On;
      end Valid;

      -----------
      -- Value --
      -----------

      --  Value attribute is handled in separate unti Exp_Imgv

      when Attribute_Value =>
         Exp_Imgv.Expand_Value_Attribute (N);

      -----------------
      -- Value_Size --
      -----------------

      --  The processing for Value_Size shares the processing for Size

      -------------
      -- Version --
      -------------

      --  The processing for Version shares the processing for Body_Version

      ----------------
      -- Wide_Image --
      ----------------

      --  We expand typ'Wide_Image (X) into

      --    String_To_Wide_String
      --      (typ'Image (X), Wide_Character_Encoding_Method)

      --  This works in all cases because String_To_Wide_String converts any
      --  wide character escape sequences resulting from the Image call to the
      --  proper Wide_Character equivalent

      --  not quite right for typ = Wide_Character ???

      when Attribute_Wide_Image => Wide_Image :
      begin
         Rewrite (N,
           Make_Function_Call (Loc,
             Name => New_Reference_To (RTE (RE_String_To_Wide_String), Loc),
             Parameter_Associations => New_List (
               Make_Attribute_Reference (Loc,
                 Prefix         => Pref,
                 Attribute_Name => Name_Image,
                 Expressions    => Exprs),

               Make_Integer_Literal (Loc,
                 Intval => Int (Wide_Character_Encoding_Method)))));

         Analyze_And_Resolve (N, Standard_Wide_String);
      end Wide_Image;

      ---------------------
      -- Wide_Wide_Image --
      ---------------------

      --  We expand typ'Wide_Wide_Image (X) into

      --    String_To_Wide_Wide_String
      --      (typ'Image (X), Wide_Character_Encoding_Method)

      --  This works in all cases because String_To_Wide_Wide_String converts
      --  any wide character escape sequences resulting from the Image call to
      --  the proper Wide_Character equivalent

      --  not quite right for typ = Wide_Wide_Character ???

      when Attribute_Wide_Wide_Image => Wide_Wide_Image :
      begin
         Rewrite (N,
           Make_Function_Call (Loc,
             Name => New_Reference_To
               (RTE (RE_String_To_Wide_Wide_String), Loc),
             Parameter_Associations => New_List (
               Make_Attribute_Reference (Loc,
                 Prefix         => Pref,
                 Attribute_Name => Name_Image,
                 Expressions    => Exprs),

               Make_Integer_Literal (Loc,
                 Intval => Int (Wide_Character_Encoding_Method)))));

         Analyze_And_Resolve (N, Standard_Wide_Wide_String);
      end Wide_Wide_Image;

      ----------------
      -- Wide_Value --
      ----------------

      --  We expand typ'Wide_Value (X) into

      --    typ'Value
      --      (Wide_String_To_String (X, Wide_Character_Encoding_Method))

      --  Wide_String_To_String is a runtime function that converts its wide
      --  string argument to String, converting any non-translatable characters
      --  into appropriate escape sequences. This preserves the required
      --  semantics of Wide_Value in all cases, and results in a very simple
      --  implementation approach.

      --  It's not quite right where typ = Wide_Character, because the encoding
      --  method may not cover the whole character type ???

      when Attribute_Wide_Value => Wide_Value :
      begin
         Rewrite (N,
           Make_Attribute_Reference (Loc,
             Prefix         => Pref,
             Attribute_Name => Name_Value,

             Expressions    => New_List (
               Make_Function_Call (Loc,
                 Name =>
                   New_Reference_To (RTE (RE_Wide_String_To_String), Loc),

                 Parameter_Associations => New_List (
                   Relocate_Node (First (Exprs)),
                   Make_Integer_Literal (Loc,
                     Intval => Int (Wide_Character_Encoding_Method)))))));

         Analyze_And_Resolve (N, Typ);
      end Wide_Value;

      ---------------------
      -- Wide_Wide_Value --
      ---------------------

      --  We expand typ'Wide_Value_Value (X) into

      --    typ'Value
      --      (Wide_Wide_String_To_String (X, Wide_Character_Encoding_Method))

      --  Wide_Wide_String_To_String is a runtime function that converts its
      --  wide string argument to String, converting any non-translatable
      --  characters into appropriate escape sequences. This preserves the
      --  required semantics of Wide_Wide_Value in all cases, and results in a
      --  very simple implementation approach.

      --  It's not quite right where typ = Wide_Wide_Character, because the
      --  encoding method may not cover the whole character type ???

      when Attribute_Wide_Wide_Value => Wide_Wide_Value :
      begin
         Rewrite (N,
           Make_Attribute_Reference (Loc,
             Prefix         => Pref,
             Attribute_Name => Name_Value,

             Expressions    => New_List (
               Make_Function_Call (Loc,
                 Name =>
                   New_Reference_To (RTE (RE_Wide_Wide_String_To_String), Loc),

                 Parameter_Associations => New_List (
                   Relocate_Node (First (Exprs)),
                   Make_Integer_Literal (Loc,
                     Intval => Int (Wide_Character_Encoding_Method)))))));

         Analyze_And_Resolve (N, Typ);
      end Wide_Wide_Value;

      ---------------------
      -- Wide_Wide_Width --
      ---------------------

      --  Wide_Wide_Width attribute is handled in separate unit Exp_Imgv

      when Attribute_Wide_Wide_Width =>
         Exp_Imgv.Expand_Width_Attribute (N, Wide_Wide);

      ----------------
      -- Wide_Width --
      ----------------

      --  Wide_Width attribute is handled in separate unit Exp_Imgv

      when Attribute_Wide_Width =>
         Exp_Imgv.Expand_Width_Attribute (N, Wide);

      -----------
      -- Width --
      -----------

      --  Width attribute is handled in separate unit Exp_Imgv

      when Attribute_Width =>
         Exp_Imgv.Expand_Width_Attribute (N, Normal);

      -----------
      -- Write --
      -----------

      when Attribute_Write => Write : declare
         P_Type : constant Entity_Id := Entity (Pref);
         U_Type : constant Entity_Id := Underlying_Type (P_Type);
         Pname  : Entity_Id;
         Decl   : Node_Id;
         Prag   : Node_Id;
         Arg3   : Node_Id;
         Wfunc  : Node_Id;

      begin
         --  If no underlying type, we have an error that will be diagnosed
         --  elsewhere, so here we just completely ignore the expansion.

         if No (U_Type) then
            return;
         end if;

         --  The simple case, if there is a TSS for Write, just call it

         Pname := Find_Stream_Subprogram (P_Type, TSS_Stream_Write);

         if Present (Pname) then
            null;

         else
            --  If there is a Stream_Convert pragma, use it, we rewrite

            --     sourcetyp'Output (stream, Item)

            --  as

            --     strmtyp'Output (Stream, strmwrite (acttyp (Item)));

            --  where strmwrite is the given Write function that converts an
            --  argument of type sourcetyp or a type acctyp, from which it is
            --  derived to type strmtyp. The conversion to acttyp is required
            --  for the derived case.

            Prag := Get_Stream_Convert_Pragma (P_Type);

            if Present (Prag) then
               Arg3 :=
                 Next (Next (First (Pragma_Argument_Associations (Prag))));
               Wfunc := Entity (Expression (Arg3));

               Rewrite (N,
                 Make_Attribute_Reference (Loc,
                   Prefix => New_Occurrence_Of (Etype (Wfunc), Loc),
                   Attribute_Name => Name_Output,
                   Expressions => New_List (
                     Relocate_Node (First (Exprs)),
                     Make_Function_Call (Loc,
                       Name => New_Occurrence_Of (Wfunc, Loc),
                       Parameter_Associations => New_List (
                         Convert_To (Etype (First_Formal (Wfunc)),
                           Relocate_Node (Next (First (Exprs)))))))));

               Analyze (N);
               return;

            --  For elementary types, we call the W_xxx routine directly

            elsif Is_Elementary_Type (U_Type) then
               Rewrite (N, Build_Elementary_Write_Call (N));
               Analyze (N);
               return;

            --  Array type case

            elsif Is_Array_Type (U_Type) then
               Build_Array_Write_Procedure (N, U_Type, Decl, Pname);
               Compile_Stream_Body_In_Scope (N, Decl, U_Type, Check => False);

            --  Tagged type case, use the primitive Write function. Note that
            --  this will dispatch in the class-wide case which is what we want

            elsif Is_Tagged_Type (U_Type) then
               Pname := Find_Prim_Op (U_Type, TSS_Stream_Write);

            --  All other record type cases, including protected records.
            --  The latter only arise for expander generated code for
            --  handling shared passive partition access.

            else
               pragma Assert
                 (Is_Record_Type (U_Type) or else Is_Protected_Type (U_Type));

               --  Ada 2005 (AI-216): Program_Error is raised when executing
               --  the default implementation of the Write attribute of an
               --  Unchecked_Union type.

               if Is_Unchecked_Union (Base_Type (U_Type)) then
                  Insert_Action (N,
                    Make_Raise_Program_Error (Loc,
                      Reason => PE_Unchecked_Union_Restriction));
               end if;

               if Has_Discriminants (U_Type)
                 and then Present
                   (Discriminant_Default_Value (First_Discriminant (U_Type)))
               then
                  Build_Mutable_Record_Write_Procedure
                    (Loc, Base_Type (U_Type), Decl, Pname);
               else
                  Build_Record_Write_Procedure
                    (Loc, Base_Type (U_Type), Decl, Pname);
               end if;

               Insert_Action (N, Decl);
            end if;
         end if;

         --  If we fall through, Pname is the procedure to be called

         Rewrite_Stream_Proc_Call (Pname);
      end Write;

      --  Component_Size is handled by Gigi, unless the component size is known
      --  at compile time, which is always true in the packed array case. It is
      --  important that the packed array case is handled in the front end (see
      --  Eval_Attribute) since Gigi would otherwise get confused by the
      --  equivalent packed array type.

      when Attribute_Component_Size =>
         null;

      --  The following attributes are handled by Gigi (except that static
      --  cases have already been evaluated by the semantics, but in any case
      --  Gigi should not count on that).

      --  In addition Gigi handles the non-floating-point cases of Pred and
      --  Succ (including the fixed-point cases, which can just be treated as
      --  integer increment/decrement operations)

      --  Gigi also handles the non-class-wide cases of Size

      when Attribute_Bit_Order                    |
           Attribute_Code_Address                 |
           Attribute_Definite                     |
           Attribute_Max                          |
           Attribute_Mechanism_Code               |
           Attribute_Min                          |
           Attribute_Null_Parameter               |
           Attribute_Passed_By_Reference          |
           Attribute_Pool_Address                 =>
         null;

      --  The following attributes are also handled by Gigi, but return a
      --  universal integer result, so may need a conversion for checking
      --  that the result is in range.

      when Attribute_Aft                          |
           Attribute_Bit                          |
           Attribute_Max_Size_In_Storage_Elements
      =>
         Apply_Universal_Integer_Attribute_Checks (N);

      --  The following attributes should not appear at this stage, since they
      --  have already been handled by the analyzer (and properly rewritten
      --  with corresponding values or entities to represent the right values)

      when Attribute_Abort_Signal                 |
           Attribute_Address_Size                 |
           Attribute_Base                         |
           Attribute_Class                        |
           Attribute_Default_Bit_Order            |
           Attribute_Delta                        |
           Attribute_Denorm                       |
           Attribute_Digits                       |
           Attribute_Emax                         |
           Attribute_Epsilon                      |
           Attribute_Has_Access_Values            |
           Attribute_Has_Discriminants            |
           Attribute_Large                        |
           Attribute_Machine_Emax                 |
           Attribute_Machine_Emin                 |
           Attribute_Machine_Mantissa             |
           Attribute_Machine_Overflows            |
           Attribute_Machine_Radix                |
           Attribute_Machine_Rounds               |
           Attribute_Maximum_Alignment            |
           Attribute_Model_Emin                   |
           Attribute_Model_Epsilon                |
           Attribute_Model_Mantissa               |
           Attribute_Model_Small                  |
           Attribute_Modulus                      |
           Attribute_Partition_ID                 |
           Attribute_Range                        |
           Attribute_Safe_Emax                    |
           Attribute_Safe_First                   |
           Attribute_Safe_Large                   |
           Attribute_Safe_Last                    |
           Attribute_Safe_Small                   |
           Attribute_Scale                        |
           Attribute_Signed_Zeros                 |
           Attribute_Small                        |
           Attribute_Storage_Unit                 |
           Attribute_Target_Name                  |
           Attribute_Type_Class                   |
           Attribute_Unconstrained_Array          |
           Attribute_Universal_Literal_String     |
           Attribute_Wchar_T_Size                 |
           Attribute_Word_Size                    =>

         raise Program_Error;

      --  The Asm_Input and Asm_Output attributes are not expanded at this
      --  stage, but will be eliminated in the expansion of the Asm call,
      --  see Exp_Intr for details. So Gigi will never see these either.

      when Attribute_Asm_Input                    |
           Attribute_Asm_Output                   =>

         null;

      end case;

   exception
      when RE_Not_Available =>
         return;
   end Expand_N_Attribute_Reference;

   ----------------------
   -- Expand_Pred_Succ --
   ----------------------

   --  For typ'Pred (exp), we generate the check

   --    [constraint_error when exp = typ'Base'First]

   --  Similarly, for typ'Succ (exp), we generate the check

   --    [constraint_error when exp = typ'Base'Last]

   --  These checks are not generated for modular types, since the proper
   --  semantics for Succ and Pred on modular types is to wrap, not raise CE.

   procedure Expand_Pred_Succ (N : Node_Id) is
      Loc  : constant Source_Ptr := Sloc (N);
      Cnam : Name_Id;

   begin
      if Attribute_Name (N) = Name_Pred then
         Cnam := Name_First;
      else
         Cnam := Name_Last;
      end if;

      Insert_Action (N,
        Make_Raise_Constraint_Error (Loc,
          Condition =>
            Make_Op_Eq (Loc,
              Left_Opnd =>
                Duplicate_Subexpr_Move_Checks (First (Expressions (N))),
              Right_Opnd =>
                Make_Attribute_Reference (Loc,
                  Prefix =>
                    New_Reference_To (Base_Type (Etype (Prefix (N))), Loc),
                  Attribute_Name => Cnam)),
          Reason => CE_Overflow_Check_Failed));
   end Expand_Pred_Succ;

   ----------------------------
   -- Find_Stream_Subprogram --
   ----------------------------

   function Find_Stream_Subprogram
     (Typ : Entity_Id;
      Nam : TSS_Name_Type) return Entity_Id
   is
      Ent : constant Entity_Id := TSS (Typ, Nam);
   begin
      if Present (Ent) then
         return Ent;
      end if;

      if Is_Tagged_Type (Typ)
        and then Is_Derived_Type (Typ)
      then
         return Find_Prim_Op (Typ, Nam);
      else
         return Find_Inherited_TSS (Typ, Nam);
      end if;
   end Find_Stream_Subprogram;

   -----------------------
   -- Get_Index_Subtype --
   -----------------------

   function Get_Index_Subtype (N : Node_Id) return Node_Id is
      P_Type : Entity_Id := Etype (Prefix (N));
      Indx   : Node_Id;
      J      : Int;

   begin
      if Is_Access_Type (P_Type) then
         P_Type := Designated_Type (P_Type);
      end if;

      if No (Expressions (N)) then
         J := 1;
      else
         J := UI_To_Int (Expr_Value (First (Expressions (N))));
      end if;

      Indx := First_Index (P_Type);
      while J > 1 loop
         Next_Index (Indx);
         J := J - 1;
      end loop;

      return Etype (Indx);
   end Get_Index_Subtype;

   -------------------------------
   -- Get_Stream_Convert_Pragma --
   -------------------------------

   function Get_Stream_Convert_Pragma (T : Entity_Id) return Node_Id is
      Typ : Entity_Id;
      N   : Node_Id;

   begin
      --  Note: we cannot use Get_Rep_Pragma here because of the peculiarity
      --  that a stream convert pragma for a tagged type is not inherited from
      --  its parent. Probably what is wrong here is that it is basically
      --  incorrect to consider a stream convert pragma to be a representation
      --  pragma at all ???

      N := First_Rep_Item (Implementation_Base_Type (T));
      while Present (N) loop
         if Nkind (N) = N_Pragma and then Chars (N) = Name_Stream_Convert then

            --  For tagged types this pragma is not inherited, so we
            --  must verify that it is defined for the given type and
            --  not an ancestor.

            Typ :=
              Entity (Expression (First (Pragma_Argument_Associations (N))));

            if not Is_Tagged_Type (T)
              or else T = Typ
              or else (Is_Private_Type (Typ) and then T = Full_View (Typ))
            then
               return N;
            end if;
         end if;

         Next_Rep_Item (N);
      end loop;

      return Empty;
   end Get_Stream_Convert_Pragma;

   ---------------------------------
   -- Is_Constrained_Packed_Array --
   ---------------------------------

   function Is_Constrained_Packed_Array (Typ : Entity_Id) return Boolean is
      Arr : Entity_Id := Typ;

   begin
      if Is_Access_Type (Arr) then
         Arr := Designated_Type (Arr);
      end if;

      return Is_Array_Type (Arr)
        and then Is_Constrained (Arr)
        and then Present (Packed_Array_Type (Arr));
   end Is_Constrained_Packed_Array;

end Exp_Attr;
