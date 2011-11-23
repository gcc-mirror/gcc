------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E X P _ C H 5                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2011, Free Software Foundation, Inc.         --
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

with Aspects;  use Aspects;
with Atree;    use Atree;
with Checks;   use Checks;
with Debug;    use Debug;
with Einfo;    use Einfo;
with Exp_Aggr; use Exp_Aggr;
with Exp_Ch6;  use Exp_Ch6;
with Exp_Ch7;  use Exp_Ch7;
with Exp_Ch11; use Exp_Ch11;
with Exp_Dbug; use Exp_Dbug;
with Exp_Pakd; use Exp_Pakd;
with Exp_Tss;  use Exp_Tss;
with Exp_Util; use Exp_Util;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Restrict; use Restrict;
with Rident;   use Rident;
with Rtsfind;  use Rtsfind;
with Sinfo;    use Sinfo;
with Sem;      use Sem;
with Sem_Aux;  use Sem_Aux;
with Sem_Ch3;  use Sem_Ch3;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Ch13; use Sem_Ch13;
with Sem_Eval; use Sem_Eval;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Snames;   use Snames;
with Stand;    use Stand;
with Stringt;  use Stringt;
with Targparm; use Targparm;
with Tbuild;   use Tbuild;
with Validsw;  use Validsw;

package body Exp_Ch5 is

   function Change_Of_Representation (N : Node_Id) return Boolean;
   --  Determine if the right hand side of assignment N is a type conversion
   --  which requires a change of representation. Called only for the array
   --  and record cases.

   procedure Expand_Assign_Array (N : Node_Id; Rhs : Node_Id);
   --  N is an assignment which assigns an array value. This routine process
   --  the various special cases and checks required for such assignments,
   --  including change of representation. Rhs is normally simply the right
   --  hand side of the assignment, except that if the right hand side is a
   --  type conversion or a qualified expression, then the RHS is the actual
   --  expression inside any such type conversions or qualifications.

   function Expand_Assign_Array_Loop
     (N      : Node_Id;
      Larray : Entity_Id;
      Rarray : Entity_Id;
      L_Type : Entity_Id;
      R_Type : Entity_Id;
      Ndim   : Pos;
      Rev    : Boolean) return Node_Id;
   --  N is an assignment statement which assigns an array value. This routine
   --  expands the assignment into a loop (or nested loops for the case of a
   --  multi-dimensional array) to do the assignment component by component.
   --  Larray and Rarray are the entities of the actual arrays on the left
   --  hand and right hand sides. L_Type and R_Type are the types of these
   --  arrays (which may not be the same, due to either sliding, or to a
   --  change of representation case). Ndim is the number of dimensions and
   --  the parameter Rev indicates if the loops run normally (Rev = False),
   --  or reversed (Rev = True). The value returned is the constructed
   --  loop statement. Auxiliary declarations are inserted before node N
   --  using the standard Insert_Actions mechanism.

   procedure Expand_Assign_Record (N : Node_Id);
   --  N is an assignment of a non-tagged record value. This routine handles
   --  the case where the assignment must be made component by component,
   --  either because the target is not byte aligned, or there is a change
   --  of representation, or when we have a tagged type with a representation
   --  clause (this last case is required because holes in the tagged type
   --  might be filled with components from child types).

   procedure Expand_Iterator_Loop (N : Node_Id);
   --  Expand loop over arrays and containers that uses the form "for X of C"
   --  with an optional subtype mark, or "for Y in C".

   procedure Expand_Predicated_Loop (N : Node_Id);
   --  Expand for loop over predicated subtype

   function Make_Tag_Ctrl_Assignment (N : Node_Id) return List_Id;
   --  Generate the necessary code for controlled and tagged assignment, that
   --  is to say, finalization of the target before, adjustment of the target
   --  after and save and restore of the tag and finalization pointers which
   --  are not 'part of the value' and must not be changed upon assignment. N
   --  is the original Assignment node.

   ------------------------------
   -- Change_Of_Representation --
   ------------------------------

   function Change_Of_Representation (N : Node_Id) return Boolean is
      Rhs : constant Node_Id := Expression (N);
   begin
      return
        Nkind (Rhs) = N_Type_Conversion
          and then
            not Same_Representation (Etype (Rhs), Etype (Expression (Rhs)));
   end Change_Of_Representation;

   -------------------------
   -- Expand_Assign_Array --
   -------------------------

   --  There are two issues here. First, do we let Gigi do a block move, or
   --  do we expand out into a loop? Second, we need to set the two flags
   --  Forwards_OK and Backwards_OK which show whether the block move (or
   --  corresponding loops) can be legitimately done in a forwards (low to
   --  high) or backwards (high to low) manner.

   procedure Expand_Assign_Array (N : Node_Id; Rhs : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);

      Lhs : constant Node_Id := Name (N);

      Act_Lhs : constant Node_Id := Get_Referenced_Object (Lhs);
      Act_Rhs : Node_Id          := Get_Referenced_Object (Rhs);

      L_Type : constant Entity_Id :=
                 Underlying_Type (Get_Actual_Subtype (Act_Lhs));
      R_Type : Entity_Id :=
                 Underlying_Type (Get_Actual_Subtype (Act_Rhs));

      L_Slice : constant Boolean := Nkind (Act_Lhs) = N_Slice;
      R_Slice : constant Boolean := Nkind (Act_Rhs) = N_Slice;

      Crep : constant Boolean := Change_Of_Representation (N);

      Larray  : Node_Id;
      Rarray  : Node_Id;

      Ndim : constant Pos := Number_Dimensions (L_Type);

      Loop_Required : Boolean := False;
      --  This switch is set to True if the array move must be done using
      --  an explicit front end generated loop.

      procedure Apply_Dereference (Arg : Node_Id);
      --  If the argument is an access to an array, and the assignment is
      --  converted into a procedure call, apply explicit dereference.

      function Has_Address_Clause (Exp : Node_Id) return Boolean;
      --  Test if Exp is a reference to an array whose declaration has
      --  an address clause, or it is a slice of such an array.

      function Is_Formal_Array (Exp : Node_Id) return Boolean;
      --  Test if Exp is a reference to an array which is either a formal
      --  parameter or a slice of a formal parameter. These are the cases
      --  where hidden aliasing can occur.

      function Is_Non_Local_Array (Exp : Node_Id) return Boolean;
      --  Determine if Exp is a reference to an array variable which is other
      --  than an object defined in the current scope, or a slice of such
      --  an object. Such objects can be aliased to parameters (unlike local
      --  array references).

      -----------------------
      -- Apply_Dereference --
      -----------------------

      procedure Apply_Dereference (Arg : Node_Id) is
         Typ : constant Entity_Id := Etype (Arg);
      begin
         if Is_Access_Type (Typ) then
            Rewrite (Arg, Make_Explicit_Dereference (Loc,
              Prefix => Relocate_Node (Arg)));
            Analyze_And_Resolve (Arg, Designated_Type (Typ));
         end if;
      end Apply_Dereference;

      ------------------------
      -- Has_Address_Clause --
      ------------------------

      function Has_Address_Clause (Exp : Node_Id) return Boolean is
      begin
         return
           (Is_Entity_Name (Exp) and then
                              Present (Address_Clause (Entity (Exp))))
             or else
           (Nkind (Exp) = N_Slice and then Has_Address_Clause (Prefix (Exp)));
      end Has_Address_Clause;

      ---------------------
      -- Is_Formal_Array --
      ---------------------

      function Is_Formal_Array (Exp : Node_Id) return Boolean is
      begin
         return
           (Is_Entity_Name (Exp) and then Is_Formal (Entity (Exp)))
             or else
           (Nkind (Exp) = N_Slice and then Is_Formal_Array (Prefix (Exp)));
      end Is_Formal_Array;

      ------------------------
      -- Is_Non_Local_Array --
      ------------------------

      function Is_Non_Local_Array (Exp : Node_Id) return Boolean is
      begin
         return (Is_Entity_Name (Exp)
                   and then Scope (Entity (Exp)) /= Current_Scope)
            or else (Nkind (Exp) = N_Slice
                       and then Is_Non_Local_Array (Prefix (Exp)));
      end Is_Non_Local_Array;

      --  Determine if Lhs, Rhs are formal arrays or nonlocal arrays

      Lhs_Formal : constant Boolean := Is_Formal_Array (Act_Lhs);
      Rhs_Formal : constant Boolean := Is_Formal_Array (Act_Rhs);

      Lhs_Non_Local_Var : constant Boolean := Is_Non_Local_Array (Act_Lhs);
      Rhs_Non_Local_Var : constant Boolean := Is_Non_Local_Array (Act_Rhs);

   --  Start of processing for Expand_Assign_Array

   begin
      --  Deal with length check. Note that the length check is done with
      --  respect to the right hand side as given, not a possible underlying
      --  renamed object, since this would generate incorrect extra checks.

      Apply_Length_Check (Rhs, L_Type);

      --  We start by assuming that the move can be done in either direction,
      --  i.e. that the two sides are completely disjoint.

      Set_Forwards_OK  (N, True);
      Set_Backwards_OK (N, True);

      --  Normally it is only the slice case that can lead to overlap, and
      --  explicit checks for slices are made below. But there is one case
      --  where the slice can be implicit and invisible to us: when we have a
      --  one dimensional array, and either both operands are parameters, or
      --  one is a parameter (which can be a slice passed by reference) and the
      --  other is a non-local variable. In this case the parameter could be a
      --  slice that overlaps with the other operand.

      --  However, if the array subtype is a constrained first subtype in the
      --  parameter case, then we don't have to worry about overlap, since
      --  slice assignments aren't possible (other than for a slice denoting
      --  the whole array).

      --  Note: No overlap is possible if there is a change of representation,
      --  so we can exclude this case.

      if Ndim = 1
        and then not Crep
        and then
           ((Lhs_Formal and Rhs_Formal)
              or else
            (Lhs_Formal and Rhs_Non_Local_Var)
              or else
            (Rhs_Formal and Lhs_Non_Local_Var))
        and then
           (not Is_Constrained (Etype (Lhs))
             or else not Is_First_Subtype (Etype (Lhs)))

         --  In the case of compiling for the Java or .NET Virtual Machine,
         --  slices are always passed by making a copy, so we don't have to
         --  worry about overlap. We also want to prevent generation of "<"
         --  comparisons for array addresses, since that's a meaningless
         --  operation on the VM.

        and then VM_Target = No_VM
      then
         Set_Forwards_OK  (N, False);
         Set_Backwards_OK (N, False);

         --  Note: the bit-packed case is not worrisome here, since if we have
         --  a slice passed as a parameter, it is always aligned on a byte
         --  boundary, and if there are no explicit slices, the assignment
         --  can be performed directly.
      end if;

      --  If either operand has an address clause clear Backwards_OK and
      --  Forwards_OK, since we cannot tell if the operands overlap. We
      --  exclude this treatment when Rhs is an aggregate, since we know
      --  that overlap can't occur.

      if (Has_Address_Clause (Lhs) and then Nkind (Rhs) /= N_Aggregate)
        or else Has_Address_Clause (Rhs)
      then
         Set_Forwards_OK  (N, False);
         Set_Backwards_OK (N, False);
      end if;

      --  We certainly must use a loop for change of representation and also
      --  we use the operand of the conversion on the right hand side as the
      --  effective right hand side (the component types must match in this
      --  situation).

      if Crep then
         Act_Rhs := Get_Referenced_Object (Rhs);
         R_Type  := Get_Actual_Subtype (Act_Rhs);
         Loop_Required := True;

      --  We require a loop if the left side is possibly bit unaligned

      elsif Possible_Bit_Aligned_Component (Lhs)
              or else
            Possible_Bit_Aligned_Component (Rhs)
      then
         Loop_Required := True;

      --  Arrays with controlled components are expanded into a loop to force
      --  calls to Adjust at the component level.

      elsif Has_Controlled_Component (L_Type) then
         Loop_Required := True;

         --  If object is atomic, we cannot tolerate a loop

      elsif Is_Atomic_Object (Act_Lhs)
              or else
            Is_Atomic_Object (Act_Rhs)
      then
         return;

      --  Loop is required if we have atomic components since we have to
      --  be sure to do any accesses on an element by element basis.

      elsif Has_Atomic_Components (L_Type)
        or else Has_Atomic_Components (R_Type)
        or else Is_Atomic (Component_Type (L_Type))
        or else Is_Atomic (Component_Type (R_Type))
      then
         Loop_Required := True;

      --  Case where no slice is involved

      elsif not L_Slice and not R_Slice then

         --  The following code deals with the case of unconstrained bit packed
         --  arrays. The problem is that the template for such arrays contains
         --  the bounds of the actual source level array, but the copy of an
         --  entire array requires the bounds of the underlying array. It would
         --  be nice if the back end could take care of this, but right now it
         --  does not know how, so if we have such a type, then we expand out
         --  into a loop, which is inefficient but works correctly. If we don't
         --  do this, we get the wrong length computed for the array to be
         --  moved. The two cases we need to worry about are:

         --  Explicit dereference of an unconstrained packed array type as in
         --  the following example:

         --    procedure C52 is
         --       type BITS is array(INTEGER range <>) of BOOLEAN;
         --       pragma PACK(BITS);
         --       type A is access BITS;
         --       P1,P2 : A;
         --    begin
         --       P1 := new BITS (1 .. 65_535);
         --       P2 := new BITS (1 .. 65_535);
         --       P2.ALL := P1.ALL;
         --    end C52;

         --  A formal parameter reference with an unconstrained bit array type
         --  is the other case we need to worry about (here we assume the same
         --  BITS type declared above):

         --    procedure Write_All (File : out BITS; Contents : BITS);
         --    begin
         --       File.Storage := Contents;
         --    end Write_All;

         --  We expand to a loop in either of these two cases

         --  Question for future thought. Another potentially more efficient
         --  approach would be to create the actual subtype, and then do an
         --  unchecked conversion to this actual subtype ???

         Check_Unconstrained_Bit_Packed_Array : declare

            function Is_UBPA_Reference (Opnd : Node_Id) return Boolean;
            --  Function to perform required test for the first case, above
            --  (dereference of an unconstrained bit packed array).

            -----------------------
            -- Is_UBPA_Reference --
            -----------------------

            function Is_UBPA_Reference (Opnd : Node_Id) return Boolean is
               Typ      : constant Entity_Id := Underlying_Type (Etype (Opnd));
               P_Type   : Entity_Id;
               Des_Type : Entity_Id;

            begin
               if Present (Packed_Array_Type (Typ))
                 and then Is_Array_Type (Packed_Array_Type (Typ))
                 and then not Is_Constrained (Packed_Array_Type (Typ))
               then
                  return True;

               elsif Nkind (Opnd) = N_Explicit_Dereference then
                  P_Type := Underlying_Type (Etype (Prefix (Opnd)));

                  if not Is_Access_Type (P_Type) then
                     return False;

                  else
                     Des_Type := Designated_Type (P_Type);
                     return
                       Is_Bit_Packed_Array (Des_Type)
                         and then not Is_Constrained (Des_Type);
                  end if;

               else
                  return False;
               end if;
            end Is_UBPA_Reference;

         --  Start of processing for Check_Unconstrained_Bit_Packed_Array

         begin
            if Is_UBPA_Reference (Lhs)
                 or else
               Is_UBPA_Reference (Rhs)
            then
               Loop_Required := True;

            --  Here if we do not have the case of a reference to a bit packed
            --  unconstrained array case. In this case gigi can most certainly
            --  handle the assignment if a forwards move is allowed.

            --  (could it handle the backwards case also???)

            elsif Forwards_OK (N) then
               return;
            end if;
         end Check_Unconstrained_Bit_Packed_Array;

      --  The back end can always handle the assignment if the right side is a
      --  string literal (note that overlap is definitely impossible in this
      --  case). If the type is packed, a string literal is always converted
      --  into an aggregate, except in the case of a null slice, for which no
      --  aggregate can be written. In that case, rewrite the assignment as a
      --  null statement, a length check has already been emitted to verify
      --  that the range of the left-hand side is empty.

      --  Note that this code is not executed if we have an assignment of a
      --  string literal to a non-bit aligned component of a record, a case
      --  which cannot be handled by the backend.

      elsif Nkind (Rhs) = N_String_Literal then
         if String_Length (Strval (Rhs)) = 0
           and then Is_Bit_Packed_Array (L_Type)
         then
            Rewrite (N, Make_Null_Statement (Loc));
            Analyze (N);
         end if;

         return;

      --  If either operand is bit packed, then we need a loop, since we can't
      --  be sure that the slice is byte aligned. Similarly, if either operand
      --  is a possibly unaligned slice, then we need a loop (since the back
      --  end cannot handle unaligned slices).

      elsif Is_Bit_Packed_Array (L_Type)
        or else Is_Bit_Packed_Array (R_Type)
        or else Is_Possibly_Unaligned_Slice (Lhs)
        or else Is_Possibly_Unaligned_Slice (Rhs)
      then
         Loop_Required := True;

      --  If we are not bit-packed, and we have only one slice, then no overlap
      --  is possible except in the parameter case, so we can let the back end
      --  handle things.

      elsif not (L_Slice and R_Slice) then
         if Forwards_OK (N) then
            return;
         end if;
      end if;

      --  If the right-hand side is a string literal, introduce a temporary for
      --  it, for use in the generated loop that will follow.

      if Nkind (Rhs) = N_String_Literal then
         declare
            Temp : constant Entity_Id := Make_Temporary (Loc, 'T', Rhs);
            Decl : Node_Id;

         begin
            Decl :=
              Make_Object_Declaration (Loc,
                 Defining_Identifier => Temp,
                 Object_Definition => New_Occurrence_Of (L_Type, Loc),
                 Expression => Relocate_Node (Rhs));

            Insert_Action (N, Decl);
            Rewrite (Rhs, New_Occurrence_Of (Temp, Loc));
            R_Type := Etype (Temp);
         end;
      end if;

      --  Come here to complete the analysis

      --    Loop_Required: Set to True if we know that a loop is required
      --                   regardless of overlap considerations.

      --    Forwards_OK:   Set to False if we already know that a forwards
      --                   move is not safe, else set to True.

      --    Backwards_OK:  Set to False if we already know that a backwards
      --                   move is not safe, else set to True

      --  Our task at this stage is to complete the overlap analysis, which can
      --  result in possibly setting Forwards_OK or Backwards_OK to False, and
      --  then generating the final code, either by deciding that it is OK
      --  after all to let Gigi handle it, or by generating appropriate code
      --  in the front end.

      declare
         L_Index_Typ : constant Node_Id := Etype (First_Index (L_Type));
         R_Index_Typ : constant Node_Id := Etype (First_Index (R_Type));

         Left_Lo  : constant Node_Id := Type_Low_Bound  (L_Index_Typ);
         Left_Hi  : constant Node_Id := Type_High_Bound (L_Index_Typ);
         Right_Lo : constant Node_Id := Type_Low_Bound  (R_Index_Typ);
         Right_Hi : constant Node_Id := Type_High_Bound (R_Index_Typ);

         Act_L_Array : Node_Id;
         Act_R_Array : Node_Id;

         Cleft_Lo  : Node_Id;
         Cright_Lo : Node_Id;
         Condition : Node_Id;

         Cresult : Compare_Result;

      begin
         --  Get the expressions for the arrays. If we are dealing with a
         --  private type, then convert to the underlying type. We can do
         --  direct assignments to an array that is a private type, but we
         --  cannot assign to elements of the array without this extra
         --  unchecked conversion.

         --  Note: We propagate Parent to the conversion nodes to generate
         --  a well-formed subtree.

         if Nkind (Act_Lhs) = N_Slice then
            Larray := Prefix (Act_Lhs);
         else
            Larray := Act_Lhs;

            if Is_Private_Type (Etype (Larray)) then
               declare
                  Par : constant Node_Id := Parent (Larray);
               begin
                  Larray :=
                    Unchecked_Convert_To
                      (Underlying_Type (Etype (Larray)), Larray);
                  Set_Parent (Larray, Par);
               end;
            end if;
         end if;

         if Nkind (Act_Rhs) = N_Slice then
            Rarray := Prefix (Act_Rhs);
         else
            Rarray := Act_Rhs;

            if Is_Private_Type (Etype (Rarray)) then
               declare
                  Par : constant Node_Id := Parent (Rarray);
               begin
                  Rarray :=
                    Unchecked_Convert_To
                      (Underlying_Type (Etype (Rarray)), Rarray);
                  Set_Parent (Rarray, Par);
               end;
            end if;
         end if;

         --  If both sides are slices, we must figure out whether it is safe
         --  to do the move in one direction or the other. It is always safe
         --  if there is a change of representation since obviously two arrays
         --  with different representations cannot possibly overlap.

         if (not Crep) and L_Slice and R_Slice then
            Act_L_Array := Get_Referenced_Object (Prefix (Act_Lhs));
            Act_R_Array := Get_Referenced_Object (Prefix (Act_Rhs));

            --  If both left and right hand arrays are entity names, and refer
            --  to different entities, then we know that the move is safe (the
            --  two storage areas are completely disjoint).

            if Is_Entity_Name (Act_L_Array)
              and then Is_Entity_Name (Act_R_Array)
              and then Entity (Act_L_Array) /= Entity (Act_R_Array)
            then
               null;

            --  Otherwise, we assume the worst, which is that the two arrays
            --  are the same array. There is no need to check if we know that
            --  is the case, because if we don't know it, we still have to
            --  assume it!

            --  Generally if the same array is involved, then we have an
            --  overlapping case. We will have to really assume the worst (i.e.
            --  set neither of the OK flags) unless we can determine the lower
            --  or upper bounds at compile time and compare them.

            else
               Cresult :=
                 Compile_Time_Compare
                   (Left_Lo, Right_Lo, Assume_Valid => True);

               if Cresult = Unknown then
                  Cresult :=
                    Compile_Time_Compare
                      (Left_Hi, Right_Hi, Assume_Valid => True);
               end if;

               case Cresult is
                  when LT | LE | EQ => Set_Backwards_OK (N, False);
                  when GT | GE      => Set_Forwards_OK  (N, False);
                  when NE | Unknown => Set_Backwards_OK (N, False);
                                       Set_Forwards_OK  (N, False);
               end case;
            end if;
         end if;

         --  If after that analysis Loop_Required is False, meaning that we
         --  have not discovered some non-overlap reason for requiring a loop,
         --  then the outcome depends on the capabilities of the back end.

         if not Loop_Required then

            --  The GCC back end can deal with all cases of overlap by falling
            --  back to memmove if it cannot use a more efficient approach.

            if VM_Target = No_VM and not AAMP_On_Target then
               return;

            --  Assume other back ends can handle it if Forwards_OK is set

            elsif Forwards_OK (N) then
               return;

            --  If Forwards_OK is not set, the back end will need something
            --  like memmove to handle the move. For now, this processing is
            --  activated using the .s debug flag (-gnatd.s).

            elsif Debug_Flag_Dot_S then
               return;
            end if;
         end if;

         --  At this stage we have to generate an explicit loop, and we have
         --  the following cases:

         --  Forwards_OK = True

         --    Rnn : right_index := right_index'First;
         --    for Lnn in left-index loop
         --       left (Lnn) := right (Rnn);
         --       Rnn := right_index'Succ (Rnn);
         --    end loop;

         --    Note: the above code MUST be analyzed with checks off, because
         --    otherwise the Succ could overflow. But in any case this is more
         --    efficient!

         --  Forwards_OK = False, Backwards_OK = True

         --    Rnn : right_index := right_index'Last;
         --    for Lnn in reverse left-index loop
         --       left (Lnn) := right (Rnn);
         --       Rnn := right_index'Pred (Rnn);
         --    end loop;

         --    Note: the above code MUST be analyzed with checks off, because
         --    otherwise the Pred could overflow. But in any case this is more
         --    efficient!

         --  Forwards_OK = Backwards_OK = False

         --    This only happens if we have the same array on each side. It is
         --    possible to create situations using overlays that violate this,
         --    but we simply do not promise to get this "right" in this case.

         --    There are two possible subcases. If the No_Implicit_Conditionals
         --    restriction is set, then we generate the following code:

         --      declare
         --        T : constant <operand-type> := rhs;
         --      begin
         --        lhs := T;
         --      end;

         --    If implicit conditionals are permitted, then we generate:

         --      if Left_Lo <= Right_Lo then
         --         <code for Forwards_OK = True above>
         --      else
         --         <code for Backwards_OK = True above>
         --      end if;

         --  In order to detect possible aliasing, we examine the renamed
         --  expression when the source or target is a renaming. However,
         --  the renaming may be intended to capture an address that may be
         --  affected by subsequent code, and therefore we must recover
         --  the actual entity for the expansion that follows, not the
         --  object it renames. In particular, if source or target designate
         --  a portion of a dynamically allocated object, the pointer to it
         --  may be reassigned but the renaming preserves the proper location.

         if Is_Entity_Name (Rhs)
           and then
             Nkind (Parent (Entity (Rhs))) = N_Object_Renaming_Declaration
           and then Nkind (Act_Rhs) = N_Slice
         then
            Rarray := Rhs;
         end if;

         if Is_Entity_Name (Lhs)
           and then
             Nkind (Parent (Entity (Lhs))) = N_Object_Renaming_Declaration
           and then Nkind (Act_Lhs) = N_Slice
         then
            Larray := Lhs;
         end if;

         --  Cases where either Forwards_OK or Backwards_OK is true

         if Forwards_OK (N) or else Backwards_OK (N) then
            if Needs_Finalization (Component_Type (L_Type))
              and then Base_Type (L_Type) = Base_Type (R_Type)
              and then Ndim = 1
              and then not No_Ctrl_Actions (N)
            then
               declare
                  Proc    : constant Entity_Id :=
                              TSS (Base_Type (L_Type), TSS_Slice_Assign);
                  Actuals : List_Id;

               begin
                  Apply_Dereference (Larray);
                  Apply_Dereference (Rarray);
                  Actuals := New_List (
                    Duplicate_Subexpr (Larray,   Name_Req => True),
                    Duplicate_Subexpr (Rarray,   Name_Req => True),
                    Duplicate_Subexpr (Left_Lo,  Name_Req => True),
                    Duplicate_Subexpr (Left_Hi,  Name_Req => True),
                    Duplicate_Subexpr (Right_Lo, Name_Req => True),
                    Duplicate_Subexpr (Right_Hi, Name_Req => True));

                  Append_To (Actuals,
                    New_Occurrence_Of (
                      Boolean_Literals (not Forwards_OK (N)), Loc));

                  Rewrite (N,
                    Make_Procedure_Call_Statement (Loc,
                      Name => New_Reference_To (Proc, Loc),
                      Parameter_Associations => Actuals));
               end;

            else
               Rewrite (N,
                 Expand_Assign_Array_Loop
                   (N, Larray, Rarray, L_Type, R_Type, Ndim,
                    Rev => not Forwards_OK (N)));
            end if;

         --  Case of both are false with No_Implicit_Conditionals

         elsif Restriction_Active (No_Implicit_Conditionals) then
            declare
                  T : constant Entity_Id :=
                        Make_Defining_Identifier (Loc, Chars => Name_T);

            begin
               Rewrite (N,
                 Make_Block_Statement (Loc,
                  Declarations => New_List (
                    Make_Object_Declaration (Loc,
                      Defining_Identifier => T,
                      Constant_Present  => True,
                      Object_Definition =>
                        New_Occurrence_Of (Etype (Rhs), Loc),
                      Expression        => Relocate_Node (Rhs))),

                    Handled_Statement_Sequence =>
                      Make_Handled_Sequence_Of_Statements (Loc,
                        Statements => New_List (
                          Make_Assignment_Statement (Loc,
                            Name       => Relocate_Node (Lhs),
                            Expression => New_Occurrence_Of (T, Loc))))));
            end;

         --  Case of both are false with implicit conditionals allowed

         else
            --  Before we generate this code, we must ensure that the left and
            --  right side array types are defined. They may be itypes, and we
            --  cannot let them be defined inside the if, since the first use
            --  in the then may not be executed.

            Ensure_Defined (L_Type, N);
            Ensure_Defined (R_Type, N);

            --  We normally compare addresses to find out which way round to
            --  do the loop, since this is reliable, and handles the cases of
            --  parameters, conversions etc. But we can't do that in the bit
            --  packed case or the VM case, because addresses don't work there.

            if not Is_Bit_Packed_Array (L_Type) and then VM_Target = No_VM then
               Condition :=
                 Make_Op_Le (Loc,
                   Left_Opnd =>
                     Unchecked_Convert_To (RTE (RE_Integer_Address),
                       Make_Attribute_Reference (Loc,
                         Prefix =>
                           Make_Indexed_Component (Loc,
                             Prefix =>
                               Duplicate_Subexpr_Move_Checks (Larray, True),
                             Expressions => New_List (
                               Make_Attribute_Reference (Loc,
                                 Prefix =>
                                   New_Reference_To
                                     (L_Index_Typ, Loc),
                                 Attribute_Name => Name_First))),
                         Attribute_Name => Name_Address)),

                   Right_Opnd =>
                     Unchecked_Convert_To (RTE (RE_Integer_Address),
                       Make_Attribute_Reference (Loc,
                         Prefix =>
                           Make_Indexed_Component (Loc,
                             Prefix =>
                               Duplicate_Subexpr_Move_Checks (Rarray, True),
                             Expressions => New_List (
                               Make_Attribute_Reference (Loc,
                                 Prefix =>
                                   New_Reference_To
                                     (R_Index_Typ, Loc),
                                 Attribute_Name => Name_First))),
                         Attribute_Name => Name_Address)));

            --  For the bit packed and VM cases we use the bounds. That's OK,
            --  because we don't have to worry about parameters, since they
            --  cannot cause overlap. Perhaps we should worry about weird slice
            --  conversions ???

            else
               --  Copy the bounds

               Cleft_Lo  := New_Copy_Tree (Left_Lo);
               Cright_Lo := New_Copy_Tree (Right_Lo);

               --  If the types do not match we add an implicit conversion
               --  here to ensure proper match

               if Etype (Left_Lo) /= Etype (Right_Lo) then
                  Cright_Lo :=
                    Unchecked_Convert_To (Etype (Left_Lo), Cright_Lo);
               end if;

               --  Reset the Analyzed flag, because the bounds of the index
               --  type itself may be universal, and must must be reanalyzed
               --  to acquire the proper type for the back end.

               Set_Analyzed (Cleft_Lo, False);
               Set_Analyzed (Cright_Lo, False);

               Condition :=
                 Make_Op_Le (Loc,
                   Left_Opnd  => Cleft_Lo,
                   Right_Opnd => Cright_Lo);
            end if;

            if Needs_Finalization (Component_Type (L_Type))
              and then Base_Type (L_Type) = Base_Type (R_Type)
              and then Ndim = 1
              and then not No_Ctrl_Actions (N)
            then

               --  Call TSS procedure for array assignment, passing the
               --  explicit bounds of right and left hand sides.

               declare
                  Proc    : constant Entity_Id :=
                              TSS (Base_Type (L_Type), TSS_Slice_Assign);
                  Actuals : List_Id;

               begin
                  Apply_Dereference (Larray);
                  Apply_Dereference (Rarray);
                  Actuals := New_List (
                    Duplicate_Subexpr (Larray,   Name_Req => True),
                    Duplicate_Subexpr (Rarray,   Name_Req => True),
                    Duplicate_Subexpr (Left_Lo,  Name_Req => True),
                    Duplicate_Subexpr (Left_Hi,  Name_Req => True),
                    Duplicate_Subexpr (Right_Lo, Name_Req => True),
                    Duplicate_Subexpr (Right_Hi, Name_Req => True));

                  Append_To (Actuals,
                     Make_Op_Not (Loc,
                       Right_Opnd => Condition));

                  Rewrite (N,
                    Make_Procedure_Call_Statement (Loc,
                      Name => New_Reference_To (Proc, Loc),
                      Parameter_Associations => Actuals));
               end;

            else
               Rewrite (N,
                 Make_Implicit_If_Statement (N,
                   Condition => Condition,

                   Then_Statements => New_List (
                     Expand_Assign_Array_Loop
                      (N, Larray, Rarray, L_Type, R_Type, Ndim,
                       Rev => False)),

                   Else_Statements => New_List (
                     Expand_Assign_Array_Loop
                      (N, Larray, Rarray, L_Type, R_Type, Ndim,
                       Rev => True))));
            end if;
         end if;

         Analyze (N, Suppress => All_Checks);
      end;

   exception
      when RE_Not_Available =>
         return;
   end Expand_Assign_Array;

   ------------------------------
   -- Expand_Assign_Array_Loop --
   ------------------------------

   --  The following is an example of the loop generated for the case of a
   --  two-dimensional array:

   --    declare
   --       R2b : Tm1X1 := 1;
   --    begin
   --       for L1b in 1 .. 100 loop
   --          declare
   --             R4b : Tm1X2 := 1;
   --          begin
   --             for L3b in 1 .. 100 loop
   --                vm1 (L1b, L3b) := vm2 (R2b, R4b);
   --                R4b := Tm1X2'succ(R4b);
   --             end loop;
   --          end;
   --          R2b := Tm1X1'succ(R2b);
   --       end loop;
   --    end;

   --  Here Rev is False, and Tm1Xn are the subscript types for the right hand
   --  side. The declarations of R2b and R4b are inserted before the original
   --  assignment statement.

   function Expand_Assign_Array_Loop
     (N      : Node_Id;
      Larray : Entity_Id;
      Rarray : Entity_Id;
      L_Type : Entity_Id;
      R_Type : Entity_Id;
      Ndim   : Pos;
      Rev    : Boolean) return Node_Id
   is
      Loc  : constant Source_Ptr := Sloc (N);

      Lnn : array (1 .. Ndim) of Entity_Id;
      Rnn : array (1 .. Ndim) of Entity_Id;
      --  Entities used as subscripts on left and right sides

      L_Index_Type : array (1 .. Ndim) of Entity_Id;
      R_Index_Type : array (1 .. Ndim) of Entity_Id;
      --  Left and right index types

      Assign : Node_Id;

      F_Or_L : Name_Id;
      S_Or_P : Name_Id;

      function Build_Step (J : Nat) return Node_Id;
      --  The increment step for the index of the right-hand side is written
      --  as an attribute reference (Succ or Pred). This function returns
      --  the corresponding node, which is placed at the end of the loop body.

      ----------------
      -- Build_Step --
      ----------------

      function Build_Step (J : Nat) return Node_Id is
         Step : Node_Id;
         Lim  : Name_Id;

      begin
         if Rev then
            Lim := Name_First;
         else
            Lim := Name_Last;
         end if;

         Step :=
            Make_Assignment_Statement (Loc,
               Name => New_Occurrence_Of (Rnn (J), Loc),
               Expression =>
                 Make_Attribute_Reference (Loc,
                   Prefix =>
                     New_Occurrence_Of (R_Index_Type (J), Loc),
                   Attribute_Name => S_Or_P,
                   Expressions => New_List (
                     New_Occurrence_Of (Rnn (J), Loc))));

      --  Note that on the last iteration of the loop, the index is increased
      --  (or decreased) past the corresponding bound. This is consistent with
      --  the C semantics of the back-end, where such an off-by-one value on a
      --  dead index variable is OK. However, in CodePeer mode this leads to
      --  spurious warnings, and thus we place a guard around the attribute
      --  reference. For obvious reasons we only do this for CodePeer.

         if CodePeer_Mode then
            Step :=
              Make_If_Statement (Loc,
                 Condition =>
                    Make_Op_Ne (Loc,
                       Left_Opnd  => New_Occurrence_Of (Lnn (J), Loc),
                       Right_Opnd =>
                         Make_Attribute_Reference (Loc,
                           Prefix => New_Occurrence_Of (L_Index_Type (J), Loc),
                           Attribute_Name => Lim)),
                 Then_Statements => New_List (Step));
         end if;

         return Step;
      end Build_Step;

   --  Start of processing for Expand_Assign_Array_Loop

   begin
      if Rev then
         F_Or_L := Name_Last;
         S_Or_P := Name_Pred;
      else
         F_Or_L := Name_First;
         S_Or_P := Name_Succ;
      end if;

      --  Setup index types and subscript entities

      declare
         L_Index : Node_Id;
         R_Index : Node_Id;

      begin
         L_Index := First_Index (L_Type);
         R_Index := First_Index (R_Type);

         for J in 1 .. Ndim loop
            Lnn (J) := Make_Temporary (Loc, 'L');
            Rnn (J) := Make_Temporary (Loc, 'R');

            L_Index_Type (J) := Etype (L_Index);
            R_Index_Type (J) := Etype (R_Index);

            Next_Index (L_Index);
            Next_Index (R_Index);
         end loop;
      end;

      --  Now construct the assignment statement

      declare
         ExprL : constant List_Id := New_List;
         ExprR : constant List_Id := New_List;

      begin
         for J in 1 .. Ndim loop
            Append_To (ExprL, New_Occurrence_Of (Lnn (J), Loc));
            Append_To (ExprR, New_Occurrence_Of (Rnn (J), Loc));
         end loop;

         Assign :=
           Make_Assignment_Statement (Loc,
             Name =>
               Make_Indexed_Component (Loc,
                 Prefix      => Duplicate_Subexpr (Larray, Name_Req => True),
                 Expressions => ExprL),
             Expression =>
               Make_Indexed_Component (Loc,
                 Prefix      => Duplicate_Subexpr (Rarray, Name_Req => True),
                 Expressions => ExprR));

         --  We set assignment OK, since there are some cases, e.g. in object
         --  declarations, where we are actually assigning into a constant.
         --  If there really is an illegality, it was caught long before now,
         --  and was flagged when the original assignment was analyzed.

         Set_Assignment_OK (Name (Assign));

         --  Propagate the No_Ctrl_Actions flag to individual assignments

         Set_No_Ctrl_Actions (Assign, No_Ctrl_Actions (N));
      end;

      --  Now construct the loop from the inside out, with the last subscript
      --  varying most rapidly. Note that Assign is first the raw assignment
      --  statement, and then subsequently the loop that wraps it up.

      for J in reverse 1 .. Ndim loop
         Assign :=
           Make_Block_Statement (Loc,
             Declarations => New_List (
              Make_Object_Declaration (Loc,
                Defining_Identifier => Rnn (J),
                Object_Definition =>
                  New_Occurrence_Of (R_Index_Type (J), Loc),
                Expression =>
                  Make_Attribute_Reference (Loc,
                    Prefix => New_Occurrence_Of (R_Index_Type (J), Loc),
                    Attribute_Name => F_Or_L))),

           Handled_Statement_Sequence =>
             Make_Handled_Sequence_Of_Statements (Loc,
               Statements => New_List (
                 Make_Implicit_Loop_Statement (N,
                   Iteration_Scheme =>
                     Make_Iteration_Scheme (Loc,
                       Loop_Parameter_Specification =>
                         Make_Loop_Parameter_Specification (Loc,
                           Defining_Identifier => Lnn (J),
                           Reverse_Present => Rev,
                           Discrete_Subtype_Definition =>
                             New_Reference_To (L_Index_Type (J), Loc))),

                   Statements => New_List (Assign, Build_Step (J))))));
      end loop;

      return Assign;
   end Expand_Assign_Array_Loop;

   --------------------------
   -- Expand_Assign_Record --
   --------------------------

   procedure Expand_Assign_Record (N : Node_Id) is
      Lhs   : constant Node_Id    := Name (N);
      Rhs   : Node_Id             := Expression (N);
      L_Typ : constant Entity_Id  := Base_Type (Etype (Lhs));

   begin
      --  If change of representation, then extract the real right hand side
      --  from the type conversion, and proceed with component-wise assignment,
      --  since the two types are not the same as far as the back end is
      --  concerned.

      if Change_Of_Representation (N) then
         Rhs := Expression (Rhs);

      --  If this may be a case of a large bit aligned component, then proceed
      --  with component-wise assignment, to avoid possible clobbering of other
      --  components sharing bits in the first or last byte of the component to
      --  be assigned.

      elsif Possible_Bit_Aligned_Component (Lhs)
              or
            Possible_Bit_Aligned_Component (Rhs)
      then
         null;

      --  If we have a tagged type that has a complete record representation
      --  clause, we must do we must do component-wise assignments, since child
      --  types may have used gaps for their components, and we might be
      --  dealing with a view conversion.

      elsif Is_Fully_Repped_Tagged_Type (L_Typ) then
         null;

      --  If neither condition met, then nothing special to do, the back end
      --  can handle assignment of the entire component as a single entity.

      else
         return;
      end if;

      --  At this stage we know that we must do a component wise assignment

      declare
         Loc   : constant Source_Ptr := Sloc (N);
         R_Typ : constant Entity_Id  := Base_Type (Etype (Rhs));
         Decl  : constant Node_Id    := Declaration_Node (R_Typ);
         RDef  : Node_Id;
         F     : Entity_Id;

         function Find_Component
           (Typ  : Entity_Id;
            Comp : Entity_Id) return Entity_Id;
         --  Find the component with the given name in the underlying record
         --  declaration for Typ. We need to use the actual entity because the
         --  type may be private and resolution by identifier alone would fail.

         function Make_Component_List_Assign
           (CL  : Node_Id;
            U_U : Boolean := False) return List_Id;
         --  Returns a sequence of statements to assign the components that
         --  are referenced in the given component list. The flag U_U is
         --  used to force the usage of the inferred value of the variant
         --  part expression as the switch for the generated case statement.

         function Make_Field_Assign
           (C   : Entity_Id;
            U_U : Boolean := False) return Node_Id;
         --  Given C, the entity for a discriminant or component, build an
         --  assignment for the corresponding field values. The flag U_U
         --  signals the presence of an Unchecked_Union and forces the usage
         --  of the inferred discriminant value of C as the right hand side
         --  of the assignment.

         function Make_Field_Assigns (CI : List_Id) return List_Id;
         --  Given CI, a component items list, construct series of statements
         --  for fieldwise assignment of the corresponding components.

         --------------------
         -- Find_Component --
         --------------------

         function Find_Component
           (Typ  : Entity_Id;
            Comp : Entity_Id) return Entity_Id
         is
            Utyp : constant Entity_Id := Underlying_Type (Typ);
            C    : Entity_Id;

         begin
            C := First_Entity (Utyp);
            while Present (C) loop
               if Chars (C) = Chars (Comp) then
                  return C;
               end if;

               Next_Entity (C);
            end loop;

            raise Program_Error;
         end Find_Component;

         --------------------------------
         -- Make_Component_List_Assign --
         --------------------------------

         function Make_Component_List_Assign
           (CL  : Node_Id;
            U_U : Boolean := False) return List_Id
         is
            CI : constant List_Id := Component_Items (CL);
            VP : constant Node_Id := Variant_Part (CL);

            Alts   : List_Id;
            DC     : Node_Id;
            DCH    : List_Id;
            Expr   : Node_Id;
            Result : List_Id;
            V      : Node_Id;

         begin
            Result := Make_Field_Assigns (CI);

            if Present (VP) then
               V := First_Non_Pragma (Variants (VP));
               Alts := New_List;
               while Present (V) loop
                  DCH := New_List;
                  DC := First (Discrete_Choices (V));
                  while Present (DC) loop
                     Append_To (DCH, New_Copy_Tree (DC));
                     Next (DC);
                  end loop;

                  Append_To (Alts,
                    Make_Case_Statement_Alternative (Loc,
                      Discrete_Choices => DCH,
                      Statements =>
                        Make_Component_List_Assign (Component_List (V))));
                  Next_Non_Pragma (V);
               end loop;

               --  If we have an Unchecked_Union, use the value of the inferred
               --  discriminant of the variant part expression as the switch
               --  for the case statement. The case statement may later be
               --  folded.

               if U_U then
                  Expr :=
                    New_Copy (Get_Discriminant_Value (
                      Entity (Name (VP)),
                      Etype (Rhs),
                      Discriminant_Constraint (Etype (Rhs))));
               else
                  Expr :=
                    Make_Selected_Component (Loc,
                      Prefix        => Duplicate_Subexpr (Rhs),
                      Selector_Name =>
                        Make_Identifier (Loc, Chars (Name (VP))));
               end if;

               Append_To (Result,
                 Make_Case_Statement (Loc,
                   Expression => Expr,
                   Alternatives => Alts));
            end if;

            return Result;
         end Make_Component_List_Assign;

         -----------------------
         -- Make_Field_Assign --
         -----------------------

         function Make_Field_Assign
           (C   : Entity_Id;
            U_U : Boolean := False) return Node_Id
         is
            A    : Node_Id;
            Expr : Node_Id;

         begin
            --  In the case of an Unchecked_Union, use the discriminant
            --  constraint value as on the right hand side of the assignment.

            if U_U then
               Expr :=
                 New_Copy (Get_Discriminant_Value (C,
                   Etype (Rhs),
                   Discriminant_Constraint (Etype (Rhs))));
            else
               Expr :=
                 Make_Selected_Component (Loc,
                   Prefix        => Duplicate_Subexpr (Rhs),
                   Selector_Name => New_Occurrence_Of (C, Loc));
            end if;

            A :=
              Make_Assignment_Statement (Loc,
                Name =>
                  Make_Selected_Component (Loc,
                    Prefix        => Duplicate_Subexpr (Lhs),
                    Selector_Name =>
                      New_Occurrence_Of (Find_Component (L_Typ, C), Loc)),
                Expression => Expr);

            --  Set Assignment_OK, so discriminants can be assigned

            Set_Assignment_OK (Name (A), True);

            if Componentwise_Assignment (N)
              and then Nkind (Name (A)) = N_Selected_Component
              and then Chars (Selector_Name (Name (A))) = Name_uParent
            then
               Set_Componentwise_Assignment (A);
            end if;

            return A;
         end Make_Field_Assign;

         ------------------------
         -- Make_Field_Assigns --
         ------------------------

         function Make_Field_Assigns (CI : List_Id) return List_Id is
            Item   : Node_Id;
            Result : List_Id;

         begin
            Item := First (CI);
            Result := New_List;

            while Present (Item) loop

               --  Look for components, but exclude _tag field assignment if
               --  the special Componentwise_Assignment flag is set.

               if Nkind (Item) = N_Component_Declaration
                 and then not (Is_Tag (Defining_Identifier (Item))
                                 and then Componentwise_Assignment (N))
               then
                  Append_To
                    (Result, Make_Field_Assign (Defining_Identifier (Item)));
               end if;

               Next (Item);
            end loop;

            return Result;
         end Make_Field_Assigns;

      --  Start of processing for Expand_Assign_Record

      begin
         --  Note that we use the base types for this processing. This results
         --  in some extra work in the constrained case, but the change of
         --  representation case is so unusual that it is not worth the effort.

         --  First copy the discriminants. This is done unconditionally. It
         --  is required in the unconstrained left side case, and also in the
         --  case where this assignment was constructed during the expansion
         --  of a type conversion (since initialization of discriminants is
         --  suppressed in this case). It is unnecessary but harmless in
         --  other cases.

         if Has_Discriminants (L_Typ) then
            F := First_Discriminant (R_Typ);
            while Present (F) loop

               --  If we are expanding the initialization of a derived record
               --  that constrains or renames discriminants of the parent, we
               --  must use the corresponding discriminant in the parent.

               declare
                  CF : Entity_Id;

               begin
                  if Inside_Init_Proc
                    and then Present (Corresponding_Discriminant (F))
                  then
                     CF := Corresponding_Discriminant (F);
                  else
                     CF := F;
                  end if;

                  if Is_Unchecked_Union (Base_Type (R_Typ)) then

                     --  Within an initialization procedure this is the
                     --  assignment to an unchecked union component, in which
                     --  case there is no discriminant to initialize.

                     if Inside_Init_Proc then
                        null;

                     else
                        --  The assignment is part of a conversion from a
                        --  derived unchecked union type with an inferable
                        --  discriminant, to a parent type.

                        Insert_Action (N, Make_Field_Assign (CF, True));
                     end if;

                  else
                     Insert_Action (N, Make_Field_Assign (CF));
                  end if;

                  Next_Discriminant (F);
               end;
            end loop;
         end if;

         --  We know the underlying type is a record, but its current view
         --  may be private. We must retrieve the usable record declaration.

         if Nkind_In (Decl, N_Private_Type_Declaration,
                            N_Private_Extension_Declaration)
           and then Present (Full_View (R_Typ))
         then
            RDef := Type_Definition (Declaration_Node (Full_View (R_Typ)));
         else
            RDef := Type_Definition (Decl);
         end if;

         if Nkind (RDef) = N_Derived_Type_Definition then
            RDef := Record_Extension_Part (RDef);
         end if;

         if Nkind (RDef) = N_Record_Definition
           and then Present (Component_List (RDef))
         then
            if Is_Unchecked_Union (R_Typ) then
               Insert_Actions (N,
                 Make_Component_List_Assign (Component_List (RDef), True));
            else
               Insert_Actions
                 (N, Make_Component_List_Assign (Component_List (RDef)));
            end if;

            Rewrite (N, Make_Null_Statement (Loc));
         end if;
      end;
   end Expand_Assign_Record;

   -----------------------------------
   -- Expand_N_Assignment_Statement --
   -----------------------------------

   --  This procedure implements various cases where an assignment statement
   --  cannot just be passed on to the back end in untransformed state.

   procedure Expand_N_Assignment_Statement (N : Node_Id) is
      Loc  : constant Source_Ptr := Sloc (N);
      Crep : constant Boolean    := Change_Of_Representation (N);
      Lhs  : constant Node_Id    := Name (N);
      Rhs  : constant Node_Id    := Expression (N);
      Typ  : constant Entity_Id  := Underlying_Type (Etype (Lhs));
      Exp  : Node_Id;

   begin
      --  Special case to check right away, if the Componentwise_Assignment
      --  flag is set, this is a reanalysis from the expansion of the primitive
      --  assignment procedure for a tagged type, and all we need to do is to
      --  expand to assignment of components, because otherwise, we would get
      --  infinite recursion (since this looks like a tagged assignment which
      --  would normally try to *call* the primitive assignment procedure).

      if Componentwise_Assignment (N) then
         Expand_Assign_Record (N);
         return;
      end if;

      --  Defend against invalid subscripts on left side if we are in standard
      --  validity checking mode. No need to do this if we are checking all
      --  subscripts.

      --  Note that we do this right away, because there are some early return
      --  paths in this procedure, and this is required on all paths.

      if Validity_Checks_On
        and then Validity_Check_Default
        and then not Validity_Check_Subscripts
      then
         Check_Valid_Lvalue_Subscripts (Lhs);
      end if;

      --  Ada 2005 (AI-327): Handle assignment to priority of protected object

      --  Rewrite an assignment to X'Priority into a run-time call

      --   For example:         X'Priority := New_Prio_Expr;
      --   ...is expanded into  Set_Ceiling (X._Object, New_Prio_Expr);

      --  Note that although X'Priority is notionally an object, it is quite
      --  deliberately not defined as an aliased object in the RM. This means
      --  that it works fine to rewrite it as a call, without having to worry
      --  about complications that would other arise from X'Priority'Access,
      --  which is illegal, because of the lack of aliasing.

      if Ada_Version >= Ada_2005 then
         declare
            Call           : Node_Id;
            Conctyp        : Entity_Id;
            Ent            : Entity_Id;
            Subprg         : Entity_Id;
            RT_Subprg_Name : Node_Id;

         begin
            --  Handle chains of renamings

            Ent := Name (N);
            while Nkind (Ent) in N_Has_Entity
              and then Present (Entity (Ent))
              and then Present (Renamed_Object (Entity (Ent)))
            loop
               Ent := Renamed_Object (Entity (Ent));
            end loop;

            --  The attribute Priority applied to protected objects has been
            --  previously expanded into a call to the Get_Ceiling run-time
            --  subprogram.

            if Nkind (Ent) = N_Function_Call
              and then (Entity (Name (Ent)) = RTE (RE_Get_Ceiling)
                          or else
                        Entity (Name (Ent)) = RTE (RO_PE_Get_Ceiling))
            then
               --  Look for the enclosing concurrent type

               Conctyp := Current_Scope;
               while not Is_Concurrent_Type (Conctyp) loop
                  Conctyp := Scope (Conctyp);
               end loop;

               pragma Assert (Is_Protected_Type (Conctyp));

               --  Generate the first actual of the call

               Subprg := Current_Scope;
               while not Present (Protected_Body_Subprogram (Subprg)) loop
                  Subprg := Scope (Subprg);
               end loop;

               --  Select the appropriate run-time call

               if Number_Entries (Conctyp) = 0 then
                  RT_Subprg_Name :=
                    New_Reference_To (RTE (RE_Set_Ceiling), Loc);
               else
                  RT_Subprg_Name :=
                    New_Reference_To (RTE (RO_PE_Set_Ceiling), Loc);
               end if;

               Call :=
                 Make_Procedure_Call_Statement (Loc,
                   Name => RT_Subprg_Name,
                   Parameter_Associations => New_List (
                     New_Copy_Tree (First (Parameter_Associations (Ent))),
                     Relocate_Node (Expression (N))));

               Rewrite (N, Call);
               Analyze (N);
               return;
            end if;
         end;
      end if;

      --  Deal with assignment checks unless suppressed

      if not Suppress_Assignment_Checks (N) then

         --  First deal with generation of range check if required

         if Do_Range_Check (Rhs) then
            Set_Do_Range_Check (Rhs, False);
            Generate_Range_Check (Rhs, Typ, CE_Range_Check_Failed);
         end if;

         --  Then generate predicate check if required

         Apply_Predicate_Check (Rhs, Typ);
      end if;

      --  Check for a special case where a high level transformation is
      --  required. If we have either of:

      --    P.field := rhs;
      --    P (sub) := rhs;

      --  where P is a reference to a bit packed array, then we have to unwind
      --  the assignment. The exact meaning of being a reference to a bit
      --  packed array is as follows:

      --    An indexed component whose prefix is a bit packed array is a
      --    reference to a bit packed array.

      --    An indexed component or selected component whose prefix is a
      --    reference to a bit packed array is itself a reference ot a
      --    bit packed array.

      --  The required transformation is

      --     Tnn : prefix_type := P;
      --     Tnn.field := rhs;
      --     P := Tnn;

      --  or

      --     Tnn : prefix_type := P;
      --     Tnn (subscr) := rhs;
      --     P := Tnn;

      --  Since P is going to be evaluated more than once, any subscripts
      --  in P must have their evaluation forced.

      if Nkind_In (Lhs, N_Indexed_Component, N_Selected_Component)
        and then Is_Ref_To_Bit_Packed_Array (Prefix (Lhs))
      then
         declare
            BPAR_Expr : constant Node_Id   := Relocate_Node (Prefix (Lhs));
            BPAR_Typ  : constant Entity_Id := Etype (BPAR_Expr);
            Tnn       : constant Entity_Id :=
                          Make_Temporary (Loc, 'T', BPAR_Expr);

         begin
            --  Insert the post assignment first, because we want to copy the
            --  BPAR_Expr tree before it gets analyzed in the context of the
            --  pre assignment. Note that we do not analyze the post assignment
            --  yet (we cannot till we have completed the analysis of the pre
            --  assignment). As usual, the analysis of this post assignment
            --  will happen on its own when we "run into" it after finishing
            --  the current assignment.

            Insert_After (N,
              Make_Assignment_Statement (Loc,
                Name       => New_Copy_Tree (BPAR_Expr),
                Expression => New_Occurrence_Of (Tnn, Loc)));

            --  At this stage BPAR_Expr is a reference to a bit packed array
            --  where the reference was not expanded in the original tree,
            --  since it was on the left side of an assignment. But in the
            --  pre-assignment statement (the object definition), BPAR_Expr
            --  will end up on the right hand side, and must be reexpanded. To
            --  achieve this, we reset the analyzed flag of all selected and
            --  indexed components down to the actual indexed component for
            --  the packed array.

            Exp := BPAR_Expr;
            loop
               Set_Analyzed (Exp, False);

               if Nkind_In
                   (Exp, N_Selected_Component, N_Indexed_Component)
               then
                  Exp := Prefix (Exp);
               else
                  exit;
               end if;
            end loop;

            --  Now we can insert and analyze the pre-assignment

            --  If the right-hand side requires a transient scope, it has
            --  already been placed on the stack. However, the declaration is
            --  inserted in the tree outside of this scope, and must reflect
            --  the proper scope for its variable. This awkward bit is forced
            --  by the stricter scope discipline imposed by GCC 2.97.

            declare
               Uses_Transient_Scope : constant Boolean :=
                                        Scope_Is_Transient
                                          and then N = Node_To_Be_Wrapped;

            begin
               if Uses_Transient_Scope then
                  Push_Scope (Scope (Current_Scope));
               end if;

               Insert_Before_And_Analyze (N,
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Tnn,
                   Object_Definition   => New_Occurrence_Of (BPAR_Typ, Loc),
                   Expression          => BPAR_Expr));

               if Uses_Transient_Scope then
                  Pop_Scope;
               end if;
            end;

            --  Now fix up the original assignment and continue processing

            Rewrite (Prefix (Lhs),
              New_Occurrence_Of (Tnn, Loc));

            --  We do not need to reanalyze that assignment, and we do not need
            --  to worry about references to the temporary, but we do need to
            --  make sure that the temporary is not marked as a true constant
            --  since we now have a generated assignment to it!

            Set_Is_True_Constant (Tnn, False);
         end;
      end if;

      --  When we have the appropriate type of aggregate in the expression (it
      --  has been determined during analysis of the aggregate by setting the
      --  delay flag), let's perform in place assignment and thus avoid
      --  creating a temporary.

      if Is_Delayed_Aggregate (Rhs) then
         Convert_Aggr_In_Assignment (N);
         Rewrite (N, Make_Null_Statement (Loc));
         Analyze (N);
         return;
      end if;

      --  Apply discriminant check if required. If Lhs is an access type to a
      --  designated type with discriminants, we must always check.

      if Has_Discriminants (Etype (Lhs)) then

         --  Skip discriminant check if change of representation. Will be
         --  done when the change of representation is expanded out.

         if not Crep then
            Apply_Discriminant_Check (Rhs, Etype (Lhs), Lhs);
         end if;

      --  If the type is private without discriminants, and the full type
      --  has discriminants (necessarily with defaults) a check may still be
      --  necessary if the Lhs is aliased. The private discriminants must be
      --  visible to build the discriminant constraints.

      --  Only an explicit dereference that comes from source indicates
      --  aliasing. Access to formals of protected operations and entries
      --  create dereferences but are not semantic aliasings.

      elsif Is_Private_Type (Etype (Lhs))
        and then Has_Discriminants (Typ)
        and then Nkind (Lhs) = N_Explicit_Dereference
        and then Comes_From_Source (Lhs)
      then
         declare
            Lt  : constant Entity_Id := Etype (Lhs);
            Ubt : Entity_Id          := Base_Type (Typ);

         begin
            --  In the case of an expander-generated record subtype whose base
            --  type still appears private, Typ will have been set to that
            --  private type rather than the underlying record type (because
            --  Underlying type will have returned the record subtype), so it's
            --  necessary to apply Underlying_Type again to the base type to
            --  get the record type we need for the discriminant check. Such
            --  subtypes can be created for assignments in certain cases, such
            --  as within an instantiation passed this kind of private type.
            --  It would be good to avoid this special test, but making changes
            --  to prevent this odd form of record subtype seems difficult. ???

            if Is_Private_Type (Ubt) then
               Ubt := Underlying_Type (Ubt);
            end if;

            Set_Etype (Lhs, Ubt);
            Rewrite (Rhs, OK_Convert_To (Base_Type (Ubt), Rhs));
            Apply_Discriminant_Check (Rhs, Ubt, Lhs);
            Set_Etype (Lhs, Lt);
         end;

         --  If the Lhs has a private type with unknown discriminants, it
         --  may have a full view with discriminants, but those are nameable
         --  only in the underlying type, so convert the Rhs to it before
         --  potential checking.

      elsif Has_Unknown_Discriminants (Base_Type (Etype (Lhs)))
        and then Has_Discriminants (Typ)
      then
         Rewrite (Rhs, OK_Convert_To (Base_Type (Typ), Rhs));
         Apply_Discriminant_Check (Rhs, Typ, Lhs);

      --  In the access type case, we need the same discriminant check, and
      --  also range checks if we have an access to constrained array.

      elsif Is_Access_Type (Etype (Lhs))
        and then Is_Constrained (Designated_Type (Etype (Lhs)))
      then
         if Has_Discriminants (Designated_Type (Etype (Lhs))) then

            --  Skip discriminant check if change of representation. Will be
            --  done when the change of representation is expanded out.

            if not Crep then
               Apply_Discriminant_Check (Rhs, Etype (Lhs));
            end if;

         elsif Is_Array_Type (Designated_Type (Etype (Lhs))) then
            Apply_Range_Check (Rhs, Etype (Lhs));

            if Is_Constrained (Etype (Lhs)) then
               Apply_Length_Check (Rhs, Etype (Lhs));
            end if;

            if Nkind (Rhs) = N_Allocator then
               declare
                  Target_Typ : constant Entity_Id := Etype (Expression (Rhs));
                  C_Es       : Check_Result;

               begin
                  C_Es :=
                    Get_Range_Checks
                      (Lhs,
                       Target_Typ,
                       Etype (Designated_Type (Etype (Lhs))));

                  Insert_Range_Checks
                    (C_Es,
                     N,
                     Target_Typ,
                     Sloc (Lhs),
                     Lhs);
               end;
            end if;
         end if;

      --  Apply range check for access type case

      elsif Is_Access_Type (Etype (Lhs))
        and then Nkind (Rhs) = N_Allocator
        and then Nkind (Expression (Rhs)) = N_Qualified_Expression
      then
         Analyze_And_Resolve (Expression (Rhs));
         Apply_Range_Check
           (Expression (Rhs), Designated_Type (Etype (Lhs)));
      end if;

      --  Ada 2005 (AI-231): Generate the run-time check

      if Is_Access_Type (Typ)
        and then Can_Never_Be_Null (Etype (Lhs))
        and then not Can_Never_Be_Null (Etype (Rhs))
      then
         Apply_Constraint_Check (Rhs, Etype (Lhs));
      end if;

      --  Ada 2012 (AI05-148): Update current accessibility level if Rhs is a
      --  stand-alone obj of an anonymous access type.

      if Is_Access_Type (Typ)
        and then Is_Entity_Name (Lhs)
        and then Present (Effective_Extra_Accessibility (Entity (Lhs))) then
         declare
            function Lhs_Entity return Entity_Id;
            --  Look through renames to find the underlying entity.
            --  For assignment to a rename, we don't care about the
            --  Enclosing_Dynamic_Scope of the rename declaration.

            ----------------
            -- Lhs_Entity --
            ----------------

            function Lhs_Entity return Entity_Id is
               Result : Entity_Id := Entity (Lhs);

            begin
               while Present (Renamed_Object (Result)) loop

                  --  Renamed_Object must return an Entity_Name here
                  --  because of preceding "Present (E_E_A (...))" test.

                  Result := Entity (Renamed_Object (Result));
               end loop;

               return Result;
            end Lhs_Entity;

            --  Local Declarations

            Access_Check : constant Node_Id :=
                             Make_Raise_Program_Error (Loc,
                               Condition =>
                                 Make_Op_Gt (Loc,
                                   Left_Opnd  =>
                                     Dynamic_Accessibility_Level (Rhs),
                                   Right_Opnd =>
                                     Make_Integer_Literal (Loc,
                                       Intval =>
                                         Scope_Depth
                                           (Enclosing_Dynamic_Scope
                                             (Lhs_Entity)))),
                               Reason => PE_Accessibility_Check_Failed);

            Access_Level_Update : constant Node_Id :=
                                    Make_Assignment_Statement (Loc,
                                     Name       =>
                                       New_Occurrence_Of
                                         (Effective_Extra_Accessibility
                                            (Entity (Lhs)), Loc),
                                     Expression =>
                                        Dynamic_Accessibility_Level (Rhs));

         begin
            if not Accessibility_Checks_Suppressed (Entity (Lhs)) then
               Insert_Action (N, Access_Check);
            end if;

            Insert_Action (N, Access_Level_Update);
         end;
      end if;

      --  Case of assignment to a bit packed array element. If there is a
      --  change of representation this must be expanded into components,
      --  otherwise this is a bit-field assignment.

      if Nkind (Lhs) = N_Indexed_Component
        and then Is_Bit_Packed_Array (Etype (Prefix (Lhs)))
      then
         --  Normal case, no change of representation

         if not Crep then
            Expand_Bit_Packed_Element_Set (N);
            return;

         --  Change of representation case

         else
            --  Generate the following, to force component-by-component
            --  assignments in an efficient way. Otherwise each component
            --  will require a temporary and two bit-field manipulations.

            --  T1 : Elmt_Type;
            --  T1 := RhS;
            --  Lhs := T1;

            declare
               Tnn : constant Entity_Id := Make_Temporary (Loc, 'T');
               Stats : List_Id;

            begin
               Stats :=
                 New_List (
                   Make_Object_Declaration (Loc,
                     Defining_Identifier => Tnn,
                     Object_Definition   =>
                       New_Occurrence_Of (Etype (Lhs), Loc)),
                   Make_Assignment_Statement (Loc,
                     Name       => New_Occurrence_Of (Tnn, Loc),
                     Expression => Relocate_Node (Rhs)),
                   Make_Assignment_Statement (Loc,
                     Name       => Relocate_Node (Lhs),
                     Expression => New_Occurrence_Of (Tnn, Loc)));

               Insert_Actions (N, Stats);
               Rewrite (N, Make_Null_Statement (Loc));
               Analyze (N);
            end;
         end if;

      --  Build-in-place function call case. Note that we're not yet doing
      --  build-in-place for user-written assignment statements (the assignment
      --  here came from an aggregate.)

      elsif Ada_Version >= Ada_2005
        and then Is_Build_In_Place_Function_Call (Rhs)
      then
         Make_Build_In_Place_Call_In_Assignment (N, Rhs);

      elsif Is_Tagged_Type (Typ) and then Is_Value_Type (Etype (Lhs)) then

         --  Nothing to do for valuetypes
         --  ??? Set_Scope_Is_Transient (False);

         return;

      elsif Is_Tagged_Type (Typ)
        or else (Needs_Finalization (Typ) and then not Is_Array_Type (Typ))
      then
         Tagged_Case : declare
            L                   : List_Id := No_List;
            Expand_Ctrl_Actions : constant Boolean := not No_Ctrl_Actions (N);

         begin
            --  In the controlled case, we ensure that function calls are
            --  evaluated before finalizing the target. In all cases, it makes
            --  the expansion easier if the side-effects are removed first.

            Remove_Side_Effects (Lhs);
            Remove_Side_Effects (Rhs);

            --  Avoid recursion in the mechanism

            Set_Analyzed (N);

            --  If dispatching assignment, we need to dispatch to _assign

            if Is_Class_Wide_Type (Typ)

               --  If the type is tagged, we may as well use the predefined
               --  primitive assignment. This avoids inlining a lot of code
               --  and in the class-wide case, the assignment is replaced
               --  by a dispatching call to _assign. It is suppressed in the
               --  case of assignments created by the expander that correspond
               --  to initializations, where we do want to copy the tag
               --  (Expand_Ctrl_Actions flag is set True in this case). It is
               --  also suppressed if restriction No_Dispatching_Calls is in
               --  force because in that case predefined primitives are not
               --  generated.

               or else (Is_Tagged_Type (Typ)
                         and then not Is_Value_Type (Etype (Lhs))
                         and then Chars (Current_Scope) /= Name_uAssign
                         and then Expand_Ctrl_Actions
                         and then
                           not Restriction_Active (No_Dispatching_Calls))
            then
               --  Fetch the primitive op _assign and proper type to call it.
               --  Because of possible conflicts between private and full view,
               --  fetch the proper type directly from the operation profile.

               declare
                  Op    : constant Entity_Id :=
                            Find_Prim_Op (Typ, Name_uAssign);
                  F_Typ : Entity_Id := Etype (First_Formal (Op));

               begin
                  --  If the assignment is dispatching, make sure to use the
                  --  proper type.

                  if Is_Class_Wide_Type (Typ) then
                     F_Typ := Class_Wide_Type (F_Typ);
                  end if;

                  L := New_List;

                  --  In case of assignment to a class-wide tagged type, before
                  --  the assignment we generate run-time check to ensure that
                  --  the tags of source and target match.

                  if Is_Class_Wide_Type (Typ)
                    and then Is_Tagged_Type (Typ)
                    and then Is_Tagged_Type (Underlying_Type (Etype (Rhs)))
                  then
                     Append_To (L,
                       Make_Raise_Constraint_Error (Loc,
                         Condition =>
                           Make_Op_Ne (Loc,
                             Left_Opnd =>
                               Make_Selected_Component (Loc,
                                 Prefix        => Duplicate_Subexpr (Lhs),
                                 Selector_Name =>
                                   Make_Identifier (Loc, Name_uTag)),
                             Right_Opnd =>
                               Make_Selected_Component (Loc,
                                 Prefix        => Duplicate_Subexpr (Rhs),
                                 Selector_Name =>
                                   Make_Identifier (Loc, Name_uTag))),
                         Reason => CE_Tag_Check_Failed));
                  end if;

                  declare
                     Left_N  : Node_Id := Duplicate_Subexpr (Lhs);
                     Right_N : Node_Id := Duplicate_Subexpr (Rhs);

                  begin
                     --  In order to dispatch the call to _assign the type of
                     --  the actuals must match. Add conversion (if required).

                     if Etype (Lhs) /= F_Typ then
                        Left_N := Unchecked_Convert_To (F_Typ, Left_N);
                     end if;

                     if Etype (Rhs) /= F_Typ then
                        Right_N := Unchecked_Convert_To (F_Typ, Right_N);
                     end if;

                     Append_To (L,
                       Make_Procedure_Call_Statement (Loc,
                         Name => New_Reference_To (Op, Loc),
                         Parameter_Associations => New_List (
                           Node1 => Left_N,
                           Node2 => Right_N)));
                  end;
               end;

            else
               L := Make_Tag_Ctrl_Assignment (N);

               --  We can't afford to have destructive Finalization Actions in
               --  the Self assignment case, so if the target and the source
               --  are not obviously different, code is generated to avoid the
               --  self assignment case:

               --    if lhs'address /= rhs'address then
               --       <code for controlled and/or tagged assignment>
               --    end if;

               --  Skip this if Restriction (No_Finalization) is active

               if not Statically_Different (Lhs, Rhs)
                 and then Expand_Ctrl_Actions
                 and then not Restriction_Active (No_Finalization)
               then
                  L := New_List (
                    Make_Implicit_If_Statement (N,
                      Condition =>
                        Make_Op_Ne (Loc,
                          Left_Opnd =>
                            Make_Attribute_Reference (Loc,
                              Prefix         => Duplicate_Subexpr (Lhs),
                              Attribute_Name => Name_Address),

                           Right_Opnd =>
                            Make_Attribute_Reference (Loc,
                              Prefix         => Duplicate_Subexpr (Rhs),
                              Attribute_Name => Name_Address)),

                      Then_Statements => L));
               end if;

               --  We need to set up an exception handler for implementing
               --  7.6.1(18). The remaining adjustments are tackled by the
               --  implementation of adjust for record_controllers (see
               --  s-finimp.adb).

               --  This is skipped if we have no finalization

               if Expand_Ctrl_Actions
                 and then not Restriction_Active (No_Finalization)
               then
                  L := New_List (
                    Make_Block_Statement (Loc,
                      Handled_Statement_Sequence =>
                        Make_Handled_Sequence_Of_Statements (Loc,
                          Statements => L,
                          Exception_Handlers => New_List (
                            Make_Handler_For_Ctrl_Operation (Loc)))));
               end if;
            end if;

            Rewrite (N,
              Make_Block_Statement (Loc,
                Handled_Statement_Sequence =>
                  Make_Handled_Sequence_Of_Statements (Loc, Statements => L)));

            --  If no restrictions on aborts, protect the whole assignment
            --  for controlled objects as per 9.8(11).

            if Needs_Finalization (Typ)
              and then Expand_Ctrl_Actions
              and then Abort_Allowed
            then
               declare
                  Blk : constant Entity_Id :=
                          New_Internal_Entity
                            (E_Block, Current_Scope, Sloc (N), 'B');

               begin
                  Set_Scope (Blk, Current_Scope);
                  Set_Etype (Blk, Standard_Void_Type);
                  Set_Identifier (N, New_Occurrence_Of (Blk, Sloc (N)));

                  Prepend_To (L, Build_Runtime_Call (Loc, RE_Abort_Defer));
                  Set_At_End_Proc (Handled_Statement_Sequence (N),
                    New_Occurrence_Of (RTE (RE_Abort_Undefer_Direct), Loc));
                  Expand_At_End_Handler
                    (Handled_Statement_Sequence (N), Blk);
               end;
            end if;

            --  N has been rewritten to a block statement for which it is
            --  known by construction that no checks are necessary: analyze
            --  it with all checks suppressed.

            Analyze (N, Suppress => All_Checks);
            return;
         end Tagged_Case;

      --  Array types

      elsif Is_Array_Type (Typ) then
         declare
            Actual_Rhs : Node_Id := Rhs;

         begin
            while Nkind_In (Actual_Rhs, N_Type_Conversion,
                                        N_Qualified_Expression)
            loop
               Actual_Rhs := Expression (Actual_Rhs);
            end loop;

            Expand_Assign_Array (N, Actual_Rhs);
            return;
         end;

      --  Record types

      elsif Is_Record_Type (Typ) then
         Expand_Assign_Record (N);
         return;

      --  Scalar types. This is where we perform the processing related to the
      --  requirements of (RM 13.9.1(9-11)) concerning the handling of invalid
      --  scalar values.

      elsif Is_Scalar_Type (Typ) then

         --  Case where right side is known valid

         if Expr_Known_Valid (Rhs) then

            --  Here the right side is valid, so it is fine. The case to deal
            --  with is when the left side is a local variable reference whose
            --  value is not currently known to be valid. If this is the case,
            --  and the assignment appears in an unconditional context, then
            --  we can mark the left side as now being valid if one of these
            --  conditions holds:

            --    The expression of the right side has Do_Range_Check set so
            --    that we know a range check will be performed. Note that it
            --    can be the case that a range check is omitted because we
            --    make the assumption that we can assume validity for operands
            --    appearing in the right side in determining whether a range
            --    check is required

            --    The subtype of the right side matches the subtype of the
            --    left side. In this case, even though we have not checked
            --    the range of the right side, we know it is in range of its
            --    subtype if the expression is valid.

            if Is_Local_Variable_Reference (Lhs)
              and then not Is_Known_Valid (Entity (Lhs))
              and then In_Unconditional_Context (N)
            then
               if Do_Range_Check (Rhs)
                 or else Etype (Lhs) = Etype (Rhs)
               then
                  Set_Is_Known_Valid (Entity (Lhs), True);
               end if;
            end if;

         --  Case where right side may be invalid in the sense of the RM
         --  reference above. The RM does not require that we check for the
         --  validity on an assignment, but it does require that the assignment
         --  of an invalid value not cause erroneous behavior.

         --  The general approach in GNAT is to use the Is_Known_Valid flag
         --  to avoid the need for validity checking on assignments. However
         --  in some cases, we have to do validity checking in order to make
         --  sure that the setting of this flag is correct.

         else
            --  Validate right side if we are validating copies

            if Validity_Checks_On
              and then Validity_Check_Copies
            then
               --  Skip this if left hand side is an array or record component
               --  and elementary component validity checks are suppressed.

               if Nkind_In (Lhs, N_Selected_Component, N_Indexed_Component)
                 and then not Validity_Check_Components
               then
                  null;
               else
                  Ensure_Valid (Rhs);
               end if;

               --  We can propagate this to the left side where appropriate

               if Is_Local_Variable_Reference (Lhs)
                 and then not Is_Known_Valid (Entity (Lhs))
                 and then In_Unconditional_Context (N)
               then
                  Set_Is_Known_Valid (Entity (Lhs), True);
               end if;

            --  Otherwise check to see what should be done

            --  If left side is a local variable, then we just set its flag to
            --  indicate that its value may no longer be valid, since we are
            --  copying a potentially invalid value.

            elsif Is_Local_Variable_Reference (Lhs) then
               Set_Is_Known_Valid (Entity (Lhs), False);

            --  Check for case of a nonlocal variable on the left side which
            --  is currently known to be valid. In this case, we simply ensure
            --  that the right side is valid. We only play the game of copying
            --  validity status for local variables, since we are doing this
            --  statically, not by tracing the full flow graph.

            elsif Is_Entity_Name (Lhs)
              and then Is_Known_Valid (Entity (Lhs))
            then
               --  Note: If Validity_Checking mode is set to none, we ignore
               --  the Ensure_Valid call so don't worry about that case here.

               Ensure_Valid (Rhs);

            --  In all other cases, we can safely copy an invalid value without
            --  worrying about the status of the left side. Since it is not a
            --  variable reference it will not be considered
            --  as being known to be valid in any case.

            else
               null;
            end if;
         end if;
      end if;

   exception
      when RE_Not_Available =>
         return;
   end Expand_N_Assignment_Statement;

   ------------------------------
   -- Expand_N_Block_Statement --
   ------------------------------

   --  Encode entity names defined in block statement

   procedure Expand_N_Block_Statement (N : Node_Id) is
   begin
      Qualify_Entity_Names (N);
   end Expand_N_Block_Statement;

   -----------------------------
   -- Expand_N_Case_Statement --
   -----------------------------

   procedure Expand_N_Case_Statement (N : Node_Id) is
      Loc    : constant Source_Ptr := Sloc (N);
      Expr   : constant Node_Id    := Expression (N);
      Alt    : Node_Id;
      Len    : Nat;
      Cond   : Node_Id;
      Choice : Node_Id;
      Chlist : List_Id;

   begin
      --  Check for the situation where we know at compile time which branch
      --  will be taken

      if Compile_Time_Known_Value (Expr) then
         Alt := Find_Static_Alternative (N);

         Process_Statements_For_Controlled_Objects (Alt);

         --  Move statements from this alternative after the case statement.
         --  They are already analyzed, so will be skipped by the analyzer.

         Insert_List_After (N, Statements (Alt));

         --  That leaves the case statement as a shell. So now we can kill all
         --  other alternatives in the case statement.

         Kill_Dead_Code (Expression (N));

         declare
            Dead_Alt : Node_Id;

         begin
            --  Loop through case alternatives, skipping pragmas, and skipping
            --  the one alternative that we select (and therefore retain).

            Dead_Alt := First (Alternatives (N));
            while Present (Dead_Alt) loop
               if Dead_Alt /= Alt
                 and then Nkind (Dead_Alt) = N_Case_Statement_Alternative
               then
                  Kill_Dead_Code (Statements (Dead_Alt), Warn_On_Deleted_Code);
               end if;

               Next (Dead_Alt);
            end loop;
         end;

         Rewrite (N, Make_Null_Statement (Loc));
         return;
      end if;

      --  Here if the choice is not determined at compile time

      declare
         Last_Alt : constant Node_Id := Last (Alternatives (N));

         Others_Present : Boolean;
         Others_Node    : Node_Id;

         Then_Stms : List_Id;
         Else_Stms : List_Id;

      begin
         if Nkind (First (Discrete_Choices (Last_Alt))) = N_Others_Choice then
            Others_Present := True;
            Others_Node    := Last_Alt;
         else
            Others_Present := False;
         end if;

         --  First step is to worry about possible invalid argument. The RM
         --  requires (RM 5.4(13)) that if the result is invalid (e.g. it is
         --  outside the base range), then Constraint_Error must be raised.

         --  Case of validity check required (validity checks are on, the
         --  expression is not known to be valid, and the case statement
         --  comes from source -- no need to validity check internally
         --  generated case statements).

         if Validity_Check_Default then
            Ensure_Valid (Expr);
         end if;

         --  If there is only a single alternative, just replace it with the
         --  sequence of statements since obviously that is what is going to
         --  be executed in all cases.

         Len := List_Length (Alternatives (N));

         if Len = 1 then

            --  We still need to evaluate the expression if it has any side
            --  effects.

            Remove_Side_Effects (Expression (N));

            Alt := First (Alternatives (N));

            Process_Statements_For_Controlled_Objects (Alt);
            Insert_List_After (N, Statements (Alt));

            --  That leaves the case statement as a shell. The alternative that
            --  will be executed is reset to a null list. So now we can kill
            --  the entire case statement.

            Kill_Dead_Code (Expression (N));
            Rewrite (N, Make_Null_Statement (Loc));
            return;

         --  An optimization. If there are only two alternatives, and only
         --  a single choice, then rewrite the whole case statement as an
         --  if statement, since this can result in subsequent optimizations.
         --  This helps not only with case statements in the source of a
         --  simple form, but also with generated code (discriminant check
         --  functions in particular)

         elsif Len = 2 then
            Chlist := Discrete_Choices (First (Alternatives (N)));

            if List_Length (Chlist) = 1 then
               Choice := First (Chlist);

               Then_Stms := Statements (First (Alternatives (N)));
               Else_Stms := Statements (Last  (Alternatives (N)));

               --  For TRUE, generate "expression", not expression = true

               if Nkind (Choice) = N_Identifier
                 and then Entity (Choice) = Standard_True
               then
                  Cond := Expression (N);

               --  For FALSE, generate "expression" and switch then/else

               elsif Nkind (Choice) = N_Identifier
                 and then Entity (Choice) = Standard_False
               then
                  Cond := Expression (N);
                  Else_Stms := Statements (First (Alternatives (N)));
                  Then_Stms := Statements (Last  (Alternatives (N)));

               --  For a range, generate "expression in range"

               elsif Nkind (Choice) = N_Range
                 or else (Nkind (Choice) = N_Attribute_Reference
                           and then Attribute_Name (Choice) = Name_Range)
                 or else (Is_Entity_Name (Choice)
                           and then Is_Type (Entity (Choice)))
                 or else Nkind (Choice) = N_Subtype_Indication
               then
                  Cond :=
                    Make_In (Loc,
                      Left_Opnd  => Expression (N),
                      Right_Opnd => Relocate_Node (Choice));

               --  For any other subexpression "expression = value"

               else
                  Cond :=
                    Make_Op_Eq (Loc,
                      Left_Opnd  => Expression (N),
                      Right_Opnd => Relocate_Node (Choice));
               end if;

               --  Now rewrite the case as an IF

               Rewrite (N,
                 Make_If_Statement (Loc,
                   Condition => Cond,
                   Then_Statements => Then_Stms,
                   Else_Statements => Else_Stms));
               Analyze (N);
               return;
            end if;
         end if;

         --  If the last alternative is not an Others choice, replace it with
         --  an N_Others_Choice. Note that we do not bother to call Analyze on
         --  the modified case statement, since it's only effect would be to
         --  compute the contents of the Others_Discrete_Choices which is not
         --  needed by the back end anyway.

         --  The reason we do this is that the back end always needs some
         --  default for a switch, so if we have not supplied one in the
         --  processing above for validity checking, then we need to supply
         --  one here.

         if not Others_Present then
            Others_Node := Make_Others_Choice (Sloc (Last_Alt));
            Set_Others_Discrete_Choices
              (Others_Node, Discrete_Choices (Last_Alt));
            Set_Discrete_Choices (Last_Alt, New_List (Others_Node));
         end if;

         Alt := First (Alternatives (N));
         while Present (Alt)
           and then Nkind (Alt) = N_Case_Statement_Alternative
         loop
            Process_Statements_For_Controlled_Objects (Alt);
            Next (Alt);
         end loop;
      end;
   end Expand_N_Case_Statement;

   -----------------------------
   -- Expand_N_Exit_Statement --
   -----------------------------

   --  The only processing required is to deal with a possible C/Fortran
   --  boolean value used as the condition for the exit statement.

   procedure Expand_N_Exit_Statement (N : Node_Id) is
   begin
      Adjust_Condition (Condition (N));
   end Expand_N_Exit_Statement;

   -----------------------------
   -- Expand_N_Goto_Statement --
   -----------------------------

   --  Add poll before goto if polling active

   procedure Expand_N_Goto_Statement (N : Node_Id) is
   begin
      Generate_Poll_Call (N);
   end Expand_N_Goto_Statement;

   ---------------------------
   -- Expand_N_If_Statement --
   ---------------------------

   --  First we deal with the case of C and Fortran convention boolean values,
   --  with zero/non-zero semantics.

   --  Second, we deal with the obvious rewriting for the cases where the
   --  condition of the IF is known at compile time to be True or False.

   --  Third, we remove elsif parts which have non-empty Condition_Actions and
   --  rewrite as independent if statements. For example:

   --     if x then xs
   --     elsif y then ys
   --     ...
   --     end if;

   --  becomes
   --
   --     if x then xs
   --     else
   --        <<condition actions of y>>
   --        if y then ys
   --        ...
   --        end if;
   --     end if;

   --  This rewriting is needed if at least one elsif part has a non-empty
   --  Condition_Actions list. We also do the same processing if there is a
   --  constant condition in an elsif part (in conjunction with the first
   --  processing step mentioned above, for the recursive call made to deal
   --  with the created inner if, this deals with properly optimizing the
   --  cases of constant elsif conditions).

   procedure Expand_N_If_Statement (N : Node_Id) is
      Loc    : constant Source_Ptr := Sloc (N);
      Hed    : Node_Id;
      E      : Node_Id;
      New_If : Node_Id;

      Warn_If_Deleted : constant Boolean :=
                          Warn_On_Deleted_Code and then Comes_From_Source (N);
      --  Indicates whether we want warnings when we delete branches of the
      --  if statement based on constant condition analysis. We never want
      --  these warnings for expander generated code.

   begin
      Process_Statements_For_Controlled_Objects (N);

      Adjust_Condition (Condition (N));

      --  The following loop deals with constant conditions for the IF. We
      --  need a loop because as we eliminate False conditions, we grab the
      --  first elsif condition and use it as the primary condition.

      while Compile_Time_Known_Value (Condition (N)) loop

         --  If condition is True, we can simply rewrite the if statement now
         --  by replacing it by the series of then statements.

         if Is_True (Expr_Value (Condition (N))) then

            --  All the else parts can be killed

            Kill_Dead_Code (Elsif_Parts (N), Warn_If_Deleted);
            Kill_Dead_Code (Else_Statements (N), Warn_If_Deleted);

            Hed := Remove_Head (Then_Statements (N));
            Insert_List_After (N, Then_Statements (N));
            Rewrite (N, Hed);
            return;

         --  If condition is False, then we can delete the condition and
         --  the Then statements

         else
            --  We do not delete the condition if constant condition warnings
            --  are enabled, since otherwise we end up deleting the desired
            --  warning. Of course the backend will get rid of this True/False
            --  test anyway, so nothing is lost here.

            if not Constant_Condition_Warnings then
               Kill_Dead_Code (Condition (N));
            end if;

            Kill_Dead_Code (Then_Statements (N), Warn_If_Deleted);

            --  If there are no elsif statements, then we simply replace the
            --  entire if statement by the sequence of else statements.

            if No (Elsif_Parts (N)) then
               if No (Else_Statements (N))
                 or else Is_Empty_List (Else_Statements (N))
               then
                  Rewrite (N,
                    Make_Null_Statement (Sloc (N)));
               else
                  Hed := Remove_Head (Else_Statements (N));
                  Insert_List_After (N, Else_Statements (N));
                  Rewrite (N, Hed);
               end if;

               return;

            --  If there are elsif statements, the first of them becomes the
            --  if/then section of the rebuilt if statement This is the case
            --  where we loop to reprocess this copied condition.

            else
               Hed := Remove_Head (Elsif_Parts (N));
               Insert_Actions      (N, Condition_Actions (Hed));
               Set_Condition       (N, Condition (Hed));
               Set_Then_Statements (N, Then_Statements (Hed));

               --  Hed might have been captured as the condition determining
               --  the current value for an entity. Now it is detached from
               --  the tree, so a Current_Value pointer in the condition might
               --  need to be updated.

               Set_Current_Value_Condition (N);

               if Is_Empty_List (Elsif_Parts (N)) then
                  Set_Elsif_Parts (N, No_List);
               end if;
            end if;
         end if;
      end loop;

      --  Loop through elsif parts, dealing with constant conditions and
      --  possible expression actions that are present.

      if Present (Elsif_Parts (N)) then
         E := First (Elsif_Parts (N));
         while Present (E) loop
            Process_Statements_For_Controlled_Objects (E);

            Adjust_Condition (Condition (E));

            --  If there are condition actions, then rewrite the if statement
            --  as indicated above. We also do the same rewrite for a True or
            --  False condition. The further processing of this constant
            --  condition is then done by the recursive call to expand the
            --  newly created if statement

            if Present (Condition_Actions (E))
              or else Compile_Time_Known_Value (Condition (E))
            then
               --  Note this is not an implicit if statement, since it is part
               --  of an explicit if statement in the source (or of an implicit
               --  if statement that has already been tested).

               New_If :=
                 Make_If_Statement (Sloc (E),
                   Condition       => Condition (E),
                   Then_Statements => Then_Statements (E),
                   Elsif_Parts     => No_List,
                   Else_Statements => Else_Statements (N));

               --  Elsif parts for new if come from remaining elsif's of parent

               while Present (Next (E)) loop
                  if No (Elsif_Parts (New_If)) then
                     Set_Elsif_Parts (New_If, New_List);
                  end if;

                  Append (Remove_Next (E), Elsif_Parts (New_If));
               end loop;

               Set_Else_Statements (N, New_List (New_If));

               if Present (Condition_Actions (E)) then
                  Insert_List_Before (New_If, Condition_Actions (E));
               end if;

               Remove (E);

               if Is_Empty_List (Elsif_Parts (N)) then
                  Set_Elsif_Parts (N, No_List);
               end if;

               Analyze (New_If);
               return;

            --  No special processing for that elsif part, move to next

            else
               Next (E);
            end if;
         end loop;
      end if;

      --  Some more optimizations applicable if we still have an IF statement

      if Nkind (N) /= N_If_Statement then
         return;
      end if;

      --  Another optimization, special cases that can be simplified

      --     if expression then
      --        return true;
      --     else
      --        return false;
      --     end if;

      --  can be changed to:

      --     return expression;

      --  and

      --     if expression then
      --        return false;
      --     else
      --        return true;
      --     end if;

      --  can be changed to:

      --     return not (expression);

      --  Only do these optimizations if we are at least at -O1 level and
      --  do not do them if control flow optimizations are suppressed.

      if Optimization_Level > 0
        and then not Opt.Suppress_Control_Flow_Optimizations
      then
         if Nkind (N) = N_If_Statement
           and then No (Elsif_Parts (N))
           and then Present (Else_Statements (N))
           and then List_Length (Then_Statements (N)) = 1
           and then List_Length (Else_Statements (N)) = 1
         then
            declare
               Then_Stm : constant Node_Id := First (Then_Statements (N));
               Else_Stm : constant Node_Id := First (Else_Statements (N));

            begin
               if Nkind (Then_Stm) = N_Simple_Return_Statement
                    and then
                  Nkind (Else_Stm) = N_Simple_Return_Statement
               then
                  declare
                     Then_Expr : constant Node_Id := Expression (Then_Stm);
                     Else_Expr : constant Node_Id := Expression (Else_Stm);

                  begin
                     if Nkind (Then_Expr) = N_Identifier
                          and then
                        Nkind (Else_Expr) = N_Identifier
                     then
                        if Entity (Then_Expr) = Standard_True
                          and then Entity (Else_Expr) = Standard_False
                        then
                           Rewrite (N,
                             Make_Simple_Return_Statement (Loc,
                               Expression => Relocate_Node (Condition (N))));
                           Analyze (N);
                           return;

                        elsif Entity (Then_Expr) = Standard_False
                          and then Entity (Else_Expr) = Standard_True
                        then
                           Rewrite (N,
                             Make_Simple_Return_Statement (Loc,
                               Expression =>
                                 Make_Op_Not (Loc,
                                   Right_Opnd =>
                                     Relocate_Node (Condition (N)))));
                           Analyze (N);
                           return;
                        end if;
                     end if;
                  end;
               end if;
            end;
         end if;
      end if;
   end Expand_N_If_Statement;

   --------------------------
   -- Expand_Iterator_Loop --
   --------------------------

   procedure Expand_Iterator_Loop (N : Node_Id) is
      Isc    : constant Node_Id    := Iteration_Scheme (N);
      I_Spec : constant Node_Id    := Iterator_Specification (Isc);
      Id     : constant Entity_Id  := Defining_Identifier (I_Spec);
      Loc    : constant Source_Ptr := Sloc (N);

      Container     : constant Node_Id   := Name (I_Spec);
      Container_Typ : constant Entity_Id := Base_Type (Etype (Container));
      Cursor        : Entity_Id;
      Iterator      : Entity_Id;
      New_Loop      : Node_Id;
      Stats         : List_Id := Statements (N);

   begin
      --  Processing for arrays

      if Is_Array_Type (Container_Typ) then

         --  for Element of Array loop
         --
         --  This case requires an internally generated cursor to iterate over
         --  the array.

         if Of_Present (I_Spec) then
            Iterator := Make_Temporary (Loc, 'C');

            --  Generate:
            --    Element : Component_Type renames Container (Iterator);

            Prepend_To (Stats,
              Make_Object_Renaming_Declaration (Loc,
                Defining_Identifier => Id,
                Subtype_Mark =>
                  New_Reference_To (Component_Type (Container_Typ), Loc),
                Name =>
                  Make_Indexed_Component (Loc,
                    Prefix => Relocate_Node (Container),
                    Expressions => New_List (
                      New_Reference_To (Iterator, Loc)))));

         --  for Index in Array loop

         --  This case utilizes the already given iterator name

         else
            Iterator := Id;
         end if;

         --  Generate:
         --    for Iterator in [reverse] Container'Range loop
         --       Element : Component_Type renames Container (Iterator);
         --       --  for the "of" form

         --       <original loop statements>
         --    end loop;

         New_Loop :=
           Make_Loop_Statement (Loc,
             Iteration_Scheme =>
               Make_Iteration_Scheme (Loc,
                 Loop_Parameter_Specification =>
                   Make_Loop_Parameter_Specification (Loc,
                     Defining_Identifier => Iterator,
                       Discrete_Subtype_Definition =>
                         Make_Attribute_Reference (Loc,
                           Prefix => Relocate_Node (Container),
                           Attribute_Name => Name_Range),
                      Reverse_Present => Reverse_Present (I_Spec))),
              Statements => Stats,
              End_Label  => Empty);

      --  Processing for containers

      else
         --  For an "of" iterator the name is a container expression, which
         --  is transformed into a call to the default iterator.

         --  For an iterator of the form "in" the name is a function call
         --  that delivers an iterator type.

         --  In both cases, analysis of the iterator has introduced an object
         --  declaration to capture the domain, so that Container is an entity.

         --  The for loop is expanded into a while loop which uses a container
         --  specific cursor to desgnate each element.

         --    Iter : Iterator_Type := Container.Iterate;
         --    Cursor : Cursor_type := First (Iter);
         --    while Has_Element (Iter) loop
         --       declare
         --       --  the block is added when Element_Type is controlled

         --          Obj : Pack.Element_Type := Element (Cursor);
         --          --  for the "of" loop form
         --       begin
         --          <original loop statements>
         --       end;

         --       Cursor := Iter.Next (Cursor);
         --    end loop;

         --  If "reverse" is present, then the initialization of the cursor
         --  uses Last and the step becomes Prev. Pack is the name of the
         --  scope where the container package is instantiated.

         declare
            Element_Type : constant Entity_Id := Etype (Id);
            Iter_Type    : Entity_Id;
            Pack         : Entity_Id;
            Decl         : Node_Id;
            Name_Init    : Name_Id;
            Name_Step    : Name_Id;

         begin
            --  The type of the iterator is the return type of the Iterate
            --  function used. For the "of" form this is the default iterator
            --  for the type, otherwise it is the type of the explicit
            --  function used in the iterator specification. The most common
            --  case will be an Iterate function in the container package.

            --  The primitive operations of the container type may not be
            --  use-visible, so we introduce the name of the enclosing package
            --  in the declarations below. The Iterator type is declared in a
            --  an instance within the container package itself.

            --  If the container type is a derived type, the cursor type is
            --  found in the package of the parent type.

            if Is_Derived_Type (Container_Typ) then
               Pack := Scope (Root_Type (Container_Typ));
            else
               Pack := Scope (Container_Typ);
            end if;

            Iter_Type := Etype (Name (I_Spec));

            if Is_Iterator (Iter_Type) then
               Pack := Scope (Pack);
            end if;

            --  The "of" case uses an internally generated cursor whose type
            --  is found in the container package. The domain of iteration
            --  is expanded into a call to the default Iterator function, but
            --  this expansion does not take place in a quantifier expressions
            --  that are analyzed with expansion disabled, and in that case the
            --  type of the iterator must be obtained from the aspect.

            if Of_Present (I_Spec) then
               declare
                  Default_Iter : constant Entity_Id :=
                                   Entity
                                     (Find_Aspect
                                       (Etype (Container),
                                        Aspect_Default_Iterator));

                  Container_Arg : Node_Id;
                  Ent           : Entity_Id;

               begin
                  Cursor := Make_Temporary (Loc, 'I');

                  if Is_Iterator (Iter_Type) then
                     null;

                  else
                     Iter_Type := Etype (Default_Iter);

                     --  Rewrite domain of iteration as a call to the default
                     --  iterator for the container type. If the container is
                     --  a derived type and the aspect is inherited, convert
                     --  container to parent type. The Cursor type is also
                     --  inherited from the scope of the parent.

                     if Base_Type (Etype (Container)) =
                        Base_Type (Etype (First_Formal (Default_Iter)))
                     then
                        Container_Arg := New_Copy_Tree (Container);

                     else
                        Container_Arg :=
                          Make_Type_Conversion (Loc,
                            Subtype_Mark =>
                              New_Occurrence_Of
                                (Etype (First_Formal (Default_Iter)), Loc),
                            Expression => New_Copy_Tree (Container));
                     end if;

                     Rewrite (Name (I_Spec),
                       Make_Function_Call (Loc,
                         Name => New_Occurrence_Of (Default_Iter, Loc),
                         Parameter_Associations =>
                           New_List (Container_Arg)));
                     Analyze_And_Resolve (Name (I_Spec));
                  end if;

                  --  Find cursor type in proper container package.

                  Ent := First_Entity (Pack);
                  while Present (Ent) loop
                     if Chars (Ent) = Name_Cursor then
                        Set_Etype (Cursor, Etype (Ent));
                        exit;
                     end if;
                     Next_Entity (Ent);
                  end loop;

                  --  Generate:
                  --    Id : Element_Type renames Container (Cursor);
                  --  This assumes that the container type has an indexing
                  --  operation with Cursor. The check that this operation
                  --  exists is performed in Check_Container_Indexing.

                  Decl :=
                    Make_Object_Renaming_Declaration (Loc,
                      Defining_Identifier => Id,
                      Subtype_Mark     =>
                        New_Reference_To (Element_Type, Loc),
                      Name             =>
                        Make_Indexed_Component (Loc,
                          Prefix      => Relocate_Node (Container_Arg),
                          Expressions =>
                            New_List (New_Occurrence_Of (Cursor, Loc))));

                  --  If the container holds controlled objects, wrap the loop
                  --  statements and element renaming declaration with a block.
                  --  This ensures that the result of Element (Cusor) is
                  --  cleaned up after each iteration of the loop.

                  if Needs_Finalization (Element_Type) then

                     --  Generate:
                     --    declare
                     --       Id : Element_Type := Pack.Element (curosr);
                     --    begin
                     --       <original loop statements>
                     --    end;

                     Stats := New_List (
                       Make_Block_Statement (Loc,
                         Declarations               => New_List (Decl),
                         Handled_Statement_Sequence =>
                           Make_Handled_Sequence_Of_Statements (Loc,
                              Statements => Stats)));

                  --  Elements do not need finalization

                  else
                     Prepend_To (Stats, Decl);
                  end if;
               end;

            --  X in Iterate (S) : type of iterator is type of explicitly
            --  given Iterate function, and the loop variable is the cursor.
            --  It will be assigned in the loop and must be a variable.

            else
               Cursor := Id;
               Set_Ekind (Cursor, E_Variable);
            end if;

            Iterator := Make_Temporary (Loc, 'I');

            --  Determine the advancement and initialization steps for the
            --  cursor.

            --  Analysis of the expanded loop will verify that the container
            --  has a reverse iterator.

            if Reverse_Present (I_Spec) then
               Name_Init := Name_Last;
               Name_Step := Name_Previous;

            else
               Name_Init := Name_First;
               Name_Step := Name_Next;
            end if;

            --  For both iterator forms, add a call to the step operation to
            --  advance the cursor. Generate:

            --     Cursor := Iterator.Next (Cursor);

            --   or else

            --     Cursor := Next (Cursor);

            declare
               Rhs : Node_Id;

            begin
               Rhs :=
                 Make_Function_Call (Loc,
                   Name                   =>
                     Make_Selected_Component (Loc,
                       Prefix        => New_Reference_To (Iterator, Loc),
                       Selector_Name => Make_Identifier (Loc, Name_Step)),
                   Parameter_Associations => New_List (
                      New_Reference_To (Cursor, Loc)));

               Append_To (Stats,
                 Make_Assignment_Statement (Loc,
                    Name       => New_Occurrence_Of (Cursor, Loc),
                    Expression => Rhs));
            end;

            --  Generate:
            --    while Iterator.Has_Element loop
            --       <Stats>
            --    end loop;

            New_Loop :=
              Make_Loop_Statement (Loc,
                Iteration_Scheme =>
                  Make_Iteration_Scheme (Loc,
                    Condition =>
                      Make_Function_Call (Loc,
                        Name                   =>
                          Make_Selected_Component (Loc,
                           Prefix => New_Occurrence_Of (Pack, Loc),
                           Selector_Name =>
                             Make_Identifier (Loc, Name_Has_Element)),

                        Parameter_Associations =>
                          New_List (
                            New_Reference_To (Cursor, Loc)))),
                Statements => Stats,
                End_Label  => Empty);

            --  Create the declarations for Iterator and cursor and insert then
            --  before the source loop. Given that the domain of iteration is
            --  already an entity, the iterator is just a renaming of that
            --  entity. Possible optimization ???
            --  Generate:

            --    I : Iterator_Type renames Container;
            --    C : Pack.Cursor_Type := Container.[First | Last];

            Insert_Action (N,
              Make_Object_Renaming_Declaration (Loc,
                Defining_Identifier => Iterator,
                Subtype_Mark  => New_Occurrence_Of (Iter_Type, Loc),
                Name          => Relocate_Node (Name (I_Spec))));

            --  Create declaration for cursor

            declare
               Decl : Node_Id;

            begin
               Decl :=
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Cursor,
                   Object_Definition   =>
                     New_Occurrence_Of (Etype (Cursor), Loc),
                   Expression          =>
                     Make_Selected_Component (Loc,
                       Prefix        => New_Reference_To (Iterator, Loc),
                       Selector_Name =>
                         Make_Identifier (Loc, Name_Init)));

               --  The cursor is only modified in expanded code, so it appears
               --  as unassigned to the warning machinery. We must suppress
               --  this spurious warning explicitly.

               Set_Warnings_Off (Cursor);
               Set_Assignment_OK (Decl);

               Insert_Action (N, Decl);
            end;

            --  If the range of iteration is given by a function call that
            --  returns a container, the finalization actions have been saved
            --  in the Condition_Actions of the iterator. Insert them now at
            --  the head of the loop.

            if Present (Condition_Actions (Isc)) then
               Insert_List_Before (N, Condition_Actions (Isc));
            end if;
         end;
      end if;

      Rewrite (N, New_Loop);
      Analyze (N);
   end Expand_Iterator_Loop;

   -----------------------------
   -- Expand_N_Loop_Statement --
   -----------------------------

   --  1. Remove null loop entirely
   --  2. Deal with while condition for C/Fortran boolean
   --  3. Deal with loops with a non-standard enumeration type range
   --  4. Deal with while loops where Condition_Actions is set
   --  5. Deal with loops over predicated subtypes
   --  6. Deal with loops with iterators over arrays and containers
   --  7. Insert polling call if required

   procedure Expand_N_Loop_Statement (N : Node_Id) is
      Loc  : constant Source_Ptr := Sloc (N);
      Isc  : constant Node_Id    := Iteration_Scheme (N);

   begin
      --  Delete null loop

      if Is_Null_Loop (N) then
         Rewrite (N, Make_Null_Statement (Loc));
         return;
      end if;

      Process_Statements_For_Controlled_Objects (N);

      --  Deal with condition for C/Fortran Boolean

      if Present (Isc) then
         Adjust_Condition (Condition (Isc));
      end if;

      --  Generate polling call

      if Is_Non_Empty_List (Statements (N)) then
         Generate_Poll_Call (First (Statements (N)));
      end if;

      --  Nothing more to do for plain loop with no iteration scheme

      if No (Isc) then
         null;

      --  Case of for loop (Loop_Parameter_Specification present)

      --  Note: we do not have to worry about validity checking of the for loop
      --  range bounds here, since they were frozen with constant declarations
      --  and it is during that process that the validity checking is done.

      elsif Present (Loop_Parameter_Specification (Isc)) then
         declare
            LPS     : constant Node_Id   := Loop_Parameter_Specification (Isc);
            Loop_Id : constant Entity_Id := Defining_Identifier (LPS);
            Ltype   : constant Entity_Id := Etype (Loop_Id);
            Btype   : constant Entity_Id := Base_Type (Ltype);
            Expr    : Node_Id;
            New_Id  : Entity_Id;

         begin
            --  Deal with loop over predicates

            if Is_Discrete_Type (Ltype)
              and then Present (Predicate_Function (Ltype))
            then
               Expand_Predicated_Loop (N);

            --  Handle the case where we have a for loop with the range type
            --  being an enumeration type with non-standard representation.
            --  In this case we expand:

            --    for x in [reverse] a .. b loop
            --       ...
            --    end loop;

            --  to

            --    for xP in [reverse] integer
            --      range etype'Pos (a) .. etype'Pos (b)
            --    loop
            --       declare
            --          x : constant etype := Pos_To_Rep (xP);
            --       begin
            --          ...
            --       end;
            --    end loop;

            elsif Is_Enumeration_Type (Btype)
              and then Present (Enum_Pos_To_Rep (Btype))
            then
               New_Id :=
                 Make_Defining_Identifier (Loc,
                   Chars => New_External_Name (Chars (Loop_Id), 'P'));

               --  If the type has a contiguous representation, successive
               --  values can be generated as offsets from the first literal.

               if Has_Contiguous_Rep (Btype) then
                  Expr :=
                     Unchecked_Convert_To (Btype,
                       Make_Op_Add (Loc,
                         Left_Opnd =>
                            Make_Integer_Literal (Loc,
                              Enumeration_Rep (First_Literal (Btype))),
                         Right_Opnd => New_Reference_To (New_Id, Loc)));
               else
                  --  Use the constructed array Enum_Pos_To_Rep

                  Expr :=
                    Make_Indexed_Component (Loc,
                      Prefix      =>
                        New_Reference_To (Enum_Pos_To_Rep (Btype), Loc),
                      Expressions =>
                        New_List (New_Reference_To (New_Id, Loc)));
               end if;

               Rewrite (N,
                 Make_Loop_Statement (Loc,
                   Identifier => Identifier (N),

                   Iteration_Scheme =>
                     Make_Iteration_Scheme (Loc,
                       Loop_Parameter_Specification =>
                         Make_Loop_Parameter_Specification (Loc,
                           Defining_Identifier => New_Id,
                           Reverse_Present => Reverse_Present (LPS),

                           Discrete_Subtype_Definition =>
                             Make_Subtype_Indication (Loc,

                               Subtype_Mark =>
                                 New_Reference_To (Standard_Natural, Loc),

                               Constraint =>
                                 Make_Range_Constraint (Loc,
                                   Range_Expression =>
                                     Make_Range (Loc,

                                       Low_Bound =>
                                         Make_Attribute_Reference (Loc,
                                           Prefix =>
                                             New_Reference_To (Btype, Loc),

                                           Attribute_Name => Name_Pos,

                                           Expressions => New_List (
                                             Relocate_Node
                                               (Type_Low_Bound (Ltype)))),

                                       High_Bound =>
                                         Make_Attribute_Reference (Loc,
                                           Prefix =>
                                             New_Reference_To (Btype, Loc),

                                           Attribute_Name => Name_Pos,

                                           Expressions => New_List (
                                             Relocate_Node
                                               (Type_High_Bound
                                                  (Ltype))))))))),

                   Statements => New_List (
                     Make_Block_Statement (Loc,
                       Declarations => New_List (
                         Make_Object_Declaration (Loc,
                           Defining_Identifier => Loop_Id,
                           Constant_Present    => True,
                           Object_Definition   =>
                             New_Reference_To (Ltype, Loc),
                           Expression          => Expr)),

                       Handled_Statement_Sequence =>
                         Make_Handled_Sequence_Of_Statements (Loc,
                           Statements => Statements (N)))),

                   End_Label => End_Label (N)));

               --  The loop parameter's entity must be removed from the loop
               --  scope's entity list, since it will now be located in the
               --  new block scope. Any other entities already associated with
               --  the loop scope, such as the loop parameter's subtype, will
               --  remain there.

               pragma Assert (First_Entity (Scope (Loop_Id)) = Loop_Id);
               Set_First_Entity (Scope (Loop_Id), Next_Entity (Loop_Id));

               if Last_Entity (Scope (Loop_Id)) = Loop_Id then
                  Set_Last_Entity (Scope (Loop_Id), Empty);
               end if;

               Analyze (N);

            --  Nothing to do with other cases of for loops

            else
               null;
            end if;
         end;

      --  Second case, if we have a while loop with Condition_Actions set, then
      --  we change it into a plain loop:

      --    while C loop
      --       ...
      --    end loop;

      --  changed to:

      --    loop
      --       <<condition actions>>
      --       exit when not C;
      --       ...
      --    end loop

      elsif Present (Isc)
        and then Present (Condition_Actions (Isc))
        and then Present (Condition (Isc))
      then
         declare
            ES : Node_Id;

         begin
            ES :=
              Make_Exit_Statement (Sloc (Condition (Isc)),
                Condition =>
                  Make_Op_Not (Sloc (Condition (Isc)),
                    Right_Opnd => Condition (Isc)));

            Prepend (ES, Statements (N));
            Insert_List_Before (ES, Condition_Actions (Isc));

            --  This is not an implicit loop, since it is generated in response
            --  to the loop statement being processed. If this is itself
            --  implicit, the restriction has already been checked. If not,
            --  it is an explicit loop.

            Rewrite (N,
              Make_Loop_Statement (Sloc (N),
                Identifier => Identifier (N),
                Statements => Statements (N),
                End_Label  => End_Label  (N)));

            Analyze (N);
         end;

      --  Here to deal with iterator case

      elsif Present (Isc)
        and then Present (Iterator_Specification (Isc))
      then
         Expand_Iterator_Loop (N);
      end if;
   end Expand_N_Loop_Statement;

   ----------------------------
   -- Expand_Predicated_Loop --
   ----------------------------

   --  Note: the expander can handle generation of loops over predicated
   --  subtypes for both the dynamic and static cases. Depending on what
   --  we decide is allowed in Ada 2012 mode and/or extensions allowed
   --  mode, the semantic analyzer may disallow one or both forms.

   procedure Expand_Predicated_Loop (N : Node_Id) is
      Loc     : constant Source_Ptr := Sloc (N);
      Isc     : constant Node_Id    := Iteration_Scheme (N);
      LPS     : constant Node_Id    := Loop_Parameter_Specification (Isc);
      Loop_Id : constant Entity_Id  := Defining_Identifier (LPS);
      Ltype   : constant Entity_Id  := Etype (Loop_Id);
      Stat    : constant List_Id    := Static_Predicate (Ltype);
      Stmts   : constant List_Id    := Statements (N);

   begin
      --  Case of iteration over non-static predicate, should not be possible
      --  since this is not allowed by the semantics and should have been
      --  caught during analysis of the loop statement.

      if No (Stat) then
         raise Program_Error;

      --  If the predicate list is empty, that corresponds to a predicate of
      --  False, in which case the loop won't run at all, and we rewrite the
      --  entire loop as a null statement.

      elsif Is_Empty_List (Stat) then
         Rewrite (N, Make_Null_Statement (Loc));
         Analyze (N);

      --  For expansion over a static predicate we generate the following

      --     declare
      --        J : Ltype := min-val;
      --     begin
      --        loop
      --           body
      --           case J is
      --              when endpoint => J := startpoint;
      --              when endpoint => J := startpoint;
      --              ...
      --              when max-val  => exit;
      --              when others   => J := Lval'Succ (J);
      --           end case;
      --        end loop;
      --     end;

      --  To make this a little clearer, let's take a specific example:

      --        type Int is range 1 .. 10;
      --        subtype L is Int with
      --          predicate => L in 3 | 10 | 5 .. 7;
      --          ...
      --        for L in StaticP loop
      --           Put_Line ("static:" & J'Img);
      --        end loop;

      --  In this case, the loop is transformed into

      --     begin
      --        J : L := 3;
      --        loop
      --           body
      --           case J is
      --              when 3  => J := 5;
      --              when 7  => J := 10;
      --              when 10 => exit;
      --              when others  => J := L'Succ (J);
      --           end case;
      --        end loop;
      --     end;

      else
         Static_Predicate : declare
            S    : Node_Id;
            D    : Node_Id;
            P    : Node_Id;
            Alts : List_Id;
            Cstm : Node_Id;

            function Lo_Val (N : Node_Id) return Node_Id;
            --  Given static expression or static range, returns an identifier
            --  whose value is the low bound of the expression value or range.

            function Hi_Val (N : Node_Id) return Node_Id;
            --  Given static expression or static range, returns an identifier
            --  whose value is the high bound of the expression value or range.

            ------------
            -- Hi_Val --
            ------------

            function Hi_Val (N : Node_Id) return Node_Id is
            begin
               if Is_Static_Expression (N) then
                  return New_Copy (N);
               else
                  pragma Assert (Nkind (N) = N_Range);
                  return New_Copy (High_Bound (N));
               end if;
            end Hi_Val;

            ------------
            -- Lo_Val --
            ------------

            function Lo_Val (N : Node_Id) return Node_Id is
            begin
               if Is_Static_Expression (N) then
                  return New_Copy (N);
               else
                  pragma Assert (Nkind (N) = N_Range);
                  return New_Copy (Low_Bound (N));
               end if;
            end Lo_Val;

         --  Start of processing for Static_Predicate

         begin
            --  Convert loop identifier to normal variable and reanalyze it so
            --  that this conversion works. We have to use the same defining
            --  identifier, since there may be references in the loop body.

            Set_Analyzed (Loop_Id, False);
            Set_Ekind    (Loop_Id, E_Variable);

            --  Loop to create branches of case statement

            Alts := New_List;
            P := First (Stat);
            while Present (P) loop
               if No (Next (P)) then
                  S := Make_Exit_Statement (Loc);
               else
                  S :=
                    Make_Assignment_Statement (Loc,
                      Name       => New_Occurrence_Of (Loop_Id, Loc),
                      Expression => Lo_Val (Next (P)));
                  Set_Suppress_Assignment_Checks (S);
               end if;

               Append_To (Alts,
                 Make_Case_Statement_Alternative (Loc,
                   Statements       => New_List (S),
                   Discrete_Choices => New_List (Hi_Val (P))));

               Next (P);
            end loop;

            --  Add others choice

            S :=
               Make_Assignment_Statement (Loc,
                 Name       => New_Occurrence_Of (Loop_Id, Loc),
                 Expression =>
                   Make_Attribute_Reference (Loc,
                     Prefix => New_Occurrence_Of (Ltype, Loc),
                     Attribute_Name => Name_Succ,
                     Expressions    => New_List (
                       New_Occurrence_Of (Loop_Id, Loc))));
            Set_Suppress_Assignment_Checks (S);

            Append_To (Alts,
              Make_Case_Statement_Alternative (Loc,
                Discrete_Choices => New_List (Make_Others_Choice (Loc)),
                Statements       => New_List (S)));

            --  Construct case statement and append to body statements

            Cstm :=
              Make_Case_Statement (Loc,
                Expression   => New_Occurrence_Of (Loop_Id, Loc),
                Alternatives => Alts);
            Append_To (Stmts, Cstm);

            --  Rewrite the loop

            D :=
               Make_Object_Declaration (Loc,
                 Defining_Identifier => Loop_Id,
                 Object_Definition   => New_Occurrence_Of (Ltype, Loc),
                 Expression          => Lo_Val (First (Stat)));
            Set_Suppress_Assignment_Checks (D);

            Rewrite (N,
              Make_Block_Statement (Loc,
                Declarations               => New_List (D),
                Handled_Statement_Sequence =>
                  Make_Handled_Sequence_Of_Statements (Loc,
                    Statements => New_List (
                      Make_Loop_Statement (Loc,
                        Statements => Stmts,
                        End_Label  => Empty)))));

            Analyze (N);
         end Static_Predicate;
      end if;
   end Expand_Predicated_Loop;

   ------------------------------
   -- Make_Tag_Ctrl_Assignment --
   ------------------------------

   function Make_Tag_Ctrl_Assignment (N : Node_Id) return List_Id is
      Asn : constant Node_Id    := Relocate_Node (N);
      L   : constant Node_Id    := Name (N);
      Loc : constant Source_Ptr := Sloc (N);
      Res : constant List_Id    := New_List;
      T   : constant Entity_Id  := Underlying_Type (Etype (L));

      Comp_Asn : constant Boolean := Is_Fully_Repped_Tagged_Type (T);
      Ctrl_Act : constant Boolean := Needs_Finalization (T)
                                       and then not No_Ctrl_Actions (N);
      Save_Tag : constant Boolean := Is_Tagged_Type (T)
                                       and then not Comp_Asn
                                       and then not No_Ctrl_Actions (N)
                                       and then Tagged_Type_Expansion;
      --  Tags are not saved and restored when VM_Target because VM tags are
      --  represented implicitly in objects.

      Next_Id : Entity_Id;
      Prev_Id : Entity_Id;
      Tag_Id  : Entity_Id;

   begin
      --  Finalize the target of the assignment when controlled

      --  We have two exceptions here:

      --   1. If we are in an init proc since it is an initialization more
      --      than an assignment.

      --   2. If the left-hand side is a temporary that was not initialized
      --      (or the parent part of a temporary since it is the case in
      --      extension aggregates). Such a temporary does not come from
      --      source. We must examine the original node for the prefix, because
      --      it may be a component of an entry formal, in which case it has
      --      been rewritten and does not appear to come from source either.

      --  Case of init proc

      if not Ctrl_Act then
         null;

      --  The left hand side is an uninitialized temporary object

      elsif Nkind (L) = N_Type_Conversion
        and then Is_Entity_Name (Expression (L))
        and then Nkind (Parent (Entity (Expression (L)))) =
                                              N_Object_Declaration
        and then No_Initialization (Parent (Entity (Expression (L))))
      then
         null;

      else
         Append_To (Res,
           Make_Final_Call
             (Obj_Ref => Duplicate_Subexpr_No_Checks (L),
              Typ     => Etype (L)));
      end if;

      --  Save the Tag in a local variable Tag_Id

      if Save_Tag then
         Tag_Id := Make_Temporary (Loc, 'A');

         Append_To (Res,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Tag_Id,
             Object_Definition   => New_Reference_To (RTE (RE_Tag), Loc),
             Expression          =>
               Make_Selected_Component (Loc,
                 Prefix        => Duplicate_Subexpr_No_Checks (L),
                 Selector_Name =>
                   New_Reference_To (First_Tag_Component (T), Loc))));

      --  Otherwise Tag_Id is not used

      else
         Tag_Id := Empty;
      end if;

      --  Save the Prev and Next fields on .NET/JVM. This is not needed on non
      --  VM targets since the fields are not part of the object.

      if VM_Target /= No_VM
        and then Is_Controlled (T)
      then
         Prev_Id := Make_Temporary (Loc, 'P');
         Next_Id := Make_Temporary (Loc, 'N');

         --  Generate:
         --    Pnn : Root_Controlled_Ptr := Root_Controlled (L).Prev;

         Append_To (Res,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Prev_Id,
             Object_Definition   =>
               New_Reference_To (RTE (RE_Root_Controlled_Ptr), Loc),
             Expression          =>
               Make_Selected_Component (Loc,
                 Prefix        =>
                   Unchecked_Convert_To
                     (RTE (RE_Root_Controlled), New_Copy_Tree (L)),
                 Selector_Name =>
                   Make_Identifier (Loc, Name_Prev))));

         --  Generate:
         --    Nnn : Root_Controlled_Ptr := Root_Controlled (L).Next;

         Append_To (Res,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Next_Id,
             Object_Definition   =>
               New_Reference_To (RTE (RE_Root_Controlled_Ptr), Loc),
             Expression          =>
               Make_Selected_Component (Loc,
                 Prefix        =>
                   Unchecked_Convert_To
                     (RTE (RE_Root_Controlled), New_Copy_Tree (L)),
                 Selector_Name =>
                   Make_Identifier (Loc, Name_Next))));
      end if;

      --  If the tagged type has a full rep clause, expand the assignment into
      --  component-wise assignments. Mark the node as unanalyzed in order to
      --  generate the proper code and propagate this scenario by setting a
      --  flag to avoid infinite recursion.

      if Comp_Asn then
         Set_Analyzed (Asn, False);
         Set_Componentwise_Assignment (Asn, True);
      end if;

      Append_To (Res, Asn);

      --  Restore the tag

      if Save_Tag then
         Append_To (Res,
           Make_Assignment_Statement (Loc,
             Name       =>
               Make_Selected_Component (Loc,
                 Prefix        => Duplicate_Subexpr_No_Checks (L),
                 Selector_Name =>
                   New_Reference_To (First_Tag_Component (T), Loc)),
             Expression => New_Reference_To (Tag_Id, Loc)));
      end if;

      --  Restore the Prev and Next fields on .NET/JVM

      if VM_Target /= No_VM
        and then Is_Controlled (T)
      then
         --  Generate:
         --    Root_Controlled (L).Prev := Prev_Id;

         Append_To (Res,
           Make_Assignment_Statement (Loc,
             Name       =>
               Make_Selected_Component (Loc,
                 Prefix        =>
                   Unchecked_Convert_To
                     (RTE (RE_Root_Controlled), New_Copy_Tree (L)),
                 Selector_Name =>
                   Make_Identifier (Loc, Name_Prev)),
             Expression => New_Reference_To (Prev_Id, Loc)));

         --  Generate:
         --    Root_Controlled (L).Next := Next_Id;

         Append_To (Res,
           Make_Assignment_Statement (Loc,
             Name       =>
               Make_Selected_Component (Loc,
                 Prefix        =>
                   Unchecked_Convert_To
                     (RTE (RE_Root_Controlled), New_Copy_Tree (L)),
                 Selector_Name => Make_Identifier (Loc, Name_Next)),
             Expression => New_Reference_To (Next_Id, Loc)));
      end if;

      --  Adjust the target after the assignment when controlled (not in the
      --  init proc since it is an initialization more than an assignment).

      if Ctrl_Act then
         Append_To (Res,
           Make_Adjust_Call
             (Obj_Ref => Duplicate_Subexpr_Move_Checks (L),
              Typ     => Etype (L)));
      end if;

      return Res;

   exception

      --  Could use comment here ???

      when RE_Not_Available =>
         return Empty_List;
   end Make_Tag_Ctrl_Assignment;

end Exp_Ch5;
