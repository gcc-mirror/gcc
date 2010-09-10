------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E X P _ C H 5                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2010, Free Software Foundation, Inc.         --
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
with Checks;   use Checks;
with Debug;    use Debug;
with Einfo;    use Einfo;
with Elists;   use Elists;
with Exp_Atag; use Exp_Atag;
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
with Ttypes;   use Ttypes;
with Uintp;    use Uintp;
with Validsw;  use Validsw;

package body Exp_Ch5 is

   function Change_Of_Representation (N : Node_Id) return Boolean;
   --  Determine if the right hand side of the assignment N is a type
   --  conversion which requires a change of representation. Called
   --  only for the array and record cases.

   procedure Expand_Assign_Array (N : Node_Id; Rhs : Node_Id);
   --  N is an assignment which assigns an array value. This routine process
   --  the various special cases and checks required for such assignments,
   --  including change of representation. Rhs is normally simply the right
   --  hand side of the assignment, except that if the right hand side is
   --  a type conversion or a qualified expression, then the Rhs is the
   --  actual expression inside any such type conversions or qualifications.

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

   procedure Expand_Non_Function_Return (N : Node_Id);
   --  Called by Expand_N_Simple_Return_Statement in case we're returning from
   --  a procedure body, entry body, accept statement, or extended return
   --  statement.  Note that all non-function returns are simple return
   --  statements.

   procedure Expand_Simple_Function_Return (N : Node_Id);
   --  Expand simple return from function. In the case where we are returning
   --  from a function body this is called by Expand_N_Simple_Return_Statement.

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

         if Nkind (Act_Lhs) = N_Slice then
            Larray := Prefix (Act_Lhs);
         else
            Larray := Act_Lhs;

            if Is_Private_Type (Etype (Larray)) then
               Larray :=
                 Unchecked_Convert_To
                   (Underlying_Type (Etype (Larray)), Larray);
            end if;
         end if;

         if Nkind (Act_Rhs) = N_Slice then
            Rarray := Prefix (Act_Rhs);
         else
            Rarray := Act_Rhs;

            if Is_Private_Type (Etype (Rarray)) then
               Rarray :=
                 Unchecked_Convert_To
                   (Underlying_Type (Etype (Rarray)), Rarray);
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
               --  type itself may be universal, and must must be reaanalyzed
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
      --  dead index variable is OK.  However, in CodePeer mode this leads to
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
           (C : Entity_Id;
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
                      Prefix => Duplicate_Subexpr (Rhs),
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
           (C : Entity_Id;
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
                   Prefix => Duplicate_Subexpr (Rhs),
                   Selector_Name => New_Occurrence_Of (C, Loc));
            end if;

            A :=
              Make_Assignment_Statement (Loc,
                Name =>
                  Make_Selected_Component (Loc,
                    Prefix => Duplicate_Subexpr (Lhs),
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
                     Insert_Action (N, Make_Field_Assign (CF, True));
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

      if Ada_Version >= Ada_05 then
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

      --  First deal with generation of range check if required

      if Do_Range_Check (Rhs) then
         Set_Do_Range_Check (Rhs, False);
         Generate_Range_Check (Rhs, Typ, CE_Range_Check_Failed);
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

         if not Change_Of_Representation (N) then
            Apply_Discriminant_Check (Rhs, Etype (Lhs), Lhs);
         end if;

      --  If the type is private without discriminants, and the full type
      --  has discriminants (necessarily with defaults) a check may still be
      --  necessary if the Lhs is aliased. The private determinants must be
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
            Lt : constant Entity_Id := Etype (Lhs);
         begin
            Set_Etype (Lhs, Typ);
            Rewrite (Rhs, OK_Convert_To (Base_Type (Typ), Rhs));
            Apply_Discriminant_Check (Rhs, Typ, Lhs);
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

            if not Change_Of_Representation (N) then
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

      --  Case of assignment to a bit packed array element

      if Nkind (Lhs) = N_Indexed_Component
        and then Is_Bit_Packed_Array (Etype (Prefix (Lhs)))
      then
         Expand_Bit_Packed_Element_Set (N);
         return;

      --  Build-in-place function call case. Note that we're not yet doing
      --  build-in-place for user-written assignment statements (the assignment
      --  here came from an aggregate.)

      elsif Ada_Version >= Ada_05
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
               --  and in the class-wide case, the assignment is replaced by
               --  dispatch call to _assign. Note that this cannot be done when
               --  discriminant checks are locally suppressed (as in extension
               --  aggregate expansions) because otherwise the discriminant
               --  check will be performed within the _assign call. It is also
               --  suppressed for assignments created by the expander that
               --  correspond to initializations, where we do want to copy the
               --  tag (No_Ctrl_Actions flag set True) by the expander and we
               --  do not need to mess with tags ever (Expand_Ctrl_Actions flag
               --  is set True in this case).

               or else (Is_Tagged_Type (Typ)
                         and then not Is_Value_Type (Etype (Lhs))
                         and then Chars (Current_Scope) /= Name_uAssign
                         and then Expand_Ctrl_Actions
                         and then not Discriminant_Checks_Suppressed (Empty))
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
                                     Make_Identifier (Loc,
                                       Chars => Name_uTag)),
                               Right_Opnd =>
                                 Make_Selected_Component (Loc,
                                   Prefix        => Duplicate_Subexpr (Rhs),
                                   Selector_Name =>
                                     Make_Identifier (Loc,
                                       Chars => Name_uTag))),
                         Reason => CE_Tag_Check_Failed));
                  end if;

                  Append_To (L,
                    Make_Procedure_Call_Statement (Loc,
                      Name => New_Reference_To (Op, Loc),
                      Parameter_Associations => New_List (
                        Unchecked_Convert_To (F_Typ,
                          Duplicate_Subexpr (Lhs)),
                        Unchecked_Convert_To (F_Typ,
                          Duplicate_Subexpr (Rhs)))));
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

         --  Move statements from this alternative after the case statement.
         --  They are already analyzed, so will be skipped by the analyzer.

         Insert_List_After (N, Statements (Alt));

         --  That leaves the case statement as a shell. So now we can kill all
         --  other alternatives in the case statement.

         Kill_Dead_Code (Expression (N));

         declare
            A : Node_Id;

         begin
            --  Loop through case alternatives, skipping pragmas, and skipping
            --  the one alternative that we select (and therefore retain).

            A := First (Alternatives (N));
            while Present (A) loop
               if A /= Alt
                 and then Nkind (A) = N_Case_Statement_Alternative
               then
                  Kill_Dead_Code (Statements (A), Warn_On_Deleted_Code);
               end if;

               Next (A);
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
            --  We still need to evaluate the expression if it has any
            --  side effects.

            Remove_Side_Effects (Expression (N));

            Insert_List_After (N, Statements (First (Alternatives (N))));

            --  That leaves the case statement as a shell. The alternative that
            --  will be executed is reset to a null list. So now we can kill
            --  the entire case statement.

            Kill_Dead_Code (Expression (N));
            Rewrite (N, Make_Null_Statement (Loc));
            return;
         end if;

         --  An optimization. If there are only two alternatives, and only
         --  a single choice, then rewrite the whole case statement as an
         --  if statement, since this can result in subsequent optimizations.
         --  This helps not only with case statements in the source of a
         --  simple form, but also with generated code (discriminant check
         --  functions in particular)

         if Len = 2 then
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

   ----------------------------------------
   -- Expand_N_Extended_Return_Statement --
   ----------------------------------------

   --  If there is a Handled_Statement_Sequence, we rewrite this:

   --     return Result : T := <expression> do
   --        <handled_seq_of_stms>
   --     end return;

   --  to be:

   --     declare
   --        Result : T := <expression>;
   --     begin
   --        <handled_seq_of_stms>
   --        return Result;
   --     end;

   --  Otherwise (no Handled_Statement_Sequence), we rewrite this:

   --     return Result : T := <expression>;

   --  to be:

   --     return <expression>;

   --  unless it's build-in-place or there's no <expression>, in which case
   --  we generate:

   --     declare
   --        Result : T := <expression>;
   --     begin
   --        return Result;
   --     end;

   --  Note that this case could have been written by the user as an extended
   --  return statement, or could have been transformed to this from a simple
   --  return statement.

   --  That is, we need to have a reified return object if there are statements
   --  (which might refer to it) or if we're doing build-in-place (so we can
   --  set its address to the final resting place or if there is no expression
   --  (in which case default initial values might need to be set).

   procedure Expand_N_Extended_Return_Statement (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);

      Return_Object_Entity : constant Entity_Id :=
                               First_Entity (Return_Statement_Entity (N));
      Return_Object_Decl   : constant Node_Id :=
                               Parent (Return_Object_Entity);
      Parent_Function      : constant Entity_Id :=
                               Return_Applies_To (Return_Statement_Entity (N));
      Parent_Function_Typ  : constant Entity_Id := Etype (Parent_Function);
      Is_Build_In_Place    : constant Boolean :=
                               Is_Build_In_Place_Function (Parent_Function);

      Return_Stm      : Node_Id;
      Statements      : List_Id;
      Handled_Stm_Seq : Node_Id;
      Result          : Node_Id;
      Exp             : Node_Id;

      function Has_Controlled_Parts (Typ : Entity_Id) return Boolean;
      --  Determine whether type Typ is controlled or contains a controlled
      --  subcomponent.

      function Move_Activation_Chain return Node_Id;
      --  Construct a call to System.Tasking.Stages.Move_Activation_Chain
      --  with parameters:
      --    From         current activation chain
      --    To           activation chain passed in by the caller
      --    New_Master   master passed in by the caller

      function Move_Final_List return Node_Id;
      --  Construct call to System.Finalization_Implementation.Move_Final_List
      --  with parameters:
      --
      --    From         finalization list of the return statement
      --    To           finalization list passed in by the caller

      --------------------------
      -- Has_Controlled_Parts --
      --------------------------

      function Has_Controlled_Parts (Typ : Entity_Id) return Boolean is
      begin
         return
           Is_Controlled (Typ)
             or else Has_Controlled_Component (Typ);
      end Has_Controlled_Parts;

      ---------------------------
      -- Move_Activation_Chain --
      ---------------------------

      function Move_Activation_Chain return Node_Id is
         Activation_Chain_Formal : constant Entity_Id :=
                                     Build_In_Place_Formal
                                       (Parent_Function, BIP_Activation_Chain);
         To                      : constant Node_Id :=
                                     New_Reference_To
                                       (Activation_Chain_Formal, Loc);
         Master_Formal           : constant Entity_Id :=
                                     Build_In_Place_Formal
                                       (Parent_Function, BIP_Master);
         New_Master              : constant Node_Id :=
                                     New_Reference_To (Master_Formal, Loc);

         Chain_Entity : Entity_Id;
         From         : Node_Id;

      begin
         Chain_Entity := First_Entity (Return_Statement_Entity (N));
         while Chars (Chain_Entity) /= Name_uChain loop
            Chain_Entity := Next_Entity (Chain_Entity);
         end loop;

         From :=
           Make_Attribute_Reference (Loc,
             Prefix         => New_Reference_To (Chain_Entity, Loc),
             Attribute_Name => Name_Unrestricted_Access);
         --  ??? Not clear why "Make_Identifier (Loc, Name_uChain)" doesn't
         --  work, instead of "New_Reference_To (Chain_Entity, Loc)" above.

         return
           Make_Procedure_Call_Statement (Loc,
             Name => New_Reference_To (RTE (RE_Move_Activation_Chain), Loc),
             Parameter_Associations => New_List (From, To, New_Master));
      end Move_Activation_Chain;

      ---------------------
      -- Move_Final_List --
      ---------------------

      function Move_Final_List return Node_Id is
         Flist : constant Entity_Id  :=
                   Finalization_Chain_Entity (Return_Statement_Entity (N));

         From : constant Node_Id := New_Reference_To (Flist, Loc);

         Caller_Final_List : constant Entity_Id :=
                               Build_In_Place_Formal
                                 (Parent_Function, BIP_Final_List);

         To : constant Node_Id := New_Reference_To (Caller_Final_List, Loc);

      begin
         --  Catch cases where a finalization chain entity has not been
         --  associated with the return statement entity.

         pragma Assert (Present (Flist));

         --  Build required call

         return
           Make_If_Statement (Loc,
             Condition =>
               Make_Op_Ne (Loc,
                 Left_Opnd  => New_Copy (From),
                 Right_Opnd => New_Node (N_Null, Loc)),
             Then_Statements =>
               New_List (
                 Make_Procedure_Call_Statement (Loc,
                   Name => New_Reference_To (RTE (RE_Move_Final_List), Loc),
                   Parameter_Associations => New_List (From, To))));
      end Move_Final_List;

   --  Start of processing for Expand_N_Extended_Return_Statement

   begin
      if Nkind (Return_Object_Decl) = N_Object_Declaration then
         Exp := Expression (Return_Object_Decl);
      else
         Exp := Empty;
      end if;

      Handled_Stm_Seq := Handled_Statement_Sequence (N);

      --  Build a simple_return_statement that returns the return object when
      --  there is a statement sequence, or no expression, or the result will
      --  be built in place. Note however that we currently do this for all
      --  composite cases, even though nonlimited composite results are not yet
      --  built in place (though we plan to do so eventually).

      if Present (Handled_Stm_Seq)
        or else Is_Composite_Type (Etype (Parent_Function))
        or else No (Exp)
      then
         if No (Handled_Stm_Seq) then
            Statements := New_List;

         --  If the extended return has a handled statement sequence, then wrap
         --  it in a block and use the block as the first statement.

         else
            Statements :=
              New_List (Make_Block_Statement (Loc,
                          Declarations => New_List,
                          Handled_Statement_Sequence => Handled_Stm_Seq));
         end if;

         --  If control gets past the above Statements, we have successfully
         --  completed the return statement. If the result type has controlled
         --  parts and the return is for a build-in-place function, then we
         --  call Move_Final_List to transfer responsibility for finalization
         --  of the return object to the caller. An alternative would be to
         --  declare a Success flag in the function, initialize it to False,
         --  and set it to True here. Then move the Move_Final_List call into
         --  the cleanup code, and check Success. If Success then make a call
         --  to Move_Final_List else do finalization. Then we can remove the
         --  abort-deferral and the nulling-out of the From parameter from
         --  Move_Final_List. Note that the current method is not quite correct
         --  in the rather obscure case of a select-then-abort statement whose
         --  abortable part contains the return statement.

         --  Check the type of the function to determine whether to move the
         --  finalization list. A special case arises when processing a simple
         --  return statement which has been rewritten as an extended return.
         --  In that case check the type of the returned object or the original
         --  expression.

         if Is_Build_In_Place
           and then
               (Has_Controlled_Parts (Parent_Function_Typ)
                 or else (Is_Class_Wide_Type (Parent_Function_Typ)
                           and then
                        Has_Controlled_Parts (Root_Type (Parent_Function_Typ)))
                 or else Has_Controlled_Parts (Etype (Return_Object_Entity))
                 or else (Present (Exp)
                           and then Has_Controlled_Parts (Etype (Exp))))
         then
            Append_To (Statements, Move_Final_List);
         end if;

         --  Similarly to the above Move_Final_List, if the result type
         --  contains tasks, we call Move_Activation_Chain. Later, the cleanup
         --  code will call Complete_Master, which will terminate any
         --  unactivated tasks belonging to the return statement master. But
         --  Move_Activation_Chain updates their master to be that of the
         --  caller, so they will not be terminated unless the return statement
         --  completes unsuccessfully due to exception, abort, goto, or exit.
         --  As a formality, we test whether the function requires the result
         --  to be built in place, though that's necessarily true for the case
         --  of result types with task parts.

         if Is_Build_In_Place and Has_Task (Etype (Parent_Function)) then
            Append_To (Statements, Move_Activation_Chain);
         end if;

         --  Build a simple_return_statement that returns the return object

         Return_Stm :=
           Make_Simple_Return_Statement (Loc,
             Expression => New_Occurrence_Of (Return_Object_Entity, Loc));
         Append_To (Statements, Return_Stm);

         Handled_Stm_Seq :=
           Make_Handled_Sequence_Of_Statements (Loc, Statements);
      end if;

      --  Case where we build a block

      if Present (Handled_Stm_Seq) then
         Result :=
           Make_Block_Statement (Loc,
             Declarations => Return_Object_Declarations (N),
             Handled_Statement_Sequence => Handled_Stm_Seq);

         --  We set the entity of the new block statement to be that of the
         --  return statement. This is necessary so that various fields, such
         --  as Finalization_Chain_Entity carry over from the return statement
         --  to the block. Note that this block is unusual, in that its entity
         --  is an E_Return_Statement rather than an E_Block.

         Set_Identifier
           (Result, New_Occurrence_Of (Return_Statement_Entity (N), Loc));

         --  If the object decl was already rewritten as a renaming, then
         --  we don't want to do the object allocation and transformation of
         --  of the return object declaration to a renaming. This case occurs
         --  when the return object is initialized by a call to another
         --  build-in-place function, and that function is responsible for the
         --  allocation of the return object.

         if Is_Build_In_Place
           and then
             Nkind (Return_Object_Decl) = N_Object_Renaming_Declaration
         then
            pragma Assert (Nkind (Original_Node (Return_Object_Decl)) =
                            N_Object_Declaration
              and then Is_Build_In_Place_Function_Call
                         (Expression (Original_Node (Return_Object_Decl))));

            Set_By_Ref (Return_Stm);  -- Return build-in-place results by ref

         elsif Is_Build_In_Place then

            --  Locate the implicit access parameter associated with the
            --  caller-supplied return object and convert the return
            --  statement's return object declaration to a renaming of a
            --  dereference of the access parameter. If the return object's
            --  declaration includes an expression that has not already been
            --  expanded as separate assignments, then add an assignment
            --  statement to ensure the return object gets initialized.

            --  declare
            --     Result : T [:= <expression>];
            --  begin
            --     ...

            --  is converted to

            --  declare
            --     Result : T renames FuncRA.all;
            --     [Result := <expression;]
            --  begin
            --     ...

            declare
               Return_Obj_Id    : constant Entity_Id :=
                                    Defining_Identifier (Return_Object_Decl);
               Return_Obj_Typ   : constant Entity_Id := Etype (Return_Obj_Id);
               Return_Obj_Expr  : constant Node_Id :=
                                    Expression (Return_Object_Decl);
               Result_Subt      : constant Entity_Id :=
                                    Etype (Parent_Function);
               Constr_Result    : constant Boolean :=
                                    Is_Constrained (Result_Subt);
               Obj_Alloc_Formal : Entity_Id;
               Object_Access    : Entity_Id;
               Obj_Acc_Deref    : Node_Id;
               Init_Assignment  : Node_Id := Empty;

            begin
               --  Build-in-place results must be returned by reference

               Set_By_Ref (Return_Stm);

               --  Retrieve the implicit access parameter passed by the caller

               Object_Access :=
                 Build_In_Place_Formal (Parent_Function, BIP_Object_Access);

               --  If the return object's declaration includes an expression
               --  and the declaration isn't marked as No_Initialization, then
               --  we need to generate an assignment to the object and insert
               --  it after the declaration before rewriting it as a renaming
               --  (otherwise we'll lose the initialization). The case where
               --  the result type is an interface (or class-wide interface)
               --  is also excluded because the context of the function call
               --  must be unconstrained, so the initialization will always
               --  be done as part of an allocator evaluation (storage pool
               --  or secondary stack), never to a constrained target object
               --  passed in by the caller. Besides the assignment being
               --  unneeded in this case, it avoids problems with trying to
               --  generate a dispatching assignment when the return expression
               --  is a nonlimited descendant of a limited interface (the
               --  interface has no assignment operation).

               if Present (Return_Obj_Expr)
                 and then not No_Initialization (Return_Object_Decl)
                 and then not Is_Interface (Return_Obj_Typ)
               then
                  Init_Assignment :=
                    Make_Assignment_Statement (Loc,
                      Name       => New_Reference_To (Return_Obj_Id, Loc),
                      Expression => Relocate_Node (Return_Obj_Expr));
                  Set_Etype (Name (Init_Assignment), Etype (Return_Obj_Id));
                  Set_Assignment_OK (Name (Init_Assignment));
                  Set_No_Ctrl_Actions (Init_Assignment);

                  Set_Parent (Name (Init_Assignment), Init_Assignment);
                  Set_Parent (Expression (Init_Assignment), Init_Assignment);

                  Set_Expression (Return_Object_Decl, Empty);

                  if Is_Class_Wide_Type (Etype (Return_Obj_Id))
                    and then not Is_Class_Wide_Type
                                   (Etype (Expression (Init_Assignment)))
                  then
                     Rewrite (Expression (Init_Assignment),
                       Make_Type_Conversion (Loc,
                         Subtype_Mark =>
                           New_Occurrence_Of
                             (Etype (Return_Obj_Id), Loc),
                         Expression =>
                           Relocate_Node (Expression (Init_Assignment))));
                  end if;

                  --  In the case of functions where the calling context can
                  --  determine the form of allocation needed, initialization
                  --  is done with each part of the if statement that handles
                  --  the different forms of allocation (this is true for
                  --  unconstrained and tagged result subtypes).

                  if Constr_Result
                    and then not Is_Tagged_Type (Underlying_Type (Result_Subt))
                  then
                     Insert_After (Return_Object_Decl, Init_Assignment);
                  end if;
               end if;

               --  When the function's subtype is unconstrained, a run-time
               --  test is needed to determine the form of allocation to use
               --  for the return object. The function has an implicit formal
               --  parameter indicating this. If the BIP_Alloc_Form formal has
               --  the value one, then the caller has passed access to an
               --  existing object for use as the return object. If the value
               --  is two, then the return object must be allocated on the
               --  secondary stack. Otherwise, the object must be allocated in
               --  a storage pool (currently only supported for the global
               --  heap, user-defined storage pools TBD ???). We generate an
               --  if statement to test the implicit allocation formal and
               --  initialize a local access value appropriately, creating
               --  allocators in the secondary stack and global heap cases.
               --  The special formal also exists and must be tested when the
               --  function has a tagged result, even when the result subtype
               --  is constrained, because in general such functions can be
               --  called in dispatching contexts and must be handled similarly
               --  to functions with a class-wide result.

               if not Constr_Result
                 or else Is_Tagged_Type (Underlying_Type (Result_Subt))
               then
                  Obj_Alloc_Formal :=
                    Build_In_Place_Formal (Parent_Function, BIP_Alloc_Form);

                  declare
                     Ref_Type       : Entity_Id;
                     Ptr_Type_Decl  : Node_Id;
                     Alloc_Obj_Id   : Entity_Id;
                     Alloc_Obj_Decl : Node_Id;
                     Alloc_If_Stmt  : Node_Id;
                     SS_Allocator   : Node_Id;
                     Heap_Allocator : Node_Id;

                  begin
                     --  Reuse the itype created for the function's implicit
                     --  access formal. This avoids the need to create a new
                     --  access type here, plus it allows assigning the access
                     --  formal directly without applying a conversion.

                     --  Ref_Type := Etype (Object_Access);

                     --  Create an access type designating the function's
                     --  result subtype.

                     Ref_Type := Make_Temporary (Loc, 'A');

                     Ptr_Type_Decl :=
                       Make_Full_Type_Declaration (Loc,
                         Defining_Identifier => Ref_Type,
                         Type_Definition =>
                           Make_Access_To_Object_Definition (Loc,
                             All_Present => True,
                             Subtype_Indication =>
                               New_Reference_To (Return_Obj_Typ, Loc)));

                     Insert_Before (Return_Object_Decl, Ptr_Type_Decl);

                     --  Create an access object that will be initialized to an
                     --  access value denoting the return object, either coming
                     --  from an implicit access value passed in by the caller
                     --  or from the result of an allocator.

                     Alloc_Obj_Id := Make_Temporary (Loc, 'R');
                     Set_Etype (Alloc_Obj_Id, Ref_Type);

                     Alloc_Obj_Decl :=
                       Make_Object_Declaration (Loc,
                         Defining_Identifier => Alloc_Obj_Id,
                         Object_Definition   => New_Reference_To
                                                  (Ref_Type, Loc));

                     Insert_Before (Return_Object_Decl, Alloc_Obj_Decl);

                     --  Create allocators for both the secondary stack and
                     --  global heap. If there's an initialization expression,
                     --  then create these as initialized allocators.

                     if Present (Return_Obj_Expr)
                       and then not No_Initialization (Return_Object_Decl)
                     then
                        --  Always use the type of the expression for the
                        --  qualified expression, rather than the result type.
                        --  In general we cannot always use the result type
                        --  for the allocator, because the expression might be
                        --  of a specific type, such as in the case of an
                        --  aggregate or even a nonlimited object when the
                        --  result type is a limited class-wide interface type.

                        Heap_Allocator :=
                          Make_Allocator (Loc,
                            Expression =>
                              Make_Qualified_Expression (Loc,
                                Subtype_Mark =>
                                  New_Reference_To
                                    (Etype (Return_Obj_Expr), Loc),
                                Expression =>
                                  New_Copy_Tree (Return_Obj_Expr)));

                     else
                        --  If the function returns a class-wide type we cannot
                        --  use the return type for the allocator. Instead we
                        --  use the type of the expression, which must be an
                        --  aggregate of a definite type.

                        if Is_Class_Wide_Type (Return_Obj_Typ) then
                           Heap_Allocator :=
                             Make_Allocator (Loc,
                               Expression =>
                                 New_Reference_To
                                   (Etype (Return_Obj_Expr), Loc));
                        else
                           Heap_Allocator :=
                             Make_Allocator (Loc,
                               Expression =>
                                 New_Reference_To (Return_Obj_Typ, Loc));
                        end if;

                        --  If the object requires default initialization then
                        --  that will happen later following the elaboration of
                        --  the object renaming. If we don't turn it off here
                        --  then the object will be default initialized twice.

                        Set_No_Initialization (Heap_Allocator);
                     end if;

                     --  If the No_Allocators restriction is active, then only
                     --  an allocator for secondary stack allocation is needed.
                     --  It's OK for such allocators to have Comes_From_Source
                     --  set to False, because gigi knows not to flag them as
                     --  being a violation of No_Implicit_Heap_Allocations.

                     if Restriction_Active (No_Allocators) then
                        SS_Allocator   := Heap_Allocator;
                        Heap_Allocator := Make_Null (Loc);

                     --  Otherwise the heap allocator may be needed, so we make
                     --  another allocator for secondary stack allocation.

                     else
                        SS_Allocator := New_Copy_Tree (Heap_Allocator);

                        --  The heap allocator is marked Comes_From_Source
                        --  since it corresponds to an explicit user-written
                        --  allocator (that is, it will only be executed on
                        --  behalf of callers that call the function as
                        --  initialization for such an allocator). This
                        --  prevents errors when No_Implicit_Heap_Allocations
                        --  is in force.

                        Set_Comes_From_Source (Heap_Allocator, True);
                     end if;

                     --  The allocator is returned on the secondary stack. We
                     --  don't do this on VM targets, since the SS is not used.

                     if VM_Target = No_VM then
                        Set_Storage_Pool (SS_Allocator, RTE (RE_SS_Pool));
                        Set_Procedure_To_Call
                          (SS_Allocator, RTE (RE_SS_Allocate));

                        --  The allocator is returned on the secondary stack,
                        --  so indicate that the function return, as well as
                        --  the block that encloses the allocator, must not
                        --  release it. The flags must be set now because the
                        --  decision to use the secondary stack is done very
                        --  late in the course of expanding the return
                        --  statement, past the point where these flags are
                        --  normally set.

                        Set_Sec_Stack_Needed_For_Return (Parent_Function);
                        Set_Sec_Stack_Needed_For_Return
                          (Return_Statement_Entity (N));
                        Set_Uses_Sec_Stack (Parent_Function);
                        Set_Uses_Sec_Stack (Return_Statement_Entity (N));
                     end if;

                     --  Create an if statement to test the BIP_Alloc_Form
                     --  formal and initialize the access object to either the
                     --  BIP_Object_Access formal (BIP_Alloc_Form = 0), the
                     --  result of allocating the object in the secondary stack
                     --  (BIP_Alloc_Form = 1), or else an allocator to create
                     --  the return object in the heap (BIP_Alloc_Form = 2).

                     --  ??? An unchecked type conversion must be made in the
                     --  case of assigning the access object formal to the
                     --  local access object, because a normal conversion would
                     --  be illegal in some cases (such as converting access-
                     --  to-unconstrained to access-to-constrained), but the
                     --  the unchecked conversion will presumably fail to work
                     --  right in just such cases. It's not clear at all how to
                     --  handle this. ???

                     Alloc_If_Stmt :=
                       Make_If_Statement (Loc,
                         Condition       =>
                           Make_Op_Eq (Loc,
                             Left_Opnd =>
                               New_Reference_To (Obj_Alloc_Formal, Loc),
                             Right_Opnd =>
                               Make_Integer_Literal (Loc,
                                 UI_From_Int (BIP_Allocation_Form'Pos
                                                (Caller_Allocation)))),
                         Then_Statements =>
                           New_List (Make_Assignment_Statement (Loc,
                                       Name       =>
                                         New_Reference_To
                                           (Alloc_Obj_Id, Loc),
                                       Expression =>
                                         Make_Unchecked_Type_Conversion (Loc,
                                           Subtype_Mark =>
                                             New_Reference_To (Ref_Type, Loc),
                                           Expression =>
                                             New_Reference_To
                                               (Object_Access, Loc)))),
                         Elsif_Parts     =>
                           New_List (Make_Elsif_Part (Loc,
                                       Condition       =>
                                         Make_Op_Eq (Loc,
                                           Left_Opnd =>
                                             New_Reference_To
                                               (Obj_Alloc_Formal, Loc),
                                           Right_Opnd =>
                                             Make_Integer_Literal (Loc,
                                               UI_From_Int (
                                                 BIP_Allocation_Form'Pos
                                                    (Secondary_Stack)))),
                                       Then_Statements =>
                                          New_List
                                            (Make_Assignment_Statement (Loc,
                                               Name       =>
                                                 New_Reference_To
                                                   (Alloc_Obj_Id, Loc),
                                               Expression =>
                                                 SS_Allocator)))),
                         Else_Statements =>
                           New_List (Make_Assignment_Statement (Loc,
                                        Name       =>
                                          New_Reference_To
                                            (Alloc_Obj_Id, Loc),
                                        Expression =>
                                          Heap_Allocator)));

                     --  If a separate initialization assignment was created
                     --  earlier, append that following the assignment of the
                     --  implicit access formal to the access object, to ensure
                     --  that the return object is initialized in that case.
                     --  In this situation, the target of the assignment must
                     --  be rewritten to denote a dereference of the access to
                     --  the return object passed in by the caller.

                     if Present (Init_Assignment) then
                        Rewrite (Name (Init_Assignment),
                          Make_Explicit_Dereference (Loc,
                            Prefix => New_Reference_To (Alloc_Obj_Id, Loc)));
                        Set_Etype
                          (Name (Init_Assignment), Etype (Return_Obj_Id));

                        Append_To
                          (Then_Statements (Alloc_If_Stmt),
                           Init_Assignment);
                     end if;

                     Insert_Before (Return_Object_Decl, Alloc_If_Stmt);

                     --  Remember the local access object for use in the
                     --  dereference of the renaming created below.

                     Object_Access := Alloc_Obj_Id;
                  end;
               end if;

               --  Replace the return object declaration with a renaming of a
               --  dereference of the access value designating the return
               --  object.

               Obj_Acc_Deref :=
                 Make_Explicit_Dereference (Loc,
                   Prefix => New_Reference_To (Object_Access, Loc));

               Rewrite (Return_Object_Decl,
                 Make_Object_Renaming_Declaration (Loc,
                   Defining_Identifier => Return_Obj_Id,
                   Access_Definition   => Empty,
                   Subtype_Mark        => New_Occurrence_Of
                                            (Return_Obj_Typ, Loc),
                   Name                => Obj_Acc_Deref));

               Set_Renamed_Object (Return_Obj_Id, Obj_Acc_Deref);
            end;
         end if;

      --  Case where we do not build a block

      else
         --  We're about to drop Return_Object_Declarations on the floor, so
         --  we need to insert it, in case it got expanded into useful code.

         Insert_List_Before (N, Return_Object_Declarations (N));

         --  Build simple_return_statement that returns the expression directly

         Return_Stm := Make_Simple_Return_Statement (Loc, Expression => Exp);

         Result := Return_Stm;
      end if;

      --  Set the flag to prevent infinite recursion

      Set_Comes_From_Extended_Return_Statement (Return_Stm);

      Rewrite (N, Result);
      Analyze (N);
   end Expand_N_Extended_Return_Statement;

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

   -----------------------------
   -- Expand_N_Loop_Statement --
   -----------------------------

   --  1. Remove null loop entirely
   --  2. Deal with while condition for C/Fortran boolean
   --  3. Deal with loops with a non-standard enumeration type range
   --  4. Deal with while loops where Condition_Actions is set
   --  5. Insert polling call if required

   procedure Expand_N_Loop_Statement (N : Node_Id) is
      Loc  : constant Source_Ptr := Sloc (N);
      Isc  : constant Node_Id    := Iteration_Scheme (N);

   begin
      --  Delete null loop

      if Is_Null_Loop (N) then
         Rewrite (N, Make_Null_Statement (Loc));
         return;
      end if;

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
         return;
      end if;

      --  Note: we do not have to worry about validity checking of the for loop
      --  range bounds here, since they were frozen with constant declarations
      --  and it is during that process that the validity checking is done.

      --  Handle the case where we have a for loop with the range type being an
      --  enumeration type with non-standard representation. In this case we
      --  expand:

      --    for x in [reverse] a .. b loop
      --       ...
      --    end loop;

      --  to

      --    for xP in [reverse] integer
      --                          range etype'Pos (a) .. etype'Pos (b) loop
      --       declare
      --          x : constant etype := Pos_To_Rep (xP);
      --       begin
      --          ...
      --       end;
      --    end loop;

      if Present (Loop_Parameter_Specification (Isc)) then
         declare
            LPS     : constant Node_Id   := Loop_Parameter_Specification (Isc);
            Loop_Id : constant Entity_Id := Defining_Identifier (LPS);
            Ltype   : constant Entity_Id := Etype (Loop_Id);
            Btype   : constant Entity_Id := Base_Type (Ltype);
            Expr    : Node_Id;
            New_Id  : Entity_Id;

         begin
            if not Is_Enumeration_Type (Btype)
              or else No (Enum_Pos_To_Rep (Btype))
            then
               return;
            end if;

            New_Id :=
              Make_Defining_Identifier (Loc,
                Chars => New_External_Name (Chars (Loop_Id), 'P'));

            --  If the type has a contiguous representation, successive values
            --  can be generated as offsets from the first literal.

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
                   Prefix => New_Reference_To (Enum_Pos_To_Rep (Btype), Loc),
                   Expressions => New_List (New_Reference_To (New_Id, Loc)));
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
                                            (Type_High_Bound (Ltype))))))))),

                Statements => New_List (
                  Make_Block_Statement (Loc,
                    Declarations => New_List (
                      Make_Object_Declaration (Loc,
                        Defining_Identifier => Loop_Id,
                        Constant_Present    => True,
                        Object_Definition   => New_Reference_To (Ltype, Loc),
                        Expression          => Expr)),

                    Handled_Statement_Sequence =>
                      Make_Handled_Sequence_Of_Statements (Loc,
                        Statements => Statements (N)))),

                End_Label => End_Label (N)));
            Analyze (N);
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
      end if;
   end Expand_N_Loop_Statement;

   --------------------------------------
   -- Expand_N_Simple_Return_Statement --
   --------------------------------------

   procedure Expand_N_Simple_Return_Statement (N : Node_Id) is
   begin
      --  Defend against previous errors (i.e. the return statement calls a
      --  function that is not available in configurable runtime).

      if Present (Expression (N))
        and then Nkind (Expression (N)) = N_Empty
      then
         return;
      end if;

      --  Distinguish the function and non-function cases:

      case Ekind (Return_Applies_To (Return_Statement_Entity (N))) is

         when E_Function          |
              E_Generic_Function  =>
            Expand_Simple_Function_Return (N);

         when E_Procedure         |
              E_Generic_Procedure |
              E_Entry             |
              E_Entry_Family      |
              E_Return_Statement =>
            Expand_Non_Function_Return (N);

         when others =>
            raise Program_Error;
      end case;

   exception
      when RE_Not_Available =>
         return;
   end Expand_N_Simple_Return_Statement;

   --------------------------------
   -- Expand_Non_Function_Return --
   --------------------------------

   procedure Expand_Non_Function_Return (N : Node_Id) is
      pragma Assert (No (Expression (N)));

      Loc         : constant Source_Ptr := Sloc (N);
      Scope_Id    : Entity_Id :=
                      Return_Applies_To (Return_Statement_Entity (N));
      Kind        : constant Entity_Kind := Ekind (Scope_Id);
      Call        : Node_Id;
      Acc_Stat    : Node_Id;
      Goto_Stat   : Node_Id;
      Lab_Node    : Node_Id;

   begin
      --  Call _Postconditions procedure if procedure with active
      --  postconditions. Here, we use the Postcondition_Proc attribute, which
      --  is needed for implicitly-generated returns. Functions never
      --  have implicitly-generated returns, and there's no room for
      --  Postcondition_Proc in E_Function, so we look up the identifier
      --  Name_uPostconditions for function returns (see
      --  Expand_Simple_Function_Return).

      if Ekind (Scope_Id) = E_Procedure
        and then Has_Postconditions (Scope_Id)
      then
         pragma Assert (Present (Postcondition_Proc (Scope_Id)));
         Insert_Action (N,
           Make_Procedure_Call_Statement (Loc,
             Name => New_Reference_To (Postcondition_Proc (Scope_Id), Loc)));
      end if;

      --  If it is a return from a procedure do no extra steps

      if Kind = E_Procedure or else Kind = E_Generic_Procedure then
         return;

      --  If it is a nested return within an extended one, replace it with a
      --  return of the previously declared return object.

      elsif Kind = E_Return_Statement then
         Rewrite (N,
           Make_Simple_Return_Statement (Loc,
             Expression =>
               New_Occurrence_Of (First_Entity (Scope_Id), Loc)));
         Set_Comes_From_Extended_Return_Statement (N);
         Set_Return_Statement_Entity (N, Scope_Id);
         Expand_Simple_Function_Return (N);
         return;
      end if;

      pragma Assert (Is_Entry (Scope_Id));

      --  Look at the enclosing block to see whether the return is from an
      --  accept statement or an entry body.

      for J in reverse 0 .. Scope_Stack.Last loop
         Scope_Id := Scope_Stack.Table (J).Entity;
         exit when Is_Concurrent_Type (Scope_Id);
      end loop;

      --  If it is a return from accept statement it is expanded as call to
      --  RTS Complete_Rendezvous and a goto to the end of the accept body.

      --  (cf : Expand_N_Accept_Statement, Expand_N_Selective_Accept,
      --  Expand_N_Accept_Alternative in exp_ch9.adb)

      if Is_Task_Type (Scope_Id) then

         Call :=
           Make_Procedure_Call_Statement (Loc,
             Name => New_Reference_To (RTE (RE_Complete_Rendezvous), Loc));
         Insert_Before (N, Call);
         --  why not insert actions here???
         Analyze (Call);

         Acc_Stat := Parent (N);
         while Nkind (Acc_Stat) /= N_Accept_Statement loop
            Acc_Stat := Parent (Acc_Stat);
         end loop;

         Lab_Node := Last (Statements
           (Handled_Statement_Sequence (Acc_Stat)));

         Goto_Stat := Make_Goto_Statement (Loc,
           Name => New_Occurrence_Of
             (Entity (Identifier (Lab_Node)), Loc));

         Set_Analyzed (Goto_Stat);

         Rewrite (N, Goto_Stat);
         Analyze (N);

      --  If it is a return from an entry body, put a Complete_Entry_Body call
      --  in front of the return.

      elsif Is_Protected_Type (Scope_Id) then
         Call :=
           Make_Procedure_Call_Statement (Loc,
             Name =>
               New_Reference_To (RTE (RE_Complete_Entry_Body), Loc),
             Parameter_Associations => New_List (
               Make_Attribute_Reference (Loc,
                 Prefix =>
                   New_Reference_To
                     (Find_Protection_Object (Current_Scope), Loc),
                 Attribute_Name =>
                   Name_Unchecked_Access)));

         Insert_Before (N, Call);
         Analyze (Call);
      end if;
   end Expand_Non_Function_Return;

   -----------------------------------
   -- Expand_Simple_Function_Return --
   -----------------------------------

   --  The "simple" comes from the syntax rule simple_return_statement.
   --  The semantics are not at all simple!

   procedure Expand_Simple_Function_Return (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);

      Scope_Id : constant Entity_Id :=
                   Return_Applies_To (Return_Statement_Entity (N));
      --  The function we are returning from

      R_Type : constant Entity_Id := Etype (Scope_Id);
      --  The result type of the function

      Utyp : constant Entity_Id := Underlying_Type (R_Type);

      Exp : constant Node_Id := Expression (N);
      pragma Assert (Present (Exp));

      Exptyp : constant Entity_Id := Etype (Exp);
      --  The type of the expression (not necessarily the same as R_Type)

      Subtype_Ind : Node_Id;
      --  If the result type of the function is class-wide and the
      --  expression has a specific type, then we use the expression's
      --  type as the type of the return object. In cases where the
      --  expression is an aggregate that is built in place, this avoids
      --  the need for an expensive conversion of the return object to
      --  the specific type on assignments to the individual components.

   begin
      if Is_Class_Wide_Type (R_Type)
        and then not Is_Class_Wide_Type (Etype (Exp))
      then
         Subtype_Ind := New_Occurrence_Of (Etype (Exp), Loc);
      else
         Subtype_Ind := New_Occurrence_Of (R_Type, Loc);
      end if;

      --  For the case of a simple return that does not come from an extended
      --  return, in the case of Ada 2005 where we are returning a limited
      --  type, we rewrite "return <expression>;" to be:

      --    return _anon_ : <return_subtype> := <expression>

      --  The expansion produced by Expand_N_Extended_Return_Statement will
      --  contain simple return statements (for example, a block containing
      --  simple return of the return object), which brings us back here with
      --  Comes_From_Extended_Return_Statement set. The reason for the barrier
      --  checking for a simple return that does not come from an extended
      --  return is to avoid this infinite recursion.

      --  The reason for this design is that for Ada 2005 limited returns, we
      --  need to reify the return object, so we can build it "in place", and
      --  we need a block statement to hang finalization and tasking stuff.

      --  ??? In order to avoid disruption, we avoid translating to extended
      --  return except in the cases where we really need to (Ada 2005 for
      --  inherently limited). We might prefer to do this translation in all
      --  cases (except perhaps for the case of Ada 95 inherently limited),
      --  in order to fully exercise the Expand_N_Extended_Return_Statement
      --  code. This would also allow us to do the build-in-place optimization
      --  for efficiency even in cases where it is semantically not required.

      --  As before, we check the type of the return expression rather than the
      --  return type of the function, because the latter may be a limited
      --  class-wide interface type, which is not a limited type, even though
      --  the type of the expression may be.

      if not Comes_From_Extended_Return_Statement (N)
        and then Is_Inherently_Limited_Type (Etype (Expression (N)))
        and then Ada_Version >= Ada_05
        and then not Debug_Flag_Dot_L
      then
         declare
            Return_Object_Entity : constant Entity_Id :=
                                     Make_Temporary (Loc, 'R', Exp);
            Obj_Decl : constant Node_Id :=
                         Make_Object_Declaration (Loc,
                           Defining_Identifier => Return_Object_Entity,
                           Object_Definition   => Subtype_Ind,
                           Expression          => Exp);

            Ext : constant Node_Id := Make_Extended_Return_Statement (Loc,
                    Return_Object_Declarations => New_List (Obj_Decl));
            --  Do not perform this high-level optimization if the result type
            --  is an interface because the "this" pointer must be displaced.

         begin
            Rewrite (N, Ext);
            Analyze (N);
            return;
         end;
      end if;

      --  Here we have a simple return statement that is part of the expansion
      --  of an extended return statement (either written by the user, or
      --  generated by the above code).

      --  Always normalize C/Fortran boolean result. This is not always needed,
      --  but it seems a good idea to minimize the passing around of non-
      --  normalized values, and in any case this handles the processing of
      --  barrier functions for protected types, which turn the condition into
      --  a return statement.

      if Is_Boolean_Type (Exptyp)
        and then Nonzero_Is_True (Exptyp)
      then
         Adjust_Condition (Exp);
         Adjust_Result_Type (Exp, Exptyp);
      end if;

      --  Do validity check if enabled for returns

      if Validity_Checks_On
        and then Validity_Check_Returns
      then
         Ensure_Valid (Exp);
      end if;

      --  Check the result expression of a scalar function against the subtype
      --  of the function by inserting a conversion. This conversion must
      --  eventually be performed for other classes of types, but for now it's
      --  only done for scalars.
      --  ???

      if Is_Scalar_Type (Exptyp) then
         Rewrite (Exp, Convert_To (R_Type, Exp));

         --  The expression is resolved to ensure that the conversion gets
         --  expanded to generate a possible constraint check.

         Analyze_And_Resolve (Exp, R_Type);
      end if;

      --  Deal with returning variable length objects and controlled types

      --  Nothing to do if we are returning by reference, or this is not a
      --  type that requires special processing (indicated by the fact that
      --  it requires a cleanup scope for the secondary stack case).

      if Is_Inherently_Limited_Type (Exptyp)
        or else Is_Limited_Interface (Exptyp)
      then
         null;

      elsif not Requires_Transient_Scope (R_Type) then

         --  Mutable records with no variable length components are not
         --  returned on the sec-stack, so we need to make sure that the
         --  backend will only copy back the size of the actual value, and not
         --  the maximum size. We create an actual subtype for this purpose.

         declare
            Ubt  : constant Entity_Id := Underlying_Type (Base_Type (Exptyp));
            Decl : Node_Id;
            Ent  : Entity_Id;
         begin
            if Has_Discriminants (Ubt)
              and then not Is_Constrained (Ubt)
              and then not Has_Unchecked_Union (Ubt)
            then
               Decl := Build_Actual_Subtype (Ubt, Exp);
               Ent := Defining_Identifier (Decl);
               Insert_Action (Exp, Decl);
               Rewrite (Exp, Unchecked_Convert_To (Ent, Exp));
               Analyze_And_Resolve (Exp);
            end if;
         end;

      --  Here if secondary stack is used

      else
         --  Make sure that no surrounding block will reclaim the secondary
         --  stack on which we are going to put the result. Not only may this
         --  introduce secondary stack leaks but worse, if the reclamation is
         --  done too early, then the result we are returning may get
         --  clobbered.

         declare
            S : Entity_Id;
         begin
            S := Current_Scope;
            while Ekind (S) = E_Block or else Ekind (S) = E_Loop loop
               Set_Sec_Stack_Needed_For_Return (S, True);
               S := Enclosing_Dynamic_Scope (S);
            end loop;
         end;

         --  Optimize the case where the result is a function call. In this
         --  case either the result is already on the secondary stack, or is
         --  already being returned with the stack pointer depressed and no
         --  further processing is required except to set the By_Ref flag to
         --  ensure that gigi does not attempt an extra unnecessary copy.
         --  (actually not just unnecessary but harmfully wrong in the case
         --  of a controlled type, where gigi does not know how to do a copy).
         --  To make up for a gcc 2.8.1 deficiency (???), we perform
         --  the copy for array types if the constrained status of the
         --  target type is different from that of the expression.

         if Requires_Transient_Scope (Exptyp)
           and then
              (not Is_Array_Type (Exptyp)
                or else Is_Constrained (Exptyp) = Is_Constrained (R_Type)
                or else CW_Or_Has_Controlled_Part (Utyp))
           and then Nkind (Exp) = N_Function_Call
         then
            Set_By_Ref (N);

            --  Remove side effects from the expression now so that other parts
            --  of the expander do not have to reanalyze this node without this
            --  optimization

            Rewrite (Exp, Duplicate_Subexpr_No_Checks (Exp));

         --  For controlled types, do the allocation on the secondary stack
         --  manually in order to call adjust at the right time:

         --    type Anon1 is access R_Type;
         --    for Anon1'Storage_pool use ss_pool;
         --    Anon2 : anon1 := new R_Type'(expr);
         --    return Anon2.all;

         --  We do the same for classwide types that are not potentially
         --  controlled (by the virtue of restriction No_Finalization) because
         --  gigi is not able to properly allocate class-wide types.

         elsif CW_Or_Has_Controlled_Part (Utyp) then
            declare
               Loc        : constant Source_Ptr := Sloc (N);
               Acc_Typ    : constant Entity_Id := Make_Temporary (Loc, 'A');
               Alloc_Node : Node_Id;
               Temp       : Entity_Id;

            begin
               Set_Ekind (Acc_Typ, E_Access_Type);

               Set_Associated_Storage_Pool (Acc_Typ, RTE (RE_SS_Pool));

               --  This is an allocator for the secondary stack, and it's fine
               --  to have Comes_From_Source set False on it, as gigi knows not
               --  to flag it as a violation of No_Implicit_Heap_Allocations.

               Alloc_Node :=
                 Make_Allocator (Loc,
                   Expression =>
                     Make_Qualified_Expression (Loc,
                       Subtype_Mark => New_Reference_To (Etype (Exp), Loc),
                       Expression   => Relocate_Node (Exp)));

               --  We do not want discriminant checks on the declaration,
               --  given that it gets its value from the allocator.

               Set_No_Initialization (Alloc_Node);

               Temp := Make_Temporary (Loc, 'R', Alloc_Node);

               Insert_List_Before_And_Analyze (N, New_List (
                 Make_Full_Type_Declaration (Loc,
                   Defining_Identifier => Acc_Typ,
                   Type_Definition     =>
                     Make_Access_To_Object_Definition (Loc,
                       Subtype_Indication => Subtype_Ind)),

                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Temp,
                   Object_Definition   => New_Reference_To (Acc_Typ, Loc),
                   Expression          => Alloc_Node)));

               Rewrite (Exp,
                 Make_Explicit_Dereference (Loc,
                 Prefix => New_Reference_To (Temp, Loc)));

               Analyze_And_Resolve (Exp, R_Type);
            end;

         --  Otherwise use the gigi mechanism to allocate result on the
         --  secondary stack.

         else
            Check_Restriction (No_Secondary_Stack, N);
            Set_Storage_Pool (N, RTE (RE_SS_Pool));

            --  If we are generating code for the VM do not use
            --  SS_Allocate since everything is heap-allocated anyway.

            if VM_Target = No_VM then
               Set_Procedure_To_Call (N, RTE (RE_SS_Allocate));
            end if;
         end if;
      end if;

      --  Implement the rules of 6.5(8-10), which require a tag check in the
      --  case of a limited tagged return type, and tag reassignment for
      --  nonlimited tagged results. These actions are needed when the return
      --  type is a specific tagged type and the result expression is a
      --  conversion or a formal parameter, because in that case the tag of the
      --  expression might differ from the tag of the specific result type.

      if Is_Tagged_Type (Utyp)
        and then not Is_Class_Wide_Type (Utyp)
        and then (Nkind_In (Exp, N_Type_Conversion,
                                 N_Unchecked_Type_Conversion)
                    or else (Is_Entity_Name (Exp)
                               and then Ekind (Entity (Exp)) in Formal_Kind))
      then
         --  When the return type is limited, perform a check that the
         --  tag of the result is the same as the tag of the return type.

         if Is_Limited_Type (R_Type) then
            Insert_Action (Exp,
              Make_Raise_Constraint_Error (Loc,
                Condition =>
                  Make_Op_Ne (Loc,
                    Left_Opnd =>
                      Make_Selected_Component (Loc,
                        Prefix => Duplicate_Subexpr (Exp),
                        Selector_Name =>
                          New_Reference_To (First_Tag_Component (Utyp), Loc)),
                    Right_Opnd =>
                      Unchecked_Convert_To (RTE (RE_Tag),
                        New_Reference_To
                          (Node (First_Elmt
                                  (Access_Disp_Table (Base_Type (Utyp)))),
                           Loc))),
                Reason => CE_Tag_Check_Failed));

         --  If the result type is a specific nonlimited tagged type, then we
         --  have to ensure that the tag of the result is that of the result
         --  type. This is handled by making a copy of the expression in the
         --  case where it might have a different tag, namely when the
         --  expression is a conversion or a formal parameter. We create a new
         --  object of the result type and initialize it from the expression,
         --  which will implicitly force the tag to be set appropriately.

         else
            declare
               ExpR       : constant Node_Id   := Relocate_Node (Exp);
               Result_Id  : constant Entity_Id :=
                              Make_Temporary (Loc, 'R', ExpR);
               Result_Exp : constant Node_Id   :=
                              New_Reference_To (Result_Id, Loc);
               Result_Obj : constant Node_Id   :=
                              Make_Object_Declaration (Loc,
                                Defining_Identifier => Result_Id,
                                Object_Definition   =>
                                  New_Reference_To (R_Type, Loc),
                                Constant_Present    => True,
                                Expression          => ExpR);

            begin
               Set_Assignment_OK (Result_Obj);
               Insert_Action (Exp, Result_Obj);

               Rewrite (Exp, Result_Exp);
               Analyze_And_Resolve (Exp, R_Type);
            end;
         end if;

      --  Ada 2005 (AI-344): If the result type is class-wide, then insert
      --  a check that the level of the return expression's underlying type
      --  is not deeper than the level of the master enclosing the function.
      --  Always generate the check when the type of the return expression
      --  is class-wide, when it's a type conversion, or when it's a formal
      --  parameter. Otherwise, suppress the check in the case where the
      --  return expression has a specific type whose level is known not to
      --  be statically deeper than the function's result type.

      --  Note: accessibility check is skipped in the VM case, since there
      --  does not seem to be any practical way to implement this check.

      elsif Ada_Version >= Ada_05
        and then Tagged_Type_Expansion
        and then Is_Class_Wide_Type (R_Type)
        and then not Scope_Suppress (Accessibility_Check)
        and then
          (Is_Class_Wide_Type (Etype (Exp))
            or else Nkind_In (Exp, N_Type_Conversion,
                                   N_Unchecked_Type_Conversion)
            or else (Is_Entity_Name (Exp)
                       and then Ekind (Entity (Exp)) in Formal_Kind)
            or else Scope_Depth (Enclosing_Dynamic_Scope (Etype (Exp))) >
                      Scope_Depth (Enclosing_Dynamic_Scope (Scope_Id)))
      then
         declare
            Tag_Node : Node_Id;

         begin
            --  Ada 2005 (AI-251): In class-wide interface objects we displace
            --  "this" to reference the base of the object --- required to get
            --  access to the TSD of the object.

            if Is_Class_Wide_Type (Etype (Exp))
              and then Is_Interface (Etype (Exp))
              and then Nkind (Exp) = N_Explicit_Dereference
            then
               Tag_Node :=
                 Make_Explicit_Dereference (Loc,
                   Unchecked_Convert_To (RTE (RE_Tag_Ptr),
                     Make_Function_Call (Loc,
                       Name => New_Reference_To (RTE (RE_Base_Address), Loc),
                       Parameter_Associations => New_List (
                         Unchecked_Convert_To (RTE (RE_Address),
                           Duplicate_Subexpr (Prefix (Exp)))))));
            else
               Tag_Node :=
                 Make_Attribute_Reference (Loc,
                   Prefix => Duplicate_Subexpr (Exp),
                   Attribute_Name => Name_Tag);
            end if;

            Insert_Action (Exp,
              Make_Raise_Program_Error (Loc,
                Condition =>
                  Make_Op_Gt (Loc,
                    Left_Opnd =>
                      Build_Get_Access_Level (Loc, Tag_Node),
                    Right_Opnd =>
                      Make_Integer_Literal (Loc,
                        Scope_Depth (Enclosing_Dynamic_Scope (Scope_Id)))),
                Reason => PE_Accessibility_Check_Failed));
         end;
      end if;

      --  If we are returning an object that may not be bit-aligned, then copy
      --  the value into a temporary first. This copy may need to expand to a
      --  loop of component operations.

      if Is_Possibly_Unaligned_Slice (Exp)
        or else Is_Possibly_Unaligned_Object (Exp)
      then
         declare
            ExpR : constant Node_Id   := Relocate_Node (Exp);
            Tnn  : constant Entity_Id := Make_Temporary (Loc, 'T', ExpR);
         begin
            Insert_Action (Exp,
              Make_Object_Declaration (Loc,
                Defining_Identifier => Tnn,
                Constant_Present    => True,
                Object_Definition   => New_Occurrence_Of (R_Type, Loc),
                Expression          => ExpR),
              Suppress            => All_Checks);
            Rewrite (Exp, New_Occurrence_Of (Tnn, Loc));
         end;
      end if;

      --  Generate call to postcondition checks if they are present

      if Ekind (Scope_Id) = E_Function
        and then Has_Postconditions (Scope_Id)
      then
         --  We are going to reference the returned value twice in this case,
         --  once in the call to _Postconditions, and once in the actual return
         --  statement, but we can't have side effects happening twice, and in
         --  any case for efficiency we don't want to do the computation twice.

         --  If the returned expression is an entity name, we don't need to
         --  worry since it is efficient and safe to reference it twice, that's
         --  also true for literals other than string literals, and for the
         --  case of X.all where X is an entity name.

         if Is_Entity_Name (Exp)
           or else Nkind_In (Exp, N_Character_Literal,
                                  N_Integer_Literal,
                                  N_Real_Literal)
           or else (Nkind (Exp) = N_Explicit_Dereference
                      and then Is_Entity_Name (Prefix (Exp)))
         then
            null;

         --  Otherwise we are going to need a temporary to capture the value

         else
            declare
               ExpR : constant Node_Id   := Relocate_Node (Exp);
               Tnn  : constant Entity_Id := Make_Temporary (Loc, 'T', ExpR);

            begin
               --  For a complex expression of an elementary type, capture
               --  value in the temporary and use it as the reference.

               if Is_Elementary_Type (R_Type) then
                  Insert_Action (Exp,
                    Make_Object_Declaration (Loc,
                      Defining_Identifier => Tnn,
                      Constant_Present    => True,
                      Object_Definition   => New_Occurrence_Of (R_Type, Loc),
                      Expression          => ExpR),
                    Suppress => All_Checks);

                  Rewrite (Exp, New_Occurrence_Of (Tnn, Loc));

               --  If we have something we can rename, generate a renaming of
               --  the object and replace the expression with a reference

               elsif Is_Object_Reference (Exp) then
                  Insert_Action (Exp,
                    Make_Object_Renaming_Declaration (Loc,
                      Defining_Identifier => Tnn,
                      Subtype_Mark        => New_Occurrence_Of (R_Type, Loc),
                      Name                => ExpR),
                    Suppress => All_Checks);

                  Rewrite (Exp, New_Occurrence_Of (Tnn, Loc));

               --  Otherwise we have something like a string literal or an
               --  aggregate. We could copy the value, but that would be
               --  inefficient. Instead we make a reference to the value and
               --  capture this reference with a renaming, the expression is
               --  then replaced by a dereference of this renaming.

               else
                  --  For now, copy the value, since the code below does not
                  --  seem to work correctly ???

                  Insert_Action (Exp,
                    Make_Object_Declaration (Loc,
                      Defining_Identifier => Tnn,
                      Constant_Present    => True,
                      Object_Definition   => New_Occurrence_Of (R_Type, Loc),
                      Expression          => Relocate_Node (Exp)),
                    Suppress => All_Checks);

                  Rewrite (Exp, New_Occurrence_Of (Tnn, Loc));

                  --  Insert_Action (Exp,
                  --    Make_Object_Renaming_Declaration (Loc,
                  --      Defining_Identifier => Tnn,
                  --      Access_Definition =>
                  --        Make_Access_Definition (Loc,
                  --          All_Present  => True,
                  --          Subtype_Mark => New_Occurrence_Of (R_Type, Loc)),
                  --      Name =>
                  --        Make_Reference (Loc,
                  --          Prefix => Relocate_Node (Exp))),
                  --    Suppress => All_Checks);

                  --  Rewrite (Exp,
                  --    Make_Explicit_Dereference (Loc,
                  --      Prefix => New_Occurrence_Of (Tnn, Loc)));
               end if;
            end;
         end if;

         --  Generate call to _postconditions

         Insert_Action (Exp,
           Make_Procedure_Call_Statement (Loc,
             Name => Make_Identifier (Loc, Name_uPostconditions),
             Parameter_Associations => New_List (Duplicate_Subexpr (Exp))));
      end if;

      --  Ada 2005 (AI-251): If this return statement corresponds with an
      --  simple return statement associated with an extended return statement
      --  and the type of the returned object is an interface then generate an
      --  implicit conversion to force displacement of the "this" pointer.

      if Ada_Version >= Ada_05
        and then Comes_From_Extended_Return_Statement (N)
        and then Nkind (Expression (N)) = N_Identifier
        and then Is_Interface (Utyp)
        and then Utyp /= Underlying_Type (Exptyp)
      then
         Rewrite (Exp, Convert_To (Utyp, Relocate_Node (Exp)));
         Analyze_And_Resolve (Exp);
      end if;
   end Expand_Simple_Function_Return;

   ------------------------------
   -- Make_Tag_Ctrl_Assignment --
   ------------------------------

   function Make_Tag_Ctrl_Assignment (N : Node_Id) return List_Id is
      Loc : constant Source_Ptr := Sloc (N);
      L   : constant Node_Id    := Name (N);
      T   : constant Entity_Id  := Underlying_Type (Etype (L));

      Ctrl_Act : constant Boolean := Needs_Finalization (T)
                                       and then not No_Ctrl_Actions (N);

      Component_Assign : constant Boolean :=
                           Is_Fully_Repped_Tagged_Type (T);

      Save_Tag : constant Boolean := Is_Tagged_Type (T)
                                       and then not Component_Assign
                                       and then not No_Ctrl_Actions (N)
                                       and then Tagged_Type_Expansion;
      --  Tags are not saved and restored when VM_Target because VM tags are
      --  represented implicitly in objects.

      Res      : List_Id;
      Tag_Tmp  : Entity_Id;

      Prev_Tmp : Entity_Id;
      Next_Tmp : Entity_Id;
      Ctrl_Ref : Node_Id;

   begin
      Res := New_List;

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
         Append_List_To (Res,
           Make_Final_Call
             (Ref         => Duplicate_Subexpr_No_Checks (L),
              Typ         => Etype (L),
              With_Detach => New_Reference_To (Standard_False, Loc)));
      end if;

      --  Save the Tag in a local variable Tag_Tmp

      if Save_Tag then
         Tag_Tmp := Make_Temporary (Loc, 'A');

         Append_To (Res,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Tag_Tmp,
             Object_Definition => New_Reference_To (RTE (RE_Tag), Loc),
             Expression =>
               Make_Selected_Component (Loc,
                 Prefix        => Duplicate_Subexpr_No_Checks (L),
                 Selector_Name => New_Reference_To (First_Tag_Component (T),
                                                    Loc))));

      --  Otherwise Tag_Tmp not used

      else
         Tag_Tmp := Empty;
      end if;

      if Ctrl_Act then
         if VM_Target /= No_VM then

            --  Cannot assign part of the object in a VM context, so instead
            --  fallback to the previous mechanism, even though it is not
            --  completely correct ???

            --  Save the Finalization Pointers in local variables Prev_Tmp and
            --  Next_Tmp. For objects with Has_Controlled_Component set, these
            --  pointers are in the Record_Controller

            Ctrl_Ref := Duplicate_Subexpr (L);

            if Has_Controlled_Component (T) then
               Ctrl_Ref :=
                 Make_Selected_Component (Loc,
                   Prefix => Ctrl_Ref,
                   Selector_Name =>
                     New_Reference_To (Controller_Component (T), Loc));
            end if;

            Prev_Tmp := Make_Temporary (Loc, 'B');

            Append_To (Res,
              Make_Object_Declaration (Loc,
                Defining_Identifier => Prev_Tmp,

                Object_Definition =>
                  New_Reference_To (RTE (RE_Finalizable_Ptr), Loc),

                Expression =>
                  Make_Selected_Component (Loc,
                    Prefix =>
                      Unchecked_Convert_To (RTE (RE_Finalizable), Ctrl_Ref),
                    Selector_Name => Make_Identifier (Loc, Name_Prev))));

            Next_Tmp := Make_Temporary (Loc, 'C');

            Append_To (Res,
              Make_Object_Declaration (Loc,
                Defining_Identifier => Next_Tmp,

                Object_Definition   =>
                  New_Reference_To (RTE (RE_Finalizable_Ptr), Loc),

                Expression          =>
                  Make_Selected_Component (Loc,
                    Prefix =>
                      Unchecked_Convert_To (RTE (RE_Finalizable),
                        New_Copy_Tree (Ctrl_Ref)),
                    Selector_Name => Make_Identifier (Loc, Name_Next))));

            --  Do the Assignment

            Append_To (Res, Relocate_Node (N));

         else
            --  Regular (non VM) processing for controlled types and types with
            --  controlled components

            --  Variables of such types contain pointers used to chain them in
            --  finalization lists, in addition to user data. These pointers
            --  are specific to each object of the type, not to the value being
            --  assigned.

            --  Thus they need to be left intact during the assignment. We
            --  achieve this by constructing a Storage_Array subtype, and by
            --  overlaying objects of this type on the source and target of the
            --  assignment. The assignment is then rewritten to assignments of
            --  slices of these arrays, copying the user data, and leaving the
            --  pointers untouched.

            Controlled_Actions : declare
               Prev_Ref : Node_Id;
               --  A reference to the Prev component of the record controller

               First_After_Root : Node_Id := Empty;
               --  Index of first byte to be copied (used to skip
               --  Root_Controlled in controlled objects).

               Last_Before_Hole : Node_Id := Empty;
               --  Index of last byte to be copied before outermost record
               --  controller data.

               Hole_Length : Node_Id := Empty;
               --  Length of record controller data (Prev and Next pointers)

               First_After_Hole : Node_Id := Empty;
               --  Index of first byte to be copied after outermost record
               --  controller data.

               Expr, Source_Size     : Node_Id;
               Source_Actual_Subtype : Entity_Id;
               --  Used for computation of the size of the data to be copied

               Range_Type  : Entity_Id;
               Opaque_Type : Entity_Id;

               function Build_Slice
                 (Rec : Entity_Id;
                  Lo  : Node_Id;
                  Hi  : Node_Id) return Node_Id;
               --  Build and return a slice of an array of type S overlaid on
               --  object Rec, with bounds specified by Lo and Hi. If either
               --  bound is empty, a default of S'First (respectively S'Last)
               --  is used.

               -----------------
               -- Build_Slice --
               -----------------

               function Build_Slice
                 (Rec : Node_Id;
                  Lo  : Node_Id;
                  Hi  : Node_Id) return Node_Id
               is
                  Lo_Bound : Node_Id;
                  Hi_Bound : Node_Id;

                  Opaque : constant Node_Id :=
                             Unchecked_Convert_To (Opaque_Type,
                               Make_Attribute_Reference (Loc,
                                 Prefix         => Rec,
                                 Attribute_Name => Name_Address));
                  --  Access value designating an opaque storage array of type
                  --  S overlaid on record Rec.

               begin
                  --  Compute slice bounds using S'First (1) and S'Last as
                  --  default values when not specified by the caller.

                  if No (Lo) then
                     Lo_Bound := Make_Integer_Literal (Loc, 1);
                  else
                     Lo_Bound := Lo;
                  end if;

                  if No (Hi) then
                     Hi_Bound := Make_Attribute_Reference (Loc,
                       Prefix => New_Occurrence_Of (Range_Type, Loc),
                       Attribute_Name => Name_Last);
                  else
                     Hi_Bound := Hi;
                  end if;

                  return Make_Slice (Loc,
                    Prefix =>
                      Opaque,
                    Discrete_Range => Make_Range (Loc,
                      Lo_Bound, Hi_Bound));
               end Build_Slice;

            --  Start of processing for Controlled_Actions

            begin
               --  Create a constrained subtype of Storage_Array whose size
               --  corresponds to the value being assigned.

               --  subtype G is Storage_Offset range
               --    1 .. (Expr'Size + Storage_Unit - 1) / Storage_Unit

               Expr := Duplicate_Subexpr_No_Checks (Expression (N));

               if Nkind (Expr) = N_Qualified_Expression then
                  Expr := Expression (Expr);
               end if;

               Source_Actual_Subtype := Etype (Expr);

               if Has_Discriminants (Source_Actual_Subtype)
                 and then not Is_Constrained (Source_Actual_Subtype)
               then
                  Append_To (Res,
                    Build_Actual_Subtype (Source_Actual_Subtype, Expr));
                  Source_Actual_Subtype := Defining_Identifier (Last (Res));
               end if;

               Source_Size :=
                 Make_Op_Add (Loc,
                   Left_Opnd =>
                     Make_Attribute_Reference (Loc,
                       Prefix =>
                         New_Occurrence_Of (Source_Actual_Subtype, Loc),
                     Attribute_Name => Name_Size),
                   Right_Opnd =>
                     Make_Integer_Literal (Loc,
                       Intval => System_Storage_Unit - 1));

               Source_Size :=
                 Make_Op_Divide (Loc,
                   Left_Opnd => Source_Size,
                   Right_Opnd =>
                     Make_Integer_Literal (Loc,
                       Intval => System_Storage_Unit));

               Range_Type := Make_Temporary (Loc, 'G');

               Append_To (Res,
                 Make_Subtype_Declaration (Loc,
                   Defining_Identifier => Range_Type,
                   Subtype_Indication =>
                     Make_Subtype_Indication (Loc,
                       Subtype_Mark =>
                         New_Reference_To (RTE (RE_Storage_Offset), Loc),
                       Constraint   => Make_Range_Constraint (Loc,
                         Range_Expression =>
                           Make_Range (Loc,
                             Low_Bound  => Make_Integer_Literal (Loc, 1),
                             High_Bound => Source_Size)))));

               --  subtype S is Storage_Array (G)

               Append_To (Res,
                 Make_Subtype_Declaration (Loc,
                   Defining_Identifier => Make_Temporary (Loc, 'S'),
                   Subtype_Indication  =>
                     Make_Subtype_Indication (Loc,
                       Subtype_Mark =>
                         New_Reference_To (RTE (RE_Storage_Array), Loc),
                       Constraint =>
                         Make_Index_Or_Discriminant_Constraint (Loc,
                           Constraints =>
                             New_List (New_Reference_To (Range_Type, Loc))))));

               --  type A is access S

               Opaque_Type := Make_Temporary (Loc, 'A');

               Append_To (Res,
                 Make_Full_Type_Declaration (Loc,
                   Defining_Identifier => Opaque_Type,
                   Type_Definition     =>
                     Make_Access_To_Object_Definition (Loc,
                       Subtype_Indication =>
                         New_Occurrence_Of (
                           Defining_Identifier (Last (Res)), Loc))));

               --  Generate appropriate slice assignments

               First_After_Root := Make_Integer_Literal (Loc, 1);

               --  For controlled object, skip Root_Controlled part

               if Is_Controlled (T) then
                  First_After_Root :=
                    Make_Op_Add (Loc,
                      First_After_Root,
                      Make_Op_Divide (Loc,
                        Make_Attribute_Reference (Loc,
                          Prefix =>
                            New_Occurrence_Of (RTE (RE_Root_Controlled), Loc),
                          Attribute_Name => Name_Size),
                        Make_Integer_Literal (Loc, System_Storage_Unit)));
               end if;

               --  For the case of a record with controlled components, skip
               --  record controller Prev/Next components. These components
               --  constitute a 'hole' in the middle of the data to be copied.

               if Has_Controlled_Component (T) then
                  Prev_Ref :=
                    Make_Selected_Component (Loc,
                      Prefix =>
                        Make_Selected_Component (Loc,
                          Prefix => Duplicate_Subexpr_No_Checks (L),
                          Selector_Name =>
                            New_Reference_To (Controller_Component (T), Loc)),
                      Selector_Name =>  Make_Identifier (Loc, Name_Prev));

                  --  Last index before hole: determined by position of the
                  --  _Controller.Prev component.

                  Last_Before_Hole := Make_Temporary (Loc, 'L');

                  Append_To (Res,
                    Make_Object_Declaration (Loc,
                      Defining_Identifier => Last_Before_Hole,
                      Object_Definition   => New_Occurrence_Of (
                        RTE (RE_Storage_Offset), Loc),
                      Constant_Present    => True,
                      Expression          =>
                        Make_Op_Add (Loc,
                          Make_Attribute_Reference (Loc,
                            Prefix => Prev_Ref,
                            Attribute_Name => Name_Position),
                          Make_Attribute_Reference (Loc,
                            Prefix => New_Copy_Tree (Prefix (Prev_Ref)),
                            Attribute_Name => Name_Position))));

                  --  Hole length: size of the Prev and Next components

                  Hole_Length :=
                    Make_Op_Multiply (Loc,
                      Left_Opnd  => Make_Integer_Literal (Loc, Uint_2),
                      Right_Opnd =>
                        Make_Op_Divide (Loc,
                          Left_Opnd =>
                            Make_Attribute_Reference (Loc,
                              Prefix         => New_Copy_Tree (Prev_Ref),
                              Attribute_Name => Name_Size),
                          Right_Opnd =>
                            Make_Integer_Literal (Loc,
                              Intval => System_Storage_Unit)));

                  --  First index after hole

                  First_After_Hole := Make_Temporary (Loc, 'F');

                  Append_To (Res,
                    Make_Object_Declaration (Loc,
                      Defining_Identifier => First_After_Hole,
                      Object_Definition   => New_Occurrence_Of (
                        RTE (RE_Storage_Offset), Loc),
                      Constant_Present    => True,
                      Expression          =>
                        Make_Op_Add (Loc,
                          Left_Opnd  =>
                            Make_Op_Add (Loc,
                              Left_Opnd  =>
                                New_Occurrence_Of (Last_Before_Hole, Loc),
                              Right_Opnd => Hole_Length),
                          Right_Opnd => Make_Integer_Literal (Loc, 1))));

                  Last_Before_Hole :=
                    New_Occurrence_Of (Last_Before_Hole, Loc);
                  First_After_Hole :=
                    New_Occurrence_Of (First_After_Hole, Loc);
               end if;

               --  Assign the first slice (possibly skipping Root_Controlled,
               --  up to the beginning of the record controller if present,
               --  up to the end of the object if not).

               Append_To (Res, Make_Assignment_Statement (Loc,
                 Name       => Build_Slice (
                   Rec => Duplicate_Subexpr_No_Checks (L),
                   Lo  => First_After_Root,
                   Hi  => Last_Before_Hole),

                 Expression => Build_Slice (
                   Rec => Expression (N),
                   Lo  => First_After_Root,
                   Hi  => New_Copy_Tree (Last_Before_Hole))));

               if Present (First_After_Hole) then

                  --  If a record controller is present, copy the second slice,
                  --  from right after the _Controller.Next component up to the
                  --  end of the object.

                  Append_To (Res, Make_Assignment_Statement (Loc,
                    Name       => Build_Slice (
                      Rec => Duplicate_Subexpr_No_Checks (L),
                      Lo  => First_After_Hole,
                      Hi  => Empty),
                    Expression => Build_Slice (
                      Rec => Duplicate_Subexpr_No_Checks (Expression (N)),
                      Lo  => New_Copy_Tree (First_After_Hole),
                      Hi  => Empty)));
               end if;
            end Controlled_Actions;
         end if;

      --  Not controlled case

      else
         declare
            Asn : constant Node_Id := Relocate_Node (N);

         begin
            --  If this is the case of a tagged type with a full rep clause,
            --  we must expand it into component assignments, so we mark the
            --  node as unanalyzed, to get it reanalyzed, but flag it has
            --  requiring component-wise assignment so we don't get infinite
            --  recursion.

            if Component_Assign then
               Set_Analyzed (Asn, False);
               Set_Componentwise_Assignment (Asn, True);
            end if;

            Append_To (Res, Asn);
         end;
      end if;

      --  Restore the tag

      if Save_Tag then
         Append_To (Res,
           Make_Assignment_Statement (Loc,
             Name =>
               Make_Selected_Component (Loc,
                 Prefix        => Duplicate_Subexpr_No_Checks (L),
                 Selector_Name => New_Reference_To (First_Tag_Component (T),
                                                    Loc)),
             Expression => New_Reference_To (Tag_Tmp, Loc)));
      end if;

      if Ctrl_Act then
         if VM_Target /= No_VM then
            --  Restore the finalization pointers

            Append_To (Res,
              Make_Assignment_Statement (Loc,
                Name =>
                  Make_Selected_Component (Loc,
                    Prefix =>
                      Unchecked_Convert_To (RTE (RE_Finalizable),
                        New_Copy_Tree (Ctrl_Ref)),
                    Selector_Name => Make_Identifier (Loc, Name_Prev)),
                Expression => New_Reference_To (Prev_Tmp, Loc)));

            Append_To (Res,
              Make_Assignment_Statement (Loc,
                Name =>
                  Make_Selected_Component (Loc,
                    Prefix =>
                      Unchecked_Convert_To (RTE (RE_Finalizable),
                        New_Copy_Tree (Ctrl_Ref)),
                    Selector_Name => Make_Identifier (Loc, Name_Next)),
                Expression => New_Reference_To (Next_Tmp, Loc)));
         end if;

         --  Adjust the target after the assignment when controlled (not in the
         --  init proc since it is an initialization more than an assignment).

         Append_List_To (Res,
           Make_Adjust_Call (
             Ref         => Duplicate_Subexpr_Move_Checks (L),
             Typ         => Etype (L),
             Flist_Ref   => New_Reference_To (RTE (RE_Global_Final_List), Loc),
             With_Attach => Make_Integer_Literal (Loc, 0)));
      end if;

      return Res;

   exception
      --  Could use comment here ???

      when RE_Not_Available =>
         return Empty_List;
   end Make_Tag_Ctrl_Assignment;

end Exp_Ch5;
