------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              E X P _ C H 5                               --
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
with Exp_Aggr; use Exp_Aggr;
with Exp_Ch7;  use Exp_Ch7;
with Exp_Ch11; use Exp_Ch11;
with Exp_Dbug; use Exp_Dbug;
with Exp_Pakd; use Exp_Pakd;
with Exp_Tss;  use Exp_Tss;
with Exp_Util; use Exp_Util;
with Hostparm; use Hostparm;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Restrict; use Restrict;
with Rident;   use Rident;
with Rtsfind;  use Rtsfind;
with Sinfo;    use Sinfo;
with Sem;      use Sem;
with Sem_Ch3;  use Sem_Ch3;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Ch13; use Sem_Ch13;
with Sem_Eval; use Sem_Eval;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Snames;   use Snames;
with Stand;    use Stand;
with Stringt;  use Stringt;
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
   --  of representation.

   function Make_Tag_Ctrl_Assignment (N : Node_Id) return List_Id;
   --  Generate the necessary code for controlled and tagged assignment,
   --  that is to say, finalization of the target before, adjustement of
   --  the target after and save and restore of the tag and finalization
   --  pointers which are not 'part of the value' and must not be changed
   --  upon assignment. N is the original Assignment node.

   function Possible_Bit_Aligned_Component (N : Node_Id) return Boolean;
   --  This function is used in processing the assignment of a record or
   --  indexed component. The argument N is either the left hand or right
   --  hand side of an assignment, and this function determines if there
   --  is a record component reference where the record may be bit aligned
   --  in a manner that causes trouble for the back end (see description
   --  of Exp_Util.Component_May_Be_Bit_Aligned for further details).

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

      procedure Apply_Dereference (Arg : in out Node_Id);
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

      procedure Apply_Dereference (Arg : in out Node_Id) is
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
      --  Deal with length check, note that the length check is done with
      --  respect to the right hand side as given, not a possible underlying
      --  renamed object, since this would generate incorrect extra checks.

      Apply_Length_Check (Rhs, L_Type);

      --  We start by assuming that the move can be done in either
      --  direction, i.e. that the two sides are completely disjoint.

      Set_Forwards_OK  (N, True);
      Set_Backwards_OK (N, True);

      --  Normally it is only the slice case that can lead to overlap,
      --  and explicit checks for slices are made below. But there is
      --  one case where the slice can be implicit and invisible to us
      --  and that is the case where we have a one dimensional array,
      --  and either both operands are parameters, or one is a parameter
      --  and the other is a global variable. In this case the parameter
      --  could be a slice that overlaps with the other parameter.

      --  Check for the case of slices requiring an explicit loop. Normally
      --  it is only the explicit slice cases that bother us, but in the
      --  case of one dimensional arrays, parameters can be slices that
      --  are passed by reference, so we can have aliasing for assignments
      --  from one parameter to another, or assignments between parameters
      --  and nonlocal variables. However, if the array subtype is a
      --  constrained first subtype in the parameter case, then we don't
      --  have to worry about overlap, since slice assignments aren't
      --  possible (other than for a slice denoting the whole array).

      --  Note: overlap is never possible if there is a change of
      --  representation, so we can exclude this case.

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

         --  In the case of compiling for the Java Virtual Machine,
         --  slices are always passed by making a copy, so we don't
         --  have to worry about overlap. We also want to prevent
         --  generation of "<" comparisons for array addresses,
         --  since that's a meaningless operation on the JVM.

        and then not Java_VM
      then
         Set_Forwards_OK  (N, False);
         Set_Backwards_OK (N, False);

         --  Note: the bit-packed case is not worrisome here, since if
         --  we have a slice passed as a parameter, it is always aligned
         --  on a byte boundary, and if there are no explicit slices, the
         --  assignment can be performed directly.
      end if;

      --  We certainly must use a loop for change of representation
      --  and also we use the operand of the conversion on the right
      --  hand side as the effective right hand side (the component
      --  types must match in this situation).

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

      --  Arrays with controlled components are expanded into a loop
      --  to force calls to adjust at the component level.

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

         --  The following code deals with the case of unconstrained bit
         --  packed arrays. The problem is that the template for such
         --  arrays contains the bounds of the actual source level array,

         --  But the copy of an entire array requires the bounds of the
         --  underlying array. It would be nice if the back end could take
         --  care of this, but right now it does not know how, so if we
         --  have such a type, then we expand out into a loop, which is
         --  inefficient but works correctly. If we don't do this, we
         --  get the wrong length computed for the array to be moved.
         --  The two cases we need to worry about are:

         --  Explicit deference of an unconstrained packed array type as
         --  in the following example:

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

         --  A formal parameter reference with an unconstrained bit
         --  array type is the other case we need to worry about (here
         --  we assume the same BITS type declared above:

         --    procedure Write_All (File : out BITS; Contents : in  BITS);
         --    begin
         --       File.Storage := Contents;
         --    end Write_All;

         --  We expand to a loop in either of these two cases

         --  Question for future thought. Another potentially more efficient
         --  approach would be to create the actual subtype, and then do an
         --  unchecked conversion to this actual subtype ???

         Check_Unconstrained_Bit_Packed_Array : declare

            function Is_UBPA_Reference (Opnd : Node_Id) return Boolean;
            --  Function to perform required test for the first case,
            --  above (dereference of an unconstrained bit packed array)

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

            --  Here if we do not have the case of a reference to a bit
            --  packed unconstrained array case. In this case gigi can
            --  most certainly handle the assignment if a forwards move
            --  is allowed.

            --  (could it handle the backwards case also???)

            elsif Forwards_OK (N) then
               return;
            end if;
         end Check_Unconstrained_Bit_Packed_Array;

      --  The back end can always handle the assignment if the right side is a
      --  string literal (note that overlap is definitely impossible in this
      --  case). If the type is packed, a string literal is always converted
      --  into aggregate, except in the case of a null slice, for which no
      --  aggregate can be written. In that case, rewrite the assignment as a
      --  null statement, a length check has already been emitted to verify
      --  that the range of the left-hand side is empty.

      --  Note that this code is not executed if we had an assignment of
      --  a string literal to a non-bit aligned component of a record, a
      --  case which cannot be handled by the backend

      elsif Nkind (Rhs) = N_String_Literal then
         if String_Length (Strval (Rhs)) = 0
           and then Is_Bit_Packed_Array (L_Type)
         then
            Rewrite (N, Make_Null_Statement (Loc));
            Analyze (N);
         end if;

         return;

      --  If either operand is bit packed, then we need a loop, since we
      --  can't be sure that the slice is byte aligned. Similarly, if either
      --  operand is a possibly unaligned slice, then we need a loop (since
      --  the back end cannot handle unaligned slices).

      elsif Is_Bit_Packed_Array (L_Type)
        or else Is_Bit_Packed_Array (R_Type)
        or else Is_Possibly_Unaligned_Slice (Lhs)
        or else Is_Possibly_Unaligned_Slice (Rhs)
      then
         Loop_Required := True;

      --  If we are not bit-packed, and we have only one slice, then no
      --  overlap is possible except in the parameter case, so we can let
      --  the back end handle things.

      elsif not (L_Slice and R_Slice) then
         if Forwards_OK (N) then
            return;
         end if;
      end if;

      --  If the right-hand side is a string literal, introduce a temporary
      --  for it, for use in the generated loop that will follow.

      if Nkind (Rhs) = N_String_Literal then
         declare
            Temp : constant Entity_Id :=
                     Make_Defining_Identifier (Loc, Name_T);
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

      --  Our task at this stage is to complete the overlap analysis, which
      --  can result in possibly setting Forwards_OK or Backwards_OK to
      --  False, and then generating the final code, either by deciding
      --  that it is OK after all to let Gigi handle it, or by generating
      --  appropriate code in the front end.

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
         --  direct assignments to an array that is a private type, but
         --  we cannot assign to elements of the array without this extra
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

         --  If both sides are slices, we must figure out whether
         --  it is safe to do the move in one direction or the other
         --  It is always safe if there is a change of representation
         --  since obviously two arrays with different representations
         --  cannot possibly overlap.

         if (not Crep) and L_Slice and R_Slice then
            Act_L_Array := Get_Referenced_Object (Prefix (Act_Lhs));
            Act_R_Array := Get_Referenced_Object (Prefix (Act_Rhs));

            --  If both left and right hand arrays are entity names, and
            --  refer to different entities, then we know that the move
            --  is safe (the two storage areas are completely disjoint).

            if Is_Entity_Name (Act_L_Array)
              and then Is_Entity_Name (Act_R_Array)
              and then Entity (Act_L_Array) /= Entity (Act_R_Array)
            then
               null;

            --  Otherwise, we assume the worst, which is that the two
            --  arrays are the same array. There is no need to check if
            --  we know that is the case, because if we don't know it,
            --  we still have to assume it!

            --  Generally if the same array is involved, then we have
            --  an overlapping case. We will have to really assume the
            --  worst (i.e. set neither of the OK flags) unless we can
            --  determine the lower or upper bounds at compile time and
            --  compare them.

            else
               Cresult := Compile_Time_Compare (Left_Lo, Right_Lo);

               if Cresult = Unknown then
                  Cresult := Compile_Time_Compare (Left_Hi, Right_Hi);
               end if;

               case Cresult is
                  when LT | LE | EQ => Set_Backwards_OK (N, False);
                  when GT | GE      => Set_Forwards_OK  (N, False);
                  when NE | Unknown => Set_Backwards_OK (N, False);
                                       Set_Forwards_OK  (N, False);
               end case;
            end if;
         end if;

         --  If after that analysis, Forwards_OK is still True, and
         --  Loop_Required is False, meaning that we have not discovered
         --  some non-overlap reason for requiring a loop, then we can
         --  still let gigi handle it.

         if not Loop_Required then
            if Forwards_OK (N) then
               return;
            else
               null;
               --  Here is where a memmove would be appropriate ???
            end if;
         end if;

         --  At this stage we have to generate an explicit loop, and
         --  we have the following cases:

         --  Forwards_OK = True

         --    Rnn : right_index := right_index'First;
         --    for Lnn in left-index loop
         --       left (Lnn) := right (Rnn);
         --       Rnn := right_index'Succ (Rnn);
         --    end loop;

         --    Note: the above code MUST be analyzed with checks off,
         --    because otherwise the Succ could overflow. But in any
         --    case this is more efficient!

         --  Forwards_OK = False, Backwards_OK = True

         --    Rnn : right_index := right_index'Last;
         --    for Lnn in reverse left-index loop
         --       left (Lnn) := right (Rnn);
         --       Rnn := right_index'Pred (Rnn);
         --    end loop;

         --    Note: the above code MUST be analyzed with checks off,
         --    because otherwise the Pred could overflow. But in any
         --    case this is more efficient!

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

         --  Cases where either Forwards_OK or Backwards_OK is true

         if Forwards_OK (N) or else Backwards_OK (N) then
            if Controlled_Type (Component_Type (L_Type))
              and then Base_Type (L_Type) = Base_Type (R_Type)
              and then Ndim = 1
              and then not No_Ctrl_Actions (N)
            then
               declare
                  Proc : constant Entity_Id :=
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
            --  Before we generate this code, we must ensure that the
            --  left and right side array types are defined. They may
            --  be itypes, and we cannot let them be defined inside the
            --  if, since the first use in the then may not be executed.

            Ensure_Defined (L_Type, N);
            Ensure_Defined (R_Type, N);

            --  We normally compare addresses to find out which way round
            --  to do the loop, since this is realiable, and handles the
            --  cases of parameters, conversions etc. But we can't do that
            --  in the bit packed case or the Java VM case, because addresses
            --  don't work there.

            if not Is_Bit_Packed_Array (L_Type) and then not Java_VM then
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

            --  For the bit packed and Java VM cases we use the bounds.
            --  That's OK, because we don't have to worry about parameters,
            --  since they cannot cause overlap. Perhaps we should worry
            --  about weird slice conversions ???

            else
               --  Copy the bounds and reset the Analyzed flag, because the
               --  bounds of the index type itself may be universal, and must
               --  must be reaanalyzed to acquire the proper type for Gigi.

               Cleft_Lo  := New_Copy_Tree (Left_Lo);
               Cright_Lo := New_Copy_Tree (Right_Lo);
               Set_Analyzed (Cleft_Lo, False);
               Set_Analyzed (Cright_Lo, False);

               Condition :=
                 Make_Op_Le (Loc,
                   Left_Opnd  => Cleft_Lo,
                   Right_Opnd => Cright_Lo);
            end if;

            if Controlled_Type (Component_Type (L_Type))
              and then Base_Type (L_Type) = Base_Type (R_Type)
              and then Ndim = 1
              and then not No_Ctrl_Actions (N)
            then

               --  Call TSS procedure for array assignment, passing the
               --  the explicit bounds of right and left hand sides.

               declare
                  Proc    : constant Node_Id :=
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

   --  The following is an example of the loop generated for the case of
   --  a two-dimensional array:

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

   --  Here Rev is False, and Tm1Xn are the subscript types for the right
   --  hand side. The declarations of R2b and R4b are inserted before the
   --  original assignment statement.

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
            Lnn (J) :=
              Make_Defining_Identifier (Loc,
                Chars => New_Internal_Name ('L'));

            Rnn (J) :=
              Make_Defining_Identifier (Loc,
                Chars => New_Internal_Name ('R'));

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

                   Statements => New_List (
                     Assign,

                     Make_Assignment_Statement (Loc,
                       Name => New_Occurrence_Of (Rnn (J), Loc),
                       Expression =>
                         Make_Attribute_Reference (Loc,
                           Prefix =>
                             New_Occurrence_Of (R_Index_Type (J), Loc),
                           Attribute_Name => S_Or_P,
                           Expressions => New_List (
                             New_Occurrence_Of (Rnn (J), Loc)))))))));
      end loop;

      return Assign;
   end Expand_Assign_Array_Loop;

   --------------------------
   -- Expand_Assign_Record --
   --------------------------

   --  The only processing required is in the change of representation
   --  case, where we must expand the assignment to a series of field
   --  by field assignments.

   procedure Expand_Assign_Record (N : Node_Id) is
      Lhs : constant Node_Id := Name (N);
      Rhs : Node_Id          := Expression (N);

   begin
      --  If change of representation, then extract the real right hand
      --  side from the type conversion, and proceed with component-wise
      --  assignment, since the two types are not the same as far as the
      --  back end is concerned.

      if Change_Of_Representation (N) then
         Rhs := Expression (Rhs);

      --  If this may be a case of a large bit aligned component, then
      --  proceed with component-wise assignment, to avoid possible
      --  clobbering of other components sharing bits in the first or
      --  last byte of the component to be assigned.

      elsif Possible_Bit_Aligned_Component (Lhs)
              or
            Possible_Bit_Aligned_Component (Rhs)
      then
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
         L_Typ : constant Entity_Id  := Base_Type (Etype (Lhs));
         Decl  : constant Node_Id    := Declaration_Node (R_Typ);
         RDef  : Node_Id;
         F     : Entity_Id;

         function Find_Component
           (Typ  : Entity_Id;
            Comp : Entity_Id) return Entity_Id;
         --  Find the component with the given name in the underlying record
         --  declaration for Typ. We need to use the actual entity because
         --  the type may be private and resolution by identifier alone would
         --  fail.

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
               if Nkind (Item) = N_Component_Declaration then
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

               if Is_Unchecked_Union (Base_Type (R_Typ)) then
                  Insert_Action (N, Make_Field_Assign (F, True));
               else
                  Insert_Action (N, Make_Field_Assign (F));
               end if;

               Next_Discriminant (F);
            end loop;
         end if;

         --  We know the underlying type is a record, but its current view
         --  may be private. We must retrieve the usable record declaration.

         if Nkind (Decl) = N_Private_Type_Declaration
           and then Present (Full_View (R_Typ))
         then
            RDef := Type_Definition (Declaration_Node (Full_View (R_Typ)));
         else
            RDef := Type_Definition (Decl);
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
      --  First deal with generation of range check if required. For now
      --  we do this only for discrete types.

      if Do_Range_Check (Rhs)
        and then Is_Discrete_Type (Typ)
      then
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
      --     reference to a bit packed array.

      --    An indexed component or selected component whose prefix is a
      --     reference to a bit packed array is itself a reference ot a
      --     bit packed array.

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

      if (Nkind (Lhs) = N_Indexed_Component
           or else
          Nkind (Lhs) = N_Selected_Component)
        and then Is_Ref_To_Bit_Packed_Array (Prefix (Lhs))
      then
         declare
            BPAR_Expr : constant Node_Id   := Relocate_Node (Prefix (Lhs));
            BPAR_Typ  : constant Entity_Id := Etype (BPAR_Expr);
            Tnn       : constant Entity_Id :=
                          Make_Defining_Identifier (Loc,
                            Chars => New_Internal_Name ('T'));

         begin
            --  Insert the post assignment first, because we want to copy
            --  the BPAR_Expr tree before it gets analyzed in the context
            --  of the pre assignment. Note that we do not analyze the
            --  post assignment yet (we cannot till we have completed the
            --  analysis of the pre assignment). As usual, the analysis
            --  of this post assignment will happen on its own when we
            --  "run into" it after finishing the current assignment.

            Insert_After (N,
              Make_Assignment_Statement (Loc,
                Name       => New_Copy_Tree (BPAR_Expr),
                Expression => New_Occurrence_Of (Tnn, Loc)));

            --  At this stage BPAR_Expr is a reference to a bit packed
            --  array where the reference was not expanded in the original
            --  tree, since it was on the left side of an assignment. But
            --  in the pre-assignment statement (the object definition),
            --  BPAR_Expr will end up on the right hand side, and must be
            --  reexpanded. To achieve this, we reset the analyzed flag
            --  of all selected and indexed components down to the actual
            --  indexed component for the packed array.

            Exp := BPAR_Expr;
            loop
               Set_Analyzed (Exp, False);

               if Nkind (Exp) = N_Selected_Component
                    or else
                  Nkind (Exp) = N_Indexed_Component
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
                  New_Scope (Scope (Current_Scope));
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
            --  since we now have a generate assignment to it!

            Set_Is_True_Constant (Tnn, False);
         end;
      end if;

      --  When we have the appropriate type of aggregate in the
      --  expression (it has been determined during analysis of the
      --  aggregate by setting the delay flag), let's perform in place
      --  assignment and thus avoid creating a temporay.

      if Is_Delayed_Aggregate (Rhs) then
         Convert_Aggr_In_Assignment (N);
         Rewrite (N, Make_Null_Statement (Loc));
         Analyze (N);
         return;
      end if;

      --  Apply discriminant check if required. If Lhs is an access type
      --  to a designated type with discriminants, we must always check.

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
        and then  Has_Discriminants (Typ)
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

      --  In the access type case, we need the same discriminant check,
      --  and also range checks if we have an access to constrained array.

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
                    Range_Check
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

      --  Ada 2005 (AI-231): Generate conversion to the null-excluding
      --  type to force the corresponding run-time check

      if Is_Access_Type (Typ)
        and then
          ((Is_Entity_Name (Lhs) and then Can_Never_Be_Null (Entity (Lhs)))
             or else Can_Never_Be_Null (Etype (Lhs)))
      then
         Rewrite (Rhs, Convert_To (Etype (Lhs),
                                   Relocate_Node (Rhs)));
         Analyze_And_Resolve (Rhs, Etype (Lhs));
      end if;

      --  If we are assigning an access type and the left side is an
      --  entity, then make sure that Is_Known_Non_Null properly
      --  reflects the state of the entity after the assignment

      if Is_Access_Type (Typ)
        and then Is_Entity_Name (Lhs)
        and then Known_Non_Null (Rhs)
        and then Safe_To_Capture_Value (N, Entity (Lhs))
      then
         Set_Is_Known_Non_Null (Entity (Lhs), Known_Non_Null (Rhs));
      end if;

      --  Case of assignment to a bit packed array element

      if Nkind (Lhs) = N_Indexed_Component
        and then Is_Bit_Packed_Array (Etype (Prefix (Lhs)))
      then
         Expand_Bit_Packed_Element_Set (N);
         return;

      elsif Is_Tagged_Type (Typ)
        or else (Controlled_Type (Typ) and then not Is_Array_Type (Typ))
      then
         Tagged_Case : declare
            L                   : List_Id := No_List;
            Expand_Ctrl_Actions : constant Boolean := not No_Ctrl_Actions (N);

         begin
            --  In the controlled case, we need to make sure that function
            --  calls are evaluated before finalizing the target. In all
            --  cases, it makes the expansion easier if the side-effects
            --  are removed first.

            Remove_Side_Effects (Lhs);
            Remove_Side_Effects (Rhs);

            --  Avoid recursion in the mechanism

            Set_Analyzed (N);

            --  If dispatching assignment, we need to dispatch to _assign

            if Is_Class_Wide_Type (Typ)

               --  If the type is tagged, we may as well use the predefined
               --  primitive assignment. This avoids inlining a lot of code
               --  and in the class-wide case, the assignment is replaced by
               --  dispatch call to _assign. Note that this cannot be done
               --  when discriminant checks are locally suppressed (as in
               --  extension aggregate expansions) because otherwise the
               --  discriminant check will be performed within the _assign
               --  call. It is also suppressed for assignmments created by the
               --  expander that correspond to initializations, where we do
               --  want to copy the tag (No_Ctrl_Actions flag set True).
               --  by the expander and we do not need to mess with tags ever
               --  (Expand_Ctrl_Actions flag is set True in this case).

               or else (Is_Tagged_Type (Typ)
                          and then Chars (Current_Scope) /= Name_uAssign
                          and then Expand_Ctrl_Actions
                          and then not Discriminant_Checks_Suppressed (Empty))
            then
               --  Fetch the primitive op _assign and proper type to call
               --  it. Because of possible conflits between private and
               --  full view the proper type is fetched directly from the
               --  operation profile.

               declare
                  Op    : constant Entity_Id :=
                            Find_Prim_Op (Typ, Name_uAssign);
                  F_Typ : Entity_Id := Etype (First_Formal (Op));

               begin
                  --  If the assignment is dispatching, make sure to use the
                  --  ??? where is rest of this comment ???

                  if Is_Class_Wide_Type (Typ) then
                     F_Typ := Class_Wide_Type (F_Typ);
                  end if;

                  L := New_List (
                    Make_Procedure_Call_Statement (Loc,
                      Name => New_Reference_To (Op, Loc),
                      Parameter_Associations => New_List (
                        Unchecked_Convert_To (F_Typ, Duplicate_Subexpr (Lhs)),
                        Unchecked_Convert_To (F_Typ,
                          Duplicate_Subexpr (Rhs)))));
               end;

            else
               L := Make_Tag_Ctrl_Assignment (N);

               --  We can't afford to have destructive Finalization Actions
               --  in the Self assignment case, so if the target and the
               --  source are not obviously different, code is generated to
               --  avoid the self assignment case
               --
               --    if lhs'address /= rhs'address then
               --       <code for controlled and/or tagged assignment>
               --    end if;

               if not Statically_Different (Lhs, Rhs)
                 and then Expand_Ctrl_Actions
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
               --  7.6.1 (18). The remaining adjustments are tackled by the
               --  implementation of adjust for record_controllers (see
               --  s-finimp.adb)

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
                            Make_Exception_Handler (Loc,
                              Exception_Choices =>
                                New_List (Make_Others_Choice (Loc)),
                              Statements        => New_List (
                                Make_Raise_Program_Error (Loc,
                                  Reason =>
                                    PE_Finalize_Raised_Exception)
                              ))))));
               end if;
            end if;

            Rewrite (N,
              Make_Block_Statement (Loc,
                Handled_Statement_Sequence =>
                  Make_Handled_Sequence_Of_Statements (Loc, Statements => L)));

            --  If no restrictions on aborts, protect the whole assignement
            --  for controlled objects as per 9.8(11)

            if Controlled_Type (Typ)
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

            Analyze (N);
            return;
         end Tagged_Case;

      --  Array types

      elsif Is_Array_Type (Typ) then
         declare
            Actual_Rhs : Node_Id := Rhs;

         begin
            while Nkind (Actual_Rhs) = N_Type_Conversion
              or else
                  Nkind (Actual_Rhs) = N_Qualified_Expression
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

      --  Scalar types. This is where we perform the processing related
      --  to the requirements of (RM 13.9.1(9-11)) concerning the handling
      --  of invalid scalar values.

      elsif Is_Scalar_Type (Typ) then

         --  Case where right side is known valid

         if Expr_Known_Valid (Rhs) then

            --  Here the right side is valid, so it is fine. The case to
            --  deal with is when the left side is a local variable reference
            --  whose value is not currently known to be valid. If this is
            --  the case, and the assignment appears in an unconditional
            --  context, then we can mark the left side as now being valid.

            if Is_Local_Variable_Reference (Lhs)
              and then not Is_Known_Valid (Entity (Lhs))
              and then In_Unconditional_Context (N)
            then
               Set_Is_Known_Valid (Entity (Lhs), True);
            end if;

         --  Case where right side may be invalid in the sense of the RM
         --  reference above. The RM does not require that we check for
         --  the validity on an assignment, but it does require that the
         --  assignment of an invalid value not cause erroneous behavior.

         --  The general approach in GNAT is to use the Is_Known_Valid flag
         --  to avoid the need for validity checking on assignments. However
         --  in some cases, we have to do validity checking in order to make
         --  sure that the setting of this flag is correct.

         else
            --  Validate right side if we are validating copies

            if Validity_Checks_On
               and then Validity_Check_Copies
            then
               Ensure_Valid (Rhs);

               --  We can propagate this to the left side where appropriate

               if Is_Local_Variable_Reference (Lhs)
                 and then not Is_Known_Valid (Entity (Lhs))
                 and then In_Unconditional_Context (N)
               then
                  Set_Is_Known_Valid (Entity (Lhs), True);
               end if;

            --  Otherwise check to see what should be done

            --  If left side is a local variable, then we just set its
            --  flag to indicate that its value may no longer be valid,
            --  since we are copying a potentially invalid value.

            elsif Is_Local_Variable_Reference (Lhs) then
               Set_Is_Known_Valid (Entity (Lhs), False);

            --  Check for case of a nonlocal variable on the left side
            --  which is currently known to be valid. In this case, we
            --  simply ensure that the right side is valid. We only play
            --  the game of copying validity status for local variables,
            --  since we are doing this statically, not by tracing the
            --  full flow graph.

            elsif Is_Entity_Name (Lhs)
              and then Is_Known_Valid (Entity (Lhs))
            then
               --  Note that the Ensure_Valid call is ignored if the
               --  Validity_Checking mode is set to none so we do not
               --  need to worry about that case here.

               Ensure_Valid (Rhs);

            --  In all other cases, we can safely copy an invalid value
            --  without worrying about the status of the left side. Since
            --  it is not a variable reference it will not be considered
            --  as being known to be valid in any case.

            else
               null;
            end if;
         end if;
      end if;

      --  Defend against invalid subscripts on left side if we are in
      --  standard validity checking mode. No need to do this if we
      --  are checking all subscripts.

      if Validity_Checks_On
        and then Validity_Check_Default
        and then not Validity_Check_Subscripts
      then
         Check_Valid_Lvalue_Subscripts (Lhs);
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
      --  Check for the situation where we know at compile time which
      --  branch will be taken

      if Compile_Time_Known_Value (Expr) then
         Alt := Find_Static_Alternative (N);

         --  Move the statements from this alternative after the case
         --  statement. They are already analyzed, so will be skipped
         --  by the analyzer.

         Insert_List_After (N, Statements (Alt));

         --  That leaves the case statement as a shell. The alternative
         --  that will be executed is reset to a null list. So now we can
         --  kill the entire case statement.

         Kill_Dead_Code (Expression (N));
         Kill_Dead_Code (Alternatives (N));
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

         --  If there is only a single alternative, just replace it with
         --  the sequence of statements since obviously that is what is
         --  going to be executed in all cases.

         Len := List_Length (Alternatives (N));

         if Len = 1 then
            --  We still need to evaluate the expression if it has any
            --  side effects.

            Remove_Side_Effects (Expression (N));

            Insert_List_After (N, Statements (First (Alternatives (N))));

            --  That leaves the case statement as a shell. The alternative
            --  that will be executed is reset to a null list. So now we can
            --  kill the entire case statement.

            Kill_Dead_Code (Expression (N));
            Rewrite (N, Make_Null_Statement (Loc));
            return;
         end if;

         --  An optimization. If there are only two alternatives, and only
         --  a single choice, then rewrite the whole case statement as an
         --  if statement, since this can result in susbequent optimizations.
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

         --  If the last alternative is not an Others choice, replace it
         --  with an N_Others_Choice. Note that we do not bother to call
         --  Analyze on the modified case statement, since it's only effect
         --  would be to compute the contents of the Others_Discrete_Choices
         --  which is not needed by the back end anyway.

         --  The reason we do this is that the back end always needs some
         --  default for a switch, so if we have not supplied one in the
         --  processing above for validity checking, then we need to
         --  supply one here.

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

   --  First we deal with the case of C and Fortran convention boolean
   --  values, with zero/non-zero semantics.

   --  Second, we deal with the obvious rewriting for the cases where the
   --  condition of the IF is known at compile time to be True or False.

   --  Third, we remove elsif parts which have non-empty Condition_Actions
   --  and rewrite as independent if statements. For example:

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
   --  Condition_Actions list. We also do the same processing if there is
   --  a constant condition in an elsif part (in conjunction with the first
   --  processing step mentioned above, for the recursive call made to deal
   --  with the created inner if, this deals with properly optimizing the
   --  cases of constant elsif conditions).

   procedure Expand_N_If_Statement (N : Node_Id) is
      Loc    : constant Source_Ptr := Sloc (N);
      Hed    : Node_Id;
      E      : Node_Id;
      New_If : Node_Id;

   begin
      Adjust_Condition (Condition (N));

      --  The following loop deals with constant conditions for the IF. We
      --  need a loop because as we eliminate False conditions, we grab the
      --  first elsif condition and use it as the primary condition.

      while Compile_Time_Known_Value (Condition (N)) loop

         --  If condition is True, we can simply rewrite the if statement
         --  now by replacing it by the series of then statements.

         if Is_True (Expr_Value (Condition (N))) then

            --  All the else parts can be killed

            Kill_Dead_Code (Elsif_Parts (N));
            Kill_Dead_Code (Else_Statements (N));

            Hed := Remove_Head (Then_Statements (N));
            Insert_List_After (N, Then_Statements (N));
            Rewrite (N, Hed);
            return;

         --  If condition is False, then we can delete the condition and
         --  the Then statements

         else
            --  We do not delete the condition if constant condition
            --  warnings are enabled, since otherwise we end up deleting
            --  the desired warning. Of course the backend will get rid
            --  of this True/False test anyway, so nothing is lost here.

            if not Constant_Condition_Warnings then
               Kill_Dead_Code (Condition (N));
            end if;

            Kill_Dead_Code (Then_Statements (N));

            --  If there are no elsif statements, then we simply replace
            --  the entire if statement by the sequence of else statements.

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

            --  If there are elsif statements, the first of them becomes
            --  the if/then section of the rebuilt if statement This is
            --  the case where we loop to reprocess this copied condition.

            else
               Hed := Remove_Head (Elsif_Parts (N));
               Insert_Actions      (N, Condition_Actions (Hed));
               Set_Condition       (N, Condition (Hed));
               Set_Then_Statements (N, Then_Statements (Hed));

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

            --  If there are condition actions, then we rewrite the if
            --  statement as indicated above. We also do the same rewrite
            --  if the condition is True or False. The further processing
            --  of this constant condition is then done by the recursive
            --  call to expand the newly created if statement

            if Present (Condition_Actions (E))
              or else Compile_Time_Known_Value (Condition (E))
            then
               --  Note this is not an implicit if statement, since it is
               --  part of an explicit if statement in the source (or of an
               --  implicit if statement that has already been tested).

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
            if Nkind (Then_Stm) = N_Return_Statement
                 and then
               Nkind (Else_Stm) = N_Return_Statement
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
                          Make_Return_Statement (Loc,
                            Expression => Relocate_Node (Condition (N))));
                        Analyze (N);
                        return;

                     elsif Entity (Then_Expr) = Standard_False
                       and then Entity (Else_Expr) = Standard_True
                     then
                        Rewrite (N,
                          Make_Return_Statement (Loc,
                            Expression =>
                              Make_Op_Not (Loc,
                                Right_Opnd => Relocate_Node (Condition (N)))));
                        Analyze (N);
                        return;
                     end if;
                  end if;
               end;
            end if;
         end;
      end if;
   end Expand_N_If_Statement;

   -----------------------------
   -- Expand_N_Loop_Statement --
   -----------------------------

   --  1. Deal with while condition for C/Fortran boolean
   --  2. Deal with loops with a non-standard enumeration type range
   --  3. Deal with while loops where Condition_Actions is set
   --  4. Insert polling call if required

   procedure Expand_N_Loop_Statement (N : Node_Id) is
      Loc  : constant Source_Ptr := Sloc (N);
      Isc  : constant Node_Id    := Iteration_Scheme (N);

   begin
      if Present (Isc) then
         Adjust_Condition (Condition (Isc));
      end if;

      if Is_Non_Empty_List (Statements (N)) then
         Generate_Poll_Call (First (Statements (N)));
      end if;

      if No (Isc) then
         return;
      end if;

      --  Handle the case where we have a for loop with the range type being
      --  an enumeration type with non-standard representation. In this case
      --  we expand:

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

      --  Second case, if we have a while loop with Condition_Actions set,
      --  then we change it into a plain loop:

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

            --  This is not an implicit loop, since it is generated in
            --  response to the loop statement being processed. If this
            --  is itself implicit, the restriction has already been
            --  checked. If not, it is an explicit loop.

            Rewrite (N,
              Make_Loop_Statement (Sloc (N),
                Identifier => Identifier (N),
                Statements => Statements (N),
                End_Label  => End_Label  (N)));

            Analyze (N);
         end;
      end if;
   end Expand_N_Loop_Statement;

   -------------------------------
   -- Expand_N_Return_Statement --
   -------------------------------

   procedure Expand_N_Return_Statement (N : Node_Id) is
      Loc         : constant Source_Ptr := Sloc (N);
      Exp         : constant Node_Id    := Expression (N);
      Exptyp      : Entity_Id;
      T           : Entity_Id;
      Utyp        : Entity_Id;
      Scope_Id    : Entity_Id;
      Kind        : Entity_Kind;
      Call        : Node_Id;
      Acc_Stat    : Node_Id;
      Goto_Stat   : Node_Id;
      Lab_Node    : Node_Id;
      Cur_Idx     : Nat;
      Return_Type : Entity_Id;
      Result_Exp  : Node_Id;
      Result_Id   : Entity_Id;
      Result_Obj  : Node_Id;

   begin
      --  Case where returned expression is present

      if Present (Exp) then

         --  Always normalize C/Fortran boolean result. This is not always
         --  necessary, but it seems a good idea to minimize the passing
         --  around of non-normalized values, and in any case this handles
         --  the processing of barrier functions for protected types, which
         --  turn the condition into a return statement.

         Exptyp := Etype (Exp);

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
      end if;

      --  Find relevant enclosing scope from which return is returning

      Cur_Idx := Scope_Stack.Last;
      loop
         Scope_Id := Scope_Stack.Table (Cur_Idx).Entity;

         if Ekind (Scope_Id) /= E_Block
           and then Ekind (Scope_Id) /= E_Loop
         then
            exit;

         else
            Cur_Idx := Cur_Idx - 1;
            pragma Assert (Cur_Idx >= 0);
         end if;
      end loop;

      if No (Exp) then
         Kind := Ekind (Scope_Id);

         --  If it is a return from procedures do no extra steps

         if Kind = E_Procedure or else Kind = E_Generic_Procedure then
            return;
         end if;

         pragma Assert (Is_Entry (Scope_Id));

         --  Look at the enclosing block to see whether the return is from
         --  an accept statement or an entry body.

         for J in reverse 0 .. Cur_Idx loop
            Scope_Id := Scope_Stack.Table (J).Entity;
            exit when Is_Concurrent_Type (Scope_Id);
         end loop;

         --  If it is a return from accept statement it should be expanded
         --  as a call to RTS Complete_Rendezvous and a goto to the end of
         --  the accept body.

         --  (cf : Expand_N_Accept_Statement, Expand_N_Selective_Accept,
         --   Expand_N_Accept_Alternative in exp_ch9.adb)

         if Is_Task_Type (Scope_Id) then

            Call := (Make_Procedure_Call_Statement (Loc,
                      Name => New_Reference_To
                        (RTE (RE_Complete_Rendezvous), Loc)));
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

         --  If it is a return from an entry body, put a Complete_Entry_Body
         --  call in front of the return.

         elsif Is_Protected_Type (Scope_Id) then

            Call :=
              Make_Procedure_Call_Statement (Loc,
                Name => New_Reference_To
                  (RTE (RE_Complete_Entry_Body), Loc),
                Parameter_Associations => New_List
                  (Make_Attribute_Reference (Loc,
                    Prefix =>
                      New_Reference_To
                        (Object_Ref
                           (Corresponding_Body (Parent (Scope_Id))),
                        Loc),
                    Attribute_Name => Name_Unchecked_Access)));

            Insert_Before (N, Call);
            Analyze (Call);

         end if;

         return;
      end if;

      T := Etype (Exp);
      Return_Type := Etype (Scope_Id);
      Utyp := Underlying_Type (Return_Type);

      --  Check the result expression of a scalar function against
      --  the subtype of the function by inserting a conversion.
      --  This conversion must eventually be performed for other
      --  classes of types, but for now it's only done for scalars.
      --  ???

      if Is_Scalar_Type (T) then
         Rewrite (Exp, Convert_To (Return_Type, Exp));
         Analyze (Exp);
      end if;

      --  Implement the rules of 6.5(8-10), which require a tag check in
      --  the case of a limited tagged return type, and tag reassignment
      --  for nonlimited tagged results. These actions are needed when
      --  the return type is a specific tagged type and the result
      --  expression is a conversion or a formal parameter, because in
      --  that case the tag of the expression might differ from the tag
      --  of the specific result type.

      if Is_Tagged_Type (Utyp)
        and then not Is_Class_Wide_Type (Utyp)
        and then (Nkind (Exp) = N_Type_Conversion
                    or else Nkind (Exp) = N_Unchecked_Type_Conversion
                    or else (Is_Entity_Name (Exp)
                               and then Ekind (Entity (Exp)) in Formal_Kind))
      then
         --  When the return type is limited, perform a check that the
         --  tag of the result is the same as the tag of the return type.

         if Is_Limited_Type (Return_Type) then
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

         --  If the result type is a specific nonlimited tagged type,
         --  then we have to ensure that the tag of the result is that
         --  of the result type. This is handled by making a copy of the
         --  expression in the case where it might have a different tag,
         --  namely when the expression is a conversion or a formal
         --  parameter. We create a new object of the result type and
         --  initialize it from the expression, which will implicitly
         --  force the tag to be set appropriately.

         else
            Result_Id :=
              Make_Defining_Identifier (Loc, New_Internal_Name ('R'));
            Result_Exp := New_Reference_To (Result_Id, Loc);

            Result_Obj :=
              Make_Object_Declaration (Loc,
                Defining_Identifier => Result_Id,
                Object_Definition   => New_Reference_To (Return_Type, Loc),
                Constant_Present    => True,
                Expression          => Relocate_Node (Exp));

            Set_Assignment_OK (Result_Obj);
            Insert_Action (Exp, Result_Obj);

            Rewrite (Exp, Result_Exp);
            Analyze_And_Resolve (Exp, Return_Type);
         end if;

      --  Ada 2005 (AI-344): If the result type is class-wide, then insert
      --  a check that the level of the return expression's underlying type
      --  is not deeper than the level of the master enclosing the function.

      elsif Ada_Version >= Ada_05
        and then Is_Class_Wide_Type (Return_Type)
      then
         Insert_Action (Exp,
           Make_Raise_Program_Error (Loc,
             Condition =>
               Make_Op_Gt (Loc,
                 Left_Opnd =>
                   Make_Function_Call (Loc,
                     Name =>
                       New_Reference_To
                         (RTE (RE_Get_Access_Level), Loc),
                     Parameter_Associations =>
                       New_List (Make_Attribute_Reference (Loc,
                                   Prefix         =>
                                      Duplicate_Subexpr (Exp),
                                   Attribute_Name =>
                                      Name_Tag))),
                 Right_Opnd =>
                   Make_Integer_Literal (Loc,
                     Scope_Depth (Enclosing_Dynamic_Scope (Scope_Id)))),
             Reason => PE_Accessibility_Check_Failed));
      end if;

      --  Deal with returning variable length objects and controlled types

      --  Nothing to do if we are returning by reference, or this is not
      --  a type that requires special processing (indicated by the fact
      --  that it requires a cleanup scope for the secondary stack case)

      if Is_Return_By_Reference_Type (T)
        or else not Requires_Transient_Scope (Return_Type)
      then
         null;

      --  Case of secondary stack not used

      elsif Function_Returns_With_DSP (Scope_Id) then

         --  Here what we need to do is to always return by reference, since
         --  we will return with the stack pointer depressed. We may need to
         --  do a copy to a local temporary before doing this return.

         No_Secondary_Stack_Case : declare
            Local_Copy_Required : Boolean := False;
            --  Set to True if a local copy is required

            Copy_Ent : Entity_Id;
            --  Used for the target entity if a copy is required

            Decl : Node_Id;
            --  Declaration used to create copy if needed

            procedure Test_Copy_Required (Expr : Node_Id);
            --  Determines if Expr represents a return value for which a
            --  copy is required. More specifically, a copy is not required
            --  if Expr represents an object or component of an object that
            --  is either in the local subprogram frame, or is constant.
            --  If a copy is required, then Local_Copy_Required is set True.

            ------------------------
            -- Test_Copy_Required --
            ------------------------

            procedure Test_Copy_Required (Expr : Node_Id) is
               Ent : Entity_Id;

            begin
               --  If component, test prefix (object containing component)

               if Nkind (Expr) = N_Indexed_Component
                    or else
                  Nkind (Expr) = N_Selected_Component
               then
                  Test_Copy_Required (Prefix (Expr));
                  return;

               --  See if we have an entity name

               elsif Is_Entity_Name (Expr) then
                  Ent := Entity (Expr);

                  --  Constant entity is always OK, no copy required

                  if Ekind (Ent) = E_Constant then
                     return;

                  --  No copy required for local variable

                  elsif Ekind (Ent) = E_Variable
                    and then Scope (Ent) = Current_Subprogram
                  then
                     return;
                  end if;
               end if;

               --  All other cases require a copy

               Local_Copy_Required := True;
            end Test_Copy_Required;

         --  Start of processing for No_Secondary_Stack_Case

         begin
            --  No copy needed if result is from a function call.
            --  In this case the result is already being returned by
            --  reference with the stack pointer depressed.

            --  To make up for a gcc 2.8.1 deficiency (???), we perform
            --  the copy for array types if the constrained status of the
            --  target type is different from that of the expression.

            if Requires_Transient_Scope (T)
              and then
                (not Is_Array_Type (T)
                   or else Is_Constrained (T) = Is_Constrained (Return_Type)
                   or else Controlled_Type (T))
              and then Nkind (Exp) = N_Function_Call
            then
               Set_By_Ref (N);

            --  We always need a local copy for a controlled type, since
            --  we are required to finalize the local value before return.
            --  The copy will automatically include the required finalize.
            --  Moreover, gigi cannot make this copy, since we need special
            --  processing to ensure proper behavior for finalization.

            --  Note: the reason we are returning with a depressed stack
            --  pointer in the controlled case (even if the type involved
            --  is constrained) is that we must make a local copy to deal
            --  properly with the requirement that the local result be
            --  finalized.

            elsif Controlled_Type (Utyp) then
               Copy_Ent :=
                 Make_Defining_Identifier (Loc,
                   Chars => New_Internal_Name ('R'));

               --  Build declaration to do the copy, and insert it, setting
               --  Assignment_OK, because we may be copying a limited type.
               --  In addition we set the special flag to inhibit finalize
               --  attachment if this is a controlled type (since this attach
               --  must be done by the caller, otherwise if we attach it here
               --  we will finalize the returned result prematurely).

               Decl :=
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Copy_Ent,
                   Object_Definition   => New_Occurrence_Of (Return_Type, Loc),
                   Expression          => Relocate_Node (Exp));

               Set_Assignment_OK (Decl);
               Set_Delay_Finalize_Attach (Decl);
               Insert_Action (N, Decl);

               --  Now the actual return uses the copied value

               Rewrite (Exp, New_Occurrence_Of (Copy_Ent, Loc));
               Analyze_And_Resolve (Exp, Return_Type);

               --  Since we have made the copy, gigi does not have to, so
               --  we set the By_Ref flag to prevent another copy being made.

               Set_By_Ref (N);

            --  Non-controlled cases

            else
               Test_Copy_Required (Exp);

               --  If a local copy is required, then gigi will make the
               --  copy, otherwise, we can return the result directly,
               --  so set By_Ref to suppress the gigi copy.

               if not Local_Copy_Required then
                  Set_By_Ref (N);
               end if;
            end if;
         end No_Secondary_Stack_Case;

      --  Here if secondary stack is used

      else
         --  Make sure that no surrounding block will reclaim the
         --  secondary-stack on which we are going to put the result.
         --  Not only may this introduce secondary stack leaks but worse,
         --  if the reclamation is done too early, then the result we are
         --  returning may get clobbered. See example in 7417-003.

         declare
            S : Entity_Id := Current_Scope;

         begin
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

         if Requires_Transient_Scope (T)
           and then
              (not Is_Array_Type (T)
                or else Is_Constrained (T) = Is_Constrained (Return_Type)
                or else Controlled_Type (T))
           and then Nkind (Exp) = N_Function_Call
         then
            Set_By_Ref (N);

         --  For controlled types, do the allocation on the sec-stack
         --  manually in order to call adjust at the right time
         --    type Anon1 is access Return_Type;
         --    for Anon1'Storage_pool use ss_pool;
         --    Anon2 : anon1 := new Return_Type'(expr);
         --    return Anon2.all;

         elsif Controlled_Type (Utyp) then
            declare
               Loc        : constant Source_Ptr := Sloc (N);
               Temp       : constant Entity_Id :=
                              Make_Defining_Identifier (Loc,
                                Chars => New_Internal_Name ('R'));
               Acc_Typ    : constant Entity_Id :=
                              Make_Defining_Identifier (Loc,
                                Chars => New_Internal_Name ('A'));
               Alloc_Node : Node_Id;

            begin
               Set_Ekind (Acc_Typ, E_Access_Type);

               Set_Associated_Storage_Pool (Acc_Typ, RTE (RE_SS_Pool));

               Alloc_Node :=
                 Make_Allocator (Loc,
                   Expression =>
                     Make_Qualified_Expression (Loc,
                       Subtype_Mark => New_Reference_To (Etype (Exp), Loc),
                       Expression => Relocate_Node (Exp)));

               Insert_List_Before_And_Analyze (N, New_List (
                 Make_Full_Type_Declaration (Loc,
                   Defining_Identifier => Acc_Typ,
                   Type_Definition     =>
                     Make_Access_To_Object_Definition (Loc,
                       Subtype_Indication =>
                          New_Reference_To (Return_Type, Loc))),

                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Temp,
                   Object_Definition   => New_Reference_To (Acc_Typ, Loc),
                   Expression          => Alloc_Node)));

               Rewrite (Exp,
                 Make_Explicit_Dereference (Loc,
                 Prefix => New_Reference_To (Temp, Loc)));

               Analyze_And_Resolve (Exp, Return_Type);
            end;

         --  Otherwise use the gigi mechanism to allocate result on the
         --  secondary stack.

         else
            Set_Storage_Pool      (N, RTE (RE_SS_Pool));

            --  If we are generating code for the Java VM do not use
            --  SS_Allocate since everything is heap-allocated anyway.

            if not Java_VM then
               Set_Procedure_To_Call (N, RTE (RE_SS_Allocate));
            end if;
         end if;
      end if;

   exception
      when RE_Not_Available =>
         return;
   end Expand_N_Return_Statement;

   ------------------------------
   -- Make_Tag_Ctrl_Assignment --
   ------------------------------

   function Make_Tag_Ctrl_Assignment (N : Node_Id) return List_Id is
      Loc : constant Source_Ptr := Sloc (N);
      L   : constant Node_Id    := Name (N);
      T   : constant Entity_Id  := Underlying_Type (Etype (L));

      Ctrl_Act : constant Boolean := Controlled_Type (T)
                                       and then not No_Ctrl_Actions (N);

      Save_Tag : constant Boolean := Is_Tagged_Type (T)
                                       and then not No_Ctrl_Actions (N)
                                       and then not Java_VM;
      --  Tags are not saved and restored when Java_VM because JVM tags
      --  are represented implicitly in objects.

      Res       : List_Id;
      Tag_Tmp   : Entity_Id;

   begin
      Res := New_List;

      --  Finalize the target of the assignment when controlled.
      --  We have two exceptions here:

      --   1. If we are in an init proc since it is an initialization
      --      more than an assignment

      --   2. If the left-hand side is a temporary that was not initialized
      --      (or the parent part of a temporary since it is the case in
      --      extension aggregates). Such a temporary does not come from
      --      source. We must examine the original node for the prefix, because
      --      it may be a component of an entry formal, in which case it has
      --      been rewritten and does not appear to come from source either.

      --  Case of init proc

      if not Ctrl_Act then
         null;

      --  The left hand side is an uninitialized  temporary

      elsif Nkind (L) = N_Type_Conversion
        and then Is_Entity_Name (Expression (L))
        and then No_Initialization (Parent (Entity (Expression (L))))
      then
         null;
      else
         Append_List_To (Res,
           Make_Final_Call (
             Ref         => Duplicate_Subexpr_No_Checks (L),
             Typ         => Etype (L),
             With_Detach => New_Reference_To (Standard_False, Loc)));
      end if;

      --  Save the Tag in a local variable Tag_Tmp

      if Save_Tag then
         Tag_Tmp :=
           Make_Defining_Identifier (Loc, New_Internal_Name ('A'));

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

      --  Processing for controlled types and types with controlled components

      --  Variables of such types contain pointers used to chain them in
      --  finalization lists, in addition to user data. These pointers are
      --  specific to each object of the type, not to the value being assigned.
      --  Thus they need to be left intact during the assignment. We achieve
      --  this by constructing a Storage_Array subtype, and by overlaying
      --  objects of this type on the source and target of the assignment.
      --  The assignment is then rewritten to assignments of slices of these
      --  arrays, copying the user data, and leaving the pointers untouched.

      if Ctrl_Act then
         Controlled_Actions : declare
            Prev_Ref : Node_Id;
            --  A reference to the Prev component of the record controller

            First_After_Root : Node_Id := Empty;
            --  Index of first byte to be copied (used to skip
            --  Root_Controlled in controlled objects).

            Last_Before_Hole : Node_Id := Empty;
            --  Index of last byte to be copied before outermost record
            --  controller data.

            Hole_Length      : Node_Id := Empty;
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
            --  Build and return a slice of an array of type S overlaid
            --  on object Rec, with bounds specified by Lo and Hi. If either
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
               --  Access value designating an opaque storage array of
               --  type S overlaid on record Rec.

            begin
               --  Compute slice bounds using S'First (1) and S'Last
               --  as default values when not specified by the caller.

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
                    Attribute_Name =>
                      Name_Size),
                Right_Opnd =>
                  Make_Integer_Literal (Loc,
                  System_Storage_Unit - 1));
            Source_Size :=
              Make_Op_Divide (Loc,
                Left_Opnd => Source_Size,
                Right_Opnd =>
                  Make_Integer_Literal (Loc,
                    Intval => System_Storage_Unit));

            Range_Type :=
              Make_Defining_Identifier (Loc,
                New_Internal_Name ('G'));

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
                Defining_Identifier =>
                  Make_Defining_Identifier (Loc,
                    New_Internal_Name ('S')),
                Subtype_Indication  =>
                  Make_Subtype_Indication (Loc,
                    Subtype_Mark =>
                      New_Reference_To (RTE (RE_Storage_Array), Loc),
                    Constraint =>
                      Make_Index_Or_Discriminant_Constraint (Loc,
                        Constraints =>
                          New_List (New_Reference_To (Range_Type, Loc))))));

            --  type A is access S

            Opaque_Type :=
              Make_Defining_Identifier (Loc,
                Chars => New_Internal_Name ('A'));

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

            --  For the case of a controlled object, skip the
            --  Root_Controlled part.

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
            --  the Prev and Next components of the record controller.
            --  These components constitute a 'hole' in the middle of the
            --  data to be copied.

            if Has_Controlled_Component (T) then
               Prev_Ref :=
                 Make_Selected_Component (Loc,
                   Prefix =>
                     Make_Selected_Component (Loc,
                       Prefix => Duplicate_Subexpr_No_Checks (L),
                       Selector_Name =>
                         New_Reference_To (Controller_Component (T), Loc)),
                   Selector_Name =>  Make_Identifier (Loc, Name_Prev));

               --  Last index before hole: determined by position of
               --  the _Controller.Prev component.

               Last_Before_Hole :=
                 Make_Defining_Identifier (Loc,
                   New_Internal_Name ('L'));

               Append_To (Res,
                 Make_Object_Declaration (Loc,
                   Defining_Identifier => Last_Before_Hole,
                   Object_Definition   => New_Occurrence_Of (
                     RTE (RE_Storage_Offset), Loc),
                   Constant_Present    => True,
                   Expression          => Make_Op_Add (Loc,
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

               First_After_Hole :=
                 Make_Defining_Identifier (Loc,
                   New_Internal_Name ('F'));

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

               Last_Before_Hole := New_Occurrence_Of (Last_Before_Hole, Loc);
               First_After_Hole := New_Occurrence_Of (First_After_Hole, Loc);
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

      else
         Append_To (Res, Relocate_Node (N));
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

      --  Adjust the target after the assignment when controlled (not in the
      --  init proc since it is an initialization more than an assignment).

      if Ctrl_Act then
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

   ------------------------------------
   -- Possible_Bit_Aligned_Component --
   ------------------------------------

   function Possible_Bit_Aligned_Component (N : Node_Id) return Boolean is
   begin
      case Nkind (N) is

         --  Case of indexed component

         when N_Indexed_Component =>
            declare
               P    : constant Node_Id   := Prefix (N);
               Ptyp : constant Entity_Id := Etype (P);

            begin
               --  If we know the component size and it is less than 64, then
               --  we are definitely OK. The back end always does assignment
               --  of misaligned small objects correctly.

               if Known_Static_Component_Size (Ptyp)
                 and then Component_Size (Ptyp) <= 64
               then
                  return False;

               --  Otherwise, we need to test the prefix, to see if we are
               --  indexing from a possibly unaligned component.

               else
                  return Possible_Bit_Aligned_Component (P);
               end if;
            end;

         --  Case of selected component

         when N_Selected_Component =>
            declare
               P    : constant Node_Id   := Prefix (N);
               Comp : constant Entity_Id := Entity (Selector_Name (N));

            begin
               --  If there is no component clause, then we are in the clear
               --  since the back end will never misalign a large component
               --  unless it is forced to do so. In the clear means we need
               --  only the recursive test on the prefix.

               if Component_May_Be_Bit_Aligned (Comp) then
                  return True;
               else
                  return Possible_Bit_Aligned_Component (P);
               end if;
            end;

         --  If we have neither a record nor array component, it means that
         --  we have fallen off the top testing prefixes recursively, and
         --  we now have a stand alone object, where we don't have a problem

         when others =>
            return False;

      end case;
   end Possible_Bit_Aligned_Component;

end Exp_Ch5;
