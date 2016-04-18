------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               L A Y O U T                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2001-2015, Free Software Foundation, Inc.         --
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
with Errout;   use Errout;
with Exp_Ch3;  use Exp_Ch3;
with Exp_Util; use Exp_Util;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Repinfo;  use Repinfo;
with Sem;      use Sem;
with Sem_Aux;  use Sem_Aux;
with Sem_Case; use Sem_Case;
with Sem_Ch13; use Sem_Ch13;
with Sem_Eval; use Sem_Eval;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Stand;    use Stand;
with Targparm; use Targparm;
with Tbuild;   use Tbuild;
with Ttypes;   use Ttypes;
with Uintp;    use Uintp;

package body Layout is

   ------------------------
   -- Local Declarations --
   ------------------------

   SSU : constant Int := Ttypes.System_Storage_Unit;
   --  Short hand for System_Storage_Unit

   Vname : constant Name_Id := Name_uV;
   --  Formal parameter name used for functions generated for size offset
   --  values that depend on the discriminant. All such functions have the
   --  following form:
   --
   --     function xxx (V : vtyp) return Unsigned is
   --     begin
   --        return ... expression involving V.discrim
   --     end xxx;

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Assoc_Add
     (Loc        : Source_Ptr;
      Left_Opnd  : Node_Id;
      Right_Opnd : Node_Id) return Node_Id;
   --  This is like Make_Op_Add except that it optimizes some cases knowing
   --  that associative rearrangement is allowed for constant folding if one
   --  of the operands is a compile time known value.

   function Assoc_Multiply
     (Loc        : Source_Ptr;
      Left_Opnd  : Node_Id;
      Right_Opnd : Node_Id) return Node_Id;
   --  This is like Make_Op_Multiply except that it optimizes some cases
   --  knowing that associative rearrangement is allowed for constant folding
   --  if one of the operands is a compile time known value

   function Assoc_Subtract
     (Loc        : Source_Ptr;
      Left_Opnd  : Node_Id;
      Right_Opnd : Node_Id) return Node_Id;
   --  This is like Make_Op_Subtract except that it optimizes some cases
   --  knowing that associative rearrangement is allowed for constant folding
   --  if one of the operands is a compile time known value

   function Bits_To_SU (N : Node_Id) return Node_Id;
   --  This is used when we cross the boundary from static sizes in bits to
   --  dynamic sizes in storage units. If the argument N is anything other
   --  than an integer literal, it is returned unchanged, but if it is an
   --  integer literal, then it is taken as a size in bits, and is replaced
   --  by the corresponding size in storage units.

   function Compute_Length (Lo : Node_Id; Hi : Node_Id) return Node_Id;
   --  Given expressions for the low bound (Lo) and the high bound (Hi),
   --  Build an expression for the value hi-lo+1, converted to type
   --  Standard.Unsigned. Takes care of the case where the operands
   --  are of an enumeration type (so that the subtraction cannot be
   --  done directly) by applying the Pos operator to Hi/Lo first.

   procedure Compute_Size_Depends_On_Discriminant (E : Entity_Id);
   --  Given an array type or an array subtype E, compute whether its size
   --  depends on the value of one or more discriminants and set the flag
   --  Size_Depends_On_Discriminant accordingly. This need not be called
   --  in front end layout mode since it does the computation on its own.

   function Expr_From_SO_Ref
     (Loc  : Source_Ptr;
      D    : SO_Ref;
      Comp : Entity_Id := Empty) return Node_Id;
   --  Given a value D from a size or offset field, return an expression
   --  representing the value stored. If the value is known at compile time,
   --  then an N_Integer_Literal is returned with the appropriate value. If
   --  the value references a constant entity, then an N_Identifier node
   --  referencing this entity is returned. If the value denotes a size
   --  function, then returns a call node denoting the given function, with
   --  a single actual parameter that either refers to the parameter V of
   --  an enclosing size function (if Comp is Empty or its type doesn't match
   --  the function's formal), or else is a selected component V.c when Comp
   --  denotes a component c whose type matches that of the function formal.
   --  The Loc value is used for the Sloc value of constructed notes.

   function SO_Ref_From_Expr
     (Expr      : Node_Id;
      Ins_Type  : Entity_Id;
      Vtype     : Entity_Id := Empty;
      Make_Func : Boolean   := False) return Dynamic_SO_Ref;
   --  This routine is used in the case where a size/offset value is dynamic
   --  and is represented by the expression Expr. SO_Ref_From_Expr checks if
   --  the Expr contains a reference to the identifier V, and if so builds
   --  a function depending on discriminants of the formal parameter V which
   --  is of type Vtype. Otherwise, if the parameter Make_Func is True, then
   --  Expr will be encapsulated in a parameterless function; if Make_Func is
   --  False, then a constant entity with the value Expr is built. The result
   --  is a Dynamic_SO_Ref to the created entity. Note that Vtype can be
   --  omitted if Expr does not contain any reference to V, the created entity.
   --  The declaration created is inserted in the freeze actions of Ins_Type,
   --  which also supplies the Sloc for created nodes. This function also takes
   --  care of making sure that the expression is properly analyzed and
   --  resolved (which may not be the case yet if we build the expression
   --  in this unit).

   function Get_Max_SU_Size (E : Entity_Id) return Node_Id;
   --  E is an array type or subtype that has at least one index bound that
   --  is the value of a record discriminant. For such an array, the function
   --  computes an expression that yields the maximum possible size of the
   --  array in storage units. The result is not defined for any other type,
   --  or for arrays that do not depend on discriminants, and it is a fatal
   --  error to call this unless Size_Depends_On_Discriminant (E) is True.

   procedure Layout_Array_Type (E : Entity_Id);
   --  Front-end layout of non-bit-packed array type or subtype

   procedure Layout_Record_Type (E : Entity_Id);
   --  Front-end layout of record type

   procedure Rewrite_Integer (N : Node_Id; V : Uint);
   --  Rewrite node N with an integer literal whose value is V. The Sloc for
   --  the new node is taken from N, and the type of the literal is set to a
   --  copy of the type of N on entry.

   procedure Set_And_Check_Static_Size
     (E      : Entity_Id;
      Esiz   : SO_Ref;
      RM_Siz : SO_Ref);
   --  This procedure is called to check explicit given sizes (possibly stored
   --  in the Esize and RM_Size fields of E) against computed Object_Size
   --  (Esiz) and Value_Size (RM_Siz) values. Appropriate errors and warnings
   --  are posted if specified sizes are inconsistent with specified sizes. On
   --  return, Esize and RM_Size fields of E are set (either from previously
   --  given values, or from the newly computed values, as appropriate).

   procedure Set_Composite_Alignment (E : Entity_Id);
   --  This procedure is called for record types and subtypes, and also for
   --  atomic array types and subtypes. If no alignment is set, and the size
   --  is 2 or 4 (or 8 if the word size is 8), then the alignment is set to
   --  match the size.

   ----------------------------
   -- Adjust_Esize_Alignment --
   ----------------------------

   procedure Adjust_Esize_Alignment (E : Entity_Id) is
      Abits     : Int;
      Esize_Set : Boolean;

   begin
      --  Nothing to do if size unknown

      if Unknown_Esize (E) then
         return;
      end if;

      --  Determine if size is constrained by an attribute definition clause
      --  which must be obeyed. If so, we cannot increase the size in this
      --  routine.

      --  For a type, the issue is whether an object size clause has been set.
      --  A normal size clause constrains only the value size (RM_Size)

      if Is_Type (E) then
         Esize_Set := Has_Object_Size_Clause (E);

      --  For an object, the issue is whether a size clause is present

      else
         Esize_Set := Has_Size_Clause (E);
      end if;

      --  If size is known it must be a multiple of the storage unit size

      if Esize (E) mod SSU /= 0 then

         --  If not, and size specified, then give error

         if Esize_Set then
            Error_Msg_NE
              ("size for& not a multiple of storage unit size",
               Size_Clause (E), E);
            return;

         --  Otherwise bump up size to a storage unit boundary

         else
            Set_Esize (E, (Esize (E) + SSU - 1) / SSU * SSU);
         end if;
      end if;

      --  Now we have the size set, it must be a multiple of the alignment
      --  nothing more we can do here if the alignment is unknown here.

      if Unknown_Alignment (E) then
         return;
      end if;

      --  At this point both the Esize and Alignment are known, so we need
      --  to make sure they are consistent.

      Abits := UI_To_Int (Alignment (E)) * SSU;

      if Esize (E) mod Abits = 0 then
         return;
      end if;

      --  Here we have a situation where the Esize is not a multiple of the
      --  alignment. We must either increase Esize or reduce the alignment to
      --  correct this situation.

      --  The case in which we can decrease the alignment is where the
      --  alignment was not set by an alignment clause, and the type in
      --  question is a discrete type, where it is definitely safe to reduce
      --  the alignment. For example:

      --    t : integer range 1 .. 2;
      --    for t'size use 8;

      --  In this situation, the initial alignment of t is 4, copied from
      --  the Integer base type, but it is safe to reduce it to 1 at this
      --  stage, since we will only be loading a single storage unit.

      if Is_Discrete_Type (Etype (E)) and then not Has_Alignment_Clause (E)
      then
         loop
            Abits := Abits / 2;
            exit when Esize (E) mod Abits = 0;
         end loop;

         Init_Alignment (E, Abits / SSU);
         return;
      end if;

      --  Now the only possible approach left is to increase the Esize but we
      --  can't do that if the size was set by a specific clause.

      if Esize_Set then
         Error_Msg_NE
           ("size for& is not a multiple of alignment",
            Size_Clause (E), E);

      --  Otherwise we can indeed increase the size to a multiple of alignment

      else
         Set_Esize (E, ((Esize (E) + (Abits - 1)) / Abits) * Abits);
      end if;
   end Adjust_Esize_Alignment;

   ---------------
   -- Assoc_Add --
   ---------------

   function Assoc_Add
     (Loc        : Source_Ptr;
      Left_Opnd  : Node_Id;
      Right_Opnd : Node_Id) return Node_Id
   is
      L : Node_Id;
      R : Uint;

   begin
      --  Case of right operand is a constant

      if Compile_Time_Known_Value (Right_Opnd) then
         L := Left_Opnd;
         R := Expr_Value (Right_Opnd);

      --  Case of left operand is a constant

      elsif Compile_Time_Known_Value (Left_Opnd) then
         L := Right_Opnd;
         R := Expr_Value (Left_Opnd);

      --  Neither operand is a constant, do the addition with no optimization

      else
         return Make_Op_Add (Loc, Left_Opnd, Right_Opnd);
      end if;

      --  Case of left operand is an addition

      if Nkind (L) = N_Op_Add then

         --  (C1 + E) + C2 = (C1 + C2) + E

         if Compile_Time_Known_Value (Sinfo.Left_Opnd (L)) then
            Rewrite_Integer
              (Sinfo.Left_Opnd (L),
               Expr_Value (Sinfo.Left_Opnd (L)) + R);
            return L;

         --  (E + C1) + C2 = E + (C1 + C2)

         elsif Compile_Time_Known_Value (Sinfo.Right_Opnd (L)) then
            Rewrite_Integer
              (Sinfo.Right_Opnd (L),
               Expr_Value (Sinfo.Right_Opnd (L)) + R);
            return L;
         end if;

      --  Case of left operand is a subtraction

      elsif Nkind (L) = N_Op_Subtract then

         --  (C1 - E) + C2 = (C1 + C2) - E

         if Compile_Time_Known_Value (Sinfo.Left_Opnd (L)) then
            Rewrite_Integer
              (Sinfo.Left_Opnd (L),
               Expr_Value (Sinfo.Left_Opnd (L)) + R);
            return L;

         --  (E - C1) + C2 = E - (C1 - C2)

         --  If the type is unsigned then only do the optimization if C1 >= C2,
         --  to avoid creating a negative literal that can't be used with the
         --  unsigned type.

         elsif Compile_Time_Known_Value (Sinfo.Right_Opnd (L))
           and then (not Is_Unsigned_Type (Etype (Sinfo.Right_Opnd (L)))
                      or else Expr_Value (Sinfo.Right_Opnd (L)) >= R)
         then
            Rewrite_Integer
              (Sinfo.Right_Opnd (L),
               Expr_Value (Sinfo.Right_Opnd (L)) - R);
            return L;
         end if;
      end if;

      --  Not optimizable, do the addition

      return Make_Op_Add (Loc, Left_Opnd, Right_Opnd);
   end Assoc_Add;

   --------------------
   -- Assoc_Multiply --
   --------------------

   function Assoc_Multiply
     (Loc        : Source_Ptr;
      Left_Opnd  : Node_Id;
      Right_Opnd : Node_Id) return Node_Id
   is
      L : Node_Id;
      R : Uint;

   begin
      --  Case of right operand is a constant

      if Compile_Time_Known_Value (Right_Opnd) then
         L := Left_Opnd;
         R := Expr_Value (Right_Opnd);

      --  Case of left operand is a constant

      elsif Compile_Time_Known_Value (Left_Opnd) then
         L := Right_Opnd;
         R := Expr_Value (Left_Opnd);

      --  Neither operand is a constant, do the multiply with no optimization

      else
         return Make_Op_Multiply (Loc, Left_Opnd, Right_Opnd);
      end if;

      --  Case of left operand is an multiplication

      if Nkind (L) = N_Op_Multiply then

         --  (C1 * E) * C2 = (C1 * C2) + E

         if Compile_Time_Known_Value (Sinfo.Left_Opnd (L)) then
            Rewrite_Integer
              (Sinfo.Left_Opnd (L),
               Expr_Value (Sinfo.Left_Opnd (L)) * R);
            return L;

         --  (E * C1) * C2 = E * (C1 * C2)

         elsif Compile_Time_Known_Value (Sinfo.Right_Opnd (L)) then
            Rewrite_Integer
              (Sinfo.Right_Opnd (L),
               Expr_Value (Sinfo.Right_Opnd (L)) * R);
            return L;
         end if;
      end if;

      --  Not optimizable, do the multiplication

      return Make_Op_Multiply (Loc, Left_Opnd, Right_Opnd);
   end Assoc_Multiply;

   --------------------
   -- Assoc_Subtract --
   --------------------

   function Assoc_Subtract
     (Loc        : Source_Ptr;
      Left_Opnd  : Node_Id;
      Right_Opnd : Node_Id) return Node_Id
   is
      L : Node_Id;
      R : Uint;

   begin
      --  Case of right operand is a constant

      if Compile_Time_Known_Value (Right_Opnd) then
         L := Left_Opnd;
         R := Expr_Value (Right_Opnd);

      --  Right operand is a constant, do the subtract with no optimization

      else
         return Make_Op_Subtract (Loc, Left_Opnd, Right_Opnd);
      end if;

      --  Case of left operand is an addition

      if Nkind (L) = N_Op_Add then

         --  (C1 + E) - C2 = (C1 - C2) + E

         if Compile_Time_Known_Value (Sinfo.Left_Opnd (L)) then
            Rewrite_Integer
              (Sinfo.Left_Opnd (L),
               Expr_Value (Sinfo.Left_Opnd (L)) - R);
            return L;

         --  (E + C1) - C2 = E + (C1 - C2)

         elsif Compile_Time_Known_Value (Sinfo.Right_Opnd (L)) then
            Rewrite_Integer
              (Sinfo.Right_Opnd (L),
               Expr_Value (Sinfo.Right_Opnd (L)) - R);
            return L;
         end if;

      --  Case of left operand is a subtraction

      elsif Nkind (L) = N_Op_Subtract then

         --  (C1 - E) - C2 = (C1 - C2) + E

         if Compile_Time_Known_Value (Sinfo.Left_Opnd (L)) then
            Rewrite_Integer
              (Sinfo.Left_Opnd (L),
               Expr_Value (Sinfo.Left_Opnd (L)) + R);
            return L;

         --  (E - C1) - C2 = E - (C1 + C2)

         elsif Compile_Time_Known_Value (Sinfo.Right_Opnd (L)) then
            Rewrite_Integer
              (Sinfo.Right_Opnd (L),
               Expr_Value (Sinfo.Right_Opnd (L)) + R);
            return L;
         end if;
      end if;

      --  Not optimizable, do the subtraction

      return Make_Op_Subtract (Loc, Left_Opnd, Right_Opnd);
   end Assoc_Subtract;

   ----------------
   -- Bits_To_SU --
   ----------------

   function Bits_To_SU (N : Node_Id) return Node_Id is
   begin
      if Nkind (N) = N_Integer_Literal then
         Set_Intval (N, (Intval (N) + (SSU - 1)) / SSU);
      end if;

      return N;
   end Bits_To_SU;

   --------------------
   -- Compute_Length --
   --------------------

   function Compute_Length (Lo : Node_Id; Hi : Node_Id) return Node_Id is
      Loc    : constant Source_Ptr := Sloc (Lo);
      Typ    : constant Entity_Id  := Etype (Lo);
      Lo_Op  : Node_Id;
      Hi_Op  : Node_Id;
      Lo_Dim : Uint;
      Hi_Dim : Uint;

   begin
      --  If the bounds are First and Last attributes for the same dimension
      --  and both have prefixes that denotes the same entity, then we create
      --  and return a Length attribute. This may allow the back end to
      --  generate better code in cases where it already has the length.

      if Nkind (Lo) = N_Attribute_Reference
        and then Attribute_Name (Lo) = Name_First
        and then Nkind (Hi) = N_Attribute_Reference
        and then Attribute_Name (Hi) = Name_Last
        and then Is_Entity_Name (Prefix (Lo))
        and then Is_Entity_Name (Prefix (Hi))
        and then Entity (Prefix (Lo)) = Entity (Prefix (Hi))
      then
         Lo_Dim := Uint_1;
         Hi_Dim := Uint_1;

         if Present (First (Expressions (Lo))) then
            Lo_Dim := Expr_Value (First (Expressions (Lo)));
         end if;

         if Present (First (Expressions (Hi))) then
            Hi_Dim := Expr_Value (First (Expressions (Hi)));
         end if;

         if Lo_Dim = Hi_Dim then
            return
              Make_Attribute_Reference (Loc,
                Prefix         => New_Occurrence_Of
                                    (Entity (Prefix (Lo)), Loc),
                Attribute_Name => Name_Length,
                Expressions    => New_List
                                    (Make_Integer_Literal (Loc, Lo_Dim)));
         end if;
      end if;

      Lo_Op := New_Copy_Tree (Lo);
      Hi_Op := New_Copy_Tree (Hi);

      --  If type is enumeration type, then use Pos attribute to convert
      --  to integer type for which subtraction is a permitted operation.

      if Is_Enumeration_Type (Typ) then
         Lo_Op :=
           Make_Attribute_Reference (Loc,
             Prefix         => New_Occurrence_Of (Typ, Loc),
             Attribute_Name => Name_Pos,
             Expressions    => New_List (Lo_Op));

         Hi_Op :=
           Make_Attribute_Reference (Loc,
             Prefix         => New_Occurrence_Of (Typ, Loc),
             Attribute_Name => Name_Pos,
             Expressions    => New_List (Hi_Op));
      end if;

      return
        Assoc_Add (Loc,
          Left_Opnd =>
            Assoc_Subtract (Loc,
              Left_Opnd  => Hi_Op,
              Right_Opnd => Lo_Op),
          Right_Opnd => Make_Integer_Literal (Loc, 1));
   end Compute_Length;

   ----------------------
   -- Expr_From_SO_Ref --
   ----------------------

   function Expr_From_SO_Ref
     (Loc  : Source_Ptr;
      D    : SO_Ref;
      Comp : Entity_Id := Empty) return Node_Id
   is
      Ent : Entity_Id;

   begin
      if Is_Dynamic_SO_Ref (D) then
         Ent := Get_Dynamic_SO_Entity (D);

         if Is_Discrim_SO_Function (Ent) then

            --  If a component is passed in whose type matches the type of
            --  the function formal, then select that component from the "V"
            --  parameter rather than passing "V" directly.

            if Present (Comp)
               and then Base_Type (Etype (Comp)) =
                        Base_Type (Etype (First_Formal (Ent)))
            then
               return
                 Make_Function_Call (Loc,
                   Name                   => New_Occurrence_Of (Ent, Loc),
                   Parameter_Associations => New_List (
                     Make_Selected_Component (Loc,
                       Prefix        => Make_Identifier (Loc, Vname),
                       Selector_Name => New_Occurrence_Of (Comp, Loc))));

            else
               return
                 Make_Function_Call (Loc,
                   Name                   => New_Occurrence_Of (Ent, Loc),
                   Parameter_Associations => New_List (
                     Make_Identifier (Loc, Vname)));
            end if;

         else
            return New_Occurrence_Of (Ent, Loc);
         end if;

      else
         return Make_Integer_Literal (Loc, D);
      end if;
   end Expr_From_SO_Ref;

   ---------------------
   -- Get_Max_SU_Size --
   ---------------------

   function Get_Max_SU_Size (E : Entity_Id) return Node_Id is
      Loc  : constant Source_Ptr := Sloc (E);
      Indx : Node_Id;
      Ityp : Entity_Id;
      Lo   : Node_Id;
      Hi   : Node_Id;
      S    : Uint;
      Len  : Node_Id;

      type Val_Status_Type is (Const, Dynamic);

      type Val_Type (Status : Val_Status_Type := Const) is
         record
            case Status is
               when Const   => Val : Uint;
               when Dynamic => Nod : Node_Id;
            end case;
         end record;
      --  Shows the status of the value so far. Const means that the value is
      --  constant, and Val is the current constant value. Dynamic means that
      --  the value is dynamic, and in this case Nod is the Node_Id of the
      --  expression to compute the value.

      Size : Val_Type;
      --  Calculated value so far if Size.Status = Const,
      --  or expression value so far if Size.Status = Dynamic.

      SU_Convert_Required : Boolean := False;
      --  This is set to True if the final result must be converted from bits
      --  to storage units (rounding up to a storage unit boundary).

      -----------------------
      -- Local Subprograms --
      -----------------------

      procedure Max_Discrim (N : in out Node_Id);
      --  If the node N represents a discriminant, replace it by the maximum
      --  value of the discriminant.

      procedure Min_Discrim (N : in out Node_Id);
      --  If the node N represents a discriminant, replace it by the minimum
      --  value of the discriminant.

      -----------------
      -- Max_Discrim --
      -----------------

      procedure Max_Discrim (N : in out Node_Id) is
      begin
         if Nkind (N) = N_Identifier
           and then Ekind (Entity (N)) = E_Discriminant
         then
            N := Type_High_Bound (Etype (N));
         end if;
      end Max_Discrim;

      -----------------
      -- Min_Discrim --
      -----------------

      procedure Min_Discrim (N : in out Node_Id) is
      begin
         if Nkind (N) = N_Identifier
           and then Ekind (Entity (N)) = E_Discriminant
         then
            N := Type_Low_Bound (Etype (N));
         end if;
      end Min_Discrim;

   --  Start of processing for Get_Max_SU_Size

   begin
      pragma Assert (Size_Depends_On_Discriminant (E));

      --  Initialize status from component size

      if Known_Static_Component_Size (E) then
         Size := (Const, Component_Size (E));

      else
         Size := (Dynamic, Expr_From_SO_Ref (Loc, Component_Size (E)));
      end if;

      --  Loop through indexes

      Indx := First_Index (E);
      while Present (Indx) loop
         Ityp := Etype (Indx);
         Lo := Type_Low_Bound (Ityp);
         Hi := Type_High_Bound (Ityp);

         Min_Discrim (Lo);
         Max_Discrim (Hi);

         --  Value of the current subscript range is statically known

         if Compile_Time_Known_Value (Lo)
              and then
            Compile_Time_Known_Value (Hi)
         then
            S := Expr_Value (Hi) - Expr_Value (Lo) + 1;

            --  If known flat bound, entire size of array is zero

            if S <= 0 then
               return Make_Integer_Literal (Loc, 0);
            end if;

            --  Current value is constant, evolve value

            if Size.Status = Const then
               Size.Val := Size.Val * S;

            --  Current value is dynamic

            else
               --  An interesting little optimization, if we have a pending
               --  conversion from bits to storage units, and the current
               --  length is a multiple of the storage unit size, then we
               --  can take the factor out here statically, avoiding some
               --  extra dynamic computations at the end.

               if SU_Convert_Required and then S mod SSU = 0 then
                  S := S / SSU;
                  SU_Convert_Required := False;
               end if;

               Size.Nod :=
                 Assoc_Multiply (Loc,
                   Left_Opnd  => Size.Nod,
                   Right_Opnd =>
                     Make_Integer_Literal (Loc, Intval => S));
            end if;

         --  Value of the current subscript range is dynamic

         else
            --  If the current size value is constant, then here is where we
            --  make a transition to dynamic values, which are always stored
            --  in storage units, However, we do not want to convert to SU's
            --  too soon, consider the case of a packed array of single bits,
            --  we want to do the SU conversion after computing the size in
            --  this case.

            if Size.Status = Const then

               --  If the current value is a multiple of the storage unit,
               --  then most certainly we can do the conversion now, simply
               --  by dividing the current value by the storage unit value.
               --  If this works, we set SU_Convert_Required to False.

               if Size.Val mod SSU = 0 then

                  Size :=
                    (Dynamic, Make_Integer_Literal (Loc, Size.Val / SSU));
                  SU_Convert_Required := False;

               --  Otherwise, we go ahead and convert the value in bits, and
               --  set SU_Convert_Required to True to ensure that the final
               --  value is indeed properly converted.

               else
                  Size := (Dynamic, Make_Integer_Literal (Loc, Size.Val));
                  SU_Convert_Required := True;
               end if;
            end if;

            --  Length is hi-lo+1

            Len := Compute_Length (Lo, Hi);

            --  Check possible range of Len

            declare
               OK  : Boolean;
               LLo : Uint;
               LHi : Uint;
               pragma Warnings (Off, LHi);

            begin
               Set_Parent (Len, E);
               Determine_Range (Len, OK, LLo, LHi);

               Len := Convert_To (Standard_Unsigned, Len);

               --  If we cannot verify that range cannot be super-flat, we need
               --  a max with zero, since length must be non-negative.

               if not OK or else LLo < 0 then
                  Len :=
                    Make_Attribute_Reference (Loc,
                      Prefix         =>
                        New_Occurrence_Of (Standard_Unsigned, Loc),
                      Attribute_Name => Name_Max,
                      Expressions    => New_List (
                        Make_Integer_Literal (Loc, 0),
                        Len));
               end if;
            end;
         end if;

         Next_Index (Indx);
      end loop;

      --  Here after processing all bounds to set sizes. If the value is a
      --  constant, then it is bits, so we convert to storage units.

      if Size.Status = Const then
         return Bits_To_SU (Make_Integer_Literal (Loc, Size.Val));

      --  Case where the value is dynamic

      else
         --  Do convert from bits to SU's if needed

         if SU_Convert_Required then

            --  The expression required is (Size.Nod + SU - 1) / SU

            Size.Nod :=
              Make_Op_Divide (Loc,
                Left_Opnd =>
                  Make_Op_Add (Loc,
                    Left_Opnd  => Size.Nod,
                    Right_Opnd => Make_Integer_Literal (Loc, SSU - 1)),
                Right_Opnd => Make_Integer_Literal (Loc, SSU));
         end if;

         return Size.Nod;
      end if;
   end Get_Max_SU_Size;

   -----------------------
   -- Layout_Array_Type --
   -----------------------

   procedure Layout_Array_Type (E : Entity_Id) is
      Loc  : constant Source_Ptr := Sloc (E);
      Ctyp : constant Entity_Id  := Component_Type (E);
      Indx : Node_Id;
      Ityp : Entity_Id;
      Lo   : Node_Id;
      Hi   : Node_Id;
      S    : Uint;
      Len  : Node_Id;

      Insert_Typ : Entity_Id;
      --  This is the type with which any generated constants or functions
      --  will be associated (i.e. inserted into the freeze actions). This
      --  is normally the type being laid out. The exception occurs when
      --  we are laying out Itype's which are local to a record type, and
      --  whose scope is this record type. Such types do not have freeze
      --  nodes (because we have no place to put them).

      ------------------------------------
      -- How An Array Type is Laid Out --
      ------------------------------------

      --  Here is what goes on. We need to multiply the component size of the
      --  array (which has already been set) by the length of each of the
      --  indexes. If all these values are known at compile time, then the
      --  resulting size of the array is the appropriate constant value.

      --  If the component size or at least one bound is dynamic (but no
      --  discriminants are present), then the size will be computed as an
      --  expression that calculates the proper size.

      --  If there is at least one discriminant bound, then the size is also
      --  computed as an expression, but this expression contains discriminant
      --  values which are obtained by selecting from a function parameter, and
      --  the size is given by a function that is passed the variant record in
      --  question, and whose body is the expression.

      type Val_Status_Type is (Const, Dynamic, Discrim);

      type Val_Type (Status : Val_Status_Type := Const) is
         record
            case Status is
               when Const =>
                  Val : Uint;
                  --  Calculated value so far if Val_Status = Const

               when Dynamic | Discrim =>
                  Nod : Node_Id;
                  --  Expression value so far if Val_Status /= Const

            end case;
         end record;
      --  Records the value or expression computed so far. Const means that
      --  the value is constant, and Val is the current constant value.
      --  Dynamic means that the value is dynamic, and in this case Nod is
      --  the Node_Id of the expression to compute the value, and Discrim
      --  means that at least one bound is a discriminant, in which case Nod
      --  is the expression so far (which will be the body of the function).

      Size : Val_Type;
      --  Value of size computed so far. See comments above

      Vtyp : Entity_Id := Empty;
      --  Variant record type for the formal parameter of the discriminant
      --  function V if Status = Discrim.

      SU_Convert_Required : Boolean := False;
      --  This is set to True if the final result must be converted from
      --  bits to storage units (rounding up to a storage unit boundary).

      Storage_Divisor : Uint := UI_From_Int (SSU);
      --  This is the amount that a nonstatic computed size will be divided
      --  by to convert it from bits to storage units. This is normally
      --  equal to SSU, but can be reduced in the case of packed components
      --  that fit evenly into a storage unit.

      Make_Size_Function : Boolean := False;
      --  Indicates whether to request that SO_Ref_From_Expr should
      --  encapsulate the array size expression in a function.

      procedure Discrimify (N : in out Node_Id);
      --  If N represents a discriminant, then the Size.Status is set to
      --  Discrim, and Vtyp is set. The parameter N is replaced with the
      --  proper expression to extract the discriminant value from V.

      ----------------
      -- Discrimify --
      ----------------

      procedure Discrimify (N : in out Node_Id) is
         Decl : Node_Id;
         Typ  : Entity_Id;

      begin
         if Nkind (N) = N_Identifier
           and then Ekind (Entity (N)) = E_Discriminant
         then
            Set_Size_Depends_On_Discriminant (E);

            if Size.Status /= Discrim then
               Decl := Parent (Parent (Entity (N)));
               Size := (Discrim, Size.Nod);
               Vtyp := Defining_Identifier (Decl);
            end if;

            Typ := Etype (N);

            N :=
              Make_Selected_Component (Loc,
                Prefix        => Make_Identifier (Loc, Vname),
                Selector_Name => New_Occurrence_Of (Entity (N), Loc));

            --  Set the Etype attributes of the selected name and its prefix.
            --  Analyze_And_Resolve can't be called here because the Vname
            --  entity denoted by the prefix will not yet exist (it's created
            --  by SO_Ref_From_Expr, called at the end of Layout_Array_Type).

            Set_Etype (Prefix (N), Vtyp);
            Set_Etype (N, Typ);
         end if;
      end Discrimify;

   --  Start of processing for Layout_Array_Type

   begin
      --  Default alignment is component alignment

      if Unknown_Alignment (E) then
         Set_Alignment (E, Alignment (Ctyp));
      end if;

      --  Calculate proper type for insertions

      if Is_Record_Type (Underlying_Type (Scope (E))) then
         Insert_Typ := Underlying_Type (Scope (E));
      else
         Insert_Typ := E;
      end if;

      --  If the component type is a generic formal type then there's no point
      --  in determining a size for the array type.

      if Is_Generic_Type (Ctyp) then
         return;
      end if;

      --  Deal with component size if base type

      if Ekind (E) = E_Array_Type then

         --  Cannot do anything if Esize of component type unknown

         if Unknown_Esize (Ctyp) then
            return;
         end if;

         --  Set component size if not set already

         if Unknown_Component_Size (E) then
            Set_Component_Size (E, Esize (Ctyp));
         end if;
      end if;

      --  (RM 13.3 (48)) says that the size of an unconstrained array
      --  is implementation defined. We choose to leave it as Unknown
      --  here, and the actual behavior is determined by the back end.

      if not Is_Constrained (E) then
         return;
      end if;

      --  Initialize status from component size

      if Known_Static_Component_Size (E) then
         Size := (Const, Component_Size (E));

      else
         Size := (Dynamic, Expr_From_SO_Ref (Loc, Component_Size (E)));
      end if;

      --  Loop to process array indexes

      Indx := First_Index (E);
      while Present (Indx) loop
         Ityp := Etype (Indx);

         --  If an index of the array is a generic formal type then there is
         --  no point in determining a size for the array type.

         if Is_Generic_Type (Ityp) then
            return;
         end if;

         Lo := Type_Low_Bound (Ityp);
         Hi := Type_High_Bound (Ityp);

         --  Value of the current subscript range is statically known

         if Compile_Time_Known_Value (Lo)
              and then
            Compile_Time_Known_Value (Hi)
         then
            S := Expr_Value (Hi) - Expr_Value (Lo) + 1;

            --  If known flat bound, entire size of array is zero

            if S <= 0 then
               Set_Esize (E, Uint_0);
               Set_RM_Size (E, Uint_0);
               return;
            end if;

            --  If constant, evolve value

            if Size.Status = Const then
               Size.Val := Size.Val * S;

            --  Current value is dynamic

            else
               --  An interesting little optimization, if we have a pending
               --  conversion from bits to storage units, and the current
               --  length is a multiple of the storage unit size, then we
               --  can take the factor out here statically, avoiding some
               --  extra dynamic computations at the end.

               if SU_Convert_Required and then S mod SSU = 0 then
                  S := S / SSU;
                  SU_Convert_Required := False;
               end if;

               --  Now go ahead and evolve the expression

               Size.Nod :=
                 Assoc_Multiply (Loc,
                   Left_Opnd  => Size.Nod,
                   Right_Opnd =>
                     Make_Integer_Literal (Loc, Intval => S));
            end if;

         --  Value of the current subscript range is dynamic

         else
            --  If the current size value is constant, then here is where we
            --  make a transition to dynamic values, which are always stored
            --  in storage units, However, we do not want to convert to SU's
            --  too soon, consider the case of a packed array of single bits,
            --  we want to do the SU conversion after computing the size in
            --  this case.

            if Size.Status = Const then

               --  If the current value is a multiple of the storage unit,
               --  then most certainly we can do the conversion now, simply
               --  by dividing the current value by the storage unit value.
               --  If this works, we set SU_Convert_Required to False.

               if Size.Val mod SSU = 0 then
                  Size :=
                    (Dynamic, Make_Integer_Literal (Loc, Size.Val / SSU));
                  SU_Convert_Required := False;

               --  If the current value is a factor of the storage unit, then
               --  we can use a value of one for the size and reduce the
               --  strength of the later division.

               elsif SSU mod Size.Val = 0 then
                  Storage_Divisor := SSU / Size.Val;
                  Size := (Dynamic, Make_Integer_Literal (Loc, Uint_1));
                  SU_Convert_Required := True;

               --  Otherwise, we go ahead and convert the value in bits, and
               --  set SU_Convert_Required to True to ensure that the final
               --  value is indeed properly converted.

               else
                  Size := (Dynamic, Make_Integer_Literal (Loc, Size.Val));
                  SU_Convert_Required := True;
               end if;
            end if;

            Discrimify (Lo);
            Discrimify (Hi);

            --  Length is hi-lo+1

            Len := Compute_Length (Lo, Hi);

            --  If Len isn't a Length attribute, then its range needs to be
            --  checked a possible Max with zero needs to be computed.

            if Nkind (Len) /= N_Attribute_Reference
              or else Attribute_Name (Len) /= Name_Length
            then
               declare
                  OK  : Boolean;
                  LLo : Uint;
                  LHi : Uint;

               begin
                  --  Check possible range of Len

                  Set_Parent (Len, E);
                  Determine_Range (Len, OK, LLo, LHi);

                  Len := Convert_To (Standard_Unsigned, Len);

                  --  If range definitely flat or superflat, result size is 0

                  if OK and then LHi <= 0 then
                     Set_Esize (E, Uint_0);
                     Set_RM_Size (E, Uint_0);
                     return;
                  end if;

                  --  If we cannot verify that range cannot be super-flat, we
                  --  need a max with zero, since length cannot be negative.

                  if not OK or else LLo < 0 then
                     Len :=
                       Make_Attribute_Reference (Loc,
                         Prefix         =>
                           New_Occurrence_Of (Standard_Unsigned, Loc),
                         Attribute_Name => Name_Max,
                         Expressions    => New_List (
                           Make_Integer_Literal (Loc, 0),
                           Len));
                  end if;
               end;
            end if;

            --  At this stage, Len has the expression for the length

            Size.Nod :=
              Assoc_Multiply (Loc,
                Left_Opnd  => Size.Nod,
                Right_Opnd => Len);
         end if;

         Next_Index (Indx);
      end loop;

      --  Here after processing all bounds to set sizes. If the value is a
      --  constant, then it is bits, and the only thing we need to do is to
      --  check against explicit given size and do alignment adjust.

      if Size.Status = Const then
         Set_And_Check_Static_Size (E, Size.Val, Size.Val);
         Adjust_Esize_Alignment (E);

      --  Case where the value is dynamic

      else
         --  Do convert from bits to SU's if needed

         if SU_Convert_Required then

            --  The expression required is:
            --    (Size.Nod + Storage_Divisor - 1) / Storage_Divisor

            Size.Nod :=
              Make_Op_Divide (Loc,
                Left_Opnd =>
                  Make_Op_Add (Loc,
                    Left_Opnd  => Size.Nod,
                    Right_Opnd => Make_Integer_Literal
                                    (Loc, Storage_Divisor - 1)),
                Right_Opnd => Make_Integer_Literal (Loc, Storage_Divisor));
         end if;

         --  If the array entity is not declared at the library level and its
         --  not nested within a subprogram that is marked for inlining, then
         --  we request that the size expression be encapsulated in a function.
         --  Since this expression is not needed in most cases, we prefer not
         --  to incur the overhead of the computation on calls to the enclosing
         --  subprogram except for subprograms that require the size.

         if not Is_Library_Level_Entity (E) then
            Make_Size_Function := True;

            declare
               Parent_Subp : Entity_Id := Enclosing_Subprogram (E);

            begin
               while Present (Parent_Subp) loop
                  if Is_Inlined (Parent_Subp) then
                     Make_Size_Function := False;
                     exit;
                  end if;

                  Parent_Subp := Enclosing_Subprogram (Parent_Subp);
               end loop;
            end;
         end if;

         --  Now set the dynamic size (the Value_Size is always the same as the
         --  Object_Size for arrays whose length is dynamic).

         --  ??? If Size.Status = Dynamic, Vtyp will not have been set.
         --  The added initialization sets it to Empty now, but is this
         --  correct?

         Set_Esize
           (E,
            SO_Ref_From_Expr
              (Size.Nod, Insert_Typ, Vtyp, Make_Func => Make_Size_Function));
         Set_RM_Size (E, Esize (E));
      end if;
   end Layout_Array_Type;

   ------------------------------------------
   -- Compute_Size_Depends_On_Discriminant --
   ------------------------------------------

   procedure Compute_Size_Depends_On_Discriminant (E : Entity_Id) is
      Indx : Node_Id;
      Ityp : Entity_Id;
      Lo   : Node_Id;
      Hi   : Node_Id;
      Res  : Boolean := False;

   begin
      --  Loop to process array indexes

      Indx := First_Index (E);
      while Present (Indx) loop
         Ityp := Etype (Indx);

         --  If an index of the array is a generic formal type then there is
         --  no point in determining a size for the array type.

         if Is_Generic_Type (Ityp) then
            return;
         end if;

         Lo := Type_Low_Bound (Ityp);
         Hi := Type_High_Bound (Ityp);

         if (Nkind (Lo) = N_Identifier
              and then Ekind (Entity (Lo)) = E_Discriminant)
           or else
            (Nkind (Hi) = N_Identifier
              and then Ekind (Entity (Hi)) = E_Discriminant)
         then
            Res := True;
         end if;

         Next_Index (Indx);
      end loop;

      if Res then
         Set_Size_Depends_On_Discriminant (E);
      end if;
   end Compute_Size_Depends_On_Discriminant;

   -------------------
   -- Layout_Object --
   -------------------

   procedure Layout_Object (E : Entity_Id) is
      T : constant Entity_Id := Etype (E);

   begin
      --  Nothing to do if backend does layout

      if not Frontend_Layout_On_Target then
         return;
      end if;

      --  Set size if not set for object and known for type. Use the RM_Size if
      --  that is known for the type and Esize is not.

      if Unknown_Esize (E) then
         if Known_Esize (T) then
            Set_Esize (E, Esize (T));

         elsif Known_RM_Size (T) then
            Set_Esize (E, RM_Size (T));
         end if;
      end if;

      --  Set alignment from type if unknown and type alignment known

      if Unknown_Alignment (E) and then Known_Alignment (T) then
         Set_Alignment (E, Alignment (T));
      end if;

      --  Make sure size and alignment are consistent

      Adjust_Esize_Alignment (E);

      --  Final adjustment, if we don't know the alignment, and the Esize was
      --  not set by an explicit Object_Size attribute clause, then we reset
      --  the Esize to unknown, since we really don't know it.

      if Unknown_Alignment (E) and then not Has_Size_Clause (E) then
         Set_Esize (E, Uint_0);
      end if;
   end Layout_Object;

   ------------------------
   -- Layout_Record_Type --
   ------------------------

   procedure Layout_Record_Type (E : Entity_Id) is
      Loc  : constant Source_Ptr := Sloc (E);
      Decl : Node_Id;

      Comp : Entity_Id;
      --  Current component being laid out

      Prev_Comp : Entity_Id;
      --  Previous laid out component

      procedure Get_Next_Component_Location
        (Prev_Comp  : Entity_Id;
         Align      : Uint;
         New_Npos   : out SO_Ref;
         New_Fbit   : out SO_Ref;
         New_NPMax  : out SO_Ref;
         Force_SU   : Boolean);
      --  Given the previous component in Prev_Comp, which is already laid
      --  out, and the alignment of the following component, lays out the
      --  following component, and returns its starting position in New_Npos
      --  (Normalized_Position value), New_Fbit (Normalized_First_Bit value),
      --  and New_NPMax (Normalized_Position_Max value). If Prev_Comp is empty
      --  (no previous component is present), then New_Npos, New_Fbit and
      --  New_NPMax are all set to zero on return. This procedure is also
      --  used to compute the size of a record or variant by giving it the
      --  last component, and the record alignment. Force_SU is used to force
      --  the new component location to be aligned on a storage unit boundary,
      --  even in a packed record, False means that the new position does not
      --  need to be bumped to a storage unit boundary, True means a storage
      --  unit boundary is always required.

      procedure Layout_Component (Comp : Entity_Id; Prev_Comp : Entity_Id);
      --  Lays out component Comp, given Prev_Comp, the previously laid-out
      --  component (Prev_Comp = Empty if no components laid out yet). The
      --  alignment of the record itself is also updated if needed. Both
      --  Comp and Prev_Comp can be either components or discriminants.

      procedure Layout_Components
        (From   : Entity_Id;
         To     : Entity_Id;
         Esiz   : out SO_Ref;
         RM_Siz : out SO_Ref);
      --  This procedure lays out the components of the given component list
      --  which contains the components starting with From and ending with To.
      --  The Next_Entity chain is used to traverse the components. On entry,
      --  Prev_Comp is set to the component preceding the list, so that the
      --  list is laid out after this component. Prev_Comp is set to Empty if
      --  the component list is to be laid out starting at the start of the
      --  record. On return, the components are all laid out, and Prev_Comp is
      --  set to the last laid out component. On return, Esiz is set to the
      --  resulting Object_Size value, which is the length of the record up
      --  to and including the last laid out entity. For Esiz, the value is
      --  adjusted to match the alignment of the record. RM_Siz is similarly
      --  set to the resulting Value_Size value, which is the same length, but
      --  not adjusted to meet the alignment. Note that in the case of variant
      --  records, Esiz represents the maximum size.

      procedure Layout_Non_Variant_Record;
      --  Procedure called to lay out a non-variant record type or subtype

      procedure Layout_Variant_Record;
      --  Procedure called to lay out a variant record type. Decl is set to the
      --  full type declaration for the variant record.

      ---------------------------------
      -- Get_Next_Component_Location --
      ---------------------------------

      procedure Get_Next_Component_Location
        (Prev_Comp  : Entity_Id;
         Align      : Uint;
         New_Npos   : out SO_Ref;
         New_Fbit   : out SO_Ref;
         New_NPMax  : out SO_Ref;
         Force_SU   : Boolean)
      is
      begin
         --  No previous component, return zero position

         if No (Prev_Comp) then
            New_Npos  := Uint_0;
            New_Fbit  := Uint_0;
            New_NPMax := Uint_0;
            return;
         end if;

         --  Here we have a previous component

         declare
            Loc       : constant Source_Ptr := Sloc (Prev_Comp);

            Old_Npos  : constant SO_Ref := Normalized_Position     (Prev_Comp);
            Old_Fbit  : constant SO_Ref := Normalized_First_Bit    (Prev_Comp);
            Old_NPMax : constant SO_Ref := Normalized_Position_Max (Prev_Comp);
            Old_Esiz  : constant SO_Ref := Esize                   (Prev_Comp);

            Old_Maxsz : Node_Id;
            --  Expression representing maximum size of previous component

         begin
            --  Case where previous field had a dynamic size

            if Is_Dynamic_SO_Ref (Esize (Prev_Comp)) then

               --  If the previous field had a dynamic length, then it is
               --  required to occupy an integral number of storage units,
               --  and start on a storage unit boundary. This means that
               --  the Normalized_First_Bit value is zero in the previous
               --  component, and the new value is also set to zero.

               New_Fbit := Uint_0;

               --  In this case, the new position is given by an expression
               --  that is the sum of old normalized position and old size.

               New_Npos :=
                 SO_Ref_From_Expr
                   (Assoc_Add (Loc,
                      Left_Opnd  =>
                        Expr_From_SO_Ref (Loc, Old_Npos),
                      Right_Opnd =>
                        Expr_From_SO_Ref (Loc, Old_Esiz, Prev_Comp)),
                    Ins_Type => E,
                    Vtype    => E);

               --  Get maximum size of previous component

               if Size_Depends_On_Discriminant (Etype (Prev_Comp)) then
                  Old_Maxsz := Get_Max_SU_Size (Etype (Prev_Comp));
               else
                  Old_Maxsz := Expr_From_SO_Ref (Loc, Old_Esiz, Prev_Comp);
               end if;

               --  Now we can compute the new max position. If the max size
               --  is static and the old position is static, then we can
               --  compute the new position statically.

               if Nkind (Old_Maxsz) = N_Integer_Literal
                 and then Known_Static_Normalized_Position_Max (Prev_Comp)
               then
                  New_NPMax := Old_NPMax + Intval (Old_Maxsz);

               --  Otherwise new max position is dynamic

               else
                  New_NPMax :=
                    SO_Ref_From_Expr
                      (Assoc_Add (Loc,
                         Left_Opnd  => Expr_From_SO_Ref (Loc, Old_NPMax),
                         Right_Opnd => Old_Maxsz),
                       Ins_Type => E,
                       Vtype    => E);
               end if;

            --  Previous field has known static Esize

            else
               New_Fbit := Old_Fbit + Old_Esiz;

               --  Bump New_Fbit to storage unit boundary if required

               if New_Fbit /= 0 and then Force_SU then
                  New_Fbit := (New_Fbit + SSU - 1) / SSU * SSU;
               end if;

               --  If old normalized position is static, we can go ahead and
               --  compute the new normalized position directly.

               if Known_Static_Normalized_Position (Prev_Comp) then
                  New_Npos := Old_Npos;

                  if New_Fbit >= SSU then
                     New_Npos := New_Npos + New_Fbit / SSU;
                     New_Fbit := New_Fbit mod SSU;
                  end if;

                  --  Bump alignment if stricter than prev

                  if Align > Alignment (Etype (Prev_Comp)) then
                     New_Npos := (New_Npos + Align - 1) / Align * Align;
                  end if;

                  --  The max position is always equal to the position if
                  --  the latter is static, since arrays depending on the
                  --  values of discriminants never have static sizes.

                  New_NPMax := New_Npos;
                  return;

               --  Case of old normalized position is dynamic

               else
                  --  If new bit position is within the current storage unit,
                  --  we can just copy the old position as the result position
                  --  (we have already set the new first bit value).

                  if New_Fbit < SSU then
                     New_Npos  := Old_Npos;
                     New_NPMax := Old_NPMax;

                  --  If new bit position is past the current storage unit, we
                  --  need to generate a new dynamic value for the position
                  --  ??? need to deal with alignment

                  else
                     New_Npos :=
                       SO_Ref_From_Expr
                         (Assoc_Add (Loc,
                            Left_Opnd  => Expr_From_SO_Ref (Loc, Old_Npos),
                            Right_Opnd =>
                              Make_Integer_Literal (Loc,
                                Intval => New_Fbit / SSU)),
                          Ins_Type => E,
                          Vtype    => E);

                     New_NPMax :=
                       SO_Ref_From_Expr
                         (Assoc_Add (Loc,
                            Left_Opnd  => Expr_From_SO_Ref (Loc, Old_NPMax),
                            Right_Opnd =>
                              Make_Integer_Literal (Loc,
                                Intval => New_Fbit / SSU)),
                            Ins_Type => E,
                            Vtype    => E);
                     New_Fbit := New_Fbit mod SSU;
                  end if;
               end if;
            end if;
         end;
      end Get_Next_Component_Location;

      ----------------------
      -- Layout_Component --
      ----------------------

      procedure Layout_Component (Comp : Entity_Id; Prev_Comp : Entity_Id) is
         Ctyp  : constant Entity_Id := Etype (Comp);
         ORC   : constant Entity_Id := Original_Record_Component (Comp);
         Npos  : SO_Ref;
         Fbit  : SO_Ref;
         NPMax : SO_Ref;
         Forc  : Boolean;

      begin
         --  Increase alignment of record if necessary. Note that we do not
         --  do this for packed records, which have an alignment of one by
         --  default, or for records for which an explicit alignment was
         --  specified with an alignment clause.

         if not Is_Packed (E)
           and then not Has_Alignment_Clause (E)
           and then Alignment (Ctyp) > Alignment (E)
         then
            Set_Alignment (E, Alignment (Ctyp));
         end if;

         --  If original component set, then use same layout

         if Present (ORC) and then ORC /= Comp then
            Set_Normalized_Position     (Comp, Normalized_Position     (ORC));
            Set_Normalized_First_Bit    (Comp, Normalized_First_Bit    (ORC));
            Set_Normalized_Position_Max (Comp, Normalized_Position_Max (ORC));
            Set_Component_Bit_Offset    (Comp, Component_Bit_Offset    (ORC));
            Set_Esize                   (Comp, Esize                   (ORC));
            return;
         end if;

         --  Parent field is always at start of record, this will overlap
         --  the actual fields that are part of the parent, and that's fine

         if Chars (Comp) = Name_uParent then
            Set_Normalized_Position     (Comp, Uint_0);
            Set_Normalized_First_Bit    (Comp, Uint_0);
            Set_Normalized_Position_Max (Comp, Uint_0);
            Set_Component_Bit_Offset    (Comp, Uint_0);
            Set_Esize                   (Comp, Esize (Ctyp));
            return;
         end if;

         --  Check case of type of component has a scope of the record we are
         --  laying out. When this happens, the type in question is an Itype
         --  that has not yet been laid out (that's because such types do not
         --  get frozen in the normal manner, because there is no place for
         --  the freeze nodes).

         if Scope (Ctyp) = E then
            Layout_Type (Ctyp);
         end if;

         --  If component already laid out, then we are done

         if Known_Normalized_Position (Comp) then
            return;
         end if;

         --  Set size of component from type. We use the Esize except in a
         --  packed record, where we use the RM_Size (since that is what the
         --  RM_Size value, as distinct from the Object_Size is useful for).

         if Is_Packed (E) then
            Set_Esize (Comp, RM_Size (Ctyp));
         else
            Set_Esize (Comp, Esize (Ctyp));
         end if;

         --  Compute the component position from the previous one. See if
         --  current component requires being on a storage unit boundary.

         --  If record is not packed, we always go to a storage unit boundary

         if not Is_Packed (E) then
            Forc := True;

         --  Packed cases

         else
            --  Elementary types do not need SU boundary in packed record

            if Is_Elementary_Type (Ctyp) then
               Forc := False;

            --  Packed array types with a modular packed array type do not
            --  force a storage unit boundary (since the code generation
            --  treats these as equivalent to the underlying modular type),

            elsif Is_Array_Type (Ctyp)
              and then Is_Bit_Packed_Array (Ctyp)
              and then Is_Modular_Integer_Type (Packed_Array_Impl_Type (Ctyp))
            then
               Forc := False;

            --  Record types with known length less than or equal to the length
            --  of long long integer can also be unaligned, since they can be
            --  treated as scalars.

            elsif Is_Record_Type (Ctyp)
              and then not Is_Dynamic_SO_Ref (Esize (Ctyp))
              and then Esize (Ctyp) <= Esize (Standard_Long_Long_Integer)
            then
               Forc := False;

            --  All other cases force a storage unit boundary, even when packed

            else
               Forc := True;
            end if;
         end if;

         --  Now get the next component location

         Get_Next_Component_Location
           (Prev_Comp, Alignment (Ctyp), Npos, Fbit, NPMax, Forc);
         Set_Normalized_Position     (Comp, Npos);
         Set_Normalized_First_Bit    (Comp, Fbit);
         Set_Normalized_Position_Max (Comp, NPMax);

         --  Set Component_Bit_Offset in the static case

         if Known_Static_Normalized_Position (Comp)
           and then Known_Normalized_First_Bit (Comp)
         then
            Set_Component_Bit_Offset (Comp, SSU * Npos + Fbit);
         end if;
      end Layout_Component;

      -----------------------
      -- Layout_Components --
      -----------------------

      procedure Layout_Components
        (From   : Entity_Id;
         To     : Entity_Id;
         Esiz   : out SO_Ref;
         RM_Siz : out SO_Ref)
      is
         End_Npos  : SO_Ref;
         End_Fbit  : SO_Ref;
         End_NPMax : SO_Ref;

      begin
         --  Only lay out components if there are some to lay out

         if Present (From) then

            --  Lay out components with no component clauses

            Comp := From;
            loop
               if Ekind (Comp) = E_Component
                 or else Ekind (Comp) = E_Discriminant
               then
                  --  The compatibility of component clauses with composite
                  --  types isn't checked in Sem_Ch13, so we check it here.

                  if Present (Component_Clause (Comp)) then
                     if Is_Composite_Type (Etype (Comp))
                       and then Esize (Comp) < RM_Size (Etype (Comp))
                     then
                        Error_Msg_Uint_1 := RM_Size (Etype (Comp));
                        Error_Msg_NE
                          ("size for & too small, minimum allowed is ^",
                           Component_Clause (Comp),
                           Comp);
                     end if;

                  else
                     Layout_Component (Comp, Prev_Comp);
                     Prev_Comp := Comp;
                  end if;
               end if;

               exit when Comp = To;
               Next_Entity (Comp);
            end loop;
         end if;

         --  Set size fields, both are zero if no components

         if No (Prev_Comp) then
            Esiz := Uint_0;
            RM_Siz := Uint_0;

            --  If record subtype with non-static discriminants, then we don't
            --  know which variant will be the one which gets chosen. We don't
            --  just want to set the maximum size from the base, because the
            --  size should depend on the particular variant.

            --  What we do is to use the RM_Size of the base type, which has
            --  the necessary conditional computation of the size, using the
            --  size information for the particular variant chosen. Records
            --  with default discriminants for example have an Esize that is
            --  set to the maximum of all variants, but that's not what we
            --  want for a constrained subtype.

         elsif Ekind (E) = E_Record_Subtype
           and then not Has_Static_Discriminants (E)
         then
            declare
               BT : constant Node_Id := Base_Type (E);
            begin
               Esiz   := RM_Size (BT);
               RM_Siz := RM_Size (BT);
               Set_Alignment (E, Alignment (BT));
            end;

         else
            --  First the object size, for which we align past the last field
            --  to the alignment of the record (the object size is required to
            --  be a multiple of the alignment).

            Get_Next_Component_Location
              (Prev_Comp,
               Alignment (E),
               End_Npos,
               End_Fbit,
               End_NPMax,
               Force_SU => True);

            --  If the resulting normalized position is a dynamic reference,
            --  then the size is dynamic, and is stored in storage units. In
            --  this case, we set the RM_Size to the same value, it is simply
            --  not worth distinguishing Esize and RM_Size values in the
            --  dynamic case, since the RM has nothing to say about them.

            --  Note that a size cannot have been given in this case, since
            --  size specifications cannot be given for variable length types.

            declare
               Align : constant Uint := Alignment (E);

            begin
               if Is_Dynamic_SO_Ref (End_Npos) then
                  RM_Siz := End_Npos;

                  --  Set the Object_Size allowing for the alignment. In the
                  --  dynamic case, we must do the actual runtime computation.
                  --  We can skip this in the non-packed record case if the
                  --  last component has a smaller alignment than the overall
                  --  record alignment.

                  if Is_Dynamic_SO_Ref (End_NPMax) then
                     Esiz := End_NPMax;

                     if Is_Packed (E)
                       or else Alignment (Etype (Prev_Comp)) < Align
                     then
                        --  The expression we build is:
                        --    (expr + align - 1) / align * align

                        Esiz :=
                          SO_Ref_From_Expr
                            (Expr =>
                               Make_Op_Multiply (Loc,
                                 Left_Opnd =>
                                   Make_Op_Divide (Loc,
                                     Left_Opnd =>
                                       Make_Op_Add (Loc,
                                         Left_Opnd =>
                                           Expr_From_SO_Ref (Loc, Esiz),
                                         Right_Opnd =>
                                           Make_Integer_Literal (Loc,
                                             Intval => Align - 1)),
                                     Right_Opnd =>
                                       Make_Integer_Literal (Loc, Align)),
                                 Right_Opnd =>
                                   Make_Integer_Literal (Loc, Align)),
                            Ins_Type => E,
                            Vtype    => E);
                     end if;

                  --  Here Esiz is static, so we can adjust the alignment
                  --  directly go give the required aligned value.

                  else
                     Esiz := (End_NPMax + Align - 1) / Align * Align * SSU;
                  end if;

               --  Case where computed size is static

               else
                  --  The ending size was computed in Npos in storage units,
                  --  but the actual size is stored in bits, so adjust
                  --  accordingly. We also adjust the size to match the
                  --  alignment here.

                  Esiz := (End_NPMax + Align - 1) / Align * Align * SSU;

                  --  Compute the resulting Value_Size (RM_Size). For this
                  --  purpose we do not force alignment of the record or
                  --  storage size alignment of the result.

                  Get_Next_Component_Location
                    (Prev_Comp,
                     Uint_0,
                     End_Npos,
                     End_Fbit,
                     End_NPMax,
                     Force_SU => False);

                  RM_Siz := End_Npos * SSU + End_Fbit;
                  Set_And_Check_Static_Size (E, Esiz, RM_Siz);
               end if;
            end;
         end if;
      end Layout_Components;

      -------------------------------
      -- Layout_Non_Variant_Record --
      -------------------------------

      procedure Layout_Non_Variant_Record is
         Esiz   : SO_Ref;
         RM_Siz : SO_Ref;
      begin
         Layout_Components (First_Entity (E), Last_Entity (E), Esiz, RM_Siz);
         Set_Esize   (E, Esiz);
         Set_RM_Size (E, RM_Siz);
      end Layout_Non_Variant_Record;

      ---------------------------
      -- Layout_Variant_Record --
      ---------------------------

      procedure Layout_Variant_Record is
         Tdef        : constant Node_Id := Type_Definition (Decl);
         First_Discr : Entity_Id;
         Last_Discr  : Entity_Id;
         Esiz        : SO_Ref;

         RM_Siz : SO_Ref;
         pragma Warnings (Off, SO_Ref);

         RM_Siz_Expr : Node_Id := Empty;
         --  Expression for the evolving RM_Siz value. This is typically an if
         --  expression which involves tests of discriminant values that are
         --  formed as references to the entity V. At the end of scanning all
         --  the components, a suitable function is constructed in which V is
         --  the parameter.

         -----------------------
         -- Local Subprograms --
         -----------------------

         procedure Layout_Component_List
           (Clist       : Node_Id;
            Esiz        : out SO_Ref;
            RM_Siz_Expr : out Node_Id);
         --  Recursive procedure, called to lay out one component list Esiz
         --  and RM_Siz_Expr are set to the Object_Size and Value_Size values
         --  respectively representing the record size up to and including the
         --  last component in the component list (including any variants in
         --  this component list). RM_Siz_Expr is returned as an expression
         --  which may in the general case involve some references to the
         --  discriminants of the current record value, referenced by selecting
         --  from the entity V.

         ---------------------------
         -- Layout_Component_List --
         ---------------------------

         procedure Layout_Component_List
           (Clist       : Node_Id;
            Esiz        : out SO_Ref;
            RM_Siz_Expr : out Node_Id)
         is
            Citems  : constant List_Id := Component_Items (Clist);
            Vpart   : constant Node_Id := Variant_Part (Clist);
            Prv     : Node_Id;
            Var     : Node_Id;
            RM_Siz  : Uint;
            RMS_Ent : Entity_Id;

         begin
            if Is_Non_Empty_List (Citems) then
               Layout_Components
                 (From   => Defining_Identifier (First (Citems)),
                  To     => Defining_Identifier (Last  (Citems)),
                  Esiz   => Esiz,
                  RM_Siz => RM_Siz);
            else
               Layout_Components (Empty, Empty, Esiz, RM_Siz);
            end if;

            --  Case where no variants are present in the component list

            if No (Vpart) then

               --  The Esiz value has been correctly set by the call to
               --  Layout_Components, so there is nothing more to be done.

               --  For RM_Siz, we have an SO_Ref value, which we must convert
               --  to an appropriate expression.

               if Is_Static_SO_Ref (RM_Siz) then
                  RM_Siz_Expr :=
                    Make_Integer_Literal (Loc,
                                          Intval => RM_Siz);

               else
                  RMS_Ent := Get_Dynamic_SO_Entity (RM_Siz);

                  --  If the size is represented by a function, then we create
                  --  an appropriate function call using V as the parameter to
                  --  the call.

                  if Is_Discrim_SO_Function (RMS_Ent) then
                     RM_Siz_Expr :=
                       Make_Function_Call (Loc,
                         Name => New_Occurrence_Of (RMS_Ent, Loc),
                         Parameter_Associations => New_List (
                           Make_Identifier (Loc, Vname)));

                  --  If the size is represented by a constant, then the
                  --  expression we want is a reference to this constant

                  else
                     RM_Siz_Expr := New_Occurrence_Of (RMS_Ent, Loc);
                  end if;
               end if;

            --  Case where variants are present in this component list

            else
               declare
                  EsizV    : SO_Ref;
                  RM_SizV  : Node_Id;
                  Dchoice  : Node_Id;
                  Discrim  : Node_Id;
                  Dtest    : Node_Id;
                  D_List   : List_Id;
                  D_Entity : Entity_Id;

               begin
                  RM_Siz_Expr := Empty;
                  Prv := Prev_Comp;

                  Var := Last (Variants (Vpart));
                  while Present (Var) loop
                     Prev_Comp := Prv;
                     Layout_Component_List
                       (Component_List (Var), EsizV, RM_SizV);

                     --  Set the Object_Size. If this is the first variant,
                     --  we just set the size of this first variant.

                     if Var = Last (Variants (Vpart)) then
                        Esiz := EsizV;

                     --  Otherwise the Object_Size is formed as a maximum
                     --  of Esiz so far from previous variants, and the new
                     --  Esiz value from the variant we just processed.

                     --  If both values are static, we can just compute the
                     --  maximum directly to save building junk nodes.

                     elsif not Is_Dynamic_SO_Ref (Esiz)
                       and then not Is_Dynamic_SO_Ref (EsizV)
                     then
                        Esiz := UI_Max (Esiz, EsizV);

                     --  If either value is dynamic, then we have to generate
                     --  an appropriate Standard_Unsigned'Max attribute call.
                     --  If one of the values is static then it needs to be
                     --  converted from bits to storage units to be compatible
                     --  with the dynamic value.

                     else
                        if Is_Static_SO_Ref (Esiz) then
                           Esiz := (Esiz + SSU - 1) / SSU;
                        end if;

                        if Is_Static_SO_Ref (EsizV) then
                           EsizV := (EsizV + SSU - 1) / SSU;
                        end if;

                        Esiz :=
                          SO_Ref_From_Expr
                            (Make_Attribute_Reference (Loc,
                               Attribute_Name => Name_Max,
                               Prefix         =>
                                 New_Occurrence_Of (Standard_Unsigned, Loc),
                               Expressions => New_List (
                                 Expr_From_SO_Ref (Loc, Esiz),
                                 Expr_From_SO_Ref (Loc, EsizV))),
                             Ins_Type => E,
                             Vtype    => E);
                     end if;

                     --  Now deal with Value_Size (RM_Siz). We are aiming at
                     --  an expression that looks like:

                     --    if      xxDx (V.disc) then rmsiz1
                     --    else if xxDx (V.disc) then rmsiz2
                     --    else ...

                     --  Where rmsiz1, rmsiz2... are the RM_Siz values for the
                     --  individual variants, and xxDx are the discriminant
                     --  checking functions generated for the variant type.

                     --  If this is the first variant, we simply set the result
                     --  as the expression. Note that this takes care of the
                     --  others case.

                     if No (RM_Siz_Expr) then

                        --  If this is the only variant and the size is a
                        --  literal, then use bit size as is, otherwise convert
                        --  to storage units and continue to the next variant.

                        if No (Prev (Var))
                          and then Nkind (RM_SizV) = N_Integer_Literal
                        then
                           RM_Siz_Expr := RM_SizV;
                        else
                           RM_Siz_Expr := Bits_To_SU (RM_SizV);
                        end if;

                     --  Otherwise construct the appropriate test

                     else
                        --  The test to be used in general is a call to the
                        --  discriminant checking function. However, it is
                        --  definitely worth special casing the very common
                        --  case where a single value is involved.

                        Dchoice := First (Discrete_Choices (Var));

                        if No (Next (Dchoice))
                          and then Nkind (Dchoice) /= N_Range
                        then
                           --  Discriminant to be tested

                           Discrim :=
                             Make_Selected_Component (Loc,
                               Prefix        =>
                                 Make_Identifier (Loc, Vname),
                               Selector_Name =>
                                 New_Occurrence_Of
                                   (Entity (Name (Vpart)), Loc));

                           Dtest :=
                             Make_Op_Eq (Loc,
                               Left_Opnd  => Discrim,
                               Right_Opnd => New_Copy (Dchoice));

                        --  Generate a call to the discriminant-checking
                        --  function for the variant. Note that the result
                        --  has to be complemented since the function returns
                        --  False when the passed discriminant value matches.

                        else
                           --  The checking function takes all of the type's
                           --  discriminants as parameters, so a list of all
                           --  the selected discriminants must be constructed.

                           D_List := New_List;
                           D_Entity := First_Discriminant (E);
                           while Present (D_Entity) loop
                              Append_To (D_List,
                                Make_Selected_Component (Loc,
                                  Prefix        =>
                                    Make_Identifier (Loc, Vname),
                                  Selector_Name =>
                                    New_Occurrence_Of (D_Entity, Loc)));

                              D_Entity := Next_Discriminant (D_Entity);
                           end loop;

                           Dtest :=
                             Make_Op_Not (Loc,
                               Right_Opnd =>
                                 Make_Function_Call (Loc,
                                   Name =>
                                     New_Occurrence_Of
                                       (Dcheck_Function (Var), Loc),
                                   Parameter_Associations =>
                                     D_List));
                        end if;

                        RM_Siz_Expr :=
                          Make_If_Expression (Loc,
                            Expressions =>
                              New_List
                                (Dtest, Bits_To_SU (RM_SizV), RM_Siz_Expr));
                     end if;

                     Prev (Var);
                  end loop;
               end;
            end if;
         end Layout_Component_List;

         Others_Present : Boolean;
         pragma Warnings (Off, Others_Present);
         --  Indicates others present, not used in this case

         procedure Non_Static_Choice_Error (Choice : Node_Id);
         --  Error routine invoked by the generic instantiation below when
         --  the variant part has a nonstatic choice.

         package Variant_Choices_Processing is new
           Generic_Check_Choices
             (Process_Empty_Choice      => No_OP,
              Process_Non_Static_Choice => Non_Static_Choice_Error,
              Process_Associated_Node   => No_OP);
         use Variant_Choices_Processing;

         -----------------------------
         -- Non_Static_Choice_Error --
         -----------------------------

         procedure Non_Static_Choice_Error (Choice : Node_Id) is
         begin
            Flag_Non_Static_Expr
              ("choice given in case expression is not static!", Choice);
         end Non_Static_Choice_Error;

      --  Start of processing for Layout_Variant_Record

      begin
         --  Call Check_Choices here to ensure that Others_Discrete_Choices
         --  gets set on any 'others' choice before the discriminant-checking
         --  functions are generated. Otherwise the function for the 'others'
         --  alternative will unconditionally return True, causing discriminant
         --  checks to fail. However, Check_Choices is now normally delayed
         --  until the type's freeze entity is processed, due to requirements
         --  coming from subtype predicates, so doing it at this point is
         --  probably not right in general, but it's not clear how else to deal
         --  with this situation. Perhaps we should only generate declarations
         --  for the checking functions here, and somehow delay generation of
         --  their bodies, but that would be a nontrivial change. ???

         declare
            VP : constant Node_Id :=
                   Variant_Part (Component_List (Type_Definition (Decl)));
         begin
            Check_Choices
              (VP, Variants (VP), Etype (Name (VP)), Others_Present);
         end;

         --  We need the discriminant checking functions, since we generate
         --  calls to these functions for the RM_Size expression, so make
         --  sure that these functions have been constructed in time.

         Build_Discr_Checking_Funcs (Decl);

         --  Lay out the discriminants

         First_Discr := First_Discriminant (E);
         Last_Discr  := First_Discr;
         while Present (Next_Discriminant (Last_Discr)) loop
            Next_Discriminant (Last_Discr);
         end loop;

         Layout_Components
           (From   => First_Discr,
            To     => Last_Discr,
            Esiz   => Esiz,
            RM_Siz => RM_Siz);

         --  Lay out the main component list (this will make recursive calls
         --  to lay out all component lists nested within variants).

         Layout_Component_List (Component_List (Tdef), Esiz, RM_Siz_Expr);
         Set_Esize (E, Esiz);

         --  If the RM_Size is a literal, set its value

         if Nkind (RM_Siz_Expr) = N_Integer_Literal then
            Set_RM_Size (E, Intval (RM_Siz_Expr));

         --  Otherwise we construct a dynamic SO_Ref

         else
            Set_RM_Size (E,
              SO_Ref_From_Expr
                (RM_Siz_Expr,
                 Ins_Type => E,
                 Vtype    => E));
         end if;
      end Layout_Variant_Record;

   --  Start of processing for Layout_Record_Type

   begin
      --  If this is a cloned subtype, just copy the size fields from the
      --  original, nothing else needs to be done in this case, since the
      --  components themselves are all shared.

      if Ekind_In (E, E_Record_Subtype, E_Class_Wide_Subtype)
        and then Present (Cloned_Subtype (E))
      then
         Set_Esize     (E, Esize     (Cloned_Subtype (E)));
         Set_RM_Size   (E, RM_Size   (Cloned_Subtype (E)));
         Set_Alignment (E, Alignment (Cloned_Subtype (E)));

      --  Another special case, class-wide types. The RM says that the size
      --  of such types is implementation defined (RM 13.3(48)). What we do
      --  here is to leave the fields set as unknown values, and the backend
      --  determines the actual behavior.

      elsif Ekind (E) = E_Class_Wide_Type then
         null;

      --  All other cases

      else
         --  Initialize alignment conservatively to 1. This value will be
         --  increased as necessary during processing of the record.

         if Unknown_Alignment (E) then
            Set_Alignment (E, Uint_1);
         end if;

         --  Initialize previous component. This is Empty unless there are
         --  components which have already been laid out by component clauses.
         --  If there are such components, we start our lay out of the
         --  remaining components following the last such component.

         Prev_Comp := Empty;

         Comp := First_Component_Or_Discriminant (E);
         while Present (Comp) loop
            if Present (Component_Clause (Comp)) then
               if No (Prev_Comp)
                 or else
                   Component_Bit_Offset (Comp) >
                   Component_Bit_Offset (Prev_Comp)
               then
                  Prev_Comp := Comp;
               end if;
            end if;

            Next_Component_Or_Discriminant (Comp);
         end loop;

         --  We have two separate circuits, one for non-variant records and
         --  one for variant records. For non-variant records, we simply go
         --  through the list of components. This handles all the non-variant
         --  cases including those cases of subtypes where there is no full
         --  type declaration, so the tree cannot be used to drive the layout.
         --  For variant records, we have to drive the layout from the tree
         --  since we need to understand the variant structure in this case.

         if Present (Full_View (E)) then
            Decl := Declaration_Node (Full_View (E));
         else
            Decl := Declaration_Node (E);
         end if;

         --  Scan all the components

         if Nkind (Decl) = N_Full_Type_Declaration
           and then Has_Discriminants (E)
           and then Nkind (Type_Definition (Decl)) = N_Record_Definition
           and then Present (Component_List (Type_Definition (Decl)))
           and then
             Present (Variant_Part (Component_List (Type_Definition (Decl))))
         then
            Layout_Variant_Record;
         else
            Layout_Non_Variant_Record;
         end if;
      end if;
   end Layout_Record_Type;

   -----------------
   -- Layout_Type --
   -----------------

   procedure Layout_Type (E : Entity_Id) is
      Desig_Type : Entity_Id;

   begin
      --  For string literal types, for now, kill the size always, this is
      --  because gigi does not like or need the size to be set ???

      if Ekind (E) = E_String_Literal_Subtype then
         Set_Esize (E, Uint_0);
         Set_RM_Size (E, Uint_0);
         return;
      end if;

      --  For access types, set size/alignment. This is system address size,
      --  except for fat pointers (unconstrained array access types), where the
      --  size is two times the address size, to accommodate the two pointers
      --  that are required for a fat pointer (data and template). Note that
      --  E_Access_Protected_Subprogram_Type is not an access type for this
      --  purpose since it is not a pointer but is equivalent to a record. For
      --  access subtypes, copy the size from the base type since Gigi
      --  represents them the same way.

      if Is_Access_Type (E) then
         Desig_Type := Underlying_Type (Designated_Type (E));

         --  If we only have a limited view of the type, see whether the
         --  non-limited view is available.

         if From_Limited_With (Designated_Type (E))
           and then Ekind (Designated_Type (E)) = E_Incomplete_Type
           and then Present (Non_Limited_View (Designated_Type (E)))
         then
            Desig_Type := Non_Limited_View (Designated_Type (E));
         end if;

         --  If Esize already set (e.g. by a size clause), then nothing further
         --  to be done here.

         if Known_Esize (E) then
            null;

         --  Access to subprogram is a strange beast, and we let the backend
         --  figure out what is needed (it may be some kind of fat pointer,
         --  including the static link for example.

         elsif Is_Access_Protected_Subprogram_Type (E) then
            null;

         --  For access subtypes, copy the size information from base type

         elsif Ekind (E) = E_Access_Subtype then
            Set_Size_Info (E, Base_Type (E));
            Set_RM_Size   (E, RM_Size (Base_Type (E)));

         --  For other access types, we use either address size, or, if a fat
         --  pointer is used (pointer-to-unconstrained array case), twice the
         --  address size to accommodate a fat pointer.

         elsif Present (Desig_Type)
           and then Is_Array_Type (Desig_Type)
           and then not Is_Constrained (Desig_Type)
           and then not Has_Completion_In_Body (Desig_Type)

           --  Debug Flag -gnatd6 says make all pointers to unconstrained thin

           and then not Debug_Flag_6
         then
            Init_Size (E, 2 * System_Address_Size);

            --  Check for bad convention set

            if Warn_On_Export_Import
              and then
                (Convention (E) = Convention_C
                   or else
                 Convention (E) = Convention_CPP)
            then
               Error_Msg_N
                 ("?x?this access type does not correspond to C pointer", E);
            end if;

         --  If the designated type is a limited view it is unanalyzed. We can
         --  examine the declaration itself to determine whether it will need a
         --  fat pointer.

         elsif Present (Desig_Type)
           and then Present (Parent (Desig_Type))
           and then Nkind (Parent (Desig_Type)) = N_Full_Type_Declaration
           and then Nkind (Type_Definition (Parent (Desig_Type))) =
                                             N_Unconstrained_Array_Definition
           and then not Debug_Flag_6
         then
            Init_Size (E, 2 * System_Address_Size);

         --  When the target is AAMP, access-to-subprogram types are fat
         --  pointers consisting of the subprogram address and a static link,
         --  with the exception of library-level access types (including
         --  library-level anonymous access types, such as for components),
         --  where a simple subprogram address is used.

         elsif AAMP_On_Target
           and then
             ((Ekind (E) = E_Access_Subprogram_Type
                and then Present (Enclosing_Subprogram (E)))
               or else
                 (Ekind (E) = E_Anonymous_Access_Subprogram_Type
                   and then
                     (not Is_Local_Anonymous_Access (E)
                       or else Present (Enclosing_Subprogram (E)))))
         then
            Init_Size (E, 2 * System_Address_Size);

         --  Normal case of thin pointer

         else
            Init_Size (E, System_Address_Size);
         end if;

         Set_Elem_Alignment (E);

      --  Scalar types: set size and alignment

      elsif Is_Scalar_Type (E) then

         --  For discrete types, the RM_Size and Esize must be set already,
         --  since this is part of the earlier processing and the front end is
         --  always required to lay out the sizes of such types (since they are
         --  available as static attributes). All we do is to check that this
         --  rule is indeed obeyed.

         if Is_Discrete_Type (E) then

            --  If the RM_Size is not set, then here is where we set it

            --  Note: an RM_Size of zero looks like not set here, but this
            --  is a rare case, and we can simply reset it without any harm.

            if not Known_RM_Size (E) then
               Set_Discrete_RM_Size (E);
            end if;

            --  If Esize for a discrete type is not set then set it

            if not Known_Esize (E) then
               declare
                  S : Int := 8;

               begin
                  loop
                     --  If size is big enough, set it and exit

                     if S >= RM_Size (E) then
                        Init_Esize (E, S);
                        exit;

                     --  If the RM_Size is greater than 64 (happens only when
                     --  strange values are specified by the user, then Esize
                     --  is simply a copy of RM_Size, it will be further
                     --  refined later on)

                     elsif S = 64 then
                        Set_Esize (E, RM_Size (E));
                        exit;

                     --  Otherwise double possible size and keep trying

                     else
                        S := S * 2;
                     end if;
                  end loop;
               end;
            end if;

         --  For non-discrete scalar types, if the RM_Size is not set, then set
         --  it now to a copy of the Esize if the Esize is set.

         else
            if Known_Esize (E) and then Unknown_RM_Size (E) then
               Set_RM_Size (E, Esize (E));
            end if;
         end if;

         Set_Elem_Alignment (E);

      --  Non-elementary (composite) types

      else
         --  For packed arrays, take size and alignment values from the packed
         --  array type if a packed array type has been created and the fields
         --  are not currently set.

         if Is_Array_Type (E)
           and then Present (Packed_Array_Impl_Type (E))
         then
            declare
               PAT : constant Entity_Id := Packed_Array_Impl_Type (E);

            begin
               if Unknown_Esize (E) then
                  Set_Esize     (E, Esize     (PAT));
               end if;

               if Unknown_RM_Size (E) then
                  Set_RM_Size   (E, RM_Size   (PAT));
               end if;

               if Unknown_Alignment (E) then
                  Set_Alignment (E, Alignment (PAT));
               end if;
            end;
         end if;

         --  If Esize is set, and RM_Size is not, RM_Size is copied from Esize.
         --  At least for now this seems reasonable, and is in any case needed
         --  for compatibility with old versions of gigi.

         if Known_Esize (E) and then Unknown_RM_Size (E) then
            Set_RM_Size (E, Esize (E));
         end if;

         --  For array base types, set component size if object size of the
         --  component type is known and is a small power of 2 (8, 16, 32, 64),
         --  since this is what will always be used.

         if Ekind (E) = E_Array_Type and then Unknown_Component_Size (E) then
            declare
               CT : constant Entity_Id := Component_Type (E);

            begin
               --  For some reason, access types can cause trouble, So let's
               --  just do this for scalar types ???

               if Present (CT)
                 and then Is_Scalar_Type (CT)
                 and then Known_Static_Esize (CT)
               then
                  declare
                     S : constant Uint := Esize (CT);
                  begin
                     if Addressable (S) then
                        Set_Component_Size (E, S);
                     end if;
                  end;
               end if;
            end;
         end if;
      end if;

      --  Lay out array and record types if front end layout set

      if Frontend_Layout_On_Target then
         if Is_Array_Type (E) and then not Is_Bit_Packed_Array (E) then
            Layout_Array_Type (E);
         elsif Is_Record_Type (E) then
            Layout_Record_Type (E);
         end if;

      --  Case of backend layout, we still do a little in the front end

      else
         --  Processing for record types

         if Is_Record_Type (E) then

            --  Special remaining processing for record types with a known
            --  size of 16, 32, or 64 bits whose alignment is not yet set.
            --  For these types, we set a corresponding alignment matching
            --  the size if possible, or as large as possible if not.

            if Convention (E) = Convention_Ada and then not Debug_Flag_Q then
               Set_Composite_Alignment (E);
            end if;

         --  Processing for array types

         elsif Is_Array_Type (E) then

            --  For arrays that are required to be atomic/VFA, we do the same
            --  processing as described above for short records, since we
            --  really need to have the alignment set for the whole array.

            if Is_Atomic_Or_VFA (E) and then not Debug_Flag_Q then
               Set_Composite_Alignment (E);
            end if;

            --  For unpacked array types, set an alignment of 1 if we know
            --  that the component alignment is not greater than 1. The reason
            --  we do this is to avoid unnecessary copying of slices of such
            --  arrays when passed to subprogram parameters (see special test
            --  in Exp_Ch6.Expand_Actuals).

            if not Is_Packed (E) and then Unknown_Alignment (E) then
               if Known_Static_Component_Size (E)
                 and then Component_Size (E) = 1
               then
                  Set_Alignment (E, Uint_1);
               end if;
            end if;

            --  We need to know whether the size depends on the value of one
            --  or more discriminants to select the return mechanism. Skip if
            --  errors are present, to prevent cascaded messages.

            if Serious_Errors_Detected = 0 then
               Compute_Size_Depends_On_Discriminant (E);
            end if;

         end if;
      end if;

      --  Final step is to check that Esize and RM_Size are compatible

      if Known_Static_Esize (E) and then Known_Static_RM_Size (E) then
         if Esize (E) < RM_Size (E) then

            --  Esize is less than RM_Size. That's not good. First we test
            --  whether this was set deliberately with an Object_Size clause
            --  and if so, object to the clause.

            if Has_Object_Size_Clause (E) then
               Error_Msg_Uint_1 := RM_Size (E);
               Error_Msg_F
                 ("object size is too small, minimum allowed is ^",
                  Expression (Get_Attribute_Definition_Clause
                                             (E, Attribute_Object_Size)));
            end if;

            --  Adjust Esize up to RM_Size value

            declare
               Size : constant Uint := RM_Size (E);

            begin
               Set_Esize (E, RM_Size (E));

               --  For scalar types, increase Object_Size to power of 2, but
               --  not less than a storage unit in any case (i.e., normally
               --  this means it will be storage-unit addressable).

               if Is_Scalar_Type (E) then
                  if Size <= System_Storage_Unit then
                     Init_Esize (E, System_Storage_Unit);
                  elsif Size <= 16 then
                     Init_Esize (E, 16);
                  elsif Size <= 32 then
                     Init_Esize (E, 32);
                  else
                     Set_Esize  (E, (Size + 63) / 64 * 64);
                  end if;

                  --  Finally, make sure that alignment is consistent with
                  --  the newly assigned size.

                  while Alignment (E) * System_Storage_Unit < Esize (E)
                    and then Alignment (E) < Maximum_Alignment
                  loop
                     Set_Alignment (E, 2 * Alignment (E));
                  end loop;
               end if;
            end;
         end if;
      end if;
   end Layout_Type;

   ---------------------
   -- Rewrite_Integer --
   ---------------------

   procedure Rewrite_Integer (N : Node_Id; V : Uint) is
      Loc : constant Source_Ptr := Sloc (N);
      Typ : constant Entity_Id  := Etype (N);
   begin
      Rewrite (N, Make_Integer_Literal (Loc, Intval => V));
      Set_Etype (N, Typ);
   end Rewrite_Integer;

   -------------------------------
   -- Set_And_Check_Static_Size --
   -------------------------------

   procedure Set_And_Check_Static_Size
     (E      : Entity_Id;
      Esiz   : SO_Ref;
      RM_Siz : SO_Ref)
   is
      SC : Node_Id;

      procedure Check_Size_Too_Small (Spec : Uint; Min : Uint);
      --  Spec is the number of bit specified in the size clause, and Min is
      --  the minimum computed size. An error is given that the specified size
      --  is too small if Spec < Min, and in this case both Esize and RM_Size
      --  are set to unknown in E. The error message is posted on node SC.

      procedure Check_Unused_Bits (Spec : Uint; Max : Uint);
      --  Spec is the number of bits specified in the size clause, and Max is
      --  the maximum computed size. A warning is given about unused bits if
      --  Spec > Max. This warning is posted on node SC.

      --------------------------
      -- Check_Size_Too_Small --
      --------------------------

      procedure Check_Size_Too_Small (Spec : Uint; Min : Uint) is
      begin
         if Spec < Min then
            Error_Msg_Uint_1 := Min;
            Error_Msg_NE ("size for & too small, minimum allowed is ^", SC, E);
            Init_Esize   (E);
            Init_RM_Size (E);
         end if;
      end Check_Size_Too_Small;

      -----------------------
      -- Check_Unused_Bits --
      -----------------------

      procedure Check_Unused_Bits (Spec : Uint; Max : Uint) is
      begin
         if Spec > Max then
            Error_Msg_Uint_1 := Spec - Max;
            Error_Msg_NE ("??^ bits of & unused", SC, E);
         end if;
      end Check_Unused_Bits;

   --  Start of processing for Set_And_Check_Static_Size

   begin
      --  Case where Object_Size (Esize) is already set by a size clause

      if Known_Static_Esize (E) then
         SC := Size_Clause (E);

         if No (SC) then
            SC := Get_Attribute_Definition_Clause (E, Attribute_Object_Size);
         end if;

         --  Perform checks on specified size against computed sizes

         if Present (SC) then
            Check_Unused_Bits    (Esize (E), Esiz);
            Check_Size_Too_Small (Esize (E), RM_Siz);
         end if;
      end if;

      --  Case where Value_Size (RM_Size) is set by specific Value_Size clause
      --  (we do not need to worry about Value_Size being set by a Size clause,
      --  since that will have set Esize as well, and we already took care of
      --  that case).

      if Known_Static_RM_Size (E) then
         SC := Get_Attribute_Definition_Clause (E, Attribute_Value_Size);

         --  Perform checks on specified size against computed sizes

         if Present (SC) then
            Check_Unused_Bits    (RM_Size (E), Esiz);
            Check_Size_Too_Small (RM_Size (E), RM_Siz);
         end if;
      end if;

      --  Set sizes if unknown

      if Unknown_Esize (E) then
         Set_Esize (E, Esiz);
      end if;

      if Unknown_RM_Size (E) then
         Set_RM_Size (E, RM_Siz);
      end if;
   end Set_And_Check_Static_Size;

   -----------------------------
   -- Set_Composite_Alignment --
   -----------------------------

   procedure Set_Composite_Alignment (E : Entity_Id) is
      Siz   : Uint;
      Align : Nat;

   begin
      --  If alignment is already set, then nothing to do

      if Known_Alignment (E) then
         return;
      end if;

      --  Alignment is not known, see if we can set it, taking into account
      --  the setting of the Optimize_Alignment mode.

      --  If Optimize_Alignment is set to Space, then we try to give packed
      --  records an aligmment of 1, unless there is some reason we can't.

      if Optimize_Alignment_Space (E)
        and then Is_Record_Type (E)
        and then Is_Packed (E)
      then
         --  No effect for record with atomic/VFA components

         if Is_Atomic_Or_VFA (E) then
            Error_Msg_N ("Optimize_Alignment has no effect for &??", E);

            if Is_Atomic (E) then
               Error_Msg_N
                 ("\pragma ignored for atomic record??", E);
            else
               Error_Msg_N
                 ("\pragma ignored for bolatile full access record??", E);
            end if;

            return;
         end if;

         --  No effect if independent components

         if Has_Independent_Components (E) then
            Error_Msg_N ("Optimize_Alignment has no effect for &??", E);
            Error_Msg_N
              ("\pragma ignored for record with independent components??", E);
            return;
         end if;

         --  No effect if any component is atomic/VFA or is a by-reference type

         declare
            Ent : Entity_Id;

         begin
            Ent := First_Component_Or_Discriminant (E);
            while Present (Ent) loop
               if Is_By_Reference_Type (Etype (Ent))
                 or else Is_Atomic_Or_VFA (Etype (Ent))
                 or else Is_Atomic_Or_VFA (Ent)
               then
                  Error_Msg_N ("Optimize_Alignment has no effect for &??", E);

                  if Is_Atomic (Etype (Ent)) or else Is_Atomic (Ent) then
                     Error_Msg_N
                       ("\pragma is ignored if atomic "
                        & "components present??", E);
                  else
                     Error_Msg_N
                       ("\pragma is ignored if bolatile full access "
                        & "components present??", E);
                  end if;

                  return;
               else
                  Next_Component_Or_Discriminant (Ent);
               end if;
            end loop;
         end;

         --  Optimize_Alignment has no effect on variable length record

         if not Size_Known_At_Compile_Time (E) then
            Error_Msg_N ("Optimize_Alignment has no effect for &??", E);
            Error_Msg_N ("\pragma is ignored for variable length record??", E);
            return;
         end if;

         --  All tests passed, we can set alignment to 1

         Align := 1;

      --  Not a record, or not packed

      else
         --  The only other cases we worry about here are where the size is
         --  statically known at compile time.

         if Known_Static_Esize (E) then
            Siz := Esize (E);
         elsif Unknown_Esize (E) and then Known_Static_RM_Size (E) then
            Siz := RM_Size (E);
         else
            return;
         end if;

         --  Size is known, alignment is not set

         --  Reset alignment to match size if the known size is exactly 2, 4,
         --  or 8 storage units.

         if Siz = 2 * System_Storage_Unit then
            Align := 2;
         elsif Siz = 4 * System_Storage_Unit then
            Align := 4;
         elsif Siz = 8 * System_Storage_Unit then
            Align := 8;

            --  If Optimize_Alignment is set to Space, then make sure the
            --  alignment matches the size, for example, if the size is 17
            --  bytes then we want an alignment of 1 for the type.

         elsif Optimize_Alignment_Space (E) then
            if Siz mod (8 * System_Storage_Unit) = 0 then
               Align := 8;
            elsif Siz mod (4 * System_Storage_Unit) = 0 then
               Align := 4;
            elsif Siz mod (2 * System_Storage_Unit) = 0 then
               Align := 2;
            else
               Align := 1;
            end if;

            --  If Optimize_Alignment is set to Time, then we reset for odd
            --  "in between sizes", for example a 17 bit record is given an
            --  alignment of 4.

         elsif Optimize_Alignment_Time (E)
           and then Siz > System_Storage_Unit
           and then Siz <= 8 * System_Storage_Unit
         then
            if Siz <= 2 * System_Storage_Unit then
               Align := 2;
            elsif Siz <= 4 * System_Storage_Unit then
               Align := 4;
            else -- Siz <= 8 * System_Storage_Unit then
               Align := 8;
            end if;

            --  No special alignment fiddling needed

         else
            return;
         end if;
      end if;

      --  Here we have Set Align to the proposed improved value. Make sure the
      --  value set does not exceed Maximum_Alignment for the target.

      if Align > Maximum_Alignment then
         Align := Maximum_Alignment;
      end if;

      --  Further processing for record types only to reduce the alignment
      --  set by the above processing in some specific cases. We do not
      --  do this for atomic/VFA records, since we need max alignment there,

      if Is_Record_Type (E) and then not Is_Atomic_Or_VFA (E) then

         --  For records, there is generally no point in setting alignment
         --  higher than word size since we cannot do better than move by
         --  words in any case. Omit this if we are optimizing for time,
         --  since conceivably we may be able to do better.

         if Align > System_Word_Size / System_Storage_Unit
           and then not Optimize_Alignment_Time (E)
         then
            Align := System_Word_Size / System_Storage_Unit;
         end if;

         --  Check components. If any component requires a higher alignment,
         --  then we set that higher alignment in any case. Don't do this if
         --  we have Optimize_Alignment set to Space. Note that that covers
         --  the case of packed records, where we already set alignment to 1.

         if not Optimize_Alignment_Space (E) then
            declare
               Comp : Entity_Id;

            begin
               Comp := First_Component (E);
               while Present (Comp) loop
                  if Known_Alignment (Etype (Comp)) then
                     declare
                        Calign : constant Uint := Alignment (Etype (Comp));

                     begin
                        --  The cases to process are when the alignment of the
                        --  component type is larger than the alignment we have
                        --  so far, and either there is no component clause for
                        --  the component, or the length set by the component
                        --  clause matches the length of the component type.

                        if Calign > Align
                          and then
                            (Unknown_Esize (Comp)
                              or else (Known_Static_Esize (Comp)
                                        and then
                                          Esize (Comp) =
                                              Calign * System_Storage_Unit))
                        then
                           Align := UI_To_Int (Calign);
                        end if;
                     end;
                  end if;

                  Next_Component (Comp);
               end loop;
            end;
         end if;
      end if;

      --  Set chosen alignment, and increase Esize if necessary to match the
      --  chosen alignment.

      Set_Alignment (E, UI_From_Int (Align));

      if Known_Static_Esize (E)
        and then Esize (E) < Align * System_Storage_Unit
      then
         Set_Esize (E, UI_From_Int (Align * System_Storage_Unit));
      end if;
   end Set_Composite_Alignment;

   --------------------------
   -- Set_Discrete_RM_Size --
   --------------------------

   procedure Set_Discrete_RM_Size (Def_Id : Entity_Id) is
      FST : constant Entity_Id := First_Subtype (Def_Id);

   begin
      --  All discrete types except for the base types in standard are
      --  constrained, so indicate this by setting Is_Constrained.

      Set_Is_Constrained (Def_Id);

      --  Set generic types to have an unknown size, since the representation
      --  of a generic type is irrelevant, in view of the fact that they have
      --  nothing to do with code.

      if Is_Generic_Type (Root_Type (FST)) then
         Set_RM_Size (Def_Id, Uint_0);

      --  If the subtype statically matches the first subtype, then it is
      --  required to have exactly the same layout. This is required by
      --  aliasing considerations.

      elsif Def_Id /= FST and then
        Subtypes_Statically_Match (Def_Id, FST)
      then
         Set_RM_Size   (Def_Id, RM_Size (FST));
         Set_Size_Info (Def_Id, FST);

      --  In all other cases the RM_Size is set to the minimum size. Note that
      --  this routine is never called for subtypes for which the RM_Size is
      --  set explicitly by an attribute clause.

      else
         Set_RM_Size (Def_Id, UI_From_Int (Minimum_Size (Def_Id)));
      end if;
   end Set_Discrete_RM_Size;

   ------------------------
   -- Set_Elem_Alignment --
   ------------------------

   procedure Set_Elem_Alignment (E : Entity_Id) is
   begin
      --  Do not set alignment for packed array types, unless we are doing
      --  front end layout, because otherwise this is always handled in the
      --  backend.

      if Is_Packed_Array_Impl_Type (E)
        and then not Frontend_Layout_On_Target
      then
         return;

      --  If there is an alignment clause, then we respect it

      elsif Has_Alignment_Clause (E) then
         return;

      --  If the size is not set, then don't attempt to set the alignment. This
      --  happens in the backend layout case for access-to-subprogram types.

      elsif not Known_Static_Esize (E) then
         return;

      --  For access types, do not set the alignment if the size is less than
      --  the allowed minimum size. This avoids cascaded error messages.

      elsif Is_Access_Type (E) and then Esize (E) < System_Address_Size then
         return;
      end if;

      --  Here we calculate the alignment as the largest power of two multiple
      --  of System.Storage_Unit that does not exceed either the object size of
      --  the type, or the maximum allowed alignment.

      declare
         S : Int;
         A : Nat;

         Max_Alignment : Nat;

      begin
         --  The given Esize may be larger that int'last because of a previous
         --  error, and the call to UI_To_Int will fail, so use default.

         if Esize (E) / SSU > Ttypes.Maximum_Alignment then
            S := Ttypes.Maximum_Alignment;

         --  If this is an access type and the target doesn't have strict
         --  alignment and we are not doing front end layout, then cap the
         --  alignment to that of a regular access type. This will avoid
         --  giving fat pointers twice the usual alignment for no practical
         --  benefit since the misalignment doesn't really matter.

         elsif Is_Access_Type (E)
           and then not Target_Strict_Alignment
           and then not Frontend_Layout_On_Target
         then
            S := System_Address_Size / SSU;

         else
            S := UI_To_Int (Esize (E)) / SSU;
         end if;

         --  If the default alignment of "double" floating-point types is
         --  specifically capped, enforce the cap.

         if Ttypes.Target_Double_Float_Alignment > 0
           and then S = 8
           and then Is_Floating_Point_Type (E)
         then
            Max_Alignment := Ttypes.Target_Double_Float_Alignment;

         --  If the default alignment of "double" or larger scalar types is
         --  specifically capped, enforce the cap.

         elsif Ttypes.Target_Double_Scalar_Alignment > 0
           and then S >= 8
           and then Is_Scalar_Type (E)
         then
            Max_Alignment := Ttypes.Target_Double_Scalar_Alignment;

         --  Otherwise enforce the overall alignment cap

         else
            Max_Alignment := Ttypes.Maximum_Alignment;
         end if;

         A := 1;
         while 2 * A <= Max_Alignment and then 2 * A <= S loop
            A := 2 * A;
         end loop;

         --  If alignment is currently not set, then we can safely set it to
         --  this new calculated value.

         if Unknown_Alignment (E) then
            Init_Alignment (E, A);

         --  Cases where we have inherited an alignment

         --  For constructed types, always reset the alignment, these are
         --  generally invisible to the user anyway, and that way we are
         --  sure that no constructed types have weird alignments.

         elsif not Comes_From_Source (E) then
            Init_Alignment (E, A);

         --  If this inherited alignment is the same as the one we computed,
         --  then obviously everything is fine, and we do not need to reset it.

         elsif Alignment (E) = A then
            null;

         --  Now we come to the difficult cases where we have inherited an
         --  alignment and size, but overridden the size but not the alignment.

         elsif Has_Size_Clause (E) or else Has_Object_Size_Clause (E) then

            --  This is tricky, it might be thought that we should try to
            --  inherit the alignment, since that's what the RM implies, but
            --  that leads to complex rules and oddities. Consider for example:

            --    type R is new Character;
            --    for R'Size use 16;

            --  It seems quite bogus in this case to inherit an alignment of 1
            --  from the parent type Character. Furthermore, if that's what the
            --  programmer really wanted for some odd reason, then he could
            --  specify the alignment directly.

            --  Furthermore we really don't want to inherit the alignment in
            --  the case of a specified Object_Size for a subtype, since then
            --  there would be no way of overriding to give a reasonable value
            --  (we don't have an Object_Subtype attribute). Consider:

            --    subtype R is Character;
            --    for R'Object_Size use 16;

            --  If we inherit the alignment of 1, then we have an inefficient
            --  alignment for the subtype, which cannot be fixed.

            --  So we make the decision that if Size (or Object_Size) is given
            --  (and, in the case of a first subtype, the alignment is not set
            --  with a specific alignment clause), we reset the alignment to
            --  the appropriate value for the specified size. This is a nice
            --  simple rule to implement and document.

            --  There is one slight glitch, which is that a confirming size
            --  clause can now change the alignment, which, if we really think
            --  that confirming rep clauses should have no effect, is a no-no.

            --    type R is new Character;
            --    for R'Alignment use 2;
            --    type S is new R;
            --    for S'Size use Character'Size;

            --  Now the alignment of S is changed to 1 instead of 2 as a result
            --  of applying the above rule to the confirming rep clause for S.
            --  Not clear this is worth worrying about. If we recorded whether
            --  a size clause was confirming we could avoid this, but right now
            --  we have no way of doing that or easily figuring it out, so we
            --  don't bother.

            --  Historical note: in versions of GNAT prior to Nov 6th, 2011, an
            --  odd distinction was made between inherited alignments larger
            --  than the computed alignment (where the larger alignment was
            --  inherited) and inherited alignments smaller than the computed
            --  alignment (where the smaller alignment was overridden). This
            --  was a dubious fix to get around an ACATS problem which seems
            --  to have disappeared anyway, and in any case, this peculiarity
            --  was never documented.

            Init_Alignment (E, A);

         --  If no Size (or Object_Size) was specified, then we inherited the
         --  object size, so we should inherit the alignment as well and not
         --  modify it. This takes care of cases like:

         --    type R is new Integer;
         --    for R'Alignment use 1;
         --    subtype S is R;

         --  Here we have R with a default Object_Size of 32, and a specified
         --  alignment of 1, and it seeems right for S to inherit both values.

         else
            null;
         end if;
      end;
   end Set_Elem_Alignment;

   ----------------------
   -- SO_Ref_From_Expr --
   ----------------------

   function SO_Ref_From_Expr
     (Expr      : Node_Id;
      Ins_Type  : Entity_Id;
      Vtype     : Entity_Id := Empty;
      Make_Func : Boolean   := False) return Dynamic_SO_Ref
   is
      Loc  : constant Source_Ptr := Sloc (Ins_Type);
      K    : constant Entity_Id := Make_Temporary (Loc, 'K');
      Decl : Node_Id;

      Vtype_Primary_View : Entity_Id;

      function Check_Node_V_Ref (N : Node_Id) return Traverse_Result;
      --  Function used to check one node for reference to V

      function Has_V_Ref is new Traverse_Func (Check_Node_V_Ref);
      --  Function used to traverse tree to check for reference to V

      ----------------------
      -- Check_Node_V_Ref --
      ----------------------

      function Check_Node_V_Ref (N : Node_Id) return Traverse_Result is
      begin
         if Nkind (N) = N_Identifier then
            if Chars (N) = Vname then
               return Abandon;
            else
               return Skip;
            end if;

         else
            return OK;
         end if;
      end Check_Node_V_Ref;

   --  Start of processing for SO_Ref_From_Expr

   begin
      --  Case of expression is an integer literal, in this case we just
      --  return the value (which must always be non-negative, since size
      --  and offset values can never be negative).

      if Nkind (Expr) = N_Integer_Literal then
         pragma Assert (Intval (Expr) >= 0);
         return Intval (Expr);
      end if;

      --  Case where there is a reference to V, create function

      if Has_V_Ref (Expr) = Abandon then

         pragma Assert (Present (Vtype));

         --  Check whether Vtype is a view of a private type and ensure that
         --  we use the primary view of the type (which is denoted by its
         --  Etype, whether it's the type's partial or full view entity).
         --  This is needed to make sure that we use the same (primary) view
         --  of the type for all V formals, whether the current view of the
         --  type is the partial or full view, so that types will always
         --  match on calls from one size function to another.

         if Has_Private_Declaration (Vtype) then
            Vtype_Primary_View := Etype (Vtype);
         else
            Vtype_Primary_View := Vtype;
         end if;

         Set_Is_Discrim_SO_Function (K);

         Decl :=
           Make_Subprogram_Body (Loc,

             Specification =>
               Make_Function_Specification (Loc,
                 Defining_Unit_Name => K,
                   Parameter_Specifications => New_List (
                     Make_Parameter_Specification (Loc,
                       Defining_Identifier =>
                         Make_Defining_Identifier (Loc, Chars => Vname),
                       Parameter_Type      =>
                         New_Occurrence_Of (Vtype_Primary_View, Loc))),
                   Result_Definition =>
                     New_Occurrence_Of (Standard_Unsigned, Loc)),

             Declarations => Empty_List,

             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                 Statements => New_List (
                   Make_Simple_Return_Statement (Loc,
                     Expression => Expr))));

      --  The caller requests that the expression be encapsulated in a
      --  parameterless function.

      elsif Make_Func then
         Decl :=
           Make_Subprogram_Body (Loc,

             Specification =>
               Make_Function_Specification (Loc,
                 Defining_Unit_Name => K,
                   Parameter_Specifications => Empty_List,
                   Result_Definition =>
                     New_Occurrence_Of (Standard_Unsigned, Loc)),

             Declarations => Empty_List,

             Handled_Statement_Sequence =>
               Make_Handled_Sequence_Of_Statements (Loc,
                 Statements => New_List (
                   Make_Simple_Return_Statement (Loc, Expression => Expr))));

      --  No reference to V and function not requested, so create a constant

      else
         Decl :=
           Make_Object_Declaration (Loc,
             Defining_Identifier => K,
             Object_Definition   =>
               New_Occurrence_Of (Standard_Unsigned, Loc),
             Constant_Present    => True,
             Expression          => Expr);
      end if;

      Append_Freeze_Action (Ins_Type, Decl);
      Analyze (Decl);
      return Create_Dynamic_SO_Ref (K);
   end SO_Ref_From_Expr;

end Layout;
