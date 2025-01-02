------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                               L A Y O U T                                --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 2001-2025, Free Software Foundation, Inc.         --
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
with Debug;          use Debug;
with Einfo;          use Einfo;
with Einfo.Entities; use Einfo.Entities;
with Einfo.Utils;    use Einfo.Utils;
with Errout;         use Errout;
with Opt;            use Opt;
with Sem_Aux;        use Sem_Aux;
with Sem_Ch13;       use Sem_Ch13;
with Sem_Eval;       use Sem_Eval;
with Sem_Util;       use Sem_Util;
with Sinfo;          use Sinfo;
with Sinfo.Nodes;    use Sinfo.Nodes;
with Sinfo.Utils;    use Sinfo.Utils;
with Snames;         use Snames;
with Ttypes;         use Ttypes;
with Uintp;          use Uintp;
with Warnsw;         use Warnsw;

package body Layout is

   ------------------------
   -- Local Declarations --
   ------------------------

   SSU : constant Int := Ttypes.System_Storage_Unit;
   --  Short hand for System_Storage_Unit

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Compute_Size_Depends_On_Discriminant (E : Entity_Id);
   --  Given an array type or an array subtype E, compute whether its size
   --  depends on the value of one or more discriminants and set the flag
   --  Size_Depends_On_Discriminant accordingly. This need not be called
   --  in front end layout mode since it does the computation on its own.

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

      if not Known_Esize (E) then
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

      if not Known_Alignment (E) then
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

         Set_Alignment (E, UI_From_Int (Abits / SSU));
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
      pragma Unreferenced (E);
   begin
      --  Nothing to do for now, assume backend does the layout

      return;
   end Layout_Object;

   -----------------
   -- Layout_Type --
   -----------------

   procedure Layout_Type (E : Entity_Id) is
      Desig_Type : Entity_Id;

   begin
      --  For string literal types, kill the size always, because gigi does not
      --  like or need the size to be set.

      if Ekind (E) = E_String_Literal_Subtype then
         Reinit_Esize (E);
         Reinit_RM_Size (E);
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

         --  If Esize already set (e.g. by a size or value size clause), then
         --  nothing further to be done here.

         if Known_Esize (E) then
            null;

         --  Access to protected subprogram is a strange beast, and we let the
         --  backend figure out what is needed (it may be some kind of fat
         --  pointer, including the static link for example).

         elsif Is_Access_Protected_Subprogram_Type (E) then
            null;

         --  For access subtypes, copy the size information from base type

         elsif Ekind (E) = E_Access_Subtype then
            Set_Size_Info (E, Base_Type (E));
            Copy_RM_Size (To => E, From => Base_Type (E));

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

         --  If unnesting subprograms, subprogram access types contain the
         --  address of both the subprogram and an activation record. But if we
         --  set that, we'll get a warning on different unchecked conversion
         --  sizes in the RTS. So leave unset in that case.

         elsif Unnest_Subprogram_Mode
           and then Is_Access_Subprogram_Type (E)
         then
            null;

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
                  S : Pos := 8;

               begin
                  loop
                     --  If size is big enough, set it and exit

                     if S >= RM_Size (E) then
                        Set_Esize (E, UI_From_Int (S));
                        exit;

                     --  If the RM_Size is greater than System_Max_Integer_Size
                     --  (happens only when strange values are specified by the
                     --  user), then Esize is simply a copy of RM_Size, it will
                     --  be further refined later on.

                     elsif S = System_Max_Integer_Size then
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
            if Known_Esize (E) and then not Known_RM_Size (E) then
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
               if not Known_Esize (E) then
                  Copy_Esize (To => E, From => PAT);
               end if;

               if not Known_RM_Size (E) then
                  Copy_RM_Size (To => E, From => PAT);
               end if;

               if not Known_Alignment (E) then
                  Copy_Alignment (To => E, From => PAT);
               end if;
            end;
         end if;

         --  For array base types, set the component size if object size of the
         --  component type is known and is a small power of 2 (8, 16, 32, 64
         --  or 128), since this is what will always be used, except if a very
         --  large alignment was specified and so Adjust_Esize_For_Alignment
         --  gave up because, in this case, the object size is not a multiple
         --  of the alignment and, therefore, cannot be the component size.

         if Ekind (E) = E_Array_Type and then not Known_Component_Size (E) then
            declare
               CT : constant Entity_Id := Component_Type (E);

            begin
               --  For some reason, access types can cause trouble, So let's
               --  just do this for scalar types.

               if Present (CT)
                 and then Is_Scalar_Type (CT)
                 and then Known_Static_Esize (CT)
                 and then not (Known_Alignment (CT)
                                and then Alignment_In_Bits (CT) >
                                           System_Max_Integer_Size)
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

         --  For non-packed arrays set the alignment of the array to the
         --  alignment of the component type if it is unknown. Skip this
         --  in full access case since a larger alignment may be needed.

         if Is_Array_Type (E)
           and then not Is_Packed (E)
           and then not Known_Alignment (E)
           and then Known_Alignment (Component_Type (E))
           and then Known_Static_Component_Size (E)
           and then Known_Static_Esize (Component_Type (E))
           and then Component_Size (E) = Esize (Component_Type (E))
           and then not Is_Full_Access (E)
         then
            Set_Alignment (E, Alignment (Component_Type (E)));
         end if;

         --  If packing was requested, the one-dimensional array is constrained
         --  with static bounds, the component size was set explicitly, and
         --  the alignment is known, we can set (if not set explicitly) the
         --  RM_Size and the Esize of the array type, as RM_Size is equal to
         --  (arr'length * arr'component_size) and Esize is the same value
         --  rounded to the next multiple of arr'alignment. This is not
         --  applicable to packed arrays that are implemented specially
         --  in GNAT, i.e. when Packed_Array_Impl_Type is set.

         if Is_Array_Type (E)
           and then Present (First_Index (E))  --  Skip types in error
           and then Number_Dimensions (E) = 1
           and then No (Packed_Array_Impl_Type (E))
           and then Has_Pragma_Pack (E)
           and then Is_Constrained (E)
           and then Compile_Time_Known_Bounds (E)
           and then Known_Component_Size (E)
           and then Known_Alignment (E)
         then
            declare
               Abits : constant Int := UI_To_Int (Alignment (E)) * SSU;
               Lo, Hi : Node_Id;
               Siz : Uint;

            begin
               Get_Index_Bounds (First_Index (E), Lo, Hi);

               --  Even if the bounds are known at compile time, they could
               --  have been replaced by an error node. Check each bound
               --  explicitly.

               if Compile_Time_Known_Value (Lo)
                 and then Compile_Time_Known_Value (Hi)
               then
                  Siz := (Expr_Value (Hi) - Expr_Value (Lo) + 1)
                    * Component_Size (E);

                  --  Do not overwrite a different value of 'Size specified
                  --  explicitly by the user. In that case, also do not set
                  --  Esize.

                  if not Known_RM_Size (E) or else RM_Size (E) = Siz then
                     Set_RM_Size (E, Siz);

                     if not Known_Esize (E) then
                        Siz := ((Siz + (Abits - 1)) / Abits) * Abits;
                        Set_Esize (E, Siz);
                     end if;
                  end if;
               end if;
            end;
         end if;
      end if;

      --  Even if the backend performs the layout, we still do a little in
      --  the front end

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

         --  For arrays that are required to be full access, we do the same
         --  processing as described above for short records, since we really
         --  need to have the alignment set for the whole array.

         if Is_Full_Access (E) and then not Debug_Flag_Q then
            Set_Composite_Alignment (E);
         end if;

         --  For unpacked array types, set an alignment of 1 if we know
         --  that the component alignment is not greater than 1. The reason
         --  we do this is to avoid unnecessary copying of slices of such
         --  arrays when passed to subprogram parameters (see special test
         --  in Exp_Ch6.Expand_Actuals).

         if not Is_Packed (E) and then not Known_Alignment (E) then
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
                  Expression (Object_Size_Clause (E)));

            end if;

            --  Adjust Esize up to RM_Size value

            declare
               Size : constant Uint := RM_Size (E);

            begin
               Set_Esize (E, Size);

               --  For scalar types, increase Object_Size to power of 2, but
               --  not less than a storage unit in any case (i.e., normally
               --  this means it will be storage-unit addressable).

               if Is_Scalar_Type (E) then
                  if Size <= SSU then
                     Set_Esize (E, UI_From_Int (SSU));
                  elsif Size <= 16 then
                     Set_Esize (E, Uint_16);
                  elsif Size <= 32 then
                     Set_Esize (E, Uint_32);
                  else
                     Set_Esize (E, (Size + 63) / 64 * 64);
                  end if;

                  --  Finally, make sure that alignment is consistent with
                  --  the newly assigned size.

                  while Alignment (E) * SSU < Esize (E)
                    and then Alignment (E) < Maximum_Alignment
                  loop
                     Set_Alignment (E, 2 * Alignment (E));
                  end loop;

               --  For the other types, apply standard adjustments

               else
                  Adjust_Esize_Alignment (E);
               end if;
            end;
         end if;
      end if;
   end Layout_Type;

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
         --  No effect for record with full access components

         if Is_Full_Access (E) then
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

         --  No effect if a component is full access or of a by-reference type

         declare
            Ent : Entity_Id;

         begin
            Ent := First_Component_Or_Discriminant (E);
            while Present (Ent) loop
               if Is_By_Reference_Type (Etype (Ent))
                 or else Is_Full_Access (Etype (Ent))
                 or else Is_Full_Access (Ent)
               then
                  Error_Msg_N ("Optimize_Alignment has no effect for &??", E);

                  if Is_Atomic (Etype (Ent)) or else Is_Atomic (Ent) then
                     Error_Msg_N
                       ("\pragma is ignored if atomic "
                        & "components present??", E);
                  else
                     Error_Msg_N
                       ("\pragma is ignored if volatile full access "
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
         elsif not Known_Esize (E) and then Known_Static_RM_Size (E) then
            Siz := RM_Size (E);
         else
            return;
         end if;

         --  Size is known, alignment is not set

         --  Reset alignment to match size if the known size is exactly 2, 4,
         --  or 8 storage units.

         if Siz = 2 * SSU then
            Align := 2;
         elsif Siz = 4 * SSU then
            Align := 4;
         elsif Siz = 8 * SSU then
            Align := 8;

            --  If Optimize_Alignment is set to Space, then make sure the
            --  alignment matches the size, for example, if the size is 17
            --  bytes then we want an alignment of 1 for the type.

         elsif Optimize_Alignment_Space (E) then
            if Siz mod (8 * SSU) = 0 then
               Align := 8;
            elsif Siz mod (4 * SSU) = 0 then
               Align := 4;
            elsif Siz mod (2 * SSU) = 0 then
               Align := 2;
            else
               Align := 1;
            end if;

            --  If Optimize_Alignment is set to Time, then we reset for odd
            --  "in between sizes", for example a 17 bit record is given an
            --  alignment of 4.

         elsif Optimize_Alignment_Time (E)
           and then Siz > SSU
           and then Siz <= 8 * SSU
         then
            if Siz <= 2 * SSU then
               Align := 2;
            elsif Siz <= 4 * SSU then
               Align := 4;
            else -- Siz <= 8 * SSU then
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
      --  do this for full access records, since we need max alignment there,

      if Is_Record_Type (E) and then not Is_Full_Access (E) then

         --  For records, there is generally no point in setting alignment
         --  higher than word size since we cannot do better than move by
         --  words in any case. Omit this if we are optimizing for time,
         --  since conceivably we may be able to do better.

         if Align > System_Word_Size / SSU
           and then not Optimize_Alignment_Time (E)
         then
            Align := System_Word_Size / SSU;
         end if;

         --  Check components. If any component requires a higher alignment,
         --  then we set that higher alignment in any case. Don't do this if we
         --  have Optimize_Alignment set to Space. Note that covers the case of
         --  packed records, where we already set alignment to 1.

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
                            (not Known_Esize (Comp)
                              or else (Known_Static_Esize (Comp)
                                        and then
                                       Esize (Comp) = Calign * SSU))
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
        and then Esize (E) < Align * SSU
      then
         Set_Esize (E, UI_From_Int (Align * SSU));
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
         Reinit_RM_Size (Def_Id);

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

   procedure Set_Elem_Alignment (E : Entity_Id; Align : Nat := 0) is
   begin
      --  Do not set alignment for packed array types, this is handled in the
      --  backend.

      if Is_Packed_Array_Impl_Type (E) then
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

      --  We attempt to set the alignment in all the other cases

      declare
         S : Int;
         A : Nat;
         M : Nat;

      begin
         --  The given Esize may be larger that int'last because of a previous
         --  error, and the call to UI_To_Int will fail, so use default.

         if Esize (E) / SSU > Ttypes.Maximum_Alignment then
            S := Ttypes.Maximum_Alignment;

         --  If this is an access type and the target doesn't have strict
         --  alignment, then cap the alignment to that of a regular access
         --  type. This will avoid giving fat pointers twice the usual
         --  alignment for no practical benefit since the misalignment doesn't
         --  really matter.

         elsif Is_Access_Type (E)
           and then not Target_Strict_Alignment
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
            M := Ttypes.Target_Double_Float_Alignment;

         --  If the default alignment of "double" or larger scalar types is
         --  specifically capped, enforce the cap.

         elsif Ttypes.Target_Double_Scalar_Alignment > 0
           and then S >= 8
           and then Is_Scalar_Type (E)
         then
            M := Ttypes.Target_Double_Scalar_Alignment;

         --  Otherwise enforce the overall alignment cap

         else
            M := Ttypes.Maximum_Alignment;
         end if;

         --  We calculate the alignment as the largest power-of-two multiple
         --  of System.Storage_Unit that does not exceed the object size of
         --  the type and the maximum allowed alignment, if none was specified.
         --  Otherwise we only cap it to the maximum allowed alignment.

         if Align = 0 then
            A := 1;
            while 2 * A <= S and then 2 * A <= M loop
               A := 2 * A;
            end loop;
         else
            A := Nat'Min (Align, M);
         end if;

         --  If alignment is currently not set, then we can safely set it to
         --  this new calculated value.

         if not Known_Alignment (E) then
            Set_Alignment (E, UI_From_Int (A));

         --  Cases where we have inherited an alignment

         --  For constructed types, always reset the alignment, these are
         --  generally invisible to the user anyway, and that way we are
         --  sure that no constructed types have weird alignments.

         elsif not Comes_From_Source (E) then
            Set_Alignment (E, UI_From_Int (A));

         --  If this inherited alignment is the same as the one we computed,
         --  then obviously everything is fine, and we do not need to reset it.

         elsif Alignment (E) = A then
            null;

         else
            --  Now we come to the difficult cases of subtypes for which we
            --  have inherited an alignment different from the computed one.
            --  We resort to the presence of alignment and size clauses to
            --  guide our choices. Note that they can generally be present
            --  only on the first subtype (except for Object_Size) and that
            --  we need to look at the Rep_Item chain to correctly handle
            --  derived types.

            declare
               function Has_Attribute_Clause
                 (E  : Entity_Id;
                  Id : Attribute_Id) return Boolean;
               --  Wrapper around Get_Attribute_Definition_Clause which tests
               --  for the presence of the specified attribute clause.

               --------------------------
               -- Has_Attribute_Clause --
               --------------------------

               function Has_Attribute_Clause
                 (E  : Entity_Id;
                  Id : Attribute_Id) return Boolean is
               begin
                  return Present (Get_Attribute_Definition_Clause (E, Id));
               end Has_Attribute_Clause;

               FST : Entity_Id;

            begin
               FST := First_Subtype (E);

               --  Deal with private types

               if Is_Private_Type (FST) then
                  FST := Full_View (FST);
               end if;

               --  If the alignment comes from a clause, then we respect it.
               --  Consider for example:

               --    type R is new Character;
               --    for R'Alignment use 1;
               --    for R'Size use 16;
               --    subtype S is R;

               --  Here R has a specified size of 16 and a specified alignment
               --  of 1, and it seems right for S to inherit both values.

               if Has_Attribute_Clause (FST, Attribute_Alignment) then
                  null;

               --  Now we come to the cases where we have inherited alignment
               --  and size, and overridden the size but not the alignment.

               elsif Has_Attribute_Clause (FST, Attribute_Size)
                 or else Has_Attribute_Clause (FST, Attribute_Object_Size)
                 or else Has_Attribute_Clause (E, Attribute_Object_Size)
               then
                  --  This is tricky, it might be thought that we should try to
                  --  inherit the alignment, since that's what the RM implies,
                  --  but that leads to complex rules and oddities. Consider
                  --  for example:

                  --    type R is new Character;
                  --    for R'Size use 16;

                  --  It seems quite bogus in this case to inherit an alignment
                  --  of 1 from the parent type Character. Furthermore, if that
                  --  is what the programmer really wanted for some odd reason,
                  --  then he could specify the alignment directly.

                  --  Moreover we really don't want to inherit the alignment in
                  --  the case of a specified Object_Size for a subtype, since
                  --  there would be no way of overriding to give a reasonable
                  --  value (as we don't have an Object_Alignment attribute).
                  --  Consider for example:

                  --    subtype R is Character;
                  --    for R'Object_Size use 16;

                  --  If we inherit the alignment of 1, then it will be very
                  --  inefficient for the subtype and this cannot be fixed.

                  --  So we make the decision that if Size (or Object_Size) is
                  --  given and the alignment is not specified with a clause,
                  --  we reset the alignment to the appropriate value for the
                  --  specified size. This is a nice simple rule to implement
                  --  and document.

                  --  There is a theoretical glitch, which is that a confirming
                  --  size clause could now change the alignment, which, if we
                  --  really think that confirming rep clauses should have no
                  --  effect, could be seen as a no-no. However that's already
                  --  implemented by Alignment_Check_For_Size_Change so we do
                  --  not change the philosophy here.

                  --  Historical note: in versions prior to Nov 6th, 2011, an
                  --  odd distinction was made between inherited alignments
                  --  larger than the computed alignment (where the larger
                  --  alignment was inherited) and inherited alignments smaller
                  --  than the computed alignment (where the smaller alignment
                  --  was overridden). This was a dubious fix to get around an
                  --  ACATS problem which seems to have disappeared anyway, and
                  --  in any case, this peculiarity was never documented.

                  Set_Alignment (E, UI_From_Int (A));

               --  If no Size (or Object_Size) was specified, then we have
               --  inherited the object size, so we should also inherit the
               --  alignment and not modify it.

               else
                  null;
               end if;
            end;
         end if;
      end;
   end Set_Elem_Alignment;

end Layout;
