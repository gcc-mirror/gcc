------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ C H 1 3                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--                            $Revision: 1.4 $
--                                                                          --
--          Copyright (C) 1992-2001, Free Software Foundation, Inc.         --
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

with Atree;    use Atree;
with Checks;   use Checks;
with Einfo;    use Einfo;
with Errout;   use Errout;
with Exp_Tss;  use Exp_Tss;
with Exp_Util; use Exp_Util;
with Hostparm; use Hostparm;
with Lib;      use Lib;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Eval; use Sem_Eval;
with Sem_Res;  use Sem_Res;
with Sem_Type; use Sem_Type;
with Sem_Util; use Sem_Util;
with Stand;    use Stand;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Table;
with Ttypes;   use Ttypes;
with Tbuild;   use Tbuild;
with Urealp;   use Urealp;

with GNAT.Heap_Sort_A; use GNAT.Heap_Sort_A;

package body Sem_Ch13 is

   SSU : constant Pos := System_Storage_Unit;
   --  Convenient short hand for commonly used constant

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Alignment_Check_For_Esize_Change (Typ : Entity_Id);
   --  This routine is called after setting the Esize of type entity Typ.
   --  The purpose is to deal with the situation where an aligment has been
   --  inherited from a derived type that is no longer appropriate for the
   --  new Esize value. In this case, we reset the Alignment to unknown.

   procedure Check_Component_Overlap (C1_Ent, C2_Ent : Entity_Id);
   --  Given two entities for record components or discriminants, checks
   --  if they hav overlapping component clauses and issues errors if so.

   function Get_Alignment_Value (Expr : Node_Id) return Uint;
   --  Given the expression for an alignment value, returns the corresponding
   --  Uint value. If the value is inappropriate, then error messages are
   --  posted as required, and a value of No_Uint is returned.

   function Is_Operational_Item (N : Node_Id) return Boolean;
   --  A specification for a stream attribute is allowed before the full
   --  type is declared, as explained in AI-00137 and the corrigendum.
   --  Attributes that do not specify a representation characteristic are
   --  operational attributes.

   procedure New_Stream_Function
     (N    : Node_Id;
      Ent  : Entity_Id;
      Subp : Entity_Id;
      Nam  : Name_Id);
   --  Create a function renaming of a given stream attribute to the
   --  designated subprogram and then in the tagged case, provide this as
   --  a primitive operation, or in the non-tagged case make an appropriate
   --  TSS entry. Used for Input. This is more properly an expansion activity
   --  than just semantics, but the presence of user-defined stream functions
   --  for limited types is a legality check, which is why this takes place
   --  here rather than in exp_ch13, where it was previously.

   procedure New_Stream_Procedure
     (N     : Node_Id;
      Ent   : Entity_Id;
      Subp  : Entity_Id;
      Nam   : Name_Id;
      Out_P : Boolean := False);
   --  Create a procedure renaming of a given stream attribute to the
   --  designated subprogram and then in the tagged case, provide this as
   --  a primitive operation, or in the non-tagged case make an appropriate
   --  TSS entry. Used for Read, Output, Write.

   procedure Check_Constant_Address_Clause (Expr : Node_Id; U_Ent : Entity_Id);
   --  Expr is an expression for an address clause. This procedure checks
   --  that the expression is constant, in the limited sense that it is safe
   --  to evaluate it at the point the object U_Ent is declared, rather than
   --  at the point of the address clause. The condition for this to be true
   --  is that the expression has no variables, no constants declared after
   --  U_Ent, and no calls to non-pure functions. If this condition is not
   --  met, then an appropriate error message is posted.

   procedure Warn_Overlay
     (Expr : Node_Id;
      Typ  : Entity_Id;
      Nam  : Node_Id);
   --  Expr is the expression for an address clause for entity Nam whose type
   --  is Typ. If Typ has a default initialization, check whether the address
   --  clause might overlay two entities, and emit a warning on the side effect
   --  that the initialization will cause.

   ----------------------------------------------
   -- Table for Validate_Unchecked_Conversions --
   ----------------------------------------------

   --  The following table collects unchecked conversions for validation.
   --  Entries are made by Validate_Unchecked_Conversion and then the
   --  call to Validate_Unchecked_Conversions does the actual error
   --  checking and posting of warnings. The reason for this delayed
   --  processing is to take advantage of back-annotations of size and
   --  alignment values peformed by the back end.

   type UC_Entry is record
      Enode  : Node_Id;   -- node used for posting warnings
      Source : Entity_Id; -- source type for unchecked conversion
      Target : Entity_Id; -- target type for unchecked conversion
   end record;

   package Unchecked_Conversions is new Table.Table (
     Table_Component_Type => UC_Entry,
     Table_Index_Type     => Int,
     Table_Low_Bound      => 1,
     Table_Initial        => 50,
     Table_Increment      => 200,
     Table_Name           => "Unchecked_Conversions");

   --------------------------------------
   -- Alignment_Check_For_Esize_Change --
   --------------------------------------

   procedure Alignment_Check_For_Esize_Change (Typ : Entity_Id) is
   begin
      --  If the alignment is known, and not set by a rep clause, and is
      --  inconsistent with the size being set, then reset it to unknown,
      --  we assume in this case that the size overrides the inherited
      --  alignment, and that the alignment must be recomputed.

      if Known_Alignment (Typ)
        and then not Has_Alignment_Clause (Typ)
        and then Esize (Typ) mod (Alignment (Typ) * SSU) /= 0
      then
         Init_Alignment (Typ);
      end if;
   end Alignment_Check_For_Esize_Change;

   -----------------------
   -- Analyze_At_Clause --
   -----------------------

   --  An at clause is replaced by the corresponding Address attribute
   --  definition clause that is the preferred approach in Ada 95.

   procedure Analyze_At_Clause (N : Node_Id) is
   begin
      Rewrite (N,
        Make_Attribute_Definition_Clause (Sloc (N),
          Name  => Identifier (N),
          Chars => Name_Address,
          Expression => Expression (N)));
      Analyze_Attribute_Definition_Clause (N);
   end Analyze_At_Clause;

   -----------------------------------------
   -- Analyze_Attribute_Definition_Clause --
   -----------------------------------------

   procedure Analyze_Attribute_Definition_Clause (N : Node_Id) is
      Loc   : constant Source_Ptr   := Sloc (N);
      Nam   : constant Node_Id      := Name (N);
      Attr  : constant Name_Id      := Chars (N);
      Expr  : constant Node_Id      := Expression (N);
      Id    : constant Attribute_Id := Get_Attribute_Id (Attr);
      Ent   : Entity_Id;
      U_Ent : Entity_Id;

      FOnly : Boolean := False;
      --  Reset to True for subtype specific attribute (Alignment, Size)
      --  and for stream attributes, i.e. those cases where in the call
      --  to Rep_Item_Too_Late, FOnly is set True so that only the freezing
      --  rules are checked. Note that the case of stream attributes is not
      --  clear from the RM, but see AI95-00137. Also, the RM seems to
      --  disallow Storage_Size for derived task types, but that is also
      --  clearly unintentional.

   begin
      Analyze (Nam);
      Ent := Entity (Nam);

      if Rep_Item_Too_Early (Ent, N) then
         return;
      end if;

      --  Rep clause applies to full view of incomplete type or private type
      --  if we have one (if not, this is a premature use of the type).
      --  However, certain semantic checks need to be done on the specified
      --  entity (i.e. the private view), so we save it in Ent.

      if Is_Private_Type (Ent)
        and then Is_Derived_Type (Ent)
        and then not Is_Tagged_Type (Ent)
        and then No (Full_View (Ent))
      then
         --  If this is a private type whose completion is a derivation
         --  from another private type, there is no full view, and the
         --  attribute belongs to the type itself, not its underlying parent.

         U_Ent := Ent;

      elsif Ekind (Ent) = E_Incomplete_Type then
         Ent := Underlying_Type (Ent);
         U_Ent := Ent;
      else
         U_Ent := Underlying_Type (Ent);
      end if;

      --  Complete other routine error checks

      if Etype (Nam) = Any_Type then
         return;

      elsif Scope (Ent) /= Current_Scope then
         Error_Msg_N ("entity must be declared in this scope", Nam);
         return;

      elsif Is_Type (U_Ent)
        and then not Is_First_Subtype (U_Ent)
        and then Id /= Attribute_Object_Size
        and then Id /= Attribute_Value_Size
        and then not From_At_Mod (N)
      then
         Error_Msg_N ("cannot specify attribute for subtype", Nam);
         return;

      end if;

      --  Switch on particular attribute

      case Id is

         -------------
         -- Address --
         -------------

         --  Address attribute definition clause

         when Attribute_Address => Address : begin
            Analyze_And_Resolve (Expr, RTE (RE_Address));

            if Present (Address_Clause (U_Ent)) then
               Error_Msg_N ("address already given for &", Nam);

            --  Case of address clause for subprogram

            elsif Is_Subprogram (U_Ent) then

               if Has_Homonym (U_Ent) then
                  Error_Msg_N
                    ("address clause cannot be given " &
                     "for overloaded subprogram",
                     Nam);
               end if;

               --  For subprograms, all address clauses are permitted,
               --  and we mark the subprogram as having a deferred freeze
               --  so that Gigi will not elaborate it too soon.

               --  Above needs more comments, what is too soon about???

               Set_Has_Delayed_Freeze (U_Ent);

            --  Case of address clause for entry

            elsif Ekind (U_Ent) = E_Entry then

               if Nkind (Parent (N)) = N_Task_Body then
                  Error_Msg_N
                    ("entry address must be specified in task spec", Nam);
               end if;

               --  For entries, we require a constant address

               Check_Constant_Address_Clause (Expr, U_Ent);

            --  Case of address clause for an object

            elsif
              Ekind (U_Ent) = E_Variable
                or else
              Ekind (U_Ent) = E_Constant
            then
               declare
                  Decl : constant Node_Id   := Declaration_Node (U_Ent);
                  Expr : constant Node_Id   := Expression (N);
                  Typ  : constant Entity_Id := Etype (U_Ent);

               begin
                  --  Exported variables cannot have an address clause,
                  --  because this cancels the effect of the pragma Export

                  if Is_Exported (U_Ent) then
                     Error_Msg_N
                       ("cannot export object with address clause", Nam);

                  --  Imported variables can have an address clause, but then
                  --  the import is pretty meaningless except to suppress
                  --  initializations, so we do not need such variables to
                  --  be statically allocated (and in fact it causes trouble
                  --  if the address clause is a local value).

                  elsif Is_Imported (U_Ent) then
                     Set_Is_Statically_Allocated (U_Ent, False);
                  end if;

                  --  We mark a possible modification of a variable with an
                  --  address clause, since it is likely aliasing is occurring.

                  Note_Possible_Modification (Nam);

                  --  If we have no initialization of any kind, then we can
                  --  safely defer the elaboration of the variable to its
                  --  freezing point, so that the address clause will be
                  --  computed at the proper point.

                  --  The same processing applies to all initialized scalar
                  --  types and all access types. Packed bit arrays of size
                  --  up to 64 are represented using a modular type with an
                  --  initialization (to zero) and can be processed like
                  --  other initialized scalar types.

                  if (No (Expression (Decl))
                       and then not Has_Non_Null_Base_Init_Proc (Typ))

                    or else
                      (Present (Expression (Decl))
                        and then Is_Scalar_Type (Typ))

                    or else
                      Is_Access_Type (Typ)

                    or else
                      (Is_Bit_Packed_Array (Base_Type (Typ))
                        and then
                          Is_Modular_Integer_Type (Packed_Array_Type (Typ)))
                  then
                     Set_Has_Delayed_Freeze (U_Ent);

                  --  Otherwise, we require the address clause to be constant

                  else
                     Check_Constant_Address_Clause (Expr, U_Ent);
                  end if;

                  if Is_Exported (U_Ent) then
                     Error_Msg_N
                       ("& cannot be exported if an address clause is given",
                        Nam);
                     Error_Msg_N
                       ("\define and export a variable " &
                        "that holds its address instead",
                        Nam);
                  end if;

                  if not Error_Posted (Expr) then
                     Warn_Overlay (Expr, Typ, Nam);
                  end if;

                  --  If entity has delayed freeze then we will generate
                  --  an alignment check at the freeze point. If there is
                  --  no delayed freeze we can do it right now.

                  if not Has_Delayed_Freeze (U_Ent) then
                     Apply_Alignment_Check (U_Ent, N);
                  end if;

                  --  Kill the size check code, since we are not allocating
                  --  the variable, it is somewhere else.

                  Kill_Size_Check_Code (U_Ent);
               end;

            --  Not a valid entity for an address clause

            else
               Error_Msg_N ("address cannot be given for &", Nam);
            end if;
         end Address;

         ---------------
         -- Alignment --
         ---------------

         --  Alignment attribute definition clause

         when Attribute_Alignment => Alignment_Block : declare
            Align : Uint := Get_Alignment_Value (Expr);

         begin
            FOnly := True;

            if not Is_Type (U_Ent)
              and then Ekind (U_Ent) /= E_Variable
              and then Ekind (U_Ent) /= E_Constant
            then
               Error_Msg_N ("alignment cannot be given for &", Nam);

            elsif Has_Alignment_Clause (U_Ent) then
               Error_Msg_Sloc := Sloc (Alignment_Clause (U_Ent));
               Error_Msg_N ("alignment clause previously given#", N);

            elsif Align /= No_Uint then
               Set_Has_Alignment_Clause (U_Ent);
               Set_Alignment            (U_Ent, Align);
            end if;
         end Alignment_Block;

         ---------------
         -- Bit_Order --
         ---------------

         --  Bit_Order attribute definition clause

         when Attribute_Bit_Order => Bit_Order : declare
         begin
            if not Is_Record_Type (U_Ent) then
               Error_Msg_N
                 ("Bit_Order can only be defined for record type", Nam);

            else
               Analyze_And_Resolve (Expr, RTE (RE_Bit_Order));

               if Etype (Expr) = Any_Type then
                  return;

               elsif not Is_Static_Expression (Expr) then
                  Error_Msg_N ("Bit_Order requires static expression", Expr);

               else
                  if (Expr_Value (Expr) = 0) /= Bytes_Big_Endian then
                     Set_Reverse_Bit_Order (U_Ent, True);
                  end if;
               end if;
            end if;
         end Bit_Order;

         --------------------
         -- Component_Size --
         --------------------

         --  Component_Size attribute definition clause

         when Attribute_Component_Size => Component_Size_Case : declare
            Csize    : constant Uint := Static_Integer (Expr);
            Btype    : Entity_Id;
            Biased   : Boolean;
            New_Ctyp : Entity_Id;
            Decl     : Node_Id;

         begin
            if not Is_Array_Type (U_Ent) then
               Error_Msg_N ("component size requires array type", Nam);
               return;
            end if;

            Btype := Base_Type (U_Ent);

            if Has_Component_Size_Clause (Btype) then
               Error_Msg_N
                 ("component size clase for& previously given", Nam);

            elsif Csize /= No_Uint then
               Check_Size (Expr, Component_Type (Btype), Csize, Biased);

               if Has_Aliased_Components (Btype)
                 and then Csize < 32
                 and then Csize /= 8
                 and then Csize /= 16
               then
                  Error_Msg_N
                    ("component size incorrect for aliased components", N);
                  return;
               end if;

               --  For the biased case, build a declaration for a subtype
               --  that will be used to represent the biased subtype that
               --  reflects the biased representation of components. We need
               --  this subtype to get proper conversions on referencing
               --  elements of the array.

               if Biased then
                  New_Ctyp :=
                    Make_Defining_Identifier (Loc,
                      Chars => New_External_Name (Chars (U_Ent), 'C', 0, 'T'));

                  Decl :=
                    Make_Subtype_Declaration (Loc,
                      Defining_Identifier => New_Ctyp,
                      Subtype_Indication  =>
                        New_Occurrence_Of (Component_Type (Btype), Loc));

                  Set_Parent (Decl, N);
                  Analyze (Decl, Suppress => All_Checks);

                  Set_Has_Delayed_Freeze        (New_Ctyp, False);
                  Set_Esize                     (New_Ctyp, Csize);
                  Set_RM_Size                   (New_Ctyp, Csize);
                  Init_Alignment                (New_Ctyp);
                  Set_Has_Biased_Representation (New_Ctyp, True);
                  Set_Is_Itype                  (New_Ctyp, True);
                  Set_Associated_Node_For_Itype (New_Ctyp, U_Ent);

                  Set_Component_Type (Btype, New_Ctyp);
               end if;

               Set_Component_Size            (Btype, Csize);
               Set_Has_Component_Size_Clause (Btype, True);
               Set_Has_Non_Standard_Rep      (Btype, True);
            end if;
         end Component_Size_Case;

         ------------------
         -- External_Tag --
         ------------------

         when Attribute_External_Tag => External_Tag :
         begin
            if not Is_Tagged_Type (U_Ent) then
               Error_Msg_N ("should be a tagged type", Nam);
            end if;

            Analyze_And_Resolve (Expr, Standard_String);

            if not Is_Static_Expression (Expr) then
               Error_Msg_N ("must be a static string", Nam);
            end if;

            Set_Has_External_Tag_Rep_Clause (U_Ent);
         end External_Tag;

         -----------
         -- Input --
         -----------

         when Attribute_Input => Input : declare
            Subp : Entity_Id := Empty;
            I    : Interp_Index;
            It   : Interp;
            Pnam : Entity_Id;

            function Has_Good_Profile (Subp : Entity_Id) return Boolean;
            --  Return true if the entity is a function with an appropriate
            --  profile for the Input attribute.

            function Has_Good_Profile (Subp : Entity_Id) return Boolean is
               F  : Entity_Id;
               Ok : Boolean := False;

            begin
               if Ekind (Subp) = E_Function then
                  F := First_Formal (Subp);

                  if Present (F) and then No (Next_Formal (F)) then
                     if Ekind (Etype (F)) = E_Anonymous_Access_Type
                       and then
                         Designated_Type (Etype (F)) =
                           Class_Wide_Type (RTE (RE_Root_Stream_Type))
                     then
                        Ok := Base_Type (Etype (Subp)) = Base_Type (Ent);
                     end if;
                  end if;
               end if;

               return Ok;
            end Has_Good_Profile;

         --  Start of processing for Input attribute definition

         begin
            FOnly := True;

            if not Is_Type (U_Ent) then
               Error_Msg_N ("local name must be a subtype", Nam);
               return;

            else
               Pnam := TSS (Base_Type (U_Ent), Name_uInput);

               if Present (Pnam)
                 and then Base_Type (Etype (Pnam)) = Base_Type (U_Ent)
               then
                  Error_Msg_Sloc := Sloc (Pnam);
                  Error_Msg_N ("input attribute already defined #", Nam);
                  return;
               end if;
            end if;

            Analyze (Expr);

            if Is_Entity_Name (Expr) then
               if not Is_Overloaded (Expr) then
                  if Has_Good_Profile (Entity (Expr)) then
                     Subp := Entity (Expr);
                  end if;

               else
                  Get_First_Interp (Expr, I, It);

                  while Present (It.Nam) loop
                     if Has_Good_Profile (It.Nam) then
                        Subp := It.Nam;
                        exit;
                     end if;

                     Get_Next_Interp (I, It);
                  end loop;
               end if;
            end if;

            if Present (Subp) then
               Set_Entity (Expr, Subp);
               Set_Etype (Expr, Etype (Subp));
               New_Stream_Function (N, U_Ent, Subp,  Name_uInput);
            else
               Error_Msg_N ("incorrect expression for input attribute", Expr);
               return;
            end if;
         end Input;

         -------------------
         -- Machine_Radix --
         -------------------

         --  Machine radix attribute definition clause

         when Attribute_Machine_Radix => Machine_Radix : declare
            Radix : constant Uint := Static_Integer (Expr);

         begin
            if not Is_Decimal_Fixed_Point_Type (U_Ent) then
               Error_Msg_N ("decimal fixed-point type expected for &", Nam);

            elsif Has_Machine_Radix_Clause (U_Ent) then
               Error_Msg_Sloc := Sloc (Alignment_Clause (U_Ent));
               Error_Msg_N ("machine radix clause previously given#", N);

            elsif Radix /= No_Uint then
               Set_Has_Machine_Radix_Clause (U_Ent);
               Set_Has_Non_Standard_Rep (Base_Type (U_Ent));

               if Radix = 2 then
                  null;
               elsif Radix = 10 then
                  Set_Machine_Radix_10 (U_Ent);
               else
                  Error_Msg_N ("machine radix value must be 2 or 10", Expr);
               end if;
            end if;
         end Machine_Radix;

         -----------------
         -- Object_Size --
         -----------------

         --  Object_Size attribute definition clause

         when Attribute_Object_Size => Object_Size : declare
            Size   : constant Uint := Static_Integer (Expr);
            Biased : Boolean;

         begin
            if not Is_Type (U_Ent) then
               Error_Msg_N ("Object_Size cannot be given for &", Nam);

            elsif Has_Object_Size_Clause (U_Ent) then
               Error_Msg_N ("Object_Size already given for &", Nam);

            else
               Check_Size (Expr, U_Ent, Size, Biased);

               if Size /= 8
                    and then
                  Size /= 16
                    and then
                  Size /= 32
                    and then
                  UI_Mod (Size, 64) /= 0
               then
                  Error_Msg_N
                    ("Object_Size must be 8, 16, 32, or multiple of 64",
                     Expr);
               end if;

               Set_Esize (U_Ent, Size);
               Set_Has_Object_Size_Clause (U_Ent);
               Alignment_Check_For_Esize_Change (U_Ent);
            end if;
         end Object_Size;

         ------------
         -- Output --
         ------------

         when Attribute_Output => Output : declare
            Subp : Entity_Id := Empty;
            I    : Interp_Index;
            It   : Interp;
            Pnam : Entity_Id;

            function Has_Good_Profile (Subp : Entity_Id) return Boolean;
            --  Return true if the entity is a procedure with an
            --  appropriate profile for the output attribute.

            function Has_Good_Profile (Subp : Entity_Id) return Boolean is
               F  : Entity_Id;
               Ok : Boolean := False;

            begin
               if Ekind (Subp) = E_Procedure then
                  F := First_Formal (Subp);

                  if Present (F) then
                     if Ekind (Etype (F)) = E_Anonymous_Access_Type
                       and then
                         Designated_Type (Etype (F)) =
                           Class_Wide_Type (RTE (RE_Root_Stream_Type))
                     then
                        Next_Formal (F);
                        Ok :=  Present (F)
                          and then Parameter_Mode (F) = E_In_Parameter
                          and then Base_Type (Etype (F)) = Base_Type (Ent)
                          and then No (Next_Formal (F));
                     end if;
                  end if;
               end if;

               return Ok;
            end Has_Good_Profile;

         begin
            FOnly := True;

            if not Is_Type (U_Ent) then
               Error_Msg_N ("local name must be a subtype", Nam);
               return;

            else
               Pnam := TSS (Base_Type (U_Ent), Name_uOutput);

               if Present (Pnam)
                 and then
                   Base_Type (Etype (Next_Formal (First_Formal (Pnam))))
                                                        = Base_Type (U_Ent)
               then
                  Error_Msg_Sloc := Sloc (Pnam);
                  Error_Msg_N ("output attribute already defined #", Nam);
                  return;
               end if;
            end if;

            Analyze (Expr);

            if Is_Entity_Name (Expr) then
               if not Is_Overloaded (Expr) then
                  if Has_Good_Profile (Entity (Expr)) then
                     Subp := Entity (Expr);
                  end if;

               else
                  Get_First_Interp (Expr, I, It);

                  while Present (It.Nam) loop
                     if Has_Good_Profile (It.Nam) then
                        Subp := It.Nam;
                        exit;
                     end if;

                     Get_Next_Interp (I, It);
                  end loop;
               end if;
            end if;

            if Present (Subp) then
               Set_Entity (Expr, Subp);
               Set_Etype (Expr, Etype (Subp));
               New_Stream_Procedure (N, U_Ent, Subp, Name_uOutput);
            else
               Error_Msg_N ("incorrect expression for output attribute", Expr);
               return;
            end if;
         end Output;

         ----------
         -- Read --
         ----------

         when Attribute_Read => Read : declare
            Subp : Entity_Id := Empty;
            I    : Interp_Index;
            It   : Interp;
            Pnam : Entity_Id;

            function Has_Good_Profile (Subp : Entity_Id) return Boolean;
            --  Return true if the entity is a procedure with an appropriate
            --  profile for the Read attribute.

            function Has_Good_Profile (Subp : Entity_Id) return Boolean is
               F     : Entity_Id;
               Ok    : Boolean := False;

            begin
               if Ekind (Subp) = E_Procedure then
                  F := First_Formal (Subp);

                  if Present (F) then
                     if Ekind (Etype (F)) = E_Anonymous_Access_Type
                       and then
                         Designated_Type (Etype (F)) =
                           Class_Wide_Type (RTE (RE_Root_Stream_Type))
                     then
                        Next_Formal (F);
                        Ok :=  Present (F)
                          and then Parameter_Mode (F) = E_Out_Parameter
                          and then Base_Type (Etype (F)) = Base_Type (Ent)
                          and then No (Next_Formal (F));
                     end if;
                  end if;
               end if;

               return Ok;
            end Has_Good_Profile;

         --  Start of processing for Read attribute definition

         begin
            FOnly := True;

            if not Is_Type (U_Ent) then
               Error_Msg_N ("local name must be a subtype", Nam);
               return;

            else
               Pnam := TSS (Base_Type (U_Ent), Name_uRead);

               if Present (Pnam)
                 and then Base_Type (Etype (Next_Formal (First_Formal (Pnam))))
                   = Base_Type (U_Ent)
               then
                  Error_Msg_Sloc := Sloc (Pnam);
                  Error_Msg_N ("read attribute already defined #", Nam);
                  return;
               end if;
            end if;

            Analyze (Expr);

            if Is_Entity_Name (Expr) then
               if not Is_Overloaded (Expr) then
                  if Has_Good_Profile (Entity (Expr)) then
                     Subp := Entity (Expr);
                  end if;

               else
                  Get_First_Interp (Expr, I, It);

                  while Present (It.Nam) loop
                     if Has_Good_Profile (It.Nam) then
                        Subp := It.Nam;
                        exit;
                     end if;

                     Get_Next_Interp (I, It);
                  end loop;
               end if;
            end if;

            if Present (Subp) then
               Set_Entity (Expr, Subp);
               Set_Etype (Expr, Etype (Subp));
               New_Stream_Procedure (N, U_Ent, Subp, Name_uRead, True);
            else
               Error_Msg_N ("incorrect expression for read attribute", Expr);
               return;
            end if;
         end Read;

         ----------
         -- Size --
         ----------

         --  Size attribute definition clause

         when Attribute_Size => Size : declare
            Size   : constant Uint := Static_Integer (Expr);
            Etyp   : Entity_Id;
            Biased : Boolean;

         begin
            FOnly := True;

            if Has_Size_Clause (U_Ent) then
               Error_Msg_N ("size already given for &", Nam);

            elsif not Is_Type (U_Ent)
              and then Ekind (U_Ent) /= E_Variable
              and then Ekind (U_Ent) /= E_Constant
            then
               Error_Msg_N ("size cannot be given for &", Nam);

            elsif Is_Array_Type (U_Ent)
              and then not Is_Constrained (U_Ent)
            then
               Error_Msg_N
                 ("size cannot be given for unconstrained array", Nam);

            elsif Size /= No_Uint then

               if Is_Type (U_Ent) then
                  Etyp := U_Ent;
               else
                  Etyp := Etype (U_Ent);
               end if;

               --  Check size, note that Gigi is in charge of checking
               --  that the size of an array or record type is OK. Also
               --  we do not check the size in the ordinary fixed-point
               --  case, since it is too early to do so (there may be a
               --  subsequent small clause that affects the size). We can
               --  check the size if a small clause has already been given.

               if not Is_Ordinary_Fixed_Point_Type (U_Ent)
                 or else Has_Small_Clause (U_Ent)
               then
                  Check_Size (Expr, Etyp, Size, Biased);
                  Set_Has_Biased_Representation (U_Ent, Biased);
               end if;

               --  For types set RM_Size and Esize if possible

               if Is_Type (U_Ent) then
                  Set_RM_Size (U_Ent, Size);

                  --  For scalar types, increase Object_Size to power of 2,
                  --  but not less than 8 in any case, i.e. byte addressable.

                  if Is_Scalar_Type (U_Ent) then
                     if Size <= 8 then
                        Init_Esize (U_Ent, 8);
                     elsif Size <= 16 then
                        Init_Esize (U_Ent, 16);
                     elsif Size <= 32 then
                        Init_Esize (U_Ent, 32);
                     else
                        Set_Esize  (U_Ent, (Size + 63) / 64 * 64);
                     end if;

                  --  For all other types, object size = value size. The
                  --  backend will adjust as needed.

                  else
                     Set_Esize (U_Ent, Size);
                  end if;

                  Alignment_Check_For_Esize_Change (U_Ent);

               --  For objects, set Esize only

               else
                  Set_Esize (U_Ent, Size);
               end if;

               Set_Has_Size_Clause (U_Ent);
            end if;
         end Size;

         -----------
         -- Small --
         -----------

         --  Small attribute definition clause

         when Attribute_Small => Small : declare
            Implicit_Base : constant Entity_Id := Base_Type (U_Ent);
            Small         : Ureal;

         begin
            Analyze_And_Resolve (Expr, Any_Real);

            if Etype (Expr) = Any_Type then
               return;

            elsif not Is_Static_Expression (Expr) then
               Error_Msg_N ("small requires static expression", Expr);
               return;

            else
               Small := Expr_Value_R (Expr);

               if Small <= Ureal_0 then
                  Error_Msg_N ("small value must be greater than zero", Expr);
                  return;
               end if;

            end if;

            if not Is_Ordinary_Fixed_Point_Type (U_Ent) then
               Error_Msg_N
                 ("small requires an ordinary fixed point type", Nam);

            elsif Has_Small_Clause (U_Ent) then
               Error_Msg_N ("small already given for &", Nam);

            elsif Small > Delta_Value (U_Ent) then
               Error_Msg_N
                 ("small value must not be greater then delta value", Nam);

            else
               Set_Small_Value (U_Ent, Small);
               Set_Small_Value (Implicit_Base, Small);
               Set_Has_Small_Clause (U_Ent);
               Set_Has_Small_Clause (Implicit_Base);
               Set_Has_Non_Standard_Rep (Implicit_Base);
            end if;
         end Small;

         ------------------
         -- Storage_Size --
         ------------------

         --  Storage_Size attribute definition clause

         when Attribute_Storage_Size => Storage_Size : declare
            Btype : constant Entity_Id := Base_Type (U_Ent);
            Sprag : Node_Id;

         begin
            if Is_Task_Type (U_Ent) then
               FOnly := True;
            end if;

            if not Is_Access_Type (U_Ent)
              and then Ekind (U_Ent) /= E_Task_Type
            then
               Error_Msg_N ("storage size cannot be given for &", Nam);

            elsif Is_Access_Type (U_Ent) and Is_Derived_Type (U_Ent) then
               Error_Msg_N
                 ("storage size cannot be given for a derived access type",
                  Nam);

            elsif Has_Storage_Size_Clause (Btype) then
               Error_Msg_N ("storage size already given for &", Nam);

            else
               Analyze_And_Resolve (Expr, Any_Integer);

               if Is_Access_Type (U_Ent) then

                  if Present (Associated_Storage_Pool (U_Ent)) then
                     Error_Msg_N ("storage pool already given for &", Nam);
                     return;
                  end if;

                  if Compile_Time_Known_Value (Expr)
                    and then Expr_Value (Expr) = 0
                  then
                     Set_No_Pool_Assigned (Btype);
                  end if;

               else -- Is_Task_Type (U_Ent)
                  Sprag := Get_Rep_Pragma (Btype, Name_Storage_Size);

                  if Present (Sprag) then
                     Error_Msg_Sloc := Sloc (Sprag);
                     Error_Msg_N
                       ("Storage_Size already specified#", Nam);
                     return;
                  end if;
               end if;

               Set_Has_Storage_Size_Clause (Btype);
            end if;
         end Storage_Size;

         ------------------
         -- Storage_Pool --
         ------------------

         --  Storage_Pool attribute definition clause

         when Attribute_Storage_Pool => Storage_Pool : declare
            Pool : Entity_Id;

         begin
            if Ekind (U_Ent) /= E_Access_Type
              and then Ekind (U_Ent) /= E_General_Access_Type
            then
               Error_Msg_N (
                 "storage pool can only be given for access types", Nam);
               return;

            elsif Is_Derived_Type (U_Ent) then
               Error_Msg_N
                 ("storage pool cannot be given for a derived access type",
                  Nam);

            elsif Has_Storage_Size_Clause (U_Ent) then
               Error_Msg_N ("storage size already given for &", Nam);
               return;

            elsif Present (Associated_Storage_Pool (U_Ent)) then
               Error_Msg_N ("storage pool already given for &", Nam);
               return;
            end if;

            Analyze_And_Resolve
              (Expr, Class_Wide_Type (RTE (RE_Root_Storage_Pool)));

            --  If the argument is a name that is not an entity name, then
            --  we construct a renaming operation to define an entity of
            --  type storage pool.

            if not Is_Entity_Name (Expr)
              and then Is_Object_Reference (Expr)
            then
               Pool :=
                 Make_Defining_Identifier (Loc,
                   Chars => New_Internal_Name ('P'));

               declare
                  Rnode : constant Node_Id :=
                            Make_Object_Renaming_Declaration (Loc,
                              Defining_Identifier => Pool,
                              Subtype_Mark        =>
                                New_Occurrence_Of (Etype (Expr), Loc),
                              Name => Expr);

               begin
                  Insert_Before (N, Rnode);
                  Analyze (Rnode);
                  Set_Associated_Storage_Pool (U_Ent, Pool);
               end;

            elsif Is_Entity_Name (Expr) then
               Pool := Entity (Expr);

               --  If pool is a renamed object, get original one. This can
               --  happen with an explicit renaming, and within instances.

               while Present (Renamed_Object (Pool))
                 and then Is_Entity_Name (Renamed_Object (Pool))
               loop
                  Pool := Entity (Renamed_Object (Pool));
               end loop;

               if Present (Renamed_Object (Pool))
                 and then Nkind (Renamed_Object (Pool)) = N_Type_Conversion
                 and then Is_Entity_Name (Expression (Renamed_Object (Pool)))
               then
                  Pool := Entity (Expression (Renamed_Object (Pool)));
               end if;

               if Present (Etype (Pool))
                 and then Etype (Pool) /= RTE (RE_Stack_Bounded_Pool)
                 and then Etype (Pool) /= RTE (RE_Unbounded_Reclaim_Pool)
               then
                  Set_Associated_Storage_Pool (U_Ent, Pool);
               else
                  Error_Msg_N ("Non sharable GNAT Pool", Expr);
               end if;

            --  The pool may be specified as the Storage_Pool of some other
            --  type. It is rewritten as a class_wide conversion of the
            --  corresponding pool entity.

            elsif Nkind (Expr) = N_Type_Conversion
              and then Is_Entity_Name (Expression (Expr))
              and then Nkind (Original_Node (Expr)) = N_Attribute_Reference
            then
               Pool := Entity (Expression (Expr));

               if Present (Etype (Pool))
                 and then Etype (Pool) /= RTE (RE_Stack_Bounded_Pool)
                 and then Etype (Pool) /= RTE (RE_Unbounded_Reclaim_Pool)
               then
                  Set_Associated_Storage_Pool (U_Ent, Pool);
               else
                  Error_Msg_N ("Non sharable GNAT Pool", Expr);
               end if;

            else
               Error_Msg_N ("incorrect reference to a Storage Pool", Expr);
               return;
            end if;
         end Storage_Pool;

         ----------------
         -- Value_Size --
         ----------------

         --  Value_Size attribute definition clause

         when Attribute_Value_Size => Value_Size : declare
            Size   : constant Uint := Static_Integer (Expr);
            Biased : Boolean;

         begin
            if not Is_Type (U_Ent) then
               Error_Msg_N ("Value_Size cannot be given for &", Nam);

            elsif Present
                   (Get_Attribute_Definition_Clause
                     (U_Ent, Attribute_Value_Size))
            then
               Error_Msg_N ("Value_Size already given for &", Nam);

            else
               if Is_Elementary_Type (U_Ent) then
                  Check_Size (Expr, U_Ent, Size, Biased);
                  Set_Has_Biased_Representation (U_Ent, Biased);
               end if;

               Set_RM_Size (U_Ent, Size);
            end if;
         end Value_Size;

         -----------
         -- Write --
         -----------

         --  Write attribute definition clause
         --  check for class-wide case will be performed later

         when Attribute_Write => Write : declare
            Subp : Entity_Id := Empty;
            I    : Interp_Index;
            It   : Interp;
            Pnam : Entity_Id;

            function Has_Good_Profile (Subp : Entity_Id) return Boolean;
            --  Return true if the entity is a procedure with an
            --  appropriate profile for the write attribute.

            function Has_Good_Profile (Subp : Entity_Id) return Boolean is
               F     : Entity_Id;
               Ok    : Boolean := False;

            begin
               if Ekind (Subp) = E_Procedure then
                  F := First_Formal (Subp);

                  if Present (F) then
                     if Ekind (Etype (F)) = E_Anonymous_Access_Type
                       and then
                         Designated_Type (Etype (F)) =
                           Class_Wide_Type (RTE (RE_Root_Stream_Type))
                     then
                        Next_Formal (F);
                        Ok :=  Present (F)
                          and then Parameter_Mode (F) = E_In_Parameter
                          and then Base_Type (Etype (F)) = Base_Type (Ent)
                          and then No (Next_Formal (F));
                     end if;
                  end if;
               end if;

               return Ok;
            end Has_Good_Profile;

         --  Start of processing for Write attribute definition

         begin
            FOnly := True;

            if not Is_Type (U_Ent) then
               Error_Msg_N ("local name must be a subtype", Nam);
               return;
            end if;

            Pnam := TSS (Base_Type (U_Ent), Name_uWrite);

            if Present (Pnam)
              and then Base_Type (Etype (Next_Formal (First_Formal (Pnam))))
                = Base_Type (U_Ent)
            then
               Error_Msg_Sloc := Sloc (Pnam);
               Error_Msg_N ("write attribute already defined #", Nam);
               return;
            end if;

            Analyze (Expr);

            if Is_Entity_Name (Expr) then
               if not Is_Overloaded (Expr) then
                  if Has_Good_Profile (Entity (Expr)) then
                     Subp := Entity (Expr);
                  end if;

               else
                  Get_First_Interp (Expr, I, It);

                  while Present (It.Nam) loop
                     if Has_Good_Profile (It.Nam) then
                        Subp := It.Nam;
                        exit;
                     end if;

                     Get_Next_Interp (I, It);
                  end loop;
               end if;
            end if;

            if Present (Subp) then
               Set_Entity (Expr, Subp);
               Set_Etype (Expr, Etype (Subp));
               New_Stream_Procedure (N, U_Ent, Subp, Name_uWrite);
            else
               Error_Msg_N ("incorrect expression for write attribute", Expr);
               return;
            end if;
         end Write;

         --  All other attributes cannot be set

         when others =>
            Error_Msg_N
              ("attribute& cannot be set with definition clause", N);

      end case;

      --  The test for the type being frozen must be performed after
      --  any expression the clause has been analyzed since the expression
      --  itself might cause freezing that makes the clause illegal.

      if Rep_Item_Too_Late (U_Ent, N, FOnly) then
         return;
      end if;
   end Analyze_Attribute_Definition_Clause;

   ----------------------------
   -- Analyze_Code_Statement --
   ----------------------------

   procedure Analyze_Code_Statement (N : Node_Id) is
      HSS   : constant Node_Id   := Parent (N);
      SBody : constant Node_Id   := Parent (HSS);
      Subp  : constant Entity_Id := Current_Scope;
      Stmt  : Node_Id;
      Decl  : Node_Id;
      StmtO : Node_Id;
      DeclO : Node_Id;

   begin
      --  Analyze and check we get right type, note that this implements the
      --  requirement (RM 13.8(1)) that Machine_Code be with'ed, since that
      --  is the only way that Asm_Insn could possibly be visible.

      Analyze_And_Resolve (Expression (N));

      if Etype (Expression (N)) = Any_Type then
         return;
      elsif Etype (Expression (N)) /= RTE (RE_Asm_Insn) then
         Error_Msg_N ("incorrect type for code statement", N);
         return;
      end if;

      --  Make sure we appear in the handled statement sequence of a
      --  subprogram (RM 13.8(3)).

      if Nkind (HSS) /= N_Handled_Sequence_Of_Statements
        or else Nkind (SBody) /= N_Subprogram_Body
      then
         Error_Msg_N
           ("code statement can only appear in body of subprogram", N);
         return;
      end if;

      --  Do remaining checks (RM 13.8(3)) if not already done

      if not Is_Machine_Code_Subprogram (Subp) then
         Set_Is_Machine_Code_Subprogram (Subp);

         --  No exception handlers allowed

         if Present (Exception_Handlers (HSS)) then
            Error_Msg_N
              ("exception handlers not permitted in machine code subprogram",
               First (Exception_Handlers (HSS)));
         end if;

         --  No declarations other than use clauses and pragmas (we allow
         --  certain internally generated declarations as well).

         Decl := First (Declarations (SBody));
         while Present (Decl) loop
            DeclO := Original_Node (Decl);
            if Comes_From_Source (DeclO)
              and then Nkind (DeclO) /= N_Pragma
              and then Nkind (DeclO) /= N_Use_Package_Clause
              and then Nkind (DeclO) /= N_Use_Type_Clause
              and then Nkind (DeclO) /= N_Implicit_Label_Declaration
            then
               Error_Msg_N
                 ("this declaration not allowed in machine code subprogram",
                  DeclO);
            end if;

            Next (Decl);
         end loop;

         --  No statements other than code statements, pragmas, and labels.
         --  Again we allow certain internally generated statements.

         Stmt := First (Statements (HSS));
         while Present (Stmt) loop
            StmtO := Original_Node (Stmt);
            if Comes_From_Source (StmtO)
              and then Nkind (StmtO) /= N_Pragma
              and then Nkind (StmtO) /= N_Label
              and then Nkind (StmtO) /= N_Code_Statement
            then
               Error_Msg_N
                 ("this statement is not allowed in machine code subprogram",
                  StmtO);
            end if;

            Next (Stmt);
         end loop;
      end if;

   end Analyze_Code_Statement;

   -----------------------------------------------
   -- Analyze_Enumeration_Representation_Clause --
   -----------------------------------------------

   procedure Analyze_Enumeration_Representation_Clause (N : Node_Id) is
      Ident    : constant Node_Id    := Identifier (N);
      Aggr     : constant Node_Id    := Array_Aggregate (N);
      Enumtype : Entity_Id;
      Elit     : Entity_Id;
      Expr     : Node_Id;
      Assoc    : Node_Id;
      Choice   : Node_Id;
      Val      : Uint;
      Err      : Boolean := False;

      Lo  : constant Uint := Expr_Value (Type_Low_Bound (Universal_Integer));
      Hi  : constant Uint := Expr_Value (Type_High_Bound (Universal_Integer));
      Min : Uint;
      Max : Uint;

   begin
      --  First some basic error checks

      Find_Type (Ident);
      Enumtype := Entity (Ident);

      if Enumtype = Any_Type
        or else Rep_Item_Too_Early (Enumtype, N)
      then
         return;
      else
         Enumtype := Underlying_Type (Enumtype);
      end if;

      if not Is_Enumeration_Type (Enumtype) then
         Error_Msg_NE
           ("enumeration type required, found}",
            Ident, First_Subtype (Enumtype));
         return;
      end if;

      if Scope (Enumtype) /= Current_Scope then
         Error_Msg_N ("type must be declared in this scope", Ident);
         return;

      elsif not Is_First_Subtype (Enumtype) then
         Error_Msg_N ("cannot give enumeration rep clause for subtype", N);
         return;

      elsif Has_Enumeration_Rep_Clause (Enumtype) then
         Error_Msg_N ("duplicate enumeration rep clause ignored", N);
         return;

      elsif Root_Type (Enumtype) = Standard_Character
        or else Root_Type (Enumtype) = Standard_Wide_Character
      then
         Error_Msg_N ("enumeration rep clause not allowed for this type", N);

      else
         Set_Has_Enumeration_Rep_Clause (Enumtype);
         Set_Has_Enumeration_Rep_Clause (Base_Type (Enumtype));
      end if;

      --  Now we process the aggregate. Note that we don't use the normal
      --  aggregate code for this purpose, because we don't want any of the
      --  normal expansion activities, and a number of special semantic
      --  rules apply (including the component type being any integer type)

      --  Badent signals that we found some incorrect entries processing
      --  the list. The final checks for completeness and ordering are
      --  skipped in this case.

      Elit := First_Literal (Enumtype);

      --  First the positional entries if any

      if Present (Expressions (Aggr)) then
         Expr := First (Expressions (Aggr));
         while Present (Expr) loop
            if No (Elit) then
               Error_Msg_N ("too many entries in aggregate", Expr);
               return;
            end if;

            Val := Static_Integer (Expr);

            if Val = No_Uint then
               Err := True;

            elsif Val < Lo or else Hi < Val then
               Error_Msg_N ("value outside permitted range", Expr);
               Err := True;
            end if;

            Set_Enumeration_Rep (Elit, Val);
            Set_Enumeration_Rep_Expr (Elit, Expr);
            Next (Expr);
            Next (Elit);
         end loop;
      end if;

      --  Now process the named entries if present

      if Present (Component_Associations (Aggr)) then
         Assoc := First (Component_Associations (Aggr));
         while Present (Assoc) loop
            Choice := First (Choices (Assoc));

            if Present (Next (Choice)) then
               Error_Msg_N
                 ("multiple choice not allowed here", Next (Choice));
               Err := True;
            end if;

            if Nkind (Choice) = N_Others_Choice then
               Error_Msg_N ("others choice not allowed here", Choice);
               Err := True;

            elsif Nkind (Choice) = N_Range then
               --  ??? should allow zero/one element range here
               Error_Msg_N ("range not allowed here", Choice);
               Err := True;

            else
               Analyze_And_Resolve (Choice, Enumtype);

               if Is_Entity_Name (Choice)
                 and then Is_Type (Entity (Choice))
               then
                  Error_Msg_N ("subtype name not allowed here", Choice);
                  Err := True;
                  --  ??? should allow static subtype with zero/one entry

               elsif Etype (Choice) = Base_Type (Enumtype) then
                  if not Is_Static_Expression (Choice) then
                     Error_Msg_N
                       ("non-static expression used for choice", Choice);
                     Err := True;

                  else
                     Elit := Expr_Value_E (Choice);

                     if Present (Enumeration_Rep_Expr (Elit)) then
                        Error_Msg_Sloc := Sloc (Enumeration_Rep_Expr (Elit));
                        Error_Msg_NE
                          ("representation for& previously given#",
                           Choice, Elit);
                        Err := True;
                     end if;

                     Set_Enumeration_Rep_Expr (Elit, Choice);

                     Expr := Expression (Assoc);
                     Val := Static_Integer (Expr);

                     if Val = No_Uint then
                        Err := True;

                     elsif Val < Lo or else Hi < Val then
                        Error_Msg_N ("value outside permitted range", Expr);
                        Err := True;
                     end if;

                     Set_Enumeration_Rep (Elit, Val);
                  end if;
               end if;
            end if;

            Next (Assoc);
         end loop;
      end if;

      --  Aggregate is fully processed. Now we check that a full set of
      --  representations was given, and that they are in range and in order.
      --  These checks are only done if no other errors occurred.

      if not Err then
         Min  := No_Uint;
         Max  := No_Uint;

         Elit := First_Literal (Enumtype);
         while Present (Elit) loop
            if No (Enumeration_Rep_Expr (Elit)) then
               Error_Msg_NE ("missing representation for&!", N, Elit);

            else
               Val := Enumeration_Rep (Elit);

               if Min = No_Uint then
                  Min := Val;
               end if;

               if Val /= No_Uint then
                  if Max /= No_Uint and then Val <= Max then
                     Error_Msg_NE
                       ("enumeration value for& not ordered!",
                                       Enumeration_Rep_Expr (Elit), Elit);
                  end if;

                  Max := Val;
               end if;

               --  If there is at least one literal whose representation
               --  is not equal to the Pos value, then note that this
               --  enumeration type has a non-standard representation.

               if Val /= Enumeration_Pos (Elit) then
                  Set_Has_Non_Standard_Rep (Base_Type (Enumtype));
               end if;
            end if;

            Next (Elit);
         end loop;

         --  Now set proper size information

         declare
            Minsize : Uint := UI_From_Int (Minimum_Size (Enumtype));

         begin
            if Has_Size_Clause (Enumtype) then
               if Esize (Enumtype) >= Minsize then
                  null;

               else
                  Minsize :=
                    UI_From_Int (Minimum_Size (Enumtype, Biased => True));

                  if Esize (Enumtype) < Minsize then
                     Error_Msg_N ("previously given size is too small", N);

                  else
                     Set_Has_Biased_Representation (Enumtype);
                  end if;
               end if;

            else
               Set_RM_Size    (Enumtype, Minsize);
               Set_Enum_Esize (Enumtype);
            end if;

            Set_RM_Size   (Base_Type (Enumtype), RM_Size   (Enumtype));
            Set_Esize     (Base_Type (Enumtype), Esize     (Enumtype));
            Set_Alignment (Base_Type (Enumtype), Alignment (Enumtype));
         end;
      end if;

      --  We repeat the too late test in case it froze itself!

      if Rep_Item_Too_Late (Enumtype, N) then
         null;
      end if;

   end Analyze_Enumeration_Representation_Clause;

   ----------------------------
   -- Analyze_Free_Statement --
   ----------------------------

   procedure Analyze_Free_Statement (N : Node_Id) is
   begin
      Analyze (Expression (N));
   end Analyze_Free_Statement;

   ------------------------------------------
   -- Analyze_Record_Representation_Clause --
   ------------------------------------------

   procedure Analyze_Record_Representation_Clause (N : Node_Id) is
      Loc     : constant Source_Ptr := Sloc (N);
      Ident   : constant Node_Id    := Identifier (N);
      Rectype : Entity_Id;
      Fent    : Entity_Id;
      CC      : Node_Id;
      Posit   : Uint;
      Fbit    : Uint;
      Lbit    : Uint;
      Hbit    : Uint := Uint_0;
      Comp    : Entity_Id;
      Ocomp   : Entity_Id;
      Biased  : Boolean;

      Max_Bit_So_Far : Uint;
      --  Records the maximum bit position so far. If all field positoins
      --  are monotonically increasing, then we can skip the circuit for
      --  checking for overlap, since no overlap is possible.

      Overlap_Check_Required : Boolean;
      --  Used to keep track of whether or not an overlap check is required

      Ccount : Natural := 0;
      --  Number of component clauses in record rep clause

   begin
      Find_Type (Ident);
      Rectype := Entity (Ident);

      if Rectype = Any_Type
        or else Rep_Item_Too_Early (Rectype, N)
      then
         return;
      else
         Rectype := Underlying_Type (Rectype);
      end if;

      --  First some basic error checks

      if not Is_Record_Type (Rectype) then
         Error_Msg_NE
           ("record type required, found}", Ident, First_Subtype (Rectype));
         return;

      elsif Is_Unchecked_Union (Rectype) then
         Error_Msg_N
           ("record rep clause not allowed for Unchecked_Union", N);

      elsif Scope (Rectype) /= Current_Scope then
         Error_Msg_N ("type must be declared in this scope", N);
         return;

      elsif not Is_First_Subtype (Rectype) then
         Error_Msg_N ("cannot give record rep clause for subtype", N);
         return;

      elsif Has_Record_Rep_Clause (Rectype) then
         Error_Msg_N ("duplicate record rep clause ignored", N);
         return;

      elsif Rep_Item_Too_Late (Rectype, N) then
         return;
      end if;

      if Present (Mod_Clause (N)) then
         declare
            Loc     : constant Source_Ptr := Sloc (N);
            M       : constant Node_Id := Mod_Clause (N);
            P       : constant List_Id := Pragmas_Before (M);
            Mod_Val : Uint;
            AtM_Nod : Node_Id;

         begin
            if Present (P) then
               Analyze_List (P);
            end if;

            --  In Tree_Output mode, expansion is disabled, but we must
            --  convert the Mod clause into an alignment clause anyway, so
            --  that the back-end can compute and back-annotate properly the
            --  size and alignment of types that may include this record.

            if Operating_Mode = Check_Semantics
              and then Tree_Output
            then
               AtM_Nod :=
                 Make_Attribute_Definition_Clause (Loc,
                   Name       => New_Reference_To (Base_Type (Rectype), Loc),
                   Chars      => Name_Alignment,
                   Expression => Relocate_Node (Expression (M)));

               Set_From_At_Mod (AtM_Nod);
               Insert_After (N, AtM_Nod);
               Mod_Val := Get_Alignment_Value (Expression (AtM_Nod));
               Set_Mod_Clause (N, Empty);

            else
               --  Get the alignment value to perform error checking

               Mod_Val := Get_Alignment_Value (Expression (M));

            end if;
         end;
      end if;

      --  Clear any existing component clauses for the type (this happens
      --  with derived types, where we are now overriding the original)

      Fent := First_Entity (Rectype);

      Comp := Fent;
      while Present (Comp) loop
         if Ekind (Comp) = E_Component
           or else Ekind (Comp) = E_Discriminant
         then
            Set_Component_Clause (Comp, Empty);
         end if;

         Next_Entity (Comp);
      end loop;

      --  All done if no component clauses

      CC := First (Component_Clauses (N));

      if No (CC) then
         return;
      end if;

      --  If a tag is present, then create a component clause that places
      --  it at the start of the record (otherwise gigi may place it after
      --  other fields that have rep clauses).

      if Nkind (Fent) = N_Defining_Identifier
        and then Chars (Fent) = Name_uTag
      then
         Set_Component_Bit_Offset    (Fent, Uint_0);
         Set_Normalized_Position     (Fent, Uint_0);
         Set_Normalized_First_Bit    (Fent, Uint_0);
         Set_Normalized_Position_Max (Fent, Uint_0);
         Init_Esize                  (Fent, System_Address_Size);

         Set_Component_Clause    (Fent,
           Make_Component_Clause (Loc,
             Component_Name =>
               Make_Identifier (Loc,
                 Chars => Name_uTag),

             Position  =>
               Make_Integer_Literal (Loc,
                 Intval => Uint_0),

             First_Bit =>
               Make_Integer_Literal (Loc,
                 Intval => Uint_0),

             Last_Bit  =>
               Make_Integer_Literal (Loc,
                 UI_From_Int (System_Address_Size))));

         Ccount := Ccount + 1;
      end if;

      Set_Has_Record_Rep_Clause (Rectype);
      Set_Has_Specified_Layout  (Rectype);

      --  A representation like this applies to the base type as well

      Set_Has_Record_Rep_Clause (Base_Type (Rectype));
      Set_Has_Non_Standard_Rep  (Base_Type (Rectype));
      Set_Has_Specified_Layout  (Base_Type (Rectype));

      Max_Bit_So_Far := Uint_Minus_1;
      Overlap_Check_Required := False;

      --  Process the component clauses

      while Present (CC) loop

         --  If pragma, just analyze it

         if Nkind (CC) = N_Pragma then
            Analyze (CC);

         --  Processing for real component clause

         else
            Ccount := Ccount + 1;
            Posit := Static_Integer (Position  (CC));
            Fbit  := Static_Integer (First_Bit (CC));
            Lbit  := Static_Integer (Last_Bit  (CC));

            if Posit /= No_Uint
              and then Fbit /= No_Uint
              and then Lbit /= No_Uint
            then
               if Posit < 0 then
                  Error_Msg_N
                    ("position cannot be negative", Position (CC));

               elsif Fbit < 0 then
                  Error_Msg_N
                    ("first bit cannot be negative", First_Bit (CC));

               --  Values look OK, so find the corresponding record component
               --  Even though the syntax allows an attribute reference for
               --  implementation-defined components, GNAT does not allow the
               --  tag to get an explicit position.

               elsif Nkind (Component_Name (CC)) = N_Attribute_Reference then

                  if Attribute_Name (Component_Name (CC)) = Name_Tag then
                     Error_Msg_N ("position of tag cannot be specified", CC);
                  else
                     Error_Msg_N ("illegal component name", CC);
                  end if;

               else
                  Comp := First_Entity (Rectype);
                  while Present (Comp) loop
                     exit when Chars (Comp) = Chars (Component_Name (CC));
                     Next_Entity (Comp);
                  end loop;

                  if No (Comp) then

                     --  Maybe component of base type that is absent from
                     --  statically constrained first subtype.

                     Comp := First_Entity (Base_Type (Rectype));
                     while Present (Comp) loop
                        exit when Chars (Comp) = Chars (Component_Name (CC));
                        Next_Entity (Comp);
                     end loop;
                  end if;

                  if No (Comp) then
                     Error_Msg_N
                       ("component clause is for non-existent field", CC);

                  elsif Present (Component_Clause (Comp)) then
                     Error_Msg_Sloc := Sloc (Component_Clause (Comp));
                     Error_Msg_N
                       ("component clause previously given#", CC);

                  else
                     --  Update Fbit and Lbit to the actual bit number.

                     Fbit := Fbit + UI_From_Int (SSU) * Posit;
                     Lbit := Lbit + UI_From_Int (SSU) * Posit;

                     if Fbit <= Max_Bit_So_Far then
                        Overlap_Check_Required := True;
                     else
                        Max_Bit_So_Far := Lbit;
                     end if;

                     if Has_Size_Clause (Rectype)
                       and then Esize (Rectype) <= Lbit
                     then
                        Error_Msg_N
                          ("bit number out of range of specified size",
                           Last_Bit (CC));
                     else
                        Set_Component_Clause     (Comp, CC);
                        Set_Component_Bit_Offset (Comp, Fbit);
                        Set_Esize                (Comp, 1 + (Lbit - Fbit));
                        Set_Normalized_First_Bit (Comp, Fbit mod SSU);
                        Set_Normalized_Position  (Comp, Fbit / SSU);

                        Set_Normalized_Position_Max
                          (Fent, Normalized_Position (Fent));

                        if Is_Tagged_Type (Rectype)
                          and then Fbit < System_Address_Size
                        then
                           Error_Msg_NE
                             ("component overlaps tag field of&",
                              CC, Rectype);
                        end if;

                        --  Test for large object that is not on a byte
                        --  boundary, defined as a large packed array not
                        --  represented by a modular type, or an object for
                        --  which a size of greater than 64 bits is specified.

                        if Fbit mod SSU /= 0 then
                           if (Is_Packed_Array_Type (Etype (Comp))
                                and then Is_Array_Type
                                     (Packed_Array_Type (Etype (Comp))))
                             or else Esize (Etype (Comp)) > 64
                           then
                              Error_Msg_N
                                ("large component must be on byte boundary",
                                 First_Bit (CC));
                           end if;
                        end if;

                        --  This information is also set in the
                        --  corresponding component of the base type,
                        --  found by accessing the Original_Record_Component
                        --  link if it is present.

                        Ocomp := Original_Record_Component (Comp);

                        if Hbit < Lbit then
                           Hbit := Lbit;
                        end if;

                        Check_Size
                          (Component_Name (CC),
                           Etype (Comp),
                           Esize (Comp),
                           Biased);

                        Set_Has_Biased_Representation (Comp, Biased);

                        if Present (Ocomp) then
                           Set_Component_Clause     (Ocomp, CC);
                           Set_Component_Bit_Offset (Ocomp, Fbit);
                           Set_Normalized_First_Bit (Ocomp, Fbit mod SSU);
                           Set_Normalized_Position  (Ocomp, Fbit / SSU);
                           Set_Esize                (Ocomp, 1 + (Lbit - Fbit));

                           Set_Normalized_Position_Max
                             (Ocomp, Normalized_Position (Ocomp));

                           Set_Has_Biased_Representation
                             (Ocomp, Has_Biased_Representation (Comp));
                        end if;

                        if Esize (Comp) < 0 then
                           Error_Msg_N ("component size is negative", CC);
                        end if;
                     end if;
                  end if;
               end if;
            end if;
         end if;

         Next (CC);
      end loop;

      --  Now that we have processed all the component clauses, check for
      --  overlap. We have to leave this till last, since the components
      --  can appear in any arbitrary order in the representation clause.

      --  We do not need this check if all specified ranges were monotonic,
      --  as recorded by Overlap_Check_Required being False at this stage.

      --  This first section checks if there are any overlapping entries
      --  at all. It does this by sorting all entries and then seeing if
      --  there are any overlaps. If there are none, then that is decisive,
      --  but if there are overlaps, they may still be OK (they may result
      --  from fields in different variants).

      if Overlap_Check_Required then
         Overlap_Check1 : declare

            OC_Fbit : array (0 .. Ccount) of Uint;
            --  First-bit values for component clauses, the value is the
            --  offset of the first bit of the field from start of record.
            --  The zero entry is for use in sorting.

            OC_Lbit : array (0 .. Ccount) of Uint;
            --  Last-bit values for component clauses, the value is the
            --  offset of the last bit of the field from start of record.
            --  The zero entry is for use in sorting.

            OC_Count : Natural := 0;
            --  Count of entries in OC_Fbit and OC_Lbit

            function OC_Lt (Op1, Op2 : Natural) return Boolean;
            --  Compare routine for Sort (See GNAT.Heap_Sort_A)

            procedure OC_Move (From : Natural; To : Natural);
            --  Move routine for Sort (see GNAT.Heap_Sort_A)

            function OC_Lt (Op1, Op2 : Natural) return Boolean is
            begin
               return OC_Fbit (Op1) < OC_Fbit (Op2);
            end OC_Lt;

            procedure OC_Move (From : Natural; To : Natural) is
            begin
               OC_Fbit (To) := OC_Fbit (From);
               OC_Lbit (To) := OC_Lbit (From);
            end OC_Move;

         begin
            CC := First (Component_Clauses (N));
            while Present (CC) loop
               if Nkind (CC) /= N_Pragma then
                  Posit := Static_Integer (Position  (CC));
                  Fbit  := Static_Integer (First_Bit (CC));
                  Lbit  := Static_Integer (Last_Bit  (CC));

                  if Posit /= No_Uint
                    and then Fbit /= No_Uint
                    and then Lbit /= No_Uint
                  then
                     OC_Count := OC_Count + 1;
                     Posit := Posit * SSU;
                     OC_Fbit (OC_Count) := Fbit + Posit;
                     OC_Lbit (OC_Count) := Lbit + Posit;
                  end if;
               end if;

               Next (CC);
            end loop;

            Sort
              (OC_Count,
               OC_Move'Unrestricted_Access,
               OC_Lt'Unrestricted_Access);

            Overlap_Check_Required := False;
            for J in 1 .. OC_Count - 1 loop
               if OC_Lbit (J) >= OC_Fbit (J + 1) then
                  Overlap_Check_Required := True;
                  exit;
               end if;
            end loop;
         end Overlap_Check1;
      end if;

      --  If Overlap_Check_Required is still True, then we have to do
      --  the full scale overlap check, since we have at least two fields
      --  that do overlap, and we need to know if that is OK since they
      --  are in the same variant, or whether we have a definite problem

      if Overlap_Check_Required then
         Overlap_Check2 : declare
            C1_Ent, C2_Ent : Entity_Id;
            --  Entities of components being checked for overlap

            Clist : Node_Id;
            --  Component_List node whose Component_Items are being checked

            Citem : Node_Id;
            --  Component declaration for component being checked

         begin
            C1_Ent := First_Entity (Base_Type (Rectype));

            --  Loop through all components in record. For each component check
            --  for overlap with any of the preceding elements on the component
            --  list containing the component, and also, if the component is in
            --  a variant, check against components outside the case structure.
            --  This latter test is repeated recursively up the variant tree.

            Main_Component_Loop : while Present (C1_Ent) loop
               if Ekind (C1_Ent) /= E_Component
                 and then Ekind (C1_Ent) /= E_Discriminant
               then
                  goto Continue_Main_Component_Loop;
               end if;

               --  Skip overlap check if entity has no declaration node. This
               --  happens with discriminants in constrained derived types.
               --  Probably we are missing some checks as a result, but that
               --  does not seem terribly serious ???

               if No (Declaration_Node (C1_Ent)) then
                  goto Continue_Main_Component_Loop;
               end if;

               Clist := Parent (List_Containing (Declaration_Node (C1_Ent)));

               --  Loop through component lists that need checking. Check the
               --  current component list and all lists in variants above us.

               Component_List_Loop : loop

                  --  If derived type definition, go to full declaration
                  --  If at outer level, check discriminants if there are any

                  if Nkind (Clist) = N_Derived_Type_Definition then
                     Clist := Parent (Clist);
                  end if;

                  --  Outer level of record definition, check discriminants

                  if Nkind (Clist) = N_Full_Type_Declaration
                    or else Nkind (Clist) = N_Private_Type_Declaration
                  then
                     if Has_Discriminants (Defining_Identifier (Clist)) then
                        C2_Ent :=
                          First_Discriminant (Defining_Identifier (Clist));

                        while Present (C2_Ent) loop
                           exit when C1_Ent = C2_Ent;
                           Check_Component_Overlap (C1_Ent, C2_Ent);
                           Next_Discriminant (C2_Ent);
                        end loop;
                     end if;

                  --  Record extension case

                  elsif Nkind (Clist) = N_Derived_Type_Definition then
                     Clist := Empty;

                  --  Otherwise check one component list

                  else
                     Citem := First (Component_Items (Clist));

                     while Present (Citem) loop
                        if Nkind (Citem) = N_Component_Declaration then
                           C2_Ent := Defining_Identifier (Citem);
                           exit when C1_Ent = C2_Ent;
                           Check_Component_Overlap (C1_Ent, C2_Ent);
                        end if;

                        Next (Citem);
                     end loop;
                  end if;

                  --  Check for variants above us (the parent of the Clist can
                  --  be a variant, in which case its parent is a variant part,
                  --  and the parent of the variant part is a component list
                  --  whose components must all be checked against the current
                  --  component for overlap.

                  if Nkind (Parent (Clist)) = N_Variant then
                     Clist := Parent (Parent (Parent (Clist)));

                  --  Check for possible discriminant part in record, this is
                  --  treated essentially as another level in the recursion.
                  --  For this case we have the parent of the component list
                  --  is the record definition, and its parent is the full
                  --  type declaration which contains the discriminant
                  --  specifications.

                  elsif Nkind (Parent (Clist)) = N_Record_Definition then
                     Clist := Parent (Parent ((Clist)));

                  --  If neither of these two cases, we are at the top of
                  --  the tree

                  else
                     exit Component_List_Loop;
                  end if;
               end loop Component_List_Loop;

               <<Continue_Main_Component_Loop>>
                  Next_Entity (C1_Ent);

            end loop Main_Component_Loop;
         end Overlap_Check2;
      end if;

      --  For records that have component clauses for all components, and
      --  whose size is less than or equal to 32, we need to know the size
      --  in the front end to activate possible packed array processing
      --  where the component type is a record.

      --  At this stage Hbit + 1 represents the first unused bit from all
      --  the component clauses processed, so if the component clauses are
      --  complete, then this is the length of the record.

      --  For records longer than System.Storage_Unit, and for those where
      --  not all components have component clauses, the back end determines
      --  the length (it may for example be appopriate to round up the size
      --  to some convenient boundary, based on alignment considerations etc).

      if Unknown_RM_Size (Rectype)
        and then Hbit + 1 <= 32
      then
         --  Nothing to do if at least one component with no component clause

         Comp := First_Entity (Rectype);
         while Present (Comp) loop
            if Ekind (Comp) = E_Component
              or else Ekind (Comp) = E_Discriminant
            then
               if No (Component_Clause (Comp)) then
                  return;
               end if;
            end if;

            Next_Entity (Comp);
         end loop;

         --  If we fall out of loop, all components have component clauses
         --  and so we can set the size to the maximum value.

         Set_RM_Size (Rectype, Hbit + 1);
      end if;

   end Analyze_Record_Representation_Clause;

   -----------------------------
   -- Check_Component_Overlap --
   -----------------------------

   procedure Check_Component_Overlap (C1_Ent, C2_Ent : Entity_Id) is
   begin
      if Present (Component_Clause (C1_Ent))
        and then Present (Component_Clause (C2_Ent))
      then
         --  Exclude odd case where we have two tag fields in the same
         --  record, both at location zero. This seems a bit strange,
         --  but it seems to happen in some circumstances ???

         if Chars (C1_Ent) = Name_uTag
           and then Chars (C2_Ent) = Name_uTag
         then
            return;
         end if;

         --  Here we check if the two fields overlap

         declare
            S1 : constant Uint := Component_Bit_Offset (C1_Ent);
            S2 : constant Uint := Component_Bit_Offset (C2_Ent);
            E1 : constant Uint := S1 + Esize (C1_Ent);
            E2 : constant Uint := S2 + Esize (C2_Ent);

         begin
            if E2 <= S1 or else E1 <= S2 then
               null;
            else
               Error_Msg_Node_2 :=
                 Component_Name (Component_Clause (C2_Ent));
               Error_Msg_Sloc := Sloc (Error_Msg_Node_2);
               Error_Msg_Node_1 :=
                 Component_Name (Component_Clause (C1_Ent));
               Error_Msg_N
                 ("component& overlaps & #",
                  Component_Name (Component_Clause (C1_Ent)));
            end if;
         end;
      end if;
   end Check_Component_Overlap;

   -----------------------------------
   -- Check_Constant_Address_Clause --
   -----------------------------------

   procedure Check_Constant_Address_Clause
     (Expr  : Node_Id;
      U_Ent : Entity_Id)
   is
      procedure Check_At_Constant_Address (Nod : Node_Id);
      --  Checks that the given node N represents a name whose 'Address
      --  is constant (in the same sense as OK_Constant_Address_Clause,
      --  i.e. the address value is the same at the point of declaration
      --  of U_Ent and at the time of elaboration of the address clause.

      procedure Check_Expr_Constants (Nod : Node_Id);
      --  Checks that Nod meets the requirements for a constant address
      --  clause in the sense of the enclosing procedure.

      procedure Check_List_Constants (Lst : List_Id);
      --  Check that all elements of list Lst meet the requirements for a
      --  constant address clause in the sense of the enclosing procedure.

      -------------------------------
      -- Check_At_Constant_Address --
      -------------------------------

      procedure Check_At_Constant_Address (Nod : Node_Id) is
      begin
         if Is_Entity_Name (Nod) then
            if Present (Address_Clause (Entity ((Nod)))) then
               Error_Msg_NE
                 ("invalid address clause for initialized object &!",
                           Nod, U_Ent);
               Error_Msg_NE
                 ("address for& cannot" &
                    " depend on another address clause! ('R'M 13.1(22))!",
                  Nod, U_Ent);

            elsif In_Same_Source_Unit (Entity (Nod), U_Ent)
              and then Sloc (U_Ent) < Sloc (Entity (Nod))
            then
               Error_Msg_NE
                 ("invalid address clause for initialized object &!",
                  Nod, U_Ent);
               Error_Msg_Name_1 := Chars (Entity (Nod));
               Error_Msg_Name_2 := Chars (U_Ent);
               Error_Msg_N
                 ("\% must be defined before % ('R'M 13.1(22))!",
                  Nod);
            end if;

         elsif Nkind (Nod) = N_Selected_Component then
            declare
               T : constant Entity_Id := Etype (Prefix (Nod));

            begin
               if (Is_Record_Type (T)
                    and then Has_Discriminants (T))
                 or else
                  (Is_Access_Type (T)
                     and then Is_Record_Type (Designated_Type (T))
                     and then Has_Discriminants (Designated_Type (T)))
               then
                  Error_Msg_NE
                    ("invalid address clause for initialized object &!",
                     Nod, U_Ent);
                  Error_Msg_N
                    ("\address cannot depend on component" &
                     " of discriminated record ('R'M 13.1(22))!",
                     Nod);
               else
                  Check_At_Constant_Address (Prefix (Nod));
               end if;
            end;

         elsif Nkind (Nod) = N_Indexed_Component then
            Check_At_Constant_Address (Prefix (Nod));
            Check_List_Constants (Expressions (Nod));

         else
            Check_Expr_Constants (Nod);
         end if;
      end Check_At_Constant_Address;

      --------------------------
      -- Check_Expr_Constants --
      --------------------------

      procedure Check_Expr_Constants (Nod : Node_Id) is
      begin
         if Nkind (Nod) in N_Has_Etype
           and then Etype (Nod) = Any_Type
         then
            return;
         end if;

         case Nkind (Nod) is
            when N_Empty | N_Error =>
               return;

            when N_Identifier | N_Expanded_Name =>
               declare
                  Ent       : constant Entity_Id  := Entity (Nod);
                  Loc_Ent   : constant Source_Ptr := Sloc (Ent);
                  Loc_U_Ent : constant Source_Ptr := Sloc (U_Ent);

               begin
                  if Ekind (Ent) = E_Named_Integer
                       or else
                     Ekind (Ent) = E_Named_Real
                       or else
                     Is_Type (Ent)
                  then
                     return;

                  elsif
                     Ekind (Ent) = E_Constant
                       or else
                     Ekind (Ent) = E_In_Parameter
                  then
                     --  This is the case where we must have Ent defined
                     --  before U_Ent. Clearly if they are in different
                     --  units this requirement is met since the unit
                     --  containing Ent is already processed.

                     if not In_Same_Source_Unit (Ent, U_Ent) then
                        return;

                     --  Otherwise location of Ent must be before the
                     --  location of U_Ent, that's what prior defined means.

                     elsif Loc_Ent < Loc_U_Ent then
                        return;

                     else
                        Error_Msg_NE
                          ("invalid address clause for initialized object &!",
                           Nod, U_Ent);
                        Error_Msg_Name_1 := Chars (Ent);
                        Error_Msg_Name_2 := Chars (U_Ent);
                        Error_Msg_N
                          ("\% must be defined before % ('R'M 13.1(22))!",
                           Nod);
                     end if;

                  elsif Nkind (Original_Node (Nod)) = N_Function_Call then
                     Check_Expr_Constants (Original_Node (Nod));

                  else
                     Error_Msg_NE
                       ("invalid address clause for initialized object &!",
                        Nod, U_Ent);
                     Error_Msg_Name_1 := Chars (Ent);
                     Error_Msg_N
                       ("\reference to variable% not allowed ('R'M 13.1(22))!",
                        Nod);
                  end if;
               end;

            when N_Integer_Literal   |
                 N_Real_Literal      |
                 N_String_Literal    |
                 N_Character_Literal =>
               return;

            when N_Range =>
               Check_Expr_Constants (Low_Bound (Nod));
               Check_Expr_Constants (High_Bound (Nod));

            when N_Explicit_Dereference =>
               Check_Expr_Constants (Prefix (Nod));

            when N_Indexed_Component =>
               Check_Expr_Constants (Prefix (Nod));
               Check_List_Constants (Expressions (Nod));

            when N_Slice =>
               Check_Expr_Constants (Prefix (Nod));
               Check_Expr_Constants (Discrete_Range (Nod));

            when N_Selected_Component =>
               Check_Expr_Constants (Prefix (Nod));

            when N_Attribute_Reference =>

               if (Attribute_Name (Nod) = Name_Address
                    or else
                   Attribute_Name (Nod) = Name_Access
                    or else
                   Attribute_Name (Nod) = Name_Unchecked_Access
                    or else
                   Attribute_Name (Nod) = Name_Unrestricted_Access)
               then
                  Check_At_Constant_Address (Prefix (Nod));

               else
                  Check_Expr_Constants (Prefix (Nod));
                  Check_List_Constants (Expressions (Nod));
               end if;

            when N_Aggregate =>
               Check_List_Constants (Component_Associations (Nod));
               Check_List_Constants (Expressions (Nod));

            when N_Component_Association =>
               Check_Expr_Constants (Expression (Nod));

            when N_Extension_Aggregate =>
               Check_Expr_Constants (Ancestor_Part (Nod));
               Check_List_Constants (Component_Associations (Nod));
               Check_List_Constants (Expressions (Nod));

            when N_Null =>
               return;

            when N_Binary_Op | N_And_Then | N_Or_Else | N_In | N_Not_In =>
               Check_Expr_Constants (Left_Opnd (Nod));
               Check_Expr_Constants (Right_Opnd (Nod));

            when N_Unary_Op =>
               Check_Expr_Constants (Right_Opnd (Nod));

            when N_Type_Conversion           |
                 N_Qualified_Expression      |
                 N_Allocator                 =>
               Check_Expr_Constants (Expression (Nod));

            when N_Unchecked_Type_Conversion =>
               Check_Expr_Constants (Expression (Nod));

               --  If this is a rewritten unchecked conversion, subtypes
               --  in this node are those created within the instance.
               --  To avoid order of elaboration issues, replace them
               --  with their base types. Note that address clauses can
               --  cause order of elaboration problems because they are
               --  elaborated by the back-end at the point of definition,
               --  and may mention entities declared in between (as long
               --  as everything is static). It is user-friendly to allow
               --  unchecked conversions in this context.

               if Nkind (Original_Node (Nod)) = N_Function_Call then
                  Set_Etype (Expression (Nod),
                    Base_Type (Etype (Expression (Nod))));
                  Set_Etype (Nod, Base_Type (Etype (Nod)));
               end if;

            when N_Function_Call =>
               if not Is_Pure (Entity (Name (Nod))) then
                  Error_Msg_NE
                    ("invalid address clause for initialized object &!",
                     Nod, U_Ent);

                  Error_Msg_NE
                    ("\function & is not pure ('R'M 13.1(22))!",
                     Nod, Entity (Name (Nod)));

               else
                  Check_List_Constants (Parameter_Associations (Nod));
               end if;

            when N_Parameter_Association =>
               Check_Expr_Constants (Explicit_Actual_Parameter (Nod));

            when others =>
               Error_Msg_NE
                 ("invalid address clause for initialized object &!",
                  Nod, U_Ent);
               Error_Msg_NE
                 ("\must be constant defined before& ('R'M 13.1(22))!",
                  Nod, U_Ent);
         end case;
      end Check_Expr_Constants;

      --------------------------
      -- Check_List_Constants --
      --------------------------

      procedure Check_List_Constants (Lst : List_Id) is
         Nod1 : Node_Id;

      begin
         if Present (Lst) then
            Nod1 := First (Lst);
            while Present (Nod1) loop
               Check_Expr_Constants (Nod1);
               Next (Nod1);
            end loop;
         end if;
      end Check_List_Constants;

   --  Start of processing for Check_Constant_Address_Clause

   begin
      Check_Expr_Constants (Expr);
   end Check_Constant_Address_Clause;

   ----------------
   -- Check_Size --
   ----------------

   procedure Check_Size
     (N      : Node_Id;
      T      : Entity_Id;
      Siz    : Uint;
      Biased : out Boolean)
   is
      UT : constant Entity_Id := Underlying_Type (T);
      M  : Uint;

   begin
      Biased := False;

      --  Immediate return if size is same as standard size or if composite
      --  item, or generic type, or type with previous errors.

      if No (UT)
        or else UT = Any_Type
        or else Is_Generic_Type (UT)
        or else Is_Generic_Type (Root_Type (UT))
        or else Is_Composite_Type (UT)
        or else (Known_Esize (UT) and then Siz = Esize (UT))
      then
         return;

      --  For fixed-point types, don't check minimum if type is not frozen,
      --  since type is not known till then
      --  at freeze time.

      elsif Is_Fixed_Point_Type (UT)
        and then not Is_Frozen (UT)
      then
         null;

      --  Cases for which a minimum check is required

      else
         M := UI_From_Int (Minimum_Size (UT));

         if Siz < M then

            --  Size is less than minimum size, but one possibility remains
            --  that we can manage with the new size if we bias the type

            M := UI_From_Int (Minimum_Size (UT, Biased => True));

            if Siz < M then
               Error_Msg_Uint_1 := M;
               Error_Msg_NE
                 ("size for& too small, minimum allowed is ^", N, T);
            else
               Biased := True;
            end if;
         end if;
      end if;
   end Check_Size;

   -------------------------
   -- Get_Alignment_Value --
   -------------------------

   function Get_Alignment_Value (Expr : Node_Id) return Uint is
      Align : constant Uint := Static_Integer (Expr);

   begin
      if Align = No_Uint then
         return No_Uint;

      elsif Align <= 0 then
         Error_Msg_N ("alignment value must be positive", Expr);
         return No_Uint;

      else
         for J in Int range 0 .. 64 loop
            declare
               M : constant Uint := Uint_2 ** J;

            begin
               exit when M = Align;

               if M > Align then
                  Error_Msg_N
                    ("alignment value must be power of 2", Expr);
                  return No_Uint;
               end if;
            end;
         end loop;

         return Align;
      end if;
   end Get_Alignment_Value;

   -------------------------------------
   -- Get_Attribute_Definition_Clause --
   -------------------------------------

   function Get_Attribute_Definition_Clause
     (E    : Entity_Id;
      Id   : Attribute_Id)
      return Node_Id
   is
      N : Node_Id;

   begin
      N := First_Rep_Item (E);
      while Present (N) loop
         if Nkind (N) = N_Attribute_Definition_Clause
           and then Get_Attribute_Id (Chars (N)) = Id
         then
            return N;
         else
            Next_Rep_Item (N);
         end if;
      end loop;

      return Empty;
   end Get_Attribute_Definition_Clause;

   --------------------
   -- Get_Rep_Pragma --
   --------------------

   function Get_Rep_Pragma (E : Entity_Id; Nam : Name_Id) return Node_Id is
      N   : Node_Id;
      Typ : Entity_Id;

   begin
      N := First_Rep_Item (E);

      while Present (N) loop
         if Nkind (N) = N_Pragma and then Chars (N) = Nam then

            if Nam = Name_Stream_Convert then

               --  For tagged types this pragma is not inherited, so we
               --  must verify that it is defined for the given type and
               --  not an ancestor.

               Typ := Entity (Expression
                       (First (Pragma_Argument_Associations (N))));

               if not Is_Tagged_Type (E)
                 or else E = Typ
                 or else (Is_Private_Type (Typ)
                           and then E = Full_View (Typ))
               then
                  return N;
               else
                  Next_Rep_Item (N);
               end if;

            else
               return N;
            end if;
         else
            Next_Rep_Item (N);
         end if;
      end loop;

      return Empty;
   end Get_Rep_Pragma;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Unchecked_Conversions.Init;
   end Initialize;

   -------------------------
   -- Is_Operational_Item --
   -------------------------

   function Is_Operational_Item (N : Node_Id) return Boolean is
   begin
      if Nkind (N) /= N_Attribute_Definition_Clause then
         return False;
      else
         declare
            Id    : constant Attribute_Id := Get_Attribute_Id (Chars (N));

         begin
            return Id = Attribute_Input
              or else Id = Attribute_Output
              or else Id = Attribute_Read
              or else Id = Attribute_Write;
         end;
      end if;
   end Is_Operational_Item;

   ------------------
   -- Minimum_Size --
   ------------------

   function Minimum_Size
     (T      : Entity_Id;
      Biased : Boolean := False)
      return   Nat
   is
      Lo     : Uint    := No_Uint;
      Hi     : Uint    := No_Uint;
      LoR    : Ureal   := No_Ureal;
      HiR    : Ureal   := No_Ureal;
      LoSet  : Boolean := False;
      HiSet  : Boolean := False;
      B      : Uint;
      S      : Nat;
      Ancest : Entity_Id;

   begin
      --  If bad type, return 0

      if T = Any_Type then
         return 0;

      --  For generic types, just return zero. There cannot be any legitimate
      --  need to know such a size, but this routine may be called with a
      --  generic type as part of normal processing.

      elsif Is_Generic_Type (Root_Type (T)) then
         return 0;

      --  Access types

      elsif Is_Access_Type (T) then
         return System_Address_Size;

      --  Floating-point types

      elsif Is_Floating_Point_Type (T) then
         return UI_To_Int (Esize (Root_Type (T)));

      --  Discrete types

      elsif Is_Discrete_Type (T) then

         --  The following loop is looking for the nearest compile time
         --  known bounds following the ancestor subtype chain. The idea
         --  is to find the most restrictive known bounds information.

         Ancest := T;
         loop
            if Ancest = Any_Type or else Etype (Ancest) = Any_Type then
               return 0;
            end if;

            if not LoSet then
               if Compile_Time_Known_Value (Type_Low_Bound (Ancest)) then
                  Lo := Expr_Rep_Value (Type_Low_Bound (Ancest));
                  LoSet := True;
                  exit when HiSet;
               end if;
            end if;

            if not HiSet then
               if Compile_Time_Known_Value (Type_High_Bound (Ancest)) then
                  Hi := Expr_Rep_Value (Type_High_Bound (Ancest));
                  HiSet := True;
                  exit when LoSet;
               end if;
            end if;

            Ancest := Ancestor_Subtype (Ancest);

            if No (Ancest) then
               Ancest := Base_Type (T);

               if Is_Generic_Type (Ancest) then
                  return 0;
               end if;
            end if;
         end loop;

      --  Fixed-point types. We can't simply use Expr_Value to get the
      --  Corresponding_Integer_Value values of the bounds, since these
      --  do not get set till the type is frozen, and this routine can
      --  be called before the type is frozen. Similarly the test for
      --  bounds being static needs to include the case where we have
      --  unanalyzed real literals for the same reason.

      elsif Is_Fixed_Point_Type (T) then

         --  The following loop is looking for the nearest compile time
         --  known bounds following the ancestor subtype chain. The idea
         --  is to find the most restrictive known bounds information.

         Ancest := T;
         loop
            if Ancest = Any_Type or else Etype (Ancest) = Any_Type then
               return 0;
            end if;

            if not LoSet then
               if Nkind (Type_Low_Bound (Ancest)) = N_Real_Literal
                 or else Compile_Time_Known_Value (Type_Low_Bound (Ancest))
               then
                  LoR := Expr_Value_R (Type_Low_Bound (Ancest));
                  LoSet := True;
                  exit when HiSet;
               end if;
            end if;

            if not HiSet then
               if Nkind (Type_High_Bound (Ancest)) = N_Real_Literal
                 or else Compile_Time_Known_Value (Type_High_Bound (Ancest))
               then
                  HiR := Expr_Value_R (Type_High_Bound (Ancest));
                  HiSet := True;
                  exit when LoSet;
               end if;
            end if;

            Ancest := Ancestor_Subtype (Ancest);

            if No (Ancest) then
               Ancest := Base_Type (T);

               if Is_Generic_Type (Ancest) then
                  return 0;
               end if;
            end if;
         end loop;

         Lo := UR_To_Uint (LoR / Small_Value (T));
         Hi := UR_To_Uint (HiR / Small_Value (T));

      --  No other types allowed

      else
         raise Program_Error;
      end if;

      --  Fall through with Hi and Lo set. Deal with biased case.

      if (Biased and then not Is_Fixed_Point_Type (T))
        or else Has_Biased_Representation (T)
      then
         Hi := Hi - Lo;
         Lo := Uint_0;
      end if;

      --  Signed case. Note that we consider types like range 1 .. -1 to be
      --  signed for the purpose of computing the size, since the bounds
      --  have to be accomodated in the base type.

      if Lo < 0 or else Hi < 0 then
         S := 1;
         B := Uint_1;

         --  S = size, B = 2 ** (size - 1) (can accommodate -B .. +(B - 1))
         --  Note that we accommodate the case where the bounds cross. This
         --  can happen either because of the way the bounds are declared
         --  or because of the algorithm in Freeze_Fixed_Point_Type.

         while Lo < -B
           or else Hi < -B
           or else Lo >= B
           or else Hi >= B
         loop
            B := Uint_2 ** S;
            S := S + 1;
         end loop;

      --  Unsigned case

      else
         --  If both bounds are positive, make sure that both are represen-
         --  table in the case where the bounds are crossed. This can happen
         --  either because of the way the bounds are declared, or because of
         --  the algorithm in Freeze_Fixed_Point_Type.

         if Lo > Hi then
            Hi := Lo;
         end if;

         --  S = size, (can accommodate 0 .. (2**size - 1))

         S := 0;
         while Hi >= Uint_2 ** S loop
            S := S + 1;
         end loop;
      end if;

      return S;
   end Minimum_Size;

   -------------------------
   -- New_Stream_Function --
   -------------------------

   procedure New_Stream_Function
     (N    : Node_Id;
      Ent  : Entity_Id;
      Subp : Entity_Id;
      Nam  : Name_Id)
   is
      Loc       : constant Source_Ptr := Sloc (N);
      Subp_Id   : Entity_Id := Make_Defining_Identifier (Loc, Nam);
      Subp_Decl : Node_Id;
      F         : Entity_Id;
      Etyp      : Entity_Id;

   begin
      F        := First_Formal (Subp);
      Etyp     := Etype (Subp);

      Subp_Decl :=
        Make_Subprogram_Renaming_Declaration (Loc,
          Specification =>

            Make_Function_Specification (Loc,
              Defining_Unit_Name => Subp_Id,
              Parameter_Specifications =>
                New_List (
                  Make_Parameter_Specification (Loc,
                    Defining_Identifier =>
                      Make_Defining_Identifier (Loc, Name_S),
                    Parameter_Type =>
                      Make_Access_Definition (Loc,
                        Subtype_Mark =>
                          New_Reference_To (
                            Designated_Type (Etype (F)), Loc)))),

              Subtype_Mark =>
                New_Reference_To (Etyp, Loc)),

        Name => New_Reference_To (Subp, Loc));

      if Is_Tagged_Type (Ent) and then not Is_Limited_Type (Ent) then
         Set_TSS (Base_Type (Ent), Subp_Id);
      else
         Insert_Action (N, Subp_Decl);
         Copy_TSS (Subp_Id, Base_Type (Ent));
      end if;

   end New_Stream_Function;

   --------------------------
   -- New_Stream_Procedure --
   --------------------------

   procedure New_Stream_Procedure
     (N     : Node_Id;
      Ent   : Entity_Id;
      Subp  : Entity_Id;
      Nam   : Name_Id;
      Out_P : Boolean := False)
   is
      Loc       : constant Source_Ptr := Sloc (N);
      Subp_Id   : Entity_Id := Make_Defining_Identifier (Loc, Nam);
      Subp_Decl : Node_Id;
      F         : Entity_Id;
      Etyp      : Entity_Id;

   begin
      F        := First_Formal (Subp);
      Etyp     := Etype (Next_Formal (F));

      Subp_Decl :=
        Make_Subprogram_Renaming_Declaration (Loc,
          Specification =>

            Make_Procedure_Specification (Loc,
              Defining_Unit_Name => Subp_Id,
              Parameter_Specifications =>
                New_List (
                  Make_Parameter_Specification (Loc,
                    Defining_Identifier =>
                      Make_Defining_Identifier (Loc, Name_S),
                    Parameter_Type =>
                      Make_Access_Definition (Loc,
                        Subtype_Mark =>
                          New_Reference_To (
                            Designated_Type (Etype (F)), Loc))),

                  Make_Parameter_Specification (Loc,
                    Defining_Identifier =>
                      Make_Defining_Identifier (Loc, Name_V),
                    Out_Present => Out_P,
                    Parameter_Type =>
                      New_Reference_To (Etyp, Loc)))),
        Name => New_Reference_To (Subp, Loc));

      if Is_Tagged_Type (Ent) and then not Is_Limited_Type (Ent) then
         Set_TSS (Base_Type (Ent), Subp_Id);
      else
         Insert_Action (N, Subp_Decl);
         Copy_TSS (Subp_Id, Base_Type (Ent));
      end if;

   end New_Stream_Procedure;

   ---------------------
   -- Record_Rep_Item --
   ---------------------

   procedure Record_Rep_Item (T : Entity_Id; N : Node_Id) is
   begin
      Set_Next_Rep_Item (N, First_Rep_Item (T));
      Set_First_Rep_Item (T, N);
   end Record_Rep_Item;

   ------------------------
   -- Rep_Item_Too_Early --
   ------------------------

   function Rep_Item_Too_Early
     (T     : Entity_Id;
      N     : Node_Id)
      return  Boolean
   is
   begin
      --  Cannot apply rep items to generic types

      if Is_Type (T)
        and then Is_Generic_Type (Root_Type (T))
      then
         Error_Msg_N
           ("representation item not allowed for generic type", N);
         return True;
      end if;

      --  Otherwise check for incompleted type

      if Is_Incomplete_Or_Private_Type (T)
        and then No (Underlying_Type (T))
      then
         Error_Msg_N
           ("representation item must be after full type declaration", N);
         return True;

      --  If the type has incompleted components, a representation clause is
      --  illegal but stream attributes and Convention pragmas are correct.

      elsif Has_Private_Component (T) then
         if (Nkind (N) = N_Pragma or else Is_Operational_Item (N)) then
            return False;
         else
            Error_Msg_N
              ("representation item must appear after type is fully defined",
                N);
            return True;
         end if;
      else
         return False;
      end if;
   end Rep_Item_Too_Early;

   -----------------------
   -- Rep_Item_Too_Late --
   -----------------------

   function Rep_Item_Too_Late
     (T     : Entity_Id;
      N     : Node_Id;
      FOnly : Boolean := False)
      return  Boolean
   is
      S           : Entity_Id;
      Parent_Type : Entity_Id;

      procedure Too_Late;
      --  Output the too late message

      procedure Too_Late is
      begin
         Error_Msg_N ("representation item appears too late!", N);
      end Too_Late;

   --  Start of processing for Rep_Item_Too_Late

   begin
      --  First make sure entity is not frozen (RM 13.1(9)). Exclude imported
      --  types, which may be frozen if they appear in a representation clause
      --  for a local type.

      if Is_Frozen (T)
        and then not From_With_Type (T)
      then
         Too_Late;
         S := First_Subtype (T);

         if Present (Freeze_Node (S)) then
            Error_Msg_NE
              ("?no more representation items for }!", Freeze_Node (S), S);
         end if;

         return True;

      --  Check for case of non-tagged derived type whose parent either has
      --  primitive operations, or is a by reference type (RM 13.1(10)).

      elsif Is_Type (T)
        and then not FOnly
        and then Is_Derived_Type (T)
        and then not Is_Tagged_Type (T)
      then
         Parent_Type := Etype (Base_Type (T));

         if Has_Primitive_Operations (Parent_Type) then
            Too_Late;
            Error_Msg_NE
              ("primitive operations already defined for&!", N, Parent_Type);
            return True;

         elsif Is_By_Reference_Type (Parent_Type) then
            Too_Late;
            Error_Msg_NE
              ("parent type & is a by reference type!", N, Parent_Type);
            return True;
         end if;
      end if;

      --  No error, link item into head of chain of rep items for the entity

      Record_Rep_Item (T, N);
      return False;
   end Rep_Item_Too_Late;

   -------------------------
   -- Same_Representation --
   -------------------------

   function Same_Representation (Typ1, Typ2 : Entity_Id) return Boolean is
      T1 : constant Entity_Id := Underlying_Type (Typ1);
      T2 : constant Entity_Id := Underlying_Type (Typ2);

   begin
      --  A quick check, if base types are the same, then we definitely have
      --  the same representation, because the subtype specific representation
      --  attributes (Size and Alignment) do not affect representation from
      --  the point of view of this test.

      if Base_Type (T1) = Base_Type (T2) then
         return True;

      elsif Is_Private_Type (Base_Type (T2))
        and then Base_Type (T1) = Full_View (Base_Type (T2))
      then
         return True;
      end if;

      --  Tagged types never have differing representations

      if Is_Tagged_Type (T1) then
         return True;
      end if;

      --  Representations are definitely different if conventions differ

      if Convention (T1) /= Convention (T2) then
         return False;
      end if;

      --  Representations are different if component alignments differ

      if (Is_Record_Type (T1) or else Is_Array_Type (T1))
        and then
         (Is_Record_Type (T2) or else Is_Array_Type (T2))
        and then Component_Alignment (T1) /= Component_Alignment (T2)
      then
         return False;
      end if;

      --  For arrays, the only real issue is component size. If we know the
      --  component size for both arrays, and it is the same, then that's
      --  good enough to know we don't have a change of representation.

      if Is_Array_Type (T1) then
         if Known_Component_Size (T1)
           and then Known_Component_Size (T2)
           and then Component_Size (T1) = Component_Size (T2)
         then
            return True;
         end if;
      end if;

      --  Types definitely have same representation if neither has non-standard
      --  representation since default representations are always consistent.
      --  If only one has non-standard representation, and the other does not,
      --  then we consider that they do not have the same representation. They
      --  might, but there is no way of telling early enough.

      if Has_Non_Standard_Rep (T1) then
         if not Has_Non_Standard_Rep (T2) then
            return False;
         end if;
      else
         return not Has_Non_Standard_Rep (T2);
      end if;

      --  Here the two types both have non-standard representation, and we
      --  need to determine if they have the same non-standard representation

      --  For arrays, we simply need to test if the component sizes are the
      --  same. Pragma Pack is reflected in modified component sizes, so this
      --  check also deals with pragma Pack.

      if Is_Array_Type (T1) then
         return Component_Size (T1) = Component_Size (T2);

      --  Tagged types always have the same representation, because it is not
      --  possible to specify different representations for common fields.

      elsif Is_Tagged_Type (T1) then
         return True;

      --  Case of record types

      elsif Is_Record_Type (T1) then

         --  Packed status must conform

         if Is_Packed (T1) /= Is_Packed (T2) then
            return False;

         --  Otherwise we must check components. Typ2 maybe a constrained
         --  subtype with fewer components, so we compare the components
         --  of the base types.

         else
            Record_Case : declare
               CD1, CD2 : Entity_Id;

               function Same_Rep return Boolean;
               --  CD1 and CD2 are either components or discriminants. This
               --  function tests whether the two have the same representation

               function Same_Rep return Boolean is
               begin
                  if No (Component_Clause (CD1)) then
                     return No (Component_Clause (CD2));

                  else
                     return
                        Present (Component_Clause (CD2))
                          and then
                        Component_Bit_Offset (CD1) = Component_Bit_Offset (CD2)
                          and then
                        Esize (CD1) = Esize (CD2);
                  end if;
               end Same_Rep;

            --  Start processing for Record_Case

            begin
               if Has_Discriminants (T1) then
                  CD1 := First_Discriminant (T1);
                  CD2 := First_Discriminant (T2);

                  while Present (CD1) loop
                     if not Same_Rep then
                        return False;
                     else
                        Next_Discriminant (CD1);
                        Next_Discriminant (CD2);
                     end if;
                  end loop;
               end if;

               CD1 := First_Component (Underlying_Type (Base_Type (T1)));
               CD2 := First_Component (Underlying_Type (Base_Type (T2)));

               while Present (CD1) loop
                  if not Same_Rep then
                     return False;
                  else
                     Next_Component (CD1);
                     Next_Component (CD2);
                  end if;
               end loop;

               return True;
            end Record_Case;
         end if;

      --  For enumeration types, we must check each literal to see if the
      --  representation is the same. Note that we do not permit enumeration
      --  representation clauses for Character and Wide_Character, so these
      --  cases were already dealt with.

      elsif Is_Enumeration_Type (T1) then

         Enumeration_Case : declare
            L1, L2 : Entity_Id;

         begin
            L1 := First_Literal (T1);
            L2 := First_Literal (T2);

            while Present (L1) loop
               if Enumeration_Rep (L1) /= Enumeration_Rep (L2) then
                  return False;
               else
                  Next_Literal (L1);
                  Next_Literal (L2);
               end if;
            end loop;

            return True;

         end Enumeration_Case;

      --  Any other types have the same representation for these purposes

      else
         return True;
      end if;

   end Same_Representation;

   --------------------
   -- Set_Enum_Esize --
   --------------------

   procedure Set_Enum_Esize (T : Entity_Id) is
      Lo : Uint;
      Hi : Uint;
      Sz : Nat;

   begin
      Init_Alignment (T);

      --  Find the minimum standard size (8,16,32,64) that fits

      Lo := Enumeration_Rep (Entity (Type_Low_Bound (T)));
      Hi := Enumeration_Rep (Entity (Type_High_Bound (T)));

      if Lo < 0 then
         if Lo >= -Uint_2**07 and then Hi < Uint_2**07 then
            Sz := 8;

         elsif Lo >= -Uint_2**15 and then Hi < Uint_2**15 then
            Sz := 16;

         elsif Lo >= -Uint_2**31 and then Hi < Uint_2**31 then
            Sz := 32;

         else pragma Assert (Lo >= -Uint_2**63 and then Hi < Uint_2**63);
            Sz := 64;
         end if;

      else
         if Hi < Uint_2**08 then
            Sz := 8;

         elsif Hi < Uint_2**16 then
            Sz := 16;

         elsif Hi < Uint_2**32 then
            Sz := 32;

         else pragma Assert (Hi < Uint_2**63);
            Sz := 64;
         end if;
      end if;

      --  That minimum is the proper size unless we have a foreign convention
      --  and the size required is 32 or less, in which case we bump the size
      --  up to 32. This is required for C and C++ and seems reasonable for
      --  all other foreign conventions.

      if Has_Foreign_Convention (T)
        and then Esize (T) < Standard_Integer_Size
      then
         Init_Esize (T, Standard_Integer_Size);

      else
         Init_Esize (T, Sz);
      end if;

   end Set_Enum_Esize;

   -----------------------------------
   -- Validate_Unchecked_Conversion --
   -----------------------------------

   procedure Validate_Unchecked_Conversion
     (N        : Node_Id;
      Act_Unit : Entity_Id)
   is
      Source : Entity_Id;
      Target : Entity_Id;
      Vnode  : Node_Id;

   begin
      --  Obtain source and target types. Note that we call Ancestor_Subtype
      --  here because the processing for generic instantiation always makes
      --  subtypes, and we want the original frozen actual types.

      --  If we are dealing with private types, then do the check on their
      --  fully declared counterparts if the full declarations have been
      --  encountered (they don't have to be visible, but they must exist!)

      Source := Ancestor_Subtype (Etype (First_Formal (Act_Unit)));

      if Is_Private_Type (Source)
        and then Present (Underlying_Type (Source))
      then
         Source := Underlying_Type (Source);
      end if;

      Target := Ancestor_Subtype (Etype (Act_Unit));

      --  If either type is generic, the instantiation happens within a
      --  generic unit, and there is nothing to check. The proper check
      --  will happen when the enclosing generic is instantiated.

      if Is_Generic_Type (Source) or else Is_Generic_Type (Target) then
         return;
      end if;

      if Is_Private_Type (Target)
        and then Present (Underlying_Type (Target))
      then
         Target := Underlying_Type (Target);
      end if;

      --  Source may be unconstrained array, but not target

      if Is_Array_Type (Target)
        and then not Is_Constrained (Target)
      then
         Error_Msg_N
           ("unchecked conversion to unconstrained array not allowed", N);
         return;
      end if;

      --  Make entry in unchecked conversion table for later processing
      --  by Validate_Unchecked_Conversions, which will check sizes and
      --  alignments (using values set by the back-end where possible).

      Unchecked_Conversions.Append
        (New_Val => UC_Entry'
           (Enode  => N,
            Source => Source,
            Target => Target));

      --  Generate N_Validate_Unchecked_Conversion node for back end if
      --  the back end needs to perform special validation checks. At the
      --  current time, only the JVM version requires such checks.

      if Java_VM then
         Vnode :=
           Make_Validate_Unchecked_Conversion (Sloc (N));
         Set_Source_Type (Vnode, Source);
         Set_Target_Type (Vnode, Target);
         Insert_After (N, Vnode);
      end if;
   end Validate_Unchecked_Conversion;

   ------------------------------------
   -- Validate_Unchecked_Conversions --
   ------------------------------------

   procedure Validate_Unchecked_Conversions is
   begin
      for N in Unchecked_Conversions.First .. Unchecked_Conversions.Last loop
         declare
            T : UC_Entry renames Unchecked_Conversions.Table (N);

            Enode  : constant Node_Id   := T.Enode;
            Source : constant Entity_Id := T.Source;
            Target : constant Entity_Id := T.Target;

            Source_Siz    : Uint;
            Target_Siz    : Uint;

         begin
            --  This validation check, which warns if we have unequal sizes
            --  for unchecked conversion, and thus potentially implementation
            --  dependent semantics, is one of the few occasions on which we
            --  use the official RM size instead of Esize. See description
            --  in Einfo "Handling of Type'Size Values" for details.

            if Errors_Detected = 0
              and then Known_Static_RM_Size (Source)
              and then Known_Static_RM_Size (Target)
            then
               Source_Siz := RM_Size (Source);
               Target_Siz := RM_Size (Target);

               if Source_Siz /= Target_Siz then
                  Warn_On_Instance := True;
                  Error_Msg_N
                    ("types for unchecked conversion have different sizes?",
                     Enode);

                  if All_Errors_Mode then
                     Error_Msg_Name_1 := Chars (Source);
                     Error_Msg_Uint_1 := Source_Siz;
                     Error_Msg_Name_2 := Chars (Target);
                     Error_Msg_Uint_2 := Target_Siz;
                     Error_Msg_N
                       ("\size of % is ^, size of % is ^?", Enode);

                     Error_Msg_Uint_1 := UI_Abs (Source_Siz - Target_Siz);

                     if Is_Discrete_Type (Source)
                       and then Is_Discrete_Type (Target)
                     then
                        if Source_Siz > Target_Siz then
                           Error_Msg_N
                             ("\^ high order bits of source will be ignored?",
                              Enode);

                        elsif Is_Modular_Integer_Type (Source) then
                           Error_Msg_N
                             ("\source will be extended with ^ high order " &
                              "zero bits?", Enode);

                        else
                           Error_Msg_N
                             ("\source will be extended with ^ high order " &
                              "sign bits?",
                              Enode);
                        end if;

                     elsif Source_Siz < Target_Siz then
                        if Is_Discrete_Type (Target) then
                           if Bytes_Big_Endian then
                              Error_Msg_N
                                ("\target value will include ^ undefined " &
                                 "low order bits?",
                                 Enode);
                           else
                              Error_Msg_N
                                ("\target value will include ^ undefined " &
                                 "high order bits?",
                                 Enode);
                           end if;

                        else
                           Error_Msg_N
                             ("\^ trailing bits of target value will be " &
                              "undefined?", Enode);
                        end if;

                     else pragma Assert (Source_Siz > Target_Siz);
                        Error_Msg_N
                          ("\^ trailing bits of source will be ignored?",
                           Enode);
                     end if;
                  end if;

                  Warn_On_Instance := False;
               end if;
            end if;

            --  If both types are access types, we need to check the alignment.
            --  If the alignment of both is specified, we can do it here.

            if Errors_Detected = 0
              and then Ekind (Source) in Access_Kind
              and then Ekind (Target) in Access_Kind
              and then Target_Strict_Alignment
              and then Present (Designated_Type (Source))
              and then Present (Designated_Type (Target))
            then
               declare
                  D_Source : constant Entity_Id := Designated_Type (Source);
                  D_Target : constant Entity_Id := Designated_Type (Target);

               begin
                  if Known_Alignment (D_Source)
                    and then Known_Alignment (D_Target)
                  then
                     declare
                        Source_Align : constant Uint := Alignment (D_Source);
                        Target_Align : constant Uint := Alignment (D_Target);

                     begin
                        if Source_Align < Target_Align
                          and then not Is_Tagged_Type (D_Source)
                        then
                           Warn_On_Instance := True;
                           Error_Msg_Uint_1 := Target_Align;
                           Error_Msg_Uint_2 := Source_Align;
                           Error_Msg_Node_2 := D_Source;
                           Error_Msg_NE
                             ("alignment of & (^) is stricter than " &
                              "alignment of & (^)?", Enode, D_Target);

                           if All_Errors_Mode then
                              Error_Msg_N
                                ("\resulting access value may have invalid " &
                                 "alignment?", Enode);
                           end if;

                           Warn_On_Instance := False;
                        end if;
                     end;
                  end if;
               end;
            end if;
         end;
      end loop;
   end Validate_Unchecked_Conversions;

   ------------------
   -- Warn_Overlay --
   ------------------

   procedure Warn_Overlay
     (Expr : Node_Id;
      Typ  : Entity_Id;
      Nam  : Node_Id)
   is
      Old  : Entity_Id := Empty;
      Decl : Node_Id;

   begin
      if not Address_Clause_Overlay_Warnings then
         return;
      end if;

      if Present (Expr)
        and then (Has_Non_Null_Base_Init_Proc (Typ)
                    or else Is_Access_Type (Typ))
        and then not Is_Imported (Entity (Nam))
      then
         if Nkind (Expr) = N_Attribute_Reference
           and then Is_Entity_Name (Prefix (Expr))
         then
            Old := Entity (Prefix (Expr));

         elsif Is_Entity_Name (Expr)
           and then Ekind (Entity (Expr)) = E_Constant
         then
            Decl := Declaration_Node (Entity (Expr));

            if Nkind (Decl) = N_Object_Declaration
              and then Present (Expression (Decl))
              and then Nkind (Expression (Decl)) = N_Attribute_Reference
              and then Is_Entity_Name (Prefix (Expression (Decl)))
            then
               Old := Entity (Prefix (Expression (Decl)));

            elsif Nkind (Expr) = N_Function_Call then
               return;
            end if;

         --  A function call (most likely to To_Address) is probably not
         --  an overlay, so skip warning. Ditto if the function call was
         --  inlined and transformed into an entity.

         elsif Nkind (Original_Node (Expr)) = N_Function_Call then
            return;
         end if;

         Decl := Next (Parent (Expr));

         --  If a pragma Import follows, we assume that it is for the current
         --  target of the address clause, and skip the warning.

         if Present (Decl)
           and then Nkind (Decl) = N_Pragma
           and then Chars (Decl) = Name_Import
         then
            return;
         end if;

         if Present (Old) then
            Error_Msg_Node_2 := Old;
            Error_Msg_N
              ("default initialization of & may modify &?",
               Nam);
         else
            Error_Msg_N
              ("default initialization of & may modify overlaid storage?",
               Nam);
         end if;

         --  Add friendly warning if initialization comes from a packed array
         --  component.

         if Is_Record_Type (Typ)  then
            declare
               Comp : Entity_Id;

            begin
               Comp := First_Component (Typ);

               while Present (Comp) loop
                  if Nkind (Parent (Comp)) = N_Component_Declaration
                    and then Present (Expression (Parent (Comp)))
                  then
                     exit;
                  elsif Is_Array_Type (Etype (Comp))
                     and then Present (Packed_Array_Type (Etype (Comp)))
                  then
                     Error_Msg_NE
                       ("packed array component& will be initialized to zero?",
                          Nam, Comp);
                     exit;
                  else
                     Next_Component (Comp);
                  end if;
               end loop;
            end;
         end if;

         Error_Msg_N
           ("use pragma Import for & to " &
              "suppress initialization ('R'M B.1(24))?",
             Nam);
      end if;
   end Warn_Overlay;

end Sem_Ch13;
