------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ C H 1 3                              --
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
with Einfo;    use Einfo;
with Errout;   use Errout;
with Exp_Tss;  use Exp_Tss;
with Exp_Util; use Exp_Util;
with Lib;      use Lib;
with Lib.Xref; use Lib.Xref;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Restrict; use Restrict;
with Rident;   use Rident;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Aux;  use Sem_Aux;
with Sem_Ch3;  use Sem_Ch3;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Eval; use Sem_Eval;
with Sem_Res;  use Sem_Res;
with Sem_Type; use Sem_Type;
with Sem_Util; use Sem_Util;
with Sem_Warn; use Sem_Warn;
with Snames;   use Snames;
with Stand;    use Stand;
with Sinfo;    use Sinfo;
with Table;
with Targparm; use Targparm;
with Ttypes;   use Ttypes;
with Tbuild;   use Tbuild;
with Urealp;   use Urealp;

with GNAT.Heap_Sort_G;

package body Sem_Ch13 is

   SSU : constant Pos := System_Storage_Unit;
   --  Convenient short hand for commonly used constant

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Alignment_Check_For_Esize_Change (Typ : Entity_Id);
   --  This routine is called after setting the Esize of type entity Typ.
   --  The purpose is to deal with the situation where an alignment has been
   --  inherited from a derived type that is no longer appropriate for the
   --  new Esize value. In this case, we reset the Alignment to unknown.

   function Get_Alignment_Value (Expr : Node_Id) return Uint;
   --  Given the expression for an alignment value, returns the corresponding
   --  Uint value. If the value is inappropriate, then error messages are
   --  posted as required, and a value of No_Uint is returned.

   function Is_Operational_Item (N : Node_Id) return Boolean;
   --  A specification for a stream attribute is allowed before the full
   --  type is declared, as explained in AI-00137 and the corrigendum.
   --  Attributes that do not specify a representation characteristic are
   --  operational attributes.

   procedure New_Stream_Subprogram
     (N    : Node_Id;
      Ent  : Entity_Id;
      Subp : Entity_Id;
      Nam  : TSS_Name_Type);
   --  Create a subprogram renaming of a given stream attribute to the
   --  designated subprogram and then in the tagged case, provide this as a
   --  primitive operation, or in the non-tagged case make an appropriate TSS
   --  entry. This is more properly an expansion activity than just semantics,
   --  but the presence of user-defined stream functions for limited types is a
   --  legality check, which is why this takes place here rather than in
   --  exp_ch13, where it was previously. Nam indicates the name of the TSS
   --  function to be generated.
   --
   --  To avoid elaboration anomalies with freeze nodes, for untagged types
   --  we generate both a subprogram declaration and a subprogram renaming
   --  declaration, so that the attribute specification is handled as a
   --  renaming_as_body. For tagged types, the specification is one of the
   --  primitive specs.

   ----------------------------------------------
   -- Table for Validate_Unchecked_Conversions --
   ----------------------------------------------

   --  The following table collects unchecked conversions for validation.
   --  Entries are made by Validate_Unchecked_Conversion and then the
   --  call to Validate_Unchecked_Conversions does the actual error
   --  checking and posting of warnings. The reason for this delayed
   --  processing is to take advantage of back-annotations of size and
   --  alignment values performed by the back end.

   --  Note: the reason we store a Source_Ptr value instead of a Node_Id
   --  is that by the time Validate_Unchecked_Conversions is called, Sprint
   --  will already have modified all Sloc values if the -gnatD option is set.

   type UC_Entry is record
      Eloc   : Source_Ptr; -- node used for posting warnings
      Source : Entity_Id;  -- source type for unchecked conversion
      Target : Entity_Id;  -- target type for unchecked conversion
   end record;

   package Unchecked_Conversions is new Table.Table (
     Table_Component_Type => UC_Entry,
     Table_Index_Type     => Int,
     Table_Low_Bound      => 1,
     Table_Initial        => 50,
     Table_Increment      => 200,
     Table_Name           => "Unchecked_Conversions");

   ----------------------------------------
   -- Table for Validate_Address_Clauses --
   ----------------------------------------

   --  If an address clause has the form

   --    for X'Address use Expr

   --  where Expr is of the form Y'Address or recursively is a reference
   --  to a constant of either of these forms, and X and Y are entities of
   --  objects, then if Y has a smaller alignment than X, that merits a
   --  warning about possible bad alignment. The following table collects
   --  address clauses of this kind. We put these in a table so that they
   --  can be checked after the back end has completed annotation of the
   --  alignments of objects, since we can catch more cases that way.

   type Address_Clause_Check_Record is record
      N : Node_Id;
      --  The address clause

      X : Entity_Id;
      --  The entity of the object overlaying Y

      Y : Entity_Id;
      --  The entity of the object being overlaid

      Off : Boolean;
      --  Whether the address is offseted within Y
   end record;

   package Address_Clause_Checks is new Table.Table (
     Table_Component_Type => Address_Clause_Check_Record,
     Table_Index_Type     => Int,
     Table_Low_Bound      => 1,
     Table_Initial        => 20,
     Table_Increment      => 200,
     Table_Name           => "Address_Clause_Checks");

   -----------------------------------------
   -- Adjust_Record_For_Reverse_Bit_Order --
   -----------------------------------------

   procedure Adjust_Record_For_Reverse_Bit_Order (R : Entity_Id) is
      Comp : Node_Id;
      CC   : Node_Id;

   begin
      --  Processing depends on version of Ada

      case Ada_Version is

         --  For Ada 95, we just renumber bits within a storage unit. We do
         --  the same for Ada 83 mode, since we recognize pragma Bit_Order
         --  in Ada 83, and are free to add this extension.

         when Ada_83 | Ada_95 =>
            Comp := First_Component_Or_Discriminant (R);
            while Present (Comp) loop
               CC := Component_Clause (Comp);

               --  If component clause is present, then deal with the non-
               --  default bit order case for Ada 95 mode.

               --  We only do this processing for the base type, and in
               --  fact that's important, since otherwise if there are
               --  record subtypes, we could reverse the bits once for
               --  each subtype, which would be incorrect.

               if Present (CC)
                 and then Ekind (R) = E_Record_Type
               then
                  declare
                     CFB : constant Uint    := Component_Bit_Offset (Comp);
                     CSZ : constant Uint    := Esize (Comp);
                     CLC : constant Node_Id := Component_Clause (Comp);
                     Pos : constant Node_Id := Position (CLC);
                     FB  : constant Node_Id := First_Bit (CLC);

                     Storage_Unit_Offset : constant Uint :=
                                             CFB / System_Storage_Unit;

                     Start_Bit : constant Uint :=
                                   CFB mod System_Storage_Unit;

                  begin
                     --  Cases where field goes over storage unit boundary

                     if Start_Bit + CSZ > System_Storage_Unit then

                        --  Allow multi-byte field but generate warning

                        if Start_Bit mod System_Storage_Unit = 0
                          and then CSZ mod System_Storage_Unit = 0
                        then
                           Error_Msg_N
                             ("multi-byte field specified with non-standard"
                              & " Bit_Order?", CLC);

                           if Bytes_Big_Endian then
                              Error_Msg_N
                                ("bytes are not reversed "
                                 & "(component is big-endian)?", CLC);
                           else
                              Error_Msg_N
                                ("bytes are not reversed "
                                 & "(component is little-endian)?", CLC);
                           end if;

                           --  Do not allow non-contiguous field

                        else
                           Error_Msg_N
                             ("attempt to specify non-contiguous field "
                              & "not permitted", CLC);
                           Error_Msg_N
                             ("\caused by non-standard Bit_Order "
                              & "specified", CLC);
                           Error_Msg_N
                             ("\consider possibility of using "
                              & "Ada 2005 mode here", CLC);
                        end if;

                        --  Case where field fits in one storage unit

                     else
                        --  Give warning if suspicious component clause

                        if Intval (FB) >= System_Storage_Unit
                          and then Warn_On_Reverse_Bit_Order
                        then
                           Error_Msg_N
                             ("?Bit_Order clause does not affect " &
                              "byte ordering", Pos);
                           Error_Msg_Uint_1 :=
                             Intval (Pos) + Intval (FB) /
                             System_Storage_Unit;
                           Error_Msg_N
                             ("?position normalized to ^ before bit " &
                              "order interpreted", Pos);
                        end if;

                        --  Here is where we fix up the Component_Bit_Offset
                        --  value to account for the reverse bit order.
                        --  Some examples of what needs to be done are:

                        --    First_Bit .. Last_Bit     Component_Bit_Offset
                        --      old          new          old       new

                        --     0 .. 0       7 .. 7         0         7
                        --     0 .. 1       6 .. 7         0         6
                        --     0 .. 2       5 .. 7         0         5
                        --     0 .. 7       0 .. 7         0         4

                        --     1 .. 1       6 .. 6         1         6
                        --     1 .. 4       3 .. 6         1         3
                        --     4 .. 7       0 .. 3         4         0

                        --  The general rule is that the first bit is
                        --  is obtained by subtracting the old ending bit
                        --  from storage_unit - 1.

                        Set_Component_Bit_Offset
                          (Comp,
                           (Storage_Unit_Offset * System_Storage_Unit) +
                             (System_Storage_Unit - 1) -
                             (Start_Bit + CSZ - 1));

                        Set_Normalized_First_Bit
                          (Comp,
                           Component_Bit_Offset (Comp) mod
                             System_Storage_Unit);
                     end if;
                  end;
               end if;

               Next_Component_Or_Discriminant (Comp);
            end loop;

         --  For Ada 2005, we do machine scalar processing, as fully described
         --  In AI-133. This involves gathering all components which start at
         --  the same byte offset and processing them together

         when Ada_05 .. Ada_Version_Type'Last =>
            declare
               Max_Machine_Scalar_Size : constant Uint :=
                                           UI_From_Int
                                             (Standard_Long_Long_Integer_Size);
            --  We use this as the maximum machine scalar size

               Num_CC : Natural;
               SSU    : constant Uint := UI_From_Int (System_Storage_Unit);

            begin
               --  This first loop through components does two things. First it
               --  deals with the case of components with component clauses
               --  whose length is greater than the maximum machine scalar size
               --  (either accepting them or rejecting as needed). Second, it
               --  counts the number of components with component clauses whose
               --  length does not exceed this maximum for later processing.

               Num_CC := 0;
               Comp   := First_Component_Or_Discriminant (R);
               while Present (Comp) loop
                  CC := Component_Clause (Comp);

                  if Present (CC) then
                     declare
                        Fbit : constant Uint :=
                                 Static_Integer (First_Bit (CC));

                     begin
                        --  Case of component with size > max machine scalar

                        if Esize (Comp) > Max_Machine_Scalar_Size then

                           --  Must begin on byte boundary

                           if Fbit mod SSU /= 0 then
                              Error_Msg_N
                                ("illegal first bit value for "
                                 & "reverse bit order",
                                 First_Bit (CC));
                              Error_Msg_Uint_1 := SSU;
                              Error_Msg_Uint_2 := Max_Machine_Scalar_Size;

                              Error_Msg_N
                                ("\must be a multiple of ^ "
                                 & "if size greater than ^",
                                 First_Bit (CC));

                              --  Must end on byte boundary

                           elsif Esize (Comp) mod SSU /= 0 then
                              Error_Msg_N
                                ("illegal last bit value for "
                                 & "reverse bit order",
                                 Last_Bit (CC));
                              Error_Msg_Uint_1 := SSU;
                              Error_Msg_Uint_2 := Max_Machine_Scalar_Size;

                              Error_Msg_N
                                ("\must be a multiple of ^ if size "
                                 & "greater than ^",
                                 Last_Bit (CC));

                              --  OK, give warning if enabled

                           elsif Warn_On_Reverse_Bit_Order then
                              Error_Msg_N
                                ("multi-byte field specified with "
                                 & "  non-standard Bit_Order?", CC);

                              if Bytes_Big_Endian then
                                 Error_Msg_N
                                   ("\bytes are not reversed "
                                    & "(component is big-endian)?", CC);
                              else
                                 Error_Msg_N
                                   ("\bytes are not reversed "
                                    & "(component is little-endian)?", CC);
                              end if;
                           end if;

                           --  Case where size is not greater than max machine
                           --  scalar. For now, we just count these.

                        else
                           Num_CC := Num_CC + 1;
                        end if;
                     end;
                  end if;

                  Next_Component_Or_Discriminant (Comp);
               end loop;

               --  We need to sort the component clauses on the basis of the
               --  Position values in the clause, so we can group clauses with
               --  the same Position. together to determine the relevant
               --  machine scalar size.

               Sort_CC : declare
                  Comps : array (0 .. Num_CC) of Entity_Id;
                  --  Array to collect component and discriminant entities. The
                  --  data starts at index 1, the 0'th entry is for the sort
                  --  routine.

                  function CP_Lt (Op1, Op2 : Natural) return Boolean;
                  --  Compare routine for Sort

                  procedure CP_Move (From : Natural; To : Natural);
                  --  Move routine for Sort

                  package Sorting is new GNAT.Heap_Sort_G (CP_Move, CP_Lt);

                  Start : Natural;
                  Stop  : Natural;
                  --  Start and stop positions in component list of set of
                  --  components with the same starting position (that
                  --  constitute components in a single machine scalar).

                  MaxL  : Uint;
                  --  Maximum last bit value of any component in this set

                  MSS   : Uint;
                  --  Corresponding machine scalar size

                  -----------
                  -- CP_Lt --
                  -----------

                  function CP_Lt (Op1, Op2 : Natural) return Boolean is
                  begin
                     return Position (Component_Clause (Comps (Op1))) <
                            Position (Component_Clause (Comps (Op2)));
                  end CP_Lt;

                  -------------
                  -- CP_Move --
                  -------------

                  procedure CP_Move (From : Natural; To : Natural) is
                  begin
                     Comps (To) := Comps (From);
                  end CP_Move;

               --  Start of processing for Sort_CC

               begin
                  --  Collect the component clauses

                  Num_CC := 0;
                  Comp   := First_Component_Or_Discriminant (R);
                  while Present (Comp) loop
                     if Present (Component_Clause (Comp))
                       and then Esize (Comp) <= Max_Machine_Scalar_Size
                     then
                        Num_CC := Num_CC + 1;
                        Comps (Num_CC) := Comp;
                     end if;

                     Next_Component_Or_Discriminant (Comp);
                  end loop;

                  --  Sort by ascending position number

                  Sorting.Sort (Num_CC);

                  --  We now have all the components whose size does not exceed
                  --  the max machine scalar value, sorted by starting
                  --  position. In this loop we gather groups of clauses
                  --  starting at the same position, to process them in
                  --  accordance with Ada 2005 AI-133.

                  Stop := 0;
                  while Stop < Num_CC loop
                     Start := Stop + 1;
                     Stop  := Start;
                     MaxL  :=
                       Static_Integer
                         (Last_Bit (Component_Clause (Comps (Start))));
                     while Stop < Num_CC loop
                        if Static_Integer
                             (Position (Component_Clause (Comps (Stop + 1)))) =
                           Static_Integer
                             (Position (Component_Clause (Comps (Stop))))
                        then
                           Stop := Stop + 1;
                           MaxL :=
                             UI_Max
                               (MaxL,
                                Static_Integer
                                  (Last_Bit
                                     (Component_Clause (Comps (Stop)))));
                        else
                           exit;
                        end if;
                     end loop;

                     --  Now we have a group of component clauses from Start to
                     --  Stop whose positions are identical, and MaxL is the
                     --  maximum last bit value of any of these components.

                     --  We need to determine the corresponding machine scalar
                     --  size. This loop assumes that machine scalar sizes are
                     --  even, and that each possible machine scalar has twice
                     --  as many bits as the next smaller one.

                     MSS := Max_Machine_Scalar_Size;
                     while MSS mod 2 = 0
                       and then (MSS / 2) >= SSU
                       and then (MSS / 2) > MaxL
                     loop
                        MSS := MSS / 2;
                     end loop;

                     --  Here is where we fix up the Component_Bit_Offset value
                     --  to account for the reverse bit order. Some examples of
                     --  what needs to be done for the case of a machine scalar
                     --  size of 8 are:

                     --    First_Bit .. Last_Bit     Component_Bit_Offset
                     --      old          new          old       new

                     --     0 .. 0       7 .. 7         0         7
                     --     0 .. 1       6 .. 7         0         6
                     --     0 .. 2       5 .. 7         0         5
                     --     0 .. 7       0 .. 7         0         4

                     --     1 .. 1       6 .. 6         1         6
                     --     1 .. 4       3 .. 6         1         3
                     --     4 .. 7       0 .. 3         4         0

                     --  The general rule is that the first bit is obtained by
                     --  subtracting the old ending bit from machine scalar
                     --  size - 1.

                     for C in Start .. Stop loop
                        declare
                           Comp : constant Entity_Id := Comps (C);
                           CC   : constant Node_Id   :=
                                    Component_Clause (Comp);
                           LB   : constant Uint :=
                                    Static_Integer (Last_Bit (CC));
                           NFB  : constant Uint := MSS - Uint_1 - LB;
                           NLB  : constant Uint := NFB + Esize (Comp) - 1;
                           Pos  : constant Uint :=
                                    Static_Integer (Position (CC));

                        begin
                           if Warn_On_Reverse_Bit_Order then
                              Error_Msg_Uint_1 := MSS;
                              Error_Msg_N
                                ("info: reverse bit order in machine " &
                                 "scalar of length^?", First_Bit (CC));
                              Error_Msg_Uint_1 := NFB;
                              Error_Msg_Uint_2 := NLB;

                              if Bytes_Big_Endian then
                                 Error_Msg_NE
                                   ("?\info: big-endian range for "
                                    & "component & is ^ .. ^",
                                    First_Bit (CC), Comp);
                              else
                                 Error_Msg_NE
                                   ("?\info: little-endian range "
                                    & "for component & is ^ .. ^",
                                    First_Bit (CC), Comp);
                              end if;
                           end if;

                           Set_Component_Bit_Offset (Comp, Pos * SSU + NFB);
                           Set_Normalized_First_Bit (Comp, NFB mod SSU);
                        end;
                     end loop;
                  end loop;
               end Sort_CC;
            end;
      end case;
   end Adjust_Record_For_Reverse_Bit_Order;

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
      CS : constant Boolean := Comes_From_Source (N);

   begin
      --  This is an obsolescent feature

      Check_Restriction (No_Obsolescent_Features, N);

      if Warn_On_Obsolescent_Feature then
         Error_Msg_N
           ("at clause is an obsolescent feature (RM J.7(2))?", N);
         Error_Msg_N
           ("\use address attribute definition clause instead?", N);
      end if;

      --  Rewrite as address clause

      Rewrite (N,
        Make_Attribute_Definition_Clause (Sloc (N),
          Name  => Identifier (N),
          Chars => Name_Address,
          Expression => Expression (N)));

      --  We preserve Comes_From_Source, since logically the clause still
      --  comes from the source program even though it is changed in form.

      Set_Comes_From_Source (N, CS);

      --  Analyze rewritten clause

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

      procedure Analyze_Stream_TSS_Definition (TSS_Nam : TSS_Name_Type);
      --  Common processing for 'Read, 'Write, 'Input and 'Output attribute
      --  definition clauses.

      -----------------------------------
      -- Analyze_Stream_TSS_Definition --
      -----------------------------------

      procedure Analyze_Stream_TSS_Definition (TSS_Nam : TSS_Name_Type) is
         Subp : Entity_Id := Empty;
         I    : Interp_Index;
         It   : Interp;
         Pnam : Entity_Id;

         Is_Read : constant Boolean := (TSS_Nam = TSS_Stream_Read);

         function Has_Good_Profile (Subp : Entity_Id) return Boolean;
         --  Return true if the entity is a subprogram with an appropriate
         --  profile for the attribute being defined.

         ----------------------
         -- Has_Good_Profile --
         ----------------------

         function Has_Good_Profile (Subp : Entity_Id) return Boolean is
            F              : Entity_Id;
            Is_Function    : constant Boolean := (TSS_Nam = TSS_Stream_Input);
            Expected_Ekind : constant array (Boolean) of Entity_Kind :=
                               (False => E_Procedure, True => E_Function);
            Typ            : Entity_Id;

         begin
            if Ekind (Subp) /= Expected_Ekind (Is_Function) then
               return False;
            end if;

            F := First_Formal (Subp);

            if No (F)
              or else Ekind (Etype (F)) /= E_Anonymous_Access_Type
              or else Designated_Type (Etype (F)) /=
                               Class_Wide_Type (RTE (RE_Root_Stream_Type))
            then
               return False;
            end if;

            if not Is_Function then
               Next_Formal (F);

               declare
                  Expected_Mode : constant array (Boolean) of Entity_Kind :=
                                    (False => E_In_Parameter,
                                     True  => E_Out_Parameter);
               begin
                  if Parameter_Mode (F) /= Expected_Mode (Is_Read) then
                     return False;
                  end if;
               end;

               Typ := Etype (F);

            else
               Typ := Etype (Subp);
            end if;

            return Base_Type (Typ) = Base_Type (Ent)
              and then No (Next_Formal (F));
         end Has_Good_Profile;

      --  Start of processing for Analyze_Stream_TSS_Definition

      begin
         FOnly := True;

         if not Is_Type (U_Ent) then
            Error_Msg_N ("local name must be a subtype", Nam);
            return;
         end if;

         Pnam := TSS (Base_Type (U_Ent), TSS_Nam);

         --  If Pnam is present, it can be either inherited from an ancestor
         --  type (in which case it is legal to redefine it for this type), or
         --  be a previous definition of the attribute for the same type (in
         --  which case it is illegal).

         --  In the first case, it will have been analyzed already, and we
         --  can check that its profile does not match the expected profile
         --  for a stream attribute of U_Ent. In the second case, either Pnam
         --  has been analyzed (and has the expected profile), or it has not
         --  been analyzed yet (case of a type that has not been frozen yet
         --  and for which the stream attribute has been set using Set_TSS).

         if Present (Pnam)
           and then (No (First_Entity (Pnam)) or else Has_Good_Profile (Pnam))
         then
            Error_Msg_Sloc := Sloc (Pnam);
            Error_Msg_Name_1 := Attr;
            Error_Msg_N ("% attribute already defined #", Nam);
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
            if Is_Abstract_Subprogram (Subp) then
               Error_Msg_N ("stream subprogram must not be abstract", Expr);
               return;
            end if;

            Set_Entity (Expr, Subp);
            Set_Etype (Expr, Etype (Subp));

            New_Stream_Subprogram (N, U_Ent, Subp, TSS_Nam);

         else
            Error_Msg_Name_1 := Attr;
            Error_Msg_N ("incorrect expression for% attribute", Expr);
         end if;
      end Analyze_Stream_TSS_Definition;

   --  Start of processing for Analyze_Attribute_Definition_Clause

   begin
      --  Process Ignore_Rep_Clauses option

      if Ignore_Rep_Clauses then
         case Id is

            --  The following should be ignored. They do not affect legality
            --  and may be target dependent. The basic idea of -gnatI is to
            --  ignore any rep clauses that may be target dependent but do not
            --  affect legality (except possibly to be rejected because they
            --  are incompatible with the compilation target).

            when Attribute_Alignment      |
                 Attribute_Bit_Order      |
                 Attribute_Component_Size |
                 Attribute_Machine_Radix  |
                 Attribute_Object_Size    |
                 Attribute_Size           |
                 Attribute_Small          |
                 Attribute_Stream_Size    |
                 Attribute_Value_Size     =>

               Rewrite (N, Make_Null_Statement (Sloc (N)));
               return;

            --  The following should not be ignored, because in the first place
            --  they are reasonably portable, and should not cause problems in
            --  compiling code from another target, and also they do affect
            --  legality, e.g. failing to provide a stream attribute for a
            --  type may make a program illegal.

            when Attribute_External_Tag   |
                 Attribute_Input          |
                 Attribute_Output         |
                 Attribute_Read           |
                 Attribute_Storage_Pool   |
                 Attribute_Storage_Size   |
                 Attribute_Write          =>
               null;

            --  Other cases are errors ("attribute& cannot be set with
            --  definition clause"), which will be caught below.

            when others =>
               null;
         end case;
      end if;

      Analyze (Nam);
      Ent := Entity (Nam);

      if Rep_Item_Too_Early (Ent, N) then
         return;
      end if;

      --  Rep clause applies to full view of incomplete type or private type if
      --  we have one (if not, this is a premature use of the type). However,
      --  certain semantic checks need to be done on the specified entity (i.e.
      --  the private view), so we save it in Ent.

      if Is_Private_Type (Ent)
        and then Is_Derived_Type (Ent)
        and then not Is_Tagged_Type (Ent)
        and then No (Full_View (Ent))
      then
         --  If this is a private type whose completion is a derivation from
         --  another private type, there is no full view, and the attribute
         --  belongs to the type itself, not its underlying parent.

         U_Ent := Ent;

      elsif Ekind (Ent) = E_Incomplete_Type then

         --  The attribute applies to the full view, set the entity of the
         --  attribute definition accordingly.

         Ent := Underlying_Type (Ent);
         U_Ent := Ent;
         Set_Entity (Nam, Ent);

      else
         U_Ent := Underlying_Type (Ent);
      end if;

      --  Complete other routine error checks

      if Etype (Nam) = Any_Type then
         return;

      elsif Scope (Ent) /= Current_Scope then
         Error_Msg_N ("entity must be declared in this scope", Nam);
         return;

      elsif No (U_Ent) then
         U_Ent := Ent;

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

            --  A little error check, catch for X'Address use X'Address;

            if Nkind (Nam) = N_Identifier
              and then Nkind (Expr) = N_Attribute_Reference
              and then Attribute_Name (Expr) = Name_Address
              and then Nkind (Prefix (Expr)) = N_Identifier
              and then Chars (Nam) = Chars (Prefix (Expr))
            then
               Error_Msg_NE
                 ("address for & is self-referencing", Prefix (Expr), Ent);
               return;
            end if;

            --  Not that special case, carry on with analysis of expression

            Analyze_And_Resolve (Expr, RTE (RE_Address));

            --  Even when ignoring rep clauses we need to indicate that the
            --  entity has an address clause and thus it is legal to declare
            --  it imported.

            if Ignore_Rep_Clauses then
               if Ekind_In (U_Ent, E_Variable, E_Constant) then
                  Record_Rep_Item (U_Ent, N);
               end if;

               return;
            end if;

            if Present (Address_Clause (U_Ent)) then
               Error_Msg_N ("address already given for &", Nam);

            --  Case of address clause for subprogram

            elsif Is_Subprogram (U_Ent) then
               if Has_Homonym (U_Ent) then
                  Error_Msg_N
                    ("address clause cannot be given " &
                     "for overloaded subprogram",
                     Nam);
                  return;
               end if;

               --  For subprograms, all address clauses are permitted, and we
               --  mark the subprogram as having a deferred freeze so that Gigi
               --  will not elaborate it too soon.

               --  Above needs more comments, what is too soon about???

               Set_Has_Delayed_Freeze (U_Ent);

            --  Case of address clause for entry

            elsif Ekind (U_Ent) = E_Entry then
               if Nkind (Parent (N)) = N_Task_Body then
                  Error_Msg_N
                    ("entry address must be specified in task spec", Nam);
                  return;
               end if;

               --  For entries, we require a constant address

               Check_Constant_Address_Clause (Expr, U_Ent);

               --  Special checks for task types

               if Is_Task_Type (Scope (U_Ent))
                 and then Comes_From_Source (Scope (U_Ent))
               then
                  Error_Msg_N
                    ("?entry address declared for entry in task type", N);
                  Error_Msg_N
                    ("\?only one task can be declared of this type", N);
               end if;

               --  Entry address clauses are obsolescent

               Check_Restriction (No_Obsolescent_Features, N);

               if Warn_On_Obsolescent_Feature then
                  Error_Msg_N
                    ("attaching interrupt to task entry is an " &
                     "obsolescent feature (RM J.7.1)?", N);
                  Error_Msg_N
                    ("\use interrupt procedure instead?", N);
               end if;

            --  Case of an address clause for a controlled object which we
            --  consider to be erroneous.

            elsif Is_Controlled (Etype (U_Ent))
              or else Has_Controlled_Component (Etype (U_Ent))
            then
               Error_Msg_NE
                 ("?controlled object& must not be overlaid", Nam, U_Ent);
               Error_Msg_N
                 ("\?Program_Error will be raised at run time", Nam);
               Insert_Action (Declaration_Node (U_Ent),
                 Make_Raise_Program_Error (Loc,
                   Reason => PE_Overlaid_Controlled_Object));
               return;

            --  Case of address clause for a (non-controlled) object

            elsif
              Ekind (U_Ent) = E_Variable
                or else
              Ekind (U_Ent) = E_Constant
            then
               declare
                  Expr  : constant Node_Id := Expression (N);
                  O_Ent : Entity_Id;
                  Off   : Boolean;

               begin
                  --  Exported variables cannot have an address clause, because
                  --  this cancels the effect of the pragma Export.

                  if Is_Exported (U_Ent) then
                     Error_Msg_N
                       ("cannot export object with address clause", Nam);
                     return;
                  end if;

                  Find_Overlaid_Entity (N, O_Ent, Off);

                  --  Overlaying controlled objects is erroneous

                  if Present (O_Ent)
                    and then (Has_Controlled_Component (Etype (O_Ent))
                                or else Is_Controlled (Etype (O_Ent)))
                  then
                     Error_Msg_N
                       ("?cannot overlay with controlled object", Expr);
                     Error_Msg_N
                       ("\?Program_Error will be raised at run time", Expr);
                     Insert_Action (Declaration_Node (U_Ent),
                       Make_Raise_Program_Error (Loc,
                         Reason => PE_Overlaid_Controlled_Object));
                     return;

                  elsif Present (O_Ent)
                    and then Ekind (U_Ent) = E_Constant
                    and then not Is_Constant_Object (O_Ent)
                  then
                     Error_Msg_N ("constant overlays a variable?", Expr);

                  elsif Present (Renamed_Object (U_Ent)) then
                     Error_Msg_N
                       ("address clause not allowed"
                          & " for a renaming declaration (RM 13.1(6))", Nam);
                     return;

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

                  Note_Possible_Modification (Nam, Sure => False);

                  --  Here we are checking for explicit overlap of one variable
                  --  by another, and if we find this then mark the overlapped
                  --  variable as also being volatile to prevent unwanted
                  --  optimizations. This is a significant pessimization so
                  --  avoid it when there is an offset, i.e. when the object
                  --  is composite; they cannot be optimized easily anyway.

                  if Present (O_Ent)
                    and then Is_Object (O_Ent)
                    and then not Off
                  then
                     Set_Treat_As_Volatile (O_Ent);
                  end if;

                  --  Legality checks on the address clause for initialized
                  --  objects is deferred until the freeze point, because
                  --  a subsequent pragma might indicate that the object is
                  --  imported and thus not initialized.

                  Set_Has_Delayed_Freeze (U_Ent);

                  --  If an initialization call has been generated for this
                  --  object, it needs to be deferred to after the freeze node
                  --  we have just now added, otherwise GIGI will see a
                  --  reference to the variable (as actual to the IP call)
                  --  before its definition.

                  declare
                     Init_Call : constant Node_Id := Find_Init_Call (U_Ent, N);
                  begin
                     if Present (Init_Call) then
                        Remove (Init_Call);
                        Append_Freeze_Action (U_Ent, Init_Call);
                     end if;
                  end;

                  if Is_Exported (U_Ent) then
                     Error_Msg_N
                       ("& cannot be exported if an address clause is given",
                        Nam);
                     Error_Msg_N
                       ("\define and export a variable " &
                        "that holds its address instead",
                        Nam);
                  end if;

                  --  Entity has delayed freeze, so we will generate an
                  --  alignment check at the freeze point unless suppressed.

                  if not Range_Checks_Suppressed (U_Ent)
                    and then not Alignment_Checks_Suppressed (U_Ent)
                  then
                     Set_Check_Address_Alignment (N);
                  end if;

                  --  Kill the size check code, since we are not allocating
                  --  the variable, it is somewhere else.

                  Kill_Size_Check_Code (U_Ent);

                  --  If the address clause is of the form:

                  --    for Y'Address use X'Address

                  --  or

                  --    Const : constant Address := X'Address;
                  --    ...
                  --    for Y'Address use Const;

                  --  then we make an entry in the table for checking the size
                  --  and alignment of the overlaying variable. We defer this
                  --  check till after code generation to take full advantage
                  --  of the annotation done by the back end. This entry is
                  --  only made if the address clause comes from source.
                  --  If the entity has a generic type, the check will be
                  --  performed in the instance if the actual type justifies
                  --  it, and we do not insert the clause in the table to
                  --  prevent spurious warnings.

                  if Address_Clause_Overlay_Warnings
                    and then Comes_From_Source (N)
                    and then Present (O_Ent)
                    and then Is_Object (O_Ent)
                  then
                     if not Is_Generic_Type (Etype (U_Ent)) then
                        Address_Clause_Checks.Append ((N, U_Ent, O_Ent, Off));
                     end if;

                     --  If variable overlays a constant view, and we are
                     --  warning on overlays, then mark the variable as
                     --  overlaying a constant (we will give warnings later
                     --  if this variable is assigned).

                     if Is_Constant_Object (O_Ent)
                       and then Ekind (U_Ent) = E_Variable
                     then
                        Set_Overlays_Constant (U_Ent);
                     end if;
                  end if;
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

         when Attribute_Alignment => Alignment : declare
            Align : constant Uint := Get_Alignment_Value (Expr);

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

               --  For an array type, U_Ent is the first subtype. In that case,
               --  also set the alignment of the anonymous base type so that
               --  other subtypes (such as the itypes for aggregates of the
               --  type) also receive the expected alignment.

               if Is_Array_Type (U_Ent) then
                  Set_Alignment (Base_Type (U_Ent), Align);
               end if;
            end if;
         end Alignment;

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
                  Flag_Non_Static_Expr
                    ("Bit_Order requires static expression!", Expr);

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
                 ("component size clause for& previously given", Nam);

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
               --  elements of the array. Note that component size clauses
               --  are ignored in VM mode.

               if VM_Target = No_VM then
                  if Biased then
                     New_Ctyp :=
                       Make_Defining_Identifier (Loc,
                         Chars =>
                           New_External_Name (Chars (U_Ent), 'C', 0, 'T'));

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

                     if Warn_On_Biased_Representation then
                        Error_Msg_N
                          ("?component size clause forces biased "
                           & "representation", N);
                     end if;
                  end if;

                  Set_Component_Size (Btype, Csize);

               --  For VM case, we ignore component size clauses

               else
                  --  Give a warning unless we are in GNAT mode, in which case
                  --  the warning is suppressed since it is not useful.

                  if not GNAT_Mode then
                     Error_Msg_N
                       ("?component size ignored in this configuration", N);
                  end if;
               end if;

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
               Flag_Non_Static_Expr
                 ("static string required for tag name!", Nam);
            end if;

            if VM_Target = No_VM then
               Set_Has_External_Tag_Rep_Clause (U_Ent);
            else
               Error_Msg_Name_1 := Attr;
               Error_Msg_N
                 ("% attribute unsupported in this configuration", Nam);
            end if;

            if not Is_Library_Level_Entity (U_Ent) then
               Error_Msg_NE
                 ("?non-unique external tag supplied for &", N, U_Ent);
               Error_Msg_N
                 ("?\same external tag applies to all subprogram calls", N);
               Error_Msg_N
                 ("?\corresponding internal tag cannot be obtained", N);
            end if;
         end External_Tag;

         -----------
         -- Input --
         -----------

         when Attribute_Input =>
            Analyze_Stream_TSS_Definition (TSS_Stream_Input);
            Set_Has_Specified_Stream_Input (Ent);

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
            Size : constant Uint := Static_Integer (Expr);

            Biased : Boolean;
            pragma Warnings (Off, Biased);

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

         when Attribute_Output =>
            Analyze_Stream_TSS_Definition (TSS_Stream_Output);
            Set_Has_Specified_Stream_Output (Ent);

         ----------
         -- Read --
         ----------

         when Attribute_Read =>
            Analyze_Stream_TSS_Definition (TSS_Stream_Read);
            Set_Has_Specified_Stream_Read (Ent);

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

               --  Check size, note that Gigi is in charge of checking that the
               --  size of an array or record type is OK. Also we do not check
               --  the size in the ordinary fixed-point case, since it is too
               --  early to do so (there may be subsequent small clause that
               --  affects the size). We can check the size if a small clause
               --  has already been given.

               if not Is_Ordinary_Fixed_Point_Type (U_Ent)
                 or else Has_Small_Clause (U_Ent)
               then
                  Check_Size (Expr, Etyp, Size, Biased);
                     Set_Has_Biased_Representation (U_Ent, Biased);

                  if Biased and Warn_On_Biased_Representation then
                     Error_Msg_N
                       ("?size clause forces biased representation", N);
                  end if;
               end if;

               --  For types set RM_Size and Esize if possible

               if Is_Type (U_Ent) then
                  Set_RM_Size (U_Ent, Size);

                  --  For scalar types, increase Object_Size to power of 2, but
                  --  not less than a storage unit in any case (i.e., normally
                  --  this means it will be byte addressable).

                  if Is_Scalar_Type (U_Ent) then
                     if Size <= System_Storage_Unit then
                        Init_Esize (U_Ent, System_Storage_Unit);
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
                  if Is_Elementary_Type (Etyp) then
                     if Size /= System_Storage_Unit
                          and then
                        Size /= System_Storage_Unit * 2
                          and then
                        Size /= System_Storage_Unit * 4
                           and then
                        Size /= System_Storage_Unit * 8
                     then
                        Error_Msg_Uint_1 := UI_From_Int (System_Storage_Unit);
                        Error_Msg_Uint_2 := Error_Msg_Uint_1 * 8;
                        Error_Msg_N
                          ("size for primitive object must be a power of 2"
                            & " in the range ^-^", N);
                     end if;
                  end if;

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
               Flag_Non_Static_Expr
                 ("small requires static expression!", Expr);
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
         -- Storage_Pool --
         ------------------

         --  Storage_Pool attribute definition clause

         when Attribute_Storage_Pool => Storage_Pool : declare
            Pool : Entity_Id;
            T    : Entity_Id;

         begin
            if Ekind (U_Ent) = E_Access_Subprogram_Type then
               Error_Msg_N
                 ("storage pool cannot be given for access-to-subprogram type",
                  Nam);
               return;

            elsif not
              Ekind_In (U_Ent, E_Access_Type, E_General_Access_Type)
            then
               Error_Msg_N
                 ("storage pool can only be given for access types", Nam);
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

            if not Denotes_Variable (Expr) then
               Error_Msg_N ("storage pool must be a variable", Expr);
               return;
            end if;

            if Nkind (Expr) = N_Type_Conversion then
               T := Etype (Expression (Expr));
            else
               T := Etype (Expr);
            end if;

            --  The Stack_Bounded_Pool is used internally for implementing
            --  access types with a Storage_Size. Since it only work
            --  properly when used on one specific type, we need to check
            --  that it is not hijacked improperly:
            --    type T is access Integer;
            --    for T'Storage_Size use n;
            --    type Q is access Float;
            --    for Q'Storage_Size use T'Storage_Size; -- incorrect

            if RTE_Available (RE_Stack_Bounded_Pool)
              and then Base_Type (T) = RTE (RE_Stack_Bounded_Pool)
            then
               Error_Msg_N ("non-shareable internal Pool", Expr);
               return;
            end if;

            --  If the argument is a name that is not an entity name, then
            --  we construct a renaming operation to define an entity of
            --  type storage pool.

            if not Is_Entity_Name (Expr)
              and then Is_Object_Reference (Expr)
            then
               Pool := Make_Temporary (Loc, 'P', Expr);

               declare
                  Rnode : constant Node_Id :=
                            Make_Object_Renaming_Declaration (Loc,
                              Defining_Identifier => Pool,
                              Subtype_Mark        =>
                                New_Occurrence_Of (Etype (Expr), Loc),
                              Name                => Expr);

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

               Set_Associated_Storage_Pool (U_Ent, Pool);

            elsif Nkind (Expr) = N_Type_Conversion
              and then Is_Entity_Name (Expression (Expr))
              and then Nkind (Original_Node (Expr)) = N_Attribute_Reference
            then
               Pool := Entity (Expression (Expr));
               Set_Associated_Storage_Pool (U_Ent, Pool);

            else
               Error_Msg_N ("incorrect reference to a Storage Pool", Expr);
               return;
            end if;
         end Storage_Pool;

         ------------------
         -- Storage_Size --
         ------------------

         --  Storage_Size attribute definition clause

         when Attribute_Storage_Size => Storage_Size : declare
            Btype : constant Entity_Id := Base_Type (U_Ent);
            Sprag : Node_Id;

         begin
            if Is_Task_Type (U_Ent) then
               Check_Restriction (No_Obsolescent_Features, N);

               if Warn_On_Obsolescent_Feature then
                  Error_Msg_N
                    ("storage size clause for task is an " &
                     "obsolescent feature (RM J.9)?", N);
                  Error_Msg_N ("\use Storage_Size pragma instead?", N);
               end if;

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

         -----------------
         -- Stream_Size --
         -----------------

         when Attribute_Stream_Size => Stream_Size : declare
            Size : constant Uint := Static_Integer (Expr);

         begin
            if Ada_Version <= Ada_95 then
               Check_Restriction (No_Implementation_Attributes, N);
            end if;

            if Has_Stream_Size_Clause (U_Ent) then
               Error_Msg_N ("Stream_Size already given for &", Nam);

            elsif Is_Elementary_Type (U_Ent) then
               if Size /= System_Storage_Unit
                    and then
                  Size /= System_Storage_Unit * 2
                    and then
                  Size /= System_Storage_Unit * 4
                     and then
                  Size /= System_Storage_Unit * 8
               then
                  Error_Msg_Uint_1 := UI_From_Int (System_Storage_Unit);
                  Error_Msg_N
                    ("stream size for elementary type must be a"
                       & " power of 2 and at least ^", N);

               elsif RM_Size (U_Ent) > Size then
                  Error_Msg_Uint_1 := RM_Size (U_Ent);
                  Error_Msg_N
                    ("stream size for elementary type must be a"
                       & " power of 2 and at least ^", N);
               end if;

               Set_Has_Stream_Size_Clause (U_Ent);

            else
               Error_Msg_N ("Stream_Size cannot be given for &", Nam);
            end if;
         end Stream_Size;

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

            elsif Is_Array_Type (U_Ent)
              and then not Is_Constrained (U_Ent)
            then
               Error_Msg_N
                 ("Value_Size cannot be given for unconstrained array", Nam);

            else
               if Is_Elementary_Type (U_Ent) then
                  Check_Size (Expr, U_Ent, Size, Biased);
                  Set_Has_Biased_Representation (U_Ent, Biased);

                  if Biased and Warn_On_Biased_Representation then
                     Error_Msg_N
                       ("?value size clause forces biased representation", N);
                  end if;
               end if;

               Set_RM_Size (U_Ent, Size);
            end if;
         end Value_Size;

         -----------
         -- Write --
         -----------

         when Attribute_Write =>
            Analyze_Stream_TSS_Definition (TSS_Stream_Write);
            Set_Has_Specified_Stream_Write (Ent);

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

      Check_Code_Statement (N);

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
              and not Nkind_In (DeclO, N_Pragma,
                                       N_Use_Package_Clause,
                                       N_Use_Type_Clause,
                                       N_Implicit_Label_Declaration)
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
              and then not Nkind_In (StmtO, N_Pragma,
                                            N_Label,
                                            N_Code_Statement)
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
      if Ignore_Rep_Clauses then
         return;
      end if;

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

      --  Ignore rep clause on generic actual type. This will already have
      --  been flagged on the template as an error, and this is the safest
      --  way to ensure we don't get a junk cascaded message in the instance.

      if Is_Generic_Actual_Type (Enumtype) then
         return;

      --  Type must be in current scope

      elsif Scope (Enumtype) /= Current_Scope then
         Error_Msg_N ("type must be declared in this scope", Ident);
         return;

      --  Type must be a first subtype

      elsif not Is_First_Subtype (Enumtype) then
         Error_Msg_N ("cannot give enumeration rep clause for subtype", N);
         return;

      --  Ignore duplicate rep clause

      elsif Has_Enumeration_Rep_Clause (Enumtype) then
         Error_Msg_N ("duplicate enumeration rep clause ignored", N);
         return;

      --  Don't allow rep clause for standard [wide_[wide_]]character

      elsif Is_Standard_Character_Type (Enumtype) then
         Error_Msg_N ("enumeration rep clause not allowed for this type", N);
         return;

      --  Check that the expression is a proper aggregate (no parentheses)

      elsif Paren_Count (Aggr) /= 0 then
         Error_Msg
           ("extra parentheses surrounding aggregate not allowed",
            First_Sloc (Aggr));
         return;

      --  All tests passed, so set rep clause in place

      else
         Set_Has_Enumeration_Rep_Clause (Enumtype);
         Set_Has_Enumeration_Rep_Clause (Base_Type (Enumtype));
      end if;

      --  Now we process the aggregate. Note that we don't use the normal
      --  aggregate code for this purpose, because we don't want any of the
      --  normal expansion activities, and a number of special semantic
      --  rules apply (including the component type being any integer type)

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

            --  Err signals that we found some incorrect entries processing
            --  the list. The final checks for completeness and ordering are
            --  skipped in this case.

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
                     Flag_Non_Static_Expr
                       ("non-static expression used for choice!", Choice);
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

   ---------------------------
   -- Analyze_Freeze_Entity --
   ---------------------------

   procedure Analyze_Freeze_Entity (N : Node_Id) is
      E : constant Entity_Id := Entity (N);

   begin
      --  For tagged types covering interfaces add internal entities that link
      --  the primitives of the interfaces with the primitives that cover them.

      --  Note: These entities were originally generated only when generating
      --  code because their main purpose was to provide support to initialize
      --  the secondary dispatch tables. They are now generated also when
      --  compiling with no code generation to provide ASIS the relationship
      --  between interface primitives and tagged type primitives. They are
      --  also used to locate primitives covering interfaces when processing
      --  generics (see Derive_Subprograms).

      if Ada_Version >= Ada_05
        and then Ekind (E) = E_Record_Type
        and then Is_Tagged_Type (E)
        and then not Is_Interface (E)
        and then Has_Interfaces (E)
      then
         --  This would be a good common place to call the routine that checks
         --  overriding of interface primitives (and thus factorize calls to
         --  Check_Abstract_Overriding located at different contexts in the
         --  compiler). However, this is not possible because it causes
         --  spurious errors in case of late overriding.

         Add_Internal_Interface_Entities (E);
      end if;
   end Analyze_Freeze_Entity;

   ------------------------------------------
   -- Analyze_Record_Representation_Clause --
   ------------------------------------------

   --  Note: we check as much as we can here, but we can't do any checks
   --  based on the position values (e.g. overlap checks) until freeze time
   --  because especially in Ada 2005 (machine scalar mode), the processing
   --  for non-standard bit order can substantially change the positions.
   --  See procedure Check_Record_Representation_Clause (called from Freeze)
   --  for the remainder of this processing.

   procedure Analyze_Record_Representation_Clause (N : Node_Id) is
      Ident   : constant Node_Id    := Identifier (N);
      Rectype : Entity_Id;
      CC      : Node_Id;
      Posit   : Uint;
      Fbit    : Uint;
      Lbit    : Uint;
      Hbit    : Uint := Uint_0;
      Comp    : Entity_Id;
      Ocomp   : Entity_Id;
      Biased  : Boolean;

      CR_Pragma : Node_Id := Empty;
      --  Points to N_Pragma node if Complete_Representation pragma present

   begin
      if Ignore_Rep_Clauses then
         return;
      end if;

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
            AtM_Nod : Node_Id;

            Mod_Val : Uint;
            pragma Warnings (Off, Mod_Val);

         begin
            Check_Restriction (No_Obsolescent_Features, Mod_Clause (N));

            if Warn_On_Obsolescent_Feature then
               Error_Msg_N
                 ("mod clause is an obsolescent feature (RM J.8)?", N);
               Error_Msg_N
                 ("\use alignment attribute definition clause instead?", N);
            end if;

            if Present (P) then
               Analyze_List (P);
            end if;

            --  In ASIS_Mode mode, expansion is disabled, but we must convert
            --  the Mod clause into an alignment clause anyway, so that the
            --  back-end can compute and back-annotate properly the size and
            --  alignment of types that may include this record.

            --  This seems dubious, this destroys the source tree in a manner
            --  not detectable by ASIS ???

            if Operating_Mode = Check_Semantics
              and then ASIS_Mode
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

      --  For untagged types, clear any existing component clauses for the
      --  type. If the type is derived, this is what allows us to override
      --  a rep clause for the parent. For type extensions, the representation
      --  of the inherited components is inherited, so we want to keep previous
      --  component clauses for completeness.

      if not Is_Tagged_Type (Rectype) then
         Comp := First_Component_Or_Discriminant (Rectype);
         while Present (Comp) loop
            Set_Component_Clause (Comp, Empty);
            Next_Component_Or_Discriminant (Comp);
         end loop;
      end if;

      --  All done if no component clauses

      CC := First (Component_Clauses (N));

      if No (CC) then
         return;
      end if;

      --  A representation like this applies to the base type

      Set_Has_Record_Rep_Clause (Base_Type (Rectype));
      Set_Has_Non_Standard_Rep  (Base_Type (Rectype));
      Set_Has_Specified_Layout  (Base_Type (Rectype));

      --  Process the component clauses

      while Present (CC) loop

         --  Pragma

         if Nkind (CC) = N_Pragma then
            Analyze (CC);

            --  The only pragma of interest is Complete_Representation

            if Pragma_Name (CC) = Name_Complete_Representation then
               CR_Pragma := CC;
            end if;

         --  Processing for real component clause

         else
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

               --  The Last_Bit specified in a component clause must not be
               --  less than the First_Bit minus one (RM-13.5.1(10)).

               elsif Lbit < Fbit - 1 then
                  Error_Msg_N
                    ("last bit cannot be less than first bit minus one",
                     Last_Bit (CC));

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

                     --  Diagnose duplicate rep clause, or check consistency
                     --  if this is an inherited component. In a double fault,
                     --  there may be a duplicate inconsistent clause for an
                     --  inherited component.

                     if Scope (Original_Record_Component (Comp)) = Rectype
                       or else Parent (Component_Clause (Comp)) = N
                     then
                        Error_Msg_Sloc := Sloc (Component_Clause (Comp));
                        Error_Msg_N ("component clause previously given#", CC);

                     else
                        declare
                           Rep1 : constant Node_Id := Component_Clause (Comp);
                        begin
                           if Intval (Position (Rep1)) /=
                                                   Intval (Position (CC))
                             or else Intval (First_Bit (Rep1)) /=
                                                   Intval (First_Bit (CC))
                             or else Intval (Last_Bit (Rep1)) /=
                                                   Intval (Last_Bit (CC))
                           then
                              Error_Msg_N ("component clause inconsistent "
                                & "with representation of ancestor", CC);
                           elsif Warn_On_Redundant_Constructs then
                              Error_Msg_N ("?redundant component clause "
                                & "for inherited component!", CC);
                           end if;
                        end;
                     end if;

                  --  Normal case where this is the first component clause we
                  --  have seen for this entity, so set it up properly.

                  else
                     --  Make reference for field in record rep clause and set
                     --  appropriate entity field in the field identifier.

                     Generate_Reference
                       (Comp, Component_Name (CC), Set_Ref => False);
                     Set_Entity (Component_Name (CC), Comp);

                     --  Update Fbit and Lbit to the actual bit number

                     Fbit := Fbit + UI_From_Int (SSU) * Posit;
                     Lbit := Lbit + UI_From_Int (SSU) * Posit;

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

                        --  This information is also set in the corresponding
                        --  component of the base type, found by accessing the
                        --  Original_Record_Component link if it is present.

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

                        if Biased and Warn_On_Biased_Representation then
                           Error_Msg_F
                             ("?component clause forces biased "
                              & "representation", CC);
                        end if;

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

      --  Check missing components if Complete_Representation pragma appeared

      if Present (CR_Pragma) then
         Comp := First_Component_Or_Discriminant (Rectype);
         while Present (Comp) loop
            if No (Component_Clause (Comp)) then
               Error_Msg_NE
                 ("missing component clause for &", CR_Pragma, Comp);
            end if;

            Next_Component_Or_Discriminant (Comp);
         end loop;

         --  If no Complete_Representation pragma, warn if missing components

      elsif Warn_On_Unrepped_Components then
         declare
            Num_Repped_Components   : Nat := 0;
            Num_Unrepped_Components : Nat := 0;

         begin
            --  First count number of repped and unrepped components

            Comp := First_Component_Or_Discriminant (Rectype);
            while Present (Comp) loop
               if Present (Component_Clause (Comp)) then
                  Num_Repped_Components := Num_Repped_Components + 1;
               else
                  Num_Unrepped_Components := Num_Unrepped_Components + 1;
               end if;

               Next_Component_Or_Discriminant (Comp);
            end loop;

            --  We are only interested in the case where there is at least one
            --  unrepped component, and at least half the components have rep
            --  clauses. We figure that if less than half have them, then the
            --  partial rep clause is really intentional. If the component
            --  type has no underlying type set at this point (as for a generic
            --  formal type), we don't know enough to give a warning on the
            --  component.

            if Num_Unrepped_Components > 0
              and then Num_Unrepped_Components < Num_Repped_Components
            then
               Comp := First_Component_Or_Discriminant (Rectype);
               while Present (Comp) loop
                  if No (Component_Clause (Comp))
                    and then Comes_From_Source (Comp)
                    and then Present (Underlying_Type (Etype (Comp)))
                    and then (Is_Scalar_Type (Underlying_Type (Etype (Comp)))
                               or else Size_Known_At_Compile_Time
                                         (Underlying_Type (Etype (Comp))))
                    and then not Has_Warnings_Off (Rectype)
                  then
                     Error_Msg_Sloc := Sloc (Comp);
                     Error_Msg_NE
                       ("?no component clause given for & declared #",
                        N, Comp);
                  end if;

                  Next_Component_Or_Discriminant (Comp);
               end loop;
            end if;
         end;
      end if;
   end Analyze_Record_Representation_Clause;

   -----------------------------------
   -- Check_Constant_Address_Clause --
   -----------------------------------

   procedure Check_Constant_Address_Clause
     (Expr  : Node_Id;
      U_Ent : Entity_Id)
   is
      procedure Check_At_Constant_Address (Nod : Node_Id);
      --  Checks that the given node N represents a name whose 'Address is
      --  constant (in the same sense as OK_Constant_Address_Clause, i.e. the
      --  address value is the same at the point of declaration of U_Ent and at
      --  the time of elaboration of the address clause.

      procedure Check_Expr_Constants (Nod : Node_Id);
      --  Checks that Nod meets the requirements for a constant address clause
      --  in the sense of the enclosing procedure.

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
                    " depend on another address clause! (RM 13.1(22))!",
                  Nod, U_Ent);

            elsif In_Same_Source_Unit (Entity (Nod), U_Ent)
              and then Sloc (U_Ent) < Sloc (Entity (Nod))
            then
               Error_Msg_NE
                 ("invalid address clause for initialized object &!",
                  Nod, U_Ent);
               Error_Msg_Node_2 := U_Ent;
               Error_Msg_NE
                 ("\& must be defined before & (RM 13.1(22))!",
                  Nod, Entity (Nod));
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
                     " of discriminated record (RM 13.1(22))!",
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
         Loc_U_Ent : constant Source_Ptr := Sloc (U_Ent);
         Ent       : Entity_Id           := Empty;

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
               Ent := Entity (Nod);

               --  We need to look at the original node if it is different
               --  from the node, since we may have rewritten things and
               --  substituted an identifier representing the rewrite.

               if Original_Node (Nod) /= Nod then
                  Check_Expr_Constants (Original_Node (Nod));

                  --  If the node is an object declaration without initial
                  --  value, some code has been expanded, and the expression
                  --  is not constant, even if the constituents might be
                  --  acceptable, as in A'Address + offset.

                  if Ekind (Ent) = E_Variable
                    and then
                      Nkind (Declaration_Node (Ent)) = N_Object_Declaration
                    and then
                      No (Expression (Declaration_Node (Ent)))
                  then
                     Error_Msg_NE
                       ("invalid address clause for initialized object &!",
                        Nod, U_Ent);

                  --  If entity is constant, it may be the result of expanding
                  --  a check. We must verify that its declaration appears
                  --  before the object in question, else we also reject the
                  --  address clause.

                  elsif Ekind (Ent) = E_Constant
                    and then In_Same_Source_Unit (Ent, U_Ent)
                    and then Sloc (Ent) > Loc_U_Ent
                  then
                     Error_Msg_NE
                       ("invalid address clause for initialized object &!",
                        Nod, U_Ent);
                  end if;

                  return;
               end if;

               --  Otherwise look at the identifier and see if it is OK

               if Ekind_In (Ent, E_Named_Integer, E_Named_Real)
                 or else Is_Type (Ent)
               then
                  return;

               elsif
                  Ekind (Ent) = E_Constant
                    or else
                  Ekind (Ent) = E_In_Parameter
               then
                  --  This is the case where we must have Ent defined before
                  --  U_Ent. Clearly if they are in different units this
                  --  requirement is met since the unit containing Ent is
                  --  already processed.

                  if not In_Same_Source_Unit (Ent, U_Ent) then
                     return;

                  --  Otherwise location of Ent must be before the location
                  --  of U_Ent, that's what prior defined means.

                  elsif Sloc (Ent) < Loc_U_Ent then
                     return;

                  else
                     Error_Msg_NE
                       ("invalid address clause for initialized object &!",
                        Nod, U_Ent);
                     Error_Msg_Node_2 := U_Ent;
                     Error_Msg_NE
                       ("\& must be defined before & (RM 13.1(22))!",
                        Nod, Ent);
                  end if;

               elsif Nkind (Original_Node (Nod)) = N_Function_Call then
                  Check_Expr_Constants (Original_Node (Nod));

               else
                  Error_Msg_NE
                    ("invalid address clause for initialized object &!",
                     Nod, U_Ent);

                  if Comes_From_Source (Ent) then
                     Error_Msg_NE
                       ("\reference to variable& not allowed"
                          & " (RM 13.1(22))!", Nod, Ent);
                  else
                     Error_Msg_N
                       ("non-static expression not allowed"
                          & " (RM 13.1(22))!", Nod);
                  end if;
               end if;

            when N_Integer_Literal   =>

               --  If this is a rewritten unchecked conversion, in a system
               --  where Address is an integer type, always use the base type
               --  for a literal value. This is user-friendly and prevents
               --  order-of-elaboration issues with instances of unchecked
               --  conversion.

               if Nkind (Original_Node (Nod)) = N_Function_Call then
                  Set_Etype (Nod, Base_Type (Etype (Nod)));
               end if;

            when N_Real_Literal      |
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
               if Attribute_Name (Nod) = Name_Address
                   or else
                  Attribute_Name (Nod) = Name_Access
                    or else
                  Attribute_Name (Nod) = Name_Unchecked_Access
                    or else
                  Attribute_Name (Nod) = Name_Unrestricted_Access
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

            when N_Binary_Op | N_Short_Circuit | N_Membership_Test =>
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

               --  If this is a rewritten unchecked conversion, subtypes in
               --  this node are those created within the instance. To avoid
               --  order of elaboration issues, replace them with their base
               --  types. Note that address clauses can cause order of
               --  elaboration problems because they are elaborated by the
               --  back-end at the point of definition, and may mention
               --  entities declared in between (as long as everything is
               --  static). It is user-friendly to allow unchecked conversions
               --  in this context.

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
                    ("\function & is not pure (RM 13.1(22))!",
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
                 ("\must be constant defined before& (RM 13.1(22))!",
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
      --  If rep_clauses are to be ignored, no need for legality checks. In
      --  particular, no need to pester user about rep clauses that violate
      --  the rule on constant addresses, given that these clauses will be
      --  removed by Freeze before they reach the back end.

      if not Ignore_Rep_Clauses then
         Check_Expr_Constants (Expr);
      end if;
   end Check_Constant_Address_Clause;

   ----------------------------------------
   -- Check_Record_Representation_Clause --
   ----------------------------------------

   procedure Check_Record_Representation_Clause (N : Node_Id) is
      Loc     : constant Source_Ptr := Sloc (N);
      Ident   : constant Node_Id    := Identifier (N);
      Rectype : Entity_Id;
      Fent    : Entity_Id;
      CC      : Node_Id;
      Fbit    : Uint;
      Lbit    : Uint;
      Hbit    : Uint := Uint_0;
      Comp    : Entity_Id;
      Pcomp   : Entity_Id;

      Max_Bit_So_Far : Uint;
      --  Records the maximum bit position so far. If all field positions
      --  are monotonically increasing, then we can skip the circuit for
      --  checking for overlap, since no overlap is possible.

      Tagged_Parent : Entity_Id := Empty;
      --  This is set in the case of a derived tagged type for which we have
      --  Is_Fully_Repped_Tagged_Type True (indicating that all components are
      --  positioned by record representation clauses). In this case we must
      --  check for overlap between components of this tagged type, and the
      --  components of its parent. Tagged_Parent will point to this parent
      --  type. For all other cases Tagged_Parent is left set to Empty.

      Parent_Last_Bit : Uint;
      --  Relevant only if Tagged_Parent is set, Parent_Last_Bit indicates the
      --  last bit position for any field in the parent type. We only need to
      --  check overlap for fields starting below this point.

      Overlap_Check_Required : Boolean;
      --  Used to keep track of whether or not an overlap check is required

      Ccount : Natural := 0;
      --  Number of component clauses in record rep clause

      procedure Check_Component_Overlap (C1_Ent, C2_Ent : Entity_Id);
      --  Given two entities for record components or discriminants, checks
      --  if they have overlapping component clauses and issues errors if so.

      procedure Find_Component;
      --  Finds component entity corresponding to current component clause (in
      --  CC), and sets Comp to the entity, and Fbit/Lbit to the zero origin
      --  start/stop bits for the field. If there is no matching component or
      --  if the matching component does not have a component clause, then
      --  that's an error and Comp is set to Empty, but no error message is
      --  issued, since the message was already given. Comp is also set to
      --  Empty if the current "component clause" is in fact a pragma.

      -----------------------------
      -- Check_Component_Overlap --
      -----------------------------

      procedure Check_Component_Overlap (C1_Ent, C2_Ent : Entity_Id) is
         CC1 : constant Node_Id := Component_Clause (C1_Ent);
         CC2 : constant Node_Id := Component_Clause (C2_Ent);
      begin
         if Present (CC1) and then Present (CC2) then

            --  Exclude odd case where we have two tag fields in the same
            --  record, both at location zero. This seems a bit strange, but
            --  it seems to happen in some circumstances, perhaps on an error.

            if Chars (C1_Ent) = Name_uTag
                 and then
               Chars (C2_Ent) = Name_uTag
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
                  Error_Msg_Node_2 := Component_Name (CC2);
                  Error_Msg_Sloc := Sloc (Error_Msg_Node_2);
                  Error_Msg_Node_1 := Component_Name (CC1);
                  Error_Msg_N
                    ("component& overlaps & #", Component_Name (CC1));
               end if;
            end;
         end if;
      end Check_Component_Overlap;

      --------------------
      -- Find_Component --
      --------------------

      procedure Find_Component is

         procedure Search_Component (R : Entity_Id);
         --  Search components of R for a match. If found, Comp is set.

         ----------------------
         -- Search_Component --
         ----------------------

         procedure Search_Component (R : Entity_Id) is
         begin
            Comp := First_Component_Or_Discriminant (R);
            while Present (Comp) loop

               --  Ignore error of attribute name for component name (we
               --  already gave an error message for this, so no need to
               --  complain here)

               if Nkind (Component_Name (CC)) = N_Attribute_Reference then
                  null;
               else
                  exit when Chars (Comp) = Chars (Component_Name (CC));
               end if;

               Next_Component_Or_Discriminant (Comp);
            end loop;
         end Search_Component;

      --  Start of processing for Find_Component

      begin
         --  Return with Comp set to Empty if we have a pragma

         if Nkind (CC) = N_Pragma then
            Comp := Empty;
            return;
         end if;

         --  Search current record for matching component

         Search_Component (Rectype);

         --  If not found, maybe component of base type that is absent from
         --  statically constrained first subtype.

         if No (Comp) then
            Search_Component (Base_Type (Rectype));
         end if;

         --  If no component, or the component does not reference the component
         --  clause in question, then there was some previous error for which
         --  we already gave a message, so just return with Comp Empty.

         if No (Comp)
           or else Component_Clause (Comp) /= CC
         then
            Comp := Empty;

         --  Normal case where we have a component clause

         else
            Fbit := Component_Bit_Offset (Comp);
            Lbit := Fbit + Esize (Comp) - 1;
         end if;
      end Find_Component;

   --  Start of processing for Check_Record_Representation_Clause

   begin
      Find_Type (Ident);
      Rectype := Entity (Ident);

      if Rectype = Any_Type then
         return;
      else
         Rectype := Underlying_Type (Rectype);
      end if;

      --  See if we have a fully repped derived tagged type

      declare
         PS : constant Entity_Id := Parent_Subtype (Rectype);

      begin
         if Present (PS) and then Is_Fully_Repped_Tagged_Type (PS) then
            Tagged_Parent := PS;

            --  Find maximum bit of any component of the parent type

            Parent_Last_Bit := UI_From_Int (System_Address_Size - 1);
            Pcomp := First_Entity (Tagged_Parent);
            while Present (Pcomp) loop
               if Ekind_In (Pcomp, E_Discriminant, E_Component) then
                  if Component_Bit_Offset (Pcomp) /= No_Uint
                    and then Known_Static_Esize (Pcomp)
                  then
                     Parent_Last_Bit :=
                       UI_Max
                         (Parent_Last_Bit,
                          Component_Bit_Offset (Pcomp) + Esize (Pcomp) - 1);
                  end if;

                  Next_Entity (Pcomp);
               end if;
            end loop;
         end if;
      end;

      --  All done if no component clauses

      CC := First (Component_Clauses (N));

      if No (CC) then
         return;
      end if;

      --  If a tag is present, then create a component clause that places it
      --  at the start of the record (otherwise gigi may place it after other
      --  fields that have rep clauses).

      Fent := First_Entity (Rectype);

      if Nkind (Fent) = N_Defining_Identifier
        and then Chars (Fent) = Name_uTag
      then
         Set_Component_Bit_Offset    (Fent, Uint_0);
         Set_Normalized_Position     (Fent, Uint_0);
         Set_Normalized_First_Bit    (Fent, Uint_0);
         Set_Normalized_Position_Max (Fent, Uint_0);
         Init_Esize                  (Fent, System_Address_Size);

         Set_Component_Clause (Fent,
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

      Max_Bit_So_Far := Uint_Minus_1;
      Overlap_Check_Required := False;

      --  Process the component clauses

      while Present (CC) loop
         Find_Component;

         if Present (Comp) then
            Ccount := Ccount + 1;

            if Fbit <= Max_Bit_So_Far then
               Overlap_Check_Required := True;
            else
               Max_Bit_So_Far := Lbit;
            end if;

            --  Check bit position out of range of specified size

            if Has_Size_Clause (Rectype)
              and then Esize (Rectype) <= Lbit
            then
               Error_Msg_N
                 ("bit number out of range of specified size",
                  Last_Bit (CC));

               --  Check for overlap with tag field

            else
               if Is_Tagged_Type (Rectype)
                 and then Fbit < System_Address_Size
               then
                  Error_Msg_NE
                    ("component overlaps tag field of&",
                     Component_Name (CC), Rectype);
               end if;

               if Hbit < Lbit then
                  Hbit := Lbit;
               end if;
            end if;

            --  Check parent overlap if component might overlap parent field

            if Present (Tagged_Parent)
              and then Fbit <= Parent_Last_Bit
            then
               Pcomp := First_Component_Or_Discriminant (Tagged_Parent);
               while Present (Pcomp) loop
                  if not Is_Tag (Pcomp)
                    and then Chars (Pcomp) /= Name_uParent
                  then
                     Check_Component_Overlap (Comp, Pcomp);
                  end if;

                  Next_Component_Or_Discriminant (Pcomp);
               end loop;
            end if;
         end if;

         Next (CC);
      end loop;

      --  Now that we have processed all the component clauses, check for
      --  overlap. We have to leave this till last, since the components can
      --  appear in any arbitrary order in the representation clause.

      --  We do not need this check if all specified ranges were monotonic,
      --  as recorded by Overlap_Check_Required being False at this stage.

      --  This first section checks if there are any overlapping entries at
      --  all. It does this by sorting all entries and then seeing if there are
      --  any overlaps. If there are none, then that is decisive, but if there
      --  are overlaps, they may still be OK (they may result from fields in
      --  different variants).

      if Overlap_Check_Required then
         Overlap_Check1 : declare

            OC_Fbit : array (0 .. Ccount) of Uint;
            --  First-bit values for component clauses, the value is the offset
            --  of the first bit of the field from start of record. The zero
            --  entry is for use in sorting.

            OC_Lbit : array (0 .. Ccount) of Uint;
            --  Last-bit values for component clauses, the value is the offset
            --  of the last bit of the field from start of record. The zero
            --  entry is for use in sorting.

            OC_Count : Natural := 0;
            --  Count of entries in OC_Fbit and OC_Lbit

            function OC_Lt (Op1, Op2 : Natural) return Boolean;
            --  Compare routine for Sort

            procedure OC_Move (From : Natural; To : Natural);
            --  Move routine for Sort

            package Sorting is new GNAT.Heap_Sort_G (OC_Move, OC_Lt);

            -----------
            -- OC_Lt --
            -----------

            function OC_Lt (Op1, Op2 : Natural) return Boolean is
            begin
               return OC_Fbit (Op1) < OC_Fbit (Op2);
            end OC_Lt;

            -------------
            -- OC_Move --
            -------------

            procedure OC_Move (From : Natural; To : Natural) is
            begin
               OC_Fbit (To) := OC_Fbit (From);
               OC_Lbit (To) := OC_Lbit (From);
            end OC_Move;

            --  Start of processing for Overlap_Check

         begin
            CC := First (Component_Clauses (N));
            while Present (CC) loop

               --  Exclude component clause already marked in error

               if not Error_Posted (CC) then
                  Find_Component;

                  if Present (Comp) then
                     OC_Count := OC_Count + 1;
                     OC_Fbit (OC_Count) := Fbit;
                     OC_Lbit (OC_Count) := Lbit;
                  end if;
               end if;

               Next (CC);
            end loop;

            Sorting.Sort (OC_Count);

            Overlap_Check_Required := False;
            for J in 1 .. OC_Count - 1 loop
               if OC_Lbit (J) >= OC_Fbit (J + 1) then
                  Overlap_Check_Required := True;
                  exit;
               end if;
            end loop;
         end Overlap_Check1;
      end if;

      --  If Overlap_Check_Required is still True, then we have to do the full
      --  scale overlap check, since we have at least two fields that do
      --  overlap, and we need to know if that is OK since they are in
      --  different variant, or whether we have a definite problem.

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
            --  list containing the component and also, if the component is in
            --  a variant, check against components outside the case structure.
            --  This latter test is repeated recursively up the variant tree.

            Main_Component_Loop : while Present (C1_Ent) loop
               if not Ekind_In (C1_Ent, E_Component, E_Discriminant) then
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
                  --  If at outer level, check discriminants if there are any.

                  if Nkind (Clist) = N_Derived_Type_Definition then
                     Clist := Parent (Clist);
                  end if;

                  --  Outer level of record definition, check discriminants

                  if Nkind_In (Clist, N_Full_Type_Declaration,
                               N_Private_Type_Declaration)
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
                  --  component for overlap).

                  if Nkind (Parent (Clist)) = N_Variant then
                     Clist := Parent (Parent (Parent (Clist)));

                     --  Check for possible discriminant part in record, this
                     --  is treated essentially as another level in the
                     --  recursion. For this case the parent of the component
                     --  list is the record definition, and its parent is the
                     --  full type declaration containing the discriminant
                     --  specifications.

                  elsif Nkind (Parent (Clist)) = N_Record_Definition then
                     Clist := Parent (Parent ((Clist)));

                     --  If neither of these two cases, we are at the top of
                     --  the tree.

                  else
                     exit Component_List_Loop;
                  end if;
               end loop Component_List_Loop;

               <<Continue_Main_Component_Loop>>
               Next_Entity (C1_Ent);

            end loop Main_Component_Loop;
         end Overlap_Check2;
      end if;

      --  For records that have component clauses for all components, and whose
      --  size is less than or equal to 32, we need to know the size in the
      --  front end to activate possible packed array processing where the
      --  component type is a record.

      --  At this stage Hbit + 1 represents the first unused bit from all the
      --  component clauses processed, so if the component clauses are
      --  complete, then this is the length of the record.

      --  For records longer than System.Storage_Unit, and for those where not
      --  all components have component clauses, the back end determines the
      --  length (it may for example be appropriate to round up the size
      --  to some convenient boundary, based on alignment considerations, etc).

      if Unknown_RM_Size (Rectype) and then Hbit + 1 <= 32 then

         --  Nothing to do if at least one component has no component clause

         Comp := First_Component_Or_Discriminant (Rectype);
         while Present (Comp) loop
            exit when No (Component_Clause (Comp));
            Next_Component_Or_Discriminant (Comp);
         end loop;

         --  If we fall out of loop, all components have component clauses
         --  and so we can set the size to the maximum value.

         if No (Comp) then
            Set_RM_Size (Rectype, Hbit + 1);
         end if;
      end if;
   end Check_Record_Representation_Clause;

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

      --  Dismiss cases for generic types or types with previous errors

      if No (UT)
        or else UT = Any_Type
        or else Is_Generic_Type (UT)
        or else Is_Generic_Type (Root_Type (UT))
      then
         return;

      --  Check case of bit packed array

      elsif Is_Array_Type (UT)
        and then Known_Static_Component_Size (UT)
        and then Is_Bit_Packed_Array (UT)
      then
         declare
            Asiz : Uint;
            Indx : Node_Id;
            Ityp : Entity_Id;

         begin
            Asiz := Component_Size (UT);
            Indx := First_Index (UT);
            loop
               Ityp := Etype (Indx);

               --  If non-static bound, then we are not in the business of
               --  trying to check the length, and indeed an error will be
               --  issued elsewhere, since sizes of non-static array types
               --  cannot be set implicitly or explicitly.

               if not Is_Static_Subtype (Ityp) then
                  return;
               end if;

               --  Otherwise accumulate next dimension

               Asiz := Asiz * (Expr_Value (Type_High_Bound (Ityp)) -
                               Expr_Value (Type_Low_Bound  (Ityp)) +
                               Uint_1);

               Next_Index (Indx);
               exit when No (Indx);
            end loop;

            if Asiz <= Siz then
               return;
            else
               Error_Msg_Uint_1 := Asiz;
               Error_Msg_NE
                 ("size for& too small, minimum allowed is ^", N, T);
               Set_Esize   (T, Asiz);
               Set_RM_Size (T, Asiz);
            end if;
         end;

      --  All other composite types are ignored

      elsif Is_Composite_Type (UT) then
         return;

      --  For fixed-point types, don't check minimum if type is not frozen,
      --  since we don't know all the characteristics of the type that can
      --  affect the size (e.g. a specified small) till freeze time.

      elsif Is_Fixed_Point_Type (UT)
        and then not Is_Frozen (UT)
      then
         null;

      --  Cases for which a minimum check is required

      else
         --  Ignore if specified size is correct for the type

         if Known_Esize (UT) and then Siz = Esize (UT) then
            return;
         end if;

         --  Otherwise get minimum size

         M := UI_From_Int (Minimum_Size (UT));

         if Siz < M then

            --  Size is less than minimum size, but one possibility remains
            --  that we can manage with the new size if we bias the type.

            M := UI_From_Int (Minimum_Size (UT, Biased => True));

            if Siz < M then
               Error_Msg_Uint_1 := M;
               Error_Msg_NE
                 ("size for& too small, minimum allowed is ^", N, T);
               Set_Esize (T, M);
               Set_RM_Size (T, M);
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
            return   Id = Attribute_Input
              or else Id = Attribute_Output
              or else Id = Attribute_Read
              or else Id = Attribute_Write
              or else Id = Attribute_External_Tag;
         end;
      end if;
   end Is_Operational_Item;

   ------------------
   -- Minimum_Size --
   ------------------

   function Minimum_Size
     (T      : Entity_Id;
      Biased : Boolean := False) return Nat
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
      R_Typ  : constant Entity_Id := Root_Type (T);

   begin
      --  If bad type, return 0

      if T = Any_Type then
         return 0;

      --  For generic types, just return zero. There cannot be any legitimate
      --  need to know such a size, but this routine may be called with a
      --  generic type as part of normal processing.

      elsif Is_Generic_Type (R_Typ)
        or else R_Typ = Any_Type
      then
         return 0;

         --  Access types. Normally an access type cannot have a size smaller
         --  than the size of System.Address. The exception is on VMS, where
         --  we have short and long addresses, and it is possible for an access
         --  type to have a short address size (and thus be less than the size
         --  of System.Address itself). We simply skip the check for VMS, and
         --  leave it to the back end to do the check.

      elsif Is_Access_Type (T) then
         if OpenVMS_On_Target then
            return 0;
         else
            return System_Address_Size;
         end if;

      --  Floating-point types

      elsif Is_Floating_Point_Type (T) then
         return UI_To_Int (Esize (R_Typ));

      --  Discrete types

      elsif Is_Discrete_Type (T) then

         --  The following loop is looking for the nearest compile time known
         --  bounds following the ancestor subtype chain. The idea is to find
         --  the most restrictive known bounds information.

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
      --  Corresponding_Integer_Value values of the bounds, since these do not
      --  get set till the type is frozen, and this routine can be called
      --  before the type is frozen. Similarly the test for bounds being static
      --  needs to include the case where we have unanalyzed real literals for
      --  the same reason.

      elsif Is_Fixed_Point_Type (T) then

         --  The following loop is looking for the nearest compile time known
         --  bounds following the ancestor subtype chain. The idea is to find
         --  the most restrictive known bounds information.

         Ancest := T;
         loop
            if Ancest = Any_Type or else Etype (Ancest) = Any_Type then
               return 0;
            end if;

            --  Note: In the following two tests for LoSet and HiSet, it may
            --  seem redundant to test for N_Real_Literal here since normally
            --  one would assume that the test for the value being known at
            --  compile time includes this case. However, there is a glitch.
            --  If the real literal comes from folding a non-static expression,
            --  then we don't consider any non- static expression to be known
            --  at compile time if we are in configurable run time mode (needed
            --  in some cases to give a clearer definition of what is and what
            --  is not accepted). So the test is indeed needed. Without it, we
            --  would set neither Lo_Set nor Hi_Set and get an infinite loop.

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

      --  Fall through with Hi and Lo set. Deal with biased case

      if (Biased
           and then not Is_Fixed_Point_Type (T)
           and then not (Is_Enumeration_Type (T)
                          and then Has_Non_Standard_Rep (T)))
        or else Has_Biased_Representation (T)
      then
         Hi := Hi - Lo;
         Lo := Uint_0;
      end if;

      --  Signed case. Note that we consider types like range 1 .. -1 to be
      --  signed for the purpose of computing the size, since the bounds have
      --  to be accommodated in the base type.

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

   ---------------------------
   -- New_Stream_Subprogram --
   ---------------------------

   procedure New_Stream_Subprogram
     (N     : Node_Id;
      Ent   : Entity_Id;
      Subp  : Entity_Id;
      Nam   : TSS_Name_Type)
   is
      Loc       : constant Source_Ptr := Sloc (N);
      Sname     : constant Name_Id    := Make_TSS_Name (Base_Type (Ent), Nam);
      Subp_Id   : Entity_Id;
      Subp_Decl : Node_Id;
      F         : Entity_Id;
      Etyp      : Entity_Id;

      Defer_Declaration : constant Boolean :=
                            Is_Tagged_Type (Ent) or else Is_Private_Type (Ent);
      --  For a tagged type, there is a declaration for each stream attribute
      --  at the freeze point, and we must generate only a completion of this
      --  declaration. We do the same for private types, because the full view
      --  might be tagged. Otherwise we generate a declaration at the point of
      --  the attribute definition clause.

      function Build_Spec return Node_Id;
      --  Used for declaration and renaming declaration, so that this is
      --  treated as a renaming_as_body.

      ----------------
      -- Build_Spec --
      ----------------

      function Build_Spec return Node_Id is
         Out_P   : constant Boolean := (Nam = TSS_Stream_Read);
         Formals : List_Id;
         Spec    : Node_Id;
         T_Ref   : constant Node_Id := New_Reference_To (Etyp, Loc);

      begin
         Subp_Id := Make_Defining_Identifier (Loc, Sname);

         --  S : access Root_Stream_Type'Class

         Formals := New_List (
                      Make_Parameter_Specification (Loc,
                        Defining_Identifier =>
                          Make_Defining_Identifier (Loc, Name_S),
                        Parameter_Type =>
                          Make_Access_Definition (Loc,
                            Subtype_Mark =>
                              New_Reference_To (
                                Designated_Type (Etype (F)), Loc))));

         if Nam = TSS_Stream_Input then
            Spec := Make_Function_Specification (Loc,
                      Defining_Unit_Name       => Subp_Id,
                      Parameter_Specifications => Formals,
                      Result_Definition        => T_Ref);
         else
            --  V : [out] T

            Append_To (Formals,
              Make_Parameter_Specification (Loc,
                Defining_Identifier => Make_Defining_Identifier (Loc, Name_V),
                Out_Present         => Out_P,
                Parameter_Type      => T_Ref));

            Spec :=
              Make_Procedure_Specification (Loc,
                Defining_Unit_Name       => Subp_Id,
                Parameter_Specifications => Formals);
         end if;

         return Spec;
      end Build_Spec;

   --  Start of processing for New_Stream_Subprogram

   begin
      F := First_Formal (Subp);

      if Ekind (Subp) = E_Procedure then
         Etyp := Etype (Next_Formal (F));
      else
         Etyp := Etype (Subp);
      end if;

      --  Prepare subprogram declaration and insert it as an action on the
      --  clause node. The visibility for this entity is used to test for
      --  visibility of the attribute definition clause (in the sense of
      --  8.3(23) as amended by AI-195).

      if not Defer_Declaration then
         Subp_Decl :=
           Make_Subprogram_Declaration (Loc,
             Specification => Build_Spec);

      --  For a tagged type, there is always a visible declaration for each
      --  stream TSS (it is a predefined primitive operation), and the
      --  completion of this declaration occurs at the freeze point, which is
      --  not always visible at places where the attribute definition clause is
      --  visible. So, we create a dummy entity here for the purpose of
      --  tracking the visibility of the attribute definition clause itself.

      else
         Subp_Id :=
           Make_Defining_Identifier (Loc,
             Chars => New_External_Name (Sname, 'V'));
         Subp_Decl :=
           Make_Object_Declaration (Loc,
             Defining_Identifier => Subp_Id,
             Object_Definition   => New_Occurrence_Of (Standard_Boolean, Loc));
      end if;

      Insert_Action (N, Subp_Decl);
      Set_Entity (N, Subp_Id);

      Subp_Decl :=
        Make_Subprogram_Renaming_Declaration (Loc,
          Specification => Build_Spec,
          Name => New_Reference_To (Subp, Loc));

      if Defer_Declaration then
         Set_TSS (Base_Type (Ent), Subp_Id);
      else
         Insert_Action (N, Subp_Decl);
         Copy_TSS (Subp_Id, Base_Type (Ent));
      end if;
   end New_Stream_Subprogram;

   ------------------------
   -- Rep_Item_Too_Early --
   ------------------------

   function Rep_Item_Too_Early (T : Entity_Id; N : Node_Id) return Boolean is
   begin
      --  Cannot apply non-operational rep items to generic types

      if Is_Operational_Item (N) then
         return False;

      elsif Is_Type (T)
        and then Is_Generic_Type (Root_Type (T))
      then
         Error_Msg_N ("representation item not allowed for generic type", N);
         return True;
      end if;

      --  Otherwise check for incomplete type

      if Is_Incomplete_Or_Private_Type (T)
        and then No (Underlying_Type (T))
      then
         Error_Msg_N
           ("representation item must be after full type declaration", N);
         return True;

      --  If the type has incomplete components, a representation clause is
      --  illegal but stream attributes and Convention pragmas are correct.

      elsif Has_Private_Component (T) then
         if Nkind (N) = N_Pragma then
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
      FOnly : Boolean := False) return Boolean
   is
      S           : Entity_Id;
      Parent_Type : Entity_Id;

      procedure Too_Late;
      --  Output the too late message. Note that this is not considered a
      --  serious error, since the effect is simply that we ignore the
      --  representation clause in this case.

      --------------
      -- Too_Late --
      --------------

      procedure Too_Late is
      begin
         Error_Msg_N ("|representation item appears too late!", N);
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
              ("?no more representation items for }", Freeze_Node (S), S);
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

      --  No error, link item into head of chain of rep items for the entity,
      --  but avoid chaining if we have an overloadable entity, and the pragma
      --  is one that can apply to multiple overloaded entities.

      if Is_Overloadable (T)
        and then Nkind (N) = N_Pragma
      then
         declare
            Pname : constant Name_Id := Pragma_Name (N);
         begin
            if Pname = Name_Convention or else
               Pname = Name_Import     or else
               Pname = Name_Export     or else
               Pname = Name_External   or else
               Pname = Name_Interface
            then
               return False;
            end if;
         end;
      end if;

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

      --  Here the two types both have non-standard representation, and we need
      --  to determine if they have the same non-standard representation.

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

               --------------
               -- Same_Rep --
               --------------

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

            --  Start of processing for Record_Case

            begin
               if Has_Discriminants (T1) then
                  CD1 := First_Discriminant (T1);
                  CD2 := First_Discriminant (T2);

                  --  The number of discriminants may be different if the
                  --  derived type has fewer (constrained by values). The
                  --  invisible discriminants retain the representation of
                  --  the original, so the discrepancy does not per se
                  --  indicate a different representation.

                  while Present (CD1)
                    and then Present (CD2)
                  loop
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
            Sz := Standard_Character_Size;  -- May be > 8 on some targets

         elsif Lo >= -Uint_2**15 and then Hi < Uint_2**15 then
            Sz := 16;

         elsif Lo >= -Uint_2**31 and then Hi < Uint_2**31 then
            Sz := 32;

         else pragma Assert (Lo >= -Uint_2**63 and then Hi < Uint_2**63);
            Sz := 64;
         end if;

      else
         if Hi < Uint_2**08 then
            Sz := Standard_Character_Size;  -- May be > 8 on some targets

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

   ------------------------------
   -- Validate_Address_Clauses --
   ------------------------------

   procedure Validate_Address_Clauses is
   begin
      for J in Address_Clause_Checks.First .. Address_Clause_Checks.Last loop
         declare
            ACCR : Address_Clause_Check_Record
                     renames Address_Clause_Checks.Table (J);

            Expr : Node_Id;

            X_Alignment : Uint;
            Y_Alignment : Uint;

            X_Size : Uint;
            Y_Size : Uint;

         begin
            --  Skip processing of this entry if warning already posted

            if not Address_Warning_Posted (ACCR.N) then

               Expr := Original_Node (Expression (ACCR.N));

               --  Get alignments

               X_Alignment := Alignment (ACCR.X);
               Y_Alignment := Alignment (ACCR.Y);

               --  Similarly obtain sizes

               X_Size := Esize (ACCR.X);
               Y_Size := Esize (ACCR.Y);

               --  Check for large object overlaying smaller one

               if Y_Size > Uint_0
                 and then X_Size > Uint_0
                 and then X_Size > Y_Size
               then
                  Error_Msg_NE
                    ("?& overlays smaller object", ACCR.N, ACCR.X);
                  Error_Msg_N
                    ("\?program execution may be erroneous", ACCR.N);
                  Error_Msg_Uint_1 := X_Size;
                  Error_Msg_NE
                    ("\?size of & is ^", ACCR.N, ACCR.X);
                  Error_Msg_Uint_1 := Y_Size;
                  Error_Msg_NE
                    ("\?size of & is ^", ACCR.N, ACCR.Y);

               --  Check for inadequate alignment, both of the base object
               --  and of the offset, if any.

               --  Note: we do not check the alignment if we gave a size
               --  warning, since it would likely be redundant.

               elsif Y_Alignment /= Uint_0
                 and then (Y_Alignment < X_Alignment
                             or else (ACCR.Off
                                        and then
                                          Nkind (Expr) = N_Attribute_Reference
                                        and then
                                          Attribute_Name (Expr) = Name_Address
                                        and then
                                          Has_Compatible_Alignment
                                            (ACCR.X, Prefix (Expr))
                                             /= Known_Compatible))
               then
                  Error_Msg_NE
                    ("?specified address for& may be inconsistent "
                       & "with alignment",
                     ACCR.N, ACCR.X);
                  Error_Msg_N
                    ("\?program execution may be erroneous (RM 13.3(27))",
                     ACCR.N);
                  Error_Msg_Uint_1 := X_Alignment;
                  Error_Msg_NE
                    ("\?alignment of & is ^",
                     ACCR.N, ACCR.X);
                  Error_Msg_Uint_1 := Y_Alignment;
                  Error_Msg_NE
                    ("\?alignment of & is ^",
                     ACCR.N, ACCR.Y);
                  if Y_Alignment >= X_Alignment then
                     Error_Msg_N
                      ("\?but offset is not multiple of alignment",
                       ACCR.N);
                  end if;
               end if;
            end if;
         end;
      end loop;
   end Validate_Address_Clauses;

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

      --  If either type is generic, the instantiation happens within a generic
      --  unit, and there is nothing to check. The proper check
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

      --  Warn if conversion between two different convention pointers

      if Is_Access_Type (Target)
        and then Is_Access_Type (Source)
        and then Convention (Target) /= Convention (Source)
        and then Warn_On_Unchecked_Conversion
      then
         --  Give warnings for subprogram pointers only on most targets. The
         --  exception is VMS, where data pointers can have different lengths
         --  depending on the pointer convention.

         if Is_Access_Subprogram_Type (Target)
           or else Is_Access_Subprogram_Type (Source)
           or else OpenVMS_On_Target
         then
            Error_Msg_N
              ("?conversion between pointers with different conventions!", N);
         end if;
      end if;

      --  Warn if one of the operands is Ada.Calendar.Time. Do not emit a
      --  warning when compiling GNAT-related sources.

      if Warn_On_Unchecked_Conversion
        and then not In_Predefined_Unit (N)
        and then RTU_Loaded (Ada_Calendar)
        and then
          (Chars (Source) = Name_Time
             or else
           Chars (Target) = Name_Time)
      then
         --  If Ada.Calendar is loaded and the name of one of the operands is
         --  Time, there is a good chance that this is Ada.Calendar.Time.

         declare
            Calendar_Time : constant Entity_Id :=
                              Full_View (RTE (RO_CA_Time));
         begin
            pragma Assert (Present (Calendar_Time));

            if Source = Calendar_Time
              or else Target = Calendar_Time
            then
               Error_Msg_N
                 ("?representation of 'Time values may change between " &
                  "'G'N'A'T versions", N);
            end if;
         end;
      end if;

      --  Make entry in unchecked conversion table for later processing by
      --  Validate_Unchecked_Conversions, which will check sizes and alignments
      --  (using values set by the back-end where possible). This is only done
      --  if the appropriate warning is active.

      if Warn_On_Unchecked_Conversion then
         Unchecked_Conversions.Append
           (New_Val => UC_Entry'
              (Eloc   => Sloc (N),
               Source => Source,
               Target => Target));

         --  If both sizes are known statically now, then back end annotation
         --  is not required to do a proper check but if either size is not
         --  known statically, then we need the annotation.

         if Known_Static_RM_Size (Source)
           and then Known_Static_RM_Size (Target)
         then
            null;
         else
            Back_Annotate_Rep_Info := True;
         end if;
      end if;

      --  If unchecked conversion to access type, and access type is declared
      --  in the same unit as the unchecked conversion, then set the
      --  No_Strict_Aliasing flag (no strict aliasing is implicit in this
      --  situation).

      if Is_Access_Type (Target) and then
        In_Same_Source_Unit (Target, N)
      then
         Set_No_Strict_Aliasing (Implementation_Base_Type (Target));
      end if;

      --  Generate N_Validate_Unchecked_Conversion node for back end in
      --  case the back end needs to perform special validation checks.

      --  Shouldn't this be in Exp_Ch13, since the check only gets done
      --  if we have full expansion and the back end is called ???

      Vnode :=
        Make_Validate_Unchecked_Conversion (Sloc (N));
      Set_Source_Type (Vnode, Source);
      Set_Target_Type (Vnode, Target);

      --  If the unchecked conversion node is in a list, just insert before it.
      --  If not we have some strange case, not worth bothering about.

      if Is_List_Member (N) then
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

            Eloc   : constant Source_Ptr := T.Eloc;
            Source : constant Entity_Id  := T.Source;
            Target : constant Entity_Id  := T.Target;

            Source_Siz    : Uint;
            Target_Siz    : Uint;

         begin
            --  This validation check, which warns if we have unequal sizes for
            --  unchecked conversion, and thus potentially implementation
            --  dependent semantics, is one of the few occasions on which we
            --  use the official RM size instead of Esize. See description in
            --  Einfo "Handling of Type'Size Values" for details.

            if Serious_Errors_Detected = 0
              and then Known_Static_RM_Size (Source)
              and then Known_Static_RM_Size (Target)

              --  Don't do the check if warnings off for either type, note the
              --  deliberate use of OR here instead of OR ELSE to get the flag
              --  Warnings_Off_Used set for both types if appropriate.

              and then not (Has_Warnings_Off (Source)
                              or
                            Has_Warnings_Off (Target))
            then
               Source_Siz := RM_Size (Source);
               Target_Siz := RM_Size (Target);

               if Source_Siz /= Target_Siz then
                  Error_Msg
                    ("?types for unchecked conversion have different sizes!",
                     Eloc);

                  if All_Errors_Mode then
                     Error_Msg_Name_1 := Chars (Source);
                     Error_Msg_Uint_1 := Source_Siz;
                     Error_Msg_Name_2 := Chars (Target);
                     Error_Msg_Uint_2 := Target_Siz;
                     Error_Msg ("\size of % is ^, size of % is ^?", Eloc);

                     Error_Msg_Uint_1 := UI_Abs (Source_Siz - Target_Siz);

                     if Is_Discrete_Type (Source)
                       and then Is_Discrete_Type (Target)
                     then
                        if Source_Siz > Target_Siz then
                           Error_Msg
                             ("\?^ high order bits of source will be ignored!",
                              Eloc);

                        elsif Is_Unsigned_Type (Source) then
                           Error_Msg
                             ("\?source will be extended with ^ high order " &
                              "zero bits?!", Eloc);

                        else
                           Error_Msg
                             ("\?source will be extended with ^ high order " &
                              "sign bits!",
                              Eloc);
                        end if;

                     elsif Source_Siz < Target_Siz then
                        if Is_Discrete_Type (Target) then
                           if Bytes_Big_Endian then
                              Error_Msg
                                ("\?target value will include ^ undefined " &
                                 "low order bits!",
                                 Eloc);
                           else
                              Error_Msg
                                ("\?target value will include ^ undefined " &
                                 "high order bits!",
                                 Eloc);
                           end if;

                        else
                           Error_Msg
                             ("\?^ trailing bits of target value will be " &
                              "undefined!", Eloc);
                        end if;

                     else pragma Assert (Source_Siz > Target_Siz);
                        Error_Msg
                          ("\?^ trailing bits of source will be ignored!",
                           Eloc);
                     end if;
                  end if;
               end if;
            end if;

            --  If both types are access types, we need to check the alignment.
            --  If the alignment of both is specified, we can do it here.

            if Serious_Errors_Detected = 0
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

                          --  Suppress warning if warnings suppressed on either
                          --  type or either designated type. Note the use of
                          --  OR here instead of OR ELSE. That is intentional,
                          --  we would like to set flag Warnings_Off_Used in
                          --  all types for which warnings are suppressed.

                          and then not (Has_Warnings_Off (D_Source)
                                          or
                                        Has_Warnings_Off (D_Target)
                                          or
                                        Has_Warnings_Off (Source)
                                          or
                                        Has_Warnings_Off (Target))
                        then
                           Error_Msg_Uint_1 := Target_Align;
                           Error_Msg_Uint_2 := Source_Align;
                           Error_Msg_Node_1 := D_Target;
                           Error_Msg_Node_2 := D_Source;
                           Error_Msg
                             ("?alignment of & (^) is stricter than " &
                              "alignment of & (^)!", Eloc);
                           Error_Msg
                             ("\?resulting access value may have invalid " &
                              "alignment!", Eloc);
                        end if;
                     end;
                  end if;
               end;
            end if;
         end;
      end loop;
   end Validate_Unchecked_Conversions;

end Sem_Ch13;
