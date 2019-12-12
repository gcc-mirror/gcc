------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                            E X P _ S P A R K                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2019, Free Software Foundation, Inc.         --
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
with Exp_Attr;
with Exp_Ch4;
with Exp_Ch5;  use Exp_Ch5;
with Exp_Dbug; use Exp_Dbug;
with Exp_Util; use Exp_Util;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Eval; use Sem_Eval;
with Sem_Prag; use Sem_Prag;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Snames;   use Snames;
with Stand;    use Stand;
with Tbuild;   use Tbuild;
with Uintp;    use Uintp;

package body Exp_SPARK is

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Expand_SPARK_N_Attribute_Reference (N : Node_Id);
   --  Replace occurrences of System'To_Address by calls to
   --  System.Storage_Elements.To_Address

   procedure Expand_SPARK_N_Freeze_Type (E : Entity_Id);
   --  Build the DIC procedure of a type when needed, if not already done

   procedure Expand_SPARK_N_Loop_Statement (N : Node_Id);
   --  Perform loop statement-specific expansion

   procedure Expand_SPARK_N_Object_Declaration (N : Node_Id);
   --  Perform object-declaration-specific expansion

   procedure Expand_SPARK_N_Object_Renaming_Declaration (N : Node_Id);
   --  Perform name evaluation for a renamed object

   procedure Expand_SPARK_N_Op_Ne (N : Node_Id);
   --  Rewrite operator /= based on operator = when defined explicitly

   procedure Expand_SPARK_N_Selected_Component (N : Node_Id);
   --  Insert explicit dereference if required

   procedure Expand_SPARK_N_Slice_Or_Indexed_Component (N : Node_Id);
   --  Insert explicit dereference if required

   ------------------
   -- Expand_SPARK --
   ------------------

   procedure Expand_SPARK (N : Node_Id) is
   begin
      case Nkind (N) is

         --  Qualification of entity names in formal verification mode
         --  is limited to the addition of a suffix for homonyms (see
         --  Exp_Dbug.Qualify_Entity_Name). We used to qualify entity names
         --  as full expansion does, but this was removed as this prevents the
         --  verification back-end from using a short name for debugging and
         --  user interaction. The verification back-end already takes care
         --  of qualifying names when needed.

         when N_Block_Statement
            | N_Entry_Declaration
            | N_Package_Body
            | N_Package_Declaration
            | N_Protected_Type_Declaration
            | N_Subprogram_Body
            | N_Task_Type_Declaration
         =>
            Qualify_Entity_Names (N);

         --  Replace occurrences of System'To_Address by calls to
         --  System.Storage_Elements.To_Address.

         when N_Attribute_Reference =>
            Expand_SPARK_N_Attribute_Reference (N);

         when N_Expanded_Name
            | N_Identifier
         =>
            Expand_SPARK_Potential_Renaming (N);

         --  Loop iterations over arrays need to be expanded, to avoid getting
         --  two names referring to the same object in memory (the array and
         --  the iterator) in GNATprove, especially since both can be written
         --  (thus possibly leading to interferences due to aliasing). No such
         --  problem arises with quantified expressions over arrays, which are
         --  dealt with specially in GNATprove.

         when N_Loop_Statement =>
            Expand_SPARK_N_Loop_Statement (N);

         when N_Object_Declaration =>
            Expand_SPARK_N_Object_Declaration (N);

         when N_Object_Renaming_Declaration =>
            Expand_SPARK_N_Object_Renaming_Declaration (N);

         when N_Op_Ne =>
            Expand_SPARK_N_Op_Ne (N);

         when N_Freeze_Entity =>
            if Is_Type (Entity (N)) then
               Expand_SPARK_N_Freeze_Type (Entity (N));
            end if;

         when N_Indexed_Component
            | N_Slice
         =>
            Expand_SPARK_N_Slice_Or_Indexed_Component (N);

         when N_Selected_Component =>
            Expand_SPARK_N_Selected_Component (N);

         --  In SPARK mode, no other constructs require expansion

         when others =>
            null;
      end case;
   end Expand_SPARK;

   --------------------------------
   -- Expand_SPARK_N_Freeze_Type --
   --------------------------------

   procedure Expand_SPARK_N_Freeze_Type (E : Entity_Id) is
   begin
      --  When a DIC is inherited by a tagged type, it may need to be
      --  specialized to the descendant type, hence build a separate DIC
      --  procedure for it as done during regular expansion for compilation.

      if Has_DIC (E) and then Is_Tagged_Type (E) then
         Build_DIC_Procedure_Body (E, For_Freeze => True);
      end if;
   end Expand_SPARK_N_Freeze_Type;

   ----------------------------------------
   -- Expand_SPARK_N_Attribute_Reference --
   ----------------------------------------

   procedure Expand_SPARK_N_Attribute_Reference (N : Node_Id) is
      Aname   : constant Name_Id      := Attribute_Name (N);
      Attr_Id : constant Attribute_Id := Get_Attribute_Id (Aname);
      Loc     : constant Source_Ptr   := Sloc (N);
      Pref    : constant Node_Id      := Prefix (N);
      Typ     : constant Entity_Id    := Etype (N);
      Expr    : Node_Id;

   begin
      if Attr_Id = Attribute_To_Address then

         --  Extract and convert argument to expected type for call

         Expr :=
           Make_Type_Conversion (Loc,
             Subtype_Mark =>
               New_Occurrence_Of (RTE (RE_Integer_Address), Loc),
             Expression   => Relocate_Node (First (Expressions (N))));

         --  Replace attribute reference with call

         Rewrite (N,
           Make_Function_Call (Loc,
             Name                   =>
               New_Occurrence_Of (RTE (RE_To_Address), Loc),
             Parameter_Associations => New_List (Expr)));
         Analyze_And_Resolve (N, Typ);

      --  Whenever possible, replace a prefix which is an enumeration literal
      --  by the corresponding literal value.

      elsif Attr_Id = Attribute_Enum_Rep then
         declare
            Exprs : constant List_Id := Expressions (N);
         begin
            if Is_Non_Empty_List (Exprs) then
               Expr := First (Exprs);
            else
               Expr := Prefix (N);
            end if;

            --  If the argument is a literal, expand it

            if Nkind (Expr) in N_Has_Entity
              and then
                (Ekind (Entity (Expr)) = E_Enumeration_Literal
                 or else
                   (Nkind (Expr) in N_Has_Entity
                    and then Ekind (Entity (Expr)) = E_Constant
                    and then Present (Renamed_Object (Entity (Expr)))
                    and then Is_Entity_Name (Renamed_Object (Entity (Expr)))
                    and then Ekind (Entity (Renamed_Object (Entity (Expr)))) =
                      E_Enumeration_Literal))
            then
               Exp_Attr.Expand_N_Attribute_Reference (N);
            end if;
         end;

      elsif Attr_Id = Attribute_Object_Size
        or else Attr_Id = Attribute_Size
        or else Attr_Id = Attribute_Value_Size
        or else Attr_Id = Attribute_VADS_Size
      then
         Exp_Attr.Expand_Size_Attribute (N);

      --  For attributes which return Universal_Integer, introduce a conversion
      --  to the expected type with the appropriate check flags set.

      elsif Attr_Id = Attribute_Alignment
        or else Attr_Id = Attribute_Bit
        or else Attr_Id = Attribute_Bit_Position
        or else Attr_Id = Attribute_Descriptor_Size
        or else Attr_Id = Attribute_First_Bit
        or else Attr_Id = Attribute_Last_Bit
        or else Attr_Id = Attribute_Length
        or else Attr_Id = Attribute_Max_Size_In_Storage_Elements
        or else Attr_Id = Attribute_Pos
        or else Attr_Id = Attribute_Position
        or else Attr_Id = Attribute_Range_Length
        or else Attr_Id = Attribute_Aft
        or else Attr_Id = Attribute_Max_Alignment_For_Allocation
      then
         --  If the expected type is Long_Long_Integer, there will be no check
         --  flag as the compiler assumes attributes always fit in this type.
         --  Since in SPARK_Mode we do not take Storage_Error into account, we
         --  cannot make this assumption and need to produce a check.
         --  ??? It should be enough to add this check for attributes 'Length
         --  and 'Range_Length when the type is as big as Long_Long_Integer.

         declare
            Typ : Entity_Id := Empty;
         begin
            if Attr_Id = Attribute_Range_Length then
               Typ := Etype (Prefix (N));

            elsif Attr_Id = Attribute_Length then
               Typ := Etype (Prefix (N));

               declare
                  Indx : Node_Id;
                  J    : Int;

               begin
                  if Is_Access_Type (Typ) then
                     Typ := Designated_Type (Typ);
                  end if;

                  if No (Expressions (N)) then
                     J := 1;
                  else
                     J := UI_To_Int (Expr_Value (First (Expressions (N))));
                  end if;

                  Indx := First_Index (Typ);
                  while J > 1 loop
                     Next_Index (Indx);
                     J := J - 1;
                  end loop;

                  Typ := Etype (Indx);
               end;
            end if;

            Apply_Universal_Integer_Attribute_Checks (N);

            if Present (Typ)
              and then RM_Size (Typ) = RM_Size (Standard_Long_Long_Integer)
            then
               Set_Do_Overflow_Check (N);
            end if;
         end;

      elsif Attr_Id = Attribute_Constrained then

         --  If the prefix is an access to object, the attribute applies to
         --  the designated object, so rewrite with an explicit dereference.

         if Is_Access_Type (Etype (Pref))
           and then
             (not Is_Entity_Name (Pref) or else Is_Object (Entity (Pref)))
         then
            Rewrite (Pref,
                     Make_Explicit_Dereference (Loc, Relocate_Node (Pref)));
            Analyze_And_Resolve (N, Standard_Boolean);
         end if;
      end if;
   end Expand_SPARK_N_Attribute_Reference;

   -----------------------------------
   -- Expand_SPARK_N_Loop_Statement --
   -----------------------------------

   procedure Expand_SPARK_N_Loop_Statement (N : Node_Id) is
      Scheme : constant Node_Id := Iteration_Scheme (N);

   begin
      --  Loop iterations over arrays need to be expanded, to avoid getting
      --  two names referring to the same object in memory (the array and the
      --  iterator) in GNATprove, especially since both can be written (thus
      --  possibly leading to interferences due to aliasing). No such problem
      --  arises with quantified expressions over arrays, which are dealt with
      --  specially in GNATprove.

      if Present (Scheme)
        and then Present (Iterator_Specification (Scheme))
        and then Is_Iterator_Over_Array (Iterator_Specification (Scheme))
      then
         Expand_Iterator_Loop_Over_Array (N);
      end if;
   end Expand_SPARK_N_Loop_Statement;

   ---------------------------------------
   -- Expand_SPARK_N_Object_Declaration --
   ---------------------------------------

   procedure Expand_SPARK_N_Object_Declaration (N : Node_Id) is
      Loc    : constant Source_Ptr := Sloc (N);
      Obj_Id : constant Entity_Id  := Defining_Identifier (N);
      Typ    : constant Entity_Id  := Etype (Obj_Id);

      Call : Node_Id;

   begin
      --  If the object declaration denotes a variable without initialization
      --  whose type is subject to pragma Default_Initial_Condition, create
      --  and analyze a dummy call to the DIC procedure of the type in order
      --  to detect potential elaboration issues.

      if Comes_From_Source (Obj_Id)
        and then Ekind (Obj_Id) = E_Variable
        and then Has_DIC (Typ)
        and then Present (DIC_Procedure (Typ))
        and then not Has_Init_Expression (N)
      then
         Call := Build_DIC_Call (Loc, Obj_Id, Typ);

         --  Partially insert the call into the tree by setting its parent
         --  pointer.

         Set_Parent (Call, N);
         Analyze (Call);
      end if;
   end Expand_SPARK_N_Object_Declaration;

   ------------------------------------------------
   -- Expand_SPARK_N_Object_Renaming_Declaration --
   ------------------------------------------------

   procedure Expand_SPARK_N_Object_Renaming_Declaration (N : Node_Id) is
      CFS    : constant Boolean    := Comes_From_Source (N);
      Loc    : constant Source_Ptr := Sloc (N);
      Obj_Id : constant Entity_Id  := Defining_Entity (N);
      Nam    : constant Node_Id    := Name (N);
      Typ    : constant Entity_Id  := Etype (Obj_Id);

   begin
      --  Transform a renaming of the form

      --    Obj_Id : <subtype mark> renames <function call>;

      --  into

      --    Obj_Id : constant <subtype mark> := <function call>;

      --  Invoking Evaluate_Name and ultimately Remove_Side_Effects introduces
      --  a temporary to capture the function result. Once potential renamings
      --  are rewritten for SPARK, the temporary may be leaked out into source
      --  constructs and lead to confusing error diagnostics. Using an object
      --  declaration prevents this unwanted side effect.

      if Nkind (Nam) = N_Function_Call then
         Rewrite (N,
           Make_Object_Declaration (Loc,
             Defining_Identifier => Obj_Id,
             Constant_Present    => True,
             Object_Definition   => New_Occurrence_Of (Typ, Loc),
             Expression          => Nam));

         --  Inherit the original Comes_From_Source status of the renaming

         Set_Comes_From_Source (N, CFS);

         --  Sever the link to the renamed function result because the entity
         --  will no longer alias anything.

         Set_Renamed_Object (Obj_Id, Empty);

         --  Remove the entity of the renaming declaration from visibility as
         --  the analysis of the object declaration will reintroduce it again.

         Remove_Entity_And_Homonym (Obj_Id);
         Analyze (N);

      --  Otherwise unconditionally remove all side effects from the name

      else
         Evaluate_Name (Nam);
      end if;
   end Expand_SPARK_N_Object_Renaming_Declaration;

   --------------------------
   -- Expand_SPARK_N_Op_Ne --
   --------------------------

   procedure Expand_SPARK_N_Op_Ne (N : Node_Id) is
      Typ : constant Entity_Id := Etype (Left_Opnd (N));

   begin
      --  Case of elementary type with standard operator

      if Is_Elementary_Type (Typ)
        and then Sloc (Entity (N)) = Standard_Location
      then
         null;

      else
         Exp_Ch4.Expand_N_Op_Ne (N);
      end if;
   end Expand_SPARK_N_Op_Ne;

   -------------------------------------
   -- Expand_SPARK_Potential_Renaming --
   -------------------------------------

   procedure Expand_SPARK_Potential_Renaming (N : Node_Id) is
      function In_Insignificant_Pragma (Nod : Node_Id) return Boolean;
      --  Determine whether arbitrary node Nod appears within a significant
      --  pragma for SPARK.

      -----------------------------
      -- In_Insignificant_Pragma --
      -----------------------------

      function In_Insignificant_Pragma (Nod : Node_Id) return Boolean is
         Par : Node_Id;

      begin
         --  Climb the parent chain looking for an enclosing pragma

         Par := Nod;
         while Present (Par) loop
            if Nkind (Par) = N_Pragma then
               return not Pragma_Significant_In_SPARK (Get_Pragma_Id (Par));

            --  Prevent the search from going too far

            elsif Is_Body_Or_Package_Declaration (Par) then
               exit;
            end if;

            Par := Parent (Par);
         end loop;

         return False;
      end In_Insignificant_Pragma;

      --  Local variables

      Loc    : constant Source_Ptr := Sloc (N);
      Obj_Id : constant Entity_Id  := Entity (N);
      Typ    : constant Entity_Id  := Etype (N);
      Ren    : Node_Id;

   --  Start of processing for Expand_SPARK_Potential_Renaming

   begin
      --  Replace a reference to a renaming with the actual renamed object

      if Ekind (Obj_Id) in Object_Kind then
         Ren := Renamed_Object (Obj_Id);

         if Present (Ren) then

            --  Do not process a reference when it appears within a pragma of
            --  no significance to SPARK. It is assumed that the replacement
            --  will violate the semantics of the pragma and cause a spurious
            --  error.

            if In_Insignificant_Pragma (N) then
               return;

            --  Instantiations and inlining of subprograms employ "prologues"
            --  which map actual to formal parameters by means of renamings.
            --  Replace a reference to a formal by the corresponding actual
            --  parameter.

            elsif Nkind (Ren) in N_Entity then
               Rewrite (N, New_Occurrence_Of (Ren, Loc));

            --  Otherwise the renamed object denotes a name

            else
               Rewrite (N, New_Copy_Tree (Ren, New_Sloc => Loc));
               Reset_Analyzed_Flags (N);
            end if;

            Analyze_And_Resolve (N, Typ);
         end if;
      end if;
   end Expand_SPARK_Potential_Renaming;

   ---------------------------------------
   -- Expand_SPARK_N_Selected_Component --
   ---------------------------------------

   procedure Expand_SPARK_N_Selected_Component (N : Node_Id) is
      Pref : constant Node_Id   := Prefix (N);
      Typ  : constant Entity_Id := Underlying_Type (Etype (Pref));

   begin
      if Present (Typ) and then Is_Access_Type (Typ) then

         --  First set prefix type to proper access type, in case it currently
         --  has a private (non-access) view of this type.

         Set_Etype (Pref, Typ);

         Insert_Explicit_Dereference (Pref);
         Analyze_And_Resolve (Pref, Designated_Type (Typ));
      end if;
   end Expand_SPARK_N_Selected_Component;

   -----------------------------------------------
   -- Expand_SPARK_N_Slice_Or_Indexed_Component --
   -----------------------------------------------

   procedure Expand_SPARK_N_Slice_Or_Indexed_Component (N : Node_Id) is
      Pref : constant Node_Id   := Prefix (N);
      Typ  : constant Entity_Id := Etype (Pref);

   begin
      if Is_Access_Type (Typ) then
         Insert_Explicit_Dereference (Pref);
         Analyze_And_Resolve (Pref, Designated_Type (Typ));
      end if;
   end Expand_SPARK_N_Slice_Or_Indexed_Component;

end Exp_SPARK;
