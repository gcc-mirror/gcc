------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                            E X P _ S P A R K                             --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2016, Free Software Foundation, Inc.         --
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
with Exp_Ch5;  use Exp_Ch5;
with Exp_Dbug; use Exp_Dbug;
with Exp_Util; use Exp_Util;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Rtsfind;  use Rtsfind;
with Sem_Eval; use Sem_Eval;
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

   procedure Expand_SPARK_Attribute_Reference (N : Node_Id);
   --  Replace occurrences of System'To_Address by calls to
   --  System.Storage_Elements.To_Address

   procedure Expand_SPARK_N_Object_Renaming_Declaration (N : Node_Id);
   --  Perform name evaluation for a renamed object

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

         when N_Expanded_Name
            | N_Identifier
         =>
            Expand_SPARK_Potential_Renaming (N);

         when N_Object_Renaming_Declaration =>
            Expand_SPARK_N_Object_Renaming_Declaration (N);

         --  Replace occurrences of System'To_Address by calls to
         --  System.Storage_Elements.To_Address

         when N_Attribute_Reference =>
            Expand_SPARK_Attribute_Reference (N);

         --  Loop iterations over arrays need to be expanded, to avoid getting
         --  two names referring to the same object in memory (the array and
         --  the iterator) in GNATprove, especially since both can be written
         --  (thus possibly leading to interferences due to aliasing). No such
         --  problem arises with quantified expressions over arrays, which are
         --  dealt with specially in GNATprove.

         when N_Loop_Statement =>
            declare
               Scheme : constant Node_Id := Iteration_Scheme (N);
            begin
               if Present (Scheme)
                 and then Present (Iterator_Specification (Scheme))
                 and then
                   Is_Iterator_Over_Array (Iterator_Specification (Scheme))
               then
                  Expand_Iterator_Loop_Over_Array (N);
               end if;
            end;

         --  In SPARK mode, no other constructs require expansion

         when others =>
            null;
      end case;
   end Expand_SPARK;

   --------------------------------------
   -- Expand_SPARK_Attribute_Reference --
   --------------------------------------

   procedure Expand_SPARK_Attribute_Reference (N : Node_Id) is
      Aname   : constant Name_Id      := Attribute_Name (N);
      Attr_Id : constant Attribute_Id := Get_Attribute_Id (Aname);
      Loc     : constant Source_Ptr   := Sloc (N);
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
        or else Attr_Id = Attribute_Object_Size
        or else Attr_Id = Attribute_Size
        or else Attr_Id = Attribute_Value_Size
        or else Attr_Id = Attribute_VADS_Size
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
      end if;
   end Expand_SPARK_Attribute_Reference;

   ------------------------------------------------
   -- Expand_SPARK_N_Object_Renaming_Declaration --
   ------------------------------------------------

   procedure Expand_SPARK_N_Object_Renaming_Declaration (N : Node_Id) is
   begin
      --  Unconditionally remove all side effects from the name

      Evaluate_Name (Name (N));
   end Expand_SPARK_N_Object_Renaming_Declaration;

   -------------------------------------
   -- Expand_SPARK_Potential_Renaming --
   -------------------------------------

   procedure Expand_SPARK_Potential_Renaming (N : Node_Id) is
      Loc    : constant Source_Ptr := Sloc (N);
      Ren_Id : constant Entity_Id  := Entity (N);
      Typ    : constant Entity_Id  := Etype (N);
      Obj_Id : Node_Id;

   begin
      --  Replace a reference to a renaming with the actual renamed object

      if Ekind (Ren_Id) in Object_Kind then
         Obj_Id := Renamed_Object (Ren_Id);

         if Present (Obj_Id) then

            --  The renamed object is an entity when instantiating generics
            --  or inlining bodies. In this case the renaming is part of the
            --  mapping "prologue" which links actuals to formals.

            if Nkind (Obj_Id) in N_Entity then
               Rewrite (N, New_Occurrence_Of (Obj_Id, Loc));

            --  Otherwise the renamed object denotes a name

            else
               Rewrite (N, New_Copy_Tree (Obj_Id, New_Sloc => Loc));
               Reset_Analyzed_Flags (N);
            end if;

            Analyze_And_Resolve (N, Typ);
         end if;
      end if;
   end Expand_SPARK_Potential_Renaming;

end Exp_SPARK;
