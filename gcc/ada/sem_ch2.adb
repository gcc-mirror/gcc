------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                              S E M _ C H 2                               --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2024, Free Software Foundation, Inc.         --
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
with Einfo;          use Einfo;
with Einfo.Entities; use Einfo.Entities;
with Einfo.Utils;    use Einfo.Utils;
with Errout;         use Errout;
with Ghost;          use Ghost;
with Mutably_Tagged; use Mutably_Tagged;
with Namet;          use Namet;
with Nlists;         use Nlists;
with Opt;            use Opt;
with Restrict;       use Restrict;
with Rident;         use Rident;
with Sem;            use Sem;
with Sem_Ch8;        use Sem_Ch8;
with Sem_Dim;        use Sem_Dim;
with Sem_Res;        use Sem_Res;
with Sem_Type;       use Sem_Type;
with Sinfo;          use Sinfo;
with Sinfo.Nodes;    use Sinfo.Nodes;
with Sinfo.Utils;    use Sinfo.Utils;
with Stand;          use Stand;
with Uintp;          use Uintp;

package body Sem_Ch2 is

   -------------------------------
   -- Analyze_Character_Literal --
   -------------------------------

   procedure Analyze_Character_Literal (N : Node_Id) is
   begin
      --  The type is eventually inherited from the context. If expansion
      --  has already established the proper type, do not modify it.

      if No (Etype (N)) then
         Set_Etype (N, Any_Character);
      end if;

      Set_Is_Static_Expression (N);

      if Comes_From_Source (N)
        and then not In_Character_Range (UI_To_CC (Char_Literal_Value (N)))
      then
         Check_Restriction (No_Wide_Characters, N);
      end if;
   end Analyze_Character_Literal;

   ------------------------
   -- Analyze_Identifier --
   ------------------------

   procedure Analyze_Identifier (N : Node_Id) is
   begin
      --  Ignore call if prior errors, and identifier has no name, since
      --  this is the result of some kind of previous error generating a
      --  junk identifier.

      if not Is_Valid_Name (Chars (N)) and then Total_Errors_Detected /= 0 then
         return;
      else
         Find_Direct_Name (N);
      end if;

      --  Generate a conversion when we see an expanded mutably tagged type

      if Is_Mutably_Tagged_CW_Equivalent_Type (Etype (N)) then
         Make_Mutably_Tagged_Conversion (N);
      end if;

      --  A Ghost entity must appear in a specific context. Only do this
      --  checking on non-overloaded expressions, as otherwise we need to
      --  wait for resolution, and the checking is done in Resolve_Entity_Name.

      if Nkind (N) in N_Expanded_Name | N_Identifier
        and then Present (Entity (N))
        and then Is_Ghost_Entity (Entity (N))
        and then not Is_Overloaded (N)
      then
         Check_Ghost_Context (Entity (N), N);
      end if;

      Analyze_Dimension (N);
   end Analyze_Identifier;

   -----------------------------
   -- Analyze_Integer_Literal --
   -----------------------------

   procedure Analyze_Integer_Literal (N : Node_Id) is
   begin
      --  As a lexical element, an integer literal has type Universal_Integer,
      --  i.e., is compatible with any integer type. This is semantically
      --  consistent and simplifies type checking and subsequent constant
      --  folding when needed. An exception is caused by 64-bit modular types,
      --  whose upper bound is not representable in a nonstatic context that
      --  will use 64-bit integers at run time. For such cases, we need to
      --  preserve the information that the analyzed literal has that modular
      --  type. For simplicity, we preserve the information for all integer
      --  literals that result from a modular operation. This happens after
      --  prior analysis (or construction) of the literal, and after type
      --  checking and resolution.

      if No (Etype (N)) or else not Is_Modular_Integer_Type (Etype (N)) then
         Set_Etype (N, Universal_Integer);
      end if;

      Set_Is_Static_Expression (N);
   end Analyze_Integer_Literal;

   -----------------------------------------
   -- Analyze_Interpolated_String_Literal --
   -----------------------------------------

   procedure Analyze_Interpolated_String_Literal (N : Node_Id) is

      procedure Check_Ambiguous_Parameterless_Call (Func_Call : Node_Id);
      --  Examine the interpretations of the call to the given parameterless
      --  function call and report the location of each interpretation.

      ----------------------------------------
      -- Check_Ambiguous_Parameterless_Call --
      ----------------------------------------

      procedure Check_Ambiguous_Parameterless_Call (Func_Call : Node_Id) is

         procedure Report_Interpretation (E : Entity_Id);
         --  Report an interpretation of the function call

         ---------------------------
         -- Report_Interpretation --
         ---------------------------

         procedure Report_Interpretation (E : Entity_Id) is
         begin
            Error_Msg_Sloc := Sloc (E);

            if Nkind (Parent (E)) = N_Full_Type_Declaration then
               Error_Msg_N ("interpretation (inherited) #!", Func_Call);
            else
               Error_Msg_N ("interpretation #!", Func_Call);
            end if;
         end Report_Interpretation;

         --  Local variables

         Error_Reported : Boolean;
         I              : Interp_Index;
         It             : Interp;

      --  Start of processing for Check_Ambiguous_Parameterless_Call

      begin
         Error_Reported := False;

         --  Examine possible interpretations

         Get_First_Interp (Name (Func_Call), I, It);
         while Present (It.Nam) loop
            if It.Nam /= Entity (Name (Func_Call))
              and then Ekind (It.Nam) = E_Function
              and then No (First_Formal (It.Nam))
            then
               if not Error_Reported then
                  Error_Msg_NE
                    ("ambiguous call to&", Func_Call,
                     Entity (Name (Func_Call)));
                  Report_Interpretation (Entity (Name (Func_Call)));
                  Error_Reported := True;
               end if;

               Report_Interpretation (It.Nam);
            end if;

            Get_Next_Interp (I, It);
         end loop;
      end Check_Ambiguous_Parameterless_Call;

      --  Local variables

      Str_Elem : Node_Id;

   --  Start of processing for Analyze_Interpolated_String_Literal

   begin
      Set_Etype (N, Any_String);

      Str_Elem := First (Expressions (N));
      while Present (Str_Elem) loop

         --  Before analyzed, a function call that has parameter is an
         --  N_Indexed_Component node, and a call to a function that has
         --  no parameters is an N_Identifier node.

         Analyze (Str_Elem);

         --  After analyzed, if it is still an N_Identifier node then we
         --  found ambiguity and could not rewrite it as N_Function_Call.

         if Nkind (Str_Elem) = N_Identifier
           and then Ekind (Entity (Str_Elem)) = E_Function
           and then Is_Overloaded (Str_Elem)
         then
            Check_Parameterless_Call (Str_Elem);
            Check_Ambiguous_Parameterless_Call (Str_Elem);
         end if;

         Next (Str_Elem);
      end loop;
   end Analyze_Interpolated_String_Literal;

   --------------------------
   -- Analyze_Real_Literal --
   --------------------------

   procedure Analyze_Real_Literal (N : Node_Id) is
   begin
      Set_Etype (N, Universal_Real);
      Set_Is_Static_Expression (N);
   end Analyze_Real_Literal;

   ----------------------------
   -- Analyze_String_Literal --
   ----------------------------

   procedure Analyze_String_Literal (N : Node_Id) is
   begin
      --  The type is eventually inherited from the context. If expansion
      --  has already established the proper type, do not modify it.

      if No (Etype (N)) then
         Set_Etype (N, Any_String);
      end if;

      --  String literals are static in Ada 95. Note that if the subtype
      --  turns out to be non-static, then the Is_Static_Expression flag
      --  will be reset in Eval_String_Literal.

      if Ada_Version >= Ada_95 then
         Set_Is_Static_Expression (N);
      end if;

      if Comes_From_Source (N) and then Has_Wide_Character (N) then
         Check_Restriction (No_Wide_Characters, N);
      end if;
   end Analyze_String_Literal;

end Sem_Ch2;
