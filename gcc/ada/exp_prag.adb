------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ P R A G                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2003 Free Software Foundation, Inc.          --
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
with Casing;   use Casing;
with Einfo;    use Einfo;
with Errout;   use Errout;
with Exp_Ch11; use Exp_Ch11;
with Exp_Tss;  use Exp_Tss;
with Exp_Util; use Exp_Util;
with Expander; use Expander;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Eval; use Sem_Eval;
with Sem_Res;  use Sem_Res;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Sinput;   use Sinput;
with Snames;   use Snames;
with Stringt;  use Stringt;
with Stand;    use Stand;
with Targparm; use Targparm;
with Tbuild;   use Tbuild;
with Uintp;    use Uintp;

package body Exp_Prag is

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Arg1 (N : Node_Id) return Node_Id;
   function Arg2 (N : Node_Id) return Node_Id;
   --  Obtain specified Pragma_Argument_Association

   procedure Expand_Pragma_Abort_Defer             (N : Node_Id);
   procedure Expand_Pragma_Assert                  (N : Node_Id);
   procedure Expand_Pragma_Import                  (N : Node_Id);
   procedure Expand_Pragma_Import_Export_Exception (N : Node_Id);
   procedure Expand_Pragma_Inspection_Point        (N : Node_Id);
   procedure Expand_Pragma_Interrupt_Priority      (N : Node_Id);

   ----------
   -- Arg1 --
   ----------

   function Arg1 (N : Node_Id) return Node_Id is
   begin
      return First (Pragma_Argument_Associations (N));
   end Arg1;

   ----------
   -- Arg2 --
   ----------

   function Arg2 (N : Node_Id) return Node_Id is
   begin
      return Next (Arg1 (N));
   end Arg2;

   ---------------------
   -- Expand_N_Pragma --
   ---------------------

   procedure Expand_N_Pragma (N : Node_Id) is
   begin
      --  Note: we may have a pragma whose chars field is not a
      --  recognized pragma, and we must ignore it at this stage.

      if Is_Pragma_Name (Chars (N)) then
         case Get_Pragma_Id (Chars (N)) is

            --  Pragmas requiring special expander action

            when Pragma_Abort_Defer =>
               Expand_Pragma_Abort_Defer (N);

            when Pragma_Assert =>
               Expand_Pragma_Assert (N);

            when Pragma_Export_Exception =>
               Expand_Pragma_Import_Export_Exception (N);

            when Pragma_Import =>
               Expand_Pragma_Import (N);

            when Pragma_Import_Exception =>
               Expand_Pragma_Import_Export_Exception (N);

            when Pragma_Inspection_Point =>
               Expand_Pragma_Inspection_Point (N);

            when Pragma_Interrupt_Priority =>
               Expand_Pragma_Interrupt_Priority (N);

            --  All other pragmas need no expander action

            when others => null;
         end case;
      end if;

   end Expand_N_Pragma;

   -------------------------------
   -- Expand_Pragma_Abort_Defer --
   -------------------------------

   --  An Abort_Defer pragma appears as the first statement in a handled
   --  statement sequence (right after the begin). It defers aborts for
   --  the entire statement sequence, but not for any declarations or
   --  handlers (if any) associated with this statement sequence.

   --  The transformation is to transform

   --    pragma Abort_Defer;
   --    statements;

   --  into

   --    begin
   --       Abort_Defer.all;
   --       statements
   --    exception
   --       when all others =>
   --          Abort_Undefer.all;
   --          raise;
   --    at end
   --       Abort_Undefer_Direct;
   --    end;

   procedure Expand_Pragma_Abort_Defer (N : Node_Id) is
      Loc  : constant Source_Ptr := Sloc (N);
      Stm  : Node_Id;
      Stms : List_Id;
      HSS  : Node_Id;
      Blk  : constant Entity_Id :=
        New_Internal_Entity (E_Block, Current_Scope, Sloc (N), 'B');

   begin
      Stms := New_List (Build_Runtime_Call (Loc, RE_Abort_Defer));

      loop
         Stm := Remove_Next (N);
         exit when No (Stm);
         Append (Stm, Stms);
      end loop;

      HSS :=
        Make_Handled_Sequence_Of_Statements (Loc,
          Statements => Stms,
          At_End_Proc =>
            New_Occurrence_Of (RTE (RE_Abort_Undefer_Direct), Loc));

      Rewrite (N,
        Make_Block_Statement (Loc,
          Handled_Statement_Sequence => HSS));

      Set_Scope (Blk, Current_Scope);
      Set_Etype (Blk, Standard_Void_Type);
      Set_Identifier (N, New_Occurrence_Of (Blk, Sloc (N)));
      Expand_At_End_Handler (HSS, Blk);
      Analyze (N);
   end Expand_Pragma_Abort_Defer;

   --------------------------
   -- Expand_Pragma_Assert --
   --------------------------

   procedure Expand_Pragma_Assert (N : Node_Id) is
      Loc  : constant Source_Ptr := Sloc (N);
      Cond : constant Node_Id    := Expression (Arg1 (N));
      Msg  : String_Id;

   begin
      --  We already know that assertions are enabled, because otherwise
      --  the semantic pass dealt with rewriting the assertion (see Sem_Prag)

      pragma Assert (Assertions_Enabled);

      --  Since assertions are on, we rewrite the pragma with its
      --  corresponding if statement, and then analyze the statement
      --  The expansion transforms:

      --    pragma Assert (condition [,message]);

      --  into

      --    if not condition then
      --       System.Assertions.Raise_Assert_Failure (Str);
      --    end if;

      --  where Str is the message if one is present, or the default of
      --  file:line if no message is given.

      --  First, we need to prepare the character literal

      if Present (Arg2 (N)) then
         Msg := Strval (Expr_Value_S (Expression (Arg2 (N))));
      else
         Build_Location_String (Loc);
         Msg := String_From_Name_Buffer;
      end if;

      --  Now generate the if statement. Note that we consider this to be
      --  an explicit conditional in the source, not an implicit if, so we
      --  do not call Make_Implicit_If_Statement.

      Rewrite (N,
        Make_If_Statement (Loc,
          Condition =>
            Make_Op_Not (Loc,
              Right_Opnd => Cond),
          Then_Statements => New_List (
            Make_Procedure_Call_Statement (Loc,
              Name =>
                New_Reference_To (RTE (RE_Raise_Assert_Failure), Loc),
              Parameter_Associations => New_List (
                Make_String_Literal (Loc, Msg))))));

      Analyze (N);

      --  If new condition is always false, give a warning

      if Nkind (N) = N_Procedure_Call_Statement
        and then Is_RTE (Entity (Name (N)), RE_Raise_Assert_Failure)
      then
         --  If original condition was a Standard.False, we assume
         --  that this is indeed intented to raise assert error
         --  and no warning is required.

         if Is_Entity_Name (Original_Node (Cond))
           and then Entity (Original_Node (Cond)) = Standard_False
         then
            return;
         else
            Error_Msg_N ("?assertion will fail at run-time", N);
         end if;
      end if;
   end Expand_Pragma_Assert;

   --------------------------
   -- Expand_Pragma_Import --
   --------------------------

   --  When applied to a variable, the default initialization must not be
   --  done. As it is already done when the pragma is found, we just get rid
   --  of the call the initialization procedure which followed the object
   --  declaration.

   --  We can't use the freezing mechanism for this purpose, since we
   --  have to elaborate the initialization expression when it is first
   --  seen (i.e. this elaboration cannot be deferred to the freeze point).

   procedure Expand_Pragma_Import (N : Node_Id) is
      Def_Id    : constant Entity_Id := Entity (Expression (Arg2 (N)));
      Typ       : Entity_Id;
      After_Def : Node_Id;

   begin
      if Ekind (Def_Id) = E_Variable then
         Typ  := Etype (Def_Id);
         After_Def := Next (Parent (Def_Id));

         if Has_Non_Null_Base_Init_Proc (Typ)
           and then Nkind (After_Def) = N_Procedure_Call_Statement
           and then Is_Entity_Name (Name (After_Def))
           and then Entity (Name (After_Def)) = Base_Init_Proc (Typ)
         then
            Remove (After_Def);

         --  Any default initialization expression should be removed
         --  (e.g., null defaults for access objects, zero initialization
         --  of packed bit arrays). Imported objects aren't allowed to
         --  have explicit initialization, so the expression must have
         --  been generated by the compiler.

         elsif Present (Expression (Parent (Def_Id))) then
            Set_Expression (Parent (Def_Id), Empty);
         end if;
      end if;
   end Expand_Pragma_Import;

   -------------------------------------------
   -- Expand_Pragma_Import_Export_Exception --
   -------------------------------------------

   --  For a VMS exception fix up the language field with "VMS"
   --  instead of "Ada" (gigi needs this), create a constant that will be the
   --  value of the VMS condition code and stuff the Interface_Name field
   --  with the unexpanded name of the exception (if not already set).
   --  For a Ada exception, just stuff the Interface_Name field
   --  with the unexpanded name of the exception (if not already set).

   procedure Expand_Pragma_Import_Export_Exception (N : Node_Id) is
   begin
      --  This pragma is only effective on OpenVMS systems, it was ignored
      --  on non-VMS systems, and we need to ignore it here as well.

      if not OpenVMS_On_Target then
         return;
      end if;

      declare
         Id     : constant Entity_Id := Entity (Expression (Arg1 (N)));
         Call   : constant Node_Id := Register_Exception_Call (Id);
         Loc    : constant Source_Ptr := Sloc (N);

      begin
         if Present (Call) then
            declare
               Excep_Internal : constant Node_Id :=
                                 Make_Defining_Identifier
                                  (Loc, New_Internal_Name ('V'));
               Export_Pragma  : Node_Id;
               Excep_Alias    : Node_Id;
               Excep_Object   : Node_Id;
               Excep_Image : String_Id;
               Exdata      : List_Id;
               Lang1       : Node_Id;
               Lang2       : Node_Id;
               Lang3       : Node_Id;
               Code        : Node_Id;

            begin
               if Present (Interface_Name (Id)) then
                  Excep_Image := Strval (Interface_Name (Id));
               else
                  Get_Name_String (Chars (Id));
                  Set_All_Upper_Case;
                  Excep_Image := String_From_Name_Buffer;
               end if;

               Exdata := Component_Associations (Expression (Parent (Id)));

               if Is_VMS_Exception (Id) then
                  Lang1 := Next (First (Exdata));
                  Lang2 := Next (Lang1);
                  Lang3 := Next (Lang2);

                  Rewrite (Expression (Lang1),
                    Make_Character_Literal (Loc,
                      Chars => Name_uV,
                      Char_Literal_Value => Get_Char_Code ('V')));
                  Analyze (Expression (Lang1));

                  Rewrite (Expression (Lang2),
                    Make_Character_Literal (Loc,
                      Chars => Name_uM,
                      Char_Literal_Value => Get_Char_Code ('M')));
                  Analyze (Expression (Lang2));

                  Rewrite (Expression (Lang3),
                    Make_Character_Literal (Loc,
                      Chars => Name_uS,
                      Char_Literal_Value => Get_Char_Code ('S')));
                  Analyze (Expression (Lang3));

                  if Exception_Code (Id) /= No_Uint then
                     Code :=
                       Make_Integer_Literal (Loc,
                         Intval => Exception_Code (Id));

                     Excep_Object :=
                       Make_Object_Declaration (Loc,
                         Defining_Identifier => Excep_Internal,
                         Object_Definition   =>
                           New_Reference_To (Standard_Integer, Loc));

                     Insert_Action (N, Excep_Object);
                     Analyze (Excep_Object);

                     Start_String;
                     Store_String_Int
                       (UI_To_Int (Exception_Code (Id)) / 8 * 8);

                     Excep_Alias :=
                       Make_Pragma
                         (Loc,
                          Name_Linker_Alias,
                          New_List
                            (Make_Pragma_Argument_Association
                               (Sloc => Loc,
                                Expression =>
                                  New_Reference_To (Excep_Internal, Loc)),

                             Make_Pragma_Argument_Association
                               (Sloc => Loc,
                                Expression =>
                                  Make_String_Literal
                                    (Sloc => Loc,
                                     Strval => End_String))));

                     Insert_Action (N, Excep_Alias);
                     Analyze (Excep_Alias);

                     Export_Pragma :=
                       Make_Pragma
                         (Loc,
                          Name_Export,
                          New_List
                            (Make_Pragma_Argument_Association
                               (Sloc => Loc,
                                Expression => Make_Identifier (Loc, Name_C)),

                             Make_Pragma_Argument_Association
                               (Sloc => Loc,
                                Expression =>
                                  New_Reference_To (Excep_Internal, Loc)),

                             Make_Pragma_Argument_Association
                               (Sloc => Loc,
                                Expression =>
                                  Make_String_Literal
                                    (Sloc => Loc,
                                     Strval => Excep_Image)),

                             Make_Pragma_Argument_Association
                               (Sloc => Loc,
                                Expression =>
                                  Make_String_Literal
                                    (Sloc => Loc,
                                     Strval => Excep_Image))));

                     Insert_Action (N, Export_Pragma);
                     Analyze (Export_Pragma);

                  else
                     Code :=
                        Unchecked_Convert_To (Standard_Integer,
                          Make_Function_Call (Loc,
                            Name =>
                              New_Reference_To (RTE (RE_Import_Value), Loc),
                            Parameter_Associations => New_List
                              (Make_String_Literal (Loc,
                                Strval => Excep_Image))));
                  end if;

                  Rewrite (Call,
                    Make_Procedure_Call_Statement (Loc,
                      Name => New_Reference_To
                                (RTE (RE_Register_VMS_Exception), Loc),
                      Parameter_Associations => New_List (Code)));

                  Analyze_And_Resolve (Code, Standard_Integer);
                  Analyze (Call);
               end if;

               if not Present (Interface_Name (Id)) then
                  Set_Interface_Name (Id,
                     Make_String_Literal
                       (Sloc => Loc,
                        Strval => Excep_Image));
               end if;
            end;
         end if;
      end;
   end Expand_Pragma_Import_Export_Exception;

   ------------------------------------
   -- Expand_Pragma_Inspection_Point --
   ------------------------------------

   --  If no argument is given, then we supply a default argument list that
   --  includes all objects declared at the source level in all subprograms
   --  that enclose the inspection point pragma.

   procedure Expand_Pragma_Inspection_Point (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);
      A     : List_Id;
      Assoc : Node_Id;
      S     : Entity_Id;
      E     : Entity_Id;

   begin
      if No (Pragma_Argument_Associations (N)) then
         A := New_List;
         S := Current_Scope;

         while S /= Standard_Standard loop
            E := First_Entity (S);
            while Present (E) loop
               if Comes_From_Source (E)
                 and then Is_Object (E)
                 and then not Is_Entry_Formal (E)
                 and then Ekind (E) /= E_Component
                 and then Ekind (E) /= E_Discriminant
                 and then Ekind (E) /= E_Generic_In_Parameter
                 and then Ekind (E) /= E_Generic_In_Out_Parameter
               then
                  Append_To (A,
                    Make_Pragma_Argument_Association (Loc,
                      Expression => New_Occurrence_Of (E, Loc)));
               end if;

               Next_Entity (E);
            end loop;

            S := Scope (S);
         end loop;

         Set_Pragma_Argument_Associations (N, A);
      end if;

      --  Expand the arguments of the pragma. Expanding an entity reference
      --  is a noop, except in a protected operation, where a reference may
      --  have to be transformed into a reference to the corresponding prival.
      --  Are there other pragmas that may require this ???

      Assoc := First (Pragma_Argument_Associations (N));

      while Present (Assoc) loop
         Expand (Expression (Assoc));
         Next (Assoc);
      end loop;
   end Expand_Pragma_Inspection_Point;

   --------------------------------------
   -- Expand_Pragma_Interrupt_Priority --
   --------------------------------------

   --  Supply default argument if none exists (System.Interrupt_Priority'Last)

   procedure Expand_Pragma_Interrupt_Priority (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);

   begin
      if No (Pragma_Argument_Associations (N)) then
         Set_Pragma_Argument_Associations (N, New_List (
           Make_Pragma_Argument_Association (Loc,
             Expression =>
               Make_Attribute_Reference (Loc,
                 Prefix =>
                   New_Occurrence_Of (RTE (RE_Interrupt_Priority), Loc),
                 Attribute_Name => Name_Last))));
      end if;
   end Expand_Pragma_Interrupt_Priority;

end Exp_Prag;
