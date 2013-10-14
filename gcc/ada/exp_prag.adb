------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P _ P R A G                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2013, Free Software Foundation, Inc.         --
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
with Casing;   use Casing;
with Debug;    use Debug;
with Einfo;    use Einfo;
with Errout;   use Errout;
with Exp_Ch11; use Exp_Ch11;
with Exp_Util; use Exp_Util;
with Expander; use Expander;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Nmake;    use Nmake;
with Opt;      use Opt;
with Restrict; use Restrict;
with Rident;   use Rident;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Ch8;  use Sem_Ch8;
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
   function Arg3 (N : Node_Id) return Node_Id;
   --  Obtain specified pragma argument expression

   procedure Expand_Pragma_Abort_Defer             (N : Node_Id);
   procedure Expand_Pragma_Check                   (N : Node_Id);
   procedure Expand_Pragma_Common_Object           (N : Node_Id);
   procedure Expand_Pragma_Import_Or_Interface     (N : Node_Id);
   procedure Expand_Pragma_Import_Export_Exception (N : Node_Id);
   procedure Expand_Pragma_Inspection_Point        (N : Node_Id);
   procedure Expand_Pragma_Interrupt_Priority      (N : Node_Id);
   procedure Expand_Pragma_Loop_Variant            (N : Node_Id);
   procedure Expand_Pragma_Psect_Object            (N : Node_Id);
   procedure Expand_Pragma_Relative_Deadline       (N : Node_Id);

   ----------
   -- Arg1 --
   ----------

   function Arg1 (N : Node_Id) return Node_Id is
      Arg : constant Node_Id := First (Pragma_Argument_Associations (N));
   begin
      if Present (Arg)
        and then Nkind (Arg) = N_Pragma_Argument_Association
      then
         return Expression (Arg);
      else
         return Arg;
      end if;
   end Arg1;

   ----------
   -- Arg2 --
   ----------

   function Arg2 (N : Node_Id) return Node_Id is
      Arg1 : constant Node_Id := First (Pragma_Argument_Associations (N));

   begin
      if No (Arg1) then
         return Empty;

      else
         declare
            Arg : constant Node_Id := Next (Arg1);
         begin
            if Present (Arg)
              and then Nkind (Arg) = N_Pragma_Argument_Association
            then
               return Expression (Arg);
            else
               return Arg;
            end if;
         end;
      end if;
   end Arg2;

   ----------
   -- Arg3 --
   ----------

   function Arg3 (N : Node_Id) return Node_Id is
      Arg1 : constant Node_Id := First (Pragma_Argument_Associations (N));

   begin
      if No (Arg1) then
         return Empty;

      else
         declare
            Arg : Node_Id := Next (Arg1);
         begin
            if No (Arg) then
               return Empty;

            else
               Next (Arg);

               if Present (Arg)
                 and then Nkind (Arg) = N_Pragma_Argument_Association
               then
                  return Expression (Arg);
               else
                  return Arg;
               end if;
            end if;
         end;
      end if;
   end Arg3;

   ---------------------
   -- Expand_N_Pragma --
   ---------------------

   procedure Expand_N_Pragma (N : Node_Id) is
      Pname : constant Name_Id := Pragma_Name (N);

   begin
      --  Note: we may have a pragma whose Pragma_Identifier field is not a
      --  recognized pragma, and we must ignore it at this stage.

      if Is_Pragma_Name (Pname) then
         case Get_Pragma_Id (Pname) is

            --  Pragmas requiring special expander action

            when Pragma_Abort_Defer =>
               Expand_Pragma_Abort_Defer (N);

            when Pragma_Check =>
               Expand_Pragma_Check (N);

            when Pragma_Common_Object =>
               Expand_Pragma_Common_Object (N);

            when Pragma_Export_Exception =>
               Expand_Pragma_Import_Export_Exception (N);

            when Pragma_Import =>
               Expand_Pragma_Import_Or_Interface (N);

            when Pragma_Import_Exception =>
               Expand_Pragma_Import_Export_Exception (N);

            when Pragma_Inspection_Point =>
               Expand_Pragma_Inspection_Point (N);

            when Pragma_Interface =>
               Expand_Pragma_Import_Or_Interface (N);

            when Pragma_Interrupt_Priority =>
               Expand_Pragma_Interrupt_Priority (N);

            when Pragma_Loop_Variant =>
               Expand_Pragma_Loop_Variant (N);

            when Pragma_Psect_Object =>
               Expand_Pragma_Psect_Object (N);

            when Pragma_Relative_Deadline =>
               Expand_Pragma_Relative_Deadline (N);

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
   -- Expand_Pragma_Check --
   --------------------------

   procedure Expand_Pragma_Check (N : Node_Id) is
      Loc  : constant Source_Ptr := Sloc (N);
      --  Location of the pragma node. Note: it is important to use this
      --  location (and not the location of the expression) for the generated
      --  statements, otherwise the implicit return statement in the body
      --  of a pre/postcondition subprogram may inherit the source location
      --  of part of the expression, which causes confusing debug information
      --  to be generated, which interferes with coverage analysis tools.

      Cond : constant Node_Id := Arg2 (N);
      Nam  : constant Name_Id := Chars (Arg1 (N));
      Msg  : Node_Id;

   begin
      --  Nothing to do if pragma is ignored

      if Is_Ignored (N) then
         return;
      end if;

      --  Since this check is active, we rewrite the pragma into a
      --  corresponding if statement, and then analyze the statement

      --  The normal case expansion transforms:

      --    pragma Check (name, condition [,message]);

      --  into

      --    if not condition then
      --       System.Assertions.Raise_Assert_Failure (Str);
      --    end if;

      --  where Str is the message if one is present, or the default of
      --  name failed at file:line if no message is given (the "name failed
      --  at" is omitted for name = Assertion, since it is redundant, given
      --  that the name of the exception is Assert_Failure.)

      --  Also, instead of "XXX failed at", we generate slightly
      --  different messages for some of the contract assertions (see
      --  code below for details).

      --  An alternative expansion is used when the No_Exception_Propagation
      --  restriction is active and there is a local Assert_Failure handler.
      --  This is not a common combination of circumstances, but it occurs in
      --  the context of Aunit and the zero footprint profile. In this case we
      --  generate:

      --    if not condition then
      --       raise Assert_Failure;
      --    end if;

      --  This will then be transformed into a goto, and the local handler will
      --  be able to handle the assert error (which would not be the case if a
      --  call is made to the Raise_Assert_Failure procedure).

      --  We also generate the direct raise if the Suppress_Exception_Locations
      --  is active, since we don't want to generate messages in this case.

      --  Note that the reason we do not always generate a direct raise is that
      --  the form in which the procedure is called allows for more efficient
      --  breakpointing of assertion errors.

      --  Generate the appropriate if statement. Note that we consider this to
      --  be an explicit conditional in the source, not an implicit if, so we
      --  do not call Make_Implicit_If_Statement.

      --  Case where we generate a direct raise

      if ((Debug_Flag_Dot_G
           or else Restriction_Active (No_Exception_Propagation))
          and then Present (Find_Local_Handler (RTE (RE_Assert_Failure), N)))
        or else (Opt.Exception_Locations_Suppressed and then No (Arg3 (N)))
      then
         Rewrite (N,
           Make_If_Statement (Loc,
             Condition =>
               Make_Op_Not (Loc,
                 Right_Opnd => Cond),
             Then_Statements => New_List (
               Make_Raise_Statement (Loc,
                 Name =>
                   New_Reference_To (RTE (RE_Assert_Failure), Loc)))));

      --  Case where we call the procedure

      else
         --  If we have a message given, use it

         if Present (Arg3 (N)) then
            Msg := Get_Pragma_Arg (Arg3 (N));

         --  Here we have no string, so prepare one

         else
            declare
               Msg_Loc : constant String :=
                           Build_Location_String (Sloc (First_Node (Cond)));
               --  Source location used in the case of a failed assertion:
               --  point to the failing condition, not Loc. Note that the
               --  source location of the expression is not usually the best
               --  choice here. For example, it gets located on the last AND
               --  keyword in a chain of boolean expressiond AND'ed together.
               --  It is best to put the message on the first character of the
               --  condition, which is the effect of the First_Node call here.

            begin
               Name_Len := 0;

               --  For Assert, we just use the location

               if Nam = Name_Assert then
                  null;

               --  For predicate, we generate the string "predicate failed
               --  at yyy". We prefer all lower case for predicate.

               elsif Nam = Name_Predicate then
                  Add_Str_To_Name_Buffer ("predicate failed at ");

               --  For special case of Precondition/Postcondition the string is
               --  "failed xx from yy" where xx is precondition/postcondition
               --  in all lower case. The reason for this different wording is
               --  that the failure is not at the point of occurrence of the
               --  pragma, unlike the other Check cases.

               elsif Nam_In (Nam, Name_Precondition, Name_Postcondition) then
                  Get_Name_String (Nam);
                  Insert_Str_In_Name_Buffer ("failed ", 1);
                  Add_Str_To_Name_Buffer (" from ");

               --  For special case of Invariant, the string is "failed
               --  invariant from yy", to be consistent with the string that is
               --  generated for the aspect case (the code later on checks for
               --  this specific string to modify it in some cases, so this is
               --  functionally important).

               elsif Nam = Name_Invariant then
                  Add_Str_To_Name_Buffer ("failed invariant from ");

               --  For all other checks, the string is "xxx failed at yyy"
               --  where xxx is the check name with current source file casing.

               else
                  Get_Name_String (Nam);
                  Set_Casing (Identifier_Casing (Current_Source_File));
                  Add_Str_To_Name_Buffer (" failed at ");
               end if;

               --  In all cases, add location string

               Add_Str_To_Name_Buffer (Msg_Loc);

               --  Build the message

               Msg := Make_String_Literal (Loc, Name_Buffer (1 .. Name_Len));
            end;
         end if;

         --  Now rewrite as an if statement

         Rewrite (N,
           Make_If_Statement (Loc,
             Condition =>
               Make_Op_Not (Loc,
                 Right_Opnd => Cond),
             Then_Statements => New_List (
               Make_Procedure_Call_Statement (Loc,
                 Name =>
                   New_Reference_To (RTE (RE_Raise_Assert_Failure), Loc),
                 Parameter_Associations => New_List (Relocate_Node (Msg))))));
      end if;

      Analyze (N);

      --  If new condition is always false, give a warning

      if Warn_On_Assertion_Failure
        and then Nkind (N) = N_Procedure_Call_Statement
        and then Is_RTE (Entity (Name (N)), RE_Raise_Assert_Failure)
      then
         --  If original condition was a Standard.False, we assume that this is
         --  indeed intended to raise assert error and no warning is required.

         if Is_Entity_Name (Original_Node (Cond))
           and then Entity (Original_Node (Cond)) = Standard_False
         then
            return;

         elsif Nam = Name_Assert then
            Error_Msg_N ("?A?assertion will fail at run time", N);
         else

            Error_Msg_N ("?A?check will fail at run time", N);
         end if;
      end if;
   end Expand_Pragma_Check;

   ---------------------------------
   -- Expand_Pragma_Common_Object --
   ---------------------------------

   --  Use a machine attribute to replicate semantic effect in DEC Ada

   --    pragma Machine_Attribute (intern_name, "common_object", extern_name);

   --  For now we do nothing with the size attribute ???

   --  Note: Psect_Object shares this processing

   procedure Expand_Pragma_Common_Object (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);

      Internal : constant Node_Id := Arg1 (N);
      External : constant Node_Id := Arg2 (N);

      Psect : Node_Id;
      --  Psect value upper cased as string literal

      Iloc : constant Source_Ptr := Sloc (Internal);
      Eloc : constant Source_Ptr := Sloc (External);
      Ploc : Source_Ptr;

   begin
      --  Acquire Psect value and fold to upper case

      if Present (External) then
         if Nkind (External) = N_String_Literal then
            String_To_Name_Buffer (Strval (External));
         else
            Get_Name_String (Chars (External));
         end if;

         Set_All_Upper_Case;

         Psect :=
           Make_String_Literal (Eloc,
             Strval => String_From_Name_Buffer);

      else
         Get_Name_String (Chars (Internal));
         Set_All_Upper_Case;
         Psect :=
           Make_String_Literal (Iloc,
             Strval => String_From_Name_Buffer);
      end if;

      Ploc := Sloc (Psect);

      --  Insert the pragma

      Insert_After_And_Analyze (N,
         Make_Pragma (Loc,
           Chars                        => Name_Machine_Attribute,
           Pragma_Argument_Associations => New_List (
             Make_Pragma_Argument_Association (Iloc,
               Expression => New_Copy_Tree (Internal)),
             Make_Pragma_Argument_Association (Eloc,
               Expression =>
                 Make_String_Literal (Sloc => Ploc,
                   Strval => "common_object")),
             Make_Pragma_Argument_Association (Ploc,
               Expression => New_Copy_Tree (Psect)))));

   end Expand_Pragma_Common_Object;

   ---------------------------------------
   -- Expand_Pragma_Import_Or_Interface --
   ---------------------------------------

   --  When applied to a variable, the default initialization must not be done.
   --  As it is already done when the pragma is found, we just get rid of the
   --  call the initialization procedure which followed the object declaration.
   --  The call is inserted after the declaration, but validity checks may
   --  also have been inserted and the initialization call does not necessarily
   --  appear immediately after the object declaration.

   --  We can't use the freezing mechanism for this purpose, since we have to
   --  elaborate the initialization expression when it is first seen (i.e. this
   --  elaboration cannot be deferred to the freeze point).

   procedure Expand_Pragma_Import_Or_Interface (N : Node_Id) is
      Def_Id    : Entity_Id;
      Init_Call : Node_Id;

   begin
      Def_Id := Entity (Arg2 (N));
      if Ekind (Def_Id) = E_Variable then

         --  Find and remove generated initialization call for object, if any

         Init_Call := Remove_Init_Call (Def_Id, Rep_Clause => N);

         --  Any default initialization expression should be removed (e.g.,
         --  null defaults for access objects, zero initialization of packed
         --  bit arrays). Imported objects aren't allowed to have explicit
         --  initialization, so the expression must have been generated by
         --  the compiler.

         if No (Init_Call) and then Present (Expression (Parent (Def_Id))) then
            Set_Expression (Parent (Def_Id), Empty);
         end if;
      end if;
   end Expand_Pragma_Import_Or_Interface;

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
         Id     : constant Entity_Id := Entity (Arg1 (N));
         Call   : constant Node_Id := Register_Exception_Call (Id);
         Loc    : constant Source_Ptr := Sloc (N);

      begin
         if Present (Call) then
            declare
               Excep_Internal : constant Node_Id := Make_Temporary (Loc, 'V');
               Export_Pragma  : Node_Id;
               Excep_Alias    : Node_Id;
               Excep_Object   : Node_Id;
               Excep_Image    : String_Id;
               Exdata         : List_Id;
               Lang_Char      : Node_Id;
               Code           : Node_Id;

            begin
               --  Compute the symbol for the code of the condition

               if Present (Interface_Name (Id)) then
                  Excep_Image := Strval (Interface_Name (Id));
               else
                  Get_Name_String (Chars (Id));
                  Set_All_Upper_Case;
                  Excep_Image := String_From_Name_Buffer;
               end if;

               Exdata := Component_Associations (Expression (Parent (Id)));

               if Is_VMS_Exception (Id) then
                  Lang_Char := Next (First (Exdata));

                  --  Change the one-character language designator to 'V'

                  Rewrite (Expression (Lang_Char),
                    Make_Character_Literal (Loc,
                      Chars => Name_uV,
                      Char_Literal_Value =>
                        UI_From_Int (Character'Pos ('V'))));
                  Analyze (Expression (Lang_Char));

                  if Exception_Code (Id) /= No_Uint then

                     --  The code for the exception is present. Create a linker
                     --  alias to define the symbol.

                     Code :=
                       Unchecked_Convert_To (Standard_A_Char,
                         Make_Integer_Literal (Loc,
                           Intval => Exception_Code (Id)));

                     --  Declare a dummy object

                     Excep_Object :=
                       Make_Object_Declaration (Loc,
                         Defining_Identifier => Excep_Internal,
                         Object_Definition   =>
                           New_Reference_To (RTE (RE_Address), Loc));

                     Insert_Action (N, Excep_Object);
                     Analyze (Excep_Object);

                     --  Clear severity bits

                     Start_String;
                     Store_String_Int
                       (UI_To_Int (Exception_Code (Id)) / 8 * 8);

                     --  Insert a pragma Linker_Alias to set the value of the
                     --  dummy object symbol.

                     Excep_Alias :=
                       Make_Pragma (Loc,
                         Chars                        => Name_Linker_Alias,
                         Pragma_Argument_Associations => New_List (
                           Make_Pragma_Argument_Association (Loc,
                             Expression =>
                               New_Reference_To (Excep_Internal, Loc)),

                           Make_Pragma_Argument_Association (Loc,
                             Expression =>
                               Make_String_Literal (Loc, End_String))));

                     Insert_Action (N, Excep_Alias);
                     Analyze (Excep_Alias);

                     --  Insert a pragma Export to give a Linker_Name to the
                     --  dummy object.

                     Export_Pragma :=
                       Make_Pragma (Loc,
                         Chars                        => Name_Export,
                         Pragma_Argument_Associations => New_List (
                           Make_Pragma_Argument_Association (Loc,
                             Expression => Make_Identifier (Loc, Name_C)),

                           Make_Pragma_Argument_Association (Loc,
                             Expression =>
                               New_Reference_To (Excep_Internal, Loc)),

                           Make_Pragma_Argument_Association (Loc,
                             Expression =>
                               Make_String_Literal (Loc, Excep_Image)),

                           Make_Pragma_Argument_Association (Loc,
                             Expression =>
                               Make_String_Literal (Loc, Excep_Image))));

                     Insert_Action (N, Export_Pragma);
                     Analyze (Export_Pragma);

                  else
                     Code :=
                        Make_Function_Call (Loc,
                          Name                   =>
                            New_Reference_To (RTE (RE_Import_Address), Loc),
                          Parameter_Associations => New_List
                            (Make_String_Literal (Loc,
                              Strval => Excep_Image)));
                  end if;

                  --  Generate the call to Register_VMS_Exception

                  Rewrite (Call,
                    Make_Procedure_Call_Statement (Loc,
                      Name => New_Reference_To
                                (RTE (RE_Register_VMS_Exception), Loc),
                      Parameter_Associations => New_List (
                        Code,
                        Unchecked_Convert_To (RTE (RE_Exception_Data_Ptr),
                          Make_Attribute_Reference (Loc,
                            Prefix         => New_Occurrence_Of (Id, Loc),
                            Attribute_Name => Name_Unrestricted_Access)))));

                  Analyze_And_Resolve (Code, RTE (RE_Address));
                  Analyze (Call);
               end if;

               if No (Interface_Name (Id)) then
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

   --------------------------------
   -- Expand_Pragma_Loop_Variant --
   --------------------------------

   --  Pragma Loop_Variant is expanded in the following manner:

   --  Original code

   --     for | while ... loop
   --        <preceding source statements>
   --        pragma Loop_Variant
   --                 (Increases => Incr_Expr,
   --                  Decreases => Decr_Expr);
   --        <succeeding source statements>
   --     end loop;

   --  Expanded code

   --     Curr_1 : <type of Incr_Expr>;
   --     Curr_2 : <type of Decr_Expr>;
   --     Old_1  : <type of Incr_Expr>;
   --     Old_2  : <type of Decr_Expr>;
   --     Flag   : Boolean := False;

   --     for | while ... loop
   --        <preceding source statements>

   --        if Flag then
   --           Old_1 := Curr_1;
   --           Old_2 := Curr_2;
   --        end if;

   --        Curr_1 := <Incr_Expr>;
   --        Curr_2 := <Decr_Expr>;

   --        if Flag then
   --           if Curr_1 /= Old_1 then
   --              pragma Check (Loop_Variant, Curr_1 > Old_1);
   --           else
   --              pragma Check (Loop_Variant, Curr_2 < Old_2);
   --           end if;
   --        else
   --           Flag := True;
   --        end if;

   --        <succeeding source statements>
   --     end loop;

   procedure Expand_Pragma_Loop_Variant (N : Node_Id) is
      Loc : constant Source_Ptr := Sloc (N);

      Last_Var : constant Node_Id := Last (Pragma_Argument_Associations (N));

      Curr_Assign : List_Id             := No_List;
      Flag_Id     : Entity_Id           := Empty;
      If_Stmt     : Node_Id             := Empty;
      Old_Assign  : List_Id             := No_List;
      Loop_Scop   : Entity_Id;
      Loop_Stmt   : Node_Id;
      Variant     : Node_Id;

      procedure Process_Variant (Variant : Node_Id; Is_Last : Boolean);
      --  Process a single increasing / decreasing termination variant. Flag
      --  Is_Last should be set when processing the last variant.

      ---------------------
      -- Process_Variant --
      ---------------------

      procedure Process_Variant (Variant : Node_Id; Is_Last : Boolean) is
         function Make_Op
           (Loc      : Source_Ptr;
            Curr_Val : Node_Id;
            Old_Val  : Node_Id) return Node_Id;
         --  Generate a comparison between Curr_Val and Old_Val depending on
         --  the change mode (Increases / Decreases) of the variant.

         -------------
         -- Make_Op --
         -------------

         function Make_Op
           (Loc      : Source_Ptr;
            Curr_Val : Node_Id;
            Old_Val  : Node_Id) return Node_Id
         is
         begin
            if Chars (Variant) = Name_Increases then
               return Make_Op_Gt (Loc, Curr_Val, Old_Val);
            else pragma Assert (Chars (Variant) = Name_Decreases);
               return Make_Op_Lt (Loc, Curr_Val, Old_Val);
            end if;
         end Make_Op;

         --  Local variables

         Expr     : constant Node_Id := Expression (Variant);
         Expr_Typ : constant Entity_Id := Etype (Expr);
         Loc      : constant Source_Ptr := Sloc (Expr);
         Loop_Loc : constant Source_Ptr := Sloc (Loop_Stmt);
         Curr_Id  : Entity_Id;
         Old_Id   : Entity_Id;
         Prag     : Node_Id;

      --  Start of processing for Process_Variant

      begin
         --  All temporaries generated in this routine must be inserted before
         --  the related loop statement. Ensure that the proper scope is on the
         --  stack when analyzing the temporaries. Note that we also use the
         --  Sloc of the related loop.

         Push_Scope (Scope (Loop_Scop));

         --  Step 1: Create the declaration of the flag which controls the
         --  behavior of the assertion on the first iteration of the loop.

         if No (Flag_Id) then

            --  Generate:
            --    Flag : Boolean := False;

            Flag_Id := Make_Temporary (Loop_Loc, 'F');

            Insert_Action (Loop_Stmt,
              Make_Object_Declaration (Loop_Loc,
                Defining_Identifier => Flag_Id,
                Object_Definition   =>
                  New_Reference_To (Standard_Boolean, Loop_Loc),
                Expression          =>
                  New_Reference_To (Standard_False, Loop_Loc)));

            --  Prevent an unwanted optimization where the Current_Value of
            --  the flag eliminates the if statement which stores the variant
            --  values coming from the previous iteration.

            --     Flag : Boolean := False;
            --     loop
            --        if Flag then         --  condition rewritten to False
            --           Old_N := Curr_N;  --  and if statement eliminated
            --        end if;
            --        . . .
            --        Flag := True;
            --     end loop;

            Set_Current_Value (Flag_Id, Empty);
         end if;

         --  Step 2: Create the temporaries which store the old and current
         --  values of the associated expression.

         --  Generate:
         --    Curr : <type of Expr>;

         Curr_Id := Make_Temporary (Loc, 'C');

         Insert_Action (Loop_Stmt,
           Make_Object_Declaration (Loop_Loc,
             Defining_Identifier => Curr_Id,
             Object_Definition   => New_Reference_To (Expr_Typ, Loop_Loc)));

         --  Generate:
         --    Old : <type of Expr>;

         Old_Id := Make_Temporary (Loc, 'P');

         Insert_Action (Loop_Stmt,
           Make_Object_Declaration (Loop_Loc,
             Defining_Identifier => Old_Id,
             Object_Definition   => New_Reference_To (Expr_Typ, Loop_Loc)));

         --  Restore original scope after all temporaries have been analyzed

         Pop_Scope;

         --  Step 3: Store value of the expression from the previous iteration

         if No (Old_Assign) then
            Old_Assign := New_List;
         end if;

         --  Generate:
         --    Old := Curr;

         Append_To (Old_Assign,
           Make_Assignment_Statement (Loc,
             Name       => New_Reference_To (Old_Id, Loc),
             Expression => New_Reference_To (Curr_Id, Loc)));

         --  Step 4: Store the current value of the expression

         if No (Curr_Assign) then
            Curr_Assign := New_List;
         end if;

         --  Generate:
         --    Curr := <Expr>;

         Append_To (Curr_Assign,
           Make_Assignment_Statement (Loc,
             Name       => New_Reference_To (Curr_Id, Loc),
             Expression => Relocate_Node (Expr)));

         --  Step 5: Create corresponding assertion to verify change of value

         --  Generate:
         --    pragma Check (Loop_Variant, Curr <|> Old);

         Prag :=
           Make_Pragma (Loc,
             Chars                        => Name_Check,
             Pragma_Argument_Associations => New_List (
               Make_Pragma_Argument_Association (Loc,
                 Expression => Make_Identifier (Loc, Name_Loop_Variant)),
               Make_Pragma_Argument_Association (Loc,
                 Expression =>
                   Make_Op (Loc,
                     Curr_Val => New_Reference_To (Curr_Id, Loc),
                     Old_Val  => New_Reference_To (Old_Id, Loc)))));

         --  Generate:
         --    if Curr /= Old then
         --       <Prag>;

         if No (If_Stmt) then

            --  When there is just one termination variant, do not compare the
            --  old and current value for equality, just check the pragma.

            if Is_Last then
               If_Stmt := Prag;
            else
               If_Stmt :=
                 Make_If_Statement (Loc,
                   Condition       =>
                     Make_Op_Ne (Loc,
                       Left_Opnd  => New_Reference_To (Curr_Id, Loc),
                       Right_Opnd => New_Reference_To (Old_Id, Loc)),
                   Then_Statements => New_List (Prag));
            end if;

         --  Generate:
         --    else
         --       <Prag>;
         --    end if;

         elsif Is_Last then
            Set_Else_Statements (If_Stmt, New_List (Prag));

         --  Generate:
         --    elsif Curr /= Old then
         --       <Prag>;

         else
            if Elsif_Parts (If_Stmt) = No_List then
               Set_Elsif_Parts (If_Stmt, New_List);
            end if;

            Append_To (Elsif_Parts (If_Stmt),
              Make_Elsif_Part (Loc,
                Condition       =>
                  Make_Op_Ne (Loc,
                    Left_Opnd  => New_Reference_To (Curr_Id, Loc),
                    Right_Opnd => New_Reference_To (Old_Id, Loc)),
                Then_Statements => New_List (Prag)));
         end if;
      end Process_Variant;

   --  Start of processing for Expand_Pragma_Loop_Variant

   begin
      --  If pragma is not enabled, rewrite as Null statement. If pragma is
      --  disabled, it has already been rewritten as a Null statement.

      if Is_Ignored (N) then
         Rewrite (N, Make_Null_Statement (Loc));
         Analyze (N);
         return;
      end if;

      --  Locate the enclosing loop for which this assertion applies. In the
      --  case of Ada 2012 array iteration, we might be dealing with nested
      --  loops. Only the outermost loop has an identifier.

      Loop_Stmt := N;
      while Present (Loop_Stmt) loop
         if Nkind (Loop_Stmt) = N_Loop_Statement
           and then Present (Identifier (Loop_Stmt))
         then
            exit;
         end if;

         Loop_Stmt := Parent (Loop_Stmt);
      end loop;

      Loop_Scop := Entity (Identifier (Loop_Stmt));

      --  Create the circuitry which verifies individual variants

      Variant := First (Pragma_Argument_Associations (N));
      while Present (Variant) loop
         Process_Variant (Variant, Is_Last => Variant = Last_Var);

         Next (Variant);
      end loop;

      --  Construct the segment which stores the old values of all expressions.
      --  Generate:
      --    if Flag then
      --       <Old_Assign>
      --    end if;

      Insert_Action (N,
        Make_If_Statement (Loc,
          Condition       => New_Reference_To (Flag_Id, Loc),
          Then_Statements => Old_Assign));

      --  Update the values of all expressions

      Insert_Actions (N, Curr_Assign);

      --  Add the assertion circuitry to test all changes in expressions.
      --  Generate:
      --    if Flag then
      --       <If_Stmt>
      --    else
      --       Flag := True;
      --    end if;

      Insert_Action (N,
        Make_If_Statement (Loc,
          Condition       => New_Reference_To (Flag_Id, Loc),
          Then_Statements => New_List (If_Stmt),
          Else_Statements => New_List (
            Make_Assignment_Statement (Loc,
              Name       => New_Reference_To (Flag_Id, Loc),
              Expression => New_Reference_To (Standard_True, Loc)))));

      --  Note: the pragma has been completely transformed into a sequence of
      --  corresponding declarations and statements. We leave it in the tree
      --  for documentation purposes. It will be ignored by the backend.

   end Expand_Pragma_Loop_Variant;

   --------------------------------
   -- Expand_Pragma_Psect_Object --
   --------------------------------

   --  Convert to Common_Object, and expand the resulting pragma

   procedure Expand_Pragma_Psect_Object (N : Node_Id)
     renames Expand_Pragma_Common_Object;

   -------------------------------------
   -- Expand_Pragma_Relative_Deadline --
   -------------------------------------

   procedure Expand_Pragma_Relative_Deadline (N : Node_Id) is
      P    : constant Node_Id    := Parent (N);
      Loc  : constant Source_Ptr := Sloc (N);

   begin
      --  Expand the pragma only in the case of the main subprogram. For tasks
      --  the expansion is done in exp_ch9. Generate a call to Set_Deadline
      --  at Clock plus the relative deadline specified in the pragma. Time
      --  values are translated into Duration to allow for non-private
      --  addition operation.

      if Nkind (P) = N_Subprogram_Body then
         Rewrite
           (N,
            Make_Procedure_Call_Statement (Loc,
              Name => New_Reference_To (RTE (RE_Set_Deadline), Loc),
              Parameter_Associations => New_List (
                Unchecked_Convert_To (RTE (RO_RT_Time),
                  Make_Op_Add (Loc,
                    Left_Opnd  =>
                      Make_Function_Call (Loc,
                        New_Reference_To (RTE (RO_RT_To_Duration), Loc),
                        New_List (Make_Function_Call (Loc,
                          New_Reference_To (RTE (RE_Clock), Loc)))),
                    Right_Opnd  =>
                      Unchecked_Convert_To (Standard_Duration, Arg1 (N)))))));

         Analyze (N);
      end if;
   end Expand_Pragma_Relative_Deadline;

end Exp_Prag;
