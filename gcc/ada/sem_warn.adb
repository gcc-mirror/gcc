------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             S E M _ W A R N                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1999-2010, Free Software Foundation, Inc.         --
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
with Debug;    use Debug;
with Einfo;    use Einfo;
with Errout;   use Errout;
with Exp_Code; use Exp_Code;
with Fname;    use Fname;
with Lib;      use Lib;
with Namet;    use Namet;
with Nlists;   use Nlists;
with Opt;      use Opt;
with Par_SCO;  use Par_SCO;
with Rtsfind;  use Rtsfind;
with Sem;      use Sem;
with Sem_Ch8;  use Sem_Ch8;
with Sem_Aux;  use Sem_Aux;
with Sem_Eval; use Sem_Eval;
with Sem_Util; use Sem_Util;
with Sinfo;    use Sinfo;
with Sinput;   use Sinput;
with Snames;   use Snames;
with Stand;    use Stand;
with Stringt;  use Stringt;
with Uintp;    use Uintp;

package body Sem_Warn is

   --  The following table collects Id's of entities that are potentially
   --  unreferenced. See Check_Unset_Reference for further details.
   --  ??? Check_Unset_Reference has zero information about this table.

   package Unreferenced_Entities is new Table.Table (
     Table_Component_Type => Entity_Id,
     Table_Index_Type     => Nat,
     Table_Low_Bound      => 1,
     Table_Initial        => Alloc.Unreferenced_Entities_Initial,
     Table_Increment      => Alloc.Unreferenced_Entities_Increment,
     Table_Name           => "Unreferenced_Entities");

   --  The following table collects potential warnings for IN OUT parameters
   --  that are referenced but not modified. These warnings are processed when
   --  the front end calls the procedure Output_Non_Modified_In_Out_Warnings.
   --  The reason that we defer output of these messages is that we want to
   --  detect the case where the relevant procedure is used as a generic actual
   --  in an instantiation, since we suppress the warnings in this case. The
   --  flag Used_As_Generic_Actual will be set in this case, but only at the
   --  point of usage. Similarly, we suppress the message if the address of the
   --  procedure is taken, where the flag Address_Taken may be set later.

   package In_Out_Warnings is new Table.Table (
     Table_Component_Type => Entity_Id,
     Table_Index_Type     => Nat,
     Table_Low_Bound      => 1,
     Table_Initial        => Alloc.In_Out_Warnings_Initial,
     Table_Increment      => Alloc.In_Out_Warnings_Increment,
     Table_Name           => "In_Out_Warnings");

   --------------------------------------------------------
   -- Handling of Warnings Off, Unmodified, Unreferenced --
   --------------------------------------------------------

   --  The functions Has_Warnings_Off, Has_Unmodified, Has_Unreferenced must
   --  generally be used instead of Warnings_Off, Has_Pragma_Unmodified and
   --  Has_Pragma_Unreferenced, as noted in the specs in Einfo.

   --  In order to avoid losing warnings in -gnatw.w (warn on unnecessary
   --  warnings off pragma) mode, i.e. to avoid false negatives, the code
   --  must follow some important rules.

   --  Call these functions as late as possible, after completing all other
   --  tests, just before the warnings is given. For example, don't write:

   --     if not Has_Warnings_Off (E)
   --       and then some-other-predicate-on-E then ..

   --  Instead the following is preferred

   --     if some-other-predicate-on-E
   --       and then Has_Warnings_Off (E)

   --  This way if some-other-predicate is false, we avoid a false indication
   --  that a Warnings (Off,E) pragma was useful in preventing a warning.

   --  The second rule is that if both Has_Unmodified and Has_Warnings_Off, or
   --  Has_Unreferenced and Has_Warnings_Off are called, make sure that the
   --  call to Has_Unmodified/Has_Unreferenced comes first, this way we record
   --  that the Warnings (Off) could have been Unreferenced or Unmodified. In
   --  fact Has_Unmodified/Has_Unreferenced includes a test for Warnings Off,
   --  and so a subsequent test is not needed anyway (though it is harmless).

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Generic_Package_Spec_Entity (E : Entity_Id) return Boolean;
   --  This returns true if the entity E is declared within a generic package.
   --  The point of this is to detect variables which are not assigned within
   --  the generic, but might be assigned outside the package for any given
   --  instance. These are cases where we leave the warnings to be posted for
   --  the instance, when we will know more.

   function Goto_Spec_Entity (E : Entity_Id) return Entity_Id;
   --  If E is a parameter entity for a subprogram body, then this function
   --  returns the corresponding spec entity, if not, E is returned unchanged.

   function Has_Pragma_Unmodified_Check_Spec (E : Entity_Id) return Boolean;
   --  Tests Has_Pragma_Unmodified flag for entity E. If E is not a formal,
   --  this is simply the setting of the flag Has_Pragma_Unmodified. If E is
   --  a body formal, the setting of the flag in the corresponding spec is
   --  also checked (and True returned if either flag is True).

   function Has_Pragma_Unreferenced_Check_Spec (E : Entity_Id) return Boolean;
   --  Tests Has_Pragma_Unreferenced flag for entity E. If E is not a formal,
   --  this is simply the setting of the flag Has_Pragma_Unreferenced. If E is
   --  a body formal, the setting of the flag in the corresponding spec is
   --  also checked (and True returned if either flag is True).

   function Never_Set_In_Source_Check_Spec (E : Entity_Id) return Boolean;
   --  Tests Never_Set_In_Source status for entity E. If E is not a formal,
   --  this is simply the setting of the flag Never_Set_In_Source. If E is
   --  a body formal, the setting of the flag in the corresponding spec is
   --  also checked (and False returned if either flag is False).

   function Operand_Has_Warnings_Suppressed (N : Node_Id) return Boolean;
   --  This function traverses the expression tree represented by the node N
   --  and determines if any sub-operand is a reference to an entity for which
   --  the Warnings_Off flag is set. True is returned if such an entity is
   --  encountered, and False otherwise.

   function Referenced_Check_Spec (E : Entity_Id) return Boolean;
   --  Tests Referenced status for entity E. If E is not a formal, this is
   --  simply the setting of the flag Referenced. If E is a body formal, the
   --  setting of the flag in the corresponding spec is also checked (and True
   --  returned if either flag is True).

   function Referenced_As_LHS_Check_Spec (E : Entity_Id) return Boolean;
   --  Tests Referenced_As_LHS status for entity E. If E is not a formal, this
   --  is simply the setting of the flag Referenced_As_LHS. If E is a body
   --  formal, the setting of the flag in the corresponding spec is also
   --  checked (and True returned if either flag is True).

   function Referenced_As_Out_Parameter_Check_Spec
     (E : Entity_Id) return Boolean;
   --  Tests Referenced_As_Out_Parameter status for entity E. If E is not a
   --  formal, this is simply the setting of Referenced_As_Out_Parameter. If E
   --  is a body formal, the setting of the flag in the corresponding spec is
   --  also checked (and True returned if either flag is True).

   procedure Warn_On_Unreferenced_Entity
     (Spec_E : Entity_Id;
      Body_E : Entity_Id := Empty);
   --  Output warnings for unreferenced entity E. For the case of an entry
   --  formal, Body_E is the corresponding body entity for a particular
   --  accept statement, and the message is posted on Body_E. In all other
   --  cases, Body_E is ignored and must be Empty.

   function Warnings_Off_Check_Spec (E : Entity_Id) return Boolean;
   --  Returns True if Warnings_Off is set for the entity E or (in the case
   --  where there is a Spec_Entity), Warnings_Off is set for the Spec_Entity.

   --------------------------
   -- Check_Code_Statement --
   --------------------------

   procedure Check_Code_Statement (N : Node_Id) is
   begin
      --  If volatile, nothing to worry about

      if Is_Asm_Volatile (N) then
         return;
      end if;

      --  Warn if no input or no output

      Setup_Asm_Inputs (N);

      if No (Asm_Input_Value) then
         Error_Msg_F
           ("?code statement with no inputs should usually be Volatile!", N);
         return;
      end if;

      Setup_Asm_Outputs (N);

      if No (Asm_Output_Variable) then
         Error_Msg_F
           ("?code statement with no outputs should usually be Volatile!", N);
         return;
      end if;

      --  Check multiple code statements in a row

      if Is_List_Member (N)
        and then Present (Prev (N))
        and then Nkind (Prev (N)) = N_Code_Statement
      then
         Error_Msg_F
           ("?code statements in sequence should usually be Volatile!", N);
         Error_Msg_F
           ("\?(suggest using template with multiple instructions)!", N);
      end if;
   end Check_Code_Statement;

   ---------------------------------
   -- Check_Infinite_Loop_Warning --
   ---------------------------------

   --  The case we look for is a while loop which tests a local variable, where
   --  there is no obvious direct or possible indirect update of the variable
   --  within the body of the loop.

   procedure Check_Infinite_Loop_Warning (Loop_Statement : Node_Id) is
      Expression : Node_Id := Empty;
      --  Set to WHILE or EXIT WHEN condition to be tested

      Ref : Node_Id := Empty;
      --  Reference in Expression to variable that might not be modified
      --  in loop, indicating a possible infinite loop.

      Var : Entity_Id := Empty;
      --  Corresponding entity (entity of Ref)

      Function_Call_Found : Boolean := False;
      --  True if Find_Var found a function call in the condition

      procedure Find_Var (N : Node_Id);
      --  Inspect condition to see if it depends on a single entity reference.
      --  If so, Ref is set to point to the reference node, and Var is set to
      --  the referenced Entity.

      function Has_Indirection (T : Entity_Id) return Boolean;
      --  If the controlling variable is an access type, or is a record type
      --  with access components, assume that it is changed indirectly and
      --  suppress the warning. As a concession to low-level programming, in
      --  particular within Declib, we also suppress warnings on a record
      --  type that contains components of type Address or Short_Address.

      function Is_Suspicious_Function_Name (E : Entity_Id) return Boolean;
      --  Given an entity name, see if the name appears to have something to
      --  do with I/O or network stuff, and if so, return True. Used to kill
      --  some false positives on a heuristic basis that such functions will
      --  likely have some strange side effect dependencies. A rather funny
      --  kludge, but warning messages are in the heuristics business.

      function Test_Ref (N : Node_Id) return Traverse_Result;
      --  Test for reference to variable in question. Returns Abandon if
      --  matching reference found. Used in instantiation of No_Ref_Found.

      function No_Ref_Found is new Traverse_Func (Test_Ref);
      --  Function to traverse body of procedure. Returns Abandon if matching
      --  reference found.

      --------------
      -- Find_Var --
      --------------

      procedure Find_Var (N : Node_Id) is
      begin
         --  Condition is a direct variable reference

         if Is_Entity_Name (N) then
            Ref := N;
            Var := Entity (Ref);

         --  Case of condition is a comparison with compile time known value

         elsif Nkind (N) in N_Op_Compare then
            if Compile_Time_Known_Value (Right_Opnd (N)) then
               Find_Var (Left_Opnd (N));

            elsif Compile_Time_Known_Value (Left_Opnd (N)) then
               Find_Var (Right_Opnd (N));

            --  Ignore any other comparison

            else
               return;
            end if;

         --  If condition is a negation, check its operand

         elsif Nkind (N) = N_Op_Not then
            Find_Var (Right_Opnd (N));

         --  Case of condition is function call

         elsif Nkind (N) = N_Function_Call then

            Function_Call_Found := True;

            --  Forget it if function name is not entity, who knows what
            --  we might be calling?

            if not Is_Entity_Name (Name (N)) then
               return;

            --  Forget it if function name is suspicious. A strange test
            --  but warning generation is in the heuristics business!

            elsif Is_Suspicious_Function_Name (Entity (Name (N))) then
               return;

            --  Forget it if warnings are suppressed on function entity

            elsif Has_Warnings_Off (Entity (Name (N))) then
               return;
            end if;

            --  OK, see if we have one argument

            declare
               PA : constant List_Id := Parameter_Associations (N);

            begin
               --  One argument, so check the argument

               if Present (PA)
                 and then List_Length (PA) = 1
               then
                  if Nkind (First (PA)) = N_Parameter_Association then
                     Find_Var (Explicit_Actual_Parameter (First (PA)));
                  else
                     Find_Var (First (PA));
                  end if;

               --  Not one argument

               else
                  return;
               end if;
            end;

         --  Any other kind of node is not something we warn for

         else
            return;
         end if;
      end Find_Var;

      ---------------------
      -- Has_Indirection --
      ---------------------

      function Has_Indirection (T : Entity_Id) return Boolean is
         Comp : Entity_Id;
         Rec  : Entity_Id;

      begin
         if Is_Access_Type (T) then
            return True;

         elsif Is_Private_Type (T)
           and then Present (Full_View (T))
           and then Is_Access_Type (Full_View (T))
         then
            return True;

         elsif Is_Record_Type (T) then
            Rec := T;

         elsif Is_Private_Type (T)
           and then Present (Full_View (T))
           and then Is_Record_Type (Full_View (T))
         then
            Rec := Full_View (T);
         else
            return False;
         end if;

         Comp := First_Component (Rec);
         while Present (Comp) loop
            if Is_Access_Type (Etype (Comp))
              or else Is_Descendent_Of_Address (Etype (Comp))
            then
               return True;
            end if;

            Next_Component (Comp);
         end loop;

         return False;
      end Has_Indirection;

      ---------------------------------
      -- Is_Suspicious_Function_Name --
      ---------------------------------

      function Is_Suspicious_Function_Name (E : Entity_Id) return Boolean is
         S : Entity_Id;

         function Substring_Present (S : String) return Boolean;
         --  Returns True if name buffer has given string delimited by non-
         --  alphabetic characters or by end of string. S is lower case.

         -----------------------
         -- Substring_Present --
         -----------------------

         function Substring_Present (S : String) return Boolean is
            Len : constant Natural := S'Length;

         begin
            for J in 1 .. Name_Len - (Len - 1) loop
               if Name_Buffer (J .. J + (Len - 1)) = S
                 and then
                   (J = 1
                     or else Name_Buffer (J - 1) not in 'a' .. 'z')
                 and then
                   (J + Len > Name_Len
                     or else Name_Buffer (J + Len) not in 'a' .. 'z')
               then
                  return True;
               end if;
            end loop;

            return False;
         end Substring_Present;

      --  Start of processing for Is_Suspicious_Function_Name

      begin
         S := E;
         while Present (S) and then S /= Standard_Standard loop
            Get_Name_String (Chars (S));

            if Substring_Present ("io")
              or else Substring_Present ("file")
              or else Substring_Present ("network")
            then
               return True;
            else
               S := Scope (S);
            end if;
         end loop;

         return False;
      end Is_Suspicious_Function_Name;

      --------------
      -- Test_Ref --
      --------------

      function Test_Ref (N : Node_Id) return Traverse_Result is
      begin
         --  Waste of time to look at the expression we are testing

         if N = Expression then
            return Skip;

         --  Direct reference to variable in question

         elsif Is_Entity_Name (N)
           and then Present (Entity (N))
           and then Entity (N) = Var
         then
            --  If this is an lvalue, then definitely abandon, since
            --  this could be a direct modification of the variable.

            if May_Be_Lvalue (N) then
               return Abandon;
            end if;

            --  If we appear in the context of a procedure call, then also
            --  abandon, since there may be issues of non-visible side
            --  effects going on in the call.

            declare
               P : Node_Id;

            begin
               P := N;
               loop
                  P := Parent (P);
                  exit when P = Loop_Statement;

                  --  Abandon if at procedure call, or something strange is
                  --  going on (perhaps a node with no parent that should
                  --  have one but does not?) As always, for a warning we
                  --  prefer to just abandon the warning than get into the
                  --  business of complaining about the tree structure here!

                  if No (P) or else Nkind (P) = N_Procedure_Call_Statement then
                     return Abandon;
                  end if;
               end loop;
            end;

            --  Reference to variable renaming variable in question

         elsif Is_Entity_Name (N)
           and then Present (Entity (N))
           and then Ekind (Entity (N)) = E_Variable
           and then Present (Renamed_Object (Entity (N)))
           and then Is_Entity_Name (Renamed_Object (Entity (N)))
           and then Entity (Renamed_Object (Entity (N))) = Var
           and then May_Be_Lvalue (N)
         then
            return Abandon;

            --  Call to subprogram

         elsif Nkind (N) = N_Procedure_Call_Statement
           or else Nkind (N) = N_Function_Call
         then
            --  If subprogram is within the scope of the entity we are dealing
            --  with as the loop variable, then it could modify this parameter,
            --  so we abandon in this case. In the case of a subprogram that is
            --  not an entity we also abandon. The check for no entity being
            --  present is a defense against previous errors.

            if not Is_Entity_Name (Name (N))
              or else No (Entity (Name (N)))
              or else Scope_Within (Entity (Name (N)), Scope (Var))
            then
               return Abandon;
            end if;

            --  If any of the arguments are of type access to subprogram, then
            --  we may have funny side effects, so no warning in this case.

            declare
               Actual : Node_Id;
            begin
               Actual := First_Actual (N);
               while Present (Actual) loop
                  if Is_Access_Subprogram_Type (Etype (Actual)) then
                     return Abandon;
                  else
                     Next_Actual (Actual);
                  end if;
               end loop;
            end;

         --  Declaration of the variable in question

         elsif Nkind (N) = N_Object_Declaration
           and then Defining_Identifier (N) = Var
         then
            return Abandon;
         end if;

         --  All OK, continue scan

         return OK;
      end Test_Ref;

   --  Start of processing for Check_Infinite_Loop_Warning

   begin
      --  Skip processing if debug flag gnatd.w is set

      if Debug_Flag_Dot_W then
         return;
      end if;

      --  Deal with Iteration scheme present

      declare
         Iter : constant Node_Id := Iteration_Scheme (Loop_Statement);

      begin
         if Present (Iter) then

            --  While iteration

            if Present (Condition (Iter)) then

               --  Skip processing for while iteration with conditions actions,
               --  since they make it too complicated to get the warning right.

               if Present (Condition_Actions (Iter)) then
                  return;
               end if;

               --  Capture WHILE condition

               Expression := Condition (Iter);

            --  For iteration, do not process, since loop will always terminate

            elsif Present (Loop_Parameter_Specification (Iter)) then
               return;
            end if;
         end if;
      end;

      --  Check chain of EXIT statements, we only process loops that have a
      --  single exit condition (either a single EXIT WHEN statement, or a
      --  WHILE loop not containing any EXIT WHEN statements).

      declare
         Ident     : constant Node_Id := Identifier (Loop_Statement);
         Exit_Stmt : Node_Id;

      begin
         --  If we don't have a proper chain set, ignore call entirely. This
         --  happens because of previous errors.

         if No (Entity (Ident))
           or else Ekind (Entity (Ident)) /= E_Loop
         then
            return;
         end if;

         --  Otherwise prepare to scan list of EXIT statements

         Exit_Stmt := First_Exit_Statement (Entity (Ident));
         while Present (Exit_Stmt) loop

            --  Check for EXIT WHEN

            if Present (Condition (Exit_Stmt)) then

               --  Quit processing if EXIT WHEN in WHILE loop, or more than
               --  one EXIT WHEN statement present in the loop.

               if Present (Expression) then
                  return;

               --  Otherwise capture condition from EXIT WHEN statement

               else
                  Expression := Condition (Exit_Stmt);
               end if;
            end if;

            Exit_Stmt := Next_Exit_Statement (Exit_Stmt);
         end loop;
      end;

      --  Return if no condition to test

      if No (Expression) then
         return;
      end if;

      --  Initial conditions met, see if condition is of right form

      Find_Var (Expression);

      --  Nothing to do if local variable from source not found. If it's a
      --  renaming, it is probably renaming something too complicated to deal
      --  with here.

      if No (Var)
        or else Ekind (Var) /= E_Variable
        or else Is_Library_Level_Entity (Var)
        or else not Comes_From_Source (Var)
        or else Nkind (Parent (Var)) = N_Object_Renaming_Declaration
      then
         return;

      --  Nothing to do if there is some indirection involved (assume that the
      --  designated variable might be modified in some way we don't see).
      --  However, if no function call was found, then we don't care about
      --  indirections, because the condition must be something like "while X
      --  /= null loop", so we don't care if X.all is modified in the loop.

      elsif Function_Call_Found and then Has_Indirection (Etype (Var)) then
         return;

      --  Same sort of thing for volatile variable, might be modified by
      --  some other task or by the operating system in some way.

      elsif Is_Volatile (Var) then
         return;
      end if;

      --  Filter out case of original statement sequence starting with delay.
      --  We assume this is a multi-tasking program and that the condition
      --  is affected by other threads (some kind of busy wait).

      declare
         Fstm : constant Node_Id :=
                  Original_Node (First (Statements (Loop_Statement)));
      begin
         if Nkind (Fstm) = N_Delay_Relative_Statement
           or else Nkind (Fstm) = N_Delay_Until_Statement
         then
            return;
         end if;
      end;

      --  We have a variable reference of the right form, now we scan the loop
      --  body to see if it looks like it might not be modified

      if No_Ref_Found (Loop_Statement) = OK then
         Error_Msg_NE
           ("?variable& is not modified in loop body!", Ref, Var);
         Error_Msg_N
           ("\?possible infinite loop!", Ref);
      end if;
   end Check_Infinite_Loop_Warning;

   ----------------------------
   -- Check_Low_Bound_Tested --
   ----------------------------

   procedure Check_Low_Bound_Tested (Expr : Node_Id) is
   begin
      if Comes_From_Source (Expr) then
         declare
            L : constant Node_Id := Left_Opnd (Expr);
            R : constant Node_Id := Right_Opnd (Expr);
         begin
            if Nkind (L) = N_Attribute_Reference
              and then Attribute_Name (L) = Name_First
              and then Is_Entity_Name (Prefix (L))
              and then Is_Formal (Entity (Prefix (L)))
            then
               Set_Low_Bound_Tested (Entity (Prefix (L)));
            end if;

            if Nkind (R) = N_Attribute_Reference
              and then Attribute_Name (R) = Name_First
              and then Is_Entity_Name (Prefix (R))
              and then Is_Formal (Entity (Prefix (R)))
            then
               Set_Low_Bound_Tested (Entity (Prefix (R)));
            end if;
         end;
      end if;
   end Check_Low_Bound_Tested;

   ----------------------
   -- Check_References --
   ----------------------

   procedure Check_References (E : Entity_Id; Anod : Node_Id := Empty) is
      E1  : Entity_Id;
      E1T : Entity_Id;
      UR  : Node_Id;

      function Body_Formal
        (E                : Entity_Id;
         Accept_Statement : Node_Id) return Entity_Id;
      --  For an entry formal entity from an entry declaration, find the
      --  corresponding body formal from the given accept statement.

      function Missing_Subunits return Boolean;
      --  We suppress warnings when there are missing subunits, because this
      --  may generate too many false positives: entities in a parent may only
      --  be referenced in one of the subunits. We make an exception for
      --  subunits that contain no other stubs.

      procedure Output_Reference_Error (M : String);
      --  Used to output an error message. Deals with posting the error on the
      --  body formal in the accept case.

      function Publicly_Referenceable (Ent : Entity_Id) return Boolean;
      --  This is true if the entity in question is potentially referenceable
      --  from another unit. This is true for entities in packages that are at
      --  the library level.

      function Warnings_Off_E1 return Boolean;
      --  Return True if Warnings_Off is set for E1, or for its Etype (E1T),
      --  or for the base type of E1T.

      -----------------
      -- Body_Formal --
      -----------------

      function Body_Formal
        (E                : Entity_Id;
         Accept_Statement : Node_Id) return Entity_Id
      is
         Body_Param : Node_Id;
         Body_E     : Entity_Id;

      begin
         --  Loop to find matching parameter in accept statement

         Body_Param := First (Parameter_Specifications (Accept_Statement));
         while Present (Body_Param) loop
            Body_E := Defining_Identifier (Body_Param);

            if Chars (Body_E) = Chars (E) then
               return Body_E;
            end if;

            Next (Body_Param);
         end loop;

         --  Should never fall through, should always find a match

         raise Program_Error;
      end Body_Formal;

      ----------------------
      -- Missing_Subunits --
      ----------------------

      function Missing_Subunits return Boolean is
         D : Node_Id;

      begin
         if not Unloaded_Subunits then

            --  Normal compilation, all subunits are present

            return False;

         elsif E /= Main_Unit_Entity then

            --  No warnings on a stub that is not the main unit

            return True;

         elsif Nkind (Unit_Declaration_Node (E)) in N_Proper_Body then
            D := First (Declarations (Unit_Declaration_Node (E)));
            while Present (D) loop

               --  No warnings if the proper body contains nested stubs

               if Nkind (D) in N_Body_Stub then
                  return True;
               end if;

               Next (D);
            end loop;

            return False;

         else
            --  Missing stubs elsewhere

            return True;
         end if;
      end Missing_Subunits;

      ----------------------------
      -- Output_Reference_Error --
      ----------------------------

      procedure Output_Reference_Error (M : String) is
      begin
         --  Never issue messages for internal names, nor for renamings

         if Is_Internal_Name (Chars (E1))
           or else Nkind (Parent (E1)) = N_Object_Renaming_Declaration
         then
            return;
         end if;

         --  Don't output message for IN OUT formal unless we have the warning
         --  flag specifically set. It is a bit odd to distinguish IN OUT
         --  formals from other cases. This distinction is historical in
         --  nature. Warnings for IN OUT formals were added fairly late.

         if Ekind (E1) = E_In_Out_Parameter
           and then not Check_Unreferenced_Formals
         then
            return;
         end if;

         --  Other than accept case, post error on defining identifier

         if No (Anod) then
            Error_Msg_N (M, E1);

         --  Accept case, find body formal to post the message

         else
            Error_Msg_NE (M, Body_Formal (E1, Accept_Statement => Anod), E1);

         end if;
      end Output_Reference_Error;

      ----------------------------
      -- Publicly_Referenceable --
      ----------------------------

      function Publicly_Referenceable (Ent : Entity_Id) return Boolean is
         P    : Node_Id;
         Prev : Node_Id;

      begin
         --  A formal parameter is never referenceable outside the body of its
         --  subprogram or entry.

         if Is_Formal (Ent) then
            return False;
         end if;

         --  Examine parents to look for a library level package spec. But if
         --  we find a body or block or other similar construct along the way,
         --  we cannot be referenced.

         Prev := Ent;
         P    := Parent (Ent);
         loop
            case Nkind (P) is

               --  If we get to top of tree, then publicly referenceable

               when N_Empty =>
                  return True;

               --  If we reach a generic package declaration, then always
               --  consider this referenceable, since any instantiation will
               --  have access to the entities in the generic package. Note
               --  that the package itself may not be instantiated, but then
               --  we will get a warning for the package entity.

               --  Note that generic formal parameters are themselves not
               --  publicly referenceable in an instance, and warnings on them
               --  are useful.

               when N_Generic_Package_Declaration =>
                  return
                    not Is_List_Member (Prev)
                      or else List_Containing (Prev)
                        /= Generic_Formal_Declarations (P);

               --  Similarly, the generic formals of a generic subprogram are
               --  not accessible.

               when N_Generic_Subprogram_Declaration  =>
                  if Is_List_Member (Prev)
                    and then List_Containing (Prev) =
                               Generic_Formal_Declarations (P)
                  then
                     return False;
                  else
                     P := Parent (P);
                  end if;

               --  If we reach a subprogram body, entity is not referenceable
               --  unless it is the defining entity of the body. This will
               --  happen, e.g. when a function is an attribute renaming that
               --  is rewritten as a body.

               when N_Subprogram_Body  =>
                  if Ent /= Defining_Entity (P) then
                     return False;
                  else
                     P := Parent (P);
                  end if;

               --  If we reach any other body, definitely not referenceable

               when N_Package_Body    |
                    N_Task_Body       |
                    N_Entry_Body      |
                    N_Protected_Body  |
                    N_Block_Statement |
                    N_Subunit         =>
                  return False;

               --  For all other cases, keep looking up tree

               when others =>
                  Prev := P;
                  P    := Parent (P);
            end case;
         end loop;
      end Publicly_Referenceable;

      ---------------------
      -- Warnings_Off_E1 --
      ---------------------

      function Warnings_Off_E1 return Boolean is
      begin
         return Has_Warnings_Off (E1T)
           or else Has_Warnings_Off (Base_Type (E1T))
           or else Warnings_Off_Check_Spec (E1);
      end Warnings_Off_E1;

   --  Start of processing for Check_References

   begin
      --  No messages if warnings are suppressed, or if we have detected any
      --  real errors so far (this last check avoids junk messages resulting
      --  from errors, e.g. a subunit that is not loaded).

      if Warning_Mode = Suppress
        or else Serious_Errors_Detected /= 0
      then
         return;
      end if;

      --  We also skip the messages if any subunits were not loaded (see
      --  comment in Sem_Ch10 to understand how this is set, and why it is
      --  necessary to suppress the warnings in this case).

      if Missing_Subunits then
         return;
      end if;

      --  Otherwise loop through entities, looking for suspicious stuff

      E1 := First_Entity (E);
      while Present (E1) loop
         E1T := Etype (E1);

         --  We are only interested in source entities. We also don't issue
         --  warnings within instances, since the proper place for such
         --  warnings is on the template when it is compiled.

         if Comes_From_Source (E1)
           and then Instantiation_Location (Sloc (E1)) = No_Location
         then
            --  We are interested in variables and out/in-out parameters, but
            --  we exclude protected types, too complicated to worry about.

            if Ekind (E1) = E_Variable
              or else
                (Ekind_In (E1, E_Out_Parameter, E_In_Out_Parameter)
                  and then not Is_Protected_Type (Current_Scope))
            then
               --  Case of an unassigned variable

               --  First gather any Unset_Reference indication for E1. In the
               --  case of a parameter, it is the Spec_Entity that is relevant.

               if Ekind (E1) = E_Out_Parameter
                 and then Present (Spec_Entity (E1))
               then
                  UR := Unset_Reference (Spec_Entity (E1));
               else
                  UR := Unset_Reference (E1);
               end if;

               --  Special processing for access types

               if Present (UR)
                 and then Is_Access_Type (E1T)
               then
                  --  For access types, the only time we made a UR entry was
                  --  for a dereference, and so we post the appropriate warning
                  --  here (note that the dereference may not be explicit in
                  --  the source, for example in the case of a dispatching call
                  --  with an anonymous access controlling formal, or of an
                  --  assignment of a pointer involving discriminant check on
                  --  the designated object).

                  if not Warnings_Off_E1 then
                     Error_Msg_NE ("?& may be null!", UR, E1);
                  end if;

                  goto Continue;

               --  Case of variable that could be a constant. Note that we
               --  never signal such messages for generic package entities,
               --  since a given instance could have modifications outside
               --  the package.

               elsif Warn_On_Constant
                 and then (Ekind (E1) = E_Variable
                             and then Has_Initial_Value (E1))
                 and then Never_Set_In_Source_Check_Spec (E1)
                 and then not Address_Taken (E1)
                 and then not Generic_Package_Spec_Entity (E1)
               then
                  --  A special case, if this variable is volatile and not
                  --  imported, it is not helpful to tell the programmer
                  --  to mark the variable as constant, since this would be
                  --  illegal by virtue of RM C.6(13).

                  if (Is_Volatile (E1) or else Has_Volatile_Components (E1))
                    and then not Is_Imported (E1)
                  then
                     Error_Msg_N
                       ("?& is not modified, volatile has no effect!", E1);

                  --  Another special case, Exception_Occurrence, this catches
                  --  the case of exception choice (and a bit more too, but not
                  --  worth doing more investigation here).

                  elsif Is_RTE (E1T, RE_Exception_Occurrence) then
                     null;

                  --  Here we give the warning if referenced and no pragma
                  --  Unreferenced or Unmodified is present.

                  else
                     --  Variable case

                     if Ekind (E1) = E_Variable then
                        if Referenced_Check_Spec (E1)
                          and then not Has_Pragma_Unreferenced_Check_Spec (E1)
                          and then not Has_Pragma_Unmodified_Check_Spec (E1)
                        then
                           if not Warnings_Off_E1 then
                              Error_Msg_N -- CODEFIX
                                ("?& is not modified, "
                                 & "could be declared constant!",
                                 E1);
                           end if;
                        end if;
                     end if;
                  end if;

               --  Other cases of a variable or parameter never set in source

               elsif Never_Set_In_Source_Check_Spec (E1)

                  --  No warning if warning for this case turned off

                  and then Warn_On_No_Value_Assigned

                  --  No warning if address taken somewhere

                  and then not Address_Taken (E1)

                  --  No warning if explicit initial value

                  and then not Has_Initial_Value (E1)

                  --  No warning for generic package spec entities, since we
                  --  might set them in a child unit or something like that

                  and then not Generic_Package_Spec_Entity (E1)

                  --  No warning if fully initialized type, except that for
                  --  this purpose we do not consider access types to qualify
                  --  as fully initialized types (relying on an access type
                  --  variable being null when it is never set is a bit odd!)

                  --  Also we generate warning for an out parameter that is
                  --  never referenced, since again it seems odd to rely on
                  --  default initialization to set an out parameter value.

                 and then (Is_Access_Type (E1T)
                            or else Ekind (E1) = E_Out_Parameter
                            or else not Is_Fully_Initialized_Type (E1T))
               then
                  --  Do not output complaint about never being assigned a
                  --  value if a pragma Unmodified applies to the variable
                  --  we are examining, or if it is a parameter, if there is
                  --  a pragma Unreferenced for the corresponding spec, or
                  --  if the type is marked as having unreferenced objects.
                  --  The last is a little peculiar, but better too few than
                  --  too many warnings in this situation.

                  if Has_Pragma_Unreferenced_Objects (E1T)
                    or else Has_Pragma_Unmodified_Check_Spec (E1)
                  then
                     null;

                  --  IN OUT parameter case where parameter is referenced. We
                  --  separate this out, since this is the case where we delay
                  --  output of the warning until more information is available
                  --  (about use in an instantiation or address being taken).

                  elsif Ekind (E1) = E_In_Out_Parameter
                    and then Referenced_Check_Spec (E1)
                  then
                     --  Suppress warning if private type, and the procedure
                     --  has a separate declaration in a different unit. This
                     --  is the case where the client of a package sees only
                     --  the private type, and it may be quite reasonable
                     --  for the logical view to be IN OUT, even if the
                     --  implementation ends up using access types or some
                     --  other method to achieve the local effect of a
                     --  modification. On the other hand if the spec and body
                     --  are in the same unit, we are in the package body and
                     --  there we have less excuse for a junk IN OUT parameter.

                     if Has_Private_Declaration (E1T)
                       and then Present (Spec_Entity (E1))
                       and then not In_Same_Source_Unit (E1, Spec_Entity (E1))
                     then
                        null;

                     --  Suppress warning for any parameter of a dispatching
                     --  operation, since it is quite reasonable to have an
                     --  operation that is overridden, and for some subclasses
                     --  needs the formal to be IN OUT and for others happens
                     --  not to assign it.

                     elsif Is_Dispatching_Operation
                             (Scope (Goto_Spec_Entity (E1)))
                     then
                        null;

                     --  Suppress warning if composite type contains any access
                     --  component, since the logical effect of modifying a
                     --  parameter may be achieved by modifying a referenced
                     --  object.

                     elsif Is_Composite_Type (E1T)
                       and then Has_Access_Values (E1T)
                     then
                        null;

                     --  Suppress warning on formals of an entry body. All
                     --  references are attached to the formal in the entry
                     --  declaration, which are marked Is_Entry_Formal.

                     elsif Ekind (Scope (E1)) = E_Entry
                       and then not Is_Entry_Formal (E1)
                     then
                        null;

                     --  OK, looks like warning for an IN OUT parameter that
                     --  could be IN makes sense, but we delay the output of
                     --  the warning, pending possibly finding out later on
                     --  that the associated subprogram is used as a generic
                     --  actual, or its address/access is taken. In these two
                     --  cases, we suppress the warning because the context may
                     --  force use of IN OUT, even if in this particular case
                     --  the formal is not modified.

                     else
                        In_Out_Warnings.Append (E1);
                     end if;

                  --  Other cases of formals

                  elsif Is_Formal (E1) then
                     if not Is_Trivial_Subprogram (Scope (E1)) then
                        if Referenced_Check_Spec (E1) then
                           if not Has_Pragma_Unmodified_Check_Spec (E1)
                             and then not Warnings_Off_E1
                           then
                              Output_Reference_Error
                                ("?formal parameter& is read but "
                                 & "never assigned!");
                           end if;

                        elsif not Has_Pragma_Unreferenced_Check_Spec (E1)
                          and then not Warnings_Off_E1
                        then
                           Output_Reference_Error
                             ("?formal parameter& is not referenced!");
                        end if;
                     end if;

                  --  Case of variable

                  else
                     if Referenced (E1) then
                        if not Has_Unmodified (E1)
                          and then not Warnings_Off_E1
                        then
                           Output_Reference_Error
                             ("?variable& is read but never assigned!");
                        end if;

                     elsif not Has_Unreferenced (E1)
                       and then not Warnings_Off_E1
                     then
                        Output_Reference_Error -- CODEFIX
                          ("?variable& is never read and never assigned!");
                     end if;

                     --  Deal with special case where this variable is hidden
                     --  by a loop variable.

                     if Ekind (E1) = E_Variable
                       and then Present (Hiding_Loop_Variable (E1))
                       and then not Warnings_Off_E1
                     then
                        Error_Msg_N
                          ("?for loop implicitly declares loop variable!",
                           Hiding_Loop_Variable (E1));

                        Error_Msg_Sloc := Sloc (E1);
                        Error_Msg_N
                          ("\?declaration hides & declared#!",
                           Hiding_Loop_Variable (E1));
                     end if;
                  end if;

                  goto Continue;
               end if;

               --  Check for unset reference

               if Warn_On_No_Value_Assigned and then Present (UR) then

                  --  For other than access type, go back to original node to
                  --  deal with case where original unset reference has been
                  --  rewritten during expansion.

                  --  In some cases, the original node may be a type conversion
                  --  or qualification, and in this case we want the object
                  --  entity inside.

                  UR := Original_Node (UR);
                  while Nkind (UR) = N_Type_Conversion
                    or else Nkind (UR) = N_Qualified_Expression
                  loop
                     UR := Expression (UR);
                  end loop;

                  --  Here we issue the warning, all checks completed

                  --  If we have a return statement, this was a case of an OUT
                  --  parameter not being set at the time of the return. (Note:
                  --  it can't be N_Extended_Return_Statement, because those
                  --  are only for functions, and functions do not allow OUT
                  --  parameters.)

                  if not Is_Trivial_Subprogram (Scope (E1)) then
                     if Nkind (UR) = N_Simple_Return_Statement
                       and then not Has_Pragma_Unmodified_Check_Spec (E1)
                     then
                        if not Warnings_Off_E1 then
                           Error_Msg_NE
                             ("?OUT parameter& not set before return", UR, E1);
                        end if;

                        --  If the unset reference is a selected component
                        --  prefix from source, mention the component as well.
                        --  If the selected component comes from expansion, all
                        --  we know is that the entity is not fully initialized
                        --  at the point of the reference. Locate a random
                        --  uninitialized component to get a better message.

                     elsif Nkind (Parent (UR)) = N_Selected_Component then
                        Error_Msg_Node_2 := Selector_Name (Parent (UR));

                        if not Comes_From_Source (Parent (UR)) then
                           declare
                              Comp : Entity_Id;

                           begin
                              Comp := First_Entity (E1T);
                              while Present (Comp) loop
                                 if Ekind (Comp) = E_Component
                                   and then Nkind (Parent (Comp)) =
                                              N_Component_Declaration
                                   and then No (Expression (Parent (Comp)))
                                 then
                                    Error_Msg_Node_2 := Comp;
                                    exit;
                                 end if;

                                 Next_Entity (Comp);
                              end loop;
                           end;
                        end if;

                        --  Issue proper warning. This is a case of referencing
                        --  a variable before it has been explicitly assigned.
                        --  For access types, UR was only set for dereferences,
                        --  so the issue is that the value may be null.

                        if not Is_Trivial_Subprogram (Scope (E1)) then
                           if not Warnings_Off_E1 then
                              if Is_Access_Type (Etype (Parent (UR))) then
                                 Error_Msg_N ("?`&.&` may be null!", UR);
                              else
                                 Error_Msg_N
                                   ("?`&.&` may be referenced before "
                                    & "it has a value!", UR);
                              end if;
                           end if;
                        end if;

                        --  All other cases of unset reference active

                     elsif not Warnings_Off_E1 then
                        Error_Msg_N
                          ("?& may be referenced before it has a value!",
                           UR);
                     end if;
                  end if;

                  goto Continue;
               end if;
            end if;

            --  Then check for unreferenced entities. Note that we are only
            --  interested in entities whose Referenced flag is not set.

            if not Referenced_Check_Spec (E1)

               --  If Referenced_As_LHS is set, then that's still interesting
               --  (potential "assigned but never read" case), but not if we
               --  have pragma Unreferenced, which cancels this warning.

              and then (not Referenced_As_LHS_Check_Spec (E1)
                          or else not Has_Unreferenced (E1))

               --  Check that warnings on unreferenced entities are enabled

              and then
                ((Check_Unreferenced and then not Is_Formal (E1))

                     --  Case of warning on unreferenced formal

                     or else
                      (Check_Unreferenced_Formals and then Is_Formal (E1))

                     --  Case of warning on unread variables modified by an
                     --  assignment, or an OUT parameter if it is the only one.

                     or else
                       (Warn_On_Modified_Unread
                          and then Referenced_As_LHS_Check_Spec (E1))

                     --  Case of warning on any unread OUT parameter (note
                     --  such indications are only set if the appropriate
                     --  warning options were set, so no need to recheck here.

                     or else
                       Referenced_As_Out_Parameter_Check_Spec (E1))

               --  All other entities, including local packages that cannot be
               --  referenced from elsewhere, including those declared within a
               --  package body.

               and then (Is_Object (E1)
                           or else
                         Is_Type (E1)
                           or else
                         Ekind (E1) = E_Label
                           or else
                         Ekind (E1) = E_Exception
                           or else
                         Ekind (E1) = E_Named_Integer
                           or else
                         Ekind (E1) = E_Named_Real
                           or else
                         Is_Overloadable (E1)

                           --  Package case, if the main unit is a package spec
                           --  or generic package spec, then there may be a
                           --  corresponding body that references this package
                           --  in some other file. Otherwise we can be sure
                           --  that there is no other reference.

                           or else
                             (Ekind (E1) = E_Package
                                and then
                                  not Is_Package_Or_Generic_Package
                                        (Cunit_Entity (Current_Sem_Unit))))

               --  Exclude instantiations, since there is no reason why every
               --  entity in an instantiation should be referenced.

               and then Instantiation_Location (Sloc (E1)) = No_Location

               --  Exclude formal parameters from bodies if the corresponding
               --  spec entity has been referenced in the case where there is
               --  a separate spec.

               and then not (Is_Formal (E1)
                              and then Ekind (Scope (E1)) = E_Subprogram_Body
                              and then Present (Spec_Entity (E1))
                              and then Referenced (Spec_Entity (E1)))

               --  Consider private type referenced if full view is referenced.
               --  If there is not full view, this is a generic type on which
               --  warnings are also useful.

               and then
                 not (Is_Private_Type (E1)
                       and then Present (Full_View (E1))
                       and then Referenced (Full_View (E1)))

               --  Don't worry about full view, only about private type

               and then not Has_Private_Declaration (E1)

               --  Eliminate dispatching operations from consideration, we
               --  cannot tell if these are referenced or not in any easy
               --  manner (note this also catches Adjust/Finalize/Initialize).

               and then not Is_Dispatching_Operation (E1)

               --  Check entity that can be publicly referenced (we do not give
               --  messages for such entities, since there could be other
               --  units, not involved in this compilation, that contain
               --  relevant references.

               and then not Publicly_Referenceable (E1)

               --  Class wide types are marked as source entities, but they are
               --  not really source entities, and are always created, so we do
               --  not care if they are not referenced.

               and then Ekind (E1) /= E_Class_Wide_Type

               --  Objects other than parameters of task types are allowed to
               --  be non-referenced, since they start up tasks!

               and then ((Ekind (E1) /= E_Variable
                           and then Ekind (E1) /= E_Constant
                           and then Ekind (E1) /= E_Component)
                          or else not Is_Task_Type (E1T))

               --  For subunits, only place warnings on the main unit itself,
               --  since parent units are not completely compiled.

               and then (Nkind (Unit (Cunit (Main_Unit))) /= N_Subunit
                          or else Get_Source_Unit (E1) = Main_Unit)

               --  No warning on a return object, because these are often
               --  created with a single expression and an implicit return.
               --  If the object is a variable there will be a warning
               --  indicating that it could be declared constant.

               and then not
                 (Ekind (E1) = E_Constant and then Is_Return_Object (E1))
            then
               --  Suppress warnings in internal units if not in -gnatg mode
               --  (these would be junk warnings for an applications program,
               --  since they refer to problems in internal units).

               if GNAT_Mode
                 or else not Is_Internal_File_Name
                               (Unit_File_Name (Get_Source_Unit (E1)))
               then
                  --  We do not immediately flag the error. This is because we
                  --  have not expanded generic bodies yet, and they may have
                  --  the missing reference. So instead we park the entity on a
                  --  list, for later processing. However for the case of an
                  --  accept statement we want to output messages now, since
                  --  we know we already have all information at hand, and we
                  --  also want to have separate warnings for each accept
                  --  statement for the same entry.

                  if Present (Anod) then
                     pragma Assert (Is_Formal (E1));

                     --  The unreferenced entity is E1, but post the warning
                     --  on the body entity for this accept statement.

                     if not Warnings_Off_E1 then
                        Warn_On_Unreferenced_Entity
                          (E1, Body_Formal (E1, Accept_Statement => Anod));
                     end if;

                  elsif not Warnings_Off_E1 then
                     Unreferenced_Entities.Append (E1);
                  end if;
               end if;

            --  Generic units are referenced in the generic body, but if they
            --  are not public and never instantiated we want to force a
            --  warning on them. We treat them as redundant constructs to
            --  minimize noise.

            elsif Is_Generic_Subprogram (E1)
              and then not Is_Instantiated (E1)
              and then not Publicly_Referenceable (E1)
              and then Instantiation_Depth (Sloc (E1)) = 0
              and then Warn_On_Redundant_Constructs
            then
               if not Warnings_Off_E1 then
                  Unreferenced_Entities.Append (E1);

                  --  Force warning on entity

                  Set_Referenced (E1, False);
               end if;
            end if;
         end if;

         --  Recurse into nested package or block. Do not recurse into a formal
         --  package, because the corresponding body is not analyzed.

         <<Continue>>
            if (Is_Package_Or_Generic_Package (E1)
                  and then Nkind (Parent (E1)) = N_Package_Specification
                  and then
                    Nkind (Original_Node (Unit_Declaration_Node (E1)))
                      /= N_Formal_Package_Declaration)

              or else Ekind (E1) = E_Block
            then
               Check_References (E1);
            end if;

            Next_Entity (E1);
      end loop;
   end Check_References;

   ---------------------------
   -- Check_Unset_Reference --
   ---------------------------

   procedure Check_Unset_Reference (N : Node_Id) is
      Typ : constant Entity_Id := Etype (N);

      function Is_OK_Fully_Initialized return Boolean;
      --  This function returns true if the given node N is fully initialized
      --  so that the reference is safe as far as this routine is concerned.
      --  Safe generally means that the type of N is a fully initialized type.
      --  The one special case is that for access types, which are always fully
      --  initialized, we don't consider a dereference OK since it will surely
      --  be dereferencing a null value, which won't do.

      function Prefix_Has_Dereference (Pref : Node_Id) return Boolean;
      --  Used to test indexed or selected component or slice to see if the
      --  evaluation of the prefix depends on a dereference, and if so, returns
      --  True, in which case we always check the prefix, even if we know that
      --  the referenced component is initialized. Pref is the prefix to test.

      -----------------------------
      -- Is_OK_Fully_Initialized --
      -----------------------------

      function Is_OK_Fully_Initialized return Boolean is
      begin
         if Is_Access_Type (Typ) and then Is_Dereferenced (N) then
            return False;
         else
            return Is_Fully_Initialized_Type (Typ);
         end if;
      end Is_OK_Fully_Initialized;

      ----------------------------
      -- Prefix_Has_Dereference --
      ----------------------------

      function Prefix_Has_Dereference (Pref : Node_Id) return Boolean is
      begin
         --  If prefix is of an access type, it certainly needs a dereference

         if Is_Access_Type (Etype (Pref)) then
            return True;

         --  If prefix is explicit dereference, that's a dereference for sure

         elsif Nkind (Pref) = N_Explicit_Dereference then
            return True;

            --  If prefix is itself a component reference or slice check prefix

         elsif Nkind (Pref) = N_Slice
           or else Nkind (Pref) = N_Indexed_Component
           or else Nkind (Pref) = N_Selected_Component
         then
            return Prefix_Has_Dereference (Prefix (Pref));

         --  All other cases do not involve a dereference

         else
            return False;
         end if;
      end Prefix_Has_Dereference;

   --  Start of processing for Check_Unset_Reference

   begin
      --  Nothing to do if warnings suppressed

      if Warning_Mode = Suppress then
         return;
      end if;

      --  Ignore reference unless it comes from source. Almost always if we
      --  have a reference from generated code, it is bogus (e.g. calls to init
      --  procs to set default discriminant values).

      if not Comes_From_Source (N) then
         return;
      end if;

      --  Otherwise see what kind of node we have. If the entity already has an
      --  unset reference, it is not necessarily the earliest in the text,
      --  because resolution of the prefix of selected components is completed
      --  before the resolution of the selected component itself. As a result,
      --  given (R /= null and then R.X > 0), the occurrences of R are examined
      --  in right-to-left order. If there is already an unset reference, we
      --  check whether N is earlier before proceeding.

      case Nkind (N) is

         --  For identifier or expanded name, examine the entity involved

         when N_Identifier | N_Expanded_Name =>
            declare
               E : constant Entity_Id := Entity (N);

            begin
               if (Ekind (E) = E_Variable
                     or else
                   Ekind (E) = E_Out_Parameter)
                 and then Never_Set_In_Source_Check_Spec (E)
                 and then not Has_Initial_Value (E)
                 and then (No (Unset_Reference (E))
                            or else
                              Earlier_In_Extended_Unit
                                (Sloc (N),  Sloc (Unset_Reference (E))))
                 and then not Has_Pragma_Unmodified_Check_Spec (E)
                 and then not Warnings_Off_Check_Spec (E)
               then
                  --  We may have an unset reference. The first test is whether
                  --  this is an access to a discriminant of a record or a
                  --  component with default initialization. Both of these
                  --  cases can be ignored, since the actual object that is
                  --  referenced is definitely initialized. Note that this
                  --  covers the case of reading discriminants of an OUT
                  --  parameter, which is OK even in Ada 83.

                  --  Note that we are only interested in a direct reference to
                  --  a record component here. If the reference is through an
                  --  access type, then the access object is being referenced,
                  --  not the record, and still deserves an unset reference.

                  if Nkind (Parent (N)) = N_Selected_Component
                    and not Is_Access_Type (Typ)
                  then
                     declare
                        ES : constant Entity_Id :=
                               Entity (Selector_Name (Parent (N)));
                     begin
                        if Ekind (ES) = E_Discriminant
                          or else
                            (Present (Declaration_Node (ES))
                               and then
                             Present (Expression (Declaration_Node (ES))))
                        then
                           return;
                        end if;
                     end;
                  end if;

                  --  Exclude fully initialized types

                  if Is_OK_Fully_Initialized then
                     return;
                  end if;

                  --  Here we have a potential unset reference. But before we
                  --  get worried about it, we have to make sure that the
                  --  entity declaration is in the same procedure as the
                  --  reference, since if they are in separate procedures, then
                  --  we have no idea about sequential execution.

                  --  The tests in the loop below catch all such cases, but do
                  --  allow the reference to appear in a loop, block, or
                  --  package spec that is nested within the declaring scope.
                  --  As always, it is possible to construct cases where the
                  --  warning is wrong, that is why it is a warning!

                  Potential_Unset_Reference : declare
                     SR : Entity_Id;
                     SE : constant Entity_Id := Scope (E);

                     function Within_Postcondition return Boolean;
                     --  Returns True iff N is within a Precondition

                     --------------------------
                     -- Within_Postcondition --
                     --------------------------

                     function Within_Postcondition return Boolean is
                        Nod : Node_Id;

                     begin
                        Nod := Parent (N);
                        while Present (Nod) loop
                           if Nkind (Nod) = N_Pragma
                             and then Pragma_Name (Nod) = Name_Postcondition
                           then
                              return True;
                           end if;

                           Nod := Parent (Nod);
                        end loop;

                        return False;
                     end Within_Postcondition;

                  --  Start of processing for Potential_Unset_Reference

                  begin
                     SR := Current_Scope;
                     while SR /= SE loop
                        if SR = Standard_Standard
                          or else Is_Subprogram (SR)
                          or else Is_Concurrent_Body (SR)
                          or else Is_Concurrent_Type (SR)
                        then
                           return;
                        end if;

                        SR := Scope (SR);
                     end loop;

                     --  Case of reference has an access type. This is a
                     --  special case since access types are always set to null
                     --  so cannot be truly uninitialized, but we still want to
                     --  warn about cases of obvious null dereference.

                     if Is_Access_Type (Typ) then
                        Access_Type_Case : declare
                           P : Node_Id;

                           function Process
                             (N : Node_Id) return Traverse_Result;
                           --  Process function for instantiation of Traverse
                           --  below. Checks if N contains reference to E other
                           --  than a dereference.

                           function Ref_In (Nod : Node_Id) return Boolean;
                           --  Determines whether Nod contains a reference to
                           --  the entity E that is not a dereference.

                           -------------
                           -- Process --
                           -------------

                           function Process
                             (N : Node_Id) return Traverse_Result
                           is
                           begin
                              if Is_Entity_Name (N)
                                and then Entity (N) = E
                                and then not Is_Dereferenced (N)
                              then
                                 return Abandon;
                              else
                                 return OK;
                              end if;
                           end Process;

                           ------------
                           -- Ref_In --
                           ------------

                           function Ref_In (Nod : Node_Id) return Boolean is
                              function Traverse is new Traverse_Func (Process);
                           begin
                              return Traverse (Nod) = Abandon;
                           end Ref_In;

                        --  Start of processing for Access_Type_Case

                        begin
                           --  Don't bother if we are inside an instance, since
                           --  the compilation of the generic template is where
                           --  the warning should be issued.

                           if In_Instance then
                              return;
                           end if;

                           --  Don't bother if this is not the main unit. If we
                           --  try to give this warning for with'ed units, we
                           --  get some false positives, since we do not record
                           --  references in other units.

                           if not In_Extended_Main_Source_Unit (E)
                                or else
                              not In_Extended_Main_Source_Unit (N)
                           then
                              return;
                           end if;

                           --  We are only interested in dereferences

                           if not Is_Dereferenced (N) then
                              return;
                           end if;

                           --  One more check, don't bother with references
                           --  that are inside conditional statements or WHILE
                           --  loops if the condition references the entity in
                           --  question. This avoids most false positives.

                           P := Parent (N);
                           loop
                              P := Parent (P);
                              exit when No (P);

                              if (Nkind (P) = N_If_Statement
                                     or else
                                   Nkind (P) = N_Elsif_Part)
                                 and then Ref_In (Condition (P))
                              then
                                 return;

                              elsif Nkind (P) = N_Loop_Statement
                                and then Present (Iteration_Scheme (P))
                                and then
                                  Ref_In (Condition (Iteration_Scheme (P)))
                              then
                                 return;
                              end if;
                           end loop;
                        end Access_Type_Case;
                     end if;

                     --  One more check, don't bother if we are within a
                     --  postcondition pragma, since the expression occurs
                     --  in a place unrelated to the actual test.

                     if not Within_Postcondition then

                        --  Here we definitely have a case for giving a warning
                        --  for a reference to an unset value. But we don't
                        --  give the warning now. Instead set Unset_Reference
                        --  in the identifier involved. The reason for this is
                        --  that if we find the variable is never ever assigned
                        --  a value then that warning is more important and
                        --  there is no point in giving the reference warning.

                        --  If this is an identifier, set the field directly

                        if Nkind (N) = N_Identifier then
                           Set_Unset_Reference (E, N);

                        --  Otherwise it is an expanded name, so set the field
                        --  of the actual identifier for the reference.

                        else
                           Set_Unset_Reference (E, Selector_Name (N));
                        end if;
                     end if;
                  end Potential_Unset_Reference;
               end if;
            end;

         --  Indexed component or slice

         when N_Indexed_Component | N_Slice =>

            --  If prefix does not involve dereferencing an access type, then
            --  we know we are OK if the component type is fully initialized,
            --  since the component will have been set as part of the default
            --  initialization.

            if not Prefix_Has_Dereference (Prefix (N))
              and then Is_OK_Fully_Initialized
            then
               return;

            --  Look at prefix in access type case, or if the component is not
            --  fully initialized.

            else
               Check_Unset_Reference (Prefix (N));
            end if;

         --  Record component

         when N_Selected_Component =>
            declare
               Pref : constant Node_Id   := Prefix (N);
               Ent  : constant Entity_Id := Entity (Selector_Name (N));

            begin
               --  If prefix involves dereferencing an access type, always
               --  check the prefix, since the issue then is whether this
               --  access value is null.

               if Prefix_Has_Dereference (Pref) then
                  null;

               --  Always go to prefix if no selector entity is set. Can this
               --  happen in the normal case? Not clear, but it definitely can
               --  happen in error cases.

               elsif No (Ent) then
                  null;

               --  For a record component, check some cases where we have
               --  reasonable cause to consider that the component is known to
               --  be or probably is initialized. In this case, we don't care
               --  if the prefix itself was explicitly initialized.

               --  Discriminants are always considered initialized

               elsif Ekind (Ent) = E_Discriminant then
                  return;

               --  An explicitly initialized component is certainly initialized

               elsif Nkind (Parent (Ent)) = N_Component_Declaration
                 and then Present (Expression (Parent (Ent)))
               then
                  return;

               --  A fully initialized component is initialized

               elsif Is_OK_Fully_Initialized then
                  return;
               end if;

               --  If none of those cases apply, check the record type prefix

               Check_Unset_Reference (Pref);
            end;

         --  For type conversions or qualifications examine the expression

         when N_Type_Conversion | N_Qualified_Expression =>
            Check_Unset_Reference (Expression (N));

         --  For explicit dereference, always check prefix, which will generate
         --  an unset reference (since this is a case of dereferencing null).

         when N_Explicit_Dereference =>
            Check_Unset_Reference (Prefix (N));

         --  All other cases are not cases of an unset reference

         when others =>
            null;

      end case;
   end Check_Unset_Reference;

   ------------------------
   -- Check_Unused_Withs --
   ------------------------

   procedure Check_Unused_Withs (Spec_Unit : Unit_Number_Type := No_Unit) is
      Cnode : Node_Id;
      Item  : Node_Id;
      Lunit : Node_Id;
      Ent   : Entity_Id;

      Munite : constant Entity_Id := Cunit_Entity (Main_Unit);
      --  This is needed for checking the special renaming case

      procedure Check_One_Unit (Unit : Unit_Number_Type);
      --  Subsidiary procedure, performs checks for specified unit

      --------------------
      -- Check_One_Unit --
      --------------------

      procedure Check_One_Unit (Unit : Unit_Number_Type) is
         Is_Visible_Renaming : Boolean := False;
         Pack                : Entity_Id;

         procedure Check_Inner_Package (Pack : Entity_Id);
         --  Pack is a package local to a unit in a with_clause. Both the unit
         --  and Pack are referenced. If none of the entities in Pack are
         --  referenced, then the only occurrence of Pack is in a USE clause
         --  or a pragma, and a warning is worthwhile as well.

         function Check_System_Aux return Boolean;
         --  Before giving a warning on a with_clause for System, check wheter
         --  a system extension is present.

         function Find_Package_Renaming
           (P : Entity_Id;
            L : Entity_Id) return Entity_Id;
         --  The only reference to a context unit may be in a renaming
         --  declaration. If this renaming declares a visible entity, do not
         --  warn that the context clause could be moved to the body, because
         --  the renaming may be intended to re-export the unit.

         function Has_Visible_Entities (P : Entity_Id) return Boolean;
         --  This function determines if a package has any visible entities.
         --  True is returned if there is at least one declared visible entity,
         --  otherwise False is returned (e.g. case of only pragmas present).

         -------------------------
         -- Check_Inner_Package --
         -------------------------

         procedure Check_Inner_Package (Pack : Entity_Id) is
            E  : Entity_Id;
            Un : constant Node_Id := Sinfo.Unit (Cnode);

            function Check_Use_Clause (N : Node_Id) return Traverse_Result;
            --  If N is a use_clause for Pack, emit warning

            procedure Check_Use_Clauses is new
              Traverse_Proc (Check_Use_Clause);

            ----------------------
            -- Check_Use_Clause --
            ----------------------

            function Check_Use_Clause (N : Node_Id) return Traverse_Result is
               Nam  : Node_Id;

            begin
               if Nkind (N) = N_Use_Package_Clause then
                  Nam := First (Names (N));
                  while Present (Nam) loop
                     if Entity (Nam) = Pack then
                        Error_Msg_Qual_Level := 1;
                        Error_Msg_NE -- CODEFIX
                          ("?no entities of package& are referenced!",
                             Nam, Pack);
                        Error_Msg_Qual_Level := 0;
                     end if;

                     Next (Nam);
                  end loop;
               end if;

               return OK;
            end Check_Use_Clause;

         --  Start of processing for Check_Inner_Package

         begin
            E := First_Entity (Pack);
            while Present (E) loop
               if Referenced_Check_Spec (E) then
                  return;
               end if;

               Next_Entity (E);
            end loop;

            --  No entities of the package are referenced. Check whether the
            --  reference to the package itself is a use clause, and if so
            --  place a warning on it.

            Check_Use_Clauses (Un);
         end Check_Inner_Package;

         ----------------------
         -- Check_System_Aux --
         ----------------------

         function Check_System_Aux return Boolean is
            Ent : Entity_Id;

         begin
            if Chars (Lunit) = Name_System
               and then Scope (Lunit) = Standard_Standard
               and then Present_System_Aux
            then
               Ent := First_Entity (System_Aux_Id);
               while Present (Ent) loop
                  if Referenced_Check_Spec (Ent) then
                     return True;
                  end if;

                  Next_Entity (Ent);
               end loop;
            end if;

            return False;
         end Check_System_Aux;

         ---------------------------
         -- Find_Package_Renaming --
         ---------------------------

         function Find_Package_Renaming
           (P : Entity_Id;
            L : Entity_Id) return Entity_Id
         is
            E1 : Entity_Id;
            R  : Entity_Id;

         begin
            Is_Visible_Renaming := False;

            E1 := First_Entity (P);
            while Present (E1) loop
               if Ekind (E1) = E_Package
                  and then Renamed_Object (E1) = L
               then
                  Is_Visible_Renaming := not Is_Hidden (E1);
                  return E1;

               elsif Ekind (E1) = E_Package
                 and then No (Renamed_Object (E1))
                 and then not Is_Generic_Instance (E1)
               then
                  R := Find_Package_Renaming (E1, L);

                  if Present (R) then
                     Is_Visible_Renaming := not Is_Hidden (R);
                     return R;
                  end if;
               end if;

               Next_Entity (E1);
            end loop;

            return Empty;
         end Find_Package_Renaming;

         --------------------------
         -- Has_Visible_Entities --
         --------------------------

         function Has_Visible_Entities (P : Entity_Id) return Boolean is
            E : Entity_Id;

         begin
            --  If unit in context is not a package, it is a subprogram that
            --  is not called or a generic unit that is not instantiated
            --  in the current unit, and warning is appropriate.

            if Ekind (P) /= E_Package then
               return True;
            end if;

            --  If unit comes from a limited_with clause, look for declaration
            --  of shadow entities.

            if Present (Limited_View (P)) then
               E := First_Entity (Limited_View (P));
            else
               E := First_Entity (P);
            end if;

            while Present (E)
              and then E /= First_Private_Entity (P)
            loop
               if Comes_From_Source (E)
                 or else Present (Limited_View (P))
               then
                  return True;
               end if;

               Next_Entity (E);
            end loop;

            return False;
         end Has_Visible_Entities;

      --  Start of processing for Check_One_Unit

      begin
         Cnode := Cunit (Unit);

         --  Only do check in units that are part of the extended main unit.
         --  This is actually a necessary restriction, because in the case of
         --  subprogram acting as its own specification, there can be with's in
         --  subunits that we will not see.

         if not In_Extended_Main_Source_Unit (Cnode) then
            return;

         --  In configurable run time mode, we remove the bodies of non-inlined
         --  subprograms, which may lead to spurious warnings, which are
         --  clearly undesirable.

         elsif Configurable_Run_Time_Mode
           and then Is_Predefined_File_Name (Unit_File_Name (Unit))
         then
            return;
         end if;

         --  Loop through context items in this unit

         Item := First (Context_Items (Cnode));
         while Present (Item) loop
            if Nkind (Item) = N_With_Clause
               and then not Implicit_With (Item)
               and then In_Extended_Main_Source_Unit (Item)
            then
               Lunit := Entity (Name (Item));

               --  Check if this unit is referenced (skip the check if this
               --  is explicitly marked by a pragma Unreferenced).

               if not Referenced (Lunit)
                 and then not Has_Unreferenced (Lunit)
               then
                  --  Suppress warnings in internal units if not in -gnatg mode
                  --  (these would be junk warnings for an application program,
                  --  since they refer to problems in internal units).

                  if GNAT_Mode
                    or else not Is_Internal_File_Name (Unit_File_Name (Unit))
                  then
                     --  Here we definitely have a non-referenced unit. If it
                     --  is the special call for a spec unit, then just set the
                     --  flag to be read later.

                     if Unit = Spec_Unit then
                        Set_Unreferenced_In_Spec (Item);

                     --  Otherwise simple unreferenced message, but skip this
                     --  if no visible entities, because that is most likely a
                     --  case where warning would be false positive (e.g. a
                     --  package with only a linker options pragma and nothing
                     --  else or a pragma elaborate with a body library task).

                     elsif Has_Visible_Entities (Entity (Name (Item))) then
                        Error_Msg_N -- CODEFIX
                          ("?unit& is not referenced!", Name (Item));
                     end if;
                  end if;

               --  If main unit is a renaming of this unit, then we consider
               --  the with to be OK (obviously it is needed in this case!)
               --  This may be transitive: the unit in the with_clause may
               --  itself be a renaming, in which case both it and the main
               --  unit rename the same ultimate package.

               elsif Present (Renamed_Entity (Munite))
                  and then
                    (Renamed_Entity (Munite) = Lunit
                      or else Renamed_Entity (Munite) = Renamed_Entity (Lunit))
               then
                  null;

               --  If this unit is referenced, and it is a package, we do
               --  another test, to see if any of the entities in the package
               --  are referenced. If none of the entities are referenced, we
               --  still post a warning. This occurs if the only use of the
               --  package is in a use clause, or in a package renaming
               --  declaration. This check is skipped for packages that are
               --  renamed in a spec, since the entities in such a package are
               --  visible to clients via the renaming.

               elsif Ekind (Lunit) = E_Package
                 and then not Renamed_In_Spec (Lunit)
               then
                  --  If Is_Instantiated is set, it means that the package is
                  --  implicitly instantiated (this is the case of parent
                  --  instance or an actual for a generic package formal), and
                  --  this counts as a reference.

                  if Is_Instantiated (Lunit) then
                     null;

                  --  If no entities in package, and there is a pragma
                  --  Elaborate_Body present, then assume that this with is
                  --  done for purposes of this elaboration.

                  elsif No (First_Entity (Lunit))
                    and then Has_Pragma_Elaborate_Body (Lunit)
                  then
                     null;

                  --  Otherwise see if any entities have been referenced

                  else
                     if Limited_Present (Item) then
                        Ent := First_Entity (Limited_View (Lunit));
                     else
                        Ent := First_Entity (Lunit);
                     end if;

                     loop
                        --  No more entities, and we did not find one that was
                        --  referenced. Means we have a definite case of a with
                        --  none of whose entities was referenced.

                        if No (Ent) then

                           --  If in spec, just set the flag

                           if Unit = Spec_Unit then
                              Set_No_Entities_Ref_In_Spec (Item);

                           elsif Check_System_Aux then
                              null;

                           --  Else give the warning

                           else
                              if not
                                Has_Unreferenced (Entity (Name (Item)))
                              then
                                 Error_Msg_N -- CODEFIX
                                   ("?no entities of & are referenced!",
                                    Name (Item));
                              end if;

                              --  Look for renamings of this package, and flag
                              --  them as well. If the original package has
                              --  warnings off, we suppress the warning on the
                              --  renaming as well.

                              Pack := Find_Package_Renaming (Munite, Lunit);

                              if Present (Pack)
                                and then not Has_Warnings_Off (Lunit)
                                and then not Has_Unreferenced (Pack)
                              then
                                 Error_Msg_NE -- CODEFIX
                                   ("?no entities of & are referenced!",
                                     Unit_Declaration_Node (Pack),
                                     Pack);
                              end if;
                           end if;

                           exit;

                        --  Case of entity being referenced. The reference may
                        --  come from a limited_with_clause, in which case the
                        --  limited view of the entity carries the flag.

                        elsif Referenced_Check_Spec (Ent)
                          or else Referenced_As_LHS_Check_Spec (Ent)
                          or else Referenced_As_Out_Parameter_Check_Spec (Ent)
                          or else
                            (From_With_Type (Ent)
                              and then Is_Incomplete_Type (Ent)
                              and then Present (Non_Limited_View (Ent))
                              and then Referenced (Non_Limited_View (Ent)))
                        then
                           --  This means that the with is indeed fine, in that
                           --  it is definitely needed somewhere, and we can
                           --  quit worrying about this one...

                           --  Except for one little detail: if either of the
                           --  flags was set during spec processing, this is
                           --  where we complain that the with could be moved
                           --  from the spec. If the spec contains a visible
                           --  renaming of the package, inhibit warning to move
                           --  with_clause to body.

                           if Ekind (Munite) = E_Package_Body then
                              Pack :=
                                Find_Package_Renaming
                                  (Spec_Entity (Munite), Lunit);
                           end if;

                           if Unreferenced_In_Spec (Item) then
                              Error_Msg_N -- CODEFIX
                                ("?unit& is not referenced in spec!",
                                 Name (Item));

                           elsif No_Entities_Ref_In_Spec (Item) then
                              Error_Msg_N -- CODEFIX
                                ("?no entities of & are referenced in spec!",
                                 Name (Item));

                           else
                              if Ekind (Ent) = E_Package then
                                 Check_Inner_Package (Ent);
                              end if;

                              exit;
                           end if;

                           if not Is_Visible_Renaming then
                              Error_Msg_N -- CODEFIX
                                ("\?with clause might be moved to body!",
                                 Name (Item));
                           end if;

                           exit;

                        --  Move to next entity to continue search

                        else
                           Next_Entity (Ent);
                        end if;
                     end loop;
                  end if;

               --  For a generic package, the only interesting kind of
               --  reference is an instantiation, since entities cannot be
               --  referenced directly.

               elsif Is_Generic_Unit (Lunit) then

                  --  Unit was never instantiated, set flag for case of spec
                  --  call, or give warning for normal call.

                  if not Is_Instantiated (Lunit) then
                     if Unit = Spec_Unit then
                        Set_Unreferenced_In_Spec (Item);
                     else
                        Error_Msg_N -- CODEFIX
                          ("?unit& is never instantiated!", Name (Item));
                     end if;

                  --  If unit was indeed instantiated, make sure that flag is
                  --  not set showing it was uninstantiated in the spec, and if
                  --  so, give warning.

                  elsif Unreferenced_In_Spec (Item) then
                     Error_Msg_N
                       ("?unit& is not instantiated in spec!", Name (Item));
                     Error_Msg_N -- CODEFIX
                       ("\?with clause can be moved to body!", Name (Item));
                  end if;
               end if;
            end if;

            Next (Item);
         end loop;
      end Check_One_Unit;

   --  Start of processing for Check_Unused_Withs

   begin
      if not Opt.Check_Withs
        or else Operating_Mode = Check_Syntax
      then
         return;
      end if;

      --  Flag any unused with clauses, but skip this step if we are compiling
      --  a subunit on its own, since we do not have enough information to
      --  determine whether with's are used. We will get the relevant warnings
      --  when we compile the parent. This is the normal style of GNAT
      --  compilation in any case.

      if Nkind (Unit (Cunit (Main_Unit))) = N_Subunit then
         return;
      end if;

      --  Process specified units

      if Spec_Unit = No_Unit then

         --  For main call, check all units

         for Unit in Main_Unit .. Last_Unit loop
            Check_One_Unit (Unit);
         end loop;

      else
         --  For call for spec, check only the spec

         Check_One_Unit (Spec_Unit);
      end if;
   end Check_Unused_Withs;

   ---------------------------------
   -- Generic_Package_Spec_Entity --
   ---------------------------------

   function Generic_Package_Spec_Entity (E : Entity_Id) return Boolean is
      S : Entity_Id;

   begin
      if Is_Package_Body_Entity (E) then
         return False;

      else
         S := Scope (E);
         loop
            if S = Standard_Standard then
               return False;

            elsif Ekind (S) = E_Generic_Package then
               return True;

            elsif Ekind (S) = E_Package then
               S := Scope (S);

            else
               return False;
            end if;
         end loop;
      end if;
   end Generic_Package_Spec_Entity;

   ----------------------
   -- Goto_Spec_Entity --
   ----------------------

   function Goto_Spec_Entity (E : Entity_Id) return Entity_Id is
   begin
      if Is_Formal (E)
        and then Present (Spec_Entity (E))
      then
         return Spec_Entity (E);
      else
         return E;
      end if;
   end Goto_Spec_Entity;

   --------------------------------------
   -- Has_Pragma_Unmodified_Check_Spec --
   --------------------------------------

   function Has_Pragma_Unmodified_Check_Spec
     (E : Entity_Id) return Boolean
   is
   begin
      if Is_Formal (E) and then Present (Spec_Entity (E)) then

         --  Note: use of OR instead of OR ELSE here is deliberate, we want
         --  to mess with Unmodified flags on both body and spec entities.

         return Has_Unmodified (E)
                  or
                Has_Unmodified (Spec_Entity (E));

      else
         return Has_Unmodified (E);
      end if;
   end Has_Pragma_Unmodified_Check_Spec;

   ----------------------------------------
   -- Has_Pragma_Unreferenced_Check_Spec --
   ----------------------------------------

   function Has_Pragma_Unreferenced_Check_Spec
     (E : Entity_Id) return Boolean
   is
   begin
      if Is_Formal (E) and then Present (Spec_Entity (E)) then

         --  Note: use of OR here instead of OR ELSE is deliberate, we want
         --  to mess with flags on both entities.

         return Has_Unreferenced (E)
                  or
                Has_Unreferenced (Spec_Entity (E));

      else
         return Has_Unreferenced (E);
      end if;
   end Has_Pragma_Unreferenced_Check_Spec;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize is
   begin
      Warnings_Off_Pragmas.Init;
      Unreferenced_Entities.Init;
      In_Out_Warnings.Init;
   end Initialize;

   ------------------------------------
   -- Never_Set_In_Source_Check_Spec --
   ------------------------------------

   function Never_Set_In_Source_Check_Spec (E : Entity_Id) return Boolean is
   begin
      if Is_Formal (E) and then Present (Spec_Entity (E)) then
         return Never_Set_In_Source (E)
                  and then
                Never_Set_In_Source (Spec_Entity (E));
      else
         return Never_Set_In_Source (E);
      end if;
   end Never_Set_In_Source_Check_Spec;

   -------------------------------------
   -- Operand_Has_Warnings_Suppressed --
   -------------------------------------

   function Operand_Has_Warnings_Suppressed (N : Node_Id) return Boolean is

      function Check_For_Warnings (N : Node_Id) return Traverse_Result;
      --  Function used to check one node to see if it is or was originally
      --  a reference to an entity for which Warnings are off. If so, Abandon
      --  is returned, otherwise OK_Orig is returned to continue the traversal
      --  of the original expression.

      function Traverse is new Traverse_Func (Check_For_Warnings);
      --  Function used to traverse tree looking for warnings

      ------------------------
      -- Check_For_Warnings --
      ------------------------

      function Check_For_Warnings (N : Node_Id) return Traverse_Result is
         R : constant Node_Id := Original_Node (N);

      begin
         if Nkind (R) in N_Has_Entity
           and then Present (Entity (R))
           and then Has_Warnings_Off (Entity (R))
         then
            return Abandon;
         else
            return OK_Orig;
         end if;
      end Check_For_Warnings;

   --  Start of processing for Operand_Has_Warnings_Suppressed

   begin
      return Traverse (N) = Abandon;

   --  If any exception occurs, then something has gone wrong, and this is
   --  only a minor aesthetic issue anyway, so just say we did not find what
   --  we are looking for, rather than blow up.

   exception
      when others =>
         return False;
   end Operand_Has_Warnings_Suppressed;

   -----------------------------------------
   -- Output_Non_Modified_In_Out_Warnings --
   -----------------------------------------

   procedure Output_Non_Modified_In_Out_Warnings is

      function No_Warn_On_In_Out (E : Entity_Id) return Boolean;
      --  Given a formal parameter entity E, determines if there is a reason to
      --  suppress IN OUT warnings (not modified, could be IN) for formals of
      --  the subprogram. We suppress these warnings if Warnings Off is set, or
      --  if we have seen the address of the subprogram being taken, or if the
      --  subprogram is used as a generic actual (in the latter cases the
      --  context may force use of IN OUT, even if the parameter is not
      --  modifies for this particular case.

      -----------------------
      -- No_Warn_On_In_Out --
      -----------------------

      function No_Warn_On_In_Out (E : Entity_Id) return Boolean is
         S  : constant Entity_Id := Scope (E);
         SE : constant Entity_Id := Spec_Entity (E);

      begin
         --  Do not warn if address is taken, since funny business may be going
         --  on in treating the parameter indirectly as IN OUT.

         if Address_Taken (S)
           or else (Present (SE) and then Address_Taken (Scope (SE)))
         then
            return True;

         --  Do not warn if used as a generic actual, since the generic may be
         --  what is forcing the use of an "unnecessary" IN OUT.

         elsif Used_As_Generic_Actual (S)
           or else (Present (SE) and then Used_As_Generic_Actual (Scope (SE)))
         then
            return True;

         --  Else test warnings off

         elsif Warnings_Off_Check_Spec (S) then
            return True;

         --  All tests for suppressing warning failed

         else
            return False;
         end if;
      end No_Warn_On_In_Out;

   --  Start of processing for Output_Non_Modified_In_Out_Warnings

   begin
      --  Loop through entities for which a warning may be needed

      for J in In_Out_Warnings.First .. In_Out_Warnings.Last loop
         declare
            E1 : constant Entity_Id := In_Out_Warnings.Table (J);

         begin
            --  Suppress warning in specific cases (see details in comments for
            --  No_Warn_On_In_Out), or if there is a pragma Unmodified.

            if Has_Pragma_Unmodified_Check_Spec (E1)
              or else No_Warn_On_In_Out (E1)
            then
               null;

            --  Here we generate the warning

            else
               --  If -gnatwc is set then output message that we could be IN

               if not Is_Trivial_Subprogram (Scope (E1)) then
                  if Warn_On_Constant then
                     Error_Msg_N
                       ("?formal parameter & is not modified!", E1);
                     Error_Msg_N
                       ("\?mode could be IN instead of `IN OUT`!", E1);

                     --  We do not generate warnings for IN OUT parameters
                     --  unless we have at least -gnatwu. This is deliberately
                     --  inconsistent with the treatment of variables, but
                     --  otherwise we get too many unexpected warnings in
                     --  default mode.

                  elsif Check_Unreferenced then
                     Error_Msg_N
                       ("?formal parameter& is read but "
                        & "never assigned!", E1);
                  end if;
               end if;

               --  Kill any other warnings on this entity, since this is the
               --  one that should dominate any other unreferenced warning.

               Set_Warnings_Off (E1);
            end if;
         end;
      end loop;
   end Output_Non_Modified_In_Out_Warnings;

   ----------------------------------------
   -- Output_Obsolescent_Entity_Warnings --
   ----------------------------------------

   procedure Output_Obsolescent_Entity_Warnings (N : Node_Id; E : Entity_Id) is
      P : constant Node_Id := Parent (N);
      S : Entity_Id;

   begin
      S := Current_Scope;

      --  Do not output message if we are the scope of standard. This means
      --  we have a reference from a context clause from when it is originally
      --  processed, and that's too early to tell whether it is an obsolescent
      --  unit doing the with'ing. In Sem_Ch10.Analyze_Compilation_Unit we make
      --  sure that we have a later call when the scope is available. This test
      --  also eliminates all messages for use clauses, which is fine (we do
      --  not want messages for use clauses, since they are always redundant
      --  with respect to the associated with clause).

      if S = Standard_Standard then
         return;
      end if;

      --  Do not output message if we are in scope of an obsolescent package
      --  or subprogram.

      loop
         if Is_Obsolescent (S) then
            return;
         end if;

         S := Scope (S);
         exit when S = Standard_Standard;
      end loop;

      --  Here we will output the message

      Error_Msg_Sloc := Sloc (E);

      --  Case of with clause

      if Nkind (P) = N_With_Clause then
         if Ekind (E) = E_Package then
            Error_Msg_NE
              ("?with of obsolescent package& declared#", N, E);
         elsif Ekind (E) = E_Procedure then
            Error_Msg_NE
              ("?with of obsolescent procedure& declared#", N, E);
         else
            Error_Msg_NE
              ("?with of obsolescent function& declared#", N, E);
         end if;

      --  If we do not have a with clause, then ignore any reference to an
      --  obsolescent package name. We only want to give the one warning of
      --  withing the package, not one each time it is used to qualify.

      elsif Ekind (E) = E_Package then
         return;

      --  Procedure call statement

      elsif Nkind (P) = N_Procedure_Call_Statement then
         Error_Msg_NE
           ("?call to obsolescent procedure& declared#", N, E);

      --  Function call

      elsif Nkind (P) = N_Function_Call then
         Error_Msg_NE
           ("?call to obsolescent function& declared#", N, E);

      --  Reference to obsolescent type

      elsif Is_Type (E) then
         Error_Msg_NE
           ("?reference to obsolescent type& declared#", N, E);

      --  Reference to obsolescent component

      elsif Ekind_In (E, E_Component, E_Discriminant) then
         Error_Msg_NE
           ("?reference to obsolescent component& declared#", N, E);

      --  Reference to obsolescent variable

      elsif Ekind (E) = E_Variable then
         Error_Msg_NE
           ("?reference to obsolescent variable& declared#", N, E);

      --  Reference to obsolescent constant

      elsif Ekind (E) = E_Constant
        or else Ekind (E) in Named_Kind
      then
         Error_Msg_NE
           ("?reference to obsolescent constant& declared#", N, E);

      --  Reference to obsolescent enumeration literal

      elsif Ekind (E) = E_Enumeration_Literal then
         Error_Msg_NE
           ("?reference to obsolescent enumeration literal& declared#", N, E);

      --  Generic message for any other case we missed

      else
         Error_Msg_NE
           ("?reference to obsolescent entity& declared#", N, E);
      end if;

      --  Output additional warning if present

      for J in Obsolescent_Warnings.First .. Obsolescent_Warnings.Last loop
         if Obsolescent_Warnings.Table (J).Ent = E then
            String_To_Name_Buffer (Obsolescent_Warnings.Table (J).Msg);
            Error_Msg_Strlen := Name_Len;
            Error_Msg_String (1 .. Name_Len) := Name_Buffer (1 .. Name_Len);
            Error_Msg_N ("\\?~", N);
            exit;
         end if;
      end loop;
   end Output_Obsolescent_Entity_Warnings;

   ----------------------------------
   -- Output_Unreferenced_Messages --
   ----------------------------------

   procedure Output_Unreferenced_Messages is
   begin
      for J in Unreferenced_Entities.First ..
               Unreferenced_Entities.Last
      loop
         Warn_On_Unreferenced_Entity (Unreferenced_Entities.Table (J));
      end loop;
   end Output_Unreferenced_Messages;

   -----------------------------------------
   -- Output_Unused_Warnings_Off_Warnings --
   -----------------------------------------

   procedure Output_Unused_Warnings_Off_Warnings is
   begin
      for J in Warnings_Off_Pragmas.First .. Warnings_Off_Pragmas.Last loop
         declare
            Wentry : Warnings_Off_Entry renames Warnings_Off_Pragmas.Table (J);
            N      : Node_Id renames Wentry.N;
            E      : Node_Id renames Wentry.E;

         begin
            --  Turn off Warnings_Off, or we won't get the warning!

            Set_Warnings_Off (E, False);

            --  Nothing to do if pragma was used to suppress a general warning

            if Warnings_Off_Used (E) then
               null;

            --  If pragma was used both in unmodified and unreferenced contexts
            --  then that's as good as the general case, no warning.

            elsif Warnings_Off_Used_Unmodified (E)
                    and
                  Warnings_Off_Used_Unreferenced (E)
            then
               null;

            --  Used only in context where Unmodified would have worked

            elsif Warnings_Off_Used_Unmodified (E) then
               Error_Msg_NE
                 ("?could use Unmodified instead of "
                  & "Warnings Off for &", Pragma_Identifier (N), E);

            --  Used only in context where Unreferenced would have worked

            elsif Warnings_Off_Used_Unreferenced (E) then
               Error_Msg_NE
                 ("?could use Unreferenced instead of "
                  & "Warnings Off for &", Pragma_Identifier (N), E);

            --  Not used at all

            else
               Error_Msg_NE
                 ("?pragma Warnings Off for & unused, "
                  & "could be omitted", N, E);
            end if;
         end;
      end loop;
   end Output_Unused_Warnings_Off_Warnings;

   ---------------------------
   -- Referenced_Check_Spec --
   ---------------------------

   function Referenced_Check_Spec (E : Entity_Id) return Boolean is
   begin
      if Is_Formal (E) and then Present (Spec_Entity (E)) then
         return Referenced (E) or else Referenced (Spec_Entity (E));
      else
         return Referenced (E);
      end if;
   end Referenced_Check_Spec;

   ----------------------------------
   -- Referenced_As_LHS_Check_Spec --
   ----------------------------------

   function Referenced_As_LHS_Check_Spec (E : Entity_Id) return Boolean is
   begin
      if Is_Formal (E) and then Present (Spec_Entity (E)) then
         return Referenced_As_LHS (E)
           or else Referenced_As_LHS (Spec_Entity (E));
      else
         return Referenced_As_LHS (E);
      end if;
   end Referenced_As_LHS_Check_Spec;

   --------------------------------------------
   -- Referenced_As_Out_Parameter_Check_Spec --
   --------------------------------------------

   function Referenced_As_Out_Parameter_Check_Spec
     (E : Entity_Id) return Boolean
   is
   begin
      if Is_Formal (E) and then Present (Spec_Entity (E)) then
         return Referenced_As_Out_Parameter (E)
           or else Referenced_As_Out_Parameter (Spec_Entity (E));
      else
         return Referenced_As_Out_Parameter (E);
      end if;
   end Referenced_As_Out_Parameter_Check_Spec;

   ----------------------------
   -- Set_Dot_Warning_Switch --
   ----------------------------

   function Set_Dot_Warning_Switch (C : Character) return Boolean is
   begin
      case C is
         when 'a' =>
            Warn_On_Assertion_Failure           := True;

         when 'A' =>
            Warn_On_Assertion_Failure           := False;

         when 'b' =>
            Warn_On_Biased_Representation       := True;

         when 'B' =>
            Warn_On_Biased_Representation       := False;

         when 'c' =>
            Warn_On_Unrepped_Components         := True;

         when 'C' =>
            Warn_On_Unrepped_Components         := False;

         when 'e' =>
            Address_Clause_Overlay_Warnings     := True;
            Check_Unreferenced                  := True;
            Check_Unreferenced_Formals          := True;
            Check_Withs                         := True;
            Constant_Condition_Warnings         := True;
            Elab_Warnings                       := True;
            Implementation_Unit_Warnings        := True;
            Ineffective_Inline_Warnings         := True;
            List_Inherited_Aspects              := True;
            Warn_On_Ada_2005_Compatibility      := True;
            Warn_On_Ada_2012_Compatibility      := True;
            Warn_On_All_Unread_Out_Parameters   := True;
            Warn_On_Assertion_Failure           := True;
            Warn_On_Assumed_Low_Bound           := True;
            Warn_On_Bad_Fixed_Value             := True;
            Warn_On_Biased_Representation       := True;
            Warn_On_Constant                    := True;
            Warn_On_Deleted_Code                := True;
            Warn_On_Dereference                 := True;
            Warn_On_Export_Import               := True;
            Warn_On_Hiding                      := True;
            Warn_On_Modified_Unread             := True;
            Warn_On_No_Value_Assigned           := True;
            Warn_On_Non_Local_Exception         := True;
            Warn_On_Object_Renames_Function     := True;
            Warn_On_Obsolescent_Feature         := True;
            Warn_On_Overlap                     := True;
            Warn_On_Overridden_Size             := True;
            Warn_On_Parameter_Order             := True;
            Warn_On_Questionable_Missing_Parens := True;
            Warn_On_Record_Holes                := True;
            Warn_On_Redundant_Constructs        := True;
            Warn_On_Reverse_Bit_Order           := True;
            Warn_On_Unchecked_Conversion        := True;
            Warn_On_Unordered_Enumeration_Type  := True;
            Warn_On_Unrecognized_Pragma         := True;
            Warn_On_Unrepped_Components         := True;
            Warn_On_Warnings_Off                := True;

         when 'g' =>
            Set_GNAT_Mode_Warnings;

         when 'h' =>
            Warn_On_Record_Holes                := True;

         when 'H' =>
            Warn_On_Record_Holes                := False;

         when 'i' =>
            Warn_On_Overlap                     := True;

         when 'I' =>
            Warn_On_Overlap                     := False;

         when 'l' =>
            List_Inherited_Aspects              := True;

         when 'L' =>
            List_Inherited_Aspects              := False;

         when 'm' =>
            Warn_On_Suspicious_Modulus_Value    := True;

         when 'M' =>
            Warn_On_Suspicious_Modulus_Value    := False;

         when 'o' =>
            Warn_On_All_Unread_Out_Parameters   := True;

         when 'O' =>
            Warn_On_All_Unread_Out_Parameters   := False;

         when 'p' =>
            Warn_On_Parameter_Order             := True;

         when 'P' =>
            Warn_On_Parameter_Order             := False;

         when 'r' =>
            Warn_On_Object_Renames_Function     := True;

         when 'R' =>
            Warn_On_Object_Renames_Function     := False;

         when 's' =>
            Warn_On_Overridden_Size             := True;

         when 'S' =>
            Warn_On_Overridden_Size             := False;

         when 'u' =>
            Warn_On_Unordered_Enumeration_Type  := True;

         when 'U' =>
            Warn_On_Unordered_Enumeration_Type  := False;

         when 'v' =>
            Warn_On_Reverse_Bit_Order           := True;

         when 'V' =>
            Warn_On_Reverse_Bit_Order           := False;

         when 'w' =>
            Warn_On_Warnings_Off                := True;

         when 'W' =>
            Warn_On_Warnings_Off                := False;

         when 'x' =>
            Warn_On_Non_Local_Exception         := True;

         when 'X' =>
            Warn_On_Non_Local_Exception         := False;
            No_Warn_On_Non_Local_Exception      := True;

         when others =>
            return False;
      end case;

      return True;
   end Set_Dot_Warning_Switch;

   ----------------------------
   -- Set_GNAT_Mode_Warnings --
   ----------------------------

   procedure Set_GNAT_Mode_Warnings is
   begin
      Address_Clause_Overlay_Warnings     := True;
      Check_Unreferenced                  := True;
      Check_Unreferenced_Formals          := True;
      Check_Withs                         := True;
      Constant_Condition_Warnings         := True;
      Elab_Warnings                       := False;
      Implementation_Unit_Warnings        := False;
      Ineffective_Inline_Warnings         := True;
      List_Inherited_Aspects              := False;
      Warn_On_Ada_2005_Compatibility      := True;
      Warn_On_Ada_2012_Compatibility      := True;
      Warn_On_All_Unread_Out_Parameters   := False;
      Warn_On_Assertion_Failure           := True;
      Warn_On_Assumed_Low_Bound           := True;
      Warn_On_Bad_Fixed_Value             := True;
      Warn_On_Biased_Representation       := True;
      Warn_On_Constant                    := True;
      Warn_On_Deleted_Code                := False;
      Warn_On_Dereference                 := False;
      Warn_On_Export_Import               := True;
      Warn_On_Hiding                      := False;
      Warn_On_Modified_Unread             := True;
      Warn_On_No_Value_Assigned           := True;
      Warn_On_Non_Local_Exception         := False;
      Warn_On_Object_Renames_Function     := False;
      Warn_On_Obsolescent_Feature         := True;
      Warn_On_Questionable_Missing_Parens := True;
      Warn_On_Redundant_Constructs        := True;
      Warn_On_Reverse_Bit_Order           := False;
      Warn_On_Object_Renames_Function     := True;
      Warn_On_Unchecked_Conversion        := True;
      Warn_On_Unordered_Enumeration_Type  := False;
      Warn_On_Unrecognized_Pragma         := True;
      Warn_On_Unrepped_Components         := False;
      Warn_On_Warnings_Off                := False;
   end Set_GNAT_Mode_Warnings;

   ------------------------
   -- Set_Warning_Switch --
   ------------------------

   function Set_Warning_Switch (C : Character) return Boolean is
   begin
      case C is
         when 'a' =>
            Check_Unreferenced                  := True;
            Check_Unreferenced_Formals          := True;
            Check_Withs                         := True;
            Constant_Condition_Warnings         := True;
            Implementation_Unit_Warnings        := True;
            Ineffective_Inline_Warnings         := True;
            List_Inherited_Aspects              := True;
            Warn_On_Ada_2005_Compatibility      := True;
            Warn_On_Ada_2012_Compatibility      := True;
            Warn_On_Assertion_Failure           := True;
            Warn_On_Assumed_Low_Bound           := True;
            Warn_On_Bad_Fixed_Value             := True;
            Warn_On_Biased_Representation       := True;
            Warn_On_Constant                    := True;
            Warn_On_Export_Import               := True;
            Warn_On_Modified_Unread             := True;
            Warn_On_No_Value_Assigned           := True;
            Warn_On_Non_Local_Exception         := True;
            Warn_On_Object_Renames_Function     := True;
            Warn_On_Obsolescent_Feature         := True;
            Warn_On_Parameter_Order             := True;
            Warn_On_Questionable_Missing_Parens := True;
            Warn_On_Redundant_Constructs        := True;
            Warn_On_Reverse_Bit_Order           := True;
            Warn_On_Unchecked_Conversion        := True;
            Warn_On_Unrecognized_Pragma         := True;
            Warn_On_Unrepped_Components         := True;

         when 'A' =>
            Address_Clause_Overlay_Warnings     := False;
            Check_Unreferenced                  := False;
            Check_Unreferenced_Formals          := False;
            Check_Withs                         := False;
            Constant_Condition_Warnings         := False;
            Elab_Warnings                       := False;
            Implementation_Unit_Warnings        := False;
            Ineffective_Inline_Warnings         := False;
            List_Inherited_Aspects              := False;
            Warn_On_Ada_2005_Compatibility      := False;
            Warn_On_Ada_2012_Compatibility      := False;
            Warn_On_All_Unread_Out_Parameters   := False;
            Warn_On_Assertion_Failure           := False;
            Warn_On_Assumed_Low_Bound           := False;
            Warn_On_Bad_Fixed_Value             := False;
            Warn_On_Biased_Representation       := False;
            Warn_On_Constant                    := False;
            Warn_On_Deleted_Code                := False;
            Warn_On_Dereference                 := False;
            Warn_On_Export_Import               := False;
            Warn_On_Hiding                      := False;
            Warn_On_Modified_Unread             := False;
            Warn_On_No_Value_Assigned           := False;
            Warn_On_Non_Local_Exception         := False;
            Warn_On_Object_Renames_Function     := False;
            Warn_On_Obsolescent_Feature         := False;
            Warn_On_Overlap                     := False;
            Warn_On_Overridden_Size             := False;
            Warn_On_Parameter_Order             := False;
            Warn_On_Record_Holes                := False;
            Warn_On_Questionable_Missing_Parens := False;
            Warn_On_Redundant_Constructs        := False;
            Warn_On_Reverse_Bit_Order           := False;
            Warn_On_Unchecked_Conversion        := False;
            Warn_On_Unordered_Enumeration_Type  := False;
            Warn_On_Unrecognized_Pragma         := False;
            Warn_On_Unrepped_Components         := False;
            Warn_On_Warnings_Off                := False;

            No_Warn_On_Non_Local_Exception      := True;

         when 'b' =>
            Warn_On_Bad_Fixed_Value             := True;

         when 'B' =>
            Warn_On_Bad_Fixed_Value             := False;

         when 'c' =>
            Constant_Condition_Warnings         := True;

         when 'C' =>
            Constant_Condition_Warnings         := False;

         when 'd' =>
            Warn_On_Dereference                 := True;

         when 'D' =>
            Warn_On_Dereference                 := False;

         when 'e' =>
            Warning_Mode                        := Treat_As_Error;

         when 'f' =>
            Check_Unreferenced_Formals          := True;

         when 'F' =>
            Check_Unreferenced_Formals          := False;

         when 'g' =>
            Warn_On_Unrecognized_Pragma         := True;

         when 'G' =>
            Warn_On_Unrecognized_Pragma         := False;

         when 'h' =>
            Warn_On_Hiding                      := True;

         when 'H' =>
            Warn_On_Hiding                      := False;

         when 'i' =>
            Implementation_Unit_Warnings        := True;

         when 'I' =>
            Implementation_Unit_Warnings        := False;

         when 'j' =>
            Warn_On_Obsolescent_Feature         := True;

         when 'J' =>
            Warn_On_Obsolescent_Feature         := False;

         when 'k' =>
            Warn_On_Constant                    := True;

         when 'K' =>
            Warn_On_Constant                    := False;

         when 'l' =>
            Elab_Warnings                       := True;

         when 'L' =>
            Elab_Warnings                       := False;

         when 'm' =>
            Warn_On_Modified_Unread             := True;

         when 'M' =>
            Warn_On_Modified_Unread             := False;

         when 'n' =>
            Warning_Mode                        := Normal;

         when 'o' =>
            Address_Clause_Overlay_Warnings     := True;

         when 'O' =>
            Address_Clause_Overlay_Warnings     := False;

         when 'p' =>
            Ineffective_Inline_Warnings         := True;

         when 'P' =>
            Ineffective_Inline_Warnings         := False;

         when 'q' =>
            Warn_On_Questionable_Missing_Parens := True;

         when 'Q' =>
            Warn_On_Questionable_Missing_Parens := False;

         when 'r' =>
            Warn_On_Redundant_Constructs        := True;

         when 'R' =>
            Warn_On_Redundant_Constructs        := False;

         when 's' =>
            Warning_Mode                        := Suppress;

         when 't' =>
            Warn_On_Deleted_Code                := True;

         when 'T' =>
            Warn_On_Deleted_Code                := False;

         when 'u' =>
            Check_Unreferenced                  := True;
            Check_Withs                         := True;
            Check_Unreferenced_Formals          := True;

         when 'U' =>
            Check_Unreferenced                  := False;
            Check_Withs                         := False;
            Check_Unreferenced_Formals          := False;

         when 'v' =>
            Warn_On_No_Value_Assigned           := True;

         when 'V' =>
            Warn_On_No_Value_Assigned           := False;

         when 'w' =>
            Warn_On_Assumed_Low_Bound           := True;

         when 'W' =>
            Warn_On_Assumed_Low_Bound           := False;

         when 'x' =>
            Warn_On_Export_Import               := True;

         when 'X' =>
            Warn_On_Export_Import               := False;

         when 'y' =>
            Warn_On_Ada_2005_Compatibility      := True;
            Warn_On_Ada_2012_Compatibility      := True;

         when 'Y' =>
            Warn_On_Ada_2005_Compatibility      := False;
            Warn_On_Ada_2012_Compatibility      := False;

         when 'z' =>
            Warn_On_Unchecked_Conversion        := True;

         when 'Z' =>
            Warn_On_Unchecked_Conversion        := False;

         when others =>
            return False;
      end case;

      return True;
   end Set_Warning_Switch;

   -----------------------------
   -- Warn_On_Known_Condition --
   -----------------------------

   procedure Warn_On_Known_Condition (C : Node_Id) is
      P           : Node_Id;
      Orig        : constant Node_Id := Original_Node (C);
      Test_Result : Boolean;

      function Is_Known_Branch return Boolean;
      --  If the type of the condition is Boolean, the constant value of the
      --  condition is a boolean literal. If the type is a derived boolean
      --  type, the constant is wrapped in a type conversion of the derived
      --  literal. If the value of the condition is not a literal, no warnings
      --  can be produced. This function returns True if the result can be
      --  determined, and Test_Result is set True/False accordingly. Otherwise
      --  False is returned, and Test_Result is unchanged.

      procedure Track (N : Node_Id; Loc : Node_Id);
      --  Adds continuation warning(s) pointing to reason (assignment or test)
      --  for the operand of the conditional having a known value (or at least
      --  enough is known about the value to issue the warning). N is the node
      --  which is judged to have a known value. Loc is the warning location.

      ---------------------
      -- Is_Known_Branch --
      ---------------------

      function Is_Known_Branch return Boolean is
      begin
         if Etype (C) = Standard_Boolean
           and then Is_Entity_Name (C)
           and then
             (Entity (C) = Standard_False or else Entity (C) = Standard_True)
         then
            Test_Result := Entity (C) = Standard_True;
            return True;

         elsif Is_Boolean_Type (Etype (C))
           and then Nkind (C) = N_Unchecked_Type_Conversion
           and then Is_Entity_Name (Expression (C))
           and then Ekind (Entity (Expression (C))) = E_Enumeration_Literal
         then
            Test_Result :=
              Chars (Entity (Expression (C))) = Chars (Standard_True);
            return True;

         else
            return False;
         end if;
      end Is_Known_Branch;

      -----------
      -- Track --
      -----------

      procedure Track (N : Node_Id; Loc : Node_Id) is
         Nod : constant Node_Id := Original_Node (N);

      begin
         if Nkind (Nod) in N_Op_Compare then
            Track (Left_Opnd (Nod), Loc);
            Track (Right_Opnd (Nod), Loc);

         elsif Is_Entity_Name (Nod)
           and then Is_Object (Entity (Nod))
         then
            declare
               CV : constant Node_Id := Current_Value (Entity (Nod));

            begin
               if Present (CV) then
                  Error_Msg_Sloc := Sloc (CV);

                  if Nkind (CV) not in N_Subexpr then
                     Error_Msg_N ("\\?(see test #)", Loc);

                  elsif Nkind (Parent (CV)) =
                          N_Case_Statement_Alternative
                  then
                     Error_Msg_N ("\\?(see case alternative #)", Loc);

                  else
                     Error_Msg_N ("\\?(see assignment #)", Loc);
                  end if;
               end if;
            end;
         end if;
      end Track;

   --  Start of processing for Warn_On_Known_Condition

   begin
      --  Adjust SCO condition if from source

      if Generate_SCO
        and then Comes_From_Source (Orig)
        and then Is_Known_Branch
      then
         declare
            Atrue : Boolean;

         begin
            Atrue := Test_Result;

            if Present (Parent (C)) and then Nkind (Parent (C)) = N_Op_Not then
               Atrue := not Atrue;
            end if;

            Set_SCO_Condition (Orig, Atrue);
         end;
      end if;

      --  Argument replacement in an inlined body can make conditions static.
      --  Do not emit warnings in this case.

      if In_Inlined_Body then
         return;
      end if;

      if Constant_Condition_Warnings
        and then Is_Known_Branch
        and then Comes_From_Source (Original_Node (C))
        and then not In_Instance
      then
         --  See if this is in a statement or a declaration

         P := Parent (C);
         loop
            --  If tree is not attached, do not issue warning (this is very
            --  peculiar, and probably arises from some other error condition)

            if No (P) then
               return;

            --  If we are in a declaration, then no warning, since in practice
            --  conditionals in declarations are used for intended tests which
            --  may be known at compile time, e.g. things like

            --    x : constant Integer := 2 + (Word'Size = 32);

            --  And a warning is annoying in such cases

            elsif Nkind (P) in N_Declaration
                    or else
                  Nkind (P) in N_Later_Decl_Item
            then
               return;

            --  Don't warn in assert or check pragma, since presumably tests in
            --  such a context are very definitely intended, and might well be
            --  known at compile time. Note that we have to test the original
            --  node, since assert pragmas get rewritten at analysis time.

            elsif Nkind (Original_Node (P)) = N_Pragma
              and then (Pragma_Name (Original_Node (P)) = Name_Assert
                          or else
                        Pragma_Name (Original_Node (P)) = Name_Check)
            then
               return;
            end if;

            exit when Is_Statement (P);
            P := Parent (P);
         end loop;

         --  Here we issue the warning unless some sub-operand has warnings
         --  set off, in which case we suppress the warning for the node. If
         --  the original expression is an inequality, it has been expanded
         --  into a negation, and the value of the original expression is the
         --  negation of the equality. If the expression is an entity that
         --  appears within a negation, it is clearer to flag the negation
         --  itself, and report on its constant value.

         if not Operand_Has_Warnings_Suppressed (C) then
            declare
               True_Branch : Boolean := Test_Result;
               Cond        : Node_Id := C;

            begin
               if Present (Parent (C))
                 and then Nkind (Parent (C)) = N_Op_Not
               then
                  True_Branch := not True_Branch;
                  Cond        := Parent (C);
               end if;

               if True_Branch then
                  if Is_Entity_Name (Original_Node (C))
                    and then Nkind (Cond) /= N_Op_Not
                  then
                     Error_Msg_NE
                       ("object & is always True?", Cond, Original_Node (C));
                     Track (Original_Node (C), Cond);

                  else
                     Error_Msg_N ("condition is always True?", Cond);
                     Track (Cond, Cond);
                  end if;

               else
                  Error_Msg_N ("condition is always False?", Cond);
                  Track (Cond, Cond);
               end if;
            end;
         end if;
      end if;
   end Warn_On_Known_Condition;

   ---------------------------------------
   -- Warn_On_Modified_As_Out_Parameter --
   ---------------------------------------

   function Warn_On_Modified_As_Out_Parameter (E : Entity_Id) return Boolean is
   begin
      return
        (Warn_On_Modified_Unread and then Is_Only_Out_Parameter (E))
           or else Warn_On_All_Unread_Out_Parameters;
   end Warn_On_Modified_As_Out_Parameter;

   ---------------------------------
   -- Warn_On_Overlapping_Actuals --
   ---------------------------------

   procedure Warn_On_Overlapping_Actuals (Subp : Entity_Id; N : Node_Id) is
      Act1, Act2   : Node_Id;
      Form1, Form2 : Entity_Id;

   begin
      if not Warn_On_Overlap then
         return;
      end if;

      --  Exclude calls rewritten as enumeration literals

      if not Nkind_In (N, N_Function_Call, N_Procedure_Call_Statement) then
         return;
      end if;

      --  Exclude calls to library subprograms. Container operations specify
      --  safe behavior when source and target coincide.

      if Is_Predefined_File_Name
           (Unit_File_Name (Get_Source_Unit (Sloc (Subp))))
      then
         return;
      end if;

      Form1 := First_Formal (Subp);
      Act1  := First_Actual (N);
      while Present (Form1) and then Present (Act1) loop
         if Ekind (Form1) /= E_In_Parameter then
            Form2 := First_Formal (Subp);
            Act2  := First_Actual (N);
            while Present (Form2) and then Present (Act2) loop
               if Form1 /= Form2
                 and then Ekind (Form2) /= E_Out_Parameter
                 and then
                   (Denotes_Same_Object (Act1, Act2)
                      or else
                    Denotes_Same_Prefix (Act1, Act2))
               then
                  --  Exclude generic types and guard against previous errors.

                  if Error_Posted (N)
                    or else No (Etype (Act1))
                    or else No (Etype (Act2))
                  then
                     null;

                  elsif Is_Generic_Type (Etype (Act1))
                          or else
                        Is_Generic_Type (Etype (Act2))
                  then
                     null;

                     --  If the actual is a function call in prefix notation,
                     --  there is no real overlap.

                  elsif Nkind (Act2) = N_Function_Call then
                     null;

                  --  If type is not by-copy we can assume that the aliasing is
                  --  intended.

                  elsif
                    Is_By_Reference_Type (Underlying_Type (Etype (Form1)))
                  then
                     null;

                  else
                     declare
                        Act  : Node_Id;
                        Form : Entity_Id;

                     begin
                        --  Find matching actual

                        Act  := First_Actual (N);
                        Form := First_Formal (Subp);
                        while Act /= Act2 loop
                           Next_Formal (Form);
                           Next_Actual (Act);
                        end loop;

                        if Is_Elementary_Type (Etype (Act1))
                          and then Ekind (Form2) = E_In_Parameter
                        then
                           null;  --  no real aliasing.

                        elsif Is_Elementary_Type (Etype (Act2))
                          and then Ekind (Form2) = E_In_Parameter
                        then
                           null;  --  ditto

                        --  If the call was written in prefix notation, and
                        --  thus its prefix before rewriting was a selected
                        --  component, count only visible actuals in the call.

                        elsif Is_Entity_Name (First_Actual (N))
                          and then Nkind (Original_Node (N)) = Nkind (N)
                          and then Nkind (Name (Original_Node (N))) =
                                                         N_Selected_Component
                          and then
                            Is_Entity_Name (Prefix (Name (Original_Node (N))))
                          and then
                            Entity (Prefix (Name (Original_Node (N)))) =
                              Entity (First_Actual (N))
                        then
                           if Act1 = First_Actual (N) then
                              Error_Msg_FE
                                ("`IN OUT` prefix overlaps with actual for&?",
                                 Act1, Form);
                           else
                              Error_Msg_FE
                                ("writable actual overlaps with actual for&?",
                                 Act1, Form);
                           end if;

                        else
                           Error_Msg_Node_2 := Form;
                           Error_Msg_FE
                             ("writable actual for & overlaps with"
                               & " actual for&?", Act1, Form1);
                        end if;
                     end;
                  end if;

                  return;
               end if;

               Next_Formal (Form2);
               Next_Actual (Act2);
            end loop;
         end if;

         Next_Formal (Form1);
         Next_Actual (Act1);
      end loop;
   end Warn_On_Overlapping_Actuals;

   ------------------------------
   -- Warn_On_Suspicious_Index --
   ------------------------------

   procedure Warn_On_Suspicious_Index (Name : Entity_Id; X : Node_Id) is

      Low_Bound : Uint;
      --  Set to lower bound for a suspicious type

      Ent : Entity_Id;
      --  Entity for array reference

      Typ : Entity_Id;
      --  Array type

      function Is_Suspicious_Type (Typ : Entity_Id) return Boolean;
      --  Tests to see if Typ is a type for which we may have a suspicious
      --  index, namely an unconstrained array type, whose lower bound is
      --  either zero or one. If so, True is returned, and Low_Bound is set
      --  to this lower bound. If not, False is returned, and Low_Bound is
      --  undefined on return.
      --
      --  For now, we limit this to standard string types, so any other
      --  unconstrained types return False. We may change our minds on this
      --  later on, but strings seem the most important case.

      procedure Test_Suspicious_Index;
      --  Test if index is of suspicious type and if so, generate warning

      ------------------------
      -- Is_Suspicious_Type --
      ------------------------

      function Is_Suspicious_Type (Typ : Entity_Id) return Boolean is
         LB : Node_Id;

      begin
         if Is_Array_Type (Typ)
           and then not Is_Constrained (Typ)
           and then Number_Dimensions (Typ) = 1
           and then (Root_Type (Typ) = Standard_String
                       or else
                     Root_Type (Typ) = Standard_Wide_String
                       or else
                     Root_Type (Typ) = Standard_Wide_Wide_String)
           and then not Has_Warnings_Off (Typ)
         then
            LB := Type_Low_Bound (Etype (First_Index (Typ)));

            if Compile_Time_Known_Value (LB) then
               Low_Bound := Expr_Value (LB);
               return Low_Bound = Uint_0 or else Low_Bound = Uint_1;
            end if;
         end if;

         return False;
      end Is_Suspicious_Type;

      ---------------------------
      -- Test_Suspicious_Index --
      ---------------------------

      procedure Test_Suspicious_Index is

         function Length_Reference (N : Node_Id) return Boolean;
         --  Check if node N is of the form Name'Length

         procedure Warn1;
         --  Generate first warning line

         ----------------------
         -- Length_Reference --
         ----------------------

         function Length_Reference (N : Node_Id) return Boolean is
            R : constant Node_Id := Original_Node (N);
         begin
            return
              Nkind (R) = N_Attribute_Reference
               and then Attribute_Name (R) = Name_Length
               and then Is_Entity_Name (Prefix (R))
               and then Entity (Prefix (R)) = Ent;
         end Length_Reference;

         -----------
         -- Warn1 --
         -----------

         procedure Warn1 is
         begin
            Error_Msg_Uint_1 := Low_Bound;
            Error_Msg_FE -- CODEFIX
              ("?index for& may assume lower bound of^", X, Ent);
         end Warn1;

      --  Start of processing for Test_Suspicious_Index

      begin
         --  Nothing to do if subscript does not come from source (we don't
         --  want to give garbage warnings on compiler expanded code, e.g. the
         --  loops generated for slice assignments. Such junk warnings would
         --  be placed on source constructs with no subscript in sight!)

         if not Comes_From_Source (Original_Node (X)) then
            return;
         end if;

         --  Case where subscript is a constant integer

         if Nkind (X) = N_Integer_Literal then
            Warn1;

            --  Case where original form of subscript is an integer literal

            if Nkind (Original_Node (X)) = N_Integer_Literal then
               if Intval (X) = Low_Bound then
                  Error_Msg_FE -- CODEFIX
                    ("\suggested replacement: `&''First`", X, Ent);
               else
                  Error_Msg_Uint_1 := Intval (X) - Low_Bound;
                  Error_Msg_FE -- CODEFIX
                    ("\suggested replacement: `&''First + ^`", X, Ent);

               end if;

            --  Case where original form of subscript is more complex

            else
               --  Build string X'First - 1 + expression where the expression
               --  is the original subscript. If the expression starts with "1
               --  + ", then the "- 1 + 1" is elided.

               Error_Msg_String (1 .. 13) := "'First - 1 + ";
               Error_Msg_Strlen := 13;

               declare
                  Sref : Source_Ptr := Sloc (First_Node (Original_Node (X)));
                  Tref : constant Source_Buffer_Ptr :=
                           Source_Text (Get_Source_File_Index (Sref));
                  --  Tref (Sref) is used to scan the subscript

                  Pctr : Natural;
                  --  Parentheses counter when scanning subscript

               begin
                  --  Tref (Sref) points to start of subscript

                  --  Elide - 1 if subscript starts with 1 +

                  if Tref (Sref .. Sref + 2) = "1 +" then
                     Error_Msg_Strlen := Error_Msg_Strlen - 6;
                     Sref := Sref + 2;

                  elsif Tref (Sref .. Sref + 1) = "1+" then
                     Error_Msg_Strlen := Error_Msg_Strlen - 6;
                     Sref := Sref + 1;
                  end if;

                  --  Now we will copy the subscript to the string buffer

                  Pctr := 0;
                  loop
                     --  Count parens, exit if terminating right paren. Note
                     --  check to ignore paren appearing as character literal.

                     if Tref (Sref + 1) = '''
                          and then
                        Tref (Sref - 1) = '''
                     then
                        null;
                     else
                        if Tref (Sref) = '(' then
                           Pctr := Pctr + 1;
                        elsif Tref (Sref) = ')' then
                           exit when Pctr = 0;
                           Pctr := Pctr - 1;
                        end if;
                     end if;

                     --  Done if terminating double dot (slice case)

                     exit when Pctr = 0
                       and then (Tref (Sref .. Sref + 1) = ".."
                                  or else
                                 Tref (Sref .. Sref + 2) = " ..");

                     --  Quit if we have hit EOF character, something wrong

                     if Tref (Sref) = EOF then
                        return;
                     end if;

                     --  String literals are too much of a pain to handle

                     if Tref (Sref) = '"' or else Tref (Sref) = '%' then
                        return;
                     end if;

                     --  If we have a 'Range reference, then this is a case
                     --  where we cannot easily give a replacement. Don't try!

                     if Tref (Sref .. Sref + 4) = "range"
                       and then Tref (Sref - 1) < 'A'
                       and then Tref (Sref + 5) < 'A'
                     then
                        return;
                     end if;

                     --  Else store next character

                     Error_Msg_Strlen := Error_Msg_Strlen + 1;
                     Error_Msg_String (Error_Msg_Strlen) := Tref (Sref);
                     Sref := Sref + 1;

                     --  If we get more than 40 characters then the expression
                     --  is too long to copy, or something has gone wrong. In
                     --  either case, just skip the attempt at a suggested fix.

                     if Error_Msg_Strlen > 40 then
                        return;
                     end if;
                  end loop;
               end;

               --  Replacement subscript is now in string buffer

               Error_Msg_FE -- CODEFIX
                 ("\suggested replacement: `&~`", Original_Node (X), Ent);
            end if;

         --  Case where subscript is of the form X'Length

         elsif Length_Reference (X) then
            Warn1;
            Error_Msg_Node_2 := Ent;
            Error_Msg_FE
              ("\suggest replacement of `&''Length` by `&''Last`",
               X, Ent);

         --  Case where subscript is of the form X'Length - expression

         elsif Nkind (X) = N_Op_Subtract
           and then Length_Reference (Left_Opnd (X))
         then
            Warn1;
            Error_Msg_Node_2 := Ent;
            Error_Msg_FE
              ("\suggest replacement of `&''Length` by `&''Last`",
               Left_Opnd (X), Ent);
         end if;
      end Test_Suspicious_Index;

   --  Start of processing for Warn_On_Suspicious_Index

   begin
      --  Only process if warnings activated

      if Warn_On_Assumed_Low_Bound then

         --  Test if array is simple entity name

         if Is_Entity_Name (Name) then

            --  Test if array is parameter of unconstrained string type

            Ent := Entity (Name);
            Typ := Etype (Ent);

            if Is_Formal (Ent)
              and then Is_Suspicious_Type (Typ)
              and then not Low_Bound_Tested (Ent)
            then
               Test_Suspicious_Index;
            end if;
         end if;
      end if;
   end Warn_On_Suspicious_Index;

   --------------------------------------
   -- Warn_On_Unassigned_Out_Parameter --
   --------------------------------------

   procedure Warn_On_Unassigned_Out_Parameter
     (Return_Node : Node_Id;
      Scope_Id    : Entity_Id)
   is
      Form  : Entity_Id;
      Form2 : Entity_Id;

   begin
      --  Ignore if procedure or return statement does not come from source

      if not Comes_From_Source (Scope_Id)
        or else not Comes_From_Source (Return_Node)
      then
         return;
      end if;

      --  Loop through formals

      Form := First_Formal (Scope_Id);
      while Present (Form) loop

         --  We are only interested in OUT parameters that come from source
         --  and are never set in the source, and furthermore only in scalars
         --  since non-scalars generate too many false positives.

         if Ekind (Form) = E_Out_Parameter
           and then Never_Set_In_Source_Check_Spec (Form)
           and then Is_Scalar_Type (Etype (Form))
           and then not Present (Unset_Reference (Form))
         then
            --  Before we issue the warning, an add ad hoc defence against the
            --  most common case of false positives with this warning which is
            --  the case where there is a Boolean OUT parameter that has been
            --  set, and whose meaning is "ignore the values of the other
            --  parameters". We can't of course reliably tell this case at
            --  compile time, but the following test kills a lot of false
            --  positives, without generating a significant number of false
            --  negatives (missed real warnings).

            Form2 := First_Formal (Scope_Id);
            while Present (Form2) loop
               if Ekind (Form2) = E_Out_Parameter
                 and then Root_Type (Etype (Form2)) = Standard_Boolean
                 and then not Never_Set_In_Source_Check_Spec (Form2)
               then
                  return;
               end if;

               Next_Formal (Form2);
            end loop;

            --  Here all conditions are met, record possible unset reference

            Set_Unset_Reference (Form, Return_Node);
         end if;

         Next_Formal (Form);
      end loop;
   end Warn_On_Unassigned_Out_Parameter;

   ---------------------------------
   -- Warn_On_Unreferenced_Entity --
   ---------------------------------

   procedure Warn_On_Unreferenced_Entity
     (Spec_E : Entity_Id;
      Body_E : Entity_Id := Empty)
   is
      E : Entity_Id := Spec_E;

   begin
      if not Referenced_Check_Spec (E)
        and then not Has_Pragma_Unreferenced_Check_Spec (E)
        and then not Warnings_Off_Check_Spec (E)
      then
         case Ekind (E) is
            when E_Variable =>

               --  Case of variable that is assigned but not read. We suppress
               --  the message if the variable is volatile, has an address
               --  clause, is aliased, or is a renaming, or is imported.

               if Referenced_As_LHS_Check_Spec (E)
                 and then No (Address_Clause (E))
                 and then not Is_Volatile (E)
               then
                  if Warn_On_Modified_Unread
                    and then not Is_Imported (E)
                    and then not Is_Aliased (E)
                    and then No (Renamed_Object (E))
                  then
                     if not Has_Pragma_Unmodified_Check_Spec (E) then
                        Error_Msg_N -- CODEFIX
                          ("?variable & is assigned but never read!", E);
                     end if;

                     Set_Last_Assignment (E, Empty);
                  end if;

               --  Normal case of neither assigned nor read (exclude variables
               --  referenced as out parameters, since we already generated
               --  appropriate warnings at the call point in this case).

               elsif not Referenced_As_Out_Parameter (E) then

                  --  We suppress the message for types for which a valid
                  --  pragma Unreferenced_Objects has been given, otherwise
                  --  we go ahead and give the message.

                  if not Has_Pragma_Unreferenced_Objects (Etype (E)) then

                     --  Distinguish renamed case in message

                     if Present (Renamed_Object (E))
                       and then Comes_From_Source (Renamed_Object (E))
                     then
                        Error_Msg_N -- CODEFIX
                          ("?renamed variable & is not referenced!", E);
                     else
                        Error_Msg_N -- CODEFIX
                          ("?variable & is not referenced!", E);
                     end if;
                  end if;
               end if;

            when E_Constant =>
               if Present (Renamed_Object (E))
                 and then Comes_From_Source (Renamed_Object (E))
               then
                  Error_Msg_N -- CODEFIX
                    ("?renamed constant & is not referenced!", E);
               else
                  Error_Msg_N -- CODEFIX
                    ("?constant & is not referenced!", E);
               end if;

            when E_In_Parameter     |
                 E_In_Out_Parameter =>

               --  Do not emit message for formals of a renaming, because
               --  they are never referenced explicitly.

               if Nkind (Original_Node (Unit_Declaration_Node (Scope (E))))
                 /= N_Subprogram_Renaming_Declaration
               then
                  --  Suppress this message for an IN OUT parameter of a
                  --  non-scalar type, since it is normal to have only an
                  --  assignment in such a case.

                  if Ekind (E) = E_In_Parameter
                    or else not Referenced_As_LHS_Check_Spec (E)
                    or else Is_Scalar_Type (Etype (E))
                  then
                     if Present (Body_E) then
                        E := Body_E;
                     end if;

                     if not Is_Trivial_Subprogram (Scope (E)) then
                        Error_Msg_NE -- CODEFIX
                          ("?formal parameter & is not referenced!",
                           E, Spec_E);
                     end if;
                  end if;
               end if;

            when E_Out_Parameter =>
               null;

            when E_Discriminant =>
               Error_Msg_N ("?discriminant & is not referenced!", E);

            when E_Named_Integer |
                 E_Named_Real    =>
               Error_Msg_N -- CODEFIX
                 ("?named number & is not referenced!", E);

            when Formal_Object_Kind =>
               Error_Msg_N -- CODEFIX
                 ("?formal object & is not referenced!", E);

            when E_Enumeration_Literal =>
               Error_Msg_N -- CODEFIX
                 ("?literal & is not referenced!", E);

            when E_Function =>
               Error_Msg_N -- CODEFIX
                 ("?function & is not referenced!", E);

            when E_Procedure =>
               Error_Msg_N -- CODEFIX
                 ("?procedure & is not referenced!", E);

            when E_Package =>
               Error_Msg_N -- CODEFIX
                 ("?package & is not referenced!", E);

            when E_Exception =>
               Error_Msg_N -- CODEFIX
                 ("?exception & is not referenced!", E);

            when E_Label =>
               Error_Msg_N -- CODEFIX
                 ("?label & is not referenced!", E);

            when E_Generic_Procedure =>
               Error_Msg_N -- CODEFIX
                 ("?generic procedure & is never instantiated!", E);

            when E_Generic_Function =>
               Error_Msg_N -- CODEFIX
                 ("?generic function & is never instantiated!", E);

            when Type_Kind =>
               Error_Msg_N -- CODEFIX
                 ("?type & is not referenced!", E);

            when others =>
               Error_Msg_N -- CODEFIX
                 ("?& is not referenced!", E);
         end case;

         --  Kill warnings on the entity on which the message has been posted

         Set_Warnings_Off (E);
      end if;
   end Warn_On_Unreferenced_Entity;

   --------------------------------
   -- Warn_On_Useless_Assignment --
   --------------------------------

   procedure Warn_On_Useless_Assignment
     (Ent : Entity_Id;
      N   : Node_Id := Empty)
   is
      P    : Node_Id;
      X    : Node_Id;

      function Check_Ref (N : Node_Id) return Traverse_Result;
      --  Used to instantiate Traverse_Func. Returns Abandon if a reference to
      --  the entity in question is found.

      function Test_No_Refs is new Traverse_Func (Check_Ref);

      ---------------
      -- Check_Ref --
      ---------------

      function Check_Ref (N : Node_Id) return Traverse_Result is
      begin
         --  Check reference to our identifier. We use name equality here
         --  because the exception handlers have not yet been analyzed. This
         --  is not quite right, but it really does not matter that we fail
         --  to output the warning in some obscure cases of name clashes.

         if Nkind (N) = N_Identifier
           and then Chars (N) = Chars (Ent)
         then
            return Abandon;
         else
            return OK;
         end if;
      end Check_Ref;

   --  Start of processing for Warn_On_Useless_Assignment

   begin
      --  Check if this is a case we want to warn on, a scalar or access
      --  variable with the last assignment field set, with warnings enabled,
      --  and which is not imported or exported. We also check that it is OK
      --  to capture the value. We are not going to capture any value, but
      --  the warning message depends on the same kind of conditions.

      if Is_Assignable (Ent)
        and then not Is_Return_Object (Ent)
        and then Present (Last_Assignment (Ent))
        and then not Is_Imported (Ent)
        and then not Is_Exported (Ent)
        and then Safe_To_Capture_Value (N, Ent)
        and then not Has_Pragma_Unreferenced_Check_Spec (Ent)
      then
         --  Before we issue the message, check covering exception handlers.
         --  Search up tree for enclosing statement sequences and handlers.

         P := Parent (Last_Assignment (Ent));
         while Present (P) loop

            --  Something is really wrong if we don't find a handled statement
            --  sequence, so just suppress the warning.

            if No (P) then
               Set_Last_Assignment (Ent, Empty);
               return;

            --  When we hit a package/subprogram body, issue warning and exit

            elsif Nkind (P) = N_Subprogram_Body
              or else Nkind (P) = N_Package_Body
            then
               --  Case of assigned value never referenced

               if No (N) then

                  --  Don't give this for OUT and IN OUT formals, since
                  --  clearly caller may reference the assigned value. Also
                  --  never give such warnings for internal variables.

                  if Ekind (Ent) = E_Variable
                    and then not Is_Internal_Name (Chars (Ent))
                  then
                     if Referenced_As_Out_Parameter (Ent) then
                        Error_Msg_NE
                          ("?& modified by call, but value never referenced",
                           Last_Assignment (Ent), Ent);
                     else
                        Error_Msg_NE -- CODEFIX
                          ("?useless assignment to&, value never referenced!",
                           Last_Assignment (Ent), Ent);
                     end if;
                  end if;

               --  Case of assigned value overwritten

               else
                  Error_Msg_Sloc := Sloc (N);

                  if Referenced_As_Out_Parameter (Ent) then
                     Error_Msg_NE
                       ("?& modified by call, but value overwritten #!",
                        Last_Assignment (Ent), Ent);
                  else
                     Error_Msg_NE -- CODEFIX
                       ("?useless assignment to&, value overwritten #!",
                        Last_Assignment (Ent), Ent);
                  end if;
               end if;

               --  Clear last assignment indication and we are done

               Set_Last_Assignment (Ent, Empty);
               return;

            --  Enclosing handled sequence of statements

            elsif Nkind (P) = N_Handled_Sequence_Of_Statements then

               --  Check exception handlers present

               if Present (Exception_Handlers (P)) then

                  --  If we are not at the top level, we regard an inner
                  --  exception handler as a decisive indicator that we should
                  --  not generate the warning, since the variable in question
                  --  may be accessed after an exception in the outer block.

                  if Nkind (Parent (P)) /= N_Subprogram_Body
                    and then Nkind (Parent (P)) /= N_Package_Body
                  then
                     Set_Last_Assignment (Ent, Empty);
                     return;

                     --  Otherwise we are at the outer level. An exception
                     --  handler is significant only if it references the
                     --  variable in question, or if the entity in question
                     --  is an OUT or IN OUT parameter, which which case
                     --  the caller can reference it after the exception
                     --  hanlder completes

                  else
                     if Is_Formal (Ent) then
                        Set_Last_Assignment (Ent, Empty);
                        return;

                     else
                        X := First (Exception_Handlers (P));
                        while Present (X) loop
                           if Test_No_Refs (X) = Abandon then
                              Set_Last_Assignment (Ent, Empty);
                              return;
                           end if;

                           X := Next (X);
                        end loop;
                     end if;
                  end if;
               end if;
            end if;

            P := Parent (P);
         end loop;
      end if;
   end Warn_On_Useless_Assignment;

   ---------------------------------
   -- Warn_On_Useless_Assignments --
   ---------------------------------

   procedure Warn_On_Useless_Assignments (E : Entity_Id) is
      Ent : Entity_Id;
   begin
      if Warn_On_Modified_Unread
        and then In_Extended_Main_Source_Unit (E)
      then
         Ent := First_Entity (E);
         while Present (Ent) loop
            Warn_On_Useless_Assignment (Ent);
            Next_Entity (Ent);
         end loop;
      end if;
   end Warn_On_Useless_Assignments;

   -----------------------------
   -- Warnings_Off_Check_Spec --
   -----------------------------

   function Warnings_Off_Check_Spec (E : Entity_Id) return Boolean is
   begin
      if Is_Formal (E) and then Present (Spec_Entity (E)) then

         --  Note: use of OR here instead of OR ELSE is deliberate, we want
         --  to mess with flags on both entities.

         return Has_Warnings_Off (E)
                  or
                Has_Warnings_Off (Spec_Entity (E));

      else
         return Has_Warnings_Off (E);
      end if;
   end Warnings_Off_Check_Spec;

end Sem_Warn;
