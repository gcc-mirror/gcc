------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                             E X P A N D E R                              --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2007, Free Software Foundation, Inc.         --
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

with Atree;     use Atree;
with Debug_A;   use Debug_A;
with Errout;    use Errout;
with Exp_Aggr;  use Exp_Aggr;
with Exp_Attr;  use Exp_Attr;
with Exp_Ch2;   use Exp_Ch2;
with Exp_Ch3;   use Exp_Ch3;
with Exp_Ch4;   use Exp_Ch4;
with Exp_Ch5;   use Exp_Ch5;
with Exp_Ch6;   use Exp_Ch6;
with Exp_Ch7;   use Exp_Ch7;
with Exp_Ch8;   use Exp_Ch8;
with Exp_Ch9;   use Exp_Ch9;
with Exp_Ch11;  use Exp_Ch11;
with Exp_Ch12;  use Exp_Ch12;
with Exp_Ch13;  use Exp_Ch13;
with Exp_Prag;  use Exp_Prag;
with Opt;       use Opt;
with Rtsfind;   use Rtsfind;
with Sem;       use Sem;
with Sem_Ch8;   use Sem_Ch8;
with Sem_Util;  use Sem_Util;
with Sinfo;     use Sinfo;
with Table;

package body Expander is

   ----------------
   -- Local Data --
   ----------------

   --  The following table is used to save values of the Expander_Active flag
   --  when they are saved by Expander_Mode_Save_And_Set. We use an extendible
   --  table (which is a bit of overkill) because it is easier than figuring
   --  out a maximum value or bothering with range checks!

   package Expander_Flags is new Table.Table (
     Table_Component_Type => Boolean,
     Table_Index_Type     => Int,
     Table_Low_Bound      => 0,
     Table_Initial        => 32,
     Table_Increment      => 200,
     Table_Name           => "Expander_Flags");

   ------------
   -- Expand --
   ------------

   procedure Expand (N : Node_Id) is
   begin
      --  If we were analyzing a default expression the Full_Analysis flag must
      --  be off. If we are in expansion mode then we must be performing a full
      --  analysis. If we are analyzing a generic then Expansion must be off.

      pragma Assert
        (not (Full_Analysis and then In_Default_Expression)
          and then (Full_Analysis or else not Expander_Active)
          and then not (Inside_A_Generic and then Expander_Active));

      --  There are three reasons for the Expander_Active flag to be false
      --
      --  The first is when are not generating code. In this mode the
      --  Full_Analysis flag indicates whether we are performing a complete
      --  analysis, in which case Full_Analysis = True or a pre-analysis in
      --  which case Full_Analysis = False. See the spec of Sem for more
      --  info on this.
      --
      --  The second reason for the Expander_Active flag to be False is that
      --  we are performing a pre-analysis. During pre-analysis all expansion
      --  activity is turned off to make sure nodes are semantically decorated
      --  but no extra nodes are generated. This is for instance needed for
      --  the first pass of aggregate semantic processing. Note that in this
      --  case the Full_Analysis flag is set to False because the node will
      --  subsequently be re-analyzed with expansion on (see the spec of sem).

      --  Finally, expansion is turned off in a regular compilation if there
      --  are serious errors. In that case there will be no further expansion,
      --  but one cleanup action may be required: if a transient scope was
      --  created (e.g. for a function that returns an unconstrained type) the
      --  scope may still be on the stack, and must be removed explicitly,
      --  given that the expansion actions that would normally process it will
      --  not take place. This prevents cascaded errors due to stack mismatch.

      if not Expander_Active then
         Set_Analyzed (N, Full_Analysis);

         if Serious_Errors_Detected > 0
           and then Scope_Is_Transient
         then
            Scope_Stack.Table
             (Scope_Stack.Last).Actions_To_Be_Wrapped_Before := No_List;
            Scope_Stack.Table
             (Scope_Stack.Last).Actions_To_Be_Wrapped_After  := No_List;

            Pop_Scope;
         end if;

         return;

      else
         Debug_A_Entry ("expanding  ", N);

         --  Processing depends on node kind. For full details on the expansion
         --  activity required in each case, see bodies of corresponding expand
         --  routines.

         begin
            case Nkind (N) is

               when N_Abort_Statement =>
                  Expand_N_Abort_Statement (N);

               when N_Accept_Statement =>
                  Expand_N_Accept_Statement (N);

               when N_Aggregate =>
                  Expand_N_Aggregate (N);

               when N_Allocator =>
                  Expand_N_Allocator (N);

               when N_And_Then =>
                  Expand_N_And_Then (N);

               when N_Assignment_Statement =>
                  Expand_N_Assignment_Statement (N);

               when N_Asynchronous_Select =>
                  Expand_N_Asynchronous_Select (N);

               when N_Attribute_Definition_Clause =>
                  Expand_N_Attribute_Definition_Clause (N);

               when N_Attribute_Reference =>
                  Expand_N_Attribute_Reference (N);

               when N_Block_Statement =>
                  Expand_N_Block_Statement (N);

               when N_Case_Statement =>
                  Expand_N_Case_Statement (N);

               when N_Conditional_Entry_Call =>
                  Expand_N_Conditional_Entry_Call (N);

               when N_Conditional_Expression =>
                  Expand_N_Conditional_Expression (N);

               when N_Delay_Relative_Statement =>
                  Expand_N_Delay_Relative_Statement (N);

               when N_Delay_Until_Statement =>
                  Expand_N_Delay_Until_Statement (N);

               when N_Entry_Body =>
                  Expand_N_Entry_Body (N);

               when N_Entry_Call_Statement =>
                  Expand_N_Entry_Call_Statement (N);

               when N_Entry_Declaration =>
                  Expand_N_Entry_Declaration (N);

               when N_Exception_Declaration =>
                  Expand_N_Exception_Declaration (N);

               when N_Exception_Renaming_Declaration =>
                  Expand_N_Exception_Renaming_Declaration (N);

               when N_Exit_Statement =>
                  Expand_N_Exit_Statement (N);

               when N_Expanded_Name =>
                  Expand_N_Expanded_Name (N);

               when N_Explicit_Dereference =>
                  Expand_N_Explicit_Dereference (N);

               when N_Extended_Return_Statement =>
                  Expand_N_Extended_Return_Statement (N);

               when N_Extension_Aggregate =>
                  Expand_N_Extension_Aggregate (N);

               when N_Freeze_Entity =>
                  Expand_N_Freeze_Entity (N);

               when N_Full_Type_Declaration =>
                  Expand_N_Full_Type_Declaration (N);

               when N_Function_Call =>
                  Expand_N_Function_Call (N);

               when N_Generic_Instantiation =>
                  Expand_N_Generic_Instantiation (N);

               when N_Goto_Statement =>
                  Expand_N_Goto_Statement (N);

               when N_Handled_Sequence_Of_Statements =>
                  Expand_N_Handled_Sequence_Of_Statements (N);

               when N_Identifier =>
                  Expand_N_Identifier (N);

               when N_Indexed_Component =>
                  Expand_N_Indexed_Component (N);

               when N_If_Statement =>
                  Expand_N_If_Statement (N);

               when N_In =>
                  Expand_N_In (N);

               when N_Loop_Statement =>
                  Expand_N_Loop_Statement (N);

               when N_Not_In =>
                  Expand_N_Not_In (N);

               when N_Null =>
                  Expand_N_Null (N);

               when N_Object_Declaration =>
                  Expand_N_Object_Declaration (N);

               when N_Object_Renaming_Declaration =>
                  Expand_N_Object_Renaming_Declaration (N);

               when N_Op_Add =>
                  Expand_N_Op_Add (N);

               when N_Op_Abs =>
                  Expand_N_Op_Abs (N);

               when N_Op_And =>
                  Expand_N_Op_And (N);

               when N_Op_Concat =>
                  Expand_N_Op_Concat (N);

               when N_Op_Divide =>
                  Expand_N_Op_Divide (N);

               when N_Op_Eq =>
                  Expand_N_Op_Eq (N);

               when N_Op_Expon =>
                  Expand_N_Op_Expon (N);

               when N_Op_Ge =>
                  Expand_N_Op_Ge (N);

               when N_Op_Gt =>
                  Expand_N_Op_Gt (N);

               when N_Op_Le =>
                  Expand_N_Op_Le (N);

               when N_Op_Lt =>
                  Expand_N_Op_Lt (N);

               when N_Op_Minus =>
                  Expand_N_Op_Minus (N);

               when N_Op_Mod =>
                  Expand_N_Op_Mod (N);

               when N_Op_Multiply =>
                  Expand_N_Op_Multiply (N);

               when N_Op_Ne =>
                  Expand_N_Op_Ne (N);

               when N_Op_Not =>
                  Expand_N_Op_Not (N);

               when N_Op_Or =>
                  Expand_N_Op_Or (N);

               when N_Op_Plus =>
                  Expand_N_Op_Plus (N);

               when N_Op_Rem =>
                  Expand_N_Op_Rem (N);

               when N_Op_Rotate_Left =>
                  Expand_N_Op_Rotate_Left (N);

               when N_Op_Rotate_Right =>
                  Expand_N_Op_Rotate_Right (N);

               when N_Op_Shift_Left =>
                  Expand_N_Op_Shift_Left (N);

               when N_Op_Shift_Right =>
                  Expand_N_Op_Shift_Right (N);

               when N_Op_Shift_Right_Arithmetic =>
                  Expand_N_Op_Shift_Right_Arithmetic (N);

               when N_Op_Subtract =>
                  Expand_N_Op_Subtract (N);

               when N_Op_Xor =>
                  Expand_N_Op_Xor (N);

               when N_Or_Else =>
                  Expand_N_Or_Else (N);

               when N_Package_Body =>
                  Expand_N_Package_Body (N);

               when N_Package_Declaration =>
                  Expand_N_Package_Declaration (N);

               when N_Package_Renaming_Declaration =>
                  Expand_N_Package_Renaming_Declaration (N);

               when N_Subprogram_Renaming_Declaration =>
                  Expand_N_Subprogram_Renaming_Declaration (N);

               when N_Pragma =>
                  Expand_N_Pragma (N);

               when N_Procedure_Call_Statement =>
                  Expand_N_Procedure_Call_Statement (N);

               when N_Protected_Type_Declaration =>
                  Expand_N_Protected_Type_Declaration (N);

               when N_Protected_Body =>
                  Expand_N_Protected_Body (N);

               when N_Qualified_Expression =>
                  Expand_N_Qualified_Expression (N);

               when N_Raise_Statement =>
                  Expand_N_Raise_Statement (N);

               when N_Raise_Constraint_Error =>
                  Expand_N_Raise_Constraint_Error (N);

               when N_Raise_Program_Error =>
                  Expand_N_Raise_Program_Error (N);

               when N_Raise_Storage_Error =>
                  Expand_N_Raise_Storage_Error (N);

               when N_Real_Literal =>
                  Expand_N_Real_Literal (N);

               when N_Record_Representation_Clause =>
                  Expand_N_Record_Representation_Clause (N);

               when N_Requeue_Statement =>
                  Expand_N_Requeue_Statement (N);

               when N_Simple_Return_Statement =>
                  Expand_N_Simple_Return_Statement (N);

               when N_Selected_Component =>
                  Expand_N_Selected_Component (N);

               when N_Selective_Accept =>
                  Expand_N_Selective_Accept (N);

               when N_Single_Task_Declaration =>
                  Expand_N_Single_Task_Declaration (N);

               when N_Slice =>
                  Expand_N_Slice (N);

               when N_Subtype_Indication =>
                  Expand_N_Subtype_Indication (N);

               when N_Subprogram_Body =>
                  Expand_N_Subprogram_Body (N);

               when N_Subprogram_Body_Stub =>
                  Expand_N_Subprogram_Body_Stub (N);

               when N_Subprogram_Declaration =>
                  Expand_N_Subprogram_Declaration (N);

               when N_Subprogram_Info =>
                  Expand_N_Subprogram_Info (N);

               when N_Task_Body =>
                  Expand_N_Task_Body (N);

               when N_Task_Type_Declaration =>
                  Expand_N_Task_Type_Declaration (N);

               when N_Timed_Entry_Call =>
                  Expand_N_Timed_Entry_Call (N);

               when N_Type_Conversion =>
                  Expand_N_Type_Conversion (N);

               when N_Unchecked_Expression =>
                  Expand_N_Unchecked_Expression (N);

               when N_Unchecked_Type_Conversion =>
                  Expand_N_Unchecked_Type_Conversion (N);

               when N_Variant_Part =>
                  Expand_N_Variant_Part (N);

               --  For all other node kinds, no expansion activity is required

               when others => null;

            end case;

         exception
            when RE_Not_Available =>
               return;
         end;

         --  Set result as analyzed and then do a possible transient wrap. The
         --  transient wrap must be done after the Analyzed flag is set on, so
         --  that we do not get a recursive attempt to expand the node N.

         Set_Analyzed (N);

         --  Deal with transient scopes

         if Scope_Is_Transient and then N = Node_To_Be_Wrapped then

            case Nkind (N) is
               when N_Statement_Other_Than_Procedure_Call |
                    N_Procedure_Call_Statement            =>
                  Wrap_Transient_Statement (N);

               when N_Object_Declaration          |
                    N_Object_Renaming_Declaration |
                    N_Subtype_Declaration         =>
                  Wrap_Transient_Declaration (N);

               when others => Wrap_Transient_Expression (N);
            end case;
         end if;

         Debug_A_Exit ("expanding  ", N, "  (done)");
      end if;

   end Expand;

   ---------------------------
   -- Expander_Mode_Restore --
   ---------------------------

   procedure Expander_Mode_Restore is
   begin
      --  Not active (has no effect) in ASIS mode (see comments in spec of
      --  Expander_Mode_Save_And_Set).

      if ASIS_Mode then
         return;
      end if;

      --  Otherwise restore the flag

      Expander_Active := Expander_Flags.Table (Expander_Flags.Last);
      Expander_Flags.Decrement_Last;

      --  Keep expander off if serious errors detected. In this case we do not
      --  need expansion, and continued expansion may cause cascaded errors or
      --  compiler bombs.

      if Serious_Errors_Detected /= 0 then
         Expander_Active := False;
      end if;
   end Expander_Mode_Restore;

   --------------------------------
   -- Expander_Mode_Save_And_Set --
   --------------------------------

   procedure Expander_Mode_Save_And_Set (Status : Boolean) is
   begin
      --  Not active (has no effect) in ASIS mode (see comments in spec of
      --  Expander_Mode_Save_And_Set).

      if ASIS_Mode then
         return;
      end if;

      --  Otherwise save and set the flag

      Expander_Flags.Increment_Last;
      Expander_Flags.Table (Expander_Flags.Last) := Expander_Active;
      Expander_Active := Status;
   end Expander_Mode_Save_And_Set;

end Expander;
