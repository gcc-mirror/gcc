------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--             D I A G N O S T I C S . C O N S T R U C T O R S              --
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

with Sinfo.Nodes;                   use Sinfo.Nodes;
with Diagnostics.Utils;             use Diagnostics.Utils;

package body Diagnostics.Constructors is

   -----------------------------------------------
   -- Make_Default_Iterator_Not_Primitive_Error --
   -----------------------------------------------

   function Make_Default_Iterator_Not_Primitive_Error
     (Expr : Node_Id;
      Subp : Entity_Id) return Diagnostic_Type
   is
   begin
      return
        Make_Diagnostic
          (Msg       => "improper function for default iterator",
           Location  => Primary_Labeled_Span (Expr),
           Id        => GNAT0001,
           Kind      => Diagnostics.Error,
           Sub_Diags =>
             (1 =>
                Continuation
                  (Msg =>
                     "default iterator defined " &
                     Sloc_To_String (Subp, Sloc (Expr)) &
                     " must be a primitive function",
                   Locations =>
                     (1 => Primary_Labeled_Span (Subp)))));
   end Make_Default_Iterator_Not_Primitive_Error;

   -------------------------------------------------
   -- Record_Default_Iterator_Not_Primitive_Error --
   -------------------------------------------------

   procedure Record_Default_Iterator_Not_Primitive_Error
     (Expr : Node_Id;
      Subp : Entity_Id)
   is
   begin
      Record_Diagnostic
        (Make_Default_Iterator_Not_Primitive_Error (Expr, Subp));
   end Record_Default_Iterator_Not_Primitive_Error;

   ---------------------------------------------------
   -- Make_Invalid_Operand_Types_For_Operator_Error --
   ---------------------------------------------------

   function Make_Invalid_Operand_Types_For_Operator_Error
     (Op     : Node_Id;
      L      : Node_Id;
      L_Type : Node_Id;
      R      : Node_Id;
      R_Type : Node_Id) return Diagnostic_Type
   is
   begin
      return
        Make_Diagnostic
          (Msg      => "invalid operand types for operator " & To_Name (Op),
           Location => Primary_Labeled_Span (Op),
           Id       => GNAT0002,
           Kind     => Diagnostics.Error,
           Spans    =>
             (1 =>
                (Secondary_Labeled_Span
                   (N     => L,
                    Label => To_Type_Name (L_Type))),
              2 =>
                Secondary_Labeled_Span
                  (N     => R,
                   Label => To_Type_Name (R_Type))));
   end Make_Invalid_Operand_Types_For_Operator_Error;

   -----------------------------------------------------
   -- Record_Invalid_Operand_Types_For_Operator_Error --
   -----------------------------------------------------

   procedure Record_Invalid_Operand_Types_For_Operator_Error
     (Op     : Node_Id;
      L      : Node_Id;
      L_Type : Node_Id;
      R      : Node_Id;
      R_Type : Node_Id)
   is

   begin
      Record_Diagnostic
        (Make_Invalid_Operand_Types_For_Operator_Error
           (Op, L, L_Type, R, R_Type));
   end Record_Invalid_Operand_Types_For_Operator_Error;

   ---------------------------------------------------------
   -- Make_Invalid_Operand_Types_For_Operator_L_Int_Error --
   ---------------------------------------------------------

   function Make_Invalid_Operand_Types_For_Operator_L_Int_Error
     (Op     : Node_Id;
      L      : Node_Id;
      L_Type : Node_Id;
      R      : Node_Id;
      R_Type : Node_Id) return Diagnostic_Type
   is
   begin
      return
        Make_Diagnostic
          (Msg      => "invalid operand types for operator " & To_Name (Op),
           Location => Primary_Labeled_Span (Op),
           Id       => GNAT0003,
           Kind     => Diagnostics.Error,
           Spans    =>
             (1 =>
                (Secondary_Labeled_Span
                   (N     => L,
                    Label =>
                      "left operand has type " &
                      To_Name (L_Type))),
              2 =>
                Secondary_Labeled_Span
                  (N     => R,
                   Label =>
                     "right operand has type " &
                     To_Name (R_Type))),
            Sub_Diags =>
              (1 => Suggestion (Msg => "Convert left operand to ""Integer""")
            )
         );
   end Make_Invalid_Operand_Types_For_Operator_L_Int_Error;

   -----------------------------------------------------------
   -- Record_Invalid_Operand_Types_For_Operator_L_Int_Error --
   -----------------------------------------------------------

   procedure Record_Invalid_Operand_Types_For_Operator_L_Int_Error
     (Op     : Node_Id;
      L      : Node_Id;
      L_Type : Node_Id;
      R      : Node_Id;
      R_Type : Node_Id)
   is

   begin
      Record_Diagnostic
        (Make_Invalid_Operand_Types_For_Operator_L_Int_Error
           (Op, L, L_Type, R, R_Type));
   end Record_Invalid_Operand_Types_For_Operator_L_Int_Error;

   ---------------------------------------------------------
   -- Make_Invalid_Operand_Types_For_Operator_R_Int_Error --
   ---------------------------------------------------------

   function Make_Invalid_Operand_Types_For_Operator_R_Int_Error
     (Op     : Node_Id;
      L      : Node_Id;
      L_Type : Node_Id;
      R      : Node_Id;
      R_Type : Node_Id) return Diagnostic_Type
   is
   begin
      return
        Make_Diagnostic
          (Msg      => "invalid operand types for operator " & To_Name (Op),
           Location => Primary_Labeled_Span (Op),
           Id       => GNAT0004,
           Kind     => Diagnostics.Error,
           Spans    =>
             (1 =>
                Secondary_Labeled_Span
                   (N     => L,
                    Label =>
                      "left operand has type " &
                      To_Name (L_Type)),
              2 =>
                Secondary_Labeled_Span
                  (N     => R,
                   Label =>
                     "right operand has type " &
                     To_Name (R_Type))),
            Sub_Diags =>
              (1 => Suggestion (Msg => "Convert right operand to ""Integer""")
            )
         );
   end Make_Invalid_Operand_Types_For_Operator_R_Int_Error;

   -----------------------------------------------------------
   -- Record_Invalid_Operand_Types_For_Operator_R_Int_Error --
   -----------------------------------------------------------

   procedure Record_Invalid_Operand_Types_For_Operator_R_Int_Error
     (Op     : Node_Id;
      L      : Node_Id;
      L_Type : Node_Id;
      R      : Node_Id;
      R_Type : Node_Id)
   is

   begin
      Record_Diagnostic
        (Make_Invalid_Operand_Types_For_Operator_R_Int_Error
           (Op, L, L_Type, R, R_Type));
   end Record_Invalid_Operand_Types_For_Operator_R_Int_Error;

   ---------------------------------------------------------
   -- Make_Invalid_Operand_Types_For_Operator_L_Acc_Error --
   ---------------------------------------------------------

   function Make_Invalid_Operand_Types_For_Operator_L_Acc_Error
     (Op     : Node_Id;
      L      : Node_Id) return Diagnostic_Type
   is

   begin
      return
        Make_Diagnostic
          (Msg      => "invalid operand types for operator " & To_Name (Op),
           Location => Primary_Labeled_Span (Op),
           Id       => GNAT0005,
           Kind     => Diagnostics.Error,
           Spans    =>
             (1 =>
                Secondary_Labeled_Span
                   (N     => L,
                    Label =>
                      "left operand is access type ")
            )
         );
   end Make_Invalid_Operand_Types_For_Operator_L_Acc_Error;

   -----------------------------------------------------------
   -- Record_Invalid_Operand_Types_For_Operator_L_Acc_Error --
   -----------------------------------------------------------

   procedure Record_Invalid_Operand_Types_For_Operator_L_Acc_Error
     (Op     : Node_Id;
      L      : Node_Id)
   is
   begin
      Record_Diagnostic
        (Make_Invalid_Operand_Types_For_Operator_R_Acc_Error
           (Op, L));
   end Record_Invalid_Operand_Types_For_Operator_L_Acc_Error;

   ---------------------------------------------------------
   -- Make_Invalid_Operand_Types_For_Operator_L_Acc_Error --
   ---------------------------------------------------------

   function Make_Invalid_Operand_Types_For_Operator_R_Acc_Error
     (Op     : Node_Id;
      R      : Node_Id) return Diagnostic_Type
   is

   begin
      return
        Make_Diagnostic
          (Msg      => "invalid operand types for operator " & To_Name (Op),
           Location => Primary_Labeled_Span (Op),
           Id       => GNAT0006,
           Kind     => Diagnostics.Error,
           Spans    =>
             (1 =>
                Secondary_Labeled_Span
                   (N     => R,
                    Label =>
                      "right operand is access type ")
            )
         );
   end Make_Invalid_Operand_Types_For_Operator_R_Acc_Error;

   -----------------------------------------------------------
   -- Record_Invalid_Operand_Types_For_Operator_R_Acc_Error --
   -----------------------------------------------------------

   procedure Record_Invalid_Operand_Types_For_Operator_R_Acc_Error
     (Op     : Node_Id;
      R      : Node_Id)
   is
   begin
      Record_Diagnostic
        (Make_Invalid_Operand_Types_For_Operator_R_Acc_Error
           (Op, R));
   end Record_Invalid_Operand_Types_For_Operator_R_Acc_Error;

   -----------------------------------------------------------
   -- Make_Invalid_Operand_Types_For_Operator_General_Error --
   -----------------------------------------------------------

   function Make_Invalid_Operand_Types_For_Operator_General_Error
     (Op : Node_Id) return Diagnostic_Type
   is

   begin
      return
        Make_Diagnostic
          (Msg      => "invalid operand types for operator " & To_Name (Op),
           Location => Primary_Labeled_Span (Op),
           Id       => GNAT0007,
           Kind     => Diagnostics.Error
         );
   end Make_Invalid_Operand_Types_For_Operator_General_Error;

   -------------------------------------------------------------
   -- Record_Invalid_Operand_Types_For_Operator_General_Error --
   -------------------------------------------------------------

   procedure Record_Invalid_Operand_Types_For_Operator_General_Error
     (Op     : Node_Id)
   is
   begin
      Record_Diagnostic
        (Make_Invalid_Operand_Types_For_Operator_General_Error (Op));
   end Record_Invalid_Operand_Types_For_Operator_General_Error;

   --------------------------------------------------
   -- Make_Pragma_No_Effect_With_Lock_Free_Warning --
   --------------------------------------------------

   function Make_Pragma_No_Effect_With_Lock_Free_Warning
     (Pragma_Node    : Node_Id; Pragma_Name : Name_Id;
      Lock_Free_Node : Node_Id; Lock_Free_Range : Node_Id)
      return Diagnostic_Type
   is
   begin
      return
        Make_Diagnostic
          (Msg      =>
             "pragma " & '"' & Get_Name_String (Pragma_Name) & '"' &
             " for " & To_Name (Lock_Free_Node) &
             " has no effect when Lock_Free given",
           Location => Primary_Labeled_Span (Pragma_Node, "No effect"),
           Id       => GNAT0008,
           Kind     => Diagnostics.Warning,
           Spans    =>
             (1 =>
                Labeled_Span
                  (Span       => To_Full_Span (Lock_Free_Range),
                   Label      => "Lock_Free in effect here",
                   Is_Primary => False,
                   Is_Region  => True)));
   end Make_Pragma_No_Effect_With_Lock_Free_Warning;

   --------------------------------------------
   -- Record_Pragma_No_Effect_With_Lock_Free --
   --------------------------------------------

   procedure Record_Pragma_No_Effect_With_Lock_Free_Warning
     (Pragma_Node : Node_Id;
      Pragma_Name : Name_Id;
      Lock_Free_Node : Node_Id;
      Lock_Free_Range : Node_Id)
   is
   begin
      Record_Diagnostic
        (Make_Pragma_No_Effect_With_Lock_Free_Warning
           (Pragma_Node, Pragma_Name, Lock_Free_Node, Lock_Free_Range));
   end Record_Pragma_No_Effect_With_Lock_Free_Warning;

   ----------------------------------
   -- Make_End_Loop_Expected_Error --
   ----------------------------------

   function Make_End_Loop_Expected_Error
     (End_Loc   : Source_Span;
      Start_Loc : Source_Ptr) return Diagnostic_Type
   is
   begin
      return
        Make_Diagnostic
          (Msg      =>
             """end loop;"" expected for ""loop"" " &
             Sloc_To_String (Start_Loc, End_Loc.Ptr),
           Location => Primary_Labeled_Span (End_Loc),
           Id       => GNAT0009,
           Kind     => Diagnostics.Error,
           Spans    => (1 => Secondary_Labeled_Span (To_Span (Start_Loc))),
           Fixes    =>
             (1 =>
                Fix
                  (Description   => "Replace with 'end loop;'",
                   Edits         =>
                     (1 => Edit (Text => "end loop;", Span => End_Loc)),
                   Applicability => Legal)));
   end Make_End_Loop_Expected_Error;

   ------------------------------------
   -- Record_End_Loop_Expected_Error --
   ------------------------------------

   procedure Record_End_Loop_Expected_Error
     (End_Loc : Source_Span; Start_Loc : Source_Ptr)
   is
   begin
      Record_Diagnostic (Make_End_Loop_Expected_Error (End_Loc, Start_Loc));
   end Record_End_Loop_Expected_Error;

   ----------------------------------------
   -- Make_Representation_Too_Late_Error --
   ----------------------------------------

   function Make_Representation_Too_Late_Error
     (Rep    : Node_Id;
      Freeze : Node_Id;
      Def    : Node_Id)
      return Diagnostic_Type
   is
   begin
      return
        Make_Diagnostic
          (Msg       =>
             "record representation cannot be specified" &
             " after the type is frozen",
           Location  =>
             Primary_Labeled_Span
               (N     => Rep,
                Label => "record representation clause specified here"),
           Id        => GNAT0010,
           Kind      => Error,
           Spans     =>
             (1 =>
                Secondary_Labeled_Span
                  (N     => Freeze,
                   Label =>
                     "Type " & To_Name (Def) & " is frozen here"),
              2 =>
                Secondary_Labeled_Span
                  (N     => Def,
                   Label =>
                     "Type " & To_Name (Def) & " is declared here")),
           Sub_Diags =>
             (1 =>
                Suggestion
                  (Msg  =>
                     "move the record representation clause" &
                     " before the freeze point " &
                     Sloc_To_String (Sloc (Freeze), Sloc (Rep)))));
   end Make_Representation_Too_Late_Error;

   ------------------------------------------
   -- Record_Representation_Too_Late_Error --
   ------------------------------------------

   procedure Record_Representation_Too_Late_Error
     (Rep    : Node_Id;
      Freeze : Node_Id;
      Def    : Node_Id)
   is
   begin
      Record_Diagnostic
        (Make_Representation_Too_Late_Error (Rep, Freeze, Def));
   end Record_Representation_Too_Late_Error;

   ------------------------------------------
   -- Make_Mixed_Container_Aggregate_Error --
   ------------------------------------------

   function Make_Mixed_Container_Aggregate_Error
     (Aggr       : Node_Id;
      Pos_Elem   : Node_Id;
      Named_Elem : Node_Id) return Diagnostic_Type
   is

   begin
      return
        Make_Diagnostic
          (Msg       =>
             "container aggregate cannot be both positional and named",
           Location  => Primary_Labeled_Span (Aggr),
           Id        => GNAT0011,
           Kind      => Diagnostics.Error,
           Spans     =>
             (1 => Secondary_Labeled_Span
               (Pos_Elem, "positional element "),
             2 => Secondary_Labeled_Span
               (Named_Elem, "named element")));
   end Make_Mixed_Container_Aggregate_Error;

   --------------------------------------------
   -- Record_Mixed_Container_Aggregate_Error --
   --------------------------------------------

   procedure Record_Mixed_Container_Aggregate_Error
     (Aggr       : Node_Id;
      Pos_Elem   : Node_Id;
      Named_Elem : Node_Id)
   is
   begin
      Record_Diagnostic
        (Make_Mixed_Container_Aggregate_Error (Aggr, Pos_Elem, Named_Elem));
   end Record_Mixed_Container_Aggregate_Error;

end Diagnostics.Constructors;
