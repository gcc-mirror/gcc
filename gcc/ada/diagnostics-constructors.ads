------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--             D I A G N O S T I C S . C O N S T R U C T O R S              --
--                                                                          --
--                                 S p e c                                  --
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
with Namet; use Namet;

package Diagnostics.Constructors is

   function Make_Default_Iterator_Not_Primitive_Error
     (Expr : Node_Id;
      Subp : Entity_Id) return Diagnostic_Type;

   procedure Record_Default_Iterator_Not_Primitive_Error
     (Expr : Node_Id;
      Subp : Entity_Id);

   function Make_Invalid_Operand_Types_For_Operator_Error
     (Op     : Node_Id;
      L      : Node_Id;
      L_Type : Node_Id;
      R      : Node_Id;
      R_Type : Node_Id) return Diagnostic_Type;

   procedure Record_Invalid_Operand_Types_For_Operator_Error
     (Op     : Node_Id;
      L      : Node_Id;
      L_Type : Node_Id;
      R      : Node_Id;
      R_Type : Node_Id);

   function Make_Invalid_Operand_Types_For_Operator_L_Int_Error
     (Op     : Node_Id;
      L      : Node_Id;
      L_Type : Node_Id;
      R      : Node_Id;
      R_Type : Node_Id) return Diagnostic_Type;

   procedure Record_Invalid_Operand_Types_For_Operator_L_Int_Error
     (Op     : Node_Id;
      L      : Node_Id;
      L_Type : Node_Id;
      R      : Node_Id;
      R_Type : Node_Id);

   function Make_Invalid_Operand_Types_For_Operator_R_Int_Error
     (Op     : Node_Id;
      L      : Node_Id;
      L_Type : Node_Id;
      R      : Node_Id;
      R_Type : Node_Id) return Diagnostic_Type;

   procedure Record_Invalid_Operand_Types_For_Operator_R_Int_Error
     (Op     : Node_Id;
      L      : Node_Id;
      L_Type : Node_Id;
      R      : Node_Id;
      R_Type : Node_Id);

   function Make_Invalid_Operand_Types_For_Operator_L_Acc_Error
     (Op     : Node_Id;
      L      : Node_Id) return Diagnostic_Type;

   procedure Record_Invalid_Operand_Types_For_Operator_L_Acc_Error
     (Op     : Node_Id;
      L      : Node_Id);

   function Make_Invalid_Operand_Types_For_Operator_R_Acc_Error
     (Op     : Node_Id;
      R      : Node_Id) return Diagnostic_Type;

   procedure Record_Invalid_Operand_Types_For_Operator_R_Acc_Error
     (Op     : Node_Id;
      R      : Node_Id);

   function Make_Invalid_Operand_Types_For_Operator_General_Error
     (Op     : Node_Id) return Diagnostic_Type;

   procedure Record_Invalid_Operand_Types_For_Operator_General_Error
     (Op     : Node_Id);

   function Make_Pragma_No_Effect_With_Lock_Free_Warning
     (Pragma_Node     : Node_Id;
      Pragma_Name     : Name_Id;
      Lock_Free_Node  : Node_Id;
      Lock_Free_Range : Node_Id)
      return Diagnostic_Type;

   procedure Record_Pragma_No_Effect_With_Lock_Free_Warning
     (Pragma_Node    : Node_Id;
      Pragma_Name    : Name_Id;
      Lock_Free_Node : Node_Id;
      Lock_Free_Range : Node_Id);

   function Make_End_Loop_Expected_Error
     (End_Loc : Source_Span;
      Start_Loc : Source_Ptr) return Diagnostic_Type;

   procedure Record_End_Loop_Expected_Error
     (End_Loc : Source_Span;
      Start_Loc : Source_Ptr);

   function Make_Representation_Too_Late_Error
     (Rep    : Node_Id;
      Freeze : Node_Id;
      Def    : Node_Id)
      return Diagnostic_Type;

   procedure Record_Representation_Too_Late_Error
     (Rep    : Node_Id;
      Freeze : Node_Id;
      Def    : Node_Id);

   function Make_Mixed_Container_Aggregate_Error
     (Aggr       : Node_Id;
      Pos_Elem   : Node_Id;
      Named_Elem : Node_Id) return Diagnostic_Type;

   procedure Record_Mixed_Container_Aggregate_Error
     (Aggr       : Node_Id;
      Pos_Elem   : Node_Id;
      Named_Elem : Node_Id);

end Diagnostics.Constructors;
