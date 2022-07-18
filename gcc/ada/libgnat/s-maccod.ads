------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                   S Y S T E M . M A C H I N E _ C O D E                  --
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
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides machine code support, both for intrinsic machine
--  operations, and also for machine code statements. It implements the
--  *System.Machine_code* package defined in ARM 13.8 and GNAT Reference Manual
--  (chapter 'Implementation of Specific Ada Features', 'Machine Code
--  Insertions').

package System.Machine_Code
  with SPARK_Mode => Off
is
   pragma No_Elaboration_Code_All;
   pragma Pure;

   pragma Implementation_Defined;
   --  All identifiers in this unit are implementation defined

   type Asm_Input_Operand  is private;
   type Asm_Output_Operand is private;
   --  These types are never used directly, they are declared only so that
   --  the calls to Asm are type correct according to Ada semantic rules.

   No_Input_Operands  : constant Asm_Input_Operand;
   No_Output_Operands : constant Asm_Output_Operand;
   --  These constants are used as default value to denote respectively no
   --  input operands and no output operands.

   type Asm_Input_Operand_List  is
     array (Integer range <>) of Asm_Input_Operand;

   type Asm_Output_Operand_List is
     array (Integer range <>) of Asm_Output_Operand;
   --  The types *Asm_Input_Operand_List* and *Asm_Output_Operand_List* are
   --  arrays of respectively *Asm_Input_Operand* and *Asm_Output_Operand*.
   --  They are used to describe lists of operands for the Asm subprograms.

   type Asm_Insn is private;
   --  This type is not used directly. It is declared only so that the
   --  aggregates used in code statements are type correct by Ada rules.

   procedure Asm (
     Template : String;
     Outputs  : Asm_Output_Operand_List;
     Inputs   : Asm_Input_Operand_List;
     Clobber  : String  := "";
     Volatile : Boolean := False);

   procedure Asm (
     Template : String;
     Outputs  : Asm_Output_Operand := No_Output_Operands;
     Inputs   : Asm_Input_Operand_List;
     Clobber  : String  := "";
     Volatile : Boolean := False);

   procedure Asm (
     Template : String;
     Outputs  : Asm_Output_Operand_List;
     Inputs   : Asm_Input_Operand := No_Input_Operands;
     Clobber  : String  := "";
     Volatile : Boolean := False);

   procedure Asm (
     Template : String;
     Outputs  : Asm_Output_Operand := No_Output_Operands;
     Inputs   : Asm_Input_Operand  := No_Input_Operands;
     Clobber  : String  := "";
     Volatile : Boolean := False);

   function Asm (
     Template : String;
     Outputs  : Asm_Output_Operand_List;
     Inputs   : Asm_Input_Operand_List;
     Clobber  : String  := "";
     Volatile : Boolean := False) return Asm_Insn;

   function Asm (
     Template : String;
     Outputs  : Asm_Output_Operand := No_Output_Operands;
     Inputs   : Asm_Input_Operand_List;
     Clobber  : String  := "";
     Volatile : Boolean := False) return Asm_Insn;

   function Asm (
     Template : String;
     Outputs  : Asm_Output_Operand_List;
     Inputs   : Asm_Input_Operand := No_Input_Operands;
     Clobber  : String  := "";
     Volatile : Boolean := False) return Asm_Insn;

   function Asm (
     Template : String;
     Outputs  : Asm_Output_Operand := No_Output_Operands;
     Inputs   : Asm_Input_Operand  := No_Input_Operands;
     Clobber  : String  := "";
     Volatile : Boolean := False) return Asm_Insn;
   --  The parameters are described in the GNAT Reference Manual [GRM].
   --
   --  These are intrinsic subprograms, fully implemented by the compiler.

   pragma Import (Intrinsic, Asm);

private

   type Asm_Input_Operand  is new Integer;
   type Asm_Output_Operand is new Integer;
   type Asm_Insn           is new Integer;
   --  All three of these types are dummy types, to meet the requirements of
   --  type consistency. No values of these types are ever referenced.

   No_Input_Operands  : constant Asm_Input_Operand  := 0;
   No_Output_Operands : constant Asm_Output_Operand := 0;

end System.Machine_Code;
