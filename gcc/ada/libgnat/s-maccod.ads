------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                   S Y S T E M . M A C H I N E _ C O D E                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2017, Free Software Foundation, Inc.         --
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
--  operations, and also for machine code statements. See GNAT documentation
--  for full details.

package System.Machine_Code is
   pragma No_Elaboration_Code_All;
   pragma Pure;

   --  All identifiers in this unit are implementation defined

   pragma Implementation_Defined;

   type Asm_Input_Operand  is private;
   type Asm_Output_Operand is private;
   --  These types are never used directly, they are declared only so that
   --  the calls to Asm are type correct according to Ada semantic rules.

   No_Input_Operands  : constant Asm_Input_Operand;
   No_Output_Operands : constant Asm_Output_Operand;

   type Asm_Input_Operand_List  is
     array (Integer range <>) of Asm_Input_Operand;

   type Asm_Output_Operand_List is
     array (Integer range <>) of Asm_Output_Operand;

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
