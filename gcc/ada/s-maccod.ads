------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                   S Y S T E M . M A C H I N E _ C O D E                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
--          Copyright (C) 1992-2004 Free Software Foundation, Inc.          --
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
-- As a special exception,  if other files  instantiate  generics from this --
-- unit, or you link  this unit with other files  to produce an executable, --
-- this  unit  does not  by itself cause  the resulting  executable  to  be --
-- covered  by the  GNU  General  Public  License.  This exception does not --
-- however invalidate  any other reasons why  the executable file  might be --
-- covered by the  GNU Public License.                                      --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides machine code support, both for intrinsic machine
--  operations, and also for machine code statements. See GNAT documentation
--  for full details.

package System.Machine_Code is
pragma Pure (Machine_Code);

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
     Volatile : Boolean := False)
     return     Asm_Insn;

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
   --  type consistenty. No values of these types are ever referenced.

   No_Input_Operands  : constant Asm_Input_Operand  := 0;
   No_Output_Operands : constant Asm_Output_Operand := 0;

end System.Machine_Code;
