------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     SYSTEM.MACHINE_STATE_OPERATIONS                      --
--                                                                          --
--                                 B o d y                                  --
--                            (Version for x86)                             --
--                                                                          --
--           Copyright (C) 1999-2002 Ada Core Technologies, Inc.            --
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

--  Note: it is very important that this unit not generate any exception
--  tables of any kind. Otherwise we get a nasty rtsfind recursion problem.
--  This means no subprograms, including implicitly generated ones.

with Unchecked_Conversion;
with System.Storage_Elements;
with System.Machine_Code; use System.Machine_Code;
with System.Memory;

package body System.Machine_State_Operations is

   use System.Exceptions;

   type Uns8  is mod 2 ** 8;
   type Uns32 is mod 2 ** 32;

   type Bits5 is mod 2 ** 5;
   type Bits6 is mod 2 ** 6;

   function To_Address is new Unchecked_Conversion (Uns32, Address);

   type Uns32_Ptr is access all Uns32;
   function To_Uns32_Ptr is new Unchecked_Conversion (Uns32,   Uns32_Ptr);

   --  Note: the type Uns32 has an alignment of 4. However, in some cases
   --  values of type Uns32_Ptr will not be aligned (notably in the case
   --  where we get the immediate field from an instruction). However this
   --  does not matter in practice, since the x86 does not require that
   --  operands be aligned.

   ----------------------
   -- General Approach --
   ----------------------

   --  For the x86 version of this unit, the Subprogram_Info_Type values
   --  are simply the starting code address for the subprogram. Popping
   --  of stack frames works by analyzing the code in the prolog, and
   --  deriving from this analysis the necessary information for restoring
   --  the registers, including the return point.

   ---------------------------
   -- Description of Prolog --
   ---------------------------

   --  If a frame pointer is present, the prolog looks like

   --     pushl %ebp
   --     movl  %esp,%ebp
   --     subl  $nnn,%esp     omitted if nnn = 0
   --     pushl %edi          omitted if edi not used
   --     pushl %esi          omitted if esi not used
   --     pushl %ebx          omitted if ebx not used

   --  If a frame pointer is not present, the prolog looks like

   --     subl  $nnn,%esp     omitted if nnn = 0
   --     pushl %ebp          omitted if ebp not used
   --     pushl %edi          omitted if edi not used
   --     pushl %esi          omitted if esi not used
   --     pushl %ebx          omitted if ebx not used

   --  Note: any or all of the save over call registers may be used and
   --  if so, will be saved using pushl as shown above. The order of the
   --  pushl instructions will be as shown above for gcc generated code,
   --  but the code in this unit does not assume this.

   -------------------------
   -- Description of Call --
   -------------------------

   --  A call looks like:

   --     pushl ...           push parameters
   --     pushl ...
   --     call  ...           perform the call
   --     addl  $nnn,%esp     omitted if no parameters

   --  Note that we are not absolutely guaranteed that the call is always
   --  followed by an addl operation that readjusts %esp for this particular
   --  call. There are two reasons for this:

   --    1) The addl can be delayed and combined in the case where more than
   --       one call appears in sequence. This can be suppressed by using the
   --       switch -fno-defer-pop and for Ada code, we automatically use
   --       this switch, but we could still be dealing with C code that was
   --       compiled without using this switch.

   --    2) Scheduling may result in moving the addl instruction away from
   --       the call. It is not clear if this actually can happen at the
   --       current time, but it is certainly conceptually possible.

   --  The addl after the call is important, since we need to be able to
   --  restore the proper %esp value when we pop the stack. However, we do
   --  not try to compensate for either of the above effects. As noted above,
   --  case 1 does not occur for Ada code, and it does not appear in practice
   --  that case 2 occurs with any significant frequency (we have never seen
   --  an example so far for gcc generated code).

   --  Furthermore, it is only in the case of -fomit-frame-pointer that we
   --  really get into trouble from not properly restoring %esp. If we have
   --  a frame pointer, then the worst that happens is that %esp is slightly
   --  more depressed than it should be. This could waste a bit of space on
   --  the stack, and even in some cases cause a storage leak on the stack,
   --  but it will not affect the functional correctness of the processing.

   ----------------------------------------
   -- Definitions of Instruction Formats --
   ----------------------------------------

   type Rcode is (eax, ecx, edx, ebx, esp, ebp, esi, edi);
   pragma Warnings (Off, Rcode);
   --  Code indicating which register is referenced in an instruction

   --  The following define the format of a pushl instruction

   Op_pushl : constant Bits5 := 2#01010#;

   type Ins_pushl is record
      Op  : Bits5 := Op_pushl;
      Reg : Rcode;
   end record;

   for Ins_pushl use record
      Op  at 0 range 3 .. 7;
      Reg at 0 range 0 .. 2;
   end record;

   Ins_pushl_ebp : constant Ins_pushl := (Op_pushl, Reg => ebp);

   type Ins_pushl_Ptr is access all Ins_pushl;

   --  For the movl %esp,%ebp instruction, we only need to know the length
   --  because we simply skip past it when we analyze the prolog.

   Ins_movl_length : constant := 2;

   --  The following define the format of addl/subl esp instructions

   Op_Immed : constant Bits6 := 2#100000#;

   Op2_addl_Immed : constant Bits5 := 2#11100#;
   pragma Unreferenced (Op2_addl_Immed);

   Op2_subl_Immed : constant Bits5 := 2#11101#;

   type Word_Byte is (Word, Byte);
   pragma Unreferenced (Byte);

   type Ins_addl_subl_byte is record
      Op   : Bits6;           -- Set to Op_Immed
      w    : Word_Byte;       -- Word/Byte flag (set to 1 = byte)
      s    : Boolean;         -- Sign extension bit (1 = extend)
      Op2  : Bits5;           -- Secondary opcode
      Reg  : Rcode;           -- Register
      Imm8 : Uns8;            -- Immediate operand
   end record;

   for Ins_addl_subl_byte use record
      Op   at 0 range 2 .. 7;
      w    at 0 range 1 .. 1;
      s    at 0 range 0 .. 0;
      Op2  at 1 range 3 .. 7;
      Reg  at 1 range 0 .. 2;
      Imm8 at 2 range 0 .. 7;
   end record;

   type Ins_addl_subl_word is record
      Op    : Bits6;          -- Set to Op_Immed
      w     : Word_Byte;      -- Word/Byte flag (set to 0 = word)
      s     : Boolean;        -- Sign extension bit (1 = extend)
      Op2   : Bits5;          -- Secondary opcode
      Reg   : Rcode;          -- Register
      Imm32 : Uns32;          -- Immediate operand
   end record;

   for Ins_addl_subl_word use record
      Op    at 0 range 2 .. 7;
      w     at 0 range 1 .. 1;
      s     at 0 range 0 .. 0;
      Op2   at 1 range 3 .. 7;
      Reg   at 1 range 0 .. 2;
      Imm32 at 2 range 0 .. 31;
   end record;

   type Ins_addl_subl_byte_Ptr is access all Ins_addl_subl_byte;
   type Ins_addl_subl_word_Ptr is access all Ins_addl_subl_word;

   ---------------------
   -- Prolog Analysis --
   ---------------------

   --  The analysis of the prolog answers the following questions:

   --    1. Is %ebp used as a frame pointer?
   --    2. How far is SP depressed (i.e. what is the stack frame size)
   --    3. Which registers are saved in the prolog, and in what order

   --  The following data structure stores the answers to these questions

   subtype SOC is Rcode range ebx .. edi;
   --  Possible save over call registers

   SOC_Max : constant := 4;
   --  Max number of SOC registers that can be pushed

   type SOC_Push_Regs_Type is array (1 .. 4) of Rcode;
   --  Used to hold the register codes of pushed SOC registers

   type Prolog_Type is record

      Frame_Reg : Boolean;
      --  This is set to True if %ebp is used as a frame register, and
      --  False otherwise (in the False case, %ebp may be saved in the
      --  usual manner along with the other SOC registers).

      Frame_Length : Uns32;
      --  Amount by which ESP is decremented on entry, includes the effects
      --  of push's of save over call registers as indicated above, e.g. if
      --  the prolog of a routine is:
      --
      --    pushl %ebp
      --    movl %esp,%ebp
      --    subl $424,%esp
      --    pushl %edi
      --    pushl %esi
      --    pushl %ebx
      --
      --  Then the value of Frame_Length would be 436 (424 + 3 * 4). A
      --  precise definition is that it is:
      --
      --    %esp on entry   minus   %esp after last SOC push
      --
      --  That definition applies both in the frame pointer present and
      --  the frame pointer absent cases.

      Num_SOC_Push : Integer range 0 .. SOC_Max;
      --  Number of save over call registers actually saved by pushl
      --  instructions (other than the initial pushl to save the frame
      --  pointer if a frame pointer is in use).

      SOC_Push_Regs : SOC_Push_Regs_Type;
      --  The First Num_SOC_Push entries of this array are used to contain
      --  the codes for the SOC registers, in the order in which they were
      --  pushed. Note that this array excludes %ebp if it is used as a frame
      --  register, since although %ebp is still considered an SOC register
      --  in this case, it is saved and restored by a separate mechanism.
      --  Also we will never see %esp represented in this list. Again, it is
      --  true that %esp is saved over call, but it is restored by a separate
      --  mechanism.

   end record;

   procedure Analyze_Prolog (A : Address; Prolog : out Prolog_Type);
   --  Given the address of the start of the prolog for a procedure,
   --  analyze the instructions of the prolog, and set Prolog to contain
   --  the information obtained from this analysis.

   ----------------------------------
   -- Machine_State_Representation --
   ----------------------------------

   --  The type Machine_State is defined in the body of Ada.Exceptions as
   --  a Storage_Array of length 1 .. Machine_State_Length. But really it
   --  has structure as defined here. We use the structureless declaration
   --  in Ada.Exceptions to avoid this unit from being implementation
   --  dependent. The actual definition of Machine_State is as follows:

   type SOC_Regs_Type is array (SOC) of Uns32;

   type MState is record
      eip : Uns32;
      --  The instruction pointer location (which is the return point
      --  value from the next level down in all cases).

      Regs : SOC_Regs_Type;
      --  Values of the save over call registers
   end record;

   for MState use record
      eip  at 0 range 0 .. 31;
      Regs at 4 range 0 .. 5 * 32 - 1;
   end record;
   --  Note: the routines Enter_Handler, and Set_Machine_State reference
   --  the fields in this structure non-symbolically.

   type MState_Ptr is access all MState;

   function To_MState_Ptr is
     new Unchecked_Conversion (Machine_State, MState_Ptr);

   ----------------------------
   -- Allocate_Machine_State --
   ----------------------------

   function Allocate_Machine_State return Machine_State is
      use System.Storage_Elements;

   begin
      return Machine_State
        (Memory.Alloc (MState'Max_Size_In_Storage_Elements));
   end Allocate_Machine_State;

   --------------------
   -- Analyze_Prolog --
   --------------------

   procedure Analyze_Prolog (A : Address; Prolog : out Prolog_Type) is
      Ptr : Address;
      Ppl : Ins_pushl_Ptr;
      Pas : Ins_addl_subl_byte_Ptr;

      function To_Ins_pushl_Ptr is
        new Unchecked_Conversion (Address, Ins_pushl_Ptr);

      function To_Ins_addl_subl_byte_Ptr is
        new Unchecked_Conversion (Address, Ins_addl_subl_byte_Ptr);

      function To_Ins_addl_subl_word_Ptr is
        new Unchecked_Conversion (Address, Ins_addl_subl_word_Ptr);

   begin
      Ptr := A;
      Prolog.Frame_Length := 0;

      if Ptr = Null_Address then
         Prolog.Num_SOC_Push := 0;
         Prolog.Frame_Reg := True;
         return;
      end if;

      if To_Ins_pushl_Ptr (Ptr).all = Ins_pushl_ebp then
         Ptr := Ptr + 1 + Ins_movl_length;
         Prolog.Frame_Reg := True;
      else
         Prolog.Frame_Reg := False;
      end if;

      Pas := To_Ins_addl_subl_byte_Ptr (Ptr);

      if Pas.Op = Op_Immed
        and then Pas.Op2 = Op2_subl_Immed
        and then Pas.Reg = esp
      then
         if Pas.w = Word then
            Prolog.Frame_Length := Prolog.Frame_Length +
                                     To_Ins_addl_subl_word_Ptr (Ptr).Imm32;
            Ptr := Ptr + 6;

         else
            Prolog.Frame_Length := Prolog.Frame_Length + Uns32 (Pas.Imm8);
            Ptr := Ptr + 3;

            --  Note: we ignore sign extension, since a sign extended
            --  value that was negative would imply a ludicrous frame size.
         end if;
      end if;

      --  Now scan push instructions for SOC registers

      Prolog.Num_SOC_Push := 0;

      loop
         Ppl := To_Ins_pushl_Ptr (Ptr);

         if Ppl.Op = Op_pushl and then Ppl.Reg in SOC then
            Prolog.Num_SOC_Push := Prolog.Num_SOC_Push + 1;
            Prolog.SOC_Push_Regs (Prolog.Num_SOC_Push) := Ppl.Reg;
            Prolog.Frame_Length := Prolog.Frame_Length + 4;
            Ptr := Ptr + 1;

         else
            exit;
         end if;
      end loop;

   end Analyze_Prolog;

   -------------------
   -- Enter_Handler --
   -------------------

   procedure Enter_Handler (M : Machine_State; Handler : Handler_Loc) is
   begin
      Asm ("mov %0,%%edx", Inputs => Machine_State'Asm_Input ("r", M));
      Asm ("mov %0,%%eax", Inputs => Handler_Loc'Asm_Input ("r", Handler));

      Asm ("mov 4(%%edx),%%ebx");    -- M.Regs (ebx)
      Asm ("mov 12(%%edx),%%ebp");   -- M.Regs (ebp)
      Asm ("mov 16(%%edx),%%esi");   -- M.Regs (esi)
      Asm ("mov 20(%%edx),%%edi");   -- M.Regs (edi)
      Asm ("mov 8(%%edx),%%esp");    -- M.Regs (esp)
      Asm ("jmp %*%%eax");
   end Enter_Handler;

   ----------------
   -- Fetch_Code --
   ----------------

   function Fetch_Code (Loc : Code_Loc) return Code_Loc is
   begin
      return Loc;
   end Fetch_Code;

   ------------------------
   -- Free_Machine_State --
   ------------------------

   procedure Free_Machine_State (M : in out Machine_State) is
   begin
      Memory.Free (Address (M));
      M := Machine_State (Null_Address);
   end Free_Machine_State;

   ------------------
   -- Get_Code_Loc --
   ------------------

   function Get_Code_Loc (M : Machine_State) return Code_Loc is

      Asm_Call_Size : constant := 2;
      --  Minimum size for a call instruction under ix86. Using the minimum
      --  size is safe here as the call point computed from the return point
      --  will always be inside the call instruction.

      MS : constant MState_Ptr := To_MState_Ptr (M);

   begin
      if MS.eip = 0 then
         return To_Address (MS.eip);
      else
         --  When doing a call the return address is pushed to the stack.
         --  We want to return the call point address, so we substract
         --  Asm_Call_Size from the return address. This value is set
         --  to 5 as an asm call takes 5 bytes on x86 architectures.

         return To_Address (MS.eip - Asm_Call_Size);
      end if;
   end Get_Code_Loc;

   --------------------------
   -- Machine_State_Length --
   --------------------------

   function Machine_State_Length
     return System.Storage_Elements.Storage_Offset
   is
   begin
      return MState'Max_Size_In_Storage_Elements;
   end Machine_State_Length;

   ---------------
   -- Pop_Frame --
   ---------------

   procedure Pop_Frame
     (M    : Machine_State;
      Info : Subprogram_Info_Type)
   is
      MS  : constant MState_Ptr := To_MState_Ptr (M);
      PL  : Prolog_Type;

      SOC_Ptr : Uns32;
      --  Pointer to stack location after last SOC push

      Rtn_Ptr : Uns32;
      --  Pointer to stack location containing return address

   begin
      Analyze_Prolog (Info, PL);

      --  Case of frame register, use EBP, safer than ESP

      if PL.Frame_Reg then
         SOC_Ptr := MS.Regs (ebp) - PL.Frame_Length;
         Rtn_Ptr := MS.Regs (ebp) + 4;
         MS.Regs (ebp) := To_Uns32_Ptr (MS.Regs (ebp)).all;

      --  No frame pointer, use ESP, and hope we have it exactly right!

      else
         SOC_Ptr := MS.Regs (esp);
         Rtn_Ptr := SOC_Ptr + PL.Frame_Length;
      end if;

      --  Get saved values of SOC registers

      for J in reverse 1 .. PL.Num_SOC_Push loop
         MS.Regs (PL.SOC_Push_Regs (J)) := To_Uns32_Ptr (SOC_Ptr).all;
         SOC_Ptr := SOC_Ptr + 4;
      end loop;

      MS.eip := To_Uns32_Ptr (Rtn_Ptr).all;
      MS.Regs (esp) := Rtn_Ptr + 4;
   end Pop_Frame;

   -----------------------
   -- Set_Machine_State --
   -----------------------

   procedure Set_Machine_State (M : Machine_State) is
      N : constant Asm_Output_Operand := No_Output_Operands;

   begin
      Asm ("mov %0,%%edx", N, Machine_State'Asm_Input ("r", M));

      --  At this stage, we have the following situation (note that we
      --  are assuming that the -fomit-frame-pointer switch has not been
      --  used in compiling this procedure.

      --     (value of M)
      --     return point
      --     old ebp          <------ current ebp/esp value

      --  The values of registers ebx/esi/edi are unchanged from entry
      --  so they have the values we want, and %edx points to the parameter
      --  value M, so we can store these values directly.

      Asm ("mov %%ebx,4(%%edx)");    -- M.Regs (ebx)
      Asm ("mov %%esi,16(%%edx)");   -- M.Regs (esi)
      Asm ("mov %%edi,20(%%edx)");   -- M.Regs (edi)

      --  The desired value of ebp is the old value

      Asm ("mov 0(%%ebp),%%eax");
      Asm ("mov %%eax,12(%%edx)");   -- M.Regs (ebp)

      --  The return point is the desired eip value

      Asm ("mov 4(%%ebp),%%eax");
      Asm ("mov %%eax,(%%edx)");   -- M.eip

      --  Finally, the desired %esp value is the value at the point of
      --  call to this routine *before* pushing the parameter value.

      Asm ("lea 12(%%ebp),%%eax");
      Asm ("mov %%eax,8(%%edx)");   -- M.Regs (esp)
   end Set_Machine_State;

   ------------------------------
   -- Set_Signal_Machine_State --
   ------------------------------

   procedure Set_Signal_Machine_State
     (M       : Machine_State;
      Context : System.Address)
   is
      pragma Warnings (Off, M);
      pragma Warnings (Off, Context);

   begin
      null;
   end Set_Signal_Machine_State;

end System.Machine_State_Operations;
