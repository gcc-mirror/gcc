------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                     SYSTEM.MACHINE_STATE_OPERATIONS                      --
--                                                                          --
--                                 B o d y                                  --
--                         (Version for IRIX/MIPS)                          --
--                                                                          --
--          Copyright (C) 1999-2005 Free Software Foundation, Inc.          --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 2,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License --
-- for  more details.  You should have  received  a copy of the GNU General --
-- Public License  distributed with GNAT;  see file COPYING.  If not, write --
-- to  the  Free Software Foundation,  51  Franklin  Street,  Fifth  Floor, --
-- Boston, MA 02110-1301, USA.                                              --
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

--  This version of Ada.Exceptions.Machine_State_Operations is for use on
--  SGI Irix systems. By means of compile time conditional calculations, it
--  can handle both n32/n64 and o32 modes.

with System.Machine_Code; use System.Machine_Code;
with System.Memory;
with System.Soft_Links; use System.Soft_Links;
with Unchecked_Conversion;

package body System.Machine_State_Operations is

   use System.Storage_Elements;

   --  The exc_unwind function in libexc operats on a Sigcontext

   --  Type sigcontext_t is defined in /usr/include/sys/signal.h.
   --  We define an equivalent Ada type here. From the comments in
   --  signal.h:

   --    sigcontext is not part of the ABI - so this version is used to
   --    handle 32 and 64 bit applications - it is a constant size regardless
   --    of compilation mode, and always returns 64 bit register values

   type Uns32 is mod 2 ** 32;
   type Uns64 is mod 2 ** 64;

   type Uns32_Ptr is access all Uns32;
   type Uns64_Array is array (Integer range <>) of Uns64;

   type Reg_Array is array (0 .. 31) of Uns64;

   type Sigcontext is record
      SC_Regmask           : Uns32;          --  0
      SC_Status            : Uns32;          --  4
      SC_PC                : Uns64;          --  8
      SC_Regs              : Reg_Array;      --  16
      SC_Fpregs            : Reg_Array;      --  272
      SC_Ownedfp           : Uns32;          --  528
      SC_Fpc_Csr           : Uns32;          --  532
      SC_Fpc_Eir           : Uns32;          --  536
      SC_Ssflags           : Uns32;          --  540
      SC_Mdhi              : Uns64;          --  544
      SC_Mdlo              : Uns64;          --  552
      SC_Cause             : Uns64;          --  560
      SC_Badvaddr          : Uns64;          --  568
      SC_Triggersave       : Uns64;          --  576
      SC_Sigset            : Uns64;          --  584
      SC_Fp_Rounded_Result : Uns64;          --  592
      SC_Pancake           : Uns64_Array (0 .. 5);
      SC_Pad               : Uns64_Array (0 .. 26);
   end record;

   type Sigcontext_Ptr is access all Sigcontext;

   SC_Regs_Pos   : constant String := "16";
   SC_Fpregs_Pos : constant String := "272";
   --  Byte offset of the Integer and Floating Point register save areas
   --  within the Sigcontext.

   function To_Sigcontext_Ptr is
     new Unchecked_Conversion (Machine_State, Sigcontext_Ptr);

   type Addr_Int is mod 2 ** Long_Integer'Size;
   --  An unsigned integer type whose size is the same as System.Address.
   --  We rely on the fact that Long_Integer'Size = System.Address'Size in
   --  all ABIs.  Type Addr_Int can be converted to Uns64.

   function To_Code_Loc is new Unchecked_Conversion (Addr_Int, Code_Loc);
   function To_Addr_Int is new Unchecked_Conversion (System.Address, Addr_Int);
   function To_Uns32_Ptr is new Unchecked_Conversion (Addr_Int, Uns32_Ptr);

   --------------------------------
   -- ABI-Dependent Declarations --
   --------------------------------

   o32  : constant Boolean := System.Word_Size = 32;
   n32  : constant Boolean := System.Word_Size = 64;
   o32n : constant Natural := Boolean'Pos (o32);
   n32n : constant Natural := Boolean'Pos (n32);
   --  Flags to indicate which ABI is in effect for this compilation. For the
   --  purposes of this unit, the n32 and n64 ABI's are identical.

   LSC : constant Character := Character'Val (o32n * Character'Pos ('w') +
                                              n32n * Character'Pos ('d'));
   --  This is 'w' for o32, and 'd' for n32/n64, used for constructing the
   --  load/store instructions used to save/restore machine instructions.

   Roff : constant Character := Character'Val (o32n * Character'Pos ('4') +
                                               n32n * Character'Pos ('0'));
   --  Offset from first byte of a __uint64 register save location where
   --  the register value is stored.  For n32/64 we store the entire 64
   --  bit register into the uint64.  For o32, only 32 bits are stored
   --  at an offset of 4 bytes. This is used as part of expressions with
   --  '+' signs on both sides, so a null offset has to be '0' and not ' '
   --  to avoid assembler syntax errors on "X + + Y" in the latter case.

   procedure Update_GP (Scp : Sigcontext_Ptr);

   ---------------
   -- Update_GP --
   ---------------

   procedure Update_GP (Scp : Sigcontext_Ptr) is

      type F_op  is mod 2 ** 6;
      type F_reg is mod 2 ** 5;
      type F_imm is new Short_Integer;

      type I_Type is record
         op    : F_op;
         rs    : F_reg;
         rt    : F_reg;
         imm   : F_imm;
      end record;

      pragma Pack (I_Type);
      for I_Type'Size use 32;

      type I_Type_Ptr is access all I_Type;

      LW : constant F_op := 2#100011#;
      Reg_GP : constant := 28;

      type Address_Int is mod 2 ** Standard'Address_Size;
      function To_I_Type_Ptr is new
        Unchecked_Conversion (Address_Int, I_Type_Ptr);

      Ret_Ins : constant I_Type_Ptr := To_I_Type_Ptr (Address_Int (Scp.SC_PC));
      GP_Ptr  : Uns32_Ptr;

   begin
      if Ret_Ins.op = LW and then Ret_Ins.rt = Reg_GP then
         GP_Ptr := To_Uns32_Ptr
           (Addr_Int (Scp.SC_Regs (Integer (Ret_Ins.rs)))
            + Addr_Int (Ret_Ins.imm));
         Scp.SC_Regs (Reg_GP) := Uns64 (GP_Ptr.all);
      end if;
   end Update_GP;

   ----------------------------
   -- Allocate_Machine_State --
   ----------------------------

   function Allocate_Machine_State return Machine_State is
   begin
      return Machine_State
        (Memory.Alloc (Sigcontext'Max_Size_In_Storage_Elements));
   end Allocate_Machine_State;

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
      SC : constant Sigcontext_Ptr := To_Sigcontext_Ptr (M);
   begin
      return To_Code_Loc (Addr_Int (SC.SC_PC));
   end Get_Code_Loc;

   --------------------------
   -- Machine_State_Length --
   --------------------------

   function Machine_State_Length return Storage_Offset is
   begin
      return Sigcontext'Max_Size_In_Storage_Elements;
   end Machine_State_Length;

   ---------------
   -- Pop_Frame --
   ---------------

   procedure Pop_Frame (M : Machine_State) is
      Scp : constant Sigcontext_Ptr := To_Sigcontext_Ptr (M);

      procedure Exc_Unwind (Scp : Sigcontext_Ptr; Fde : Long_Integer := 0);
      pragma Import (C, Exc_Unwind, "exc_unwind");

      pragma Linker_Options ("-lexc");

   begin
      --  exc_unwind is apparently not thread-safe under IRIX, so protect it
      --  against race conditions within the GNAT run time.
      --  ??? Note that we might want to use a fine grained lock here since
      --  Lock_Task is used in many other places.

      Lock_Task.all;

      Exc_Unwind (Scp);

      Unlock_Task.all;

      if Scp.SC_PC = 0 or else Scp.SC_PC = 1 then

         --  A return value of 0 or 1 means exc_unwind couldn't find a parent
         --  frame. Propagate_Exception expects a zero return address to
         --  indicate TOS.

         Scp.SC_PC := 0;

      else
         --  Set the GP to restore to the caller value (not callee value)
         --  This is done only in o32 mode. In n32/n64 mode, GP is a normal
         --  callee save register

         if o32 then
            Update_GP (Scp);
         end if;

         --  Adjust the return address to the call site, not the
         --  instruction following the branch delay slot.  This may
         --  be necessary if the last instruction of a pragma No_Return
         --  subprogram is a call. The first instruction following the
         --  delay slot may be the start of another subprogram. We back
         --  off the address by 8, which points safely into the middle
         --  of the generated subprogram code, avoiding end effects.

         Scp.SC_PC := Scp.SC_PC - 8;
      end if;
   end Pop_Frame;

   -----------------------
   -- Set_Machine_State --
   -----------------------

   procedure Set_Machine_State (M : Machine_State) is

      STOREI : constant String (1 .. 2) := 's' & LSC;
      --  This is "sw" in o32 mode, and "sd" in n32 mode

      STOREF : constant String (1 .. 4) := 's' & LSC & "c1";
      --  This is "swc1" in o32 mode and "sdc1" in n32 mode

      Scp : Sigcontext_Ptr;

   begin
      --  Save the integer registers. Note that we know that $4 points
      --  to M, since that is where the first parameter is passed.
      --  Restore integer registers from machine state. Note that we know
      --  that $4 points to M since this is the standard calling sequence

      <<Past_Prolog>>

      Asm (STOREI & " $16,  16*8+" & Roff & "+" & SC_Regs_Pos & "($4)");
      Asm (STOREI & " $17,  17*8+" & Roff & "+" & SC_Regs_Pos & "($4)");
      Asm (STOREI & " $18,  18*8+" & Roff & "+" & SC_Regs_Pos & "($4)");
      Asm (STOREI & " $19,  19*8+" & Roff & "+" & SC_Regs_Pos & "($4)");
      Asm (STOREI & " $20,  20*8+" & Roff & "+" & SC_Regs_Pos & "($4)");
      Asm (STOREI & " $21,  21*8+" & Roff & "+" & SC_Regs_Pos & "($4)");
      Asm (STOREI & " $22,  22*8+" & Roff & "+" & SC_Regs_Pos & "($4)");
      Asm (STOREI & " $23,  23*8+" & Roff & "+" & SC_Regs_Pos & "($4)");
      Asm (STOREI & " $24,  24*8+" & Roff & "+" & SC_Regs_Pos & "($4)");
      Asm (STOREI & " $25,  25*8+" & Roff & "+" & SC_Regs_Pos & "($4)");
      Asm (STOREI & " $26,  26*8+" & Roff & "+" & SC_Regs_Pos & "($4)");
      Asm (STOREI & " $27,  27*8+" & Roff & "+" & SC_Regs_Pos & "($4)");
      Asm (STOREI & " $28,  28*8+" & Roff & "+" & SC_Regs_Pos & "($4)");
      Asm (STOREI & " $29,  29*8+" & Roff & "+" & SC_Regs_Pos & "($4)");
      Asm (STOREI & " $30,  30*8+" & Roff & "+" & SC_Regs_Pos & "($4)");
      Asm (STOREI & " $31,  31*8+" & Roff & "+" & SC_Regs_Pos & "($4)");

      --  Restore floating-point registers from machine state

      Asm (STOREF & " $f16, 16*8+" & Roff & "+" & SC_Fpregs_Pos & "($4)");
      Asm (STOREF & " $f17, 17*8+" & Roff & "+" & SC_Fpregs_Pos & "($4)");
      Asm (STOREF & " $f18, 18*8+" & Roff & "+" & SC_Fpregs_Pos & "($4)");
      Asm (STOREF & " $f19, 19*8+" & Roff & "+" & SC_Fpregs_Pos & "($4)");
      Asm (STOREF & " $f20, 20*8+" & Roff & "+" & SC_Fpregs_Pos & "($4)");
      Asm (STOREF & " $f21, 21*8+" & Roff & "+" & SC_Fpregs_Pos & "($4)");
      Asm (STOREF & " $f22, 22*8+" & Roff & "+" & SC_Fpregs_Pos & "($4)");
      Asm (STOREF & " $f23, 23*8+" & Roff & "+" & SC_Fpregs_Pos & "($4)");
      Asm (STOREF & " $f24, 24*8+" & Roff & "+" & SC_Fpregs_Pos & "($4)");
      Asm (STOREF & " $f25, 25*8+" & Roff & "+" & SC_Fpregs_Pos & "($4)");
      Asm (STOREF & " $f26, 26*8+" & Roff & "+" & SC_Fpregs_Pos & "($4)");
      Asm (STOREF & " $f27, 27*8+" & Roff & "+" & SC_Fpregs_Pos & "($4)");
      Asm (STOREF & " $f28, 28*8+" & Roff & "+" & SC_Fpregs_Pos & "($4)");
      Asm (STOREF & " $f29, 29*8+" & Roff & "+" & SC_Fpregs_Pos & "($4)");
      Asm (STOREF & " $f30, 30*8+" & Roff & "+" & SC_Fpregs_Pos & "($4)");
      Asm (STOREF & " $f31, 31*8+" & Roff & "+" & SC_Fpregs_Pos & "($4)");

      --  Set the PC value for the context to a location after the
      --  prolog has been executed.

      Scp := To_Sigcontext_Ptr (M);
      Scp.SC_PC := Uns64 (To_Addr_Int (Past_Prolog'Address));

      --  We saved the state *inside* this routine, but what we want is
      --  the state at the call site. So we need to do one pop operation.
      --  This pop operation will properly set the PC value in the machine
      --  state, so there is no need to save PC in the above code.

      Pop_Frame (M);
   end Set_Machine_State;

end System.Machine_State_Operations;
