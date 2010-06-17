------------------------------------------------------------------------------
--                                                                          --
--                         GNAT COMPILER COMPONENTS                         --
--                                                                          --
--                       S Y S T E M . A U X _ D E C                        --
--                                                                          --
--                                 B o d y                                  --
--                                                                          --
--          Copyright (C) 1992-2010, Free Software Foundation, Inc.         --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/Or modify it under --
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

--  This is the Alpha/VMS version.

pragma Style_Checks (All_Checks);
--  Turn off alpha ordering check on subprograms, this unit is laid
--  out to correspond to the declarations in the DEC 83 System unit.

with System.Machine_Code; use System.Machine_Code;
package body System.Aux_DEC is

   ------------------------
   -- Fetch_From_Address --
   ------------------------

   function Fetch_From_Address (A : Address) return Target is
      type T_Ptr is access all Target;
      function To_T_Ptr is new Ada.Unchecked_Conversion (Address, T_Ptr);
      Ptr : constant T_Ptr := To_T_Ptr (A);
   begin
      return Ptr.all;
   end Fetch_From_Address;

   -----------------------
   -- Assign_To_Address --
   -----------------------

   procedure Assign_To_Address (A : Address; T : Target) is
      type T_Ptr is access all Target;
      function To_T_Ptr is new Ada.Unchecked_Conversion (Address, T_Ptr);
      Ptr : constant T_Ptr := To_T_Ptr (A);
   begin
      Ptr.all := T;
   end Assign_To_Address;

   -----------------------
   -- Clear_Interlocked --
   -----------------------

   procedure Clear_Interlocked
     (Bit       : in out Boolean;
      Old_Value : out Boolean)
   is
      use ASCII;
      Clr_Bit : Boolean := Bit;
      Old_Bit : Boolean;

   begin
      --  All these ASM sequences should be commented. I suggest definining
      --  a constant called E which is LF & HT and then you have more space
      --  for line by line comments ???

      System.Machine_Code.Asm
        (
         "lda $16, %2"      & LF & HT &
         "mb"               & LF & HT &
         "sll $16, 3, $17 " & LF & HT &
         "bis $31, 1, $1"   & LF & HT &
         "and $17, 63, $18" & LF & HT &
         "bic $17, 63, $17" & LF & HT &
         "sra $17, 3, $17"  & LF & HT &
         "bis $31, 1, %1"   & LF & HT &
         "sll %1, $18, $18" & LF & HT &
         "1:"               & LF & HT &
         "ldq_l $1, 0($17)" & LF & HT &
         "and $1, $18, %1"  & LF & HT &
         "bic $1, $18, $1"  & LF & HT &
         "stq_c $1, 0($17)" & LF & HT &
         "cmpeq %1, 0, %1"  & LF & HT &
         "beq $1, 1b"       & LF & HT &
         "mb"               & LF & HT &
         "xor %1, 1, %1"    & LF & HT &
         "trapb",
         Outputs  => (Boolean'Asm_Output ("=m", Clr_Bit),
                      Boolean'Asm_Output ("=r", Old_Bit)),
         Inputs   => Boolean'Asm_Input ("m", Clr_Bit),
         Clobber  => "$1, $16, $17, $18",
         Volatile => True);

         Bit := Clr_Bit;
         Old_Value := Old_Bit;
   end Clear_Interlocked;

   procedure Clear_Interlocked
     (Bit          : in out Boolean;
      Old_Value    : out Boolean;
      Retry_Count  : Natural;
      Success_Flag : out Boolean)
   is
      use ASCII;
      Clr_Bit : Boolean := Bit;
      Succ, Old_Bit : Boolean;

   begin
      System.Machine_Code.Asm
        (
         "lda $16, %3"      & LF & HT &
         "mb"               & LF & HT &
         "sll $16, 3, $18 " & LF & HT &
         "bis $31, 1, %1"   & LF & HT &
         "and $18, 63, $19" & LF & HT &
         "bic $18, 63, $18" & LF & HT &
         "sra $18, 3, $18"  & LF & HT &
         "bis $31, %4, $17" & LF & HT &
         "sll %1, $19, $19" & LF & HT &
         "1:"               & LF & HT &
         "ldq_l %2, 0($18)" & LF & HT &
         "and %2, $19, %1"  & LF & HT &
         "bic %2, $19, %2"  & LF & HT &
         "stq_c %2, 0($18)" & LF & HT &
         "beq %2, 2f"       & LF & HT &
         "cmpeq %1, 0, %1"  & LF & HT &
         "br 3f"            & LF & HT &
         "2:"               & LF & HT &
         "subq $17, 1, $17" & LF & HT &
         "bgt $17, 1b"      & LF & HT &
         "3:"               & LF & HT &
         "mb"               & LF & HT &
         "xor %1, 1, %1"    & LF & HT &
         "trapb",
         Outputs  => (Boolean'Asm_Output ("=m", Clr_Bit),
                      Boolean'Asm_Output ("=r", Old_Bit),
                      Boolean'Asm_Output ("=r", Succ)),
         Inputs   => (Boolean'Asm_Input ("m", Clr_Bit),
                      Natural'Asm_Input ("rJ", Retry_Count)),
         Clobber  => "$16, $17, $18, $19",
         Volatile => True);

         Bit := Clr_Bit;
         Old_Value := Old_Bit;
         Success_Flag := Succ;
   end Clear_Interlocked;

   ---------------------
   -- Set_Interlocked --
   ---------------------

   procedure Set_Interlocked
     (Bit       : in out Boolean;
      Old_Value : out Boolean)
   is
      use ASCII;
      Set_Bit : Boolean := Bit;
      Old_Bit : Boolean;

   begin
      --  Don't we need comments on these long asm sequences???

      System.Machine_Code.Asm
        (
         "lda $16, %2"      & LF & HT &
         "sll $16, 3, $17 " & LF & HT &
         "bis $31, 1, $1"   & LF & HT &
         "and $17, 63, $18" & LF & HT &
         "mb"               & LF & HT &
         "bic $17, 63, $17" & LF & HT &
         "sra $17, 3, $17"  & LF & HT &
         "bis $31, 1, %1"   & LF & HT &
         "sll %1, $18, $18" & LF & HT &
         "1:"               & LF & HT &
         "ldq_l $1, 0($17)" & LF & HT &
         "and $1, $18, %1"  & LF & HT &
         "bis $1, $18, $1"  & LF & HT &
         "stq_c $1, 0($17)" & LF & HT &
         "cmovne %1, 1, %1" & LF & HT &
         "beq $1, 1b"       & LF & HT &
         "mb"               & LF & HT &
         "trapb",
         Outputs  => (Boolean'Asm_Output ("=m", Set_Bit),
                      Boolean'Asm_Output ("=r", Old_Bit)),
         Inputs   => Boolean'Asm_Input ("m", Set_Bit),
         Clobber  => "$1, $16, $17, $18",
         Volatile => True);

         Bit := Set_Bit;
         Old_Value := Old_Bit;
   end Set_Interlocked;

   procedure Set_Interlocked
     (Bit          : in out Boolean;
      Old_Value    : out Boolean;
      Retry_Count  : Natural;
      Success_Flag : out Boolean)
   is
      use ASCII;
      Set_Bit : Boolean := Bit;
      Succ, Old_Bit : Boolean;

   begin
      System.Machine_Code.Asm
        (
         "lda $16, %3"      & LF & HT &
         "mb"               & LF & HT &
         "sll $16, 3, $18 " & LF & HT &
         "bis $31, 1, %1"   & LF & HT &
         "and $18, 63, $19" & LF & HT &
         "bic $18, 63, $18" & LF & HT &
         "sra $18, 3, $18"  & LF & HT &
         "bis $31, %4, $17" & LF & HT &
         "sll %1, $19, $19" & LF & HT &
         "1:"               & LF & HT &
         "ldq_l %2, 0($18)" & LF & HT &
         "and %2, $19, %1"  & LF & HT &
         "bis %2, $19, %2"  & LF & HT &
         "stq_c %2, 0($18)" & LF & HT &
         "beq %2, 2f"       & LF & HT &
         "cmovne %1, 1, %1" & LF & HT &
         "br 3f"            & LF & HT &
         "2:"               & LF & HT &
         "subq $17, 1, $17" & LF & HT &
         "bgt $17, 1b"      & LF & HT &
         "3:"               & LF & HT &
         "mb"               & LF & HT &
         "trapb",
         Outputs  => (Boolean'Asm_Output ("=m", Set_Bit),
                      Boolean'Asm_Output ("=r", Old_Bit),
                      Boolean'Asm_Output ("=r", Succ)),
         Inputs   => (Boolean'Asm_Input ("m", Set_Bit),
                      Natural'Asm_Input ("rJ", Retry_Count)),
         Clobber  => "$16, $17, $18, $19",
         Volatile => True);

         Bit := Set_Bit;
         Old_Value := Old_Bit;
         Success_Flag := Succ;
   end Set_Interlocked;

   ---------------------
   -- Add_Interlocked --
   ---------------------

   procedure Add_Interlocked
     (Addend : Short_Integer;
      Augend : in out Aligned_Word;
      Sign   : out Integer)
   is
      use ASCII;
      Overflowed : Boolean := False;

   begin
      System.Machine_Code.Asm
        (
         "lda $18, %0"         & LF & HT &
         "bic $18, 6, $21"     & LF & HT &
         "mb"                  & LF & HT &
         "1:"                  & LF & HT &
         "ldq_l $0, 0($21)"    & LF & HT &
         "extwl $0, $18, $19"  & LF & HT &
         "mskwl $0, $18, $0"   & LF & HT &
         "addq $19, %3, $20"   & LF & HT &
         "inswl $20, $18, $17" & LF & HT &
         "xor $19, %3, $19"    & LF & HT &
         "bis $17, $0, $0"     & LF & HT &
         "stq_c $0, 0($21)"    & LF & HT &
         "beq $0, 1b"          & LF & HT &
         "srl $20, 16, $0"     & LF & HT &
         "mb"                  & LF & HT &
         "srl $20, 12, $21"    & LF & HT &
         "zapnot $20, 3, $20"  & LF & HT &
         "and $0, 1, $0"       & LF & HT &
         "and $21, 8, $21"     & LF & HT &
         "bis $21, $0, $0"     & LF & HT &
         "cmpeq $20, 0, $21"   & LF & HT &
         "xor $20, 2, $20"     & LF & HT &
         "sll $21, 2, $21"     & LF & HT &
         "bis $21, $0, $0"     & LF & HT &
         "bic $20, $19, $21"   & LF & HT &
         "srl $21, 14, $21"    & LF & HT &
         "and $21, 2, $21"     & LF & HT &
         "bis $21, $0, $0"     & LF & HT &
         "and $0, 2, %2"       & LF & HT &
         "bne %2, 2f"          & LF & HT &
         "and $0, 4, %1"       & LF & HT &
         "cmpeq %1, 0, %1"     & LF & HT &
         "and $0, 8, $0"       & LF & HT &
         "lda $16, -1"         & LF & HT &
         "cmovne $0, $16, %1"  & LF & HT &
         "2:",
         Outputs  => (Aligned_Word'Asm_Output ("=m", Augend),
                      Integer'Asm_Output ("=r", Sign),
                      Boolean'Asm_Output ("=r", Overflowed)),
         Inputs   => (Short_Integer'Asm_Input ("r", Addend),
                      Aligned_Word'Asm_Input ("m", Augend)),
         Clobber  => "$0, $1, $16, $17, $18, $19, $20, $21",
         Volatile => True);

         if Overflowed then
            raise Constraint_Error;
         end if;
   end Add_Interlocked;

   ----------------
   -- Add_Atomic --
   ----------------

   procedure Add_Atomic
     (To     : in out Aligned_Integer;
      Amount : Integer)
   is
      use ASCII;

   begin
      System.Machine_Code.Asm
        (
         "mb"              & LF & HT &
         "1:"              & LF & HT &
         "ldl_l $1, %0"    & LF & HT &
         "addl $1, %2, $0" & LF & HT &
         "stl_c $0, %1"    & LF & HT &
         "beq $0, 1b"      & LF & HT &
         "mb",
         Outputs  => Aligned_Integer'Asm_Output ("=m", To),
         Inputs   => (Aligned_Integer'Asm_Input ("m", To),
                      Integer'Asm_Input ("rJ", Amount)),
         Clobber  => "$0, $1",
         Volatile => True);
   end Add_Atomic;

   procedure Add_Atomic
     (To           : in out Aligned_Integer;
      Amount       : Integer;
      Retry_Count  : Natural;
      Old_Value    : out Integer;
      Success_Flag : out Boolean)
   is
      use ASCII;

   begin
      System.Machine_Code.Asm
        (
         "mb"               & LF & HT &
         "bis $31, %5, $17" & LF & HT &
         "1:"               & LF & HT &
         "ldl_l $1, %0"     & LF & HT &
         "addl $1, %4, $0"  & LF & HT &
         "stl_c $0, %3"     & LF & HT &
         "beq $0, 2f"       & LF & HT &
         "3:"               & LF & HT &
         "mb"               & LF & HT &
         "stq $0, %2"       & LF & HT &
         "stl $1, %1"       & LF & HT &
         "br 4f"            & LF & HT &
         "2:"               & LF & HT &
         "subq $17, 1, $17" & LF & HT &
         "bgt $17, 1b"      & LF & HT &
         "br 3b"            & LF & HT &
         "4:",
         Outputs  => (Aligned_Integer'Asm_Output ("=m", To),
                      Integer'Asm_Output ("=m", Old_Value),
                      Boolean'Asm_Output ("=m", Success_Flag)),
         Inputs   => (Aligned_Integer'Asm_Input ("m", To),
                      Integer'Asm_Input ("rJ", Amount),
                      Natural'Asm_Input ("rJ", Retry_Count)),
         Clobber  => "$0, $1, $17",
         Volatile => True);
   end Add_Atomic;

   procedure Add_Atomic
     (To     : in out Aligned_Long_Integer;
      Amount : Long_Integer)
   is
      use ASCII;

   begin
      System.Machine_Code.Asm
        (
         "mb"              & LF & HT &
         "1:"              & LF & HT &
         "ldq_l $1, %0"    & LF & HT &
         "addq $1, %2, $0" & LF & HT &
         "stq_c $0, %1"    & LF & HT &
         "beq $0, 1b"      & LF & HT &
         "mb",
         Outputs  => Aligned_Long_Integer'Asm_Output ("=m", To),
         Inputs   => (Aligned_Long_Integer'Asm_Input ("m", To),
                      Long_Integer'Asm_Input ("rJ", Amount)),
         Clobber  => "$0, $1",
         Volatile => True);
   end Add_Atomic;

   procedure Add_Atomic
     (To           : in out Aligned_Long_Integer;
      Amount       : Long_Integer;
      Retry_Count  : Natural;
      Old_Value    : out Long_Integer;
      Success_Flag : out Boolean)
   is
      use ASCII;

   begin
      System.Machine_Code.Asm
        (
         "mb"               & LF & HT &
         "bis $31, %5, $17" & LF & HT &
         "1:"               & LF & HT &
         "ldq_l $1, %0"     & LF & HT &
         "addq $1, %4, $0"  & LF & HT &
         "stq_c $0, %3"     & LF & HT &
         "beq $0, 2f"       & LF & HT &
         "3:"               & LF & HT &
         "mb"               & LF & HT &
         "stq $0, %2"       & LF & HT &
         "stq $1, %1"       & LF & HT &
         "br 4f"            & LF & HT &
         "2:"               & LF & HT &
         "subq $17, 1, $17" & LF & HT &
         "bgt $17, 1b"      & LF & HT &
         "br 3b"            & LF & HT &
         "4:",
         Outputs  => (Aligned_Long_Integer'Asm_Output ("=m", To),
                      Long_Integer'Asm_Output ("=m", Old_Value),
                      Boolean'Asm_Output ("=m", Success_Flag)),
         Inputs   => (Aligned_Long_Integer'Asm_Input ("m", To),
                      Long_Integer'Asm_Input ("rJ", Amount),
                      Natural'Asm_Input ("rJ", Retry_Count)),
         Clobber  => "$0, $1, $17",
         Volatile => True);
   end Add_Atomic;

   ----------------
   -- And_Atomic --
   ----------------

   procedure And_Atomic
     (To   : in out Aligned_Integer;
      From : Integer)
   is
      use ASCII;

   begin
      System.Machine_Code.Asm
        (
         "mb"             & LF & HT &
         "1:"             & LF & HT &
         "ldl_l $1, %0"   & LF & HT &
         "and $1, %2, $0" & LF & HT &
         "stl_c $0, %1"   & LF & HT &
         "beq $0, 1b"     & LF & HT &
         "mb",
         Outputs  => Aligned_Integer'Asm_Output ("=m", To),
         Inputs   => (Aligned_Integer'Asm_Input ("m", To),
                      Integer'Asm_Input ("rJ", From)),
         Clobber  => "$0, $1",
         Volatile => True);
   end And_Atomic;

   procedure And_Atomic
     (To           : in out Aligned_Integer;
      From         : Integer;
      Retry_Count  : Natural;
      Old_Value    : out Integer;
      Success_Flag : out Boolean)
   is
      use ASCII;

   begin
      System.Machine_Code.Asm
        (
         "mb"               & LF & HT &
         "bis $31, %5, $17" & LF & HT &
         "1:"               & LF & HT &
         "ldl_l $1, %0"     & LF & HT &
         "and $1, %4, $0"   & LF & HT &
         "stl_c $0, %3"     & LF & HT &
         "beq $0, 2f"       & LF & HT &
         "3:"               & LF & HT &
         "mb"               & LF & HT &
         "stq $0, %2"       & LF & HT &
         "stl $1, %1"       & LF & HT &
         "br 4f"            & LF & HT &
         "2:"               & LF & HT &
         "subq $17, 1, $17" & LF & HT &
         "bgt $17, 1b"      & LF & HT &
         "br 3b"            & LF & HT &
         "4:",
         Outputs  => (Aligned_Integer'Asm_Output ("=m", To),
                      Integer'Asm_Output ("=m", Old_Value),
                      Boolean'Asm_Output ("=m", Success_Flag)),
         Inputs   => (Aligned_Integer'Asm_Input ("m", To),
                      Integer'Asm_Input ("rJ", From),
                      Natural'Asm_Input ("rJ", Retry_Count)),
         Clobber  => "$0, $1, $17",
         Volatile => True);
   end And_Atomic;

   procedure And_Atomic
     (To   : in out Aligned_Long_Integer;
      From : Long_Integer)
   is
      use ASCII;

   begin
      System.Machine_Code.Asm
        (
         "mb"             & LF & HT &
         "1:"             & LF & HT &
         "ldq_l $1, %0"   & LF & HT &
         "and $1, %2, $0" & LF & HT &
         "stq_c $0, %1"   & LF & HT &
         "beq $0, 1b"     & LF & HT &
         "mb",
         Outputs  => Aligned_Long_Integer'Asm_Output ("=m", To),
         Inputs   => (Aligned_Long_Integer'Asm_Input ("m", To),
                      Long_Integer'Asm_Input ("rJ", From)),
         Clobber  => "$0, $1",
         Volatile => True);
   end And_Atomic;

   procedure And_Atomic
     (To           : in out Aligned_Long_Integer;
      From         : Long_Integer;
      Retry_Count  : Natural;
      Old_Value    : out Long_Integer;
      Success_Flag : out Boolean)
   is
      use ASCII;

   begin
      System.Machine_Code.Asm
        (
         "mb"               & LF & HT &
         "bis $31, %5, $17" & LF & HT &
         "1:"               & LF & HT &
         "ldq_l $1, %0"     & LF & HT &
         "and $1, %4, $0"   & LF & HT &
         "stq_c $0, %3"     & LF & HT &
         "beq $0, 2f"       & LF & HT &
         "3:"               & LF & HT &
         "mb"               & LF & HT &
         "stq $0, %2"       & LF & HT &
         "stq $1, %1"       & LF & HT &
         "br 4f"            & LF & HT &
         "2:"               & LF & HT &
         "subq $17, 1, $17" & LF & HT &
         "bgt $17, 1b"      & LF & HT &
         "br 3b"            & LF & HT &
         "4:",
         Outputs  => (Aligned_Long_Integer'Asm_Output ("=m", To),
                      Long_Integer'Asm_Output ("=m", Old_Value),
                      Boolean'Asm_Output ("=m", Success_Flag)),
         Inputs   => (Aligned_Long_Integer'Asm_Input ("m", To),
                      Long_Integer'Asm_Input ("rJ", From),
                      Natural'Asm_Input ("rJ", Retry_Count)),
         Clobber  => "$0, $1, $17",
         Volatile => True);
   end And_Atomic;

   ---------------
   -- Or_Atomic --
   ---------------

   procedure Or_Atomic
     (To   : in out Aligned_Integer;
      From : Integer)
   is
      use ASCII;

   begin
      System.Machine_Code.Asm
        (
         "mb"             & LF & HT &
         "1:"             & LF & HT &
         "ldl_l $1, %0"   & LF & HT &
         "bis $1, %2, $0" & LF & HT &
         "stl_c $0, %1"   & LF & HT &
         "beq $0, 1b"     & LF & HT &
         "mb",
         Outputs  => Aligned_Integer'Asm_Output ("=m", To),
         Inputs   => (Aligned_Integer'Asm_Input ("m", To),
                      Integer'Asm_Input ("rJ", From)),
         Clobber  => "$0, $1",
         Volatile => True);
   end Or_Atomic;

   procedure Or_Atomic
     (To           : in out Aligned_Integer;
      From         : Integer;
      Retry_Count  : Natural;
      Old_Value    : out Integer;
      Success_Flag : out Boolean)
   is
      use ASCII;

   begin
      System.Machine_Code.Asm
        (
         "mb"               & LF & HT &
         "bis $31, %5, $17" & LF & HT &
         "1:"               & LF & HT &
         "ldl_l $1, %0"     & LF & HT &
         "bis $1, %4, $0"   & LF & HT &
         "stl_c $0, %3"     & LF & HT &
         "beq $0, 2f"       & LF & HT &
         "3:"               & LF & HT &
         "mb"               & LF & HT &
         "stq $0, %2"       & LF & HT &
         "stl $1, %1"       & LF & HT &
         "br 4f"            & LF & HT &
         "2:"               & LF & HT &
         "subq $17, 1, $17" & LF & HT &
         "bgt $17, 1b"      & LF & HT &
         "br 3b"            & LF & HT &
         "4:",
         Outputs  => (Aligned_Integer'Asm_Output ("=m", To),
                      Integer'Asm_Output ("=m", Old_Value),
                      Boolean'Asm_Output ("=m", Success_Flag)),
         Inputs   => (Aligned_Integer'Asm_Input ("m", To),
                      Integer'Asm_Input ("rJ", From),
                      Natural'Asm_Input ("rJ", Retry_Count)),
         Clobber  => "$0, $1, $17",
         Volatile => True);
   end Or_Atomic;

   procedure Or_Atomic
     (To   : in out Aligned_Long_Integer;
      From : Long_Integer)
   is
      use ASCII;

   begin
      System.Machine_Code.Asm
        (
         "mb"             & LF & HT &
         "1:"             & LF & HT &
         "ldq_l $1, %0"   & LF & HT &
         "bis $1, %2, $0" & LF & HT &
         "stq_c $0, %1"   & LF & HT &
         "beq $0, 1b"     & LF & HT &
         "mb",
         Outputs  => Aligned_Long_Integer'Asm_Output ("=m", To),
         Inputs   => (Aligned_Long_Integer'Asm_Input ("m", To),
                      Long_Integer'Asm_Input ("rJ", From)),
         Clobber  => "$0, $1",
         Volatile => True);
   end Or_Atomic;

   procedure Or_Atomic
     (To           : in out Aligned_Long_Integer;
      From         : Long_Integer;
      Retry_Count  : Natural;
      Old_Value    : out Long_Integer;
      Success_Flag : out Boolean)
   is
      use ASCII;

   begin
      System.Machine_Code.Asm
        (
         "mb"               & LF & HT &
         "bis $31, %5, $17" & LF & HT &
         "1:"               & LF & HT &
         "ldq_l $1, %0"     & LF & HT &
         "bis $1, %4, $0"   & LF & HT &
         "stq_c $0, %3"     & LF & HT &
         "beq $0, 2f"       & LF & HT &
         "3:"               & LF & HT &
         "mb"               & LF & HT &
         "stq $0, %2"       & LF & HT &
         "stq $1, %1"       & LF & HT &
         "br 4f"            & LF & HT &
         "2:"               & LF & HT &
         "subq $17, 1, $17" & LF & HT &
         "bgt $17, 1b"      & LF & HT &
         "br 3b"            & LF & HT &
         "4:",
         Outputs  => (Aligned_Long_Integer'Asm_Output ("=m", To),
                      Long_Integer'Asm_Output ("=m", Old_Value),
                      Boolean'Asm_Output ("=m", Success_Flag)),
         Inputs   => (Aligned_Long_Integer'Asm_Input ("m", To),
                      Long_Integer'Asm_Input ("rJ", From),
                      Natural'Asm_Input ("rJ", Retry_Count)),
         Clobber  => "$0, $1, $17",
         Volatile => True);
   end Or_Atomic;

   ------------
   -- Insqhi --
   ------------

   procedure Insqhi
     (Item   : Address;
      Header : Address;
      Status : out Insq_Status)
   is
      use ASCII;

   begin
      System.Machine_Code.Asm
        (
         "bis $31, %1, $17" & LF & HT &
         "bis $31, %2, $16" & LF & HT &
         "mb"               & LF & HT &
         "call_pal 0x87"    & LF & HT &
         "mb",
         Outputs  => Insq_Status'Asm_Output ("=v", Status),
         Inputs   => (Address'Asm_Input ("rJ", Item),
                      Address'Asm_Input ("rJ", Header)),
         Clobber  => "$16, $17",
         Volatile => True);
   end Insqhi;

   ------------
   -- Remqhi --
   ------------

   procedure Remqhi
     (Header : Address;
      Item   : out Address;
      Status : out Remq_Status)
   is
      use ASCII;

   begin
      System.Machine_Code.Asm
        (
         "bis $31, %2, $16" & LF & HT &
         "mb"               & LF & HT &
         "call_pal 0x93"    & LF & HT &
         "mb"               & LF & HT &
         "bis $31, $1, %1",
         Outputs  => (Remq_Status'Asm_Output ("=v", Status),
                      Address'Asm_Output ("=r", Item)),
         Inputs   => Address'Asm_Input ("rJ", Header),
         Clobber  => "$1, $16",
         Volatile => True);
   end Remqhi;

   ------------
   -- Insqti --
   ------------

   procedure Insqti
     (Item   : Address;
      Header : Address;
      Status : out Insq_Status)
   is
      use ASCII;

   begin
      System.Machine_Code.Asm
        (
         "bis $31, %1, $17" & LF & HT &
         "bis $31, %2, $16" & LF & HT &
         "mb"               & LF & HT &
         "call_pal 0x88"    & LF & HT &
         "mb",
         Outputs  => Insq_Status'Asm_Output ("=v", Status),
         Inputs   => (Address'Asm_Input ("rJ", Item),
                      Address'Asm_Input ("rJ", Header)),
         Clobber  => "$16, $17",
         Volatile => True);
   end Insqti;

   ------------
   -- Remqti --
   ------------

   procedure Remqti
     (Header : Address;
      Item   : out Address;
      Status : out Remq_Status)
   is
      use ASCII;

   begin
      System.Machine_Code.Asm
        (
         "bis $31, %2, $16" & LF & HT &
         "mb"               & LF & HT &
         "call_pal 0x94"    & LF & HT &
         "mb"               & LF & HT &
         "bis $31, $1, %1",
         Outputs  => (Remq_Status'Asm_Output ("=v", Status),
                      Address'Asm_Output ("=r", Item)),
         Inputs   => Address'Asm_Input ("rJ", Header),
         Clobber  => "$1, $16",
         Volatile => True);
   end Remqti;

end System.Aux_DEC;
