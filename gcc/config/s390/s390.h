/* Definitions of target machine for GNU compiler, for IBM S/390
   Copyright (C) 1999, 2000, 2001 Free Software Foundation, Inc.
   Contributed by Hartmut Penner (hpenner@de.ibm.com) and
                  Ulrich Weigand (weigand@de.ibm.com).
This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

#ifndef _S390_H
#define _S390_H

#define TARGET_VERSION fprintf (stderr, " (S/390)");

extern int flag_pic; 

/* Run-time compilation parameters selecting different hardware subsets.  */

extern int target_flags;

/* Target macros checked at runtime of compiler.  */

#define TARGET_HARD_FLOAT          (target_flags & 1)
#define TARGET_BACKCHAIN           (target_flags & 2)
#define TARGET_SMALL_EXEC          (target_flags & 4)
#define TARGET_DEBUG_ARG           (target_flags & 8)
#define TARGET_64BIT               (target_flags & 16)
#define TARGET_MVCLE               (target_flags & 32)

#define TARGET_DEFAULT             0x3
#define TARGET_SOFT_FLOAT          (!(target_flags & 1))

/* Macro to define tables used to set the flags.  This is a list in braces
   of pairs in braces, each pair being { "NAME", VALUE }
   where VALUE is the bits to set or minus the bits to clear.
   An empty string NAME is used to identify the default VALUE.  */

#define TARGET_SWITCHES           		       		       \
{ { "hard-float",    1,N_("Use hardware fp")},         		       \
  { "soft-float",   -1,N_("Don't use hardware fp")},	      	       \
  { "backchain",     2,N_("Set backchain")},           		       \
  { "no-backchain", -2,N_("Don't set backchain (faster, but debug harder")}, \
  { "small-exec",    4,N_("Use bras for execucable < 64k")},           \
  { "no-small-exec",-4,N_("Don't use bras")},            	       \
  { "debug_arg",     8,N_("Additional debug prints")},        	       \
  { "no-debug_arg", -8,N_("Don't print additional debug prints")},     \
  { "64",           16,N_("64 bit mode")},         	               \
  { "31",          -16,N_("31 bit mode")},                             \
  { "mvcle",        32,N_("mvcle use")},         	               \
  { "no-mvcle",    -32,N_("mvc&ex")},                                  \
  { "", TARGET_DEFAULT, 0 } }

/* Define this to change the optimizations performed by default.  */
#define OPTIMIZATION_OPTIONS(LEVEL,SIZE) optimization_options(LEVEL,SIZE)

/* The current function count for create unique internal labels.  */

extern int s390_function_count;

/* The amount of space used for outgoing arguments.  */

extern int current_function_outgoing_args_size;

/* Target machine storage layout.  */

/* Define this if most significant bit is lowest numbered in instructions
   that operate on numbered bit-fields.  */

#define BITS_BIG_ENDIAN 1

/* Define this if most significant byte of a word is the lowest numbered.  */

#define BYTES_BIG_ENDIAN 1

/* Define this if MS word of a multiword is the lowest numbered.  */

#define WORDS_BIG_ENDIAN 1

/* Number of bits in an addressable storage unit.  */

#define BITS_PER_UNIT 8

/* Width in bits of a "word", which is the contents of a machine register.  */

#define BITS_PER_WORD (TARGET_64BIT ? 64 : 32)
#define MAX_BITS_PER_WORD 32

/* Width of a word, in units (bytes).  */

#define UNITS_PER_WORD (TARGET_64BIT ? 8 : 4)
#define MIN_UNITS_PER_WORD 4

/* Width in bits of a pointer.  See also the macro `Pmode' defined below.  */

#define POINTER_SIZE (TARGET_64BIT ? 64 : 32)

/* A C expression for the size in bits of the type `short' on the
   target machine.  If you don't define this, the default is half a
   word.  (If this would be less than one storage unit, it is
   rounded up to one unit.)  */
#define SHORT_TYPE_SIZE 16

/* A C expression for the size in bits of the type `int' on the
   target machine.  If you don't define this, the default is one
   word.  */
#define INT_TYPE_SIZE 32

/* A C expression for the size in bits of the type `long' on the
   target machine.  If you don't define this, the default is one
   word.  */
#define LONG_TYPE_SIZE (TARGET_64BIT ? 64 : 32)
#define MAX_LONG_TYPE_SIZE 32

/* A C expression for the size in bits of the type `long long' on the
   target machine.  If you don't define this, the default is two
   words.  */
#define LONG_LONG_TYPE_SIZE 64

/* Right now we only support two floating point formats, the 
   32 and 64 bit ieee formats.  */

#define FLOAT_TYPE_SIZE 32
#define DOUBLE_TYPE_SIZE 64
#define LONG_DOUBLE_TYPE_SIZE 64

/* Define this macro if it is advisable to hold scalars in registers
   in a wider mode than that declared by the program.  In such cases,
   the value is constrained to be within the bounds of the declared
   type, but kept valid in the wider mode.  The signedness of the
   extension may differ from that of the type.  */

#define PROMOTE_MODE(MODE, UNSIGNEDP, TYPE)		\
if (INTEGRAL_MODE_P (MODE) &&	        	    	\
    GET_MODE_SIZE (MODE) < UNITS_PER_WORD) { 		\
  (MODE) = Pmode;					\
	  }

/* Defining PROMOTE_FUNCTION_ARGS eliminates some unnecessary zero/sign
   extensions applied to char/short functions arguments.  Defining
   PROMOTE_FUNCTION_RETURN does the same for function returns.  */

#define PROMOTE_FUNCTION_ARGS
#define PROMOTE_FUNCTION_RETURN
#define PROMOTE_FOR_CALL_ONLY

/* Allocation boundary (in *bits*) for storing pointers in memory.  */

#define POINTER_BOUNDARY 32

/* Allocation boundary (in *bits*) for storing arguments in argument list.  */

#define PARM_BOUNDARY (TARGET_64BIT ? 64 : 32)

/* Boundary (in *bits*) on which stack pointer should be aligned.  */

#define STACK_BOUNDARY 64

/* Allocation boundary (in *bits*) for the code of a function.  */

#define FUNCTION_BOUNDARY 32

/* There is no point aligning anything to a rounder boundary than this.  */

#define BIGGEST_ALIGNMENT 64

/* Alignment of field after `int : 0' in a structure.  */

#define EMPTY_FIELD_BOUNDARY 32

/* Alignment on even adresses for LARL instruction.  */

#define CONSTANT_ALIGNMENT(EXP, ALIGN) (ALIGN) < 16 ? 16 : (ALIGN)

#define DATA_ALIGNMENT(TYPE, ALIGN) (ALIGN) < 16 ? 16 : (ALIGN)

/* Define this if move instructions will actually fail to work when given
   unaligned data.  */

#define STRICT_ALIGNMENT 0

/* real arithmetic */

#define REAL_ARITHMETIC

/* Define target floating point format.  */

#undef TARGET_FLOAT_FORMAT
#ifdef IEEE_FLOAT
#define TARGET_FLOAT_FORMAT IEEE_FLOAT_FORMAT
#else
#define TARGET_FLOAT_FORMAT IBM_FLOAT_FORMAT
#endif

/* Define if special allocation order desired.  */

#define REG_ALLOC_ORDER                                         \
{  1, 2, 3, 4, 5, 0, 14, 13, 12, 11, 10, 9, 8, 7, 6,            \
   16, 17, 18, 19, 20, 21, 22, 23,                              \
   24, 25, 26, 27, 28, 29, 30, 31,                              \
   15, 32, 33 }

/* Standard register usage.  */
 
#define INT_REGNO_P(N)   ( (N) >= 0 && (N) < 16 )
#ifdef IEEE_FLOAT
#define FLOAT_REGNO_P(N) ( (N) >= 16 && (N) < 32 )
#else
#define FLOAT_REGNO_P(N) ( (N) >= 16 && (N) < 20 )
#endif
#define CC_REGNO_P(N)    ( (N) == 33 )

/* Number of actual hardware registers.  The hardware registers are
   assigned numbers for the compiler from 0 to just below
   FIRST_PSEUDO_REGISTER.
   All registers that the compiler knows about must be given numbers,
   even those that are not normally considered general registers.
   For the 390, we give the data registers numbers 0-15,
   and the floating point registers numbers 16-19.
   G5 and following have 16 IEEE floating point register,
   which get numbers 16-31.  */

#define FIRST_PSEUDO_REGISTER 34

/* The following register have a fix usage
   GPR 12: GOT register points to the GOT, setup in prologue,
           GOT contains pointer to variables in shared libraries 
   GPR 13: Base register setup in prologue to point to the
           literal table of each function
   GPR 14: Return registers holds the return address
   GPR 15: Stack pointer */

#define PIC_OFFSET_TABLE_REGNUM 12
#define BASE_REGISTER 13
#define RETURN_REGNUM 14
#define STACK_POINTER_REGNUM 15

#define FIXED_REGISTERS				\
{ 0, 0, 0, 0, 					\
  0, 0, 0, 0, 					\
  0, 0, 0, 0, 					\
  0, 1, 1, 1,					\
  0, 0, 0, 0, 					\
  0, 0, 0, 0, 					\
  0, 0, 0, 0, 					\
  0, 0, 0, 0, 					\
  1, 1 }

/* 1 for registers not available across function calls.  These must include
   the FIXED_REGISTERS and also any registers that can be used without being
   saved.
   The latter must include the registers where values are returned
   and the register where structure-value addresses are passed.  */

#define CALL_USED_REGISTERS			\
{ 1, 1, 1, 1, 					\
  1, 1, 0, 0, 					\
  0, 0, 0, 0, 					\
  0, 1, 1, 1,					\
  1, 1, 0, 0, 					\
  1, 1, 1, 1, 					\
  1, 1, 1, 1, 					\
  1, 1, 1, 1, 					\
  1, 1 }

/* If not pic code, gpr 12 can be used.  */

#define CONDITIONAL_REGISTER_USAGE				\
do								\
  {								\
    if (flag_pic)						\
      {								\
	fixed_regs[PIC_OFFSET_TABLE_REGNUM] = 1;		\
	call_used_regs[PIC_OFFSET_TABLE_REGNUM] = 1;		\
      }								\
 } while (0)

/* The following register have a special usage
   GPR 11: Frame pointer if needed to point to automatic variables. 
   GPR 32: In functions with more the 5 args this register
           points to that arguments, it is always eliminated
	   with stack- or frame-pointer. 
   GPR 33: Condition code 'register' */

#define FRAME_POINTER_REGNUM 11

#define ARG_POINTER_REGNUM 32

#define CC_REGNUM 33

/* We use the register %r0 to pass the static chain to a nested function.
  
   Note: It is assumed that this register is call-clobbered!
         We can't use any of the function-argument registers either,
         and register 1 is needed by the trampoline code, so we have
         no other choice but using this one ...  */

#define STATIC_CHAIN_REGNUM 0

/* Return number of consecutive hard regs needed starting at reg REGNO
   to hold something of mode MODE.
   This is ordinarily the length in words of a value of mode MODE
   but can be less for certain modes in special long registers.  */

#define HARD_REGNO_NREGS(REGNO, MODE)                           \
  (FLOAT_REGNO_P(REGNO)?                                        \
    (GET_MODE_CLASS(MODE) == MODE_COMPLEX_FLOAT ? 2 : 1) :      \
   INT_REGNO_P(REGNO)?                                          \
    ((GET_MODE_SIZE(MODE)+UNITS_PER_WORD-1) / UNITS_PER_WORD) : \
   1)

/* Value is 1 if hard register REGNO can hold a value of machine-mode MODE.
   The gprs can hold QI, HI, SI, SF, DF, SC and DC.
   Even gprs can hold DI.
   The floating point registers can hold DF, SF, DC and SC.  */

#define HARD_REGNO_MODE_OK(REGNO, MODE)                             \
  (FLOAT_REGNO_P(REGNO)?                                            \
   (GET_MODE_CLASS(MODE) == MODE_FLOAT ||                           \
    GET_MODE_CLASS(MODE) == MODE_COMPLEX_FLOAT) :                   \
   INT_REGNO_P(REGNO)?                                              \
    (!((TARGET_64BIT && (MODE) == TImode) ||                        \
     (!TARGET_64BIT && (MODE) == DImode)) || ((REGNO) & 1) == 0 ) : \
   CC_REGNO_P(REGNO)?                                               \
     GET_MODE_CLASS (MODE) == MODE_CC :                             \
   0)

/* Value is 1 if it is a good idea to tie two pseudo registers when one has
   mode MODE1 and one has mode MODE2.
   If HARD_REGNO_MODE_OK could produce different values for MODE1 and MODE2,
   for any hard reg, then this must be 0 for correct output.  */

#define MODES_TIEABLE_P(MODE1, MODE2)		\
   (((MODE1) == SFmode || (MODE1) == DFmode)	\
   == ((MODE2) == SFmode || (MODE2) == DFmode))


/* Define this macro if references to a symbol must be treated
   differently depending on something about the variable or
   function named by the symbol (such as what section it is in).

   On s390, if using PIC, mark a SYMBOL_REF for a non-global symbol
   so that we may access it directly in the GOT.  */

#define ENCODE_SECTION_INFO(DECL)                               \
do                                                              \
  {                                                             \
    if (flag_pic)                                               \
      {                                                         \
        rtx rtl = (TREE_CODE_CLASS (TREE_CODE (DECL)) != 'd'    \
                   ? TREE_CST_RTL (DECL) : DECL_RTL (DECL));    \
                                                                \
        if (GET_CODE (rtl) == MEM)                              \
          {                                                     \
            SYMBOL_REF_FLAG (XEXP (rtl, 0))                     \
              = (TREE_CODE_CLASS (TREE_CODE (DECL)) != 'd'      \
                 || ! TREE_PUBLIC (DECL));                      \
          }                                                     \
      }                                                         \
  }                                                             \
while (0)
  

/* This is an array of structures.  Each structure initializes one pair
   of eliminable registers.  The "from" register number is given first,
   followed by "to".  Eliminations of the same "from" register are listed
   in order of preference.  */

#define ELIMINABLE_REGS				        \
{{ FRAME_POINTER_REGNUM, STACK_POINTER_REGNUM},	        \
 { ARG_POINTER_REGNUM, STACK_POINTER_REGNUM},	        \
 { ARG_POINTER_REGNUM, FRAME_POINTER_REGNUM}}  

#define CAN_ELIMINATE(FROM, TO) (1)

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET) 			  \
{ if ((FROM) == FRAME_POINTER_REGNUM && (TO) == STACK_POINTER_REGNUM) 	  \
  { (OFFSET) = 0; }     						  \
  else if ((FROM) == ARG_POINTER_REGNUM && (TO) == FRAME_POINTER_REGNUM)  \
  { (OFFSET) = s390_arg_frame_offset (); }     				  \
  else if ((FROM) == ARG_POINTER_REGNUM && (TO) == STACK_POINTER_REGNUM)  \
  { (OFFSET) = s390_arg_frame_offset (); }     				  \
}

#define CAN_DEBUG_WITHOUT_FP

/* Value should be nonzero if functions must have frame pointers.
   Zero means the frame pointer need not be set up (and parms may be
   accessed via the stack pointer) in functions that seem suitable.
   This is computed in `reload', in reload1.c.  */

#define FRAME_POINTER_REQUIRED 0

/* Define the classes of registers for register constraints in the
   machine description.  Also define ranges of constants.
  
   One of the classes must always be named ALL_REGS and include all hard regs.
   If there is more than one class, another class must be named NO_REGS
   and contain no registers.
 
   The name GENERAL_REGS must be the name of a class (or an alias for
   another name such as ALL_REGS).  This is the class of registers
   that is allowed by "g" or "r" in a register constraint.
   Also, registers outside this class are allocated only when
   instructions express preferences for them.
  
   The classes must be numbered in nondecreasing order; that is,
   a larger-numbered class must never be contained completely
   in a smaller-numbered class.
  
   For any two classes, it is very desirable that there be another
   class that represents their union.  */

/*#define SMALL_REGISTER_CLASSES 1*/

enum reg_class
{
  NO_REGS, ADDR_REGS, GENERAL_REGS,
  FP_REGS, CC_REGS, ALL_REGS, LIM_REG_CLASSES
};

#define N_REG_CLASSES (int) LIM_REG_CLASSES

/* Give names of register classes as strings for dump file.  */

#define REG_CLASS_NAMES                                                 \
{ "NO_REGS","ADDR_REGS", "GENERAL_REGS", "FP_REGS", "CC_REGS", "ALL_REGS" }

/* Define which registers fit in which classes.  This is an initializer for
   a vector of HARD_REG_SET of length N_REG_CLASSES.
   G5 and latter have 16 register and support IEEE floating point operations.  */

#define REG_CLASS_CONTENTS \
{				       			\
  { 0x00000000, 0x00000000 },	/* NO_REGS */		\
  { 0x0000fffe, 0x00000001 },	/* ADDR_REGS */		\
  { 0x0000ffff, 0x00000001 },	/* GENERAL_REGS */	\
  { 0xffff0000, 0x00000000 },	/* FP_REGS */		\
  { 0x00000000, 0x00000002 },	/* CC_REGS */		\
  { 0xffffffff, 0x00000003 },	/* ALL_REGS */		\
}


/* The same information, inverted:
   Return the class number of the smallest class containing
   reg number REGNO.  This could be a conditional expression
   or could index an array.  */

#define REGNO_REG_CLASS(REGNO) (regclass_map[REGNO])

extern enum reg_class regclass_map[];	/* smalled class containing REGNO   */

/* The class value for index registers, and the one for base regs.  */

#define INDEX_REG_CLASS ADDR_REGS
#define BASE_REG_CLASS ADDR_REGS

/* Get reg_class from a letter such as appears in the machine description.  */

#define REG_CLASS_FROM_LETTER(C)                                        \
  ((C) == 'a' ? ADDR_REGS :                                             \
   (C) == 'd' ? GENERAL_REGS :                                          \
   (C) == 'f' ? FP_REGS : NO_REGS)

/* The letters I, J, K, L and M in a register constraint string can be used
   to stand for particular ranges of immediate operands.
   This macro defines what the ranges are.
   C is the letter, and VALUE is a constant value.
   Return 1 if VALUE is in the range specified by C.  */

#define CONST_OK_FOR_LETTER_P(VALUE, C)                                 \
  ((C) == 'I' ? (unsigned long) (VALUE) < 256 :                         \
   (C) == 'J' ? (unsigned long) (VALUE) < 4096 :                        \
   (C) == 'K' ? (VALUE) >= -32768 && (VALUE) < 32768 :                  \
   (C) == 'L' ? (unsigned long) (VALUE) < 65536 : 0)

/* Similar, but for floating constants, and defining letters G and H.
   Here VALUE is the CONST_DOUBLE rtx itself.  */

#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C)  1

/* 'Q' means a memory-reference for a S-type operand.  */

#define EXTRA_CONSTRAINT(OP, C)                               	\
     ((C) == 'Q' ?  s_operand (OP, GET_MODE (OP)) : 		\
      (C) == 'S' ?  larl_operand (OP, GET_MODE (OP)) : 0)

/* Given an rtx X being reloaded into a reg required to be in class CLASS,
   return the class of reg to actually use.  In general this is just CLASS;
   but on some machines in some cases it is preferable to use a more
   restrictive class.  */

#define PREFERRED_RELOAD_CLASS(X, CLASS)                                 \
    (GET_CODE (X) == CONST_DOUBLE ?                                      \
     (GET_MODE_CLASS (GET_MODE (X)) == MODE_FLOAT ? FP_REGS : ADDR_REGS) :\
     (GET_CODE (X) == CONST_INT ?                                        \
     (GET_MODE_CLASS (GET_MODE (X)) == MODE_FLOAT ? FP_REGS : ADDR_REGS) :\
     GET_CODE (X) == PLUS ||                                            \
     GET_CODE (X) == LABEL_REF ||                                        \
     GET_CODE (X) == SYMBOL_REF ||                                       \
     GET_CODE (X) == CONST ? ADDR_REGS : (CLASS)))

/* Return the maximum number of consecutive registers needed to represent
   mode MODE in a register of class CLASS.  */

#define CLASS_MAX_NREGS(CLASS, MODE)   					\
     ((CLASS) == FP_REGS ? 						\
      (GET_MODE_CLASS (MODE) == MODE_COMPLEX_FLOAT ? 2 : 1) :  		\
      (GET_MODE_SIZE (MODE) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

/* If we are copying between FP registers and anything else, we need a memory
   location.  */

#define SECONDARY_MEMORY_NEEDED(CLASS1, CLASS2, MODE) \
 ((CLASS1) != (CLASS2) && ((CLASS1) == FP_REGS || (CLASS2) == FP_REGS))

/* Get_secondary_mem widens its argument to BITS_PER_WORD which loses on 64bit
   because the movsi and movsf patterns don't handle r/f moves. */

#define SECONDARY_MEMORY_NEEDED_MODE(MODE)		\
 (GET_MODE_BITSIZE (MODE) < 32				\
  ? mode_for_size (32, GET_MODE_CLASS (MODE), 0)	\
  : MODE)


/* A C expression whose value is nonzero if pseudos that have been
   assigned to registers of class CLASS would likely be spilled
   because registers of CLASS are needed for spill registers.

   The default value of this macro returns 1 if CLASS has exactly one
   register and zero otherwise.  On most machines, this default
   should be used.  Only define this macro to some other expression
   if pseudo allocated by `local-alloc.c' end up in memory because
   their hard registers were needed for spill registers.  If this
   macro returns nonzero for those classes, those pseudos will only
   be allocated by `global.c', which knows how to reallocate the
   pseudo to another register.  If there would not be another
   register available for reallocation, you should not change the
   definition of this macro since the only effect of such a
   definition would be to slow down register allocation.  */

/* Stack layout; function entry, exit and calling.  */

/* The current return address is on Offset 56 of the current frame
   if we are in an leaf_function. Otherwise we have to go one stack
   back.
   The return address of anything farther back is accessed normally
   at an offset of 56 from the frame pointer.

   FIXME: builtin_return_addr does not work correctly in a leaf
          function, we need to find way to find out, if we
          are in a leaf function
  */

#define _RETURN_ADDR_OFFSET (TARGET_64BIT ? 112 : 56)

#define RETURN_ADDR_RTX(count, frame)                                   \
   gen_rtx (MEM, Pmode,                                                 \
            memory_address (Pmode,                                      \
                              plus_constant (                           \
                              copy_to_reg (gen_rtx (MEM, Pmode,         \
                              memory_address (Pmode, frame))),          \
                              _RETURN_ADDR_OFFSET)));

/* The following macros will turn on dwarf2 exception hndling
   Other code location for this exception handling are 
   in s390.md (eh_return insn) and in linux.c in the prologue.  */

#define INCOMING_RETURN_ADDR_RTX  gen_rtx_REG (Pmode, RETURN_REGNUM)

/* We have 31 bit mode.  */

#define MASK_RETURN_ADDR (GEN_INT (0x7fffffff))

/* Location, from where return address to load.  */

#define DWARF_FRAME_RETURN_COLUMN  14

/* Describe how we implement __builtin_eh_return.  */
#define EH_RETURN_DATA_REGNO(N) ((N) < 4 ? (N) + 6 : INVALID_REGNUM)
#define EH_RETURN_STACKADJ_RTX  gen_rtx_REG (Pmode, 10)
#define EH_RETURN_HANDLER_RTX \
  gen_rtx_MEM (Pmode, plus_constant (arg_pointer_rtx, -40))

/* Define this if pushing a word on the stack makes the stack pointer a
   smaller address.  */

#define STACK_GROWS_DOWNWARD

/* Define this if the nominal address of the stack frame is at the
   high-address end of the local variables; that is, each additional local
   variable allocated goes at a more negative offset in the frame.  */

/* #define FRAME_GROWS_DOWNWARD   */

/* Offset from stack-pointer to first location of outgoing args.  */

#define STACK_POINTER_OFFSET (TARGET_64BIT ? 160 : 96)

/* Offset within stack frame to start allocating local variables at.
   If FRAME_GROWS_DOWNWARD, this is the offset to the END of the
   first local allocated.  Otherwise, it is the offset to the BEGINNING
   of the first local allocated.  */

#define STARTING_FRAME_OFFSET \
     (STACK_POINTER_OFFSET + current_function_outgoing_args_size)

#define INITIAL_FRAME_POINTER_OFFSET(DEPTH) (DEPTH) = 0

/* If we generate an insn to push BYTES bytes, this says how many the stack
   pointer really advances by.  On S/390, we have no push instruction.  */

/* #define PUSH_ROUNDING(BYTES) */

/* Accumulate the outgoing argument count so we can request the right
   DSA size and determine stack offset.  */

#define ACCUMULATE_OUTGOING_ARGS 1

/* Offset from the stack pointer register to an item dynamically
   allocated on the stack, e.g., by `alloca'.

   The default value for this macro is `STACK_POINTER_OFFSET' plus the
   length of the outgoing arguments.  The default is correct for most
   machines.  See `function.c' for details.  */
#define STACK_DYNAMIC_OFFSET(FUNDECL) (STARTING_FRAME_OFFSET)

/* Offset of first parameter from the argument pointer register value. 
   On the S/390, we define the argument pointer to the start of the fixed
   area.  */
#define FIRST_PARM_OFFSET(FNDECL) 0

/* Define this if stack space is still allocated for a parameter passed
   in a register.  The value is the number of bytes allocated to this
   area.  */
/* #define REG_PARM_STACK_SPACE(FNDECL)    32 */

/* Define this if the above stack space is to be considered part of the
   space allocated by the caller.  */
/* #define OUTGOING_REG_PARM_STACK_SPACE */

/* 1 if N is a possible register number for function argument passing.
   On S390, general registers 2 - 6 and floating point register 0 and 2
   are used in this way.  */

#define FUNCTION_ARG_REGNO_P(N) (((N) >=2 && (N) <7) || \
                                 (N) == 16 || (N) == 17)

/* Define a data type for recording info about an argument list during
   the scan of that argument list.  This data type should hold all
   necessary information about the function itself and about the args
   processed so far, enough to enable macros such as FUNCTION_ARG to
   determine where the next arg should go.  */

typedef struct s390_arg_structure
{
  int gprs;			/* gpr so far */
  int fprs;			/* fpr so far */
}
CUMULATIVE_ARGS;


/* Initialize a variable CUM of type CUMULATIVE_ARGS for a call to
   a function whose data type is FNTYPE.
   For a library call, FNTYPE is 0.  */

#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME, NN) \
  ((CUM).gprs=0, (CUM).fprs=0)

/* Update the data in CUM to advance over an argument of mode MODE and
   data type TYPE.  (TYPE is null for libcalls where that information
   may not be available.) */

#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED)                    \
  s390_function_arg_advance(&CUM, MODE, TYPE, NAMED)

/* Define where to put the arguments to a function.  Value is zero to push
   the argument on the stack, or a hard register in which to store the
   argument.  */

#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED)   \
  s390_function_arg(&CUM, MODE, TYPE, NAMED)

/* Define where to expect the arguments of a function.  Value is zero, if
   the argument is on the stack, or a hard register in which the argument
   is stored. It is the same like FUNCTION_ARG, except for unnamed args
   That means, that all in case of varargs used, the arguments are expected
   from the stack. 
   S/390 has already space on the stack for args coming in registers, 
   they are pushed in prologue, if needed.  */  


/* Define the `__builtin_va_list' type.  */

#define BUILD_VA_LIST_TYPE(VALIST) \
  (VALIST) = s390_build_va_list ()

/* Implement `va_start' for varargs and stdarg.  */

#define EXPAND_BUILTIN_VA_START(stdarg, valist, nextarg) \
  s390_va_start (stdarg, valist, nextarg)

/* Implement `va_arg'.  */

#define EXPAND_BUILTIN_VA_ARG(valist, type) \
  s390_va_arg (valist, type)

/* For an arg passed partly in registers and partly in memory, this is the
   number of registers used.  For args passed entirely in registers or
   entirely in memory, zero.  */

#define FUNCTION_ARG_PARTIAL_NREGS(CUM, MODE, TYPE, NAMED) 0


/* Define if returning from a function call automatically pops the
   arguments described by the number-of-args field in the call.  */

#define RETURN_POPS_ARGS(FUNDECL, FUNTYPE, SIZE) 0


/* Define how to find the value returned by a function.  VALTYPE is the
   data type of the value (as a tree).
   If the precise function being called is known, FUNC is its FUNCTION_DECL;
   otherwise, FUNC is 15.  */

#define RET_REG(MODE) ((GET_MODE_CLASS (MODE) == MODE_INT       \
                       || TARGET_SOFT_FLOAT ) ? 2 : 16)


/* for structs the address is passed, and the Callee makes a
   copy, only if needed */

#define FUNCTION_ARG_PASS_BY_REFERENCE(CUM, MODE, TYPE, NAMED) \
  s390_function_arg_pass_by_reference (MODE, TYPE)


/* Register 2 (and 3) for integral values
   or floating point register 0 (and 2) for fp values are used.  */

#define FUNCTION_VALUE(VALTYPE, FUNC)				\
  gen_rtx_REG ((INTEGRAL_TYPE_P (VALTYPE)			\
		&& TYPE_PRECISION (VALTYPE) < BITS_PER_WORD)	\
	       || POINTER_TYPE_P (VALTYPE)			\
	       ? word_mode : TYPE_MODE (VALTYPE),		\
	       TREE_CODE (VALTYPE) == REAL_TYPE && TARGET_HARD_FLOAT ? 16 : 2)

/* Define how to find the value returned by a library function assuming
   the value has mode MODE.  */

#define LIBCALL_VALUE(MODE)  gen_rtx (REG, MODE, RET_REG (MODE))

/* 1 if N is a possible register number for a function value.  */

#define FUNCTION_VALUE_REGNO_P(N) ((N) == 2 || (N) == 16)

/* The definition of this macro implies that there are cases where
   a scalar value cannot be returned in registers.  */

#define RETURN_IN_MEMORY(type)       		\
  (TYPE_MODE (type) == BLKmode || 		\
   TYPE_MODE (type) == DCmode  || 		\
   TYPE_MODE (type) == SCmode)

/* Mode of stack savearea.
   FUNCTION is VOIDmode because calling convention maintains SP.
   BLOCK needs Pmode for SP.
   NONLOCAL needs twice Pmode to maintain both backchain and SP.  */

#define STACK_SAVEAREA_MODE(LEVEL)      \
  (LEVEL == SAVE_FUNCTION ? VOIDmode    \
  : LEVEL == SAVE_NONLOCAL ? (TARGET_64BIT ? TImode : DImode) : Pmode)

/* Structure value address is passed as invisible first argument (gpr 2).  */

#define STRUCT_VALUE 0

/* This macro definition sets up a default value for `main' to return.  */

#define DEFAULT_MAIN_RETURN  c_expand_return (integer_zero_node)

/* Length in units of the trampoline for entering a nested function.  */

#define TRAMPOLINE_SIZE (TARGET_64BIT ? 36 : 20)

/* Initialize the dynamic part of trampoline.  */

#define INITIALIZE_TRAMPOLINE(ADDR, FNADDR, CXT)                       \
   s390_initialize_trampoline ((ADDR), (FNADDR), (CXT))

/* Template for constant part of trampoline.  */

#define TRAMPOLINE_TEMPLATE(FILE)                                       \
   s390_trampoline_template (FILE)

/* Output assembler code to FILE to increment profiler label # LABELNO
   for profiling a function entry.  */

#define FUNCTION_PROFILER(FILE, LABELNO) 			\
do {                                     			\
  extern rtx s390_profile[];  					\
  extern s390_pool_count;     					\
  rtx tmp;                                    			\
  static char label[128];                     			\
  fprintf (FILE, "# function profiler \n");   			\
  if (TARGET_64BIT) 						\
    {								\
      rtx tmp[1];						\
      output_asm_insn ("stg\t14,8(15)", tmp);			\
      sprintf (label, "%sP%d", LPREFIX, LABELNO);       	\
      tmp[0] = gen_rtx_SYMBOL_REF (Pmode, label);		\
      SYMBOL_REF_FLAG (tmp[0]) = 1;				\
      output_asm_insn ("larl\t1,%0", tmp);      		\
      tmp[0] = gen_rtx_SYMBOL_REF (Pmode, "_mcount");	        \
      if (flag_pic)						\
        {							\
          tmp[0] = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, tmp[0]), 113); \
          tmp[0] = gen_rtx_CONST (Pmode, tmp[0]);		\
        }							\
      output_asm_insn ("brasl\t14,%0", tmp);			\
      output_asm_insn ("lg\t14,8(15)", tmp);			\
    }								\
  else								\
    {  								\
      output_asm_insn ("l     14,4(15)", s390_profile);		\
      s390_pool_count = 0;                             		\
      output_asm_insn ("st    14,4(15)", s390_profile);		\
      output_asm_insn ("l     14,%4", s390_profile);		\
      output_asm_insn ("l     1,%9", s390_profile);		\
      if (flag_pic) 						\
	{   							\
	  output_asm_insn ("ar    1,13", s390_profile);   	\
	  output_asm_insn ("bas   14,0(14,13)", s390_profile); 	\
	} 							\
      else 							\
	{							\
	  output_asm_insn ("basr  14,14", s390_profile);	\
	}                  					\
      output_asm_insn ("l     14,4(15)", s390_profile);		\
    }                     					\
} while (0)

/* #define PROFILE_BEFORE_PROLOGUE */

/* There are three profiling modes for basic blocks available.
   The modes are selected at compile time by using the options
   -a or -ax of the gnu compiler.
   The variable `profile_block_flag' will be set according to the
   selected option.

   profile_block_flag == 0, no option used:

      No profiling done.

   profile_block_flag == 1, -a option used.

      Count frequency of execution of every basic block.

   profile_block_flag == 2, -ax option used.

      Generate code to allow several different profiling modes at run time. 
      Available modes are:
             Produce a trace of all basic blocks.
             Count frequency of jump instructions executed.
      In every mode it is possible to start profiling upon entering
      certain functions and to disable profiling of some other functions.

    The result of basic-block profiling will be written to a file `bb.out'.
    If the -ax option is used parameters for the profiling will be read
    from file `bb.in'.

*/

/* The following macro shall output assembler code to FILE
   to initialize basic-block profiling.

   If profile_block_flag == 2

	Output code to call the subroutine `__bb_init_trace_func'
	and pass two parameters to it. The first parameter is
	the address of a block allocated in the object module.
	The second parameter is the number of the first basic block
	of the function.

	The name of the block is a local symbol made with this statement:
	
	    ASM_GENERATE_INTERNAL_LABEL (BUFFER, "LPBX", 0);

	Of course, since you are writing the definition of
	`ASM_GENERATE_INTERNAL_LABEL' as well as that of this macro, you
	can take a short cut in the definition of this macro and use the
	name that you know will result.

	The number of the first basic block of the function is
	passed to the macro in BLOCK_OR_LABEL.

	If described in a virtual assembler language the code to be
	output looks like:

		parameter1 <- LPBX0
		parameter2 <- BLOCK_OR_LABEL
		call __bb_init_trace_func

    else if profile_block_flag != 0

	Output code to call the subroutine `__bb_init_func'
	and pass one single parameter to it, which is the same
	as the first parameter to `__bb_init_trace_func'.

	The first word of this parameter is a flag which will be nonzero if
	the object module has already been initialized.  So test this word
	first, and do not call `__bb_init_func' if the flag is nonzero.
	Note: When profile_block_flag == 2 the test need not be done
	but `__bb_init_trace_func' *must* be called.

	BLOCK_OR_LABEL may be used to generate a label number as a
	branch destination in case `__bb_init_func' will not be called.

	If described in a virtual assembler language the code to be
	output looks like:

		cmp (LPBX0),0
		jne local_label
		parameter1 <- LPBX0
		call __bb_init_func
local_label:

*/

#undef	FUNCTION_BLOCK_PROFILER
#define FUNCTION_BLOCK_PROFILER(FILE, BLOCK_OR_LABEL)			            \
do									            \
  {									            \
    if (TARGET_64BIT) 						                    \
      {								                    \
    	rtx tmp[1];	                                                            \
	fprintf (FILE, "# function block profiler %d \n", profile_block_flag);      \
	output_asm_insn ("ipm   0", tmp);              		    	            \
	output_asm_insn ("aghi  15,-224", tmp);                           	    \
	output_asm_insn ("stmg  14,5,160(15)", tmp);             		    \
	output_asm_insn ("larl  2,.LPBX0", tmp);               		    	    \
	switch (profile_block_flag) 					            \
	  {									    \
	  case 2:								    \
	    if (BLOCK_OR_LABEL < 0x10000) {                                         \
	      tmp[0] = gen_rtx_CONST_INT (Pmode, (BLOCK_OR_LABEL));                 \
	      output_asm_insn ("llill 3,%x0", tmp);                                 \
	    } else {                                         	     	            \
	      int bo = BLOCK_OR_LABEL;                                              \
	      tmp[0] = gen_rtx_CONST_INT (Pmode, bo&0x7fff);                        \
	      output_asm_insn ("llill 3,%x0", tmp);                                 \
	      tmp[0] = gen_rtx_CONST_INT (Pmode, (bo&0xffff0000)>>16);              \
	      output_asm_insn ("iilh 3,%x0", tmp);                                  \
 	    }                                                                       \
            tmp[0] = gen_rtx_SYMBOL_REF (Pmode, "__bb_init_trace_func");            \
            if (flag_pic)							    \
              {									    \
                tmp[0] = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, tmp[0]), 113); 	    \
                tmp[0] = gen_rtx_CONST (Pmode, tmp[0]);				    \
              }									    \
            output_asm_insn ("brasl\t14,%0", tmp);			            \
	    break;								    \
	  default:								    \
	    output_asm_insn ("cli   7(2),0", tmp);                       	    \
	    output_asm_insn ("jne   2f", tmp);	        		            \
            tmp[0] = gen_rtx_SYMBOL_REF (Pmode, "__bb_init_func");                  \
            if (flag_pic)							    \
              {									    \
                tmp[0] = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, tmp[0]), 113); 	    \
                tmp[0] = gen_rtx_CONST (Pmode, tmp[0]);				    \
              }									    \
            output_asm_insn ("brasl\t14,%0", tmp);			            \
	    break;								    \
	  }									    \
	output_asm_insn ("2:", tmp);                                                \
	output_asm_insn ("lmg   14,5,160(15)", tmp);                                \
	output_asm_insn ("aghi  15,224", tmp);    		                    \
	output_asm_insn ("spm   0", tmp);       				    \
      }								                    \
    else                                                                            \
      {								                    \
    	extern rtx s390_profile[];						    \
	fprintf (FILE, "# function block profiler %d \n", profile_block_flag);      \
	output_asm_insn ("ipm   0", s390_profile);     		    	            \
	output_asm_insn ("ahi   15,-128", s390_profile);                  	    \
	output_asm_insn ("stm   14,5,96(15)", s390_profile);     		    \
	output_asm_insn ("l     2,%6", s390_profile);     		    	    \
	if (flag_pic)							            \
	  output_asm_insn ("ar    2,13", s390_profile);			            \
	switch (profile_block_flag) 					            \
	  {									    \
	  case 2:								    \
	    output_asm_insn ("l     4,%1", s390_profile);		       	    \
	    if (BLOCK_OR_LABEL < 0x8000) {                                          \
	      s390_profile[8] = gen_rtx_CONST_INT (Pmode, (BLOCK_OR_LABEL));        \
	      output_asm_insn ("lhi   3,%8", s390_profile);                         \
	    } else {                                         	     	            \
	      int bo = BLOCK_OR_LABEL;                                              \
	      s390_profile[8] = gen_rtx_CONST_INT (Pmode, (bo&0xffff8000)>>15);     \
	      output_asm_insn ("lhi   3,%8", s390_profile);                         \
	      output_asm_insn ("sll   3,15", s390_profile);                         \
	      s390_profile[8] = gen_rtx_CONST_INT (Pmode, bo&0x7fff);               \
	      output_asm_insn ("ahi   3,%8", s390_profile);                         \
	    }                                                                       \
	    break;								    \
	  default:								    \
	    output_asm_insn ("l     4,%0", s390_profile);	      		    \
	    output_asm_insn ("cli   3(2),0", s390_profile);              	    \
	    output_asm_insn ("jne   2f", s390_profile);			            \
	    break;								    \
	  }									    \
	if (flag_pic)							            \
	  output_asm_insn ("bas   14,0(4,13)", s390_profile);	       	            \
	else								            \
	  output_asm_insn ("basr  14,4", s390_profile);	                            \
	output_asm_insn ("2:", s390_profile);                                       \
	output_asm_insn ("lm    14,5,96(15)", s390_profile);                        \
	output_asm_insn ("ahi   15,128", s390_profile);		                    \
	output_asm_insn ("spm   0", s390_profile);				    \
      }								                    \
  } while (0)

/* The following macro shall output assembler code to FILE
   to increment a counter associated with basic block number BLOCKNO.

   If profile_block_flag == 2

	Output code to initialize the global structure `__bb' and
	call the function `__bb_trace_func' which will increment the
	counter.

	`__bb' consists of two words. In the first word the number
	of the basic block has to be stored. In the second word
	the address of a block allocated in the object module 
	has to be stored.

	The basic block number is given by BLOCKNO.

	The address of the block is given by the label created with 

	    ASM_GENERATE_INTERNAL_LABEL (BUFFER, "LPBX", 0);

	by FUNCTION_BLOCK_PROFILER.

	Of course, since you are writing the definition of
	`ASM_GENERATE_INTERNAL_LABEL' as well as that of this macro, you
	can take a short cut in the definition of this macro and use the
	name that you know will result.

	If described in a virtual assembler language the code to be
	output looks like:

		move BLOCKNO -> (__bb)
		move LPBX0 -> (__bb+4)
		call __bb_trace_func

	Note that function `__bb_trace_func' must not change the
	machine state, especially the flag register. To grant
	this, you must output code to save and restore registers
	either in this macro or in the macros MACHINE_STATE_SAVE
	and MACHINE_STATE_RESTORE. The last two macros will be
	used in the function `__bb_trace_func', so you must make
	sure that the function prologue does not change any 
	register prior to saving it with MACHINE_STATE_SAVE.

   else if profile_block_flag != 0

	Output code to increment the counter directly.
	Basic blocks are numbered separately from zero within each
	compiled object module. The count associated with block number
	BLOCKNO is at index BLOCKNO in an array of words; the name of 
	this array is a local symbol made with this statement:

	    ASM_GENERATE_INTERNAL_LABEL (BUFFER, "LPBX", 2);

	Of course, since you are writing the definition of
	`ASM_GENERATE_INTERNAL_LABEL' as well as that of this macro, you
	can take a short cut in the definition of this macro and use the
	name that you know will result. 

	If described in a virtual assembler language the code to be
	output looks like:

		inc (LPBX2+4*BLOCKNO)

*/

#define BLOCK_PROFILER(FILE, BLOCKNO)		                	           \
do									           \
  {									           \
    if (TARGET_64BIT)				                                   \
      {						                                   \
    	rtx tmp[1];	                                                           \
	fprintf (FILE, "# block profiler %d block %d \n",                          \
			 profile_block_flag,BLOCKNO); 	                           \
	output_asm_insn ("ipm   14", tmp);              		           \
	output_asm_insn ("aghi  15,-224", tmp);                           	   \
	output_asm_insn ("stmg  14,5,160(15)", tmp);             		   \
	output_asm_insn ("larl  2,_bb", tmp);                      		   \
        if ((BLOCKNO*8) < 0x10000) {                                               \
	  tmp[0] = gen_rtx_CONST_INT (Pmode, (BLOCKNO*8));                         \
	  output_asm_insn ("llill 3,%x0", tmp);                                    \
	} else {                                              	                   \
	  int bo = BLOCKNO*8;                                                      \
	  tmp[0] = gen_rtx_CONST_INT (Pmode, bo&0xffff);                           \
	  output_asm_insn ("llill 3,%x0", tmp);                                    \
	  tmp[0] = gen_rtx_CONST_INT (Pmode, (bo&0xffff0000)>>16);                 \
	  output_asm_insn ("iilh 3,%x0", tmp);                                     \
	}                                                       	           \
    	switch (profile_block_flag) 					           \
	  {									   \
	  case 2:								   \
	    output_asm_insn ("stg   3,0(2)", tmp);              	     	   \
    	    output_asm_insn ("larl  3,.LPBX0", tmp);                   		   \
	    output_asm_insn ("stg   3,0(2)", tmp);              	     	   \
            tmp[0] = gen_rtx_SYMBOL_REF (Pmode, "__bb_trace_func");                \
            if (flag_pic)							   \
              {									   \
                tmp[0] = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, tmp[0]), 113); 	   \
                tmp[0] = gen_rtx_CONST (Pmode, tmp[0]);				   \
              }									   \
            output_asm_insn ("brasl\t14,%0", tmp);			           \
	    break;								   \
	  default:								   \
            output_asm_insn ("larl  2,.LPBX2", tmp);                   		   \
            output_asm_insn ("la    2,0(2,3)", tmp);                   		   \
	    output_asm_insn ("lg    3,0(2)", tmp);              	     	   \
    	    output_asm_insn ("aghi  3,1", tmp);                  	     	   \
    	    output_asm_insn ("stg   3,0(2)", tmp);              	     	   \
    	    break;								   \
         }					                                   \
	output_asm_insn ("lmg   14,5,160(15)", tmp);                               \
	output_asm_insn ("ahi   15,224", tmp);          	                   \
	output_asm_insn ("spm   14", tmp);          				   \
      }						                                   \
    else					                                   \
      {						                                   \
    	extern rtx s390_profile[];                                                 \
	fprintf (FILE, "# block profiler %d block %d \n",                          \
			 profile_block_flag,BLOCKNO); 	                           \
	output_asm_insn ("ipm   14", s390_profile);     		    	   \
	output_asm_insn ("ahi   15,-128", s390_profile);     	       	           \
	output_asm_insn ("stm   14,5,96(15)", s390_profile);     		   \
	switch (profile_block_flag) 					           \
	  {									   \
	  case 2:								   \
	    output_asm_insn ("l     4,%2", s390_profile);     	     	           \
	    output_asm_insn ("l     2,%5", s390_profile);     	     	           \
	    if (flag_pic)							   \
	      output_asm_insn ("ar    2,13", s390_profile);			   \
	    if (BLOCKNO < 0x8000) {                                                \
	      s390_profile[7] = gen_rtx_CONST_INT (Pmode, (BLOCKNO)*4);            \
	      output_asm_insn ("lhi   3,%8", s390_profile);                        \
	    } else {                                         	     	           \
	      int bo = BLOCKNO;                                                    \
	      s390_profile[8] = gen_rtx_CONST_INT (Pmode, (bo&0xffff8000)>>15);    \
	      output_asm_insn ("lhi   3,%8", s390_profile);                        \
	      output_asm_insn ("sll   3,15", s390_profile);                        \
	      s390_profile[8] = gen_rtx_CONST_INT (Pmode, bo&0x7fff);              \
	      output_asm_insn ("ahi   3,%7", s390_profile);                        \
	    }                                                                      \
	    output_asm_insn ("st    3,0(2)", s390_profile);     	     	   \
	    output_asm_insn ("mvc   0(4,2),%5", s390_profile);     	     	   \
	    if (flag_pic)							   \
	      output_asm_insn ("bas   14,0(4,13)", s390_profile);		   \
	    else								   \
	      output_asm_insn ("basr  14,4", s390_profile);	                   \
	    break;								   \
	  default:								   \
	    if (BLOCKNO < 0x2000) {                                                \
	      s390_profile[8] = gen_rtx_CONST_INT (Pmode, (BLOCKNO)*4);            \
	      output_asm_insn ("lhi   2,%8", s390_profile);                        \
	    } else {                                         	     	           \
	      int bo = BLOCKNO*4;                                                  \
	      s390_profile[8] = gen_rtx_CONST_INT (Pmode, (bo&0xffff8000)>>15);    \
	      output_asm_insn ("lhi   2,%8", s390_profile);                        \
	      output_asm_insn ("sll   2,15", s390_profile);                        \
	      s390_profile[8] = gen_rtx_CONST_INT (Pmode, bo&0x7fff);              \
	      output_asm_insn ("ahi   2,%8", s390_profile);                        \
	    }                                                                      \
	    output_asm_insn ("a     2,%7", s390_profile);     	     	           \
	    if (flag_pic)							   \
	      output_asm_insn ("l     3,0(2,13)", s390_profile);           	   \
	    else                                                                   \
	      output_asm_insn ("l     3,0(2)", s390_profile);     	     	   \
	    output_asm_insn ("ahi   3,1", s390_profile);         	     	   \
	    if (flag_pic)							   \
	      output_asm_insn ("st    3,0(2,13)", s390_profile);          	   \
	    else                                                                   \
	      output_asm_insn ("st    3,0(2)", s390_profile);     	     	   \
	    break;								   \
	  }									   \
	output_asm_insn ("lm    14,5,96(15)", s390_profile);                       \
	output_asm_insn ("ahi   15,128", s390_profile);		                   \
	output_asm_insn ("spm   14", s390_profile);				   \
     }                                                                             \
  } while (0)


/* The following macro shall output assembler code to FILE
   to indicate a return from function during basic-block profiling.

   If profiling_block_flag == 2:

	Output assembler code to call function `__bb_trace_ret'.

	Note that function `__bb_trace_ret' must not change the
	machine state, especially the flag register. To grant
	this, you must output code to save and restore registers
	either in this macro or in the macros MACHINE_STATE_SAVE_RET
	and MACHINE_STATE_RESTORE_RET. The last two macros will be
	used in the function `__bb_trace_ret', so you must make
	sure that the function prologue does not change any 
	register prior to saving it with MACHINE_STATE_SAVE_RET.

   else if profiling_block_flag != 0:

	The macro will not be used, so it need not distinguish
	these cases.
*/

#define FUNCTION_BLOCK_PROFILER_EXIT(FILE) \
do {                                                                       \
    if (TARGET_64BIT)				                           \
      {						                           \
    	rtx tmp[1];	                                                   \
	fprintf (FILE, "# block profiler exit \n");                        \
	output_asm_insn ("ipm   14", tmp);                                 \
	output_asm_insn ("aghi  15,-224", tmp);              	       	   \
	output_asm_insn ("stmg  14,5,160(15)", tmp);              	   \
        tmp[0] = gen_rtx_SYMBOL_REF (Pmode, "__bb_trace_ret");             \
        if (flag_pic)							   \
          {								   \
            tmp[0] = gen_rtx_UNSPEC (Pmode, gen_rtvec (1, tmp[0]), 113);   \
            tmp[0] = gen_rtx_CONST (Pmode, tmp[0]);			   \
          }								   \
    	output_asm_insn ("brasl 14,%0", tmp);                       	   \
	output_asm_insn ("lmg   14,5,160(15)", tmp);              	   \
	output_asm_insn ("aghi  15,224", tmp);              	       	   \
    	output_asm_insn ("spm   14", tmp);                                 \
      }                                                                    \
    else                                                                   \
      {                                                                    \
    	extern rtx s390_profile[];				           \
	fprintf (FILE, "# block profiler exit \n");                        \
	output_asm_insn ("ipm   14", s390_profile);     		   \
	output_asm_insn ("ahi   15,-128", s390_profile);     	       	   \
	output_asm_insn ("stm   14,5,96(15)", s390_profile);     	   \
	output_asm_insn ("l     4,%3", s390_profile);		      	   \
	if (flag_pic)							   \
	  output_asm_insn ("bas   14,0(4,13)", s390_profile);	           \
	else								   \
	  output_asm_insn ("basr  14,4", s390_profile);	                   \
	output_asm_insn ("lm    14,5,96(15)", s390_profile);               \
	output_asm_insn ("ahi   15,128", s390_profile);		           \
	output_asm_insn ("spm   14", s390_profile);			   \
     }                                                                     \
  } while (0)

/* The function `__bb_trace_func' is called in every basic block
   and is not allowed to change the machine state. Saving (restoring)
   the state can either be done in the BLOCK_PROFILER macro,
   before calling function (rsp. after returning from function)
   `__bb_trace_func', or it can be done inside the function by
   defining the macros:

	MACHINE_STATE_SAVE(ID)
	MACHINE_STATE_RESTORE(ID)

   In the latter case care must be taken, that the prologue code
   of function `__bb_trace_func' does not already change the
   state prior to saving it with MACHINE_STATE_SAVE.

   The parameter `ID' is a string identifying a unique macro use.

   On the s390 all save/restore is done in macros above
*/

/*
#define MACHINE_STATE_SAVE(ID) \
      fprintf (FILE, "\tahi   15,-128 # save state\n");			  \
      fprintf (FILE, "\tstm   14,5,96(15)\n");				  \

#define MACHINE_STATE_RESTORE(ID) \
      fprintf (FILE, "\tlm    14,5,96(15) # restore state\n");            \
      fprintf (FILE, "\tahi   15,128\n");		 		  \
*/


/* Define EXIT_IGNORE_STACK if, when returning from a function, the stack
   pointer does not matter (provided there is a frame pointer).  */

#define EXIT_IGNORE_STACK       1

/* Addressing modes, and classification of registers for them.  */

/* #define HAVE_POST_INCREMENT */
/* #define HAVE_POST_DECREMENT */

/* #define HAVE_PRE_DECREMENT */
/* #define HAVE_PRE_INCREMENT */

/* These assume that REGNO is a hard or pseudo reg number.  They give
   nonzero only if REGNO is a hard reg of the suitable class or a pseudo
   reg currently allocated to a suitable hard reg.
   These definitions are NOT overridden anywhere.  */

#define REGNO_OK_FOR_INDEX_P(REGNO)                                     \
  (((REGNO) > 0 && (REGNO) < 16) || (REGNO) == ARG_POINTER_REGNUM       \
   /* || (REGNO) == FRAME_POINTER_REGNUM */                                 \
    || (reg_renumber[REGNO] > 0 && reg_renumber[REGNO] < 16))

#define REGNO_OK_FOR_BASE_P(REGNO) REGNO_OK_FOR_INDEX_P (REGNO)

#define REGNO_OK_FOR_DATA_P(REGNO)                                      \
  ((REGNO) < 16 || (unsigned) reg_renumber[REGNO] < 16)

#define REGNO_OK_FOR_FP_P(REGNO)                                        \
  FLOAT_REGNO_P(REGNO)

/* Now macros that check whether X is a register and also,
   strictly, whether it is in a specified class.  */

/* 1 if X is a data register.  */

#define DATA_REG_P(X) (REG_P (X) && REGNO_OK_FOR_DATA_P (REGNO (X)))

/* 1 if X is an fp register.  */

#define FP_REG_P(X) (REG_P (X) && REGNO_OK_FOR_FP_P (REGNO (X)))

/* 1 if X is an address register.  */

#define ADDRESS_REG_P(X) (REG_P (X) && REGNO_OK_FOR_BASE_P (REGNO (X)))

/* Maximum number of registers that can appear in a valid memory address.  */

#define MAX_REGS_PER_ADDRESS 2

/* Recognize any constant value that is a valid address.  */

#define CONSTANT_ADDRESS_P(X) 0

#define SYMBOLIC_CONST(X)       \
(GET_CODE (X) == SYMBOL_REF                                             \
 || GET_CODE (X) == LABEL_REF                                           \
 || (GET_CODE (X) == CONST && symbolic_reference_mentioned_p (X)))

/* General operand is everything except SYMBOL_REF, CONST and CONST_DOUBLE
   they have to be forced to constant pool
   CONST_INT have to be forced into constant pool, if greater than
   64k. Depending on the insn they have to be force into constant pool
   for smaller value; in this case we have to work with nonimmediate operand.  */

#define LEGITIMATE_PIC_OPERAND_P(X)  \
      legitimate_pic_operand_p (X)

/* Nonzero if the constant value X is a legitimate general operand.
   It is given that X satisfies CONSTANT_P or is a CONST_DOUBLE.  */

#define LEGITIMATE_CONSTANT_P(X) \
     legitimate_constant_p (X)

/* The macros REG_OK_FOR..._P assume that the arg is a REG rtx and check
   its validity for a certain class.  We have two alternate definitions
   for each of them.  The usual definition accepts all pseudo regs; the
   other rejects them all.  The symbol REG_OK_STRICT causes the latter
   definition to be used.

   Most source files want to accept pseudo regs in the hope that they will
   get allocated to the class that the insn wants them to be in.
   Some source files that are used after register allocation
   need to be strict.  */

/* 
 * Nonzero if X is a hard reg that can be used as an index or if it is
 * a pseudo reg.  
 */

#define REG_OK_FOR_INDEX_NONSTRICT_P(X)               			\
((GET_MODE (X) == Pmode) &&						\
 ((REGNO (X) > 0 && REGNO (X) < 16) ||					\
  (REGNO (X) == ARG_POINTER_REGNUM) ||					\
  (REGNO (X) >= FIRST_PSEUDO_REGISTER)))

/* Nonzero if X is a hard reg that can be used as a base reg or if it is
   a pseudo reg.  */

#define REG_OK_FOR_BASE_NONSTRICT_P(X)    REG_OK_FOR_INDEX_NONSTRICT_P (X)

/* Nonzero if X is a hard reg that can be used as an index.  */

#define REG_OK_FOR_INDEX_STRICT_P(X) 				\
((GET_MODE (X) == Pmode) && (REGNO_OK_FOR_INDEX_P (REGNO (X))))

/* Nonzero if X is a hard reg that can be used as a base reg.  */

#define REG_OK_FOR_BASE_STRICT_P(X)				\
((GET_MODE (X) == Pmode) && (REGNO_OK_FOR_BASE_P (REGNO (X))))


#ifndef REG_OK_STRICT
#define REG_OK_FOR_INDEX_P(X)  REG_OK_FOR_INDEX_NONSTRICT_P(X)
#define REG_OK_FOR_BASE_P(X)   REG_OK_FOR_BASE_NONSTRICT_P(X)
#else
#define REG_OK_FOR_INDEX_P(X)  REG_OK_FOR_INDEX_STRICT_P(X)
#define REG_OK_FOR_BASE_P(X)   REG_OK_FOR_BASE_STRICT_P(X)
#endif


/* GO_IF_LEGITIMATE_ADDRESS recognizes an RTL expression that is a
   valid memory address for an instruction.
   The MODE argument is the machine mode for the MEM expression
   that wants to use this address.

   The other macros defined here are used only in GO_IF_LEGITIMATE_ADDRESS,
   except for CONSTANT_ADDRESS_P which is actually machine-independent.  */

#ifdef REG_OK_STRICT
#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, ADDR)                         \
{                                                                       \
  if (legitimate_address_p (MODE, X, 1))                                \
    goto ADDR;                                                          \
}
#else
#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, ADDR)                         \
{                                                                       \
  if (legitimate_address_p (MODE, X, 0))                                \
    goto ADDR;                                                          \
}
#endif


/* S/390 has no mode dependent addresses.  */

#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR, LABEL)

/* Try machine-dependent ways of modifying an illegitimate address
   to be legitimate.  If we find one, return the new, valid address.
   This macro is used in only one place: `memory_address' in explow.c.  */

#define LEGITIMIZE_ADDRESS(X, OLDX, MODE, WIN)                          \
{                                                                       \
  (X) = legitimize_address (X, OLDX, MODE);                             \
  if (memory_address_p (MODE, X))                                       \
    goto WIN;                                                           \
}

/* Specify the machine mode that this machine uses for the index in the
   tablejump instruction.  */

#define CASE_VECTOR_MODE (TARGET_64BIT ? DImode : SImode)

/* Define this if the tablejump instruction expects the table to contain
   offsets from the address of the table.
   Do not define this if the table should contain absolute addresses.  */

/* #define CASE_VECTOR_PC_RELATIVE */

/* Load from integral MODE < SI from memory into register makes sign_extend
   or zero_extend  
   In our case sign_extension happens for Halfwords, other no extension.  */

#define LOAD_EXTEND_OP(MODE) 					\
(TARGET_64BIT ? ((MODE) == QImode ? ZERO_EXTEND :               \
                 (MODE) == HImode ? SIGN_EXTEND : NIL)          \
              : ((MODE) == HImode ? SIGN_EXTEND : NIL))

/* Specify the tree operation to be used to convert reals to integers.  */

#define IMPLICIT_FIX_EXPR FIX_ROUND_EXPR

/* Define this if fixuns_trunc is the same as fix_trunc.  */

/* #define FIXUNS_TRUNC_LIKE_FIX_TRUNC */

/* We use "unsigned char" as default.  */

#define DEFAULT_SIGNED_CHAR 0

/* This is the kind of divide that is easiest to do in the general case.  */

#define EASY_DIV_EXPR TRUNC_DIV_EXPR

/* Max number of bytes we can move from memory to memory in one reasonably
   fast instruction.  */

#define MOVE_MAX 256

/* Define this if zero-extension is slow (more than one real instruction).  */

#define SLOW_ZERO_EXTEND

/* Nonzero if access to memory by bytes is slow and undesirable.  */

#define SLOW_BYTE_ACCESS 1

/* Define if shifts truncate the shift count which implies one can omit
   a sign-extension or zero-extension of a shift count.  */

/* #define SHIFT_COUNT_TRUNCATED */

/* Value is 1 if truncating an integer of INPREC bits to OUTPREC bits
   is done just by pretending it is already truncated.  */

#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC)  1

/* We assume that the store-condition-codes instructions store 0 for false
   and some other value for true.  This is the value stored for true.  */

/* #define STORE_FLAG_VALUE -1 */

/* When a prototype says `char' or `short', really pass an `int'.  */

#define PROMOTE_PROTOTYPES 1

/* Don't perform CSE on function addresses.  */

#define NO_FUNCTION_CSE

/* Specify the machine mode that pointers have.
   After generation of rtl, the compiler makes no further distinction
   between pointers and any other objects of this machine mode.  */

#define Pmode (TARGET_64BIT ? DImode : SImode)

/* A function address in a call instruction is a byte address (for
   indexing purposes) so give the MEM rtx a byte's mode.  */

#define FUNCTION_MODE QImode


/* A part of a C `switch' statement that describes the relative costs
   of constant RTL expressions.  It must contain `case' labels for
   expression codes `const_int', `const', `symbol_ref', `label_ref'
   and `const_double'.  Each case must ultimately reach a `return'
   statement to return the relative cost of the use of that kind of
   constant value in an expression.  The cost may depend on the
   precise value of the constant, which is available for examination
   in X, and the rtx code of the expression in which it is contained,
   found in OUTER_CODE.

   CODE is the expression code--redundant, since it can be obtained
   with `GET_CODE (X)'.  */
/* Force_const_mem does not work out of reload, because the saveable_obstack
   is set to reload_obstack, which does not live long enough. 
   Because of this we cannot use force_const_mem in addsi3.
   This leads to problems with gen_add2_insn with a constant greater
   than a short. Because of that we give a addition of greater
   constants a cost of 3 (reload1.c 10096).  */


#define CONST_COSTS(RTX, CODE, OUTER_CODE)                      \
  case CONST:                                                   \
    if ((GET_CODE (XEXP (RTX, 0)) == MINUS) &&                  \
	(GET_CODE (XEXP (XEXP (RTX, 0), 1)) != CONST_INT))      \
     return 1000;                                               \
  case CONST_INT:                                               \
       if ((OUTER_CODE == PLUS) &&                              \
	   ((INTVAL (RTX) > 32767) ||                           \
	   (INTVAL (RTX) < -32768))) 	                        \
         return 3;                                              \
  case LABEL_REF:                                               \
  case SYMBOL_REF:                                              \
  case CONST_DOUBLE:                                            \
    return 1;                                                   \


/* Like `CONST_COSTS' but applies to nonconstant RTL expressions.
   This can be used, for example, to indicate how costly a multiply
   instruction is.  In writing this macro, you can use the construct
   `COSTS_N_INSNS (N)' to specify a cost equal to N fast
   instructions.  OUTER_CODE is the code of the expression in which X
   is contained.

   This macro is optional; do not define it if the default cost
   assumptions are adequate for the target machine.  */

#define RTX_COSTS(X, CODE, OUTER_CODE)                                  \
  case ASHIFT:                                                          \
  case ASHIFTRT:                                                        \
  case LSHIFTRT:                                                        \
  case PLUS:                                                            \
  case AND:                                                             \
  case IOR:                                                             \
  case XOR:                                                             \
  case MINUS:                                                           \
  case NEG:                                                             \
  case NOT:                                                             \
          return 1;                                                     \
  case MULT:                                                            \
    if (GET_MODE (XEXP (X, 0)) == DImode)                               \
      return 40;                                                        \
        else                                                            \
      return 7;                                                         \
  case DIV:                                                             \
  case UDIV:                                                            \
  case MOD:                                                             \
  case UMOD:                                                            \
          return 33;


/* An expression giving the cost of an addressing mode that contains
   ADDRESS.  If not defined, the cost is computed from the ADDRESS
   expression and the `CONST_COSTS' values.

   For most CISC machines, the default cost is a good approximation
   of the true cost of the addressing mode.  However, on RISC
   machines, all instructions normally have the same length and
   execution time.  Hence all addresses will have equal costs.

   In cases where more than one form of an address is known, the form
   with the lowest cost will be used.  If multiple forms have the
   same, lowest, cost, the one that is the most complex will be used.

   For example, suppose an address that is equal to the sum of a
   register and a constant is used twice in the same basic block.
   When this macro is not defined, the address will be computed in a
   register and memory references will be indirect through that
   register.  On machines where the cost of the addressing mode
   containing the sum is no higher than that of a simple indirect
   reference, this will produce an additional instruction and
   possibly require an additional register.  Proper specification of
   this macro eliminates this overhead for such machines.

   Similar use of this macro is made in strength reduction of loops.

   ADDRESS need not be valid as an address.  In such a case, the cost
   is not relevant and can be any value; invalid addresses need not be
   assigned a different cost.

   On machines where an address involving more than one register is as
   cheap as an address computation involving only one register,
   defining `ADDRESS_COST' to reflect this can cause two registers to
   be live over a region of code where only one would have been if
   `ADDRESS_COST' were not defined in that manner.  This effect should
   be considered in the definition of this macro.  Equivalent costs
   should probably only be given to addresses with different numbers
   of registers on machines with lots of registers.

   This macro will normally either not be defined or be defined as a
   constant.

   On s390 symbols are expensive if compiled with fpic
   lifetimes.  */

#define ADDRESS_COST(RTX) \
  ((flag_pic && GET_CODE (RTX) == SYMBOL_REF) ? 2 : 1)

/* On s390, copy between fprs and gprs is expensive.  */

#define REGISTER_MOVE_COST(MODE, CLASS1, CLASS2)                        \
  (((CLASS1 != CLASS2) &&                                               \
   (CLASS1 == FP_REGS || CLASS2 == FP_REGS)) ? 10 : 1)


/* A C expression for the cost of moving data of mode M between a
   register and memory.  A value of 2 is the default; this cost is
   relative to those in `REGISTER_MOVE_COST'.

   If moving between registers and memory is more expensive than
   between two registers, you should define this macro to express the
   relative cost.  */

#define MEMORY_MOVE_COST(M, C, I) 1

/* A C expression for the cost of a branch instruction.  A value of 1
   is the default; other values are interpreted relative to that.  */

#define BRANCH_COST 1

/* Add any extra modes needed to represent the condition code. */
#define EXTRA_CC_MODES \
	CC (CCZmode, "CCZ") \
	CC (CCAmode, "CCA") \
	CC (CCUmode, "CCU") \
	CC (CCSmode, "CCS") \
	CC (CCTmode, "CCT")
 
/* Given a comparison code (EQ, NE, etc.) and the first operand of a COMPARE,
   return the mode to be used for the comparison. */
 
#define SELECT_CC_MODE(OP,X,Y)              \
 (   (OP) == EQ  || (OP) == NE  ? CCZmode   \
   : (OP) == LE  || (OP) == LT  ||          \
     (OP) == GE  || (OP) == GT  ? CCSmode   \
   : (OP) == LEU || (OP) == LTU ||          \
     (OP) == GEU || (OP) == GTU ? CCUmode   \
   : CCmode )
 
 
/* Define the information needed to generate branch and scc insns.  This is
   stored from the compare operation.  Note that we can't use "rtx" here
   since it hasn't been defined!  */
 
extern struct rtx_def *s390_compare_op0, *s390_compare_op1;
 
extern int s390_match_ccmode PARAMS ((struct rtx_def *, int));


/* How to refer to registers in assembler output.  This sequence is
   indexed by compiler's hard-register-number (see above).  */

#define REGISTER_NAMES							\
{ "%r0",  "%r1",  "%r2",  "%r3",  "%r4",  "%r5",  "%r6",  "%r7",	\
  "%r8",  "%r9", "%r10", "%r11", "%r12", "%r13", "%r14", "%r15",	\
  "%f0",  "%f2",  "%f4",  "%f6",  "%f1",  "%f3",  "%f5",  "%f7",	\
  "%f8",  "%f10", "%f12", "%f14", "%f9", "%f11", "%f13", "%f15",	\
  "%ap", "%cc"								\
}

/* implicit call of memcpy, not bcopy   */

#define TARGET_MEM_FUNCTIONS

/* Print operand X (an rtx) in assembler syntax to file FILE.
   CODE is a letter or dot (`z' in `%z0') or 0 if no letter was specified.
   For `%' followed by punctuation, CODE is the punctuation and X is null.  */

#define PRINT_OPERAND(FILE, X, CODE) print_operand (FILE, X, CODE)

#define PRINT_OPERAND_ADDRESS(FILE, ADDR) print_operand_address (FILE, ADDR)


/* Define the codes that are matched by predicates in aux-output.c.  */

#define PREDICATE_CODES                                                 \
  {"s_operand",       { MEM }},                                          \
  {"bras_sym_operand",{ SYMBOL_REF, CONST }},                            \
  {"r_or_s_operand",  { MEM, SUBREG, REG }},                             \
  {"r_or_im8_operand",  { CONST_INT, SUBREG, REG }},                     \
  {"r_or_s_or_im8_operand",  { MEM, SUBREG, REG, CONST_INT }},           \
  {"r_or_x_or_im16_operand", { MEM, SUBREG, REG, CONST_INT }},           \
  {"const0_operand", { CONST_INT, CONST_DOUBLE }},	                 \
  {"const1_operand", { CONST_INT, CONST_DOUBLE }},	                 \
  {"tmxx_operand", { CONST_INT, MEM }},


/* A C statement (sans semicolon) to update the integer variable COST
   based on the relationship between INSN that is dependent on
   DEP_INSN through the dependence LINK.  The default is to make no
   adjustment to COST.  This can be used for example to specify to
   the scheduler that an output- or anti-dependence does not incur
   the same cost as a data-dependence.  */

#define ADJUST_COST(insn, link, dep_insn, cost) \
  (cost) = s390_adjust_cost (insn, link, dep_insn, cost)


/* Constant Pool for all symbols operands which are changed with
   force_const_mem during insn generation (expand_insn).  */

extern struct rtx_def *s390_pool_start_insn;
extern int s390_pool_count;
extern int s390_nr_constants;

/* Function is splitted in chunk, if literal pool could overflow
   Value need to be lowered, if problems with displacement overflow.  */

#define S390_REL_MAX 55000
#define S390_CHUNK_MAX 0x2000
#define S390_CHUNK_OV 0x8000
#define S390_POOL_MAX 0xe00

#define ASM_OUTPUT_POOL_PROLOGUE(FILE, FUNNAME, fndecl, size)  	        \
{								       	\
  register rtx insn;						       	\
  struct pool_constant *pool;					       	\
								        \
    if (s390_pool_count == -1)                                        	\
     {							                \
       s390_nr_constants = 0;				                \
       for (pool = first_pool; pool; pool = pool->next)	                \
	 if (pool->mark) s390_nr_constants++;		                \
       return;                                      	                \
     }                                                                  \
    if (first_pool == 0) {                                              \
      s390_asm_output_pool_prologue (FILE, FUNNAME, fndecl, size);    	\
      return;							      	\
    }								       	\
    for (pool = first_pool; pool; pool = pool->next)		       	\
      pool->mark = 0;						       	\
  								       	\
    insn = s390_pool_start_insn;				       	\
  								       	\
    if (insn==NULL_RTX)	 					       	\
      insn = get_insns ();		 		                \
    else		       			                        \
      insn = NEXT_INSN (insn);		                                \
    for (; insn; insn = NEXT_INSN (insn)) {    		                \
      if (GET_RTX_CLASS (GET_CODE (insn)) == 'i') {		       	\
        if (s390_stop_dump_lit_p (insn)) { 	       		       	\
	  mark_constants (PATTERN (insn));			       	\
	  break;						       	\
        } else							       	\
	  mark_constants (PATTERN (insn));			       	\
      }								       	\
    }								       	\
								       	\
    /* Mark entries referenced by other entries */			\
    for (pool = first_pool; pool; pool = pool->next)		       	\
      if (pool->mark)							\
        mark_constants(pool->constant);					\
								       	\
    s390_asm_output_pool_prologue (FILE, FUNNAME, fndecl, size);     	\
}

/* We need to return, because otherwise the pool is deleted of the 
   constant pool after the first output.  */

#define ASM_OUTPUT_POOL_EPILOGUE(FILE, FUNNAME, fndecl, size) return;

#define ASM_OUTPUT_SPECIAL_POOL_ENTRY(FILE, EXP, MODE, ALIGN, LABELNO, WIN) \
{                                                                       \
  if ((s390_pool_count == 0) || (s390_pool_count > 0 && LABELNO >= 0)) 	\
    {									\
      fprintf (FILE, ".LC%d:\n", LABELNO);                              \
      LABELNO = ~LABELNO;                                               \
    }                                                                   \
  if (s390_pool_count > 0)						\
    {									\
      fprintf (FILE, ".LC%d_%X:\n", ~LABELNO, s390_pool_count);         \
    }                            					\
									\
  /* Output the value of the constant itself.  */			\
  switch (GET_MODE_CLASS (pool->mode))					\
    {									\
    case MODE_FLOAT:							\
      if (GET_CODE (x) != CONST_DOUBLE)					\
	abort ();							\
      									\
      memcpy ((char *) &u, (char *) &CONST_DOUBLE_LOW (x), sizeof u);	\
      assemble_real (u.d, pool->mode);					\
      break;								\
      									\
    case MODE_INT:							\
    case MODE_PARTIAL_INT:						\
      if (flag_pic && (GET_CODE (x) == CONST ||                         \
		   GET_CODE (x) == SYMBOL_REF ||                        \
                   GET_CODE (x) == LABEL_REF ))                         \
        {								\
          fprintf (FILE, "%s\t",TARGET_64BIT ? ASM_QUAD : ASM_LONG);	\
          s390_output_symbolic_const (FILE, x); 			\
          fputc ('\n', (FILE));						\
	}                                                               \
      else                                                              \
        assemble_integer (x, GET_MODE_SIZE (pool->mode), 1);		\
      break;								\
      									\
    default:								\
      abort ();								\
    }									\
  goto WIN;								\
}

#endif 
