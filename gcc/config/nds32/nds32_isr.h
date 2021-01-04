/* Intrinsic definitions of Andes NDS32 cpu for GNU compiler
   Copyright (C) 2012-2021 Free Software Foundation, Inc.
   Contributed by Andes Technology Corporation.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   Under Section 7 of GPL version 3, you are granted additional
   permissions described in the GCC Runtime Library Exception, version
   3.1, as published by the Free Software Foundation.

   You should have received a copy of the GNU General Public License and
   a copy of the GCC Runtime Library Exception along with this program;
   see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
   <http://www.gnu.org/licenses/>.  */

#ifndef _NDS32_ISR_H
#define _NDS32_ISR_H

/* Attribute of a interrupt or exception handler:

   NDS32_READY_NESTED: This handler is interruptible if user re-enable GIE bit.
   NDS32_NESTED      : This handler is interruptible.  This is not suitable
                       exception handler.
   NDS32_NOT_NESTED  : This handler is NOT interruptible.  Users have to do
                       some work if nested is wanted
   NDS32_CRITICAL    : This handler is critical ISR, which means it is small
                       and efficient.  */
#define NDS32_READY_NESTED   0
#define NDS32_NESTED         1
#define NDS32_NOT_NESTED     2
#define NDS32_CRITICAL       3

/* Attribute of a interrupt or exception handler:

   NDS32_SAVE_ALL_REGS    : Save all registers in a table.
   NDS32_SAVE_PARTIAL_REGS: Save partial registers.  */
#define NDS32_SAVE_CALLER_REGS   0
#define NDS32_SAVE_ALL_REGS      1

/* There are two version of Register table for interrupt and exception handler,
   one for 16-register CPU the other for 32-register CPU.  These structures are
   used for context switching or system call handling.  The address of this
   data can be get from the input argument of the handler functions.

   For system call handling, r0 to r5 are used to pass arguments.  If more
   arguments are used they are put into the stack and its starting address is
   in sp.  Return value of system call can be put into r0 and r1 upon exit from
   system call handler.  System call ID is in a system register and it can be
   fetched via intrinsic function.  For more information please read ABI and
   other related documents.

   For context switching, at least 2 values need to saved in kernel.  One is
   IPC and the other is the stack address of current task.  Use intrinsic
   function to get IPC and  the input argument of the handler functions + 8 to
   get stack address of current task.  To do context switching, you replace
   new_sp with the stack address of new task and replace IPC system register
   with IPC of new task, then, just return from handler.  The context switching
   will happen.  */

/* Register table for exception handler; 32-register version.  */
typedef struct
{
  int r0;
  int r1;
  int r2;
  int r3;
  int r4;
  int r5;
  int r6;
  int r7;
  int r8;
  int r9;
  int r10;
  int r11;
  int r12;
  int r13;
  int r14;
  int r15;
  int r16;
  int r17;
  int r18;
  int r19;
  int r20;
  int r21;
  int r22;
  int r23;
  int r24;
  int r25;
  int r26;
  int r27;
  int fp;
  int gp;
  int lp;
  int sp;
} NDS32_GPR32;

/* Register table for exception handler; 16-register version.  */
typedef struct
{
  int r0;
  int r1;
  int r2;
  int r3;
  int r4;
  int r5;
  int r6;
  int r7;
  int r8;
  int r9;
  int r10;
  int r15;
  int fp;
  int gp;
  int lp;
  int sp;
} NDS32_GPR16;


/* Use NDS32_REG32_TAB or NDS32_REG16_TAB in your program to
   access register table.  */
typedef struct
{
  union
    {
      int          reg_a[32] ;
      NDS32_GPR32  reg_s ;
    } u ;
} NDS32_REG32_TAB;

typedef struct
{
  union
    {
      int          reg_a[16] ;
      NDS32_GPR16  reg_s ;
    } u ;
} NDS32_REG16_TAB;

typedef struct
{
  int    d0lo;
  int    d0hi;
  int    d1lo;
  int    d1hi;
} NDS32_DX_TAB;

typedef struct
{
#ifdef __NDS32_EB__
  float    fsr0;
  float    fsr1;
  float    fsr2;
  float    fsr3;
  float    fsr4;
  float    fsr5;
  float    fsr6;
  float    fsr7;
#else
  float    fsr1;
  float    fsr0;
  float    fsr3;
  float    fsr2;
  float    fsr5;
  float    fsr4;
  float    fsr7;
  float    fsr6;
#endif
} NDS32_FSR8;

typedef struct
{
  double   dsr0;
  double   dsr1;
  double   dsr2;
  double   dsr3;
} NDS32_DSR4;

typedef struct
{
#ifdef __NDS32_EB__
  float    fsr0;
  float    fsr1;
  float    fsr2;
  float    fsr3;
  float    fsr4;
  float    fsr5;
  float    fsr6;
  float    fsr7;
  float    fsr8;
  float    fsr9;
  float    fsr10;
  float    fsr11;
  float    fsr12;
  float    fsr13;
  float    fsr14;
  float    fsr15;
#else
  float    fsr1;
  float    fsr0;
  float    fsr3;
  float    fsr2;
  float    fsr5;
  float    fsr4;
  float    fsr7;
  float    fsr6;
  float    fsr9;
  float    fsr8;
  float    fsr11;
  float    fsr10;
  float    fsr13;
  float    fsr12;
  float    fsr15;
  float    fsr14;
#endif
} NDS32_FSR16;

typedef struct
{
  double   dsr0;
  double   dsr1;
  double   dsr2;
  double   dsr3;
  double   dsr4;
  double   dsr5;
  double   dsr6;
  double   dsr7;
} NDS32_DSR8;

typedef struct
{
#ifdef __NDS32_EB__
  float    fsr0;
  float    fsr1;
  float    fsr2;
  float    fsr3;
  float    fsr4;
  float    fsr5;
  float    fsr6;
  float    fsr7;
  float    fsr8;
  float    fsr9;
  float    fsr10;
  float    fsr11;
  float    fsr12;
  float    fsr13;
  float    fsr14;
  float    fsr15;
  float    fsr16;
  float    fsr17;
  float    fsr18;
  float    fsr19;
  float    fsr20;
  float    fsr21;
  float    fsr22;
  float    fsr23;
  float    fsr24;
  float    fsr25;
  float    fsr26;
  float    fsr27;
  float    fsr28;
  float    fsr29;
  float    fsr30;
  float    fsr31;
#else
  float    fsr1;
  float    fsr0;
  float    fsr3;
  float    fsr2;
  float    fsr5;
  float    fsr4;
  float    fsr7;
  float    fsr6;
  float    fsr9;
  float    fsr8;
  float    fsr11;
  float    fsr10;
  float    fsr13;
  float    fsr12;
  float    fsr15;
  float    fsr14;
  float    fsr17;
  float    fsr16;
  float    fsr19;
  float    fsr18;
  float    fsr21;
  float    fsr20;
  float    fsr23;
  float    fsr22;
  float    fsr25;
  float    fsr24;
  float    fsr27;
  float    fsr26;
  float    fsr29;
  float    fsr28;
  float    fsr31;
  float    fsr30;
#endif
} NDS32_FSR32;

typedef struct
{
  double   dsr0;
  double   dsr1;
  double   dsr2;
  double   dsr3;
  double   dsr4;
  double   dsr5;
  double   dsr6;
  double   dsr7;
  double   dsr8;
  double   dsr9;
  double   dsr10;
  double   dsr11;
  double   dsr12;
  double   dsr13;
  double   dsr14;
  double   dsr15;
} NDS32_DSR16;

typedef struct
{
  double   dsr0;
  double   dsr1;
  double   dsr2;
  double   dsr3;
  double   dsr4;
  double   dsr5;
  double   dsr6;
  double   dsr7;
  double   dsr8;
  double   dsr9;
  double   dsr10;
  double   dsr11;
  double   dsr12;
  double   dsr13;
  double   dsr14;
  double   dsr15;
  double   dsr16;
  double   dsr17;
  double   dsr18;
  double   dsr19;
  double   dsr20;
  double   dsr21;
  double   dsr22;
  double   dsr23;
  double   dsr24;
  double   dsr25;
  double   dsr26;
  double   dsr27;
  double   dsr28;
  double   dsr29;
  double   dsr30;
  double   dsr31;
} NDS32_DSR32;

typedef struct
{
  union
    {
      NDS32_FSR8   fsr_s ;
      NDS32_DSR4   dsr_s ;
    } u ;
} NDS32_FPU8_TAB;

typedef struct
{
  union
    {
      NDS32_FSR16  fsr_s ;
      NDS32_DSR8   dsr_s ;
    } u ;
} NDS32_FPU16_TAB;

typedef struct
{
  union
    {
      NDS32_FSR32  fsr_s ;
      NDS32_DSR16  dsr_s ;
    } u ;
} NDS32_FPU32_TAB;

typedef struct
{
  union
    {
      NDS32_FSR32  fsr_s ;
      NDS32_DSR32  dsr_s ;
    } u ;
} NDS32_FPU64_TAB;

typedef struct
{
  int    ipc;
  int    ipsw;
#if defined(NDS32_EXT_FPU_CONFIG_0)
  NDS32_FPU8_TAB fpr;
#elif defined(NDS32_EXT_FPU_CONFIG_1)
  NDS32_FPU16_TAB fpr;
#elif defined(NDS32_EXT_FPU_CONFIG_2)
  NDS32_FPU32_TAB fpr;
#elif defined(NDS32_EXT_FPU_CONFIG_3)
  NDS32_FPU64_TAB fpr;
#endif
#if __NDS32_DX_REGS__
  NDS32_DX_TAB dxr;
#endif
#if __NDS32_EXT_IFC__
  int    ifc_lp;
  int    filler;
#endif
#if __NDS32_REDUCED_REGS__ || __NDS32_REDUCE_REGS
  NDS32_REG16_TAB gpr;
#else
  NDS32_REG32_TAB gpr;
#endif
} NDS32_CONTEXT;

/* Predefined Vector Definition.

   For IVIC Mode: 9 to 14 are for hardware interrupt
                  and 15 is for software interrupt.
   For EVIC Mode: 9 to 72 are for hardware interrupt
                  and software interrupt can be routed to any one of them.

   You may want to define your hardware interrupts in the following way
   for easy maintainance.

     IVIC mode:
       #define MY_HW_IVIC_TIMER NDS32_VECTOR_INTERRUPT_HW0 + 1
       #define MY_HW_IVIC_USB   NDS32_VECTOR_INTERRUPT_HW0 + 3
     EVIC mode:
     #define MY_HW_EVIC_DMA   NDS32_VECTOR_INTERRUPT_HW0 + 2
     #define MY_HW_EVIC_SWI   NDS32_VECTOR_INTERRUPT_HW0 + 10 */
#define NDS32_VECTOR_RESET               0
#define NDS32_VECTOR_TLB_FILL            1
#define NDS32_VECTOR_PTE_NOT_PRESENT     2
#define NDS32_VECTOR_TLB_MISC            3
#define NDS32_VECTOR_TLB_VLPT_MISS       4
#define NDS32_VECTOR_MACHINE_ERROR       5
#define NDS32_VECTOR_DEBUG_RELATED       6
#define NDS32_VECTOR_GENERAL_EXCEPTION   7
#define NDS32_VECTOR_SYSCALL             8
#define NDS32_VECTOR_INTERRUPT_HW0       9
#define NDS32_VECTOR_INTERRUPT_HW1       10
#define NDS32_VECTOR_INTERRUPT_HW2       11
#define NDS32_VECTOR_INTERRUPT_HW3       12
#define NDS32_VECTOR_INTERRUPT_HW4       13
#define NDS32_VECTOR_INTERRUPT_HW5       14
#define NDS32_VECTOR_INTERRUPT_HW6       15
#define NDS32_VECTOR_SWI                 15  /* THIS IS FOR IVIC MODE ONLY */
#define NDS32_VECTOR_INTERRUPT_HW7       16
#define NDS32_VECTOR_INTERRUPT_HW8       17
#define NDS32_VECTOR_INTERRUPT_HW9       18
#define NDS32_VECTOR_INTERRUPT_HW10      19
#define NDS32_VECTOR_INTERRUPT_HW11      20
#define NDS32_VECTOR_INTERRUPT_HW12      21
#define NDS32_VECTOR_INTERRUPT_HW13      22
#define NDS32_VECTOR_INTERRUPT_HW14      23
#define NDS32_VECTOR_INTERRUPT_HW15      24
#define NDS32_VECTOR_INTERRUPT_HW16      25
#define NDS32_VECTOR_INTERRUPT_HW17      26
#define NDS32_VECTOR_INTERRUPT_HW18      27
#define NDS32_VECTOR_INTERRUPT_HW19      28
#define NDS32_VECTOR_INTERRUPT_HW20      29
#define NDS32_VECTOR_INTERRUPT_HW21      30
#define NDS32_VECTOR_INTERRUPT_HW22      31
#define NDS32_VECTOR_INTERRUPT_HW23      32
#define NDS32_VECTOR_INTERRUPT_HW24      33
#define NDS32_VECTOR_INTERRUPT_HW25      34
#define NDS32_VECTOR_INTERRUPT_HW26      35
#define NDS32_VECTOR_INTERRUPT_HW27      36
#define NDS32_VECTOR_INTERRUPT_HW28      37
#define NDS32_VECTOR_INTERRUPT_HW29      38
#define NDS32_VECTOR_INTERRUPT_HW30      39
#define NDS32_VECTOR_INTERRUPT_HW31      40
#define NDS32_VECTOR_INTERRUPT_HW32      41
#define NDS32_VECTOR_INTERRUPT_HW33      42
#define NDS32_VECTOR_INTERRUPT_HW34      43
#define NDS32_VECTOR_INTERRUPT_HW35      44
#define NDS32_VECTOR_INTERRUPT_HW36      45
#define NDS32_VECTOR_INTERRUPT_HW37      46
#define NDS32_VECTOR_INTERRUPT_HW38      47
#define NDS32_VECTOR_INTERRUPT_HW39      48
#define NDS32_VECTOR_INTERRUPT_HW40      49
#define NDS32_VECTOR_INTERRUPT_HW41      50
#define NDS32_VECTOR_INTERRUPT_HW42      51
#define NDS32_VECTOR_INTERRUPT_HW43      52
#define NDS32_VECTOR_INTERRUPT_HW44      53
#define NDS32_VECTOR_INTERRUPT_HW45      54
#define NDS32_VECTOR_INTERRUPT_HW46      55
#define NDS32_VECTOR_INTERRUPT_HW47      56
#define NDS32_VECTOR_INTERRUPT_HW48      57
#define NDS32_VECTOR_INTERRUPT_HW49      58
#define NDS32_VECTOR_INTERRUPT_HW50      59
#define NDS32_VECTOR_INTERRUPT_HW51      60
#define NDS32_VECTOR_INTERRUPT_HW52      61
#define NDS32_VECTOR_INTERRUPT_HW53      62
#define NDS32_VECTOR_INTERRUPT_HW54      63
#define NDS32_VECTOR_INTERRUPT_HW55      64
#define NDS32_VECTOR_INTERRUPT_HW56      65
#define NDS32_VECTOR_INTERRUPT_HW57      66
#define NDS32_VECTOR_INTERRUPT_HW58      67
#define NDS32_VECTOR_INTERRUPT_HW59      68
#define NDS32_VECTOR_INTERRUPT_HW60      69
#define NDS32_VECTOR_INTERRUPT_HW61      70
#define NDS32_VECTOR_INTERRUPT_HW62      71
#define NDS32_VECTOR_INTERRUPT_HW63      72

#define NDS32ATTR_RESET(option)          __attribute__((reset(option)))
#define NDS32ATTR_EXCEPT(type)           __attribute__((exception(type)))
#define NDS32ATTR_EXCEPTION(type)        __attribute__((exception(type)))
#define NDS32ATTR_INTERRUPT(type)        __attribute__((interrupt(type)))
#define NDS32ATTR_ISR(type)              __attribute__((interrupt(type)))

#endif /* nds32_isr.h */
