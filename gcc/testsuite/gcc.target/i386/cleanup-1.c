/* { dg-do run { target *-*-linux* } } */
/* { dg-options "-fexceptions -fnon-call-exceptions -fasynchronous-unwind-tables -O2" } */
/* Test complex CFA value expressions.  */

#include <unwind.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>

static _Unwind_Reason_Code
force_unwind_stop (int version, _Unwind_Action actions,
		   _Unwind_Exception_Class exc_class,
		   struct _Unwind_Exception *exc_obj,
		   struct _Unwind_Context *context,
		   void *stop_parameter)
{
  if (actions & _UA_END_OF_STACK)
    abort ();
  return _URC_NO_REASON;
}

static void
force_unwind ()
{
  struct _Unwind_Exception *exc = malloc (sizeof (*exc));
  memset (&exc->exception_class, 0, sizeof (exc->exception_class));
  exc->exception_cleanup = 0;

  _Unwind_ForcedUnwind (exc, force_unwind_stop, 0);
  abort ();
}

int count;

static void
counter (void *p __attribute__((unused)))
{
  ++count;
}

static void
handler (void *p __attribute__((unused)))
{
  if (count != 2)
    abort ();
  _exit (0);
}

static int __attribute__((noinline))
fn5 (void)
{
  char dummy __attribute__((cleanup (counter)));
  force_unwind ();
  return 0;
}

void
bar (void)
{
  char dummy __attribute__((cleanup (counter)));
  fn5 ();
}

void __attribute__((noinline))
foo (int x)
{
  char buf[256];
#ifdef __i386__
  __asm (
	"testl	%0, %0\n\t"
	"jnz	1f\n\t"
	".subsection 1\n\t"
	".type	_L_mutex_lock_%=, @function\n"
"_L_mutex_lock_%=:\n"
"1:\t"	"leal	%1, %%ecx\n"
"2:\t"	"call	bar\n"
"3:\t"	"jmp	18f\n"
"4:\t"	".size _L_mutex_lock_%=, .-_L_mutex_lock_%=\n\t"
	".previous\n\t"
	".section	.eh_frame,\"a\",@progbits\n"
"5:\t"	".long	7f-6f	# Length of Common Information Entry\n"
"6:\t"	".long	0x0	# CIE Identifier Tag\n\t"
	".byte	0x1	# CIE Version\n\t"
	".ascii \"zR\\0\"	# CIE Augmentation\n\t"
	".uleb128 0x1	# CIE Code Alignment Factor\n\t"
	".sleb128 -4	# CIE Data Alignment Factor\n\t"
	".byte	0x8	# CIE RA Column\n\t"
	".uleb128 0x1	# Augmentation size\n\t"
	".byte	0x1b	# FDE Encoding (pcrel sdata4)\n\t"
	".byte	0xc	# DW_CFA_def_cfa\n\t"
	".uleb128 0x4\n\t"
	".uleb128 0x0\n\t"
	".align 4\n"
"7:\t"	".long	17f-8f	# FDE Length\n"
"8:\t"	".long	8b-5b	# FDE CIE offset\n\t"
	".long	1b-.	# FDE initial location\n\t"
	".long	4b-1b	# FDE address range\n\t"
	".uleb128 0x0	# Augmentation size\n\t"
	".byte	0x16	# DW_CFA_val_expression\n\t"
	".uleb128 0x8\n\t"
	".uleb128 10f-9f\n"
"9:\t"	".byte	0x78	# DW_OP_breg8\n\t"
	".sleb128 3b-1b\n"
"10:\t"	".byte	0x40 + (2b-1b) # DW_CFA_advance_loc\n\t"
	".byte	0x16	# DW_CFA_val_expression\n\t"
	".uleb128 0x8\n\t"
	".uleb128 12f-11f\n"
"11:\t"	".byte	0x78	# DW_OP_breg8\n\t"
	".sleb128 3b-2b\n"
"12:\t"	".byte	0x40 + (3b-2b-1) # DW_CFA_advance_loc\n\t"
	".byte	0x16	# DW_CFA_val_expression\n\t"
	".uleb128 0x8\n\t"
	".uleb128 16f-13f\n"
"13:\t"	".byte	0x78	# DW_OP_breg8\n\t"
	".sleb128 15f-14f\n\t"
	".byte	0x0d	# DW_OP_const4s\n"
"14:\t"	".4byte	3b-.\n\t"
	".byte	0x1c	# DW_OP_minus\n\t"
	".byte	0x0d	# DW_OP_const4s\n"
"15:\t"	".4byte	18f-.\n\t"
	".byte	0x22	# DW_OP_plus\n"
"16:\t"	".align 4\n"
"17:\t"	".previous\n"
"18:"
	: : "r" (x), "m" (x), "r" (buf)
	: "memory", "eax", "edx", "ecx");
#elif defined __x86_64__
  __asm (
	"testl	%0, %0\n\t"
	"jnz	1f\n\t"
	".subsection 1\n\t"
	".type	_L_mutex_lock_%=, @function\n"
"_L_mutex_lock_%=:\n"
"1:\t"	"leaq	%1, %%rdi\n"
"2:\t"	"subq	$128, %%rsp\n"
"3:\t"	"call	bar\n"
"4:\t"	"addq	$128, %%rsp\n"
"5:\t"	"jmp	24f\n"
"6:\t"	".size _L_mutex_lock_%=, .-_L_mutex_lock_%=\n\t"
	".previous\n\t"
	".section	.eh_frame,\"a\",@progbits\n"
"7:\t"	".long	9f-8f	# Length of Common Information Entry\n"
"8:\t"	".long	0x0	# CIE Identifier Tag\n\t"
	".byte	0x1	# CIE Version\n\t"
	".ascii \"zR\\0\"	# CIE Augmentation\n\t"
	".uleb128 0x1	# CIE Code Alignment Factor\n\t"
	".sleb128 -8	# CIE Data Alignment Factor\n\t"
	".byte	0x10	# CIE RA Column\n\t"
	".uleb128 0x1	# Augmentation size\n\t"
	".byte	0x1b	# FDE Encoding (pcrel sdata4)\n\t"
	".byte	0x12	# DW_CFA_def_cfa_sf\n\t"
	".uleb128 0x7\n\t"
	".sleb128 16\n\t"
	".align 8\n"
"9:\t"	".long	23f-10f	# FDE Length\n"
"10:\t"	".long	10b-7b	# FDE CIE offset\n\t"
	".long	1b-.	# FDE initial location\n\t"
	".long	6b-1b	# FDE address range\n\t"
	".uleb128 0x0	# Augmentation size\n\t"
	".byte	0x16	# DW_CFA_val_expression\n\t"
	".uleb128 0x10\n\t"
	".uleb128 12f-11f\n"
"11:\t"	".byte	0x80	# DW_OP_breg16\n\t"
	".sleb128 4b-1b\n"
"12:\t"	".byte	0x40 + (2b-1b) # DW_CFA_advance_loc\n\t"
	".byte	0x16	# DW_CFA_val_expression\n\t"
	".uleb128 0x10\n\t"
	".uleb128 14f-13f\n"
"13:\t"	".byte	0x80	# DW_OP_breg16\n\t"
	".sleb128 4b-2b\n"
"14:\t"	".byte	0x40 + (3b-2b) # DW_CFA_advance_loc\n\t"
	".byte	0x0e	# DW_CFA_def_cfa_offset\n\t"
	".uleb128 0\n\t"
	".byte	0x16	# DW_CFA_val_expression\n\t"
	".uleb128 0x10\n\t"
	".uleb128 16f-15f\n"
"15:\t"	".byte	0x80	# DW_OP_breg16\n\t"
	".sleb128 4b-3b\n"
"16:\t"	".byte	0x40 + (4b-3b-1) # DW_CFA_advance_loc\n\t"
	".byte	0x0e	# DW_CFA_def_cfa_offset\n\t"
	".uleb128 128\n\t"
	".byte	0x16	# DW_CFA_val_expression\n\t"
	".uleb128 0x10\n\t"
	".uleb128 20f-17f\n"
"17:\t"	".byte	0x80	# DW_OP_breg16\n\t"
	".sleb128 19f-18f\n\t"
	".byte	0x0d	# DW_OP_const4s\n"
"18:\t"	".4byte	4b-.\n\t"
	".byte	0x1c	# DW_OP_minus\n\t"
	".byte	0x0d	# DW_OP_const4s\n"
"19:\t"	".4byte	24f-.\n\t"
	".byte	0x22	# DW_OP_plus\n"
"20:\t"	".byte	0x40 + (5b-4b+1) # DW_CFA_advance_loc\n\t"
	".byte	0x13	# DW_CFA_def_cfa_offset_sf\n\t"
	".sleb128 16\n\t"
	".byte	0x16	# DW_CFA_val_expression\n\t"
	".uleb128 0x10\n\t"
	".uleb128 22f-21f\n"
"21:\t"	".byte	0x80	# DW_OP_breg16\n\t"
	".sleb128 4b-5b\n"
"22:\t"	".align 8\n"
"23:\t"	".previous\n"
"24:"
	: : "r" (x), "m" (x), "r" (buf)
	: "memory", "rax", "rdx", "rcx", "rsi", "rdi",
	  "r8", "r9", "r10", "r11");
#else
# error Unsupported test architecture
#endif
}

static int __attribute__((noinline))
fn2 (void)
{
  foo (3);
  return 0;
}

static int __attribute__((noinline))
fn1 (void)
{
  fn2 ();
  return 0;
}

static void *
fn0 (void)
{
  char dummy __attribute__((cleanup (handler)));
  fn1 ();
  return 0;
}

int
main (void)
{
  fn0 ();
  return 0;
}
