/* { dg-do run { target { *-*-linux* && { ! { ia32 } } } } } */
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
#ifdef __x86_64__
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
"5:\t"	"jmp	21f\n"
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
	".byte	0xc	# DW_CFA_def_cfa\n\t"
	".uleb128 0x7\n\t"
	".uleb128 0x0\n\t"
	".align 8\n"
"9:\t"	".long	20f-10f	# FDE Length\n"
"10:\t"	".long	10b-7b	# FDE CIE offset\n\t"
	".long	1b-.	# FDE initial location\n\t"
	".long	6b-1b	# FDE address range\n\t"
	".uleb128 0x0	# Augmentation size\n\t"
	/* This CFA expression computes the address right
	   past the jnz instruction above, from %rip somewhere
	   within the _L_mutex_lock_%= subsection.  */
	".byte	0x16	# DW_CFA_val_expression\n\t"
	".uleb128 0x10\n\t"
	".uleb128 19f-11f\n"
"11:\t"	".byte	0x80	# DW_OP_breg16\n\t"
	".sleb128 0\n"
"12:\t"	".byte	0x12	# DW_OP_dup\n\t"
	".byte	0x94	# DW_OP_deref_size\n\t"
	".byte	1\n\t"
	".byte	0x12	# DW_OP_dup\n\t"
	".byte	0x08	# DW_OP_const1u\n\t"
	".byte	0x48\n\t"
	".byte	0x2e	# DW_OP_ne\n\t"
	".byte	0x28	# DW_OP_bra\n\t"
	".2byte	16f-13f\n"
"13:\t"	".byte	0x13	# DW_OP_drop\n\t"
	".byte	0x23	# DW_OP_plus_uconst\n\t"
	".uleb128 1\n\t"
	".byte	0x12	# DW_OP_dup\n\t"
	".byte	0x94	# DW_OP_deref_size\n\t"
	".byte	1\n\t"
	".byte	0x08	# DW_OP_const1u\n\t"
	".byte	0x81\n\t"
	".byte	0x2e	# DW_OP_ne\n\t"
	".byte	0x28	# DW_OP_bra\n\t"
	".2byte	15f-14f\n"
"14:\t"	".byte	0x23	# DW_OP_plus_uconst\n\t"
	".uleb128 3b-2b-1\n\t"
	".byte	0x2f	# DW_OP_skip\n\t"
	".2byte	12b-15f\n"
"15:\t"	".byte	0x23	# DW_OP_plus_uconst\n\t"
	".uleb128 2b-1b-1\n\t"
	".byte	0x2f	# DW_OP_skip\n\t"
	".2byte	12b-16f\n"
"16:\t"	".byte	0x08	# DW_OP_const1u\n\t"
	".byte	0xe8\n\t"
	".byte	0x2e	# DW_OP_ne\n\t"
	".byte	0x28	# DW_OP_bra\n\t"
	".2byte	18f-17f\n"
"17:\t"	".byte	0x23	# DW_OP_plus_uconst\n\t"
	".uleb128 4b-3b\n\t"
	".byte	0x2f	# DW_OP_skip\n\t"
	".2byte	12b-18f\n"
"18:\t"	".byte	0x23	# DW_OP_plus_uconst\n\t"
	".uleb128 1\n\t"
	".byte	0x12	# DW_OP_dup\n\t"
	".byte	0x94	# DW_OP_deref_size\n\t"
	".byte	4\n\t"
	".byte	0x08	# DW_OP_const1u\n\t"
	".byte	72 - (6b-5b) * 8 # (6b-5b) == 5 ? 32 : 56\n\t"
	".byte	0x24	# DW_OP_shl\n\t"
	".byte	0x08	# DW_OP_const1u\n\t"
	".byte	72 - (6b-5b) * 8 # (6b-5b) == 5 ? 32 : 56\n\t"
	".byte	0x26	# DW_OP_shra\n\t"
	".byte	0x22	# DW_OP_plus\n\t"
	".byte	0x23	# DW_OP_plus_uconst\n\t"
	".uleb128 6b-5b-1\n"
"19:\t"	".byte	0x40 + (3b-1b) # DW_CFA_advance_loc\n\t"
	".byte	0xe	# DW_CFA_def_cfa_offset\n\t"
	".uleb128 128\n\t"
	".byte	0x40 + (5b-3b) # DW_CFA_advance_loc\n\t"
	".byte	0xe	# DW_CFA_def_cfa_offset\n\t"
	".uleb128 0\n\t"
	".align 8\n"
"20:\t"	".previous\n"
"21:"
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
