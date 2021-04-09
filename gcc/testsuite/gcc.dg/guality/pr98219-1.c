/* { dg-do run { target { { i?86-*-* x86_64-*-* } && { ! ia32 } } } } */
/* { dg-options "-g -muintr -mgeneral-regs-only" } */

#include <x86gprintrin.h>

extern void exit (int);
typedef unsigned int uword_t __attribute__ ((mode (__word__)));

#define UIRRV		0x12345670
#define RIP		0x12345671
#define RFLAGS		0x12345672
#define RSP		0x12345673

#define STRING(x)	XSTRING(x)
#define XSTRING(x)	#x
#define ASMNAME(cname)  ASMNAME2 (__USER_LABEL_PREFIX__, cname)
#define ASMNAME2(prefix, cname) XSTRING (prefix) cname

void
__attribute__((interrupt, used))
fn (struct __uintr_frame *frame, uword_t uirrv)
{
  if (UIRRV != uirrv)		/* BREAK */
    __builtin_abort ();
  if (RIP != frame->rip)
    __builtin_abort ();
  if (RFLAGS != frame->rflags)
    __builtin_abort ();
  if (RSP != frame->rsp)
    __builtin_abort ();

  exit (0);
}

int
main ()
{
  asm ("push	$" STRING (RSP) ";		\
	push	$" STRING (RFLAGS) ";		\
	push	$" STRING (RIP) ";		\
	push	$" STRING (UIRRV) ";		\
	jmp	" ASMNAME ("fn"));
  return 0;
}

/* { dg-final { gdb-test 22 "uirrv" "0x12345670" } } */
/* { dg-final { gdb-test 22 "frame->rip" "0x12345671" } } */
/* { dg-final { gdb-test 22 "frame->rflags" "0x12345672" } } */
/* { dg-final { gdb-test 22 "frame->rsp" "0x12345673" } } */
