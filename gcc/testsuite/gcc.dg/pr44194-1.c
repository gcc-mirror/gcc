/* { dg-do compile { target { { { { { { { { { { i?86-*-* x86_64-*-* } && x32 } || lp64 } && { ! s390*-*-* } } && { ! hppa*64*-*-* } } && { ! alpha*-*-* } } && { { ! powerpc*-*-linux* } || powerpc_elfv2 } } && { ! nvptx-*-* } } } } } } */
/* { dg-options "-O2 -fdump-rtl-dse1 -fdump-rtl-final" } */

/* Restrict to 64-bit targets since 32-bit targets usually return small
   structures in memory.  */

struct ints { int a, b, c; } foo();
void bar(int a, int b);

void func() {
  struct ints s = foo();
  bar(s.a, s.b);
}

/* { dg-final { scan-rtl-dump "global deletions = (2|3)" "dse1" } } */

/* Here we want to ignore frame-related instructions, marked as insn/f,
   that do things like store the link register to the stack.  We also want
   to treat insns the same regardless of whether they have a scheduling
   :TI marker, so match both "insn " and "insn:".  */
/* { dg-final { scan-rtl-dump-not "insn\[: \]\[^\n\]*set \\(mem(?!\[^\n\]*scratch)" "final" } } */
