/* { dg-do compile { target { { { { { { i?86-*-* x86_64-*-* } && x32 } || lp64 } && { ! s390*-*-* } } && { ! hppa*64*-*-* } } && { ! alpha*-*-* } } } } */
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
/* { dg-final { cleanup-rtl-dump "dse1" } } */

/* { dg-final { scan-rtl-dump-not "insn \[^\n\]*set \\(mem" "final" } } */
/* { dg-final { cleanup-rtl-dump "final" } } */
