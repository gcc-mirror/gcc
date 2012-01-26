/* { dg-do compile { target { { { { i?86-*-* x86_64-*-* } && x32 } || lp64 } && { ! s390*-*-* } } } } */
/* { dg-options "-O2 -fdump-rtl-dse1" } */
/* Restricting to 64-bit targets since 32-bit targets return
   structures in memory.  */

#include <stdint.h>

struct ints { int a, b, c; } foo();
void bar(int a, int b);

void func() {
  struct ints s = foo();
  bar(s.a, s.b);
}
/* { dg-final { scan-rtl-dump "global deletions = 2"  "dse1" } } */
/* { dg-final { cleanup-rtl-dump "dse1" } } */
