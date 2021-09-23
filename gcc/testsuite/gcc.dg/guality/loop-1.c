/* { dg-do run } */
/* { dg-options "-fno-tree-scev-cprop -fno-tree-vectorize -g" } */

#include "../nop.h"

void __attribute__((noipa,noinline))
foo (int n)
{
  if (n == 0)
    return;
  int i = 0;
  do
    {
      ++i; /* { dg-final { gdb-test . "i" "0" } } */
    }
  while (i < n);
  /* The following works only with final value replacement or with the NOP
     but not without (which means -Og).  Vectorization breaks it, so disable
     that.  At -O3 it currently fails, PR89983.  */
  __asm__ volatile (NOP : : "g" (i) : "memory"); /* { dg-final { gdb-test . "i" "1" { xfail { aarch64*-*-* && { any-opts "-ftracer" } } } } } */
}
int main() { foo(1); }
