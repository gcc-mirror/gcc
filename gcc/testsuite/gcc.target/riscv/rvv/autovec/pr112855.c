/* { dg-do run } */
/* { dg-options "-O3" } */
/* { dg-require-effective-target rv64 } */
/* { dg-require-effective-target riscv_v } */

#include <assert.h>
int a;
int b = 100;
int c[25];
int d;
int main() {
  int e;
  d = 0;
  for (; d < 5; d++) {
    e = 0;
    for (; e < 5; e++)
      c[d * 5 + e] = 0;
  }
  if (b)
    if (a)
      for (;;)
        ;
  b++;
  int volatile f = *c;
  assert(b == 101);
}
