/* { dg-do run } */
/* { dg-options "-O3" } */
/* { dg-require-effective-target rv64 } */
/* { dg-require-effective-target riscv_v } */

#include <assert.h>
int a;
void c(int b) { a = b; }
char d;
char *const e = &d;
long f = 66483309998;
unsigned long g[2];
short h;
int k;
void __attribute__ ((noinline)) l() {
  int i = 0;
  for (; i < 2; i++) {
    {
      unsigned long *m = &g[0];
      *m &= 2;
      if (f && *e)
        for (;;)
          ;
    }
    k = f;
    g[1] = k;
    for (; h;)
      ;
  }
}
int main() {
  l();
  assert (g[1] == 2058800558);
  c(g[1] >> 32);
  assert (a == 0);
}
