/* { dg-xfail-run-if "invalid assumption" { sparc*-*-* && lp64 } "*" "" } */
/* { dg-options "-fno-inline" } */

#include <assert.h>

#define ALIGNMENT 64

unsigned test(unsigned n, unsigned p)
{
  static struct { char __attribute__((__aligned__(ALIGNMENT))) c; } s;
  unsigned x;

  assert(__alignof__(s) == ALIGNMENT);
  asm ("" : "=g" (x), "+m" (s) : "0" (&x));

  return n ? test(n - 1, x) : (x ^ p);
}

int main (int argc, char *argv[] __attribute__((unused)))
{
  unsigned int x = test(argc, 0);

  x |= test(argc + 1, 0);
  x |= test(argc + 2, 0);

  return !(x & (ALIGNMENT - 1));
}
