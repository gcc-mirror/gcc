/* Test that the correct data prefetch instructions are generated for i386
   variants that use 3DNow! prefetchw or SSE prefetch instructions with
   locality hints.  */

/* { dg-do compile { target i?86-*-* } } */

char *msg = "howdy there";

void foo (char *p)
{
  __builtin_prefetch (p, 0, 0);
  __builtin_prefetch (p, 0, 1);
  __builtin_prefetch (p, 0, 2);
  __builtin_prefetch (p, 0, 3);
  __builtin_prefetch (p, 1, 0);
  __builtin_prefetch (p, 1, 1);
  __builtin_prefetch (p, 1, 2);
  __builtin_prefetch (p, 1, 3);
}

int main ()
{
  foo (msg);
  exit (0);
}

/* { dg-final { scan-assembler "prefetchw" } } */
/* { dg-final { scan-assembler "prefetchnta" } } */
/* { dg-final { scan-assembler "prefetcht" } } */
