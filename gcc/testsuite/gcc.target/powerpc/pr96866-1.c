/* The "%a" modifier can't get the address of extern symbol directly from TOC
   with -fPIC, even the symbol is propagated for "X" constraint under -O2. */
/* { dg-options "-fPIC -O2 -mno-pcrel" } */

/* It's to verify no ICE here, ignore error messages about invalid 'asm'.  */
/* { dg-excess-errors "pr96866-1.c" } */

int x[2];

int __attribute__ ((noipa))
f1 (void)
{
  int n;
  int *p = x;
  *p++;
  __asm__ volatile("ld %0, %a1" : "=r"(n) : "X"(p));
  return n;
}
