/* The "%a" modifier can't get the address of extern symbol directly from TOC
   with -fPIC. */
/* { dg-options "-fPIC -O2 -mno-pcrel" } */

/* It's to verify no ICE here, ignore error messages about invalid 'asm'.  */
/* { dg-excess-errors "pr96866-2.c" } */

void
f (void)
{
  extern int x;
  __asm__ volatile("#%a0" ::"X"(&x));
}
