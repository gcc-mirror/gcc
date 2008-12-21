/* { dg-options "-O2 -mr10k-cache-barrier=store" } */

/* Test that indirect calls are protected.  */

int bar (int);

NOMIPS16 void
foo (void (*fn) (void), int x)
{
  if (x)
    (*fn) ();
}

/* { dg-final { scan-assembler-times "\tcache\t" 1 } } */
