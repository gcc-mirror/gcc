/* { dg-options "-mr10k-cache-barrier=store -mabi=64" } */

int x[4];
void bar (void);

/* Test that out-of-range stores to static objects are protected by a
   cache barrier.  */

NOMIPS16 void
foo (int n)
{
  while (n--)
    {
      x[4] = 1;
      bar ();
    }
}

/* { dg-final { scan-assembler "\tcache\t" } } */
