/* { dg-options "-mr10k-cache-barrier=store -mno-abicalls -mabi=64" } */

/* Test that in-range stores to static objects do not get an unnecessary
   cache barrier.  */

int x[4];
void bar (void);

NOMIPS16 void
foo (int n)
{
  while (n--)
    {
      x[3] = 1;
      bar ();
    }
}

/* { dg-final { scan-assembler-not "\tcache\t" } } */
