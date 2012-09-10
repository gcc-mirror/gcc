/* { dg-options "-mr10k-cache-barrier=store -mno-abicalls" } */

void bar1 (void);
void bar2 (void);
void bar3 (void);

NOMIPS16 void
foo (int *x, int sel, int n)
{
  if (sel)
    {
      bar1 ();
      x[0] = 1;
    }
  else
    {
      bar2 ();
      x[1] = 0;
    }
  /* If there is one copy of this code, reached by two unconditional edges,
     then it shouldn't need a third cache barrier.  */
  x[2] = 2;
  while (n--)
    bar3 ();
}

/* { dg-final { scan-assembler-times "\tcache\t" 2 } } */
