/* { dg-options "-O2 -fdump-tree-optimized -fwhole-program" } */
int b[100];
void abort (void);

void
large_function ()
{
  int i;
  for (i = 0; i < 99; i++)
    if (b[i] / (b[i+1] + 1))
      abort ();
}

int
main ()
{
  large_function ();
}

/* Function should be inlined as called once.  */
/* { dg-final { scan-tree-dump-not "large_function" "optimized"} } */


