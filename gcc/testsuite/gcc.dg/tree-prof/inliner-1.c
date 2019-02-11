/* { dg-options "-O2 -fdump-tree-optimized -fgnu89-inline" } */
int a;
int b[100];
void abort (void);

inline void
cold_function ()
{
  int i;
  for (i = 0; i < 99; i++)
    if (b[i] / (b[i+1] + 1))
      abort ();
}

inline void
hot_function ()
{
  int i;
  for (i = 0; i < 99; i++)
    if (b[i] / (b[i+1] + 1))
      abort ();
}

int
main ()
{
  int i;
  for (i = 0; i < 1000000; i++)
    {
      if (a)
        cold_function ();   /* Should not be inlined.  */
      else
        hot_function ();    /* Should be inlined.  */
    }
  return 0;
}

/* The call to hot_function should be inlined, while cold_function should
   not be.  Look for the "cold_function ();" call statement and not for
   its declaration or other occurrences of the string in the dump.  */
/* { dg-final-use { scan-tree-dump "cold_function ..;" "optimized"} } */
/* { dg-final-use { scan-tree-dump-not "hot_function ..;" "optimized"} } */
