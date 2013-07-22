/* { dg-options "-O2 -fdump-tree-optimized" } */
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

main ()
{
  int i;
  for (i = 0; i < 100; i++)
    {
      if (a)
        cold_function ();
      else
        hot_function ();
    }
  return 0;
}

/* cold function should be inlined, while hot function should not.  
   Look for "cold_function () [tail call];" call statement not for the
   declaration or other appearances of the string in dump.  */
/* { dg-final-use { scan-tree-dump "cold_function ..;" "optimized"} } */
/* { dg-final-use { scan-tree-dump-not "hot_function ..;" "optimized"} } */
/* { dg-final-use { cleanup-tree-dump "optimized" } } */
