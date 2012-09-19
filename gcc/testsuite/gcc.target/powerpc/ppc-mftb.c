/* { dg-do run { target { powerpc*-*-* } } } */

/* Test if __builtin_ppc_mftb () is compatible with the current processor and
   if it's changing between reads.  A read failure might indicate a Power
   ISA or binutils change.  */

int
main (void)
{
  unsigned long t = __builtin_ppc_mftb ();
  int j;

  for (j = 0; j < 1000000; j++)
    if (t != __builtin_ppc_mftb ())
      return 0;

  return 1;
}
