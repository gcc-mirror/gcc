/* __builtin_dwarf_cfa must give the CFA even in a function that realigns
   its frame.  PLAIN and REALIGNED take no arguments and are called from
   the same frame of MAIN, so both must see the same CFA: the stack
   pointer before the call.  On x86 REALIGNED is realigned with a DRAP
   in the -mforce-drap variants that stackalign.exp adds, and also by
   default with -m32.  */

/* { dg-do run } */

extern void abort (void);

__attribute__ ((noipa)) void
sink (volatile char *p)
{
  p[0] = 0;
}

__attribute__ ((noipa)) void *
plain (void)
{
  return __builtin_dwarf_cfa ();
}

__attribute__ ((noipa)) void *
realigned (void)
{
  volatile char buf[64] __attribute__ ((aligned (64)));
  sink (buf);
  return __builtin_dwarf_cfa ();
}

int
main (void)
{
  if (plain () != realigned ())
    abort ();
  return 0;
}
