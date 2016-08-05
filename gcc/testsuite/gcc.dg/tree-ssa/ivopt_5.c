/* { dg-options "-O2 -fdump-tree-ivopts -fkeep-gc-roots-live" } */

/* Only integer ivopts here when using -fkeep-gc-roots-live.   */

void foo (char *pstart, int n)
{
  char *p;
  char *pend = pstart + n;

  for (p = pstart; p < pend; p++)
    *p = 1;
}

void foo1 (char *pstart, int n)
{
  char *p;
  char *pend = pstart + n;

  for (p = pstart; p != pend; p++)
    *p = 1;
}

/* { dg-final { scan-tree-dump-times "ivtmp.\[0-9_\]* = PHI <\[^0\]\[^,\]*, \[^0\]" 0 "ivopts"} } */
