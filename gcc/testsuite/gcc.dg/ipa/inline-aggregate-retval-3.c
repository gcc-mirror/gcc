/* Volatile accesses remain after inlining.  */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-ipa-fnsummary-details -fno-early-inlining -fno-partial-inlining -fno-ipa-cp" } */

int
from_pointer (volatile int *p)
{
  return *p;
}

int
from_parameter (volatile int p)
{
  return p;
}

struct V { long d[8]; };

struct V
from_aggregate_parameter (volatile struct V p)
{
  return p;
}

/* { dg-final { scan-ipa-dump-times { =\{v\} [^\n]*;\n[^\n]*\n\t\tAccounting} 3 "fnsummary" } } */
/* { dg-final { scan-ipa-dump-not { =\{v\} [^\n]*;\n[^\n]*\n\t\t[^\n]*[Ww]ill be eliminated by inlining} "fnsummary" } } */
