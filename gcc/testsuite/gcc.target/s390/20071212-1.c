/* This used to fail due to bug in the On constraint causing a slgfi
   to be emitted with an immediate not fitting into 32bit.  */

/* { dg-do compile } */
/* { dg-options "-O3 -march=z9-109" } */

long
foo (long a)
{
  return a - (1ULL << 32);
}
