/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-options "-O1 -mdejagnu-cpu=G5 -fno-split-wide-types -ftree-loop-vectorize" } */

long long int
n7 (int po, long long int r4)
{
  while (po < 1)
    {
      r4 |= 1;
      ++po;
    }
  return r4;
}
