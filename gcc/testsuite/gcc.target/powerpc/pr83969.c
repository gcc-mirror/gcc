/* { dg-do compile { target { powerpc*-*-* } } } */
/* { dg-skip-if "do not override -mcpu" { powerpc*-*-* } { "-mcpu=*" } { "-mcpu=G5" } } */
/* { dg-options "-O1 -mcpu=G5 -fno-split-wide-types -ftree-loop-vectorize" } */

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
