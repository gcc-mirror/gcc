/* { dg-do compile } */
/* { dg-additional-options "-march=armv9-a -msve-vector-bits=128" { target { aarch64-*-* } } } */
/* { dg-additional-options "-mavx512bw -mavx512vl --param vect-partial-vector-usage=1" { target { avx512bw && avx512vl } } } */

char *b;
bool c(int l)
{
  bool d = true;
  for (int a = 0; a < l; a++)
    if (b[a])
      d = false;
  return d;
}
