/* { dg-do compile { target int128 } } */
/* { dg-options "-O2 -mxop" } */
typedef __int128 __attribute__((__vector_size__ (sizeof (__int128)))) U;
typedef int __attribute__((__vector_size__ (sizeof (int)))) V;

U u;
V v;

U
foo (void)
{
  return (0 != __builtin_convertvector (v, U)) <= (0 != u);
}
