/* { dg-do run } */
/* { dg-options "-O2 -march=native" } */

int
main ()
{
  if (__builtin_cpu_supports ("avx512bmm"))
    {
#ifndef __AVX512BMM__
      __builtin_abort ();
#endif
    }
  else
    {
#ifdef __AVX512BMM__
      __builtin_abort ();
#endif
    }
  return 0;
}
