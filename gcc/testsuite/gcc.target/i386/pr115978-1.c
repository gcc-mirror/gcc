/* { dg-do run } */
/* { dg-options "-O2 -march=native" } */

int
main ()
{
  if (__builtin_cpu_supports ("apxf"))
    {
#ifdef __x86_64__
# ifndef __APX_F__
      __builtin_abort ();
# endif
#else
# ifdef __APX_F__
      __builtin_abort ();
# endif
#endif
      return 0;
    }

  return 0;
}
