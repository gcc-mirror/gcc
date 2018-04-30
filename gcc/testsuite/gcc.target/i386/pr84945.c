/* PR target/84945 */
/* { dg-do run } */
/* { dg-options "-O2" } */

int
main ()
{
  /* AVX512_VNNI instructions are all EVEX encoded, so if
     __builtin_cpu_supports says avx512vnni is available and avx512f is not,
     this is a GCC bug.  Ditto for AVX512_BITALG  */
  if (!__builtin_cpu_supports ("avx512f")
      && (__builtin_cpu_supports ("avx512vnni")
	  || __builtin_cpu_supports ("avx512bitalg")))
    __builtin_abort ();
  return 0;
}
