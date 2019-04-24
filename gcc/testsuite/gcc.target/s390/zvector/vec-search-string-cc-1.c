/* { dg-do run } */
/* { dg-require-effective-target s390_vxe2 } */
/* { dg-options "-O3 -mzarch -march=arch13 -mzvector --save-temps" } */

#include <vecintrin.h>

void __attribute__((noinline,noclone))
vstrs1 ()
{
  int cc;
  vector signed char haystack = { 'h', 'o', 'l', 'a', 'h', 'i', 'h', 'o',
				  'h', 'i', 'h', 'o' };
  vector signed char needle = { 'h', 'i', 'h', 'o' };
  vector unsigned char length = { 0 };
  length[7] = 4;

  vector unsigned char result = vec_search_string_cc (haystack, needle,
						      length, &cc);

  if (result[7] != 4)
    __builtin_abort ();

  /* CC2 indicates a full match.  */
  if (cc != 2)
    __builtin_abort ();
}

int
main ()
{
  vstrs1 ();

  return 0;
}

/* { dg-final { scan-assembler-times "vstrsb\t" 1 } } */
