/* { dg-do run { target { power10_hw } } } */
/* { dg-options "-mdejagnu-cpu=power10 -save-temps" } */

#include <altivec.h>

#define DEBUG 0

#if DEBUG
#include <stdio.h>
#endif

extern void abort (void);

int
main (int argc, char *argv [])
{
  int i;
  vector unsigned long long int vresult_ullint;
  vector unsigned long long int expected_vresult_ullint;
  vector unsigned long long int src_va_ullint;
  unsigned int long long src_a_ullint;

  /* Replace doubleword size chunk specified as a constant that can be
     represented by an int.  Should generate a vinsd instruction.  Note, this
     test requires command line option -flax-vector-conversions.  */
  src_a_ullint = 456;
  src_va_ullint = (vector unsigned long long int) { 0, 11 };
  vresult_ullint = (vector unsigned long long int) { 0, 2 };
  expected_vresult_ullint = (vector unsigned long long int) { 0, 456 };
						 
  vresult_ullint = (vector unsigned long long int)
    vec_replace_unaligned ((vector unsigned char)src_va_ullint,
			   src_a_ullint, 0);

  if (!vec_all_eq (vresult_ullint, expected_vresult_ullint)) {
#if DEBUG
    printf("ERROR, vec_replace_unaligned ((vector unsigned char)src_vb_ullint, src_a_ullint, index)\n");
    for (i = 0; i < 2; i++)
      printf(" vresult_ullint[%d] = %d, expected_vresult_ullint[%d] = %d\n",
	     i, vresult_ullint[i], i, expected_vresult_ullint[i]);
#else
    abort();
#endif
  }
  
  return 0;
}

/* { dg-final { scan-assembler-times {\mvinsd\M} 1 } } */
