/* { dg-do run } */
/* { dg-require-effective-target p9vector_hw } */
/* { dg-options "-O2 -mdejagnu-cpu=power9 -save-temps" } */

/* Verify the vec_rlm and vec_rlmi builtins works correctly.  */
/* { dg-final { scan-assembler-times {\mvrldmi\M} 1 } } */

#include <altivec.h>

#ifdef DEBUG
#include <stdio.h>
#include <stdlib.h>
#endif

void abort (void);

int main ()
{
  int i;

  vector unsigned int vec_arg1_int, vec_arg2_int, vec_arg3_int;
  vector unsigned int vec_result_int, vec_expected_result_int;
  
  vector unsigned long long int vec_arg1_di, vec_arg2_di, vec_arg3_di;
  vector unsigned long long int vec_result_di, vec_expected_result_di;

  unsigned int mask_begin, mask_end, shift;
  unsigned long long int mask;

/* Check vec int version of vec_rlmi builtin */
  mask = 0;
  mask_begin = 0;
  mask_end   = 4;
  shift = 16;

  for (i = 0; i < 31; i++)
    if ((i >= mask_begin) && (i <= mask_end))
      mask |= 0x80000000ULL >> i;

  for (i = 0; i < 4; i++) {
    vec_arg1_int[i] = 0x12345678 + i*0x11111111;
    vec_arg2_int[i] = 0xA1B1CDEF;
    vec_arg3_int[i] = mask_begin << 16 | mask_end << 8 | shift;

    /* do rotate */
    vec_expected_result_int[i] =  ( vec_arg2_int[i] & ~mask) 
      | ((vec_arg1_int[i] << shift) | (vec_arg1_int[i] >> (32-shift))) & mask;
      
  }

  /* vec_rlmi(arg1, arg2, arg3)
     result - rotate each element of arg2 left and inserts it into arg1 
     element based on the mask specified in arg3.  The shift, mask
     start and end is specified in arg3.  */
  vec_result_int = vec_rlmi (vec_arg1_int, vec_arg2_int, vec_arg3_int);

  for (i = 0; i < 4; i++) {
    if (vec_result_int[i] != vec_expected_result_int[i])
#ifdef DEBUG
      printf("ERROR: i = %d, vec_rlmi int result 0x%x, does not match "
	     "expected result 0x%x\n", i, vec_result_int[i],
	     vec_expected_result_int[i]);
#else
      abort();
#endif
    }

/* Check vec long long int version of vec_rlmi builtin */
  mask = 0;
  mask_begin = 0;
  mask_end   = 4;
  shift = 16;

  for (i = 0; i < 31; i++)
    if ((i >= mask_begin) && (i <= mask_end))
      mask |= 0x8000000000000000ULL >> i;

  for (i = 0; i < 2; i++) {
    vec_arg1_di[i] = 0x1234567800000000 + i*0x11111111;
    vec_arg2_di[i] = 0xA1B1C1D1E1F12345;
    vec_arg3_di[i] = mask_begin << 16 | mask_end << 8 | shift;

    /* do rotate */
    vec_expected_result_di[i] =  ( vec_arg2_di[i] & ~mask) 
      | ((vec_arg1_di[i] << shift) | (vec_arg1_di[i] >> (64-shift))) & mask;
  }

  /* vec_rlmi(arg1, arg2, arg3)
     result - rotate each element of arg1 left and inserts it into arg2 
     element based on the mask specified in arg3.  The shift, mask, start
     and end is specified in arg3.  */
  vec_result_di = vec_rlmi (vec_arg1_di, vec_arg2_di, vec_arg3_di);

  for (i = 0; i < 2; i++) {
    if (vec_result_di[i] != vec_expected_result_di[i])
#ifdef DEBUG
      printf("ERROR: i = %d, vec_rlmi int long long result 0x%llx, does not match "
	     "expected result 0x%llx\n", i, vec_result_di[i],
	     vec_expected_result_di[i]);
#else
      abort();
#endif
    }

  /* Check vec int version of vec_rlnm builtin */
  mask = 0;
  mask_begin = 0;
  mask_end   = 4;
  shift = 16;

  for (i = 0; i < 31; i++)
    if ((i >= mask_begin) && (i <= mask_end))
      mask |= 0x80000000ULL >> i;

  for (i = 0; i < 4; i++) {
    vec_arg1_int[i] = 0x12345678 + i*0x11111111;
    vec_arg2_int[i] = shift;
    vec_arg3_int[i] = mask_begin << 8 | mask_end;
    vec_expected_result_int[i] = (vec_arg1_int[i] << shift) & mask;
  }

  /* vec_rlnm(arg1, arg2, arg3)
     result - rotate each element of arg1 left by shift in element of arg2.
       Then AND with mask whose  start/stop bits are specified in element of
       arg3.  */
  vec_result_int = vec_rlnm (vec_arg1_int, vec_arg2_int, vec_arg3_int);
  for (i = 0; i < 4; i++) {
    if (vec_result_int[i] != vec_expected_result_int[i])
#ifdef DEBUG
      printf("ERROR: vec_rlnm, i = %d, int result 0x%x does not match "
	     "expected result 0x%x\n", i, vec_result_int[i],
	     vec_expected_result_int[i]);
#else
      abort();
#endif
    }

/* Check vec long int version of builtin */
  mask = 0;
  mask_begin = 0;
  mask_end   = 4;
  shift = 20;

  for (i = 0; i < 63; i++)
    if ((i >= mask_begin) && (i <= mask_end))
      mask |= 0x8000000000000000ULL >> i;
  
  for (i = 0; i < 2; i++) {
    vec_arg1_di[i] = 0x123456789ABCDE00ULL + i*0x1111111111111111ULL;
    vec_arg2_di[i] = shift;
    vec_arg3_di[i] = mask_begin << 8 | mask_end;
    vec_expected_result_di[i] = (vec_arg1_di[i] << shift) & mask;
  }

  vec_result_di = vec_rlnm (vec_arg1_di, vec_arg2_di, vec_arg3_di);

  for (i = 0; i < 2; i++) {
    if (vec_result_di[i] != vec_expected_result_di[i])
#ifdef DEBUG
      printf("ERROR: vec_rlnm, i = %d, long long int result 0x%llx does not "
	     "match expected result 0x%llx\n", i, vec_result_di[i],
	     vec_expected_result_di[i]);
#else
      abort();
#endif
    }

    /* Check vec int version of vec_vrlnm builtin */
  mask = 0;
  mask_begin = 0;
  mask_end   = 4;
  shift = 16;

  for (i = 0; i < 31; i++)
    if ((i >= mask_begin) && (i <= mask_end))
      mask |= 0x80000000ULL >> i;

  for (i = 0; i < 4; i++) {
    vec_arg1_int[i] = 0x12345678 + i*0x11111111;
    vec_arg2_int[i] = mask_begin << 16 | mask_end << 8 | shift;
    vec_expected_result_int[i] = (vec_arg1_int[i] << shift) & mask;
  }

  /* vec_vrlnm(arg1, arg2, arg3)
     result - rotate each element of arg1 left then AND with mask.  The mask
       start, stop bits is specified in the second argument.  The shift amount
       is also specified in the second argument.  */
  vec_result_int = vec_vrlnm (vec_arg1_int, vec_arg2_int);

  for (i = 0; i < 4; i++) {
    if (vec_result_int[i] != vec_expected_result_int[i])
#ifdef DEBUG
      printf("ERROR: vec_vrlnm, i = %d, int result 0x%x does not match "
	     "expected result 0x%x\n", i, vec_result_int[i],
	     vec_expected_result_int[i]);
#else
      abort();
#endif
    }

/* Check vec long int version of vec_vrlnm builtin */
  mask = 0;
  mask_begin = 0;
  mask_end   = 4;
  shift = 20;

  for (i = 0; i < 63; i++)
    if ((i >= mask_begin) && (i <= mask_end))
      mask |= 0x8000000000000000ULL >> i;
  
  for (i = 0; i < 2; i++) {
    vec_arg1_di[i] = 0x123456789ABCDE00ULL + i*0x1111111111111111ULL;
    vec_arg2_di[i] = mask_begin << 16 | mask_end << 8 | shift;
    vec_expected_result_di[i] = (vec_arg1_di[i] << shift) & mask;
  }

  vec_result_di = vec_vrlnm (vec_arg1_di, vec_arg2_di);

  for (i = 0; i < 2; i++) {
    if (vec_result_di[i] != vec_expected_result_di[i])
#ifdef DEBUG
      printf("ERROR: vec_vrlnm, i = %d, long long int result 0x%llx does not "
	     "match expected result 0x%llx\n", i, vec_result_di[i],
	     vec_expected_result_di[i]);
#else
      abort();
#endif
    }

  return 0;
}
