/* { dg-do run { target { power10_hw } } } */
/* { dg-do link { target { ! power10_hw } } } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-mdejagnu-cpu=power10 -save-temps -O2" } */
#include <altivec.h>

#define DEBUG 0

#if DEBUG
#include <stdio.h>
#endif

extern void abort (void);

volatile vector double vresult_d_undefined;

int
main (int argc, char *argv [])
{
  int i;
  vector int vsrc_a_int;
  vector int vresult_int;
  vector int expected_vresult_int;
  int src_a_int = 13;

  vector unsigned int vsrc_a_uint;
  vector unsigned int vresult_uint;
  vector unsigned int expected_vresult_uint;
  unsigned int src_a_uint = 7;

  vector float vresult_f;
  vector float expected_vresult_f;
  vector float vsrc_a_f;
  float src_a_f = 23.0;

  vector double vsrc_a_d;
  vector double vresult_d;
  vector double expected_vresult_d;
 
  /* Vector splati word */
  vresult_int = (vector signed int) { 1, 2, 3, 4 };
  expected_vresult_int = (vector signed int) { -13, -13, -13, -13 }; 
						 
  vresult_int = vec_splati ( -13 );

  if (!vec_all_eq (vresult_int,  expected_vresult_int)) {
#if DEBUG
    printf("ERROR, vec_splati (src_a_int)\n");
    for(i = 0; i < 4; i++)
      printf(" vresult_int[%d] = %d, expected_vresult_int[%d] = %d\n",
	     i, vresult_int[i], i, expected_vresult_int[i]);
#else
    abort();
#endif
  }

  vresult_f = (vector float) { 1.0, 2.0, 3.0, 4.0 };
  expected_vresult_f = (vector float) { 23.0, 23.0, 23.0, 23.0 };
						 
  vresult_f = vec_splati (23.0f);

  if (!vec_all_eq (vresult_f,  expected_vresult_f)) {
#if DEBUG
    printf("ERROR, vec_splati (src_a_f)\n");
    for(i = 0; i < 4; i++)
      printf(" vresult_f[%d] = %f, expected_vresult_f[%d] = %f\n",
	     i, vresult_f[i], i, expected_vresult_f[i]);
#else
    abort();
#endif
  }

  /* Vector splati double */
  vresult_d = (vector double) { 2.0, 3.0 };
  expected_vresult_d = (vector double) { -31.0, -31.0 };
						 
  vresult_d = vec_splatid (-31.0f);

  if (!vec_all_eq (vresult_d,  expected_vresult_d)) {
#if DEBUG
    printf("ERROR, vec_splati (-31.0f)\n");
    for(i = 0; i < 2; i++)
      printf(" vresult_d[%i] = %f, expected_vresult_d[%i] = %f\n",
	     i, vresult_d[i], i, expected_vresult_d[i]);
#else
    abort();
#endif
  }

  /* This test will generate a "note" to the user that the argument is
     subnormal.  It is not an error, but results are not defined.  Because this
     is undefined, we cannot check that any value is correct.  Just store it in
     a volatile variable so the XXSPLTIDP instruction gets generated and the
     warning message printed. */
  vresult_d_undefined = vec_splatid (6.6E-42f);

  /* Vector splat immediate */
  vsrc_a_int = (vector int) { 2, 3, 4, 5 };
  vresult_int = (vector int) { 1, 1, 1, 1 };
  expected_vresult_int = (vector int) { 2, 20, 4, 20 };
						 
  vresult_int = vec_splati_ins (vsrc_a_int, 1, 20);

  if (!vec_all_eq (vresult_int,  expected_vresult_int)) {
#if DEBUG
    printf("ERROR, vec_splati_ins (vsrc_a_int, 1, 20)\n");
    for(i = 0; i < 4; i++)
      printf(" vresult_int[%i] = %d, expected_vresult_int[%i] = %d\n",
	     i, vresult_int[i], i, expected_vresult_int[i]);
#else
    abort();
#endif
  }
  
  vsrc_a_uint = (vector unsigned int) { 4, 5, 6, 7 };
  vresult_uint = (vector unsigned int) { 1, 1, 1, 1 };
  expected_vresult_uint = (vector unsigned int) { 4, 40, 6, 40 };
						 
  vresult_uint = vec_splati_ins (vsrc_a_uint, 1, 40);

  if (!vec_all_eq (vresult_uint,  expected_vresult_uint)) {
#if DEBUG
    printf("ERROR, vec_splati_ins (vsrc_a_uint, 1, 40)\n");
    for(i = 0; i < 4; i++)
      printf(" vresult_uint[%i] = %d, expected_vresult_uint[%i] = %d\n",
	     i, vresult_uint[i], i, expected_vresult_uint[i]);
#else
    abort();
#endif
  }
  
  vsrc_a_f = (vector float) { 2.0, 3.0, 4.0, 5.0 };
  vresult_f = (vector float) { 1.0, 1.0, 1.0, 1.0 };
  expected_vresult_f = (vector float) { 2.0, 20.1, 4.0, 20.1 };
						 
  vresult_f = vec_splati_ins (vsrc_a_f, 1, 20.1f);

  if (!vec_all_eq (vresult_f,  expected_vresult_f)) {
#if DEBUG
    printf("ERROR, vec_splati_ins (vsrc_a_f, 1, 20.1)\n");
    for(i = 0; i < 4; i++)
      printf(" vresult_f[%i] = %f, expected_vresult_f[%i] = %f\n",
	     i, vresult_f[i], i, expected_vresult_f[i]);
#else
    abort();
#endif
  }

  return 0;
}

/* { dg-final { scan-assembler-times {\mxxspltiw\M} 2 } } */
/* { dg-final { scan-assembler-times {\mxxspltidp\M} 2 } } */
/* { dg-final { scan-assembler-times {\mxxsplti32dx\M} 3 } } */


