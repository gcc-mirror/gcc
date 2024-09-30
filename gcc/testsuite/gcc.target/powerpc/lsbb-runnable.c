/*
 Test the least significant bit by byte instruction
    xvtlsbb BF,XB
 Using the builtins
    int vec_test_lsbb_all_zeros (vector unsigned char);
    int vec_test_lsbb_all_ones (vector unsigned char);
 */

/* { dg-do run { target { power10_hw } } } */
/* { dg-do link { target { ! power10_hw } } } */
/* { dg-require-effective-target power10_ok } */
/* { dg-options "-fno-inline -mdejagnu-cpu=power10 -O2" } */

#include <altivec.h>
#include <stdio.h>

void abort (void);

#define ITERS 7
vector signed char input_svec[ITERS] = {
  {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
  {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
  {0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1},
  {1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0},
  {0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff},
  {0xfe, 0xfe, 0xfe, 0xfe, 0xfe, 0xfe, 0xfe, 0xfe, 0xfe, 0xfe, 0xfe, 0xfe, 0xfe, 0xfe, 0xfe, 0xfe},
  {0xfe, 0xfa, 0xfb, 0xfc, 0xfd, 0xfe, 0xf0, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7, 0xf8, 0xf9}
};

vector unsigned char input_uvec[ITERS] = {
  {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
  {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
  {0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1},
  {1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0},
  {0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff},
  {0xfe, 0xfe, 0xfe, 0xfe, 0xfe, 0xfe, 0xfe, 0xfe, 0xfe, 0xfe, 0xfe, 0xfe, 0xfe, 0xfe, 0xfe, 0xfe},
  {0xfe, 0xfa, 0xfb, 0xfc, 0xfd, 0xfe, 0xf0, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7, 0xf8, 0xf9}
};

vector bool char input_bvec[ITERS] = {
  {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
  {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},
  {0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1},
  {1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0, 1, 0},
  {0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff},
  {0xfe, 0xfe, 0xfe, 0xfe, 0xfe, 0xfe, 0xfe, 0xfe, 0xfe, 0xfe, 0xfe, 0xfe, 0xfe, 0xfe, 0xfe, 0xfe},
  {0xfe, 0xfa, 0xfb, 0xfc, 0xfd, 0xfe, 0xf0, 0xf1, 0xf2, 0xf3, 0xf4, 0xf5, 0xf6, 0xf7, 0xf8, 0xf9}
};

int expected_allzeros_results[ITERS] = {1, 0, 0, 0, 0, 1, 0};
int expected_allones_results[ITERS] =  {0, 1, 0, 0, 1, 0, 0};

int test_for_zeros_uc(vector unsigned char vuc) {
  return vec_test_lsbb_all_zeros(vuc);
}

int test_for_zeros_sc(vector signed char vsc) {
  return vec_test_lsbb_all_zeros(vsc);
}

int test_for_zeros_bc(vector bool char vbc) {
  return vec_test_lsbb_all_zeros(vbc);
}

int test_for_ones_sc(vector signed char vsc) {
  return vec_test_lsbb_all_ones(vsc);
}

int test_for_ones_uc(vector unsigned char vuc) {
  return vec_test_lsbb_all_ones(vuc);
}

int test_for_ones_bc(vector bool char vbc) {
  return vec_test_lsbb_all_ones(vbc);
}

int main ()
{
  int allzeros, allones;
  int iter;
  int failcount=0;
  vector signed char srcvec_sc;
  vector unsigned char srcvec_uc;
  vector bool char srcvec_bc;

  /* Signed type tests.  */
  for (iter=0;iter<ITERS;iter++) {
    srcvec_sc = input_svec[iter];
    allzeros = test_for_zeros_sc(srcvec_sc);
    allones = test_for_ones_sc(srcvec_sc);
    if (allzeros != expected_allzeros_results[iter]) {
      printf("fail on allzero signed char check. iter %d, result was %d \n",
	     iter, allzeros);
      failcount++;
    }
    if (allones != expected_allones_results[iter]) {
      printf("fail on allones signed char check. iter %d, result was %d \n",
	     iter, allones);
      failcount++;
    }
  }

  if (failcount)
    abort();

  /* Unsigned type tests.  */
  for (iter=0;iter<ITERS;iter++) {
    srcvec_uc = input_uvec[iter];
    allzeros = test_for_zeros_uc(srcvec_uc);
    allones = test_for_ones_uc(srcvec_uc);
    if (allzeros != expected_allzeros_results[iter]) {
      printf("fail on allzero unsigned char check. iter %d, result was %d \n",
	     iter, allzeros);
      failcount++;
    }
    if (allones != expected_allones_results[iter]) {
      printf("fail on allones unsigned char check. iter %d, result was %d \n",
	     iter, allones);
      failcount++;
    }
  }

  if (failcount)
    abort();

  /* Bool char type tests.  */
  for (iter=0;iter<ITERS;iter++) {
    srcvec_bc = input_bvec[iter];
    allzeros = test_for_zeros_bc(srcvec_bc);
    allones = test_for_ones_bc(srcvec_bc);
    if (allzeros != expected_allzeros_results[iter]) {
      printf("fail on allzero bool char check. iter %d, result was %d \n",
	     iter, allzeros);
      failcount++;
    }
    if (allones != expected_allones_results[iter]) {
      printf("fail on allones bool char check. iter %d, result was %d \n",
	     iter, allones);
      failcount++;
    }
  }

  if (failcount)
    abort();

  
  return 0;
}

