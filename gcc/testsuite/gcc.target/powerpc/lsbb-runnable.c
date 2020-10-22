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
vector char input_vec[ITERS] = {
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

int test_for_zeros(vector char vc) {
  return vec_test_lsbb_all_zeros(vc);
}

int test_for_ones(vector char vc) {
  return vec_test_lsbb_all_ones(vc);
}

int main ()
{
int allzeros,allones;
int iter;
int failcount=0;
vector char srcvec;

for (iter=0;iter<ITERS;iter++) {
  srcvec = input_vec[iter];
  allzeros = test_for_zeros(srcvec);
  allones = test_for_ones(srcvec);
  if (allzeros != expected_allzeros_results[iter]) {
    printf("fail on allzero check. iter %d, result was %d \n", iter, allzeros);
    failcount++;
  }
  if (allones != expected_allones_results[iter]) {
    printf("fail on allones check. iter %d, result was %d \n", iter, allones);
    failcount++;
  }
}

if (failcount)
  abort();
return 0;
}

