/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

#include <stdarg.h>

#define N 64

unsigned char X[N] __attribute__ ((__aligned__(__BIGGEST_ALIGNMENT__))) = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63};

unsigned int
foo (int len) {
  int i;
  unsigned int result = 0;
  unsigned short prod;

  for (i=0; i<len; i++) {
    prod = X[i] * 15;
    result += prod;
  }
  return result;
}


