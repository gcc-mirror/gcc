/* { dg-do compile } */
/* { dg-require-effective-target vect_dotprod_hisi } */
/* Ensure that, given the same input datatype, both the two-way and four-way
   dot products are autovectorized, with the correct operation then selected
   based on the distinct output types.  */
#include <stdint.h>

uint32_t udot4(int n, uint8_t* data) {
  uint32_t sum = 0;
  for (int i=0; i<n; i+=1) {
    sum += data[i] * data[i];
  }
  return sum;
}

int32_t sdot4(int n, int8_t* data) {
  int32_t sum = 0;
  for (int i=0; i<n; i+=1) {
    sum += data[i] * data[i];
  }
  return sum;
}

uint32_t udot2(int n, uint16_t* data) {
  uint32_t sum = 0;
  for (int i=0; i<n; i+=1) {
    sum += data[i] * data[i];
  }
  return sum;
}

int32_t sdot2(int n, int16_t* data) {
  int32_t sum = 0;
  for (int i=0; i<n; i+=1) {
    sum += data[i] * data[i];
  }
  return sum;
}

/* { dg-final { scan-tree-dump-times "vectorized 1 loops" 4 "vect" } } */
/* { dg-final { scan-tree-dump-times "vect_recog_dot_prod_pattern: detected" 4 "vect" } } */
