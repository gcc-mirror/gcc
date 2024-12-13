/* { dg-do run { target { riscv_v } } } */
/* { dg-additional-options "-std=c99" } */

#define STEP 10

char d[225];
int e[STEP];

int main() {
  // store 0, 10, 20, 30, 40, 50, 60, 70, 80, 90
  for (long h = 0; h < STEP; ++h)
    d[h * STEP] = 9;

  // load 30, 40, 50, 60, 70, 80, 90
  // store 3,  4,  5,  6,  7,  8,  9
  for (int h = 3; h < STEP; h += 1)
    e[h] = d[h * STEP];

  if (e[5] != 9) {
    __builtin_abort ();
  }

  return 0;
}
