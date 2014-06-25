/* { dg-do run } */
/* { dg-require-effective-target arm_neon_hw } */
/* { dg-options "-O2 -fno-common -ftree-vectorize -mno-unaligned-access" }  */
/* { dg-add-options arm_neon } */


/* Test for-mno-unaligned-access and -ftree-vectorize  and results bus error. */
#define N 128

char ia[N];
char ib[N+1];

int main() {
  int i;
  for(i = 0; i < N; ++i) {
    ia[i] = ib[i + 1];
  }

  return 0;
}

