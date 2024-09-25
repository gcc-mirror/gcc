/* { dg-do compile } */
/* { dg-options "-O2 -fdump-rtl-ext_dce" } */
/* { dg-final { scan-rtl-dump {Successfully transformed} "ext_dce" } } */

void
core_init_matrix(short* A, short* B, int seed) {
  int  order = 1;

  for (int i = 0; i < seed; i++) {
    for (int j = 0; j < seed; j++) {
      short val = seed + order;
      B[i] = val;
      A[i] = val;
      order++;
    }
  }
}
