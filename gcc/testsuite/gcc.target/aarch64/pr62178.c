/* { dg-do compile } */
/* { dg-options "-O3" } */

int a[30 +1][30 +1], b[30 +1][30 +1], r[30 +1][30 +1];

void foo (void) {
  int i, j, k;

  for ( i = 1; i <= 30; i++ )
    for ( j = 1; j <= 30; j++ ) {
      r[i][j] = 0;
      for(k = 1; k <= 30; k++ )
        r[i][j] += a[i][k]*b[k][j];
    }
}

/* { dg-final { scan-assembler "ldr\\ts\[0-9\]+, \\\[x\[0-9\]+, \[0-9\]+\\\]!" } } */
/* { dg-final { scan-assembler "ldr\\tq\[0-9\]+, \\\[x\[0-9\]+\\\], \[0-9\]+" } } */
/* { dg-final { scan-assembler "mla\\tv\[0-9\]+\.4s, v\[0-9\]+\.4s, v\[0-9\]+\.s\\\[0\\\]" } } */
