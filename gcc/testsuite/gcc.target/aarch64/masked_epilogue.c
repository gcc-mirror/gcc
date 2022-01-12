/* { dg-do compile } */
/* { dg-options "-O2 -ftree-vectorize -fdump-tree-vect-details -march=armv8-a+sve -msve-vector-bits=scalable" } */

void f(unsigned char y[restrict],
       unsigned char x[restrict], int n) {
  for (int i = 0; i < n; ++i)
    y[i] = (y[i] + x[i] + 1) >> 1;
}

/* { dg-final { scan-tree-dump {LOOP EPILOGUE VECTORIZED \(MODE=VNx} "vect" } } */
