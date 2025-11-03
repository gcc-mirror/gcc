/* { dg-additional-options "-march=armv8-a+sve" { target aarch64*-*-* } } */
/* Check that we don't ICE.  */
int a;
int b;
int main() {
  for (char t = 0; t < 14; t += 2)
    for (int u = 0; u < 242; u += 4) {
      a = a < 0 ? a : 0;
      b = b < 0 ? b : 0;
    }
}

/* { dg-final { scan-tree-dump-times "optimized: loop vectorized" 1 "vect" { target aarch64*-*-* } } } */
