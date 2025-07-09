/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-fsigned-char -fno-strict-aliasing -fwrapv" } */
/* { dg-additional-options "-march=rv64gcv_zvl1024b -mrvv-vector-bits=zvl -mrvv-max-lmul=m8 -O3" { target { riscv_v } } } */

char g;
unsigned char h;
int i[9][6];
int main() {
  int k[5];
  if (g)
    goto l;
  for (; h <= 5; h++)
    i[0][h] = *k;
l:
  return 0;
}

/* { dg-final { scan-tree-dump "loop vectorized" "vect" { target riscv_v } } } */
