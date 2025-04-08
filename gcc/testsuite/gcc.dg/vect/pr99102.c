/* { dg-additional-options "-O2 -ftree-vectorize -fno-vect-cost-model -fdump-tree-vect-details" } */
/* { dg-additional-options "-march=armv8-a+sve -msve-vector-bits=256" { target aarch64_sve256_hw } } */
long a[44];
short d, e = -7;
__attribute__((noipa)) void b(char f, short j, short k, unsigned l) {
  for (int g = 0; g < 9; g += f)
    for (int b = 0; b < 90; b -= k)
      for (int h = 0; h < f; h++)
        for (short i = 0; i < 15; i += 4)
          if (!l)
            a[i] = j;
}
int main() {
  for (long c = 0; c < 2; ++c)
    a[c] = 7;
  b(9, d, e, 5);
  if (!a[0])
    __builtin_abort();
}
/* { dg-final { scan-tree-dump "MASK_SCATTER_STORE" "vect"  { target aarch64_sve256_hw } } } */
