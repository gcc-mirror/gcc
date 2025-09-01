/* { dg-do run } */
/* { dg-require-effective-target riscv_v_ok } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O2" } */

typedef unsigned long uint64_t;
typedef unsigned int uint32_t;
typedef unsigned char uint8_t;
typedef uint8_t a __attribute__((vector_size(4)));
int b, c;

uint64_t d() {
  a e = {5, 9, 1, 5};
  a bla = {0, 0, 0, 0};
  int *f = &b;
  uint32_t g = 0;
  int i = 0;
  for (; i < 2; i++)
    for (c = 0; c <= 2; c++) {
      *f ^= e[3] + 9;
      e = __builtin_shufflevector(
          ~__builtin_shufflevector(bla, e, 1, 4, 3, 4), e, 0, 1, 1, 7);
    }
  return g;
}

int main() {
  int j = d ();
  if (b != 0)
    __builtin_abort ();
}
