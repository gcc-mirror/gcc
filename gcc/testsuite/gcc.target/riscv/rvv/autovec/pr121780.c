/* { dg-do run } */
/* { dg-require-effective-target riscv_v_ok } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O1 -Wno-overflow" } */

typedef char int8_t;
typedef unsigned char uint8_t;
typedef short int16_t;
typedef int int32_t;
typedef unsigned int uint32_t;
typedef long int64_t;
typedef unsigned long uint64_t;

typedef uint8_t a;
typedef a b __attribute__((vector_size(4)));
typedef a c __attribute__((vector_size(8)));
int d[10];
struct {
  int16_t e;
} f;
int64_t g;
int32_t j(int32_t, int32_t, int8_t, uint8_t, int64_t) {
  c k = {20927, 0, 9, 6, 6, 2};
  int *l = &d[1];
  for (;;) {
    asm goto("" : : : : m);
    k = __builtin_shufflevector(
        __builtin_shufflevector(k, k, 3, 1, 1, 9, 4, 5, 9, 0),
        __builtin_shufflevector((b){}, (b){}, 1, 3, 7, 6, 0, 0, 6, 2), 10, 2, 1,
        7, 7, 7, 4, 5);
    for (; f.e >= 0;) {
      *l = k[7];
      uint32_t n[][7][6] = {};
      return n[0][5][0];
    m:
    }
  }
}

int main() {
  uint64_t o = 1;
  j(o, g, g, o, 0);
  int BS_CHECKSUM = 5;
  for (int i = 0; i < 10; i++)
    BS_CHECKSUM ^= d[i];
  if (BS_CHECKSUM != 7)
    __builtin_abort ();
}
