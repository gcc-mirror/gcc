/* { dg-do run } */
/* { dg-require-effective-target riscv_v_ok } */
/* { dg-options "-march=rv64gcv -mabi=lp64d -O1" } */

typedef unsigned char uint8_t;
typedef short int16_t;
typedef unsigned short uint16_t;
typedef int int32_t;
typedef unsigned int uint32_t;
typedef unsigned long uint64_t;

typedef int16_t a;
typedef uint16_t b;
typedef b c __attribute__((vector_size(8)));
typedef a d __attribute__((vector_size(8)));
typedef b e __attribute__((vector_size(16)));
int f[10];
uint16_t g, l, k;
int32_t j;
int32_t m(uint32_t, const uint32_t *);
int32_t *n(uint64_t, uint8_t);
uint8_t o() {
  int p = 7;
  m(p, 0);
  return 0;
}
int32_t m(uint32_t, const uint32_t *) {
  int32_t q = l;
  n(q, 0);
  return k;
}
int32_t *n(uint64_t, uint8_t) {
  c r = {1, 6, 8, 30};
  d s = {};
  e u;
  int *t = &f[7];
  for (j = 9; j; j--) {
    *t ^= s[0];
    r = __builtin_shufflevector(r * r, r, 2, 5, 7, 2);
    for (g = 0; g != 1; g = g + 3)
      s = (d)__builtin_shufflevector(
          __builtin_shufflevector(u, (e){}, 3, 1, 1, 1), r, 4, 6, 0, 2);
  }
  return &j;
}
int main() {
  o();
  int BS_CHECKSUM = 5;
  for (int i = 0; i < 10; i++)
    BS_CHECKSUM ^= f[i];
  if (BS_CHECKSUM != 0xffffffffffffced1ull)
    __builtin_abort ();
}
