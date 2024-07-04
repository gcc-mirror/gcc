/* { dg-do compile } */
/* { dg-options "-O2" } */

typedef __UINT32_TYPE__ uint32_t;
typedef uint32_t uint32x4 __attribute__((vector_size(16)));
typedef struct {
  uint32x4 b, d;
} prng_t;
prng_t prng_rand_128_r_x;
int main_flags;
int main() {
  uint32_t ref_crc[] = {7, 3};
  uint32x4 e = (prng_rand_128_r_x.b << 27) + (prng_rand_128_r_x.b >> 32 - 27);
  prng_rand_128_r_x.d = e;
  if (ref_crc[main_flags])
    __builtin_abort ();
  return 0;
}
