/* { dg-do run } */
/* { dg-options "-Os -mavx512bw" } */
/* { dg-require-effective-target avx512bw } */

typedef __attribute__((__vector_size__ (4))) unsigned char W;
typedef __attribute__((__vector_size__ (64))) int V;
typedef __attribute__((__vector_size__ (64))) long long Vq;

W w;
V v;
Vq vq;

static inline W
foo (short m)
{
  unsigned k = __builtin_ia32_pcmpgtq512_mask ((Vq) { }, vq, m);
  W r = (W) k + w;
  return r;
}

static inline W
foo1 (short m)
{
  unsigned k = __builtin_ia32_pcmpgtd512_mask ((V) {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1}, v, m);
  W r = (W) k + w;
  return r;
}

int
main ()
{
  if (!__builtin_cpu_supports ("avx512bw"))
    return 0;
  W y = foo1 (65535);
  if (!y[0] || !y[1] || y[2] || y[3])
    __builtin_abort();
  W x = foo (65535);
  if (x[0] || x[1] || x[2] || x[3])
    __builtin_abort();

  return 0;
}
