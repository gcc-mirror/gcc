typedef char __attribute__((__vector_size__ (64))) U;
typedef int __attribute__((__vector_size__ (64))) V;

int g;
U u;

static inline __attribute__((__always_inline__)) void
bar (short a, short b, V w)
{
  V v = __builtin_shufflevector ((V) { }, a % (0 != w), 17, 22, 20, 15,
				 20, 23, 17, 20, 16, 21, 16, 19, 18, 14, 15,
				 14) ^ b;
  g *= __builtin_memcmp_eq (0, 0, 2);
  v |= 6;
  __builtin_ilogb (0);
  u = (U) w + (U) v;
}

void
foo (void)
{
  bar (5, 4, (V){30, 4, 1, 5, 6});
}
