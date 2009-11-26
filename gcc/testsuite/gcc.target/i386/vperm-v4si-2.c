/* { dg-do run } */
/* { dg-require-effective-target ssse3 } */
/* { dg-options "-O -mssse3" } */

#include "isa-check.h"

typedef int S;
typedef int V __attribute__((vector_size(16)));
typedef int IV __attribute__((vector_size(16)));
typedef union { S s[4]; V v; } U;

static U i[2], b, c;

extern int memcmp (const void *, const void *, __SIZE_TYPE__);
#define assert(T) ((T) || (__builtin_trap (), 0))

#define TEST(E0, E1, E2, E3) \
  b.v = __builtin_ia32_vec_perm_v4si (i[0].v, i[1].v, (IV){E0, E1, E2, E3}); \
  c.s[0] = i[0].s[E0]; \
  c.s[1] = i[0].s[E1]; \
  c.s[2] = i[0].s[E2]; \
  c.s[3] = i[0].s[E3]; \
  __asm__("" : : : "memory"); \
  assert (memcmp (&b, &c, sizeof(c)) == 0);

#include "vperm-4-2.inc"

int main()
{
  i[0].s[0] = 0;
  i[0].s[1] = 1;
  i[0].s[2] = 2;
  i[0].s[3] = 3;
  i[0].s[4] = 4;
  i[0].s[5] = 5;
  i[0].s[6] = 6;
  i[0].s[7] = 7;

  check();
  return 0;
}
