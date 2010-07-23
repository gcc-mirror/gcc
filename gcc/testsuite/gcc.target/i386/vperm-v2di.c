/* { dg-do run } */
/* { dg-options "-O -msse2" } */
/* { dg-require-effective-target sse2 } */

#include "isa-check.h"
#include "sse-os-support.h"

typedef long long S;
typedef long long V __attribute__((vector_size(16)));
typedef long long IV __attribute__((vector_size(16)));
typedef union { S s[2]; V v; } U;

static U i[2], b, c;

extern int memcmp (const void *, const void *, __SIZE_TYPE__);
#define assert(T) ((T) || (__builtin_trap (), 0))

#define TEST(E0, E1) \
  b.v = __builtin_ia32_vec_perm_v2di (i[0].v, i[1].v, (IV){E0, E1}); \
  c.s[0] = i[0].s[E0]; \
  c.s[1] = i[0].s[E1]; \
  __asm__("" : : : "memory"); \
  assert (memcmp (&b, &c, sizeof(c)) == 0);

#include "vperm-2-2.inc"

int main()
{
  check_isa ();

  if (!sse_os_support ())
    exit (0);

  i[0].s[0] = 0;
  i[0].s[1] = 1;
  i[0].s[2] = 2;
  i[0].s[3] = 3;

  check();
  return 0;
}
