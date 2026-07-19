/* { dg-do run } */
/* { dg-options "-O1 -march=x86-64" } */

#include <stdint.h>

typedef int64_t v2i64 __attribute__ ((vector_size (16)));
void *g4, *f3_p2;
int8_t g15, g23;
v2i64 g21;
_Bool g22, f3_c4;
__attribute__ ((__vector_size__ (16 * sizeof (int)))) int g26;
int16_t g29;

__attribute__((noipa, noinline, target("avx2")))
void
do_test (v2i64 a1)
{
  int16_t v3;
lbl_entry:
  f3_p2 = &a1;
  v3 = g29;
  if (g22)
    goto lbl_bf4;
  f3_c4 = v3 - 709;
  if (f3_c4)
    return;
lbl_bf4:
  g21 = a1;
  uint8_t __ov_tmp_g15;
  g15 = __ov_tmp_g15;
  a1[0] = 0;
  g26 = g26 == ~g26;
  g4 = f3_p2;
  g23 = 0;
  goto lbl_entry;
}

int
main (void)
{
 if (__builtin_cpu_supports ("avx2"))
   do_test (g21);

  return 0;
}
