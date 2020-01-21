/* { dg-do run { target aarch64_sve_hw } } */
/* { dg-additional-options "-O2" } */

#include <arm_sve.h>

struct s { int x[32]; };

void __attribute__((noipa)) consume (void *ptr) {}

void __attribute__((noipa))
check_var (svint32_t *ptr)
{
  svbool_t pg = svptrue_b8 ();
  if (svptest_any (pg, svcmpne (pg, *ptr, svindex_s32 (0, 1))))
    __builtin_abort ();
}

int
main (void)
{
  svint32_t res = svindex_s32 (0, 1);
  {
    __SVBool_t pg = svptrue_b8 ();
    consume (&pg);
  }
  {
    struct s zeros = { 0 };
    consume (&zeros);
  }
  check_var (&res);
  return 0;
}
