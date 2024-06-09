#include <arm_sve.h>

uint64_t u;
struct S { int i; } he;

void
foo ()
{
  svuint64_t vld_clz = svld1_u64 (svwhilelt_b64 (0, 4), (uint64_t *) &he);
  vld_clz = svclz_u64_z (svwhilelt_b64 (0, 1), vld_clz);
  svst1_u64 (svwhilelt_b64 (0, 1), &u, vld_clz);
}
