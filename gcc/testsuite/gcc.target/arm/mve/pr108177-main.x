#include <arm_mve.h>
extern void abort (void);

__attribute__ ((noipa)) void
write_expected (uint32x4_t v, void *a)
{
  TYPE _v = (TYPE) v;
  INTRINSIC (a, _v);
}

void test (uint32x4_t, void *, mve_pred16_t, mve_pred16_t);

int main(void)
{
  uint32x4_t v = {0, 1, 2, 3};
  uint32_t actual[] = {0, 0, 0, 0};
  uint32_t expected[] = {0, 0, 0, 0};

  write_expected (v, &(expected[0]));

  mve_pred16_t p1 = 0xff00;
  mve_pred16_t p2 = 0x00ff;

  test (v, (void *)&actual[0], p1, p2);

  if (__builtin_memcmp (&actual[0], &expected[0], 16) != 0)
    abort ();

  return 0;
}

