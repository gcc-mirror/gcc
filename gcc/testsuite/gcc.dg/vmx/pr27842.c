#include <altivec.h>

extern void abort (void);
extern int memcmp (const void *, const void *, __SIZE_TYPE__);

void test (vector float *p, int n)
{
  int i;
  for (i = 0; i < n; i++)
    p[i] = vec_abs (p[i]);
}

int
main (void)
{
  vector float p = (vector float){ 0.5, 0.5, 0.5, 0.5 };
  vector float q = p;

  test (&p, 1);

  if (memcmp (&p, &q, sizeof (p)))
    abort ();

  return 0;
}

