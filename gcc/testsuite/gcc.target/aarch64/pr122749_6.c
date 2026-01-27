/* { dg-do run { target arm_v8_neon_hw } } */
/* { dg-additional-options "-Ofast -std=gnu99 --param vect-epilogues-nomask=0 -fwrapv -fdump-tree-vect-details -fdump-tree-widening_mul" } */

typedef float elem_t;

__attribute__ ((noipa))
elem_t
foo2 (elem_t *buf, int len)
{
  elem_t x = 0;

  for (int i = 0; i < len; i++)
    x += (elem_t) i * buf[i];

  return x;
}

static elem_t
reference (elem_t *buf, int len)
{
  elem_t x = 0;

#pragma GCC novector
  for (int i = 0; i < len; i++)
    x += (elem_t) i * buf[i];

  return x;
}

int
main (void)
{
  elem_t buf[] = { 1.0f, 2.0f, 1.0f, 2.0f, 1.0f, 2.0f };
  int len = sizeof (buf) / sizeof (buf[0]);
  elem_t want = reference (buf, len);
  elem_t got = foo2 (buf, len);

  if (want != got)
    __builtin_abort ();

  return 0;
}

/* { dg-final { scan-tree-dump-times "\\.FMA" 4 "widening_mul" } } */
/* { dg-final { scan-tree-dump-times "vectorized 1 loop" 1 "vect" } } */
