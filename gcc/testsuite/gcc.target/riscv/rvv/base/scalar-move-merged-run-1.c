/* { dg-do run { target { riscv_v } } } */
/* { dg-options "-O3" } */

#define TEST_VAL 2

typedef long long vl_t __attribute__((vector_size(2 * sizeof (long long))));

void init_vl (vl_t *u)
{
  vl_t t;
  long long *p = (long long *)&t;

  p[0] = p[1] = TEST_VAL;

  *u = t;
}

int
main ()
{
  vl_t vl = {};

  init_vl (&vl);

  if (vl[0] != TEST_VAL || vl[1] != TEST_VAL)
    __builtin_abort ();

  return 0;
}
