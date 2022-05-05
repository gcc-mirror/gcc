/* { dg-require-effective-target vect_int } */
/* { dg-additional-options "-Wno-psabi" } */

typedef __INT32_TYPE__   int32_t;
typedef int32_t vnx4si __attribute__((vector_size (32)));

extern void check_for_uninit (vnx4si v);

void test_1a (vnx4si *out, int a, int b)
{
  vnx4si v = (vnx4si) { 1, 2, 3, 4, 5, 6, a, b };
  check_for_uninit (v);
}

void test_1b (vnx4si *out, int a, int b)
{
  check_for_uninit ((vnx4si) { 1, 2, 3, 4, 5, 6, a, b });
}

static __attribute__((noipa)) void
called_by_test_2 (vnx4si *out, int a, int b)
{
  *out = (vnx4si) { 1, 2, 3, 4, 5, 6, a, b };
}

void test_2 (vnx4si *out, int a, int b)
{
  vnx4si v;
  called_by_test_2 (&v, a, b);
  check_for_uninit (v);
}
