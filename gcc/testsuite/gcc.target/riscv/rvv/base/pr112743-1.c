/* Test that we do not have ice when compile */
/* { dg-do compile } */
/* { dg-options "-march=rv64gc_zve32f_zvfh_zfh -mabi=lp64d -O2" } */

typedef struct test_a {
  void *x;
  char a[10];
  short b[2];
  int c[1];
} test_type_t;

void
test_copy_memory (test_type_t *out, test_type_t *in)
{
  *out = *in;
}
