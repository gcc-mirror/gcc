/* { dg-do run } */
/* { dg-options "-msse" { target { { i?86-*-* x86_64-*-* } && ilp32 } } } */
/* { dg-require-effective-target sse_runtime { target { { i?86-*-* x86_64-*-* } && ilp32 } } } */

typedef float v2sf __attribute__ ((vector_size (2 * sizeof(float))));

static void test (void)
{
  v2sf a = {1.0, 0.0};
  v2sf b = {0.0, 1.0};
  v2sf d;
  d = a + b;
}

int main ()
{
  test ();
  return 0;
}
