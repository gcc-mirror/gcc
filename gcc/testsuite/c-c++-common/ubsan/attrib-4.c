/* { dg-do compile } */
/* { dg-options "-fsanitize=undefined" } */

/* Test that we don't instrument functions marked with
   no_sanitize_undefined attribute.  */

struct S { int a[16]; };

__attribute__((no_sanitize_undefined)) long long
foo (int *a, long long *b, struct S *c)
{
  return a[1] + *b + c->a[a[0]];
}

/* { dg-final { scan-assembler-not "__ubsan_handle" } } */
