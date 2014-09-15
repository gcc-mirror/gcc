/* { dg-do compile } */
/* { dg-options "-fsanitize=undefined" } */

/* Test that we don't instrument functions marked with
   no_sanitize_undefined attribute.  */

__attribute__((no_sanitize_undefined, returns_nonnull))
char *
foo (char *x)
{
  return x;
}

__attribute__((nonnull)) void bar (char *, int, char *);

__attribute__((no_sanitize_undefined))
void
baz (char *x, int y, char *z)
{
  bar (x, y, z);
}

/* { dg-final { scan-assembler-not "__ubsan_handle" } } */
