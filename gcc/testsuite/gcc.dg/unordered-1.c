/* { dg-do compile } */

void bar (int);

void foo (int i, float f)
{
  bar (__builtin_isgreater (i, i));	/* { dg-error "non-floating-point" } */
  bar (__builtin_isgreater (f, i));
  bar (__builtin_isgreater (i, f));
  bar (__builtin_isgreater (f, 2));
  bar (__builtin_isgreater (f, 2.0f));
  bar (__builtin_isgreater (f, 2.0));
  bar (__builtin_isgreater (2, f));
  bar (__builtin_isgreater (2.0f, f));
  bar (__builtin_isgreater (2.0, f));
  bar (__builtin_isgreater (&f, i));	/* { dg-error "non-floating-point" } */
  bar (__builtin_isgreater (f, &i));	/* { dg-error "non-floating-point" } */
}
