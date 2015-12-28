/* { dg-do compile { target { ! ia32 } } } */
/* { dg-options "-fdiagnostics-show-caret -mno-fp-ret-in-387" } */

extern long double fminl (long double __x, long double __y);

#define TEST_EQ(FUNC) do { \
  if ((long)FUNC##l(xl,xl) != (long)xl) \
    return; \
  } while (0)

void
foo (long double xl)
{
  TEST_EQ (fmin); /* { dg-error "x87 register return with x87 disabled" } */
}

/* { dg-begin-multiline-output "" }
   TEST_EQ (fmin);
            ^
   { dg-end-multiline-output "" } */

/* { dg-begin-multiline-output "" }
   if ((long)FUNC##l(xl,xl) != (long)xl) \
             ^~~~
   { dg-end-multiline-output "" } */
