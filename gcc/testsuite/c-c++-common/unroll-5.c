/* { dg-do compile } */

extern void bar (int);

int j;

void test (void)
{
  #pragma GCC unroll 4+4
  for (unsigned long i = 1; i <= 8; ++i)
    bar(i);

  #pragma GCC unroll -1	/* { dg-error "requires an assignment-expression that evaluates to a non-negative integral constant less than" } */
  for (unsigned long i = 1; i <= 8; ++i)
    bar(i);

  #pragma GCC unroll 20000000000	/* { dg-error "requires an assignment-expression that evaluates to a non-negative integral constant less than" } */
  for (unsigned long i = 1; i <= 8; ++i)
    bar(i);

  #pragma GCC unroll j	/* { dg-error "requires an assignment-expression that evaluates to a non-negative integral constant less than" } */
                        /* { dg-error "cannot appear in a constant-expression|is not usable in a constant expression" "" { target c++ } .-1 } */
  for (unsigned long i = 1; i <= 8; ++i)
    bar(i);

  #pragma GCC unroll  4.2	/* { dg-error "requires an assignment-expression that evaluates to a non-negative integral constant less than" } */
  for (unsigned long i = 1; i <= 8; ++i)
    bar(i);
}
