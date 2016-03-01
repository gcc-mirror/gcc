/* { dg-do compile } */
/* { dg-options "-fstack-check=generic" } */

/* Check that the expected warning is issued for large frames.  */

#define ONE(s) char a##s[32];
#define TEN(s) ONE(s##0) ONE(s##1) ONE(s##2) ONE(s##3) ONE(s##4) \
               ONE(s##5) ONE(s##6) ONE(s##7) ONE(s##8) ONE(s##9)
#define HUNDRED(s) TEN(s##0) TEN(s##1) TEN(s##2) TEN(s##3) TEN(s##4) \
                   TEN(s##5) TEN(s##6) TEN(s##7) TEN(s##8) TEN(s##9)

void foo(void)
{
  HUNDRED(a)
  HUNDRED(b)
} /* { dg-warning "frame size too large for reliable stack checking" } */
