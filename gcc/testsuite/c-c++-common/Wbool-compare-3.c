/* { dg-do compile } */
/* { dg-options "-Wbool-compare" } */

#ifndef __cplusplus
# define bool _Bool
#endif

#define A 0
#define B 1

int
foo (int i, bool b)
{
  int r = 0;

  r += i <= (A || B);
  r += i <= b;
  r += i <= A;
  r += i < (A || B);
  r += i < b;
  r += i < A;
  r += i > (A || B);
  r += i > b;
  r += i > A;
  r += i >= (A || B);
  r += i >= b;
  r += i >= A;

  return r;
}
