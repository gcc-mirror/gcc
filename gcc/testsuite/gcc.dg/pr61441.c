/* { dg-do run { target { *-*-linux* *-*-gnu* } } } */
/* { dg-options "-O1 -lm -fexcess-precision=standard -fsignaling-nans" } */
/* { dg-add-options ieee } */
/* { dg-require-effective-target issignaling } */

#define _GNU_SOURCE
#include <stdio.h>
#include <math.h>

void conversion()
{
  float sNaN = __builtin_nansf ("");
  double x = (double) sNaN;
  if (issignaling(x))
  {
    __builtin_abort();
  }
}

enum op {Add, Mult, Div, Abs};

void operation(enum op t)
{
  float x, y;
  float sNaN = __builtin_nansf ("");
  switch (t)
  {
    case Abs:
      x = fabs(sNaN);
      break;
    case Add:
      x = sNaN + 2.0;
      break;
    case Mult:
      x = sNaN * 2.0;
      break;
    case Div:
    default:
      x = sNaN / 2.0;
      break;
  }
  if (t == Abs)
  {
    if (!issignaling(x))
    {
      __builtin_abort();
    }
  }
  else if (issignaling(x))
  {
    __builtin_abort();
  }
}

int main (void)
{
  conversion();
  operation(Add);
  operation(Mult);
  operation(Div);
#if __FLT_EVAL_METHOD__ == 0
  operation(Abs);
#endif
  return 0;
}
