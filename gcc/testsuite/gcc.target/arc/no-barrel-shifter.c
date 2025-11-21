/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-skip-if "" { { barrelshifter } } } */
int fromfloat(float fx);

float foo(float fx)
{
  int x = fromfloat(fx);
  int sign = (x >> 31) & 1;
  unsigned int mag = x & 0x7fffffff;

  if (mag > 0x7f800000)
    return fx;
  if (mag == 0x7f800000)
    return (sign == 0);
  return fx * (27 + sign);
}

/* { dg-final { scan-assembler-not "add.f\\s\+\[0-9\]\+,r\[0-9\]\+,r\[0-9\]\+\\n\\s\+beq.d" } } */
