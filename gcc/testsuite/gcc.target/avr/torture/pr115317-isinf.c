/* { dg-do run { target { ! avr_tiny } } } */

extern int isinff (float);
extern int isinf (double);
extern int isinfl (long double);

int tst_isinf (float x, int val)
{
  double y;
  long double z;

  __asm ("" : "+r"(x));
  if (isinff (x) != val)
    __builtin_exit (__LINE__);

  y = x;
  __asm ("" : "+r"(y));
  if (isinf (y) != val)
  __builtin_exit (__LINE__);

  z = x;
  __asm ("" : "+r"(z));
  if (isinfl (z) != val)
  __builtin_exit (__LINE__);
}

static float make_f (__UINT32_TYPE__ i)
{
  float f;
  __builtin_memcpy (&f, &i, 4);
  return f;
}

int main (void)
{
  tst_isinf (__builtin_huge_valf(), 1);
  tst_isinf (-__builtin_huge_valf(), -1);
  tst_isinf (__builtin_nanf(""), 0);
  tst_isinf (0.0f, 0);
  tst_isinf (-0.0f, 0);
  tst_isinf (1.0f, 0);
  tst_isinf (-1.0f, 0);
  tst_isinf (make_f (0x7f800000), 1);
  tst_isinf (make_f (0xff800000), -1);
  tst_isinf (make_f (0x7f7fffff), 0);
  tst_isinf (make_f (0xff7fffff), 0);
  tst_isinf (make_f (0x7f800001), 0);
  tst_isinf (make_f (0xff800001), 0);
  tst_isinf (make_f (0x00800000), 0);
  tst_isinf (make_f (0x80800000), 0);
  tst_isinf (make_f (0x00400000), 0);
  tst_isinf (make_f (0x80400000), 0);
  
  return 0;
}
