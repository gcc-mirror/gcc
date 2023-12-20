/* { dg-do compile } */
/* { dg-options "-march=rv64ifd -mabi=lp64d -O" } */

_Float16 gvar = 9.87654;

union U {
  unsigned short i16;
  _Float16 f16;
};

_Float16 test1(unsigned short input)
{
  union U tmp;
  tmp.i16 = input;

  return tmp.f16;
}

_Float16 test2()
{
  return 1.234f;
}

_Float16 test3()
{
  return gvar;
}

_Float16 test()
{
  return 0.0f;
}

/* { dg-final { scan-assembler-times "li\[ \t\]" 4 } } */
/* { dg-final { scan-assembler-times "fmv\.w\.x\[ \t\]" 4 } } */

