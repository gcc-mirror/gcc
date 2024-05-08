/* { dg-do compile } */
/* { dg-options "-march=rv64ifd -mabi=lp64d -mcmodel=medlow -O" } */

_Bfloat16 gvar = 9.87654;
union U
{
  unsigned short i16;
  _Bfloat16 f16;
};

_Bfloat16
test1 (unsigned short input)
{
  union U tmp;
  tmp.i16 = input;
  return tmp.f16;
}

_Bfloat16
test2 ()
{
  return 1.234f;
}

_Bfloat16
test3 ()
{
  return gvar;
}

_Bfloat16
test ()
{
  return 0.0f;
}

/* { dg-final { scan-assembler-times "li\[ \t\]" 4 } } */
/* { dg-final { scan-assembler-times "fmv\.w\.x\[ \t\]" 4 } } */
