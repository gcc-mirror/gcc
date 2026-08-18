/* { dg-do compile } */
/* { dg-additional-options "-fgimple" } */

float a;

void __GIMPLE (ssa,guessed_local(1073741824),startwith("slp"))
b ()
{
  float [[gnu::vector_size(8)]] c;
  float _1;
  float _2;
  float _3;
  float _4;
  float _5;
  float _6;
  float _7;
  float _8;
  float _9;

  __BB(2,guessed_local(1073741824)):
  _1 = a;
  _2 = _1 + _Literal (float) 0.0;
  c_12 = __BIT_INSERT (c_11(D), _2, 32u);
  _3 = a;
  _4 = _3 + _Literal (float) 0.0;
  c_13 = __BIT_INSERT (c_12, _4, 0u);
  _5 = __BIT_FIELD_REF <float> (c_13, 32u, 32u);
  _6 = _5 * _Literal (float) 0.0;
  _7 = __BIT_FIELD_REF <float> (c_13, 32u, 32u);
  _8 = _7 * _Literal (float) 2.0e+0;
  _9 = _6 + _8;
  a = _9;
  return;

}


