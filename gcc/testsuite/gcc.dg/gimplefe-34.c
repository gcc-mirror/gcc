/* { dg-do compile } */
/* { dg-options "-fgimple" } */
/* { dg-additional-options "-fno-common" { target hppa*-*-hpux* } } */

typedef unsigned char v16qi __attribute__((vector_size(16)));
typedef unsigned char v8qi __attribute__((vector_size(8)));

v16qi x;

void __GIMPLE foo (unsigned char *p)
{
  v8qi _2;
  v16qi _3;

bb_2:
  _2 = __MEM <v8qi, 8> (p_1(D));
  _3 = _Literal (v16qi) { _2, _Literal (v8qi) { _Literal (unsigned char) 0, _Literal (unsigned char) 0, _Literal (unsigned char) 0, _Literal (unsigned char) 0, _Literal (unsigned char) 0, _Literal (unsigned char) 0, _Literal (unsigned char) 0 } };
  x = _3;
  return;
}

