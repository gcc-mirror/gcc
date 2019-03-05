/* PR target/88278 */
/* { dg-do compile } */
/* { dg-options "-O2 -msse2 -mno-sse3 -fgimple -masm=att" } */
/* { dg-final { scan-assembler-times "movq\[ \t]+\\(" 2 } } */
/* { dg-final { scan-assembler-not "punpcklqdq\[ \t]+" } } */
/* { dg-final { scan-assembler-not "pxor\[ \t]+" } } */

typedef unsigned char v16qi __attribute__((vector_size(16)));
typedef unsigned char v8qi __attribute__((vector_size(8)));
typedef unsigned int v4si __attribute__((vector_size(16)));
typedef unsigned int v2si __attribute__((vector_size(8)));

v16qi __GIMPLE foo (unsigned char *p)
{
  v8qi _2;
  v16qi _3;

bb_2:
  _2 = __MEM <v8qi, 8> (p_1(D));
  _3 = _Literal (v16qi) { _2, _Literal (v8qi) { _Literal (unsigned char) 0, _Literal (unsigned char) 0, _Literal (unsigned char) 0, _Literal (unsigned char) 0, _Literal (unsigned char) 0, _Literal (unsigned char) 0, _Literal (unsigned char) 0 } };
  return _3;
}


v4si __GIMPLE bar (unsigned int *p)
{
  v2si _2;
  v4si _3;

bb_2:
  _2 = __MEM <v2si, 32> (p_1(D));
  _3 = _Literal (v4si) { _2, _Literal (v2si) { 0u, 0u } };
  return _3;
}
