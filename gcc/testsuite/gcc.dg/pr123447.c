/* PR middle-end/123447 */
/* { dg-do compile { target aarch64*-*-* } } */
/* { dg-options "-O2 -mstrict-align" } */

typedef __attribute__((__vector_size__(32))) _Decimal64 D;
typedef __attribute__((__vector_size__(64))) int V;
typedef __attribute__((__vector_size__(64))) _Decimal64 D64;

D d;

void foo1 () {
  D _4;
  D64 _5;
  V _1;
  _1 = (V) { 9, -64497, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };
  _5 = (D64) _1;
  _4 = __builtin_shufflevector (_5, _5, 0, 1, 2, 3);
  d = _4;
}
