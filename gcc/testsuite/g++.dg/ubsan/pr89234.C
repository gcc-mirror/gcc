// PR rtl-optimization/89234
// { dg-do compile { target dfp } }
// { dg-options "-O2 -fnon-call-exceptions -fsanitize=null" }

typedef float __attribute__((mode (SD))) _Decimal32;

void
foo (_Decimal32 *b, _Decimal32 c)
{
  *b = c + 1.5;
}
