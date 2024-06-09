/* PR tree-optimization/112734 */
/* { dg-do compile { target bitint } } */
/* { dg-options "-std=c23 -fnon-call-exceptions -ftrapv" } */

#if __BITINT_MAXWIDTH__ >= 128
_BitInt(128) out;
#else
int out;
#endif

int
main ()
{
  _BitInt(8) q[1];
  out -= 1;
}
