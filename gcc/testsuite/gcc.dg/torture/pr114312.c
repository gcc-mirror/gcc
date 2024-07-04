/* { dg-do compile } */
/* { dg-require-effective-target bitint } */

#if __BITINT_MAXWIDTH__ >= 129
typedef _BitInt(129) B;
B b;

B
foo(void)
{
  _BitInt(64) a = 1;
  a &= b * b;
  return b << a;
}
#endif
