/* PR target/95535 */
/* { dg-do compile { target lp64 } } */
/* { dg-options "-O2 -mlzcnt" } */
/* { dg-final { scan-assembler-not "cltq" } } */

unsigned int foo (void);

unsigned long
f1 (unsigned int x)
{
  return __builtin_clz (x);
}

unsigned long
f2 (unsigned int x)
{
  return (unsigned) __builtin_clz (x);
}

unsigned long
f3 (unsigned int x)
{
  return __builtin_clz (x) & 63ULL;
}

unsigned long
f4 (unsigned int x)
{
  return __builtin_clz (x) & 1023ULL;
}

unsigned long
f5 (void)
{
  return __builtin_clz (foo ());
}

unsigned long
f6 (void)
{
  return (unsigned) __builtin_clz (foo ());
}

unsigned long
f7 (void)
{
  return __builtin_clz (foo ()) & 63ULL;
}

unsigned long
f8 (void)
{
  return __builtin_clz (foo ()) & 1023ULL;
}
