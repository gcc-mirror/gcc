/* PR tree-optimization/60971 */
/* { dg-do run } */

#ifndef __cplusplus
#define bool _Bool
#endif

volatile unsigned char c;

__attribute__((noinline)) unsigned char
foo (void)
{
  return c;
}

__attribute__((noinline)) bool
bar (void)
{
  return foo () & 1;
}

int
main ()
{
  c = 0x41;
  c = bar ();
  if (c != 1)
    __builtin_abort ();
  c = 0x20;
  c = bar ();
  if (c != 0)
    __builtin_abort ();
  return 0;
}
