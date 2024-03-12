/* PR tree-optimization/109778 */
/* { dg-lto-do run } */
/* { dg-lto-options { "-O2 -flto" } } */
/* { dg-require-effective-target int32 } */

int bar (int);

__attribute__((noipa)) int
foo (int x)
{
  x = bar (x);
  x = (x << 16) | (int) ((unsigned) x >> 16);
  return x & 0x10000000;
}

int
main ()
{
  if (foo (0) || foo (-1))
    __builtin_abort ();
  return 0;
}
