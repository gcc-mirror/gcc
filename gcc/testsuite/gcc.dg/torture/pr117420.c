/* PR tree-optimization/117420 */
/* { dg-do run } */

int a;

__attribute__((noipa)) int
foo ()
{
  int b = -(1 | -(a < 1));
  int c = (~b & 2) / 2;
  if (b != 1)
    __builtin_abort ();
}

__attribute__((noipa)) int
bar ()
{
  int b = -(1 | -(a < 1));
  int c = (~b & 2) / 2;
  if (b != -1)
    __builtin_abort ();
}

__attribute__((noipa)) int
baz ()
{
  int b = -(1 | -(a < 1));
  int c = (~b & 2) / 2;
  if (c != 1)
    __builtin_abort ();
}

__attribute__((noipa)) int
qux ()
{
  int b = -(1 | -(a < 1));
  int c = (~b & 2) / 2;
  if (c != 0)
    __builtin_abort ();
}

int
main ()
{
  foo ();
  a = 1;
  bar ();
  a = 0;
  baz ();
  a = 1;
  qux ();
}
