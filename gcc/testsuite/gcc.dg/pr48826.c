/* { dg-options "-O -g -w" } */

void bar (int *);

void
foo ()
{
  int *const pc = __builtin_return_address (0);
  bar (pc);
}
