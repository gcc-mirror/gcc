// PR tree-optimization/105189
// { dg-do run }

int
foo ()
{
  return -1;
}

int
main ()
{
  int c = foo () >= 0U && 1;
  if (c != 1)
    __builtin_abort ();
  int d = foo () >= 3U && 1;
  if (d != 1)
    __builtin_abort ();
}
