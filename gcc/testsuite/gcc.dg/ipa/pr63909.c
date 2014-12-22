/* { dg-options "-O2 -fno-guess-branch-probability" } */

int z;

__attribute__((noinline))
void g ()
{
  if (++z)
    __builtin_exit (0);
  g ();
}

__attribute__((noinline))
void f ()
{
  if (++z)
    __builtin_exit (0);
  f ();
}

int main()
{
  f ();
  g ();

  return 0;
}
