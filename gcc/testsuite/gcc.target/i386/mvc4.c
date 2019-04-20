/* { dg-do run } */
/* { dg-require-ifunc "" } */

__attribute__((target_clones("default","avx")))
int
foo ()
{
  return -2;
}

int
bar ()
{
  return 2;
}

int
main ()
{
  int r = 0;
  r += bar ();
  r += foo ();
  r += bar ();
  r += foo ();
  r += bar ();
  return r - 2;
}
