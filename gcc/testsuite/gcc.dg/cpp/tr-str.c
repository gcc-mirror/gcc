/* Test whether traditional stringify works.  */
/* { dg-do run } */
/* { dg-options "-traditional-cpp" } */
#define foo(a, b) c="a"; d="b";

extern void abort ();

int main ()
{
  char *c, *d;

  foo (p, q);
  if (c[0] != 'p' || d[0] != 'q')
    abort ();

  exit (0);
}
