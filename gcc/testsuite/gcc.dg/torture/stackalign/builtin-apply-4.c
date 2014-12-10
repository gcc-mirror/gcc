/* PR tree-optimization/20076 */
/* { dg-do run } */
/* { dg-additional-options "-fgnu89-inline" } */
/* { dg-require-effective-target untyped_assembly } */

extern void abort (void);

double
foo (int arg)
{
  if (arg != 116)
    abort();
  return arg + 1;
}

inline double
bar (int arg)
{
  foo (arg);
  __builtin_return (__builtin_apply ((void (*) ()) foo,
				     __builtin_apply_args (), 16));
}

int
main (int argc, char **argv)
{
  if (bar (116) != 117.0)
    abort ();

  return 0;
}
