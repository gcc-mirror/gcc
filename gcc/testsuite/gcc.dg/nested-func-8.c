/* { dg-do run } */
/* { dg-options "-O -fno-inline" } */

extern void abort (void);

/* Return 0 and clobber the static chain.  */

int
zero (int n)
{
  int
  nested (int m)
  {
    return m - n;
  }

  return nested (n);
}

/* Return the triple of ARG in a convoluted manner.  */

int
triple (int arg)
{
  int
  read_arg (void)
  {
    return arg;
  }

  int
  parent (int nested_arg)
  {
    int
    child1 (void)
    {
      return parent (zero (5));
    }

    int
    child2 (void)
    {
      return nested_arg + read_arg ();
    }

    return (nested_arg == 0 ? 0 : child1 ()) + child2 ();
  }

  return parent (arg);
}

int main(void)
{
  if (triple (13) != 3 * 13)
    abort ();
  return 0;
}
