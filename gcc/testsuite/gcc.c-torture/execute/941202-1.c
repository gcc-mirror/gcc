/* { dg-skip-if "requires alloca" { ! alloca } { "-O0" } { "" } } */
g (x, y)
{
  if (x != 3)
    abort ();
}

static inline
f (int i)
{
  int *tmp;

  tmp = (int *) alloca (sizeof (i));
  *tmp = i;
  g (*tmp, 0);
}

main ()
{
  f (3);
  exit (0);
};
