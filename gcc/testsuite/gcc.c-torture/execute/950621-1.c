struct s
{
  int a;
  int b;
  struct s *dummy;
};

f (struct s *sp)
{
  return sp && sp->a == -1 && sp->b == -1;
}

main ()
{
  struct s x;
  x.a = x.b = -1;
  if (f (&x) == 0)
    abort ();
  exit (0);
}
