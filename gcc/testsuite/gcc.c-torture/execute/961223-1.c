struct s {
  double d;
};

inline struct s
sub (struct s s)
{
  s.d += 1.0;
  return s;
}

main ()
{
  struct s t = { 2.0 };
  t = sub (t);
  if (t.d != 3.0)
    abort ();
  exit (0);
}
