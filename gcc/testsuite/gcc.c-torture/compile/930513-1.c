struct s {
  int f1 : 26;
  int f2 : 8;
};

f (struct s *x)
{
  return x->f2++ == 0;
}
