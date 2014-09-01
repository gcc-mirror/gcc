struct foo
{
  short  d;
  int a;
};

int
bar (d, u)
     short d;
  struct foo u;
{

  u.d = d;
  return (int) (&u);
}
