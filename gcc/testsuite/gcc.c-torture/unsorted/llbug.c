union foo
{
  long long  d;
};

int
bar (long long d)
{
  union foo u;

  u.d = d;
  return (int) &u;
}
