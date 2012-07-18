/* PR target/38621  */
struct s
{
  char a[512];
  int b;
  int c;
};

long long
foo (struct s *p, int m, int r)
{
  if (r == m)
    p->b = 3;
  p->c = 1;
  return m;
}
