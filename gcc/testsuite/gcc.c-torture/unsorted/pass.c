int
foo (a, b, c)
{
  return a + b + c;
}

int
bar ()
{
  int q, w, e, r, t, y;

  return foo ((int) & q, q, w, e, q, (int) &w);
}
