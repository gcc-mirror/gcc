int b, c, d, e = 1, f, g, h, j;

static int
fn1 ()
{
  int a = c;
  if (h)
    return 9;
  g = (c || b) % e;
  if ((g || f) && b)
    return 9;
  e = d;
  for (c = 0; c > -4; c--)
    ;
  if (d)
    c--;
  j = c;
  return d;
}

int
main ()
{
  fn1 ();

  if (c != -4)
    __builtin_abort ();

  return 0;
}
