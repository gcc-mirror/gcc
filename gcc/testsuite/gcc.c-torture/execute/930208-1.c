typedef union {
  long l;
  struct { char b3, b2, b1, b0; } c;
} T;

f (T u)
{
  ++u.c.b0;
  ++u.c.b3;
  return (u.c.b1 != 2 || u.c.b2 != 2);
}

main ()
{
  T u;
  u.c.b1 = 2;
  u.c.b2 = 2;
  u.c.b0 = ~0;
  u.c.b3 = ~0;
  if (f (u))
    abort();
  exit (0);
}
