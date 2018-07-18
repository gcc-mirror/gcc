/* { dg-do compile } */

long a;
long *b;
void c ();
void d ();
void
e (long f)
{
  if (a)
    *b = f;
}
void
g ()
{
  c (g, e);
}
void
c (int f, int h ())
{
  d (f, h, "");
}
void
d (int f, int h (), char *i, char *k)
{
  int j;
  d (f, h, i + 1, k);
  while (--j)
    h (*i);
}
