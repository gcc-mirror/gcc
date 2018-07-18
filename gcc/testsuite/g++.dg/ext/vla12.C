// VLA sizeof test
// { dg-do compile }
// { dg-options "" }
// { dg-require-effective-target alloca }

int
f1 (int i)
{
  char a[sizeof (i) + 6 + i];
  char b[sizeof (a) + 1];
  return sizeof (b);
}

int
f2 (int i)
{
  char a[sizeof (i) + 6 + i];
  char b[sizeof (a)];
  return sizeof (b);
}

int
f3 (int i)
{
  char a[sizeof (i) + 6 + i];
  char b[sizeof (i) + i];
  char c[sizeof (a) + sizeof (b) + 7];
  return sizeof (c);
}
