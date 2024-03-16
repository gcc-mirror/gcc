struct ii
{
  int a;
  int b;
};

struct foo
{
  int a;
  struct ii ab;
  int b;
};

struct ii
foo (int *p, struct foo a)
{
  p[0] = a.a;
  p[1] = a.ab.a;
  p[2] = a.ab.b;
  p[3] = a.b;
  return a.ab;
}

void
str (struct ii ab, struct ii *p)
{
  *p = ab;
}

void
ll (long long ab, long long *p)
{
  *p = ab;
}

