// { dg-do compile }
// { dg-options "-Wunused -W" }

long
f1 (unsigned long long x)
{
  unsigned long long a = 1;
  const union { unsigned long long l; unsigned int p[2]; } b = { x };
  const union { unsigned long long l; unsigned int p[2]; } c = { a };
  return b.p[0] + c.p[0];
}

int
f2 (int x, int y)
{
  int a = 1;
  int b[] = { 1, 2, x, a, 3, 4 };
  return b[y];
}

int
f3 (int a)	// { dg-warning "unused parameter" }
{
  return 0;
}

int
f4 (int a)	// { dg-warning "set but not used" }
{
  a = 1;
  return 0;
}

int
f5 (int a)
{
  a = 1;
  return a;
}

int
f6 (int &a)
{
  return a;
}

void
f7 (int &a)
{
  a = 1;
}

struct S
{
  S (int i) : j(i) {}
  S (long i) : j(i + 1) {}
  int j;
};
