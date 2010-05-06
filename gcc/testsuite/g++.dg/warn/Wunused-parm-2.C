// { dg-do compile }
// { dg-options "-Wunused -W" }

template <int N>
long
f1 (unsigned long long x)
{
  unsigned long long a = 1;
  const union { unsigned long long l; unsigned int p[2]; } b = { x };
  const union { unsigned long long l; unsigned int p[2]; } c = { a };
  return b.p[0] + c.p[0];
}

template <int N>
int
f2 (int x, int y)
{
  int a = 1;
  int b[] = { 1, 2, x, a, 3, 4 };
  return b[y];
}

template <int N>
int
f3 (int a)	// { dg-warning "unused parameter" }
{
  return 0;
}

template <int N>
int
f4 (int a)	// { dg-warning "set but not used" }
{
  a = 1;
  return 0;
}

template <int N>
int
f5 (int a)
{
  a = 1;
  return a;
}

void
test ()
{
  (void) f1<0> (0);
  (void) f2<0> (0, 0);
  (void) f3<0> (0);
  (void) f4<0> (0);
  (void) f5<0> (0);
}
