/* { dg-do compile } */
/* { dg-options "-Wunused" } */

struct S { int e; };

int
f1 (void)
{
  int a;
  int b;
  int c;
  int d;
  S s;
  a = 1;
  b = 2;
  c = 3;
  d = 4;
  s.e = 5;
  __typeof (c) e;	// { dg-warning "set but not used" }
  __decltype (d) f;	// { dg-warning "set but not used" }
  __decltype (s.e) g;	// { dg-warning "set but not used" }
  e = 1;
  f = 1;
  g = 1;
  return sizeof (a) + __alignof__ (b);
}

template <int N>
int f2 (void)
{
  int a;
  int b;
  int c;
  int d;
  a = 1;
  b = 2;
  c = 3;
  d = 4;
  __typeof (c) e;	// { dg-warning "set but not used" }
  __decltype (d) f;	// { dg-warning "set but not used" }
  e = 1;
  f = 1;
  return sizeof (a) + __alignof__ (b);
}

void
test (void)
{
  (void) f2<0> ();
}
