// PR c++/44412
// { dg-do compile }
// { dg-options "-Wunused" }

struct S
{
  static const int a = 3;
  static int b;
  int c;
};

const int S::a;
int S::b = 4;

int
f1 ()
{
  S s;
  return s.a;
}

int
f2 ()
{
  S s;
  return s.b;
}

void
f3 ()
{
  S s;		// { dg-warning "set but not used" }
  s.c = 6;
}

int
f4 ()
{
  S s;
  s.c = 6;
  return s.c;
}
