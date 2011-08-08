// Test for value-initialization via {}
// { dg-options -std=c++0x }
// { dg-do run }

void * operator new (__SIZE_TYPE__, void *p) { return p; }
void * operator new[] (__SIZE_TYPE__, void *p) { return p; }

// Empty base so A isn't an aggregate
struct B {};
struct A: B {
  int i;
};

struct C: A {
  C(): A{} {}
};

int f(A a) { return a.i; }

int main()
{
  A a{};
  C c;
  int space = 42;
  A* ap = new (&space) A{};
  int space1[1] = { 42 };
  A* a1p = new (space1) A[1]{};
  if (a.i != 0
      || c.i != 0
      || ap->i != 0
      || a1p[0].i != 0
      || A{}.i != 0
      || f({}) != 0)
    return 1;
}
