// Test for value-initialization via {}
// { dg-do run { target c++11 } }
/* { dg-options "-Og -fno-early-inlining -finline-small-functions -fpack-struct" } */
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

int main()
{
  int space = 42;
  A* ap = new (&space) A{};
  int space1[1] = { 42 };
  A* a1p = new (space1) A[1]{};
  if (ap->i != 0 ||
      a1p[0].i != 0)
    return 1;
  return 0;
}
