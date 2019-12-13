// PR target/92744
// { dg-do compile }
// { dg-options "-Os -fno-tree-ccp" }

typedef float T __attribute__((mode(SD)));
struct A { T a; };
void foo ();

bool
operator!= (A x, A y)
{
  return x.a != y.a;
}

void
bar (A x, A y)
{
  if (x != y)
    foo ();
}
