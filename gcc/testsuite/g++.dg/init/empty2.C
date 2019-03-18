// PR c++/86485
// { dg-additional-options -Wmaybe-uninitialized }

struct E {};
struct S { S () {} E e; };
void foo (S);

void
bar (bool b)
{
  foo (b ? S () : S ());
}
