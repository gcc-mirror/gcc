// DR 342, PR c++/48582
// { dg-options -std=c++0x }

struct A;
template < void * = nullptr > void f() { }
template < void (A::*)() = nullptr > void g() { }
template < int A::* = nullptr > void h() { }

int main()
{
  // { dg-final { scan-assembler "_Z1fILPv0EEvv" } }
  f();
  f<nullptr>();

  // { dg-final { scan-assembler "_Z1gILM1AFvvE0EEvv" } }
  g();
  g<nullptr>();

  // { dg-final { scan-assembler "_Z1fILPv0EEvv" } }
  h();
  h<nullptr>();

  constexpr void * ptr = nullptr;
  f<ptr>();
}
