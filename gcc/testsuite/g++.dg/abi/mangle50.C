// DR 342, PR c++/48582
// { dg-do compile { target c++11 } }
// { dg-additional-options -fabi-compat-version=0 }

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
