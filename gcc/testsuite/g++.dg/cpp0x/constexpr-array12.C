// { dg-do compile { target c++11 } }

struct A { int ar[3]; };
int main()
{
  constexpr A a1 = { 0, a1.ar[0] };
  constexpr A a2 = { a2.ar[0] };	// { dg-error "uninitialized" }
}
