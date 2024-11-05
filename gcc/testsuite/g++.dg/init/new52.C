// PR c++/117101
// { dg-do "compile" { target c++11 } }

using size_t = decltype(sizeof(int));
void* operator new(size_t, // { dg-bogus "first parameter" "" { xfail *-*-* } }
		   void void **p) noexcept // { dg-error "two or more" }
{
  return p; // { dg-error "not declared" }
}
int x;
void f() {
    int y;
    new (&y) int(x);
}
