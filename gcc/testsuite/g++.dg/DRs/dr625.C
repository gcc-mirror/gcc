// DR 625 - Use of auto as a template-argument
// PR c++/97479
// { dg-do compile { target c++14 } }

template<typename>
struct A { };

void f(int);

int main()
{
  A<decltype(auto)> x = A<void>(); // { dg-error "not permitted|invalid|cannot convert" }
  A<auto> a = A<void>(); // { dg-error "not permitted|invalid|cannot convert" }
  void (*p)(auto); // { dg-error "parameter" }
}
