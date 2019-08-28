// PR c++/91360 - Implement C++20 P1143R2: constinit
// { dg-do compile { target c++2a } }
// Variable templates.

int nonconst;

template<typename T>
constinit T v1 = 42;

template<typename T>
constinit T v2 = nonconst; // { dg-error "does not have a constant initializer|not usable" }

void
fn ()
{
  v1<int>;
  v2<int>;
}
