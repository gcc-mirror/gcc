// PR c++/85112
// { dg-do compile { target c++11 } }

struct A
{
  int m;
  int n : 4;
};

int i;  // { dg-message "not const" }

void foo()
{
  constexpr int j = i;  // { dg-error "not usable" }
  A a;
  a.n = j;
}
