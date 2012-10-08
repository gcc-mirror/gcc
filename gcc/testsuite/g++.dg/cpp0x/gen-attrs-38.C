// { dg-do compile { target c++11 } }
// PR c++/36625

template <int N>
struct A {
  struct S { short f[3]; } [[gnu::aligned (N)]]; // { dg-warning "ignored" }
};

int main ()
{
  A<4>::S s;
}
