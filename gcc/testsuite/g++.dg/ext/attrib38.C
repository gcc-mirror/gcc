// PR c++/36625

template <int N>
struct A {
  struct S { short f[3]; } __attribute__ ((aligned (N)));
};

int main ()
{
  A<4>::S s;
}
