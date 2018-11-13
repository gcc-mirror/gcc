// { dg-do compile { target c++11 } }
// { dg-final { scan-assembler-not "weak.*_Z" } }

using P = struct {}*;

template <int N>
void f(int(*)[((P)0, N)]) {}

template <int N>
struct A { };

template <int N>
void g(A<((P)0,N)>) {}

int main()
{
  f<1>(0);
  g<1>({});
}
