// PR c++/83300
// { dg-do compile { target c++11 } }

template<int N>
using T = int __attribute__((vector_size (sizeof(int) * N)));

void
f (T<4>)
{
}
