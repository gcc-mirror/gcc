// PR c++/82760
// { dg-do run { target c++17 } }

#include <new>
#include <cstddef>

struct alignas(2 * alignof (std::max_align_t)) aligned_foo {
  char x[2048];

  ~aligned_foo() { }
  aligned_foo() { __builtin_memset(x, 0, sizeof(x)); }
};

int main()
{
  aligned_foo * gFoo = new (std::nothrow) aligned_foo[2];
  delete[] gFoo;
}
