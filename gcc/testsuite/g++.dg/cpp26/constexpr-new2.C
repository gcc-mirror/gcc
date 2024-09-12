// C++26 P2747R2 - constexpr placement new
// { dg-do compile { target c++26 } }
// { dg-skip-if "requires hosted libstdc++ for memory allocator" { ! hostedlib } }

#include <memory>
#include <new>

#ifndef __cpp_lib_constexpr_new
# error "__cpp_lib_constexpr_new"
#elif __cpp_lib_constexpr_new < 202406L
# error "__cpp_lib_constexpr_new < 202406"
#endif

struct S {
  constexpr S () : a (42), b (43) {}
  constexpr S (int c, int d) : a (c), b (d) {}
  int a, b;
};
struct T {
  int a, b;
};

constexpr bool
foo ()
{
  std::allocator<int> a;
  auto b = a.allocate (3);
  ::new (b) int ();
  ::new (b + 1) int (1);
  ::new (b + 2) int {2};
  if (b[0] != 0 || b[1] != 1 || b[2] != 2)
    return false;
  a.deallocate (b, 3);
  std::allocator<S> c;
  auto d = c.allocate (4);
  ::new (d) S;
  ::new (d + 1) S ();
  ::new (d + 2) S (7, 8);
  ::new (d + 3) S { 9, 10 };
  if (d[0].a != 42 || d[0].b != 43
      || d[1].a != 42 || d[1].b != 43
      || d[2].a != 7 || d[2].b != 8
      || d[3].a != 9 || d[3].b != 10)
    return false;
  d[0].~S ();
  d[1].~S ();
  d[2].~S ();
  d[3].~S ();
  c.deallocate (d, 4);
  std::allocator<T> e;
  auto f = e.allocate (3);
  ::new (f) T ();
  ::new (f + 1) T (7, 8);
  ::new (f + 2) T { .a = 9, .b = 10 };
  if (f[0].a != 0 || f[0].b != 0
      || f[1].a != 7 || f[1].b != 8
      || f[2].a != 9 || f[2].b != 10)
    return false;
  f[0].~T ();
  f[1].~T ();
  f[2].~T ();
  e.deallocate (f, 3);
  auto g = a.allocate (3);
  new (g) int[] {1, 2, 3};
  if (g[0] != 1 || g[1] != 2 || g[2] != 3)
    return false;
  new (g) int[] {4, 5};
  if (g[0] != 4 || g[1] != 5)
    return false;
  a.deallocate (g, 3);
  return true;
}

static_assert (foo ());
