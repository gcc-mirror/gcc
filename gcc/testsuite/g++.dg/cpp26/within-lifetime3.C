// P3450R1 - Extend std::is_within_lifetime
// { dg-do compile { target c++20 } }

#include "../cpp2a/construct_at.h"

namespace std {
  template <class T>
  consteval bool
  is_within_lifetime (const T *p) noexcept
  {
    return __builtin_is_within_lifetime (p);
  }
}

consteval bool
foo (bool x)
{
  std::allocator <int> a;
  auto b = a.allocate (1);
#if 0
  // [allocator.members]/5 says it should start lifetime of the array
  // but not its elements, and b points to the first element.
  if (std::is_within_lifetime (b))
    return false;
#endif
  std::construct_at (b);
  if (!std::is_within_lifetime (b))
    return false;
  std::destroy_at (b);
  if (std::is_within_lifetime (b))
    return false;
  a.deallocate (b, 1);
  if (x)
    __builtin_is_within_lifetime (b);	// { dg-error "'__builtin_is_within_lifetime' on allocated storage after deallocation is not a constant expression" }
  return true;
}

static_assert (foo (false));
bool a = foo (true);			// { dg-error "call to consteval function 'foo\\\(true\\\)' is not a constant expression" }

consteval bool
bar (int x)
{
  int *a;
  decltype (nullptr) *b;
  {
    int c = 42;
    a = &c;
    if (!std::is_within_lifetime (a))
      return false;
  }
  {
    int c = 42;
    if (x == 1)
      __builtin_is_within_lifetime (a);	// { dg-error "'__builtin_is_within_lifetime' on 'c' after its storage has been released is not a constant expression" }
  }
  if (x == 2)
    __builtin_is_within_lifetime (a);	// { dg-error "'__builtin_is_within_lifetime' on 'c' after its storage has been released is not a constant expression" }
  {
    int c[42] = {};
    a = &c[40];
    if (!std::is_within_lifetime (a))
      return false;
  }
  if (x == 3)
    __builtin_is_within_lifetime (a);	// { dg-error "'__builtin_is_within_lifetime' on 'c' after its storage has been released is not a constant expression" }
  {
    decltype (nullptr) d = nullptr;
    b = &d;
    if (!std::is_within_lifetime (b))
      return false;
  }
  {
    decltype (nullptr) d = nullptr;
    if (x == 4)
      __builtin_is_within_lifetime (b);	// { dg-error "'__builtin_is_within_lifetime' on 'd' after its storage has been released is not a constant expression" }
  }
  if (x == 5)
    __builtin_is_within_lifetime (b);	// { dg-error "'__builtin_is_within_lifetime' on 'd' after its storage has been released is not a constant expression" }
  {
    decltype (nullptr) d[42] = {};
    b = &d[40];
    if (!std::is_within_lifetime (b))
      return false;
  }
  if (x == 6)
    __builtin_is_within_lifetime (b);	// { dg-error "'__builtin_is_within_lifetime' on 'd' after its storage has been released is not a constant expression" }
  {
    int c;
    a = &c;
    if (!std::is_within_lifetime (a))
      return false;
  }
  {
    int c;
    if (x == 7)
      __builtin_is_within_lifetime (a);	// { dg-error "'__builtin_is_within_lifetime' on 'c' after its storage has been released is not a constant expression" }
  }
  if (x == 8)
    __builtin_is_within_lifetime (a);	// { dg-error "'__builtin_is_within_lifetime' on 'c' after its storage has been released is not a constant expression" }
  {
    int c[42];
    a = &c[40];
    if (!std::is_within_lifetime (a))
      return false;
  }
  if (x == 9)
    __builtin_is_within_lifetime (a);	// { dg-error "'__builtin_is_within_lifetime' on 'c' after its storage has been released is not a constant expression" }
  {
    decltype (nullptr) d;
    b = &d;
    if (!std::is_within_lifetime (b))
      return false;
  }
  {
    decltype (nullptr) d;
    if (x == 10)
      __builtin_is_within_lifetime (b);	// { dg-error "'__builtin_is_within_lifetime' on 'd' after its storage has been released is not a constant expression" }
  }
  if (x == 11)
    __builtin_is_within_lifetime (b);	// { dg-error "'__builtin_is_within_lifetime' on 'd' after its storage has been released is not a constant expression" }
  {
    decltype (nullptr) d[42];
    b = &d[40];
    if (!std::is_within_lifetime (b))
      return false;
  }
  if (x == 12)
    __builtin_is_within_lifetime (b);	// { dg-error "'__builtin_is_within_lifetime' on 'd' after its storage has been released is not a constant expression" }
  int e[2];
  if (!std::is_within_lifetime (&e)
      || !std::is_within_lifetime (&e[0])
      || !std::is_within_lifetime (&e[1]))
    return true;
  std::destroy_at (&e[0]);
  if (!std::is_within_lifetime (&e)
      || std::is_within_lifetime (&e[0])
      || !std::is_within_lifetime (&e[1]))
    return false;
  std::construct_at (&e[0]);
  std::destroy_at (&e[1]);
  if (!std::is_within_lifetime (&e)
      || !std::is_within_lifetime (&e[0])
      || std::is_within_lifetime (&e[1]))
    return false;
  std::construct_at (&e[1]);
  if (!std::is_within_lifetime (&e)
      || !std::is_within_lifetime (&e[0])
      || !std::is_within_lifetime (&e[1]))
    return false;
  struct { int a, b; } f;
  if (!std::is_within_lifetime (&f)
      || !std::is_within_lifetime (&f.a)
      || !std::is_within_lifetime (&f.b))
    return true;
  std::destroy_at (&f.a);
  if (!std::is_within_lifetime (&f)
      || std::is_within_lifetime (&f.a)
      || !std::is_within_lifetime (&f.b))
    return false;
  std::construct_at (&f.a);
  std::destroy_at (&f.b);
  if (!std::is_within_lifetime (&f)
      || !std::is_within_lifetime (&f.a)
      || std::is_within_lifetime (&f.b))
    return false;
  std::construct_at (&f.b);
  if (!std::is_within_lifetime (&f)
      || !std::is_within_lifetime (&f.a)
      || !std::is_within_lifetime (&f.b))
    return false;
  return true;
}

static_assert (bar (0));
bool b = bar (1);			// { dg-error "call to consteval function 'bar\\\(1\\\)' is not a constant expression" }
bool c = bar (2);			// { dg-error "call to consteval function 'bar\\\(2\\\)' is not a constant expression" }
bool d = bar (3);			// { dg-error "call to consteval function 'bar\\\(3\\\)' is not a constant expression" }
bool e = bar (4);			// { dg-error "call to consteval function 'bar\\\(4\\\)' is not a constant expression" }
bool f = bar (5);			// { dg-error "call to consteval function 'bar\\\(5\\\)' is not a constant expression" }
bool g = bar (6);			// { dg-error "call to consteval function 'bar\\\(6\\\)' is not a constant expression" }
bool h = bar (7);			// { dg-error "call to consteval function 'bar\\\(7\\\)' is not a constant expression" }
bool i = bar (8);			// { dg-error "call to consteval function 'bar\\\(8\\\)' is not a constant expression" }
bool j = bar (9);			// { dg-error "call to consteval function 'bar\\\(9\\\)' is not a constant expression" }
bool k = bar (10);			// { dg-error "call to consteval function 'bar\\\(10\\\)' is not a constant expression" }
bool l = bar (11);			// { dg-error "call to consteval function 'bar\\\(11\\\)' is not a constant expression" }
bool m = bar (12);			// { dg-error "call to consteval function 'bar\\\(12\\\)' is not a constant expression" }

struct A { int a = 42; bool b = std::is_within_lifetime (&a); };
constexpr A n;

struct B { consteval B () : a (42), b (std::is_within_lifetime (&a)) {} int a; bool b; };
constexpr B o;

struct C { consteval C () { __builtin_is_within_lifetime (this); } } p;

struct D {
  consteval D () : a (0), b (0), c (0) {}
  consteval D (int x, int y, int z)
  : a (x), b (y + std::is_within_lifetime (&this->a) * 32
		+ std::is_within_lifetime (&this->b) * 64
		+ std::is_within_lifetime (&this->c) * 128), c (z) {}
  constexpr ~D () {}
  int a, b, c;
};

consteval int
qux ()
{
  D a[2] = {};
  std::destroy_at (&a[0]);
  std::construct_at (&a[0], 4, 5, 6);
  return a[0].a + a[0].b + a[0].c;
}

// FIXME: In mem-initializer of b, a should be already constructed,
// so within lifetime, but b is in the middle of construction and c
// construction has not started yet.
static_assert (qux () == 4 + 5 + 6 + 32);	// { dg-bogus "note: the comparison reduces to '\\\(239 == 47\\\)'" "" { xfail *-*-* } }
						// { dg-bogus "static assertion failed" "" { xfail *-*-* } .-1 }

consteval bool
corge (bool x)
{
  int *p;
  {
    int a;
    p = &a;
    if (!std::is_within_lifetime (&a))
      return false;
    std::destroy_at (&a);
    if (std::is_within_lifetime (&a))
      return false;
    std::construct_at (&a);
    if (!std::is_within_lifetime (&a))
      return false;
  }
  if (x)
    __builtin_is_within_lifetime (p);		// { dg-error "'__builtin_is_within_lifetime' on 'a' after its storage has been released is not a constant expression" }
  return true;
}

static_assert (corge (false));
bool q = corge (true);				// { dg-error "call to consteval function 'corge\\\(true\\\)' is not a constant expression" }
