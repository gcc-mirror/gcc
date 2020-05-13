// P0784R7
// { dg-do compile { target c++20 } }

namespace std
{
  typedef __SIZE_TYPE__ size_t;
}

inline void *operator new (std::size_t, void *p) noexcept
{ return p; }
void *operator new (std::size_t) noexcept;

constexpr bool
foo ()
{
  auto p = static_cast<int *> (::operator new (sizeof (int)));	// { dg-error "call to non-'constexpr' function" }
  *p = 1;
  ::operator delete (p);
  return false;
}

struct S { constexpr S () : s (0) {} int s; };

constexpr bool
bar ()
{
  auto p = static_cast<S *> (::operator new (sizeof (S)));	// { dg-error "call to non-'constexpr' function" }
  auto q = new (p) S ();
  q->s++;
  q->~S ();
  ::operator delete (p);
  return false;
}
