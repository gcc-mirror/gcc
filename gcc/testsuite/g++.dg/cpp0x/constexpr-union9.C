// PR c++/118661
// { dg-do compile { target c++11 } }

using nullptr_t = decltype (nullptr);
union U { int i; nullptr_t n; };
constexpr U u = { 42 };
static_assert (u.n == nullptr, "");

#if __cplusplus >= 201402L
constexpr nullptr_t
foo ()
{
  union U { int i; nullptr_t n; } u = { 42 };
  return u.n;
}
#endif
