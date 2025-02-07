// PR c++/118661
// { dg-do compile { target c++11 } }

using nullptr_t = decltype (nullptr);
constexpr volatile nullptr_t a = {};
constexpr nullptr_t b = a;

constexpr nullptr_t
foo ()
{
#if __cplusplus >= 201402L
  volatile nullptr_t c = {};
  return c;
#else
  return nullptr;
#endif
}

static_assert (b == nullptr, "");
static_assert (foo () == nullptr, "");
