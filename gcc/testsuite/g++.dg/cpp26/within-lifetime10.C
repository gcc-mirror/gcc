// P3450R1 - Extend std::is_within_lifetime
// { dg-do compile { target c++20 } }

#ifndef USE_HEADER
#if __has_builtin(__builtin_is_within_lifetime)
namespace std {
  template <typename U = void, typename T>
  consteval bool
  is_within_lifetime (const T *p) noexcept
  {
    return (__builtin_is_within_lifetime (p)
	    && __builtin_constant_p (static_cast <const volatile U *> (p)
				     && true));
  }
}
#endif
#endif

consteval int
foo ()
{
  char a = 0;
  struct B {} b;
  struct C : B {} c;
  struct D : C {} d;
  struct E : B {} e;
  if (!std::is_within_lifetime (&a))
    return __LINE__;
  if (!std::is_within_lifetime <char> (&a))
    return __LINE__;
  if (!std::is_within_lifetime <char const> (&a))
    return __LINE__;
  if (!std::is_within_lifetime <char volatile const> (&a))
    return __LINE__;
  if (!std::is_within_lifetime (&b))
    return __LINE__;
  if (!std::is_within_lifetime <B> (&b))
    return __LINE__;
  if (std::is_within_lifetime <C> (&b))
    return __LINE__;
  if (!std::is_within_lifetime (&c))
    return __LINE__;
  if (!std::is_within_lifetime <B> (&c))
    return __LINE__;
  if (!std::is_within_lifetime <C> (&c))
    return __LINE__;
  B *pb = &b;
  B *pc = &c;
  B *pd = &d;
  B *pe = &e;
  if (!std::is_within_lifetime <B> (pb))
    return __LINE__;
  if (!std::is_within_lifetime <B> (pc))
    return __LINE__;
  if (!std::is_within_lifetime <B> (pd))
    return __LINE__;
  if (!std::is_within_lifetime <B> (pe))
    return __LINE__;
  if (std::is_within_lifetime <C> (pb))
    return __LINE__;
  if (!std::is_within_lifetime <C> (pc))
    return __LINE__;
  if (!std::is_within_lifetime <C> (pd))
    return __LINE__;
  if (std::is_within_lifetime <C> (pe))
    return __LINE__;
  if (std::is_within_lifetime <D> (pb))
    return __LINE__;
  if (std::is_within_lifetime <D> (pc))
    return __LINE__;
  if (!std::is_within_lifetime <D> (pd))
    return __LINE__;
  if (std::is_within_lifetime <D> (pe))
    return __LINE__;
  if (std::is_within_lifetime <E> (pb))
    return __LINE__;
  if (std::is_within_lifetime <E> (pc))
    return __LINE__;
  if (std::is_within_lifetime <E> (pd))
    return __LINE__;
  if (!std::is_within_lifetime <E> (pe))
    return __LINE__;
  C *pf = &c;
  C *pg = &d;
  if (!std::is_within_lifetime <B> (pf))
    return __LINE__;
  if (!std::is_within_lifetime <B> (pg))
    return __LINE__;
  if (!std::is_within_lifetime <C> (pf))
    return __LINE__;
  if (!std::is_within_lifetime <C> (pg))
    return __LINE__;
  if (std::is_within_lifetime <D> (pf))
    return __LINE__;
  if (!std::is_within_lifetime <D> (pg))
    return __LINE__;
  return 0;
}

static_assert (foo () == 0);

static constexpr char a = 0;
static constexpr struct B { int b; } b {};
static constexpr struct C : B {} c {};
static constexpr struct D : C {} d {};
static constexpr struct E : B {} e {};
static constexpr const B *pb = &b;
static constexpr const B *pc = &c;
static constexpr const B *pd = &d;
static constexpr const B *pe = &e;
static constexpr const C *pf = &c;
static constexpr const C *pg = &d;

consteval int
bar ()
{
  if (!std::is_within_lifetime (&a))
    return __LINE__;
  if (!std::is_within_lifetime <char> (&a))
    return __LINE__;
  if (!std::is_within_lifetime <char const> (&a))
    return __LINE__;
  if (!std::is_within_lifetime <char volatile const> (&a))
    return __LINE__;
  if (!std::is_within_lifetime (&b))
    return __LINE__;
  if (!std::is_within_lifetime <B> (&b))
    return __LINE__;
  if (std::is_within_lifetime <C> (&b))
    return __LINE__;
  if (!std::is_within_lifetime (&c))
    return __LINE__;
  if (!std::is_within_lifetime <B> (&c))
    return __LINE__;
  if (!std::is_within_lifetime <C> (&c))
    return __LINE__;
  if (!std::is_within_lifetime <B> (pb))
    return __LINE__;
  if (!std::is_within_lifetime <B> (pc))
    return __LINE__;
  if (!std::is_within_lifetime <B> (pd))
    return __LINE__;
  if (!std::is_within_lifetime <B> (pe))
    return __LINE__;
  if (std::is_within_lifetime <C> (pb))
    return __LINE__;
  if (!std::is_within_lifetime <C> (pc))
    return __LINE__;
  if (!std::is_within_lifetime <C> (pd))
    return __LINE__;
  if (std::is_within_lifetime <C> (pe))
    return __LINE__;
  if (std::is_within_lifetime <D> (pb))
    return __LINE__;
  if (std::is_within_lifetime <D> (pc))
    return __LINE__;
  if (!std::is_within_lifetime <D> (pd))
    return __LINE__;
  if (std::is_within_lifetime <D> (pe))
    return __LINE__;
  if (std::is_within_lifetime <E> (pb))
    return __LINE__;
  if (std::is_within_lifetime <E> (pc))
    return __LINE__;
  if (std::is_within_lifetime <E> (pd))
    return __LINE__;
  if (!std::is_within_lifetime <E> (pe))
    return __LINE__;
  if (!std::is_within_lifetime <B> (pf))
    return __LINE__;
  if (!std::is_within_lifetime <B> (pg))
    return __LINE__;
  if (!std::is_within_lifetime <C> (pf))
    return __LINE__;
  if (!std::is_within_lifetime <C> (pg))
    return __LINE__;
  if (std::is_within_lifetime <D> (pf))
    return __LINE__;
  if (!std::is_within_lifetime <D> (pg))
    return __LINE__;
  return 0;
}

static_assert (bar () == 0);
