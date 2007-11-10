// PR middle-end/34018
// { dg-do compile }
// { dg-options "-O1" }

template <typename E, unsigned long N>
struct A
{
  typedef E F;
  E elems[N];
  A () {}
  E *begin () { return elems; }
  const E *begin () const { return elems; }

  explicit A (F const &v0, F const &v1, F const &v2)
  {
    elems[0] = v0;
    elems[1] = v1;
    elems[2] = v2;
  }
};

template <typename E1, typename E2, typename E3>
inline void
bar (const E1 *a1, E2 const &a2, E3 *a3, unsigned long const &sz)
{
  E3 *r = a3 + sz;
  for (;a3 != r; a1++, a3++)
    *a3 = *a1 - a2;
}

template<typename E, unsigned long N>
inline A<E, N>
operator- (A<E, N> const& a1, E const& a2)
{
  typedef A<E, N> G;
  G a3;
  bar (a1.begin (), a2, a3.begin (), N);
  return a3;
}

struct B
{
  B (A<unsigned long, 3> const &m) : n (m - static_cast<unsigned long>(1)) {}
  A<unsigned long, 3> n;
};

void
foo ()
{
  B t (A<unsigned long, 3> (0,0,0));
}
