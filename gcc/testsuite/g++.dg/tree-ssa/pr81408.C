/* { dg-do compile } */
/* { dg-options "-O2 -std=gnu++11 -fopt-info-loop-missed" } */
struct p
{
  char *ay;
};

namespace a {
void b () __attribute__ ((__noreturn__));
template <typename> struct d;
template <typename e> struct d<e *>
{
  typedef e f;
};
struct g
{
  template <typename h> using i = h *;
};
}
using a::d;
template <typename j, typename> class k
{
  j l;

public:
  typename d<j>::f operator* () { return p(); }
  void operator++ () { ++l; }
  j
  aa ()
  {
    return l;
  }
};
template <typename m, typename n, typename ab>
bool
operator!= (k<m, ab> o, k<n, ab> p2)
{
  return o.aa () != p2.aa ();
}
namespace a {
struct F
{
  struct q
  {
    using ai = g::i<p>;
  };
  using r = q::ai;
};
class H
{
public:
  k<F::r, int> begin ();
  k<F::r, int> end ();
};
int s;
class I
{
public:
  void
  aq (char)
  {
    if (s)
      b ();
  }
};
class u : public I
{
public:
  void
  operator<< (u o (u))
  {
    o (*this);
  }
  u operator<< (void *);
};
template <typename at, typename au>
at
av (au o)
{
  o.aq ('\n');
  return at();
}
u ax;
}
a::H t;
void
ShowHelpListCommands ()
{
  for (auto c : t) /* { dg-message "note: missed loop optimization: niters analysis .*" } */
    a::ax << c.ay << a::av;
}

