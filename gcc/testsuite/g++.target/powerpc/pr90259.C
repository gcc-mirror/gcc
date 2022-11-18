/* { dg-require-effective-target long_double_ibm128 } */
/* { dg-options "-O2 -ffloat-store -fgcse -fnon-call-exceptions -fno-forward-propagate -fno-omit-frame-pointer -fstack-protector-all" } */
/* { dg-add-options long_double_ibm128 } */

/* Verify there is no ICE.  */

template <int a> struct b
{
  static constexpr int c = a;
};
template <bool a> using d = b<a>;
struct e
{
  int f;
  int
  g ()
  {
    return __builtin_ceil (f / (long double) h);
  }
  float h;
};
template <typename, typename> using k = d<!bool ()>;
template <typename> class n
{
public:
  e ae;
  void af ();
};
template <typename l>
void
n<l>::af ()
{
  ae.g ();
}
template <bool> using m = int;
template <typename ag, typename ah, typename ai = m<k<ag, ah>::c>>
using aj = n<ai>;
struct o
{
  void
  af ()
  {
    al.af ();
  }
  aj<int, int> al;
};
template <typename> class am;
template <typename i> class ao
{
protected:
  static i *ap (int);
};
template <typename, typename> class p;
template <typename ar, typename i, typename... j> class p<ar (j...), i> : ao<i>
{
public:
  static ar
  as (const int &p1, j...)
  {
    (*ao<i>::ap (p1)) (j ()...);
  }
};
template <typename ar, typename... j> class am<ar (j...)>
{
  template <typename, typename> using av = int;

public:
  template <typename i, typename = av<d<!bool ()>, void>,
	    typename = av<i, void>>
  am (i);
  using aw = ar (*) (const int &, j...);
  aw ax;
};
template <typename ar, typename... j>
template <typename i, typename, typename>
am<ar (j...)>::am (i)
{
  ax = p<ar (j...), i>::as;
}
struct G
{
  void ba (am<void (o)>);
};
struct q
{
  q ()
  {
    G a;
    a.ba (r ());
  }
  struct r
  {
    void
    operator() (o p1)
    try
      {
	p1.af ();
      }
    catch (int)
      {
      }
  };
} s;
