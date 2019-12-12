// PR c++/89767
// { dg-do compile { target c++14 } }
// { dg-options "-O2 -Wall" }

template <typename d> struct e { using g = d; };
template <typename d, template <typename> class> using h = e<d>;
template <typename d, template <typename> class i>
using j = typename h<d, i>::g;
template <typename c> int k(c);
template <typename...> class au;
struct l { template <typename c> using m = typename c::f; };
struct s : l { using af = j<au<int, int> *, m>; };
template <unsigned long, typename> struct o;
template <long p, typename c> using q = typename o<p, c>::g;
template <typename> struct r;
template <typename c> struct r<c *> { typedef c aj; };
template <typename ak, typename> struct al { typename r<ak>::aj operator*(); void operator++(); };
template <typename am, typename an, typename ao>
bool operator!=(al<am, ao>, al<an, ao>);
template <unsigned long, typename...> struct ap;
template <unsigned long aq, typename ar, typename... as>
struct ap<aq, ar, as...> : ap<1, as...> {};
template <unsigned long aq, typename ar> struct ap<aq, ar> {};
template <typename... at> class au : public ap<0, at...> {};
template <unsigned long p, typename ar, typename... as>
struct o<p, au<ar, as...>> : o<p - 1, au<as...>> {};
template <typename ar, typename... as> struct o<0, au<ar, as...>> { typedef ar g; };
template <long p, typename ar> constexpr ar av(ap<p, ar> __t) { return ar (); }
template <int p, typename... at> constexpr q<p, au<at...>> aw(au<at...> __t) { av<p>(__t); return q<p, au<at...>> (); }
struct bg { typedef s::af af; };
struct F { typedef al<bg::af, int> bk; bk begin(); bk end(); };
void bo() { int t = 0; F cv; for (auto bp : cv) [t, n = k(aw<1>(bp))] {}; }
