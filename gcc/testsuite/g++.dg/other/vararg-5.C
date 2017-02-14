// PR middle-end/78716
// { dg-do compile }

template <typename = int, typename = int, typename = int, typename = int,
	           typename = int>
		   struct a;
		   template <typename> struct b;
		   template <typename = int, typename d = void> class e : b<d>::c {
		     public:
		         typedef e f;
			   typedef typename b<d>::c g;
			     e(__builtin_va_list *s) : g(__builtin_va_arg(*s, int)) {}
		   };
template <> struct b<void> { typedef e<> c; };
template <> struct e<> { template <typename h> e(h); };
template <typename i> class a<i> : public e<i> {};
template <typename i, typename j, typename k, typename l>
class a<i, j, k, l> : e<typename a<j>::f> {
  public:
      template <typename m, typename n, typename o, typename p>
	  a(a<m, n, o, p>) : a::f(0) {}
};
template <typename i, typename j, typename k, typename l> a<> r(i, j, k, l);
void q() { a<float, float>(r(4, 6, 9, 7)); }
