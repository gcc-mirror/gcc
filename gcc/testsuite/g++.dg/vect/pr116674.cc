// { dg-do compile }
// { dg-require-effective-target c++11 }
// { dg-additional-options "-Ofast" }
// { dg-additional-options "-march=x86-64-v3" { target { x86_64-*-* i?86-*-* } } }

namespace std {
    typedef int a;
    template <typename> struct b;
    template <typename> class aa {};
    template <typename c> c d(c e, c) { return e; }
    template <typename c> struct b<aa<c>> {
	using f = c;
	using g = c *;
	template <typename h> using j = aa<h>;
    };
} // namespace std
namespace l {
    template <typename ab> struct m : std::b<ab> {
	typedef std::b<ab> n;
	typedef typename n::f &q;
	template <typename c> struct ac { typedef typename n::j<c> ad; };
    };
} // namespace l
namespace std {
    template <typename c, typename ab> struct o {
	typedef typename l::m<ab>::ac<c>::ad ae;
	typedef typename l::m<ae>::g g;
	struct p {
	    g af;
	};
	struct ag : p {
	    ag(ae) {}
	};
	typedef ab u;
	o(a, u e) : ah(e) {}
	ag ah;
    };
    template <typename c, typename ab = aa<c>> class r : o<c, ab> {
	typedef o<c, ab> s;
	typedef typename s::ae ae;
	typedef l::m<ae> w;

    public:
	c f;
	typedef typename w::q q;
	typedef a t;
	typedef ab u;
	r(t x, u e = u()) : s(ai(x, e), e) {}
	q operator[](t x) { return *(this->ah.af + x); }
	t ai(t x, u) { return x; }
    };
    extern "C" __attribute__((__simd__)) double exp(double);
} // namespace std
using namespace std;
int ak;
double v, y;
void am(double, int an, double, double, double, double, double, double, double,
	double, double, double, int, double, double, double, double,
	r<double> ap, double, double, double, double, double, double, double,
	double, r<double> ar, r<double> as, double, double, r<double> at,
	r<double> au, r<double> av, double, double) {
    double ba;
    for (int k;;)
      for (int i; i < an; ++i) {
	  y = i;
	  v = d(y, 25.0);
	  ba = exp(v);
	  ar[i * (ak + 1)] = ba;
	  as[i * (ak + 1)] = ar[i * (ak + 1)];
	  if (k && ap[k]) {
	      at[i * (ak + 1)] = av[i * (ak + 1)] = as[i * (ak + 1)];
	      au[i * (ak + 1)] = ar[i * (ak + 1)];
	  } else {
	      au[i * (ak + 1)] = ba;
	      at[i * (ak + 1)] = av[i * (ak + 1)] = k;
	  }
      }
}
void b(int bc) {
    double bd, be, bf, bg, bh, ao, ap, bn, bo, bp, bq, br, bs, bt, bu, bv, bw, bx,
    by, aq, ar, as, bz, ca, at, au, av, cb, aw;
    int bi;
    am(bh, bc, bi, bi, bi, bi, bv, bw, bx, by, bu, bt, bi, ao, bn, bo, bp, ap, bq,
       br, bs, bd, be, bf, bg, aq, ar, as, bz, ca, at, au, av, cb, aw);
}
