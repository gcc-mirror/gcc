/* PR target/97682 */
/* { dg-do compile } */
/* { dg-options "-fPIC -O2 -march=rv64g -mabi=lp64" } */

template <typename ab, int ac> struct g { ab b[ac]; };
long i, m;

namespace std
{
  template <typename c> struct t
  {
    int a;
    c h;
  };

  struct ad
  {
    enum { j };
  };

  template <typename k> k l(k);
  struct al {};
  template <typename k> k aa(k);

  struct v
  {
    template <typename n, typename q> static q o(n, n, q);
  };

  template <int, typename n, typename q> void p(n z, n ao, q ap)
  {
    v::o(z, ao, ap);
  }

  template <int ae, typename n, typename q> void r(n z, n ao, q ap)
  {
    p<ae>(z, ao, ap);
  }

  template <int ae, typename n, typename q> void af(n z, n ao, q)
  {
    r<ae>(aa(z), aa(ao), 0);
  }

  template <typename n, typename q> void ag(n z, n ao, q ap)
  {
    af<ad::j>(l(z), l(ao), ap);
  }

  template <typename> class allocator;
  template <typename ah, typename ai, typename aj> void ak(ah, ai, aj);

  template <typename s> class aq
  {
    template <typename am> struct ar { using f = am *; };
  public:
    using an = typename ar<s>::f;
  };

  template <typename s> class as
  {
  public:
    using an = typename aq<s>::an;
    an operator->();
  };

  struct ay
  {
    int at();
  };

  template <typename s, typename = allocator<s>> class vector : ay
  {
  public:
    long au();
    long x;
    void av() { _M_default_append(x); }
    void _M_default_append(unsigned long);
    void aw();
    long ax(int);
  };

  template <typename s, typename y>
  void vector<s, y>::_M_default_append(unsigned long z)
  {
    long az = au();
    int w = at(), bc = at();
    i = ax(w);
    m = ax(w);
    if (i || m)
      aw();
    ak(az, z, bc);
  }
}

namespace llvm
{
  template <int bd> class bh
  {
    enum { bf = bd } * bg[bf];
  };

  template <class> class bi;

  class bm
  {
    using bj = bi<int>;
    std::as<bj> bk;
    void bl();
  };

  template <class> struct bn;

  class br
  {
    bh<8> bo;
  };

  class ca
  {
    int *d;
    int e;
  };

  template <class bp> class bv : std::al, br
  {
    g<std::t<ca>, 8> b;
  };

  template <class ab> bv<ab> bt(ab);

  class BlockFrequencyInfoImplBase
  {
  public:
    struct FrequencyData;
    std::vector<FrequencyData> bu;
  };

  template <class> struct cb { using bw = int; };
  template <class bx> class bi : BlockFrequencyInfoImplBase
  {
    using bw = typename cb<bx>::bw;
  public:
    void bl();
  };

  template <class bx> void bi<bx>::bl()
  {
    const bw *by;
    bv<const int *> bz;
    ag(bz, bt(by), 0);
    bu.av();
  }

  template <> struct bn<const int *> { using u = ca; };
  void bm::bl() { bk->bl(); }
}

/* The t1 register is to initial symbol reference for call instruction.  */
/* { dg-final { scan-assembler "la\tt1,.*FrequencyData.*_M_default_append.*" } } */
