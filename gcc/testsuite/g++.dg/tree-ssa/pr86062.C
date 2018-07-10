// { dg-do compile }
// { dg-require-effective-target c++14 }
// { dg-options "-O2 -ffinite-math-only -fdump-tree-fre1" }

#include <array>

struct I { double i,s; I(double d):i(d),s(d){} };
typedef std::array<double,3> P;
typedef std::array<I,3> AP;

static AP c(P const&p){
      return {p[0],p[1],p[2]};
}
template<class T> auto const& ac(T const&p, int n){return p[n];}
static double g(P const&p, int n)
{
    I res = ac(c(p),n);
      return res.s-res.i;
}

__attribute__((flatten)) double fff(P p){ return g(p,1); }

// { dg-final { scan-tree-dump "return 0.0;" "fre1" } }
