/* { dg-do compile { target { musttail } } } */
/* { dg-options "-std=gnu++11" } */
/* { dg-additional-options "-fdelayed-branch" { target sparc*-*-* } } */

template <class T> T f();

double g() { [[gnu::musttail]] return f<int>(); } /* { dg-error "cannot tail-call" } */

template <class T>
__attribute__((noinline, noclone, noipa))
T g1() { [[gnu::musttail]] return f<T>(); } /* { dg-error "target is not able" "" { target { ! external_musttail } } } */

template <class T>
__attribute__((noinline, noclone, noipa))
T g2() { [[gnu::musttail]] return f<T>(); } /* { dg-error "target is not able" "" { target { ! external_musttail } } } */

template <class T>
__attribute__((noinline, noclone, noipa))
/* Would work with -O1.  */
T g3() { [[gnu::musttail]] return f<T>(); } /* { dg-error "cannot tail-call" } */

template <class T>
__attribute__((noinline, noclone, noipa))
T g4() { [[gnu::musttail]] return f<double>(); } /* { dg-error "cannot tail-call" } */

class C
{
  double x;
public:
  C(double x) : x(x) {}
  ~C() { asm("":::"memory"); }
};

int main()
{
  g1<int>();
  g2<double>();
  g3<C>();
  g4<int>();
}
