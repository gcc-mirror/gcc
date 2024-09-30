/* { dg-do compile { target { musttail } } } */
/* { dg-options "-std=gnu++11" } */
/* { dg-additional-options "-fdelayed-branch" { target sparc*-*-* } } */

template <class T> T f();

class C
{
  double x;
public:
  C(double x) : x(x) {}
  ~C() { asm("":::"memory"); }
  operator int() { return x; }
};

template <class T>
__attribute__((noinline, noclone, noipa))
T g5() { [[gnu::musttail]] return f<C> (); } /* { dg-error "cannot tail-call" } */

C h();

__attribute__((noinline, noclone, noipa))
int g6() { [[gnu::musttail]] return h (); }  /* { dg-error "cannot tail-call" } */

__attribute__((noinline, noclone, noipa))
C g7() { [[gnu::musttail]] return h (); } /* { dg-error "cannot tail-call" } */

int main()
{
  g5<int> ();
  g6 ();
  g7 ();
}
