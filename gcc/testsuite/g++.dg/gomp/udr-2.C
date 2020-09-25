// { dg-do compile }
// { dg-options "-fopenmp" }

struct W { int w; W () : w (0) {} W (int x) : w (x) {} };
namespace N1
{
  int v;
  #pragma omp declare reduction (foo : long int : omp_out |= v)	// { dg-error "combiner refers to variable" }
  #pragma omp declare reduction (foo : char : omp_out = v)	// { dg-error "combiner refers to variable" }
  typedef short T;
  #pragma omp declare reduction (foo : T : omp_out += N1::v)	// { dg-error "combiner refers to variable" }
  #pragma omp declare reduction (foo : int : v *= omp_in)	// { dg-error "combiner refers to variable" }
  #pragma omp declare reduction (foo : W : omp_out.w *= omp_in.w + v) // { dg-error "combiner refers to variable" }
}
namespace N2
{
  int v;
  template <typename T1, typename T2, typename T3, typename T4, typename T5>
  struct S
  {
    #pragma omp declare reduction (foo : T1 : omp_out |= v)	// { dg-error "combiner refers to variable" }
    #pragma omp declare reduction (foo : T2 : omp_out = v)	// { dg-error "combiner refers to variable" }
    typedef T3 T;
    #pragma omp declare reduction (foo : T : omp_out += N1::v)	// { dg-error "combiner refers to variable" }
    #pragma omp declare reduction (foo : T4 : v *= omp_in)	// { dg-error "combiner refers to variable" }
    #pragma omp declare reduction (foo : T5 : omp_out.w *= omp_in.w + v) // { dg-error "combiner refers to variable" }
  };
  S<long int, char, short, _Complex double, W> s;
  template <typename T1, typename T2, typename T3, typename T4, typename T5>
  int foo ()
  {
    #pragma omp declare reduction (foo : T1 : omp_out |= v)	// { dg-error "combiner refers to variable" }
    #pragma omp declare reduction (foo : T2 : omp_out = v)	// { dg-error "combiner refers to variable" }
    typedef T3 T;
    #pragma omp declare reduction (foo : T : omp_out += N1::v)	// { dg-error "combiner refers to variable" }
    #pragma omp declare reduction (foo : T4 : v *= omp_in)	// { dg-error "combiner refers to variable" }
    // { dg-error "in assignment" "" { target *-*-* } .-1 }
    #pragma omp declare reduction (foo : T5 : omp_out.w *= omp_in.w + v) // { dg-error "combiner refers to variable" }
    return 0;
  }
  int x = foo <long int, char, short, _Complex double, W> ();
}
namespace N3
{
  int v;
  #pragma omp declare reduction (foo : long int : omp_out |= omp_in) initializer (omp_priv = v) // { dg-error "initializer refers to variable" }
  #pragma omp declare reduction (foo : char : omp_out += omp_in) initializer (omp_priv ((char) N3::v)) // { dg-error "initializer refers to variable" }
  typedef short T;
  #pragma omp declare reduction (foo : T : omp_out += omp_in) initializer (omp_priv = (short) v) // { dg-error "initializer refers to variable" }
  #pragma omp declare reduction (foo : _Complex double : omp_out *= omp_in) initializer (omp_priv (v)) // { dg-error "initializer refers to variable" }
  #pragma omp declare reduction (foo : W : omp_out.w *= omp_in.w) initializer (omp_priv (N3::v)) // { dg-error "initializer refers to variable" }
}
namespace N4
{
  int v;
  template <typename T1, typename T2, typename T3, typename T4, typename T5>
  struct S
  {
    #pragma omp declare reduction (foo : T1 : omp_out |= omp_in) initializer (omp_priv = v) // { dg-error "initializer refers to variable" }
    #pragma omp declare reduction (foo : T2 : omp_out += omp_in) initializer (omp_priv ((char) N3::v)) // { dg-error "initializer refers to variable" }
    typedef T3 T;
    #pragma omp declare reduction (foo : T : omp_out += omp_in) initializer (omp_priv = (short) v) // { dg-error "initializer refers to variable" }
    #pragma omp declare reduction (foo : T4 : omp_out *= omp_in) initializer (omp_priv (v)) // { dg-error "initializer refers to variable" }
    #pragma omp declare reduction (foo : T5 : omp_out.w *= omp_in.w) initializer (omp_priv (N3::v)) // { dg-error "initializer refers to variable" }
  };
  S<long int, char, short, _Complex double, W> s;
  template <typename T1, typename T2, typename T3, typename T4, typename T5>
  int foo ()
  {
    #pragma omp declare reduction (foo : T1 : omp_out |= omp_in) initializer (omp_priv = v) // { dg-error "initializer refers to variable" }
    #pragma omp declare reduction (foo : T2 : omp_out += omp_in) initializer (omp_priv ((char) N3::v)) // { dg-error "initializer refers to variable" }
    typedef T3 T;
    #pragma omp declare reduction (foo : T : omp_out += omp_in) initializer (omp_priv = (short) v) // { dg-error "initializer refers to variable" }
    #pragma omp declare reduction (foo : T4 : omp_out *= omp_in) initializer (omp_priv (v)) // { dg-error "initializer refers to variable" }
    #pragma omp declare reduction (foo : T5 : omp_out.w *= omp_in.w) initializer (omp_priv (N3::v)) // { dg-error "initializer refers to variable" }
    return 0;
  }
  int x = foo <long int, char, short, _Complex double, W> ();
}
template <typename T>
void init (T &, int &);
template <typename T>
void initializer (T, int &);
namespace N5
{
  int v;
  #pragma omp declare reduction (foo : long int : omp_out |= omp_in) initializer (init (omp_priv, v)) // { dg-error "initializer refers to variable" }
  #pragma omp declare reduction (foo : char : omp_out += omp_in) initializer (initializer (&omp_priv, N3::v)) // { dg-error "initializer refers to variable" }
  typedef short T;
  #pragma omp declare reduction (foo : T : omp_out += omp_in) initializer (init (omp_priv, v)) // { dg-error "initializer refers to variable" }
  #pragma omp declare reduction (foo : _Complex double : omp_out *= omp_in) initializer (initializer (&omp_priv, v)) // { dg-error "initializer refers to variable" }
  #pragma omp declare reduction (foo : W : omp_out.w *= omp_in.w) initializer (init (omp_priv, N3::v)) // { dg-error "initializer refers to variable" }
}
namespace N6
{
  int v;
  template <typename T1, typename T2, typename T3, typename T4, typename T5>
  struct S
  {
    #pragma omp declare reduction (foo : T1 : omp_out |= omp_in) initializer (initializer (&omp_priv, v)) // { dg-error "initializer refers to variable" }
    #pragma omp declare reduction (foo : T2 : omp_out += omp_in) initializer (init (omp_priv, N3::v)) // { dg-error "initializer refers to variable" }
    typedef T3 T;
    #pragma omp declare reduction (foo : T : omp_out += omp_in) initializer (init (omp_priv, v)) // { dg-error "initializer refers to variable" }
    #pragma omp declare reduction (foo : T4 : omp_out *= omp_in) initializer (init (omp_priv, v)) // { dg-error "initializer refers to variable" }
    #pragma omp declare reduction (foo : T5 : omp_out.w *= omp_in.w) initializer (initializer (&omp_priv, N3::v)) // { dg-error "initializer refers to variable" }
  };
  S<long int, char, short, _Complex double, W> s;
  template <typename T1, typename T2, typename T3, typename T4, typename T5>
  int foo ()
  {
    #pragma omp declare reduction (foo : T1 : omp_out |= omp_in) initializer (init (omp_priv, v)) // { dg-error "initializer refers to variable" }
    #pragma omp declare reduction (foo : T2 : omp_out += omp_in) initializer (init (omp_priv, N3::v)) // { dg-error "initializer refers to variable" }
    typedef T3 T;
    #pragma omp declare reduction (foo : T : omp_out += omp_in) initializer (initializer (&omp_priv, v)) // { dg-error "initializer refers to variable" }
    #pragma omp declare reduction (foo : T4 : omp_out *= omp_in) initializer (init (omp_priv, v)) // { dg-error "initializer refers to variable" }
    #pragma omp declare reduction (foo : T5 : omp_out.w *= omp_in.w) initializer (initializer (omp_priv, N3::v)) // { dg-error "initializer refers to variable" }
    return 0;
  }
  int x = foo <long int, char, short, _Complex double, W> ();
}
