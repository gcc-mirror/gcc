// { dg-do compile }
// { dg-options "-fopenmp" }

namespace N1
{
  #pragma omp declare reduction (| : long int : omp_out |= omp_in)	// { dg-error "predeclared arithmetic type" }
  #pragma omp declare reduction (+ : char : omp_out += omp_in)		// { dg-error "predeclared arithmetic type" }
  typedef short T;
  #pragma omp declare reduction (min : T : omp_out += omp_in)		// { dg-error "predeclared arithmetic type" }
  #pragma omp declare reduction (* : _Complex double : omp_out *= omp_in)// { dg-error "predeclared arithmetic type" }
}
namespace N2
{
  template <typename T1, typename T2, typename T3, typename T4>
  struct S
  {
    #pragma omp declare reduction (| : T1 : omp_out |= omp_in)		// { dg-error "predeclared arithmetic type" }
    #pragma omp declare reduction (+ : T2 : omp_out += omp_in)		// { dg-error "predeclared arithmetic type" }
    typedef T3 T;
    #pragma omp declare reduction (min : T : omp_out += omp_in)		// { dg-error "predeclared arithmetic type" }
    #pragma omp declare reduction (* : T4 : omp_out *= omp_in)		// { dg-error "predeclared arithmetic type" }
  };
  S<long int, char, short, _Complex double> s;
  template <typename T1, typename T2, typename T3, typename T4>
  int foo ()
  {
    #pragma omp declare reduction (| : T1 : omp_out |= omp_in)		// { dg-error "predeclared arithmetic type" }
    #pragma omp declare reduction (+ : T2 : omp_out += omp_in)		// { dg-error "predeclared arithmetic type" }
    typedef T3 T;
    #pragma omp declare reduction (min : T : omp_out += omp_in)		// { dg-error "predeclared arithmetic type" }
    #pragma omp declare reduction (* : T4 : omp_out *= omp_in)		// { dg-error "predeclared arithmetic type" }
    return 0;
  }
  int x = foo <long int, char, short, _Complex double> ();
}
namespace N3
{
  void bar ();
  #pragma omp declare reduction (| : __typeof (bar) : omp_out |= omp_in)// { dg-error "function or array type" }
  #pragma omp declare reduction (+ : char () : omp_out += omp_in)	// { dg-error "function or array type" }
  typedef short T;
  #pragma omp declare reduction (min : T[2] : omp_out += omp_in)	// { dg-error "function or array type" }
  #pragma omp declare reduction (baz : char & : omp_out *= omp_in)	// { dg-error "reference type" }
}
namespace N4
{
  void bar ();
  template <typename T1, typename T2, typename T3, typename T4>
  struct S
  {
    #pragma omp declare reduction (| : T1 : omp_out |= omp_in)		// { dg-error "function or array type" }
    #pragma omp declare reduction (+ : T2 : omp_out += omp_in)		// { dg-error "function or array type" }
    typedef T3 T;
    #pragma omp declare reduction (min : T : omp_out += omp_in)		// { dg-error "function or array type" }
    #pragma omp declare reduction (baz : T4 : omp_out *= omp_in)	// { dg-error "function or array type" }
  };
  S<__typeof (bar), char (), short [3], char []> s;
  template <typename T1, typename T2, typename T3, typename T4>
  int foo ()
  {
    #pragma omp declare reduction (| : T1 : omp_out |= omp_in)		// { dg-error "function or array type" }
    #pragma omp declare reduction (+ : T2 : omp_out += omp_in)		// { dg-error "function or array type" }
    typedef T3 T;
    #pragma omp declare reduction (min : T : omp_out += omp_in)		// { dg-error "function or array type" }
    #pragma omp declare reduction (baz : T4 : omp_out *= omp_in)	// { dg-error "function or array type" }
    return 0;
  }
  int x = foo <__typeof (bar), char (), short[], char [2]> ();
}
namespace N5
{
  template <typename T>
  struct S
  {
    #pragma omp declare reduction (baz : T : omp_out *= omp_in)		// { dg-error "reference type" }
  };
  S<char &> s;
  template <typename T>
  int foo ()
  {
    #pragma omp declare reduction (baz : T : omp_out *= omp_in)		// { dg-error "reference type" }
    return 0;
  }
  int x = foo <char &> ();
}
namespace N6
{
  struct A { int a; A () : a (0) {} };
  #pragma omp declare reduction (| : const A : omp_out.a |= omp_in.a)	// { dg-error "const, volatile or __restrict" }
  #pragma omp declare reduction (+ : __const A : omp_out.a += omp_in.a)	// { dg-error "const, volatile or __restrict" }
  typedef volatile A T;
  #pragma omp declare reduction (min : T : omp_out.a += omp_in.a)	// { dg-error "const, volatile or __restrict" }
  #pragma omp declare reduction (* : A *__restrict : omp_out->a *= omp_in->a)// { dg-error "const, volatile or __restrict" }
}
namespace N7
{
  struct A { int a; A () : a (0) {} };
  template <typename T1, typename T2, typename T3, typename T4>
  struct S
  {
    #pragma omp declare reduction (| : T1 : omp_out |= omp_in)		// { dg-error "const, volatile or __restrict" }
    #pragma omp declare reduction (+ : T2 : omp_out += omp_in)		// { dg-error "const, volatile or __restrict" }
    typedef T3 T;
    #pragma omp declare reduction (min : T : omp_out += omp_in)		// { dg-error "const, volatile or __restrict" }
    #pragma omp declare reduction (* : T4 : omp_out *= omp_in)		// { dg-error "const, volatile or __restrict" }
  };
  S<const A, __const A, volatile A, A *__restrict> s;
  template <typename T1, typename T2, typename T3, typename T4>
  int foo ()
  {
    #pragma omp declare reduction (| : T1 : omp_out |= omp_in)		// { dg-error "const, volatile or __restrict" }
    #pragma omp declare reduction (+ : T2 : omp_out += omp_in)		// { dg-error "const, volatile or __restrict" }
    typedef T3 T;
    #pragma omp declare reduction (min : T : omp_out += omp_in)		// { dg-error "const, volatile or __restrict" }
    #pragma omp declare reduction (* : T4 : omp_out *= omp_in)		// { dg-error "const, volatile or __restrict" }
    return 0;
  }
  int x = foo <const A, __const A, volatile A, A *__restrict> ();
}
