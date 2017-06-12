// { dg-do run { target c++11 } }
#include <cassert>

template<typename T>
  bool
  f()
  { return __is_aggregate(T); } 

template<typename T>
  class My
  {
  public:
    bool
    f()
    { return !!__is_aggregate(T); }
  };

template<typename T>
  class My2
  {
  public:
    static const bool trait = __is_aggregate(T);
  };

template<typename T>
  const bool My2<T>::trait;

template<typename T, bool b = __is_aggregate(T)>
  struct My3_help
  { static const bool trait = b; };

template<typename T, bool b>
  const bool My3_help<T, b>::trait;

template<typename T>
  class My3
  {
  public:
    bool
    f()
    { return My3_help<T>::trait; }
  };

#define PTEST(T) (__is_aggregate(T) && f<T>() \
		  && My<T>().f() && My2<T>::trait && My3<T>().f())

#define NTEST(T) (!__is_aggregate(T) && !f<T>() \
		  && !My<T>().f() && !My2<T>::trait && !My3<T>().f())

struct A { int a, b, c; };
class B { static int a; private: static int b; public: int c; };
struct C { C () {} int a, b, c; };
struct D { explicit D (int) {} int a, b, c; };
struct E : public A { int d, e, f; };
struct F : public C { using C::C; int d, e, f; };
class G { int a, b; };
struct H { private: int a, b; };
struct I { protected: int a, b; };
struct J { int a, b; void foo (); };
struct K { int a, b; virtual void foo (); };
struct L : virtual public A { int d, e; };
struct M : protected A { int d, e; };
struct N : private A { int d, e; };
typedef int T;
typedef float U;
typedef int V __attribute__((vector_size (4 * sizeof (int))));
typedef double W __attribute__((vector_size (8 * sizeof (double))));

int
main ()
{
  assert (NTEST (void));
  assert (NTEST (int));
  assert (NTEST (double));
  assert (NTEST (T));
  assert (NTEST (U));
  assert (PTEST (V));
  assert (PTEST (W));
  assert (PTEST (A));
  assert (PTEST (B));
  assert (NTEST (C));
  assert (NTEST (D));
#if __cplusplus >= 201703L
  assert (PTEST (E));
#else
  assert (NTEST (E));
#endif
  assert (NTEST (F));
  assert (NTEST (G));
  assert (NTEST (H));
  assert (NTEST (I));
  assert (PTEST (J));
  assert (NTEST (K));
  assert (NTEST (L));
  assert (NTEST (M));
  assert (NTEST (N));
  assert (PTEST (int[]));
  assert (PTEST (double[]));
  assert (PTEST (T[2]));
  assert (PTEST (U[]));
  assert (PTEST (V[]));
  assert (PTEST (W[]));
  assert (PTEST (A[19]));
  assert (PTEST (B[]));
  assert (PTEST (C[]));
  assert (PTEST (D[]));
  assert (PTEST (E[]));
  assert (PTEST (F[]));
  assert (PTEST (G[]));
  assert (PTEST (H[]));
  assert (PTEST (I[]));
  assert (PTEST (J[24]));
  assert (PTEST (K[]));
  assert (PTEST (L[]));
  assert (PTEST (M[6]));
  assert (PTEST (N[]));
}
