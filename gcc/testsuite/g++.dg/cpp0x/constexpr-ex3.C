// { dg-options "-std=c++0x -ftrack-macro-expansion=0" }

#define SA(X) static_assert (X, #X)

struct A
{
  int i;
  constexpr A(int _i) { i = _i; } // { dg-error "empty body|uninitialized member" }
};

template <class T>
struct B
{
  T t;
  constexpr B(T _t): t(_t) { }
};

B<int> b(1);		       // { dg-message "not declared .constexpr" }
SA(b.t==1);			// { dg-error "non-constant condition|'b'" }
constexpr B<int> b2(1);
SA(b2.t==1);

template <class T>
constexpr T f(T a, T b)
{
  typedef T myT;
  return a + b;
}

SA(f(1,2)==3);
