// { dg-do compile }
// { dg-additional-options "-fisolate-erroneous-paths-attribute" }

class A;
struct B {
  typedef A type;
};
template <typename> struct I : B {};
class C {
public:
  C(char *);
  int size();
};
template <typename> struct D;
template <typename _Tp, typename = D<_Tp>> class F {
  class G {
    template <typename> static _Tp *__test();
    typedef int _Del;

  public:
    typedef decltype(__test<_Del>()) type;
  };

public:
  typename I<_Tp>::type operator*() {
    typename G::type a = 0;
    return *a;
  }
};
class H {
  F<A> Out;
  H();
};
void fn1(void *, void *, int) __attribute__((__nonnull__));
class A {
  int OutBufEnd, OutBufCur;

public:
  void operator<<(C p1) {
    int b, c = p1.size();
    if (OutBufEnd)
      fn1(&OutBufCur, &b, c);
  }
};
char* a;
H::H() { *Out << a; }
