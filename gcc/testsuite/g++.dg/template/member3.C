template<typename T>
struct A {
  template<typename L> struct SubA { };

  template<typename T1,typename L> void f(T1 & t1, SubA<L> & t2) { }
  template<typename U> void g(SubA<U> & suba) { }
  template<typename U> void h(SubA<U> & suba) { }
};

int main(void) {
  int i;
  A<int> a;
  A<int>::SubA<int> suba;

  a.f(i,suba);
  a.g(suba);
  a.h(suba);
}

