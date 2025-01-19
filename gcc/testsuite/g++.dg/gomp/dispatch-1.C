struct S { 
  int a; 
  void f0(double); 
  int operator()() { return a; }
  bool operator!() { return !a; }
};

int f0(int);
template <typename T>
T f1(T a, T b);
void (*f2)(void);

namespace N {
  class C{};
  void f0(C);
  int a;
}

int test() {
  int result;
  double d = 5.0;
  N::C c;
  S s;
  S* sp = &s;
  int &r = result;
  #pragma omp dispatch
  result = f0(5);
  #pragma omp dispatch
  r = f0(5);
  #pragma omp dispatch
  N::a = ::f0(5);
  #pragma omp dispatch
  sp->a = f1<int>(5, 10);
  #pragma omp dispatch
  s.a = f1(5, 10);
  #pragma omp dispatch
  f2();
  #pragma omp dispatch
  N::f0(c);
  #pragma omp dispatch
  f0(c);
  #pragma omp dispatch
  s.f0(d);
  #pragma omp dispatch
  sp->f0(d);
  #pragma omp dispatch
  sp->f0(d);
  #pragma omp dispatch
  s();
  #pragma omp dispatch
  !s;
  return result; 
}
