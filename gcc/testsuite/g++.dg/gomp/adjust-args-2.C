struct S { 
  int a;
  int g (const void *b);
  #pragma omp declare variant (g) match (construct={dispatch}) adjust_args (need_device_ptr: b)
  int f0(const void *b); 
  int operator()() { return a; }
  bool operator!() { return !a; }
};

template <typename T>
T f0(T a, T *b);

#pragma omp declare variant (f0) match (construct={dispatch}) adjust_args (need_device_ptr: a, b)
template <typename T>
T f1(T a, T *b);

namespace N {
  class C{
    public:
  void g(C *c);
  #pragma omp declare variant (g) match (construct={dispatch}) adjust_args (need_device_ptr: c)
  void f0(C *c);
  };
  void g(C *c);
  #pragma omp declare variant (g) match (construct={dispatch}) adjust_args (need_device_ptr: c)
  void f0(C *c);
}

#pragma omp declare variant (g) match (construct={dispatch}) adjust_args (need_device_ptr: c)
void f3(N::C *c);
void f4(S *&s);
#pragma omp declare variant (f4) match (construct={dispatch}) adjust_args (need_device_ptr: s)
void f5(S *&s);

void test() {
  S s, *sp;
  N::C c;
  int *a, b;
  #pragma omp dispatch
  s.f0(a);
  #pragma omp dispatch
  f1(b, a);
  #pragma omp dispatch
  c.f0(&c);
  #pragma omp dispatch
  N::f0(&c);
  #pragma omp dispatch
  f3(&c);
  #pragma omp dispatch
  f5(sp);
}
