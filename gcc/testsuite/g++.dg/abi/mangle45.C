// Testcase for mangling of parameters used other than in a trailing return type
// { dg-do compile { target c++11 } }
// { dg-options "-fabi-version=5 -fabi-compat-version=5 -Wabi=4" }

template<class T> void f(T p, decltype(p)) { }	     // L = 1 { dg-warning "mangle" }
template<class T> void g(T p, decltype(p) (*)()) { } // L = 1 { dg-warning "mangle" }
// G++ incorrectly rejects these currently.
// template<class T> void h(T p, auto (*)()->decltype(p));    // L = 1
// template<class T> void i(T p, auto (*)(T q)->decltype(q)); // L = 0
// template<class T> void j(T p, auto (*)(decltype(p))->T);   // L = 2
template<class T> void k(T p, int (*(*)(T* p))[sizeof(p)]) {} // L = 1 { dg-warning "mangle" }

int garg();
int (*karg (int*))[sizeof(int)];
int main()
{
  // { dg-final { scan-assembler  "\n_?_Z1fIiEvT_DtfL0p_E\[: \t\n\]" } }
  f (1,0);
  // { dg-final { scan-assembler  "\n_?_Z1gIiEvT_PFDtfL0p_EvE\[: \t\n\]" } }
  g (1,garg);
  // h (1,0);
  // i (1,0);
  // j (1,0);
  // { dg-final { scan-assembler  "\n_?_Z1kIiEvT_PFPAszfL0p__iPS0_E\[: \t\n\]" } }
  k (1,karg);
}
