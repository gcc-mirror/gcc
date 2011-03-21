// Testcase for mangling of parameters used other than in a trailing return type
// { dg-options "-std=c++0x -fabi-version=5" }

template<class T> void f(T p, decltype(p)) { }                // L = 1
template<class T> void g(T p, decltype(p) (*)()) { }          // L = 1
// G++ incorrectly rejects these currently.
// template<class T> void h(T p, auto (*)()->decltype(p));    // L = 1
// template<class T> void i(T p, auto (*)(T q)->decltype(q)); // L = 0
// template<class T> void j(T p, auto (*)(decltype(p))->T);   // L = 2
template<class T> void k(T p, int (*(*)(T* p))[sizeof(p)]) {} // L = 1

int garg();
int (*karg (int*))[sizeof(int)];
int main()
{
  // { dg-final { scan-assembler  "_Z1fIiEvT_DtfL0p_E" } }
  f (1,0);
  // { dg-final { scan-assembler  "_Z1gIiEvT_PFDtfL0p_EvE" } }
  g (1,garg);
  // h (1,0);
  // i (1,0);
  // j (1,0);
  // { dg-final { scan-assembler  "_Z1kIiEvT_PFPAszfL0p__iPS0_E" } }
  k (1,karg);
}
