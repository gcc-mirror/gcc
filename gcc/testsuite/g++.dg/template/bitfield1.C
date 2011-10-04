// PR c++/50280

struct S { int bf : 3; };

template<class _T1>
void make_pair(_T1& __x) {}

void foo() {
  const S s = S();
  make_pair(s.bf);
}
