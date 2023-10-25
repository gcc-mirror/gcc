// PR c++/99804

struct S {
  enum E { A, B } e : 1;
  void f(E);
  template<class> void g() { f(e); }
};

int main() {
  S s;
  s.g<int>();
}
