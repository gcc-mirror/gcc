// PR c++/88294
// { dg-do compile { target c++11 } }

constexpr int foo (bool b) { return b; }

template<typename> struct A
{
  constexpr int f () { return 0; }
  bool b = true;
  void g () noexcept (f()) { }
  void g2 () noexcept (this->f()) { }
  void g3 () noexcept (b) { } // { dg-error "use of .this. in a constant expression|use of parameter|.this. is not a constant" }
  void g4 (int i) noexcept (i) { } // { dg-error "use of parameter" }
  void g5 () noexcept (A::f()) { }
  void g6 () noexcept (foo(b)) { } // { dg-error "use of .this. in a constant expression|use of parameter|.this. is not a constant" }
  void g7 () noexcept (int{f()}) { }
};

int main ()
{
  A<int> a;
  a.g ();
  a.g2 ();
  a.g3 ();
  a.g4 (1);
  a.g5 ();
  a.g6 ();
  a.g7 ();
}
