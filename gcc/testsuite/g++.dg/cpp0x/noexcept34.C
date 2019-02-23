// PR c++/88294
// { dg-do compile { target c++11 } }

constexpr int foo (bool b) { return b; }

template<typename> struct A
{
  constexpr int f () { return 0; }
  bool b = true;
  void g () noexcept (f()) { } // { dg-error "use of parameter" }
  void g2 () noexcept (this->f()) { } // { dg-error "use of parameter" }
  void g3 () noexcept (b) { } // { dg-error "use of .this. in a constant expression|use of parameter" }
  void g4 (int i) noexcept (i) { } // { dg-error "use of parameter" }
  void g5 () noexcept (A::f()) { } // { dg-error "use of parameter" }
  void g6 () noexcept (foo(b)) { } // { dg-error "use of .this. in a constant expression|use of parameter" }
  void g7 () noexcept (int{f()}) { } // { dg-error "use of parameter" }
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
