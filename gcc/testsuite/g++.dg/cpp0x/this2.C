// PR c++/88548
// { dg-do compile { target c++11 } }

struct S {
  int a;
  template <class> static auto m1 ()
    -> decltype(this->a) { return 0; } // { dg-error "'this'" }
};
