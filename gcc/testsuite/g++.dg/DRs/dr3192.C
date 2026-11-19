// CWG 3192
// { dg-do compile { target c++11 } }

  void f() {
    struct X {
      struct S;
      enum E : int;
    };
    struct X::S {};         // OK
    enum X::E : int {};     // ill-formed, but all major implementations accept
  }
