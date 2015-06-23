// PR c++/55877
// { dg-require-weak "" }

namespace N1 {
  typedef struct {
    typedef enum { X, Y } A;
    typedef struct { } B;
    struct C {
      // { dg-final { scan-assembler ".weak\(_definition\)?\[ \t\]_?_ZN2N11D1C3fn1ENS0_1BE" { target c++11 } } }
      static void fn1 (B) { }
      // { dg-final { scan-assembler ".weak\(_definition\)?\[ \t\]_?_ZN2N11D1C3fn2ES1_" { target c++11 } } }
      static void fn2 (C) { }
    };
  } D;

  void *p = (void *) D::C::fn1;
  void *q = (void *) D::C::fn2;
}

namespace N2 {
  typedef struct {
    typedef enum { X, Y } A;
    typedef struct { } B;
    struct C {
      // { dg-final { scan-assembler-not ".weak\(_definition\)?\[ \t\]_?_ZN2N23._31C3fn1ENS0_1BE" { target c++11 } } }
      static void fn1 (B) { }
      // { dg-final { scan-assembler-not ".weak\(_definition\)?\[ \t\]_?_ZN2N23._31C3fn2ES1_" { target c++11 } } }
      static void fn2 (C) { }
    };
  } const D;

  void *p = (void *) D::C::fn1;
  void *q = (void *) D::C::fn2;
}

namespace N3 {
  typedef struct {
    typedef enum { X, Y } A;
    typedef struct { } B;
    template <class T> struct C {
      // { dg-final { scan-assembler ".weak\(_definition\)?\[ \t\]_?_ZN2N31D1CIiE3fn1ENS0_1BE" { target c++11 } } }
      static void fn1 (B) { }
      // { dg-final { scan-assembler ".weak\(_definition\)?\[ \t\]_?_ZN2N31D1CIiE3fn2ES2_" { target c++11 } } }
      static void fn2 (C) { }
    };
  } D;

  void *p = (void *) D::C<int>::fn1;
  void *q = (void *) D::C<int>::fn2;
}

namespace N4 {
  typedef struct {
    typedef enum { X, Y } A;
    typedef struct { } B;
    template <class T> struct C {
      // { dg-final { scan-assembler-not ".weak\(_definition\)?\[ \t\]_?_ZN2N43._91CIiE3fn1ENS0_1BE" { target c++11 } } }
      static void fn1 (B) { } // { not-dg-error "no linkage" "" { target { ! c++11 } } }
      // { dg-final { scan-assembler-not ".weak\(_definition\)?\[ \t\]_?_ZN2N43._91CIiE3fn2ES2_" { target c++11 } } }
      static void fn2 (C) { } // { not-dg-error "no linkage" "" { target { ! c++11 } } }
    };
  } const D;

  void *p = (void *) D::C<int>::fn1;
  void *q = (void *) D::C<int>::fn2;
}
