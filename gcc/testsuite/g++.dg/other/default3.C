// PR c++/16829
// { dg-do "compile" }

void f1(int = 0, int);                       // { dg-error "default" }

void f2(int = 0, int) {}                     // { dg-error "default" }

void f3(int, int);
void f3(int = 0, int);                       // { dg-error "default" }

void f4(int, int);
void f4(int = 0, int) {}                     // { dg-error "default" }

void f5();
void f5(int = 0, int);                       // { dg-error "default" }

void f6();
void f6(int = 0, int) {}                     // { dg-error "default" }

template<typename> void g1(int = 0, int);    // { dg-error "default" }

template<typename> void g2(int = 0, int) {}  // { dg-error "default" }

template<typename> void g3(int, int);
template<typename> void g3(int = 0, int);    // { dg-error "default" }

template<typename> void g4(int, int);
template<typename> void g4(int = 0, int) {}  // { dg-error "default" "" { xfail *-*-* } }

template<typename> void g5();
template<typename> void g5(int = 0, int);    // { dg-error "default" }

template<typename> void g6();
template<typename> void g6(int = 0, int) {}  // { dg-error "default" }

template<typename T> void g7(T, T)   {}
template<typename T> void g7(T* = 0, T*) {}  // { dg-error "default" }


struct A
{
  void F1(int = 0, int);                       // { dg-error "default" }

  void F2(int = 0, int) {}                     // { dg-error "default" }

  void F3(int, int);

  void F4();
  void F4(int = 0, int);                       // { dg-error "default" }

  void F5();
  void F5(int = 0, int) {}                     // { dg-error "default" }

  template<typename> void G1(int = 0, int);    // { dg-error "default" }

  template<typename> void G2(int = 0, int) {}  // { dg-error "default" }

  template<typename> void G3(int, int);

  template<typename> void G4();
  template<typename> void G4(int = 0, int);    // { dg-error "default" }

  template<typename> void G5();
  template<typename> void G5(int = 0, int) {}  // { dg-error "default" }

  template<typename T> void G6(T, T)   {}
  template<typename T> void G6(T* = 0, T*) {}  // { dg-error "default" }
};

void A::F3(int = 0, int) {}                     // { dg-error "default" }

template<typename> void A::G3(int = 0, int) {}  // { dg-error "default" }


template<typename> struct B
{
  void F1(int = 0, int);                       // { dg-error "default" }

  void F2(int = 0, int) {}                     // { dg-error "default" }

  void F3(int, int);

  void F4();
  void F4(int = 0, int);                       // { dg-error "default" }

  void F5();
  void F5(int = 0, int) {}                     // { dg-error "default" }

  template<typename> void G1(int = 0, int);    // { dg-error "default" }

  template<typename> void G2(int = 0, int) {}  // { dg-error "default" }

  template<typename> void G3(int, int);

  template<typename> void G4();
  template<typename> void G4(int = 0, int);    // { dg-error "default" }

  template<typename> void G5();
  template<typename> void G5(int = 0, int) {}  // { dg-error "default" }

  template<typename T> void G6(T, T)   {}
  template<typename T> void G6(T* = 0, T*) {}  // { dg-error "default" }
};

template<typename T>
void B<T>::F3(int = 0, int) {}  // { dg-error "default" }

template<typename T> template<typename>
void B<T>::G3(int = 0, int) {}  // { dg-error "default" }
