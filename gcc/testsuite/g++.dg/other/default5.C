// PR c++/28274
// { dg-do compile }

void f1(int, int, int, int, int = 0);
void f1(int, int, int, int = 0, int);
void f1(int, int, int = 0, int, int);
void f1(int = 0, int, int, int, int);    // { dg-error "default" }

void f2(int, int, int, int, int = 0) {}
void f2(int, int, int, int = 0, int);
void f2(int, int, int = 0, int, int);
void f2(int = 0, int, int, int, int);    // { dg-error "default" }

void f3(int, int, int, int, int = 0);
void f3(int, int, int, int = 0, int) {}
void f3(int, int, int = 0, int, int);
void f3(int = 0, int, int, int, int);    // { dg-error "default" }

void f4(int, int, int, int, int = 0);
void f4(int, int, int, int = 0, int);
void f4(int, int, int = 0, int, int) {}
void f4(int = 0, int, int, int, int);    // { dg-error "default" }

void f5(int, int, int, int, int = 0);
void f5(int, int, int, int = 0, int);
void f5(int, int, int = 0, int, int);
void f5(int = 0, int, int, int, int) {}  // { dg-error "default" }


struct A
{
  void F1(int, int, int = 0);
  void F2(int, int, int = 0);
};

void A::F1(int, int = 0, int) {}
void A::F2(int = 0, int, int) {}  // { dg-error "default" }


template<int> struct B
{
  void F1(int, int, int = 0);
  void F2(int, int, int = 0);
};

template<int N> void B<N>::F1(int, int = 0, int) {}
template<int N> void B<N>::F2(int = 0, int, int) {}  // { dg-error "default" }
