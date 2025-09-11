// PR c++/121779
// A version of operator-8.C where the operator<=> return type is int.

// { dg-do compile { target c++20 } }

struct A {
  bool operator==(int);
  int operator<=>(int);
};

template<class T>
void f() {
  A a;
  (void)(a != 0);
  (void)(0 != a);
  (void)(a < 0);
  (void)(0 < a);
  (void)(a <= 0);
  (void)(0 <= a);
  (void)(a > 0);
  (void)(0 > a);
  (void)(a >= 0);
  (void)(0 >= a);
}

// These later-declared namespace-scope overloads shouldn't be considered
// when instantiating f<int>.
bool operator!=(A, int) = delete;
bool operator<(A, int) = delete;
bool operator<=(A, int) = delete;
bool operator>(A, int) = delete;
bool operator>=(A, int) = delete;

bool operator!=(int, A) = delete;
bool operator<(int, A) = delete;
bool operator<=(int, A) = delete;
bool operator>(int, A) = delete;
bool operator>=(int, A) = delete;

template void f<int>();
