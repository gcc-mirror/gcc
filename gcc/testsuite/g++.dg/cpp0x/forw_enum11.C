// { dg-do compile { target c++11 } }

enum { A = 1 };
struct T
{
  int i1, i2, i3, i4, i5, i6, i7;
  enum E2 : int;

  void f();
};

enum T::E2 : int { A1 = A, A2 = 23 };

static_assert(int(T::A1) == 1, "error");
static_assert(int(T::A2) == 23, "error");

void T::f()
{
  static_assert(int(T::A1) == 1, "error");
  static_assert(int(T::A2) == 23, "error");
  static_assert(int(A1) == 1, "error");
  static_assert(int(A2) == 23, "error");
}
