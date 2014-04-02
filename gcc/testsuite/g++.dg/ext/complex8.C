// PR libstdc++/48760
// { dg-do run { target c++11 } }

constexpr _Complex int i{1,2};
constexpr _Complex int j{3};

#define SA(X) static_assert((X),#X)

SA(__real i == 1);
SA(__imag i == 2);
SA(__real j == 3);
SA(__imag j == 0);

struct A
{
  _Complex int c;
  constexpr A(int i, int j): c{i,j} { }
  constexpr A(int i): c{i} { }
};

constexpr A a1(1,2);
constexpr A a2(3);

SA(__real a1.c == 1);
SA(__imag a1.c == 2);
SA(__real a2.c == 3);
SA(__imag a2.c == 0);

typedef _Complex int ci;

SA((__real ci{1,2} == 1));
SA((__imag ci{1,2} == 2));
SA((__real ci{3} == 3));
SA((__imag ci{3} == 0));

struct B
{
  _Complex int c;
  int i;
};

constexpr B b1 = { { 1,2 }, 42 };
constexpr B b2 = { { 3 }, 24 };
// No brace elision for complex.
constexpr B b3 = { 5, 6 };

SA(__real b1.c == 1);
SA(__imag b1.c == 2);
SA(b1.i == 42);
SA(__real b2.c == 3);
SA(__imag b2.c == 0);
SA(b2.i == 24);
SA(__real b3.c == 5);
SA(__imag b3.c == 0);
SA(b3.i == 6);

int main()
{
  ci* p = new ci{1,2};
  if (__real *p != 1 || __imag *p != 2)
    return 1;
  delete p;
  p = new ci{3};
  if (__real *p != 3 || __imag *p != 0)
    return 1;
}
