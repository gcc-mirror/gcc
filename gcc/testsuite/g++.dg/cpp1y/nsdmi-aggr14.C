// PR c++/100252
// { dg-do run { target c++14 } }

#define SA(X) static_assert ((X),#X)

struct A {
  int x;
  int y = x;
};

struct B {
  int x = 0;
  int y = A{x}.y;
};

constexpr B csb1 = { };
SA(csb1.x == 0 && csb1.y == csb1.x);
constexpr B csb2 = { 1 };
SA(csb2.x == 1 && csb2.y == csb2.x);
constexpr B csb3 = { 1, 2 };
SA(csb3.x == 1 && csb3.y == 2);

B sb1 = { };
B sb2 = { 1 };
B sb3 = { 1, 2};

struct C {
  int x = 0;
  int y = (true, A{x}.y) + (A{x}.y, 0);
};

constexpr C csc1 = { };
SA(csc1.x == 0 && csc1.y == csc1.x);
constexpr C csc2 = { 1 };
SA(csc2.x == 1 && csc2.y == csc2.x);
constexpr C csc3 = { 1, 2 };
SA(csc3.x == 1 && csc3.y == 2);

C sc1 = { };
C sc2 = { 1 };
C sc3 = { 1, 2};

struct D {
  int x = 0;
  int y = (A{x}.y);
};

constexpr D csd1 = { };
SA(csd1.x == 0 && csd1.y == csd1.x);
constexpr D csd2 = { 1 };
SA(csd2.x == 1 && csd2.y == csd2.x);
constexpr D csd3 = { 1, 2 };
SA(csd3.x == 1 && csd3.y == 2);

D sd1 = { };
D sd2 = { 1 };
D sd3 = { 1, 2};

struct E {
  int x = 0;
  int y = x ? A{x}.y : A{x}.y;
};

constexpr E cse1 = { };
SA(cse1.x == 0 && cse1.y == cse1.x);
constexpr E cse2 = { 1 };
SA(cse2.x == 1 && cse2.y == cse2.x);
constexpr E cse3 = { 1, 2 };
SA(cse3.x == 1 && cse3.y == 2);

E se1 = { };
E se2 = { 1 };
E se3 = { 1, 2};

int
main ()
{
  if (sb1.x != 0 || sb1.x != sb1.y)
    __builtin_abort();
  if (sb2.x != 1 || sb2.x != sb2.y)
    __builtin_abort();
  if (sb3.x != 1 || sb3.y != 2)
    __builtin_abort();

  if (sc1.x != 0 || sc1.x != sc1.y)
    __builtin_abort();
  if (sc2.x != 1 || sc2.x != sc2.y)
    __builtin_abort();
  if (sc3.x != 1 || sc3.y != 2)
    __builtin_abort();

  B b1 = { };
  B b2 = { 1 };
  B b3 = { 1, 2};
  if (b1.x != 0 || b1.x != b1.y)
    __builtin_abort();
  if (b2.x != 1 || b2.x != b2.y)
    __builtin_abort();
  if (b3.x != 1 || b3.y != 2)
    __builtin_abort();

  C c1 = { };
  C c2 = { 1 };
  C c3 = { 1, 2};
  if (c1.x != 0 || c1.x != c1.y)
    __builtin_abort();
  if (c2.x != 1 || c2.x != c2.y)
    __builtin_abort();
  if (c3.x != 1 || c3.y != 2)
    __builtin_abort();

  D d1 = { };
  D d2 = { 1 };
  D d3 = { 1, 2};
  if (d1.x != 0 || d1.x != d1.y)
    __builtin_abort();
  if (d2.x != 1 || d2.x != d2.y)
    __builtin_abort();
  if (d3.x != 1 || d3.y != 2)
    __builtin_abort();

  E e1 = { };
  E e2 = { 1 };
  E e3 = { 1, 2};
  if (e1.x != 0 || e1.x != e1.y)
    __builtin_abort();
  if (e2.x != 1 || e2.x != e2.y)
    __builtin_abort();
  if (e3.x != 1 || e3.y != 2)
    __builtin_abort();
}
