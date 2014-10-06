// PR c++/55250
// { dg-do compile { target c++11 } }

#define SA(X) static_assert((X),#X)

struct GS { constexpr operator int() { return 1; } };
enum GE { y = 1 };

constexpr int Test1(int x) { typedef int T; return T(x) + 1; }
constexpr int Test2(int x) { using T = int; return T(x) + 1; }
constexpr int Test3(int x) { typedef GS T; return x + T(); }
constexpr int Test4(int x) { using T = GS; return x + T(); }
constexpr int Test5(int x) { typedef GE T; return x + T::y; }
constexpr int Test6(int x) { using T = GE; return x + T::y; }

SA(Test1(2) == 3);
SA(Test2(2) == 3);
SA(Test3(2) == 3);
SA(Test4(2) == 3);
SA(Test5(2) == 3);
SA(Test6(2) == 3);

struct S1
{
  constexpr S1() { typedef int T; SA(T(1) == 1); }
};

struct S2
{
  constexpr S2() { using T = int; SA(T(1) == 1); }
};

struct S3
{
  constexpr S3() { typedef GS T; SA(T() == 1); }
};

struct S4
{
  constexpr S4() { using T = GS; SA(T() == 1); }
};

struct S5
{
  constexpr S5() { typedef GE T; SA(T::y == 1); }
};

struct S6
{
  constexpr S6() { using T = GE; SA(T::y == 1); }
};

S1 s1;
S2 s2;
S3 s3;
S4 s4;
S5 s5;
S6 s6;
