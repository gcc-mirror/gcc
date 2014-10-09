// PR c++/55250
// { dg-do compile { target c++14 } }

#define SA(X) static_assert((X),#X)

constexpr int Test1(int x) { enum E { y = 1 }; return x + y; }

constexpr int Test2(int x) { struct T { constexpr operator int() { return 1; } }; return x + T(); }

constexpr int Test3(int x) { typedef enum E { y = 1 } EE; return x + EE::y; }

constexpr int Test4(int x) { typedef struct T { constexpr operator int() { return 1; } } TT; return x + TT(); }

constexpr int Test5(int x) { using EE = enum E { y = 1 }; return x + EE::y; }

constexpr int Test6(int x) { using TT = struct T { constexpr operator int() { return 1; } }; return x + TT(); }

SA(Test1(2) == 3);
SA(Test2(2) == 3);
SA(Test3(2) == 3);
SA(Test4(2) == 3);
SA(Test5(2) == 3);
SA(Test6(2) == 3);

struct S1
{
  constexpr S1() { enum E { y = 1 }; SA(y == 1); }
};

struct S2
{
  constexpr S2() { struct T { constexpr operator int() { return 1; } }; SA(T() == 1); }
};

struct S3
{
  constexpr S3() { typedef enum E { y = 1} EE; SA(EE::y == 1); }
};

struct S4
{
  constexpr S4() { typedef struct T { constexpr operator int() { return 1; } } TT; SA(TT() == 1); }
};

struct S5
{
  constexpr S5() { using EE = enum E { y = 1}; SA(EE::y == 1); }
};

struct S6
{
  constexpr S6() { using TT = struct T { constexpr operator int() { return 1; } }; SA(TT() == 1); }
};

S1 s1;
S2 s2;
S3 s3;
S4 s4;
S5 s5;
S6 s6;
