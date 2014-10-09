// PR c++/55250
// { dg-do compile { target c++11 } }

constexpr int Test1(int x) { enum E { y = 1 }; return x; }  // { dg-error "not a return-statement" "" { target { c++11_only } } }

constexpr int Test2(int x) { struct T { }; return x; }  // { dg-error "not a return-statement" "" { target { c++11_only } } }

constexpr int Test3(int x) { typedef enum E { y = 1 } EE; return x; }  // { dg-error "not a return-statement" "" { target { c++11_only } } }

constexpr int Test4(int x) { typedef struct T { } TT; return x; }  // { dg-error "not a return-statement" "" { target { c++11_only } } }

constexpr int Test5(int x) { using EE = enum E { y = 1 }; return x; }  // { dg-error "not a return-statement" "" { target { c++11_only } } }

constexpr int Test6(int x) { using TT = struct T { }; return x; }  // { dg-error "not a return-statement" "" { target { c++11_only } } }

struct S1
{
  constexpr S1() { enum E { y = 1 }; }  // { dg-error "does not have empty body" "" { target { c++11_only } } }
};

struct S2
{
  constexpr S2() { struct T { }; }  // { dg-error "does not have empty body" "" { target { c++11_only } } }
};

struct S3
{
  constexpr S3() { typedef enum E { y = 1 } EE; }  // { dg-error "does not have empty body" "" { target { c++11_only } } }
};

struct S4
{
  constexpr S4() { typedef struct T { } TT; }  // { dg-error "does not have empty body" "" { target { c++11_only } } }
};

struct S5
{
  constexpr S5() { using EE = enum E { y = 1 }; }  // { dg-error "does not have empty body" "" { target { c++11_only } } }
};

struct S6
{
  constexpr S6() { using TT = struct T { }; }  // { dg-error "does not have empty body" "" { target { c++11_only } } }
};
