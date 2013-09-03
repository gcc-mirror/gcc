// PR c++/51424
// { dg-do compile { target c++11 } }

template <class T >
struct S
{
  S() : S() {}          // { dg-error "delegates to itself" }
  S(int x) : S(x) {}    // { dg-error "delegates to itself" }
};

struct B1
{
  B1() : B1() {}        // { dg-error "delegates to itself" }
  B1(int y) : B1(y) {}  // { dg-error "delegates to itself" }
};

struct V1 : virtual B1
{
  V1() : B1() {}
  V1(int x) : B1(x) {}
};

struct B2
{
  B2() : B2() {}        // { dg-error "delegates to itself" }
  B2(int y) : B2(y) {}  // { dg-error "delegates to itself" }
};

struct V2 : virtual B2
{
  V2() : V2() {}        // { dg-error "delegates to itself" }
  V2(int x) : V2(x) {}  // { dg-error "delegates to itself" }
};

struct B3
{
  B3() {}
  B3(int y) {}
};

struct V3 : virtual B3
{
  V3() : V3() {}        // { dg-error "delegates to itself" }
  V3(int x) : V3(x) {}  // { dg-error "delegates to itself" }
};

struct CE1
{
  constexpr CE1() : CE1() {}        // { dg-error "delegates to itself" }
  constexpr CE1(int x) : CE1(x) {}  // { dg-error "delegates to itself" }
};

struct CEB2
{
  constexpr CEB2() : CEB2() {}        // { dg-error "delegates to itself" }
  constexpr CEB2(int x) : CEB2(x) {}  // { dg-error "delegates to itself" }
};

struct CE2 : CEB2
{
  constexpr CE2() : CEB2() {}
  constexpr CE2(int x) : CEB2(x) {}
};

S<int> s1;
S<int> s2(1);
