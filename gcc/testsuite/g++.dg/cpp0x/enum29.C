// PR c++/51312
// { dg-do compile { target c++11 } }

struct X0
{
  constexpr operator int() const { return 1; }
};

struct X1
{
  enum RE1 { re1 = 1 };
  constexpr operator RE1() const { return re1; }
};

struct X2
{
  constexpr operator int() const { return __INT_MAX__; }
};

struct X3
{
  enum RE3 { re3 = __INT_MAX__ };
  constexpr operator RE3() const { return re3; }
};

struct X4
{
  constexpr operator double() const { return 1.0; }
};

struct X5
{
  constexpr operator int() const { return __INT_MAX__; }
  constexpr operator unsigned() const { return __INT_MAX__ * 2U + 1; }
};

enum E0 { e0 = X0() };
enum E1 { e1 = X1() };
enum E2 { e2 = X2() };
enum E3 { e3 = X3() };
enum E4 { e4 = X4() };  // { dg-error "integral" }
enum E5 { e5 = X5() };  // { dg-error "ambiguous" }

enum F0 : int { f0 = X0() };
enum F1 : int { f1 = X1() };
enum F2 : int { f2 = X2() };
enum F3 : int { f3 = X3() };
enum F4 : int { f4 = X4() };  // { dg-error "narrowing" }
enum F5 : int { f5 = X5() };

enum G0 : signed char { g0 = X0() };
enum G1 : signed char { g1 = X1() };
enum G2 : signed char { g2 = X2() };  // { dg-error "narrowing" }
enum G3 : signed char { g3 = X3() };  // { dg-error "narrowing" }
enum G4 : signed char { g4 = X4() };  // { dg-error "narrowing" }
enum G5 : signed char { g5 = X5() };  // { dg-error "ambiguous" }
