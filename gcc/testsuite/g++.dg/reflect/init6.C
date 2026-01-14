// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }

using info = decltype(^^int);

struct S {
  info i;
};

struct N {
  info i = ^^void;
};

S s1;  // { dg-error "consteval-only variable" }
constinit S s2{};
constexpr S s3{^^int};

N n1;  // { dg-error "consteval-only variable" }
constinit N n2;
constexpr N n3;

template<typename T>
struct X {
  T t;
};

X<info> x1;  // { dg-error "consteval-only variable" }
constinit X<info> x2{};
constexpr X<info> x3{^^int};

void
g ()
{
  static constexpr S s{^^void};
  static constexpr X<info> x{^^void};
  static constexpr N n;
}
