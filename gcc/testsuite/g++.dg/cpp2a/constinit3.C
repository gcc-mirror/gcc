// PR c++/91360 - Implement C++20 P1143R2: constinit
// { dg-do compile { target c++20 } }

constinit constinit int v1; // { dg-error "duplicate .constinit." }
constexpr constinit int v2 = 1; // { dg-error "can use at most one of the .constinit. and .constexpr. specifiers" }
constinit constexpr int v3 = 1; // { dg-error "an use at most one of the .constinit. and .constexpr. specifiers" }

extern static constinit int v4; // { dg-error "'static' specifier conflicts with 'extern'" }
extern thread_local constinit int v5;
extern constinit int v6;

constinit typedef int T; // { dg-error ".constinit. cannot appear in a typedef declaration" }

struct S2 {
  constinit int m1; // { dg-error "non-static data member .m1. declared .constinit." }
  constinit unsigned int b : 32; // { dg-error " non-static data member .b. declared .constinit." }
};

struct S3 {
  constinit S3() {} // { dg-error ".constinit. on function return type is not allowed" }
  constinit ~S3() {} // { dg-error ".constinit. on function return type is not allowed" }
};

constinit struct S4 { // { dg-error ".constinit. cannot be used for type declarations" }
};

template<constinit int I> // { dg-error "a parameter cannot be declared .constinit." }
struct X { };

int
fn1 ()
{
  // Not static storage
  constinit int a1 = 42; // { dg-error "17:.constinit. can only be applied to a variable with static or thread storage" }
  constinit int a2 = 42; // { dg-error "17:.constinit. can only be applied to a variable with static or thread storage" }
  extern constinit int e1;

  return 0;
}

constinit int // { dg-error ".constinit. on function return type is not allowed" }
fn3 ()
{
}

void
fn2 (int i, constinit int p) // { dg-error "a parameter cannot be declared .constinit." }
{
  constinit auto l = [i](){ return i; }; // { dg-error "18:.constinit. can only be applied to a variable with static or thread storage" }
}

struct B { int d; };

void
fn3 (B b)
{
  constinit auto [ a ] = b; // { dg-error ".constinit. can only be applied to a variable with static or thread storage" }
}
