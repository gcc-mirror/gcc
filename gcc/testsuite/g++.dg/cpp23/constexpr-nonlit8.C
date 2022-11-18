// PR c++/106649
// P2448 - Relaxing some constexpr restrictions
// { dg-do compile { target c++14 } }
// { dg-options "" }

// No constexpr constructors = not a literal type.
struct NonLiteral {
  NonLiteral() {}
};

// C++23: It is possible to write a constexpr function for which no
// invocation satisfies the requirements of a core constant expression.
constexpr NonLiteral
fn0 (int) // { dg-warning "invalid return type" "" { target c++20_down } }
{
  return NonLiteral{};
}

constexpr int
fn1 (NonLiteral) // { dg-warning "invalid type" "" { target c++20_down } }
{
  return 42;
}

// From P2448.
void f(int& i) {
    i = 0;
}

constexpr void g(int& i) {
    f(i); // { dg-warning "call to" "" { target c++20_down } }
}

// [dcl.constexpr] used to have this.
constexpr int f(bool b)
  { return b ? throw 0 : 0; }           // OK
constexpr int f() { return f(true); }   // ill-formed, no diagnostic required

struct B {
  constexpr B(int) : i(0) { }
  int i;
};

int global;

struct D : B {
  constexpr D() : B(global) { } // { dg-warning "not usable" "" { target c++20_down } }
  // ill-formed, no diagnostic required
  // lvalue-to-rvalue conversion on non-constant global
};

// If no specialization of the template would satisfy the requirements
// for a constexpr function when considered as a non-template function,
// the template is ill-formed, no diagnostic required.
template<typename>
constexpr void
fn2 ()
{
  int i = 42;
  f (i);
}

void
fn3 ()
{
  fn2<int>();
}

constexpr volatile int cvi = 10;

constexpr int
fn4 ()
{
  return cvi;  // { dg-warning "lvalue-to-rvalue conversion" "" { target c++20_down } }
}

constexpr unsigned int
fn5 (int *p)
{
  unsigned int *q = reinterpret_cast<unsigned int *>(p); // { dg-warning "reinterpret_cast" "" { target c++20_down } }
  return *q;
}

constexpr int
fn6 (int i)
{
  void *p = (void *) 1LL; // { dg-warning ".reinterpret_cast. from integer to pointer" "" { target c++20_down } }
  return 42;
}

constexpr int
fn7 (int i)
{
  static int s = i; // { dg-error "static" "" { target c++20_down } }
  return s;
}
