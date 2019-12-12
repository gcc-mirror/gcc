// PR c++/91361 - P1152R4: Deprecating some uses of volatile.
// { dg-do compile { target c++17 } }

#define ACCESS_ONCE(x) (*(volatile __typeof(x) *)&(x))

struct S {
  volatile int a : 4;
  int b : 2;
};

struct T {
  int a : 4;
  int b : 2;
};

union U {
  char c;
  int i;
};

struct W {
  W();
  W(volatile W&);
  W& operator=(volatile W&) volatile;
};

volatile int // { dg-warning ".volatile.-qualified return type is deprecated" "" { target c++2a } }
fn (volatile int i) // { dg-warning ".volatile.-qualified parameter is deprecated" "" { target c++2a } }
{
  volatile int v = 10;
  int *volatile p = nullptr;

  // Pre/post ++/--.
  v++; // { dg-warning "expression of .volatile.-qualified type is deprecated" "" { target c++2a } }
  ++v; // { dg-warning "expression of .volatile.-qualified type is deprecated" "" { target c++2a } }
  v--; // { dg-warning "expression of .volatile.-qualified type is deprecated" "" { target c++2a } }
  --v; // { dg-warning "expression of .volatile.-qualified type is deprecated" "" { target c++2a } }
  p++; // { dg-warning "expression of .volatile.-qualified type is deprecated" "" { target c++2a } }
  ++p; // { dg-warning "expression of .volatile.-qualified type is deprecated" "" { target c++2a } }
  p--; // { dg-warning "expression of .volatile.-qualified type is deprecated" "" { target c++2a } }
  --p; // { dg-warning "expression of .volatile.-qualified type is deprecated" "" { target c++2a } }
  return v + i + *p;
}

void
fn2 ()
{
  volatile int vi = 42;
  int i = 24;

  // Discarded-value expression ([expr.context]).
  // The lvalue-to-rvalue conversion is applied here:
  vi;
  // ...but not here.  Otherwise we'd write to VI and then immediately read it.
  vi = 42;
  vi = i;
  vi = i = 42;
  i = vi = 42; // { dg-warning "assignment with .volatile.-qualified left operand is deprecated" "" { target c++2a } }
  &(vi = i); // { dg-warning "assignment with .volatile.-qualified left operand is deprecated" "" { target c++2a } }
  (vi = 42, 45);
  (i = vi = 42, 10); // { dg-warning "assignment with .volatile.-qualified left operand is deprecated" "" { target c++2a } }
  i = vi; // LHS not volatile.
  i = (vi = i, 42);
  static_cast<void>(vi = i);
  static_cast<void>(i = vi = 42); // { dg-warning "assignment with .volatile.-qualified left operand is deprecated" "" { target c++2a } }
  (void)(vi = i);
  (void)(i = vi = 42); // { dg-warning "assignment with .volatile.-qualified left operand is deprecated" "" { target c++2a } }

  // Unevaluated operand.
  decltype(vi = 42) x = vi;
  decltype(i = vi = 42) x3 = i;

  // Compound assignments.
  vi += i; // { dg-warning "assignment with .volatile.-qualified left operand is deprecated" "" { target c++2a } }
  vi -= i; // { dg-warning "assignment with .volatile.-qualified left operand is deprecated" "" { target c++2a } }
  vi %= i; // { dg-warning "assignment with .volatile.-qualified left operand is deprecated" "" { target c++2a } }
  vi ^= i; // { dg-warning "assignment with .volatile.-qualified left operand is deprecated" "" { target c++2a } }
  vi |= i; // { dg-warning "assignment with .volatile.-qualified left operand is deprecated" "" { target c++2a } }
  vi /= i; // { dg-warning "assignment with .volatile.-qualified left operand is deprecated" "" { target c++2a } }
  vi = vi += 42; // { dg-warning "assignment with .volatile.-qualified left operand is deprecated" "" { target c++2a } }
  vi += vi = 42; // { dg-warning "assignment with .volatile.-qualified left operand is deprecated" "" { target c++2a } }
  i *= vi;
  decltype(vi -= 42) x2 = vi; // { dg-warning "assignment with .volatile.-qualified left operand is deprecated" "" { target c++2a } }

  // Structured bindings.
  int a[] = { 10, 5 };
  const auto & [cxr, cyr] = a;
  const volatile auto & [cvxr, cvyr] = a; // { dg-warning ".volatile.-qualified structured binding is deprecated" "" { target c++2a } }
  volatile auto & [vxr, vyr] = a; // { dg-warning ".volatile.-qualified structured binding is deprecated" "" { target c++2a } }
}

void
fn3 ()
{
  volatile int i, j, k = 0;
  i = j = k; // { dg-warning "assignment with .volatile.-qualified left operand is deprecated" "" { target c++2a } }

  ACCESS_ONCE(j);

  S s;
  s.b = 1;

  volatile U u;
  u.c = 42;
  i = u.c = 42; // { dg-warning "assignment with .volatile.-qualified left operand is deprecated" "" { target c++2a } }
  u.c += 42; // { dg-warning "assignment with .volatile.-qualified left operand is deprecated" "" { target c++2a } }

  volatile T t;
  t.a = 3;
  j = t.a = 3; // { dg-warning "assignment with .volatile.-qualified left operand is deprecated" "" { target c++2a } }
  t.a += 3; // { dg-warning "assignment with .volatile.-qualified left operand is deprecated" "" { target c++2a } }

  volatile int *src = &i;
  *src; // No assignment, don't warn.
}

void
fn4 ()
{
  volatile W vw;
  W w;
  // Assignment to objects of a class is defined by the copy/move assignment
  // operator.
  vw = w;
  w = vw;
}

template<typename T>
void raccoon ()
{
  volatile T t, u;
  t = 42;
  u = t = 42; // { dg-warning "assignment with .volatile.-qualified left operand is deprecated" "" { target c++2a } }
  t &= 42; // { dg-warning "assignment with .volatile.-qualified left operand is deprecated" "" { target c++2a } }
}

void
fn5 ()
{
  raccoon<int>();
}
