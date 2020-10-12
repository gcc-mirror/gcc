// PR c++/96223
// { dg-do compile { target c++20 } }
// DR 1787 (if an indeterminate value is produced by an evaluation, the
// behavior is undefined except in certain cases)
// Note that P1331R2 explicitly disallows in a constant evaluation:
// - an lvalue-to-rvalue conversion that is applied to an object with
// indeterminate value ([basic.indet]).

#include <cstddef>

constexpr int
fn1 ()
{
  unsigned char foo;
  unsigned char u = foo; // { dg-error "not usable in a constant expression" }
  return 0;
}

constexpr int
fn2 ()
{
  unsigned char foo;
  int i = foo; // { dg-error "not usable in a constant expression" }
  return 0;
}

constexpr int
fn3 ()
{
  unsigned char foo;
  char8_t u = foo; // { dg-error "not usable in a constant expression" }
  return 0;
}

constexpr int
fn4 ()
{
  std::byte foo;
  std::byte b = foo; // { dg-error "not usable in a constant expression" }
  return 0;
}

constexpr int w1 = fn1 ();
constexpr int w2 = fn2 ();
constexpr int w3 = fn3 ();
constexpr int w4 = fn4 ();
