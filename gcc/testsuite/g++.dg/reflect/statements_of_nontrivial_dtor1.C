// { dg-do compile { target c++26 } }
// { dg-additional-options "-freflection" }
// Test std::meta::statements_of and return_value_of on bodies that use a
// class with a non-trivial destructor.

#include <meta>
#include <type_traits>

using namespace std::meta;

void destroyed (double);

struct T { double v; };
struct D { double v; ~D () { destroyed (v); } };

static_assert (std::is_trivially_destructible_v<T>);
static_assert (!std::is_trivially_destructible_v<D>);

T make_t (double x) { return {x}; }
D make_d (double x) { return {x}; }

double t_local_then_return (double x) { T a = make_t (x); return a.v; }
T t_return_call (double x) { return make_t (x); }
double d_local_then_return (double x) { D a = make_d (x); return a.v; }
D d_return_call (double x) { return make_d (x); }

consteval info
return_of (info fn)
{
  info body = body_of (fn);
  if (is_return_statement (body))
    return body;
  for (info s : statements_of (body))
    if (is_return_statement (s))
      return s;
  throw "no return statement";
}

consteval bool
returns_call_to (info fn, info callee)
{
  info v = return_value_of (return_of (fn));
  return is_function_call (v) && callee_of (v) == callee;
}

// The local's destructor doesn't hide the return.
static_assert (is_return_statement (return_of (^^t_local_then_return)));
static_assert (is_return_statement (return_of (^^d_local_then_return)));
static_assert (statements_of (body_of (^^d_local_then_return)).size ()
	       == statements_of (body_of (^^t_local_then_return)).size ());

// `return f (x);` returns a call, whether or not the class is trivial.
static_assert (returns_call_to (^^t_return_call, ^^make_t));
static_assert (returns_call_to (^^d_return_call, ^^make_d));
static_assert (operands_of (return_value_of (return_of (^^d_return_call)))
	       .size () == 2);
