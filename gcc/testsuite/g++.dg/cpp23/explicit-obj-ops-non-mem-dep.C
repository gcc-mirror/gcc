// P0847R7
// { dg-do compile { target c++23 } }

// operators that are not required to be members
// called in a dependent context (as non dependent exprs)
// see header
#include "explicit-obj-ops-non-mem.h"

// noop, indicates which versions are ill-formed
// I could not find a way to test the invalid cases
// without requires expressions
#define TEST_INVALID(X)

template<typename T = void>
void do_calls()
{
  Value value{};
  TEST_OPS(value)
  TEST_OPS(static_cast<Value&&>(value))
  TEST_OPS(static_cast<Value const&>(value))
  TEST_OPS(static_cast<Value const&&>(value))
  
  LRef l_ref{};
  TEST_OPS(l_ref)
  TEST_INVALID(static_cast<LRef&&>(l_ref))
  TEST_INVALID(static_cast<LRef const&>(l_ref))
  TEST_INVALID(static_cast<LRef const&&>(l_ref))

  RRef r_ref{};
  TEST_INVALID(r_ref)
  TEST_OPS(static_cast<RRef&&>(r_ref))
  TEST_INVALID(static_cast<RRef const&>(r_ref))
  TEST_INVALID(static_cast<RRef const&&>(r_ref))

  ConstLRef const_l_ref{};
  TEST_OPS(const_l_ref)
  TEST_OPS(static_cast<ConstLRef&&>(const_l_ref))
  TEST_OPS(static_cast<ConstLRef const&>(const_l_ref))
  TEST_OPS(static_cast<ConstLRef const&&>(const_l_ref))

  ConstRRef const_r_ref{};
  TEST_INVALID(const_r_ref)
  TEST_OPS(static_cast<ConstRRef&&>(const_r_ref))
  TEST_INVALID(static_cast<ConstRRef const&>(const_r_ref))
  TEST_OPS(static_cast<ConstRRef const&&>(const_r_ref))

  Deduced deduced{};
  TEST_OPS(deduced)
  TEST_OPS(static_cast<Deduced&&>(deduced))
  TEST_OPS(static_cast<Deduced const&>(deduced))
  TEST_OPS(static_cast<Deduced const&&>(deduced))

  VALIDATE_RETURN_TYPES(deduced, Deduced&)
  VALIDATE_RETURN_TYPES(static_cast<Deduced&&>(deduced), Deduced&&)
  VALIDATE_RETURN_TYPES(static_cast<Deduced const&>(deduced), Deduced const&)
  VALIDATE_RETURN_TYPES(static_cast<Deduced const&&>(deduced), Deduced const&&)
}

