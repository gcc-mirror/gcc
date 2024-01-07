// P0847R7
// { dg-do compile { target c++23 } }

// well-formed and ill-formed uses of non-member capable operators in a requires expression

#include "explicit-obj-ops-non-mem.h"

// we only need the structs from the header
#undef TEST_OPS
#undef VALIDATE_RETURN_TYPES

// It's very hard to test for incorrect successes without requires, and by extension a non dependent variable
// so for the time being, there are no non dependent tests invalid calls.

template<typename T, typename U>
concept same_as = __is_same(T, U);

#define TEST_INVALID(OPERAND, CORRECT_TYPE) \
  static_assert(!requires{ (OPERAND) += 0; }, "Unexpected success calling operator += with " #OPERAND);		\
  static_assert(!requires{ (OPERAND) -= 0; }, "Unexpected success calling operator -= with " #OPERAND);		\
  static_assert(!requires{ (OPERAND) *= 0; }, "Unexpected success calling operator *= with " #OPERAND);		\
  static_assert(!requires{ (OPERAND) /= 0; }, "Unexpected success calling operator /= with " #OPERAND);		\
  static_assert(!requires{ (OPERAND) %= 0; }, "Unexpected success calling operator %= with " #OPERAND);		\
  static_assert(!requires{ (OPERAND) &= 0; }, "Unexpected success calling operator &= with " #OPERAND);		\
  static_assert(!requires{ (OPERAND) |= 0; }, "Unexpected success calling operator |= with " #OPERAND);		\
  static_assert(!requires{ (OPERAND) ^= 0; }, "Unexpected success calling operator ^= with " #OPERAND);		\
  static_assert(!requires{ (OPERAND) <<= 0; }, "Unexpected success calling operator <<= with " #OPERAND);	\
  static_assert(!requires{ (OPERAND) >>= 0; }, "Unexpected success calling operator >>= with " #OPERAND);	\
														\
  static_assert(!requires{ ++(OPERAND); }, "Unexpected success calling operator pre++ with " #OPERAND);		\
  static_assert(!requires{ --(OPERAND); }, "Unexpected success calling operator pre-- with " #OPERAND);		\
  static_assert(!requires{ (OPERAND)++; }, "Unexpected success calling operator post++ with " #OPERAND);	\
  static_assert(!requires{ (OPERAND)--; }, "Unexpected success calling operator post-- with " #OPERAND);	\
														\
  static_assert(!requires{ +(OPERAND); }, "Unexpected success calling operator unary+ with " #OPERAND);		\
  static_assert(!requires{ -(OPERAND); }, "Unexpected success calling operator unary- with " #OPERAND);		\
  static_assert(!requires{ (OPERAND) + 0; }, "Unexpected success calling operator binary+ with " #OPERAND);	\
  static_assert(!requires{ (OPERAND) - 0; }, "Unexpected success calling operator binary- with " #OPERAND);	\
  static_assert(!requires{ (OPERAND) * 0; }, "Unexpected success calling operator binary* with " #OPERAND);	\
  static_assert(!requires{ (OPERAND) / 0; }, "Unexpected success calling operator / with " #OPERAND);		\
  static_assert(!requires{ (OPERAND) % 0; }, "Unexpected success calling operator % with " #OPERAND);		\
  static_assert(!requires{ (OPERAND) & 0; }, "Unexpected success calling operator binary& with " #OPERAND);	\
  static_assert(!requires{ (OPERAND) | 0; }, "Unexpected success calling operator | with " #OPERAND);		\
  static_assert(!requires{ (OPERAND) ^ 0; }, "Unexpected success calling operator ^ with " #OPERAND);		\
  static_assert(!requires{ (OPERAND) << 0; }, "Unexpected success calling operator << with " #OPERAND);		\
  static_assert(!requires{ (OPERAND) >> 0; }, "Unexpected success calling operator >> with " #OPERAND);		\
														\
  static_assert(!requires{ !(OPERAND); }, "Unexpected success calling operator ! with " #OPERAND);		\
  static_assert(!requires{ (OPERAND) && 0; }, "Unexpected success calling operator && with " #OPERAND);		\
  static_assert(!requires{ (OPERAND) || 0; }, "Unexpected success calling operator || with " #OPERAND);		\
														\
  static_assert(!requires{ (OPERAND) == 0; }, "Unexpected success calling operator == with " #OPERAND);		\
  static_assert(!requires{ (OPERAND) != 0; }, "Unexpected success calling operator != with " #OPERAND);		\
  static_assert(!requires{ (OPERAND) < 0; }, "Unexpected success calling operator < with " #OPERAND);		\
  static_assert(!requires{ (OPERAND) > 0; }, "Unexpected success calling operator > with " #OPERAND);		\
  static_assert(!requires{ (OPERAND) <= 0; }, "Unexpected success calling operator <= with " #OPERAND);		\
  static_assert(!requires{ (OPERAND) >= 0; }, "Unexpected success calling operator >= with " #OPERAND);		\
  static_assert(!requires{ (OPERAND) <=> 0; }, "Unexpected success calling operator <=> with " #OPERAND);	\
														\
  static_assert(!requires{ *(OPERAND); }, "Unexpected success calling operator unary* with " #OPERAND);		\
  static_assert(!requires{ (OPERAND) ->* 0; }, "Unexpected success calling operator ->* with " #OPERAND);	\
  /* We need to check the return type to confirm the built-in operator was not selected.  */			\
  static_assert(!requires{ {&(OPERAND)} -> same_as<CORRECT_TYPE>; },						\
		"Unexpected success calling operator unary& with " #OPERAND);					\
  static_assert(!requires{ {(OPERAND), 0} -> same_as<CORRECT_TYPE>; },						\
		"Unexpected success calling operator , with " #OPERAND);

#define TEST_VALID(OPERAND, CORRECT_TYPE) \
  static_assert(requires{ (OPERAND) += 0; }, "Unexpected failure calling operator += with " #OPERAND);		\
  static_assert(requires{ (OPERAND) -= 0; }, "Unexpected failure calling operator -= with " #OPERAND);		\
  static_assert(requires{ (OPERAND) *= 0; }, "Unexpected failure calling operator *= with " #OPERAND);		\
  static_assert(requires{ (OPERAND) /= 0; }, "Unexpected failure calling operator /= with " #OPERAND);		\
  static_assert(requires{ (OPERAND) %= 0; }, "Unexpected failure calling operator %= with " #OPERAND);		\
  static_assert(requires{ (OPERAND) &= 0; }, "Unexpected failure calling operator &= with " #OPERAND);		\
  static_assert(requires{ (OPERAND) |= 0; }, "Unexpected failure calling operator |= with " #OPERAND);		\
  static_assert(requires{ (OPERAND) ^= 0; }, "Unexpected failure calling operator ^= with " #OPERAND);		\
  static_assert(requires{ (OPERAND) <<= 0; }, "Unexpected failure calling operator <<= with " #OPERAND);	\
  static_assert(requires{ (OPERAND) >>= 0; }, "Unexpected failure calling operator >>= with " #OPERAND);	\
														\
  static_assert(requires{ ++(OPERAND); }, "Unexpected failure calling operator pre++ with " #OPERAND);		\
  static_assert(requires{ --(OPERAND); }, "Unexpected failure calling operator pre-- with " #OPERAND);		\
  static_assert(requires{ (OPERAND)++; }, "Unexpected failure calling operator post++ with " #OPERAND);		\
  static_assert(requires{ (OPERAND)--; }, "Unexpected failure calling operator post-- with " #OPERAND);		\
														\
  static_assert(requires{ +(OPERAND); }, "Unexpected failure calling operator unary+ with " #OPERAND);		\
  static_assert(requires{ -(OPERAND); }, "Unexpected failure calling operator unary- with " #OPERAND);		\
  static_assert(requires{ (OPERAND) + 0; }, "Unexpected failure calling operator binary+ with " #OPERAND);	\
  static_assert(requires{ (OPERAND) - 0; }, "Unexpected failure calling operator binary- with " #OPERAND);	\
  static_assert(requires{ (OPERAND) * 0; }, "Unexpected failure calling operator binary* with " #OPERAND);	\
  static_assert(requires{ (OPERAND) / 0; }, "Unexpected failure calling operator / with " #OPERAND);		\
  static_assert(requires{ (OPERAND) % 0; }, "Unexpected failure calling operator % with " #OPERAND);		\
  static_assert(requires{ (OPERAND) & 0; }, "Unexpected failure calling operator binary& with " #OPERAND);	\
  static_assert(requires{ (OPERAND) | 0; }, "Unexpected failure calling operator | with " #OPERAND);		\
  static_assert(requires{ (OPERAND) ^ 0; }, "Unexpected failure calling operator ^ with " #OPERAND);		\
  static_assert(requires{ (OPERAND) << 0; }, "Unexpected failure calling operator << with " #OPERAND);		\
  static_assert(requires{ (OPERAND) >> 0; }, "Unexpected failure calling operator >> with " #OPERAND);		\
														\
  static_assert(requires{ !(OPERAND); }, "Unexpected failure calling operator ! with " #OPERAND);		\
  static_assert(requires{ (OPERAND) && 0; }, "Unexpected failure calling operator && with " #OPERAND);		\
  static_assert(requires{ (OPERAND) || 0; }, "Unexpected failure calling operator || with " #OPERAND);		\
														\
  static_assert(requires{ (OPERAND) == 0; }, "Unexpected failure calling operator == with " #OPERAND);		\
  static_assert(requires{ (OPERAND) != 0; }, "Unexpected failure calling operator != with " #OPERAND);		\
  static_assert(requires{ (OPERAND) < 0; }, "Unexpected failure calling operator < with " #OPERAND);		\
  static_assert(requires{ (OPERAND) > 0; }, "Unexpected failure calling operator > with " #OPERAND);		\
  static_assert(requires{ (OPERAND) <= 0; }, "Unexpected failure calling operator <= with " #OPERAND);		\
  static_assert(requires{ (OPERAND) >= 0; }, "Unexpected failure calling operator >= with " #OPERAND);		\
  static_assert(requires{ (OPERAND) <=> 0; }, "Unexpected failure calling operator <=> with " #OPERAND);	\
														\
  static_assert(requires{ *(OPERAND); }, "Unexpected failure calling operator unary* with " #OPERAND);		\
  static_assert(requires{ (OPERAND) ->* 0; }, "Unexpected failure calling operator ->* with " #OPERAND);	\
  /* We need to check the return type to confirm we selected our overload, not the built-in operator.  */	\
  static_assert(requires{ {&(OPERAND)} -> same_as<CORRECT_TYPE>; },						\
		"Unexpected failure calling operator unary& with " #OPERAND);					\
  static_assert(requires{ {(OPERAND), 0} -> same_as<CORRECT_TYPE>; },						\
		"Unexpected failure calling operator , with " #OPERAND);

// Return types need to be tested for the deduced case

#define TEST_VALID_WITH_RETURN_TYPES(OPERAND, CORRECT_TYPE) \
  static_assert(requires{ {(OPERAND) += 0} -> same_as<CORRECT_TYPE>; });	\
  static_assert(requires{ {(OPERAND) -= 0} -> same_as<CORRECT_TYPE>; });	\
  static_assert(requires{ {(OPERAND) *= 0} -> same_as<CORRECT_TYPE>; });	\
  static_assert(requires{ {(OPERAND) /= 0} -> same_as<CORRECT_TYPE>; });	\
  static_assert(requires{ {(OPERAND) %= 0} -> same_as<CORRECT_TYPE>; });	\
  static_assert(requires{ {(OPERAND) &= 0} -> same_as<CORRECT_TYPE>; });	\
  static_assert(requires{ {(OPERAND) |= 0} -> same_as<CORRECT_TYPE>; });	\
  static_assert(requires{ {(OPERAND) ^= 0} -> same_as<CORRECT_TYPE>; });	\
  static_assert(requires{ {(OPERAND) <<= 0} -> same_as<CORRECT_TYPE>; });	\
  static_assert(requires{ {(OPERAND) >>= 0} -> same_as<CORRECT_TYPE>; });	\
										\
  static_assert(requires{ {++(OPERAND)} -> same_as<CORRECT_TYPE>; });		\
  static_assert(requires{ {--(OPERAND)} -> same_as<CORRECT_TYPE>; });		\
  static_assert(requires{ {(OPERAND)++} -> same_as<CORRECT_TYPE>; });		\
  static_assert(requires{ {(OPERAND)--} -> same_as<CORRECT_TYPE>; });		\
										\
  static_assert(requires{ {+(OPERAND)} -> same_as<CORRECT_TYPE>; });		\
  static_assert(requires{ {-(OPERAND)} -> same_as<CORRECT_TYPE>; });		\
  static_assert(requires{ {(OPERAND) + 0} -> same_as<CORRECT_TYPE>; });		\
  static_assert(requires{ {(OPERAND) - 0} -> same_as<CORRECT_TYPE>; });		\
  static_assert(requires{ {(OPERAND) * 0} -> same_as<CORRECT_TYPE>; });		\
  static_assert(requires{ {(OPERAND) / 0} -> same_as<CORRECT_TYPE>; });		\
  static_assert(requires{ {(OPERAND) % 0} -> same_as<CORRECT_TYPE>; });		\
  static_assert(requires{ {(OPERAND) & 0} -> same_as<CORRECT_TYPE>; });		\
  static_assert(requires{ {(OPERAND) | 0} -> same_as<CORRECT_TYPE>; });		\
  static_assert(requires{ {(OPERAND) ^ 0} -> same_as<CORRECT_TYPE>; });		\
  static_assert(requires{ {(OPERAND) << 0} -> same_as<CORRECT_TYPE>; });	\
  static_assert(requires{ {(OPERAND) >> 0} -> same_as<CORRECT_TYPE>; });	\
										\
  static_assert(requires{ {!(OPERAND)} -> same_as<CORRECT_TYPE>; });		\
  static_assert(requires{ {(OPERAND) && 0} -> same_as<CORRECT_TYPE>; });	\
  static_assert(requires{ {(OPERAND) || 0} -> same_as<CORRECT_TYPE>; });	\
										\
  static_assert(requires{ {(OPERAND) == 0} -> same_as<CORRECT_TYPE>; });	\
  static_assert(requires{ {(OPERAND) != 0} -> same_as<CORRECT_TYPE>; });	\
  static_assert(requires{ {(OPERAND) < 0} -> same_as<CORRECT_TYPE>; });		\
  static_assert(requires{ {(OPERAND) > 0} -> same_as<CORRECT_TYPE>; });		\
  static_assert(requires{ {(OPERAND) <= 0} -> same_as<CORRECT_TYPE>; });	\
  static_assert(requires{ {(OPERAND) >= 0} -> same_as<CORRECT_TYPE>; });	\
  static_assert(requires{ {(OPERAND) <=> 0} -> same_as<CORRECT_TYPE>; });	\
										\
  static_assert(requires{ {*(OPERAND)} -> same_as<CORRECT_TYPE>; });		\
  static_assert(requires{ {(OPERAND) ->* 0} -> same_as<CORRECT_TYPE>; });	\
  static_assert(requires{ {&(OPERAND)} -> same_as<CORRECT_TYPE>; });		\
  static_assert(requires{ {(OPERAND), 0} -> same_as<CORRECT_TYPE>; });

template<typename DepValue = Value>
void test_value()
{
  DepValue value{};
  TEST_VALID(value, DepValue)
  TEST_VALID(static_cast<DepValue&&>(value), DepValue)
  TEST_VALID(static_cast<DepValue const&>(value), DepValue)
  TEST_VALID(static_cast<DepValue const&&>(value), DepValue)
}

template<typename DepLRef = LRef>
void test_l_ref()
{
  DepLRef l_ref{};
  TEST_VALID(l_ref, DepLRef&)
  TEST_INVALID(static_cast<DepLRef&&>(l_ref), DepLRef&)
  TEST_INVALID(static_cast<DepLRef const&>(l_ref), DepLRef&)
  TEST_INVALID(static_cast<DepLRef const&&>(l_ref), DepLRef&)
}

template<typename DepRRef = RRef>
void test_r_ref()
{
  DepRRef r_ref{};
  TEST_INVALID(r_ref, DepRRef&&)
  TEST_VALID(static_cast<DepRRef&&>(r_ref), DepRRef&&)
  TEST_INVALID(static_cast<DepRRef const&>(r_ref), DepRRef&&)
  TEST_INVALID(static_cast<DepRRef const&&>(r_ref), DepRRef&&)
}

template<typename DepConstLRef = ConstLRef>
void test_const_l_ref()
{
  DepConstLRef const_l_ref{};
  TEST_VALID(const_l_ref, DepConstLRef const&)
  TEST_VALID(static_cast<DepConstLRef&&>(const_l_ref), DepConstLRef const&)
  TEST_VALID(static_cast<DepConstLRef const&>(const_l_ref), DepConstLRef const&)
  TEST_VALID(static_cast<DepConstLRef const&&>(const_l_ref), DepConstLRef const&)
}

template<typename DepConstRRef = ConstRRef>
void test_const_r_ref()
{
  DepConstRRef const_r_ref{};
  TEST_INVALID(const_r_ref, DepConstRRef const&&)
  TEST_VALID(static_cast<DepConstRRef&&>(const_r_ref), DepConstRRef const&&)
  TEST_INVALID(static_cast<DepConstRRef const&>(const_r_ref), DepConstRRef const&&)
  TEST_VALID(static_cast<DepConstRRef const&&>(const_r_ref), DepConstRRef const&&)
}

template<typename DepDeduced = Deduced>
void test_deduced()
{
  DepDeduced deduced{};

  TEST_VALID_WITH_RETURN_TYPES(deduced, DepDeduced&)
  TEST_VALID_WITH_RETURN_TYPES(static_cast<DepDeduced&&>(deduced), DepDeduced&&)
  TEST_VALID_WITH_RETURN_TYPES(static_cast<DepDeduced const&>(deduced), DepDeduced const&)
  TEST_VALID_WITH_RETURN_TYPES(static_cast<DepDeduced const&&>(deduced), DepDeduced const&&)
}

void test()
{
  test_value();
  test_l_ref();
  test_r_ref();
  test_const_l_ref();
  test_const_r_ref();
  test_deduced();
}

