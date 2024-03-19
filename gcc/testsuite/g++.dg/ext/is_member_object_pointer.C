// { dg-do compile { target c++11 } }

#include <testsuite_tr1.h>

using namespace __gnu_test;

#define SA(X) static_assert((X),#X)

#define SA_TEST_NON_VOLATILE(TRAIT, TYPE, EXPECT)	\
  SA(TRAIT(TYPE) == EXPECT);						\
  SA(TRAIT(const TYPE) == EXPECT)

#define SA_TEST_CATEGORY(TRAIT, TYPE, EXPECT)	\
  SA(TRAIT(TYPE) == EXPECT);					\
  SA(TRAIT(const TYPE) == EXPECT);				\
  SA(TRAIT(volatile TYPE) == EXPECT);			\
  SA(TRAIT(const volatile TYPE) == EXPECT)

// Positive tests.
SA_TEST_CATEGORY(__is_member_object_pointer, int (ClassType::*), true);
SA_TEST_CATEGORY(__is_member_object_pointer, ClassType (ClassType::*), true);

// Negative tests.
SA_TEST_NON_VOLATILE(__is_member_object_pointer, int (ClassType::*) (int), false);
SA_TEST_NON_VOLATILE(__is_member_object_pointer, int (ClassType::*) (float, ...), false);
SA_TEST_NON_VOLATILE(__is_member_object_pointer, ClassType (ClassType::*) (ClassType), false);
SA_TEST_NON_VOLATILE(__is_member_object_pointer, float (ClassType::*) (int, float, int[], int&), false);

// Sanity check.
SA_TEST_CATEGORY(__is_member_object_pointer, ClassType, false);
