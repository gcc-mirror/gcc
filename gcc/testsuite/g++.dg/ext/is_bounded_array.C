// { dg-do compile { target c++11 } }

#include <testsuite_tr1.h>

using namespace __gnu_test;

#define SA(X) static_assert((X),#X)

#define SA_TEST_CONST(TRAIT, TYPE, EXPECT)	\
  SA(TRAIT(TYPE) == EXPECT);			\
  SA(TRAIT(const TYPE) == EXPECT)

#define SA_TEST_CATEGORY(TRAIT, TYPE, EXPECT)	\
  SA(TRAIT(TYPE) == EXPECT);					\
  SA(TRAIT(const TYPE) == EXPECT);				\
  SA(TRAIT(volatile TYPE) == EXPECT);			\
  SA(TRAIT(const volatile TYPE) == EXPECT)

SA_TEST_CATEGORY(__is_bounded_array, int[2], true);
SA_TEST_CATEGORY(__is_bounded_array, int[], false);
SA_TEST_CATEGORY(__is_bounded_array, int[2][3], true);
SA_TEST_CATEGORY(__is_bounded_array, int[][3], false);
SA_TEST_CATEGORY(__is_bounded_array, float*[2], true);
SA_TEST_CATEGORY(__is_bounded_array, float*[], false);
SA_TEST_CATEGORY(__is_bounded_array, float*[2][3], true);
SA_TEST_CATEGORY(__is_bounded_array, float*[][3], false);
SA_TEST_CATEGORY(__is_bounded_array, ClassType[2], true);
SA_TEST_CATEGORY(__is_bounded_array, ClassType[], false);
SA_TEST_CATEGORY(__is_bounded_array, ClassType[2][3], true);
SA_TEST_CATEGORY(__is_bounded_array, ClassType[][3], false);
SA_TEST_CATEGORY(__is_bounded_array, int(*)[2], false);
SA_TEST_CATEGORY(__is_bounded_array, int(*)[], false);
SA_TEST_CATEGORY(__is_bounded_array, int(&)[2], false);
SA_TEST_CONST(__is_bounded_array, int(&)[], false);

// Sanity check.
SA_TEST_CATEGORY(__is_bounded_array, ClassType, false);
SA_TEST_CONST(__is_bounded_array, void(), false);
