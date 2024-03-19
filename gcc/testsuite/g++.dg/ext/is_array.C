// { dg-do compile { target c++11 } }

#include <testsuite_tr1.h>

using namespace __gnu_test;

#define SA(X) static_assert((X),#X)
#define SA_TEST_CATEGORY(TRAIT, X, expect) \
  SA(TRAIT(X) == expect);                  \
  SA(TRAIT(const X) == expect);            \
  SA(TRAIT(volatile X) == expect);         \
  SA(TRAIT(const volatile X) == expect)

SA_TEST_CATEGORY(__is_array, int[2], true);
SA_TEST_CATEGORY(__is_array, int[], true);
SA_TEST_CATEGORY(__is_array, int[2][3], true);
SA_TEST_CATEGORY(__is_array, int[][3], true);
SA_TEST_CATEGORY(__is_array, float*[2], true);
SA_TEST_CATEGORY(__is_array, float*[], true);
SA_TEST_CATEGORY(__is_array, float*[2][3], true);
SA_TEST_CATEGORY(__is_array, float*[][3], true);
SA_TEST_CATEGORY(__is_array, ClassType[2], true);
SA_TEST_CATEGORY(__is_array, ClassType[], true);
SA_TEST_CATEGORY(__is_array, ClassType[2][3], true);
SA_TEST_CATEGORY(__is_array, ClassType[][3], true);

// Sanity check.
SA_TEST_CATEGORY(__is_array, ClassType, false);
