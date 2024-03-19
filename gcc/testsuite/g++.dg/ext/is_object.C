// { dg-do compile { target c++11 } }

#include <testsuite_tr1.h>

using namespace __gnu_test;

#define SA(X) static_assert((X),#X)

#define SA_TEST_NON_VOLATILE(TRAIT, TYPE, EXPECT)	\
  SA(TRAIT(TYPE) == EXPECT);				\
  SA(TRAIT(const TYPE) == EXPECT)

#define SA_TEST_CATEGORY(TRAIT, TYPE, EXPECT)	\
  SA(TRAIT(TYPE) == EXPECT);			\
  SA(TRAIT(const TYPE) == EXPECT);		\
  SA(TRAIT(volatile TYPE) == EXPECT);		\
  SA(TRAIT(const volatile TYPE) == EXPECT)

SA_TEST_NON_VOLATILE(__is_object, int (int), false);
SA_TEST_NON_VOLATILE(__is_object, ClassType (ClassType), false);
SA_TEST_NON_VOLATILE(__is_object,
		     float (int, float, int[], int&), false);
SA_TEST_CATEGORY(__is_object, int&, false);
SA_TEST_CATEGORY(__is_object, ClassType&, false);
SA_TEST_NON_VOLATILE(__is_object, int(&)(int), false);
SA_TEST_CATEGORY(__is_object, void, false);

// Sanity check.
SA_TEST_CATEGORY(__is_object, ClassType, true);
