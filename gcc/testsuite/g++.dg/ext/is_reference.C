// { dg-do compile { target c++11 } }

#include <testsuite_tr1.h>

using namespace __gnu_test;

#define SA(X) static_assert((X),#X)
#define SA_TEST_CATEGORY(TRAIT, TYPE, EXPECT)	\
  SA(TRAIT(TYPE) == EXPECT);					\
  SA(TRAIT(const TYPE) == EXPECT);				\
  SA(TRAIT(volatile TYPE) == EXPECT);			\
  SA(TRAIT(const volatile TYPE) == EXPECT)

// Positive tests.
SA_TEST_CATEGORY(__is_reference, int&, true);
SA_TEST_CATEGORY(__is_reference, ClassType&, true);
SA(__is_reference(int(&)(int)));
SA_TEST_CATEGORY(__is_reference, int&&, true);
SA_TEST_CATEGORY(__is_reference, ClassType&&, true);
SA(__is_reference(int(&&)(int)));
SA_TEST_CATEGORY(__is_reference, IncompleteClass&, true);

// Negative tests
SA_TEST_CATEGORY(__is_reference, void, false);
SA_TEST_CATEGORY(__is_reference, int*, false);
SA_TEST_CATEGORY(__is_reference, int[3], false);
SA(!__is_reference(int(int)));
SA(!__is_reference(int(*const)(int)));
SA(!__is_reference(int(*volatile)(int)));
SA(!__is_reference(int(*const volatile)(int)));

// Sanity check.
SA_TEST_CATEGORY(__is_reference, ClassType, false);
SA_TEST_CATEGORY(__is_reference, IncompleteClass, false);
