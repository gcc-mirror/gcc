// { dg-do compile { target c++11 } }

#define SA(X) static_assert((X),#X)

#define SA_TEST_FN(TRAIT, TYPE, EXPECT)		\
  SA(TRAIT(TYPE) == EXPECT);			\
  SA(TRAIT(const TYPE) == EXPECT);

#define SA_TEST_CATEGORY(TRAIT, TYPE, EXPECT)	\
  SA(TRAIT(TYPE) == EXPECT);			\
  SA(TRAIT(const TYPE) == EXPECT);		\
  SA(TRAIT(volatile TYPE) == EXPECT);		\
  SA(TRAIT(const volatile TYPE) == EXPECT)

class ClassType { };
class IncompleteClass;

// Positive tests.
SA_TEST_CATEGORY(__is_reference, int&, true);
SA_TEST_CATEGORY(__is_reference, ClassType&, true);
SA_TEST_FN(__is_reference, int(&)(int), true);
SA_TEST_CATEGORY(__is_reference, int&&, true);
SA_TEST_CATEGORY(__is_reference, ClassType&&, true);
SA_TEST_FN(__is_reference, int(&&)(int), true);
SA_TEST_CATEGORY(__is_reference, IncompleteClass&, true);

// Negative tests
SA_TEST_CATEGORY(__is_reference, void, false);
SA_TEST_CATEGORY(__is_reference, int*, false);
SA_TEST_CATEGORY(__is_reference, int[3], false);
SA_TEST_FN(__is_reference, int(int), false);
SA_TEST_FN(__is_reference, int(*const)(int), false);
SA_TEST_FN(__is_reference, int(*volatile)(int), false);
SA_TEST_FN(__is_reference, int(*const volatile)(int), false);

// Sanity check.
SA_TEST_CATEGORY(__is_reference, ClassType, false);
SA_TEST_CATEGORY(__is_reference, IncompleteClass, false);
