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

SA_TEST_FN(__is_object, int (int), false);
SA_TEST_FN(__is_object, ClassType (ClassType), false);
SA_TEST_FN(__is_object, float (int, float, int[], int&), false);
SA_TEST_CATEGORY(__is_object, int&, false);
SA_TEST_CATEGORY(__is_object, ClassType&, false);
SA_TEST_FN(__is_object, int(&)(int), false);
SA_TEST_CATEGORY(__is_object, void, false);

// Sanity check.
SA_TEST_CATEGORY(__is_object, ClassType, true);
