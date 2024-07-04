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

// Positive tests.
SA_TEST_CATEGORY(__is_member_object_pointer, int (ClassType::*), true);
SA_TEST_CATEGORY(__is_member_object_pointer, ClassType (ClassType::*), true);

// Negative tests.
SA_TEST_FN(__is_member_object_pointer, int (ClassType::*) (int), false);
SA_TEST_FN(__is_member_object_pointer, int (ClassType::*) (float, ...), false);
SA_TEST_FN(__is_member_object_pointer, ClassType (ClassType::*) (ClassType), false);
SA_TEST_FN(__is_member_object_pointer, float (ClassType::*) (int, float, int[], int&), false);

// Sanity check.
SA_TEST_CATEGORY(__is_member_object_pointer, ClassType, false);
