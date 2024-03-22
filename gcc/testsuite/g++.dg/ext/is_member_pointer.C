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

SA_TEST_CATEGORY(__is_member_pointer, int (ClassType::*), true);
SA_TEST_CATEGORY(__is_member_pointer, ClassType (ClassType::*), true);

SA_TEST_FN(__is_member_pointer, int (ClassType::*)(int), true);
SA_TEST_FN(__is_member_pointer, int (ClassType::*)(int) const, true);
SA_TEST_FN(__is_member_pointer, int (ClassType::*)(float, ...), true);
SA_TEST_FN(__is_member_pointer, ClassType (ClassType::*)(ClassType), true);
SA_TEST_FN(__is_member_pointer, float (ClassType::*)(int, float, int[], int&), true);

// Sanity check.
SA_TEST_CATEGORY(__is_member_pointer, ClassType, false);
