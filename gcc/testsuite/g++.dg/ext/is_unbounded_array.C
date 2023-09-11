// { dg-do compile { target c++11 } }

#define SA(X) static_assert((X),#X)

#define SA_TEST_CATEGORY(TRAIT, TYPE, EXPECT)	\
  SA(TRAIT(TYPE) == EXPECT);					\
  SA(TRAIT(const TYPE) == EXPECT);				\
  SA(TRAIT(volatile TYPE) == EXPECT);			\
  SA(TRAIT(const volatile TYPE) == EXPECT)

class ClassType { };
class IncompleteClass;
union IncompleteUnion;

SA_TEST_CATEGORY(__is_unbounded_array, int[2], false);
SA_TEST_CATEGORY(__is_unbounded_array, int[], true);
SA_TEST_CATEGORY(__is_unbounded_array, int[2][3], false);
SA_TEST_CATEGORY(__is_unbounded_array, int[][3], true);
SA_TEST_CATEGORY(__is_unbounded_array, float*[2], false);
SA_TEST_CATEGORY(__is_unbounded_array, float*[], true);
SA_TEST_CATEGORY(__is_unbounded_array, float*[2][3], false);
SA_TEST_CATEGORY(__is_unbounded_array, float*[][3], true);
SA_TEST_CATEGORY(__is_unbounded_array, ClassType[2], false);
SA_TEST_CATEGORY(__is_unbounded_array, ClassType[], true);
SA_TEST_CATEGORY(__is_unbounded_array, ClassType[2][3], false);
SA_TEST_CATEGORY(__is_unbounded_array, ClassType[][3], true);
SA_TEST_CATEGORY(__is_unbounded_array, IncompleteClass[2][3], false);
SA_TEST_CATEGORY(__is_unbounded_array, IncompleteClass[][3], true);
SA_TEST_CATEGORY(__is_unbounded_array, int(*)[2], false);
SA_TEST_CATEGORY(__is_unbounded_array, int(*)[], false);
SA_TEST_CATEGORY(__is_unbounded_array, int(&)[2], false);
SA_TEST_CATEGORY(__is_unbounded_array, int(&)[], false);

// Sanity check.
SA_TEST_CATEGORY(__is_unbounded_array, ClassType, false);
SA_TEST_CATEGORY(__is_unbounded_array, IncompleteClass, false);
SA_TEST_CATEGORY(__is_unbounded_array, IncompleteUnion, false);
