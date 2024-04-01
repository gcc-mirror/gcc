// { dg-do compile { target c++11 } }
// { dg-options "" }

#define SA(X) static_assert((X),#X)

#define SA_TEST_CATEGORY(TRAIT, TYPE, EXPECT)	\
  SA(TRAIT(TYPE) == EXPECT);			\
  SA(TRAIT(const TYPE) == EXPECT);		\
  SA(TRAIT(volatile TYPE) == EXPECT);		\
  SA(TRAIT(const volatile TYPE) == EXPECT)

class ClassType { };

constexpr int sz0 = 0;
constexpr int sz2 = 2;

SA_TEST_CATEGORY(__is_array, int[2], true);
SA_TEST_CATEGORY(__is_array, int[], true);
SA_TEST_CATEGORY(__is_array, int[0], false);
SA_TEST_CATEGORY(__is_array, int[2][3], true);
SA_TEST_CATEGORY(__is_array, int[][3], true);
SA_TEST_CATEGORY(__is_array, int[0][3], false);
SA_TEST_CATEGORY(__is_array, int[3][0], false);
SA_TEST_CATEGORY(__is_array, float*[2], true);
SA_TEST_CATEGORY(__is_array, float*[], true);
SA_TEST_CATEGORY(__is_array, float*[2][3], true);
SA_TEST_CATEGORY(__is_array, float*[][3], true);
SA_TEST_CATEGORY(__is_array, ClassType[2], true);
SA_TEST_CATEGORY(__is_array, ClassType[], true);
SA_TEST_CATEGORY(__is_array, ClassType[0], false);
SA_TEST_CATEGORY(__is_array, ClassType[2][3], true);
SA_TEST_CATEGORY(__is_array, ClassType[][3], true);
SA_TEST_CATEGORY(__is_array, ClassType[0][3], false);
SA_TEST_CATEGORY(__is_array, ClassType[2][0], false);
SA_TEST_CATEGORY(__is_array, int[sz2], true);
SA_TEST_CATEGORY(__is_array, int[sz0], false);

// Sanity check.
SA_TEST_CATEGORY(__is_array, ClassType, false);
