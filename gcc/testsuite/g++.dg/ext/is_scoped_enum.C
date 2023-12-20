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

enum class E { e1, e2 };
SA_TEST_CATEGORY(__is_scoped_enum, E, true);
enum class Ec : char { e1, e2 };
SA_TEST_CATEGORY(__is_scoped_enum, Ec, true);

// negative tests
enum U { u1, u2 };
SA_TEST_CATEGORY(__is_scoped_enum, U, false);
enum F : int { f1, f2 };
SA_TEST_CATEGORY(__is_scoped_enum, F, false);
struct S;
SA_TEST_CATEGORY(__is_scoped_enum, S, false);
struct S { };
SA_TEST_CATEGORY(__is_scoped_enum, S, false);

SA_TEST_CATEGORY(__is_scoped_enum, int, false);
SA_TEST_CATEGORY(__is_scoped_enum, int[], false);
SA_TEST_CATEGORY(__is_scoped_enum, int[2], false);
SA_TEST_CATEGORY(__is_scoped_enum, int[][2], false);
SA_TEST_CATEGORY(__is_scoped_enum, int[2][3], false);
SA_TEST_CATEGORY(__is_scoped_enum, int*, false);
SA_TEST_CATEGORY(__is_scoped_enum, int&, false);
SA_TEST_CATEGORY(__is_scoped_enum, int*&, false);
SA_TEST_FN(__is_scoped_enum, int(), false);
SA_TEST_FN(__is_scoped_enum, int(*)(), false);
SA_TEST_FN(__is_scoped_enum, int(&)(), false);

enum opaque_unscoped : short;
enum class opaque_scoped;
enum class opaque_scoped_with_base : long;

SA_TEST_CATEGORY(__is_scoped_enum, opaque_unscoped, false);
SA_TEST_CATEGORY(__is_scoped_enum, opaque_scoped, true);
SA_TEST_CATEGORY(__is_scoped_enum, opaque_scoped_with_base, true);

enum unscoped {
  u_is_scoped = __is_scoped_enum(unscoped),
};
SA( ! unscoped::u_is_scoped );

enum unscoped_fixed : char {
  uf_is_scoped = __is_scoped_enum(unscoped_fixed),
};
SA( ! unscoped_fixed::uf_is_scoped );

enum class scoped {
  is_scoped = __is_scoped_enum(scoped),
};
SA( (bool) scoped::is_scoped );
