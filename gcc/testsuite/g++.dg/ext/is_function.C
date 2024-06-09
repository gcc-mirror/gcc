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

struct A
{ void fn(); };

template<typename>
struct AHolder { };

template<class T, class U>
struct AHolder<U T::*>
{ using type = U; };

// Positive tests.
SA_TEST_FN(__is_function, int (int), true);
SA_TEST_FN(__is_function, ClassType (ClassType), true);
SA_TEST_FN(__is_function, float (int, float, int[], int&), true);
SA_TEST_FN(__is_function, int (int, ...), true);
SA_TEST_FN(__is_function, bool (ClassType) const, true);
SA_TEST_FN(__is_function, AHolder<decltype(&A::fn)>::type, true);

void fn();
SA_TEST_FN(__is_function, decltype(fn), true);

// Negative tests.
SA_TEST_CATEGORY(__is_function, int, false);
SA_TEST_CATEGORY(__is_function, int*, false);
SA_TEST_CATEGORY(__is_function, int&, false);
SA_TEST_CATEGORY(__is_function, void, false);
SA_TEST_CATEGORY(__is_function, void*, false);
SA_TEST_CATEGORY(__is_function, void**, false);
SA_TEST_CATEGORY(__is_function, decltype(nullptr), false);

class AbstractClass
{
  virtual void rotate(int) = 0;
};
SA_TEST_CATEGORY(__is_function, AbstractClass, false);
SA_TEST_FN(__is_function, int(&)(int), false);
SA_TEST_FN(__is_function, int(*)(int), false);

SA_TEST_CATEGORY(__is_function, A, false);
SA_TEST_CATEGORY(__is_function, decltype(&A::fn), false);

struct FnCallOverload
{ void operator()(); };
SA_TEST_CATEGORY(__is_function, FnCallOverload, false);

// Sanity check.
class IncompleteClass;
union IncompleteUnion;
SA_TEST_CATEGORY(__is_function, ClassType, false);
SA_TEST_CATEGORY(__is_function, IncompleteClass, false);
SA_TEST_CATEGORY(__is_function, IncompleteUnion, false);
