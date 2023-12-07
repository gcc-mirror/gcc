// { dg-do compile { target c++11 } }

#include <testsuite_tr1.h>

using namespace __gnu_test;

#define SA(X) static_assert((X),#X)
#define SA_TEST_CATEGORY(TRAIT, TYPE, EXPECT)	\
  SA(TRAIT(TYPE) == EXPECT);					\
  SA(TRAIT(const TYPE) == EXPECT);				\
  SA(TRAIT(volatile TYPE) == EXPECT);			\
  SA(TRAIT(const volatile TYPE) == EXPECT)

struct A
{ void fn(); };

template<typename>
struct AHolder { };

template<class T, class U>
struct AHolder<U T::*>
{ using type = U; };

// Positive tests.
SA(__is_function(int (int)));
SA(__is_function(ClassType (ClassType)));
SA(__is_function(float (int, float, int[], int&)));
SA(__is_function(int (int, ...)));
SA(__is_function(bool (ClassType) const));
SA(__is_function(AHolder<decltype(&A::fn)>::type));

void fn();
SA(__is_function(decltype(fn)));

// Negative tests.
SA_TEST_CATEGORY(__is_function, int, false);
SA_TEST_CATEGORY(__is_function, int*, false);
SA_TEST_CATEGORY(__is_function, int&, false);
SA_TEST_CATEGORY(__is_function, void, false);
SA_TEST_CATEGORY(__is_function, void*, false);
SA_TEST_CATEGORY(__is_function, void**, false);
SA_TEST_CATEGORY(__is_function, std::nullptr_t, false);

SA_TEST_CATEGORY(__is_function, AbstractClass, false);
SA(!__is_function(int(&)(int)));
SA(!__is_function(int(*)(int)));

SA_TEST_CATEGORY(__is_function, A, false);
SA_TEST_CATEGORY(__is_function, decltype(&A::fn), false);

struct FnCallOverload
{ void operator()(); };
SA_TEST_CATEGORY(__is_function, FnCallOverload, false);

// Sanity check.
SA_TEST_CATEGORY(__is_function, ClassType, false);
SA_TEST_CATEGORY(__is_function, IncompleteClass, false);
SA_TEST_CATEGORY(__is_function, IncompleteUnion, false);
