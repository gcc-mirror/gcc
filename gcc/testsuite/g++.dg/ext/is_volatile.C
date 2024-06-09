// { dg-do compile { target c++11 } }

#define SA(X) static_assert((X),#X)

class ClassType { };
using cClassType = const ClassType;
using vClassType = volatile ClassType;
using cvClassType = const volatile ClassType;

// Positive tests.
SA(__is_volatile(volatile int));
SA(__is_volatile(const volatile int));
SA(__is_volatile(vClassType));
SA(__is_volatile(cvClassType));

// Negative tests.
SA(!__is_volatile(int));
SA(!__is_volatile(const int));
SA(!__is_volatile(ClassType));
SA(!__is_volatile(cClassType));
