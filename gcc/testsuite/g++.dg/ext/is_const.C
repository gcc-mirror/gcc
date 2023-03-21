// { dg-do compile { target c++11 } }

#define SA(X) static_assert((X),#X)

class ClassType { };
using cClassType = const ClassType;
using vClassType = volatile ClassType;
using cvClassType = const volatile ClassType;

// Positive tests.
SA(__is_const(const int));
SA(__is_const(const volatile int));
SA(__is_const(cClassType));
SA(__is_const(cvClassType));

// Negative tests.
SA(!__is_const(int));
SA(!__is_const(volatile int));
SA(!__is_const(ClassType));
SA(!__is_const(vClassType));
