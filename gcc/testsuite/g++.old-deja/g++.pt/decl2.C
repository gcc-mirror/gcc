// Build don't link:
// crash test - XFAIL *-*-*

// Simplified from testcase by Christophe Boyanique <boyan@imac.u-paris2.fr>

template <class T> struct foo { foo(); };
template<class T> foo<T>::foo() {}
T; // ERROR - no type
