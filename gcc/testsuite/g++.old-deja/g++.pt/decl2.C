// Build don't link:

// Simplified from testcase by Christophe Boyanique <boyan@imac.u-paris2.fr>

template <class T> struct foo { foo(); };
template<class T> foo<T>::foo() {}
T // ERROR - currently an ICE - XFAIL *-*-*
