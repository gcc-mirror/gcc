// Build don't link:

template <class T> class A;
// template <>
class A<int>; // ERROR - missing template header - XFAIL *-*-*
