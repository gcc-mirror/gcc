// Make sure that forward declarations of specializations work...

template <class T> class A { };
class A<int>;
A<int> a;			// ERROR - incomplete type
class A<int> { };
