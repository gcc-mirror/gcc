// It is illegal to use the name of a class template for anything else,
// including another class template.

template <class T> class A { };	// ERROR - 
template <class U, class V> class A { }; // ERROR - 
