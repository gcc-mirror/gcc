// { dg-do assemble  }
// It is illegal to use the name of a class template for anything else,
// including another class template.

template <class T> class A { };	// { dg-error "" } 
template <class U, class V> class A { }; // { dg-error "" } 
