// { dg-do assemble  }
// GROUPS passed templates overloading
template<class T> class Vector { };
template<class T> struct Sort { static void sort (Vector<typename T::foo> &); };
template<class T> void Sort<T>::sort (Vector<typename T::foo> &) { }
struct whee { typedef int foo; };

void f (Vector<int> &vi) { Sort<whee>::sort (vi); }
