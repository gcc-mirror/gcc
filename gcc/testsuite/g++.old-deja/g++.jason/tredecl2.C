// { dg-do assemble  }
// No bug; making sure my fix for tredecl.C doesn't break other cases

template<class T> struct Foo { Foo<T> * me() { return this; } };
Foo<int> i;
