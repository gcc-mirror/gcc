template <class T>
 struct Foo
 {};
 
 template <class T>
 void Foo<T>::NON_EXISTENT(int* val = new int()) {} // { dg-error "" }
 
