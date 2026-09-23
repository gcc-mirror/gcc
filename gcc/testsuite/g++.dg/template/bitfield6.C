// PR c++/125674
// { dg-do compile }

template <typename T>
struct A {
  int f ();
  int i : f;	// { dg-error "width of bit-field 'A<T>::i' has non-integral type '<unresolved overloaded function type>'" }
};
A<int> a;
