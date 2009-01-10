// { dg-do compile }
// This code used to be accepted but it is invalid as there is no
// value initialization of a reference type.
// PR c++/36695

// We should we able to diagnostic this without instantiating the template
template <int a1>
int f()
{
  typedef int& T;
  T a = T();  // { dg-error "value-initialization of reference" }
}

