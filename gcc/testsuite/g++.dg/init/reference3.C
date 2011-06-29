// { dg-do compile }
// This code used to be accepted but it is invalid as there is no
// value initialization of a reference type.
// PR c++/36695

template <typename T>
T f()
{
  T a = T();  // { dg-error "value-initialization of reference" }
}

int &a = f<int&>(); // { dg-message "required from here" }

