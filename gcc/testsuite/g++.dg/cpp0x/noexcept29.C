// { dg-do compile { target c++11 } }
// { dg-additional-options "-fno-exceptions" }

// PR68475 we used to not check eh spec matching with -fno-exceptions,
// but this could lead to ICEs.

template <typename> struct traits;

template <typename T> struct X
{
  void Foo () noexcept (traits <T>::foo ()); // { dg-message "previous declaration" }
};

template <typename T>
void
X<T>::Foo () noexcept (traits <T>::bar ()) // { dg-error "different exception specifier" }
{
}

