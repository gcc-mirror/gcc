// PR c++/3478
// { dg-options "-fshow-column" }

template <typename> struct A
{
    enum E {};
};

template <typename T> void foo()
{
  enum          A<void>::E e1;
  typename      A<T>::E    e2;
  enum          A<T>::E    e3;
  enum typename A<T>::E    e4;
}

// { dg-error "3:two or more data types in declaration of 'e4'" "2 or more" { target *-*-* } 14 }
