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

// Here, columns nums are not very accurate either. Still acceptable though
// { dg-error "30:invalid type in declaration before ';' token" "" { target *-*-* } 14 }
// { dg-error "30:two or more data types in declaration of 'e4'" "" { target *-*-* } 14 }
