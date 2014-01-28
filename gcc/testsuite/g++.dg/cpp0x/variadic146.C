// PR c++/58606
// { dg-require-effective-target c++11 }

template<int&...I> struct A
{
  template<typename> struct B;

  template<typename T> struct B<T*> {};
};
