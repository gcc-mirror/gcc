// { dg-do compile }
// Origin: <gawrilow at math dot tu-berlin dot de>
// PR c++/10750: error when using a static const member initialized 
//  with a dependent expression as constant-expression

struct A
{
  enum { a = 42 };
};

template <class Q>
struct B
{
  static const int b = Q::a;
};

template <typename T, template <typename> class P>
struct C
{
  static const bool a = T::a;
  static const bool a_ = a;
  static const bool b = P<T>::b;
  static const bool b_ = b;
  static const int c = sizeof(T);
  static const int c_ = c;
};

template struct C<A,B>;
