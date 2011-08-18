// Origin PR c++/45625
// { dg-do compile }

struct Outer
{
  static const int value = 1 ;

  template< int value >
  struct Inner
  {
    static const int*
    get_value()
    { 
      return &value ;// { dg-error "lvalue required" }
    }
  };
};

template class Outer::Inner<2>;
