// PR c++/54987

struct Argument1
{
};

struct Argument2
{
};

template<typename T>
struct Template1
{
  Template1() {}

  template<typename Y>
  Template1( const Template1<Y>& ) {}
};

template<typename T>
struct Template2
{
  Template2() {}

  template<typename Y>
  Template2( const Template1<Y>& ) {}
};

template <typename T>
struct make_type
{
  typedef Argument1 type;
};

template<typename T>
void foo( T, Template1<typename make_type<T>::type> )
{
}

template<typename T>
void foo( T, Template2<typename make_type<T>::type> )
{
}

int main()
{
  Template1<Argument2> t;
  Argument1 a;
  foo( a, t );  // { dg-error "ambiguous" }
  return 0;
}
