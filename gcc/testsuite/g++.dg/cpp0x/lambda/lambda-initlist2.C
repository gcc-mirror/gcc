// PR c++/54947
// { dg-options -std=gnu++11 }

struct X
{
  template<typename L>
    X(L)
    { }
};

template<typename A>
  void
  test()
  {
    int i = 0;

    A a_ok_1( [=] { return i; } );  // OK
    A a_ok_2( [i] { return i; } );  // OK

    A a_err_1{ [i] { return i; } };  // error
    A a_err_2{ [=] { return i; } };  // error
  }

int main()
{
  test<X>();
}
