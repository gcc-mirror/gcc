// PR c++/72801
// { dg-do compile { target c++11 } }

template < typename, typename > struct A {};

template < typename ... T > struct B
{ 
  template < typename > struct C
  { 
    static const int a = 0;
  };

  template < typename R, typename ... S >
  struct C < R (A < T, S > ...) >
  { 
    static const int a = 1;
  };
};

#define SA(X) static_assert ((X), #X)
SA(B <>::C<int()>::a == 1);

