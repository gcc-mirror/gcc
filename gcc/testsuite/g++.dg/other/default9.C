// PR c++/15339

template<typename> void fun(int); 
template<typename> void fun(int = 0);  // { dg-error "default arguments" }

class A
{
  template<typename> void fun(int);
};

template<typename> void A::fun(int = 0) { } // { dg-error "default arguments" }

class B
{
  void fun(int);
};

void B::fun(int = 0) { }
