// PR c++/14550

struct A { 
  A();
};

template <int> void foo()
{
  A *p = new A;
}

void bar()
{
  foo<0>();
}


