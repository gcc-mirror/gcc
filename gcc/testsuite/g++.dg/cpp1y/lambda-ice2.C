// PR c++/61135
// { dg-do compile { target c++14 } }

struct A
{
  int funcA(){return 0;}
};

template<class>
struct B:virtual public A{
  void funcB(){
    [a=this->funcA()]{};
  }
};

int main()
{
  B<A> b;
  b.funcB();
  return 0;
}
