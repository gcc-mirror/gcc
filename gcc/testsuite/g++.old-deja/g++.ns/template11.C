// { dg-do run  }
void foo(){}

namespace Bar{
  template<class X>
    class Y{
      friend void foo(Y<X>){}
    };
}

int main()
{
  Bar::Y<int> y;
  foo(y);
  foo();
}
