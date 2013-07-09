// PR c++/57526
// { dg-require-effective-target c++11 }

template<class T>
struct A
{
  void bar( ) { }

  void foo( )
  {
    auto* this_ptr = this;
    auto lc = [&]( )
      {
	this_ptr->bar();
      };
    lc();
  }
};

int main()
{
  A<int> a;
  a.foo();
}
