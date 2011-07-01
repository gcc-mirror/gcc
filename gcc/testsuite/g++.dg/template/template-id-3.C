// PR c++/48261

typedef double (*gaddType)(double,double);
struct Foo2
{
  static gaddType add;
};

template<typename T>
struct Something
{
  void work()
  {
    double x=T::template add<double>(5.0,6.0); // { dg-error "add" }
  }
};

int main()
{
  Something<Foo2> s2;
  s2.work();
}
