// Red Hat bug 750545
// { dg-do run { target c++98 } }

class excep {};
class A
{
public:
  ~A() { throw excep(); }
};

class B
{
  A a;
};

class C
{
  B b;
};

void f()
{
  C* c = new C();

  try
  {
    delete c;
  }
  catch(...)
  {}
}

int main()
{
  f();
}
