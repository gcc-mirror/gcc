void foo();

struct A
{
  ~A()
  {
    try
    {
      foo();
      foo();
    }
    catch (...)
    {
    }
  }
};

void bar()
{
  A a1, a2;
}
void foo();

struct A
{
  ~A()
  {
    try
    {
      foo();
      foo();
    }
    catch (...)
    {
    }
  }
};

void bar()
{
  A a1, a2;
}
