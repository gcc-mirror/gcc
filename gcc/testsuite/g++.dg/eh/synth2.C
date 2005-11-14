// PR c++/24580
// { dg-do run }

struct vbase {};

struct foo : virtual vbase
{
  foo()
  {
    throw "exception in foo ctor";
  }
};

struct bar :  public foo {};

int main()
{
  try
    {
      bar a;
    }
  catch ( ... ) { }
  return 0;
}
