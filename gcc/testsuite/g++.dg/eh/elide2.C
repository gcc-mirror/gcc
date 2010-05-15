// PR c++/13944

// Verify that we don't call terminate() if initializing the exception
// object throws.

// { dg-do run }

struct A
{
  A() { }
  A(const A&) { throw 1; }
};

A a;

int main()
{
  try
    {
      throw a;
    }
  catch (int)
    {
      return 0;
    }
  catch (A&)
    {
      return 2;
    }
  return 3;
}
