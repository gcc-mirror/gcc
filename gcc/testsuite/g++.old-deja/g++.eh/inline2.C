// Test that inlining a destructor with a catch block doesn't confuse the
// enclosing try block.
// Special g++ Options: -O

struct A {
  ~A()
  {
    try { throw 1; }
    catch (...) { }
  }
};

int main ()
{
  try
    {
      A a;
      throw 42;
    }
  catch (int i)
    {
      return (i != 42);
    }
}
